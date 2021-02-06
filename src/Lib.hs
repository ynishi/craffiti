{-# LANGUAGE DisambiguateRecordFields #-}

module Lib
  ( parse
  , run
  ) where

import RIO
import RIO.List as RL

import qualified Data.Yaml as Y
import Options.Applicative
import System.Directory
import System.FilePath
import qualified Turtle as T

import qualified Control.Foldl as Fold
import Data.ByteString.UTF8 as BSU

import Plugin
import Prep

data Opt = Opt
  { optCommand :: Command
  }

data Command = New
  { projectName :: String
  , initOpt :: Bool
  , pluginOpt :: Maybe String
  }

optParser :: Parser Opt
optParser =
  Opt <$>
  subparser (command "new" (info createOptions (progDesc "create new project")))

createOptions :: Parser Command
createOptions =
  New <$> strArgument (metavar "projectName" <> help "project name for create") <*>
  switch (long "init" <> short 'i' <> help "whether to only init") <*>
  optional
    (strOption
       (long "plugin" <> short 'p' <> metavar "TARGET=PLUGIN" <>
        help "set front plugin"))

parse :: IO Opt
parse = execParser opts
  where
    opts =
      info
        (optParser <**> helper)
        (fullDesc <> progDesc "A scaffolding cli tool for rapid prototyping" <>
         header "craffiti - a simple cli")

projectDirs :: [T.FilePath]
projectDirs = ["", ".craffiti", "front", "server", "ai", "batch"]

data PluginMeta = PluginMeta
  { pDir :: String
  , pPlugin :: String
  } deriving (Show)

toPluginMeta opt = PluginMeta dir plugin
  where
    d = '='
    dir = takeWhile (/= d) opt
    plugin = fromMaybe "" . RL.tailMaybe . dropWhile (/= d) $ opt

run :: Opt -> IO ()
run opt =
  case optCommand opt of
    New projectName initOpt pluginOpt ->
      runSimpleApp $ do
        let pluginMeta = toPluginMeta $ fromMaybe "" pluginOpt
        home <- liftIO getHomeDirectory
        let pluginPath =
              home </> ".craffiti" </> "plugins" </> pPlugin pluginMeta ++
              ".yml"
        isExistsPlugin <- liftIO . doesFileExist $ pluginPath
        if isExistsPlugin
          then logInfo $
               displayBytesUtf8 $ BSU.fromString $ "Plugin: " ++ show pluginPath
          else error ("Plugin not found: " ++ show pluginPath)
        prepDataRaw <- liftIO $ Y.decodeFile pluginPath
        let prepData = pdr2pd [] $ fromMaybe (PrepDataRaw {}) prepDataRaw
        let pluginPrep = pluginProgram projectName prepData
        logInfo $
          displayBytesUtf8 $
          BSU.fromString $ "Create new project: " ++ projectName
        let projectPath = T.decodeString projectName
        let ppath = T.fromString $ pDir pluginMeta
        mapM_
          (\d -> T.mkdir $ projectPath T.</> d)
          (projectDirs ++ ([ppath | ppath `notElem` projectDirs]))
        cdIn projectName "front"
        liftPrep (reactProgram projectName)
        cdInParent "server"
        liftPrep (stackProgram projectName)
        cdInParent "ai"
        noOp
        cdInParent "batch"
        noOp
        cdInParent . pDir $ pluginMeta
        liftPrep pluginPrep
        logInfo $
          displayBytesUtf8 $ BSU.fromString "success, Have a fan to craft!"
      where liftPrep = liftIO . runPrep initOpt
            noOp = liftPrep noOpProgram
  where
    cdInParent = cdIn ".."
    cdIn parent target =
      T.cd (T.fromString parent) >> T.cd (T.fromString target)
