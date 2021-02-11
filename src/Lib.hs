{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

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

import Data.ByteString.UTF8 as BSU
import Data.List.Split
import qualified Data.Map as M

import Control.Monad.Operational (Program)
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

projectBaseDirs :: [T.FilePath]
projectBaseDirs = ["", ".craffiti"]

data DoPrep = DoPrep
  { pdDir :: T.FilePath
  , pdProgram :: Program Prep (String, Int)
  }

data PluginMeta = PluginMeta
  { pDir :: String
  , pPlugin :: String
  } deriving (Show)

toPrepMeta :: String -> PluginMeta
toPrepMeta opt = PluginMeta dir plugin
  where
    d = '='
    dir = takeWhile (/= d) opt
    plugin = fromMaybe "" . RL.tailMaybe . dropWhile (/= d) $ opt

toPrepMetas :: String -> [PluginMeta]
toPrepMetas = map toPrepMeta . splitOn ","

toPluginPrep ::
     (MonadIO m, MonadReader env m, HasLogFunc env, IsString a)
  => ProjectName
  -> PluginMeta
  -> m (a, Program Prep (String, Int))
toPluginPrep projectName PluginMeta {pDir, pPlugin} = do
  home <- liftIO getHomeDirectory
  let pluginPath = home </> ".craffiti" </> "plugins" </> pPlugin ++ ".yml"
  isExistsPlugin <- liftIO . doesFileExist $ pluginPath
  if isExistsPlugin
    then logInfo $
         displayBytesUtf8 $ BSU.fromString $ "Plugin: " ++ show pluginPath
    else error ("Plugin not found: " ++ show pluginPath)
  prepDataRaw <- liftIO $ Y.decodeFileEither pluginPath
  let prepData = pdr2pd [] $ fromRight emptyPrepDataRaw prepDataRaw
  return (T.fromString pDir, pluginProgram projectName prepData)

toPluginPreps ::
     (Traversable t, MonadIO m, MonadReader env m, HasLogFunc env, IsString a)
  => ProjectName
  -> t PluginMeta
  -> m (t (a, Program Prep (String, Int)))
toPluginPreps projectName = mapM (toPluginPrep projectName)

defaultPreps ::
     (Ord k, IsString k) => ProjectName -> Map k (Program Prep (String, Int))
defaultPreps projectName =
  M.fromList
    [ ("front", reactProgram projectName)
    , ("server", stackProgram projectName)
    , ("ai", noOpProgram)
    , ("batch", noOpProgram)
    ]

run :: Opt -> IO ()
run opt =
  case optCommand opt of
    New projectName initOpt pluginOpt ->
      runSimpleApp $ do
        let pluginMetas = toPrepMetas $ fromMaybe "" pluginOpt
        pluginPreps <- toPluginPreps projectName pluginMetas
        logInfo $
          displayBytesUtf8 $
          BSU.fromString $ "Create new project: " ++ projectName
        let preps :: [DoPrep] =
              RL.map snd . M.toList . M.mapWithKey DoPrep $
              M.union (M.fromList pluginPreps) (defaultPreps projectName)
        projectPathAbs <- liftIO . makeAbsolute $ projectName
        let projectPath = T.fromString projectPathAbs
        mapM_ (\d -> T.mkdir $ projectPath T.</> d) projectBaseDirs
        mapM_
          (\DoPrep {pdDir, pdProgram} -> do
             logInfo . displayBytesUtf8 $ BSU.fromString ("do:" ++ show pdDir)
             let pdPath = projectPath T.</> pdDir
             T.mkdir pdPath >> T.cd pdPath
             liftIO $ runPrep initOpt pdProgram)
          preps
        logInfo $
          displayBytesUtf8 $ BSU.fromString "success, Have a fan to craft!"
