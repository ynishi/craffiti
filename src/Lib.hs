{-# LANGUAGE DisambiguateRecordFields #-}

module Lib
  ( parse
  , run
  ) where

import RIO

import Options.Applicative
import qualified Turtle as T

import Data.ByteString.UTF8 as BSU
import qualified Control.Foldl as Fold

import Prep

data Opt = Opt
  { optCommand :: Command
  }

data Command = New
  { projectName :: String,
    initOpt :: Bool}


optParser :: Parser Opt
optParser =
  Opt <$>
  subparser (command "new" (info createOptions (progDesc "create new project")))

createOptions :: Parser Command
createOptions =
  New <$> strArgument (metavar "projectName" <> help "project name for create")
      <*> switch (long "init" <> short 'i' <> help "whether to only init")

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

run :: Opt -> IO ()
run opt =
  case optCommand opt of
    New projectName initOpt ->
      runSimpleApp $ do
        logInfo $
          displayBytesUtf8 $
          BSU.fromString $ "Create new project: " ++ projectName
        let projectPath = T.decodeString projectName
        mapM_ (\d -> T.mkdir $ projectPath T.</> d) projectDirs
        T.cd projectPath
        T.cd "front"
        liftIO $ runPrep initOpt (reactProgram projectName)
        T.cd ".." >> T.cd "server"
        liftIO $ runPrep initOpt (stackProgram projectName)
        T.cd ".." >> T.cd "ai"
        liftIO $ runPrep initOpt noOpProgram
        T.cd ".." >> T.cd "batch"
        liftIO $ runPrep initOpt noOpProgram
        logInfo $
          displayBytesUtf8 $ BSU.fromString "success, Have a fan to craft!"
