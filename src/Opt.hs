{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

module Opt
  ( Opt(..)
  , Command(..)
  , parse
  ) where

import Options.Applicative
import RIO

data Opt = Opt
  { optCommand :: Command
  }

data Command = New
  { projectName :: String
  , initOpt :: Bool
  , pluginOpt :: Maybe String
  , disableOpt :: Maybe String
  }

parse :: IO Opt
parse = execParser opts
  where
    opts =
      info
        (optParser <**> helper)
        (fullDesc <> progDesc "A scaffolding cli tool for rapid prototyping" <>
         header "craffiti - a simple cli")

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
       (long "plugin" <> short 'p' <> metavar "TARGET=PLUGIN(,...)" <>
        help "set plugin with target, support comma separation")) <*>
  optional
    (strOption
       (long "disable" <> short 'd' <> metavar "TARGET(,...)" <>
        help "set disable target, support comma separation"))
