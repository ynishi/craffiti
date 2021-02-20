{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

data Command
  = New { projectName :: String
        , isInitOpt :: Bool
        , pluginOpt :: Maybe String
        , disableOpt :: Maybe String }
  | Update { isDryRun :: Bool
           , pluginOpt :: Maybe String }

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
  subparser
    (subCommand "new" "create new project" createNewOptions <>
     subCommand "update" "update project" createUpdateOptions)

subCommand :: String -> String -> Parser a -> Mod CommandFields a
subCommand name desc parser =
  command name (info (parser <**> helper) (progDesc desc))

createNewOptions :: Parser Command
createNewOptions =
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

createUpdateOptions :: Parser Command
createUpdateOptions =
  Update <$> switch (long "dryrun" <> short 'd' <> help "whether to dryrun") <*>
  optional
    (strOption
       (long "plugin" <> short 'p' <> metavar "TARGET=PLUGIN(,...)" <>
        help "set plugin with target, support comma separation"))
