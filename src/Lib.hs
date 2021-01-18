{-# LANGUAGE DisambiguateRecordFields #-}

module Lib
  ( parse
  , greet
  ) where

import RIO

import Options.Applicative

import Data.ByteString.UTF8 as BSU

newtype Opt = Opt
  { hello :: String
  }

opt :: Parser Opt
opt =
  Opt <$>
  strOption (long "hello" <> metavar "TARGET" <> help "Target for greeting")

parse :: IO Opt
parse = execParser opts
  where
    opts =
      info
        (opt <**> helper)
        (fullDesc <> progDesc "Print greet TARGET" <> header "craffiti - a simple cli")

greet :: Opt -> IO ()
greet (Opt target) =
  runSimpleApp $
  logInfo $ displayBytesUtf8 $ BSU.fromString $ "Hello, " ++ target ++ "!"
