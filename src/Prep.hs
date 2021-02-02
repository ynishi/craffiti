{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
module Prep where

import RIO
import Data.ByteString.UTF8 as BSU
import qualified Turtle as T
import qualified Control.Foldl as Fold

import Control.Monad.Operational as O

-- GADTs style Prep include main tasks
-- when integrate with Applicative, make instance of it, but GADTs is not supported default Functor, use Coyoneda.Functor
-- This support for Type Sig for Functor
-- or Core Data don't use GADTs.
-- https://jakewheat.github.io/intro_to_parsing/#getting-started parsec support to Applicative, Parser is instance of Applicative.

data Prep a where
  Require :: String -> Prep(String, Int)
  Init :: [String] -> Prep (String, Int)
  Install :: [String] -> FilePath -> Prep (String, Int)
  Check :: [Int] -> Prep (String, Int)

instance Show (Prep a) where
  show (Require x) = x
  show (Init x) = show x
  show (Install x f) = show x ++ show f
  show (Check x) = show x

type ProjectName = String

reactProgram :: ProjectName -> Program Prep (String,Int)
reactProgram projectName = do
  (x, xi) <- singleton . Init $ ["npx", "create-react-app", projectName, "--template", "typescript"]
  (y, yi) <- singleton $ Install ["npm", "install"] projectName
  (_, i) <- singleton $ Check [xi, yi]
  return (x++y, i)

stackProgram :: ProjectName -> Program Prep (String,Int)
stackProgram projectName = do
  (x, xi) <- singleton . Init $ ["stack", "new", projectName]
  (y, yi) <- singleton $ Install ["stack", "build"] projectName
  (_, i) <- singleton $ Check [xi, yi]
  return (x++y, i)

noOpProgram :: Program Prep (String,Int)
noOpProgram = do
  (_, xi) <- singleton . Init $ ["echo", "noOp", "init"]
  (_, yi) <- singleton $ Install ["echo", "noOp", "init"] "."
  (_, i) <- singleton $ Check [xi, yi]
  return ("", i)

runPrep :: Bool -> Program Prep a -> IO a
runPrep initOpt = interpretWithMonad eval where
  eval :: Prep a -> IO a
  eval (Init commands) = runSimpleApp $ do
    logInfo . displayBytesUtf8 . BSU.fromString $ "start init:" ++ show commands
    res <- Prep.get . toCmd $ commands
    logInfo . displayBytesUtf8 . BSU.fromString $ "end init:" ++ show commands ++ ":"++ show res
    return  (show res, 0)
  eval (Install commands projectName) = runSimpleApp $ do
      res <- if initOpt then
         logInfo . displayBytesUtf8 . BSU.fromString $ "option init set, don't install:" ++ show commands
      else do
         T.cd $ T.fromString projectName
         logInfo . displayBytesUtf8 . BSU.fromString $ "start install:" ++ show commands
         res <- Prep.get . toCmd $ commands
         logInfo . displayBytesUtf8 . BSU.fromString $ "end install:" ++ show commands ++ show res
      return  (show res, 0)
  eval (Check x) = runSimpleApp $ do
    logInfo . displayBytesUtf8 . BSU.fromString $ "check:" ++ show x
    return (show x, sum x)

get cmd = T.fold (T.inshell cmd T.empty) Fold.head

toCmd :: [String] -> Text
toCmd = T.fromString . unwords