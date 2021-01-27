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
  Init :: String -> Prep (String, Int)
  Install :: String -> FilePath -> Prep (String, Int)
  Check :: [Int] -> Prep (String, Int)

instance Show (Prep a) where
  show (Init x) = x
  show (Install x f) = x ++ show f
  show (Check x) = show x

type ProjectName = String

reactProgram :: ProjectName -> Program Prep (String,Int)
reactProgram projectName = do
  (x, xi) <- singleton . Init $ "npx create-react-app " ++ projectName
  (y, yi) <- singleton $ Install "npm start" projectName
  (_, i) <- singleton $ Check [xi, yi]
  return (x++y, i)

runPrep :: Program Prep a -> IO a
runPrep = interpretWithMonad eval where
  eval :: Prep a -> IO a
  eval (Init x) = do
    T.cd "front"
    runSimpleApp . logInfo . displayBytesUtf8 . BSU.fromString $ "start init:" ++ x
    res <- Prep.get (T.fromString x)
    runSimpleApp . logInfo . displayBytesUtf8 . BSU.fromString $ "end init:" ++ x ++ show res
    return  (show res, 0)
  eval (Install x _) = do
    runSimpleApp . logInfo . displayBytesUtf8 . BSU.fromString $ "start install:" ++ x
    res <- Prep.get (T.fromString x)
    runSimpleApp . logInfo . displayBytesUtf8 . BSU.fromString $ "end install:" ++ x ++ show res
    return  (show res, 0)
  eval (Check x) = do
    runSimpleApp . logInfo . displayBytesUtf8 . BSU.fromString $ "check:" ++ show x
    return (show x, sum x)

get cmd = T.fold (T.inshell cmd T.empty) Fold.head