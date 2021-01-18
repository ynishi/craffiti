module Main where

import Lib
import RIO

main :: IO ()
main = greet =<< parse
