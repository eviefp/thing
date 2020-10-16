module Main where

import Command as C
import Prelude

main :: IO ()
main = C.getCommand >>= print
