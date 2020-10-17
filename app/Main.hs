module Main where

import Command as C
import Prelude

main :: IO ()
main =
    C.getCommand >>= \case
        C.Create { name = n } -> putStrLn $ "Create " <> n
        C.List            -> putstrLn "List"
        C.Update { C.name = n } -> putstrLn $ "Update" <> n
