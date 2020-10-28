module Main where

import           Command       as C
import qualified CreateCommand as CC
import           Prelude

main :: IO ()
main =
    C.getCommand >>= \case
        C.Create { name, template} ->
            CC.go (CC.Name name) template
