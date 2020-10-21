module Template where

import Data.Text
    (Text)

newtype Hook = PostHook Text

data Template
  = Template
      { hooks       :: [Hook]
      , description :: Text
      }

