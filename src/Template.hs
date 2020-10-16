module Template where

import Prelude

import Data.Text
    (Text)
import GHC.Generics
    (Generic)

newtype TString = TString Text
    deriving stock (Generic)

newtype FileName = FileName String
    deriving stock (Generic)

newtype DirectoryName = DirectoryName String
    deriving stock (Generic)

data File = StaticFile FileName | TemplateFileName TString
    deriving stock (Generic)

data DirectoryType = StaticDirectory DirectoryName | TemplateDirectoryName TString
    deriving stock (Generic)

data Directory
  = Directory
      { dirType    :: DirectoryType
      , dirFiles   :: [File]
      , dirSubdirs :: [Directory]
      }
  deriving stock (Generic)

data Root
  = Root
      { rootFiles :: [File]
      , rootDirs  :: [Directory]
      }
  deriving stock (Generic)

data Template
  = Template
      { name        :: Text
      , description :: Text
      , structure   :: Root
      }
  deriving stock (Generic)
