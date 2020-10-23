module CreateCommand
    ( go
    ) where

import           Control.Lens
    ((&), (?~))
import           Control.Monad
    (guard)
import qualified Data.Aeson             as A
import           Data.Either.Extra
    (fromRight')
import           Data.Foldable
    (traverse_)
import           Data.Maybe
    (fromJust, fromMaybe)
import           Data.Text
    (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import qualified Data.Yaml              as Yaml
import           GHC.Generics
    (Generic)
import           Prelude
import qualified System.Directory       as Dir
import qualified System.Directory.Extra as Extra
import           System.FilePath
    ((</>))
import qualified System.FilePath        as FP
import qualified System.Process         as Process
import qualified System.Process.Lens    as LP
import qualified Text.Glabrous          as G

go :: Text -> String -> IO ()
go name templatePath = do
    checkPreconditions name templatePath
    createProjectDir name
    templateFiles <- findAllTemplateFiles templatePath
    traverse_ (processFile name templatePath) templateFiles
    executePostCreateHooks templatePath (T.unpack name)

checkPreconditions :: Text -> String -> IO ()
checkPreconditions name templatePath =
    (&&)
        <$> (not <$> Dir.doesDirectoryExist (T.unpack name))
        <*> Dir.doesDirectoryExist templatePath
        >>= guard

createProjectDir :: Text -> IO ()
createProjectDir = Dir.createDirectory . T.unpack

findAllTemplateFiles :: String -> IO [String]
findAllTemplateFiles path =
    filter (not . (`elem` skipFiles))  <$> Extra.listFilesRecursive path
  where
    skipFiles :: [String]
    skipFiles = [ "thing.template.yaml" ]

processFile :: Text -> String -> String -> IO ()
processFile name templatePath path = do
    template <- fromRight' <$> G.readTemplateFile path
    let
        name' = fromMaybe name $ T.stripSuffix "/" name
        name'' = FP.takeFileName $ T.unpack name'
        context = G.fromList [("name", T.pack name'')]
        result  = G.partialProcess' template context
    case result of
        G.Final t -> saveFile name templatePath path t
        _         -> error $ "Could not process file " <> path

saveFile :: Text -> String -> String -> Text -> IO ()
saveFile dir templatePath originalPath content = do
    let
        relativePath =
            fromJust
                $ T.stripPrefix
                    (T.pack templatePath)
                    (T.pack originalPath)
        correctedRelativePath =
            fromMaybe relativePath $ T.stripPrefix "/" relativePath
        newPath = T.unpack dir </> T.unpack correctedRelativePath
        directory = FP.takeDirectory newPath
    Dir.createDirectoryIfMissing True directory
    TIO.writeFile newPath content

executePostCreateHooks :: String -> String -> IO ()
executePostCreateHooks templatePath path = do
    hooks <- hooks <$> readTemplateFile templatePath
    confirmHooks hooks >>= guard
    traverse_ (performHook path) hooks

data TemplateFile
  = TemplateFile
      { hooks       :: [Hook]
      , description :: String
      }
  deriving stock Generic
  deriving anyclass (A.FromJSON)

newtype Hook = Hook String
  deriving newtype (A.FromJSON, Show)

readTemplateFile :: String -> IO TemplateFile
readTemplateFile base = Yaml.decodeFileThrow $ base </> "thing.template.yaml"

confirmHooks :: [Hook] -> IO Bool
confirmHooks hooks = do
    putStrLn "Template hooks:"
    traverse_ print hooks
    putStrLn "Execute y/n: "
    getLine >>= \case
        "y" -> pure True
        _   -> pure False

performHook :: String -> Hook -> IO ()
performHook path (Hook hook) = do
    let process = Process.shell hook
    currentDir <- Dir.getCurrentDirectory
    let process' = process & LP.cwd_ ?~ (currentDir </> path)
    (_, _, _, handle) <- Process.createProcess process'
    _ <- Process.waitForProcess handle
    pure ()

