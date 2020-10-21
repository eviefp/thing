module CreateCommand
    ( go
    ) where

import           Control.Monad
    (guard)
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
import           Prelude
import qualified System.Directory       as Dir
import qualified System.Directory.Extra as Extra
import           System.FilePath
    ((</>))
import qualified System.FilePath        as FP
import qualified Text.Glabrous          as G

go :: Text -> String -> IO ()
go name templatePath =
    checkPreconditions name templatePath
        *> createProjectDir name
        *> findAllTemplateFiles templatePath
        >>= traverse_ (processFile name templatePath)

checkPreconditions :: Text -> String -> IO ()
checkPreconditions name templatePath =
    (&&)
        <$> (not <$> Dir.doesDirectoryExist (T.unpack name))
        <*> Dir.doesDirectoryExist templatePath
        >>= guard

createProjectDir :: Text -> IO ()
createProjectDir = Dir.createDirectory . T.unpack

findAllTemplateFiles :: String -> IO [String]
findAllTemplateFiles = Extra.listFilesRecursive

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

