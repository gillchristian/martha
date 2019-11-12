{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Martha
  ( run,
    runMartha,
  )
where

import Cheapskate (def, markdown)
import Cheapskate.Lucid (renderDoc)
import Control.Monad (forM_, when)
import Data.Bifunctor (bimap)
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Lucid
import qualified Static
import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified System.FilePath as Path
import System.FilePath ((<.>), (</>))
import Utils (headOr)

type Extension = String

type FileName = FilePath

type FullPath = FilePath

type Item = Either File Directory

data Directory
  = -- | A directory: name, full path and contents (other directories and files)
    Directory FileName FullPath [Directory] [File]
  deriving (Show, Eq, Ord, Generic)

data File
  = -- | A file: name, full path and extension
    File FileName FullPath Extension
  deriving (Show, Eq, Ord, Generic)

fPath :: File -> FilePath
fPath (File _ path _) = path

dName :: Directory -> FilePath
dName (Directory name _ _ _) = name

-- -----------------------------------------------------------------------------

traverseDirectory :: FilePath -> FilePath -> IO Directory
traverseDirectory path name = do
  contents <- filter (not . isIgnoredPath) <$> Dir.getDirectoryContents path
  (files, dirs) <-
    normaliseItems <$> Maybe.catMaybes <$> traverse (toItem path) contents
  pure $ Directory name path dirs files

toItem :: FilePath -> FilePath -> IO (Maybe Item)
toItem rootPath name = do
  let path = Path.normalise $ rootPath </> name
  exists <- Dir.doesPathExist path
  isSym <- Dir.pathIsSymbolicLink path
  isDir <- Dir.doesDirectoryExist path
  if not exists || isSym
    then pure $ Nothing
    else
      if isDir
        then Just <$> Right <$> traverseDirectory path name
        else pure $ Just $ Left $ File name path (takeExtension name)

isEmpty :: Directory -> Bool
isEmpty (Directory _ _ [] []) = True
isEmpty _ = False

normaliseFiles :: [File] -> [File]
normaliseFiles = List.sort . filter isMdFile

normaliseDirs :: [Directory] -> [Directory]
normaliseDirs = List.sort . filter (not . isEmpty)

normaliseItems :: [Item] -> ([File], [Directory])
normaliseItems = bimap normaliseFiles normaliseDirs . Either.partitionEithers

-- TODO: filter by '.gitignore'
isIgnoredPath :: FilePath -> Bool
isIgnoredPath "." = True
isIgnoredPath ".." = True
isIgnoredPath ".git" = True
isIgnoredPath "node_modules" = True
isIgnoredPath ".stack-work" = True
isIgnoredPath _ = False

isMdFile :: File -> Bool
isMdFile (File _ _ "md") = True
isMdFile _ = False

-- -----------------------------------------------------------------------------

data ToCEntry
  = -- | A ToC Entry contains the parts of the directory's path and all the files in it
    ToCEntry [FilePath] [File]

toc :: Directory -> [ToCEntry]
toc = go []
  where
    go :: [FilePath] -> Directory -> [ToCEntry]
    go _ (Directory _ _ [] []) = []
    go acc (Directory name _ [] files) = [ToCEntry (reverse $ name : acc) files]
    go acc (Directory name _ dirs []) = go (name : acc) =<< dirs
    go acc (Directory name _ dirs files) =
      (ToCEntry (reverse $ name : acc) files) : (go (name : acc) =<< dirs)

-- -----------------------------------------------------------------------------

takeExtension :: FilePath -> Extension
takeExtension path =
  case Path.takeExtension path of
    ('.' : extension) -> extension
    extension -> extension

makeAbsolute :: FilePath -> FilePath
makeAbsolute path
  | Path.isAbsolute path = path
  | otherwise = Path.pathSeparator : path

-- -----------------------------------------------------------------------------

type Html' = Html ()

renderToC :: [ToCEntry] -> Html'
renderToC [] = div_ [class_ "toc"] mempty
renderToC entries =
  div_ [class_ "toc", style_ "font-family: monospace; display: flex;"] $ do
    ul_ [class_ "directories"] $ do
      mapM_ renderEntry entries

renderEntry :: ToCEntry -> Html'
renderEntry (ToCEntry dirs files) =
  li_ [class_ "li-directory"] $ do
    mapM_ renderDirCrumb dirs
    ul_ [class_ "files"] $ do
      mapM_ renderFileEntry files

renderDirCrumb :: FilePath -> Html'
renderDirCrumb path = do
  span_ . toHtml . Text.pack $ path
  span_ "/"

renderFileEntry :: File -> Html'
renderFileEntry (File name url _) =
  li_ [class_ "li-file"] $
    a_
      [href_ $ Text.pack $ (makeAbsolute url) <.> "html"]
      (toHtml $ Text.pack name)

empty :: Text.Text
empty = ""

renderPath :: Directory -> Maybe Html' -> Html'
renderPath rootDir mContent = html_ $ do
  head_ $ do
    link_ [rel_ "icon", href_ "data:,"]
    title_ $ toHtml $ "Markdown within " ++ dName rootDir
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    -- CSS
    link_ [rel_ "stylesheet", type_ "text/css", href_ "/highlight.css"]
    link_ [rel_ "stylesheet", type_ "text/css", href_ "/bulma.css"]
    link_ [rel_ "stylesheet", type_ "text/css", href_ "/styles.css"]
    -- JS
    script_ [src_ "/highlight.js"] empty
    script_ "hljs.initHighlightingOnLoad()"
  body_ [class_ "Site"] $ do
    header_ [class_ "header"] $ do
      h1_ $ do
        i_ "Index of "
        a_ [href_ "/"] $ toHtml $ Text.pack (dName rootDir ++ "/")
    main_ [class_ "Site-content"] $ do
      renderToC . toc $ rootDir
      Maybe.fromMaybe mempty mContent
    footer_ ""

renderMarkdown :: String -> Html'
renderMarkdown =
  div_ [class_ "content"] . renderDoc . markdown def . Text.pack

-- -----------------------------------------------------------------------------

output :: Directory -> Directory -> IO ()
output rootDir (Directory _ path [] []) = outputIndex rootDir path []
output rootDir (Directory _ path dirs files) = do
  let outPath = Path.normalise (".martha" </> path)
  putStrLn $ "Building: " ++ path
  Dir.createDirectory outPath
  forM_ dirs (output rootDir)
  forM_ files (outputFile rootDir)
  outputIndex rootDir path files

outputFile :: Directory -> File -> IO ()
outputFile rootDir (File _ path _) = do
  content <- Just <$> renderMarkdown <$> readFile path
  Lucid.renderToFile
    (".martha" </> path <.> "html")
    (renderPath rootDir content)

outputIndex :: Directory -> FilePath -> [File] -> IO ()
outputIndex rootDir dirPath files = do
  mContent <-
    traverse
      (fmap renderMarkdown . readFile . fPath)
      (List.find isReadme files)
  Lucid.renderToFile
    (".martha" </> Path.takeDirectory dirPath </> "index.html")
    (renderPath rootDir mContent)

isReadme :: File -> Bool
isReadme (File "README.md" _ _) = True
isReadme (File "martha.md" _ _) = True
isReadme _ = False

-- -----------------------------------------------------------------------------

build :: Directory -> IO ()
build rootDir = do
  output rootDir rootDir
  -- static files
  writeFile (".martha" </> "highlight.js") $ Text.unpack Static.highlightJS
  writeFile (".martha" </> "highlight.css") $ Text.unpack Static.highlightCSS
  writeFile (".martha" </> "bulma.css") $ Text.unpack Static.bulmaCSS
  writeFile (".martha" </> "styles.css") $ Text.unpack Static.css

cleanupPreviousOutput :: IO ()
cleanupPreviousOutput = do
  hasReadmeDir <- Dir.doesPathExist ".martha"
  when hasReadmeDir $ Dir.removeDirectoryRecursive ".martha"

getRoot :: FilePath -> IO (FilePath, String)
getRoot path =
  (,)
    <$> Dir.makeRelativeToCurrentDirectory path
    <*> fmap Path.takeFileName Dir.getCurrentDirectory

runMartha :: FilePath -> IO ()
runMartha path = do
  Dir.setCurrentDirectory path
  (rootPath, rootName) <- getRoot path
  cleanupPreviousOutput
  putStrLn $ "Traversing \"" ++ rootName ++ "\" to find Markdown files"
  rootDir <- traverseDirectory rootPath rootName
  build rootDir

run :: IO ()
run = do
  path <- headOr "." <$> Env.getArgs
  runMartha path
