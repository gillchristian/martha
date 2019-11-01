{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import Cheapskate (def, markdown)
import Cheapskate.Lucid (renderDoc)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Lucid
import qualified Network.Wai.Handler.Warp as Warp
import Servant
import Servant.HTML.Lucid
import qualified Static
import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified System.FilePath as Path
import System.FilePath ((</>))

serverMain :: Int -> IO ()
serverMain port = Warp.run port app

app :: Application
app = serve readmeAPI readmeServer

-- GET /
-- GET /*
type ReadmeAPI =
  Get '[HTML] (Html ()) :<|> CaptureAll "path" String :> Get '[HTML] (Html ())

readmeAPI :: Proxy ReadmeAPI
readmeAPI = Proxy

readmeServer :: Server ReadmeAPI
readmeServer = rootHandler :<|> pathHandler

-- -----------------------------------------------------------------------------

directoryHandler :: FilePath -> Handler (Html ())
directoryHandler givenPath = do
  isDir <- liftIO $ Dir.doesDirectoryExist givenPath
  let directory = applyWhen (not isDir) Path.takeDirectory givenPath
  basePath <- liftIO $ Dir.makeRelativeToCurrentDirectory directory
  root <- liftIO $ Path.takeFileName <$> Dir.getCurrentDirectory
  allContents <- liftIO $ filter (/= ".") <$> Dir.getDirectoryContents directory
  dirContent <- liftIO $ List.sort <$> traverse (toItem basePath) allContents
  let crumbs = breadcrumbs ("/", root) basePath
  let withLayout = renderPath (root </> basePath) crumbs dirContent
  case whatToRender isDir givenPath dirContent of
    (RenderMarkdown path) ->
      liftIO $ withLayout <$> Just <$> renderMarkdown <$> readFile path
    (RenderMonospace path) ->
      liftIO $
        withLayout <$> Just <$> (renderMonospace extension) <$> readFile path
      where
        extension = takeExtension path
    (RenderHtml path) -> liftIO $ renderHtml <$> readFile path
    RenderDirectory -> pure $ withLayout Nothing

rootHandler :: Handler (Html ())
rootHandler = directoryHandler "."

pathHandler :: [String] -> Handler (Html ())
pathHandler [] = rootHandler
pathHandler paths = directoryHandler $ List.intercalate "/" paths

-- -----------------------------------------------------------------------------

renderPath :: FilePath -> [Breadcrumb] -> [Item] -> Maybe (Html ()) -> Html ()
renderPath dir crumbs contents fileContent = html_ $ do
  head_ $ do
    link_ [rel_ "icon", href_ "data:,"]
    title_ $ toHtml $ "Files within " ++ dir
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    -- CSS
    style_ Static.highlightCSS
    style_ Static.bulmaCSS
    style_ Static.css
    -- JS
    script_ Static.highlightJS
    script_ "hljs.initHighlightingOnLoad()"
  body_ $ main_ $ do
    div_ [class_ "explorer"] $ do
      header_ $ do
        h1_ $ do
          i_ "Index of "
          mapM_ renderBreadcrumb crumbs
      ul_ $ mapM_ renderFileItem contents
    Maybe.fromMaybe mempty fileContent

renderFileItem :: Item -> Html ()
renderFileItem (Directory name url) =
  li_
    $ a_
      [class_ "directory", href_ $ Text.pack $ makeAbsolute url]
    $ toHtml
    $ Text.pack name
renderFileItem (Folder name url) =
  li_
    $ a_
      [class_ "folder", href_ $ Text.pack $ makeAbsolute url]
    $ toHtml
    $ Text.pack
    $ Path.addTrailingPathSeparator name
renderFileItem (File name url extension) =
  li_
    $ a_
      [ class_ $ Text.pack $ "file " ++ extension,
        href_ $ Text.pack $ makeAbsolute url
      ]
    $ toHtml name

renderBreadcrumb :: (FilePath, FilePath) -> Html ()
renderBreadcrumb (url, name) =
  a_
    [href_ $ Text.pack $ makeAbsolute url]
    $ toHtml
    $ Text.pack
    $ Path.addTrailingPathSeparator name

renderMarkdown :: String -> Html ()
renderMarkdown = div_ [class_ "content"] . renderDoc . markdown def . Text.pack

renderMonospace :: Extension -> String -> Html ()
renderMonospace "" =
  div_ [class_ "monospace"] . pre_ . code_ [class_ "text"] . toHtml . Text.pack
renderMonospace "txt" =
  div_ [class_ "monospace"] . pre_ . code_ [class_ "text"] . toHtml . Text.pack
renderMonospace ext =
  div_ [class_ "monospace"]
    . pre_
    . code_ [class_ $ Text.pack ext]
    . toHtml
    . Text.pack

renderHtml :: String -> Html ()
renderHtml = toHtmlRaw

-- -----------------------------------------------------------------------------

type Breadcrumb = (FilePath, FilePath)

breadcrumbs :: Breadcrumb -> FilePath -> [Breadcrumb]
breadcrumbs root baseDir = root : go baseDir []
  where
    go :: FilePath -> [(FilePath, FilePath)] -> [(FilePath, FilePath)]
    go "." acc = acc
    go "/" acc = acc
    go base acc = go nextDir $ (base, file) : acc
      where
        (dir, file) = Path.splitFileName base
        nextDir = (Path.dropTrailingPathSeparator dir)

makeAbsolute :: FilePath -> FilePath
makeAbsolute path
  | Path.isAbsolute path = path
  | otherwise = Path.pathSeparator : path

whatToRender :: Bool -> FilePath -> [Item] -> ToRender FilePath
whatToRender isDir givenPath dirContent =
  pathToRender $ (,) <$> toRead <*> extension
  where
    toRead = fileOrReadme isDir givenPath dirContent
    extension = fmap takeExtension toRead

type Extension = String

pathToRender :: Maybe (FilePath, Extension) -> ToRender FilePath
pathToRender (Just (path, "md")) = RenderMarkdown path
pathToRender (Just (path, "html")) = RenderHtml path
pathToRender (Just (path, _)) = RenderMonospace path
pathToRender Nothing = RenderDirectory

fileOrReadme :: Bool -> FilePath -> [Item] -> Maybe FilePath
fileOrReadme True givenPath dirContent =
  fmap (givenPath </>) $ findReadme dirContent
fileOrReadme False givenPath _ = Just givenPath

findReadme :: [Item] -> Maybe FilePath
findReadme contents
  | null files = Nothing
  | List.elem "README.md" files = Just "README.md"
  | List.elem "readme.md" files = Just "readme.md"
  | List.elem "README.txt" files = Just "README.txt"
  | List.elem "readme.txt" files = Just "readme.txt"
  | List.elem "README" files = Just "README"
  | List.elem "readme" files = Just "readme"
  | otherwise = Nothing
  where
    files = map iName $ filter isFile contents

-- -----------------------------------------------------------------------------

data Item
  = Directory -- '.' and '..' are directories any other is Folder
      { iName :: FilePath,
        iUrl :: FilePath
      }
  | Folder
      {iName :: FilePath, iUrl :: FilePath}
  | File
      { iName :: FilePath,
        iUrl :: FilePath,
        iExtension :: String
      }
  deriving (Show, Eq, Ord, Generic, Aeson.ToJSON, Aeson.FromJSON)

isFile :: Item -> Bool
isFile (File _ _ _) = True
isFile _ = False

data ToRender a
  = RenderMarkdown a
  | RenderHtml a
  | RenderMonospace a
  | RenderDirectory
  deriving (Show, Eq)

takeExtension :: FilePath -> Extension
takeExtension path =
  case Path.takeExtension path of
    ('.' : extension) -> extension
    extension -> extension

-- -----------------------------------------------------------------------------

toItem :: FilePath -> FilePath -> IO Item
toItem basePath name = do
  canonical <- Dir.canonicalizePath $ basePath </> name
  url <- Dir.makeRelativeToCurrentDirectory canonical
  isDir <- Dir.doesDirectoryExist url
  pure $
    if isDir
      then toItem' name url Nothing
      else toItem' name url $ Just (Path.takeExtension name)

toItem' :: FilePath -> FilePath -> Maybe String -> Item
toItem' ".." url _ = Directory ".." url
toItem' "." url _ = Directory "." url
toItem' name url Nothing = Folder name url
toItem' name url (Just ('.' : extension)) = File name url extension
toItem' name url (Just extension) = File name url extension

-- -----------------------------------------------------------------------------

headOr :: a -> [a] -> a
headOr a [] = a
headOr _ (a : _) = a

applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen True f a = f a
applyWhen False _ a = a

-- -----------------------------------------------------------------------------

run :: IO ()
run = do
  path <- headOr "." <$> Env.getArgs
  Dir.setCurrentDirectory path
  putStrLn "Serving http://localhost:7000"
  serverMain 7000
