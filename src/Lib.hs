{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.List as List
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Lucid
import qualified Network.Wai.Handler.Warp as Warp
import Servant
import Servant.HTML.Lucid
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
directoryHandler directory = do
  basePath <- liftIO $ Dir.makeRelativeToCurrentDirectory directory
  parent <- liftIO $ Path.takeFileName <$> Dir.getCurrentDirectory
  let withParent = parent </> basePath
  allContents <- liftIO $ Dir.getDirectoryContents directory
  dirContent <- liftIO $ traverse (toItem basePath) allContents
  let crumbs = breadcrumbs ("/", parent) basePath
  pure $ renderPath withParent crumbs dirContent

rootHandler :: Handler (Html ())
rootHandler = directoryHandler "."

pathHandler :: [String] -> Handler (Html ())
pathHandler [] = rootHandler
pathHandler paths = directoryHandler $ List.intercalate "/" paths

-- -----------------------------------------------------------------------------

renderPath :: FilePath -> [Breadcrumb] -> [Item] -> Html ()
renderPath dir crumbs contents = html_ $ do
  head_ $ do
    title_ $ toHtml $ "Files within " ++ dir
    -- TODO: CSS !
    style_ "body{margin:0;padding:30px;background:#fff;font-family:-apple-system,BlinkMacSystemFont,\"Segoe UI\",Roboto,Oxygen,Ubuntu,Cantarell,\"Fira Sans\",\"Droid Sans\",\"Helvetica Neue\",sans-serif;-webkit-font-smoothing:antialiased}main{max-width:920px}header{display:flex;justify-content:space-between;flex-wrap:wrap}h1{font-size:18px;font-weight:500;margin-top:0;color:#000}header h1 a{font-size:18px;font-weight:500;margin-top:0;margin-right:3px;color:#000}h1 i{font-style:normal}ul{margin:0 0 0 -2px;padding:20px 0 0 0}ul li{list-style:none;font-size:14px;display:flex;justify-content:space-between}a{text-decoration:none}ul a{color:#000;padding:10px 5px;margin:0 -5px;white-space:nowrap;overflow:hidden;display:block;width:100%;text-overflow:ellipsis}header a{color:#0076ff;font-size:11px;font-weight:400;display:inline-block;line-height:20px}svg{height:13px;vertical-align:text-bottom}ul a::before{display:inline-block;vertical-align:middle;margin-right:10px;width:24px;text-align:center;line-height:12px}ul a.file::before{content:url(\"data:image/svg+xml;utf8,<svg width='15' height='19' fill='none' xmlns='http://www.w3.org/2000/svg'><path d='M10 8C8.34 8 7 6.66 7 5V1H3c-1.1 0-2 .9-2 2v13c0 1.1.9 2 2 2h9c1.1 0 2-.9 2-2V8h-4zM8 5c0 1.1.9 2 2 2h3.59L8 1.41V5zM3 0h5l7 7v9c0 1.66-1.34 3-3 3H3c-1.66 0-3-1.34-3-3V3c0-1.66 1.34-3 3-3z' fill='black'/></svg>\")}ul a:hover{text-decoration:underline}ul a.folder::before{content:url(\"data:image/svg+xml;utf8,<svg width='20' height='16' fill='none' xmlns='http://www.w3.org/2000/svg'><path d='M18.784 3.87a1.565 1.565 0 0 0-.565-.356V2.426c0-.648-.523-1.171-1.15-1.171H8.996L7.908.25A.89.89 0 0 0 7.302 0H2.094C1.445 0 .944.523.944 1.171v2.3c-.21.085-.398.21-.565.356a1.348 1.348 0 0 0-.377 1.004l.398 9.83C.42 15.393 1.048 16 1.8 16h15.583c.753 0 1.36-.586 1.4-1.339l.398-9.83c.021-.313-.125-.69-.397-.962zM1.843 3.41V1.191c0-.146.104-.272.25-.272H7.26l1.234 1.088c.083.042.167.104.293.104h8.282c.125 0 .25.126.25.272V3.41H1.844zm15.54 11.712H1.78a.47.47 0 0 1-.481-.46l-.397-9.83c0-.147.041-.252.125-.356a.504.504 0 0 1 .377-.147H17.78c.125 0 .272.063.377.147.083.083.125.209.125.334l-.418 9.83c-.021.272-.23.482-.481.482z' fill='black'/></svg>\")}ul a.lambda::before{content:url(\"data:image/svg+xml; utf8,<svg width='15' height='19' fill='none' xmlns='http://www.w3.org/2000/svg'><path d='M3.5 14.4354H5.31622L7.30541 9.81311H7.43514L8.65315 13.0797C9.05676 14.1643 9.55405 14.5 10.7 14.5C11.0171 14.5 11.291 14.4677 11.5 14.4032V13.1572C11.3847 13.1766 11.2622 13.2024 11.1541 13.2024C10.6351 13.2024 10.3829 13.0281 10.1595 12.4664L8.02613 7.07586C7.21171 5.01646 6.54865 4.5 5.11441 4.5C4.83333 4.5 4.62432 4.53228 4.37207 4.59038V5.83635C4.56667 5.81052 4.66036 5.79761 4.77568 5.79761C5.64775 5.79761 5.9 6.0042 6.4045 7.19852L6.64234 7.77954L3.5 14.4354Z' fill='black'/><rect x='0.5' y='0.5' width='14' height='18' rx='2.5' stroke='black'/></svg>\")}ul a.file.gif::before,ul a.file.jpg::before,ul a.file.png::before,ul a.file.svg::before{content:url(\"data:image/svg+xml;utf8,<svg width='16' height='16' viewBox='0 0 80 80' xmlns='http://www.w3.org/2000/svg' fill='none' stroke='black' stroke-width='5' stroke-linecap='round' stroke-linejoin='round'><rect x='6' y='6' width='68' height='68' rx='5' ry='5'/><circle cx='24' cy='24' r='8'/><path d='M73 49L59 34 37 52m16 20L27 42 7 58'/></svg>\")}::selection{background-color:#79ffe1;color:#000}::-moz-selection{background-color:#79ffe1;color:#000}@media (min-width:768px){ul{display:flex;flex-wrap:wrap}ul li{width:230px;padding-right:20px}}@media (min-width:992px){body{padding:45px}h1,header h1 a{font-size:15px}ul li{font-size:13px;box-sizing:border-box;justify-content:flex-start}}"
    link_ [rel_ "icon", href_ "data:,"]
  body_ $ main_ $ do
    header_ $ do
      h1_ $ do
        i_ "Index of "
        mapM_ renderBreadcrumb crumbs
    ul_ [id_ "files"] $
      mapM_ renderFileItem contents

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
renderFileItem (File name _ extension) =
  li_
    $ a_
      [class_ $ Text.pack $ "file " ++ extension]
    -- TODO add when files are supported
    -- [class_ $ Text.pack $ "file " ++ extension, href_ $ Text.pack $ makeAbsolute url]
    $ toHtml name

renderBreadcrumb :: (FilePath, FilePath) -> Html ()
renderBreadcrumb (url, name) =
  a_
    [href_ $ Text.pack $ makeAbsolute url]
    $ toHtml
    $ Text.pack
    $ makeAbsolute name

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

-- -----------------------------------------------------------------------------

data Item
  = Directory -- '.' and '..' are directories any other is Folder
      { dirName :: FilePath,
        dirUrl :: FilePath
      }
  | Folder
      {folderName :: FilePath, folderUrl :: FilePath}
  | File
      { fileName :: FilePath,
        fileUrl :: FilePath,
        fileExtension :: String
      }
  deriving (Show, Eq, Ord, Generic, Aeson.ToJSON, Aeson.FromJSON)

-- -----------------------------------------------------------------------------

toItem :: FilePath -> FilePath -> IO Item
toItem basePath name = do
  canonical <- Dir.canonicalizePath $ basePath </> name
  url <- Dir.makeRelativeToCurrentDirectory canonical
  isDir <- Dir.doesDirectoryExist url
  pure $
    if isDir
      then toItem' name url Nothing
      else toItem' name url $ Just $ Path.takeExtension name

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

-- -----------------------------------------------------------------------------

run :: IO ()
run = do
  path <- headOr "." <$> Env.getArgs
  Dir.setCurrentDirectory path
  putStrLn "Serving http://localhost:7000"
  serverMain 7000
