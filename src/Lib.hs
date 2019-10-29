module Lib where

import qualified Data.Aeson as Aeson
import qualified Data.Maybe as Maybe
import GHC.Generics (Generic)
import qualified System.Directory as Dir
import qualified Zero.Server as Server

-- Server.Method
-- Server.Handler
--
-- Server.FromJSON
-- Server.ToJSON
--
-- Server.Request
-- Server.Response
-- Server.StatefulHandler
--
-- Server.decodeJson
--
-- Server.startServer
--
-- Server.effectfulHandler
-- Server.simpleHandler
-- Server.statefulHandler
-- Server.handlersWithState
--
-- Server.failureResponse
-- Server.stringResponse
-- Server.jsonResponse
--
-- Server.requestBody
-- Server.requestParameter
-- Server.requestParams

data Item
  = Directory {path :: FilePath, files :: [FilePath]}
  | File {path :: FilePath}
  deriving (Show, Eq, Ord, Generic, Aeson.ToJSON, Aeson.FromJSON)

-- data DirsBody
--   = DirsBody
--       {path :: Maybe FilePath}
--   deriving (Show, Eq, Generic, Aeson.ToJSON, Aeson.FromJSON)

-- HANDLERS ---

helloHandler :: [Item] -> Server.Request -> Server.Response
helloHandler dirContent _ =
  Server.jsonResponse dirContent

toItem :: FilePath -> IO Item
toItem p = toItem' p <$> Dir.doesDirectoryExist p
  where
    toItem' p' True = Directory p' [] -- TODO: ???
    toItem' p' False = File p'

-- sequence :: (Traversable t, Monad m) => t (m a) -> m (t a)
--
-- traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)

-- SERVER -----

run :: IO ()
run = do
  allContents <- Dir.getDirectoryContents "."
  dirContent <- sequence $ fmap toItem allContents
  Server.startServer
    [Server.simpleHandler Server.GET "/hello" (helloHandler dirContent)]
