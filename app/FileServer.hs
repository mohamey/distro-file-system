{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module FileServer where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Bson
import Data.List
import Data.List.Split
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.ByteString as B
import Database.MongoDB
import GHC.Generics
import Network.HTTP.Types.Status
import Network.Wai.Handler.Warp
import Network.Wai
import Prelude ()
import Prelude.Compat 
import Servant
import System.Directory

-- A representation of files on the server
-- This will be stored in the mongodb database
data FileObject = FileObject {
  path :: String,
  fileContent :: TL.Text
} deriving Generic

instance FromJSON FileObject
instance ToJSON FileObject

-- This will be returned on a post request
data ApiResponse = ApiResponse {
  result :: Bool,
  message :: String
} deriving Generic

instance FromJSON ApiResponse
instance ToJSON ApiResponse

-- Handle deleting files from the fileserver
data DeleteObject = DeleteObject {
  filePath :: String
} deriving Generic

instance FromJSON DeleteObject
instance ToJSON DeleteObject

-- The API Definition
type API = "upload" :> ReqBody '[JSON] FileObject :> Post '[JSON] ApiResponse
         :<|> "remove" :> ReqBody '[JSON] DeleteObject :> Delete '[JSON] ApiResponse
         :<|> "files" :> Raw

api :: Proxy API
api = Proxy

server :: Server API
server = uploadNewFile
    :<|> deleteFile
    :<|> serveDirectory "static-files" -- Serve files from the static-files directory for get requests

  where
    -- Upload a new file to the server
    uploadNewFile :: FileObject -> Handler ApiResponse
    uploadNewFile newFile = liftIO $ do
      let dirParts = splitOn "/" (path newFile)
      let directory = "static-files" ++ (intercalate "/" (init dirParts)) -- Get the directory for the new file
      createDirectoryIfMissing True directory -- Creates parent directories too
      TLIO.writeFile ("static-files" ++ path newFile) (fileContent newFile)
      return ApiResponse {result=True, message="Success"}

    deleteFile :: DeleteObject -> Handler ApiResponse
    deleteFile specifiedFile = liftIO $ do
      putStrLn "Deleting file"
      let actualPath = "static-files" ++ filePath specifiedFile
      putStrLn actualPath
      doesFileExist actualPath >>=
        (\res -> if res then
          removeFile actualPath >>
            return ApiResponse {result=True, message="File successfully removed"}
         else
          return ApiResponse {result=False, message="File does not exist"})

app :: Application
app = serve api server

runServer :: Int -> IO ()
runServer port = run port app
