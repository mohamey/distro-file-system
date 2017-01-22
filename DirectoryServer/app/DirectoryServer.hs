{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module DirectoryServer where

import Lib

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import qualified Data.Text as T
import Database.MongoDB
import Network.Wai.Handler.Warp
import Network.Wai
import Prelude ()
import Prelude.Compat as PC
import Servant

api :: Proxy API
api = Proxy

server :: Server API
server = uploadFileIndexes
    :<|> updateFileIndex
    :<|> resolveFileID
    :<|> listFiles

  where
    -- Upload new file indexes to directory server
    uploadFileIndexes :: [DirectoryDesc] -> Handler ApiResponse
    uploadFileIndexes files = do
      -- Convert each index to document
      let docs = map fileIndexToDoc files
      -- Insert each document into DB using insertAll
      liftIO $ withMongoDbConnection (insertAll_ "files" docs )
      -- Send back response
      return ApiResponse {result=True, message="Successfully added indexes to DB"}

    updateFileIndex :: UpdateObject -> Handler ApiResponse
    updateFileIndex updateObj = do
      let oldFileDoc = fileIndexToDoc $ old updateObj
      let newFileDoc = fileIndexToDoc $ new updateObj

      -- Replace the old file index with the new one
      liftIO $ withMongoDbConnection $ replace (select oldFileDoc "files") newFileDoc

      -- Send back response
      return ApiResponse {result=True, message="Successfully modified file"}

    resolveFileID :: String -> Handler (Either ApiResponse DirectoryDesc)
    resolveFileID fileID = do
      -- Build a query using file id
      let objID = read fileID :: ObjectId
      let query = select ["_id" =: objID] "files"
      res <- liftIO $ withMongoDbConnection $ findOne query
      -- Send back either the file index or an api response
      case res of
        Just fi -> return $ Right $docToFileIndex fi
        Nothing -> return $ Left ApiResponse {result=False, message="File not found by ID"}

    listFiles :: Handler [FileSummary]
    listFiles = do
      -- Get all files listed in database
      let query = select [] "files"
      liftIO $ do
        withMongoDbConnection $ do
          docs <- find query >>= drainCursor
          let fileIndexes = map docToFileIndex docs
          let fileSummaries = map fileIndexToSummary fileIndexes
          return fileSummaries

-- MongoDB Stuffs
withMongoDbConnection :: Action IO a -> IO a
withMongoDbConnection act = do
  let ip = "127.0.0.1"
  let database = "DIRECTORYDB"
  pipe <- connect (host ip)
  ret <- runResourceT $ liftIO $ access pipe master (T.pack database) act
  close pipe
  return ret

app :: Application
app = serve api server

runServer :: Int -> IO ()
runServer portNo = do
  liftIO $ putStrLn $ "Server running on 127.0.0.1:" ++ (show portNo)
  run portNo app
