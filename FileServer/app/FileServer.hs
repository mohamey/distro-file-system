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
import Lib

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import qualified Data.List as DL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import Database.MongoDB
import Network.Wai.Handler.Warp
import Network.Wai
import Prelude ()
import Prelude.Compat as PC
import Servant
import System.Directory



api :: Proxy API
api = Proxy

server :: Server API
server = uploadNewFile
    :<|> deleteFile
    :<|> updateFile
    :<|> getFile
    :<|> listFiles

  where
    -- Upload a new file to the server
    uploadNewFile :: FileObject -> Handler ApiResponse
    uploadNewFile newFile = liftIO $ do
      let dirParts = TL.splitOn "/" $ TL.pack (path newFile)
      let dirTail = TL.unpack (TL.intercalate "/" (DL.init dirParts)) -- Get the directory for the new file
      let file = DL.last dirParts
      let directory = if '/' == (PC.head dirTail) then dirTail else "/" ++ dirTail
      let actualDirectory = "static-files" ++ directory
      let actualPath = actualDirectory ++ "/" ++ (TL.unpack file)
      putStrLn actualPath
      let fileDoc = FileIndex {fileName=(TL.unpack file), fileLocation=directory}
      doesFileExist actualPath >>=
        (\res -> if res then
            return ApiResponse {result=False, message="File already exists"}
          else
            createDirectoryIfMissing True actualDirectory >> -- Creates parent directories too
              TLIO.writeFile actualPath (fileContent newFile) >>
                -- Write new file to the database
                withMongoDbConnection (insertFile $ fileIndexToDoc fileDoc) >>
                  return ApiResponse {result=True, message="Success"})

    deleteFile :: ObjIdentifier -> Handler ApiResponse
    deleteFile specifiedFile = liftIO $ do
      -- Get the file name, specified path, and actual path
      let dirParts = TL.splitOn "/" $ TL.pack (filePath specifiedFile)
      let specFileName = DL.last dirParts
      let dir = TL.intercalate "/" (DL.init dirParts)
      let directory = if (TL.head dir) == '/' then "static-files" ++ (TL.unpack dir) else "static-files/" ++ (TL.unpack dir)
      let actualPath = directory ++ "/" ++ TL.unpack specFileName
      -- Create fileobject for mongodb
      let fi = FileIndex {fileName=(TL.unpack specFileName), fileLocation=directory}
      let mongoDoc = fileIndexToDoc fi
      doesFileExist actualPath >>= -- Check if file exists
        (\res -> if res then
          removeFile actualPath >> -- Remove the file
            withMongoDbConnection (deleteOne $ select mongoDoc "files") >>
              return ApiResponse {result=True, message="File successfully removed"}
         else
          return ApiResponse {result=False, message="File does not exist"})

    updateFile :: FileObject -> Handler ApiResponse
    updateFile specifiedFile = liftIO $ do
      putStrLn "Updating File"
      let actualPath = if (PC.head (path specifiedFile)) == '/' then "static-files" ++ (path specifiedFile) else "static-files/" ++ (path specifiedFile)
      putStrLn $ "Updating " ++ actualPath
      doesFileExist actualPath >>= -- Check if the file exists
        (\res -> if res then
            TLIO.writeFile actualPath (fileContent specifiedFile) >> -- Update the file
              return ApiResponse {result=True, message="File successfully updated"}
        else
            return ApiResponse {result=False, message="File does not exist"})

    getFile :: Maybe String -> Handler FileObject
    getFile mp = case mp of
      Nothing -> liftIO $ do
        putStrLn "Path not specified"
        return FileObject {path="", fileContent="Path not specified"}
      Just p -> liftIO $ do
        putStrLn "Getting file: "
        let actualPath = "static-files" ++ p
        doesFileExist actualPath >>=
          (\res -> if res then
              TLIO.readFile actualPath >>=
                (\txt -> return FileObject{path=p, fileContent=txt})
          else
              return FileObject{path="", fileContent="File Not Found"})

    listFiles :: Handler [ObjIdentifier]
    listFiles = liftIO $ do
      withMongoDbConnection $ do
        docs <- find (select [] "files") >>= drainCursor
        return (docToObjs docs)

app :: Application
app = serve api server

runServer :: Int -> IO ()
runServer port = run port app

-- MongoDB Stuffs
withMongoDbConnection :: Action IO a -> IO a
withMongoDbConnection act = do
  let ip = "127.0.0.1"
  let database = "FILEDB"
  pipe <- connect (host ip)
  ret <- runResourceT $ liftIO $ access pipe master (T.pack database) act
  close pipe
  return ret

