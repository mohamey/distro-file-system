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
import Data.Proxy as DP
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import Database.MongoDB
import qualified Network.HTTP.Client as HPC
import Network.Wai.Handler.Warp
import Network.Wai
import Prelude ()
import Prelude.Compat as PC
import Servant
import Servant.Client
import System.Directory

api :: Proxy API
api = Proxy

server :: Int -> BaseUrl -> Server API
server pn adr = uploadNewFile
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
      fileExists <- doesFileExist actualPath
      case fileExists of
        True -> return ApiResponse {result=False, message="File already exists"}
        False -> do
            createDirectoryIfMissing True actualDirectory -- Creates parent directories too
            TLIO.writeFile actualPath (fileContent newFile)
            -- Write new file to the database
            stringId <- withMongoDbConnection (insert "files" $ fileIndexToDoc fileDoc)
            -- -- Convert FileIndex to [DirectoryDesc]
            let dd = DirectoryDesc {
                  dbID = show stringId,
                  fName = show file,
                  fLocation = directory,
                  fileServer = "127.0.0.1",
                  port = pn
            }
            -- Send dd to directory server
            manager <- liftIO $ HPC.newManager HPC.defaultManagerSettings -- Get a HTTP manager
            response <- liftIO $ runClientM (postRequest [dd]) (ClientEnv manager adr)
            case response of
              Left err -> do
                putStrLn $ "Error posting new file to directory server: \n" ++ show err
                return ApiResponse {result=False, message="Failed to post to directory server"}
              Right res -> do
                case (result res) of
                  True -> do
                    putStrLn "Successfully added file to directory server"
                    return ApiResponse {result=True, message="Success"}
                  False -> do
                    putStrLn "Failed to add file to directory server"
                    return ApiResponse {result=False, message="Failed to update directory server"}

    deleteFile :: ObjIdentifier -> Handler ApiResponse
    deleteFile specifiedFile = liftIO $ do
      -- Get the file name, specified path, and actual path
      let dirParts = TL.splitOn "/" $ TL.pack (filePath specifiedFile)
      let specFileName = DL.last dirParts
      let dir = TL.intercalate "/" (DL.init dirParts)
      let directory = if (TL.head dir) == '/' then "static-files" ++ (TL.unpack dir) else "static-files/" ++ (TL.unpack dir)
      let actualPath = directory ++ "/" ++ TL.unpack specFileName
      -- Create fileobject for mongodb
      let fi = FileIndex {fileName=(TL.unpack specFileName), fileLocation=(TL.unpack dir)}
      let mongoDoc = fileIndexToDoc fi
      fileExists <- doesFileExist actualPath
      case fileExists of
        False -> do return ApiResponse {result=False, message="File not found on fileserver"}
        True -> do
          maybeDoc <- withMongoDbConnection (findOne $ select mongoDoc "files")
          case maybeDoc of
            Nothing -> do
              putStrLn $ "Specified file not found:\n" ++ show mongoDoc
              return ApiResponse{result=False, message="File not found"}
            Just doc -> do
              withMongoDbConnection (deleteOne $ select mongoDoc "files")
              -- Send delete request to directory server
              let dd = docToDirDesc "127.0.0.1" pn doc
              -- Send delete request to directory server
              manager <- liftIO $ HPC.newManager HPC.defaultManagerSettings -- Get a HTTP manager
              response <- liftIO $ runClientM (deleteRequest dd) (ClientEnv manager adr)
              case response of
                Left err -> do
                  putStrLn $ "An error occurred deleting the file listing from the directory server:\n" ++ show err
                  return ApiResponse{result=False, message="An error occurred deleting file from directory server"}
                Right res -> do
                  removeFile actualPath
                  return res

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

app :: Int -> BaseUrl -> Application
app pn adr = serve api (server pn adr)

-- First send existing file list to directory server
runServer :: Int -> String -> Int -> IO ()
runServer portNo dirServerAddress dirServerPort = do
  liftIO $ withMongoDbConnection $ do
    -- Send fileserver details to Directory server
    manager <- liftIO $ HPC.newManager HPC.defaultManagerSettings -- Get a HTTP manager
    let fs = FileServer {address="127.0.0.1", portNum=portNo}
    resp <- liftIO $ runClientM (addRequest fs) (ClientEnv manager (url dirServerAddress dirServerPort))
    case resp of
      Left err -> liftIO $ putStrLn $ "Error adding file server to directory server: \n" ++ show err
      Right res -> liftIO $ putStrLn $ show (message res)
    docs <- find (select [] "files") >>= drainCursor -- Get all entries in database
    let dds = map (docToDirDesc "127.0.0.1" portNo) docs -- Convert the documents to DirectoryDesc
    -- Send list to directory server
    response <- liftIO $ runClientM (postRequest dds) (ClientEnv manager (url dirServerAddress dirServerPort))
    case response of
      Left x -> liftIO $ putStrLn $ "Error uploading list of files to directory server:\n" ++ show x
      Right res -> case result res of
        True -> liftIO $ putStrLn "Successfully uploaded paths to directory server"
        _ -> liftIO $ putStrLn "Failed to upload paths to directory server"
    liftIO $ run portNo (app portNo (url dirServerAddress dirServerPort))

-- MongoDB Stuffs
withMongoDbConnection :: Action IO a -> IO a
withMongoDbConnection act = do
  let ip = "127.0.0.1"
  let database = "FILEDB"
  pipe <- connect (host ip)
  ret <- runResourceT $ liftIO $ access pipe master (T.pack database) act
  close pipe
  return ret

url :: String -> Int -> BaseUrl
url s p = BaseUrl Http s p ""

upload :: [DirectoryDesc] -> ClientM ApiResponse
update :: UpdateObject -> ClientM ApiResponse
add :: FileServer -> ClientM ApiResponse
deleteDD :: DirectoryDesc -> ClientM ApiResponse

dsapi :: DP.Proxy DSAPI
dsapi = DP.Proxy

upload :<|> update :<|> add :<|> deleteDD = client dsapi

postRequest :: [DirectoryDesc] -> ClientM ApiResponse
postRequest postList = do
  res <- upload postList
  return res

addRequest :: FileServer -> ClientM ApiResponse
addRequest fs = do
  res <- add fs
  return res

deleteRequest :: DirectoryDesc -> ClientM ApiResponse
deleteRequest dd = do
  res <- deleteDD dd
  return res
