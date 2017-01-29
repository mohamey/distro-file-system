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
              :<|> deleteSecondary
              :<|> updateFile
              :<|> closeFile
              :<|> openFile
              :<|> getFile
              :<|> listFiles

  where
    -- Upload a new file to the server
    uploadNewFile :: FileObject -> Handler ApiResponse
    uploadNewFile newFile = liftIO $ do
      let (fpath, fname) = splitFullPath (TL.pack $ path newFile)
      let directory = "/" ++ (TL.unpack fpath)
      let actualDirectory = "static-files/" ++ (TL.unpack fpath)
      let actualPath = actualDirectory ++ "/" ++ (TL.unpack fname)
      putStrLn actualPath
      let time = modifiedLast newFile
      let fileDoc = FileIndex {fileName=(TL.unpack fname), fileLocation=directory, lastModified=time}
      fileExists <- doesFileExist actualPath
      docExists <- withMongoDbConnection $ findOne $ select ["name"=:TL.toStrict fname, "path"=:T.pack directory] "files"
      -- Only if the file is not present in the file system and there is no database listing
      case (fileExists == False) && (docExists == Nothing) of
        False -> return ApiResponse {result=False, message="File already exists"}
        True -> do
            createDirectoryIfMissing True actualDirectory -- Creates parent directories too
            TLIO.writeFile actualPath (fileContent newFile)
            -- Write new file to the database
            stringId <- withMongoDbConnection (insert "files" $ fileIndexToDoc fileDoc)
            -- -- Convert FileIndex to [DirectoryDesc]
            let dd = DirectoryDesc {
                  dbID = show stringId,
                  fName = TL.unpack fname,
                  fLocation = directory,
                  fileServer = "127.0.0.1",
                  port = pn,
                  modified = time
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
      putStrLn "Function called to delete primary file copy"
      let (fpath, fname) = splitFullPath (TL.pack $ filePath specifiedFile)
      let directory = if (TL.head fpath) == '/' then "static-files" ++ (TL.unpack fpath) else "static-files/" ++ (TL.unpack fpath)
      let actualPath = directory ++ "/" ++ TL.unpack fname
      -- Create fileobject for mongodb
      let mongoDoc = ["name" =: (TL.toStrict fname), "path" =: (TL.toStrict fpath)]
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
              -- Delete secondary copy of files
              liftIO $ putStrLn "Getting secondary copies of file"
              manager <- liftIO $ HPC.newManager HPC.defaultManagerSettings -- Get a HTTP manager
              secondaries <- liftIO $ runClientM (getSecondariesReq (filePath specifiedFile)) (ClientEnv manager adr)
              case secondaries of
                Left err -> do
                  liftIO $ putStrLn $ show err
                  return ApiResponse {result=False, message="Fileserver failed to get a list of secondary file locations"}
                Right eitherRes -> do
                  case eitherRes of
                    Left x -> return x
                    Right dds -> do
                      -- Delete the secondaries
                      let addresses = map (\ x -> url (fileServer x) (port x)) dds
                      -- Mass Delete fileservers
                      putStrLn "Deleting Secondaries"
                      putStrLn $ show $ addresses
                      massRunClient addresses (deleteFsReq specifiedFile) manager
                      -- Since transactions have not yet been implemented, there is no guarantee all secondary copies will be deleted and no clean way of ensuring they are deleted in an all or nothing approach
                      putStrLn "Deleting local primary copy"
                      withMongoDbConnection (deleteOne $ select mongoDoc "files")
                      -- Send delete request to directory server
                      let dd = docToDirDesc "127.0.0.1" pn doc
                      -- Send delete request to directory server
                      response <- liftIO $ runClientM (deleteRequest dd) (ClientEnv manager adr)
                      case response of
                        Left err -> do
                          putStrLn $ "An error occurred deleting the file listing from the directory server:\n" ++ show err
                          return ApiResponse{result=False, message="An error occurred deleting file from directory server"}
                        Right res -> do
                          removeFile actualPath
                          return res

    deleteSecondary :: ObjIdentifier -> Handler ApiResponse
    deleteSecondary specifiedFile = liftIO $ do
  -- Get the file name, specified path, and actual path
      putStrLn "Function called to delete local secondary copy of file"
      let (path, fname) = splitFullPath (TL.pack $ filePath specifiedFile)
      let directory = if (TL.head path) == '/' then "static-files" ++ (TL.unpack path) else "static-files/" ++ (TL.unpack path)
      let actualPath = directory ++ "/" ++ TL.unpack fname
      fileExists <- doesFileExist actualPath
      putStrLn "Deleting secondary"
      case fileExists of
        False -> do return ApiResponse {result=False, message="File not found on fileserver"}
        True -> do
          let mongoDoc = ["name" =: (TL.toStrict fname), "path" =: (TL.toStrict path)]
          maybeDoc <- withMongoDbConnection (findOne $ select mongoDoc "files")
          case maybeDoc of
            Nothing -> do
              putStrLn $ "Specified document not found\n"
              return ApiResponse{result=False, message="File not found"}
            Just doc -> do
              -- Send delete request to directory server
              let dd = docToDirDesc "127.0.0.1" pn doc
              putStrLn "Sending delete request to directory server"
              manager <- liftIO $ HPC.newManager HPC.defaultManagerSettings -- Get a HTTP manager
              response <- liftIO $ runClientM (deleteRequest dd) (ClientEnv manager adr)
              case response of
                Left err -> do
                  putStrLn $ "An error occurred deleting the file listing from the directory server:\n" ++ show err
                  return ApiResponse{result=False, message="An error occurred deleting file from directory server"}
                Right res -> do
                  liftIO $ putStrLn "Local copy of secondary file found"
                  withMongoDbConnection (deleteOne $ select mongoDoc "files")
                  liftIO $ putStrLn "Local secondary removed"
                  removeFile actualPath
                  return res

    updateFile :: FileObject -> Handler ApiResponse
    updateFile specifiedFile = liftIO $ do
      putStrLn "Updating file"
      let actualPath = if (PC.head (path specifiedFile)) == '/' then "static-files" ++ (path specifiedFile) else "static-files/" ++ (path specifiedFile)
      putStrLn $ "Updating " ++ actualPath
      fileExists <- doesFileExist actualPath
      case fileExists of
        False -> do
          return ApiResponse {result=False, message="File does not exist"}
        True -> do
          TLIO.writeFile actualPath (fileContent specifiedFile)
          let (fname, fpath) = splitFullPath (TL.pack $ path specifiedFile)
          let fIndex = FileIndex {fileName=(TL.unpack fname), fileLocation=(TL.unpack fpath), lastModified=(modifiedLast specifiedFile)}
          withMongoDbConnection $ replace (select ["name" =: (TL.toStrict fname), "path" =: (TL.toStrict fpath)] "files") (fileIndexToDoc fIndex)
          return ApiResponse {result=True, message="File successfully updated"}

    getFile :: Maybe String -> Handler FileRequest
    getFile mp = case mp of
      Nothing -> liftIO $ do
        putStrLn "Path not specified"
        return $ Left $ ApiResponse False "Path not specified"
      Just p -> liftIO $ do
        putStrLn "Getting file: "
        let (fp, f) = splitFullPath (TL.pack p)
        putStrLn p
        putStrLn (TL.unpack fp)
        let query = ["name" =: (TL.toStrict f), "path" =: (TL.toStrict fp)]
        maybeDoc <- withMongoDbConnection $ findOne $ select query "files"
        case maybeDoc of
          Nothing -> do
            putStrLn $ show query
            return $ Left $ ApiResponse False "Document listing not found"
          Just doc -> do
            let actualPath = "static-files" ++ p
            let fi = docToFileIndex doc
            pathExists <- doesFileExist actualPath
            case pathExists of
              False -> do
                return $ Left $ ApiResponse False "File not found"
              True -> do
                txt <- TLIO.readFile actualPath
                return $ Right $ FileObject p txt (lastModified fi)

    closeFile :: FileObject -> Handler ApiResponse
    closeFile fObject = do
      -- Remove the file listing from the database of opened files
      liftIO $ putStrLn (path fObject)
      -- Get a list of secondary copies of the file
      manager <- liftIO $ HPC.newManager HPC.defaultManagerSettings -- Get a HTTP manager
      secondaries <- liftIO $ runClientM (getSecondariesReq (path fObject)) (ClientEnv manager adr)
      case secondaries of
        Left err -> do
          liftIO $ putStrLn $ show err
          return ApiResponse {result=False, message="Fileserver failed to get a list of secondary file locations"}
        Right eitherRes -> do
          case eitherRes of
            Left x -> return x
            Right dds -> do
              -- Update file object with current time
              let time = modifiedLast fObject
              let newFObject = FileObject {path=(path fObject), fileContent=(fileContent fObject), modifiedLast=time}
              -- Mass update fileservers
              let addresses = map (\ x -> url (fileServer x) (port x)) dds
              liftIO $ massRunClient addresses (updateFsReq newFObject) manager
              -- Since transactions have not yet been implemented, there is no guarantee all secondary copies will be updated and no clean way of ensuring they are updated in an all or nothing approach
              -- Update file with new timestamp
              let (p, f) = splitFullPath (TL.pack $ path fObject)
              let fi = FileIndex{fileName=(TL.unpack f), fileLocation=(TL.unpack p), lastModified=time}
              liftIO $ withMongoDbConnection $ replace (select ["name" =: (TL.toStrict f), "path" =: (TL.toStrict p)] "files") (fileIndexToDoc fi) 
              -- remove from file from list of open files
              liftIO $ withMongoDbConnection (deleteOne $ select ["path" =: (path fObject)] "openFiles")
              -- Call function to update file
              updateFile newFObject

    openFile :: Maybe String -> Handler FileRequest
    openFile mp = do
      case mp of
        Nothing -> do
          liftIO $ putStrLn "path not specified"
          return $ Left ApiResponse {result=False, message="File path not specified"}
        Just p -> do
          liftIO $ putStrLn "Getting the file"
          let actualPath = "static-files" ++ p
          fileExists <- liftIO $ doesFileExist actualPath
          case fileExists of
            False -> do
              return $ Left ApiResponse {result=False, message="File not found on file server"}
            True -> do
              -- Check the DB to make sure its not open
              maybeDoc <- liftIO $ withMongoDbConnection (findOne $ select ["path" =: p] "openFiles")
              case maybeDoc of
                Just _ -> return $ Left ApiResponse {result=False, message="File already opened by another client"}
                Nothing -> do
                  fContent <- liftIO $ TLIO.readFile actualPath
                  -- Get files time
                  let (fp, f) = splitFullPath (TL.pack p)
                  mDoc <- liftIO $ withMongoDbConnection $ findOne $ select ["name" =: (TL.toStrict f), "path" =: (TL.toStrict fp)] "files"
                  case mDoc of
                    Nothing -> do
                      return $ Left $ ApiResponse False "File listing not found in database"
                    Just doc -> do
                      let fi = docToFileIndex doc
                      -- Add file path to list of open files
                      liftIO $ putStrLn p
                      liftIO $ withMongoDbConnection (insert_ "openFiles" ["path" =: p])
                      -- Send file back to Client
                      return $ Right FileObject{path=p, fileContent=fContent, modifiedLast=(lastModified fi)}

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

-- Takes object, list of addresses, client action
massRunClient :: [BaseUrl] -> ClientM a -> HPC.Manager -> IO ()
massRunClient [] _ _ = return ()
massRunClient (x:xs) action manager = do
  res <- runClientM action (ClientEnv manager x)
  case res of
    Left err -> do putStrLn $ show err
    Right _ -> putStrLn "Success removing remote secondary"
  massRunClient xs action manager

------------------------------------------ Directory Server Client ------------------------------------------

upload :: [DirectoryDesc] -> ClientM ApiResponse
update :: UpdateObject -> ClientM ApiResponse
add :: FileServer -> ClientM ApiResponse
deleteDD :: DirectoryDesc -> ClientM ApiResponse
getSecondaries :: Maybe String -> ClientM (Either ApiResponse [DirectoryDesc])

dsapi :: DP.Proxy DSAPI
dsapi = DP.Proxy

upload :<|> update :<|> add :<|> deleteDD :<|> getSecondaries = client dsapi

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

getSecondariesReq :: String -> ClientM DescsRequest
getSecondariesReq s = do
  res <- getSecondaries (Just s)
  return res


--------------------------- FileServer Client API ---------------------------------
updateFS :: FileObject -> ClientM ApiResponse
deleteFS :: ObjIdentifier -> ClientM ApiResponse

fsapi :: DP.Proxy FSAPI
fsapi = DP.Proxy

updateFS :<|> deleteFS = client fsapi

-- Queries to be performed
updateFsReq :: FileObject -> ClientM ApiResponse
updateFsReq fo = do
  res <- updateFS fo
  return res

deleteFsReq :: ObjIdentifier -> ClientM ApiResponse
deleteFsReq oi = do
  res <- deleteFS oi
  return res
