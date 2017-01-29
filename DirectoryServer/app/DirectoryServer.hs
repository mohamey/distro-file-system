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
import qualified Data.List as DL
import qualified Data.Text as T
import Database.MongoDB
import qualified Network.Socket as NS
import Network.Wai.Handler.Warp
import Network.Wai
import Prelude ()
import Prelude.Compat as PC
import Servant
import System.Random

api :: Proxy API
api = Proxy

server :: Server API
server = uploadFileIndexes
    :<|> updateFileIndex
    :<|> resolveFileID
    :<|> listFiles
    :<|> addFS
    :<|> getFS
    :<|> deleteDD
    :<|> getSecondaries

  where
    -- Upload new file indexes to directory server
    uploadFileIndexes :: [DirectoryDesc] -> NS.SockAddr -> Handler ApiResponse
    uploadFileIndexes files (NS.SockAddrInet _ nAddress) = do
      let quadruple = NS.hostAddressToTuple nAddress
      -- Convert each index to document
      docs <- liftIO $ convertDocs files $ (tupleToIP quadruple)
      -- Insert each document into DB using insertAll
      liftIO $ withMongoDbConnection (insertAll_ "files" docs )
      -- Send back response
      return ApiResponse {result=True, message="Successfully added indexes to DB"}

    updateFileIndex :: UpdateObject -> Handler ApiResponse
    updateFileIndex updateObj = do
      let oldFileDoc = dirDescToDoc (old updateObj) True
      let newFileDoc = dirDescToDoc (new updateObj) True

      -- Replace the old file index with the new one
      liftIO $ withMongoDbConnection $ replace (select oldFileDoc "files") newFileDoc

      -- Send back response
      return ApiResponse {result=True, message="Successfully modified file"}

    resolveFileID :: ResolveRequest -> Handler DescRequest
    resolveFileID rr = do
      -- Build a query using file id
      let objID = read (requestId rr) :: ObjectId
      -- Resolve the file id
      let query = select ["_id" =: objID] "files"
      res <- liftIO $ withMongoDbConnection $ findOne query
      -- Send back either the file index or an api response
      case res of
        Nothing -> return $ Left ApiResponse {result=False, message="File not found by ID"}
        Just fi -> do
          -- Read whether returned doc is primary or secondary
          let docClass = read (show $ valueAt "primary" fi) :: Bool
          if (prim rr) && (docClass == True)
            then
              return $ Right $ docToDirDesc "localID" fi
            else case (prim rr) of
              True -> do -- Retrieve the primary
                let dd = docToDirDesc "localID" fi
                let fileNme = fName dd
                let filePth = init $ fLocation dd
                liftIO $ putStrLn fileNme
                liftIO $ putStrLn filePth
                let newQuery = select ["name" =: fileNme, "path" =: filePth, "primary" =: True] "files"
                rres <- liftIO $ withMongoDbConnection $ findOne newQuery
                case rres of
                  Nothing -> return $ Left ApiResponse {result=False, message="Primary File not found by ID"}
                  Just f -> return $ Right $ docToDirDesc "localID" f
              False -> do -- Just send back what we have
                return $ Right $ docToDirDesc "localID" fi

    listFiles :: Handler [FileSummary]
    listFiles = do
      -- Get all secondary files listed in database
      let primaryQuery = select ["primary"=:True] "files"
      let secondaryQuery = select ["primary"=:False] "files"
      liftIO $ do
        withMongoDbConnection $ do
          primaryDocs <- find primaryQuery >>= drainCursor
          secondaryDocs <- find secondaryQuery >>= drainCursor
          -- This returns secondary file locations where available
          let unionTest = (\x y -> ((show $ valueAt "name" x) == (show $ valueAt "name" y) && (show $ valueAt "path" x) == (show $ valueAt "path" y)))
          let docs = DL.unionBy unionTest secondaryDocs primaryDocs
          let fileIndexes = map (docToDirDesc "_id") docs
          let fileSummaries = map dirDescToSummary fileIndexes
          -- Remove duplicate paths before sending it back
          return $ DL.nubBy (\ x y -> (fullPath x) == (fullPath y) )fileSummaries

    addFS :: Maybe Int -> NS.SockAddr -> Handler ApiResponse
    addFS maybePort (NS.SockAddrInet _ nAddress) = do
      case maybePort of
        Nothing -> do
          return ApiResponse {result=False, message="No port number specified"}
        Just portNumber -> do
          let quadruple = NS.hostAddressToTuple nAddress
          let doc = ["address" =: (tupleToIP quadruple), "port" =: portNumber]
          liftIO $ withMongoDbConnection (insert_ "fileservers" doc)
          return ApiResponse {result=True, message="Successfully added file server"}

    getFS :: Handler FileServer
    getFS = do
      let query = select [] "fileservers"
      liftIO $ do
        withMongoDbConnection $ do
          docs <- find query >>= drainCursor
          let fileservers = map docToFileServer docs
          fsIndex <- liftIO $ randomRIO (0, ((length fileservers) -1))
          return $ fileservers !! fsIndex

    deleteDD :: DirectoryDesc -> Handler ApiResponse
    deleteDD dd = do
      -- Resolve file by ID
        liftIO $ withMongoDbConnection $ deleteOne $ select (dirDescToDoc dd True) "files"
        return ApiResponse{result=True, message="File deleted"}

    getSecondaries :: Maybe String -> Handler DescsRequest
    getSecondaries Nothing = return $ Left ApiResponse {result=False, message="No path provided"}
    getSecondaries (Just p) = do
      let parts = T.splitOn "/" (T.pack p)
      let name = last parts
      let path = T.intercalate "/" (init parts)
      liftIO $ putStrLn $ T.unpack path
      let query = select ["name" =: name, "path" =: path, "primary"=:False] "files"
      docs <- liftIO $ withMongoDbConnection $ find query >>= drainCursor
      let dds = map (docToDirDesc "localID") docs
      return $ Right dds

-- MongoDB Stuffs
withMongoDbConnection :: Action IO a -> IO a
withMongoDbConnection act = do
  let ip = "127.0.0.1"
  let database = "DIRECTORYDB"
  pipe <- connect (host ip)
  ret <- runResourceT $ liftIO $ access pipe master (T.pack database) act
  close pipe
  return ret

convertDocs :: [DirectoryDesc] -> String -> IO [Document]
convertDocs [] _ = return []
convertDocs (x:xs) ipAddr = do
  -- Overwrite ip address & port with actual ip and port
  let dd = DirectoryDesc (dbID x) (fName x) (fLocation x) ipAddr (port x) (modified x)
  converted <- convertToDoc dd
  convertedTail <- convertDocs xs ipAddr
  case converted of
    Nothing -> do
      putStrLn "Inconsistent dates, secondary copy discarded"
      return convertedTail
    Just convertedDoc -> do
      return (convertedDoc : convertedTail)

-- Convert DirDesc to Document, but classify file as primary/secondary
convertToDoc :: DirectoryDesc -> IO (Maybe Document)
convertToDoc dd = do
  let fNme = T.pack $ fName dd
  let fPth = T.pack $ fLocation dd
  res <- liftIO $ withMongoDbConnection $ findOne $ select ["name"=:fNme, "path"=:fPth, "primary" =: True] "files"
  case res of
    Nothing -> do
      return $ Just $ dirDescToDoc dd True
    Just existing ->do
      -- Check the timestamp for the new doc matches the existing primary
      let existingDD = docToDirDesc "_id" existing
      case (modified existingDD) == (modified dd) of
        True -> do
          return $ Just $ dirDescToDoc dd False
        False -> do
          -- Last modified timestamp does not match primary, discard
          return Nothing

app :: Application
app = serve api server

runServer :: Int -> IO ()
runServer portNo = do
  -- Clear out the database
  withMongoDbConnection $ delete $ select [] "fileservers"
  withMongoDbConnection $ delete $ select [] "files"
  liftIO $ putStrLn $ "Server running on 127.0.0.1:" ++ (show portNo)
  run portNo app
