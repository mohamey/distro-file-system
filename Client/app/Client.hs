{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Client where

import Lib

import Control.Monad.IO.Class
import qualified Data.Map.Strict as Map
import Data.Proxy as DP
import qualified Data.Text.Lazy.IO as TLIO
import qualified Network.HTTP.Client as HPC
import Servant.API
import Servant.Client
import System.Directory

-------------------------------- File Server API --------------------------------------------

upload :: FileObject -> ClientM ApiResponse
remove :: ObjIdentifier -> ClientM ApiResponse
update :: FileObject -> ClientM ApiResponse
get :: Maybe String -> ClientM FileObject
list :: ClientM [ObjIdentifier]

papi :: DP.Proxy API
papi = DP.Proxy

upload :<|> remove :<|> update :<|> get :<|> list = client papi

-- Write queries to be performed
getRequest :: String -> ClientM FileObject
getRequest reqPath = do
  fi <- get (Just reqPath)
  return fi

postRequest :: FileObject -> ClientM ApiResponse
postRequest postFile = do
  res <- upload postFile
  return res

putRequest :: FileObject -> ClientM ApiResponse
putRequest putFile = do
  res <- update putFile
  return res

deleteRequest :: ObjIdentifier -> ClientM ApiResponse
deleteRequest fpath = do
  res <- remove fpath
  return res

------------------------------- Directory Server API ------------------------------------------

dresolve :: String -> ClientM (Either ApiResponse DirectoryDesc)
dlist :: ClientM [FileSummary]
dgetFs :: ClientM FileServer

dsapi :: DP.Proxy DSAPI
dsapi = DP.Proxy

dresolve :<|> dlist :<|> dgetFs = client dsapi

dResolveRequest :: String -> ClientM (Either ApiResponse DirectoryDesc)
dResolveRequest idString = do
  res <- dresolve idString
  return res

listRequest :: ClientM [FileSummary]
listRequest = do
  res <- dlist
  return res

getFileServer :: ClientM FileServer
getFileServer = do
  res <- dgetFs
  return res

-- Args list should just have one index, the requested path
parseCommand :: String -> [String] -> BaseUrl -> Env -> IO ()
parseCommand "get" (p:_) adr env = do
  manager <- HPC.newManager HPC.defaultManagerSettings
  case Map.lookup p env of -- Find object id of requested file
    Just fileIdString -> do
      -- Query Directory server for file using it's id
      res <- runClientM (dResolveRequest fileIdString) (ClientEnv manager adr)
      case res of
        Left err -> do
          putStrLn $ "Error: " ++ show err
          prompt env adr
        -- Check the directory server response
        Right dsResponse -> do
          case dsResponse of
            Left x -> do
              putStrLn (message x)
              prompt env adr
            Right dd -> do
              -- Query the returned file server for the file
              rres <- runClientM (getRequest (fLocation dd ++ "/" ++ fName dd)) (ClientEnv manager (url (fileServer dd) (port dd)))
              case rres of
                Left err -> do
                  putStrLn $ "Error retrieving file from file server:\n" ++ show err
                  prompt env adr
                Right x -> do
                  putStrLn (show $ fileContent x)
                  prompt env adr
    Nothing -> do
      putStrLn "Could not resolve file path locally"
      prompt env adr

parseCommand "post" (f:_) adr env = liftIO $ do
  manager <- HPC.newManager HPC.defaultManagerSettings
  fileExists <- doesFileExist f
  case fileExists of
    True -> do
      -- Get File Server to send to
      response <- runClientM getFileServer (ClientEnv manager adr)
      case response of
        Left err -> do
          putStrLn $ "Error requesting file server for POST: \n" ++ show err
          prompt env adr
        Right fs -> do
          let serverAddress = address fs
          let serverPort = portNum fs
          fileContents <- TLIO.readFile f
          res <- runClientM (postRequest (FileObject f fileContents)) (ClientEnv manager (url serverAddress serverPort))
          case res of
            Left err -> do
              putStrLn $ "Error posting file to fileserver\n" ++ show err
              prompt env adr
            Right rresponse -> do
              putStrLn $ "Response from fileserver: \n" ++ show (message rresponse)
              prompt env adr
    False -> do
        putStrLn "File not found"
        prompt env adr

parseCommand "put" (f:_) adr env = liftIO $ do
  manager <- HPC.newManager HPC.defaultManagerSettings
  doesFileExist f >>=
    (\res -> if res then
        TLIO.readFile f >>=
          (\fileText -> runClientM (putRequest (FileObject f fileText)) (ClientEnv manager adr) >>=
            (\response -> case response of
                Left err -> do
                  putStrLn $ "Error: " ++ show err
                  prompt env adr
                Right apiRes -> do
                  case (result apiRes) of
                    False -> do
                      putStrLn $ "Put failed: " ++ (message apiRes)
                      prompt env adr
                    True -> do
                      putStrLn $ "Put Successful: " ++ message(apiRes)
                      prompt env adr))
   else do
        putStrLn "File not found"
        prompt env adr)

parseCommand "delete" (fp:_) adr env = liftIO $ do
  manager <- HPC.newManager HPC.defaultManagerSettings
  case Map.lookup fp env of -- Find object id of requested file
    Nothing -> do
      putStrLn "File path did not resolve locally"
      prompt env adr
    Just fileIdString -> do
      -- Query Directory server for file using it's id
      res <- runClientM (dResolveRequest fileIdString) (ClientEnv manager adr)
      case res of
        Left err -> do
          putStrLn $ "Failed to resolve file path: \n" ++ (show err)
          prompt env adr
        Right response -> do
          case response of
            Left apiRes -> do
              putStrLn $ "Error resolving file on directory server:\n" ++ (message apiRes)
              prompt env adr
            Right dd -> do
              -- Using file path and dd details, send delete request
              let objId = ObjIdentifier {filePath=fp}
              rres <- runClientM (deleteRequest objId) (ClientEnv manager (url (fileServer dd) (port dd)))
              case rres of
                Left error -> do
                  putStrLn $ "Servant error: \n" ++ show error
                  prompt env adr
                Right deleteResponse -> do
                  putStrLn (message deleteResponse)
                  prompt env adr

parseCommand "list" _  adr env = liftIO $ do
  manager <- HPC.newManager HPC.defaultManagerSettings
  res <- runClientM listRequest (ClientEnv manager adr)
  case res of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      prompt env adr
    Right summaries -> do
      let values = map fileId summaries
      let keys = map fullPath summaries
      let zippedList = zip keys values
      let newEnv = Map.fromList zippedList
      mapM_ print keys
      prompt newEnv adr

parseCommand _ _ adr e = putStrLn "Command unrecognized" >> prompt e adr

run :: String -> Int -> IO ()
run dirServerAddress dirServerPort = do
  prompt Map.empty (url dirServerAddress dirServerPort)

prompt :: Env -> BaseUrl -> IO ()
prompt env burl = do
  putStrLn "Please enter a command:"
  command <- getLine
  let commandParts = words command
  parseCommand (head commandParts) (tail commandParts) burl env

url :: String -> Int -> BaseUrl
url s p = BaseUrl Http s p ""

type Env = Map.Map String String

