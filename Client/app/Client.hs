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

dupload :: [DirectoryDesc] -> ClientM ApiResponse
dupdate :: UpdateObject -> ClientM ApiResponse
dresolve :: String -> ClientM (Either ApiResponse DirectoryDesc)
dlist :: ClientM [FileSummary]
dadd :: FileServer -> ClientM ApiResponse

dsapi :: DP.Proxy DSAPI
dsapi = DP.Proxy

dupload :<|> dupdate :<|> dresolve :<|> dlist :<|> dadd = client dsapi

dResolveRequest :: String -> ClientM (Either ApiResponse DirectoryDesc)
dResolveRequest idString = do
  res <- dresolve idString
  return res

listRequest :: ClientM [FileSummary]
listRequest = do
  res <- dlist
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
  doesFileExist f >>=
    (\res -> if res then
        TLIO.readFile f >>=
          (\fileText -> runClientM (postRequest (FileObject f fileText)) (ClientEnv manager adr) >>=
            (\response -> case response of
                Left err -> do
                  putStrLn $ "Error: " ++ show err
                  prompt env adr
                Right apiRes -> do
                  case (result apiRes) of
                    False -> do
                      putStrLn $ "Post failed: " ++ (message apiRes)
                      prompt env adr
                    True -> do
                      putStrLn $ "Post Successful: " ++ message(apiRes))) >> prompt env adr
   else do
        putStrLn "File not found") >> prompt env adr

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
  res <- runClientM (deleteRequest (ObjIdentifier fp)) (ClientEnv manager adr)
  case res of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      prompt env adr
    Right apiRes -> do
      case (result apiRes) of
        False -> do
          putStrLn $ "Delete failed: " ++ (message apiRes)
          prompt env adr
        True -> do
          putStrLn $ "Delete Successful: " ++ message(apiRes)
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

parseCommand _ [] adr e = putStrLn "No Arguments provided" >> prompt e adr
parseCommand _ _ _ _ = putStrLn "Command unrecognized"

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

