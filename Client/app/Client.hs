{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Client where

import Lib

import Control.Monad.IO.Class
import Data.Proxy as DP
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import Network.HTTP.Client
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

dsapi :: DP.Proxy DSAPI
dsapi = DP.Proxy

dupload :<|> dupdate :<|> dresolve :<|> dlist = client dsapi

listRequest :: ClientM [FileSummary]
listRequest = do
  res <- dlist
  return res

-- Args list should just have one index, the requested path
parseCommand :: String -> [String] -> BaseUrl -> IO ()
parseCommand "get" [] _ = putStrLn "No Arguments provided"
parseCommand "get" (p:_) adr = do
  manager <- newManager defaultManagerSettings
  res <- runClientM (getRequest p) (ClientEnv manager adr)
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right fi -> do
      putStrLn $ show (fileContent fi)

parseCommand "post" [] _ = putStrLn "No Arguments provided"
parseCommand "post" (f:_) adr = liftIO $ do
  manager <- newManager defaultManagerSettings
  doesFileExist f >>=
    (\res -> if res then
        TLIO.readFile f >>=
          (\fileText -> runClientM (postRequest (FileObject f fileText)) (ClientEnv manager adr) >>=
            (\response -> case response of
                Left err -> putStrLn $ "Error: " ++ show err
                Right apiRes -> do
                  case (result apiRes) of
                    False -> do
                      putStrLn $ "Post failed: " ++ (message apiRes)
                    True -> do
                      putStrLn $ "Post Successful: " ++ message(apiRes)))
   else do
        putStrLn "File not found")

parseCommand "put" [] _ = putStrLn "No Arguments provided"
parseCommand "put" (f:_) adr = liftIO $ do
  manager <- newManager defaultManagerSettings
  doesFileExist f >>=
    (\res -> if res then
        TLIO.readFile f >>=
          (\fileText -> runClientM (putRequest (FileObject f fileText)) (ClientEnv manager adr) >>=
            (\response -> case response of
                Left err -> putStrLn $ "Error: " ++ show err
                Right apiRes -> do
                  case (result apiRes) of
                    False -> do
                      putStrLn $ "Put failed: " ++ (message apiRes)
                    True -> do
                      putStrLn $ "Put Successful: " ++ message(apiRes)))
   else do
        putStrLn "File not found")

parseCommand "delete" [] _ = putStrLn "No Arguments provided"
parseCommand "delete" (fp:_) adr = liftIO $ do
  manager <- newManager defaultManagerSettings
  res <- runClientM (deleteRequest (ObjIdentifier fp)) (ClientEnv manager adr)
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right apiRes -> do
      case (result apiRes) of
        False -> do
          putStrLn $ "Delete failed: " ++ (message apiRes)
        True -> do
          putStrLn $ "Delete Successful: " ++ message(apiRes)

parseCommand "list" _  adr = liftIO $ do
  manager <- newManager defaultManagerSettings
  res <- runClientM listRequest (ClientEnv manager adr)
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right summaries -> do
      let paths = map fullPath summaries
      mapM_ print paths

parseCommand _ _ _ = putStrLn "Command unrecognized"

run :: String -> Int -> IO ()
run dirServerAddress dirServerPort = do
  putStrLn "Please enter a command:"
  command <- getLine
  let commandParts = words command
  parseCommand (head commandParts) (tail commandParts) (url dirServerAddress dirServerPort)

url :: String -> Int -> BaseUrl
url s p = BaseUrl Http s p ""
