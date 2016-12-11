{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Client where

import Lib

import Control.Monad.IO.Class
import Data.Proxy as DP
import qualified Data.Text.Lazy.IO as TLIO
import Network.HTTP.Client
import Servant.API
import Servant.Client
import System.Directory

upload :: FileObject -> ClientM ApiResponse
remove :: ObjIdentifier -> ClientM ApiResponse
update :: FileObject -> ClientM ApiResponse
get :: Maybe String -> ClientM FileObject

papi :: DP.Proxy API
papi = DP.Proxy

upload :<|> remove :<|> update :<|> get = client papi

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

-- Args list should just have one index, the requested path
parseCommand :: String -> [String] -> IO ()
parseCommand _ [] = putStrLn "No Arguments provided"
parseCommand "get" (p:_) = do
  manager <- newManager defaultManagerSettings
  res <- runClientM (getRequest p) (ClientEnv manager url)
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right fi -> do
      putStrLn $ show (fileContent fi)
parseCommand "post" (f:_) = liftIO $ do
  manager <- newManager defaultManagerSettings
  doesFileExist f >>=
    (\res -> if res then
        TLIO.readFile f >>=
          (\fileText -> runClientM (postRequest (FileObject f fileText)) (ClientEnv manager url) >>=
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
parseCommand "put" (f:_) = liftIO $ do
  manager <- newManager defaultManagerSettings
  doesFileExist f >>=
    (\res -> if res then
        TLIO.readFile f >>=
          (\fileText -> runClientM (putRequest (FileObject f fileText)) (ClientEnv manager url) >>=
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
parseCommand "delete" (fp:_) = liftIO $ do
  manager <- newManager defaultManagerSettings
  res <- runClientM (deleteRequest (ObjIdentifier fp)) (ClientEnv manager url)
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right apiRes -> do
      case (result apiRes) of
        False -> do
          putStrLn $ "Delete failed: " ++ (message apiRes)
        True -> do
          putStrLn $ "Delete Successful: " ++ message(apiRes)

parseCommand _ _ = putStrLn "Command unrecognized"

run :: IO ()
run = do
  putStrLn "Please enter a command:"
  command <- getLine
  let commandParts = words command
  parseCommand (head commandParts) (tail commandParts)

url :: BaseUrl
url = BaseUrl Http "localhost" 8080 ""
