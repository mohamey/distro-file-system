{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Client where

import Lib

import Data.Proxy as DP
import Network.HTTP.Client
import Servant.API
import Servant.Client

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

parseCommand _ _ = putStrLn "Command unrecognized"

run :: IO ()
run = do
  putStrLn "Please enter a command:\n"
  command <- getLine
  let commandParts = words command
  parseCommand (head commandParts) (tail commandParts)

url :: BaseUrl
url = BaseUrl Http "localhost" 8080 ""
