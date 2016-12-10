{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Client where

import Lib

import Data.Proxy
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client

upload :: FileObject -> ClientM ApiResponse
remove :: ObjIdentifier -> ClientM ApiResponse
update :: FileObject -> ClientM ApiResponse
get :: Maybe String -> ClientM FileObject

papi :: Proxy API
papi = Proxy

upload :<|> remove :<|> update :<|> get = client papi

-- Write queries to be performed
queries :: ClientM (FileObject)
queries = do
  fi <- get (Just "/temp/test.txt")
  return (fi)

run :: IO ()
run = do
  manager <- newManager defaultManagerSettings
  let url = BaseUrl Http "localhost" 8080 ""
  putStrLn $ showBaseUrl url
  res <- runClientM queries (ClientEnv manager url)
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (fi) -> do
      putStrLn $ show (fileContent fi)
