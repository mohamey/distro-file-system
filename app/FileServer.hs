{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module FileServer
  (runServer) where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Bson
import qualified Data.ByteString as B

import Database.MongoDB
import Network.HTTP.Types.Status
import Network.Wai.Handler.Warp
import Network.Wai

import Prelude ()
import Prelude.Compat

import Servant


-- data FileObject = FileObject {
--   name :: String,
--   fileBytes :: B.ByteString
-- }

-- type API = "file" :> ReqBody '[B.ByteString] B.ByteString:> Post '[JSON] Bool

-- api :: Proxy API
-- api = Proxy

-- server :: Server API
-- server = postFile

--   where
--     postFile :: B.ByteString -> Handler Bool
--     postFile bs = liftIO $ do
--       -- Convert bytestring to file
--       B.writeFile ".temp/file" bs
      
--       -- Print out the files name
--       -- Return true
--       return True

-- app :: Application
-- app = serve api server

-- server1 :: Server FileAPI
-- server1 = serveDirectory "static-files"

-- app1 :: Application
-- app1 = serve fileAPI server1

-- runServer :: Int -> IO ()
-- runServer port = run port app

-- data GetResponse = GetResponse {
--   successful :: Bool,
--   fileBytes :: B.ByteString
-- }


-- Handle GET requests for documents in mongodb
type API = "file" :> QueryParam "name" String :> Get '[JSON] Response

api :: Proxy API
api = Proxy

server :: Server API
server = getFile

  where
    getFile :: Maybe String -> Handler Response
    getFile (Just name) =  do
      bs <- B.readFile name
      -- putStrLn $ "Got the file"
      return responseLBS ok200 [] bs

app = serve api server
