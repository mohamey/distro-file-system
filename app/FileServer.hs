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

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson.Compat
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import qualified Data.ByteString as B
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Utils.StaticFiles
import System.Directory
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html

data User = User
  {
    name :: String
    , age :: Int
    , email :: String
    , registration_date :: Day 
  } deriving (Eq, Show, Generic)

instance ToJSON User

users1 :: [User]
users1 =
  [
    User "Isaac Newton" 371 "isaac@newton.co.uk" (fromGregorian 1683 3 1)
  , User "Albert Einstein" 136 "ae@mc2.org" (fromGregorian 1905 12 1)
  ]

type UserAPI1 = "users" :> Get '[JSON] [User]


server1 :: Server UserAPI1
server1 = return users1

userAPI :: Proxy UserAPI1
userAPI = Proxy

app1 :: Application
app1 = serve userAPI server1

-- data ServerFile = ServerFile
--   {
--     fileName :: String
--     , file :: B.ByteString
--   } deriving (Eq, Show, Generic)

-- instance ToJSON ServerFile

newtype BS = BS [B.ByteString]
  deriving Generic

type FileAPI = "file" :> Raw

-- fileAPI :: Proxy FileAPI
-- fileAPI = Proxy

server2 :: Server FileAPI
server2 = serveDirectory "file"

runServer :: Int -> IO ()
runServer port = run port server2
