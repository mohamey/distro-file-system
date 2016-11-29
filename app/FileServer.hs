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

-- import Control.Monad.Except
-- import Control.Monad.Reader
-- import Data.Aeson.Compat
-- import Data.Aeson.Types
-- import Data.Attoparsec.ByteString
-- import Data.ByteString
-- import Data.List
-- import Data.Maybe
-- import Data.String.Conversions
-- import Data.Time.Calendar
-- import GHC.Generics
-- import Lucid
-- import Network.HTTP.Media ((//), (/:))
-- import Network.Wai
import Network.Wai.Handler.Warp
import Servant
-- import System.Directory
-- import Text.Blaze
-- import Text.Blaze.Html.Renderer.Utf8
-- import qualified Data.Aeson.Parser
-- import qualified Text.Blaze.Html

type FileAPI = "files" :> Raw

fileAPI :: Proxy FileAPI
fileAPI = Proxy

server1 :: Server FileAPI
server1 = serveDirectory "static-files"

app1 :: Application
app1 = serve fileAPI server1

runServer :: Int -> IO ()
runServer port = run port app1
