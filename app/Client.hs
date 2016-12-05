{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DeriveGeneric         #-}

module Client where

import Lib
import qualified FileServer as FS
import GHC.Generics
import Network.HTTP.Client
import Options.Applicative
import Servant as SC
import qualified Servant.Client as SC
import qualified Servant.API as SC

-- Print responses from server
class PrintResponse a where
  resp :: Show a => a -> String

-- Print ApiResponse objects properly
instance PrintResponse ApiResponse where
  resp r = case result r of
    True -> "Success: " ++ message r
    False -> "Error: " ++ message r

api :: SC.Proxy FS.API
api = SC.Proxy

data Temp = Temp {
  a :: String
} deriving Generic

getReq :: String -> ClientM ApiResponse

-- -- Pass in command line arguments
-- opts :: IO (ParserInfo (IO ()))
-- opts = do

--   return $ info ( helper
--                 <*> subparser
--                   ( command "get-file"
--                             (withInfo ( doGetFile "localhost" 8080 ))))
