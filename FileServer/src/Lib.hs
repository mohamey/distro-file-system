{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Lib where

import Data.Aeson
import Data.Bson
import Data.Text as T
import qualified Data.Text.Lazy as TL
import Database.MongoDB
import Database.MongoDB.Query
import GHC.Generics
import Prelude ()
import Prelude.Compat as PC
import Servant

-- A representation of files on the server
-- This will be stored in the mongodb database
data FileObject = FileObject {
  path :: String,
  fileContent :: TL.Text
} deriving Generic


instance FromJSON FileObject
instance ToJSON FileObject

-- This will be returned on a post request
data ApiResponse = ApiResponse {
  result :: Bool,
  message :: String
} deriving Generic

instance FromJSON ApiResponse
instance ToJSON ApiResponse

-- Object that's stored in the database
data FileIndex = FileIndex {
  fileName :: T.Text,
  fileLocation :: T.Text
} deriving Generic

-- Handle deleting files from the fileserver
data ObjIdentifier = ObjIdentifier {
  filePath :: String
} deriving Generic

instance FromJSON ObjIdentifier
instance ToJSON ObjIdentifier


fileIndexToDoc :: FileIndex -> Document
fileIndexToDoc (FileIndex {fileName=fn, fileLocation=fl}) = ["name" =: fn, "path" =: fl]

docToFileIndex :: Document -> FileIndex
docToFileIndex doc = FileIndex (T.pack (show (valueAt "name" doc))) (T.pack (show (valueAt "path" doc)))

resolveFileIndex :: FileIndex -> T.Text
resolveFileIndex fi = T.append (fileLocation fi) (fileName fi) 

fileIndexToObjId :: FileIndex -> ObjIdentifier
fileIndexToObjId fi = ObjIdentifier (show (resolveFileIndex fi))

insertFile :: Document -> Action IO ()
insertFile newFile = insert_ "files" newFile

-- Take list of strings, return objidentifiers
stringToObj :: [String] -> [ObjIdentifier]
stringToObj [] = []
stringToObj x = (ObjIdentifier (PC.head x)) : stringToObj (PC.tail x)

concatStrings :: [[Char]] -> [[Char]] -> [[Char]]
concatStrings [] _ = []
concatStrings _ [] = []
concatStrings x y = ((PC.head x) ++ (PC.head y)) : concatStrings (PC.tail x) (PC.tail y)

drainCursor :: Cursor -> Action IO [Document]
drainCursor cur = drainCursor' cur []
  where
    drainCursor' cur res  = do
      batch <- nextBatch cur
      if batch == []
        then return res
        else drainCursor' cur (res ++ batch)

-- Drain a cursor into ObjIdentifiers
docToObjs :: [Document] -> [ObjIdentifier]
docToObjs docs = PC.map fileIndexToObjId fileIndexes
  where
    fileIndexes = PC.map docToFileIndex docs

-- The API Definition
type API = "upload" :> ReqBody '[JSON] FileObject :> Post '[JSON] ApiResponse
         :<|> "remove" :> ReqBody '[JSON] ObjIdentifier :> Delete '[JSON] ApiResponse
         :<|> "update" :> ReqBody '[JSON] FileObject :> Put '[JSON] ApiResponse
         :<|> "files" :> QueryParam "path" String :> Get '[JSON] FileObject
         :<|> "list" :> Get '[JSON] [ObjIdentifier]
