{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Client where

import Lib

import Control.Monad.IO.Class
import qualified Data.Map.Strict as Map
import Data.Proxy as DP
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import qualified Network.HTTP.Client as HPC
import Servant.API
import Servant.Client
import System.Directory
import System.Process

-------------------------------- File Server API --------------------------------------------

upload :: FileObject -> ClientM ApiResponse
remove :: ObjIdentifier -> ClientM ApiResponse
update :: FileObject -> ClientM ApiResponse
closeF :: FileObject -> ClientM ApiResponse
openF :: Maybe String -> ClientM (Either ApiResponse FileObject)
get :: Maybe String -> ClientM FileObject
list :: ClientM [ObjIdentifier]

papi :: DP.Proxy API
papi = DP.Proxy

upload :<|> remove :<|> update :<|> closeF :<|> openF :<|> get :<|> list = client papi

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

closeRequest :: FileObject -> ClientM ApiResponse
closeRequest fo = do
  res <- closeF fo
  return res

openRequest :: String -> ClientM (Either ApiResponse FileObject)
openRequest s = do
  res <- openF (Just s)
  return res

deleteRequest :: ObjIdentifier -> ClientM ApiResponse
deleteRequest fpath = do
  res <- remove fpath
  return res

------------------------------- Directory Server API ------------------------------------------

dresolve :: ResolveRequest -> ClientM (Either ApiResponse DirectoryDesc)
dlist :: ClientM [FileSummary]
dgetFs :: ClientM FileServer

dsapi :: DP.Proxy DSAPI
dsapi = DP.Proxy

dresolve :<|> dlist :<|> dgetFs = client dsapi

dResolveRequest :: ResolveRequest -> ClientM (Either ApiResponse DirectoryDesc)
dResolveRequest rr = do
  res <- dresolve rr
  return res

listRequest :: ClientM [FileSummary]
listRequest = do
  res <- dlist
  return res

getFileServer :: ClientM FileServer
getFileServer = do
  res <- dgetFs
  return res

-- Args list should just have one index, the requested path
parseCommand :: String -> [String] -> BaseUrl -> Env -> IO ()
parseCommand "get" (p:_) adr env = do
  manager <- HPC.newManager HPC.defaultManagerSettings
  case Map.lookup p env of -- Find object id of requested file
    Nothing -> do
      putStrLn $ redCode ++ "Could not resolve file path locally"
      prompt env adr
    Just fileIdString -> do
      -- Query Directory server for file using it's id
      let rr = ResolveRequest {requestId=fileIdString, prim=False}
      res <- runClientM (dResolveRequest rr) (ClientEnv manager adr)
      case res of
        Left err -> do
          putStrLn $ redCode ++ "Error: " ++ show err
          prompt env adr
        -- Check the directory server response
        Right dsResponse -> do
          case dsResponse of
            Left x -> do
              putStrLn $ redCode ++ "Error: " ++ (message x)
              prompt env adr
            Right dd -> do
              -- Query the returned file server for the file
              rres <- runClientM (getRequest (fLocation dd ++ "/" ++ fName dd)) (ClientEnv manager (url (fileServer dd) (port dd)))
              case rres of
                Left err -> do
                  putStrLn $ redCode ++  "Error retrieving file from file server:\n" ++ show err
                  prompt env adr
                Right x -> do
                  putStrLn $ greenCode ++ "File Retrieved: " ++ resetCode
                  putStrLn (show $ fileContent x)
                  writeFile (path x) (TL.unpack $ fileContent x)
                  prompt env adr

parseCommand "post" (f:_) adr env = liftIO $ do
  manager <- HPC.newManager HPC.defaultManagerSettings
  fileExists <- doesFileExist f
  case fileExists of
    False -> do
        putStrLn $ redCode ++ "File not found"
        prompt env adr
    True -> do
      -- Resolve file path to make sure file doesn't exist
      case Map.lookup ("/" ++ f) env of
        Just _ -> do
          putStrLn $ blueCode ++ "File already exists, use put command to update it"
          prompt env adr
        Nothing -> do
          -- Get File Server to send to
          response <- runClientM getFileServer (ClientEnv manager adr)
          case response of
            Left err -> do
              putStrLn $ redCode ++ "Error requesting file server for POST: \n" ++ show err
              prompt env adr
            Right fs -> do
              let serverAddress = address fs
              let serverPort = portNum fs
              fileContents <- TLIO.readFile f
              res <- runClientM (postRequest (FileObject f fileContents)) (ClientEnv manager (url serverAddress serverPort))
              case res of
                Left err -> do
                  putStrLn $ redCode ++ "Error posting file to fileserver\n" ++ show err
                  prompt env adr
                Right rresponse -> do
                  putStrLn $ greenCode ++ "Response from fileserver: \n" ++ resetCode ++ show (message rresponse)
                  prompt env adr

parseCommand "put" (f:_) adr env = liftIO $ do
  manager <- HPC.newManager HPC.defaultManagerSettings
  fileExists <- doesFileExist f
  case fileExists of
    False -> do
      putStrLn $ redCode ++ "File not found"
      prompt env adr
    True -> do
      fileText <- TLIO.readFile f
      response <- runClientM (putRequest (FileObject f fileText)) (ClientEnv manager adr)
      case response of
          Left err -> do
            putStrLn $ redCode ++ "Error: " ++ show err
            prompt env adr
          Right apiRes -> do
            case (result apiRes) of
              False -> do
                putStrLn $ redCode ++ "Put failed: " ++ (message apiRes)
                prompt env adr
              True -> do
                putStrLn $ greenCode ++ "Put Successful: " ++ resetCode ++ message(apiRes)
                prompt env adr

parseCommand "delete" (fp:_) adr env = liftIO $ do
  manager <- HPC.newManager HPC.defaultManagerSettings
  case Map.lookup fp env of -- Find object id of requested file
    Nothing -> do
      putStrLn $ redCode ++ "File path did not resolve locally"
      prompt env adr
    Just fileIdString -> do
      -- Query Directory server for file using it's id
      let rr = ResolveRequest {requestId=fileIdString, prim=True}
      res <- runClientM (dResolveRequest rr) (ClientEnv manager adr)
      case res of
        Left err -> do
          putStrLn $ redCode ++ "Failed to resolve file path: \n" ++ (show err)
          prompt env adr
        Right response -> do
          case response of
            Left apiRes -> do
              putStrLn $ redCode ++ "Error resolving file on directory server:\n" ++ (message apiRes)
              prompt env adr
            Right dd -> do
              -- Using file path and dd details, send delete request
              let objId = ObjIdentifier {filePath=fp}
              rres <- runClientM (deleteRequest objId) (ClientEnv manager (url (fileServer dd) (port dd)))
              case rres of
                Left error -> do
                  putStrLn $ redCode ++ "Servant error: \n" ++ show error
                  prompt env adr
                Right deleteResponse -> do
                  putStrLn (message deleteResponse)
                  prompt env adr

parseCommand "open" (p:_) adr env = do
  manager <- HPC.newManager HPC.defaultManagerSettings
  case Map.lookup p env of -- Find object id of requested file
    Nothing -> do
      putStrLn $ redCode ++ "Could not resolve file path locally"
      prompt env adr
    Just fileIdString -> do
      -- Query Directory server for file using it's id
      let rr = ResolveRequest {requestId=fileIdString, prim=True}
      res <- runClientM (dResolveRequest rr) (ClientEnv manager adr)
      case res of
        Left err -> do
          putStrLn $ redCode ++ "Error: " ++ show err
          prompt env adr
        -- Check the directory server response
        Right dsResponse -> do
          case dsResponse of
            Left x -> do
              putStrLn $ redCode ++ "Error: " ++ (message x)
              prompt env adr
            Right dd -> do
              -- Query the returned file server for the file
              let fsUrl = url (fileServer dd) (port dd)
              rres <- runClientM (openRequest (fLocation dd ++ fName dd)) (ClientEnv manager fsUrl)
              case rres of
                Left err -> do
                  putStrLn $ redCode ++ "Error retrieving file from file server:\n" ++ show err
                  prompt env adr
                Right eitherRes -> do
                  case eitherRes of
                    Left y -> do
                      putStrLn $ redCode ++ "Could not retrieve file from fileserver\n" ++ show (message y)
                      prompt env adr
                    Right x -> do
                      -- Write the received file locally
                      createDirectoryIfMissing True "temp"
                      let nameParts = TL.splitOn "/" (TL.pack p)
                      let tempPath = "temp/" ++ (TL.unpack $ last nameParts)
                      TLIO.writeFile tempPath (fileContent x)
                      -- Now open the file in text editor
                      procHandle <- spawnCommand $ "vim " ++ tempPath
                      _ <- waitForProcess procHandle
                      -- Update the file at the fileserver
                      f <- TLIO.readFile tempPath
                      let fObject = FileObject {path=p, fileContent=f}
                      updateRes <- runClientM (closeRequest fObject) (ClientEnv manager fsUrl)
                      case updateRes of
                        Left err -> do
                          putStrLn $ redCode ++ "Servant error uploading updated file\n" ++ show err
                          prompt env adr
                        Right resres -> do
                          case result resres of
                            True -> do
                              removeFile tempPath
                              putStrLn $ greenCode ++ "Successfully updated file"
                              prompt env adr
                            False -> do
                              putStrLn $ redCode ++ "Failed to upload updated file"
                              prompt env adr

parseCommand "list" _  adr env = liftIO $ do
  manager <- HPC.newManager HPC.defaultManagerSettings
  res <- runClientM listRequest (ClientEnv manager adr)
  case res of
    Left err -> do
      putStrLn $ redCode ++ "Error: " ++ show err
      prompt env adr
    Right summaries -> do
      let values = map fileId summaries
      let keys = map fullPath summaries
      let zippedList = zip keys values
      let newEnv = Map.fromList zippedList
      putStrLn $ greenCode ++ "Available Files:"
      mapM_ (\x -> putStrLn $ whiteCode ++ x) keys
      prompt newEnv adr

parseCommand _ _ adr e = do
  putStrLn $ redCode ++ "Command unrecognized"
  prompt e adr

run :: String -> Int -> IO ()
run dirServerAddress dirServerPort = do
  let adr = url dirServerAddress dirServerPort
  parseCommand "list" [] adr Map.empty

prompt :: Env -> BaseUrl -> IO ()
prompt env burl = do
  putStrLn $ whiteCode ++ "Please enter a command:"
  command <- getLine
  let commandParts = words command
  parseCommand (head commandParts) (tail commandParts) burl env

url :: String -> Int -> BaseUrl
url s p = BaseUrl Http s p ""

type Env = Map.Map String String

