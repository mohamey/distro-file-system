{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Client where

import Lib

import Control.Monad.IO.Class
import qualified Data.Map.Strict as Map
import Data.Time.Clock
import Data.Proxy as DP
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import qualified Network.HTTP.Client as HPC
import Servant.API
import Servant.Client
import System.Directory
import System.Exit
import System.Process

-------------------------------- File Server API --------------------------------------------

upload :: FileObject -> ClientM ApiResponse
remove :: ObjIdentifier -> ClientM ApiResponse
update :: FileObject -> ClientM ApiResponse
closeF :: FileObject -> ClientM ApiResponse
openF :: Maybe String -> ClientM FileRequest
get :: Maybe String -> ClientM FileRequest
list :: ClientM [ObjIdentifier]

papi :: DP.Proxy API
papi = DP.Proxy

upload :<|> remove :<|> update :<|> closeF :<|> openF :<|> get :<|> list = client papi

-- Write queries to be performed
getRequest :: String -> ClientM FileRequest
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

openRequest :: String -> ClientM FileRequest
openRequest s = do
  res <- openF (Just s)
  return res

deleteRequest :: ObjIdentifier -> ClientM ApiResponse
deleteRequest fpath = do
  res <- remove fpath
  return res

------------------------------- Directory Server API ------------------------------------------

dresolve :: ResolveRequest -> ClientM DescRequest
dlist :: ClientM [FileSummary]
dgetFs :: ClientM FileServer

dsapi :: DP.Proxy DSAPI
dsapi = DP.Proxy

dresolve :<|> dlist :<|> dgetFs = client dsapi

dResolveRequest :: ResolveRequest -> ClientM DescRequest
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
parseCommand :: String -> [String] -> BaseUrl -> Env -> Cache -> IO ()
parseCommand "get" (p:_) adr env cache = do
  manager <- HPC.newManager HPC.defaultManagerSettings
  case Map.lookup p env of -- Find object id of requested file
    Nothing -> do
      putStrLn $ redCode ++ "Could not resolve file path locally"
      prompt env cache adr
    Just fileIdString -> do
      -- Query Directory server for file using it's id
      maybeDD <- getFileDetails fileIdString False adr
      case maybeDD of
        Nothing -> do
          putStrLn $ redCode ++ "Failed to retrieve file details from the directory server"
          prompt env cache adr
        Just dd -> do
          -- Cache path and file
          let cacheDir = "cache" ++ fLocation dd
          let cachePth = "cache" ++ p
          -- Check if there is a cached copy and if it's up to date
          case Map.lookup (TL.pack p) cache of
            Nothing -> do
              -- Query the returned file server for the file
              maybeContents <- runGetRequest  p (url (fileServer dd) (port dd))
              -- If theres no cached copy, directory might not exist yet
              createDirectoryIfMissing True cacheDir
              case maybeContents of
                Nothing -> do
                  prompt env cache adr
                Just (f, t) -> do
                  putStrLn $ TL.unpack f
                  writeFile cachePth (TL.unpack f)
                  let newCache = Map.insert (TL.pack p) t cache
                  prompt env newCache adr
            Just timestamp -> do
              let remoteTimestamp = modified dd
              if timestamp <= remoteTimestamp then do
                -- Cache copy up to date, print it
                putStrLn $ blueCode ++ "Cached copy up to date, opening..."
                contents <- TLIO.readFile cachePth
                putStrLn $ whiteCode ++ (TL.unpack contents)
                prompt env cache adr
              else do
                maybeContents <- runGetRequest p (url (fileServer dd) (port dd))
                case maybeContents of
                  Nothing -> do
                    prompt env cache adr
                  Just (f, t) -> do
                    putStrLn $ TL.unpack f
                    writeFile cachePth (TL.unpack f)
                    let nCache = Map.delete (TL.pack p) cache
                    let newCache = Map.insert (TL.pack p) t nCache
                    prompt env newCache adr

parseCommand "post" (f:_) adr env cache = liftIO $ do
  manager <- HPC.newManager HPC.defaultManagerSettings
  fileExists <- doesFileExist f
  case fileExists of
    False -> do
        putStrLn $ redCode ++ "File not found"
        prompt env cache adr
    True -> do
      -- Resolve file path to make sure file doesn't exist
      case Map.lookup ("/" ++ f) env of
        Just _ -> do
          putStrLn $ blueCode ++ "File already exists, use put command to update it"
          prompt env cache adr
        Nothing -> do
          -- Get File Server to send to
          response <- runClientM getFileServer (ClientEnv manager adr)
          case response of
            Left err -> do
              putStrLn $ redCode ++ "Error requesting file server for POST: \n" ++ show err
              prompt env cache adr
            Right fs -> do
              let serverAddress = address fs
              let serverPort = portNum fs
              fileContents <- TLIO.readFile f
              time <- getCurrentTime
              res <- runClientM (postRequest (FileObject f fileContents time)) (ClientEnv manager (url serverAddress serverPort))
              case res of
                Left err -> do
                  putStrLn $ redCode ++ "Error posting file to fileserver\n" ++ show err
                  prompt env cache adr
                Right rresponse -> do
                  putStrLn $ greenCode ++ "Response from fileserver: \n" ++ resetCode ++ show (message rresponse)
                  maybeEnv <- getListing adr
                  case maybeEnv of
                    Nothing -> do
                      putStrLn $ redCode ++ "Failed to update local listing of files, try again later"
                      prompt env cache adr
                    Just newEnv -> do
                      prompt newEnv cache adr

parseCommand "put" (f:_) adr env cache = liftIO $ do
  manager <- HPC.newManager HPC.defaultManagerSettings
  fileExists <- doesFileExist f
  case fileExists of
    False -> do
      putStrLn $ redCode ++ "File not found"
      prompt env cache adr
    True -> do
      fileText <- TLIO.readFile f
      time <- getCurrentTime
      response <- runClientM (putRequest (FileObject f fileText time)) (ClientEnv manager adr)
      case response of
          Left err -> do
            putStrLn $ redCode ++ "Error: " ++ show err
            prompt env cache adr
          Right apiRes -> do
            case (result apiRes) of
              False -> do
                putStrLn $ redCode ++ "Put failed: " ++ (message apiRes)
                prompt env cache adr
              True -> do
                putStrLn $ greenCode ++ "Put Successful: " ++ resetCode ++ message(apiRes)
                prompt env cache adr

parseCommand "delete" (fp:_) adr env cache = liftIO $ do
  manager <- HPC.newManager HPC.defaultManagerSettings
  case Map.lookup fp env of -- Find object id of requested file
    Nothing -> do
      putStrLn $ redCode ++ "File path did not resolve locally"
      prompt env cache adr
    Just fileIdString -> do
      maybeDD <- getFileDetails fileIdString True adr
      case maybeDD of
        Nothing -> do
          putStrLn $ redCode ++ "Failed to get file details from directory server"
          prompt env cache adr
        Just dd -> do
          -- Using file path and dd details, send delete request
          let objId = ObjIdentifier {filePath=fp}
          rres <- runClientM (deleteRequest objId) (ClientEnv manager (url (fileServer dd) (port dd)))
          case rres of
            Left error -> do
              putStrLn $ redCode ++ "Servant error: \n" ++ show error
              prompt env cache adr
            Right deleteResponse -> do
              case (result deleteResponse) of
                False -> do
                  putStrLn $ redCode ++ (message deleteResponse)
                  prompt env cache adr
                True -> do
                  let newEnv = Map.delete fp env
                  putStrLn $ greenCode ++ "File Deleted"
                  prompt newEnv cache adr

parseCommand "open" (p:_) adr env cache = do
  manager <- HPC.newManager HPC.defaultManagerSettings
  case Map.lookup p env of -- Find object id of requested file
    Nothing -> do
      putStrLn $ redCode ++ "Could not resolve file path locally"
      prompt env cache adr
    Just fileIdString -> do
      -- Check if the file has been cached
      maybeDD <- getFileDetails fileIdString True adr
      case maybeDD of
        Nothing -> do
          putStrLn $ redCode ++ "Could not get file details from directory server"
          prompt env cache adr
        Just dd -> do
          let fsUrl = url (fileServer dd) (port dd)
          let filePth = fLocation dd ++ fName dd
          let cachePth = "cache" ++ filePth
          time <- getCurrentTime
          -- Check if file is present in cache
          case Map.lookup (TL.pack p) cache of
            Nothing -> do
              putStrLn $ blueCode ++ "No cache entry found, retrieving file"
              maybeFileObj <- runOpenRequest (fLocation dd ++ fName dd) fsUrl
              case maybeFileObj of
                Nothing -> do
                  prompt env cache adr
                Just x -> do
                  -- Write the received file locally
                  let (cp, _) = splitFullPath $ TL.pack cachePth
                  createDirectoryIfMissing True (TL.unpack cp)
                  TLIO.writeFile cachePth (fileContent x)
                  -- Now open the file in text editor
                  _ <- editFile "vim" cachePth
                  -- Update cache to save file details
                  let newCache = Map.insert (TL.pack p) time cache
                  -- Update the file at the fileserver
                  f <- TLIO.readFile cachePth
                  let fObject = FileObject p f time
                  _ <- runCloseRequest fObject fsUrl
                  prompt env newCache adr
            Just timestamp -> do
              -- Get timestamp of remote primary file
              putStrLn $ blueCode ++ "Cache listing found"
              let remoteTimestamp = modified dd
              case timestamp <= remoteTimestamp of
                True -> do
                  putStrLn $ blueCode ++ "Opening cached copy..."
                  _ <- editFile "vim" cachePth
                  let newCache = Map.insert (TL.pack p) time cache
                  -- Update the file at the fileserver
                  f <- TLIO.readFile cachePth
                  let fObject = FileObject p f time
                  _ <- runCloseRequest fObject fsUrl
                  prompt env newCache adr
                False -> do
                  putStrLn $ blueCode ++ "Cache out of date, retrieving up to date file"
                  maybeFileObj <- runOpenRequest filePth fsUrl
                  case maybeFileObj of
                    Nothing -> do
                      prompt env cache adr
                    Just x -> do
                      TLIO.writeFile cachePth (fileContent x)
                      _ <- editFile "vim" cachePth
                      let newCache = Map.insert (TL.pack p) time cache
                      -- Update the file at the fileserver
                      f <- TLIO.readFile cachePth
                      let fObject = FileObject p f time
                      _ <- runCloseRequest fObject fsUrl
                      prompt env newCache adr

parseCommand "list" _  adr env cache = liftIO $ do
  maybeEnv <- getListing adr
  case maybeEnv of
    Nothing -> do
      prompt env cache adr
    Just newEnv -> do
      putStrLn $ greenCode ++ "Available Files:"
      mapM_ (\x -> putStrLn $ whiteCode ++ x) (Map.keys newEnv)
      prompt newEnv cache adr

parseCommand _ _ adr e cache = do
  putStrLn $ redCode ++ "Command unrecognized"
  prompt e cache adr

getListing :: BaseUrl -> IO (Maybe Env)
getListing adr = do
  manager <- HPC.newManager HPC.defaultManagerSettings
  res <- runClientM listRequest (ClientEnv manager adr)
  case res of
    Left err -> do
      putStrLn $ redCode ++ "Failed to update file listing" ++ show err
      return Nothing
    Right summaries -> do
      let values = map fileId summaries
      let keys = map fullPath summaries
      let zippedList = zip keys values
      let newEnv = Map.fromList zippedList
      return $ Just newEnv

run :: String -> Int -> IO ()
run dirServerAddress dirServerPort = do
  let adr = url dirServerAddress dirServerPort
  parseCommand "list" [] adr Map.empty Map.empty

prompt :: Env -> Cache -> BaseUrl -> IO ()
prompt env cache burl = do
  putStrLn $ whiteCode ++ "Please enter a command:" ++ resetCode
  command <- getLine
  let commandParts = words command
  parseCommand (head commandParts) (tail commandParts) burl env cache

url :: String -> Int -> BaseUrl
url s p = BaseUrl Http s p ""

type Env = Map.Map String String

type Cache = Map.Map TL.Text UTCTime

getFileDetails :: String -> Bool -> BaseUrl -> IO (Maybe DirectoryDesc)
getFileDetails idString getPrimary adr = do
  manager <- HPC.newManager HPC.defaultManagerSettings
  let rr = ResolveRequest idString getPrimary
  res <- runClientM (dResolveRequest rr) (ClientEnv manager adr)
  case res of
    Left err -> do
      putStrLn $ redCode ++ "Error: " ++ show err
      return Nothing
    -- Check the directory server response
    Right dsResponse -> do
      case dsResponse of
        Left x -> do
          putStrLn $ redCode ++ "Error: " ++ (message x)
          return Nothing
        Right dd -> do
          return $ Just dd

runCloseRequest :: FileObject -> BaseUrl -> IO Bool
runCloseRequest fo adr = do
  manager <- HPC.newManager HPC.defaultManagerSettings
  updateRes <- runClientM (closeRequest fo) (ClientEnv manager adr)
  case updateRes of
    Left err -> do
      putStrLn $ redCode ++ "Servant error uploading updated file\n" ++ show err
      return False
    Right resres -> do
      case result resres of
        True -> do
          putStrLn $ greenCode ++ "Successfully updated file"
          return True
        False -> do
          putStrLn $ redCode ++ "Failed to upload updated file"
          return False

editFile :: String -> String -> IO ExitCode
editFile editor path = do
  procHandle <- spawnCommand $ editor ++ " " ++ path
  res <- waitForProcess procHandle
  return res

runOpenRequest :: String -> BaseUrl -> IO (Maybe FileObject)
runOpenRequest fl adr = do
  manager <- HPC.newManager HPC.defaultManagerSettings
  rres <- runClientM (openRequest fl) (ClientEnv manager adr)
  case rres of
    Left err -> do
      putStrLn $ redCode ++ "Error retrieving file from file server:\n" ++ show err
      return Nothing
    Right eitherRes -> do
      case eitherRes of
        Left y -> do
          putStrLn $ redCode ++ "Could not retrieve file from fileserver\n" ++ show (message y)
          return Nothing
        Right x -> do
          return $ Just x

runGetRequest :: String -> BaseUrl -> IO (Maybe (TL.Text, UTCTime))
runGetRequest fPth adr = do
  manager <- HPC.newManager HPC.defaultManagerSettings
  rres <- runClientM (getRequest fPth) (ClientEnv manager adr)
  case rres of
    Left err -> do
      putStrLn $ redCode ++  "Error retrieving file from file server:\n" ++ show err
      return Nothing
    Right (Left res) -> do
      putStrLn $ redCode ++ "Error:" ++ (message res)
      return Nothing
    Right (Right x) -> do
      putStrLn $ greenCode ++ "File Retrieved: " ++ resetCode
      return $ Just ((fileContent x), modifiedLast x)
