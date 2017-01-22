{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class
import FileServer
import System.Environment

main :: IO ()
main = do --runServer port
  args <- getArgs
  let port = args !! 0
  let dirServerAddress = args !! 1
  let dirServerPort = args !! 2
  liftIO $ runServer (read port) dirServerAddress (read dirServerPort)
