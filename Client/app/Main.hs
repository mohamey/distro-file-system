module Main where

import Control.Monad.IO.Class
import Client
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let address = args !! 0
  let portNo = args !! 1
  liftIO $ run address (read portNo)
