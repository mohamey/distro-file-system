module Main where

import Lib
import FileServer
import Client

main :: IO ()
main = runServer 8080
