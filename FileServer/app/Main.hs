{-# LANGUAGE OverloadedStrings #-}
module Main where

import FileServer
import System.Environment

main :: IO ()
main = do --runServer port
  getArgs >>=
    (\args -> let port = read (head args) :: Int in runServer port)
