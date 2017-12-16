module Main where

import Web
import Http
import System.Environment

main :: IO ()
--main = runServer $ \msg -> return msg
main = print $ parseRequest "GET asdf HTTP/1.1\r\nheader1: value\r\nheader2: value\r\n\r\nbody"