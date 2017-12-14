module Main where

import Web
import Http

main :: IO ()
--main = runServer $ \msg -> return msg
main = print $ parseRequest "line\r\nheader1\r\nheader2\r\nbody"