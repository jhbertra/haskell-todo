module Main where

import Web
import Network.Protocol.Http.Parser

main :: IO ()
main = runServer $ \msg -> return msg
    