module Main where

import Web

main :: IO ()
main = runServer $ \msg -> return msg
    