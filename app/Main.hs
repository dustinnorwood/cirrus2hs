module Main (main) where

import Lib
import System.Environment (getArgs)

main :: IO ()
main = do
    date <- getArgs >>= \args -> case args of
        [] -> pure ""
        (d:_) -> pure d
    someFunc date
