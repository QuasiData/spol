{-# LANGUAGE OverloadedStrings #-}

module Compiler (test) where

import Lexer

import Data.Text (Text)
import Data.Text qualified as T


test :: IO ()
test = do
    input <- readFile "examples/first.sp"
    print $ lexer $ T.pack input
