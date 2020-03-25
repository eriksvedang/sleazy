{-# LANGUAGE OverloadedStrings #-}

module Main where

import Types
import AST
import ToC
import qualified Data.Text as Text
import Data.Text (Text(..))

ex1 :: AST
ex1 = Func IntTy "main" []
      (Do [ Call "printf" [String "hello, world"]
          , Call "printf" [Name "x"]
          ])

toCStr = Text.unpack . toC

main :: IO ()
main = do
  putStrLn "Sleazy"
  putStrLn "ex1:"
  putStrLn (toCStr ex1)
