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

ex2 = Func VoidTy "f" [Param IntTy "x", Param IntTy "y"]
      (Do [ Return (Call "+" [Name "x", Name "y"])])

toCStr = Text.unpack . toC

main :: IO ()
main = do
  putStrLn "---"
  putStrLn (toCStr ex1)
  putStrLn "---"
  putStrLn (toCStr ex2)
  putStrLn "---"
