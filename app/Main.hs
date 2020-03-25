{-# LANGUAGE OverloadedStrings #-}

module Main where

import Types
import AST
import ToC
import ToLisp
import Transforms
import qualified Data.Text as Text
import Data.Text (Text(..))

ex1 :: AST
ex1 = Func IntTy "main" []
      (Do [ Call "printf" [String "hello, world"]
          , Call "printf" [Name "x"]
          ])

ex2 = Func VoidTy "f" [Param IntTy "x", Param IntTy "y"]
      (Do [ Control "unless" [Call ">" [Name "x", Name "y"]]
            (Do [Return (Call "+" [Name "x", Name "y"])])])

ex3 = Func IntTy "sum" [Param (NamedTy "int*") "arr"]
      (Do [ Decl IntTy "total" (Name "0")
          , Control "for" [Decl IntTy "i" (Name "0"), Call "<" [Name "i", Name "10"], Call "++" [Name "i"]]
            (Do [Call "+=" [(Name "total"), (Name "1")]])
          , Return (Name "total")
          ])

toCStr = Text.unpack . toC . transform
toLispStr = Text.unpack . toLisp 0

main :: IO ()
main = do
  -- putStrLn "---"
  -- putStrLn (toCStr ex1)
  putStrLn "---"
  putStrLn (toCStr ex2)
  putStrLn "---"
  -- putStrLn (toCStr ex3)
  -- putStrLn "------"
  -- putStrLn (toLispStr ex1)
  -- putStrLn "---"
  -- putStrLn (toLispStr ex2)
  -- putStrLn "---"
  -- putStrLn (toLispStr ex3)
  -- putStrLn "---"
