{-# LANGUAGE OverloadedStrings #-}

-- | Note: This is just an example of how to emit to other languages, besides C.

module ToLisp where

import Types
import AST
import qualified Data.Text as Text
import Data.Text (Text(..))
import qualified Data.Map as Map
import Data.Map (Map(..))

addIndent :: Integer -> Text
addIndent lvl = build lvl
  where build 0 = ""
        build x = Text.cons ' ' (build (x - 1))

toLisp :: Integer -> AST -> Text
toLisp indent (Func _ name params body) =
  addIndent indent <>
  "(define " <> name <> " (" <> Text.intercalate " " (map paramToLisp params) <> ")\n" <> toLisp (indent + 2) body <> ")"
toLisp indent (Do forms) =
  addIndent indent <> "(do\n" <> addIndent (indent + 2) <>
  Text.intercalate ("\n" <> addIndent (indent + 2)) (map (toLisp (indent + 2)) forms)
  <> ")"
toLisp indent (Call name args) =
  "(" <>  name <> " " <> Text.intercalate " " (map (toLisp indent) args) <> ")"
toLisp indent (Return expr) =
  toLisp indent expr
toLisp indent (Name name) =
  name
toLisp indent (String s) =
  "\"" <> s <> "\""
toLisp indent (Decl t n expr) =
  "(define " <> n <> " " <> toLisp indent expr <> ")"
toLisp indent (Control n exprs body) =
  "(" <> n <> " " <> Text.intercalate " " (map (toLisp indent) exprs) <> "\n" <>
  toLisp (indent + 2) body <> ")"

paramToLisp :: Param -> Text
paramToLisp (Param _ n) = n
