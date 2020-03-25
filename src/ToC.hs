{-# LANGUAGE OverloadedStrings #-}

module ToC where

import Types
import AST
import qualified Data.Text as Text
import Data.Text (Text(..))

toC :: AST -> Text
toC (Func ret name params body) =
  tyToC ret <> " " <> name <> "(" <> Text.intercalate ", " (map paramToC params) <> ") {\n" <> toC body <> "}"
toC (Do forms) =
  Text.intercalate ";\n" (map toC forms) <> ";\n"
toC (Call name args) =
  name <> "(" <> Text.intercalate ", " (map toC args) <> ")"
toC (Name name) =
  name
toC (String s) =
  "\"" <> s <> "\""

paramToC :: Param -> Text
paramToC (Param t n) =
  tyToC t <> " " <> n

tyToC :: Ty -> Text
tyToC (NamedTy name) = name
tyToC IntTy = "int"
tyToC FloatTy = "float"
