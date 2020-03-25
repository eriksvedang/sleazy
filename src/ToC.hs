{-# LANGUAGE OverloadedStrings #-}

module ToC where

import Types
import AST
import qualified Data.Text as Text
import Data.Text (Text(..))
import qualified Data.Map as Map
import Data.Map (Map(..))

data CallMode = FExpr | Binary | Unary deriving (Eq, Show)

toC :: AST -> Text
toC (Func ret name params body) =
  tyToC ret <> " " <> name <> "(" <> Text.intercalate ", " (map paramToC params) <> ") {\n" <> toC body <> "}"
toC (Do forms) =
  Text.concat (map ((<> ";\n") . toC) forms)
toC (Call name args) =
  case callMode name of
    FExpr -> name <> "(" <> Text.intercalate ", " (map toC args) <> ")"
    Binary -> let [a, b] = args
              in  toC a <> " " <> name <> " " <> toC b
    Unary -> let [a] = args
             in  toC a <> name
toC (Return expr) =
  "return " <> toC expr
toC (Name name) =
  name
toC (String s) =
  "\"" <> s <> "\""
toC (Decl t n expr) =
  tyToC t <> " " <> n <> " = " <> toC expr
toC (Control n exprs body) =
  n <> "(" <> Text.intercalate "; " (map toC exprs) <> ") {\n" <> toC body <> "}"

paramToC :: Param -> Text
paramToC (Param t n) =
  tyToC t <> " " <> n

tyToC :: Ty -> Text
tyToC (NamedTy name) = name
tyToC IntTy = "int"
tyToC FloatTy = "float"
tyToC VoidTy = "void"

callMode :: Text -> CallMode
callMode functionName =
  case Map.lookup functionName mappings of
    Just found -> found
    Nothing -> FExpr
  where mappings = Map.fromList [ ("+", Binary)
                                , ("+=", Binary)
                                , ("-", Binary)
                                , ("-=", Binary)
                                , ("*", Binary)
                                , ("*=", Binary)
                                , ("/", Binary)
                                , ("/=", Binary)
                                , ("++", Unary)
                                , ("--", Unary)
                                , ("<", Binary)
                                , ("<=", Binary)
                                , (">", Binary)
                                , (">=", Binary)
                                ]
