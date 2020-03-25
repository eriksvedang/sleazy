{-# LANGUAGE OverloadedStrings #-}

module Transforms where

import AST
import qualified Data.Text as Text
import Data.Text (Text(..))
import Data.Maybe (mapMaybe)
import qualified Data.Map as Map
import Data.Map (Map(..))
import Debug.Trace (trace)

data Transform = Transform { transformIn :: AST  -- If some syntax matches this...
                           , transformOut :: AST -- ...this is what gets output.
                           }
                 deriving (Show, Eq)

unlessTransform =
  Transform
  (Control "unless" [LogicVar "expr"] (LogicVar "body"))
  (Control "if" [Call "!" [LogicVar "expr"]] (LogicVar "body"))

xTransform =
  Transform
  (Call ">" [(LogicVar "a"), (LogicVar "b")])
  (Name "REDACTED")

transforms = [unlessTransform, xTransform]

transform :: AST -> AST
transform ast =
  case applyTransforms transforms ast of
    (Func ret name params body) -> Func ret name params (transform body)
    (Do exprs) -> Do (map transform exprs)
    (Call name args) -> Call name (map transform args)
    (Control name exprs body) -> Control name (map transform exprs) (transform body)
    x -> x

applyTransforms :: [Transform] -> AST -> AST
applyTransforms transforms expr =
  case mapMaybe (matchExprWithTransform expr) transforms of
    [] -> expr
    [(transform, singleMappings)] ->
      --trace ("Got single transform: " ++ show transform ++ " and mappings: " ++ show singleMappings) $
      substitute singleMappings (transformOut transform)
    multiple -> error ("Multiple matching transforms: " ++ show multiple)

matchExprWithTransform :: AST -> Transform -> Maybe (Transform, Map Text AST)
matchExprWithTransform ast transform = match' ast (transformIn transform)
  where match' (Control a exprsA bodyA) (Control b exprsB bodyB) =
          if a == b
          then Just (transform, Map.fromList (concat (zipWith createMapping exprsA exprsB) ++ createMapping bodyA bodyB))
          else Nothing
        match' (Call a argsA) (Call b argsB) =
          if a == b
          then Just (transform, Map.fromList (concat (zipWith createMapping argsA argsB)))
          else Nothing
        match' _ _ =
          Nothing

createMapping :: AST -> AST -> [(Text, AST)]
createMapping expr (LogicVar var) = [(var, expr)]
createMapping _ _ = []

substitute :: Map Text AST -> AST -> AST
substitute mappings expr =
  case expr of
    (Func ret name params body) -> Func ret name params (sub body)
    (Control name exprs body) -> Control name (map sub exprs) (sub body)
    (Call name args) -> Call name (map sub args)
    (LogicVar name) ->
      case Map.lookup name mappings of
        Just found -> found
        Nothing -> error ("Found no substitution for logic var '" ++ Text.unpack name ++ "'\nMappings: " ++ show mappings)
    _ -> expr

  where sub :: AST -> AST
        sub = substitute mappings
