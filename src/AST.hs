module AST where

import Types
import qualified Data.Text as Text
import Data.Text (Text(..))

data AST = Func { funcRetTy :: Ty
                , funcName :: Text
                , funcParams :: [Param]
                , funcBody :: AST
                }
         | Do { doForms :: [AST]
              }
         | Call { callName :: Text
                , callArgs :: [AST]
                }
         | Return { returnExpr :: AST
                  }
         | Name { name :: Text
                }
         | String { str :: Text
                  }
         deriving (Show, Eq)

data Param = Param { paramType :: Ty
                   , paramName :: Text
                   }
           deriving (Show, Eq)
