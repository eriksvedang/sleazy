module AST where

import Types
import qualified Data.Text as Text
import Data.Text (Text(..))

data AST = Func { funcRetTy :: Ty
                , funcName :: Text
                , funcParams :: [AST]
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
         | Decl { declTy :: Ty
                , declName :: Text
                , declExpr :: AST
                }
         | Control { ctrlName :: Text
                   , ctrlExprs :: [AST]
                   , ctlrBody :: AST
                   }
         | Param { paramType :: Ty
                 , paramName :: Text
                 }
         | LogicVar { logicVarName :: Text -- Used by the code transformations
                    }
         deriving (Show, Eq)
