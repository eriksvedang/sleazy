module Types where

import qualified Data.Text as Text
import Data.Text (Text(..))

data Ty = NamedTy Text
        | IntTy
        | FloatTy
        | VoidTy
        deriving (Show, Eq)
