module Types where

-- Type representation
data Type = BoolTy
          | FuncTy Type Type
          deriving(Eq, Ord, Show)
