module TypeChecker where

import Types
import SyntaxTree

import qualified Data.IntMap as M

import Control.Monad.Reader
import Control.Monad.Except

-- Context associating to variables their Type
data TypingContext = TypingContext
                   { depth :: Int
                   , typings :: M.IntMap Type
                   }

makeTypingContext :: TypingContext
makeTypingContext = TypingContext 0 M.empty

addBinding :: Type -> TypingContext -> TypingContext
addBinding ty (TypingContext d typings) =
    let newDepth = d + 1
        newTypings = M.insert (negate newDepth) ty typings
    in TypingContext newDepth newTypings

typeFor :: Index -> TypingContext -> Maybe Type
typeFor idx (TypingContext d typings) = M.lookup (idx - d) typings

data TypeError = ApplyNoFunctionType
               | WrongDomain
               | CondWrongType
               | BranchesHaveDifferentType
               | UnboundVariable
               deriving(Show)

typeCheck' :: Term -> ReaderT TypingContext (Either TypeError) Type
typeCheck' (FunApp t1 t2) = do
    t1Type <- typeCheck' t1
    t2Type <- typeCheck' t2
    case t1Type of
        FuncTy domain codomain | domain == t2Type -> return codomain
                               | otherwise        -> throwError WrongDomain
        _ -> throwError ApplyNoFunctionType

typeCheck' (LambdaAbs _ ty body) = do
    bodyType <- local (addBinding ty) (typeCheck' body)
    return $ FuncTy ty bodyType

typeCheck' (IfTerm cond b1 b2) = do
    condTy <- typeCheck' cond
    if condTy /= BoolTy
    then throwError CondWrongType
    else do
        b1Ty <- typeCheck' b1
        b2Ty <- typeCheck' b2
        if b1Ty /= b2Ty
        then throwError BranchesHaveDifferentType
        else return b1Ty

typeCheck' (Var _ idx) = do
    ty <- reader (typeFor idx)
    case ty of
        Nothing -> throwError UnboundVariable
        Just ty -> return ty

typeCheck' TrueVal = return BoolTy
typeCheck' FalseVal = return BoolTy

typeCheck :: Term -> Either TypeError Type
typeCheck t = runReaderT (typeCheck' t) makeTypingContext
