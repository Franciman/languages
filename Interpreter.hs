module Interpreter where

import SyntaxTree

-- Here we have the semantics for the Term language

-- Some terms are treated as values
isValue :: Term -> Bool
isValue TrueVal = True
isValue FalseVal = True
isValue LambdaAbs{} = True
isValue _ = False

-- Term substitution is an important concept to get right,
-- in order to correctly capture the function application semantics.

-- shift free vars to avoid variable capture.
-- the cutoff term is to discern between bound and unbound vars
shift :: Int -> Int -> Term -> Term
shift _ _ TrueVal = TrueVal
shift _ _ FalseVal = FalseVal
shift cutoff depth (Var name index) | index < cutoff = Var name index -- It is a bound variable, no need to shift
                                    | otherwise      = Var name (index + depth) -- It is a free var, shift it

shift cutoff depth (LambdaAbs name ty body) = LambdaAbs name ty (shift (cutoff + 1) depth body)
shift cutoff depth (FunApp f a) = FunApp (shift cutoff depth f) (shift cutoff depth a)

shift' :: Int -> Term -> Term
shift' = shift 0

substitute :: Int -> Term -> Term -> Term
substitute depth (Var name index) s | index == depth = shift' depth s
                                    | otherwise = Var name index

substitute _ TrueVal s = TrueVal
substitute _ FalseVal s = FalseVal
substitute depth (IfTerm cond b1 b2) s = IfTerm (substitute depth cond s) (substitute depth b1 s) (substitute depth b2 s)
substitute depth (LambdaAbs name ty body) s = LambdaAbs name ty (substitute (depth + 1) body s)
substitute depth (FunApp f a) s = FunApp (substitute depth f s) (substitute depth a s)

evaluate :: Term -> Term
evaluate (IfTerm cond b1 b2) =
    let vCond = evaluate cond
    in case vCond of
        TrueVal -> evaluate b1
        FalseVal -> evaluate b2
        _        -> error "Unreachable"

evaluate (FunApp f a) =
    let vArg = evaluate a
        vFun = evaluate f
    in case vFun of
          (LambdaAbs _ _ body) -> evaluate . shift' (negate 1) $ substitute 0 body (shift' 1 vArg)
          _                    -> error "Unreachable"

evaluate t | isValue t = t
           | otherwise = error "Unreachable"
