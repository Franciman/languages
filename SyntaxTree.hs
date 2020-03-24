module SyntaxTree where

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S
import Data.Maybe (fromJust)
import Control.Monad.Reader
import Data.List (foldl')
import Data.Char (chr)

import Types


type FuncArgs = S.Set (T.Text, Type)
-- A more concrete syntax tree
data Expr = IfExpr Expr Expr Expr
          | Lambda FuncArgs Expr
          | FunAppExpr Expr Expr
          | TrueLit
          | FalseLit
          | Identifier T.Text


-- This is the more abstract syntax tree that we use as internal
-- representation of the program.
-- We use de Brujin indices to represent variables
-- We represent indices as integers
type Index = Int

-- Ironically we retain names in the nameless representation,
-- so that we can easily reconstruct the original expr using the same names
data Term = FunApp Term Term
          | LambdaAbs T.Text Type Term
          | IfTerm Term Term Term
          | Var T.Text Index
          | TrueVal
          | FalseVal
          deriving(Show)

-- Here we define how to convert between the two syntaxes.
-- This is one of the two pieces that provide the semantics of the program.
-- This part is only concerned with syntactic transformations,
-- we'd like to see Expr as a Term with a lot of sugar on it.
-- The value of beauty is undeniable and so we require that the user
-- be able to work with a beautiful syntax.
-- While internally we value simplicity and consistence, even if this means
-- austerity, this will make the internal definitions and proofs more beautiful.
-- So in an attempt to please everybody we end up with two different syntaxes.
-- And here we define a syntactic morphism between the two representations.

-- We start by defining how each Expr is converted to a Term.

-- a (\x. a x)

-- This context keeps track of variables and their indices
-- it helps in conversion from named to nameless form
data Context = Context
             { depth :: Int -- The number of lambda abstractions we have met
             , bindings :: M.Map T.Text Index
             }

-- Enter a new binding scope
-- increase depth
bindVariable :: T.Text -> Context -> Context
bindVariable name (Context d bs) = let newDepth = d + 1 -- increase depth
                                       newBindings = M.insert name (negate newDepth) bs
                                   in Context newDepth newBindings

indexFor :: T.Text -> Context -> Maybe Index
indexFor name (Context d bindings) = (+d) <$> M.lookup name bindings

-- Unsafe version
-- TODO: find a way to statically ensure things will work fine, I know this is hard, tho
indexFor' :: T.Text -> Context -> Index
indexFor' name  = fromJust . indexFor name


freeVarsExpr :: Expr -> S.Set T.Text
freeVarsExpr (IfExpr c b1 b2) = freeVarsExpr c `S.union` freeVarsExpr b1 `S.union` freeVarsExpr b2
freeVarsExpr (Lambda args body) = freeVarsExpr body `S.difference` S.map fst args -- The args are not free vars!
freeVarsExpr (FunAppExpr t1 t2) = freeVarsExpr t1 `S.union` freeVarsExpr t2
freeVarsExpr (Identifier t) = S.singleton t
freeVarsExpr TrueLit = S.empty
freeVarsExpr FalseLit = S.empty

makeContext :: Expr -> Context
makeContext exp = let vars = S.toList (freeVarsExpr exp) -- Collect all free vars
                      bindings = M.fromList $ zip vars [0..] -- Assign a unique index to each free var
                  in Context 0 bindings

convertToTerm :: Expr -> Reader Context Term
convertToTerm (IfExpr cond b1 b2) = IfTerm <$> convertToTerm cond <*> convertToTerm b1 <*> convertToTerm b2
convertToTerm (FunAppExpr f a) = FunApp <$> convertToTerm f <*> convertToTerm a
convertToTerm TrueLit = return TrueVal
convertToTerm FalseLit = return FalseVal
convertToTerm (Lambda args body) = do
    -- we treat multivalue functions as curried, so unary functions returning functions
    let args' = S.toList args
    let curriedArgs = foldl' (\rest (argName, argType) -> LambdaAbs argName argType . rest) id (args')
    -- Create a converter that keeps track of the new bound variables
    let bindArgs = foldl' (\rest arg -> local (bindVariable arg) . rest) id (fst <$> args')
    body' <- bindArgs $ convertToTerm body
    return $ curriedArgs body'

convertToTerm (Identifier i) = Var i <$> reader (indexFor' i)

-- Convert external representation to internal
toTerm :: Expr -> Term
toTerm exp = runReader (convertToTerm exp) (makeContext exp)

-- Convert internal representation to external
toExpr :: Term -> Expr
toExpr (IfTerm cond b1 b2) = IfExpr (toExpr cond) (toExpr b1) (toExpr b2)
toExpr (LambdaAbs name ty body) = Lambda (S.singleton (name, ty)) (toExpr body)
toExpr (FunApp f a) = FunAppExpr (toExpr f) (toExpr a)
toExpr (Var name i) = Identifier name
toExpr TrueVal = TrueLit
toExpr FalseVal = FalseLit

