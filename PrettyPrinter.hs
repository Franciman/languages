module PrettyPrinter where

import SyntaxTree
import Types

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Set as S
import Data.List (intersperse)

prettyPrintType :: Type -> B.Builder
prettyPrintType BoolTy = B.fromString "Bool"
prettyPrintType (FuncTy t1 t2) = prettyPrintType t1
                              <> B.fromString " -> "
                              <> prettyPrintType t2

prettyPrint' :: Expr -> B.Builder
prettyPrint' (IfExpr cond b1 b2) =
    B.fromString "if(" <> prettyPrint' cond <> B.fromString "){"
                       <> prettyPrint' b1
                       <> B.fromString "} else {"
                       <> prettyPrint' b2 <> B.fromString "}"

prettyPrint' (Lambda args body) =
    let as = intersperse (B.fromString ", ")
           . map (\(name, ty) -> B.fromText name
                              <> B.singleton ':'
                              <> prettyPrintType ty)
           $ S.toList args

    in B.singleton '[' <> mconcat as <> B.fromString "]"  <> prettyPrint' body




prettyPrint' (FunAppExpr f a) = prettyPrint' f <> B.singleton ' ' <> prettyPrint' a
prettyPrint' TrueLit = B.fromString "true"
prettyPrint' FalseLit = B.fromString "false"
prettyPrint' (Identifier i) = B.fromText i

prettyPrint :: Expr -> T.Text
prettyPrint = B.toLazyText . prettyPrint'
