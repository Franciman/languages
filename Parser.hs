{-# language OverloadedStrings #-}
module Parser where
    
import Lexer
import SyntaxTree
import Types

import qualified Text.Megaparsec as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Functor

import Data.List (foldl')

parseTerm :: Parser Expr
parseTerm = parseIf M.<|> parseFunction M.<|> parseLiteral M.<|> parseIdentifier M.<|> parseSubExpr

parseSubExpr :: Parser Expr
parseSubExpr = M.between (symbol "(") (symbol ")") parseExpr

parseIf :: Parser Expr
parseIf = do
    symbol "if"
    cond <- parseExpr
    symbol "{"
    branch1 <- parseExpr
    symbol "}"
    symbol "else"
    symbol "{"
    branch2 <- parseExpr
    symbol "}"
    return $ IfExpr cond branch1 branch2

parseFunction :: Parser Expr
parseFunction = do
    symbol "["
    args <- parseArg `M.sepBy` symbol ","
    symbol "]"
    Lambda (S.fromList args) <$> parseExpr

parseArg :: Parser (T.Text, Type)
parseArg = do
    name <- lexeme identifier
    symbol ":"
    ty <- parseType
    return (name, ty)


parseTypeAtom :: Parser Type
parseTypeAtom =
    (M.between (symbol "(") (symbol ")") parseType) M.<|> (symbol "Bool" $> BoolTy)

parseType :: Parser Type
parseType = do
    atoms <- parseTypeAtom `M.sepBy` symbol "->"
    case atoms of
        [] -> M.empty
        (a:as) -> return $ foldl' FuncTy a as


parseLiteral :: Parser Expr
parseLiteral = (TrueLit <$ symbol "true") M.<|> (FalseLit <$ symbol "false")

parseIdentifier :: Parser Expr
parseIdentifier = Identifier <$> lexeme identifier

parseExpr :: Parser Expr
parseExpr = do
    terms <- M.some parseTerm
    case terms of
        [] -> M.empty
        (a:as) -> return $ foldl' FunAppExpr a as

parseProgram :: String -> T.Text -> Either String Expr
parseProgram programName input = case M.runParser (spaceConsumer *> parseExpr) programName input of
    Left err -> Left (M.errorBundlePretty err)
    Right exp -> Right exp
