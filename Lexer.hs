{-# language OverloadedStrings #-}
module Lexer where
    
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec as M
import qualified Data.Text as T
import Data.String
import Data.Void
import Control.Applicative

type Parser = M.Parsec Void T.Text

spaceConsumer :: Parser ()
spaceConsumer = L.space C.space1 (L.skipLineComment ";")  empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: T.Text -> Parser T.Text
symbol = L.symbol spaceConsumer

identifier :: Parser T.Text
identifier = do
    start <- C.letterChar
    rest <- M.many C.alphaNumChar
    return $ T.pack (start : rest)
