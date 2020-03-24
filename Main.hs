module Main where

import Parser (parseProgram)
import PrettyPrinter (prettyPrint)
import qualified Data.Text.IO as T

import TypeChecker
import SyntaxTree

import Interpreter

main :: IO ()
main = do
  program <- T.getLine
  case parseProgram "<stdin>" program of
      Left err -> putStrLn err
      Right exp -> do
          let term = toTerm exp
          print (prettyPrint . toExpr $ term)
          putStrLn "Typechecking result:"
          print (typeCheck term)
          putStrLn "Evaluation result:"
          print (prettyPrint . toExpr . evaluate $ term)


