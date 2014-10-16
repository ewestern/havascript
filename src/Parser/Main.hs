module Main where

import Lexer
import Syntax
import Parser
import Text.ParserCombinators.Parsec.Prim (parseFromFile)
import Text.Parsec
import System.Environment

main = do
  args <- getArgs
  --contents <- readFile $ 
  res <- parseFromFile pFileInput (head args) 
  case res of
    Left s -> putStrLn $ groom s
    Right a -> print a


