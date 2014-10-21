{-# LANGUAGE OverloadedStrings #-}

module Lexer where

import Text.Parsec.Text (Parser)
import Text.Parsec
import qualified Data.Text as T
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Language (emptyDef, LanguageDef)
import Syntax

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser def
          where 
            names = ["break", "case", "catch", "continue", "debugger", "default", "delete", "do", "else", "finally", "for", "function", "if", "in", "instanceof", "new", "return", "switch", "this", "throw", "try", "typeof", "var", "void", "while", "with"]
            ops = ["[", "]", "(", ")", "{", "}", ".", ",", ";", "<", ">", "<=", ">=", "==", "!=", "===", "!==", "+", "-", "*", "%", "++", "--", "<<", ">>", ">>>", "&", "|", "^", "!", "~", "&&", "||", "?", ":", "=", "+=", "-=", "*=", "%=", "<<=", ">>=", ">>>=", "&=", "!=", "^="]
            def = emptyDef {
              Tok.commentStart =  "/*"
              , Tok.commentEnd =  "*/"
              , Tok.commentLine  = "//"
              , Tok.identStart = letter <|> (char '_') <|> (char '$') 
              , Tok.identLetter = alphaNum <|> char '_'
              , Tok.reservedOpNames = ops 
              , Tok.reservedNames = names 
            }


comma :: Parser T.Text 
comma = Tok.comma lexer

ticks :: Parser a -> Parser a
ticks p = between (symbol "`") (symbol "`") p

parens :: Parser a -> Parser a
parens = Tok.parens lexer

quotes :: Parser a -> Parser a 
quotes p = between (char '"') (symbol "\"") p

squares :: Parser a -> Parser a
squares = Tok.squares lexer

braces :: Parser a -> Parser a
braces = Tok.braces lexer

symbol :: T.Text -> Parser T.Text 
symbol = Tok.symbol lexer

dot :: Parser T.Text
dot = Tok.dot lexer

colon :: Parser T.Text 
colon = Tok.colon lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

commaSep1 :: Parser a -> Parser [a]
commaSep1 = Tok.commaSep1 lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

semiSep1 :: Parser a -> Parser [a]
semiSep1 = Tok.semiSep1 lexer

identifier :: Parser T.Text 
identifier = Tok.identifier lexer

reserved :: T.Text -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: T.Text -> Parser ()
reservedOp = Tok.reservedOp lexer

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

number :: Parser (Either Integer Double)
number = Tok.naturalOrFloat lexer

lexeme :: Parser a -> Parser a
lexeme = Tok.lexeme lexer

stringLiteral :: Parser T.Text 
stringLiteral = Tok.stringLiteral lexer
{-stringLiteral :: Parser String-}
{-stringLiteral = q-}
