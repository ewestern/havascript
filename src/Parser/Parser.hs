{#- LANGUAGE OverloadedStrings #-}
import Text.Parsec
import Control.Applicative ((<*>), (<$>), (*>))

import Lexer

pDecLiteral :: Parser NumLiteral
pDecLiteral = do
  en <- number
  case en of
    (Left i) -> return $ DecLiteral i
    (Right d) -> return $ DecLiteral i

pNumLiteral :: Parser NumLiteral
pNumLiteral = pDecLiteral
-- add Hex Parser

pNullLiteral :: Parser Literal
pNullLiteral = reserverd "null" *> NullLiteral 

pBoolLiteral :: Parser Literal
pBoolLiteral = pTrue <|> pFalse 
  where
    pTrue = reserved "true" *> True
    pFalse = reserved "false" *> False 

pLiteral :: Parser Literal
pLiteral = pBoolLiteral <|> pNullLiteral <|> StringLiteral <|> pNumLiteral

pArrayLiteral :: Parser ArrayLiteral
pArrayLiteral = squares (commaSep pAssignmentExpression)


pObjectLiteral :: Parser ObjectLiteral
pObjectLiteral = braces ( 

pPropertyAssignment :: Parser PropertyAssignment
pPropertyAssignment = try assign <|> get <|> set 
  where
    assign = do
      pn <- pPropertyName
      colon
      ae <- pAssignmentExpression
      return $ PropertyAssignment pn ae 
    get = do
      pn <- pPropertyName
      parens (return ())
      fp <- braces pFunctionBody
      return $ PropertyGet pn fp
    set = do
      pn <- pPropertyName
      pl <- parens pIdentifier 
      fb <- braces pFunctionBody
      return $ PropertySet pn pl fb

pPropertyName :: Parser PropertyName
pPropertyName = (PropertyNameIdentifier <$> identifier) <|> (PropertyNameString <$> stringLiteral) <|> (PropertyNameNumber <$> pNumLiteral)

pMemberExpression :: Parser MemberExpression 
pMemberExpression = (MemberExpressionPrimary <$> pPrimaryExpression) <|> (MemberExpressionFunction <$> pFunctionExpression)
  where 
    pMemberSquare = MemberExpressionSquare <$> pMemberExpression <*> (squares pExpression)
    pMemberDot = MemberExpressionSquare <$> pMemberExpression <*>  
