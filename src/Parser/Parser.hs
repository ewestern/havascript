{-# LANGUAGE OverloadedStrings #-}
module Parser where
import Text.Parsec
import Control.Applicative ((<*>), (<$>), (*>))
import Text.Parsec.Text
import Syntax
import Lexer


pDecLiteral :: Parser NumLiteral
pDecLiteral = do
  en <- number
  case en of
    (Left i) -> return $ DecLiteral i
    (Right d) -> return $ DecLiteral d

pNumLiteral :: Parser NumLiteral
pNumLiteral = pDecLiteral
-- add Hex Parser

pNullLiteral :: Parser Literal
pNullLiteral = reserved "null" *> NullLiteral 

pBoolLiteral :: Parser Literal
pBoolLiteral = pTrue <|> pFalse 
  where
    pTrue = reserved "true" *> True
    pFalse = reserved "false" *> False 

pLiteral :: Parser Literal
pLiteral = pBoolLiteral <|> pNullLiteral <|> StringLiteral <|> pNumLiteral

pArrayLiteral :: Parser ArrayLiteral
pArrayLiteral = squares (commaSep pAssignmentExpression)


{-pObjectLiteral :: Parser ObjectLiteral-}
{-pObjectLiteral = braces ( -}

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
    set = PropertySet <$> pPropertyName <*> (parens identifier) <*> (braces pFunctionBody)  <?> "Object setter"

pPropertyName :: Parser PropertyName
pPropertyName = (PropertyNameIdentifier <$> identifier) <|> (PropertyNameString <$> stringLiteral) <|> (PropertyNameNumber <$> pNumLiteral)

pMemberExpression :: Parser MemberExpression 
pMemberExpression = (MemberExpressionPrimary <$> pPrimaryExpression) <|> (MemberExpressionFunction <$> pFunctionExpression) <|> pMemberSquare <|> pMemberDot <|> pMemberNew <?> "Member Expression"
  where 
    pMemberSquare = MemberExpressionSquare <$> pMemberExpression <*> (squares pExpression)
    pMemberDot = do
      me <- pMemberExpression
      dot
      i <- identifier
      return $ MemberExpressionSquare me i
    pMemberNew = MemberExpressionNew <$> reserved "new" *> pMemberExpression <*> pArguments

pNewExpression :: Parser NewExpression
pNewExpression = (NewExpressionMember <$> pMemberExpression) <|> (NewExpressionNew <$> reserved "new" *> pNewExpresion)  <?> "New Expression"

pCallExpression :: Parser CallExpression
pCallExpression = try (CallExpressionMember <$>  pMemberExpression <*> pArguments) <|> (CallExpressionArgs <$> pCallExpression <*> pArguments) <|> (CallExpressionExp <$> pCallExpression <*> (squares pExpression)) <|> (CallExpressionIdent <$> pCallExpression <*> dot *> identifier) <?> "Call Expression"    

pArguments :: Parser Arguments
pArguments = parens (return []) <|> parens pArgumentList

pArgumentList :: Parser ArgumentList
pArgumentList = commaSep1 pArgumentExpression

pLHSExpression :: Parser LHSExpression
pLHSExpression = (LHSExpressionNew <$> pNewExpression) <|> (LHSExpressionCall <$> pCallExpression) <?> "LHS Expression"

pPostfixExpression :: Parser PostfixExpression
pPostfixExpression = try (PostfixLHS <$> pLHSExpression) <|> pInc <|> pDec
  -- ensure no line break
  where
    pInc = PostfixInc <$> pLHSExpression <* reservedOp "++"
    pDec = PostfixDec <$> pLHSExpression <* reservedOp "--"

pUnaryExpression :: Parser UnaryExpression
pUnaryExpression = try UnaryExpression <$> pPostfixExpression <|> UnaryExpressionDelete <$> (reserved "delete" *> pUnaryExpression) <|> UnaryExpressionVoid <$> (reserved "void" *> pUnaryExpression ) <|> UnaryExpressionTypeOf <$> (reserved "typeof" *> pUnaryExpression) <|> UnaryExpressionInc <$> (reservedOp "++" *> pUnaryExpression) <|> UnaryExpressionDec <$> (reservedOp "--" *> pUnaryExpression) <|> UnaryExpressionPlus <$> (reservedOp "+" *> pUnaryExpression) <|> UnaryExpressionMinus <$> (reservedOp "-" *> pUnaryExpression) <|> UnaryExpressionTilde <$> (reservedOp "~" *> pUnaryExpression) <|> UnaryExpressionNot <$> (reservedOp "!" *> pUnaryExpression) <?> "Unary Expression"

pMultiplicativeExpression :: Parser MultiplicativeExpression
pMultiplicativeExpression =  try MultUnary <$> pUnaryExpression <|> MultDivide <$> pMultiplicativeExpression <*> (reservedOp "*" *> pUnaryExpression) <|> MultTimes <$> pMultiplicativeExpression <*> (reservedOp "/" *> pUnaryExpression) <|> MultMod <$> pMultiplicativeExpression <*> (reservedOp "%" *> pUnaryExpression) <?> "Multiplicative Expression"

pAdditiveExpression :: Parser AdditiveExpression
pAdditiveExpression = try AddMult <$> pMultiplicativeExpresison <|> AddPlus <$> pAdditiveExpression <*> (reservedOp "+" *> pMultiplicativeExpression) <|> AddMinus <$> pAdditiveExpression <*> (reservedOp "-" *> pMultiplicativeExpression) <?> "Additive Expression"

pShiftExpression :: Parser ShiftExpression
pShiftExpression = ShiftAff        
 

-- placeholders

pAssignmentExpression :: Parser AssignmentExpression
pAssignmentExpression = return AssignmentExpression 

pFunctionExpression :: Parser FunctionExpression
pFunctionExpression = return $ FunctionExpression Nothing Nothing  (FunctionBody Nothing) 
