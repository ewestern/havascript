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
    pTrue = reserved "true" >> return True
    pFalse = reserved "false" >> return False 

pLiteral :: Parser Literal
pLiteral = pBoolLiteral <|> pNullLiteral <|> StringLiteral <|> pNumLiteral

pArrayLiteral :: Parser ArrayLiteral
pArrayLiteral = squares (commaSep pAssignmentExpression)

pPropertyAssignment :: Parser PropertyAssignment
pPropertyAssignment = try assign <|> get <|> set 
  where
    assign = PropertyAssignment <$> pPropertyName <*> (colon *> pAssignmentExpression) 
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
    pMemberDot = MemberExpressionSquare <$> pMemberExpression <*> (dot *> identifier) 
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
pShiftExpression = try ShiftAdd <$> pAdditiveExpression <|> ShiftRight <$> pShiftExpression <*> (reservedOp ">>" *> pAdditiveExpression) <|> ShiftLeft <$> pShiftExpression <*> (reservedOp "<<" *> pAdditiveExpression) <|> ShiftZ <$> pShiftExpression <*> (reservedOp ">>>" *> pAdditiveExpression) <?> "Shift Expression"

pRelationalExpression :: Parser RelationalExpression 
pRelationalExpression = try RelationShift <$> pShiftExpression <|> RelationLT <$> pRelationalExpression <*> (reservedOp "<" *> pShiftExpression) <|> RelationGT <$> pRelationalExpression <*> (reservedOp ">" *> pShiftExpression) <|> RelationLTE <$> pRelationalExpression <*> (reservedOp "<=" *> pShiftExpression) <|> RelationGTE <$> pRelationalExpression <*> (reservedOp ">=" *> pShiftExpression) <|> RelationIO <$> pRelationalExpression <*> (reserved "instanceof" *> pShiftExpression) <|> pRelationalExpression <*> (reserved "in" *> pShiftExpression) <?> "Relational Expression"


pRelationalExpressionNI :: Parser RelationalExpressionNI
pRelationalExpressionNI = try RelationShiftNI <$> pShiftExpression <|> RelationLTNI <$> pRelationalExpressionNI <*> (reservedOp "<" *> pShiftExpression) <|> RelationGTNI <$> pRelationalExpressionNI <*> (reservedOp ">" *> pShiftExpression) <|> RelationLTENI <$> pRelationalExpressionNI <*> (reservedOp "<=" *> pShiftExpression) <|> RelationGTENI <$> pRelationalExpressionNI <*> (reservedOp ">=" *> pShiftExpression) <|> RelationIONI <$> pRelationalExpression <*> (reserved "instanceof" *> pShiftExpression) <?> "Relational Expression NI"

pEqualityExpression :: Parser EqualityExpression
pEqualityExpression = try EqualityExp <$> pRelationalExpression <|> EqalityExpEq <$> pEqualityExp <*> (reservedOp "==" *> pRelationalExpression) <|> EqualityExpNE <$> pEqualityExpression <*> (reservedOp "!=" pRelationalExpression) <|>   EqualityExpEqq <$> pEqualityExpression <*> (reservedOp "===" *> pRelationalExpression) <|> EqualityExpNEE <$> pEqualityExpresion <*> (reservedOp "!==" *> pRelationalExpression) <?> "Equality Expression"

-- EqualityNI
--
pBitwiseAndExp :: Parser BitwiseAndExp
pBitwiseAndExp = try BitwiseAndEq <$> pEqualityExpression <|> BitwiseAnd <$> pBitwiseAndExp <*> (reservedOp "&" *> pEqualityExpression) <?> "Bitwise And Exp"

--- BitwiseAndNI
--
pBitwiseXORExp :: Parser BitwiseXORExp
pBitwiseXORExp = try BitwiseXORExp <$> pBitwiseAndExp <|> BitwiseXORExpHat <$> pBitwiseXORExp <*> (reservedOp "^" *> pBitwiseAndExp) <?> "Bitwise XOR Exp"

pBitwiseOrExp :: Parser BitwiseOrExp
pBitwiseOrExp = try BitwiseOrExp <$> pBitwiseXORExp <|> BitwiseOrExpPipe <$> pBitwiseOrExp <*> (reservedOp "|" *> pBitwiseXORExp) <?> "Reserved OR Exp"

pLogicalAndExp :: Parser LogicalAndExp
pLogicalAndExp = try LogicalAndExp <$> pBitwiseORExp <|> LogicalAndExpAmp <$> pLogicalAndExp <*> (reservedOp "&" *> pBitwiseORExp) <?> "Logical And Exp"

pLogicalOrExp :: Parser LogicalOrExp
pLogicalOrExp = try LogicalOrExp <$> pLogicalAndExp <|> LogicalOrExpDPipe <$> pLogicalOrExp <*> (reservedOp "||" *> pLogicalAndExp) <?> "Logical Or Exp"   

pConditionalExp :: Parser ConditionalExp
pConditionalExp = try ConditionalExp <$> pLogicalOrExp <|> ConditionalExpTern <$> pLogicalOrExp <*> (reservedOp "?" *> pAssignmentExpression) <*> (reservedOp ":" *> pAssignmentExpression) <?> "Conditional Exp"

pAssignmentExpression :: Parser AssignmentExpression
pAssignmentExpression = try AssignmentExpression <$> pConditionalExp <|> AssignmentExpEq <$> pLHSExpression <*> (reservedOp "=" *> pAssignmentExpression) <|> AssignmentExpOp <$> pLHSExpression <*> pAssignmentOperator <*> pAssignmentExpression <?> "Assignment Expression"

pAssignmentOperator :: Parser AssignmentOperator
pAssignmentOperator = reservedOp "*=" >> return TimesEquals <|> reservedOp "/=" >> return DivEquals <|> reservedOp "%=" >> return ModEquals <|> reservedOp "-=" >> return MinusEquals <|> reservedOp "<<=" return LSEquals <|> reservedOp ">>=" return RSEquals <|> reservedOp ">>>=" return RSSEqual <|> reservedOp "&=" >> return AndEquals <|> reservedOp "^=" return XOREquals <|> reservedOp "|=" return OREquals  

pExpression :: Parser Expression
pExpression = commaSep1 pAssignmentExpression 

pStatement :: Parser Statement
pStatement = try BlockStmt <$> pBlock <|> VariableStmt <$> pVariableStatement <|> EmptyStmt <$> pEmptyStatement <|>    

-- placeholders

{-pAssignmentExpression :: Parser AssignmentExpression-}
{-pAssignmentExpression = return AssignmentExpression -}
{-pFunctionExpression :: Parser FunctionExpression-}
{-pFunctionExpression = return $ FunctionExpression Nothing Nothing  (FunctionBody Nothing) -}
