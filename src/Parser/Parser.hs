{-# LANGUAGE OverloadedStrings #-}

module Parser where
import Text.Parsec
import Control.Applicative hiding ((<|>), many)
{-import Control.Applicative ((<*>), (<$>), (*>), (<*))-}
{-import Text.Parsec.Text-}
import Syntax
import Lexer

pPrimaryExpression :: Parser PrimaryExpression
pPrimaryExpression = (reserved "this" >> return This) <|> IdentifierExp <$> identifier <|> LiteralExp <$> pLiteral <|> ArrayExp <$> pArrayLiteral <|> ObjectExp <$> pObjectLiteral <|> ExpressionExp <$> pExpression 

pDecLiteral :: Parser NumLiteral
pDecLiteral = do
  en <- number
  case en of
    (Left i) -> return $ DecLiteral . fromIntegral $  i
    (Right d) -> return $ DecLiteral d

pNumLiteral :: Parser NumLiteral
pNumLiteral = pDecLiteral
-- add Hex Parser

pNullLiteral :: Parser Literal
pNullLiteral = reserved "null" >> return NullLiteral 

pBoolLiteral :: Parser Literal
pBoolLiteral = BooleanLiteral <$> pTrue <|> BooleanLiteral <$> pFalse 
  where
    pTrue = reserved "true" >> return True
    pFalse = reserved "false" >> return False 

pLiteral :: Parser Literal
pLiteral = pBoolLiteral <|> pNullLiteral <|> StringLiteral <$> stringLiteral <|> NumericLiteral <$> pNumLiteral

pArrayLiteral :: Parser ArrayLiteral
pArrayLiteral = squares (commaSep pAssignmentExpression)

pObjectLiteral :: Parser ObjectLiteral
pObjectLiteral = braces pPropertyNameAndValueList

pPropertyNameAndValueList :: Parser PropertyNameAndValueList
pPropertyNameAndValueList = many pPropertyAssignment

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
    pMemberDot = MemberExpressionDot <$> pMemberExpression <*> (dot *> identifier) 
    pMemberNew = reserved "new" *> (MemberExpressionNew <$> pMemberExpression <*> pArguments)

pNewExpression :: Parser NewExpression
pNewExpression = (NewExpressionMember <$> pMemberExpression) <|> reserved "new" *> (NewExpressionNew <$> pNewExpression) <?> "New Expression"

pCallExpression :: Parser CallExpression
pCallExpression = try (CallExpressionMember <$>  pMemberExpression <*> pArguments) <|> CallExpressionArgs <$> pCallExpression <*> pArguments <|> CallExpressionExp <$> pCallExpression <*> (squares pExpression) <|> CallExpressionIdent <$> pCallExpression <*> (dot *> identifier) <?> "Call Expression"    

pArguments :: Parser Arguments
pArguments = parens (return []) <|> parens pArgumentList

pArgumentList :: Parser ArgumentList
pArgumentList = commaSep1 pAssignmentExpression

pLHSExpression :: Parser LHSExpression
pLHSExpression = (LHSExpressionNew <$> pNewExpression) <|> (LHSExpressionCall <$> pCallExpression) <?> "LHS Expression"

pPostfixExpression :: Parser PostfixExpression
pPostfixExpression = try (PostfixLHS <$> pLHSExpression) <|> pInc <|> pDec
  -- ensure no line break
  where
    pInc = PostfixInc <$> pLHSExpression <* reservedOp "++"
    pDec = PostfixDec <$> pLHSExpression <* reservedOp "--"

pUnaryExpression :: Parser UnaryExpression
pUnaryExpression = try (UnaryExpression <$> pPostfixExpression) <|> UnaryExpressionDelete <$> (reserved "delete" *> pUnaryExpression) <|> UnaryExpressionVoid <$> (reserved "void" *> pUnaryExpression ) <|> UnaryExpressionTypeOf <$> (reserved "typeof" *> pUnaryExpression) <|> UnaryExpressionInc <$> (reservedOp "++" *> pUnaryExpression) <|> UnaryExpressionDec <$> (reservedOp "--" *> pUnaryExpression) <|> UnaryExpressionPlus <$> (reservedOp "+" *> pUnaryExpression) <|> UnaryExpressionMinus <$> (reservedOp "-" *> pUnaryExpression) <|> UnaryExpressionTilde <$> (reservedOp "~" *> pUnaryExpression) <|> UnaryExpressionNot <$> (reservedOp "!" *> pUnaryExpression) <?> "Unary Expression"

pMultiplicativeExpression :: Parser MultiplicativeExpression
pMultiplicativeExpression =  try (MultUnary <$> pUnaryExpression) <|> MultDivide <$> pMultiplicativeExpression <*> (reservedOp "*" *> pUnaryExpression) <|> MultTimes <$> pMultiplicativeExpression <*> (reservedOp "/" *> pUnaryExpression) <|> MultMod <$> pMultiplicativeExpression <*> (reservedOp "%" *> pUnaryExpression) <?> "Multiplicative Expression"

pAdditiveExpression :: Parser AdditiveExpression
pAdditiveExpression = try (AddMult <$> pMultiplicativeExpression) <|> AddPlus <$> pAdditiveExpression <*> (reservedOp "+" *> pMultiplicativeExpression) <|> AddMinus <$> pAdditiveExpression <*> (reservedOp "-" *> pMultiplicativeExpression) <?> "Additive Expression"

pShiftExpression :: Parser ShiftExpression
pShiftExpression = try (ShiftAdd <$> pAdditiveExpression) <|> ShiftRight <$> pShiftExpression <*> (reservedOp ">>" *> pAdditiveExpression) <|> ShiftLeft <$> pShiftExpression <*> (reservedOp "<<" *> pAdditiveExpression) <|> ShiftZ <$> pShiftExpression <*> (reservedOp ">>>" *> pAdditiveExpression) <?> "Shift Expression"

pRelationalExpression :: Parser RelationalExpression 
pRelationalExpression = try (RelationShift <$> pShiftExpression) <|> RelationLT <$> pRelationalExpression <*> (reservedOp "<" *> pShiftExpression) <|> RelationGT <$> pRelationalExpression <*> (reservedOp ">" *> pShiftExpression) <|> RelationLTE <$> pRelationalExpression <*> (reservedOp "<=" *> pShiftExpression) <|> RelationGTE <$> pRelationalExpression <*> (reservedOp ">=" *> pShiftExpression) <|> RelationIO <$> pRelationalExpression <*> (reserved "instanceof" *> pShiftExpression) <|> RelationIn <$> pRelationalExpression <*> (reserved "in" *> pShiftExpression) <?> "Relational Expression"


pRelationalExpressionNI :: Parser RelationalExpressionNI
pRelationalExpressionNI = try (RelationShiftNI <$> pShiftExpression) <|> RelationLTNI <$> pRelationalExpressionNI <*> (reservedOp "<" *> pShiftExpression) <|> RelationGTNI <$> pRelationalExpressionNI <*> (reservedOp ">" *> pShiftExpression) <|> RelationLTENI <$> pRelationalExpressionNI <*> (reservedOp "<=" *> pShiftExpression) <|> RelationGTENI <$> pRelationalExpressionNI <*> (reservedOp ">=" *> pShiftExpression) <|> RelationIONI <$> pRelationalExpressionNI <*> (reserved "instanceof" *> pShiftExpression) <?> "Relational Expression NI"

pEqualityExpression :: Parser EqualityExpression
pEqualityExpression = try (EqualityExp <$> pRelationalExpression) <|> EqualityExpEq <$> pEqualityExpression <*> (reservedOp "==" *> pRelationalExpression) <|> EqualityExpNE <$> pEqualityExpression <*> (reservedOp "!=" *> pRelationalExpression) <|>   EqualityExpEqq <$> pEqualityExpression <*> (reservedOp "===" *> pRelationalExpression) <|> EqualityExpNEE <$> pEqualityExpression <*> (reservedOp "!==" *> pRelationalExpression) <?> "Equality Expression"

-- EqualityNI
--
pBitwiseAndExp :: Parser BitwiseAndExp
pBitwiseAndExp = try (BitwiseAndEq <$> pEqualityExpression) <|> BitwiseAnd <$> pBitwiseAndExp <*> (reservedOp "&" *> pEqualityExpression) <?> "Bitwise And Exp"

--- BitwiseAndNI
--
pBitwiseXORExp :: Parser BitwiseXORExp
pBitwiseXORExp = try (BitwiseXORExp <$> pBitwiseAndExp) <|> BitwiseXORExpHat <$> pBitwiseXORExp <*> (reservedOp "^" *> pBitwiseAndExp) <?> "Bitwise XOR Exp"

pBitwiseOrExp :: Parser BitwiseOrExp
pBitwiseOrExp = try (BitwiseOrExp <$> pBitwiseXORExp) <|> BitwiseOrExpPipe <$> pBitwiseOrExp <*> (reservedOp "|" *> pBitwiseXORExp) <?> "Reserved OR Exp"

pLogicalAndExp :: Parser LogicalAndExp
pLogicalAndExp = try (LogicalAndExp <$> pBitwiseOrExp) <|> LogicalAndExpAmp <$> pLogicalAndExp <*> (reservedOp "&" *> pBitwiseOrExp) <?> "Logical And Exp"

pLogicalOrExp :: Parser LogicalOrExp
pLogicalOrExp = try (LogicalOrExp <$> pLogicalAndExp) <|> LogicalOrExpDPipe <$> pLogicalOrExp <*> (reservedOp "||" *> pLogicalAndExp) <?> "Logical Or Exp"   

pConditionalExp :: Parser ConditionalExp
pConditionalExp = try (ConditionalExp <$> pLogicalOrExp) <|> ConditionalExpTern <$> pLogicalOrExp <*> (reservedOp "?" *> pAssignmentExpression) <*> (reservedOp ":" *> pAssignmentExpression) <?> "Conditional Exp"

pAssignmentExpression :: Parser AssignmentExpression
pAssignmentExpression = try (AssignmentExpression <$> pConditionalExp) <|> AssignmentExpEq <$> pLHSExpression <*> (reservedOp "=" *> pAssignmentExpression) <|> AssignmentExpOp <$> pLHSExpression <*> pAssignmentOperator <*> pAssignmentExpression <?> "Assignment Expression"

pAssignmentOperator :: Parser AssignmentOperator
pAssignmentOperator = reservedOp "*=" *> return TimesEquals <|> reservedOp "/=" *> return DivEquals <|> reservedOp "%=" *> return ModEquals <|> reservedOp "-=" *> return MinusEquals <|> reservedOp "<<=" *> return LSEquals <|> reservedOp ">>=" *> return RSEquals <|> reservedOp ">>>=" *> return RSSEquals <|> reservedOp "&=" *> return AndEquals <|> reservedOp "^=" *> return XOREquals <|> reservedOp "|=" *> return OREquals  

pExpression :: Parser Expression
pExpression = commaSep1 pAssignmentExpression 

pStatement :: Parser Statement
pStatement = try (BlockStmt <$> pBlock) <|> VariableStmt <$> pVariableStatement <|> EmptyStmt <$> pEmptyStatement <|> ExpressionStmt <$> pExpressionStatement <|> IfStmt <$> pIfStatement <|> IterationStmt <$> pIterationStatement <|> ContinueStmt <$> pContinueStatement <|> BreakStmt <$> pBreakStatement <|> ReturnStmt <$> pReturnStatement <|> WithStmt <$> pWithStatement <|> LabeledStmt <$> pLabelledStatement <|> SwitchStmt <$> pSwitchStatement <|> ThrowStmt <$> pThrowStatement <|> TryStmt <$> pTryStatement <|> DebugStmt <$> pDebuggerStatement  <?> "Statement"

pBlock :: Parser Block 
pBlock = braces pStatementList

pStatementList :: Parser StatementList
pStatementList = many1 pStatement

pVariableStatement :: Parser VariableStatement
pVariableStatement = reserved "var" >> pVariableDeclarationList <* reservedOp ";"

pVariableDeclarationList :: Parser VariableDeclarationList
pVariableDeclarationList = commaSep1 pVariableDeclaration

-- VDL No in

pVariableDeclaration :: Parser VariableDeclaration
pVariableDeclaration = VariableDeclaration <$> identifier <*> (optionMaybe pInitializer)

-- VD No in

pInitializer :: Parser Initializer
pInitializer = reservedOp "=" >> pAssignmentExpression

pEmptyStatement :: Parser EmptyStatement
pEmptyStatement = reservedOp ";" >> return EmptyStatement

pExpressionStatement :: Parser ExpressionStatement
pExpressionStatement = do
  notFollowedBy funcOrBrace
  exp <- pExpression
  reservedOp ";"
  return $ ExpressionStatement exp
  where
    funcOrBrace = reserved "function" <|> reservedOp "{"

pIfStatement :: Parser IfStatement
pIfStatement = do
  reserved "if"
  exp <- parens pExpression
  st <- pStatement
  optionMaybe (reserved "else")
  st2 <- optionMaybe pStatement
  return $ IfStatement exp st st2

pIterationStatement :: Parser IterationStatement
pIterationStatement = try pDoWhile <|> pWhile <|> pForLHS 
 where
   -- fix "no in"s
   pDoWhile = DoWhile <$> (reserved "do" *> pStatement) <*> (reserved "while" *> (parens pExpression))
   pWhile = While <$> (reserved "while" *> (parens pExpression)) <*> pStatement
   pForLHS = do
     reserved "for"
     (lhs, exp) <- (,) <$> pLHSExpression <*> (reserved "in" *> pExpression)
     st <- pStatement
     return $ ForLHS lhs exp st 

pContinueStatement :: Parser ContinueStatement
pContinueStatement = reserved "continue" >> (optionMaybe identifier) <* reservedOp ";"

 
pBreakStatement :: Parser BreakStatement
pBreakStatement = reserved "break" >> (optionMaybe identifier) <* reservedOp ";"


pReturnStatement :: Parser ReturnStatement
pReturnStatement = reserved "return" >> (optionMaybe pExpression) <* reservedOp ";"

pWithStatement :: Parser WithStatement
pWithStatement = reserved "with" >> WithStatement <$> (parens pExpression) <*> pStatement

pSwitchStatement :: Parser SwitchStatement
pSwitchStatement = reserved "switch" >> SwitchStatement <$> (parens pExpression) <*> pCaseBlock

pCaseBlock :: Parser CaseBlock
pCaseBlock = braces $ CaseBlock <$> (optionMaybe pCaseClauses) <*> (optionMaybe pDefaultClause) <*> (optionMaybe pCaseClauses) 

pCaseClauses :: Parser CaseClauses
pCaseClauses = many1 pCaseClause

pCaseClause :: Parser CaseClause
pCaseClause = reserved "case" >> CaseClause <$> (pExpression <* reservedOp ":") <*> (optionMaybe pStatementList)

pDefaultClause :: Parser DefaultClause
pDefaultClause = reserved "default" >> reservedOp ":" >> DefaultClause <$> (optionMaybe pStatementList)

pLabelledStatement :: Parser LabelledStatement
pLabelledStatement = LabelledStatement <$> identifier <*> (reservedOp ":" *> pStatement)

pThrowStatement :: Parser ThrowStatement
pThrowStatement = reserved "throw" >> ThrowStatement <$> pExpression <* reservedOp ";"

pTryStatement :: Parser TryStatement
pTryStatement = try pTryBlockCatch <|> pTryFinally <|> pTryCF 
  where
    pTryBlockCatch = reserved "try" *> (TryCatch <$> pBlock <*> pCatch)
    pTryFinally = reserved "try" *> (TryFinally <$> pBlock <*> pFinally)
    pTryCF = reserved "try" *> (TryCF <$> pBlock <*> pCatch <*> pFinally)

pCatch :: Parser Catch
pCatch = reserved "catch" *> (Catch <$> (parens identifier) <*> pBlock)

pFinally :: Parser Finally
pFinally = reserved "finally" *> (Finally <$> pBlock)

pDebuggerStatement :: Parser DebuggerStatement
pDebuggerStatement = reserved "debugger" *> reservedOp ";" *> pure Debugger

pFunctionDeclaration :: Parser FunctionDeclaration
pFunctionDeclaration = do
  reserved "function"
  id <- identifier
  fpl <- optionMaybe (parens pFormalParameterList)
  fb <- braces pFunctionBody
  return $ FunctionDeclaration id fpl fb
  
pFunctionExpression :: Parser FunctionExpression
pFunctionExpression = do
  reserved "function"
  id <- optionMaybe identifier
  fpl <- optionMaybe (parens pFormalParameterList)
  fb <- braces pFunctionBody
  return $ FunctionExpression id fpl fb
 
pFormalParameterList :: Parser FormalParameterList
pFormalParameterList = commaSep1 identifier 

pFunctionBody :: Parser FunctionBody
pFunctionBody = FunctionBody <$> optionMaybe pSourceElements

pProgram :: Parser Program
pProgram = Program <$>  optionMaybe pSourceElements

pSourceElements :: Parser SourceElements
pSourceElements = many1 pSourceElement

pSourceElement :: Parser SourceElement
pSourceElement = try (SourceElement <$> pStatement) <|> SourceElementFunc <$> pFunctionDeclaration 
-- placeholders

{-pAssignmentExpression :: Parser AssignmentExpression-}
{-pAssignmentExpression = return AssignmentExpression -}
{-pFunctionExpression :: Parser FunctionExpression-}
{-pFunctionExpression = return $ FunctionExpression Nothing Nothing  (FunctionBody Nothing) -}
