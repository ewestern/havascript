{-# LANGUAGE OverloadedStrings #-}

module Parser.Parser where
import Text.Parsec
import Control.Applicative hiding ((<|>), many)
import Parser.Syntax
import Parser.Lexer
import Text.Parsec.Expr
import Debug.Trace

pPrimaryExpression :: Parser PrimaryExpression
pPrimaryExpression = 
        (reserved "this" >> return This)
    <|> (IdentifierExp <$> identifier <?> "Identifier expression" ) 
    <|> LiteralExp <$> pLiteral
    <|> ArrayExp <$> pArrayLiteral 
    <|> ObjectExp <$> pObjectLiteral 
    {-<|> ExpressionExp <$> pExpression <?> "Primary Expression"-}



pDecLiteral :: Parser NumLiteral
pDecLiteral = do
  en <- number
  case en of
    (Left i) -> return $ DecLiteral . fromIntegral $  i
    (Right d) -> return $ DecLiteral d

pNumLiteral :: Parser NumLiteral
pNumLiteral = pDecLiteral <?> "Num Literal"
-- add Hex Parser

pNullLiteral :: Parser Literal
pNullLiteral = reserved "null" >> return NullLiteral 

pBoolLiteral :: Parser Literal
pBoolLiteral = BooleanLiteral <$> pTrue <|> BooleanLiteral <$> pFalse 
  where
    pTrue = reserved "true" >> return True
    pFalse = reserved "false" >> return False 

pLiteral :: Parser Literal
pLiteral = pBoolLiteral <|> pNullLiteral <|> StringLiteral <$> stringLiteral <|> NumericLiteral <$> pNumLiteral <?> "Literal"

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
pMemberExpression = 
              MemberExpressionPrimary <$> pPrimaryExpression 
          <|> MemberExpressionFunction <$> pFunctionExpression 
          <|> pMemberSquare 
          <|> pMemberDot 
          <|> pMemberNew 
          <?> "Member Expression"
  where 
    pMemberSquare = MemberExpressionSquare <$> pMemberExpression <*> (squares pExpression)
    pMemberDot = MemberExpressionDot <$> pMemberExpression <*> (dot *> identifier) <?>  "MemberDot"
    pMemberNew = reserved "new" *> (MemberExpressionNew <$> pMemberExpression <*> pArguments) <?> "Member New"

pNewExpression :: Parser NewExpression
pNewExpression = 
        (NewExpressionMember <$> pMemberExpression) 
    <|> reserved "new" *> (NewExpressionNew <$> pNewExpression) 
    <?> "New Expression"

pCallExpression :: Parser CallExpression
pCallExpression = 
            CallExpressionMember <$> pMemberExpression <*> pArguments 
        <|> CallExpressionArgs <$> pCallExpression <*> pArguments 
        <|> CallExpressionExp <$> pCallExpression <*> (squares pExpression) 
        <|> CallExpressionIdent <$> pCallExpression <*> (dot *> identifier) 
        <?> "Call Expression"    

pArguments :: Parser Arguments
pArguments = parens pArgumentList

pArgumentList :: Parser ArgumentList
pArgumentList = commaSep pAssignmentExpression

pLHSExpression :: Parser LHSExpression
pLHSExpression = 
            try (LHSExpressionCall <$> pCallExpression )
        <|> LHSExpressionNew <$> pNewExpression
        <?> "LHS Expression"


pMultOp :: Parser (OperatorExpression -> OperatorExpression -> OperatorExpression)
pMultOp =  (flip OperatorExpressionBinary) <$> helper
  where
    helper =
          reservedOp "*" *> pure MultTimes
      <|> reservedOp "/" *> pure MultDivide
      <|> reservedOp "%" *> pure MultMod

pAddOp :: Parser (OperatorExpression -> OperatorExpression -> OperatorExpression)
pAddOp =  (flip OperatorExpressionBinary) <$> helper
  where
    helper =
          reservedOp "+" *> pure AddPlus
      <|> reservedOp "-" *> pure AddMinus

pShiftOp :: Parser (OperatorExpression -> OperatorExpression -> OperatorExpression)
pShiftOp =  (flip OperatorExpressionBinary) <$> helper
  where
    helper =
          reservedOp "<<" *> pure ShiftLeft
      <|> reservedOp ">>" *> pure ShiftRight
      <|> reservedOp ">>>" *> pure ShiftRRight

pRelOp :: Parser (OperatorExpression -> OperatorExpression -> OperatorExpression)
pRelOp =  (flip OperatorExpressionBinary) <$> helper
  where
    helper =
          reservedOp "<" *> pure RelLT
      <|> reservedOp ">" *> pure RelGT
      <|> reservedOp "<=" *> pure RelLTE
      <|> reservedOp ">=" *> pure RelGTE
      <|> reserved "instanceof" *> pure RelInstanceOf
      <|> reserved "in" *> pure RelIn

pEqOp :: Parser (OperatorExpression -> OperatorExpression -> OperatorExpression)
pEqOp =  (flip OperatorExpressionBinary) <$> helper
  where
    helper =
          reservedOp "==" *> pure EqEq
      <|> reservedOp "!=" *> pure EqNE
      <|> reservedOp "===" *> pure EqEqq
      <|> reservedOp "!==" *> pure EqNEE
      <|> reservedOp "|" *> pure BitOr
      <|> reservedOp "^" *> pure BitXOr
      <|> reservedOp "&" *> pure BitAnd
      <|> reservedOp "&&" *> pure LogAnd
      <|> reservedOp "||" *> pure LogOr <?> "binary operator"


pPrefixOperator :: Parser (OperatorExpression -> OperatorExpression)
pPrefixOperator = OperatorExpressionPrefix <$> helper
  where
    helper = 
            reserved "delete" *> pure PrefixDelete
        <|> reserved "void" *> pure PrefixVoid
        <|> reservedOp "++" *> pure PrefixIncr
        <|> reservedOp "--" *> pure PrefixDecr
        <|> reservedOp "+" *> pure PrefixPlus
        <|> reservedOp "-" *> pure PrefixMinus
        <|> reservedOp "~" *> pure PrefixTilde
        <|> reservedOp "!" *> pure PrefixNot <?> "prefix operator"

pPostfixOperator :: Parser (OperatorExpression -> OperatorExpression)
pPostfixOperator = (flip OperatorExpressionPostfix) <$> helper
  where
      helper = 
            reservedOp "++" *> pure PostfixIncr
        <|> reservedOp "--" *> pure PostfixDecr <?> "postfix operator"

      

pTerm :: Parser OperatorExpression
pTerm = OperatorExpressionLHS <$> pLHSExpression <?> "Term"

-- start with postfix

table = [ [Postfix pPostfixOperator]
        , [Prefix pPrefixOperator]
        , [Infix pMultOp AssocLeft]
        , [Infix pAddOp AssocLeft]
        , [Infix pShiftOp AssocLeft]
        , [Infix pRelOp AssocLeft]
        , [Infix pEqOp AssocLeft] ]

pOperatorExpression :: Parser OperatorExpression
pOperatorExpression = buildExpressionParser table pTerm <?> "operator expression" 


pConditionalExp :: Parser ConditionalExp
pConditionalExp = 
            try (ConditionalExp <$> pOperatorExpression)
        {-<|> ConditionalExp <$> pTerm-}
        <|> ConditionalExpTern <$> pOperatorExpression <*> (reservedOp "?" *> pAssignmentExpression) 
        <*> (reservedOp ":" *> pAssignmentExpression) 
        <?> "Conditional Exp"

pAssignmentExpression :: Parser AssignmentExpression
pAssignmentExpression = 
        try (AssignmentExpression <$> pConditionalExp) 
        <|> AssignmentExpEq <$> pLHSExpression <*> (reservedOp "=" *> pAssignmentExpression) 
        <|> AssignmentExpOp <$> pLHSExpression <*> pAssignmentOperator <*> pAssignmentExpression 
        <?> "Assignment Expression"

pAssignmentOperator :: Parser AssignmentOperator
pAssignmentOperator = 
        reservedOp "*=" *> return TimesEquals 
        <|> reservedOp "/=" *> return DivEquals 
        <|> reservedOp "%=" *> return ModEquals 
        <|> reservedOp "-=" *> return MinusEquals 
        <|> reservedOp "<<=" *> return LSEquals 
        <|> reservedOp ">>=" *> return RSEquals 
        <|> reservedOp ">>>=" *> return RSSEquals 
        <|> reservedOp "&=" *> return AndEquals 
        <|> reservedOp "^=" *> return XOREquals 
        <|> reservedOp "|=" *> return OREquals  
        <?> "Assignment Operator"

pExpression :: Parser Expression
pExpression = commaSep1 pAssignmentExpression  <?> "Expression"

-- TODO: removed Block here. Look into that.
pStatement :: Parser Statement
pStatement =  VariableStmt <$> pVariableStatement 
          <|> EmptyStmt <$> pEmptyStatement 
          {-<|> ExpressionStmt <$> pExpressionStatement -} -- TODO!!
          <|> IfStmt <$> (pIfStatement  <?> "if statement")
          <|> IterationStmt <$> pIterationStatement 
          <|> ContinueStmt <$> pContinueStatement 
          <|> BreakStmt <$> pBreakStatement 
          <|> ReturnStmt <$> pReturnStatement 
          <|> WithStmt <$> pWithStatement 
          <|> LabeledStmt <$> pLabelledStatement 
          <|> SwitchStmt <$> pSwitchStatement 
          <|> ThrowStmt <$> pThrowStatement 
          <|> TryStmt <$> pTryStatement 
          <|> BlockStmt <$> pBlock
          <|> DebugStmt <$> pDebuggerStatement  <?> "Statement"

pBlock :: Parser Block 
pBlock = braces pStatementList <?> "Block"

pStatementList :: Parser StatementList
pStatementList = many1 pStatement

pVariableStatement :: Parser VariableStatement
pVariableStatement = reserved "var" >> pVariableDeclarationList <* reservedOp ";"

pVariableDeclarationList :: Parser VariableDeclarationList
pVariableDeclarationList = try (commaSep1 pVariableDeclaration) <?> "Variable Dec List"

-- VDL No in

pVariableDeclaration :: Parser VariableDeclaration
pVariableDeclaration = try (VariableDeclaration <$> identifier <*> (optionMaybe pInitializer)) <?> "Variable Declaration"

-- VD No in

pInitializer :: Parser Initializer
pInitializer = try (reservedOp "=" >> pAssignmentExpression) <?> "initializer"

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
pReturnStatement = reserved "return" >> (optionMaybe pExpression) <* reservedOp ";" <?> "return statement"

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
  fpl <- parens pFormalParameterList
  fb <- braces pFunctionBody
  return $ FunctionDeclaration id fpl fb
  
pFunctionExpression :: Parser FunctionExpression
pFunctionExpression = do
  reserved "function"
  id <- optionMaybe identifier
  fpl <- parens pFormalParameterList
  fb <- braces pFunctionBody
  return $ FunctionExpression id fpl fb
 
pFormalParameterList :: Parser FormalParameterList
pFormalParameterList = commaSep identifier 

pFunctionBody :: Parser FunctionBody
pFunctionBody = FunctionBody <$> optionMaybe pSourceElements

pProgram :: Parser Program
pProgram = Program <$>  pSourceElements

pSourceElements :: Parser SourceElements
pSourceElements = many pSourceElement

pSourceElement :: Parser SourceElement
pSourceElement = SourceElement <$> pStatement <|> SourceElementFunc <$> pFunctionDeclaration <?> "source element"

