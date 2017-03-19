
module Parser.Syntax where

data NumLiteral =
   DecLiteral Double 
  | HexLiteral String deriving (Eq, Show)

data Literal =
  NullLiteral
  | BooleanLiteral Bool
  | NumericLiteral NumLiteral 
  | StringLiteral String
  | RegExLiteral String deriving (Eq, Show)


type Identifier = String 
data PrimaryExpression = 
    This
  | IdentifierExp Identifier
  | LiteralExp Literal
  | ArrayExp ArrayLiteral
  | ObjectExp ObjectLiteral
  | ExpressionExp Expression deriving (Eq, Show)


type ArrayLiteral =  [AssignmentExpression]
type ObjectLiteral = PropertyNameAndValueList

type PropertyNameAndValueList = [PropertyAssignment]

data PropertyAssignment = 
    PropertyAssignment PropertyName AssignmentExpression
  | PropertyGet PropertyName FunctionBody
  | PropertySet PropertyName Identifier FunctionBody deriving (Eq, Show)


data PropertyName = 
    PropertyNameIdentifier Identifier
  | PropertyNameString String 
  | PropertyNameNumber NumLiteral deriving (Eq, Show)


type PropertySetParamList = Identifier 

data MemberExpression = 
    MemberExpressionPrimary PrimaryExpression
  | MemberExpressionFunction FunctionExpression
  | MemberExpressionSquare MemberExpression Expression
  | MemberExpressionDot MemberExpression Identifier
  | MemberExpressionNew MemberExpression Arguments deriving (Eq, Show)


data NewExpression =
    NewExpressionMember MemberExpression
  | NewExpressionNew NewExpression deriving (Eq, Show)


data CallExpression = 
  CallExpressionMember MemberExpression Arguments
  | CallExpressionArgs CallExpression Arguments
  | CallExpressionExp CallExpression Expression
  | CallExpressionIdent CallExpression Identifier deriving (Eq, Show)


type Arguments = ArgumentList

type ArgumentList = [AssignmentExpression]

data LHSExpression = 
    LHSExpressionNew NewExpression
  | LHSExpressionCall CallExpression deriving (Eq, Show)


data PostfixExpression = 
  PostfixLHS LHSExpression
  | PostfixInc LHSExpression
  | PostfixDec LHSExpression deriving (Eq, Show)


data UnaryOp
  = UnaryDelete
  | UnaryVoid
  | UnaryTypeOf
  | UnaryInc
  | UnaryDec
  | UnaryPlus
  | UnaryMinus
  | UnaryTilde
  | UnaryNot deriving (Eq, Show)

data UnaryExpression = 
    UnaryExpression UnaryOp UnaryExpression
  | UnaryPostfix PostfixExpression deriving (Eq, Show)


data MultiplicativeOp
  = MultTimes
  | MultDivide
  | MultMod deriving (Eq, Show)

data MultiplicativeExpression = 
    MultUnary UnaryExpression
  | MultiplicativeExpression MultiplicativeExpression MultiplicativeOp UnaryExpression deriving (Eq, Show)


data AdditiveExpression = 
    AddMult MultiplicativeExpression
  | AddPlus AdditiveExpression MultiplicativeExpression
  | AddMinus AdditiveExpression MultiplicativeExpression deriving (Eq, Show)


data ShiftExpression = 
  ShiftAdd AdditiveExpression
  | ShiftLeft ShiftExpression AdditiveExpression
  | ShiftRight ShiftExpression AdditiveExpression
  | ShiftZ ShiftExpression AdditiveExpression deriving (Eq, Show)

  
data RelationalExpression = 
  RelationShift ShiftExpression
  | RelationLT RelationalExpression ShiftExpression
  | RelationGT RelationalExpression ShiftExpression
  | RelationLTE RelationalExpression ShiftExpression
  | RelationGTE RelationalExpression ShiftExpression
  | RelationIO RelationalExpression ShiftExpression
  | RelationIn RelationalExpression ShiftExpression deriving (Eq, Show)

  
data RelationalExpressionNI = 
  RelationShiftNI ShiftExpression
  | RelationLTNI RelationalExpressionNI ShiftExpression
  | RelationGTNI RelationalExpressionNI ShiftExpression
  | RelationLTENI RelationalExpressionNI ShiftExpression
  | RelationGTENI  RelationalExpressionNI ShiftExpression
  | RelationIONI RelationalExpressionNI ShiftExpression  deriving (Eq, Show)


data EqualityExpression = 
  EqualityExp RelationalExpression
  | EqualityExpEq EqualityExpression RelationalExpression
  | EqualityExpNE EqualityExpression RelationalExpression
  | EqualityExpEqq EqualityExpression RelationalExpression
  | EqualityExpNEE EqualityExpression RelationalExpression deriving (Eq, Show)


data BitwiseAndExp =
  BitwiseAndEq EqualityExpression
  | BitwiseAnd BitwiseAndExp EqualityExpression deriving (Eq, Show)


data BitwiseAndExpNI =
  BitwiseAndNIEq EqualityExpression
  | BitwiseAndNI BitwiseAndExp EqualityExpression deriving (Eq, Show)

 
data BitwiseXORExp = 
  BitwiseXORExp BitwiseAndExp
  | BitwiseXORExpHat BitwiseXORExp BitwiseAndExp deriving (Eq, Show)

   

data BitwiseXORExpNI = 
  BitwiseXORExpNI BitwiseAndExpNI
  | BitwiseXORExpHatNI BitwiseXORExpNI BitwiseAndExpNI deriving (Eq, Show)



data BitwiseOrExp = 
  BitwiseOrExp BitwiseXORExp
  | BitwiseOrExpPipe BitwiseOrExp BitwiseXORExp deriving (Eq, Show)


data BitwiseOrExpNI = 
  BitwiseOrExpNI BitwiseXORExpNI
  | BitwiseOrExpPipeNI BitwiseOrExpNI BitwiseXORExpNI deriving (Eq, Show)


data LogicalAndExp = 
  LogicalAndExp BitwiseOrExp
  | LogicalAndExpAmp LogicalAndExp BitwiseOrExp deriving (Eq, Show)


data LogicalAndExpNI = 
  LogicalAndExpNI BitwiseOrExpNI
  | LogicalAndExpAmpNI LogicalAndExpNI BitwiseOrExpNI deriving (Eq, Show)


data LogicalOrExp = 
  LogicalOrExp LogicalAndExp
  | LogicalOrExpDPipe LogicalOrExp LogicalAndExp deriving (Eq, Show)


data LogicalOrExpNI = 
  LogicalOrExpNI LogicalAndExpNI
  | LogicalOrExpDPipeNI LogicalOrExpNI LogicalAndExpNI deriving (Eq, Show)


data ConditionalExp = 
  ConditionalExp LogicalOrExp
  | ConditionalExpTern LogicalOrExp AssignmentExpression AssignmentExpression deriving (Eq, Show)


data ConditionalExpNI =
  ConditionalExpNI LogicalOrExpNI
  | ConditionalExpTernNI LogicalOrExpNI AssignmentExpression AssignmentExpressionNI deriving (Eq, Show)


data AssignmentExpression = 
  AssignmentExpression ConditionalExp
  | AssignmentExpEq LHSExpression AssignmentExpression
  | AssignmentExpOp LHSExpression AssignmentOperator AssignmentExpression deriving (Eq, Show)


data AssignmentExpressionNI = 
  AssignmentExpressionNI ConditionalExpNI
  | AssignmentExpEqNI LHSExpression AssignmentExpressionNI
  | AssignmentExpOpNI LHSExpression AssignmentOperator AssignmentExpressionNI deriving (Eq, Show)


data AssignmentOperator =
  TimesEquals
  | DivEquals
  | ModEquals
  | PlusEquals
  | MinusEquals
  | LSEquals
  | RSEquals
  | RSSEquals
  | AndEquals
  | XOREquals
  | OREquals deriving (Eq, Show)


type Expression = [AssignmentExpression]
{-data Expression = -}
  {-Expression AssignmentExpression-}
  {-| ExpressionSeq AssignmentExpression-}

data ExpressionNI = 
  ExpressionNI AssignmentExpressionNI
  | ExpressionNISeq AssignmentExpressionNI deriving (Eq, Show)


data Statement = 
  BlockStmt Block 
  | VariableStmt VariableStatement
  | EmptyStmt EmptyStatement
  | ExpressionStmt ExpressionStatement
  | IfStmt IfStatement
  | IterationStmt IterationStatement
  | ContinueStmt ContinueStatement
  | BreakStmt BreakStatement
  | ReturnStmt ReturnStatement
  | WithStmt WithStatement
  | LabeledStmt LabelledStatement
  | SwitchStmt SwitchStatement
  | ThrowStmt ThrowStatement
  | TryStmt TryStatement
  | DebugStmt DebuggerStatement deriving (Eq, Show)

  
type Block = StatementList

type StatementList = [Statement]

type VariableStatement = VariableDeclarationList

type VariableDeclarationList = [VariableDeclaration]

type VariableDeclarationListNI = [VariableDeclarationNI]

data VariableDeclaration = VariableDeclaration Identifier (Maybe Initializer) deriving (Eq, Show)

data VariableDeclarationNI = VariableDeclarationNI Identifier (Maybe InitializerNI) deriving (Eq, Show)


type Initializer = AssignmentExpression
type InitializerNI = AssignmentExpressionNI

data EmptyStatement = EmptyStatement deriving (Eq, Show)

-- todo: what is this
data ExpressionStatement = ExpressionStatement Expression deriving (Eq, Show)


data IfStatement = IfStatement Expression Statement (Maybe Statement) deriving (Eq, Show)


data IterationStatement = 
  DoWhile Statement Expression
  | While Expression Statement
  | ForExp (Maybe ExpressionNI) (Maybe Expression) (Maybe Expression) Statement
  | ForVar VariableDeclarationListNI (Maybe Expression) (Maybe Expression) Statement
  | ForLHS LHSExpression Expression Statement
  | ForVarIn VariableDeclarationNI Expression Statement deriving (Eq, Show)


type ContinueStatement = Maybe Identifier

type BreakStatement = Maybe Identifier

type ReturnStatement = Maybe Expression

data WithStatement = 
  WithStatement Expression Statement deriving (Eq, Show)


data SwitchStatement = 
  SwitchStatement Expression CaseBlock deriving (Eq, Show)


data CaseBlock = 
  CaseBlock (Maybe CaseClauses) (Maybe DefaultClause) (Maybe CaseClauses) deriving (Eq, Show)


type CaseClauses = [CaseClause]

data CaseClause = 
  CaseClause Expression (Maybe StatementList) deriving (Eq, Show)


data DefaultClause = 
  DefaultClause (Maybe StatementList) deriving (Eq, Show)


data LabelledStatement = 
  LabelledStatement Identifier Statement deriving (Eq, Show)


data ThrowStatement = 
  ThrowStatement Expression deriving (Eq, Show)


data TryStatement = 
  TryCatch Block Catch
  | TryFinally Block Finally
  | TryCF Block Catch Finally deriving (Eq, Show)


data Catch = Catch Identifier Block deriving (Eq, Show)


data Finally = Finally Block deriving (Eq, Show)


data DebuggerStatement = Debugger deriving (Eq, Show)


data FunctionDeclaration = 
  FunctionDeclaration Identifier (Maybe FormalParameterList) FunctionBody deriving (Eq, Show)

 
data FunctionExpression = 
  FunctionExpression (Maybe Identifier) (Maybe FormalParameterList) FunctionBody deriving (Eq, Show)


type FormalParameterList = [Identifier]

data FunctionBody = 
  FunctionBody (Maybe SourceElements) deriving (Eq, Show)


data Program = 
  Program (Maybe SourceElements) deriving (Eq, Show)


type SourceElements = [SourceElement]

data SourceElement = 
  SourceElement Statement
  | SourceElementFunc FunctionDeclaration deriving (Eq, Show)


