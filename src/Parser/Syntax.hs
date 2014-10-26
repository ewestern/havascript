
module Syntax where
{-import qualified Data.Text as T-}

data NumLiteral =
   DecLiteral Double 
  | HexLiteral String deriving (Show)

data Literal =
  NullLiteral
  | BooleanLiteral Bool
  | NumericLiteral NumLiteral 
  | StringLiteral String
  | RegExLiteral String deriving (Show)


type Identifier = String 
data PrimaryExpression = 
   This
  | IdentifierExp Identifier
  | LiteralExp Literal
  | ArrayExp ArrayLiteral
  | ObjectExp ObjectLiteral
  | ExpressionExp Expression deriving (Show)


type ArrayLiteral =  [AssignmentExpression]
type ObjectLiteral = PropertyNameAndValueList

type PropertyNameAndValueList = [PropertyAssignment]

data PropertyAssignment = 
  PropertyAssignment PropertyName AssignmentExpression
  | PropertyGet PropertyName FunctionBody
  | PropertySet PropertyName Identifier FunctionBody deriving (Show)


data PropertyName = 
  PropertyNameIdentifier Identifier
  | PropertyNameString String 
  | PropertyNameNumber NumLiteral deriving (Show)


type PropertySetParamList = Identifier 

data MemberExpression = 
  MemberExpressionPrimary PrimaryExpression
  | MemberExpressionFunction FunctionExpression
  | MemberExpressionSquare MemberExpression Expression
  | MemberExpressionDot MemberExpression Identifier
  | MemberExpressionNew MemberExpression Arguments deriving (Show)


data NewExpression =
  NewExpressionMember MemberExpression
  | NewExpressionNew NewExpression deriving (Show)


data CallExpression = 
  CallExpressionMember MemberExpression Arguments
  | CallExpressionArgs CallExpression Arguments
  | CallExpressionExp CallExpression Expression
  | CallExpressionIdent CallExpression Identifier deriving (Show)


type Arguments = ArgumentList

type ArgumentList = [AssignmentExpression]

data LHSExpression = 
  LHSExpressionNew NewExpression
  | LHSExpressionCall CallExpression deriving (Show)


data PostfixExpression = 
  PostfixLHS LHSExpression
  | PostfixInc LHSExpression
  | PostfixDec LHSExpression deriving (Show)


data UnaryExpression = 
  UnaryExpression PostfixExpression
  | UnaryExpressionDelete UnaryExpression
  | UnaryExpressionVoid UnaryExpression
  | UnaryExpressionTypeOf UnaryExpression
  | UnaryExpressionInc UnaryExpression
  | UnaryExpressionDec UnaryExpression
  | UnaryExpressionPlus UnaryExpression
  | UnaryExpressionMinus UnaryExpression
  | UnaryExpressionTilde UnaryExpression
  | UnaryExpressionNot UnaryExpression deriving (Show)


data MultiplicativeExpression = 
  MultUnary UnaryExpression
  | MultTimes MultiplicativeExpression UnaryExpression
  | MultDivide MultiplicativeExpression UnaryExpression
  | MultMod MultiplicativeExpression UnaryExpression deriving (Show)


data AdditiveExpression = 
  AddMult MultiplicativeExpression
  | AddPlus AdditiveExpression MultiplicativeExpression
  | AddMinus AdditiveExpression MultiplicativeExpression deriving (Show)


data ShiftExpression = 
  ShiftAdd AdditiveExpression
  | ShiftLeft ShiftExpression AdditiveExpression
  | ShiftRight ShiftExpression AdditiveExpression
  | ShiftZ ShiftExpression AdditiveExpression deriving (Show)

  
data RelationalExpression = 
  RelationShift ShiftExpression
  | RelationLT RelationalExpression ShiftExpression
  | RelationGT RelationalExpression ShiftExpression
  | RelationLTE RelationalExpression ShiftExpression
  | RelationGTE RelationalExpression ShiftExpression
  | RelationIO RelationalExpression ShiftExpression
  | RelationIn RelationalExpression ShiftExpression deriving (Show)

  
data RelationalExpressionNI = 
  RelationShiftNI ShiftExpression
  | RelationLTNI RelationalExpressionNI ShiftExpression
  | RelationGTNI RelationalExpressionNI ShiftExpression
  | RelationLTENI RelationalExpressionNI ShiftExpression
  | RelationGTENI  RelationalExpressionNI ShiftExpression
  | RelationIONI RelationalExpressionNI ShiftExpression  deriving (Show)


data EqualityExpression = 
  EqualityExp RelationalExpression
  | EqualityExpEq EqualityExpression RelationalExpression
  | EqualityExpNE EqualityExpression RelationalExpression
  | EqualityExpEqq EqualityExpression RelationalExpression
  | EqualityExpNEE EqualityExpression RelationalExpression deriving (Show)


data BitwiseAndExp =
  BitwiseAndEq EqualityExpression
  | BitwiseAnd BitwiseAndExp EqualityExpression deriving (Show)


data BitwiseAndExpNI =
  BitwiseAndNIEq EqualityExpression
  | BitwiseAndNI BitwiseAndExp EqualityExpression deriving (Show)

 
data BitwiseXORExp = 
  BitwiseXORExp BitwiseAndExp
  | BitwiseXORExpHat BitwiseXORExp BitwiseAndExp deriving (Show)

   

data BitwiseXORExpNI = 
  BitwiseXORExpNI BitwiseAndExpNI
  | BitwiseXORExpHatNI BitwiseXORExpNI BitwiseAndExpNI deriving (Show)



data BitwiseOrExp = 
  BitwiseOrExp BitwiseXORExp
  | BitwiseOrExpPipe BitwiseOrExp BitwiseXORExp deriving (Show)


data BitwiseOrExpNI = 
  BitwiseOrExpNI BitwiseXORExpNI
  | BitwiseOrExpPipeNI BitwiseOrExpNI BitwiseXORExpNI deriving (Show)


data LogicalAndExp = 
  LogicalAndExp BitwiseOrExp
  | LogicalAndExpAmp LogicalAndExp BitwiseOrExp deriving (Show)


data LogicalAndExpNI = 
  LogicalAndExpNI BitwiseOrExpNI
  | LogicalAndExpAmpNI LogicalAndExpNI BitwiseOrExpNI deriving (Show)


data LogicalOrExp = 
  LogicalOrExp LogicalAndExp
  | LogicalOrExpDPipe LogicalOrExp LogicalAndExp deriving (Show)


data LogicalOrExpNI = 
  LogicalOrExpNI LogicalAndExpNI
  | LogicalOrExpDPipeNI LogicalOrExpNI LogicalAndExpNI deriving (Show)


data ConditionalExp = 
  ConditionalExp LogicalOrExp
  | ConditionalExpTern LogicalOrExp AssignmentExpression AssignmentExpression deriving (Show)


data ConditionalExpNI =
  ConditionalExpNI LogicalOrExpNI
  | ConditionalExpTernNI LogicalOrExpNI AssignmentExpression AssignmentExpressionNI deriving (Show)


data AssignmentExpression = 
  AssignmentExpression ConditionalExp
  | AssignmentExpEq LHSExpression AssignmentExpression
  | AssignmentExpOp LHSExpression AssignmentOperator AssignmentExpression deriving (Show)


data AssignmentExpressionNI = 
  AssignmentExpressionNI ConditionalExpNI
  | AssignmentExpEqNI LHSExpression AssignmentExpressionNI
  | AssignmentExpOpNI LHSExpression AssignmentOperator AssignmentExpressionNI deriving (Show)


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
  | OREquals deriving (Show)


type Expression = [AssignmentExpression]
{-data Expression = -}
  {-Expression AssignmentExpression-}
  {-| ExpressionSeq AssignmentExpression-}

data ExpressionNI = 
  ExpressionNI AssignmentExpressionNI
  | ExpressionNISeq AssignmentExpressionNI deriving (Show)


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
  | DebugStmt DebuggerStatement deriving (Show)

  
type Block = StatementList

type StatementList = [Statement]

type VariableStatement = VariableDeclarationList

type VariableDeclarationList = [VariableDeclaration]

type VariableDeclarationListNI = [VariableDeclarationNI]

data VariableDeclaration = VariableDeclaration Identifier (Maybe Initializer) deriving (Show)

data VariableDeclarationNI = VariableDeclarationNI Identifier (Maybe InitializerNI) deriving (Show)


type Initializer = AssignmentExpression
type InitializerNI = AssignmentExpressionNI

data EmptyStatement = EmptyStatement deriving (Show)

-- todo: what is this
data ExpressionStatement = ExpressionStatement Expression deriving (Show)


data IfStatement = IfStatement Expression Statement (Maybe Statement) deriving (Show)


data IterationStatement = 
  DoWhile Statement Expression
  | While Expression Statement
  | ForExp (Maybe ExpressionNI) (Maybe Expression) (Maybe Expression) Statement
  | ForVar VariableDeclarationListNI (Maybe Expression) (Maybe Expression) Statement
  | ForLHS LHSExpression Expression Statement
  | ForVarIn VariableDeclarationNI Expression Statement deriving (Show)


type ContinueStatement = Maybe Identifier

type BreakStatement = Maybe Identifier

type ReturnStatement = Maybe Expression

data WithStatement = 
  WithStatement Expression Statement deriving (Show)


data SwitchStatement = 
  SwitchStatement Expression CaseBlock deriving (Show)


data CaseBlock = 
  CaseBlock (Maybe CaseClauses) (Maybe DefaultClause) (Maybe CaseClauses) deriving (Show)


type CaseClauses = [CaseClause]

data CaseClause = 
  CaseClause Expression (Maybe StatementList) deriving (Show)


data DefaultClause = 
  DefaultClause (Maybe StatementList) deriving (Show)


data LabelledStatement = 
  LabelledStatement Identifier Statement deriving (Show)


data ThrowStatement = 
  ThrowStatement Expression deriving (Show)


data TryStatement = 
  TryCatch Block Catch
  | TryFinally Block Finally
  | TryCF Block Catch Finally deriving (Show)


data Catch = Catch Identifier Block deriving (Show)


data Finally = Finally Block deriving (Show)


data DebuggerStatement = Debugger deriving (Show)


data FunctionDeclaration = 
  FunctionDeclaration Identifier (Maybe FormalParameterList) FunctionBody deriving (Show)

 
data FunctionExpression = 
  FunctionExpression (Maybe Identifier) (Maybe FormalParameterList) FunctionBody deriving (Show)


type FormalParameterList = [Identifier]

data FunctionBody = 
  FunctionBody (Maybe SourceElements) deriving (Show)


data Program = 
  Program (Maybe SourceElements) deriving (Show)


type SourceElements = [SourceElement]

data SourceElement = 
  SourceElement Statement
  | SourceElementFunc FunctionDeclaration deriving (Show)


