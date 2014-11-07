{-#Language GADTs #-}
{-#Language ExistentialQuantification #-}

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

data RelationalOperator = 
  LT
  | GT
  | LTE
  | GTE
  | IO deriving (Show)

data ShiftOperator = 
  LShift
  | RShift
  | ZShift deriving (Show)

data AdditiveOperator = 
  Plus
  | Minus deriving (Show)

data MultiplicativeOperator = 
  Times
  | Divide
  | Modulo deriving (Show)

data UnaryOperator = 
  Delete
  | Void
  | Typeof
  | IncU
  | DecU
  | PosU
  | Neg
  | Tilde
  | Not deriving (Show)

data PostfixOperator = 
  IncP
  | DecP deriving (Show)

data AssignmentExpression = forall a . AssignmentExpression (Exp a)

instance Show AssignmentExpression where
  show (AssignmentExpression a) = show a

data Exp a where
  Assignment :: LHSExpression -> Exp a -> Exp a 
  AssignmentOp :: LHSExpression -> AssignmentOperator -> Exp a -> Exp a
  Conditional :: Exp a -> Exp a -> Exp a -> Exp a  
  LogicalOr :: Exp a -> Exp a -> Exp a 
  LogicalAnd :: Exp a -> Exp a -> Exp a 
  BitwiseOr  :: Exp a -> Exp a -> Exp a 
  BitwiseXOR :: Exp a -> Exp a -> Exp a
  {-RelationalExp :: Exp a -> Exp a -> Exp a-}
  BitwiseAnd :: Exp a -> Exp a -> Exp a
  Equality :: EqualityOperator -> Exp a -> Exp a -> Exp a
  Relational :: RelationalOperator -> Exp a -> Exp a -> Exp a
  Shift :: ShiftOperator -> Exp a -> Exp a -> Exp a
  Additive :: AdditiveOperator -> Exp a -> Exp a -> Exp a
  Multiplicative :: MultiplicativeOperator -> Exp a -> Exp a -> Exp a
  Unary :: UnaryOperator -> Exp a -> Exp a
  Postfix :: PostfixOperator -> LHSExpression -> Exp a
  deriving (Show)


data EqualityOperator = 
  Equal
  | NotEqual
  | IsEqual 
  | IsNotEqual deriving (Show)

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

{-data ExpressionNI = -}
  {-ExpressionNI AssignmentExpressionNI-}
  {-| ExpressionNISeq AssignmentExpressionNI deriving (Show)-}


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

{-type VariableDeclarationListNI = [VariableDeclarationNI]-}

data VariableDeclaration = VariableDeclaration Identifier (Maybe Initializer) deriving (Show)

{-data VariableDeclarationNI = VariableDeclarationNI Identifier (Maybe InitializerNI) deriving (Show)-}


type Initializer = AssignmentExpression
{-type InitializerNI = AssignmentExpressionNI-}

data EmptyStatement = EmptyStatement deriving (Show)

-- todo: what is this
data ExpressionStatement = ExpressionStatement Expression deriving (Show)


data IfStatement = IfStatement Expression Statement (Maybe Statement) deriving (Show)


data IterationStatement = 
  DoWhile Statement Expression
  | While Expression Statement
  {-| ForExp (Maybe ExpressionNI) (Maybe Expression) (Maybe Expression) Statement-}
  {-| ForVar VariableDeclarationListNI (Maybe Expression) (Maybe Expression) Statement-}
  | ForLHS LHSExpression Expression Statement deriving (Show)
  {-| ForVarIn VariableDeclarationNI Expression Statement deriving (Show)-}


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


