
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


data BinaryOperator
  = MultTimes
  | MultDivide
  | MultMod
  | AddPlus
  | AddMinus
  | ShiftLeft
  | ShiftRight
  | ShiftRRight
  | RelLT
  | RelGT
  | RelLTE
  | RelGTE
  | RelInstanceOf
  | RelIn
  | EqEq
  | EqNE
  | EqEqq
  | EqNEE
  | BitOr
  | BitAnd
  | BitXOr
  | LogAnd
  | LogOr deriving (Eq, Show)
  

data PrefixOperator
  = PrefixDelete
  | PrefixVoid
  | PrefixIncr
  | PrefixDecr
  | PrefixPlus
  | PrefixMinus
  | PrefixTilde
  | PrefixNot deriving (Eq, Show)

data PostfixOperator
  = PostfixIncr
  | PostfixDecr deriving (Eq, Show)

--- 

data OperatorExpression
  =  OperatorExpressionBinary OperatorExpression BinaryOperator OperatorExpression
  |  OperatorExpressionPrefix PrefixOperator OperatorExpression 
  |  OperatorExpressionPostfix OperatorExpression PostfixOperator
  |  OperatorExpressionLHS LHSExpression deriving (Eq, Show)
  


data ConditionalExp = 
    ConditionalExp OperatorExpression
  | ConditionalExpTern OperatorExpression AssignmentExpression AssignmentExpression deriving (Eq, Show)

data AssignmentExpression = 
  AssignmentExpression ConditionalExp
  | AssignmentExpEq LHSExpression AssignmentExpression
  | AssignmentExpOp LHSExpression AssignmentOperator AssignmentExpression deriving (Eq, Show)

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

data VariableDeclaration = VariableDeclaration Identifier (Maybe Initializer) deriving (Eq, Show)

type Initializer = AssignmentExpression

data EmptyStatement = EmptyStatement deriving (Eq, Show)

-- todo: what is this
data ExpressionStatement = ExpressionStatement Expression deriving (Eq, Show)


data IfStatement = IfStatement Expression Statement (Maybe Statement) deriving (Eq, Show)


data IterationStatement = 
  DoWhile Statement Expression
  | While Expression Statement
-- TODO come back to these!
  {-| ForExp (Maybe ExpressionNI) (Maybe Expression) (Maybe Expression) Statement-}
  {-| ForVar VariableDeclarationListNI (Maybe Expression) (Maybe Expression) Statement-}
  | ForLHS LHSExpression Expression Statement deriving (Eq, Show)



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
  FunctionDeclaration Identifier FormalParameterList FunctionBody deriving (Eq, Show)

 
data FunctionExpression = 
  FunctionExpression (Maybe Identifier) FormalParameterList FunctionBody deriving (Eq, Show)


type FormalParameterList = [Identifier]

data FunctionBody = 
  FunctionBody (Maybe SourceElements) deriving (Eq, Show)


data Program = 
  Program SourceElements deriving (Eq, Show)


type SourceElements = [SourceElement]

data SourceElement = 
    SourceElement Statement
  | SourceElementFunc FunctionDeclaration deriving (Eq, Show)


