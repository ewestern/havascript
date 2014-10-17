module Syntax where
import qualified Data.Text as T

data NumLiteral =
   DecLiteral Float
  | HexLiteral T.Text

data Literal =
  NullLiteral
  | BooleanLiteral Bool
  | NumericLiteral NumLiteral 
  | StringLiteral T.Text
  | RegExLiteral T.Text

type Identifier = T.Text 
data PrimaryExpression = 
   This
  | IdentifierExp Identifier
  | LiteralExp Literal
  | ArrayExp ArrayLiteral
  | ObjectExp ObjectLiteral
  | ExpressionExp Expression

type ArrayLiteral =  [AssignmentExpression]
type ObjectLiteral = PropertyNameAndValueList

type PropertyNameAndValueList = [PropertyAssignment]

data PropertyAssignment = 
  PropertyAssignment PropertyName AssignmentExpression
  | PropertyGet PropertyName FunctionBody
  | PropertySet PropertyName Identifier FunctionBody

data PropertyName = 
  PropertyNameIdentifier Identifier
  | PropertyNameString T.Text 
  | PropertyNameNumber NumLiteral

type PropertySetParamList = Identifier 

data MemberExpression = 
  MemberExpressionPrimary PrimaryExpression
  | MemberExpressionFunction FunctionExpression
  | MemberExpressionSquare Expression
  | MemberExpressionDot  Identifier
  | MemberExpressionNew MemberExpression Arguments

data NewExpression =
  NewExpressionMember MemberExpression
  | NewExpressionNew NewExpression

data CallExpression = 
  CallExpressionMember MemberExpression Arguments
  | CallExpressionArgs Arguments
  | CallExpressionExp Expression
  | CallExpressionIdent Identifier

type Arguments = ArgumentList

type ArgumentList = [AssignmentExpression]

data LHSExpression = 
  LHSExpressionNew NewExpression
  | LHSExpressionCall CallExpression

data PostfixExpression = 
  PostfixLHS LHSExpression
  | PostfixInc LHSExpression
  | PostgixDec LHSExpression

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
  | UnaryExpressionNot UnaryExpression

data MultiplicativeExpression = 
  MultUnary UnaryExpression
  | MultTimes MultiplicativeExpression UnaryExpression
  | MultDivide MultiplicativeExpression UnaryExpression
  | MultMod MultiplicativeExpression UnaryExpression

data AdditiveExpression = 
  AddMult MultiplicativeExpression
  | AddPlus MultiplicativeExpression
  | AddMinus MultiplicativeExpression

data ShiftExpression = 
  ShiftAdd AdditiveExpression
  | ShiftLeft ShiftExpression AdditiveExpression
  | ShiftRight ShiftExpression AdditiveExpression
  | ShiftZ ShiftExpression AdditiveExpression
  
data RelationalExpression = 
  RelationShift ShiftExpression
  | RelationLT RelationalExpression ShiftExpression
  | RelationGT RelationalExpression ShiftExpression
  | RelationLTE RelationalExpression ShiftExpression
  | RelationGTE RelationalExpression ShiftExpression
  | RelationIO RelationalExpression ShiftExpression
  | RelationIn RelationalExpression ShiftExpression
  
data RelationalExpressionNI = 
  RelationShiftNI ShiftExpression
  | RelationLTNI RelationalExpressionNI ShiftExpression
  | RelationGTNI RelationalExpressionNI ShiftExpression
  | RelationLTENI RelationalExpressionNI ShiftExpression
  | RelationGTENI  RelationalExpressionNI ShiftExpression
  | RelationIONI RelationalExpressionNI ShiftExpression  

data EqualityExpression = 
  EqualityExp ShiftExpression
  | EqualityExpEq EqualityExpression ShiftExpression
  | EqualityExpNE EqualityExpression ShiftExpression
  | EqualityExpEqq EqualityExpression ShiftExpression
  | EqualityExpNEE EqualityExpression ShiftExpression

data BitwiseAndExp =
  BitwiseAndEq EqualityExpression
  | BitwiseAnd BitwiseAndExp EqualityExpression

data BitwiseAndExpNI =
  BitwiseAndNIEq EqualityExpression
  | BitwiseAndNI BitwiseAndExp EqualityExpression
 
data BitwiseXORExp = 
  BitwiseXORExp BitwiseAndExp
  | BitwiseXORExpHat BitwiseXORExp BitwiseAndExp
   

data BitwiseXORExpNI = 
  BitwiseXORExpNI BitwiseAndExpNI
  | BitwiseXORExpHatNI BitwiseXORExpNI BitwiseAndExpNI


data BitwiseORExp = 
  BitwiseOrExp BitwiseXORExp
  | BitwiseOrExpPipe BitwiseORExp BitwiseXORExp

data BitwiseORExpNI = 
  BitwiseOrExpNI BitwiseXORExpNI
  | BitwiseORExpPipeNI BitwiseORExpNI BitwiseXORExpNI

data LogicalAndExp = 
  LogicalAndExp BitwiseORExp
  | LogicalAndExpAmp LogicalAndExp BitwiseORExp

data LogicalAndExpNI = 
  LogicalAndExpNI BitwiseORExpNI
  | LogicalAndExpAmpNI LogicalAndExpNI BitwiseORExpNI

data LogicalOrExp = 
  LogicalOrExp LogicalAndExp
  | LogicalOrExpDPipe LogicalOrExp LogicalAndExp

data LogicalOrExpNI = 
  LogicalOrExpNI LogicalAndExpNI
  | LogicalOrExpDPipeNI LogicalOrExpNI LogicalAndExpNI

data ConditionalExp = 
  ConditionalExp LogicalOrExp
  | ConditionalExpTern LogicalOrExp AssignmentExpression AssignmentExpression

data ConditionalExpNI =
  ConditionalExpNI LogicalOrExpNI
  | ConditionalExpTernNI LogicalOrExpNI AssignmentExpression AssignmentExpressionNI

data AssignmentExpression = 
  AssignmentExpression ConditionalExp
  | AssignmentExpEq LHSExpression AssignmentExpression
  | AssignmentExpOp LHSExpression AssignmentOperator AssignmentExpression

data AssignmentExpressionNI = 
  AssignmentExpressionNI ConditionalExpNI
  | AssignmentExpEqNI LHSExpression AssignmentExpressionNI
  | AssignmentExpOpNI LHSExpression AssignmentOperator AssignmentExpressionNI

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
  | OREquals

data Expression = 
  Expression AssignmentExpression
  | ExpressionSeq AssignmentExpression

data ExpressionNI = 
  ExpressionNI AssignmentExpressionNI
  | ExpressionNISeq AssignmentExpressionNI

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
  | DebugStmt DebuggerStatement
  
type Block = StatementList

type StatementList = [Statement]

type VariableStatement = VariableDeclarationList

type VariableDeclarationList = [VariableDeclaration]

type VariableDeclarationListNI = [VariableDeclarationNI]

data VariableDeclaration = VariableDeclaration Identifier (Maybe Initializer)


data VariableDeclarationNI = VariableDeclarationNI Identifier (Maybe InitializerNI)

type Initializer = AssignmentExpression
type InitializerNI = AssignmentExpressionNI

data EmptyStatement = EmptyStatement

-- todo: what is this
data ExpressionStatement = ExpressionStatement

data IfStatement = 
  IfElseStatement Expression Statement Statement
  | IfStatement Expression Statement

data IterationStatement = 
  DoWhile Statement Expression
  | While Expression Statement
  | ForExp (Maybe ExpressionNI) (Maybe Expression) (Maybe Expression) Statement
  | ForVar VariableDeclarationListNI (Maybe Expression) (Maybe Expression) Statement
  | ForLHS LHSExpression Expression Statement
  | ForVarIn VariableDeclarationNI Expression Statement

type ContinueStatement = Maybe Identifier

type BreakStatement = Maybe Identifier

type ReturnStatement = Maybe Expression

data WithStatement = 
  WithStatement Expression Statement

data SwitchStatement = 
  SwitchStatement Expression CaseBlock

data CaseBlock = 
  CaseBlock (Maybe CaseClauses) (Maybe DefaultClause) (Maybe CaseClauses)

type CaseClauses = [CaseClause]

data CaseClause = 
  CaseClause Expression (Maybe StatementList)

data DefaultClause = 
  DefaultClause (Maybe StatementList)

data LabelledStatement = 
  LabelledStatement Identifier Statement

data ThrowStatement = 
  ThrowStatement Expression

data TryStatement = 
  TryCatch Block Catch
  | TryFinally Block Finally
  | TryCF Block Catch Finally

data Catch = Catch Identifier Block

data Finally = Finally Block

data DebuggerStatement = Debugger

data FunctionDeclaration = 
  FunctionDeclaration Identifier (Maybe FormalParameterList) FunctionBody
 
data FunctionExpression = 
  FunctionExpression (Maybe Identifier) (Maybe FormalParameterList) FunctionBody

type FormalParameterList = [Identifier]

data FunctionBody = 
  FunctionBody (Maybe SourceElements)

data Program = 
  Program (Maybe SourceElements)

type SourceElements = [SourceElement]

data SourceElement = 
  SourceElement Statement
  | SourceElementFunc FunctionDeclaration



