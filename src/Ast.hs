module Ast where


data File = File
  { packageDec :: PackageDec
  , imports :: [ImportAst]
  , primaryClass :: Class
  } deriving (Show)

data PackageDec = PackageDec Path
  deriving (Show)

data ImportAst = ImportAst Path
  deriving (Show)

data Path = Path [String]
  deriving (Show, Eq)

data SuperClass = SuperClass Path [TypeParam]
  deriving (Show)

data Class = Class
  { visibility :: Visibility
  , className :: String
  , classType :: ClassType
  , classTypeParams :: [TypeParam]
  , superClass :: Maybe SuperClass
  , interfaces :: [SuperClass]
  , members :: [Member]
  } deriving (Show)

data Visibility = PublicVis | ProtectedVis | PrivateVis
  deriving (Show)

data ClassType = RegularType | InterfaceType | AbstractType
  deriving (Show)

data TypeParam = TypeParam
  { typeName :: String
  , bounds :: [Type]
  } deriving (Show)

data Member = FieldMember Field | MethodMember Method
  deriving (Show)

data Field = Field
  { fieldVisibility :: Visibility
  , fieldName :: String
  , fieldType :: Type
  } deriving (Show)

data Method = Method
  { methodVisibility :: Visibility
  , isStatic :: Bool
  , isAbstract :: Bool
  , isConst :: Bool
  , methodName :: String
  , methodTypeParams :: [TypeParam]
  , params :: [Param]
  , returnType :: Type
  , body :: MethodBody
  } deriving (Show)

data MethodBody = Prototype | MethodBody [Statement] | Native String | Redirect String
  deriving (Show)

data Param = Param
  { paramName :: String
  , paramType :: Type
  } deriving (Show, Eq)

data Type = Primitive PrimitiveType | Array Type | ClassType Path | Arguments Type [Type]
  deriving (Show, Eq)

data PrimitiveType = UnitPrimType | U8PrimType | U16PrimType | U32PrimType | U64PrimType | I8PrimType | I16PrimType | I32PrimType | I64PrimType | F32PrimType | F64PrimType | BoolPrimType | CharPrimType
  deriving (Show, Eq)

data Statement = WhileStmt WhileStatement | ForStmt ForStatement | IfStmt IfExpr | ReturnStmt ReturnStatement | LetStmt LetStatement | ExprStmt Expr | BreakStmt BreakStatement | ContinueStmt ContinueStatement | Hanging Expr
  deriving (Show, Eq)

data WhileStatement = WhileStatement Expr [Statement]
  deriving (Show, Eq)

data ForStatement = ForStatement String Expr [Statement]
  deriving (Show, Eq)

data ReturnStatement = ReturnExpr Expr | ReturnUnit
  deriving (Show, Eq)

data LetStatement = LetStatement String (Maybe Type) Expr
  deriving (Show, Eq)

data BreakStatement = BreakStatement
  deriving (Show, Eq)

data ContinueStatement = ContinueStatement
  deriving (Show, Eq)

data IfExpr = IfExpr Expr [Statement] (Maybe (Either [Statement] IfExpr))
  deriving (Show, Eq)

data Expr =
    BinaryOp BinaryOp Expr Expr
  | UnaryOp UnaryOp Expr
  | Call Expr [Expr]
  | NewExpr Path [Expr]
  | Var String
  | FieldAccess Expr String
  | ArrayAccess Expr Expr
  | ArrayLiteral [Expr]
  | Literal Literal
  | StaticAccess Path
  | ThisExpr
  | SuperExpr
  | NullExpr
  | Cast Type Expr
  | InstanceOf Expr Type
  | Paren Expr
  | IfExprExpr IfExpr
  | Closure [Param] [Statement]
  deriving (Show, Eq)

data BinaryOp = Add | Sub | Mul | Div | Mod | And | Or | Eq | Neq | Lt | Gt | Le | Ge | BitAnd | BitOr | BitXor | LShift | RShift | ExclusiveRangeOp | InclusiveRangeOp | LogicalAnd | LogicalOr
  deriving (Show, Eq)

data UnaryOp = Neg | NotOp
  deriving (Show, Eq)

data Literal = IntLit String | FloatLit String | BoolLit Bool | CharLit Char | StringLit String
  deriving (Show, Eq)




