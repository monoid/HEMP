module HempDecl where
import LLVM.Core


data BinaryCode = BoPlus
                | BoMinus
                | BoMult
                | BoDiv
                | BoExpt
                | BoAppend
                | BoAnd
                | BoOr
                deriving (Show, Eq)

data Token =
           LIntVal  (Integer, Bool) |
           LFloatVal String |
           LString   String |
           LChar     Char   |
           LEChar    Char   |
           LIdent    String |
           LAssign          |
           LPtPt            |
           LPoint           |
           LComma           |
           LColon           |
           LSemicolon       |
           LLeftSqBr        |
           LRighSqBr        |
           LLeftRoBr        |
           LRighRoBr        |
           LCmp CmpPredicate | -- Comparsion: <, >, <=, >=, ~=
           LEqual           |
           LBin BinaryCode  |
           LNot             |
           -- keywords
           LArray           |
           LAt              |
           LBegin           |
           LBoolean         |
           LCase            |
           LCharacter       |
           LComplex         |
           LCross           |
           LDefault         |
           LDo              |
           LDot             |
           LDouble          |
           LDoubleComplex   |
           LElse            |
           LElseIf          |
           LEnd             |
           LError           |
           LFalse           |
           LFor             |
           LForeign         |
           LForward         |
           LFrom            |
           LFunction        |
           LIf              |
           LImag            |
           LInOut           |
           LInteger         |
           LIn              |
           LInterface       |
           LIs              |
           LLet             |
           LModule          |
           LNil             |
           LNull            |
           LOf              |
           LOtherwise       |
           LOut             |
           LProgram         |
           LReal            |
           LRecord          |
           LReturns         |
           LState           |
           LStream          |
           LSuffix          |
           LThen            |
           LTo              |
           LTrue            |
           LType            |
           LWhen            |
           LWhile           |
           LUnion           |
           LUnless          |
           LUntil           |
           LInitial         |
           LOld             |
           LValue           |
           LDefine          |
           LUnderscore
           deriving (Eq, Show)

binop (LBin bc) a b = BinOp bc a b

data GDeclration = GFunctionDeclration String [Argument] [Type] [Expression]
                 | GTypeDeclaration String Type
                 | ForwardFunctionDecl [Type] [Type]
                 deriving (Show, Eq)

type Argument = (String, Type)

data TPrimitive = TBoolean
                | TNum TNum
                | TNull
                | TChar
                deriving (Show, Eq, Ord)

data TNum = RealTypes RealTypes
          | TComplex TFrac
          deriving (Show, Eq, Ord)

data RealTypes = TInteger
               | TFrac TFrac
               deriving (Show, Eq, Ord)

data TFrac = TReal
           | TDouble
           deriving (Show, Eq, Ord)

data Type = NamedType String
          | TPrimitive TPrimitive
          | TArray Int Type
          | TStream Type
          | TRecord [(String, Type)]
          | TUnion [(String, Type)]
          | TFunction [Type] [Type]
          | TTuple [Type]
          deriving (Show, Eq)

data Expression = Constant Token
                | Identifier String
                | Aref Expression [Expression]
                | RecordAccess Expression Token
                | FunCall Expression [Expression]
                | Not Expression
                | Neg Expression
                | BoolOp CmpPredicate Expression Expression
                | BinOp BinaryCode Expression Expression
                | Complex Expression Expression
                | Old Expression
                -- Actually, there should be special node that constructs
                -- SISAL virtual 'tuples', and special type for these tuples.
                -- Lists should be eliminated in IfThenElse, Let and loops.
                | IfThenElse Expression [Expression] [Expression]
                | ForLoop ForRange [([String],[Expression])] [(LoopExpression, Maybe (Bool, Expression))]
                | Let [([String], [Expression])] [Expression]
                deriving (Show, Eq)

data ForRange = ForRangeCross ForRange ForRange
                | ForRangeDot ForRange ForRange
                | ForInRange String Expression Expression
                | ForInArray String Expression
                | ForInArrayIndexed String Expression String
                deriving (Show, Eq)

data LoopExpression = ArrayOf Expression
                    | ValueOf String Expression
                    deriving (Show, Eq)

-- Sequence of bindings (variable name and type information, inferred
-- or declared) and, maybe, parent environment.
type EnvDict = [(String, Type)]

-- Environment has optional parent environment and environment dictonary
data Env = Env (Maybe Env) EnvDict


data TExp = TConstant Token
          | TVariable String -- TODO
          | TConversion TPair Type -- From type
          | TArith BinaryCode TPair TPair
          | TCmp CmpPredicate TPair TPair
          | TStdFuncall String [TPair]
          | TFuncall String [TPair]
          | TNot TPair
          | TNeg TPair
          | TConv TPair Type
          -- Compound expressions
          | TSimpleIfThenElse TPair TPair TPair
          | TIfThenElse TPair [TPair] [TPair]
          | TLet [([String], [TPair])] [TPair]
          deriving (Show, Eq)

-- Expression with type
data TPair = TPair TExp Type -- TODO: what is type of TIfThenElse?
             deriving (Show, Eq)
