module HempDecl where

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
           LCmp      String | -- Comparsion: <, >, <=, >=, ~=
           LEqual           |
           LPlus            |
           LMinus           |
           LExpt            |
           LAppend          |
           LMult            |
           LDiv             |
           LAnd             |
           LOr              |
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

data GDeclration = GFunctionDeclration String [Argument] [Type Single] [Expression]
                 | GTypeDeclaration String (Type Single)
                 | ForwardFunctionDecl [Type Single] [Type Single]
                 deriving (Show)

data Argument = Argument String (Type Single) deriving (Show)

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

data Single
data Tuple

data Type a where
     NamedType :: String -> Type Single
     TPrimitive :: TPrimitive -> Type Single
     TArray :: Int -> Type Single -> Type Single
     TStream :: Type Single -> Type Single
     TRecord :: [(String, Type Single)] -> Type Single
     TUnion :: [(String, Type Single)] -> Type Single
     TFunction :: [Type Single] -> [Type Single] -> Type Single
     TTuple :: [Type Single] -> Type Tuple

deriving instance (Show (Type a))


data Expression = Constant Token
                | Identifier String
                | Aref Expression [Expression]
                | RecordAccess Expression Token
                | FunCall Expression [Expression]
                | Not Expression
                | Neg Expression
                | BinOp Token Expression Expression
                | Complex Expression Expression
                | Old Expression
                -- SISAL's virtual 'tuples' that can be returned by
                -- compound expressions like if, loops and functions.
                -- Values of this type cannot be assigned to
                -- variables (but each element can).
                | Tuple [Expression]
                -- Compound expressions
                | IfThenElse Expression [Expression] [Expression]
                | Let [([String], [Expression])] [Expression]
                deriving (Show, Eq)
