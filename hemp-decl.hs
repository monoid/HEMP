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

data GDeclration = GFunctionDeclration String [Argument] [Type] [Expression]
                 | GTypeDeclaration String Type
                 | ForwardFunctionDecl [Type] [Type]
                 deriving (Show, Eq)

data Argument = Argument String Type deriving (Show, Eq)

data PrimitiveType = BooleanType
                   | NumericType NumericType
                   | NullType
                   | CharacterType
                   deriving (Show, Eq, Ord)

data NumericType = RealTypes RealTypes
                 | ComplexType FractionalType
                 deriving (Show, Eq, Ord)

data RealTypes = IntegerType
               | FractionalType FractionalType
               deriving (Show, Eq, Ord)

data FractionalType = RealType
                    | DoubleType
                    deriving (Show, Eq, Ord)

data Type = NamedType String
          | PrimitiveType PrimitiveType
          | ArrayType Int Type
          | StreamType Type
          | RecordType [([String], Type)]
          | UnionType [([String], Type)]
          | FunctionType [Type] [Type]
          deriving (Show, Eq)

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
                -- List of if/elseif conds and exps, and then else exps
                | IfThenElse [(Expression, [Expression])] [Expression]
                | Let [([String], [Expression])] [Expression]
                deriving (Show, Eq)
