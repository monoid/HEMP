module HempDecl where

data Token =
           TIntVal  (Integer, Bool) |
           TFloatVal String |
           TString   String |
           TChar     Char   |
           TEChar    Char   |
           TIdent    String |
           TAssign          |
           TPtPt            |
           TPoint           |
           TComma           |
           TColon           |
           TSemicolon       |
           TLeftSqBr        |
           TRighSqBr        |
           TLeftRoBr        |
           TRighRoBr        |
           TCmp      String | -- Comparsion: <, >, <=, >=, ~=
           TEqual           |
           TPlus            |
           TMinus           |
           TExpt            |
           TAppend          |
           TMult            |
           TDiv             |
           TAnd             |
           TOr              |
           TNot             |
           -- keywords
           TArray           |
           TAt              |
           TBegin           |
           TBoolean         |
           TCase            |
           TCharacter       |
           TComplex         |
           TCross           |
           TDefault         |
           TDo              |
           TDot             |
           TDouble          |
           TDoubleComplex   |
           TElse            |
           TElseIf          |
           TEnd             |
           TError           |
           TFalse           |
           TFor             |
           TForeign         |
           TForward         |
           TFrom            |
           TFunction        |
           TIf              |
           TImag            |
           TInOut           |
           TInteger         |
           TIn              |
           TInterface       |
           TIs              |
           TLet             |
           TModule          |
           TNil             |
           TNull            |
           TOf              |
           TOtherwise       |
           TOut             |
           TProgram         |
           TReal            |
           TRecord          |
           TReturns         |
           TState           |
           TStream          |
           TSuffix          |
           TThen            |
           TTo              |
           TTrue            |
           TType            |
           TWhen            |
           TWhile           |
           TUnion           |
           TUnless          |
           TUntil           |
           TInitial         |
           TOld             |
           TValue           |
           TDefine          |
           TUnderscore
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
