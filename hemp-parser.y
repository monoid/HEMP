{ -- -*- mode: haskell -*-
module Main where
import HempLexer
}

%name hemp
%tokentype { Token }
%error { parseError }

%left "||"
%left "|"
%left "&"
%nonassoc cmp "="
%left "+" "-"
%left "*" "/"
%right "**"
%left "[" "("
%left "."
%left UNARY

%token
        int     { TIntVal $$ }
        float   { TFloatVal $$ }
        string  { TString $$ }
        char    { TChar $$ }
        echar   { TEChar $$ }
        identifier { TIdent $$ }
        ":="       { TAssign }
        ".."       { TPtPt }
        "."        { TPoint }
        ","        { TComma }
        ":"        { TColon }
        ";"        { TSemicolon }
        "["        { TLeftSqBr }
        "]"        { TRighSqBr }
        "("        { TLeftRoBr }
        ")"        { TRighRoBr }
        cmp        { TCmp $$ }
        "="        { TEqual }
        "+"        { TPlus }
        "-"        { TMinus }
        "**"       { TExpt }
        "||"       { TAppend }
        "*"        { TMult }
        "/"        { TDiv }
        "&"        { TAnd }
        "|"        { TOr }
        "~"        { TNot }
        array { TArray }
        at { TAt }
        begin { TBegin }
        boolean { TBoolean }
        case { TCase }
        character { TCharacter }
        complex { TComplex }
        cross { TCross }
        default { TDefault }
        do { TDo }
        dot { TDot }
        double { TDouble }
        doublecomplex { TDoubleComplex }
        else { TElse }
        elseif { TElseIf }
        end { TEnd }
        err { TError }
        false { TFalse }
        for { TFor }
        foreign { TForeign }
        forward { TForward }
        from { TFrom }
        function { TFunction }
        if { TIf }
        imag { TImag }
        inout { TInOut }
        integer { TInteger }
        in { TIn }
        interface { TInterface }
        is { TIs }
        let { TLet }
        module { TModule }
        nil { TNil }
        null { TNull }
        of { TOf }
        otherwise { TOtherwise }
        out { TOut }
        program { TProgram }
        real { TReal }
        record { TRecord }
        returns { TReturns }
        state { TState }
        stream { TStream }
        suffix { TSuffix }
        then { TThen }
        to { TTo }
        true { TTrue }
        type { TType }
        when { TWhen }
        while { TWhile }
        union { TUnion }
        unless { TUnless }
        until { TUntil }
        initial { TInitial }
        old { TOld }
        value { TValue }
        define { TDefine }
        underscore { TUnderscore }
%%

Program:
        TopDeclarationList { reverse $1 }

--
-- Types
--
Type:
        identifier { NamedType $1 }
        | PrimitiveType { PrimitiveType $1 }
        | ArrayType     { $1 }
        | StreamType    { $1 }
        | RecordType    { $1 }
        | UnionType     { $1 }
        | FunctionType  { $1 }

TypeList:
        Type { [$1] }
        | TypeList "," Type { $1 ++ [$3] }

MaybeTypeList:
        TypeList { $1 }
        | { [] }

PrimitiveType:
        boolean { BooleanType }
        | integer { IntegerType }
        | real    { RealType }
        | double  { DoubleType }
        | complex { ComplexType }
        | doublecomplex { DoubleComplexType }
        | null    { NullType }
        | character { CharacterType }

-- Array type
ArrayType:
        array DimSpec of Type { ArrayType $2 $4 }
DimSpec:
        "[" DimSpec1 "]" { $2 }
        | { 1::Int }
DimSpec1:
        ".." { 1::Int }
        | DimSpec1 "," ".." { $1 + 1 }

-- Stream type
StreamType:
        stream of Type { StreamType $3 }

-- Record and union has common structure
RecordType:
        record "[" ListOfRecFields MayBeComma "]" { RecordType (reverse $3) }
UnionType:
        union "[" ListOfRecFields MayBeComma "]" { UnionType (reverse $3) }
ListOfRecFields:
        FieldGroup { [$1] }
        | ListOfRecFields "," FieldGroup { $3:$1 }
FieldGroup:
        IdentifierList ":" Type { ($1, $3) }
IdentifierList:
        identifier { [$1] }
        | IdentifierList "," identifier { $1++[$3] }
MayBeComma:
        "," { 0 }
        | { 0 }

-- Function type
FunctionType:
        function "(" MaybeTypeList returns TypeList ")" { FunctionType $3 $5 }

--
-- Top-level declarations: functions and type definitions
-- 
TopDeclarationList:
        FunctionOrTypeDef { [$1] }
        | TopDeclarationList FunctionOrTypeDef { $2:$1 }

FunctionOrTypeDef:
        FunctionDecl { $1 }
        | TypeDecl   { $1 }
        | ForwardFunctionDecl { $1 }

ExpressionList:
        Expression { [$1] }
        -- TODO: suboptimal
        | ExpressionList "," Expression { $1 ++ [$3] }

Expression:
        Constant { Constant $1 }
        | identifier { Identifier $1 }
        | "(" Expression ")" { $2 }
        | old Expression %prec UNARY { Old $2 }
        | "~" Expression %prec UNARY { Not $2 }
        | "+" Expression %prec UNARY { $2 }
        | "-" Expression %prec UNARY { Neg $2 }
        | Expression "." identifier { RecordAccess $1 $2 }
        -- TODO: array/stream access contains not just Expression List,
        -- but it may contain ranges...
        | Expression "[" ExpressionList "]" { Aref $1 $3 }
        | Expression "(" ExpressionList ")" { FunCall $1 $3 }
        | Expression "|" Expression { BinOp $2 $1 $3 }
        | Expression "&" Expression { BinOp $2 $1 $3 }
        | Expression "||" Expression { BinOp $2 $1 $3 }
        | Expression "+" Expression { BinOp $2 $1 $3 }
        | Expression "-" Expression { BinOp $2 $1 $3 }
        | Expression "*" Expression { BinOp $2 $1 $3 }
        | Expression "/" Expression { BinOp $2 $1 $3 }
        | Expression cmp Expression { BinOp (TCmp $2) $1 $3 }
        | Expression "=" Expression { BinOp $2 $1 $3 }
        | Expression "**" Expression { BinOp $2 $1 $3 }
        | "(" Expression "," Expression ")" { Complex $2 $4 }
        | CompaundExpression { $1 }

-- Just to keep Expression rule managable, we introduce CompaundExpression
CompaundExpression:
        if IfConditions else ExpressionList end if { IfThenElse $2 $4 }
        | let ListOfAssignments MayBeSemicolon in ExpressionList end let { Let $2 $5 }

ListOfAssignments:
        Assignment { [$1] }
        | ListOfAssignments ";" Assignment { $1 ++ [$3] }
Assignment:
        IdentifierList ":=" ExpressionList { ($1, $3) }
MayBeSemicolon:
        ";" { 0 }
        | { 0 }

IfConditions:
        IfCondition { [$1] }
        | IfConditions elseif IfCondition { $1 ++ [$3] }
IfCondition:
        Expression then ExpressionList { ($1, $3) }

Constant:
        int { TIntVal $1 }
        | float { TFloatVal $1 }
        | string { TString $1 }
        | char { TChar $1 }
        | echar { TEChar $1 }

FunctionDecl:
        function identifier "(" MayBeArgDecl returns TypeList  ")" ExpressionList end function { GFunctionDeclration $2 $4 $6 $8 }

MayBeArgDecl:
        ArgDecl { $1 }
        | { [] }

ArgDecl:
        ArgsNType { $1 }
        | ArgDecl ";" ArgsNType { $1 ++ $3 }

ArgsNType:
        IdentifierList ":" Type { zipWith Argument $1 (repeat $3) }

ForwardFunctionDecl:
        forward function identifier "(" MaybeTypeList returns TypeList ")" ";"
        { ForwardFunctionDecl $5 $7 }

TypeDecl:
        type identifier "=" Type ";" { GTypeDeclaration $2 $4 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data GDeclration = GFunctionDeclration String [Argument] [Type] [Expression]
                 | GTypeDeclaration String Type
                 | ForwardFunctionDecl [Type] [Type]
                 deriving (Show, Eq)

data Argument = Argument String Type deriving (Show, Eq)

data PrimitiveType = BooleanType
                   | IntegerType
                   | RealType
                   | DoubleType
                   | ComplexType
                   | DoubleComplexType
                   | NullType
                   | CharacterType
                   deriving (Show, Eq)


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

main = do
     inStr <- getContents
     let parseTree = hemp (alexScanTokens inStr)
     putStrLn ("parseTree: " ++ show(parseTree))
     print "done"
}
