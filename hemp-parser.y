{
module Main where
import HempLexer
}

%name hemp
%tokentype { Token }
%error { parseError }

%token
        int     { TIntVal $$ }
        float   { TFloatVal $$ }
        string  { TString $$ }
        char    { TChar $$ }
        echar   { TEChar $$ }
        identifier { TIdent $$ }
        assign     { TAssign }
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
        boolean { $1 }
        | integer { $1 }
        | real    { $1 }
        | double  { $1 }
        | complex { $1 }
        | doublecomplex { $1 }
        | null    { $1 }
        | character { $1 }

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
        IdentifierList ":" Type { (reverse $1, $3) }
IdentifierList:
        identifier { [$1] }
        | IdentifierList "," identifier { $3:$1 }
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
        | Expression "[" Expression "]" { Aref $1 $3 }
        | Expression "(" ExpressionList ")" { FunCall $1 $3 }
        | "~" Expression { Not $2 }
        | "+" Expression { $2 }
        | "-" Expression { Neg $2 }
        | "(" Expression "," Expression ")" { Complex $2 $4 }
        | if Expression then ExpressionList else ExpressionList { IfThenElse $2 $4 $6 }

Constant:
        int { TIntVal $1 }
        | float { TFloatVal $1 }
        | string { TString $1 }
        | char { TChar $1 }
        | echar { TEChar $1 }

FunctionDecl:
        function identifier "("  ")" ExpressionList end function { GFunctionDeclration $2 [] $5 }

ForwardFunctionDecl:
        forward function identifier "(" MaybeTypeList returns TypeList ")" ";"
        { ForwardFunctionDecl $5 $7 }

TypeDecl:
        type identifier "=" Type ";" { GTypeDeclaration $2 $4 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data GDeclration = GFunctionDeclration String [Argument] [Expression]
                 | GTypeDeclaration String Type
                 | ForwardFunctionDecl [Type] [Type]
                 deriving (Show, Eq)

data Argument = Argument String Type deriving (Show, Eq)

data Type = NamedType String
          | PrimitiveType Token
          | ArrayType Int Type
          | StreamType Type
          | RecordType [([String], Type)]
          | UnionType [([String], Type)]
          | FunctionType [Type] [Type]
          deriving (Show, Eq)

data Expression = Constant Token
                | Identifier String
                | Aref Expression Expression
                | FunCall Expression [Expression]
                | Not Expression
                | Neg Expression
                | Complex Expression Expression
                | IfThenElse Expression [Expression] [Expression]
                deriving (Show, Eq)

main = do
     inStr <- getContents
     let parseTree = hemp (alexScanTokens inStr)
     putStrLn ("parseTree: " ++ show(parseTree))
     print "done"
}
