{ -- -*- mode: haskell -*-
module Main where
import HempDecl
import HempLexer
import HempTypes
import LLVM.Core
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
        int     { LIntVal $$ }
        float   { LFloatVal $$ }
        string  { LString $$ }
        char    { LChar $$ }
        echar   { LEChar $$ }
        identifier { LIdent $$ }
        ":="       { LAssign }
        ".."       { LPtPt }
        "."        { LPoint }
        ","        { LComma }
        ":"        { LColon }
        ";"        { LSemicolon }
        "["        { LLeftSqBr }
        "]"        { LRighSqBr }
        "("        { LLeftRoBr }
        ")"        { LRighRoBr }
        cmp        { LCmp $$ }
        "="        { LEqual }
        "+"        { LBin BoPlus }
        "-"        { LBin BoMinus }
        "**"       { LBin BoExpt }
        "||"       { LBin BoAppend }
        "*"        { LBin BoMult }
        "/"        { LBin BoDiv }
        "&"        { LBin BoAnd }
        "|"        { LBin BoOr }
        "~"        { LNot }
        array { LArray }
        at { LAt }
        begin { LBegin }
        boolean { LBoolean }
        case { LCase }
        character { LCharacter }
        complex { LComplex }
        cross { LCross }
        default { LDefault }
        do { LDo }
        dot { LDot }
        double { LDouble }
        doublecomplex { LDoubleComplex }
        else { LElse }
        elseif { LElseIf }
        end { LEnd }
        err { LError }
        false { LFalse }
        for { LFor }
        foreign { LForeign }
        forward { LForward }
        from { LFrom }
        function { LFunction }
        if { LIf }
        imag { LImag }
        inout { LInOut }
        integer { LInteger }
        in { LIn }
        interface { LInterface }
        is { LIs }
        let { LLet }
        module { LModule }
        nil { LNil }
        null { LNull }
        of { LOf }
        otherwise { LOtherwise }
        out { LOut }
        program { LProgram }
        real { LReal }
        record { LRecord }
        returns { LReturns }
        state { LState }
        stream { LStream }
        suffix { LSuffix }
        then { LThen }
        to { LTo }
        true { LTrue }
        type { LType }
        when { LWhen }
        while { LWhile }
        union { LUnion }
        unless { LUnless }
        until { LUntil }
        initial { LInitial }
        old { LOld }
        value { LValue }
        define { LDefine }
        underscore { LUnderscore }
%%

--
-- Optional trailing elements without value, like comma, semicolon etc.
--
maybe(c): c { 0 }
        |   { 0 }

--
-- Optional lists.  d is the list itself, not element
--
maybeL(d): d { $1 }
         |   { [] }

Program:
        TopDeclarationList { reverse $1 }

--
-- Types
--
Type:
        identifier { NamedType $1 }
        | PrimitiveType { TPrimitive $1 }
        | ArrayType     { $1 }
        | StreamType    { $1 }
        | RecordType    { $1 }
        | UnionType     { $1 }
        | FunctionType  { $1 }

TypeList:
        Type { [$1] }
        | TypeList "," Type { $1 ++ [$3] }

PrimitiveType:
        boolean { TBoolean }
        | integer { TNum (RealTypes TInteger) }
        | real    { TNum (RealTypes (TFrac TReal)) }
        | double  { TNum (RealTypes (TFrac TDouble)) }
        | complex { TNum (TComplex TReal) }
        | doublecomplex { TNum (TComplex TDouble) }
        | null    { TNull }
        | character { TChar }

-- Array type
ArrayType:
        array DimSpec of Type { TArray $2 $4 }
DimSpec:
        "[" DimSpec1 "]" { $2 }
        | { 1::Int }
DimSpec1:
        ".." { 1::Int }
        | DimSpec1 "," ".." { $1 + 1 }

-- Stream type
StreamType:
        stream of Type { TStream $3 }

-- Record and union has common structure
RecordType:
        record "[" ListOfRecFields maybe(",") "]" { TRecord $3 }
UnionType:
        union "[" ListOfRecFields maybe(",") "]" { TUnion (reverse $3) }
ListOfRecFields:
        FieldGroup { $1 }
        | ListOfRecFields "," FieldGroup { $1 ++ $3 }
FieldGroup:
        IdentifierList ":" Type { zip $1 (repeat $3) }
IdentifierList:
        identifier { [$1] }
        | IdentifierList "," identifier { $1++[$3] }

-- Function type
FunctionType:
        function "(" maybeL(TypeList) returns TypeList ")" { TFunction $3 $5 }

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
        | Expression "|" Expression { binop $2 $1 $3 }
        | Expression "&" Expression { binop $2 $1 $3 }
        | Expression "||" Expression { binop $2 $1 $3 }
        | Expression "+" Expression { binop $2 $1 $3 }
        | Expression "-" Expression { binop $2 $1 $3 }
        | Expression "*" Expression { binop $2 $1 $3 }
        | Expression "/" Expression { binop $2 $1 $3 }
        | Expression cmp Expression { BoolOp $2 $1 $3 }
        | Expression "=" Expression { BoolOp CmpEQ $1 $3 }
        | Expression "**" Expression { binop $2 $1 $3 }
        | "(" Expression "," Expression ")" { Complex $2 $4 }
        | CompaundExpression { $1 }

-- Just to keep Expression rule managable, we introduce CompaundExpression
CompaundExpression:
        if IfConditions else ExpressionList end if { expandIfThen $2 $4 }
        | let ListOfAssignments maybe(";") in ExpressionList end let { Let $2 $5 }

ListOfAssignments:
        Assignment { [$1] }
        | ListOfAssignments ";" Assignment { $1 ++ [$3] }
Assignment:
        IdentifierList ":=" ExpressionList { ($1, $3) }

IfConditions:
        IfCondition { [$1] }
        | IfConditions elseif IfCondition { $1 ++ [$3] }
IfCondition:
        Expression then ExpressionList { ($1, $3) }

Constant:
        int { LIntVal $1 }
        | float { LFloatVal $1 }
        | string { LString $1 }
        | char { LChar $1 }
        | echar { LEChar $1 }

FunctionDecl:
        function identifier "(" ArgDecl returns TypeList  ")" ExpressionList end function { GFunctionDeclration $2 $4 $6 $8 }

ArgDecl:  { [] }
       | ArgsNType { $1 }
       | ArgDecl ";" ArgsNType { $1 ++ $3 }

ArgsNType:
        IdentifierList ":" Type { zipWith (,) $1 (repeat $3) }

ForwardFunctionDecl:
        forward function identifier "(" maybeL(TypeList) returns TypeList ")" ";"
        { ForwardFunctionDecl $5 $7 }

TypeDecl:
        type identifier "=" Type ";" { GTypeDeclaration $2 $4 }

{
expandIfThen :: [(Expression, [Expression])] -> [Expression] -> Expression
expandIfThen [(if', then')] else' = IfThenElse if' then' else'
expandIfThen ((if', then'):more) lastElse =
                   IfThenElse if' then' [(expandIfThen more lastElse)]

parseError :: [Token] -> a
parseError (x:xs) = error ("Parse error at " ++ (show x))
parseError _ = error "Parse error"


main = do
     inStr <- getContents
     let decls = hemp (alexScanTokens inStr)
     mapM (\(GFunctionDeclration n a r body) ->
                        putStrLn $ (n ++ ": " ++ (show $ map (deduceTypes $ Env Nothing a) body)))
          decls
     print "done"
}

-- main = do
--      inStr <- getContents
--      let parseTree = hemp (alexScanTokens inStr)
--      putStrLn ("parseTree: " ++ show(parseTree))
--      print "done"
-- }
