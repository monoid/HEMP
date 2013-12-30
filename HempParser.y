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

--
-- List (reversed!)
--
list1(d): list1(d) d  { $2:$1 }
  |  { [] }

--
-- List with separator (reversed!)
--
list(d,s): list(d, s) s d { $3:$1 }
  | d { [$1] }
  |  { [] }

-- Non-empty list (reversed!)
listNE1(d): listNE1(d) d { $2:$1 }
  | d { [$1] }

-- Non-empty list with separator (reversed!)
listNE(d, s): listNE(d, s) s d { $3:$1 }
  | d { [$1] }

Program:
        TopDeclarationList { $1 }

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

TypeList: listNE(Type, ",") { reverse($1) }

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
        "[" listNE("..", ",") "]" { length($2) }
        | { 1::Int }

-- Stream type
StreamType:
        stream of Type { TStream $3 }

-- Record and union has common structure
RecordType:
        record "[" ListOfRecFields maybe(",") "]" { TRecord (reverse $3) }
UnionType:
        union "[" ListOfRecFields maybe(",") "]" { TUnion (reverse $3) }
-- Causes shift-reduce conflicts
-- ListOfRecFields: listNE(FieldGroup, ",") { reverse($1) }
ListOfRecFields:
        FieldGroup { $1 }
        | ListOfRecFields "," FieldGroup { $1 ++ $3 }
FieldGroup:
        IdentifierList ":" Type { zip $1 (repeat $3) }
IdentifierList: listNE(identifier, ",") { reverse($1) }

-- Function type
FunctionType:
        function "(" maybeL(TypeList) returns TypeList ")" { TFunction $3 $5 }

--
-- Top-level declarations: functions and type definitions
-- 
TopDeclarationList: listNE1(FunctionOrTypeDef) { reverse($1) }

FunctionOrTypeDef:
        FunctionDecl { $1 }
        | TypeDecl   { $1 }
        | ForwardFunctionDecl { $1 }

ExpressionList: listNE(Expression, ",") { reverse($1) }

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
        | let ListOfAssignments maybe(";") in ExpressionList end let { Let (reverse $2) $5 }
        | ForSimple { $1 }

----------------------------------------------------------------------
--
-- Compaund expressions
--

-- Causes shift-reduce conflict
-- ListOfAssignments: listNE(Assignment, ";") { reverse($1) }
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

ForSimple:
  for ForRangeCross ForBody ReturnList end for { ForLoop $2 $3 $4 }

ForRangeCross:
  ForRangeDot { $1 }
  | ForRangeDot cross ForRangeDot { ForRangeCross $1 $3 }
ForRangeDot:
  ForRange { $1 }
  | ForRange dot ForRange { ForRangeDot $1 $3 }

ForRange: identifier in Expression "," Expression { ForInRange $1 $3 $5 }
  | identifier in Expression { ForInArray $1 $3 }
  | identifier in Expression at identifier { ForInArrayIndexed $1 $3 $5 }
  | "(" ForRangeCross ")" { $2 }

ForBody: ListOfAssignments { $1 }

-- STUB
ReturnList: returns list1(LoopExpression) { $2 }

LoopExpression: value of identifier Expression { ValueOf $3 $4 }
  | array of Expression { ArrayOf $3 }

----------------------------------------------------------------------
--
-- Expressions
--
Constant:
        int { LIntVal $1 }
        | float { LFloatVal $1 }
        | string { LString $1 }
        | char { LChar $1 }
        | echar { LEChar $1 }

FunctionDecl:
        function identifier "(" ArgDecl returns TypeList  ")" ExpressionList end function { GFunctionDeclration $2 $4 $6 $8 }

ArgDecl: list(ArgsNType, ";") { reverse $ concat $1 }

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
                        putStrLn $ (n ++ ": " ++ (show body)))
          decls
     print "done"
}

-- main = do
--      inStr <- getContents
--      let parseTree = hemp (alexScanTokens inStr)
--      putStrLn ("parseTree: " ++ show(parseTree))
--      print "done"
-- }
