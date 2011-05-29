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
        TopDeclarationList { $1 }

TopDeclarationList:
        FunctionOrTypeDef { 0 }
        | TopDeclarationList FunctionOrTypeDef { 0 }

FunctionOrTypeDef:
        FunctionDecl { GFunction $1 }
        | TypeDecl   { GTypeDef $1 }

FunctionDecl:
        function identifier "("  ")" begin end { GFunctionDeclration $2 [] [] }

TypeDecl:
        type identifier "=" integer ";" { GTypeDeclaration $2 $4 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data FunctionOrTypeDef = GFunction GFunctionDeclration
                       | GTypeDef GTypeDeclaration

data GFunctionDeclration = GFunctionDeclration String [Argument] [Expression]
data GTypeDeclaration = GTypeDeclaration String Token
data Argument = Argument String
data Expression = Constant Token

main = do
     inStr <- getContents
     let parseTree = hemp (alexScanTokens inStr)
     putStrLn ("parseTree: " ++ show(parseTree))
     print "done"
}
