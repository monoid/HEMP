{ -- -*- mode: haskell -*-
module HempLexer where
import Char (ord, isDigit, isLower, isUpper)
import Numeric (readInt)
}

%wrapper "basic"

$digit = 0-9            -- digits
$alpha = [a-zA-Z_]      -- alphabetic
$alphaNum = [a-zA-Z0-9]

@mantissa = (\.$digit*)
@exponent = ([edED][\+\-]?$digit+)

tokens :-
       $white+                  ;       -- whitespace
       "%".*                    ;       -- comment

       -- constants
       $digit+ "#" $alphaNum+   { \s -> TIntVal (readHashedInt s) }
       $digit+(@mantissa|@exponent|@mantissa@exponent)
                                { \s -> TFloatVal s }
       $digit+                  { \s -> TIntVal ((read s), True) }
       \" ([^\"]|\\.)+ \"       { \s -> TString ((init . tail) s) }
       '[^']'                   { \s -> TChar (s!!1) }  -- ordinary chars
       '\\.'                    { \s -> TEChar (s!!2) } -- '\n' etc
       -- keywords
       ":="                     { \s -> TAssign }
       "="                      { \s -> TEqual }
       ".."                     { \s -> TPtPt }
       "."                      { \s -> TPoint }
       ","                      { \s -> TComma }
       ":"                      { \s -> TColon }
       ";"                      { \s -> TSemicolon }
       "["                      { \s -> TLeftSqBr }
       "]"                      { \s -> TRighSqBr }
       "("                      { \s -> TLeftRoBr }
       ")"                      { \s -> TRighRoBr }
       "<" | ">" | "~=" | "<=" | ">="
                                { \s -> TCmp s }
       "+"                      { \s -> TPlus }
       "-"                      { \s -> TMinus }
       "**"                     { \s -> TExpt }
       "||"                     { \s -> TAppend }
       "*"                      { \s -> TMult }
       "/"                      { \s -> TDiv }
       "&"                      { \s -> TAnd }
       "~"                      { \s -> TNot }
       "array"                  { \s -> TArray }
       "at"                     { \s -> TAt }
       "begin"                  { \s -> TBegin }
       "boolean"                { \s -> TBoolean }
       "case"                   { \s -> TCase }
       "character"              { \s -> TCharacter }
       "complex"                { \s -> TComplex }
       "cross"                  { \s -> TCross }
       "default"                { \s -> TDefault }
       "do"                     { \s -> TDo }
       "dot"                    { \s -> TDot }
       "double"                 { \s -> TDouble }
       "double_complex"         { \s -> TDoubleComplex }
       "else"                   { \s -> TElse }
       "elseif"                 { \s -> TElseIf }
       "end"                    { \s -> TEnd }
       "false"                  { \s -> TFalse }
       "for"                    { \s -> TFor }
       "foreign"                { \s -> TForeign }
       "forward"                { \s -> TForward }
       "from"                   { \s -> TFrom }
       "function"               { \s -> TFunction }
       "if"                     { \s -> TIf }
       "imag"                   { \s -> TImag }
       "inout"                  { \s -> TInOut }
       "in"                     { \s -> TIn }
       "integer"                { \s -> TInteger }
       "interface"              { \s -> TInterface }
       "is"                     { \s -> TIs }
       "let"                    { \s -> TLet }
       "module"                 { \s -> TModule }
       "nil"                    { \s -> TNil }
       "null"                   { \s -> TNull }
       "of"                     { \s -> TOf }
       "otherwise"              { \s -> TOtherwise }
       "out"                    { \s -> TOut }
       "program"                { \s -> TProgram }
       "real"                   { \s -> TReal }
       "record"                 { \s -> TRecord }
       "returns"                { \s -> TReturns }
       "state"                  { \s -> TState }
       "stream"                 { \s -> TStream }
       "suffix"                 { \s -> TSuffix }
       "then"                   { \s -> TThen }
       "to"                     { \s -> TTo }
       "true"                   { \s -> TTrue }
       "type"                   { \s -> TType }
       "when"                   { \s -> TWhen }
       "while"                  { \s -> TWhile }
       "union"                  { \s -> TUnion }
       "unless"                 { \s -> TUnless }
       "until"                  { \s -> TUntil }
       "initial"                { \s -> TInitial }
       "old"                    { \s -> TOld }
       "value"                  { \s -> TValue }
       "define"                 { \s -> TDefine }
       "_"                      { \s -> TUnderscore }
       -- variable
       $alpha $alphaNum*        { \s -> TIdent s }

{
--
-- Some functions to parse hashed integer: 16#CAFE, 2#100110
--

-- helper function for Numeric.readInt
isBaseDigit :: Int -> Char -> Bool
isBaseDigit base char = 0 <= v && v < base
        where v = digitToInt base char

-- helper function for Numeric.readInt
digitToInt :: Int -> Char -> Int
digitToInt base char | isDigit char = ord(char) - ord('0')
                     -- isLower and isUpper return True on some other
                     -- char (cyrillic letters, e.g.), but lexer gives us
                     -- only latin chars anyway.
                     | isLower char = ord(char) - ord('a') + 10
                     | isUpper char = ord(char) - ord('A') + 10
                     | otherwise = -1

-- Function that parses hashed integer
readHashedInt :: String -> (Integer, Bool)
readHashedInt s = (toInteger n, t == [])
              where (base, hnum) = break (=='#') s
                    base' = (read base)
                    [(n, t)] = readInt base' (isBaseDigit base') (digitToInt base') (tail hnum)

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
}
