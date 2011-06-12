{ -- -*- mode: haskell -*-
module HempLexer where
import Char (ord, isDigit, isLower, isUpper)
import Numeric (readInt)
import HempDecl
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
       $digit+ "#" $alphaNum+   { \s -> LIntVal (readHashedInt s) }
       $digit+(@mantissa|@exponent|@mantissa@exponent)
                                { \s -> LFloatVal s }
       $digit+                  { \s -> LIntVal ((read s), True) }
       \" ([^\"]|\\.)+ \"       { \s -> LString ((init . tail) s) }
       '[^']'                   { \s -> LChar (s!!1) }  -- ordinary chars
       '\\.'                    { \s -> LEChar (s!!2) } -- '\n' etc
       -- keywords
       ":="                     { \s -> LAssign }
       "="                      { \s -> LEqual }
       ".."                     { \s -> LPtPt }
       "."                      { \s -> LPoint }
       ","                      { \s -> LComma }
       ":"                      { \s -> LColon }
       ";"                      { \s -> LSemicolon }
       "["                      { \s -> LLeftSqBr }
       "]"                      { \s -> LRighSqBr }
       "("                      { \s -> LLeftRoBr }
       ")"                      { \s -> LRighRoBr }
       "<" | ">" | "~=" | "<=" | ">="
                                { \s -> LCmp s }
       "+"                      { \s -> LPlus }
       "-"                      { \s -> LMinus }
       "**"                     { \s -> LExpt }
       "||"                     { \s -> LAppend }
       "*"                      { \s -> LMult }
       "/"                      { \s -> LDiv }
       "&"                      { \s -> LAnd }
       "~"                      { \s -> LNot }
       "array"                  { \s -> LArray }
       "at"                     { \s -> LAt }
       "begin"                  { \s -> LBegin }
       "boolean"                { \s -> LBoolean }
       "case"                   { \s -> LCase }
       "character"              { \s -> LCharacter }
       "complex"                { \s -> LComplex }
       "cross"                  { \s -> LCross }
       "default"                { \s -> LDefault }
       "do"                     { \s -> LDo }
       "dot"                    { \s -> LDot }
       "double"                 { \s -> LDouble }
       "double_complex"         { \s -> LDoubleComplex }
       "else"                   { \s -> LElse }
       "elseif"                 { \s -> LElseIf }
       "end"                    { \s -> LEnd }
       "false"                  { \s -> LFalse }
       "for"                    { \s -> LFor }
       "foreign"                { \s -> LForeign }
       "forward"                { \s -> LForward }
       "from"                   { \s -> LFrom }
       "function"               { \s -> LFunction }
       "if"                     { \s -> LIf }
       "imag"                   { \s -> LImag }
       "inout"                  { \s -> LInOut }
       "in"                     { \s -> LIn }
       "integer"                { \s -> LInteger }
       "interface"              { \s -> LInterface }
       "is"                     { \s -> LIs }
       "let"                    { \s -> LLet }
       "module"                 { \s -> LModule }
       "nil"                    { \s -> LNil }
       "null"                   { \s -> LNull }
       "of"                     { \s -> LOf }
       "otherwise"              { \s -> LOtherwise }
       "out"                    { \s -> LOut }
       "program"                { \s -> LProgram }
       "real"                   { \s -> LReal }
       "record"                 { \s -> LRecord }
       "returns"                { \s -> LReturns }
       "state"                  { \s -> LState }
       "stream"                 { \s -> LStream }
       "suffix"                 { \s -> LSuffix }
       "then"                   { \s -> LThen }
       "to"                     { \s -> LTo }
       "true"                   { \s -> LTrue }
       "type"                   { \s -> LType }
       "when"                   { \s -> LWhen }
       "while"                  { \s -> LWhile }
       "union"                  { \s -> LUnion }
       "unless"                 { \s -> LUnless }
       "until"                  { \s -> LUntil }
       "initial"                { \s -> LInitial }
       "old"                    { \s -> LOld }
       "value"                  { \s -> LValue }
       "define"                 { \s -> LDefine }
       "_"                      { \s -> LUnderscore }
       -- variable
       $alpha $alphaNum*        { \s -> LIdent s }

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

}
