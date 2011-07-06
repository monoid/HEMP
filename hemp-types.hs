module HempTypes where

import HempDecl
import LLVM.Core

-- Base type of primitive types, arrays and stream.
-- For example, base type of array (or stream) of integer is integer.
baseArithType :: Type -> Maybe TPrimitive
baseArithType (TPrimitive a) = Just a
baseArithType (TArray n a) = baseArithType a
baseArithType (TStream a) = baseArithType a
baseArithType _ = Nothing

numeric a = case a of
                 TNum _ -> True
                 otherwise -> False

-- Common primitive type
commonType :: TPrimitive -> TPrimitive -> Maybe TPrimitive
commonType a b
           | a > b = commonType b a
           | a == b = Just a
commonType (TNum a) (TNum b) = Just (TNum (commonNumType a b))
commonType _ _ = Nothing


-- Common numeric type
commonNumType :: TNum -> TNum -> TNum
commonNumType a b
              | a > b = commonNumType b a
commonNumType (RealTypes (TFrac a)) (TComplex b) = TComplex (max a b)
commonNumType a b = max a b


-- Common types for arrays and primitive types
congruentType :: Type -> Type -> Maybe Type
congruentType a b =
    case (a, b) of
    -- TODO: get rid of m and n at all...  Do nested
    -- array instead. Or substract 1 from both m and n
         (TArray m a', TArray n b') ->
                    do
                        c <- congruentType a' b'
                        return (TArray 1 c) -- 1 is just a stub
         (TPrimitive a', TPrimitive b') ->
                    do
                        c <- commonType a' b'
                        return (TPrimitive c)
         (TPrimitive a', TArray m b') ->
                    do
                        c <- congruentType (TPrimitive a') b'
                        return (TArray 1 c) -- 1 is just a stub
         (TArray m b', TPrimitive a') ->
                    do
                        c <- congruentType (TPrimitive a') b'
                        return (TArray 1 c) -- 1 is just a stub
                               
         _ -> Nothing


-- Do tupe deduction, converting Expression to TPair
deduceTypes :: Expression -> TPair
-- Constant
deduceTypes (Constant t) =
    case t of
         LIntVal (a, _) -> TPair (TConstant t) (TPrimitive (TNum (RealTypes TInteger)))
         LFloatVal s -> TPair (TConstant t) (TPrimitive (TNum (RealTypes (TFrac TReal))))
         LTrue -> TPair (TConstant t) (TPrimitive TBoolean)
         LFalse -> TPair (TConstant t) (TPrimitive TBoolean)

-- Not
deduceTypes (Not e) = let a@(TPair e' (TPrimitive TBoolean)) = deduceTypes e
                      in TPair (TNot a) (TPrimitive TBoolean) 

-- Negation
deduceTypes (Neg e) = let a@(TPair e' (TPrimitive (TNum t))) = deduceTypes e
                      in TPair (TNeg a) (TPrimitive (TNum t))
-- Comparsion
deduceTypes (BinOp (LCmp op) e1 e2) =
      let a1@(TPair e1' (TPrimitive t1)) = deduceTypes e1
          a2@(TPair e2' (TPrimitive t2)) = deduceTypes e2
          Just tc = commonType t1 t2
          tc' = TPrimitive tc
      in TPair (TCmp op (conv a1 tc') (conv a2 tc')) (TPrimitive TBoolean)

-- create conversion node if type of pair is different from required type;
-- if they match, just return the pair
conv :: TPair -> Type -> TPair
conv p@(TPair e t) t' | t == t' = p
-- t' is duplicated here.  Do we have to keep type in TConv?
conv p t' = TPair (TConv p t') t'
