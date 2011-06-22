module HempTypes where

import HempDecl

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

