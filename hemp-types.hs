module HempTypes where

import HempDecl

-- Base type of primitive types, arrays and stream.
-- For example, base type of array (or stream) of integer is integer.
baseArithType :: Type -> Maybe PrimitiveType
baseArithType (PrimitiveType a) = Just a
baseArithType (ArrayType n a) = baseArithType a
baseArithType (StreamType a) = baseArithType a
baseArithType _ = Nothing

numeric a = case a of
                 NumericType _ -> True
                 otherwise -> False

-- Common primitive type
commonType :: PrimitiveType -> PrimitiveType -> Maybe PrimitiveType
commonType a b
           | a > b = commonType b a
           | a == b = Just a
commonType (NumericType a) (NumericType b) = Just (NumericType (commonNumType a b))
commonType _ _ = Nothing


-- Common numeric type
commonNumType :: NumericType -> NumericType -> NumericType
commonNumType a b
              | a > b = commonNumType b a
commonNumType (RealTypes (FractionalType a)) (ComplexType b) = ComplexType (max a b)
commonNumType a b = max a b


-- Common types for arrays and primitive types
congruentType :: Type -> Type -> Maybe Type
congruentType a b =
    case (a, b) of
    -- TODO: get rid of m and n at all...  Do nested
    -- array instead. Or substract 1 from both m and n
         (ArrayType m a', ArrayType n b') ->
                    do
                        c <- congruentType a' b'
                        return (ArrayType 1 c) -- 1 is just a stub
         (PrimitiveType a', PrimitiveType b') ->
                    do
                        c <- commonType a' b'
                        return (PrimitiveType c)
         (PrimitiveType a', ArrayType m b') ->
                    do
                        c <- congruentType (PrimitiveType a') b'
                        return (ArrayType 1 c) -- 1 is just a stub
         (ArrayType m b', PrimitiveType a') ->
                    do
                        c <- congruentType (PrimitiveType a') b'
                        return (ArrayType 1 c) -- 1 is just a stub
                               
         _ -> Nothing

