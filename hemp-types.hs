module HempTypes where

import HempDecl

baseArithType :: Type -> Maybe PrimitiveType
baseArithType (PrimitiveType a) = Just a
baseArithType (ArrayType n a) = baseArithType a
baseArithType (StreamType a) = baseArithType a
baseArithType _ = Nothing

numeric a = case a of
                 NumericType _ -> True
                 otherwise -> False

commonType :: PrimitiveType -> PrimitiveType -> Maybe PrimitiveType
commonType a b = if a > b then
                    commonType b a
                 else if a == b then
                      Just a
                 else case (a, b) of
                           (NumericType a', NumericType b') -> Just (NumericType (commonNumType a' b'))
                           _ -> Nothing

commonNumType :: NumericType -> NumericType -> NumericType
commonNumType a b = if a > b then
                       commonNumType b a
                    else case (a, b) of
                         (RealTypes (FractionalType a'), ComplexType b') -> ComplexType (max a' b')
                         (a', b') -> max a' b'

congruentType :: Type -> Type -> Maybe Type
congruentType a b = case (a, b) of
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

