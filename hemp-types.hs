module HempTypes where

import HempDecl

baseArithType :: Type -> Maybe PrimitiveType
baseArithType (PrimitiveType a) = Just a
baseArithType (ArrayType n a) = baseArithType a
baseArithType (StreamType a) = baseArithType a
baseArithType _ = Nothing

numeric a = case a of
                 IntegerType -> True                 
                 RealType -> True
                 DoubleType -> True
                 ComplexType -> True
                 DoubleComplexType -> True
                 otherwise -> False

commonType :: PrimitiveType -> PrimitiveType -> Maybe PrimitiveType
commonType a b = if a > b then
                    commonType b a
                 else if a == b then
                      Just a
                 else case (a, b) of
                           (IntegerType, RealType) -> Just RealType
                           (IntegerType, DoubleType) -> Just DoubleType
                           (IntegerType, ComplexType) -> Just ComplexType
                           (IntegerType, DoubleComplexType) -> Just DoubleComplexType
                           (RealType, DoubleType) -> Just DoubleType
                           (RealType, ComplexType) -> Just ComplexType
                           (RealType, DoubleComplexType) -> Just DoubleComplexType
                           (DoubleType, ComplexType) -> Just DoubleComplexType
                           (DoubleType, DoubleComplexType) -> Just DoubleComplexType
                           _ -> Nothing

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

