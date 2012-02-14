module HempTypes where
import HempDecl
import LLVM.Core
import Control.Monad

class Inferrable f where
      commonSupertype :: f -> f -> Maybe f

instance Inferrable TPrimitive where
         commonSupertype (TNum a) (TNum b) = liftM TNum (commonSupertype a b)
         commonSupertype _ _ = Nothing

instance Inferrable TNum where
         commonSupertype (RealTypes a) (RealTypes b) = liftM RealTypes (commonSupertype a b)
         commonSupertype (TComplex a) (TComplex b) = liftM TComplex (commonSupertype a b)
         commonSupertype (TComplex a) (RealTypes b) = liftM (TComplex . unTFrac) (commonSupertype (TFrac a) b)
                         where unTFrac (TFrac a) = a
         commonSupertype a'@(RealTypes b) b'@(TComplex a) = commonSupertype b' a'
instance Inferrable RealTypes where
         commonSupertype TInteger a = Just a
         commonSupertype a TInteger = Just a
         commonSupertype (TFrac a) (TFrac b) = liftM TFrac (commonSupertype a b)

instance Inferrable TFrac where
         commonSupertype a b = Just (max a b)


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

-- Common types for arrays and primitive types
congruentType :: Type -> Type -> Maybe Type
congruentType a b =
    case (a, b) of
    -- TODO: get rid of m and n at all...  Do nested
    -- array instead. Or substract 1 from both m and n
         (TArray m a', TArray n b') ->
                    -- 1 is just a stub
                    liftM (TArray 1) (congruentType a' b')
         (TPrimitive a', TPrimitive b') ->
                    liftM TPrimitive (commonSupertype a' b')
         (TPrimitive a', TArray m b') ->
                    -- 1 is just a stub
                    liftM (TArray 1) (congruentType (TPrimitive a') b')
         (TArray m b', TPrimitive a') ->
                    -- 1 is just a stub
                    liftM (TArray 1) (congruentType (TPrimitive a') b')
         _ -> Nothing


-- Do tupe deduction, converting Expression to TPair
deduceTypes :: Env -> Expression -> TPair
-- Constant
deduceTypes _ (Constant t) =
    case t of
         LIntVal (a, _) -> TPair (TConstant t) (TPrimitive (TNum (RealTypes TInteger)))
         LFloatVal s -> TPair (TConstant t) (TPrimitive (TNum (RealTypes (TFrac TReal))))
         LTrue -> TPair (TConstant t) (TPrimitive TBoolean)
         LFalse -> TPair (TConstant t) (TPrimitive TBoolean)

-- Not
deduceTypes v (Not e) = let a@(TPair e' (TPrimitive TBoolean)) = deduceTypes v e
                      in TPair (TNot a) (TPrimitive TBoolean) 

-- Negation
deduceTypes v (Neg e) = let a@(TPair e' (TPrimitive (TNum t))) = deduceTypes v e
                      in TPair (TNeg a) (TPrimitive (TNum t))
-- Comparsion
deduceTypes v (BinOp (LCmp op) e1 e2) =
      let a1@(TPair e1' (TPrimitive t1)) = deduceTypes v e1
          a2@(TPair e2' (TPrimitive t2)) = deduceTypes v e2
          Just tc = commonSupertype t1 t2 -- TODO: what if it is None
          tc' = TPrimitive tc
      in TPair (TCmp op (conv a1 tc') (conv a2 tc')) (TPrimitive TBoolean)

-- Equality is a special case
deduceTypes v (BinOp LEqual e1 e2) =
      let a1@(TPair e1' (TPrimitive t1)) = deduceTypes v e1
          a2@(TPair e2' (TPrimitive t2)) = deduceTypes v e2
          Just tc = commonSupertype t1 t2
          tc' = TPrimitive tc
      in TPair (TCmp CmpEQ (conv a1 tc') (conv a2 tc')) (TPrimitive TBoolean)

deduceTypes v (Identifier n) =
      let Just t = lookupVar n v
      in TPair (TVariable n) t

-- create conversion node if type of pair is different from required type;
-- if they match, just return the pair
conv :: TPair -> Type -> TPair
conv p@(TPair e t) t' | t == t' = p
-- t' is duplicated here.  Do we have to keep type in TConv?
conv p t' = TPair (TConv p t') t'


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
--
-- Environment

maybeOr :: (Maybe a) -> (Maybe a) -> (Maybe a)
maybeOr Nothing y = y
maybeOr x _ = x

-- import Maybe
-- maybeOr a b = if isJust a then a else b
-- maybeOr' a b = maybe b (Just) a

lookupVar :: String -> Env -> Maybe Type
lookupVar name (Env parent bindings) =
          maybeOr (lookup name bindings)
                  (parent >>= lookupVar name)
