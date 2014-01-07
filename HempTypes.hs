{-# LANGUAGE FlexibleInstances #-}
module HempTypes where
import HempDecl
import LLVM.Core
import Control.Monad
import Data.Maybe

class Inferrable f where
      commonSupertype :: f -> f -> Maybe f

class ExpWithType f where
  typeOf :: f -> Type

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

instance Inferrable Type where
  commonSupertype a b = Just a -- STUB

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

instance ExpWithType TPair where
  typeOf (TPair _ t) = t

instance ExpWithType (LoopExpression Type TPair) where
  typeOf (ArrayOf a _) = a
  typeOf (ValueOf _ a _) = a

assertion True = Just True
assertion False = Nothing

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

class HempOp f where
  validArgTypes :: f -> Type -> Type -> Bool
  resultType :: f -> Type -> Type -> Maybe Type
  operation  :: f -> Expression -> Expression -> TPair -> TPair -> TExp

  validArgTypes f a b = isJust (resultType f a b)

instance HempOp CmpPredicate where
  resultType pred a b = Just (TPrimitive TBoolean)
  operation  pred a b = TCmp pred

instance HempOp BinaryCode where
  resultType pred (TPrimitive a) (TPrimitive b) = liftM TPrimitive (commonSupertype a b)
  operation pred a b = TArith pred

deduceBinaryTypes :: HempOp op => Env -> op -> Expression -> Expression -> Maybe TPair
deduceBinaryTypes v op e1 e2 =
  do a1@(TPair e1' t1'@(TPrimitive t1)) <- deduceTypes v e1
     a2@(TPair e2' t2'@(TPrimitive t2)) <- deduceTypes v e2
     tc <- commonSupertype t1 t2 -- TODO: what if it is None
     let tc' = TPrimitive tc
     rt <- resultType op t1' t2'
     return (TPair (operation op e1 e2
                   (conv a1 tc')
                   (conv a2 tc'))
            rt)
  
-- Do tupe deduction, converting Expression to TPair
deduceTypes :: Env -> Expression -> Maybe TPair
-- Constant
deduceTypes _ (Constant t) =
    Just (case t of
       LIntVal (a, _) -> TPair (TConstant t) (TPrimitive (TNum (RealTypes TInteger)))
       LFloatVal s -> TPair (TConstant t) (TPrimitive (TNum (RealTypes (TFrac TReal))))
       LTrue -> TPair (TConstant t) (TPrimitive TBoolean)
       LFalse -> TPair (TConstant t) (TPrimitive TBoolean))

-- Not
deduceTypes v (Not e) =
  do a@(TPair e' (TPrimitive TBoolean)) <- deduceTypes v e
     return $ TPair (TNot a) (TPrimitive TBoolean)

-- Negation
deduceTypes v (Neg e) =
  do a@(TPair e' (TPrimitive (TNum t))) <- deduceTypes v e
     return $ TPair (TNeg a) (TPrimitive (TNum t))


-- Comparsion
deduceTypes v (BoolOp bop e1 e2) = deduceBinaryTypes v bop e1 e2

deduceTypes v (BinOp bc e1 e2) = deduceBinaryTypes v bc e1 e2

deduceTypes v (Identifier n) =
      do t <- lookupVar n v
         return $ TPair (TVariable n) t

deduceTypes v (IfThenElse cond thenBranch elseBranch) =
  do
    tb' <- mapM (deduceTypes v) thenBranch
    eb' <- mapM (deduceTypes v) elseBranch
    assertion (length tb' == length eb')
    common' <- zipWithM commonSupertype (map typeOf tb')
                                        (map typeOf eb')
    cond'@(TPair _ ct) <- deduceTypes v cond
    -- Check that ct is boolean
    assertion (TPrimitive TBoolean == ct)
    return $ TPair (TIfThenElse cond' (zipWith conv tb' common')
                                      (zipWith conv eb' common'))
                   (TTuple common')

deduceTypes v (ForLoop ranges body ret) =
  -- допустимы ли float в range?  Нет, нет, ещё раз нет.
  -- допустимы ли old в body?  Нет, т.к. нет начального значения
  do
    (v', range') <- deduceForRanges v ranges
    (v'', body') <- deduceBody False v' body
    ret' <- deduceRet v'' ret
    return $ TPair (TForSimple range' body' ret')
                   (TTuple $ map (typeOf . fst) ret')

deduceForRanges :: Env -> ForRange () Expression -> Maybe (Env, ForRange Type TPair)

deduceForRanges v (ForRangeCross r1 r2) =
  do
    (v', r1') <- deduceForRanges v r1
    (v'', r2') <- deduceForRanges v' r2
    return (v'', ForRangeCross r1' r2')

deduceForRanges v (ForRangeDot r1 r2) =
  do
    (v', r1') <- deduceForRanges v r1
    (v'', r2') <- deduceForRanges v' r2
    return (v'', ForRangeDot r1' r2')

deduceForRanges v (ForInRange (n,_) r1 r2) =
  do
    r1'@(TPair _ t1') <- deduceTypes v r1
    r2'@(TPair _ t2') <- deduceTypes v r2
    ct <- commonSupertype t1' t2'
    return (Env (Just v) [(n, ct)],
            ForInRange (n, ct) (conv r1' ct) (conv r2' ct))

deduceForRanges v (ForInArray (n,_) r1) =
  do
    r1'@(TPair _ t1') <- deduceTypes v r1
    at <- arrayType t1'
    return (Env (Just v) [(n, at)],
            ForInArray (n, at) r1')

deduceForRanges v (ForInArrayIndexed (n,_) r1 (ni,_)) =
  do
    r1'@(TPair _ t1') <- deduceTypes v r1
    at <- arrayType t1'
    return (Env (Just v) [(n, at)],
            ForInArrayIndexed (n, at) r1' (ni, TPrimitive (TNum (RealTypes TInteger))))

deduceBody :: Bool -> Env -> [([String],[Expression])] -> Maybe (Env, [TPair])
deduceBody False v =
  foldM deduceAssignment (v,[])

deduceAssignment :: (Env, [TPair]) -> ([String], [Expression]) -> Maybe (Env, [TPair])
deduceAssignment (v,a') (vars, exprs) =
  do
    ep <- mapM (deduceTypes v) exprs
    let types = map typeOf ep
    vars' <- adjustVars vars types
    let env' = Env (Just v) vars'
    return (env', a' ++ ep)

deduceRet v ret = Nothing

adjustVars vars types =
  do
    let typeList = expandTuples types
    assertion (length vars == length typeList)
    return $ zip vars types

expandTuples a = concat $ map expandTuple a

expandTuple :: Type -> [Type]
expandTuple (TTuple types) = types
expandTuple t = [t]

arrayType :: Type -> Maybe Type
arrayType (TArray 1 t) = Just t
arrayType (TArray n t) = Just $ TArray (n-1) t
arrayType _ = Nothing

-- create conversion node if type of pair is different from required type;
-- if they match, just return the pair
conv :: TPair -> Type -> TPair
conv p@(TPair e t) t' | t == t' = p
-- t' is duplicated here.  Do we have to keep type in TConv?
conv p t' = TPair (TConv p t') t'


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
--
-- Environment

maybeOr :: Maybe a -> Maybe a -> Maybe a
maybeOr Nothing y = y
maybeOr x _ = x

-- import Maybe
-- maybeOr a b = if isJust a then a else b
-- maybeOr' a b = maybe b (Just) a

lookupVar :: String -> Env -> Maybe Type
lookupVar name (Env parent bindings) =
          maybeOr (lookup name bindings)
                  (parent >>= lookupVar name)
