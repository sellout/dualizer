{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Dual.TH where

import           Control.Arrow
import           Control.Comonad
import           Control.Error.Util
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Maybe
import           Data.Tuple
import           Data.Void
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax hiding (lift)

-- TODO: Extract as many as possible into `Dual.Prelude`.
initialTypeDuals :: Map Name Type
initialTypeDuals = Map.fromList [ (''Either,      TupleT 2)
                                -- TODO: Automate these self-duals somehow.
                                -- , (''Id,          ConT ''Id)
                                , (''Bool,        ConT ''Bool)
                                , (''Char,        ConT ''Char)
                                , (''Int,         ConT ''Int)
                                -- , (''Mu,          ConT ''Nu)
                                -- , (''Nu,          ConT ''Mu)
                                , (''Functor,     ConT ''Functor)
                                , (''Monad,       ConT ''Comonad)
                                , (''Comonad,     ConT ''Monad)
                                , (''Maybe,       ConT ''Maybe)
                                -- , (''Recursive,   ConT ''Corecursive)
                                -- , (''Corecursive, ConT ''Recursive)
                                -- , (''Embeddable,  ConT ''Projectable)
                                -- , (''Projectable, ConT ''Embeddable)
                                ]

shareDuals :: Map Name Type -> Q Exp
shareDuals duals =
  [e|pure [] <* (putQ . maybe $(liftData duals) (flip Map.union $(liftData duals)) =<< getQ)|]

-- | Call this at the top of your module to set up the already-known duals.
importPreludeDuals :: Q [Dec]
importPreludeDuals =
  -- TODO: Replace with `$(shareDuals initialTypeDuals)`
  pure [] <* (putQ . maybe initialTypeDuals (flip Map.union initialTypeDuals) =<< getQ)

typeFromName :: Name -> Q Type
typeFromName =
  (\case
      ClassI (ClassD _ n _ _ _) _ -> pure $ ConT n
      ClassOpI _ t _ -> pure t
      TyConI (DataD _ n _ _ _ _) -> pure $ ConT n
      FamilyI d _ -> fail "not yet getting type families" -- FIXME
      PrimTyConI n _ _ -> pure $ ConT n
      DataConI _ t _ -> pure t
      -- PatSynI _ _ -> fail "pattern synonym is not a type"
      VarI _ _ _ -> fail "value is not a type"
      TyVarI _ t -> pure t)
  <=< reify

-- initialValueDuals :: Map Name Exp
-- initialValueDuals = Map.fromList [ ('(.),   '(.))
--                                  , ('cata,  'ana)
--                                  , ('gcata, 'gana)]

-- | Returns a Type that is the dual of the named type.
dualName :: Map Name Type -> Name -> ExceptT Type Q Type
dualName db name =
  maybe (dualType' db <=< lift $ typeFromName name) pure $ Map.lookup name db

retrieveDuals :: Q (Map Name Type)
retrieveDuals = maybe (fail "no duals imported") pure =<< getQ

dualType' :: Map Name Type -> Type -> ExceptT Type Q Type
dualType' db = \case
  ForallT vs c t       -> ForallT vs c <$> dualType' db t
  -- FIXME: Generalize arrow handling.
  AppT (AppT ArrowT t) (AppT (AppT ArrowT t') t'') ->
    AppT <$> (AppT ArrowT <$> dualType' db t'') <*> (AppT <$> (AppT ArrowT <$> dualType' db t') <*> dualType' db t) 
  AppT (AppT ArrowT t) t' -> AppT <$> (AppT ArrowT <$> dualType' db t') <*> dualType' db t
  AppT t t'            -> AppT <$> dualType' db t <*> dualType' db t'
  SigT t k             -> flip SigT k <$> (dualType' db t)
  VarT n               -> pure $ VarT n
  ConT n               -> dualName db n
  PromotedT n          -> pure $ PromotedT n
  InfixT t n t'        -> dualName db n -- t t'
  UInfixT t n t'       -> dualName db n -- t t'
  (ParensT t)          -> pure $ ParensT t
  (TupleT 0)           -> pure $ ConT ''Void
  (TupleT 1)           -> pure $ TupleT 1
  (TupleT 2)           -> pure $ ConT ''Either
  f@(TupleT _)         -> throwE f
  f@(UnboxedTupleT i)  -> throwE f -- pure $ UnboxedSumT i
  -- UnboxedSumT a        -> pure $ UnboxedTupleT a
  ArrowT               -> pure ArrowT
  EqualityT            -> pure EqualityT
  ListT                -> pure ListT
  PromotedTupleT 0     -> pure $ ConT ''Void
  PromotedTupleT 1     -> pure $ PromotedTupleT 1
  PromotedTupleT 2     -> pure $ ConT ''Either
  f@(PromotedTupleT _) -> throwE f
  PromotedNilT         -> pure PromotedNilT
  PromotedConsT        -> pure PromotedConsT
  StarT                -> pure StarT
  ConstraintT          -> pure ConstraintT
  LitT l               -> pure $ LitT l
  WildCardT            -> pure WildCardT

-- | Returns a type that is the dual of the input type.
dualType :: Type -> Q Type
dualType type' = do
  duals <- retrieveDuals
  exceptT (\t -> fail $ "no dual for type " ++ show t) pure
    $ dualType' duals type'

-- | Indicates that some name represents the dual of itself (e.g., `Functor`).
labelSelfDual :: Name -> Q [Dec]
labelSelfDual name = do
  pure [] <* (putQ =<< Map.insert name <$> typeFromName name <*> retrieveDuals)
  
-- | This provides a mapping one way, but not the other. Useful for aliased
--   functions (`return`) and overconstrained versions (e.g., mapping
--  `traverse ↔ distribute` but also `forM → distribute`).
labelSemiDual :: Name -> Name -> Q [Dec]
labelSemiDual name coname = do
  pure [] <* (putQ =<< Map.insert name <$> typeFromName coname <*> retrieveDuals)

labelDualDataT :: Name -> Name -> Type -> Type -> Q [Dec]
labelDualDataT name coname type' cotype' =
  pure [] <* (putQ =<< Map.insert coname type' <$> (Map.insert name cotype' <$> retrieveDuals))

-- | This marks two types as dual to each other, without any generation.
labelDualData :: Name -> Name -> Q [Dec]
labelDualData name coname =
  join $ labelDualDataT name coname <$> typeFromName name <*> typeFromName coname
stripForall :: Type -> Type
stripForall (ForallT _ _ t) = t
stripForall t               = t

-- | Given a class, creates a new class that represents its dual, with the list
--   containing name mappings of methods to their duals.
makeDualClass :: Name -> String -> [(Name, String)] -> Q [Dec]
makeDualClass name co methods = do
  let coname = mkName co
  info <- reify name
  type' <- typeFromName name
  case info of
    ClassI (ClassD ctx _ tVars fds _) _ -> do
      ctx' <- traverse dualType ctx
      meths' <- traverse (sequenceA . (mkName *** ((dualType . stripForall) <=< typeFromName)) . swap) methods
      labelDualDataT name coname type' (ConT coname)
      pure [ClassD ctx' coname tVars fds (fmap (uncurry SigD) meths')]
    _ -> fail "not a type class"

-- | Creates a value that can be referenced in other modules to load the duals
--   defined in this module. Should be used at the bottom of any module that
--   uses this module.
exportDuals :: String -> Q [Dec]
exportDuals name = do
  exp <- shareDuals =<< retrieveDuals :: Q Exp
  pure [ValD (VarP $ mkName name) (NormalB exp) []]
