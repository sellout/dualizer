{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Unsafe #-}
-- FIXME: remove these
{-# OPTIONS_GHC -Wwarn=incomplete-patterns
    -Wwarn=name-shadowing
    -Wwarn=unused-matches #-}

-- | Operations to connect dual constructions.
module Categorical.Dual
  ( importDuals,
    exportDuals,
    emptyDuals, -- shouldn’t export this
    shareDuals,
    dualType,
    dualExp,
    makeDualClass,
    makeDualDec,
    makeDualExp,
    labelDual,
    labelSelfDual,
    labelSemiDual,
  )
where

import safe Control.Applicative (pure, (<*), (<*>))
import safe Control.Arrow ((***))
import safe Control.Category (id, (.))
import safe Control.Lens (makeLenses, (%~), (&))
import safe Control.Monad (Monad, fail, join, (<=<), (=<<))
import safe Control.Monad.Trans.Class (lift)
import safe Control.Monad.Trans.Except
  ( ExceptT (ExceptT),
    runExceptT,
    throwE,
    withExceptT,
  )
import safe Data.Bitraversable (bisequence)
import safe Data.Data (Data)
import safe Data.Either (Either (Left, Right), either)
import safe Data.Eq (Eq)
import safe Data.Function (const, flip, ($))
import safe Data.Functor (fmap, (<$>))
import safe Data.List ((++))
import safe Data.Map (Map)
import safe qualified Data.Map as Map
import safe Data.Maybe (maybe)
import safe Data.Monoid (Monoid, mempty)
import safe Data.Semigroup (Semigroup, (<>))
import safe Data.String (String)
import safe Data.Traversable (sequenceA, traverse)
import safe Data.Tuple (swap, uncurry)
import safe Data.Void (Void)
import safe Language.Haskell.TH.Syntax
  ( Body (GuardedB, NormalB),
    Clause (Clause),
    Con (ForallC, GadtC, InfixC, NormalC, RecC, RecGadtC),
    Dec
      ( ClassD,
        ClosedTypeFamilyD,
        DataD,
        DataFamilyD,
        FunD,
        NewtypeD,
        OpenTypeFamilyD,
        SigD,
        TySynD,
        ValD
      ),
    Exp
      ( AppE,
        ArithSeqE,
        CaseE,
        CompE,
        ConE,
        CondE,
        DoE,
        InfixE,
        LamCaseE,
        LamE,
        LetE,
        ListE,
        LitE,
        MultiIfE,
        ParensE,
        RecConE,
        RecUpdE,
        SigE,
        StaticE,
        TupE,
        UInfixE,
        UnboundVarE,
        UnboxedTupE,
        VarE
      ),
    Guard (NormalG, PatG),
    Info
      ( ClassI,
        ClassOpI,
        DataConI,
        FamilyI,
        PrimTyConI,
        TyConI,
        TyVarI,
        VarI
      ),
    Match (Match),
    Name,
    Pat (LitP, TupP, VarP),
    Q,
    Stmt (BindS, LetS, NoBindS, ParS),
    TySynEqn (TySynEqn),
    Type
      ( AppT,
        ArrowT,
        ConT,
        ConstraintT,
        EqualityT,
        ForallT,
        InfixT,
        ListT,
        LitT,
        ParensT,
        PromotedConsT,
        PromotedNilT,
        PromotedT,
        PromotedTupleT,
        SigT,
        StarT,
        TupleT,
        UInfixT,
        UnboxedTupleT,
        VarT,
        WildCardT
      ),
    TypeFamilyHead (TypeFamilyHead),
    getQ,
    liftData,
    mkName,
    putQ,
    recover,
    reify,
  )
import safe Text.Show (Show, show)
import safe Prelude (undefined)

data DualMappings = DualMappings
  { _dualTypes :: Map Name Type,
    _dualValues :: Map Name Exp
  }
  deriving (Data, Eq)

makeLenses ''DualMappings

instance Semigroup DualMappings where
  DualMappings t v <> DualMappings t' v' =
    -- NB: I reversed the order here, because I _think_ this is supposed to be
    --     right-biased?
    DualMappings (t' `Map.union` t) (v' `Map.union` v)

instance Monoid DualMappings where
  mempty = DualMappings Map.empty Map.empty

-- | The empty set of duals, should only be used to initalize the duals for
--   `Prelude`.
emptyDuals :: Q DualMappings
emptyDuals = pure $ DualMappings Map.empty Map.empty

reifyDuals :: DualMappings -> Q Exp
reifyDuals duals =
  [e|maybe $(liftData duals) ($(liftData duals) <>) <$> getQ|]

shareDuals :: DualMappings -> Q Exp
shareDuals duals =
  [e|pure [] <* (putQ . maybe $(liftData duals) ($(liftData duals) <>) =<< getQ)|]

-- TODO: Move this somewhere better
data AndMaybe a b = Only a | Indeed a b deriving (Eq, Show)

andMaybe :: (a -> c) -> (a -> b -> c) -> a `AndMaybe` b -> c
andMaybe f g = \case
  Only a -> f a
  Indeed a b -> g a b

fromName :: Name -> Q (Type `AndMaybe` Exp)
fromName =
  ( \case
      ClassI (ClassD _ n _ _ _) _ -> pure . Only $ ConT n
      ClassI d _ -> fail $ "unknown dec to extract name from: " ++ show d
      ClassOpI n t _ -> pure . Indeed t $ VarE n
      TyConI (DataD _ n _ _ _ _) -> pure . Only $ ConT n
      TyConI (TySynD n _ _) -> pure . Only $ ConT n
      TyConI (NewtypeD _ n _ _ _ _) -> pure . Only $ ConT n
      TyConI d -> fail $ "unknown dec to extract name from: " ++ show d
      FamilyI d _ -> fail "not yet getting type families" -- FIXME
      PrimTyConI n _ _ -> pure . Only $ ConT n
      DataConI n t _ -> pure . Indeed t $ ConE n
      -- PatSynI _ _ -> fail "pattern synonym is not a type"
      VarI n t _ -> pure . Indeed t $ VarE n
      TyVarI _ t -> pure $ Only t
  )
    <=< reify

typeFromName :: Name -> Q Type
typeFromName = fmap (andMaybe id const) . fromName

expFromName :: Name -> Q Exp
expFromName =
  andMaybe (\t -> fail $ show t ++ " is not a value") (\_ e -> pure e)
    <=< fromName

-- | Returns a Type that is the dual of the named type.
dualTypeName :: Map Name Type -> Name -> ExceptT Type Q Type
dualTypeName db name =
  maybe (dualType' db <=< lift $ typeFromName name) pure $ Map.lookup name db

dualExpName :: DualMappings -> Name -> ExceptT (Either Type Exp) Q Exp
dualExpName db name =
  maybe (dualExp' db <=< lift $ expFromName name) pure $
    Map.lookup name (_dualValues db)

retrieveDuals :: Q DualMappings
retrieveDuals = maybe (fail "no duals imported") pure =<< getQ

-- FIXME: This can get into an infinite loop in the case of missing duals.
dualType' :: Map Name Type -> Type -> ExceptT Type Q Type
dualType' db = \case
  ForallT vs c t ->
    ForallT vs <$> traverse (dualType' db) c <*> dualType' db t
  AppT (AppT ArrowT t) inner@(AppT (AppT ArrowT _) _) -> do
    t' <- dualType' db t
    AppT (AppT ArrowT t') <$> dualType' db inner
  AppT (AppT ArrowT t) t' -> AppT <$> (AppT ArrowT <$> dualType' db t') <*> dualType' db t
  AppT t t' -> AppT <$> dualType' db t <*> dualType' db t'
  SigT t k -> flip SigT k <$> (dualType' db t)
  VarT n -> pure $ VarT n
  ConT n -> dualTypeName db n
  PromotedT n -> pure $ PromotedT n
  InfixT t n t' -> dualTypeName db n -- t t'
  UInfixT t n t' -> dualTypeName db n -- t t'
  (ParensT t) -> pure $ ParensT t
  (TupleT 0) -> pure $ ConT ''Void
  (TupleT 1) -> pure $ TupleT 1
  (TupleT 2) -> pure $ ConT ''Either
  f@(TupleT _) -> throwE f
  f@(UnboxedTupleT i) -> throwE f -- pure $ UnboxedSumT i
  -- UnboxedSumT a        -> pure $ UnboxedTupleT a
  ArrowT -> pure ArrowT
  EqualityT -> pure EqualityT
  ListT -> pure ListT
  PromotedTupleT 0 -> pure $ ConT ''Void
  PromotedTupleT 1 -> pure $ PromotedTupleT 1
  PromotedTupleT 2 -> pure $ ConT ''Either
  f@(PromotedTupleT _) -> throwE f
  PromotedNilT -> pure PromotedNilT
  PromotedConsT -> pure PromotedConsT
  StarT -> pure StarT
  ConstraintT -> pure ConstraintT
  LitT l -> pure $ LitT l
  WildCardT -> pure WildCardT

exceptT :: (Monad m) => (t1 -> m c) -> (t2 -> m c) -> ExceptT t1 m t2 -> m c
exceptT f g =
  ( \case
      Left a -> f a
      Right a -> g a
  )
    <=< runExceptT

-- | Returns a type that is the dual of the input type.
dualType :: Type -> Q Type
dualType type' = do
  duals <- _dualTypes <$> retrieveDuals
  exceptT (\t -> fail $ "no dual for type " ++ show t) pure $
    dualType' duals type'

dualGuard' :: DualMappings -> Guard -> ExceptT (Either Type Exp) Q Guard
dualGuard' db = \case
  NormalG e -> NormalG <$> dualExp' db e
  PatG ss -> PatG <$> traverse (dualStmt' db) ss

dualDec' :: DualMappings -> Dec -> ExceptT (Either Type Exp) Q Dec
dualDec' db = pure

dualPat' :: DualMappings -> Pat -> ExceptT (Either Type Exp) Q Pat
dualPat' db = \case
  LitP l -> pure $ LitP l
  VarP n -> pure $ VarP n
  TupP ps -> TupP <$> traverse (dualPat' db) ps -- FIXME: should also Either?
  -- UnboxedTupP [Pat]
  -- UnboxedSumP Pat SumAlt SumArity
  x -> lift . fail $ "unhandled pattern " ++ show x

-- ConP Name [Pat]
-- InfixP Pat Name Pat
-- UInfixP Pat Name Pat
-- ParensP Pat
-- TildeP Pat
-- BangP Pat
-- AsP Name Pat
-- WildP
-- RecP Name [FieldPat]
-- ListP [Pat]
-- SigP Pat Type
-- ViewP Exp Pat

dualBody' :: DualMappings -> Body -> ExceptT (Either Type Exp) Q Body
dualBody' db = \case
  GuardedB xs ->
    GuardedB <$> traverse (bisequence . (dualGuard' db *** dualExp' db)) xs
  NormalB e -> NormalB <$> dualExp' db e

dualMatch' :: DualMappings -> Match -> ExceptT (Either Type Exp) Q Match
dualMatch' db (Match p b ds) =
  Match <$> dualPat' db p <*> dualBody' db b <*> traverse (dualDec' db) ds

{- ORMOLU_DISABLE -}
{- because it can’t handle CPP within a declaration -}
dualExp' :: DualMappings -> Exp -> ExceptT (Either Type Exp) Q Exp
dualExp' db = \case
  v@(VarE n) ->
    ExceptT . recover (pure $ pure v) . runExceptT $ dualExpName db n
  ConE n -> dualExpName db n
  l@(LitE _) -> pure l
  AppE a b -> AppE <$> dualExp' db a <*> dualExp' db b
  -- AppTypeE e t -> AppTypeE <$> dualExp' db e <*> dualType' (_dualTypes db) t
  InfixE a o b ->
    InfixE
    <$> traverse (dualExp' db) a
    <*> dualExp' db o
    <*> traverse (dualExp' db) b
  UInfixE a o b -> UInfixE <$> dualExp' db a <*> dualExp' db o <*> dualExp' db b
  ParensE e -> ParensE <$> dualExp' db e
  LamE p e -> LamE p <$> dualExp' db e
  LamCaseE matches -> LamCaseE <$> traverse (dualMatch' db) matches
#if MIN_VERSION_template_haskell(2, 16, 0)
  TupE es -> TupE <$> traverse (traverse $ dualExp' db) es -- FIXME: Doesn’t seem right.
  UnboxedTupE es -> UnboxedTupE <$> traverse (traverse $ dualExp' db) es
#else
  TupE es -> TupE <$> traverse (dualExp' db) es -- FIXME: Doesn’t seem right.
  UnboxedTupE es -> UnboxedTupE <$> traverse (dualExp' db) es
#endif
  -- UnboxedSumE e alt ar ->
  --   UnboxedTupE <$> traverse (dualExp' db) es <*> pure alt <*> pure ar
  CondE t c a -> CondE <$> dualExp' db t <*> dualExp' db c <*> dualExp' db a
  MultiIfE cases ->
    MultiIfE <$> traverse (bisequence . (dualGuard' db *** dualExp' db)) cases
  LetE ds e -> LetE <$> traverse (dualDec' db) ds <*> dualExp' db e
  CaseE e ms -> CaseE <$> dualExp' db e <*> traverse (dualMatch' db) ms
#if MIN_VERSION_template_haskell(2, 17, 0)
  DoE m ss -> DoE m <$> traverse (dualStmt' db) ss
#else
  DoE ss -> DoE <$> traverse (dualStmt' db) ss
#endif
  CompE ss -> CompE <$> traverse (dualStmt' db) ss
  ArithSeqE r -> pure $ ArithSeqE r
  ListE es -> ListE <$> traverse (dualExp' db) es
  SigE e t -> SigE <$> dualExp' db e <*> withExceptT Left (dualType' (_dualTypes db) t)
  e@(RecConE _ _) -> throwE $ Right e
  e@(RecUpdE _ _) -> throwE $ Right e
  StaticE e -> StaticE <$> dualExp' db e
  UnboundVarE n -> pure $ UnboundVarE n
{- ORMOLU_ENABLE -}

dualClause' :: DualMappings -> Clause -> ExceptT (Either Type Exp) Q Clause
dualClause' db (Clause ps b ds) =
  Clause
    <$> traverse (dualPat' db) ps
    <*> dualBody' db b
    <*> traverse (dualDec' db) ds

dualStmt' :: DualMappings -> Stmt -> ExceptT (Either Type Exp) Q Stmt
dualStmt' db = \case
  BindS p e -> BindS <$> dualPat' db p <*> dualExp' db e
  LetS ds -> LetS <$> traverse (dualDec' db) ds
  NoBindS e -> NoBindS <$> dualExp' db e
  ParS sss -> ParS <$> traverse (traverse (dualStmt' db)) sss

handleMissingDual :: ExceptT (Either Type Exp) Q a -> Q a
handleMissingDual =
  exceptT
    (\t -> fail $ "no dual for " ++ either (\t -> "type " ++ show t) (\e -> "expression " ++ show e) t)
    pure

-- | Convert an expression to its dual (i.e., an implementation for the dual
--   of the input expression’s type)
dualExp :: Exp -> Q Exp
dualExp exp = do
  duals <- retrieveDuals
  handleMissingDual $ dualExp' duals exp

-- | Indicates that some name represents the dual of itself (e.g., `Functor`).
labelSelfDual :: Name -> Q [Dec]
labelSelfDual name = do
  duals <- retrieveDuals
  a <- fromName name
  putQ $
    andMaybe
      (\t -> duals & dualTypes %~ Map.insert name t)
      (\_ e -> duals & dualValues %~ Map.insert name e)
      a
  pure []

-- | This provides a mapping one way, but not the other. Useful for aliased
--   functions (`return`) and overconstrained versions (e.g., mapping
--  `traverse ↔ distribute` but also `mapM → distribute`).
labelSemiDual :: Name -> Name -> Q [Dec]
labelSemiDual name coname = do
  duals <- retrieveDuals
  a <- fromName name
  b <- fromName coname
  case (a, b) of
    (Only _, Only t) -> putQ $ duals & dualTypes %~ Map.insert name t
    (Indeed _ _, Indeed _ e) -> putQ $ duals & dualValues %~ Map.insert name e
    (_, _) -> fail $ show name ++ " and " ++ show coname ++ "are not in the same namespace: " ++ show a ++ " " ++ show b
  pure []

labelDualDataT :: Name -> Name -> Type -> Type -> Q [Dec]
labelDualDataT name coname type' cotype' = do
  duals <- retrieveDuals
  pure [] <* (putQ $ duals & dualTypes %~ (Map.insert coname type' . Map.insert name cotype'))

addDualExp :: Name -> Name -> Exp -> Exp -> Q DualMappings
addDualExp name coname exp' coexp' = do
  duals <- retrieveDuals
  pure $ duals & dualValues %~ (Map.insert coname exp' . Map.insert name coexp')

labelDualExpT :: Name -> Name -> Exp -> Exp -> Q [Dec]
labelDualExpT name coname exp' coexp' = do
  duals <- retrieveDuals
  pure [] <* (putQ $ duals & dualValues %~ (Map.insert coname exp' . Map.insert name coexp'))

-- | Indicate that two names are duals of each other.
labelDual :: Name -> Name -> Q [Dec]
labelDual name coname = do
  a <- fromName name
  b <- fromName coname
  case (a, b) of
    (Only a, Only b) -> labelDualDataT name coname a b
    (Indeed _ a, Indeed _ b) -> labelDualExpT name coname a b
    (_, _) -> fail $ show name ++ " and " ++ show coname ++ "are not in the same namespace: " ++ show a ++ " " ++ show b

stripForall :: Type -> Type
stripForall (ForallT _ _ t) = t
stripForall t = t

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
      (ClassD ctx' coname tVars fds (fmap (uncurry SigD) meths') :)
        <$> labelDualDataT name coname type' (ConT coname)
    _ -> fail "not a type class"

makeDualExp :: String -> Q Type -> Q Exp -> String -> Q [Dec]
makeDualExp str type' exp' costr = do
  let name = mkName str
      coname = mkName costr

  sequenceA
    [ SigD name <$> type',
      ValD (VarP name) <$> (NormalB <$> exp') <*> pure [],
      SigD coname <$> (dualType =<< type'),
      ValD (VarP coname) <$> (NormalB <$> (dualExp =<< exp')) <*> pure []
    ]

-- | Creates a value that can be referenced in other modules to load the duals
--   defined in this module. Should be used at the bottom of any module that
--   uses this module.
exportDuals :: String -> Q [Dec]
exportDuals name = do
  typ <- [t|Q DualMappings|]
  exp <- reifyDuals =<< retrieveDuals
  let name' = mkName name
  pure
    [ SigD name' typ,
      ValD (VarP name') (NormalB exp) []
    ]

-- | Imports duals from other modules via the var created by `exportDuals` in
--   that other module.
importDuals :: Q DualMappings -> Q [Dec]
importDuals duals = do
  oldDuals <- getQ
  newDuals <- duals
  putQ $ maybe newDuals (newDuals <>) oldDuals
  pure []

errorMultipleNewNames :: Name -> Q a
errorMultipleNewNames n =
  fail $ "declaration introduces multiple new names: " ++ show n

errorNoNewName :: Q a
errorNoNewName = fail "declaration doesn’t introduce a new name"

dualCon' :: Map Name Type -> Name -> Con -> ExceptT Type Q Con
dualCon' db coname = \case
  NormalC _ bts -> NormalC coname <$> traverse (traverse $ dualType' db) bts
  -- TODO: Probably want to dualize field names, too.
  RecC _ vbts -> RecC coname <$> traverse (\(a, b, c) -> fmap (a,b,) $ dualType' db c) vbts
  InfixC bt _ bt' ->
    InfixC
      <$> traverse (dualType' db) bt
      <*> pure coname
      <*> traverse (dualType' db) bt'
  ForallC tvbs cx cn ->
    ForallC tvbs <$> traverse (dualType' db) cx <*> dualCon' db coname cn
  GadtC ns bts t -> undefined -- how do we handle the multiple names here
  RecGadtC ns vbts t -> undefined -- and here?

dualTySynEqn' :: Map Name Type -> TySynEqn -> ExceptT Type Q TySynEqn
#if MIN_VERSION_template_haskell(2, 15, 0)
dualTySynEqn' db (TySynEqn bs t t') =
  TySynEqn bs <$> dualType' db t <*> dualType' db t'
#else
dualTySynEqn' db (TySynEqn ts t) =
  TySynEqn <$> traverse (dualType' db) ts <*> dualType' db t
#endif

dualizeDec :: DualMappings -> Name -> Dec -> Q [Dec]
dualizeDec db coname d =
  handleMissingDual $
    (\c -> [d, c])
      <$> case d of
        FunD n cs -> do
          newMap <- lift $ addDualExp n coname (VarE n) (VarE coname)
          FunD coname <$> traverse (dualClause' newMap) cs
        -- TODO: Handle other vals
        ValD (VarP n) b ds -> do
          newMap <- lift $ addDualExp n coname (VarE n) (VarE coname)
          ValD (VarP coname)
            <$> dualBody' newMap b
            <*> traverse (dualDec' newMap) ds
        DataD cx n tvbs k [cn] dcs ->
          withExceptT Left $
            DataD
              <$> traverse (dualType' (_dualTypes db)) cx
              <*> pure coname
              <*> pure tvbs
              <*> pure k
              <*> ((: []) <$> dualCon' (_dualTypes db) coname cn)
              <*> pure dcs -- Should actually dualize this
        DataD _ n _ _ _ _ -> lift $ errorMultipleNewNames n
        NewtypeD cx n tvbs k cn dcs ->
          withExceptT Left $
            NewtypeD
              <$> traverse (dualType' (_dualTypes db)) cx
              <*> pure coname
              <*> pure tvbs
              <*> pure k
              <*> dualCon' (_dualTypes db) coname cn
              <*> pure dcs -- Should actually dualize this
        TySynD _ tvbs t ->
          TySynD coname tvbs <$> withExceptT Left (dualType' (_dualTypes db) t)
        SigD _ t -> SigD coname <$> withExceptT Left (dualType' (_dualTypes db) t)
        DataFamilyD _ tvbs k -> pure $ DataFamilyD coname tvbs k
        OpenTypeFamilyD (TypeFamilyHead n tvbs frs ia) ->
          pure . OpenTypeFamilyD $ TypeFamilyHead coname tvbs frs ia
        ClosedTypeFamilyD (TypeFamilyHead n tvbs frs ia) tses ->
          ClosedTypeFamilyD (TypeFamilyHead coname tvbs frs ia)
            <$> withExceptT Left (traverse (dualTySynEqn' $ _dualTypes db) tses)
        _ -> lift $ errorNoNewName

-- | Creates both the original declaration and its dual. Should only work for
--   declarations that introduce exactly one top-level name.
makeDualDec :: Q [Dec] -> String -> Q [Dec]
makeDualDec decs co = do
  let coname = mkName co
  db <- retrieveDuals
  fmap join . traverse (dualizeDec db coname) =<< decs
