{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Unsafe #-}

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

import safe Control.Applicative (pure, (<*>))
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
import safe Data.Functor (fmap, (<$), (<$>))
import safe Data.List (nub)
import safe Data.Map (Map)
import safe qualified Data.Map as Map
import safe Data.Maybe (maybe)
import safe Data.Monoid (Monoid, mappend, mempty)
import safe Data.Semigroup (Semigroup, (<>))
import safe Data.String (String)
import safe Data.Traversable (sequenceA, traverse)
import safe Data.Tuple (swap, uncurry)
import safe Data.Void (Void)
import safe qualified Language.Haskell.TH as TH
import safe Language.Haskell.TH.Syntax
  ( Body (GuardedB, NormalB),
    Clause (Clause),
    Con (ForallC, GadtC, InfixC, NormalC, RecC, RecGadtC),
    Guard (NormalG, PatG),
    Match (Match),
    Name,
    Q,
    TySynEqn (TySynEqn),
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
  { _dualTypes :: Map Name TH.Type,
    _dualValues :: Map Name TH.Exp
  }
  deriving stock (Data, Eq)

makeLenses ''DualMappings

instance Semigroup DualMappings where
  DualMappings t v <> DualMappings t' v' =
    -- NB: I reversed the order here, because I _think_ this is supposed to be
    --     right-biased?
    DualMappings (t' `Map.union` t) (v' `Map.union` v)

instance Monoid DualMappings where
  mappend = (<>)
  mempty = DualMappings Map.empty Map.empty

-- | The empty set of duals, should only be used to initalize the duals for
--   `Prelude`.
emptyDuals :: Q DualMappings
emptyDuals = pure $ DualMappings Map.empty Map.empty

reifyDuals :: DualMappings -> Q TH.Exp
reifyDuals duals =
  [e|maybe $(liftData duals) ($(liftData duals) <>) <$> getQ|]

shareDuals :: DualMappings -> Q TH.Exp
shareDuals duals =
  [e|[] <$ (putQ . maybe $(liftData duals) ($(liftData duals) <>) =<< getQ)|]

-- TODO: Move this somewhere better
data AndMaybe a b = Only a | Indeed a b deriving stock (Eq, Show)

andMaybe :: (a -> c) -> (a -> b -> c) -> a `AndMaybe` b -> c
andMaybe f g = \case
  Only a -> f a
  Indeed a b -> g a b

fromInfo :: TH.Info -> Q (TH.Type `AndMaybe` TH.Exp)
fromInfo = \case
  TH.ClassI (TH.ClassD _ n _ _ _) _ -> pure . Only $ TH.ConT n
  TH.ClassI d _ -> fail $ "unknown dec to extract name from: " <> show d
  TH.ClassOpI n t _ -> pure . Indeed t $ TH.VarE n
  TH.TyConI (TH.DataD _ n _ _ _ _) -> pure . Only $ TH.ConT n
  TH.TyConI (TH.TySynD n _ _) -> pure . Only $ TH.ConT n
  TH.TyConI (TH.NewtypeD _ n _ _ _ _) -> pure . Only $ TH.ConT n
  TH.TyConI d -> fail $ "unknown dec to extract name from: " <> show d
  TH.FamilyI d _ -> fail $ "not yet getting type families – " <> show d -- FIXME
  TH.PrimTyConI n _ _ -> pure . Only $ TH.ConT n
  TH.DataConI n t _ -> pure . Indeed t $ TH.ConE n
  TH.PatSynI _ _ -> fail "pattern synonym is not a type"
  TH.VarI n t _ -> pure . Indeed t $ TH.VarE n
  TH.TyVarI _ t -> pure $ Only t

fromName :: Name -> Q (TH.Type `AndMaybe` TH.Exp)
fromName = fromInfo <=< reify

typeFromName :: Name -> Q TH.Type
typeFromName = fmap (andMaybe id const) . fromName

expFromName :: Name -> Q TH.Exp
expFromName =
  andMaybe (\t -> fail $ show t <> " is not a value") (\_ e -> pure e)
    <=< fromName

-- | Returns a `TH.Type` that is the dual of the named type.
dualTypeName :: Map Name TH.Type -> Name -> ExceptT TH.Type Q TH.Type
dualTypeName db name =
  maybe (dualType' db <=< lift $ typeFromName name) pure $ Map.lookup name db

dualExpName :: DualMappings -> Name -> ExceptT (Either TH.Type TH.Exp) Q TH.Exp
dualExpName db name =
  maybe (dualExp' db <=< lift $ expFromName name) pure $
    Map.lookup name (_dualValues db)

retrieveDuals :: Q DualMappings
retrieveDuals = maybe (fail "no duals imported") pure =<< getQ

-- FIXME: This can get into an infinite loop in the case of missing duals.
dualType' :: Map Name TH.Type -> TH.Type -> ExceptT TH.Type Q TH.Type
dualType' db = \case
  TH.ForallT vs c t ->
    TH.ForallT vs <$> traverse (dualType' db) c <*> dualType' db t
  TH.AppT (TH.AppT TH.ArrowT t) inner@(TH.AppT (TH.AppT TH.ArrowT _) _) -> do
    t' <- dualType' db t
    TH.AppT (TH.AppT TH.ArrowT t') <$> dualType' db inner
  TH.AppT (TH.AppT TH.ArrowT t) t' ->
    TH.AppT <$> (TH.AppT TH.ArrowT <$> dualType' db t') <*> dualType' db t
  TH.AppT t t' -> TH.AppT <$> dualType' db t <*> dualType' db t'
  TH.SigT t k -> flip TH.SigT k <$> dualType' db t
  TH.VarT n -> pure $ TH.VarT n
  TH.ConT n -> dualTypeName db n
  TH.PromotedT n -> pure $ TH.PromotedT n
  TH.InfixT _t n _t' -> dualTypeName db n -- t t'
  TH.UInfixT _t n _t' -> dualTypeName db n -- t t'
  TH.ParensT t -> pure $ TH.ParensT t
  TH.TupleT 0 -> pure $ TH.ConT ''Void
  TH.TupleT 1 -> pure $ TH.TupleT 1
  TH.TupleT 2 -> pure $ TH.ConT ''Either
  f@(TH.TupleT _) -> throwE f
  TH.UnboxedSumT a -> pure $ TH.UnboxedTupleT a
  TH.ArrowT -> pure TH.ArrowT
  TH.EqualityT -> pure TH.EqualityT
  TH.ListT -> pure TH.ListT
  TH.PromotedTupleT 0 -> pure $ TH.ConT ''Void
  TH.PromotedTupleT 1 -> pure $ TH.PromotedTupleT 1
  TH.PromotedTupleT 2 -> pure $ TH.ConT ''Either
  f@(TH.PromotedTupleT _) -> throwE f
  TH.PromotedNilT -> pure TH.PromotedNilT
  TH.PromotedConsT -> pure TH.PromotedConsT
  TH.StarT -> pure TH.StarT
  TH.ConstraintT -> pure TH.ConstraintT
  TH.LitT l -> pure $ TH.LitT l
  TH.WildCardT -> pure TH.WildCardT
#if MIN_VERSION_template_haskell(2, 19, 0)
  TH.PromotedInfixT _t n _t' -> dualTypeName db n -- t t'
  TH.PromotedUInfixT _t n _t' -> dualTypeName db n -- t t'
#endif
#if MIN_VERSION_template_haskell(2, 17, 0)
  TH.MulArrowT -> pure TH.MulArrowT
#endif
#if MIN_VERSION_template_haskell(2, 16, 0)
  TH.ForallVisT vs t -> TH.ForallVisT vs <$> dualType' db t
  TH.UnboxedTupleT i -> pure $ TH.UnboxedSumT i
#else
  f@(TH.UnboxedTupleT _) -> throwE f
#endif
#if MIN_VERSION_template_haskell(2, 15, 0)
  TH.AppKindT t k -> TH.AppKindT <$> dualType' db t <*> pure k
  TH.ImplicitParamT n t -> TH.ImplicitParamT n <$> dualType' db t
#endif

exceptT :: (Monad m) => (t1 -> m c) -> (t2 -> m c) -> ExceptT t1 m t2 -> m c
exceptT f g =
  ( \case
      Left a -> f a
      Right a -> g a
  )
    <=< runExceptT

-- | Returns a type that is the dual of the input type.
dualType :: TH.Type -> Q TH.Type
dualType type' = do
  duals <- _dualTypes <$> retrieveDuals
  exceptT (\t -> fail $ "no dual for type " <> show t) pure $
    dualType' duals type'

dualGuard' :: DualMappings -> Guard -> ExceptT (Either TH.Type TH.Exp) Q Guard
dualGuard' db = \case
  NormalG e -> NormalG <$> dualExp' db e
  PatG ss -> PatG <$> traverse (dualStmt' db) ss

dualDec' :: DualMappings -> TH.Dec -> ExceptT (Either TH.Type TH.Exp) Q TH.Dec
dualDec' _db = pure

dualPat' :: DualMappings -> TH.Pat -> ExceptT (Either TH.Type TH.Exp) Q TH.Pat
dualPat' db = \case
  TH.LitP l -> pure $ TH.LitP l
  TH.VarP n -> pure $ TH.VarP n
  TH.TupP ps -> TH.TupP <$> traverse (dualPat' db) ps -- FIXME: should also Either?
  p@(TH.UnboxedTupP _ps) -> lift . fail $ "unhandled pattern " <> show p
  p@(TH.UnboxedSumP _p _a _a') -> lift . fail $ "unhandled pattern " <> show p
  p@(TH.InfixP _p _n _p') -> lift . fail $ "unhandled pattern " <> show p
  p@(TH.UInfixP _p _n _p') -> lift . fail $ "unhandled pattern " <> show p
  p@(TH.ParensP _p) -> lift . fail $ "unhandled pattern " <> show p
  p@(TH.TildeP _p) -> lift . fail $ "unhandled pattern " <> show p
  p@(TH.BangP _p) -> lift . fail $ "unhandled pattern " <> show p
  p@(TH.AsP _n _p) -> lift . fail $ "unhandled pattern " <> show p
  p@TH.WildP -> lift . fail $ "unhandled pattern " <> show p
  p@(TH.RecP _n _fps) -> lift . fail $ "unhandled pattern " <> show p
  p@(TH.ListP _ps) -> lift . fail $ "unhandled pattern " <> show p
  p@(TH.SigP _p _t) -> lift . fail $ "unhandled pattern " <> show p
  p@(TH.ViewP _e _p) -> lift . fail $ "unhandled pattern " <> show p
#if MIN_VERSION_template_haskell(2, 22, 0)
  p@(TH.TypeP _t) -> lift . fail $ "unhandled pattern " <> show p
  p@(TH.InvisP _t) -> lift . fail $ "unhandled pattern " <> show p
#endif
#if MIN_VERSION_template_haskell(2, 18, 0)
  p@(TH.ConP _n _ts _ps) -> lift . fail $ "unhandled pattern " <> show p
#else
  p@(TH.ConP _n _ps) -> lift . fail $ "unhandled pattern " <> show p
#endif

dualBody' :: DualMappings -> Body -> ExceptT (Either TH.Type TH.Exp) Q Body
dualBody' db = \case
  GuardedB xs ->
    GuardedB <$> traverse (bisequence . (dualGuard' db *** dualExp' db)) xs
  NormalB e -> NormalB <$> dualExp' db e

dualMatch' :: DualMappings -> Match -> ExceptT (Either TH.Type TH.Exp) Q Match
dualMatch' db (Match p b ds) =
  Match <$> dualPat' db p <*> dualBody' db b <*> traverse (dualDec' db) ds

dualExp' :: DualMappings -> TH.Exp -> ExceptT (Either TH.Type TH.Exp) Q TH.Exp
dualExp' db = \case
  v@(TH.VarE n) ->
    ExceptT . recover (pure $ pure v) . runExceptT $ dualExpName db n
  TH.ConE n -> dualExpName db n
  l@(TH.LitE _) -> pure l
  TH.AppE a b -> TH.AppE <$> dualExp' db a <*> dualExp' db b
  TH.AppTypeE e t ->
    TH.AppTypeE
      <$> dualExp' db e
      <*> withExceptT Left (dualType' (_dualTypes db) t)
  TH.InfixE a o b ->
    TH.InfixE
      <$> traverse (dualExp' db) a
      <*> dualExp' db o
      <*> traverse (dualExp' db) b
  TH.UInfixE a o b ->
    TH.UInfixE <$> dualExp' db a <*> dualExp' db o <*> dualExp' db b
  TH.ParensE e -> TH.ParensE <$> dualExp' db e
  TH.LamE p e -> TH.LamE p <$> dualExp' db e
  TH.LamCaseE matches -> TH.LamCaseE <$> traverse (dualMatch' db) matches
  TH.UnboxedSumE e alt ar ->
    TH.UnboxedSumE <$> dualExp' db e <*> pure alt <*> pure ar
  TH.CondE t c a ->
    TH.CondE <$> dualExp' db t <*> dualExp' db c <*> dualExp' db a
  TH.MultiIfE cases ->
    TH.MultiIfE
      <$> traverse (bisequence . (dualGuard' db *** dualExp' db)) cases
  TH.LetE ds e -> TH.LetE <$> traverse (dualDec' db) ds <*> dualExp' db e
  TH.CaseE e ms -> TH.CaseE <$> dualExp' db e <*> traverse (dualMatch' db) ms
  TH.CompE ss -> TH.CompE <$> traverse (dualStmt' db) ss
  TH.ArithSeqE r -> pure $ TH.ArithSeqE r
  TH.ListE es -> TH.ListE <$> traverse (dualExp' db) es
  TH.SigE e t ->
    TH.SigE <$> dualExp' db e <*> withExceptT Left (dualType' (_dualTypes db) t)
  e@(TH.RecConE _ _) -> throwE $ Right e
  e@(TH.RecUpdE _ _) -> throwE $ Right e
  TH.StaticE e -> TH.StaticE <$> dualExp' db e
  TH.UnboundVarE n -> pure $ TH.UnboundVarE n
  TH.LabelE l -> pure $ TH.LabelE l
#if MIN_VERSION_template_haskell(2, 22, 0)
  e@(TH.TypeE _) -> throwE $ Right e
#endif
#if MIN_VERSION_template_haskell(2, 21, 0)
  e@(TH.TypedBracketE _) -> throwE $ Right e
  e@(TH.TypedSpliceE _) -> throwE $ Right e
#endif
#if MIN_VERSION_template_haskell(2, 19, 0)
  TH.LamCasesE cs -> TH.LamCasesE <$> traverse (dualClause' db) cs
#endif
#if MIN_VERSION_template_haskell(2, 18, 0)
  TH.GetFieldE e f -> TH.GetFieldE <$> dualExp' db e <*> pure f
  TH.ProjectionE fs -> pure $ TH.ProjectionE fs
#endif
#if MIN_VERSION_template_haskell(2, 17, 0)
  TH.DoE m ss -> TH.DoE m <$> traverse (dualStmt' db) ss
#else
  TH.DoE ss -> TH.DoE <$> traverse (dualStmt' db) ss
#endif
#if MIN_VERSION_template_haskell(2, 16, 0)
  TH.TupE es -> TH.TupE <$> traverse (traverse $ dualExp' db) es -- FIXME: Doesn’t seem right.
  TH.UnboxedTupE es -> TH.UnboxedTupE <$> traverse (traverse $ dualExp' db) es
#else
  TH.TupE es -> TH.TupE <$> traverse (dualExp' db) es -- FIXME: Doesn’t seem right.
  e@(TH.UnboxedTupE _) -> throwE $ pure e
#endif
#if MIN_VERSION_template_haskell(2, 15, 0)
#if MIN_VERSION_template_haskell(2, 17, 0)
  TH.MDoE m ss -> TH.MDoE m <$> traverse (dualStmt' db) ss
#else
  TH.MDoE ss -> TH.MDoE <$> traverse (dualStmt' db) ss
#endif
  TH.ImplicitParamVarE n -> pure $ TH.ImplicitParamVarE n
#endif

dualClause' :: DualMappings -> Clause -> ExceptT (Either TH.Type TH.Exp) Q Clause
dualClause' db (Clause ps b ds) =
  Clause
    <$> traverse (dualPat' db) ps
    <*> dualBody' db b
    <*> traverse (dualDec' db) ds

dualStmt' :: DualMappings -> TH.Stmt -> ExceptT (Either TH.Type TH.Exp) Q TH.Stmt
dualStmt' db = \case
  TH.BindS p e -> TH.BindS <$> dualPat' db p <*> dualExp' db e
  TH.LetS ds -> TH.LetS <$> traverse (dualDec' db) ds
  TH.NoBindS e -> TH.NoBindS <$> dualExp' db e
  TH.ParS sss -> TH.ParS <$> traverse (traverse (dualStmt' db)) sss
#if MIN_VERSION_template_haskell(2, 15, 0)
  TH.RecS ss -> TH.RecS <$> traverse (dualStmt' db) ss
#endif

handleMissingDual :: ExceptT (Either TH.Type TH.Exp) Q a -> Q a
handleMissingDual =
  exceptT
    ( fail
        . ("no dual for " <>)
        . either (("type " <>) . show) (("expression " <>) . show)
    )
    pure

-- | Convert an expression to its dual (i.e., an implementation for the dual
--   of the input expression’s type)
dualExp :: TH.Exp -> Q TH.Exp
dualExp exp = do
  duals <- retrieveDuals
  handleMissingDual $ dualExp' duals exp

-- | Indicates that some name represents the dual of itself (e.g., `Functor`).
labelSelfDual :: Name -> Q [a]
labelSelfDual name = do
  duals <- retrieveDuals
  a <- fromName name
  []
    <$ putQ
      ( andMaybe
          (\t -> duals & dualTypes %~ Map.insert name t)
          (\_ e -> duals & dualValues %~ Map.insert name e)
          a
      )

-- | This provides a mapping one way, but not the other. Useful for aliased
--   functions (`return`) and overconstrained versions (e.g., mapping
--  `traverse ↔ distribute` but also `mapM → distribute`).
labelSemiDual :: Name -> Name -> Q [a]
labelSemiDual name coname = do
  duals <- retrieveDuals
  a <- fromName name
  b <- fromName coname
  [] <$ case (a, b) of
    (Only _, Only t) -> putQ $ duals & dualTypes %~ Map.insert name t
    (Indeed _ _, Indeed _ e) -> putQ $ duals & dualValues %~ Map.insert name e
    (_, _) ->
      fail $
        show name
          <> " and "
          <> show coname
          <> "are not in the same namespace: "
          <> show a
          <> " "
          <> show b

labelDualDataT :: Name -> Name -> TH.Type -> TH.Type -> Q [a]
labelDualDataT name coname type' cotype' = do
  duals <- retrieveDuals
  [] <$ putQ (duals & dualTypes %~ (Map.insert coname type' . Map.insert name cotype'))

addDualExp :: Name -> Name -> TH.Exp -> TH.Exp -> Q DualMappings
addDualExp name coname exp' coexp' = do
  duals <- retrieveDuals
  pure $ duals & dualValues %~ (Map.insert coname exp' . Map.insert name coexp')

labelDualExpT :: Name -> Name -> TH.Exp -> TH.Exp -> Q [a]
labelDualExpT name coname exp' coexp' = do
  duals <- retrieveDuals
  [] <$ putQ (duals & dualValues %~ (Map.insert coname exp' . Map.insert name coexp'))

-- | Indicate that two names are duals of each other.
labelDual :: Name -> Name -> Q [TH.Dec]
labelDual name coname = do
  a <- fromName name
  b <- fromName coname
  case (a, b) of
    (Only a', Only b') -> labelDualDataT name coname a' b'
    (Indeed _ a', Indeed _ b') -> labelDualExpT name coname a' b'
    (_, _) ->
      fail $
        show name
          <> " and "
          <> show coname
          <> "are not in the same namespace: "
          <> show a
          <> " "
          <> show b

stripForall :: TH.Type -> TH.Type
stripForall (TH.ForallT _ _ t) = t
stripForall t = t

-- | Given a class, creates a new class that represents its dual, with the list
--   containing name mappings of methods to their duals.
makeDualClass :: Name -> String -> [(Name, String)] -> Q [TH.Dec]
makeDualClass name co methods = do
  let coname = mkName co
  info <- reify name
  type' <- typeFromName name
  case info of
    TH.ClassI (TH.ClassD ctx _ tVars fds _) _ -> do
      ctx' <- nub <$> traverse dualType ctx
      meths' <-
        traverse
          ( sequenceA
              . (mkName *** ((dualType . stripForall) <=< typeFromName))
              . swap
          )
          methods
      (TH.ClassD ctx' coname tVars fds (fmap (uncurry TH.SigD) meths') :)
        <$> labelDualDataT name coname type' (TH.ConT coname)
    _ -> fail "not a type class"

makeDualExp :: String -> Q TH.Type -> Q TH.Exp -> String -> Q [TH.Dec]
makeDualExp str type' exp' costr = do
  let name = mkName str
      coname = mkName costr

  sequenceA
    [ TH.SigD name <$> type',
      TH.ValD (TH.VarP name) <$> (NormalB <$> exp') <*> pure [],
      TH.SigD coname <$> (dualType =<< type'),
      TH.ValD (TH.VarP coname) <$> (NormalB <$> (dualExp =<< exp')) <*> pure []
    ]

-- | Creates a value that can be referenced in other modules to load the duals
--   defined in this module. Should be used at the bottom of any module that
--   uses this module.
exportDuals :: String -> Q [TH.Dec]
exportDuals name = do
  typ <- [t|Q DualMappings|]
  exp <- reifyDuals =<< retrieveDuals
  let name' = mkName name
  pure
    [ TH.SigD name' typ,
      TH.ValD (TH.VarP name') (NormalB exp) []
    ]

-- | Imports duals from other modules via the var created by `exportDuals` in
--   that other module.
importDuals :: Q DualMappings -> Q [a]
importDuals duals = do
  oldDuals <- getQ
  newDuals <- duals
  [] <$ putQ (maybe newDuals (newDuals <>) oldDuals)

errorMultipleNewNames :: Name -> Q a
errorMultipleNewNames n =
  fail $ "declaration introduces multiple new names: " <> show n

errorNoNewName :: Q a
errorNoNewName = fail "declaration doesn’t introduce a new name"

dualCon' :: Map Name TH.Type -> Name -> Con -> ExceptT TH.Type Q Con
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
  GadtC _ns _bts _t -> undefined -- how do we handle the multiple names here
  RecGadtC _ns _vbts _t -> undefined -- and here?

dualTySynEqn' :: Map Name TH.Type -> TySynEqn -> ExceptT TH.Type Q TySynEqn
#if MIN_VERSION_template_haskell(2, 15, 0)
dualTySynEqn' db (TySynEqn bs t t') =
  TySynEqn bs <$> dualType' db t <*> dualType' db t'
#else
dualTySynEqn' db (TySynEqn ts t) =
  TySynEqn <$> traverse (dualType' db) ts <*> dualType' db t
#endif

dualizeDec :: DualMappings -> Name -> TH.Dec -> Q [TH.Dec]
dualizeDec db coname d =
  handleMissingDual $ (d :) . pure <$> dualizeDec' db coname d

dualizeDec' ::
  DualMappings -> Name -> TH.Dec -> ExceptT (Either TH.Type TH.Exp) Q TH.Dec
dualizeDec' db coname = \case
  TH.FunD n cs -> do
    newMap <- lift $ addDualExp n coname (TH.VarE n) (TH.VarE coname)
    TH.FunD coname <$> traverse (dualClause' newMap) cs
  -- TODO: Handle other vals
  TH.ValD (TH.VarP n) b ds -> do
    newMap <- lift $ addDualExp n coname (TH.VarE n) (TH.VarE coname)
    TH.ValD (TH.VarP coname)
      <$> dualBody' newMap b
      <*> traverse (dualDec' newMap) ds
  TH.ValD {} -> lift errorNoNewName
  TH.DataD cx _n tvbs k [cn] dcs ->
    withExceptT Left $
      TH.DataD
        <$> traverse (dualType' (_dualTypes db)) cx
        <*> pure coname
        <*> pure tvbs
        <*> pure k
        <*> ((: []) <$> dualCon' (_dualTypes db) coname cn)
        <*> pure dcs -- Should actually dualize this
  TH.DataD _ n _ _ _ _ -> lift $ errorMultipleNewNames n
  TH.NewtypeD cx _n tvbs k cn dcs ->
    withExceptT Left $
      TH.NewtypeD
        <$> traverse (dualType' (_dualTypes db)) cx
        <*> pure coname
        <*> pure tvbs
        <*> pure k
        <*> dualCon' (_dualTypes db) coname cn
        <*> pure dcs -- Should actually dualize this
  TH.TySynD _ tvbs t ->
    TH.TySynD coname tvbs <$> withExceptT Left (dualType' (_dualTypes db) t)
  TH.ClassD {} -> lift errorNoNewName
  TH.InstanceD {} -> lift errorNoNewName
  TH.SigD _ t -> TH.SigD coname <$> withExceptT Left (dualType' (_dualTypes db) t)
  TH.ForeignD {} -> lift errorNoNewName
  TH.InfixD {} -> lift errorNoNewName
  TH.PragmaD {} -> lift errorNoNewName
  TH.DataFamilyD _ tvbs k -> pure $ TH.DataFamilyD coname tvbs k
  TH.DataInstD {} -> lift errorNoNewName
  TH.NewtypeInstD {} -> lift errorNoNewName
  TH.TySynInstD {} -> lift errorNoNewName
  TH.OpenTypeFamilyD (TypeFamilyHead _n tvbs frs ia) ->
    pure . TH.OpenTypeFamilyD $ TypeFamilyHead coname tvbs frs ia
  TH.ClosedTypeFamilyD (TypeFamilyHead _n tvbs frs ia) tses ->
    TH.ClosedTypeFamilyD (TypeFamilyHead coname tvbs frs ia)
      <$> withExceptT Left (traverse (dualTySynEqn' $ _dualTypes db) tses)
  TH.RoleAnnotD {} -> lift errorNoNewName
  TH.StandaloneDerivD {} -> lift errorNoNewName
  TH.DefaultSigD {} -> lift errorNoNewName
  TH.PatSynD {} -> lift errorNoNewName
  TH.PatSynSigD {} -> lift errorNoNewName
#if MIN_VERSION_template_haskell(2, 20, 0)
  TH.TypeDataD {} -> lift errorNoNewName
#endif
#if MIN_VERSION_template_haskell(2, 19, 0)
  TH.DefaultD {} -> lift errorNoNewName
#endif
#if MIN_VERSION_template_haskell(2, 16, 0)
  TH.KiSigD {} -> lift errorNoNewName
#endif
#if MIN_VERSION_template_haskell(2, 15, 0)
  TH.ImplicitParamBindD {} -> lift errorNoNewName
#endif

-- | Creates both the original declaration and its dual. Should only work for
--   declarations that introduce exactly one top-level name.
makeDualDec :: Q [TH.Dec] -> String -> Q [TH.Dec]
makeDualDec decs co = do
  let coname = mkName co
  db <- retrieveDuals
  fmap join . traverse (dualizeDec db coname) =<< decs
