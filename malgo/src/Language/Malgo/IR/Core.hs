{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Language.Malgo.IR.Core where

import Data.Set.Optics
import Data.Text (unpack)
import Koriel.Prelude
import Language.Malgo.IR.Op
import Language.Malgo.Id
import Language.Malgo.Monad (MonadUniq)
import Language.Malgo.Pretty
import Language.Malgo.TypeRep.CType
import Text.PrettyPrint.HughesPJ
  ( brackets,
    char,
    doubleQuotes,
    parens,
    quotes,
    sep,
    text,
    vcat,
    ($$),
  )
import qualified Text.PrettyPrint.HughesPJ as P

class HasFreeVar f where
  freevars :: Ord a => f a -> Set a

{-
Unboxed values  unboxed
-}
data Unboxed
  = Int32 Integer
  | Int64 Integer
  | Float Float
  | Double Double
  | Char Char
  | String String
  deriving stock (Eq, Ord, Show)

instance HasCType Unboxed where
  cTypeOf Int32 {} = Int32T
  cTypeOf Int64 {} = Int64T
  cTypeOf Float {} = FloatT
  cTypeOf Double {} = DoubleT
  cTypeOf Char {} = CharT
  cTypeOf String {} = StringT

instance Pretty Unboxed where
  pPrint (Int32 x) = pPrint x
  pPrint (Int64 x) = pPrint x
  pPrint (Float x) = pPrint x
  pPrint (Double x) = pPrint x
  pPrint (Char x) = quotes (char x)
  pPrint (String x) = doubleQuotes (text x)

{-
Atoms  a ::= unboxed | x
-}
data Atom a
  = Var a
  | Unboxed Unboxed
  deriving stock (Eq, Show, Functor, Foldable)

instance HasCType a => HasCType (Atom a) where
  cTypeOf (Var x) = cTypeOf x
  cTypeOf (Unboxed x) = cTypeOf x

instance Pretty a => Pretty (Atom a) where
  pPrint (Var x) = pPrint x
  pPrint (Unboxed x) = pPrint x

instance HasFreeVar Atom where
  freevars (Var x) = setOf equality x
  freevars Unboxed {} = mempty

class HasAtom f where
  atom :: Traversal' (f a) (Atom a)

instance HasAtom Atom where
  atom = castOptic equality

{-
Expressions  e ::= a               Atom
                 | f a_1 ... a_n   Function call (arity(f) >= 1)
                 | p a_1 ... a_n   Saturated primitive operation (n >= 1)
                 | a_1[a_2]        Read array
                 | a_1[a_2] <- a_3 Write array
                 | LET x = obj IN e
                 | MATCH e WITH { alt_1; ... alt_n; } (n >= 0)
-}
data Exp a
  = Atom (Atom a)
  | Call (Atom a) [Atom a]
  | CallDirect a [Atom a]
  | PrimCall Text CType [Atom a]
  | BinOp Op (Atom a) (Atom a)
  | ArrayRead (Atom a) (Atom a)
  | ArrayWrite (Atom a) (Atom a) (Atom a)
  | Cast CType (Atom a)
  | Let [(a, Obj a)] (Exp a)
  | Match (Exp a) (NonEmpty (Case a))
  | Error CType
  deriving stock (Eq, Show, Functor, Foldable)

instance HasCType a => HasCType (Exp a) where
  cTypeOf (Atom x) = cTypeOf x
  cTypeOf (Call f xs) =
    case cTypeOf f of
      ps :-> r -> go ps (map cTypeOf xs) r
      _ -> errorDoc $ "Invalid type:" <+> P.quotes (pPrint $ cTypeOf f)
    where
      go [] [] v = v
      go (p : ps) (x : xs) v = replaceOf tyVar p x (go ps xs v)
      go _ _ _ = bug Unreachable
  cTypeOf (CallDirect f xs) =
    case cTypeOf f of
      ps :-> r -> go ps (map cTypeOf xs) r
      _ -> bug Unreachable
    where
      go [] [] v = v
      go (p : ps) (x : xs) v = replaceOf tyVar p x (go ps xs v)
      go _ _ _ = bug Unreachable
  cTypeOf (PrimCall _ t xs) =
    case t of
      ps :-> r -> go ps (map cTypeOf xs) r
      _ -> bug Unreachable
    where
      go [] [] v = v
      go (p : ps) (x : xs) v = replaceOf tyVar p x (go ps xs v)
      go _ _ _ = bug Unreachable
  cTypeOf (BinOp o x _) =
    case o of
      Add -> cTypeOf x
      Sub -> cTypeOf x
      Mul -> cTypeOf x
      Div -> cTypeOf x
      Mod -> cTypeOf x
      FAdd -> cTypeOf x
      FSub -> cTypeOf x
      FMul -> cTypeOf x
      FDiv -> cTypeOf x
      Eq -> boolT
      Neq -> boolT
      Lt -> boolT
      Gt -> boolT
      Le -> boolT
      Ge -> boolT
      And -> boolT
      Or -> boolT
    where
      boolT = SumT [Con "True" [], Con "False" []]
  cTypeOf (ArrayRead a _) = case cTypeOf a of
    ArrayT t -> t
    _ -> bug Unreachable
  cTypeOf ArrayWrite {} = SumT [Con "Tuple0" []]
  cTypeOf (Cast ty _) = ty
  cTypeOf (Let _ e) = cTypeOf e
  cTypeOf (Match _ (c :| _)) = cTypeOf c
  cTypeOf (Error t) = t

returnType :: CType -> CType
returnType (_ :-> t) = t
returnType _ = bug Unreachable

instance Pretty a => Pretty (Exp a) where
  pPrint (Atom x) = pPrint x
  pPrint (Call f xs) = parens $ pPrint f <+> sep (map pPrint xs)
  pPrint (CallDirect f xs) = parens $ "direct" <+> pPrint f <+> sep (map pPrint xs)
  pPrint (PrimCall p _ xs) = parens $ "prim" <+> text (unpack p) <+> sep (map pPrint xs)
  pPrint (BinOp o x y) = parens $ pPrint o <+> pPrint x <+> pPrint y
  pPrint (ArrayRead a b) = pPrint a <> brackets (pPrint b)
  pPrint (ArrayWrite a b c) = parens $ pPrint a <> brackets (pPrint b) <+> "<-" <+> pPrint c
  pPrint (Cast ty x) = parens $ "cast" <+> pPrint ty <+> pPrint x
  pPrint (Let xs e) = parens $ "let" $$ parens (vcat (map (\(v, o) -> parens $ pPrint v $$ pPrint o) xs)) $$ pPrint e
  pPrint (Match v cs) = parens $ "match" <+> pPrint v $$ vcat (toList $ fmap pPrint cs)
  pPrint (Error _) = "ERROR"

instance HasFreeVar Exp where
  freevars (Atom x) = freevars x
  freevars (Call f xs) = freevars f <> foldMap freevars xs
  freevars (CallDirect _ xs) = foldMap freevars xs
  freevars (PrimCall _ _ xs) = foldMap freevars xs
  freevars (BinOp _ x y) = freevars x <> freevars y
  freevars (ArrayRead a b) = freevars a <> freevars b
  freevars (ArrayWrite a b c) = freevars a <> freevars b <> freevars c
  freevars (Cast _ x) = freevars x
  freevars (Let xs e) = foldr (sans . view _1) (freevars e <> foldMap (freevars . view _2) xs) xs
  freevars (Match e cs) = freevars e <> foldMap freevars cs
  freevars (Error _) = mempty

instance HasAtom Exp where
  atom = traversalVL $ \f -> \case
    Atom x -> Atom <$> f x
    Call x xs -> Call <$> f x <*> traverse f xs
    CallDirect x xs -> CallDirect x <$> traverse f xs
    PrimCall p t xs -> PrimCall p t <$> traverse f xs
    BinOp o x y -> BinOp o <$> f x <*> f y
    ArrayRead a b -> ArrayRead <$> f a <*> f b
    ArrayWrite a b c -> ArrayWrite <$> f a <*> f b <*> f c
    Cast ty x -> Cast ty <$> f x
    Let xs e -> Let <$> traverseOf (traversed % _2 % atom) f xs <*> traverseOf atom f e
    Match e cs -> Match <$> traverseOf atom f e <*> traverseOf (traversed % atom) f cs
    Error t -> pure (Error t)

{-
Alternatives  alt ::= UNPACK(C x_1 ... x_n) -> e  (n >= 0)
                    | SWITCH u -> e
                    | BIND x -> e
-}
data Case a
  = Unpack Con [a] (Exp a)
  | Switch Unboxed (Exp a)
  | Bind a (Exp a)
  deriving stock (Eq, Show, Functor, Foldable)

instance Pretty a => Pretty (Case a) where
  pPrint (Unpack c xs e) = parens $ sep ["unpack" <+> parens (pPrint c <+> sep (map pPrint xs)), pPrint e]
  pPrint (Switch u e) = parens $ sep ["switch" <+> pPrint u, pPrint e]
  pPrint (Bind x e) = parens $ sep ["bind" <+> pPrint x, pPrint e]

instance HasFreeVar Case where
  freevars (Unpack _ xs e) = foldr sans (freevars e) xs
  freevars (Switch _ e) = freevars e
  freevars (Bind x e) = sans x $ freevars e

instance HasCType a => HasCType (Case a) where
  cTypeOf (Unpack _ _ e) = cTypeOf e
  cTypeOf (Switch _ e) = cTypeOf e
  cTypeOf (Bind _ e) = cTypeOf e

instance HasAtom Case where
  atom = traversalVL $ \f -> \case
    Unpack con xs e -> Unpack con xs <$> traverseOf atom f e
    Switch u e -> Switch u <$> traverseOf atom f e
    Bind a e -> Bind a <$> traverseOf atom f e

{-
Heap objects  obj ::= FUN(x_1 ... x_n -> e)  Function (arity = n >= 1)
                    | PAP(f a_1 ... a_n)     Partial application (f is always a FUN with arity(f) > n >= 1)
                    | PACK(C a_1 ... a_n)    Saturated constructor (n >= 0)
                    | ARRAY(a, n)            Array (n > 0)
-}
data Obj a
  = Fun [a] (Exp a)
  | Pack CType Con [Atom a]
  | Array (Atom a) (Atom a)
  deriving stock (Eq, Show, Functor, Foldable)

instance Pretty a => Pretty (Obj a) where
  pPrint (Fun xs e) = parens $ sep ["fun" <+> parens (sep $ map pPrint xs), pPrint e]
  pPrint (Pack ty c xs) = parens $ sep (["pack", pPrint c] <> map pPrint xs) <+> ":" <+> pPrint ty
  pPrint (Array a n) = parens $ sep ["array", pPrint a, pPrint n]

instance HasFreeVar Obj where
  freevars (Fun as e) = foldr sans (freevars e) as
  freevars (Pack _ _ xs) = foldMap freevars xs
  freevars (Array a n) = freevars a <> freevars n

instance HasCType a => HasCType (Obj a) where
  cTypeOf (Fun xs e) = map cTypeOf xs :-> cTypeOf e
  cTypeOf (Pack t _ _) = t
  cTypeOf (Array a _) = ArrayT $ cTypeOf a

instance HasAtom Obj where
  atom = traversalVL $ \f -> \case
    Fun xs e -> Fun xs <$> traverseOf atom f e
    Pack ty con xs -> Pack ty con <$> traverseOf (traversed % atom) f xs
    Array a n -> Array <$> traverseOf atom f a <*> traverseOf atom f n

{-
Programs  prog ::= f_1 = obj_1; ...; f_n = obj_n
-}
data Program a = Program
  { -- | トップレベル関数。topBinds以外の自由変数を持たない
    topFuncs :: [(a, ([a], Exp a))],
    mainExp :: Exp a
  }
  deriving stock (Eq, Show, Functor)

instance Pretty a => Pretty (Program a) where
  pPrint Program {mainExp, topFuncs} =
    parens ("entry" $$ pPrint mainExp)
      $$ vcat (map (\(f, (ps, e)) -> parens $ "define" <+> pPrint f <+> parens (sep $ map pPrint ps) $$ pPrint e) topFuncs)

appObj :: Traversal' (Obj a) (Exp a)
appObj = traversalVL $ \f -> \case
  Fun ps e -> Fun ps <$> f e
  o -> pure o

appCase :: Traversal' (Case a) (Exp a)
appCase = traversalVL $ \f -> \case
  Unpack con ps e -> Unpack con ps <$> f e
  Switch u e -> Switch u <$> f e
  Bind x e -> Bind x <$> f e

appProgram :: Traversal' (Program a) (Exp a)
appProgram = traversalVL $ \f Program {mainExp, topFuncs} ->
  Program <$> traverse (rtraverse (rtraverse f)) topFuncs <*> f mainExp

runDef :: Functor f => WriterT (Endo a) f a -> f a
runDef m = uncurry (flip appEndo) <$> runWriterT m

let_ ::
  (MonadUniq m, MonadWriter (Endo (Exp (Id a))) m) =>
  a ->
  Obj (Id a) ->
  m (Atom (Id a))
let_ otype obj = do
  x <- newId otype "$let"
  tell $ Endo $ \e -> Let [(x, obj)] e
  pure (Var x)

destruct ::
  (MonadUniq m, MonadWriter (Endo (Exp (Id CType))) m) =>
  Exp (Id CType) ->
  Con ->
  m [Atom (Id CType)]
destruct val con@(Con _ ts) = do
  vs <- traverse (newId ?? "$p") ts
  tell $ Endo $ \e -> Match val (Unpack con vs e :| [])
  pure $ map Var vs

bind :: (MonadUniq m, MonadWriter (Endo (Exp (Id CType))) m) => Exp (Id CType) -> m (Atom (Id CType))
bind (Atom a) = pure a
bind v = do
  x <- newId (cTypeOf v) "$d"
  tell $ Endo $ \e -> Match v (Bind x e :| [])
  pure (Var x)

cast ::
  (MonadUniq f, MonadWriter (Endo (Exp (Id CType))) f) =>
  CType ->
  Exp (Id CType) ->
  f (Atom (Id CType))
cast ty e
  | ty == cTypeOf e = bind e
  | otherwise = do
    v <- bind e
    x <- newId ty "$cast"
    tell $ Endo $ \e -> Match (Cast ty v) (Bind x e :| [])
    pure (Var x)
