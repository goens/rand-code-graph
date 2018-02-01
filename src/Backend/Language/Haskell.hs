{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
module Backend.Language.Haskell where


import           Backend.Language.Common
import           Data.Default.Class
import           Data.Function
import           Data.Functor.Foldable
import           Data.List                 (intersperse)
import           Data.String
import           Data.Text.Prettyprint.Doc


data DestrF a
  = VarDF Symbol
  | ListDF [a]
  | ConDF Symbol [a]
  | TupDF [a]
  deriving (Functor, Foldable, Traversable)

newtype Destr = Destr (DestrF Destr)

type instance Base Destr = DestrF

instance Recursive Destr where project (Destr a) = a
instance Corecursive Destr where embed = Destr

pattern VarD v = Destr (VarDF v)
pattern ListD l = Destr (ListDF l)
pattern ConD s v = Destr (ConDF s v)
pattern TupD v = Destr (TupDF v)

data ExprF a
  = VarEF Symbol
  | ApplyEF a a
  | LambdaEF (Function a)
  | DoEF (GStmt a)
  | LetEF Destr a a
  | IfEF a a a
  | LitEF Lit
  deriving (Functor, Traversable, Foldable)

newtype Expr = Expr (ExprF Expr)

type instance Base Expr = ExprF

instance Recursive Expr where project (Expr e) = e
instance Corecursive Expr where embed = Expr

pattern VarE s = Expr (VarEF s)
pattern ApplyE a b = Expr (ApplyEF a b)
pattern LambdaE f = Expr (LambdaEF f)
pattern DoE s = Expr (DoEF s)
pattern LetE d e b = Expr (LetEF d e b)
pattern IfE i t e = Expr (IfEF i t e)
pattern LitE l = Expr (LitEF l)


data Lit
  = LitInt Int
  | LitStr String
  | LitBool Bool
  | LitList [Expr]

data StmtF e a
  = BindSF Destr e a
  | LetSF Destr e a
  | RetSF e
  deriving (Functor, Foldable, Traversable)

newtype GStmt e = GStmt (StmtF e (GStmt e))

type Stmt = GStmt Expr

instance Functor GStmt where
  fmap f = cata $ embed . \case
    BindSF d e a -> BindSF d (f e) a
    LetSF d e a -> LetSF d (f e) a
    RetSF e -> RetSF $ f e

instance Foldable GStmt where
  foldMap f = cata $ \case
    BindSF _ e a -> f e `mappend` a
    LetSF _ e a -> f e `mappend` a
    RetSF e -> f e

instance Traversable GStmt where
  sequenceA = cata $ fmap embed . \case
    BindSF b e a -> BindSF b <$> e <*> a
    LetSF b e a -> LetSF b <$> e <*> a
    RetSF e -> RetSF <$> e

type instance Base (GStmt e) = StmtF e

instance Recursive (GStmt e) where project (GStmt s) = s
instance Corecursive (GStmt e) where embed = GStmt

pattern BindS d e s = GStmt (BindSF d e s)
pattern LetS d e s = GStmt (LetSF d e s)
pattern RetS e = GStmt (RetSF e)

renderProgram :: Program Expr -> Serialized
renderProgram Program{..} = vsep $ intersperse line
  $  map renderNamedFunction functions
  ++ [renderNamedFunction $ NamedFunction "main" main]


renderNamedFunction :: NamedFunction Expr -> Serialized
renderNamedFunction NamedFunction{functionName = name, namedFunctionFunction = Function{..}} =
  vsep
    [ hsep $ rname : "::" : intersperse "->" (map (const "Int") parameters ++ ["IO ()"])
    , hsep $ rname : rparams ++ ["=", renderExpr functionBody]
    ]
  where
    rparams = map pretty parameters
    rname = pretty name
    mkSig = intersperse "->" . map (const $ "Int")


renderFunction :: Function Expr -> Serialized
renderFunction Function{..} = "\\" <> hsep (map pretty parameters) <+> "->" <+> renderExpr functionBody


renderExpr :: Expr -> Serialized
renderExpr = renderPrec False
  where
    renderPrec _ (VarE v) = pretty v
    renderPrec _ (LitE lit) = renderLit lit
    renderPrec p other | p = parens r
                       | otherwise = r
      where
        r = case other of
              LetE var e b -> "let" <+> renderDestr var <+> "=" <+> renderExpr e <+> "in" <+> renderExpr b
              IfE b t e -> "if" <+> renderExpr b <+> "then" <+> renderExpr t <+> "else" <+> renderExpr e
              ApplyE fun arg -> renderPrec False fun <+> renderPrec True arg
              DoE b ->  "do" <> line <> indent 2 (renderStmt b)
              LambdaE l -> renderFunction l


renderLit (LitInt i)   = pretty $ show i
renderLit (LitStr str) = pretty $ show str
renderLit (LitBool b)  = pretty $ show b
renderLit (LitList l)  = brackets $ hcat $ intersperse "," $ map renderExpr l


renderStmt (BindS var val cont) = renderDestr var <+> "<-" <+> renderExpr val <> line <> renderStmt cont
renderStmt (RetS e) = renderExpr e
renderStmt (LetS var val cont) = "let" <+> renderDestr var <+> "=" <+> renderExpr val <> line <> renderStmt cont


renderDestr = renderDestrPrec False

renderDestrPrec _ (VarD s) = pretty s
renderDestrPrec _ (ListD l) = brackets $ hcat $ intersperse "," $ map (renderDestrPrec False) l
renderDestrPrec _ (TupD l) = parens $ hcat $ intersperse "," $ map (renderDestrPrec False) l
renderDestrPrec p (ConD con fields) | p = parens r
                                    | otherwise = r
  where
    r = hsep $ pretty con : map (renderDestrPrec True) fields
