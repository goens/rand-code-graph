{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
module Backend.Language.Clojure where


import           Backend.Language.Common
import           Data.Default.Class
import           Data.Functor.Foldable     hiding (Nil)
import           Data.List                 (intersperse)
import           Data.List.Split           (chunksOf)
import           Data.Maybe                (fromMaybe)
import           Data.String
import           Data.Text.Prettyprint.Doc


data ExprF a
  = FormF [a]
  | SymF Symbol
  | IntF Int
  | StrF String
  | BoolF Bool
  | VectF [a]
  | ListF [a]
  | NilF
  | KeywordF Symbol
  deriving (Functor, Traversable, Foldable)

newtype Expr = Expr (ExprF Expr)

pattern Form a = Expr (FormF a)
pattern Sym s = Expr (SymF s)
pattern Int i = Expr (IntF i)
pattern Str s = Expr (StrF s)
pattern Bool b = Expr (BoolF b)
pattern Vect v = Expr (VectF v)
pattern List l = Expr (ListF l)
pattern Nil = Expr NilF
pattern Keyword k = Expr (KeywordF k)

type instance Base Expr = ExprF

instance Recursive Expr where
  project (Expr e) = e

instance Corecursive Expr where
  embed = Expr

data RenderOpts = RenderOpts
  { defnSym :: Symbol
  , fnSym   :: Symbol
  }

instance Default RenderOpts where
  def = RenderOpts defnSym' "fn"


letSym :: Symbol
letSym = "let"

defnSym' :: Symbol
defnSym' = "defn"

renderProgram :: RenderOpts -> Program Expr -> Serialized
renderProgram opts Program{..} = vsep $ intersperse line $ map (renderNamedFunction opts) functions ++ [renderFunction opts main]


renderNamedFunction :: RenderOpts -> NamedFunction Expr -> Serialized
renderNamedFunction opts NamedFunction{functionName = name, namedFunctionFunction = Function{..}} =
  parens $ pretty (defnSym opts) <+> pretty name <+> brackets (hsep (map pretty parameters)) <> line <> indent 2 (renderExpr functionBody)

renderFunction :: RenderOpts -> Function Expr -> Serialized
renderFunction opts Function{..} =
  align $ parens $ pretty (fnSym opts) <+> brackets (hsep (map pretty parameters)) <> softline <> indent 2 (renderExpr functionBody)

renderExpr :: Expr -> Serialized
renderExpr = para worker
  where
    worker = \case
      FormF ((Sym s, _):(Vect bndvec, _):body)
        | s == letSym ->
          parens $ vsep [ pretty letSym <+> brackets (align $ vsep $ map hsep $ chunksOf 2 $ map (align . renderExpr) bndvec)
                        , indent 2 $ vsep $ map snd body
                        ]
      FormF ((Sym s, _):(Sym name, _):(Vect _, bndVec):body)
        | s == defnSym' ->
          parens $ vsep [ pretty defnSym' <+> pretty name <+> bndVec
                        , indent 2 $ vsep $ map snd body
                        ]
      FormF stuff -> parens $ hsep (map snd stuff)
      SymF s      -> pretty s
      KeywordF k  -> pretty ':' <> pretty k
      IntF i      -> pretty $ show i
      StrF str    -> pretty $ show str
      BoolF True  -> "true"
      BoolF False -> "false"
      VectF l     -> brackets $ hsep $ map snd l
      ListF l     -> renderExpr $ Form [Sym "quote", Form $ map fst l]
      NilF        -> "nil"


mkLet :: [(Expr, Expr)] -> [Expr] -> Expr
mkLet = mkLetWith letSym


mkLetWith :: Symbol -> [(Expr, Expr)] -> [Expr] -> Expr
mkLetWith l assigns body = Form $
  [ Sym l
  , Vect $ assigns >>= \(a, b) -> [a, b]
  ] ++ body

mkIf :: Expr -> Expr -> Maybe Expr -> Expr
mkIf cond th el = Form $
  [ Sym "if"
  , cond
  , th
  , fromMaybe Nil el
  ]
