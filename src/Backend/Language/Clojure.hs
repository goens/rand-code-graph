{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Backend.Language.Clojure where


import           Backend.Language.Common
import           Data.Default.Class
import           Data.List                 (intersperse)
import           Data.String
import           Data.Text.Prettyprint.Doc
import Data.Maybe (fromMaybe)


data Expr
  = Form [Expr]
  | Sym Symbol
  | Int Int
  | Str String
  | Bool Bool
  | Vect [Expr]
  | List [Expr]
  | Nil
  | Keyword Symbol


data RenderOpts = RenderOpts
  { defnSym :: Symbol
  , fnSym   :: Symbol
  }

instance Default RenderOpts where
  def = RenderOpts "defn" "fn"

renderProgram :: RenderOpts -> Program Expr -> Serialized
renderProgram opts Program{..} = vsep $ intersperse line $ map (renderNamedFunction opts) functions ++ [renderFunction opts main]


renderNamedFunction :: RenderOpts -> NamedFunction Expr -> Serialized
renderNamedFunction opts NamedFunction{functionName = name, namedFunctionFunction = Function{..}} =
  parens $ pretty (defnSym opts) <+> pretty name <+> brackets (hsep (map pretty parameters)) <> line <> indent 2 (renderExpr functionBody)

renderFunction :: RenderOpts -> Function Expr -> Serialized
renderFunction opts Function{..} =
  align $ parens $ pretty (fnSym opts) <+> brackets (hsep (map pretty parameters)) <> softline <> indent 2 (renderExpr functionBody)

renderExpr :: Expr -> Serialized
renderExpr (Form stuff) = parens $ hsep (map renderExpr stuff)
renderExpr (Sym s)      = pretty s
renderExpr (Keyword k)  = pretty ':' <> pretty k
renderExpr (Int i)      = pretty $ show i
renderExpr (Str str)    = pretty $ show str
renderExpr (Bool True)  = "true"
renderExpr (Bool False) = "false"
renderExpr (Vect l)     = brackets $ hsep $ map renderExpr l
renderExpr (List l)     = renderExpr $ Form [Sym "quote", Form l]
renderExpr Nil          = "nil"


mkLet :: [(Expr, Expr)] -> [Expr] -> Expr
mkLet = mkLetWith "let"


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
