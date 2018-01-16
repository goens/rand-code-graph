{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards, FlexibleContexts   #-}
module Backend.Language.Haskell where


import           Backend.Language.Common
import           Data.Default.Class
import           Data.Function
import           Data.List                 (intersperse)
import           Data.String
import           Data.Text.Prettyprint.Doc


data Destr
  = VarD Symbol
  | ListD [Destr]
  | ConD Symbol [Destr]
  | TupD [Destr]

data Expr
  = VarE Symbol
  | ApplyE Expr Expr
  | LambdaE (Function Expr)
  | DoE Stmt
  | LetE Destr Expr Expr
  | IfE Expr Expr Expr
  | LitE Lit

data Lit
  = LitInt Int
  | LitStr String
  | LitBool Bool
  | LitList [Expr]

data Stmt
  = BindS Destr Expr Stmt
  | LetS Destr Expr Stmt
  | RetS Expr



renderProgram :: Program Expr -> Serialized
renderProgram Program{..} = vsep $ intersperse line
  $  map renderNamedFunction functions
  ++ [renderNamedFunction $ NamedFunction "main" main]


renderNamedFunction :: NamedFunction Expr -> Serialized
renderNamedFunction NamedFunction{functionName = name, namedFunctionFunction = Function{..}} =
  vsep
    [ rname <+> "::" <+> mkSig rparams
    , rname  <+> hsep rparams <+> "=" <+> renderExpr functionBody
    ]
  where
    rparams = map pretty parameters
    rname = pretty name
    mkSig = hsep . intersperse "->" . map (const $ "Int")


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
