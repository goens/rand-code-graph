{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
module Program where

import           Data.List                 (intersperse)
import           Data.String
import           Data.Text.Prettyprint.Doc

newtype Symbol = Symbol { symToStr :: String } deriving IsString

instance Pretty Symbol where
  pretty = pretty . symToStr

data Program = Program
  { functions :: [(Symbol, Function)]
  , main      :: Function
  }

data Function = Function
  { parameters   :: [Symbol]
  , functionBody :: Expression
  }

data Expression
  = VarE Symbol
  | ApplyE Expression [Expression]
  | LambdaE Function
  | BlockE Block
  | LetE Symbol Expression Expression
  | ConditionalE Expression Expression Expression

data Block = Block
  { statements       :: [Statement]
  , returnExpression :: Expression
  }

data Statement
  = BindValue Symbol Block



data Renderer a = Renderer
  { renderProgram :: [Doc a] -> Doc a -> Doc a
  , renderFunction :: Doc a -> [Doc a] -> Doc a -> Doc a
  , renderLambda :: [Doc a] -> Doc a -> Doc a
  , renderVar :: Doc a -> Doc a
  , renderApply :: Doc a -> [Doc a] -> Doc a
  , renderBlockExpression :: Doc a -> Doc a -- how to render a block when used in a bare expression
  , renderBlock :: [Doc a] -> Doc a -> Doc a
  , renderLet :: Doc a -> Doc a -> Doc a -> Doc a
  , renderConditional :: Doc a -> Doc a -> Doc a -> Doc a
  , renderSymbol :: Symbol -> Doc a
  , renderBindValue :: Doc a -> Doc a -> Doc a
  }

haskell :: Renderer a
haskell = Renderer
  { renderProgram = \funs main -> vsep $ intersperse line $ funs ++ [main]
  , renderFunction = \name params body ->
      vsep
        [ name <+> "::" <+> mkSig params
        , name <+> hsep params <+> "=" <+> body
        ]
  , renderLambda = \params body -> "\\" <> hsep params <+> "->" <+> body
  , renderVar = id
  , renderSymbol = pretty
  , renderBlockExpression = parens
  , renderBlock = \stmts ret -> "do" <> line <> indent 2 (vsep $ stmts ++ ["pure" <+> ret])
  , renderLet = \var e r -> "let" <+> var <+> "=" <+> e <+> "in" <+> r
  , renderConditional = \b t e -> "if" <+> b <+> "then" <+> t <+> "else" <+> e
  , renderBindValue = \var val -> var <+> "<-" <+> val
  , renderApply = \fun args -> fun <+> hsep args
  }
  where
    mkSig = hsep . intersperse "->" . map (const $ "Int")


clojure :: Renderer a
clojure = Renderer
  { renderProgram = \funs main -> vsep $ intersperse line $ funs ++ [main]
  , renderFunction = \name params body ->
      parens $ "defn" <+> name <+> toArgList params <> line <> indent 2 body
  , renderLambda = \params body -> parens $ "fn" <+> toArgList params <+> body
  , renderVar = id
  , renderSymbol = pretty
  , renderBlockExpression = id
  , renderBlock = \stmts ret -> parens $ "let" <+> brackets (align (vsep stmts)) <> line <> indent 2 ret
  , renderLet = \var e r -> parens $ "let" <+> brackets (var <+> align e) <> line <> indent 2 r
  , renderConditional = \b t e -> parens $ "if" <+> b <+> t <+> e
  , renderBindValue = \var val -> var <+> align val
  , renderApply = \fun args -> parens $ hsep (fun:args)
  }
  where
    toArgList = brackets . hsep


render :: Renderer a -> Program -> Doc a
render Renderer{..} Program{..} = renderProgram (map (uncurry renderFun) functions) $ renderFun mainFunName main
  where
    mainFunName = Symbol "main"
    renderFun name Function{..} = renderFunction (renderSymbol name) (map renderSymbol parameters) (renderExpression functionBody)
    renderExpression (VarE e) = renderVar $ renderSymbol e
    renderExpression (ApplyE fun params) = renderApply (renderExpression fun) (map renderExpression params)
    renderExpression (LambdaE Function{..}) = renderLambda (map renderSymbol parameters) (renderExpression functionBody)
    renderExpression (BlockE b) = renderBlockExpression $ renderBlockV b
    renderExpression (LetE sym val body) = renderLet (renderSymbol sym) (renderExpression val) (renderExpression body)
    renderExpression (ConditionalE i t e) = renderConditional (renderExpression i) (renderExpression t) (renderExpression e)

    renderBlockV Block{..} = renderBlock (map renderStatement statements) $ renderExpression returnExpression

    renderStatement (BindValue sym b) = renderBindValue (renderSymbol sym) (renderBlockV b)


-- If you want to test it try running `render haskell testprog` and `render clojure testprog` in ghci
testprog =
  Program
    [ ("f", Function ["a", "b"] (ApplyE (VarE "plus") [VarE "a", VarE "b"])) 
    ]
    (Function [] (BlockE (Block [BindValue a (Block [] $ ApplyE (VarE getSomething) [VarE aHandle])] (VarE a))))
  where
    a = "a"
    getSomething = "getSomething"
    aHandle = "handle"
