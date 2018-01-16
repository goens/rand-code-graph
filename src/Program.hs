{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Program where

import           Data.Default.Class
import           Data.Function
import           Data.List                 (intersperse)
import           Data.String
import           Data.Text.Prettyprint.Doc

type Serialized = Doc ()

newtype Symbol = Symbol { symToStr :: String } deriving IsString

type Type = ()

data Typed a = Typed Type a

instance Pretty Symbol where
  pretty = pretty . symToStr

data Program lang = Program
  { functions :: [NamedFunction lang]
  , main      :: Function lang
  }

data NamedFunction lang = NamedFunction
  { functionName          :: Symbol
  , namedFunctionFunction :: Function lang
  }

data Function = Function
  { parameters   :: [Symbol]
  , functionBody :: lang
  }

data HsDestr
  = DestrVar Symbol
  | DestrList [HsDestr]
  | DestrCon Symbol [HsDestr]
  | DestrTuple [HsDestr]

data HsExpr
  = VarE Symbol
  | ApplyE HsExpr [HsExpr]
  | LambdaE (Function HsExpr)
  | DoE HsStmt
  | LetE HsDestr HsExpr HsExpr
  | IfE HsExpr HsExpr HsExpr
  | LitE (Lit HsExpr)

data Lit lang
  = LitInt Int
  | LitStr String
  | LitBool Bool
  | LitList [lang]

data HsStmt
  = BindValue HsDestr HsExpr HsStmt
  | LetAssign HsDestr HsExpr HsStmt
  | Return HsExpr

data CljExpr
  = Form [CljExpr]
  | CljLit (Lit CljExpr)
  | CljSym Symbol
  | Keyword Symbol


instance Renderable (Program CljExpr) where
  render Program{..} = vsep $ intersperse line $ map r functions ++ [r main]
instance Renderable (NamedFunction CljExpr) where
  render NamedFunction{functionName = name, namedFunctionFunction = Function{..}} =
    parens $ "defn" <+> render name <+> brackets (hsep (map render parameters)) <> line <> indent 2 (render functionBody)
instance Renderable (Function CljExpr) where
  render Function{..} =
    align $ parens $ "fn" <+> brackets (hsep (map render parameters)) <> softline <> indent 2 (render functionBody)
instance Renderable CljExpr where
  render (Form stuff) = parens $ hsep (map render stuff)
  render (CljLit l) = render l
  render (CljSym s) = render s
  render (Keyword k) = pretty ':' <> render k
instance Renderable HsDestr where
  render 

instance Renderable (Lit CljExpr) where
  render (LitInt i) = pretty $ show i
  render (LitStr str) = pretty $ show str
  render (LitBool True) = "true"
  render (LitBool False) = "false"
  render (LitList l) = brackets $ hsep $ map r l

-- If you want to test it try running `render haskell testprog` and `render clojure testprog` in ghci
testprog =
  Program
    [ NamedFunction "f" (Function ["a", "b"] (ApplyE (VarE "plus") [VarE "a", VarE "b"]))
    ]
    (Function ["a"] (BlockE (Block (BindValue a (ApplyE (VarE getSomething) [VarE aHandle]) (Return (VarE a))))))
  where
    a = "a"
    getSomething = "getSomething"
    aHandle = "handle"
