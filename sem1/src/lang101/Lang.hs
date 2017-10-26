{-# LANGUAGE TupleSections #-}

module Main where

import Data.Maybe

type Name = String

data Simple = Const Name
            | Var Name
            | Any
            deriving (Eq,Show,Read)

data PrologTerm = Sim Simple
                | Relation Name [Simple]
                | Implies Name [Name] [PrologTerm]
                deriving (Eq,Show,Read)

type Prolog = [PrologTerm]

data Subst = Subst { sVar :: Name
                   , sValue :: Name
                   } deriving (Eq,Show,Read)

toIdent :: Fact -> Maybe Simple
toIdent (Ident s) = Just s
toIdent _ = Nothing


eval :: Prolog -> (Name,[Simple]) -> Maybe [Subst]
eval knowledge (relName, relArgs) = let
  mRelation = findRelation knowledge relName relArgs
  mImplies = findImplication knowledge relName relArgs
  in case mRelation of
       Just relSubst -> Just relSubst
       Nothing -> mImplies


test101 :: Prolog
test101 =
  [ Relation "human" [Const "sokrat"]
  , Relation "human" [Const "platon"]
  , Implies "mortal" ["Someone"]
    [ Relation "human" [Var "Someone"]
    ]
  ]


main = return ()

