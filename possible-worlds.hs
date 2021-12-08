{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}

module PossibleWorlds where

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map, (!))
import Control.Applicative
import Data.List (subsequences, nub, intercalate, elemIndex)
import Control.Monad (ap, (>=>))
import System.Process
import Data.Maybe (fromMaybe)

import Queries
import Utils

-- Boolean Algebras

class BooleanAlgebra b where
   bot         :: b
   top         :: b
   complement  :: b -> b
   wedge       :: b -> b -> b
   vee         :: b -> b -> b

-- Bool as instance of a boolean algebra

instance BooleanAlgebra Bool where
  bot        = False
  top        = True
  complement = not
  wedge      = (&&)
  vee        = (||)

truth :: Fact f => (f -> Bool) -> Q f () -> Bool
truth truthf = fold (const True) truthf

-- Pointwise boolean algebra

instance BooleanAlgebra b => BooleanAlgebra (w -> b) where
  bot = const bot
  top = const top
  complement = \x w   -> complement (x w)
  wedge      = \x y w -> x w `wedge` y w
  vee        = \x y w -> x w `vee`   y w

models :: Fact f => Q f a -> World f -> Bool
models = fold (const top) elem

-- World as instance of a boolean algebra

type World f = Set f

instance Fact f => BooleanAlgebra (Set (World f)) where
   top         = Set.fromList universe
   bot         = Set.empty
   complement  = Set.difference top
   wedge       = Set.intersection
   vee         = Set.union

possibleWorlds :: Fact f => Q f a -> Set (World f)
possibleWorlds = fold (const top) lit where
  lit f   =  Set.filter (elem f) top

universe :: Fact f => [World f]
universe = [ Set.fromList l | l <- subsequences facts ]

-- Possible Worlds in terms of boolean algebras

fold :: (Fact f, BooleanAlgebra b) => (a -> b) -> (f -> b) -> Q f a -> b
fold gen inj (Var x)           =  gen x
fold gen inj Fail              =  bot
fold gen inj (Or p q)          =  fold gen inj p `vee` fold gen inj q
fold gen inj (Lit (Pos f) p)   =  inj f `wedge` fold gen inj p
fold gen inj (Lit (Neg f) p)   =  complement (inj f) `wedge` fold gen inj p

-- Possible Worlds in terms of semirings

val :: (Fact f, Semiring r) => (Lit f -> r) -> (Set (World f) -> r)
val gen = sum .  map (prod . lits) . Set.toList where
  prod = foldr times one
  sum  = foldr plus zero
  lits s = map lit facts where
    lit f
      | Set.member f s  =  gen (Pos f)
      | otherwise       =  gen (Neg f)
