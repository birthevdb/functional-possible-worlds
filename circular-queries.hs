{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}

module CircularQueries where

import qualified Data.Set as Set
import Data.Set (Set)
import Control.Applicative
import Control.Monad (ap)

import Utils

-------------
-- Queries --
-------------

-- Extension of the Query data type with constructor for recursive queries.

data Q i o f a where
  Var   :: a -> Q i o f a
  Fail  :: Q i o f a
  Or    :: Q i o f a -> Q i o f a -> Q i o f a
  Lit   :: Lit f -> Q i o f a -> Q i o f a
  Rec   :: i -> (o -> Q i o f a) -> Q i o f a
  deriving (Functor)

-- Applicative, Alternative an Monad instances

instance Applicative (Q i o f) where
   pure   =  Var
   (<*>)  =  ap

instance Alternative (Q i o f) where
   empty = Fail
   (<|>) = Or

instance Monad (Q i o f) where
   return  =  Var
   Var x       >>=  k  =  k x
   Fail        >>=  k  =  Fail
   Or p q      >>=  k  =  Or (p >>= k) (q >>= k)
   Lit c p     >>=  k  =  Lit c (p >>= k)
   Rec i p     >>=  k  =  Rec i ((>>= k) . p)

-- smart constructors

rec :: i -> Q i o f o
rec i = Rec i Var

lit :: Lit f -> Q i o f ()
lit f = Lit f (Var ())

(/\) :: Q i o f o -> Q i o f o -> Q i o f o
(/\) = (>>)

(\/) :: Q i o f o -> Q i o f o -> Q i o f o
(\/) = (<|>)

-----------------------
-- Least fixed point --
-----------------------

-- fixed point

data Void

fix :: Ord i => ((i -> Q i o f o) -> (i -> Q i o f o)) -> i -> Q Void Void f o
fix f x = go (f rec x) (Set.singleton x) where
  go (Var x) h   = Var x
  go  Fail h     = Fail
  go (Or p q) h  = Or (go p h) (go q h)
  go (Lit l p) h = Lit l (go p h)
  go (Rec i p) h | Set.member i h = Fail
                 | otherwise = go (f rec i >>= p) (Set.insert i h)

-------------
-- Example --
-------------

-- People

data Person = Alice | Bob deriving (Eq,Ord)
person = [Alice, Bob]

-- Facts

data FSS = Stress Person | Influences Person Person deriving (Eq, Ord)

instance Fact FSS where
  facts = [Stress p | p <- person] ++ [Influences p1 p2 | p1 <- person, p2 <- person]

-- smart constructors

stress :: Person -> Q i o FSS ()
stress p = Lit (Pos (Stress p)) (Var ())

influences :: Person -> Person -> Q i o FSS ()
influences p1 p2 = Lit (Pos (Influences p1 p2)) (Var ())

-- auxiliary function

exists :: [a] -> (a -> Q i o f b) -> Q i o f b
exists l f = foldr Or Fail (map f l)

-- smokers

smokesF :: (Person -> Q i () FSS ()) -> Person -> Q i () FSS ()
smokesF smk p = stress p \/ exists person (\q -> influences q p /\ smk q)

smokes :: Person -> Q Void Void FSS ()
smokes = fix smokesF

-- Probabilities

smkProb :: Lit FSS -> Prob
smkProb (Pos f) = go f where
  go (Stress p) = 0.3
  go (Influences x y) = 0.2
smkProb (Neg f) = 1 - smkProb (Pos f)
