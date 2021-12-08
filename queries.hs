{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}

module Queries where

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map, (!))
import Control.Applicative
import Data.List (subsequences, nub, intercalate, elemIndex)
import Control.Monad (ap, (>=>))
import System.Process
import Data.Maybe (fromMaybe)

import Utils

-- Queries

data Q f a where
   Var    :: a -> Q f a
   Fail   :: Q f a
   Or     :: Q f a -> Q f a -> Q f a
   Lit    :: Lit f -> Q f a -> Q f a
   deriving (Show, Eq, Functor)

-- Applicative, Alternative an Monad instances

instance Applicative (Q f) where
   pure   =  Var
   (<*>)  =  ap

instance Alternative (Q f) where
 empty = Fail
 (<|>) = Or

instance Monad (Q f) where
   return  =  Var
   Var x       >>=  k  =  k x
   Fail        >>=  k  =  Fail
   Or p q      >>=  k  =  Or (p >>= k) (q >>= k)
   Lit c p     >>=  k  =  Lit c (p >>= k)

-- smart constructors

true :: Q f ()
true = return ()

false :: Q f ()
false = empty

(/\) :: Q f a -> Q f a -> Q f a
(/\) = (>>)

(\/) :: Q f a -> Q f a -> Q f a
(\/) = (<|>)

lit :: Lit f -> Q f ()
lit f = Lit f (Var ())

-------------
-- Example --
-------------

-- Facts

data FGmb = Heads | Red1 | Red2 deriving (Eq, Ord, Show)

instance Fact FGmb where
  facts = [Heads, Red1, Red2]

win :: Q FGmb ()
win = (heads /\ (red1 \/ red2)) \/ ((red1 /\ red2) \/ (blue1 /\ blue2))
  where heads  =  lit  (Pos  Heads)
        red1   =  lit  (Pos  Red1)
        blue1  =  lit  (Neg  Red1)
        red2   =  lit  (Pos  Red2)
        blue2  =  lit  (Neg  Red2)

-- Probabilities

gmbProb :: Lit FGmb -> Prob
gmbProb (Pos f) = go f where
  go Heads  =  0.4
  go Red1   =  0.3
  go Red2   =  0.2
gmbProb (Neg f) = 1 - gmbProb (Pos f)
