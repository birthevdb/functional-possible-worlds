{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}

module Utils where

-- Commutative Semirings

class Semiring r where
  zero   :: r
  one    :: r
  plus   :: r -> r -> r
  times  :: r -> r -> r

srsum :: Semiring r => [r] -> r
srsum = foldr plus zero

srprod :: Semiring r => [r] -> r
srprod = foldr times one

-- Probabilities

newtype Prob = P Float deriving (Eq, Ord, Num, Fractional)

instance Show Prob where
   show (P f) = show f

instance Semiring Prob where
   zero   =  0
   one    =  1
   plus   =  (+)
   times  =  (*)

-- Literals

data Lit f = Pos f | Neg f deriving (Eq, Ord, Show)

-- Facts

class Ord f => Fact f where
   facts :: [f]
