{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}

module Normalization where

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map, (!))
import Control.Applicative
import Data.List (subsequences, nub, intercalate, elemIndex)
import Control.Monad (ap)
import System.Process
import Data.Maybe (fromMaybe)

import Queries
import Utils

-- CNF (Conjunctive normal form)

type CNF f a = [[Vars f a]]
data Vars f a = PosVar f | NegVar f | Val a

instance Show f => Show (Vars f a) where
  show (PosVar f) = show f
  show (NegVar f) = "-" ++ show f


-- transform a query to CNF

cnf :: Q f a -> CNF f a
cnf (Var x)   = [[Val x]]
cnf Fail      = [[]]
cnf (Or p q)  = orCNF (cnf p) (cnf q)
cnf (Lit l p) = andCNF (litCNF l) (cnf p)

-- helper functions

andCNF :: CNF f a -> CNF f a -> CNF f a
andCNF = (++)

orCNF :: CNF f a -> CNF f a -> CNF f a
orCNF cnf1 cnf2 = (++) <$> cnf1 <*> cnf2

litCNF :: Lit f -> CNF f a
litCNF (Pos l) = [[(PosVar l)]]
litCNF (Neg l) = [[(NegVar l)]]

-- convert to .cnf file

toCNF :: (Fact f, Eq a) => CNF f a -> Facts f a -> String
toCNF lst fcts = "p cnf " ++  (show $ length fcts) ++ " " ++ (show $ length lst) ++ "\n"
                    ++ intercalate "\n" (map (toCNFLine fcts) lst)

type Facts f a = [Either (Lit f) a]

toCNFVar :: (Fact f, Eq a) => Facts f a -> Vars f a -> String
toCNFVar fcts (PosVar l)  = case elemIndex (Left (Pos l)) fcts of
  Just i -> show (i + 1)
toCNFVar fcts (NegVar l) = case elemIndex (Left (Neg l)) fcts of
  Just i -> "-" ++ show (i + 1)
toCNFVar fcts (Val x) = case elemIndex (Right x) fcts of
  Just i -> show (i + 1)

toCNFLine :: (Fact f, Eq a) => Facts f a -> [Vars f a] -> String
toCNFLine fcts lst = intercalate " " (map (toCNFVar fcts) lst) ++ " 0"


-- d-DNNF (deterministic, decomposable negation normal form)

fromNNF :: (Semiring r) => [[String]] -> Facts f a -> (Lit f -> r) -> r
fromNNF str fcts inj = go Map.empty str fcts inj
  where
    go :: (Semiring r) => Map Int r -> [[String]] -> Facts f a -> (Lit f -> r) -> r
    -- base cases
    go nds []    _    inj = fromMaybe zero (Map.lookup 1 nds)
    go nds [arc] fcts inj = query
      where
        start = nds ! (read $ head arc)
        end   = nds ! (read $ head $ tail arc)
        lits  = parseLits (map read (tail $ tail arc)) fcts inj
        query = foldr times end lits
    -- parse nodes
    go nds (("o":i:_):lns) fcts inj = go (Map.insert (read i) zero nds) lns fcts inj  -- placeholder
    go nds (("t":i:_):lns) fcts inj = go (Map.insert (read i) one  nds) lns fcts inj
    go nds (("f":i:_):lns) fcts inj = go (Map.insert (read i) zero nds) lns fcts inj
    -- parse composite arcs
    go nds (arc1:arc2:lns) fcts inj | head arc1 == head arc2 = go (Map.insert i query nds) lns fcts inj
      where
        i     = read $ head arc1
        start = nds ! i
        end   = \arc -> nds ! (read $ head $ tail arc)
        lits  = \arc -> parseLits (map read (tail $ tail arc)) fcts inj
        q1    = foldr times (end arc1) (lits arc1)
        q2    = foldr times (end arc2) (lits arc2)
        query = q1 `plus` q2

parseLits :: (Semiring r) => [Int] -> Facts f a -> (Lit f -> r) -> [r]
parseLits []     _    inj           = []
parseLits (x:xs) fcts inj  | x == 0 = []
                           | x <  0 = case fcts !! (-x - 1) of
                              Left l  -> inj l : parseLits xs fcts inj
                              Right x -> zero  : parseLits xs fcts inj
                           | x >  0 = case fcts !! (x  - 1) of
                              Left l  -> inj l : parseLits xs fcts inj
                              Right x -> one   : parseLits xs fcts inj

-- normalizing using the d4 compiler

normalize :: (Fact f, Semiring r, Eq a) => (Lit f -> r) -> Q f a -> IO r
normalize inj x = d4 (cnf x) inj fcts
  where fcts = uniqueFacts (cnf x)

uniqueFacts :: (Fact f, Eq a) => [[Vars f a]] -> Facts f a
uniqueFacts = nub . go . concat
  where
    go [] = []
    go (PosVar l:lst) = Left (Pos l) : go lst
    go (NegVar l:lst) = Left (Neg l) : go lst
    go (Val x   :lst) = Right x      : go lst

d4 :: (Fact f, Semiring r, Eq a) => CNF f a -> (Lit f -> r) -> Facts f a -> IO r
d4 lst inj fcts = do
  writeFile "example.cnf" (toCNF lst fcts)
  readProcess "./d4/d4" ["-dDNNF", "example.cnf", "-out=example.nnf"] ""
  str <- readFile "example.nnf"
  return $ fromNNF (map words (lines str)) fcts inj
