# A Functional Account of Probabilistic Programming with Possible Worlds

This repository is based on our paper "A Functional Account of
Probabilistic Programming with Possible Worlds".

The file `utils.hs` contains the generic data types for commutative
semirings, probabilities, literals and facts.

The file `queries.hs` shows the term monad construction for queries,
together with an example.

The file `possible-worls.hs` contains the generic class of boolean
algebras and several example instances of those, e.g. `Bool`, the
pointwise boolean algebra, worlds.
It defines an interpretation of possible worlds in terms of boolean
algebras and in terms of commutative semirings.
```
> val gmbProb (possibleWorlds win)
0.7720001
```
The file `normalization.hs` shows how to interpret a query using
normalization. We use the d4 compiler that transforms a query in CNF
(Conjunctive Normal Form) into d-DNNF (deterministic, Decomposable
  Negation Normal Form).
For example,
```
> normalize gmbProb win
0.772
```
The file `circular-queries.hs` contains an extension of the data
type for queries that also supports circular constructions, together
with a least fixed point interpretation.
```
> vald4 smkProb (smokes Alice)
0.34200004
```



Compile and use the code in the files using
```
ghci -f "filename.hs"
```
