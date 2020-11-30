# Heuristics for Genome Rearrangement with Replicated Genes

A collection of heuristics to find rearrangement distances between genomes with multiple genes.

## Dependencies

- [ghc](https://www.haskell.org/ghc/)
- [cabal](https://www.haskell.org/cabal/)
- [stack](https://docs.haskellstack.org/en/stable/README/)

## Execution

To build the aplication just run the folowing command on the root:
#+begin_src sh
stack build
#+end_src

This aplication has 3 diferent executables:
- HeurGRDG: the heuristics to solve rearrangement programs
- MCSP: algorithms for the MCSP (Minimmum Common String Partition) problem, and some variations
- DB: generator of instances

To see the options for each of these executables run:
```bash
stack exec HeurGRDG -- -h
stack exec DB -- -h
stack exec MCSP -- -h
```

## Distance Between Permutations

The heuristics depend upon a black box to calculate the distances between permutations.

As default, we added our own implementation of some know algorithms from literature:

For a custom algorithm two methods are available. The simplest one is provide a custom executables with the `-p` flag. That executable should read permutations from the standard input (the other permutation is the identity) and print distances in the standard output. In the end of execution, a character `%` will be printed in the standard input.

The more efficient approach is to provide your own implementation for the `perm_rearrange.h`. To use that option, just pass the flag `-c` with a value bigger than 0.
