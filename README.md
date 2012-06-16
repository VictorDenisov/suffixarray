suffixarray
===========

Effective implementation of suffix arrays for Haskell.
Current implementation is not optimal. It works n log(n), but the way it
produces collections during calculation makes GC slow. 
