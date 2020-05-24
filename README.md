# LangToGroup
Implementation of [Isoperimetric and Isodiametric Functions of Groups](https://arxiv.org/abs/math/9811105) in Haskell.

Implementation algorithm building the presentation of the group for a formal language with the preservation of the language. The implementation of this algorithm is necessary for research in the field of formal languages with the involvement of the group-theoretical methods. At present, there are only theoretical works showing the possibility of such construction, however, the algorithm and its implementation are absent up to this point. As far as we know, this implementation is the first in its area.  

## Build status
[![Build Status](https://travis-ci.org/YaccConstructor/LangToGroup.svg?branch=master)](https://travis-ci.org/YaccConstructor/LangToGroup)

## Building 
For build run:

``stack build``

## Testing
For test run:

``stack test``

## Runing experiments
For run experiments with deterministic symmetrization:

``stack exec -- LangToGroup-printer --is_det true``

with nondeterministic symmetrization:

``stack exec -- LangToGroup-printer --is_det false``
