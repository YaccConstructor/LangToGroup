# LangToGroup
Implementation of [Isoperimetric and Isodiametric Functions of Groups](https://arxiv.org/abs/math/9811105) in Haskell

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
