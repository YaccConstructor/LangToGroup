# LangToGroup

This project provides an opportunity to build a grammar group representation in two ways, which is displayed in the following picture:
![](media/approaches.png)



## First approach
Implementation of [Isoperimetric and Isodiametric Functions of Groups](https://arxiv.org/abs/math/9811105) in Haskell.

Implementation algorithm building the presentation of the group for a formal language with the preservation of the language. The implementation of this algorithm is necessary for research in the field of formal languages with the involvement of the group-theoretical methods. At present, there are only theoretical works showing the possibility of such construction, however, the algorithm and its implementation are absent up to this point. As far as we know, this implementation is the first in its area.  

## Second approach
Implementation of building Turing machine by boolean grammar algorithm, described in [Boolean grammars](https://doi.org/10.1016/j.ic.2004.03.006) and algorithm, descrbied in [An Introduction to the Theory of Groups](https://doi.org/10.1007/978-1-4612-4176-8), which by given Turing machine builds group presentation via building semigroup presentation.

## Modifications of second approach
There are two modifications of second approach, which produce a more compact
group presentation, but this modifications haven't strict theoretical proof of
saving properties of original approach. So, use them if size of group
presentation is more important than theoretical properties.

## Build status
[![Build Status](https://travis-ci.org/YaccConstructor/LangToGroup.svg?branch=master)](https://travis-ci.org/YaccConstructor/LangToGroup)

## Building 
To build run:

``stack build``

## Testing
For run the tests:

``stack test``

## Usage
```bash
stack run -- LangToGroup-user <options>
```

```
Options:
    -i, --input <file_path>
        Full path to file with grammar definition
    -o, --output <file_path>
        Full path to file for printing results
    -e, --error <file_path>
        Full path to file, where errors should be recorded during parsing
    -f, --first-approach
        Use first approach
    -s, --second-approach
        Use second approach
    -a, --second-approach-a
        Use second approach, modification (a)
    -b, --second-approach-b
        Use second approach, modification (b)
    -q, --quiet
        Print only information about result
    -h, --help
        Print help information and exit
```

## Experiments
Here are the tables with some examples of building group presentations by different grammars, where:
- **States** -- number of states in built Turing machine, 
- **Gen** -- number of generators, 
- **Rel** -- number of relations, 
- **N** -- number of rules in normal grammar.

### Running experiments for the first approach

| Language | Grammar | N | States | Generators | Relations | 
| -- | -- |-- | -- | -- | --|
| 1 rule | CFG |1 | 4 | 56187 |  89508 | 
| <img src="https://render.githubusercontent.com/render/math?math=\{a^{*} \}"> | CFG |3 | 9 | 204903 | 347370 |
| Dyck | CFG |6 | 19 | 957619 | 1478859 |


### Running experiments for the second approach

| Language | Grammar | N | States | Generators | Relations | 
|----------|---------|---|------------|-----------| --|
| 1 rule | CFG | 1 | -- | -- | -- |
| <img src="https://render.githubusercontent.com/render/math?math=\{a^{*} \}"> | CFG |3 |  | - | - |
| Dyck | CFG | - | - | - | - |
| <img src="https://render.githubusercontent.com/render/math?math=\{a^{n} \mid n \in N \}"> | CFG| 2 | 221 |  7384 | 149982 | 
| <img src="https://render.githubusercontent.com/render/math?math=\{a^{n} b^{n} \mid n \in N \}"> | CFG| 5 | 719 |  29510 | 719150 | 
| <img src="https://render.githubusercontent.com/render/math?math=\{a^{n} b^{n} c^{n} \mid n \in N \}"> | Conjunctive | 14 | 3459 |  179112 | 5619872 | 
| <img src="https://render.githubusercontent.com/render/math?math=\{ww \mid w \in \{a,b\}*\}">| Boolean| 14 | 2498 | 125587 | 3691770 |
| <img src="https://render.githubusercontent.com/render/math?math=\{a^{m} b^{n} c^{n} \mid (m != n), m, n \in N\}"> | Boolean | 14 | 3461 |185307 | 5999829| 


## Execution 

For run experiments and print its numerical results you can use ``is_det <bool>`` flag.

So, for print experiments' results using deterministic symmetrization:

``stack exec -- LangToGroup-printer --is_det true``

using nondeterministic symmetrization:

``stack exec -- LangToGroup-printer --is_det false``


### Printing example transformations in LaTeX
Grammar's transformations also can be printed in LaTeX. For this should be used ``--print_example <grammar>`` flag.
The following grammars can be used as print examples: "one\" --- one rule grammar, \"a*\" --- grammar for regular language 
<img src="https://render.githubusercontent.com/render/math?math=L = \{a*\}">
, \"dyck\" --- Dyck language grammar. 

For example,

``stack exec -- LangToGroup-printer --is_det true --print_example one``

Output filename can be specified by ``-o <filename>`` flag.

### Printing example group presentation in Gap-format file
For this you can use ``-G`` flag without a parameter, but with ``--is_det <bool>`` and ``--print_example <grammar>`` flags.
Also, output filename can be specified by ``-o <filename>`` flag, if it does not speccified it been printing with default filename "out.txt".

For example, following prints in "out.txt" a group presentation, which obtained from Dyck language grammar using nondeterministic symmetrization: 

``stack exec -- LangToGroup-printer -G --is_det false --print_example dyck``

### Building group presentation by custom grammar file
For this you should use ``-i <filepath>`` flag for configuring full path to input file with grammar and ``-o <filepath>`` for configuring full path to output file for storing possible errors.
So, for printing turing machine from given custom grammar file:

``stack exec -- stack exec -- LangToGroup-user -i <filepath> -o <filepath>``

Examples of grammar files given below.

**Boolean grammar**

    S; S Sa; c v b
    S-> c&! v&! Sa&! Eps
    Sa->! b
    S-> a& b&! v&! Sa&! Eps
**Conjunctive grammar**

    S; S Abc D Cr; c b d e
    S-> D c& d Abc
    Abc-> b
    D-> Cr
    Cr-> e
**Context-free grammar**

    S; S A D1; c2 b e
    S-> c2 D1 A
    A-> b
    D1-> e


    
