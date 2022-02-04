---
title: 'LangToGroup: A Haskell package for group presentation construction from formal language'
tags:
  - Haskell
  - formal languages
  - group theory
  - Turing machines
  - group presentation construction
authors:
  - name: Maksym Shamrai
    affiliation: 3
  - name: Sergey Kuzivanov
    affiliation: 1
  - name: Ekaterina Vinnik
    affiliation: 1
  - name: Marin Gleb
    affiliation: 4
  - name: Semyon Grigorev
    affiliation: "1, 2"
affiliations:
 - name: Saint Petersburg State University
   index: 1
 - name: JetBrains Research
   index: 2
 - name: Kyiv Academic University
   index: 3
 - name: National Research University Higher School of Economics
   index: 4
date: ?? November 2021
bibliography: paper.bib
---


# Summary


The formal language analysis approach has begun new development in the 50s of the last century [@chomsky1; @chomsky2]. During this time, it found application not only in areas directly related to programming but, among other things, in the analysis of graph data models [@Hellings2015QueryingFP; @Azimov], which are used, for example, in bioinformatics [@Huber2007] and social networks [@scott1988social]. Development entails new problems and challenges. Several of them are associated with the advent of new language classes [@ConjGrammars; @BoolGrammars], but not restricted with that. 

Sometimes it happens that researchers lack the traditional methods of the field and come up with new ones. It seems like the most obvious way to invent a new approach is to connect your field with another field of study which already well known and try to apply methods of that field. So, Sapir, Birget, and Rips did like that [@Sapir]. They linked group theory and computational complexity theory by constructing a group presentation from a Turing machine. And had proved that group presentation can be treated like a computational model. Thus, they presented a new approach for representing computation, and accordingly, this can be used to take a fresh look at open problems.

Inspired by this, we wondered what if we treat formal language as a group presentation? This will allow us to use various group theoretical methods for formal languages. Moreover, it allows experimenting with computational group theory algorithms [@GAP4]. But construction by hand like that takes a lot of time and paper. So it requires an efficient tool for automatic group presentation construction from formal language.


# Statement of need


Modern research methods should not be limited to the traditional framework but should use all possible means to achieve a breakthrough result. The LangToGroup provides new opportunities for researchers in research related to computational models. Accordingly, the tool adds ways to analyze a problem, and sometimes understanding is a key that can lead to some insights about the task.

Mainly LangToGroup package has been developed for research in disciplines such as official languages, computational theory, and group theory, but this may be of interest to researchers in many other fields. The tool connects "computation" with group theory. That allows us to think mathematically about computational processes and use the concepts of group theory to analyze and solve problems.


# Solution description


LangToGroup is a Haskell console application for group presentation construction. Haskell was chosen due to its rich and convenient type system,
which we apply to represent algorithmic types such as formal grammar,
Turing machine, S-machine, semigroup presentation, or group presentation. Furthermore, it is worth noting that data types were intentionally described in the same way as in the definitions of the corresponding concepts. This was done to maintain the maximum similarity to the articles [@Sapir, @rotman1995word]. 

The package provides the capability to obtain a presentation of a group from a Turing machine in two ways described in [@Sapir] and [@rotman1995word]. All of them are building a presentation of a group for a Turing Machine with preservation of its language. The first one does it through Symmetric Turing Machine and S-machine sequential constructions. The second one is via building a semigroup presentation. Moreover, two modifications of the second approach which output smaller presentations were developed as well. The time algorithms take depends on input grammar, but usually, it is no more than a few minutes.

From the beginning, the tool has been developing for the study of formal languages because of what algorithms for constructing a Turing machine from context-free and Boolean languages have additionally developed. Moreover, LangToGroup supports several types of input and output and each step of the construction can be printed in a document of LaTeX format for convenient usage. Therefore, LangToGroup presents a full pipeline of group presentation construction from formal languages and also can be used for construction from Turing machines. 


# References
