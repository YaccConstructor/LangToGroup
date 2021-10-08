module GroupPresentation.Relation (
    Relation,
    relation,
    relator,
    Relations,
    module GroupPresentation.Element,
  ) where

import GroupPresentation.Element

import Containers

type Relation = Pair EWord

relation :: EWord -> EWord -> Relation
relation = curry Pair

relator :: EWord -> Relation
relator ew = relation ew []

type Relations = Set Relation
