module Object (
    Object (..),
    getObject,
    getObjects,
    objectsInfo,
    module Containers,
  ) where

import Containers

import Control.Monad (forM)
import Data.List (find, intercalate)
import Data.List.Split (splitWhen)

data Object =
      Grammar
    | TuringMachine
    | GroupPresentation
    deriving (Eq, Ord)

data ObjDescr = Object
    { names :: [String]
    , obj   :: Object
    , descr :: String
    }

objects :: [ObjDescr]
objects =
    [ Object ["grammar"] Grammar
        "Input grammar (context-free, conjunctive or boolean)"
    , Object ["turing_machine", "tm"] TuringMachine
        "Produced Turing machine (its type depends on used approach)"
    , Object ["group_presentation", "gp"] GroupPresentation
        "Produced group presentation"
    ]

getObject :: MonadFail m => String -> m Object
getObject s =
    maybe (fail $ "Unknown object: " ++ s) return $
        obj <$> find (\object -> s `elem` names object) objects

getObjects :: MonadFail m => String -> m (Set Object)
getObjects = fmap fromList . traverse getObject . splitWhen (== ',')

objectsInfo :: String
objectsInfo = unlines $
    [ "Objects:" ] ++
    ( do
        object <- objects
        [ "  " ++ intercalate ", " (names object), "    " ++ descr object ]
      ) ++
    [ "Note: When enumerating objects, they must be separated by commas" ]
