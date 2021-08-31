module TuringMachine.Optimization.Safe (
    secure,
  ) where

import TuringMachine as TM
import Containers.Set (cartesianProduct)

loop :: State -> Symbol -> TM.Quadruple
loop q s = ((q, s), (S s, q))

addQs :: TuringMachine -> Quadruples
addQs tm =
    let allQs = (tm^.allStates) <\ finalState
        allSs = tm^.allSymbols
        qsFstParts = keysSet (tm^.quadruples)
        allFstParts = cartesianProduct allQs allSs
        notQsFstParts = allFstParts \\ qsFstParts
    in  fromDistinctAscList $
            map (uncurry loop) $
                toList notQsFstParts

secure :: TuringMachine -> TuringMachine
secure = do
    aqs <- addQs
    quadruples %~ (\/ aqs)
