module BiTMReader where

import TMReader
import TMTypes

type BiTMReader a = (TMInfo, TMInfo) -> a

runBiTMReader :: BiTMReader a -> TuringMachine -> TuringMachine -> a
runBiTMReader btmr tm1 = runTMReader (runTMReader (curry btmr) tm1)

forTM1 :: TMReader a -> BiTMReader a
forTM1 tmr = tmr . fst

forTM2 :: TMReader a -> BiTMReader a
forTM2 tmr = tmr . snd
