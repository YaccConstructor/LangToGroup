module Move where

import TMTypes

data Move = ToLeft | ToRight

versa :: Move -> Move
versa ToLeft  = ToRight
versa ToRight = ToLeft

toSymbolMove :: Move -> SymbolMove
toSymbolMove ToLeft  = L
toSymbolMove ToRight = R
