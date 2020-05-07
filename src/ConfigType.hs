
-- |This module represents a configuration of the Turing machine 'TMType.TM'.
module ConfigType where
    
import TMType

-- |This is type of configurations of the Turing machine.
newtype Configs = Configs [[([Square], State, [Square])]]
    deriving (Eq, Ord, Show)