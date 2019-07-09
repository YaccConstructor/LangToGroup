module ConfigType where
    import TMType

    newtype Configs = Configs [[([String], State, [String])]]
        deriving (Eq, Ord, Show)