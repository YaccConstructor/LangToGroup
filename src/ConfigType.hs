module ConfigType where
    import TMType

    newtype Configs = Configs [[([Square], State, [Square])]]
        deriving (Eq, Ord, Show)