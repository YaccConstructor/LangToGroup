module XTMSymbol where

import OTMReader
import qualified TMType as OTM
import qualified TMTypes as ITM
import qualified TMSemigroup as ITMS
import qualified Data.Set as Set
import qualified Data.Map.Lazy as Map
import Control.Monad ((>=>), when)

type ITMSymbol = ITM.Symbol

type TTMSymbol = Maybe OTMSymbol

type MTMSymbol = Int

type OTMSymbol = Either OTM.State OTM.Square

t2mTMS :: TTMSymbol -> OTMReader MTMSymbol
t2mTMS Nothing = return 0
t2mTMS (Just (Left state)) = do
    states <- getStates
    return $ Set.findIndex state states + 1
t2mTMS (Just (Right symbol)) = do
    amountStates <- Set.size <$> getStates
    symbols <- getSymbols
    return $ Set.findIndex symbol symbols + amountStates + 1

m2iTMS :: MTMSymbol -> OTMReader ITMSymbol
m2iTMS = return . ITM.S

t2iTMS :: TTMSymbol -> OTMReader ITMSymbol
t2iTMS = t2mTMS >=> m2iTMS

i2mTMS :: ITMSymbol -> OTMReader MTMSymbol
i2mTMS (ITM.S x) = return x

m2tTMS :: MTMSymbol -> OTMReader TTMSymbol
m2tTMS 0 = return Nothing
m2tTMS x = do
    states <- getStates
    symbols <- getSymbols
    let i = x - 1
    let j = i - Set.size states
    return $ Just $
        if i < Set.size states
        then Left  $ Set.elemAt i states
        else Right $ Set.elemAt j symbols

i2tTMS :: ITMSymbol -> OTMReader TTMSymbol
i2tTMS = i2mTMS >=> m2tTMS

forAllPossibleITMS :: (ITMSymbol -> OTMReader [a]) -> OTMReader [a]
forAllPossibleITMS f = forAllPossibleMTMS $ m2iTMS >=> f

forAllPossibleMTMS :: (MTMSymbol -> OTMReader [a]) -> OTMReader [a]
forAllPossibleMTMS f = do
    amountStates <- Set.size <$> getStates
    amountSymbols <- Set.size <$> getSymbols
    concat <$> sequence
        [
            f x
            | x <- [0 .. amountStates + amountSymbols]
        ]

ttmsIsBlank :: TTMSymbol -> OTMReader Bool
ttmsIsBlank Nothing = return True
ttmsIsBlank _ = return False

ttmsIsState :: TTMSymbol -> OTMReader Bool
ttmsIsState (Just (Left _)) = return True
ttmsIsState _ = return False

ttmsIsSquare :: TTMSymbol -> OTMReader Bool
ttmsIsSquare (Just (Right _)) = return True
ttmsIsSquare _ = return False

itmsIsBlank :: ITMSymbol -> OTMReader Bool
itmsIsBlank = i2tTMS >=> ttmsIsBlank

itmsIsState :: ITMSymbol -> OTMReader Bool
itmsIsState = i2tTMS >=> ttmsIsState

itmsIsSquare :: ITMSymbol -> OTMReader Bool
itmsIsSquare = i2tTMS >=> ttmsIsSquare
