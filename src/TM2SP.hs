{-# LANGUAGE MultiWayIf #-}

-- |Module `TM2SP` include functions `semigroupGamma`, `semigroupGamma_1` and
--  `semigroupGamma_2` for converting Turing machine to semigroup presentation.
module TM2SP (
    convertInput,
    semigroupGamma,
    SemigroupPresentation_1 (SP1, unSP1),
    semigroupGamma_1,
    semigroupGamma_2,
  ) where

import TM2SP.Generators
import TM2SP.Relations
import TuringMachine
import qualified SemigroupPresentation as SP
import ShowInfo

tm2sp ::
    MonadFail m =>
    Gens ->
    Rels ->
    TuringMachine ->
    m SP.SemigroupPresentation
tm2sp gs rs' tm = do
    gd <- gens gs tm
    rs <- rels rs' gd tm
    return $ SP.semigroupPresentation rs gd

convertInput :: MonadFail m => TuringMachine -> (String, Int) -> m String
convertInput tm ("", _) = do
    let a = tm^.alphabet
    b <- a !? blank
    let b' = "s_" ++ show (numSymbol b)
    return $ unwords ["h", "q_1", b', "h"]
convertInput tm (str, pos) = do
    let a = tm^.alphabet
    ss <- traverse (a !?) $ showedSymbols str
    b' <- ("s_" ++) <$> show <$> numSymbol <$> (a !? blank)
    let ss' = ("s_" ++) <$> show <$> numSymbol <$> ss
        ss'' = if
            | pos < 0 ->
                ["q_1"] ++ replicate (-pos) b' ++ ss'
            | pos >= length str ->
                ss' ++ replicate (pos - length str) b' ++ ["q_1", b']
            | otherwise ->
                take pos ss' ++ ["q_1"] ++ drop pos ss'
    return $ unwords $ "h" +> ss'' <+ "h"

semigroupGamma :: MonadFail m => TuringMachine -> m SP.SemigroupPresentation
semigroupGamma =
    tm2sp [
            simple "q h",
            "s_{}" `from` strNumSymbols,
            "q_{}" `from` strNumStates
        ] [[
            [
            "q_i s_j" === "q_l s_k"
              ] & for' (
                "q_{i} s_{j} s_{k} q_{l}" `in'`
                (strQuadruples.withoutLoops.takeOnly(withoutMove))
              ),
            [
            "q_i s_j s_B" === "s_j q_l s_B",
            "q_i s_j h" === "s_j q_l s_0 h"
              ] & for' (
                "q_{i} s_{j} R q_{l}" `in'`
                (strQuadruples.takeOnly(withMove toRight))
              ),
            [
            "s_B q_i s_j" === "q_l s_B s_j",
            "h q_i s_j" === "h q_l s_0 s_j"
              ] & for' (
                "q_{i} s_{j} L q_{l}" `in'`
                (strQuadruples.takeOnly(withMove toLeft))
              ),
            "q_0 s_B" === "q_0",
            "s_B q_0 h" === "q_0 h",
            "h q_0 h" === "q"
        ] & for' ("s_{B}" `in'` strNumSymbols)]

newtype SemigroupPresentation_1 = SP1 { unSP1 :: SP.SemigroupPresentation }

instance ShowInfo (SemigroupPresentation_1) where
    showTitle = const $ Title "Semigroup Presentation"
    showInfo (SP1 sp) = showInfo sp
    showListTitle = const $ Title "List of Semigroup Presentations"

semigroupGamma_1 :: MonadFail m => TuringMachine -> m SemigroupPresentation_1
semigroupGamma_1 =
    (fmap.fmap) SP1 $ tm2sp [
            "q_{}" `from` strNumStates,
            simple "h",
            "s_{}" `from` strNumSymbols
        ] [[
            [
            "q_i s_j" === "q_l s_k"
              ] & for' (
                "q_{i} s_{j} s_{k} q_{l}" `in'`
                (strQuadruples.withoutLoops.takeOnly(withoutMove))
              ),
            [
            "q_i h" === "q_l s_k h"
              ] & for' (
                "q_{i} s_0 s_{k} q_{l}" `in'`
                (strQuadruples.withoutLoops.takeOnly(withoutMove&.fromSymbol(blankSymbol)))
              ),
            [
            "q_i s_j" === "s_j q_l"
              ] & for' (
                "q_{i} s_{j} R q_{l}" `in'`
                (strQuadruples.takeOnly(withMove toRight))
              ),
            [
            "q_i h" === "s_0 q_l h"
              ] & for' (
                "q_{i} s_0 R q_{l}" `in'`
                (strQuadruples.takeOnly(withMove(toRight)&.fromSymbol(blankSymbol)))
              ),
            [
            "s_B q_i s_j" === "q_l s_B s_j",
            "h q_i s_j" === "h q_l s_0 s_j"
              ] & for' (
                "q_{i} s_{j} L q_{l}" `in'`
                (strQuadruples.takeOnly(withMove toLeft))
              ),
            [
            "s_B q_i h" === "q_l s_B h"
              ] & for' (
                "q_{i} s_0 L q_{l}" `in'`
                (strQuadruples.takeOnly(withMove(toLeft)&.fromSymbol(blankSymbol)))
              ),
            "q_0 s_B" === "q_0",
            "s_B q_0 h" === "q_0 h"
        ] & for' ("s_{B}" `in'` strNumSymbols)]

semigroupGamma_2 :: MonadFail m => TuringMachine -> m SemigroupPresentation_1
semigroupGamma_2 =
    (fmap.fmap) SP1 $ tm2sp [
            "q_{}" `from` strNumStates,
            "qR_{}" `from` strNumStates,
            simple "h",
            "s_{}" `from` strNumSymbols
        ] [[
            [
            "q_i s_j" === "q_l s_k"
              ] & for' (
                "q_{i} s_{j} s_{k} q_{l}" `in'`
                (strQuadruples.withoutLoops.takeOnly(withoutMove))
              ),
            [
            "q_i h" === "q_l s_k h"
              ] & for' (
                "q_{i} s_0 s_{k} q_{l}" `in'`
                (strQuadruples.withoutLoops.takeOnly(withoutMove&.fromSymbol(blankSymbol)))
              ),
            [
            "q_i s_j" === "s_j q_l"
              ] & for' (
                "q_{i} s_{j} R q_{l}" `in'`
                (strQuadruples.takeOnly(withMove toRight))
              ),
            [
            "q_i h" === "s_0 q_l h"
              ] & for' (
                "q_{i} s_0 R q_{l}" `in'`
                (strQuadruples.takeOnly(withMove(toRight)&.fromSymbol(blankSymbol)))
              ),
            [
            "s_j qR_i" === "qR_l s_j"
              ] & for' (
                "qR_{i} s_{j} L qR_{l}" `in'`
                (strQuadruples.takeOnly(withMove toLeft))
              ),
            [
            "h qR_i" === "h qR_l s_0"
              ] & for' (
                "qR_{i} s_0 L qR_{l}" `in'`
                (strQuadruples.takeOnly(withMove(toLeft)&.fromSymbol(blankSymbol)))
              ),
            [
            "q_A s_B" === "s_B qR_A"
              ] & for' (
                "q_{A} qR_{A} s_{B}" `in'`
                (strQuadruples.withoutLoops.takeFromPart.copy(0))
              ),
            [
            "q_A h" === "s_0 qR_A h",
            "h qR_A" === "h q_A s_0"
              ] & for' (
                "q_{A} qR_{A} s_0" `in'`
                (strQuadruples.withoutLoops.takeOnly(fromSymbol(blankSymbol)).takeFromPart.copy(0))
              ),
            "q_0 s_B" === "q_0",
            "s_B q_0 h" === "q_0 h"
        ] & for' ("s_{B}" `in'` strNumSymbols)]
