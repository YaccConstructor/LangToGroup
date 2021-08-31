{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

module Format (
    SimpleFormat,
    TaggedFormat,
    MultiFormat,
    MaybeFormat(..),
    isFormat,
    MaybeSimpleFormat,
    MaybeTaggedFormat,
    format,
    apply,
    format2str,
    match,
  ) where

import Data.List (isPrefixOf, isSuffixOf, intercalate)
import Data.Function ((&))
import Data.Maybe (isJust)
import Control.Applicative ((<|>))

data SimpleFormat = SimpleFormat String String

data TaggedFormat = TaggedFormat String String String

unTagged :: TaggedFormat -> SimpleFormat
unTagged (TaggedFormat st _ fn) = SimpleFormat st fn

newtype MultiFormat = MultiFormat [String]

data MaybeFormat f =
      JustFormat f
    | JustString String

isFormat :: MaybeFormat f -> Bool
isFormat (JustFormat _) = True
isFormat _ = False

type MaybeSimpleFormat = MaybeFormat SimpleFormat

type MaybeTaggedFormat = MaybeFormat TaggedFormat

class Format f where
    format_ :: MonadFail m => Char -> Char -> String -> m f

instance Format SimpleFormat where
    format_ op cp = go "" where
        go acc (op':cp':cs) | op' == op && cp' == cp =
            return $ SimpleFormat (reverse acc) cs
        go acc (c:cs) = go (c:acc) cs
        go _ "" = fail $
            "SimpleFormat must contain \"" ++ [op, cp] ++ "\" substring"

instance Format TaggedFormat where
    format_ op cp = go False "" "" where
        go False acc1 acc2 (op':cs) | op' == op =
            go True acc1 acc2 cs
        go True acc1 acc2 (cp':cs) | cp' == cp =
            return $ TaggedFormat (reverse acc1) (reverse acc2) cs
        go False acc1 acc2 (c:cs) = go False (c:acc1) acc2 cs
        go True acc1 acc2 (c:cs) = go True  acc1 (c:acc2) cs
        go _ _ _ _ = fail $
            "TaggedFormat must contain substring surrounded by '" ++
            [op] ++ "' and '" ++ [cp] ++ "' chars"

instance Format MultiFormat where
    format_ op cp = fmap MultiFormat . go "" where
        go _ (op1:cp1:op2:cp2:_)
            | op1 == op && cp1 == cp && op2 == op && cp2 == cp =
                fail $
                    "MultiFormat mustn't contain \"" ++
                    [op, cp, op, cp] ++ "\" substring"
        go acc (op':cp':cs)
            | op' == op && cp' == cp = (reverse acc :) <$> go "" cs
        go acc (c:cs) = go (c:acc) cs
        go acc "" = return [reverse acc]

instance Format f => Format (MaybeFormat f) where
    format_ op cp s = return $
        case format_ op cp s of
            Just f  -> JustFormat f
            Nothing -> JustString s

format :: (Format f, MonadFail m) => String -> m f
format = format_ '{' '}'

class Format f => Apply f i where
    apply :: f -> i -> String

instance Apply SimpleFormat String where
    apply (SimpleFormat st fn) s = st ++ s ++ fn

instance Apply TaggedFormat String where
    apply f s = apply (unTagged f) s

instance Apply TaggedFormat () where
    apply (TaggedFormat st md fn) () = st ++ md ++ fn

instance Apply MultiFormat [String] where
    apply (MultiFormat ss) ss' = foldr (++) "" $ zipWith (++) ss $ ss' ++ repeat ""

instance Apply f i => Apply (MaybeFormat f) i where
    apply (JustFormat f) s = apply f s
    apply (JustString s) _ = s

format2str :: Apply f () => f -> String
format2str = flip apply ()

class Format f => Match f r where
    match :: f -> String -> r

instance Match SimpleFormat Bool where
    match (SimpleFormat st fn) s = isPrefixOf st s && isSuffixOf fn s

instance MonadFail m => Match SimpleFormat (m String) where
    match f@(SimpleFormat st fn) s =
        if match f s
        then return $ drop (length st) $ take (length s - length fn) s
        else fail $
            "Can't match string \"" ++ s ++ "\" with format \"" ++
            st ++ "{...}" ++ fn ++ "\""

instance Match SimpleFormat a => Match TaggedFormat a where
    match f s = match (unTagged f) s

instance Match MultiFormat (Maybe [String]) where
    match (MultiFormat ss) s =
        go Nothing s ss & (
            flip maybe return $ fail $
                "Can't match string \"" ++ s ++ "\" with format \"" ++
                intercalate "{...}" ss ++ "\""
          ) where
            go _ "" [""] = Just []
            go _ "" _ = Nothing
            go Nothing (c:cs) ((c':cs'):ss')
                | c == c' = go Nothing cs (cs':ss')
                | otherwise = Nothing
            go Nothing cs ("":s':ss') = go (Just "") cs (s':ss')
            go Nothing _ [""] = Nothing
            go (Just acc) (c:cs) ss' =
                (reverse acc :) <$> go Nothing (c:cs) ss'
                  <|>
                go (Just $ c:acc) cs ss'
            go _ _ _ = Nothing

instance Match MultiFormat Bool where
    match f s = isJust (match f s :: Maybe [String])

instance Match f Bool => Match (MaybeFormat f) Bool where
    match (JustFormat f) s = match f s
    match (JustString s') s = s' == s

instance (MonadFail m, Match f (m String)) => Match (MaybeFormat f) (m String) where
    match (JustFormat f) s = match f s
    match (JustString s') s =
        if s' == s
        then return ""
        else fail $ 
            "Can't match string \"" ++ s ++ "\" with format \"" ++ s' ++ "\""
