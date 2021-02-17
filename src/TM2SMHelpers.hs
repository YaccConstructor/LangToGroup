module TM2SMHelpers where

import SMType
import Data.Set (Set)
import qualified Data.Set as Set
import qualified TMType

eTag :: Set Tag
eTag = Set.fromList []
standardV :: Int -> Maybe StateVal
standardV i = Just $ StateVal i Nothing Nothing
eTagState :: StateName -> String -> State
eTagState name i = State name i eTag Nothing
genRange :: Show a => StateName -> [a] -> [State]
genRange name range = [eTagState name (show i) | i <- range]
gen :: StateName -> [State]
gen name = genRange name [0..4]
addTag :: Tag -> State -> State
addTag newTag q = q {s_tags = Set.insert newTag (s_tags q) }
addTags :: [Tag] -> [State] -> [State]
addTags newTags qs = [addTag newTag p | p <- qs, newTag <- newTags]
getai :: [TMType.TapeCommand] -> (TMType.Square, Int)
getai c =
    let get cmd i =
            case cmd of
                TMType.PreSMCommand ((a, _), _) : t
                    | a /= TMType.ES -> (a, i)
                    | otherwise -> get t (i + 1)
                _ -> error "Must be PreSMCommand"
    in
    get c 1

addICmdSmTag :: TMCMD -> SMTag -> State -> State
addICmdSmTag cmd tag q =
    let (Command c) = cmd
        (_, j) = getai c
    in
    case tag of
        T4 -> q {s_val = Just $ StateVal j jcmd jsmtag}
        T9 -> q {s_val = Just $ StateVal j jcmd jsmtag}
        TAlpha -> q {s_val = Just $ StateVal 0 jcmd jsmtag}
        TOmega -> q {s_val = Just $ StateVal (k + 1) jcmd jsmtag}
            where
                k = length c
    where   jcmd = Just cmd
            jsmtag = Just tag

quoteTag :: Set Tag
quoteTag = Set.fromList [Quote]
dashTag :: Set Tag
dashTag = Set.fromList [Dash]
hatTag :: Set Tag
hatTag = Set.fromList [Hat]
hatdashTag :: Set Tag
hatdashTag = Set.fromList [Hat, Dash]
newState :: StateName
                  -> String -> Set Tag -> Int -> Maybe TMCMD -> Maybe SMTag -> State
newState name idx tags i cmd tag = State name idx tags $ Just $ StateVal i cmd tag
eX :: Int -> Smb
eX j       = SmbQ $ newState X "" eTag j Nothing Nothing
eX' :: Int -> Smb
eX' j      = SmbQ $ newState X "" quoteTag j Nothing Nothing
eF :: String -> Int -> Smb
eF idx j   = SmbQ $ newState F idx eTag j Nothing Nothing
eF' :: String -> Int -> Smb
eF' idx j  = SmbQ $ newState F idx quoteTag j Nothing Nothing
eE :: Int -> Smb
eE j       = SmbQ $ newState E "" eTag j Nothing Nothing
eE' :: Int -> Smb
eE' j      = SmbQ $ newState E "" quoteTag j Nothing Nothing
eP :: Int -> Smb
eP j       = SmbQ $ newState P "" eTag j Nothing Nothing
eQ :: Int -> Smb
eQ j       = SmbQ $ newState Q "" eTag j Nothing Nothing
eR :: Int -> Smb
eR j       = SmbQ $ newState R "" eTag j Nothing Nothing
eS :: Int -> Smb
eS j       = SmbQ $ newState S "" eTag j Nothing Nothing
eT :: Int -> Smb
eT j       = SmbQ $ newState T "" eTag j Nothing Nothing
eU :: Int -> Smb
eU j       = SmbQ $ newState U "" eTag j Nothing Nothing
ePd :: Int -> Smb
ePd j      = SmbQ $ newState P "" dashTag j Nothing Nothing
eQd :: Int -> Smb
eQd j      = SmbQ $ newState Q "" dashTag j Nothing Nothing
eRd :: Int -> Smb
eRd j      = SmbQ $ newState R "" dashTag j Nothing Nothing
eSd :: Int -> Smb
eSd j      = SmbQ $ newState S "" dashTag j Nothing Nothing
eTd :: Int -> Smb
eTd j      = SmbQ $ newState T "" dashTag j Nothing Nothing
eUd :: Int -> Smb
eUd j      = SmbQ $ newState U "" dashTag j Nothing Nothing

getJIdx :: [TMType.TapeCommand] -> Int -> (String, String)
getJIdx c j =
    let internal cmd i =
            case cmd of
                TMType.PreSMCommand ((_, TMType.StateOmega(TMType.State b)), (_, TMType.StateOmega(TMType.State b1))) : t
                    | j == i -> (b, b1)
                    | otherwise -> internal t (i + 1)
                _ -> error "Not found j"
    in
        internal c 1
