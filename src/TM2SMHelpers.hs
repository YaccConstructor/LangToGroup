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
e_x :: Int -> Smb
e_x j       = SmbQ $ newState X "" eTag j Nothing Nothing
e_x' :: Int -> Smb
e_x' j      = SmbQ $ newState X "" quoteTag j Nothing Nothing
e_f :: String -> Int -> Smb
e_f idx j   = SmbQ $ newState F idx eTag j Nothing Nothing 
e_f' :: String -> Int -> Smb
e_f' idx j  = SmbQ $ newState F idx quoteTag j Nothing Nothing
e_e :: Int -> Smb
e_e j       = SmbQ $ newState E "" eTag j Nothing Nothing
e_e' :: Int -> Smb
e_e' j      = SmbQ $ newState E "" quoteTag j Nothing Nothing
e_p :: Int -> Smb
e_p j       = SmbQ $ newState P "" eTag j Nothing Nothing 
e_q :: Int -> Smb
e_q j       = SmbQ $ newState Q "" eTag j Nothing Nothing 
e_r :: Int -> Smb
e_r j       = SmbQ $ newState R "" eTag j Nothing Nothing 
e_s :: Int -> Smb
e_s j       = SmbQ $ newState S "" eTag j Nothing Nothing 
e_t :: Int -> Smb
e_t j       = SmbQ $ newState T "" eTag j Nothing Nothing 
e_u :: Int -> Smb
e_u j       = SmbQ $ newState U "" eTag j Nothing Nothing 
e_pd :: Int -> Smb
e_pd j      = SmbQ $ newState P "" dashTag j Nothing Nothing
e_qd :: Int -> Smb
e_qd j      = SmbQ $ newState Q "" dashTag j Nothing Nothing
e_rd :: Int -> Smb
e_rd j      = SmbQ $ newState R "" dashTag j Nothing Nothing
e_sd :: Int -> Smb
e_sd j      = SmbQ $ newState S "" dashTag j Nothing Nothing
e_td :: Int -> Smb
e_td j      = SmbQ $ newState T "" dashTag j Nothing Nothing
e_ud :: Int -> Smb
e_ud j      = SmbQ $ newState U "" dashTag j Nothing Nothing

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