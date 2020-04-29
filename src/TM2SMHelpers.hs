module TM2SMHelpers where
    
import SMType
import Data.Set (Set)
import qualified Data.Set as Set
import qualified TMType

delta = Y $ TMType.Value "\\delta" 
alpha = Y $ TMType.Value "\\alpha" 
omega = Y $ TMType.Value "\\omega" 
eTag = Set.fromList []
standardV i = Just $ StateVal i Nothing Nothing
eTagState name i = State name i eTag Nothing
genRange name range = [eTagState name (show i) | i <- range]
gen name = genRange name [0..4]
addTag newTag q = q {s_tags = Set.insert newTag (s_tags q) }
addTags newTags qs = [addTag newTag p | p <- qs, newTag <- newTags]
getai cmd =
    let get cmd i =  
            case cmd of
                TMType.PreSMCommand ((a, _), _) : t 
                    | a /= TMType.eL -> (a, i)
                    | otherwise -> get t (i + 1)
    in 
    get cmd 1
addICmdSmTag cmd smTag q =
    let (Command c) = cmd
        (_, j) = getai c
    in
    case smTag of
        T4 -> q {s_val = Just $ StateVal j jcmd jsmtag}
        T9 -> q {s_val = Just $ StateVal j jcmd jsmtag}
        TAlpha -> q {s_val = Just $ StateVal 0 jcmd jsmtag}
        TOmega -> q {s_val = Just $ StateVal (k + 1) jcmd jsmtag}
            where 
                k = length c
    where   jcmd = Just cmd
            jsmtag = Just smTag


quoteTag = Set.fromList [Quote]
dashTag = Set.fromList [Dash]
hatTag = Set.fromList [Hat]
hatdashTag = Set.fromList [Hat, Dash]
newState name idx tags i cmd smTag = State name idx tags $ Just $ StateVal i cmd smTag
e_x j       = SmbQ $ newState X "" eTag j Nothing Nothing
e_x' j      = SmbQ $ newState X "" quoteTag j Nothing Nothing
e_f idx j   = SmbQ $ newState F idx eTag j Nothing Nothing 
e_f' idx j  = SmbQ $ newState F idx quoteTag j Nothing Nothing
e_e j       = SmbQ $ newState E "" eTag j Nothing Nothing
e_e' j      = SmbQ $ newState E "" quoteTag j Nothing Nothing
e_p j       = SmbQ $ newState P "" eTag j Nothing Nothing 
e_q j       = SmbQ $ newState Q "" eTag j Nothing Nothing 
e_r j       = SmbQ $ newState R "" eTag j Nothing Nothing 
e_s j       = SmbQ $ newState S "" eTag j Nothing Nothing 
e_t j       = SmbQ $ newState T "" eTag j Nothing Nothing 
e_u j       = SmbQ $ newState U "" eTag j Nothing Nothing 
e_pd j      = SmbQ $ newState P "" dashTag j Nothing Nothing
e_qd j      = SmbQ $ newState Q "" dashTag j Nothing Nothing
e_rd j      = SmbQ $ newState R "" dashTag j Nothing Nothing
e_sd j      = SmbQ $ newState S "" dashTag j Nothing Nothing
e_td j      = SmbQ $ newState T "" dashTag j Nothing Nothing
e_ud j      = SmbQ $ newState U "" dashTag j Nothing Nothing

getJIdx cmd j =
    let internal cmd i =  
            case cmd of
                TMType.PreSMCommand ((_, TMType.StateOmega(TMType.State b)), (_, TMType.StateOmega(TMType.State b1))) : t 
                    | j == i -> (b, b1)
                    | otherwise -> internal t (i + 1)
    in
        internal cmd 1