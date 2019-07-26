module TM2SM where

import SMType
import Data.Set (Set)
import qualified Data.Set as Set
import qualified TMType
import Data.List (transpose, groupBy, sortBy)
import Data.Maybe (fromJust)
import Debug.Trace (trace)


devidePositiveNegativeCommands :: [[TMType.TapeCommand]] -> ([[TMType.TapeCommand]], [[TMType.TapeCommand]], [[TMType.TapeCommand]], [[TMType.TapeCommand]])
devidePositiveNegativeCommands commands = do
    let check21 command = 
            case command of
                TMType.PreSMCommand((a@(TMType.Value ah), b),(a1, b1)) : t 
                    | a /= TMType.emptySymbol && head ah /= 'E' -> True
                    | otherwise -> check21 t
                TMType.PreSMCommand((TMType.BCommand _, _),(_, _)) : t -> True
                TMType.PreSMCommand((TMType.PCommand _, _),(_, _)) : t -> True
                [] -> False

    let reverseCommand (TMType.PreSMCommand((a, b),(a1, b1))) =  TMType.PreSMCommand((a1, b1),(a, b))

    let devidePositiveNegativeCommandsInternal commands (accP21, accP22, accN21, accN22) =
            case commands of
                h : t 
                    | check21 h -> devidePositiveNegativeCommandsInternal t (h : accP21, accP22, accN21, accN22)
                    | check21 reversedH -> devidePositiveNegativeCommandsInternal t (accP21, accP22, h : accN21, accN22)
                    | notElem reversedH accP22 -> devidePositiveNegativeCommandsInternal t (accP21, h : accP22, accN21, accN22)
                    | otherwise -> devidePositiveNegativeCommandsInternal t (accP21, accP22, accN21, h : accN22)
                    where reversedH = map reverseCommand h
                [] -> (accP21, accP22, accN21, accN22)
    devidePositiveNegativeCommandsInternal commands ([], [], [], [])

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
                    | a /= TMType.emptySymbol -> (a, i)
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
        
copySM :: SM -> (State -> Bool) -> Tag -> SM
copySM sm qFilter newTag =
   let q = map (Set.map (\x -> if qFilter x then addTag newTag x else x)) $ qn sm
       filterWord (Word w) = Word(map (\s -> case s of SmbQ q | qFilter q -> SmbQ (addTag newTag q); _ -> s ) w)
       prog = map (\ (SRule l) -> SRule(map (\(w1, w2) -> (filterWord w1, filterWord w2)) l)) (srs sm)
   in
   SM (yn sm) q prog

copySMForCommand :: SM -> SMTag -> TMCMD -> SM
copySMForCommand sm smTag cmd =
   let q = map (Set.map (addICmdSmTag cmd smTag)) (qn sm)
       filterWord (Word w) = Word(map (\s -> case s of SmbQ q -> SmbQ (addICmdSmTag cmd smTag q); _ -> s ) w)
       prog = map (\ (SRule l) -> SRule(map (\(w1, w2) -> (filterWord w1, filterWord w2)) l)) (srs sm)
   in
   SM (yn sm) q prog

createSMs y =
    let ps@[p0,p1,p2,p3,p4] = gen P
        p's@[p0',p1',p2',p3',p4'] = addTags [Quote] ps
        pds@[p0d,p1d,p2d,p3d,p4d] = addTags [Dash] ps
        p'ds@[p0'd,p1'd,p2'd,p3'd,p4'd] = addTags [Dash] p's

        qs@[q0,q1,q2,q3,q4] = gen Q 
        q's@[q0',q1',q2',q3',q4'] = addTags [Quote] qs
        qds@[q0d,q1d,q2d,q3d,q4d] = addTags [Dash] qs
        q'ds@[q0'd,q1'd,q2'd,q3'd,q4'd] = addTags [Dash] q's

        rs@[r0,r1,r2,r3,r4] = gen R 
        r's@[r0',r1',r2',r3',r4'] = addTags [Quote] rs
        rds@[r0d,r1d,r2d,r3d,r4d] = addTags [Dash] rs
        r'ds@[r0'd,r1'd,r2'd,r3'd,r4'd] = addTags [Dash] r's

        ss@[s0,s1,s2,s3,s4] = gen S
        s's@[s0',s1',s2',s3',s4'] = addTags [Quote] ss
        sds@[s0d,s1d,s2d,s3d,s4d] = addTags [Dash] ss
        s'ds@[s0'd,s1'd,s2'd,s3'd,s4'd] = addTags [Dash] s's

        ts@[t0,t1,t2,t3,t4] = gen T
        t's@[t0',t1',t2',t3',t4'] = addTags [Quote] ts
        tds@[t0d,t1d,t2d,t3d,t4d] = addTags [Dash] ts
        t'ds@[t0'd,t1'd,t2'd,t3'd,t4'd] = addTags [Dash] t's

        us@[u0,u1,u2,u3,u4] = gen U
        u's@[u0',u1',u2',u3',u4'] = addTags [Quote] us
        uds@[u0d,u1d,u2d,u3d,u4d] = addTags [Dash] us
        u'ds@[u0'd,u1'd,u2'd,u3'd,u4'd] = addTags [Dash] u's

        xs@[x0,x1,x2,x3,x4] = eTagState X "" : genRange X [1..4]
        x's@[x0',x1',x2',x3',x4'] = addTags [Quote] xs
        xds@[x0d,x1d,x2d,x3d,x4d] = addTags [Dash] xs
        x'ds@[x0'd,x1'd,x2'd,x3'd,x4'd] = addTags [Dash] x's

        e = State E "" eTag Nothing 
        e' = State E "" (Set.fromList [Quote]) Nothing
        f = State F "" eTag Nothing
        f' = State F "" (Set.fromList [Quote]) Nothing

        sm1 :: SM
        sm1 =
            let
               rl1 = 
                  SRule [ 
                    (Word [SmbQ q1], Word [SmbY' delta, SmbY' delta, SmbQ q1, SmbY delta, SmbY delta]),
                    (Word [SmbQ r1], Word [SmbY' delta, SmbQ r1, SmbY delta])
                  ]
               rl2 = 
                  SRule [ 
                    (Word [SmbQ p1, SmbQ q1], Word [SmbQ p2, SmbQ q2]),
                    (Word [SmbQ r1], Word [SmbQ r2]),
                    (Word [SmbQ s1], Word [SmbQ s2]),
                    (Word [SmbQ t1], Word [SmbQ t2]),
                    (Word [SmbQ u1], Word [SmbY delta, SmbQ u2])
                  ]
               rl3 = 
                  SRule [ 
                    (Word [SmbQ p1, SmbY delta, SmbQ q1], Word [SmbQ p3, SmbY delta, SmbQ q3]),
                    (Word [SmbQ r1], Word [SmbQ r3]),
                    (Word [SmbQ s1], Word [SmbQ s3]),
                    (Word [SmbQ t1], Word [SmbQ t3]),
                    (Word [SmbQ u1], Word [SmbQ u3])
            
                  ]
            in
            SM [[delta],[delta],[delta],[delta]] (map Set.fromList [[p1,p2,p3],[q1,q2,q3],[r1,r2,r3],[s1,s2,s3],[t1,t2,t3],[u1,u2,u3]]) [rl1,rl2,rl3]
            
        sm2 :: SM 
        sm2 =
            let     
               rl1 = 
                  SRule [ 
                    (Word [SmbQ q2], Word [SmbY delta, SmbQ q2, SmbY' delta]),
                    (Word [SmbQ s2], Word [SmbY' delta, SmbQ s2, SmbY delta])
                  ]

               rl2 = 
                  SRule [ 
                    (Word [SmbQ q2, SmbQ r2, SmbQ s2], Word [SmbQ q1, SmbQ r1, SmbQ s1]),
                    (Word [SmbQ p2], Word [SmbQ p1]),
                    (Word [SmbQ t2], Word [SmbQ t1]),
                    (Word [SmbQ u2], Word [SmbQ u1])
                  ]
            
            in
            SM [[delta],[delta],[delta],[delta]] (map Set.fromList [[p1,p2],[q1,q2],[r1,r2],[s1,s2],[t1,t2],[u1,u2]]) [rl1,rl2]
            
            
        sm3 :: SM
        sm3 = SM (yn sm1) (qn sm1) ((srs sm1) ++ (srs sm2))
            
        sm4 :: SM
        sm4 =
            let sm3' = copySM sm3 (\q -> s_idx q /= "3") Quote
            in  
            SM (yn sm1) (map Set.unions $ transpose [qn sm3, qn sm3']) (srs sm3 ++ srs sm3')
            
            
        sm4d :: SM
        sm4d = copySM sm4 (\ _ -> True) Dash
            
        sm5 :: [Y] -> SM
        sm5 y =
            let st = map Set.fromList [[e], [x0,x4], [f], [e'], 
                      ps ++ [p0',p1',p2'],
                      qs ++ [q0',q1',q2'],
                      rs ++ [r0',r1',r2'],
                      ss ++ [s0',s1',s2'],
                      ts ++ [t0',t1',t2'],
                      us ++ [u0',u1',u2'],
                      pds ++ [p0'd,p1'd,p2'd],
                      qds ++ [q0'd,q1'd,q2'd],
                      rds ++ [r0'd,r1'd,r2'd],
                      sds ++ [s0'd,s1'd,s2'd],
                      tds ++ [t0'd,t1'd,t2'd],
                      uds ++ [u0'd,u1'd,u2'd],
                      [f']]
                ys = [y,y,[],[],[delta],[delta],[delta],[delta],[delta],[],[delta],[delta],[delta],[delta],[delta],[]]
                prg = map (\a -> SRule[ (Word [SmbQ x0], Word [SmbY' a, SmbQ x0, SmbY a]),
                                        (Word [SmbQ p1'], Word [SmbQ p0']),
                                        (Word [SmbQ q1', SmbQ r1', SmbQ s1', SmbQ t1', SmbQ u1', SmbQ p0d],
                                         Word [SmbY' delta, SmbQ q0', SmbQ r0', SmbQ s0', SmbQ t0', SmbQ u0', SmbQ p1d, SmbY delta]),
                                        (Word [SmbQ q0d, SmbQ r0d, SmbQ s0d, SmbQ t0d, SmbQ u0d],
                                         Word [SmbQ q1d, SmbQ r1d, SmbQ s1d, SmbQ t1d, SmbQ u1d])
                                      ]) y
            in 
            SM ys st prg
            
        sm6 :: [Y] -> SM
        sm6 y =
            let sm5' = sm5 y 
                prg = [SRule[ (Word [SmbQ p0'], Word [SmbQ p1]),
                              (Word [SmbQ q0', SmbQ r0', SmbQ s0', SmbQ t0', SmbQ u0', SmbQ p1'd],
                               Word [SmbQ q1, SmbQ r1, SmbQ s1, SmbQ t1, SmbQ u1 ,SmbQ p0d]),
                              (Word [SmbQ q1'd, SmbQ r1'd, SmbQ s1'd, SmbQ t1'd, SmbQ u1'd],
                               Word [SmbQ q0d, SmbQ r0d, SmbQ s0d, SmbQ t0d, SmbQ u0d]) ]]
            in
            SM (yn sm5') (qn sm5') prg
  
        sm7 :: [Y] -> SM
        sm7 y =
            let sm5' = sm5 y 
                prg = [SRule[ (Word [SmbQ e, SmbQ x0], Word [SmbQ e, SmbQ x4]),
                              (Word [SmbQ p1, SmbQ q1, SmbQ r1, SmbQ s1, SmbQ t1, SmbQ u1, SmbQ p0d],
                               Word [SmbQ p4, SmbQ q4, SmbQ r4, SmbQ s4, SmbQ t4, SmbQ u4, SmbQ p4d]),
                              (Word [SmbQ q0d, SmbQ r0d, SmbQ s0d, SmbQ t0d, SmbQ u0d],
                               Word [SmbQ q4d, SmbQ r4d, SmbQ s4d, SmbQ t4d, SmbQ u4d]) ]]
        
            in 
            SM (yn sm5') (qn sm5') prg

        sm8 :: [Y] -> SM
        sm8 y =
            let sm5' = sm5 y 
            in  
            SM (yn sm5') (qn sm5') ((srs sm4) ++ (srs sm5') ++ (srs sm4d) ++ (srs (sm6 y)) ++ (srs (sm7 y)))
            
        sm9 :: [Y] -> SM
        sm9 y = 
            let sm8' = sm8 y 
                sm8c = copySM sm8' (\q -> (notElem (s_name q) [E, F]) && (s_idx q /= "4")) Hat
            in  
            SM (yn sm1) (map Set.unions $ transpose [qn sm8', qn sm8c]) (srs sm8' ++ srs sm8c)
            
        smAlpha :: SM
        smAlpha = SM [[alpha],[alpha]] (map Set.fromList [[e],[x0,x1,x2],[f]]) [SRule [(Word [SmbQ x0], Word [SmbY' alpha, SmbQ x0, SmbY alpha]),
                                                                            (Word [SmbQ e, SmbQ x0], Word [SmbQ e, SmbQ x1]),
                                                                            (Word [SmbQ x1], Word [SmbY alpha, SmbQ x1, SmbY' alpha]),
                                                                            (Word [SmbQ x1, SmbQ f], Word [SmbQ x2, SmbQ f])]]
            
        smOmega :: SM
        smOmega = SM [[omega],[omega]] (map Set.fromList [[e'],[x0',x1',x2'],[f']]) [SRule [(Word [SmbQ x0'], Word [SmbY omega, SmbQ x0', SmbY' omega]),
                                                                                (Word [SmbQ x0', SmbQ f'], Word [SmbQ x1', SmbQ f']),
                                                                                (Word [SmbQ x1'], Word [SmbY' omega, SmbQ x1', SmbY omega]),
                                                                                (Word [SmbQ e', SmbQ x1'], Word [SmbQ e', SmbQ x2'])]]
            
   in
      [sm4, sm9 y, smAlpha, smOmega]

quoteTag = Set.fromList [Quote]
dashTag = Set.fromList [Dash]
hatTag = Set.fromList [Hat]
hatdashTag = Set.fromList [Hat, Dash]
newState name idx tags i cmd smTag = State name idx tags $ Just $ StateVal i cmd smTag
e_x j       = newState X "" eTag j Nothing Nothing
e_x' j      = newState X "" quoteTag j Nothing Nothing
e_f idx j   = newState F idx eTag j Nothing Nothing 
e_f' idx j  = newState F idx quoteTag j Nothing Nothing
e_e j       = newState E "" eTag j Nothing Nothing
e_e' j      = newState E "" quoteTag j Nothing Nothing
e_p j       = newState P "" eTag j Nothing Nothing 
e_q j       = newState Q "" eTag j Nothing Nothing 
e_r j       = newState R "" eTag j Nothing Nothing 
e_s j       = newState S "" eTag j Nothing Nothing 
e_t j       = newState T "" eTag j Nothing Nothing 
e_u j       = newState U "" eTag j Nothing Nothing 
e_pd j      = newState P "" dashTag j Nothing Nothing
e_qd j      = newState Q "" dashTag j Nothing Nothing
e_rd j      = newState R "" dashTag j Nothing Nothing
e_sd j      = newState S "" dashTag j Nothing Nothing
e_td j      = newState T "" dashTag j Nothing Nothing
e_ud j      = newState U "" dashTag j Nothing Nothing

getJIdx cmd j =
    let internal cmd i =  
            case cmd of
                TMType.PreSMCommand ((_, TMType.StateOmega(TMType.State b)), (_, TMType.StateOmega(TMType.State b1))) : t 
                    | j == i -> (b, b1)
                    | otherwise -> internal t (i + 1)
    in
        internal cmd 1
            
createConnectingRules :: TMCMD -> [SRule]
createConnectingRules cmd = do
    let (Command command) = cmd 
    let k = length command    

    let e_f0        = e_f "" 0
    let e_fl'       = e_f' "" (k + 1)

    let x j sm        = newState X "" eTag j (Just cmd) (Just sm)
    let x' j sm       = newState X "" quoteTag j (Just cmd) (Just sm)
    let f' idx j sm   = newState F idx quoteTag j (Just cmd) (Just sm)
    let f idx j sm    = newState F idx eTag j (Just cmd) (Just sm) 
    let fl' sm        = newState F "" quoteTag (k + 1) (Just cmd) (Just sm)
    let f0 sm         = newState F "" eTag 0 (Just cmd) (Just sm) 
    let e' j sm       = newState E "" quoteTag j (Just cmd) (Just sm)
    let e j sm        = newState E "" eTag j (Just cmd) (Just sm)
    let p j sm        = newState P "" eTag j (Just cmd) (Just sm) 
    let q j sm        = newState Q "" eTag j (Just cmd) (Just sm) 
    let r j sm        = newState R "" eTag j (Just cmd) (Just sm) 
    let s j sm        = newState S "" eTag j (Just cmd) (Just sm) 
    let t j sm        = newState T "" eTag j (Just cmd) (Just sm) 
    let u j sm        = newState U "" eTag j (Just cmd) (Just sm) 
    let pd j sm       = newState P "" dashTag j (Just cmd) (Just sm)
    let qd j sm       = newState Q "" dashTag j (Just cmd) (Just sm)
    let rd j sm       = newState R "" dashTag j (Just cmd) (Just sm)
    let sd j sm       = newState S "" dashTag j (Just cmd) (Just sm)
    let td j sm       = newState T "" dashTag j (Just cmd) (Just sm)
    let ud j sm       = newState U "" dashTag j (Just cmd) (Just sm)

    let p_idx idx j sm        = newState P idx eTag j (Just cmd) (Just sm) 
    let q_idx idx j sm        = newState Q idx eTag j (Just cmd) (Just sm) 
    let r_idx idx j sm        = newState R idx eTag j (Just cmd) (Just sm) 
    let s_idx idx j sm        = newState S idx eTag j (Just cmd) (Just sm) 
    let t_idx idx j sm        = newState T idx eTag j (Just cmd) (Just sm)
    let u_idx idx j sm        = newState U idx eTag j (Just cmd) (Just sm)
    let x_idx idx j sm        = newState X idx eTag j (Just cmd) (Just sm)    
    
    let p_idx' idx j sm        = newState P idx quoteTag j (Just cmd) (Just sm) 
    let q_idx' idx j sm        = newState Q idx quoteTag j (Just cmd) (Just sm) 
    let r_idx' idx j sm        = newState R idx quoteTag j (Just cmd) (Just sm) 
    let s_idx' idx j sm        = newState S idx quoteTag j (Just cmd) (Just sm) 
    let t_idx' idx j sm        = newState T idx quoteTag j (Just cmd) (Just sm)
    let u_idx' idx j sm        = newState U idx quoteTag j (Just cmd) (Just sm)
    let x_idx' idx j sm        = newState X idx quoteTag j (Just cmd) (Just sm)

    let ph_idx idx j sm        = newState P idx hatTag j (Just cmd) (Just sm) 
    let qh_idx idx j sm        = newState Q idx hatTag j (Just cmd) (Just sm) 
    let rh_idx idx j sm        = newState R idx hatTag j (Just cmd) (Just sm) 
    let sh_idx idx j sm        = newState S idx hatTag j (Just cmd) (Just sm) 
    let th_idx idx j sm        = newState T idx hatTag j (Just cmd) (Just sm)
    let uh_idx idx j sm        = newState U idx hatTag j (Just cmd) (Just sm)
    let xh j sm                = newState X "" hatTag j (Just cmd) (Just sm)

    let phd_idx idx j sm        = newState P idx hatdashTag j (Just cmd) (Just sm) 
    let qhd_idx idx j sm        = newState Q idx hatdashTag j (Just cmd) (Just sm) 
    let rhd_idx idx j sm        = newState R idx hatdashTag j (Just cmd) (Just sm) 
    let shd_idx idx j sm        = newState S idx hatdashTag j (Just cmd) (Just sm) 
    let thd_idx idx j sm        = newState T idx hatdashTag j (Just cmd) (Just sm)
    let uhd_idx idx j sm        = newState U idx hatdashTag j (Just cmd) (Just sm)

    let (a, i) = getai command
    let getFromJ j = a where (a, _) = getJIdx command j
    let getToJ j = a where (_, a) = getJIdx command j

    let p4         = p_idx "1" i 
    let q4         = q_idx "1" i
    let r4         = r_idx "1" i
    let s4         = s_idx "1" i 
    let t4         = t_idx "1" i
    let u4         = u_idx "1" i
    let pd4 sm     = newState P "0" dashTag i (Just cmd) (Just sm) 
    let qd4 sm     = newState Q "0" dashTag i (Just cmd) (Just sm) 
    let rd4 sm     = newState R "0" dashTag i (Just cmd) (Just sm) 
    let sd4 sm     = newState S "0" dashTag i (Just cmd) (Just sm) 
    let td4 sm     = newState T "0" dashTag i (Just cmd) (Just sm)
    let ud4 sm     = newState U "0" dashTag i (Just cmd) (Just sm)

    let rule4 = 
            SRule $ (++) 
            [(Word [SmbQ $ e_e 0], Word [SmbQ $ e 0 T4]),
            (Word [SmbQ $ e_fl'], Word [SmbQ $ fl' T4]),
            (Word [SmbQ $ e_x 0, SmbQ $ e_f0], Word [SmbQ $ x 0 T4, SmbQ $ f0 T4]),
            (Word [SmbQ $ e_e' (k + 1), SmbQ $ e_x' (k + 1)], Word [SmbQ $ e' (k + 1) T4, SmbQ $ x' (k + 1) T4]),
            (Word [SmbQ $ e_e i], Word [SmbQ $ e i T4]),
            (Word [SmbQ $ e_x i, SmbQ $ e_f (getFromJ i) i, SmbQ $ e_e' i, SmbQ $ e_p i], 
            Word [SmbQ $ x i T4, SmbQ $ f (getFromJ i) i T4, SmbQ $ e' i T4, SmbQ $ p4 T4]),
            (Word [SmbQ $ e_q i, SmbQ $ e_r i, SmbQ $ e_s i, SmbQ $ e_t i, SmbQ $ e_u i, SmbQ $ e_pd i, SmbQ $ e_qd i, SmbQ $ e_rd i, SmbQ $ e_sd i, SmbQ $ e_td i, SmbQ $ e_ud i, SmbQ $ e_f' (getFromJ i) i], 
            Word [SmbQ $ q4 T4, SmbQ $ r4 T4, SmbQ $ s4 T4, SmbQ $ t4 T4, SmbQ $ u4 T4, SmbQ $ pd4 T4, SmbQ $ qd4 T4, SmbQ $ rd4 T4, SmbQ $ sd4 T4, SmbQ $ td4 T4, SmbQ $ ud4 T4, SmbQ $ f' (getFromJ i) i T4])]
            $ concat 
            [[(Word [SmbQ $ e_e j], Word [SmbQ $ e j T4]),
            (Word [SmbQ $ e_x j, SmbQ $ e_f (getFromJ j) j, SmbQ $ e_e' j, SmbQ $ e_p j], 
            Word [SmbQ $ x j T4, SmbQ $ f (getFromJ j) j T4, SmbQ $ e' j T4, SmbQ $ p j T4]),
            (Word [SmbQ $ e_q j, SmbQ $ e_r j, SmbQ $ e_s j, SmbQ $ e_t j, SmbQ $ e_u j, SmbQ $ e_pd j, SmbQ $ e_qd j, SmbQ $ e_rd j, SmbQ $ e_sd j, SmbQ $ e_td j, SmbQ $ e_ud j, SmbQ $ e_f' (getFromJ j) j], 
            Word [SmbQ $ q j T4, SmbQ $ r j T4, SmbQ $ s j T4, SmbQ $ t j T4, SmbQ $ u j T4, SmbQ $ pd j T4, SmbQ $ qd j T4, SmbQ $ rd j T4, SmbQ $ sd j T4, SmbQ $ td j T4, SmbQ $ ud j T4, SmbQ $ f' (getFromJ j) j T4])]
            | j <- [1 .. i - 1] ++ [i + 1 .. k]]

    let rule4alpha =
            SRule $ (++)
            [(Word [SmbQ $ x 0 T4, SmbQ $ f0 T4], Word [SmbY' alpha, SmbQ $ x 0 TAlpha, SmbQ $ f0 TAlpha]),
            (Word [SmbQ $ e' (k + 1) T4, SmbQ $ x' (k + 1) T4], Word [SmbQ $ e' (k + 1) TAlpha, SmbQ $ x' (k + 1) TAlpha, SmbY' omega]),
            (Word [SmbQ $ e i T4], Word [SmbQ $ e i TAlpha]),
            (Word [SmbQ $ x i T4, SmbQ $ f (getFromJ i) i T4, SmbQ $ e' i T4, SmbQ $ p_idx' "1" i T4], 
            Word [SmbY' $ Y a, SmbQ $ x i TAlpha, SmbQ $ f (getToJ i) i TAlpha, SmbQ $ e' i TAlpha, SmbQ $ p4 TAlpha, SmbY' delta]),
            (Word [SmbQ $ q_idx' "1" i T4, SmbQ $ r_idx' "1" i T4, SmbQ $ s_idx' "1" i T4, SmbQ $ t_idx' "1" i T4, SmbQ $ u_idx' "1" i T4], 
            Word [SmbQ $ q4 TAlpha, SmbQ $ r4 TAlpha, SmbQ $ s4 TAlpha, SmbQ $ t4 TAlpha, SmbQ $ u4 TAlpha]),
            (Word [SmbQ $ e 0 T4], Word [SmbQ $ e 0 TAlpha]),
            (Word [SmbQ $ fl' T4], Word [SmbQ $ fl' TAlpha]),
            (Word [SmbQ $ pd i T4, SmbQ $ qd i T4, SmbQ $ rd i T4, SmbQ $ sd i T4, SmbQ $ td i T4, SmbQ $ ud i T4, SmbQ $ f' (getFromJ i) i T4], 
            Word [SmbQ $ pd i TAlpha, SmbQ $ qd i TAlpha, SmbQ $ rd i TAlpha, SmbQ $ sd i TAlpha, SmbQ $ td i TAlpha, SmbQ $ ud i TAlpha, SmbQ $ f' (getToJ i) i TAlpha])]
            $ concat 
            [[(Word [SmbQ $ e j T4], Word [SmbQ $ e j TAlpha]),
            (Word [SmbQ $ x j T4, SmbQ $ f (getFromJ j) j T4, SmbQ $ e' j T4, SmbQ $ p j T4], 
            Word [SmbQ $ x j TAlpha, SmbQ $ f (getToJ j) j TAlpha, SmbQ $ e' j TAlpha, SmbQ $ p j TAlpha]),
            (Word [SmbQ $ q j T4, SmbQ $ r j T4, SmbQ $ s j T4, SmbQ $ t j T4, SmbQ $ u j T4, SmbQ $ pd j T4, SmbQ $ qd j T4, SmbQ $ rd j T4, SmbQ $ sd j T4, SmbQ $ td j T4, SmbQ $ ud j T4, SmbQ $ f' (getFromJ j) j T4],
            Word [SmbQ $ q j TAlpha, SmbQ $ r j TAlpha, SmbQ $ s j TAlpha, SmbQ $ t j TAlpha, SmbQ $ u j TAlpha, SmbQ $ pd j TAlpha, SmbQ $ qd j TAlpha, SmbQ $ rd j TAlpha, SmbQ $ sd j TAlpha, SmbQ $ td j TAlpha, SmbQ $ ud j TAlpha, SmbQ $ f' (getToJ j) j TAlpha])] 
            | j <- [1 .. i - 1] ++ [i + 1 .. k] ]

    let changeMachine from to =
            (++)
            [(Word [SmbQ $ e i from], Word [SmbQ $ e i to]),
            (Word [SmbQ $ x i from, SmbQ $ f (getToJ i) i from, SmbQ $ e' i from, SmbQ $ p4 from], 
            Word [SmbQ $ x i to, SmbQ $ f (getToJ i) i to, SmbQ $ e' i to, SmbQ $ p4 to]),
            (Word [SmbQ $ q4 from, SmbQ $ r4 from, SmbQ $ s4 from, SmbQ $ t4 from, SmbQ $ u4 from, SmbQ $ pd4 from, SmbQ $ qd4 from, SmbQ $ rd4 from, SmbQ $ sd4 from, SmbQ $ td4 from, SmbQ $ ud4 from, SmbQ $ f' (getToJ i) i from],
            Word [SmbQ $ q4 to, SmbQ $ r4 to, SmbQ $ s4 to, SmbQ $ t4 to, SmbQ $ u4 to, SmbQ $ pd4 to, SmbQ $ qd4 to, SmbQ $ rd4 to, SmbQ $ sd4 to, SmbQ $ td4 to, SmbQ $ ud4 to, SmbQ $ f' (getToJ i) i to])] 
            $ concat
            [[(Word [SmbQ $ e j from], Word [SmbQ $ e j to]),
            (Word [SmbQ $ x j from, SmbQ $ f (getToJ j) j from, SmbQ $ e' j from, SmbQ $ p j from], 
            Word [SmbQ $ x j to, SmbQ $ f (getToJ j) j to, SmbQ $ e' j to, SmbQ $ p j to]),
            (Word [SmbQ $ q j from, SmbQ $ r j from, SmbQ $ s j from, SmbQ $ t j from, SmbQ $ u j from, SmbQ $ pd j from, SmbQ $ qd j from, SmbQ $ rd j from, SmbQ $ sd j from, SmbQ $ td j from, SmbQ $ ud j from, SmbQ $ f' (getToJ j) j from],
            Word [SmbQ $ q j to, SmbQ $ r j to, SmbQ $ s j to, SmbQ $ t j to, SmbQ $ u j to, SmbQ $ pd j to, SmbQ $ qd j to, SmbQ $ rd j to, SmbQ $ sd j to, SmbQ $ td j to, SmbQ $ ud j to, SmbQ $ f' (getToJ j) j to])]
            | j <- [1 .. i - 1] ++ [i + 1 .. k]]

    let rulealphaomega = 
            SRule $ (++)
            [(Word [SmbQ $ e 0 TAlpha], Word [SmbQ $ e 0 TOmega]),
            (Word [SmbQ $ x_idx "2" 0 TAlpha, SmbQ $ f0 TAlpha], Word [SmbQ $ x 0 TOmega, SmbQ $ f0 TOmega]),
            (Word [SmbQ $ e' (k + 1) TAlpha, SmbQ $ x' (k + 1) TAlpha], Word [SmbQ $ e' (k + 1) TOmega, SmbQ $ x' (k + 1) TOmega]),
            (Word [SmbQ $ fl' TAlpha], Word [SmbQ $ fl' TOmega])]
            $ changeMachine TAlpha TOmega

    let ruleomega9 = 
            SRule $ (++)
            [(Word [SmbQ $ e 0 TOmega], Word [SmbQ $ e 0 T9]),
            (Word [SmbQ $ x 0 TOmega, SmbQ $ f0 TOmega], Word [SmbQ $ x 0 T9, SmbQ $ f0 T9]),
            (Word [SmbQ $ e' (k + 1) TOmega, SmbQ $ x_idx' "2" (k + 1) TOmega], Word [SmbQ $ e' (k + 1) TOmega, SmbQ $ x' (k + 1) T9]),
            (Word [SmbQ $ fl' TOmega], Word [SmbQ $ fl' T9])]
            $ changeMachine TOmega T9

    let rule9 = 
            SRule $ (++)
            [(Word [SmbQ $ e 0 T9], Word [SmbQ $ e_e 0]),
            (Word [SmbQ $ x 0 T9, SmbQ $ f0 T9], Word [SmbQ $ e_x 0, SmbQ e_f0]),
            (Word [SmbQ $ e' (k + 1) T9, SmbQ $ x' (k + 1) T9], Word [SmbQ $ e_e' (k + 1), SmbQ $ e_x' (k + 1)]),
            (Word [SmbQ $ fl' T9], Word [SmbQ e_fl']),
            (Word [SmbQ $ e i T9], Word [SmbQ $ e_e i]),
            (Word [SmbQ $ xh i T9, SmbQ $ f (getToJ i) i T9, SmbQ $ e' i T9, SmbQ $ ph_idx "1" i T9], 
            Word [SmbQ $ e_x i, SmbQ $ e_f (getToJ i) i, SmbQ $ e_e' i, SmbQ $ e_p i]),
            (Word [SmbQ $ qh_idx "1" i T9, SmbQ $ rh_idx "1" i T9, SmbQ $ sh_idx "1" i T9, SmbQ $ th_idx "1" i T9, SmbQ $ uh_idx "1" i T9, SmbQ $ phd_idx "0" i T9, SmbQ $ qhd_idx "0" i T9, SmbQ $ rhd_idx "0" i T9, SmbQ $ shd_idx "0" i T9, SmbQ $ thd_idx "0" i T9, SmbQ $ uhd_idx "0" i T9, SmbQ $ f' (getToJ i) i T9],
            Word [SmbQ $ e_q i, SmbQ $ e_r i, SmbQ $ e_s i, SmbQ $ e_t i, SmbQ $ e_u i, SmbQ $ e_pd i, SmbQ $ e_qd i, SmbQ $ e_rd i, SmbQ $ e_sd i, SmbQ $ e_td i, SmbQ $ e_ud i, SmbQ $ e_f' (getToJ i) i])]
            $ concat 
            [[(Word [SmbQ $ e j T9], Word [SmbQ $ e_e j]),
            (Word [SmbQ $ x j T9, SmbQ $ f (getToJ j) j T9, SmbQ $ e' j T9, SmbQ $ p j T9], 
            Word [SmbQ $ e_x j, SmbQ $ e_f (getToJ j) j, SmbQ $ e_e' j, SmbQ $ e_p j]),
            (Word [SmbQ $ q j T9, SmbQ $ r j T9, SmbQ $ s j T9, SmbQ $ t j T9, SmbQ $ u j T9, SmbQ $ pd j T9, SmbQ $ qd j T9, SmbQ $ rd j T9, SmbQ $ sd j T9, SmbQ $ td j T9, SmbQ $ ud j T9, SmbQ $ f' (getToJ j) j T9],
            Word [SmbQ $ e_q j, SmbQ $ e_r j, SmbQ $ e_s j, SmbQ $ e_t j, SmbQ $ e_u j, SmbQ $ e_pd j, SmbQ $ e_qd j, SmbQ $ e_rd j, SmbQ $ e_sd j, SmbQ $ e_td j, SmbQ $ e_ud j, SmbQ $ e_f' (getToJ j) j])] 
            | j <- [1 .. i - 1] ++ [i + 1 .. k] ]

    [rule4, rule4alpha, rulealphaomega, ruleomega9, rule9]

createPos22Rule cmd = do
    let (Command command) = cmd 
    let k = length command
    let (_, i) = getai command
    let getFromJ j = a where (a, _) = getJIdx command j
    let getToJ j = a where (_, a) = getJIdx command j
    SRule $ (++)
        [(Word [SmbQ $ e_e i, SmbQ $ e_x i, SmbQ $ e_f (getFromJ i) i, SmbQ $ e_e' i, SmbQ $ e_p i, SmbQ $ e_q i, SmbQ $ e_r i, SmbQ $ e_s i, SmbQ $ e_t i, SmbQ $ e_u i, SmbQ $ e_pd i, SmbQ $ e_qd i, SmbQ $ e_rd i, SmbQ $ e_sd i, SmbQ $ e_td i, SmbQ $ e_ud i, SmbQ $ e_f' (getFromJ i) i], 
        Word [SmbQ $ e_e i, SmbQ $ e_x i, SmbQ $ e_f (getToJ i) i, SmbQ $ e_e' i, SmbQ $ e_p i, SmbQ $ e_q i, SmbQ $ e_r i, SmbQ $ e_s i, SmbQ $ e_t i, SmbQ $ e_u i, SmbQ $ e_pd i, SmbQ $ e_qd i, SmbQ $ e_rd i, SmbQ $ e_sd i, SmbQ $ e_td i, SmbQ $ e_ud i, SmbQ $ e_f' (getToJ i) i])]
        $ concat 
        [[(Word [SmbQ $ e_f (getFromJ j) j], Word [SmbQ $ e_f (getToJ j) j]),
        (Word [SmbQ $ e_f' (getFromJ j) j], Word [SmbQ $ e_f' (getToJ j) j])] 
        | j <- [1 .. i - 1] ++ [i + 1 .. k]]
    
mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

symmetrization (SRule wordPairs) = do 
    let groupByWord w (left, other) =
            case w of
                smb@(SmbY _) : t -> groupByWord t (smb : left, other)
                smb@(SmbY' _) : t -> groupByWord t (smb : left, other)
                smb@(SmbQ _) : t -> (reverse left, smb : t)
    let reverseYs smb = case smb of
                            SmbY y -> SmbY' y
                            SmbY' y -> SmbY y 
                            _ -> error (show smb)
    let mapWords (Word w1, Word w2) = (Word midle, Word $ reversedLeftYs ++ w1 ++ reversedRightYs)
                                        where 
                                            (left, other1) = groupByWord w2 ([], [])
                                            (right, midle) = mapTuple reverse $ groupByWord (reverse other1) ([], [])
                                            reversedLeftYs = map reverseYs left
                                            reversedRightYs = map reverseYs right

    SRule $ map mapWords wordPairs


smFinal (TMType.TM (inputAlphabet,
        tapeAlphabets, 
        TMType.MultiTapeStates tapesStates, 
        TMType.Commands commandsSet, 
        _,
        _)
        ) = 
    let numOfTapes = length $ trace ("I'm here! 1") tapeAlphabets
        gamma = [T4, T9, TAlpha, TOmega]
        y = map (\(TMType.TapeAlphabet a) -> map (Y $) $ Set.toList a) tapeAlphabets
        getFinalForTape tag = zipWith (\i idxs -> map (\(TMType.State idx) -> State F idx tag $ standardV i) idxs) [1 .. numOfTapes] $ map Set.toList tapesStates
        standatdState name tags = [[State name "" tags (standardV i)]  | i <- [1..numOfTapes]]
        es = (:) [State E "" eTag $ standardV 0] $ standatdState E eTag
        e's = standatdState E quoteTag ++ [[State E "" quoteTag . standardV $ numOfTapes + 1]]
        fs = (:) [State F "" eTag $ standardV 0] $ getFinalForTape eTag
        f's = getFinalForTape quoteTag ++ [[State F "" quoteTag . standardV $ numOfTapes + 1]]
        xs = [[State X "" eTag (standardV i)]  | i <- [0 .. numOfTapes]] ++ [[State X "" quoteTag . standardV $ numOfTapes + 1]]
        [ps,qs,rs,ss,ts,us,pds,qds,rds,sds,tds,uds] = [standatdState name tag | name <- [P, Q, R, S, T, U], tag <- [eTag, dashTag]]
        standardStates = foldr (++) [] [es, e's, fs, f's, xs, ps, qs, rs, ss, ts, us, pds, qds, rds, sds, tds, uds]

        (pos21, pos22, neg21, neg22) = devidePositiveNegativeCommands $ Set.toList commandsSet
        sms = [[f $ Command c | c <- pos21 ] | f <- zipWith copySMForCommand (createSMs $ concat y) gamma]
        smsRules = concatMap srs $ concat $ sms
        groupByStates s1 s2 = s_name e1 == s_name e2 
                                && Set.member Dash tag1 == Set.member Dash tag2 
                                && id1 == id2
                                && (s_name e1 /= E || Set.null tag1 == Set.null tag2) 
                                && (s_name e1 /= F || Set.null tag1 == Set.null tag2)
                                where   e1 = Set.elemAt 0 s1 
                                        e2 = Set.elemAt 0 s2
                                        tag1 = s_tags e1
                                        tag2 = s_tags e2
                                        id1 = tape . fromJust $ s_val e1
                                        id2 = tape . fromJust $ s_val e2
        sortByNames s1 s2 = compare (s_name e1, id1, Set.member Dash tag1, be1, bf1) (s_name e2, id2, Set.member Dash tag2, be2, bf2) 
                                where   e1 = Set.elemAt 0 s1
                                        e2 = Set.elemAt 0 s2 
                                        tag1 = s_tags e1
                                        tag2 = s_tags e2
                                        id1 = tape . fromJust $ s_val e1
                                        id2 = tape . fromJust $ s_val e2
                                        be1 = s_name e1 == E && Set.null tag1 
                                        be2 = s_name e2 == E && Set.null tag2 
                                        bf1 = s_name e1 == F && Set.null tag1 
                                        bf2 = s_name e2 == F && Set.null tag2 
        filterSmsStates s = s_name e /= F
                                where   e = Set.elemAt 0 s
        smsStates = filter filterSmsStates . concat . map (map Set.unions . transpose . map qn) $ trace ("sms " ++ (show sms)) sms
        otherStates = [[s {s_val = Just $ (fromJust $ s_val s) {tmCommand = Just $ Command c, smTag = Just g}} | s <- state ] | g <- gamma, c <- pos21, state <- standardStates]
        finalSmStates = map Set.unions . groupBy groupByStates . sortBy sortByNames $ (map Set.fromList standardStates) ++ (map Set.fromList otherStates) ++ smsStates
        smsConnectingRules = concatMap createConnectingRules $ map (Command $) pos21
        smsPos22Rules = map createPos22Rule $ map (Command $) pos22

        finalSmRules = smsRules ++ smsConnectingRules ++ smsPos22Rules
        symmFinalSmRules = (++) finalSmRules $ map symmetrization finalSmRules
                                
    in
        --SM y (trace ("finalSmsStates " ++ (show finalSmStates)) finalSmStates) (trace ("symmFinalSmRules " ++ (show symmFinalSmRules)) symmFinalSmRules)
        SM y (trace ("finalSmsStates " ++ (show finalSmStates)) finalSmStates) (trace ("symmFinalSmRules " ++ (show symmFinalSmRules)) symmFinalSmRules)
