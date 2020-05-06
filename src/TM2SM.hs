{-# LANGUAGE LambdaCase #-}

module TM2SM where

import SMType
import qualified Data.Set as Set
import qualified TMType
import TM2SMHelpers
import Data.List (transpose, groupBy, sortBy, length, zip4)
import Data.Maybe (fromJust, catMaybes)
import Helpers

splitPosNegCmds :: [[TMType.TapeCommand]] -> ([[TMType.TapeCommand]], [[TMType.TapeCommand]], [[TMType.TapeCommand]], [[TMType.TapeCommand]])
splitPosNegCmds commands = do
    let check21 command = 
            case command of
                TMType.PreSMCommand((TMType.Value _ _, _),_) : _ -> True
                TMType.PreSMCommand((TMType.ES, _),_) : t -> check21 t
                TMType.PreSMCommand((TMType.E _, _),_) : _ -> False
                TMType.PreSMCommand((TMType.BCommand _, _),(_, _)) : _ -> True
                TMType.PreSMCommand((TMType.PCommand _, _),(_, _)) : _ -> True
                [] -> False
                cmd -> error $ "Must be PreSMCommand: " ++ show cmd

    let reverseCommand (TMType.PreSMCommand((a, b),(a1, b1))) =  TMType.PreSMCommand((a1, b1),(a, b))
        reverseCommand _ = error "Must be PreSMCommand"

    let splitPosNegCmdsInternal cmds (accP21, accP22, accN21, accN22) =
            case cmds of
                h : t 
                    | check21 h -> splitPosNegCmdsInternal t (h : accP21, accP22, accN21, accN22)
                    | check21 reversedH -> splitPosNegCmdsInternal t (accP21, accP22, h : accN21, accN22)
                    | notElem reversedH accP22 -> splitPosNegCmdsInternal t (accP21, h : accP22, accN21, accN22)
                    | otherwise -> splitPosNegCmdsInternal t (accP21, accP22, accN21, h : accN22)
                    where reversedH = map reverseCommand h
                [] -> (accP21, accP22, accN21, accN22)
    splitPosNegCmdsInternal commands ([], [], [], [])


copySMForCommand :: SM -> SMTag -> TMCMD -> SM
copySMForCommand sm tag cmd =
   let q = map (Set.map (addICmdSmTag cmd tag)) (qn sm)
       filterWord (Word w) = Word(map (\case SmbQ smb -> SmbQ (addICmdSmTag cmd tag smb) ; s -> s) w)
       prog = map (\ (SRule l) -> SRule(map (\(w1, w2) -> (filterWord w1, filterWord w2)) l)) (srs sm)
   in
   SM (yn sm) q prog

createSMs :: [Y] -> [SM]
createSMs y =
    let ps@[_,p1,p2,p3,p4] = gen P
        p's@[p0',p1',p2',_,_] = addTags [Quote] ps
        pds@[p0d,p1d,_,_,p4d] = addTags [Dash] ps
        [p0'd,p1'd,p2'd,_,_] = addTags [Dash] p's

        qs@[_,q1,q2,q3,q4] = gen Q 
        q's@[q0',q1',q2',_,_] = addTags [Quote] qs
        qds@[q0d,q1d,_,_,q4d] = addTags [Dash] qs
        [q0'd,q1'd,q2'd,_,_] = addTags [Dash] q's

        rs@[_,r1,r2,r3,r4] = gen R 
        r's@[r0',r1',r2',_,_] = addTags [Quote] rs
        rds@[r0d,r1d,_,_,r4d] = addTags [Dash] rs
        [r0'd,r1'd,r2'd,_,_] = addTags [Dash] r's

        ss@[_,s1,s2,s3,s4] = gen S
        s's@[s0',s1',s2',_,_] = addTags [Quote] ss
        sds@[s0d,s1d,_,_,s4d] = addTags [Dash] ss
        [s0'd,s1'd,s2'd,_,_] = addTags [Dash] s's

        ts@[_,t1,t2,t3,t4] = gen T
        t's@[t0',t1',t2',_,_] = addTags [Quote] ts
        tds@[t0d,t1d,_,_,t4d] = addTags [Dash] ts
        [t0'd,t1'd,t2'd,_,_] = addTags [Dash] t's

        us@[_,u1,u2,u3,u4] = gen U
        u's@[u0',u1',u2',_,_] = addTags [Quote] us
        uds@[u0d,u1d,_,_,u4d] = addTags [Dash] us
        [u0'd,u1'd,u2'd,_,_] = addTags [Dash] u's

        xs@[x0,x1,x2,_,x4] = eTagState X "" : genRange X [1..4]
        [x0',x1',x2',_,_] = addTags [Quote] xs

        e = State E "" eTag Nothing 
        e' = State E "" (Set.fromList [Quote]) Nothing
        f = State F "" eTag Nothing
        f' = State F "" (Set.fromList [Quote]) Nothing

        copySM :: SM -> (State -> Bool) -> Tag -> SM
        copySM sm qFilter newTag =
            let qss = map (Set.map (\x -> if qFilter x then addTag newTag x else x)) $ qn sm
                filterWord (Word w) = Word(map (\s -> case s of SmbQ q | qFilter q -> SmbQ (addTag newTag q); _ -> s ) w)
                prog = map (\ (SRule l) -> SRule(map (\(w1, w2) -> (filterWord w1, filterWord w2)) l)) (srs sm)
            in
                SM (yn sm) qss prog

        sm1 :: SM
        sm1 =
            let
               rl1 = 
                  SRule [ 
                    (Word [SmbQ q1], Word [SmbY' Delta, SmbY' Delta, SmbQ q1, SmbY Delta, SmbY Delta]),
                    (Word [SmbQ r1], Word [SmbY' Delta, SmbQ r1, SmbY Delta])
                  ]
               rl2 = 
                  SRule [ 
                    (Word [SmbQ p1, SmbQ q1], Word [SmbQ p2, SmbQ q2]),
                    (Word [SmbQ r1], Word [SmbQ r2]),
                    (Word [SmbQ s1], Word [SmbQ s2]),
                    (Word [SmbQ t1], Word [SmbQ t2]),
                    (Word [SmbQ u1], Word [SmbY Delta, SmbQ u2])
                  ]
               rl3 = 
                  SRule [ 
                    (Word [SmbQ p1, SmbY Delta, SmbQ q1], Word [SmbQ p3, SmbY Delta, SmbQ q3]),
                    (Word [SmbQ r1], Word [SmbQ r3]),
                    (Word [SmbQ s1], Word [SmbQ s3]),
                    (Word [SmbQ t1], Word [SmbQ t3]),
                    (Word [SmbQ u1], Word [SmbQ u3])
            
                  ]
            in
            SM [[Delta],[Delta],[Delta],[Delta]] (map Set.fromList [[p1,p2,p3],[q1,q2,q3],[r1,r2,r3],[s1,s2,s3],[t1,t2,t3],[u1,u2,u3]]) [rl1,rl2,rl3]
            
        sm2 :: SM 
        sm2 =
            let     
               rl1 = 
                  SRule [ 
                    (Word [SmbQ q2], Word [SmbY Delta, SmbQ q2, SmbY' Delta]),
                    (Word [SmbQ s2], Word [SmbY' Delta, SmbQ s2, SmbY Delta])
                  ]

               rl2 = 
                  SRule [ 
                    (Word [SmbQ q2, SmbQ r2, SmbQ s2], Word [SmbQ q1, SmbQ r1, SmbQ s1]),
                    (Word [SmbQ p2], Word [SmbQ p1]),
                    (Word [SmbQ t2], Word [SmbQ t1]),
                    (Word [SmbQ u2], Word [SmbQ u1])
                  ]
            
            in
            SM [[Delta],[Delta],[Delta],[Delta]] (map Set.fromList [[p1,p2],[q1,q2],[r1,r2],[s1,s2],[t1,t2],[u1,u2]]) [rl1,rl2]
            
            
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
        sm5 ys =
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
                yss = [ys,ys,[],[],[Delta],[Delta],[Delta],[Delta],[Delta],[],[Delta],[Delta],[Delta],[Delta],[Delta],[]]
                prg = map (\a -> SRule[ (Word [SmbQ x0], Word [SmbY' a, SmbQ x0, SmbY a]),
                                        (Word [SmbQ p1'], Word [SmbQ p0']),
                                        (Word [SmbQ q1', SmbQ r1', SmbQ s1', SmbQ t1', SmbQ u1', SmbQ p0d],
                                         Word [SmbY' Delta, SmbQ q0', SmbQ r0', SmbQ s0', SmbQ t0', SmbQ u0', SmbQ p1d, SmbY Delta]),
                                        (Word [SmbQ q0d, SmbQ r0d, SmbQ s0d, SmbQ t0d, SmbQ u0d],
                                         Word [SmbQ q1d, SmbQ r1d, SmbQ s1d, SmbQ t1d, SmbQ u1d])
                                      ]) y
            in 
            SM yss st prg
            
        sm6 :: [Y] -> SM
        sm6 ys =
            let sm5' = sm5 ys 
                prg = [SRule[ (Word [SmbQ p0'], Word [SmbQ p1]),
                              (Word [SmbQ q0', SmbQ r0', SmbQ s0', SmbQ t0', SmbQ u0', SmbQ p1'd],
                               Word [SmbQ q1, SmbQ r1, SmbQ s1, SmbQ t1, SmbQ u1 ,SmbQ p0d]),
                              (Word [SmbQ q1'd, SmbQ r1'd, SmbQ s1'd, SmbQ t1'd, SmbQ u1'd],
                               Word [SmbQ q0d, SmbQ r0d, SmbQ s0d, SmbQ t0d, SmbQ u0d]) ]]
            in
            SM (yn sm5') (qn sm5') prg
  
        sm7 :: [Y] -> SM
        sm7 ys =
            let sm5' = sm5 ys
                prg = [SRule[ (Word [SmbQ e, SmbQ x0], Word [SmbQ e, SmbQ x4]),
                              (Word [SmbQ p1, SmbQ q1, SmbQ r1, SmbQ s1, SmbQ t1, SmbQ u1, SmbQ p0d],
                               Word [SmbQ p4, SmbQ q4, SmbQ r4, SmbQ s4, SmbQ t4, SmbQ u4, SmbQ p4d]),
                              (Word [SmbQ q0d, SmbQ r0d, SmbQ s0d, SmbQ t0d, SmbQ u0d],
                               Word [SmbQ q4d, SmbQ r4d, SmbQ s4d, SmbQ t4d, SmbQ u4d]) ]]
        
            in 
            SM (yn sm5') (qn sm5') prg

        sm8 :: [Y] -> SM
        sm8 ys =
            let sm5' = sm5 ys
            in  
            SM (yn sm5') (qn sm5') ((srs sm4) ++ (srs sm5') ++ (srs sm4d) ++ (srs (sm6 ys)) ++ (srs (sm7 ys)))
            
        sm9 :: [Y] -> SM
        sm9 ys = 
            let sm8' = sm8 ys 
                sm8c = copySM sm8' (\q -> (notElem (s_name q) [E, F]) && (s_idx q /= "4")) Hat
            in  
            SM (yn sm1) (map Set.unions $ transpose [qn sm8', qn sm8c]) (srs sm8' ++ srs sm8c)
            
        smAlpha :: SM
        smAlpha = SM [[Alpha],[Alpha]] (map Set.fromList [[e],[x0,x1,x2],[f]]) 
                                                                                [SRule [(Word [SmbQ x0], Word [SmbY' Alpha, SmbQ x0, SmbY Alpha])],
                                                                                SRule [(Word [SmbQ e, SmbQ x0], Word [SmbQ e, SmbQ x1])],
                                                                                SRule [(Word [SmbQ x1], Word [SmbY Alpha, SmbQ x1, SmbY' Alpha])],
                                                                                SRule [(Word [SmbQ x1, SmbQ f], Word [SmbQ x2, SmbQ f])]]
            
        smOmega :: SM
        smOmega = SM [[Omega],[Omega]] (map Set.fromList [[e'],[x0',x1',x2'],[f']]) 
                                                                                [SRule [(Word [SmbQ x0'], Word [SmbY Omega, SmbQ x0', SmbY' Omega])],
                                                                                SRule [(Word [SmbQ x0', SmbQ f'], Word [SmbQ x1', SmbQ f'])],
                                                                                SRule [(Word [SmbQ x1'], Word [SmbY' Omega, SmbQ x1', SmbY Omega])],
                                                                                SRule [(Word [SmbQ e', SmbQ x1'], Word [SmbQ e', SmbQ x2'])]]
            
   in
      [sm4, sm9 y, smAlpha, smOmega]
            
genConnectingRules :: TMCMD -> [SRule]
genConnectingRules cmd = do
    let (Command command) = cmd 
    let k = length command    

    let e_f0        = e_f "" 0
    let e_fl'       = e_f' "" (k + 1)

    let x j sm        = SmbQ $ newState X "" eTag j (Just cmd) (Just sm)
    let x' j sm       = SmbQ $ newState X "" quoteTag j (Just cmd) (Just sm)
    let f' idx j sm   = SmbQ $ newState F idx quoteTag j (Just cmd) (Just sm)
    let f idx j sm    = SmbQ $ newState F idx eTag j (Just cmd) (Just sm) 
    let fl' sm        = SmbQ $ newState F "" quoteTag (k + 1) (Just cmd) (Just sm)
    let f0 sm         = SmbQ $ newState F "" eTag 0 (Just cmd) (Just sm) 
    let e' j sm       = SmbQ $ newState E "" quoteTag j (Just cmd) (Just sm)
    let e j sm        = SmbQ $ newState E "" eTag j (Just cmd) (Just sm)
    let p j sm        = SmbQ $ newState P "" eTag j (Just cmd) (Just sm) 
    let q j sm        = SmbQ $ newState Q "" eTag j (Just cmd) (Just sm) 
    let r j sm        = SmbQ $ newState R "" eTag j (Just cmd) (Just sm) 
    let s j sm        = SmbQ $ newState S "" eTag j (Just cmd) (Just sm) 
    let t j sm        = SmbQ $ newState T "" eTag j (Just cmd) (Just sm) 
    let u j sm        = SmbQ $ newState U "" eTag j (Just cmd) (Just sm) 
    let pd j sm       = SmbQ $ newState P "" dashTag j (Just cmd) (Just sm)
    let qd j sm       = SmbQ $ newState Q "" dashTag j (Just cmd) (Just sm)
    let rd j sm       = SmbQ $ newState R "" dashTag j (Just cmd) (Just sm)
    let sd j sm       = SmbQ $ newState S "" dashTag j (Just cmd) (Just sm)
    let td j sm       = SmbQ $ newState T "" dashTag j (Just cmd) (Just sm)
    let ud j sm       = SmbQ $ newState U "" dashTag j (Just cmd) (Just sm)

    let p_index index j sm        = SmbQ $ newState P index eTag j (Just cmd) (Just sm) 
    let q_index index j sm        = SmbQ $ newState Q index eTag j (Just cmd) (Just sm) 
    let r_index index j sm        = SmbQ $ newState R index eTag j (Just cmd) (Just sm) 
    let s_index index j sm        = SmbQ $ newState S index eTag j (Just cmd) (Just sm) 
    let t_index index j sm        = SmbQ $ newState T index eTag j (Just cmd) (Just sm)
    let u_index index j sm        = SmbQ $ newState U index eTag j (Just cmd) (Just sm)
    let x_index index j sm        = SmbQ $ newState X index eTag j (Just cmd) (Just sm)    
    
    let p_index' index j sm        = SmbQ $ newState P index quoteTag j (Just cmd) (Just sm) 
    let q_index' index j sm        = SmbQ $ newState Q index quoteTag j (Just cmd) (Just sm) 
    let r_index' index j sm        = SmbQ $ newState R index quoteTag j (Just cmd) (Just sm) 
    let s_index' index j sm        = SmbQ $ newState S index quoteTag j (Just cmd) (Just sm) 
    let t_index' index j sm        = SmbQ $ newState T index quoteTag j (Just cmd) (Just sm)
    let u_index' index j sm        = SmbQ $ newState U index quoteTag j (Just cmd) (Just sm)
    let x_index' index j sm        = SmbQ $ newState X index quoteTag j (Just cmd) (Just sm)

    let ph_index index j sm        = SmbQ $ newState P index hatTag j (Just cmd) (Just sm) 
    let qh_index index j sm        = SmbQ $ newState Q index hatTag j (Just cmd) (Just sm) 
    let rh_index index j sm        = SmbQ $ newState R index hatTag j (Just cmd) (Just sm) 
    let sh_index index j sm        = SmbQ $ newState S index hatTag j (Just cmd) (Just sm) 
    let th_index index j sm        = SmbQ $ newState T index hatTag j (Just cmd) (Just sm)
    let uh_index index j sm        = SmbQ $ newState U index hatTag j (Just cmd) (Just sm)
    let xh j sm                = SmbQ $ newState X "" hatTag j (Just cmd) (Just sm)

    let phd_index index j sm        = SmbQ $ newState P index hatdashTag j (Just cmd) (Just sm) 
    let qhd_index index j sm        = SmbQ $ newState Q index hatdashTag j (Just cmd) (Just sm) 
    let rhd_index index j sm        = SmbQ $ newState R index hatdashTag j (Just cmd) (Just sm) 
    let shd_index index j sm        = SmbQ $ newState S index hatdashTag j (Just cmd) (Just sm) 
    let thd_index index j sm        = SmbQ $ newState T index hatdashTag j (Just cmd) (Just sm)
    let uhd_index index j sm        = SmbQ $ newState U index hatdashTag j (Just cmd) (Just sm)

    let (a, i) = getai command
    let getFromJ j = state where (state, _) = getJIdx command j
    let getToJ j = state where (_, state) = getJIdx command j

    let p4         = p_index "1" i 
    let q4         = q_index "1" i
    let r4         = r_index "1" i
    let s4         = s_index "1" i 
    let t4         = t_index "1" i
    let u4         = u_index "1" i
    let pd4 sm     = SmbQ $ newState P "0" dashTag i (Just cmd) (Just sm) 
    let qd4 sm     = SmbQ $ newState Q "0" dashTag i (Just cmd) (Just sm) 
    let rd4 sm     = SmbQ $ newState R "0" dashTag i (Just cmd) (Just sm) 
    let sd4 sm     = SmbQ $ newState S "0" dashTag i (Just cmd) (Just sm) 
    let td4 sm     = SmbQ $ newState T "0" dashTag i (Just cmd) (Just sm)
    let ud4 sm     = SmbQ $ newState U "0" dashTag i (Just cmd) (Just sm)

    let rule4 = 
            SRule $ (++) 
            [(Word [e_e 0], Word [e 0 T4]),
            (Word [e_fl'], Word [fl' T4]),
            (Word [e_x 0, e_f0], Word [x 0 T4, f0 T4]),
            (Word [e_e' (k + 1), e_x' (k + 1)], Word [e' (k + 1) T4, x' (k + 1) T4]),
            (Word [e_e i], Word [e i T4]),
            (Word [e_x i, e_f (getFromJ i) i, e_e' i, e_p i], 
            Word [x i T4, f (getFromJ i) i T4, e' i T4, p4 T4]),
            (Word [e_q i, e_r i, e_s i, e_t i, e_u i, e_pd i, e_qd i, e_rd i, e_sd i, e_td i, e_ud i, e_f' (getFromJ i) i], 
            Word [q4 T4, r4 T4, s4 T4, t4 T4, u4 T4, pd4 T4, qd4 T4, rd4 T4, sd4 T4, td4 T4, ud4 T4, f' (getFromJ i) i T4])]
            $ concat 
            [[(Word [e_e j], Word [e j T4]),
            (Word [e_x j, e_f (getFromJ j) j, e_e' j, e_p j], 
            Word [x j T4, f (getFromJ j) j T4, e' j T4, p j T4]),
            (Word [e_q j, e_r j, e_s j, e_t j, e_u j, e_pd j, e_qd j, e_rd j, e_sd j, e_td j, e_ud j, e_f' (getFromJ j) j], 
            Word [q j T4, r j T4, s j T4, t j T4, u j T4, pd j T4, qd j T4, rd j T4, sd j T4, td j T4, ud j T4, f' (getFromJ j) j T4])]
            | j <- [1 .. i - 1] ++ [i + 1 .. k]]

    let rule4alpha =
            SRule $ (++)
            [(Word [x 0 T4, f0 T4], Word [SmbY' Alpha, x 0 TAlpha, f0 TAlpha]),
            (Word [e' (k + 1) T4, x' (k + 1) T4], Word [e' (k + 1) TAlpha, x' (k + 1) TAlpha, SmbY' Omega]),
            (Word [e i T4], Word [e i TAlpha]),
            (Word [x i T4, f (getFromJ i) i T4, e' i T4, p_index' "1" i T4], 
            Word [SmbY' $ Y a, x i TAlpha, f (getToJ i) i TAlpha, e' i TAlpha, p4 TAlpha, SmbY' Delta]),
            (Word [q_index' "1" i T4, r_index' "1" i T4, s_index' "1" i T4, t_index' "1" i T4, u_index' "1" i T4], 
            Word [q4 TAlpha, r4 TAlpha, s4 TAlpha, t4 TAlpha, u4 TAlpha]),
            (Word [e 0 T4], Word [e 0 TAlpha]),
            (Word [fl' T4], Word [fl' TAlpha]),
            (Word [pd4 T4, qd4 T4, rd4 T4, sd4 T4, td4 T4, ud4 T4, f' (getFromJ i) i T4], 
            Word [pd4 TAlpha, qd4 TAlpha, rd4 TAlpha, sd4 TAlpha, td4 TAlpha, ud4 TAlpha, f' (getToJ i) i TAlpha])]
            $ concat 
            [[(Word [e j T4], Word [e j TAlpha]),
            (Word [x j T4, f (getFromJ j) j T4, e' j T4, p j T4], 
            Word [x j TAlpha, f (getToJ j) j TAlpha, e' j TAlpha, p j TAlpha]),
            (Word [q j T4, r j T4, s j T4, t j T4, u j T4, pd j T4, qd j T4, rd j T4, sd j T4, td j T4, ud j T4, f' (getFromJ j) j T4],
            Word [q j TAlpha, r j TAlpha, s j TAlpha, t j TAlpha, u j TAlpha, pd j TAlpha, qd j TAlpha, rd j TAlpha, sd j TAlpha, td j TAlpha, ud j TAlpha, f' (getToJ j) j TAlpha])] 
            | j <- [1 .. i - 1] ++ [i + 1 .. k] ]

    let changeMachine from to =
            (++)
            [(Word [e i from], Word [e i to]),
            (Word [x i from, f (getToJ i) i from, e' i from, p4 from], 
            Word [x i to, f (getToJ i) i to, e' i to, p4 to]),
            (Word [q4 from, r4 from, s4 from, t4 from, u4 from, pd4 from, qd4 from, rd4 from, sd4 from, td4 from, ud4 from, f' (getToJ i) i from],
            Word [q4 to, r4 to, s4 to, t4 to, u4 to, pd4 to, qd4 to, rd4 to, sd4 to, td4 to, ud4 to, f' (getToJ i) i to])] 
            $ concat
            [[(Word [e j from], Word [e j to]),
            (Word [x j from, f (getToJ j) j from, e' j from, p j from], 
            Word [x j to, f (getToJ j) j to, e' j to, p j to]),
            (Word [q j from, r j from, s j from, t j from, u j from, pd j from, qd j from, rd j from, sd j from, td j from, ud j from, f' (getToJ j) j from],
            Word [q j to, r j to, s j to, t j to, u j to, pd j to, qd j to, rd j to, sd j to, td j to, ud j to, f' (getToJ j) j to])]
            | j <- [1 .. i - 1] ++ [i + 1 .. k]]

    let rulealphaomega = 
            SRule $ (++)
            [(Word [e 0 TAlpha], Word [e 0 TOmega]),
            (Word [x_index "2" 0 TAlpha, f0 TAlpha], Word [x 0 TOmega, f0 TOmega]),
            (Word [e' (k + 1) TAlpha, x' (k + 1) TAlpha], Word [e' (k + 1) TOmega, x' (k + 1) TOmega]),
            (Word [fl' TAlpha], Word [fl' TOmega])]
            $ changeMachine TAlpha TOmega

    let ruleomega9 = 
            SRule $ (++)
            [(Word [e 0 TOmega], Word [e 0 T9]),
            (Word [x 0 TOmega, f0 TOmega], Word [x 0 T9, f0 T9]),
            (Word [e' (k + 1) TOmega, x_index' "2" (k + 1) TOmega], Word [e' (k + 1) T9, x' (k + 1) T9]),
            (Word [fl' TOmega], Word [fl' T9])]
            $ changeMachine TOmega T9

    let rule9 = 
            SRule $ (++)
            [(Word [e 0 T9], Word [e_e 0]),
            (Word [x 0 T9, f0 T9], Word [e_x 0,  e_f0]),
            (Word [e' (k + 1) T9, x' (k + 1) T9], Word [e_e' (k + 1), e_x' (k + 1)]),
            (Word [fl' T9], Word [ e_fl']),
            (Word [e i T9], Word [e_e i]),
            (Word [xh i T9, f (getToJ i) i T9, e' i T9, ph_index "1" i T9], 
            Word [e_x i, e_f (getToJ i) i, e_e' i, e_p i]),
            (Word [qh_index "1" i T9, rh_index "1" i T9, sh_index "1" i T9, th_index "1" i T9, uh_index "1" i T9, phd_index "0" i T9, qhd_index "0" i T9, rhd_index "0" i T9, shd_index "0" i T9, thd_index "0" i T9, uhd_index "0" i T9, f' (getToJ i) i T9],
            Word [e_q i, e_r i, e_s i, e_t i, e_u i, e_pd i, e_qd i, e_rd i, e_sd i, e_td i, e_ud i, e_f' (getToJ i) i])]
            $ concat 
            [[(Word [e j T9], Word [e_e j]),
            (Word [x j T9, f (getToJ j) j T9, e' j T9, p j T9], 
            Word [e_x j, e_f (getToJ j) j, e_e' j, e_p j]),
            (Word [q j T9, r j T9, s j T9, t j T9, u j T9, pd j T9, qd j T9, rd j T9, sd j T9, td j T9, ud j T9, f' (getToJ j) j T9],
            Word [e_q j, e_r j, e_s j, e_t j, e_u j, e_pd j, e_qd j, e_rd j, e_sd j, e_td j, e_ud j, e_f' (getToJ j) j])] 
            | j <- [1 .. i - 1] ++ [i + 1 .. k] ]

    [rule4, rule4alpha, rulealphaomega, ruleomega9, rule9]

genPos22Rule :: TMCMD -> SRule
genPos22Rule cmd = do
    let (Command command) = cmd 
    let k = length command
    let (_, i) = getai command
    let getFromJ j = a where (a, _) = getJIdx command j
    let getToJ j = a where (_, a) = getJIdx command j
    SRule $ (++)
        [(Word [e_e i, e_x i, e_f (getFromJ i) i, e_e' i, e_p i, e_q i, e_r i, e_s i, e_t i, e_u i, e_pd i, e_qd i, e_rd i, e_sd i, e_td i, e_ud i, e_f' (getFromJ i) i], 
        Word [e_e i, e_x i, e_f (getToJ i) i, e_e' i, e_p i, e_q i, e_r i, e_s i, e_t i, e_u i, e_pd i, e_qd i, e_rd i, e_sd i, e_td i, e_ud i, e_f' (getToJ i) i])]
        $ concat 
        [[(Word [e_f (getFromJ j) j], Word [e_f (getToJ j) j]),
        (Word [e_f' (getFromJ j) j], Word [e_f' (getToJ j) j])] 
        | j <- [1 .. i - 1] ++ [i + 1 .. k]]

symSM :: SRule -> SRule
symSM (SRule wordPairs) = do 
    let groupByWord w (left, other) =
            case w of
                smb@(SmbY _) : t -> groupByWord t (smb : left, other)
                smb@(SmbY' _) : t -> groupByWord t (smb : left, other)
                smb@( _) : t -> (reverse left, smb : t)
                [] -> error "There is no state in word"
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

sigmaFunc :: [TMType.State] -> [[Smb]] -> SMType.Word
sigmaFunc states u = 
    Word $
    (e_e 0) : alphan ++ [e_x 0, e_f "" 0] ++
    (concat $ 
    map (\(i, TMType.State q, d, w) -> 
        (e_e i) : w ++ [e_x i, e_f q i, e_e' i, e_p i] ++ d 
                    ++ [e_q i, e_r i, e_s i, e_t i, e_u i, e_pd i, e_qd i, e_rd i, e_sd i, e_td i, e_ud i, e_f' q i]) 
    $ zip4 [1..] states deltan u) ++
    [e_e' (k + 1), e_x' (k + 1)] ++ omegan ++ [e_f' "" (k + 1)]
    where 
            un = map length u
            k = length states
            n = foldl (+) 0 un
            alphan = replicate n $ SmbY Alpha
            omegan = replicate n $ SmbY Omega
            deltan = map (\i -> replicate i $ SmbY Delta) un 

renameRightLeftBoundings :: [[TMType.TapeCommand]] -> [[TMType.TapeCommand]]
renameRightLeftBoundings = map (renameRightLeftBoundingsInternal 1 []) 
    where
        f q = TMType.StateOmega q
        renameRightLeftBoundingsInternal i acc command =
            case command of
                TMType.SingleTapeCommand ((l1, s1, _), (l2, s2, _)) : t 
                    | l1 == TMType.LBS -> renameRightLeftBoundingsInternal (i + 1) ((TMType.PreSMCommand ((newRight, newS1), (newRight, newS2))) : acc) t
                    | otherwise -> renameRightLeftBoundingsInternal (i + 1) ((TMType.PreSMCommand ((l1, newS1), (l2, newS2))) : acc) t
                        where   newRight = TMType.E i
                                newS1 = f s1
                                newS2 = f s2
                [] -> reverse acc
                TMType.PreSMCommand _ : _ -> error "PreSMCommand found"

tm2sm :: TMType.TM -> (SM, SMType.Word, [TMType.State])    
tm2sm (TMType.TM (_,
        tapeAlphabets, 
        TMType.MultiTapeStates tapesStates, 
        TMType.Commands commandsSet, 
        TMType.StartStates startStates,
        TMType.AccessStates accessStates)
        ) = 
    let numOfTapes = length tapeAlphabets
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

        commands = renameRightLeftBoundings $ Set.toList commandsSet
        (pos21, pos22, _, _) = splitPosNegCmds $ commands
        sms = [[f $ Command c | f <- zipWith copySMForCommand (createSMs $ (!!) y $ (snd $ getai c) - 1) gamma] | c <- pos21 ]
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
        smsStates = filter filterSmsStates . concatMap (map Set.unions . transpose . map qn) $ transpose sms
        otherStates = [[s {s_val = Just $ (fromJust $ s_val s) {tmCommand = Just $ Command c, smTag = Just g}} | s <- state ] | g <- gamma, c <- pos21, state <- standardStates]
        smsConnectingRules = concatMap genConnectingRules $ map (Command $) pos21
        crStates = groupBy groupByStatesFunc . sortBy sortByNamesStates . concatMap getStatesFromRule $ smsConnectingRules
                where 
                        getStatesFromWord (SmbQ q) = Just q
                        getStatesFromWord _ = Nothing
                        getStatesFromWords (Word l, Word r) = catMaybes $ map getStatesFromWord (l ++ r)
                        getStatesFromRule (SRule r) = concatMap getStatesFromWords r
                        groupByStatesFunc s1 s2 = s_name s1 == s_name s2 
                                            && Set.member Dash tag1 == Set.member Dash tag2 
                                            && id1 == id2
                                            && (s_name s1 /= E || Set.null tag1 == Set.null tag2) 
                                            && (s_name s1 /= F || Set.null tag1 == Set.null tag2)
                                            where   tag1 = s_tags s1
                                                    tag2 = s_tags s2
                                                    id1 = tape . fromJust $ s_val s1
                                                    id2 = tape . fromJust $ s_val s2
                        sortByNamesStates s1 s2 = compare (s_name s1, id1, Set.member Dash tag1, be1, bf1) (s_name s2, id2, Set.member Dash tag2, be2, bf2) 
                                            where   tag1 = s_tags s1
                                                    tag2 = s_tags s2
                                                    id1 = tape . fromJust $ s_val s1
                                                    id2 = tape . fromJust $ s_val s2
                                                    be1 = s_name s1 == E && Set.null tag1 
                                                    be2 = s_name s2 == E && Set.null tag2 
                                                    bf1 = s_name s1 == F && Set.null tag1 
                                                    bf2 = s_name s2 == F && Set.null tag2 
        finalSmStates = map Set.unions . groupBy groupByStates . sortBy sortByNames $ (map Set.fromList standardStates) ++ (map Set.fromList otherStates) ++ smsStates ++ (map Set.fromList crStates)
        smsPos22Rules = map genPos22Rule $ map (Command $) pos22

        finalSmRules = smsRules ++ smsConnectingRules ++ smsPos22Rules
        --symmFinalSmRules = (++) finalSmRules $ map symSM finalSmRules
                                
    in
        (SM y finalSmStates finalSmRules, sigmaFunc accessStates $ replicate numOfTapes [], startStates)
