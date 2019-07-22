module TM2SM where

import SMType
import Data.Set (Set)
import qualified Data.Set as Set


devidePositiveNegativeCommands :: [[TapeCommand]] -> ([[TapeCommand]], [[TapeCommand]])
devidePositiveNegativeCommands commands = do
    let check21 command = 
            case command of
                PreSMCommand((a, b),(a1, b1)) : t 
                    | a /= emptySymbol && a /= "E" -> True
                    | otherwise -> check21 t
                [] -> False

    let reverseCommand (PreSMCommand((a, b),(a1, b1))) =  PreSMCommand((a1, b1),(a, b))

    let devidePositiveNegativeCommandsInternal commands (accS, accA) =
            case commands of
                h : t 
                    | check21 h || List.notElem (map reverseCommand h) accS -> devidePositiveNegativeCommandsInternal t (h : accS, accA)
                    | otherwise -> devidePositiveNegativeCommandsInternal t (accS, h : accA)
                [] -> (accS, accA)
    devidePositiveNegativeCommandsInternal commands ([], [])

delta = Y "delta" 
alpha = Y "alpha" 
omega = Y "omega" 
       
eTag = Set.fromList []

standardV i = StateVal i Nothing Nothing

gen name j cmd s = [ State name i eTag (StateVal j (Just cmd) (Just s)) | i <- [0..4] ]

addTag newTag q j cmd s =  
      let _q = q j cmd s 
      in
      _q {s_tags = Set.insert newTag (s_tags _q) }
addTags newTags qs = [addTag newTag p | p <- qs, newTag <- newTags]
        
copySM :: SM -> (State -> Bool) -> Tag -> SM
copySM sm qFilter newTag =   
   let q = map (map (addTag newTag)) (qn sm)
       filterWord (Word w) = Word(map (\s -> case s of SmbQ q | qFilter q -> SmbQ (addTag newTag q); _ -> s ) w)
       prog = map (\ (SRule l) -> SRule(map (\(w1, w2) -> (filterWord w1, filterWord w2)) l)) (srs sm)
   in
   SM (yn sm) q prog


createSMs y j cmd =
   let ps@[p0,p1,p2,p3,p4] = gen P j cmd 
       p's@[p0',p1',p2',p3',p4'] = addTags [Quote] ps
       pds@[p0d,p1d,p2d,p3d,p4d] = addTags [Dash] ps
       p'ds@[p0'd,p1'd,p2'd,p3'd,p4'd] = addTags [Dash] p's
       
       qs@[q0,q1,q2,q3,q4] = gen Q j cmd 
       q's@[q0',q1',q2',q3',q4'] = addTags [Quote] qs
       qds@[q0d,q1d,q2d,q3d,q4d] = addTags [Dash] qs
       q'ds@[q0'd,q1'd,q2'd,q3'd,q4'd] = addTags [Dash] q's

       rs@[r0,r1,r2,r3,r4] = gen R j cmd 
       r's@[r0',r1',r2',r3',r4'] = addTags [Quote] rs
       rds@[r0d,r1d,r2d,r3d,r4d] = addTags [Dash] rs
       r'ds@[r0'd,r1'd,r2'd,r3'd,r4'd] = addTags [Dash] r's

       ss@[s0,s1,s2,s3,s4] = gen S j cmd 
       s's@[s0',s1',s2',s3',s4'] = addTags [Quote] ss
       sds@[s0d,s1d,s2d,s3d,s4d] = addTags [Dash] ss
       s'ds@[s0'd,s1'd,s2'd,s3'd,s4'd] = addTags [Dash] s's

       ts@[t0,t1,t2,t3,t4] = gen T j cmd 
       t's@[t0',t1',t2',t3',t4'] = addTags [Quote] ts
       tds@[t0d,t1d,t2d,t3d,t4d] = addTags [Dash] ts
       t'ds@[t0'd,t1'd,t2'd,t3'd,t4'd] = addTags [Dash] t's

       us@[u0,u1,u2,u3,u4] = gen U j cmd 
       u's@[u0',u1',u2',u3',u4'] = addTags [Quote] us
       uds@[u0d,u1d,u2d,u3d,u4d] = addTags [Dash] us
       u'ds@[u0'd,u1'd,u2'd,u3'd,u4'd] = addTags [Dash] u's

       xs@[x0,x1,x2,x3,x4] = gen X j cmd 
       x's@[x0',x1',x2',x3',x4'] = addTags [Quote] xs
       xds@[x0d,x1d,x2d,x3d,x4d] = addTags [Dash] xs
       x'ds@[x0'd,x1'd,x2'd,x3'd,x4'd] = addTags [Dash] x's

       e  s = State E 0 eTag (StateVal j (Just cmd) (Just s))  
       e' s = State E 0 (Set.fromList [Quote]) (StateVal j (Just cmd) (Just s)) 
       f  s = State F 0 eTag (StateVal j (Just cmd) (Just s)) 
       f' s = State F 0 (Set.fromList [Quote]) (StateVal j (Just cmd) (Just s)) 
 
       sm1 =
           let
              rl1 = 
                 SRule [ 
                   (Word [SmbQ q1], Word [SmbY' delta, SmbY' delta, SmbQ q2, SmbY delta, SmbY delta]),
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
           SM [[delta],[delta],[delta],[delta]] [[p1,p2,p3],[q1,q2,q3],[r1,r2,r3],[s1,s2,s3],[t1,t2,t3],[u1,u2,u3]] [rl1,rl2,rl3]
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
           SM [[delta],[delta],[delta],[delta]] [[p1,p2],[q1,q2],[r1,r2],[s1,s2],[t1,t2],[u1,u2]] [rl1,rl2]
       sm3 = SM (yn sm1) (qn sm1) ((srs sm1) ++ (srs sm2))
       sm4 =
           let sm3' = copySM sm3 (\q -> s_id q == 3) Quote
           in  
           SM (yn sm1) (qn sm3 ++ qn sm3') (srs sm3 ++ srs sm3')
       sm4d = copySM sm4 (\ _ -> True) Dash
       sm5 y j cmd s =
           let st = [[_e], [_x0,_x4], [_f], [_e'], 
                     (map (\x -> x j cmd s) ps) ++ [_p0',_p1',_p2'],
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
                     (map (\x -> x j cmd s) uds) ++ [_u0'd,_u1'd,_u2'd],
                     [_f']]
               ys = [y,y,[],[],[delta],[delta],[delta],[delta],[delta],[],[delta],[delta],[delta],[delta],[delta],[]]
               prg = map (\a -> SRule[ (Word [SmbQ _x0], Word [SmbY' a, SmbQ _x0, SmbY a]),
                                       (Word [SmbQ _p1'], Word [SmbQ _p0']),
                                       (Word [SmbQ _q1', SmbQ _r1', SmbQ _s1', SmbQ _t1', SmbQ _u1', SmbQ _p0d],
                                        Word [SmbY' delta, SmbQ _q0', SmbQ _r0', SmbQ _s0', SmbQ _t0', SmbQ _u0', SmbQ _p1d, SmbY delta]),
                                       (Word [SmbQ _q0d, SmbQ _r0d, SmbQ _s0d, SmbQ _t0d, SmbQ _u0d],
                                        Word [SmbQ _q1d, SmbQ _r1d, SmbQ _s1d, SmbQ _t1d, SmbQ _u1d])
                                     ]) y
           in 
           SM ys st prg

       sm6 y j cmd s =
            let sm5' = sm5 y j cmd s 
                prg = [SRule[ (Word [SmbQ _p0'], Word [SmbQ _p1]),
                             (Word [SmbQ _q0', SmbQ _r0', SmbQ _s0', SmbQ _t0', SmbQ _u0', SmbQ _p1'd],
                              Word [SmbQ _q1, SmbQ _r1, SmbQ _s1, SmbQ _t1, SmbQ _u1 ,SmbQ _p0d]),
                             (Word [SmbQ _q1'd, SmbQ _r1'd, SmbQ _s1'd, SmbQ _t1'd, SmbQ _u1'd],
                              Word [SmbQ _q0d, SmbQ _r0d, SmbQ _s0d, SmbQ _t0d, SmbQ _u0d]) ]]
           in
           SM (yn sm5') (qn sm5') prg
  
       sm7 y j cmd s =
           let sm5' = sm5 y j cmd s
               prg = [SRule[ (Word [SmbQ _e, SmbQ _x0], Word [SmbQ _e, SmbQ _x4]),
                             (Word [SmbQ _p1, SmbQ _q1, SmbQ _r1, SmbQ _s1, SmbQ _t1, SmbQ _u1, SmbQ _p0d],
                              Word [SmbQ _p4, SmbQ _q4, SmbQ _r4, SmbQ _s4, SmbQ _t4, SmbQ _u4, SmbQ _p4d]),
                             (Word [SmbQ _q0d, SmbQ _r0d, SmbQ _s0d, SmbQ _t0d, SmbQ _u0d],
                              Word [SmbQ _q4d, SmbQ _r4d, SmbQ _s4d, SmbQ _t4d, SmbQ _u4d]) ]]

           in 
           SM (yn sm5') (qn sm5') prg

       sm8 y j cmd s =
           let sm5' = sm5 y j cmd s
           in  
           SM (yn sm5') (qn sm5') (srs sm4 ++ srs sm5' ++ srs sm4d ++ srs (sm6 y j cmd s) ++ srs (sm7 y j cmd s))

       sm9 y = 
           let sm8' = sm8 y T9
               sm8c = copySM sm8' (\q -> (notElem (s_name q) [E, F]) || (4 /= (s_id q))) Hat
           in  
           SM (yn sm1) (qn sm8' ++ qn sm8c) (srs sm8' ++ srs sm8c)

       smAlpha j cmd =
          SM [[alpha],[alpha]] [[_e],[_x0,_x1,_x2],[_f]] [SRule [(Word [SmbQ _x0], Word [SmbY' alpha, SmbQ _x0, SmbY alpha]),
                                                                   (Word [SmbQ _e, SmbQ _x0], Word [SmbQ _e, SmbQ _x1]),
                                                                   (Word [SmbQ _x1], Word [SmbY alpha, SmbQ _x1, SmbY' alpha]),
                                                                   (Word [SmbQ _x1, SmbQ _f], Word [SmbQ _x2, SmbQ _f])]]

       smOmega j cmd =
          let [_e',_x0',_x1',_x2',_f'] = map (\ x -> x TOmega) [e',x0',x1',x2',f'] 
          in
             SM [[omega],[omega]] [[e' TOmega],[x0',x1',x2'],[f']] [SRule [(Word [SmbQ x0'], Word [SmbY omega, SmbQ x0', SmbY' omega]),
                                                                           (Word [SmbQ x0', SmbQ f'], Word [SmbQ _x1', SmbQ _f']),
                                                                           (Word [SmbQ _x1'], Word [SmbY' omega, SmbQ _x1', SmbY omega]),
                                                                           (Word [SmbQ _e', SmbQ _x1'], Word [SmbQ _e', SmbQ _x2'])]]
   in
      (sm4,sm9,smAlpha,smOmega)
   
smFinal tm = 
   let numOfTapes = 2
       gamma = [T4, T9, TAlpha, TOmega]
       commands = []
       y = []
       getFinalForTape i (Just tag) = State F "" (Set.fromList [tag]) $ standardV i
       getFinalForTape i Nothing = State F "" eTag $ standardV i
       standatdState name tags = [[State name 0 tags (standardV i)]  | i <- [1..numOfTapes]]
       standatdState' name tags = [[State name 0 tags (standardV i)]  | i <- [0..numOfTapes + 1]]
       es = standatdState' E eTag
       e's = standatdState' E (Set.fromList [Quote])
       fs = [ getFinalForTape i Nothing | i <- [0..numOfTapes + 1]]
       f's = [ getFinalForTape i $ Just Quote | i <- [0..numOfTapes + 1]]
       xs = standatdState X eTag
       ps = standatdState P eTag
       qs = standatdState Q eTag
       rs = standatdState R eTag
       ss = standatdState S eTag
       ts = standatdState T eTag
       us = standatdState U eTag
       pds = standatdState P (Set.fromList [Dash])
       qds = standatdState Q (Set.fromList [Dash])
       rds = standatdState R (Set.fromList [Dash])
       sds = standatdState S (Set.fromList [Dash])
       tds = standatdState T (Set.fromList [Dash])
       uds = standatdState U (Set.fromList [Dash])
       
       
       
   in
   SM y [] []
