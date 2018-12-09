module TM2SM where

import SMType
import Data.Set (Set)
import qualified Data.Set as Set


delta = Y "delta" 
alpha = Y "alpha" 
omega = Y "omega" 
       
eTag = Set.fromList []

gen name = [ State name i eTag "" | i <- [0..4] ]

addTag newTag q = q {s_tags = Set.insert newTag (s_tags q) }
addTagLst newTag qs = [addTag newTag p | p <- qs]
addTags newTags qs = [addTag newTag p | p <- qs, newTag <- newTags]
        
ps@[p0,p1,p2,p3,p4] = gen P
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

xs@[x0,x1,x2,x3,x4] = gen X
x's@[x0',x1',x2',x3',x4'] = addTags [Quote] xs
xds@[x0d,x1d,x2d,x3d,x4d] = addTags [Dash] xs
x'ds@[x0'd,x1'd,x2'd,x3'd,x4'd] = addTags [Dash] x's

e = State E 0 eTag "" 
e' = State E 0 (Set.fromList [Quote]) "" 
f = State F 0 eTag "" 
f' = State F 0 (Set.fromList [Quote]) "" 


copySM :: SM -> (State String -> Bool) -> Tag -> SM
copySM sm qFilter newTag =   
   let q = map (map (addTag newTag)) (qn sm)
       filterWord (Word w) = Word(map (\s -> case s of SmbQ q | qFilter q -> SmbQ (addTag newTag q); _ -> s ) w)
       prog = map (\ (SRule l) -> SRule(map (\(w1, w2) -> (filterWord w1, filterWord w2)) l)) (srs sm)
   in
   SM (yn sm) q prog

sm1 :: SM
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
    SM [[delta],[delta],[delta],[delta]] [[p1,p2],[q1,q2],[r1,r2],[s1,s2],[t1,t2],[u1,u2]] [rl1,rl2]


sm3 :: SM
sm3 = SM (yn sm1) (qn sm1) ((srs sm1) ++ (srs sm2))

sm4 :: SM
sm4 =
    let sm3' = copySM sm3 (\q -> s_id q == 3) Quote
    in  
    SM (yn sm1) (qn sm3 ++ qn sm3') (srs sm3 ++ srs sm3')


sm4d :: SM
sm4d = copySM sm4 (\ _ -> True) Dash

sm5 :: [Y] -> SM
sm5 y =
    let st = [[e], [x0,x4], [f], [e'], 
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
        sm8c = copySM sm8' (\q -> (notElem (s_name q) [E, F]) || (4 /= (s_id q))) Hat
    in  
    SM (yn sm1) (qn sm8' ++ qn sm8c) (srs sm8' ++ srs sm8c)

smAlpha :: SM
smAlpha = SM [[alpha],[alpha]] [[e],[x0,x1,x2],[f]] [SRule [(Word [SmbQ x0], Word [SmbY' alpha, SmbQ x0, SmbY alpha]),
                                                                 (Word [SmbQ e, SmbQ x0], Word [SmbQ e, SmbQ x1]),
                                                                 (Word [SmbQ x1], Word [SmbY alpha, SmbQ x1, SmbY' alpha]),
                                                                 (Word [SmbQ x1, SmbQ f], Word [SmbQ x2, SmbQ f])]]

smOmega :: SM
smOmega = SM [[omega],[omega]] [[e'],[x0',x1',x2'],[f']] [SRule [(Word [SmbQ x0'], Word [SmbY omega, SmbQ x0', SmbY' omega]),
                                                                      (Word [SmbQ x0', SmbQ f'], Word [SmbQ x1', SmbQ f']),
                                                                      (Word [SmbQ x1'], Word [SmbY' omega, SmbQ x1', SmbY omega]),
                                                                      (Word [SmbQ e', SmbQ x1'], Word [SmbQ e', SmbQ x2'])]]

--smFinal tm = 
