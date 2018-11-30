module TM2SM where

import SMType


delta = Y "delta" 
alpha = Y "alpha" 
omega = Y "omega" 
       
p1 = Q "p1"
p2 = Q "p2"
p3 = Q "p3"       
       
q1 = Q "q1"
q2 = Q "q2"
q3 = Q "q3"       

r1 = Q "r1"
r2 = Q "r2"
r3 = Q "r3"       

s1 = Q "s1"
s2 = Q "s2"
s3 = Q "s3"       

t1 = Q "t1"
t2 = Q "t2"
t3 = Q "t3"

e = Q "E"
e' = Q "E'"
f = Q "F"
f' = Q "F'"
x = Q "x"
x1 = Q "x1"
x2 = Q "x2"
x3 = Q "x3"
x4 = Q "x4'"

x' = Q "x'"
x1' = Q "x1'"
x2' = Q "x2'"



copySM :: SM -> (Q -> Bool) -> String -> SM
copySM sm qFilter c =
   let q = map (map (\q -> case q of Q s | qFilter q -> Q (s ++ c); _ -> q)) (qn sm)
       filterWord (Word w) = Word(map (\s -> case s of SmbQ q@(Q s) | qFilter q -> SmbQ (Q (s ++ c)); _ -> s ) w)
       prog = map (\ (SRule l) -> SRule(map (\(w1, w2) -> (filterWord w1, filterWord w2)) l)) (srs sm)
   in
   SM (n sm) (yn sm) q prog

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
            (Word [SmbQ t1], Word [SmbQ t2])
          ]
          
       rl3 = 
          SRule [ 
            (Word [SmbQ p1, SmbY delta, SmbQ q1], Word [SmbQ p3, SmbY delta, SmbQ q3]),
            (Word [SmbQ r1], Word [SmbQ r3]),
            (Word [SmbQ s1], Word [SmbQ s3]),
            (Word [SmbQ t1], Word [SmbQ t3])

          ]
 
    in
    SM (N 4) [[delta],[delta],[delta],[delta]] [[p1,p2,p3],[q1,q2,q3],[r1,r2,r3],[s1,s2,s3],[t1,t2,t3]] [rl1,rl2,rl3]

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
            (Word [SmbQ t2], Word [SmbQ t1])
          ]

    in
    SM (N 4) [[delta],[delta],[delta],[delta]] [[p1,p2],[q1,q2],[r1,r2],[s1,s2],[t1,t2]] [rl1,rl2]


sm3 :: SM
sm3 = SM (N 4) (yn sm1) (qn sm1) ((srs sm1) ++ (srs sm2))

sm4 :: SM
sm4 =
    let sm3' = copySM sm3 (\(Q s) -> notElem s ["p3","q3","r3","s3","t3"]) "'"
    in  
    SM (N 4) (yn sm1) (qn sm3 ++ qn sm3') (srs sm3 ++ srs sm3')


sm4d :: SM
sm4d = copySM sm4 (\ _ -> True) "-"

sm5 :: [Y] -> SM
sm5 y =
    let genQs q = map (\q -> Q q) [q ++ "0", q ++ "1", q ++ "2", q ++ "3", q ++ "4", q ++ "0'", q ++ "1'", q ++ "2'"]
        qs = [[e], [x,x4], [f], [e'], genQs "p", genQs "r", genQs "s", genQs "t", genQs "u", genQs "p-", genQs "r-", genQs "s-", genQs "t-", genQs "u-", [f']]
        ys = [y,y,[],[],[delta],[delta],[delta],[delta],[delta],[],[delta],[delta],[delta],[delta],[delta],[]]
        prg = map (\a -> SRule[ (Word [SmbQ x], Word [SmbY' a, SmbQ x, SmbY a]),
                                (Word [SmbQ (Q "p1'")], Word [SmbQ (Q "p0'")])
                                --(),
                              ]) y
    in 
    SM (N 4) ys qs prg

sm6 :: [Y] -> SM
sm6 y =
    let sm5' = sm5 y 
    in
    SM (N 4) (yn sm5') (qn sm5') []

sm7 :: [Y] -> SM
sm7 y =
    let sm5' = sm5 y 
    in 
    SM (N 4) (yn sm5') (qn sm5') []

sm8 :: [Y] -> SM
sm8 y =
    let sm5' = sm5 y 
    in  
    SM (N 4) (yn sm5') (qn sm5') ((srs sm4) ++ (srs sm5') ++ (srs sm4d) ++ (srs (sm6 y)) ++ (srs (sm7 y)))

sm9 :: [Y] -> SM
sm9 y = 
    let sm8' = sm8 y 
        sm8c = copySM sm8' (\(Q s) -> (notElem s ["E","E'","F","F'"]) || (notElem '4' s)) "^"
    in  
    SM (N 4) (yn sm1) (qn sm8' ++ qn sm8c) (srs sm8' ++ srs sm8c)

smAlpha :: SM
smAlpha = SM (N 4) [[alpha],[alpha]] [[e],[x,x1,x2],[f]] [SRule [(Word [SmbQ x], Word [SmbY' alpha, SmbQ x, SmbY alpha]),
                                                                 (Word [SmbQ e, SmbQ x], Word [SmbQ e, SmbQ x1]),
                                                                 (Word [SmbQ x1], Word [SmbY alpha, SmbQ x1, SmbY' alpha]),
                                                                 (Word [SmbQ x1, SmbQ f], Word [SmbQ x2, SmbQ f])]]

smOmega :: SM
smOmega = SM (N 4) [[omega],[omega]] [[e'],[x',x1',x2'],[f']] [SRule [(Word [SmbQ x'], Word [SmbY omega, SmbQ x', SmbY' omega]),
                                                                      (Word [SmbQ x', SmbQ f'], Word [SmbQ x1', SmbQ f']),
                                                                      (Word [SmbQ x1'], Word [SmbY' omega, SmbQ x1', SmbY omega]),
                                                                      (Word [SmbQ e', SmbQ x1'], Word [SmbQ e', SmbQ x2'])]]

