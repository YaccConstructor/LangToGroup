module TM2SM where

import SMType


sm1 :: SM
sm1 =
    let     
       delta = Y "delta" 
       
       p1 = Q "p1"
       p2 = Q "p2"
       p3 = Q "p3"       
       
       q1 = Q "q1"
       q2 = Q "q2"
       q3 = Q "q3"       
       
       r1 = Q "r1"
       r2 = Q "r2"
       r3 = Q "r3"       

       s1 = Q "t1"
       s2 = Q "t2"
       s3 = Q "t3"       

       t1 = Q "t1"
       t2 = Q "t2"
       t3 = Q "t3"       

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
    SM(N 4, Yn [[delta],[delta],[delta],[delta]], Qn [[p1,p2,p3],[q1,q2,q3],[r1,r2,r3],[s1,s2,s3],[t1,t2,t3]], SRules [rl1,rl2,rl3])

sm2 :: SM 
sm2 = SM(N 1, Yn [], Qn [], SRules [])

sm3 :: SM
sm3 = SM(N 1, Yn [], Qn [], SRules [])

sm4 :: SM
sm4 = SM(N 1, Yn [], Qn [], SRules [])

sm5 :: SM
sm5 = SM(N 1, Yn [], Qn [], SRules [])

sm6 :: SM
sm6 = SM(N 1, Yn [], Qn [], SRules [])

sm7 :: SM
sm7 = SM(N 1, Yn [], Qn [], SRules [])

sm8 :: SM
sm8 = SM(N 1, Yn [], Qn [], SRules [])

sm9 :: SM
sm9 = SM(N 1, Yn [], Qn [], SRules [])

smAlpha :: SM
smAlpha = SM(N 1, Yn [], Qn [], SRules [])

smOmega :: SM
smOmega = SM(N 1, Yn [], Qn [], SRules [])
