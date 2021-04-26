    1 rem gc-test
    2 rem 2021-04-25 johann@klasek.at
    3 ifpeek(50433)<>71orpeek(50434)<>67thenload"blodgc",8,1
    4 sys50432
    5 dimi,b$,s
    6 m=5000
    7 dima$(m)
   10 s=1:iflen(ti$)=7thens=2:rem cbmii
   20 print "{clr}free string space:     "fre(s)-65536*(fre(s)<0)
   30 b$=chr$(65)
   40 print "array setup ..."
   50 fori=1tom:a$(i)=b$:next
   60 print "garbage collection ..."
   70 ti$="000000"
   80 print fre(s)"bytes free"
   90 print "time  (ticks/hhmmss) : "str$(ti)"/"ti$
