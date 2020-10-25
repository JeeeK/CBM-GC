    1 rem gc-test
    2 rem 2013,2014-08-30 johann@klasek.at
    3 ifpeek(50433)<>71orpeek(50434)<>67thenload"supergc",8,1
    4 sys50432
    5 dimi,b$,s
    6 m=5000
    7 dima$(m)
   10 s=1:iflen(ti$)=7thens=2:rem cbmii
   20 print "{clr}freier stringspeicher: "fre(s)-65536*(fre(s)<0)
   30 b$=chr$(65)
   40 print "array setup ..."
   50 fori=1tom:a$(i)=b$:next
   60 print "garbage collection ..."
   70 ti$="000000"
   80 print fre(s)"bytes frei"
   90 print "dauer (ticks/hhmmss) : "str$(ti)"/"ti$
