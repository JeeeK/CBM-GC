

;installation ==1c01==
   10 forj=1to4
   20 : readn1$,n2$,s1,sl,sh,e1
   30 : sys57812n1$,8,1:poke780,0:sys65493
   40 : open2,8,1,n2$+",p,w"
   50 : print#2,chr$(sl)chr$(sh);
   60 : fori=s1toe1
   70 :   print#2,chr$(peek(i));
   80 : next i
   90 : close2
  100 nextj
  110 data"g1","garbcol1",17652,244,180,17978
  120 data"g2","garbcol2",17594,186,228,17618
  130 data"g3","garbcol3",17524,116,228,17577
  140 data"g4","garbcol4",18113,193,182,18132

