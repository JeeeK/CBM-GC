    0 rem garbage-collection-demo
    1 ifpeek(50433)<>71orpeek(50434)<>67thenload"blodgc",8,1
    2 sys50432
    3 goto10
    7 t=fnt(peek(d+11))*10000+fnt(peek(d+10))*100+fnt(peek(d+9))+fnt(peek(d+8))/10
    8 print t:return
    9 d=56576:poked+14,128orpeek(d+14):poked+15,127andpeek(d+15):poked+11,0:poked+10,0:poked+9,0:poked+8,0:return
   10 a=700
   11 deffnf(x)=fre(m)-65536*(fre(m)<0)
   12 deffnt(x)=int(x/16)*10+(xand15)
   13 dimt,d,e,e1,e2,m,l,a$,s,a,b,s$,rt,i,j
   14 gosub1000
   19 :
   30 l=40:ifty=5thenifpeek(224)=0thenl=80:rem pet
   32 ifty=6thenifpeek(215)and128thenl=80:rem c128
   33 ifty=1orty=6thenrt=1
   35 m=1:ifty=0orty>6thenm=2:l=80: rem cbm2 (!500er)
   38 s$=chr$(1)
   39 :
   40 print"{clr}";
   41 print"garbage collection demo"
   42 print
   50 e=fnf(1):a$=chr$(45):e1=e-fnf(1)-1
   51 e=fnf(1):dima$(a):e2=(e-fnf(1)-7)/(a+1)
   52 print "string overhead:"e1
   53 print "array element length:"e2
   54 e=e1+e2
   55 s=(fnf(1)-(a-99)*(14+e1)-90*(13+e1)-9*(12+e1))-7*1-5-20
   56 rem 7*1: 1 array, 5: s(0)
   57 print "element overhead:"e
   58 print "memory reduced by:"s
   60 dima(s/5):rem shorten stringheap
   65 s$=right$("     ",1+s-int(s/5)*5)
   68 print"free string memory:"fnf(1)
   99 :
  100 print"{home}";
  105 fori=1toint((l*25-a)/l):print:next
  110 b=(l*25-a)-(i-1)*l:ifb=0then120
  115 fori=1tob:print" ";:next
  120 ti$="000000":ifrtthengosub9
  130 fori=1toa:a$(i)=str$(i):forj=1to10:a$(i)=a$(i)+a$:next:print".";:next
  140 print
  150 ifrtthenprint"rtc:";:gosub7
  160 print"ti:"ti;"ti$: "ti$,"free:"fnf(1)
  170 geta$:ifa$=""goto170
  180 print
  190 fori=1toa:printa$(i);:next
  199 end
  200 :
  201 rem for different timing variants
  202 rem replace line 130 with
  203 rem  a) 230 - empty measurement w/o gc
  204 rem  b) 330 - with gc
  210 :
  230 a$="":fori=1toa:a$(1)=str$(i):forj=1to10:a$(i)=a$(i)+a$:next:print".";:next
  300 :
  330 fori=1toa:a$(i)=str$(i):forj=1to10:a$(i)=a$(i)+a$:next:print".";:next
  340 :
  999 end
 1000 rem hardware detection
 1010 bs=0:ty=0:fori=1to1e4:read bp,bv
 1020 if bp=0 then return
 1030 b=peek(bp+1)*256+peek(bp)
 1080 if b=bv then bs=b:ty=i
 1090 next:return
 1100 rem basic-ram start pointer,value
 1110 data 43,2049 : rem 1: c64
 1120 data 43,4609 : rem 2: expanded vic20
 1130 data 43,1025 : rem 3: expanded vic20 (+3k)
 1140 data 43,4097 : rem 4: c16,plus4,unexpanded vic20
 1150 data 40,1025 : rem 5: pet
 1160 data 45,7169 : rem 6: c128
 1170 data 45,768 : rem 7: cbm2/cbm610,cbm2/cbm510 ?!
 1180 data 0,0 : rem endmarker

