

;garbage start ==0801==
    1 on i goto 200,300,400,500,600
  100 fori=40960to49151:pokei,peek(i):next
  110 fori=57344to65535:pokei,peek(i):next
  120 i=1:load"garbcol1",8,1
  200 i=i+1:load"garbcol2",8,1
  300 i=i+1:load"garbcol3",8,1
  400 i=i+1:load"garbcol4",8,1
  500 poke53280,13:poke53281,11:poke1,peek(1)and not 2:print"{gry3}";
  510 poke57818,8:rem "load auf ,8 voreingestellt
  520 poke60648,asc("O"):poke60649,34:poke60650,asc("?"):poke60651,asc("*")
  525 poke60652,13:poke60653,asc("r"):poke60654,asc("U"):rem"lO"?* statt "load"
  530 poke46991,0:rem"asc("")=0 (keinfehler!)
  540 poke65130,15:rem"kein cia reset
  550 poke42773,174:poke42774,167:rem"nach list nicht autom. end
  560 i=i+1:load"svop",8,1
  600 poke64822,244:poke64823,207:poke64846,250:poke64847,207:sys65418
  610 rem"save / open mit "@" korrigiert

