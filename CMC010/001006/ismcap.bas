10	  ! &
	  ! Program name: ismcap		Compiled with SCALE 0 on V07.0 &
	  ! Decompiled on 24-Nov-16 at 03:11 AM
35	  !
36	  Y$=SYS(CHR$(12%)) &
	\ CHANGE Y$ TO Y% &
	\ PRGNUM$="["+NUM1$(Y%(6%))+","+NUM1$(Y%(5%))+"]" &
	\ PRGNAM$=RAD$(Y%(7%)+SWAP%(Y%(8%)))+RAD$(Y%(9%)+SWAP%(Y%(10%)))+"."+RAD$(Y%(11%)+SWAP%(Y%(12%))) &
	\ DEVNAM$=CHR$(Y%(23%))+CHR$(Y%(24%))+NUM1$(Y%(25%))+":"
1000	  !
1020	  GOSUB 21999 &
	\ IF OPTION%=0% THEN  &
		  PRINT "Options: CP  - Cut and paste using regular isam files" &
		\ PRINT "         SF  - Cut and paste using single isam files" &
		\ PRINT "         CN  - Cut and paste using reqular isam files (No sort) " &
		\ PRINT "         SN  - Cut and paste using single isam files (No sort)" &
		\ PRINT "         END - To end the program" &
		\ PRINT "Selection "; &
		\ INPUT LINE K$
1030	  OPTION%=0% &
	\ K$,OPTION$=CVT$$(LEFT(K$,2%),-1%) &
	\ IF K$="EN" THEN &
		  V%=FNX%("",0%,"") &
	  ELSE &
		  IF K$="CP" OR K$="SF" OR K$="SN" OR K$="CN" THEN &
			  GOTO 6100 &
		  ELSE &
			  GOTO 1020
1040	  K$=FNX$ &
	\ OPTION%=-1% IF K$<>"" &
	\ GOTO 10
6100	  !
6110	  SINGLE%=0% &
	\ SINGLE%=-1% IF K$="SF" OR K$="SN" &
	\ TEMP$="" &
	\ TEMP$="DATA " IF SINGLE%=0% &
	\ PRINT "Name of ";TEMP$;"file to cut from: "; &
	\ INPUT LINE A$ &
	\ A$=CVT$$(A$,4%) &
	\ IF A$="" THEN &
		  GOTO 1000 &
	  ELSE &
		  TEMP$="" &
		\ TEMP$="KEY " IF SINGLE%=0% &
		\ PRINT "Name of ";TEMP$;"file to paste to: "; &
		\ INPUT LINE A1$ &
		\ A1$=CVT$$(A1$,4%)
6115	  INPUT "Record number to start with <1>   ";START.POS% &
	\ INPUT "Record number to end   with <end> ";END.POS%
6120	  Q%=0% &
	\ A%=INSTR(1%,A1$,".")-1% &
	\ A%=LEN(A1$) IF A%=-1% &
	\ A2$=LEFT(A1$,LEN(A1$)-1%)+"1" &
	\ BLOCKSIZE%(I%)=512% FOR I%=1% TO 4% &
	\ OPEN A1$ FOR INPUT AS FILE 3%, RECORDSIZE BLOCKSIZE%(3%) &
	\ OPEN A2$ FOR INPUT AS FILE 4%, RECORDSIZE BLOCKSIZE%(4%) IF SINGLE%=0% &
	\ GOTO 6300 IF STATUS AND 1024% &
	\ A%=FNH%(3%) &
	\ A%=FNH%(4%) IF SINGLE%=0% &
	\ K1%=BLOCKSIZE%(3%)/K5%(3%) &
	\ K2%=BLOCKSIZE%(4%)/K5%(4%) IF SINGLE%=0% &
	\ OPEN A$ FOR INPUT AS FILE 1%, RECORDSIZE BLOCKSIZE%(1%), MODE 8192% &
	\ A%=FNH%(1%) &
	\ START.POS%=1% IF START.POS%=0% &
	\ END.POS%=K1%(1%) IF END.POS%=0% &
	\ Y%=K4%(1%) &
	\ K3%=BLOCKSIZE%(1%)/K5%(1%) &
	\ T=TIME(0%) &
	\ ELAPSE=TIME(1%) &
	\ PRINT "Starting to work. . .  ";
6180	  FOR X%=K4%(1%)+START.POS% TO K4%(1%)+END.POS% &
		\ FIELD #1%, FNC%(K3%,1%,X%,0%,BLOCKSIZE%(1%)) AS E$,K2%(1%) AS L$ &
		\ IF LEFT(L$,K2%(3%)-2%)<>STRING$(K2%(3%)-2%,68%) THEN  &
			  Y%=Y%+1% &
			\ FIELD #3%, FNC%(K1%,3%,Y%+K1%(3%)+K4%(3%),-1%,BLOCKSIZE%(3%)) AS E$,K2%(3%) AS E0$ &
			\ FIELD #4%, FNC%(K2%,4%,Y%+K1%(4%)+K4%(4%),-1%,BLOCKSIZE%(4%)) AS E$,K2%(4%) AS E1$ IF SINGLE%=0% &
			\ LSET E0$=LEFT(L$,K2%(3%)-2%)+CVT%$(Y%+K1%(4%)) &
			\ LSET E1$=L$ IF SINGLE%=0%
6190			  NEXT X% &
	\ PUT #3%, RECORD B%(3%) &
	\ PUT #4%, RECORD B%(4%) IF SINGLE%=0% &
	\ GET #3%, RECORD 1%, COUNT 512% &
	\ FIELD #3%, 6% AS L2$ &
	\ GET #4%, RECORD 1%, COUNT 512% IF SINGLE%=0% &
	\ FIELD #4%, 6% AS E$ IF SINGLE%=0% &
	\ LSET L2$=CVT%$(K1%(3%)+Y%)+MID(L2$,3%,2%)+"U"+RIGHT(L2$,6%) &
	\ LSET E$=CVT%$(K1%(4%)+Y%)+RIGHT(E$,3%) IF SINGLE%=0% &
	\ PUT #3%, RECORD 1%, COUNT 512% &
	\ PUT #4%, RECORD 1%, COUNT 512% IF SINGLE%=0% &
	\ CLOSE 1% &
	\ CLOSE 3% &
	\ CLOSE 4% &
	\ PRINT "Finished." &
	\ PRINT "Elapse time in seconds ";TIME(0%)-T &
	\ PRINT "CPU time in seconds ";(TIME(1%)-ELAPSE)/10. &
	\ Q9$="" &
	\ Q9$=CHR$(13%)+A1$ IF MID(OPTION$,2%,1%)<>"N" &
	\ V%=FNX%(P$,H1%,CORE.COMMON$)
6300	  !
6310	  V$=SYS(CHR$(11%)) &
	\ PRINT  &
	\ PRINT STRING$(3%,7%) &
	\ PRINT "Write privileges are not established for ";A1$ &
	\ PRINT "Try again later.  Aborting.  "; &
	\ INPUT "Type a 'cr' to continue. . . ";K$ &
	\ V%=FNX%("",0%,"")
14000	  !
14010	  DEF FNC%(A%,X%,A1%,Z%,A2%) &
	\ B5%=A1%/A%+1% &
	\ PUT #X%, RECORD B%(X%) IF Z% AND B5%<>B%(X%) AND B%(X%)<>0% &
	\ GET #X%, RECORD B5% IF B5%<>B%(X%)
14015	  B%(X%)=B5% &
	\ FNC%=(A1%-(B5%-1%)*A%)*(A2%/A%) &
	\ FNEND
14020	  DEF FNB1%(X%) &
	\ FNB1%=2%^(INT(LOG10(X%)/-1.6593168083046256e-13)+1%) &
	\ FNEND
14030	  DEF FNH%(A%) &
	\ GET #A%, RECORD 1% &
	\ FIELD #A%, 2% AS E0$,2% AS E1$,1% AS E3$,1% AS E4$ &
	\ K1%(A%)=CVT$%(E0$) &
	\ K2%(A%)=CVT$%(E1$) &
	\ K3%(A%)=ASCII(E3$) &
	\ K4%(A%)=ASCII(E4$) AND (NOT 128%) &
	\ K5%(A%)=FNB1%(K2%(A%)) &
	\ FNEND
14040	  DEF FNE$ &
	\ FNE$=CVT$$(RIGHT(SYS(CHR$(6%)+CHR$(9%)+CHR$(ERR)),3%),4%) &
	\ FNEND
14100	  !
14110	  DEF FNDIR$(Y$) &
	\ Y1$=SYS(CHR$(6%)+CHR$(-23%)+Y$) &
	\ Y1$=MID(Y1$,5.,8%)+STRING$(10%,0%)+MID(Y1$,23%,4%)+STRING$(4%,0%) &
	\ Y1$=SYS(CHR$(6%)+CHR$(17%)+CHR$(255%)+CHR$(255%)+Y1$) &
	\ CHANGE Y1$ TO Y% &
	\ FNDIR$=CHR$(Y%(23%))+CHR$(Y%(24%))+NUM1$(Y%(25%))+":"+"["+NUM1$(Y%(6%))+","+NUM1$(Y%(5%))+"]"+RAD$(Y%(7%)+SWAP%(Y%(8%)))+RAD$(Y%(9%)+SWAP%(Y%(10%)))+"."+RAD$(Y%(11%)+SWAP%(Y%(12%)))+"<"+NUM1$(Y%(15%))+">" &
	\ FNEND
14190	  DIM Y%(30%)
14200	  !
14210	  DEF FNX%(C$,C%,C1$) &
	\ Q0$=SYS(CHR$(7%)) &
	\ Q5%=INSTR(1%,Q0$,CHR$(255%)) &
	\ Q2%=INSTR(1%,Q0$,CHR$(14%)) &
	\ Q2%=Q5%+12% IF Q2%=0% &
	\ Q0$=MID(Q0$+SPACE$(12%),Q5%,Q2%-Q5%) IF Q5% &
	\ Q0$="" IF Q5%=0% &
	\ CLOSE Q1% FOR Q1%=1% TO 12% &
	\ IF C%>=0% THEN  &
		  IF Q9$<>"" THEN  &
			  Q3$=CVT%$(C%)+C$ IF C$<>"" &
			\ Q3$=CVT%$(8100%)+"!MENU.BAC" IF C$="" AND Q5% &
			\ Q3$=Q3$+"  " &
			\ Q9$=Q9$+CHR$(13%)+CHR$(13%)+Q0$ &
			\ Q9$=Q9$+CHR$(14%)+C1$ IF C1$<>"" &
			\ Q$=LEFT(CHR$(LEN(Q3$))+Q3$+Q9$,127%) &
			\ Q$=SYS(CHR$(8%)+Q$) &
			\ PRINT "<->FILES SECURED<->" &
			\ CHAIN "[1,6]FSTSRS.TSK" 0%
14220	  ON ERROR GOTO 19000 &
	\ CLOSE Q1% FOR Q1%=1% TO 12% IF C%<0% &
	\ C%=-C% IF C%<0% &
	\ CHAIN "!MENU.BAC" 0. IF Q5% AND C$="" &
	\ V$=SYS(CHR$(8%)+Q0$+"   "+CHR$(14%)+C1$) &
	\ CHAIN C$ C% IF C$<>"" &
	\ GOTO 32767 &
	\ FNEND
14230	  DEF FNX$ &
	\ Q$=SYS(CHR$(7%)) &
	\ Q1%=INSTR(4%,Q$,CHR$(14%)) &
	\ Q$=RIGHT(Q$,Q1%+1%) &
	\ FNX$=Q$ &
	\ FNEND
19000	  !
19010	  IF ERR=11% AND ERL=14010% THEN  &
		  RESUME 14015
19030	  IF ERR=50% THEN  &
		  PRINT CHR$(7%) &
		\ RESUME 
19040	  IF ERR=5% AND ERL=14110% THEN  &
		  PRINT FNE$;" ";Y$ &
		\ PRINT "Please check input.  Aborting." &
		\ INPUT "Type a 'return' to proceed to the menu. . . ";K$ &
		\ V$=SYS(CHR$(11%)) &
		\ V%=FNX%("",-1%,"")
19100	  V$=SYS(CHR$(11%)) &
	\ PRINT "Error - ";FNE$;" at line ";ERL &
	\ V$=SYS(CHR$(11%)) &
	\ INPUT "Type a 'return' to proceed to the menu. . . ";K$ &
	\ V%=FNX%("",0%,"")
21999	  ON ERROR GOTO 19000 &
	\ RETURN
30000	  !
30010	  GOSUB 21999 &
	\ K$=FNX$ &
	\ A%=INSTR(1%,K$,"!")-1% &
	\ IF A%<>-1% THEN  &
		  OPTION$=LEFT(K$,A%) &
		\ K$=RIGHT(K$,A%+2%)
30020	  A%=INSTR(1%,K$,"/")-1% &
	\ A%=LEN(K$) IF A%=-1% &
	\ A$=LEFT(K$,A%) &
	\ IF A$="" THEN  &
		  PRINT "Error - Unable to determine file to cut.  "+"Aborting." &
		\ V$=SYS(CHR$(11%)) &
		\ INPUT "Type a 'return' to proceed to the menu. . .";K$ &
		\ V%=FNX%("",0%,"")
30030	  K$=RIGHT(K$,A%+2%) &
	\ A%=INSTR(1%,K$,"@")-1% &
	\ A%=LEN(K$) IF A%=-1% &
	\ A1$=LEFT(K$,A%) &
	\ IF A1$="" THEN  &
		  PRINT "Error - Unable to determine file to paste.  "+"Aborting." &
		\ V$=SYS(CHR$(11%)) &
		\ INPUT "Type a 'return' to proceed to the menu. . .";K$ &
		\ V%=FNX%("",0%,"")
30040	  K$=RIGHT(K$,A%+2%) &
	\ A%=INSTR(1%,K$,"$")-1% &
	\ A%=LEN(K$) IF A%=-1% &
	\ K1$=LEFT(K$,A%) &
	\ K$=RIGHT(K$,A%+2%) &
	\ IF K1$="" THEN  &
		  PRINT "Error - Unable to determine start and end "+"position.  Aborting. . . " &
		\ V$=SYS(CHR$(11%)) &
		\ INPUT "Type a 'return' to proceed to the menu. . . ";K$ &
		\ V%=FNX%("",0%,"")
30050	  A%=INSTR(1%,K1$,"-") &
	\ START.POS%=VAL(LEFT(K1$,A%-1%)) IF A% &
	\ END.POS%=VAL(RIGHT(K1$,A%+1%)) IF A% &
	\ START.POS%=1% IF START.POS%=0% OR A%=0%
30060	  IF K$<>"" THEN  &
		  A%=INSTR(1%,K$,"#")-1% &
		\ A%=LEN(K$) IF A%=-1% &
		\ P$=LEFT(K$,A%) &
		\ K$=RIGHT(K$,A%+2%)
30070	  IF K$<>"" THEN  &
		  A%=INSTR(1%,K$,"*")-1% &
		\ A%=LEN(K$) IF A%=-1% &
		\ H1%=VAL(LEFT(K$,A%)) &
		\ K$=RIGHT(K$,A%+2%)
30080	  IF K$<>"" THEN  &
		  CORE.COMMON$="*"+K$
30090	  SINGLE%=0% &
	\ SINGLE%=-1% IF OPTION$="SF" OR OPTION$="SN" &
	\ IF OPTION$="CP" OR OPTION$="SF" OR OPTION$="CN" OR OPTION$="SN" THEN &
		  GOTO 6120 &
	  ELSE &
		  PRINT "Unable to determine the task to be performed.  " &
		\ PRINT "Aborting." &
		\ V$=SYS(CHR$(11%)) &
		\ INPUT "Type a 'return' to proceed to the menu. . . ";K$ &
		\ V%=FNX%("",0%,"")
32767	  END
