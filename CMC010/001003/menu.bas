10	  ! &
	  ! Program name: menu		Compiled with SCALE 0 on V09.7 &
	  ! Decompiled on 24-Nov-16 at 02:11 AM
20	  GOSUB 18000
30	  GOSUB 18100
300	  !
320	  GOTO 340
330	  PRINT  &
	\ PRINT "System - "; &
	\ INPUT #1%, S$ &
	\ S$=CVT$$(S$,36%) &
	\ S$=LEFT(S$,6%)
340	  S$=CVT$$(S$,128%) &
	\ GOSUB 400 &
	\ L%=L% FOR L%=80% UNTIL S$=MID(A0$(L%),2%,LEN(S$)) OR ASCII(A0$(L%))<>255% &
	\ GOSUB 4000 IF ASCII(A0$(L%))<>255%
360	  GOTO 900 IF ASCII(A0$(L%))=255% AND S$<>"" &
	\ PRINT  &
	\ IF S$<>"" THEN  &
		  PRINT "Type <RETURN> for a selections list." &
		\ GOTO 330
380	  PRINT "Special "; IF MID(A0$(1%),2%,5%)="OTHER" &
	\ PRINT "Systems are:" &
	\ FOR L%=80% UNTIL ASCII(A0$(L%))<>255% &
		\ I%=INSTR(1%,A0$(L%),"-") &
		\ I1%=INSTR(1%,A0$(L%),":") &
		\ A$=RIGHT(A0$(L%),I1%+1%) &
		\ PRINT LEFT(MID(A0$(L%),2%,I%-2%),6%);TAB(7%);"- ";CVT$$(LEFT(A$,LEN(A$)-2%),128%) &
	\ NEXT L% &
	\ PRINT "END    - (TO RETURN TO READY)" &
	\ PRINT "OTHER  - (CHANGE TO/FROM SPECIAL SYSTEMS)" &
	\ GOTO 330
400	  !
410	  GOTO 7000 IF LEFT("OTHER",LEN(LEFT(S$,5%)))=LEFT(S$,5%) AND S$<>"" &
	\ IF LEFT(S$,3%)="BYE" THEN  &
		  K$="BYE" &
		\ GOTO 2540
430	  IF S$="END" THEN  &
		  V$=SYS(CHR$(8%)) &
		\ GOTO 10000
499	  RETURN
900	  !
910	  S$=MID(A0$(L%),2%,INSTR(1%,A0$(L%),"-")-2%) &
	\ S$=LEFT(S$,6%)
1000	  !
1005	  L%=L% FOR L%=80% UNTIL MID(A0$(L%),2%,LEN(S$))=S$ &
	\ I%=INSTR(1%,A0$(L%),"-") &
	\ I1%=INSTR(1%,A0$(L%),":") &
	\ I1%=64% IF I1%=0% &
	\ I%=VAL(MID(A0$(L%),I%+1%,I1%-I%-1%)) &
	\ V%=FNK%(L%) UNLESS K%
1006	  IF MID(A0$(L%),I1%+1%,1%)="*" THEN  &
		  L%=I% &
		\ S$="" UNLESS S$="DOCUME" OR S$="DOC" &
		\ S$=S$+"*" IF S$="DOCUME" OR S$="DOC" &
		\ GOTO 2040
1010	  PRINT  &
	\ PRINT CHR$(7%);S$;"> ";
1020	  INPUT LINE #1%, K$ &
	\ K$=CVT$$(K$,36%) &
	\ PRINT  &
	\ GOTO 3000 IF K$="" &
	\ GOSUB 2500
2010	  K1$=LEFT(K$,6%) &
	\ FOR L%=I% UNTIL ASCII(A0$(L%))<>255%
2020		  GOTO 2040 IF MID(A0$(L%),2%,LEN(K1$))=K1$
2030			  NEXT L% &
	\ K1$=K$ IF S$="DOCUME" OR S$="DOC" &
	\ GOTO 5000 IF S$="DOCUME" OR S$="DOC" &
	\ V$=SYS(CHR$(8%)+CHR$(255%)+S$) &
	\ PRINT "Attempting to run ";K$;". . . " &
	\ D%=1%
2035	  GOTO 19999 IF D$(D%)="" &
	\ CHAIN D$(D%)+K$ 0%
2040	  I%=INSTR(1%,A0$(L%),"-") &
	\ I1%=INSTR(1%,A0$(L%),":") &
	\ V%=FNK%(L%) UNLESS K%
2050	  K1$=MID(A0$(L%),I%+1%,I1%-I%-1%+7%*(I1%=0%))
2052	  GOTO 5000 IF LEFT(S$,6%)="DOCUME" OR LEFT(S$,6%)="DOC"
2055	  CLOSE 1% &
	\ CLOSE 12%
2057	  V$=SYS(CHR$(8%)+CHR$(255%)+S$+SPACE$(6%-LEN(S$))) &
	\ D%=1%
2060	  GOTO 19999 IF D$(D%)="" &
	\ CHAIN D$(D%)+K1$ 0% &
	\ STOP
2500	  !
2510	  GOTO 330 IF K$="END" &
	\ IF K$="CZQ" OR K$="EXIT" OR K$="QUIT" THEN  &
		  V$=SYS(CHR$(8%)) &
		\ GOTO 10000
2540	  IF LEFT(K$,3%)="BYE" THEN  &
		  V$=SYS(CHR$(8%)+"F") &
		\ PRINT "Thank you, and "; &
		\ I%=RND*20.+1. &
		\ PRINT "have a good "; IF I%<15% &
		\ PRINT "enjoy your "; IF I%>=15% AND I%<19% &
		\ PRINT "have a great "; IF I%>=19% &
		\ PRINT "morning "; IF TIME(0.)<0. &
		\ PRINT "afternoon "; IF TIME(0.)>0. AND TIME(0.)<0. &
		\ PRINT "evening "; IF TIME(0.)>0. AND TIME(0.)<0. &
		\ PRINT "night "; IF TIME(0.)>0. &
		\ PRINT ". . ." &
		\ CHAIN "$LOGOUT" 0%
2550	  IF LEFT(K$,3%)="CHA" AND INSTR(1%,K$," ")<>0% THEN  &
		  S$=RIGHT(K$,INSTR(1%,K$," ")+1%) &
		\ GOTO 340
2560	  IF K$="ZIP" THEN  &
		  V$=SYS(CHR$(8%)+CHR$(255%)+S$) &
		\ CHAIN "[1,7]ZIP" 0%
2565	  IF K$="LA50" THEN  &
		  V$=SYS(CHR$(8%)+CHR$(255%)+S$) &
		\ CHAIN "$LA50" 0%
2570	  RETURN
3000	  !
3020	  PRINT "Choices are:" &
	\ FOR L%=I% TO 255% &
		\ IF LEFT(A0$(L%),1%)=CHR$(255%) THEN  &
			  J%=INSTR(1%,A0$(L%),"-") &
			\ J1%=INSTR(1%,A0$(L%),":") &
			\ A$=RIGHT(A0$(L%),J1%+1%) &
			\ PRINT "    ";MID(A0$(L%),2%,J%-2%); &
			\ PRINT TAB(10%);" - ";LEFT(A$,LEN(A$)-2%) UNLESS J1%=0% &
			\ PRINT  IF I%=0% &
		\ NEXT L%
3030	  PRINT "    END    - RETURN TO 'SYSTEM' QUESTION"
3040	  PRINT "    BYE    - LOG OUT"
3900	  GOTO 1010
4000	  !
4010	  FOR L%=80% UNTIL L%=99% OR ASCII(A0$(L%))<>255%
4020		  I%=INSTR(1%,A0$(L%),"-") &
		\ I1%=INSTR(1%,A0$(L%),":")
4025		  K$=MID(A0$(L%),2%,I%-2%) &
		\ GOTO 4070 IF K$="DOCUME" OR K$="DOC"
4030		  I1%=I%+4% IF I1%=0% &
		\ I%=VAL(MID(A0$(L%),I%+1%,I1%-I%-1%))
4040		  FOR L1%=I% UNTIL ASCII(A0$(L1%))<>255% OR L1%=255%
4050			  GOTO 4060 IF MID(A0$(L1%),2%,LEN(S$))<>S$ &
			\ I%=INSTR(1%,A0$(L1%),"-") &
			\ I1%=INSTR(1%,A0$(L1%),":") &
			\ V%=FNK%(L1%) UNLESS K% &
			\ I1%=I%+7% IF I1%=0% &
			\ K1$=MID(A0$(L1%),I%+1%,I1%-I%-1%) &
			\ PRINT  &
			\ PRINT LEFT(K$,6%);"> ";MID(A0$(L1%),2%,I%-2%) &
			\ PRINT  &
			\ V$=SYS(CHR$(8%)+CHR$(255%)+K$+SPACE$(6%-LEN(K$))) &
			\ D%=1%
4055			  GOTO 19999 IF D$(D%)="" &
			\ CLOSE 12% &
			\ CHAIN D$(D%)+K1$ 0%
4060				  NEXT L1%
4070			  NEXT L% &
	\ RETURN
5000	  !
5005	  INPUT #1%, "Set Page. . .";K$ &
	\ D%=1%
5010	  GOTO 19999 IF D$(D%)="" &
	\ OPEN D1$(D%)+K1$ FOR INPUT AS FILE 10%
5020	  INPUT LINE #10%, K$ &
	\ PRINT K$; &
	\ GOTO 5020
5030	  CLOSE 10% &
	\ IF S$="DOCUME*" OR S$="DOC*" THEN &
		  GOTO 330 &
	  ELSE &
		  GOTO 1005
7000	  !
7010	  V$=A0$(1%) &
	\ CLOSE 12% &
	\ GOTO 7050 IF MID(V$,2%,5%)<>"OTHER" &
	\ D%=1%
7020	  GOTO 19999 IF D$(D%)="" &
	\ CHAIN D$(D%)+"!MENU" 0% &
	\ STOP
7050	  D%=1%
7060	  GOTO 19999 IF D2$(D%)="" &
	\ OPEN D2$(D%)+"!OTHER.FIL" FOR INPUT AS FILE 12% &
	\ K%=STATUS AND 1024%
7120	  PRINT "NOTE: THESE OPTIONS MAY RETURN TO 'Ready' WHEN ENDED." &
	\ GOTO 330
8000	  !
8100	  !
8110	  S$=SYS(CHR$(7%)) &
	\ I%=INSTR(1%,S$,CHR$(255%)) &
	\ GOTO 20 IF I%=0% &
	\ I1%=INSTR(I%,S$,CHR$(13%)) &
	\ I1%=I%+7% IF I1%=0% &
	\ S$=MID(S$,I%+1%,I1%-I%-1%) &
	\ I%,I1%=0% &
	\ GOTO 20
8180	  I%=I%+1%
8181	  I%=I%+1%
8182	  I%=I%+1%
8183	  I%=I%+1%
8184	  I%=I%+1%
8185	  I%=I%+1%
8186	  I%=I%+1%
8187	  I%=I%+1%
8188	  I%=I%+1%
8189	  I%=I%+1%
8190	  I%=I%+1%
8191	  I%=I%+1%
8192	  I%=I%+1%
8193	  I%=I%+1%
8194	  I%=I%+1%
8195	  I%=I%+1%
8196	  I%=I%+1%
8197	  I%=I%+1%
8198	  I%=I%+1%
8199	  I%=I%+1%
8200	  I%=100%-I% &
	\ GOSUB 18000 &
	\ S$=MID(A0$(I%),2%,INSTR(1%,A0$(I%),"-")-2%) &
	\ I%=0% &
	\ GOTO 30
9000	  !
9010	  DEF FNK%(L%) &
	\ A0%=1%+CVT$%(RIGHT(A0$(L%),63%)) &
	\ A0$(L%)=LEFT(A0$(L%),62%)+CVT%$(A0%) &
	\ FNK%=A0% &
	\ FNEND
10000	  !
10010	  CLOSE 1% &
	\ CLOSE 12% &
	\ GOTO 32767
18000	  !
18010	  RANDOMIZE &
	\ ON ERROR GOTO 19000 &
	\ D%=1%
18030	  OPEN "KB:" FOR INPUT AS FILE 1%
18040	  D$(1%)="PP0:" &
	\ D$(2%)="SY:"
18050	  D1$(1%)="SY:"
18060	  D2$(1%)="SS0:" &
	\ D2$(2%)="SY:"
18080	  GOTO 20000 IF D2$(D%)="" &
	\ OPEN D2$(D%)+"UNIQUE.FIL" FOR INPUT AS FILE 12% &
	\ K%=STATUS AND 1024% &
	\ DIM #12%, A0$(255%)=64%
18090	  RETURN
18100	  !
18110	  OPEN "MESSAGE.FIL" FOR INPUT AS FILE 6% &
	\ PRINT "NOTE: A message from CMC. . ." &
	\ PRINT STRING$(10%,7%)
18120	  INPUT LINE #6%, K$ &
	\ PRINT K$; &
	\ GOTO 18120
18130	  PRINT  &
	\ INPUT "Remove message ";K$ &
	\ KILL "MESSAGE.FIL" IF LEFT(K$,1%)="Y" &
	\ CLOSE 6% &
	\ PRINT 
18140	  RETURN
19000	  !
19005	  GOTO 19990 IF ERR>11% OR ERR<2% &
	\ ON ERR GOTO 19990,19100,19990,19990,19010,19140,19990,19990,19990,19120,19050
19010	  RESUME 18140 IF ERL=18120% OR ERL=18110% &
	\ IF ERL=2035% THEN  &
		  D%=D%+1% &
		\ RESUME  IF D$(D%)<>"" &
		\ PRINT K$;" not found." &
		\ RESUME 1010
19020	  D%=D%+1% &
	\ RESUME 
19050	  RESUME 5030 IF ERL=5020% &
	\ RESUME 18130 IF ERL=18120%
19100	  IF ERL=2035% THEN  &
		  PRINT  &
		\ PRINT "Illegal option!" &
		\ RESUME 1010
19110	  IF ERR=2% AND ERL=5010% THEN  &
		  GOTO 19999
19120	  IF ERL=20010% THEN  &
		  D%=D%+1% &
		\ RESUME 20010
19130	  GOTO 19999
19140	  D%=D%+1% &
	\ RESUME 
19990	  PRINT "MENU error #";ERR;"has occurred at line";ERL &
	\ PRINT CVT$$(RIGHT(SYS(CHR$(6%)+CHR$(9%)+CHR$(ERR)),3%),4%) &
	\ V$=SYS(CHR$(8%)+CVT%$(ERR)+CVT%$(ERL)+"MENU") &
	\ CHAIN "!ERRLOG" 1000.
19999	  PRINT  &
	\ PRINT "A MENU ERROR HAS OCCURRED.  PLEASE CALL CMC." &
	\ S$="" &
	\ RESUME 20
20000	  !
20005	  D%=1%
20010	  IF D2$(D%)<>"" THEN  &
		  OPEN D2$(D%)+"#UNIQUE.FIL" FOR INPUT AS FILE 12% &
		\ K%=STATUS AND 1024% &
		\ GOTO 18090
20015	  D%=1% &
	\ GOTO 19999 IF D2$(D%)="" &
	\ OPEN D2$(D%)+"!OTHER.FIL" FOR INPUT AS FILE 12% &
	\ K%=STATUS AND 1024% &
	\ GOTO 18090
30000	  !
30010	  S$=MID(SYS(CHR$(7%)),4%,6%) &
	\ GOTO 10
32767	  END
