10	  ! &
	  ! Program name: [21,0]PROFIX		Compiled with SCALE 0 on V07.0 &
	  ! Decompiled on 30-Jun-83 at 09:29 AM by UNBAC Version 1
20	  ON ERROR GOTO 19000
50	  DIM Q0$(11%), Q1$(60%), Q2$(20%)
100	  !
110	  PRINT "Input file name (.BAS) "; &
	\ INPUT LINE F0$ &
	\ F0$=CVT$$(F0$,4%) &
	\ F0$=F0$+".BAS" IF INSTR(1%,F0$,".")=0% &
	\ OPEN F0$ FOR INPUT AS FILE 2%
120	  PRINT "Output to file (.OUT) "; &
	\ INPUT LINE F1$ &
	\ F1$=CVT$$(F1$,4%) &
	\ F1$=F1$+".OUT" IF INSTR(1%,F1$,".")=0% &
	\ OPEN F1$ FOR INPUT AS FILE 3%
125	  PRINT "Output file already exists !!!" &
	\ INPUT "Shall I overwrite it (Y or N)";K$ &
	\ GOTO 125 IF K$<>"Y" AND K$<>"N" &
	\ GOTO 120 IF K$="N"
130	  OPEN F1$ FOR OUTPUT AS FILE 3%
300	  !
305	  READ Q0% &
	\ READ Q0$(I%) FOR I%=1% TO Q0%
306	  READ Q5% &
	\ READ Q2$(I%) FOR I%=1% TO Q5%
310	  Q1%=0% &
	\ INPUT "Shall I re-format inside of lines (N)";Q1$ &
	\ GOTO 330 IF Q1$<>"Y"
320	  Q1%=Q1%+1% &
	\ READ Q1$(Q1%) &
	\ GOTO 320
330	  !
1000	  !
1010	  INPUT LINE #2%, L$
1012	  I0%=0% &
	\ FOR I%=1% TO LEN(L$) &
		\ I0%=INSTR(1%,"0123456789",MID(L$,I%,1%)) &
		\ GOTO 1015 IF I0%=0% &
		\ L%=L%*10%+I0%-1% IF I0%<=10% &
	\ NEXT I%
1015	  L$=RIGHT(L$,I%)
1020	  GOSUB 4000 IF L% AND L3$<>"" &
	\ L$=CVT$$(L$,1%+4%+8%+128%)
1030	  I0%=0% &
	\ I$="REM" &
	\ GOSUB 2000 &
	\ I$="DATA" &
	\ GOSUB 2000 &
	\ I$="!" &
	\ GOSUB 2000 &
	\ I$="'" &
	\ GOSUB 2000 &
	\ I$='"' &
	\ GOSUB 2000
1040	  IF I0%=0% THEN &
		  L1$=CVT$$(L$,16%+32%) &
		\ GOSUB 3000 &
		\ GOTO 1090
1050	  L1$=CVT$$(LEFT(L$,I0%-1%),16%+32%) &
	\ GOSUB 3000 &
	\ L$=RIGHT(L$,I0%) &
	\ GOTO 1100 IF I0$="REM" OR I0$="!" OR I0$="DATA"
1060	  I0%=INSTR(2%,L$,I0$) &
	\ IF I0%=0% THEN &
		  L$=L$+I0$ &
		\ GOTO 1060
1070	  L3$=L3$+LEFT(L$,I0%) &
	\ L$=RIGHT(L$,I0%+1%) &
	\ GOTO 1030
1090	  GOTO 1010
1100	  !
1110	  GOSUB 4000 IF L3$<>""
1120	  PRINT #3%, CHR$(9%);L$;CHR$(10%);CHR$(13%);CHR$(0%); &
	\ INPUT LINE #2%, L$ &
	\ GOTO 1012 IF INSTR(1%,"0123456789",LEFT(L$,1%)) &
	\ L$=CVT$$(L$,1%+4%+8%+128%) &
	\ GOTO 1120
2000	  !
2010	  Z%=INSTR(1%,L$,I$) &
	\ RETURN IF Z%=0% OR I0%<>0% AND Z%>I0% &
	\ I0%=Z% &
	\ I0$=I$ &
	\ RETURN
3000	  !
3005	  IF L% THEN &
		  PRINT #3% &
		\ PRINT #3%, NUM1$(L%); &
		\ L%=0% &
		\ T0%=0%
3010	  I9%=1% &
	\ I%=INSTR(1%,L1$,":") &
	\ IF I% THEN &
		  L1$=LEFT(L1$,I%-1%)+"\"+RIGHT(L1$,I%+1%) &
		\ GOTO 3010
3015	  GOSUB 5000 IF Q1%
3020	  L1$=RIGHT(L1$,2%) IF LEFT(L1$,1%)=" " AND RIGHT(L3$,LEN(L3$))=" " &
	\ I%=INSTR(I9%,L1$,"\") &
	\ IF I% THEN &
		  L3$=L3$+CVT$$(LEFT(L1$,I%-1%),128%) &
		\ L1$=RIGHT(L1$,I%) &
		\ I9%=2% &
		\ GOSUB 4000 &
		\ GOTO 3020
3030	  L3$=L3$+L1$ &
	\ L1$="" &
	\ RETURN
4000	  !
4005	  T1%=T1%-1% IF FNI1%(L3$,"NEXT") OR FNI1%(L3$,"FNEND")
4010	  PRINT #3%, STRING$(T0%+T1%+1%,9%); &
	\ IF LEFT(L3$,1%)<>"\" THEN &
		  PRINT #3%, "  "; &
	  ELSE &
		  PRINT #3%, "\ "; &
		\ L3$=RIGHT(L3$,2%)
4020	  L3$=CVT$$(L3$,8%) &
	\ T1%=T1%+1% IF LEFT(L3$,3%)="FOR" OR LEFT(L3$,5%)="WHILE" OR LEFT(L3$, &
		5%)="UNTIL" OR LEFT(L3$,3%)="DEF" AND INSTR(1%,L3$,"=")=0% &
	\ T1%=0% IF T1%<0%
4030	  I%=FNI1%(L3$,"THEN") &
	\ I1%=FNI1%(L3$,"ELSE") &
	\ I%=I1% IF I%=0% OR I1%<>0% AND I1%<I% &
	\ GOTO 4050 UNLESS I% &
	\ IF MID(L3$,I%,4%)="THEN" THEN &
		  PRINT #3%, FNP0$(LEFT(L3$,I%-1%),T0%+T1%+1%);STRING$(T0%+T1%+ &
			1%,9%);"  THEN	  "; &
		\ L3$=RIGHT(L3$,I%+4%) &
		\ T0%=T0%+1% &
		\ GOTO 4020
4040	  PRINT #3%, FNP0$(LEFT(L3$,I%-1%),T0%+T1%+1%);STRING$(T0%+T1%,9%); &
		"  ELSE	  "; &
	\ L3$=RIGHT(L3$,I%+4%) &
	\ GOTO 4020
4050	  PRINT #3%, FNP0$(L3$,T0%+T1%+1%); &
	\ L3$=""
4060	  RETURN
5000	  !
5010	  L1$=CVT$$(L1$,2%)
5020	  FOR Z%=1% TO Q1% &
		\ Z1%=0%
5030		  Z1%=INSTR(Z1%+1%,L1$,Q1$(Z%)) &
		\ GOTO 5040 IF Z1%=0% &
		\ FOR Z3%=1% TO Q0% &
			\ GOTO 5032 IF INSTR(1%,Q0$(Z3%),Q1$(Z%))=0% OR Q0$(Z3% &
				)=Q1$(Z%) &
			\ Z4%=INSTR(Z1%-INSTR(1%,Q0$(Z3%),Q1$(Z%))-1%,L1$,Q0$( &
				Z3%)) &
			\ GOTO 5030 IF Z1%-Z4%+1%=INSTR(1%,Q0$(Z3%),Q1$(Z%))
5032		  NEXT Z3% &
		\ L1$=LEFT(L1$,Z1%-1%)+" "+RIGHT(L1$,Z1%) &
		\ Z1%=Z1%+LEN(Q1$(Z%))+1% &
		\ L1$=LEFT(L1$,Z1%-1%)+" "+RIGHT(L1$,Z1%) &
		\ GOTO 5030
5040	  NEXT Z% &
	\ L1$=CVT$$(L1$,16%) &
	\ RETURN
13000	  !
14000	  DEF FNC9%(C9$,C9%)
14010	  C1%=0%
14020	  C1$='"'
14100	  C%=INSTR(C1%,C9$,C1$) &
	\ GOTO 14200 IF C%=0% OR C%>C9%
14110	  C%=INSTR(C%+1%,C9$,C1$) &
	\ GOTO 14800 IF C%=0% OR C%>C9%
14120	  C1%=C%+1% &
	\ GOTO 14100
14200	  GOTO 14700 IF C1$="'" &
	\ C1$="'" &
	\ GOTO 14100
14700	  FNC9%=0% &
	\ GOTO 14999
14800	  FNC9%=-1%
14999	  FNEND
15000	  !
15010	  DEF FNI%(Z$,Z1$) &
	\ Z%=72% &
	\ Z%=Z%-8% IF ASCII(MID(Z$,Z1%,1%))=9% FOR Z1%=1% TO 2% &
	\ Z1%,Z2%=0%
15020	  Z2%=INSTR(Z2%+1%,Z$,Z1$) &
	\ GOTO 15090 IF Z2%=0% OR Z2%+LEN(Z1$)>Z% &
	\ Z1%=Z2% IF FNC9%(Z$,Z2%)=0% &
	\ GOTO 15020
15090	  FNI%=Z1% &
	\ FNEND
15100	  !
15110	  DEF FNI1%(Z$,Z1$) &
	\ Z%=-1% &
	\ Z%=INSTR(Z%+1%,Z$,Z1$) UNTIL Z%=0% OR Z%<>-1% AND FNC9%(Z$,Z%)=0% &
	\ FNI1%=Z% &
	\ FNEND
16000	  !
16010	  DEF FNP0$(Z$,Z%) &
	\ Z$=CVT$$(Z$,136%) &
	\ IF Z%*8%+2%+LEN(Z$)<80% THEN &
		  FNP0$=Z$+CHR$(10%)+CHR$(13%)+CHR$(0%) &
		\ GOTO 16090
16020	  FOR Z1%=77%-Z%*8% TO 9% STEP -1% &
		\ Z1$=MID(Z$,Z1%,1%) &
		\ GOTO 16030 IF FNC9%(Z$,Z1%) &
		\ IF INSTR(1%,"+-;,",Z1$) THEN &
			  PRINT #3%, CVT$$(LEFT(Z$,Z1%),128%);CHR$(10%);CHR$( &
				13%);CHR$(0%);STRING$(T0%+T1%+2%,9%);"  "; &
			\ FNP0$=FNP0$(RIGHT(Z$,Z1%+1%),Z%+1%) &
			\ GOTO 16090
16030	  NEXT Z1% &
	\ FNP0$=Z$+CHR$(10%)+CHR$(13%)+CHR$(0%) &
	\ GOTO 16090
16090	  FNEND
18000	  !
18005	  DATA	11 &

18010	  DATA	GOTO,INPUT,OUTPUT,ERROR,ASCI,FOR,STOP,RECORDSIZE,RECORD, &
		RECOUNT,RESTORE &

18050	  !
18055	  DATA	9 &

18060	  DATA	ELSE,THEN,AND,OR,NOT,;,",",+,- &

18100	  !
18110	  DATA	\,NAME,NOT,AND,XOR,IMP,EQV,MAT,CHAIN,CHANGE,CLOSE,DATA,DEF,DIM, &
		FOR,WHILE,UNTIL,GOTO,GOSUB,THEN,ERROR,ELSE,INPUT,LINE,KILL,LET, &
		NAME,AS,NEXT,OPEN,PRINT,USING,READ,OUTPUT,MODE,USING,RESUME,IF, &
		UNLESS,SLEEP,WAIT,LSET,RSET,FIELD,GET,PUT,UNLOCK,FILE, &
		RECORDSIZE,STEP,RECORD,COUNT,TO,OR,ON &

19000	  !
19010	  IF ERL=110% AND ERR=11% THEN &
		  RESUME 32767
19012	  IF ERL=110% THEN &
		  PRINT "Error on opening file (";NUM1$(ERR);")  [";CVT$$( &
			RIGHT(SYS(CHR$(6%)+CHR$(9%)+CHR$(ERR)),3%),128%);"]" &
		\ RESUME 110
19020	  IF ERL=120% AND ERR=11% THEN &
		  RESUME 32767
19022	  IF ERL=120% AND ERR=5% THEN &
		  RESUME 130
19024	  IF ERL=120% THEN &
		  PRINT "Error on opening output file (";NUM1$(ERR);")  ["; &
			CVT$$(RIGHT(SYS(CHR$(6%)+CHR$(9%)+CHR$(ERR)),3%),4%);"]" &
		\ RESUME 120
19030	  IF ERR=57% AND ERL=320% THEN &
		  Q1%=Q1%-1% &
		\ RESUME 330
19990	  IF ERR=11% THEN &
		  GOSUB 4000 &
		\ PRINT #3% &
		\ CLOSE 1% &
		\ CLOSE 2% &
		\ CLOSE 3% &
		\ GOTO 32767
19999	  ON ERROR GOTO 0 &
	\ STOP
32767	  END
