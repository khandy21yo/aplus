21000	!!! ISAM (KEVIN'S BASE; BILL LORTZ' SORT--SEP. 16, 1980)
21004 PRINT "Fall through to ISAM!" &
	: STOP
21005 DIM Q$(12%),Q%(12%,11%) &

21010 DEF FNO%(C%,C$,C1$,C2$) &
	: ON ERROR GOTO 21900 &
	: Q9%=0% &
	: Q1%=INSTR(1%,C$,"<") &
	: C$=LEFT(C$,Q1%-1%) IF Q1%
21020 Q9%=46% IF C%<1% OR C%>11% &
	: UNLESS Q9% THEN Q8%=INSTR(1%,C1$,"/SF")<>0% &
	: Q9%=7% IF (Q%(C%+1%,0%) AND NOT(Q8%)) OR Q%(C%,0%) &
	: Q%=8192% &
	: Q$=C$ &
	: Q0%,Q6%=C% &
	: GOSUB 21040 &
	: CLOSE C% IF Q9% &
	: UNLESS Q9% THEN Q%(C%,0%)=Q%(C%,0%) OR 1% &
	: IF Q8% THEN Q%(Q0%,0%)=Q%(Q0%,0%) OR 2% ELSE &
		 Q$=LEFT(C$,LEN(C$)-1%)+"1" &
	: Q0%=C%+1% &
	: GOSUB 21040 &
	: Q9%=Q9% OR Q%(C%,1%)<>Q%(Q0%,1%) AND Q9%=0% &
	: UNLESS Q9% THEN Q%(Q0%,0%)=Q%(Q0%,0%) OR 2%
21030 GOSUB 21999 &
	: FNO%=Q9% &
	: FNEND &

21040 OPEN Q$ FOR INPUT AS FILE Q0%, MODE Q% &
	: Q%(Q0%,0%)=Q%(Q0%,0%) OR 4%
21050 GET #Q0%, RECORD 1% &
	: Q%(Q0%,6%)=1% &
	: Q%(Q0%,7%)=0% &
	: FIELD #Q0%, 2% AS Q0$, 2% AS Q1$, 1% AS Q2$, 1% AS Q3$ &
	: Q%(Q0%,1%),Q%(Q0%,5%)=CVT$%(Q0$) &
	: GOTO 21899 IF Q2$<>"U" AND Q2$<>"S" &
	: Q%(Q0%,2%)=CVT$%(Q1$) &
	: Q%(Q0%,3%)=(Q2$="U") &
	: Q%(Q0%,4%)=(ASCII(Q3$) AND 127%) &
	: Q3%=1% &
	: Q3%=Q3%*2% UNTIL Q3%>=Q%(Q0%,2%) &
	: Q%(Q0%,8%)=Q3% &
	: Q%(Q0%,9%)=512%/Q%(Q0%,8%) &
	: IF Q%(Q0%,3%) THEN UNLESS C%<>Q0% OR INSTR(1%,Q9$,Q$) THEN &
		PRINT "File ";Q$;" is not sorted"; UNLESS INSTR(1%,C1$,"/NS") &
	: Q%(0%,5%)=Q%(0%,1%)/10%*9%
21060 RETURN &

21200 DEF FNC%(C%) &
	: PRINT FNX%("",0%,"") IF C%=0% &
	: Q0%=C% &
	: GOSUB 21440 &
	: UNLESS Q9% THEN CLOSE C% &
	: Q%(C%,0%)=0% &
	: UNLESS (Q%(C%+1%,0%) AND 3%)<>2% THEN CLOSE C%+1% &
	: Q%(C%+1%,0%)=0%
21210 GOSUB 21999 &
	: FNC%=0% &
	: FNEND &

21220 DEF FNN%(C%) &
	: Q0%=C%*((C%<0%)-(C%>0%)) &
	: GOSUB 21440 &
	: GOTO 21250 IF Q9% &
	: Q6%=C%*((C%<0%)-(C%>0%))
21230 Q%,Q%(Q0%,10%)=Q%(Q0%,10%)+(C%<0%)-(C%>0%) &
	: IF Q%<=0% OR Q%>Q%(Q0%,1%) THEN Q%(Q0%,10%)=Q%-(C%<0%)+(C%>0%) &
	: Q9%=11% &
	: GOTO 21250
21240 FIELD #Q0%, FNQ%(Q0%,Q%)+Q%(Q0%,2%)-2% AS Q$, 2% AS Q$ &
	: Q1%=CVT$%(Q$) &
	: GOTO 21230 IF Q1%=-1% &
	: Q%(Q0%+1%,10%)=Q1% UNLESS Q%(Q0%,0%) AND 2%
21250 GOSUB 21999 &
	: FNN%=Q9% &
	: FNEND &

21260 DEF FNG%(C%,C$) &
	: Q0%=C%*[(C%<0%)-(C%>0%)] &
	: GOSUB 21440 &
	: GOTO 21360 IF Q9% &
	: Q6%=Q0% &
	: UNLESS C%<0% THEN Q9%=31% IF LEN(C$)>Q%(C%,2%)-2% &
	: GOTO 21360 IF Q9% &
	: GOTO 21350 IF C$=="" &
	: Q4%=LEN(C$) &
	: Q2%=Q%(C%,5%) &
	: GOTO 21330 IF Q2%=0% &
	: Q1%=1% &
	: GOTO 21280
21270 Q%(Q0%,10%)=VAL(C$) &
	: Q9%=88% IF Q%(Q0%,10%)<1% OR Q%(Q0%,10%)>Q%(Q0%,1%) &
	: GOTO 21360
21280 Q3%=(Q%(C%,6%)-1%)*Q%(C%,9%)-Q%(C%,4%) &
	: UNLESS Q3%<=Q1% THEN IF Q3%<=Q2% THEN &
		FIELD #C%, FNQ%(C%,Q3%) AS Q$, Q4% AS Q$ &
	: IF Q$<C$ THEN Q1%=Q3% ELSE Q2%=Q3%-1%
21290 Q3%=Q3%+Q%(C%,9%)-1% &
	: IF Q3%>=Q1% AND Q3%<=Q2% &
	  THEN FIELD #C%, FNQ%(C%,Q3%) AS Q$, Q4% AS Q$ &
	: IF Q$<C$ THEN Q1%=Q3% ELSE Q2%=Q3%-1%
21300 Q3%=Q1%/2%+Q2%/2%+((Q1% OR Q2%) AND 1%) &
	: FIELD #C%, FNQ%(C%,Q3%) AS Q$, Q4% AS Q$ &
	: IF Q$>=C$ THEN Q2%=Q3%-1% ELSE Q1%=Q3%
21310 GOTO 21280 IF Q2%>Q1% &
	: Q3%,Q%(C%,10%)=Q2% &
	: IF Q3%=1% THEN FIELD #C%, FNQ%(C%,Q3%) AS Q$, Q4% AS Q$, &
		Q%(C%,2%)-Q4%-2% AS Q1$, 2% AS Q1$ &
	: IF Q$>=C$ AND CVT$%(Q1$)<>-1% THEN &
		Q%(C%+1%,10%)=CVT$%(Q1$) IF (Q%(C%,0%) AND 2%)=0% &
	: Q9%=0% &
	: IF Q$<>C$ THEN 21330 ELSE 21360
21320 Q9%=FNN%(C%) &
	: IF Q9%=0% THEN FIELD #C%, FNQ%(C%,Q%(C%,10%)) AS Q$, Q4% AS Q$ &
	: GOTO 21360 IF Q$==C$
21330 FOR Q3%=Q%(C%,1%) TO Q%(C%,5%)+1% STEP -1% &
	: FIELD #C%, FNQ%(C%,Q3%) AS Q$, Q4% AS Q$, &
		Q%(C%,2%)-Q4%-2% AS Q1$, 2% AS Q1$ &
	: GOTO 21340 IF Q$==C$ AND CVT$%(Q1$)<>-1% &
	: NEXT Q3% &
	: Q9%=88% &
	: GOTO 21360
21340 Q%(C%,10%)=Q3% &
	: Q%(C%+1%,10%)=CVT$%(Q1$) UNLESS Q%(C%,0%) AND 2% &
	: Q9%=0% &
	: GOTO 21360
21350 Q%(C%,10%)=0% &
	: Q9%=FNN%(C%)
21360 Q9%=88% IF Q9%=11% &
	: GOSUB 21999 &
	: FNG%=Q9% &
	: FNEND &

21440 Q9%=0% &
	: Q9%=46% IF Q0%<1% OR Q0%>11% &
	: RETURN IF Q9% &
	: Q9%=9% IF (Q%(Q0%,0%) AND 3%)=0% &
	: RETURN IF Q9% &
	: ON ERROR GOTO 21900 &
	: RETURN &

21600 DEF FNL$ &
	: Q0%=Q6% &
	: Q0%=Q0%+1% IF (Q%(Q0%,0%) AND 2%)=0% &
	: IF Q%(Q0%,0%) THEN &
		FIELD #Q0%, FNQ%(Q0%,Q%(Q0%,10%)) AS Q$, Q%(Q0%,2%) AS FNL$
21610 FNEND &

21620 DEF FNL% &
	: Q0%=Q6% &
	: Q0%=Q0%+1% IF (Q%(Q0%,0%) AND 2%)=0% &
	: IF Q%(Q0%,0%) THEN FNL%=FNQ%(Q0%,Q%(Q0%,10%))
21630 FNEND &

21640 DEF FNS%=Q9% &

21650 DEF FNT=Q%(Q6%,1%) &

21660 DEF FNR(C%)=Q%(C%,10%) &

21700 DEF FNX%(C$,C%,C1$) &
	: Q0$=SYS(CHR$(7%)) &
	: Q5%=INSTR(1%,Q0$,CHR$(255%)) &
	: Q2%=INSTR(1%,Q0$,CHR$(14%)) : Q2%=Q5%+12% IF Q2%=0% &
	: Q0$=MID(Q0$+SPACE$(12%),Q5%,Q2%-Q5%) IF Q5% &
	: Q0$="" IF Q5%=0%
21710 ON ERROR GOTO 21900 &
	: CLOSE Q1% FOR Q1%=1% TO 12% IF C%<0% &
	: C%=-C% IF C%<0% &
	: CHAIN "ISM:MENU" 8100  IF Q5% AND C$="" &
	: V$=SYS(CHR$(8%)+Q0$+"   "+CHR$(14%)+C1$) &
	: CHAIN C$ C% IF C$<>"" &
	: GOTO 32767 &
	: FNEND &

21720 DEF FNX$ &
	: Q$=SYS(CHR$(7%)) &
	: Q1%=INSTR(4%,Q$,CHR$(14%)) &
	: Q$=RIGHT(Q$,Q1%+1%) &
	: Q$="" IF Q1%=0% &
	: FNX$=Q$ &
	: FNEND &

21800 DEF FNQ%(C%,R%) &
	: R%=R%+Q%(C%,4%) IF R% &
	: Q7%=R%/Q%(C%,9%)+1% &
	: GOTO 21820 IF Q7%=Q%(C%,6%)
21810 GET #C%, RECORD Q7% &
	: Q%(C%,6%)=Q7%
21820 FNQ%=R%*Q%(C%,8%) AND 511% &
	: FNEND &

21899 PRINT C$;" HAS A FILE HEADER PROBLEM." &
	: PRINT &
	: PRINT "DO NOT PROCEED!  CALL CMC IMMEDIATELY."; &
	: V$=SYS(CHR$(5%)) &
	: STOP
21900 IF ERL=21040% THEN Q9%=ERR &
	: Q%(C%,0%)=0% &
	: CLOSE C% &
	: RESUME 21060
21910 IF ERR=10% AND ERL=21800% THEN Q9%=10% &
	: RESUME 21810
21920 IF ERR=11% AND ERL=21810% THEN Q9%=11% &
	: Q%(C%,6%)=Q7% &
	: RESUME 21820
21930 IF (ERR=71% OR ERR=5%) AND ERL=21710% THEN PRINT &
	"ERROR";ERR;"IN MENU CHAIN.  STATEMENT, FILE OR ACCOUNT NOT FOUND." &
	: V$=SYS(CHR$(5%)) &
	: STOP
21940 PRINT "UNTRAPPED ERROR";ERR;"AT LINE";ERL &
	: Q9%=ERR &
	: PRINT &
	: PRINT "DO NOT PROCEED!  CALL CMC IMMEDIATELY."; &
	: V$=SYS(CHR$(5%)) &
	: STOP
21999 ON ERROR GOTO 19000 &
	: RETURN &

32767 END
