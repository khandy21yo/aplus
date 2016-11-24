10	  ! &
	  ! Program name: chart2		Compiled with SCALE 0 on V07.0 &
	  ! Decompiled on 24-Nov-16 at 02:11 AM
20	  DIM F$(27%), T$(3%), T%(2%), T(42%), T1(22%), T2(30%,15%), T2$(30%)
30	  IF FNO%(2%,"CHART.DAT","","") THEN  &
		  PRINT "ERROR";FNS%;"IN OPENING CHART OF ACCOUNTS FILE.  ABORT." &
		\ GOTO 10000
40	  OPEN "KB:" FOR INPUT AS FILE 12% &
	\ OPEN "NL:" AS FILE 1%, RECORDSIZE 256% &
	\ FIELD #1%, 8% AS F$(1%),40% AS F$(2%),2% AS F$(3%),2% AS F$(4%),2% AS F$(5%) &
	\ FIELD #1%, 54%+(L%*8%-8%) AS V$,8% AS F$(L%+5%) FOR L%=1% TO 22% &
	\ FIELD #1%, 256% AS T0$
70	  GOTO 1000 IF FNG%(2%,"ZZZZZ[1]") &
	\ V%=CVT$%(MID(FNL$,13%,2%)) &
	\ IF V%<>0% THEN  &
		  PRINT "LAST UPDATE WAS NOT COMPLETED. INFORMATION "+"IN THE FILE IS UNRELIABLE" &
		\ PRINT "UNTIL MONTH IS RE-UPDATED."
80	  DEF FNM1$(M%) &
	\ FNM1$=MID("JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC",M%*3%-2%,3%) &
	\ FNEND
90	  DEF FNY0%(V%) &
	\ Y%=VAL(RIGHT(DATE$(0%),8%)) &
	\ Y%=Y%-1% IF INSTR(1%,"JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC",CVT$$(MID(DATE$(0%),4%,3%),-1%))/3%+1%<V% &
	\ FNY0%=Y% &
	\ FNEND
100	  STOP IF FNG%(2%,"ZZZZZ[1]") &
	\ Y2%=CVT$%(MID(FNL$,9%,2%)) &
	\ Y3%=FNY0%(Y1%) &
	\ FOR L%=1% TO 15% &
		\ C$=NUM1$(Y2%)+"."+NUM1$(Y3%) &
		\ C$="0"+C$ IF INSTR(1%,C$,".")=2% &
		\ Y2%=Y2%-1% &
		\ IF Y2%=0% THEN  &
			  Y2%=12% &
			\ Y3%=Y3%-1%
110			  NEXT L%
300	  !
310	  GOTO 1000 IF FNG%(2%,"ZZZZZ[1]") &
	\ Y1%=CVT$%(MID(FNL$,9%,2%)) &
	\ Y%=CVT$%(MID(FNL$,21%,2%)) &
	\ FOR L%=1% TO 15% &
		\ C$=NUM1$(Y1%)+"."+NUM1$(Y%) &
		\ C$="0"+C$ IF INSTR(1%,C$,".")=2% &
		\ Y1%=Y1%-1% &
		\ IF Y1%=0% THEN  &
			  Y1%=12% &
			\ Y%=Y%-1%
320		  S1$=C$+S1$ &
	\ NEXT L%
1000	  !
1020	  PRINT  &
	\ S$=FNX$ &
	\ GOTO 3000 IF S$="TOT" &
	\ GOTO 4000 IF S$="RES" &
	\ GOTO 5000 IF S$="BYP" &
	\ GOTO 7000 IF S$="ZZZ" &
	\ GOTO 10000
2000	  !
2010	  LSET T0$=FNL$ &
	\ T$(L%)=F$(L%)+"" FOR L%=1% TO 3% &
	\ T%(L%)=CVT$%(F$(L%+3%)) FOR L%=1% TO 2% &
	\ T(L%)=CVT$F(F$(L%+5%)) FOR L%=1% TO 22% &
	\ RETURN
2100	  !
2110	  RSET F$(1%)=T$(1%) &
	\ LSET F$(L%)=T$(L%) FOR L%=2% TO 3% &
	\ LSET F$(L%+3%)=CVT%$(T%(L%)) FOR L%=1% TO 2% &
	\ LSET F$(L%+5%)=CVTF$(T(L%)) FOR L%=1% TO 22% &
	\ RETURN
2200	  !
2210	  ON ERROR GOTO 2220 &
	\ KILL "SS0:GL.DAS"
2220	  RESUME 2230
2230	  ON ERROR GOTO 0 &
	\ RETURN
3000	  !
3010	  GOTO 10000 IF FNG%(2%,"ZZZZZ[1]") &
	\ M1%,V%=CVT$%(MID(FNL$,9%,2%)) &
	\ Y1%=FNY0%(V%) &
	\ GOTO 10000 IF FNG%(2%,"") &
	\ PRINT "PLEASE WAIT." &
	\ T2%=0% &
	\ T1(L%)=0. FOR L%=1% TO 22%
3020	  GOSUB 2000 &
	\ FOR I%=1% TO T2% &
		\ IF T$(3%)=T2$(I%) THEN  &
			  T2(I%,L%)=T2(I%,L%)+INT(T(L%+2%)*100.+6.8213185310482798e-15)/100. FOR L%=1% TO 15% &
			\ GOTO 3040
3030			  NEXT I% &
	\ T2%=T2%+1% &
	\ T2$(T2%)=T$(3%) &
	\ T2(T2%,I%)=T(I%+2%) FOR I%=1% TO 15%
3040	  T1(I%)=T1(I%)+T(I%) FOR I%=1% TO 22% &
	\ GOTO 3020 UNLESS FNN%(2%)
3050	  INPUT "SET PAGE";S$ &
	\ P%=66% &
	\ GOSUB 3100 &
	\ T$(1%)="TOTALS" &
	\ T$(2%)="" &
	\ T(I%)=T1(I%) FOR I%=1% TO 17% &
	\ GOSUB 3200 &
	\ FOR I%=1% TO T2%-1% &
		\ T(L%+2%)=T2(I%,L%) FOR L%=1% TO 15% &
		\ T$(1%)=T2$(I%) &
		\ GOSUB 3200 &
	\ NEXT I% &
	\ GOTO 10000
3100	  !
3110	  M%=M1% &
	\ Y%=Y1% &
	\ PRINT  FOR L%=1% TO 66%-P% &
	\ PRINT  &
	\ FOR L%=1% TO 3% &
		\ FOR L1%=1% TO 5% &
			\ IF L%=1% AND L1%=1% THEN  &
				  PRINT "      ";FNM1$(M%);",";Y%; &
				\ GOTO 3150
3120			  Y%=Y1% &
			\ M%=M1%-L1%*3%+4%-L%
3130			  IF M%<=0% THEN  &
				  M%=12%+M% &
				\ Y%=Y%-1% &
				\ GOTO 3130
3140			  PRINT "      ";FNM1$(M%);",";Y%;
3150				  NEXT L1% &
		\ PRINT  &
	\ NEXT L% &
	\ PRINT  &
	\ PRINT  &
	\ P%=6% &
	\ RETURN
3200	  !
3210	  C$="###,###,###.##" &
	\ PRINT "*** "+CVT$$(T$(1%),-1%)+"  "+T$(3%)+"  "+CVT$$(T$(2%),128%) &
	\ FOR L%=1% TO 3% &
		\ FOR L1%=1% TO 5% &
			\ M%=M1%-L1%*3%+4%-L%
3220			  IF M%<=0% THEN  &
				  M%=12%+M% &
				\ GOTO 3220
3230			  IF L%=1% AND L1%=1% THEN  &
				  PRINT USING C$, T(4%); &
				\ GOTO 3270
3240			  IF L%=2% AND L1%=1% THEN  &
				  PRINT USING C$, T(3%); &
				\ GOTO 3270
3250			  IF L%=3% AND L1%=5% THEN  &
				  PRINT USING C$, T(5%); &
				\ GOTO 3270
3260			  PRINT USING C$, T(M%+5%);
3270				  NEXT L1% &
		\ PRINT  &
	\ NEXT L% &
	\ T1(L%)=T1(L%)+T(L%) FOR L%=1% TO 22% &
	\ PRINT  &
	\ P%=P%+5% &
	\ GOSUB 3100 IF P%>55% &
	\ RETURN
4000	  !
4010	  C%=0% &
	\ V%=FNG%(2%,"ZZZZZ[1]") &
	\ IF V%<>0% THEN  &
		  PRINT "RUN 'ZZZ' OPTION!" &
		\ GOTO 10000
4020	  I%=CVT$%(MID(FNL$,9%,2%)) &
	\ I%=I%+1% &
	\ I%=1% IF I%=13% &
	\ Y%=CVT$%(MID(FNL$,21%,2%)) &
	\ V%=CVT$%(MID(FNL$,13%,2%)) &
	\ IF V% THEN  &
		  PRINT "MONTH NOT FULLY UPDATED." &
		\ PRINT "DO YOU WISH TO CONTINUE (Y/N)? "; &
		\ INPUT #12%, S$ &
		\ GOTO 10000 IF LEFT(S$,1%)<>"Y" &
		\ C%=-1% &
		\ GOTO 4050
4030	  IF Y%>99% THEN  &
		  INPUT "CURRENT YEAR ";Y% &
		\ K$=FNL$ &
		\ STOP IF FNU%(2%,RIGHT(K$,20%)+CVT%$(Y%)+RIGHT(K$,23%))
4040	  V%=CVT$%(MID(FNL$,15%,2%)) &
	\ IF V%=0% THEN  &
		  INPUT "A RESET HAS BEEN DONE.  DO ANOTHER";K$ &
		\ GOTO 10000 IF K$<>"Y"
4050	  B$=MID(FNL$,19%,1%) &
	\ M%=CVT$%(MID(FNL$,9%,2%)) &
	\ IF C% THEN  &
		  M%=M%+1% &
		\ M%=1% IF M%=13%
4060	  M1%=M%-1% &
	\ M1%=12% IF M1%=0% &
	\ Y9%=Y% &
	\ Y9%=Y9%-1% IF M%=1% &
	\ IF C%=0% THEN  &
		  PRINT FNM1$(M%);" ";NUM1$(Y%);" WAS THE LAST MONTH ";"CLOSED.  CONFIRM RESET TO ";FNM1$(M1%);" ";NUM1$(Y9%);" (Y or N). "; &
		\ INPUT #12%, S$ &
		\ GOTO 10000 IF LEFT(S$,1%)<>"Y" &
		\ S$=RIGHT(NUM1$(100%+Y%),2%) &
		\ IF FNO%(4%,"CK"+FNM1$(M%)+"."+S$+"T","","")<>0% THEN  &
			  PRINT "CKDATA FILE NOT RENAMED." &
			\ GOTO 4080
4070	  IF FNO%(6%,"CK"+FNM1$(M%)+".DAT","","")<>5% THEN  &
		  PRINT "CK"+FNM1$(M%)+".DAT ERROR.  CALL CMC." &
		\ GOTO 10000
4080	  GOTO 10000 IF FNG%(2%,"")
4090	  GOSUB 2000 &
	\ GOTO 4110 IF T$(1%)="ZZZZZ[1]" &
	\ IF C%=0% OR T%(1%)=I% THEN  &
		  PRINT "!"; &
		\ PRINT  IF POS(0%)>75% &
		\ T(4%)=T(3%) &
		\ T(3%)=T(16%) IF M%=1% &
		\ T(16%)=T(5%) IF M%=1% &
		\ T(3%)=T(17%) IF M%=2% &
		\ T(17%)=T(5%) IF M%=2% &
		\ T(3%)=T(M%+3%) IF M%>2% &
		\ T(M%+3%)=T(5%) IF M%>2% &
		\ T(5%)=0% &
		\ T%(1%)=M1% &
		\ T(2%)=T(2%)-T(1%) IF B$="V" &
		\ GOSUB 2100 &
		\ STOP IF FNU%(2%,T0$)
4100	  GOTO 4090 UNLESS FNN%(2%)
4110	  GOSUB 2200 &
	\ PRINT  &
	\ PRINT "RESET COMPLETE." &
	\ PRINT  &
	\ Y%=Y%-1% IF M%=1% &
	\ V%=FNG%(2%,"ZZZZZ[1]") &
	\ IF C% THEN  &
		  STOP IF FNU%(2%,LEFT(FNL$,8%)+CVT%$(M1%)+MID(FNL$,11%,2%)+CVT%$(0%)+MID(FNL$,15%,6%)+CVT%$(Y%)+RIGHT(FNL$,23%)) &
		\ GOTO 10000
4120	  STOP IF FNU%(2%,LEFT(FNL$,8%)+CVT%$(M1%)+MID(FNL$,11%,4%)+CVT%$(0%)+MID(FNL$,17%,4%)+CVT%$(Y%)+RIGHT(FNL$,23%)) &
	\ GOTO 10000 IF FNC%(4%) &
	\ NAME "CK"+FNM1$(M%)+"."+S$+"T" AS "CK"+FNM1$(M%)+".DAT" &
	\ NAME "CK"+FNM1$(M%)+"."+S$+"1" AS "CK"+FNM1$(M%)+".DA1" &
	\ GOTO 10000
5000	  !
5010	  V%=FNG%(2%,"ZZZZZ[1]") &
	\ M%=CVT$%(MID(FNL$,9%,2%)) &
	\ Y%=CVT$%(MID(FNL$,21%,2%)) &
	\ M1%=M%+1% &
	\ M1%=1% IF M1%=13% &
	\ IF M1%=CVT$%(MID(FNL$,17%,2%)) THEN  &
		  PRINT "BYPASSING FIRST MONTH OF FISCAL YEAR IS NOT ALLOWED!" &
		\ GOTO 10000
5020	  PRINT "VERIFY BYPASSING ";FNM1$(M1%);", WHICH HAS NO CKDATA FILE "; &
	\ INPUT "(Y/N) ";C$ &
	\ GOTO 10000 IF C$<>"Y" &
	\ PRINT "BYPASSING ";FNM1$(M1%);". . ." &
	\ GOTO 10000 IF FNG%(2%,"")
5030	  PRINT "!"; &
	\ PRINT  IF POS(0%)>70% &
	\ GOSUB 2000 &
	\ IF T$(1%)<>"ZZZZZ[1]" THEN  &
		  T(5%)=T(M%+4%) &
		\ T(M%+4%)=T(3%) &
		\ T(3%)=T(4%) &
		\ T%(1%)=M1% &
		\ GOSUB 2100 &
		\ STOP IF FNU%(2%,T0$) &
		\ GOTO 5030 UNLESS FNN%(2%)
5040	  V%=FNG%(2%,"ZZZZZ[1]") &
	\ Y%=Y%+1% IF M%=12% &
	\ STOP IF FNU%(2%,LEFT(FNL$,8%)+CVT%$(M1%)+MID(FNL$,11%,10%)+CVT%$(Y%)+RIGHT(FNL$,23%)) &
	\ GOSUB 2200 &
	\ PRINT  &
	\ PRINT "UPDATE COMPLETE." &
	\ GOTO 10000
7000	  !
7010	  IF FNG%(2%,"ZZZZZ[1]") THEN  &
		  INPUT "CURRENT MONTH # ";C% &
		\ INPUT "CURRENT YEAR IS ";Y% &
		\ INPUT "START OF FISCAL YEAR MONTH #";C1% &
		\ STOP IF FNA%(2%,"ZZZZZ[1]"+CVT%$(C%)+STRING$(6%,0%)+CVT%$(C1%)+STRING$(2%,0%)+CVT%$(Y%)) &
		\ PRINT "RECORD ADDED." &
		\ GOTO 10000
7020	  PRINT "ON EACH QUESTION, TYPE RETURN FOR NO CHANGE." &
	\ PRINT  &
	\ PRINT "     LAST MONTH UPDATED IS";CVT$%(MID(FNL$,9%,2%)); &
	\ INPUT #12%, ".  CHANGE TO ? ";M% &
	\ IF M% THEN  &
		  STOP IF FNU%(2%,"ZZZZZ[1]"+CVT%$(M%)+RIGHT(FNL$,11%)) &
		\ GOSUB 2200
7030	  PRINT "     CURRENT YEAR IS ";CVT$%(MID(FNL$,21%,2%)); &
	\ INPUT #12%, ".  CHANGE TO ? ";M% &
	\ IF M% THEN  &
		  STOP IF FNU%(2%,LEFT(FNL$,20%)+CVT%$(M%)+RIGHT(FNL$,23%))
7040	  PRINT "     FIRST MONTH OF FISCAL YEAR IS";CVT$%(MID(FNL$,17%,2%)); &
	\ INPUT #12%, ".  CHANGE TO ? ";M% &
	\ IF M% THEN  &
		  STOP IF FNU%(2%,"ZZZZZ[1]"+MID(FNL$,9%,8%)+CVT%$(M%)+RIGHT(FNL$,19%))
7050	  PRINT "     OLD VAR BUDGET=";MID(FNL$,19%,1%);"   "; &
	\ INPUT "     DO YOU WANT VAR BUDGET (Y/N/ ) ";V$ &
	\ V1$=" " &
	\ V1$="V" IF V$="Y" &
	\ IF V$="" THEN &
		  PRINT "NO CHANGE" &
	  ELSE &
		  STOP IF FNU%(2%,LEFT(FNL$,18%)+V1$+RIGHT(FNL$,20%)) &
		\ PRINT "CHANGED"
8000	  !
8010	  PRINT  &
	\ PRINT "NOW YOU CAN ZERO THE DATA IN SEVERAL ACCOUNTS" &
	\ PRINT 
8020	  INPUT "START WITH ACCOUNT ";S0$ &
	\ GOTO 10000 IF S0$="" &
	\ S0$=SPACE$(8%-LEN(S0$))+S0$
8030	  INPUT "END WITH ACCOUNT ";S1$ &
	\ GOTO 10000 IF S1$="" &
	\ S1$=SPACE$(8%-LEN(S1$))+S1$
8040	  PRINT "VERIFY THE ACCOUNTS: FROM ";S0$;" TO ";S1$;"  (Y/N) "; &
	\ INPUT V$ &
	\ IF LEFT(V$,1%)<>"Y" THEN  &
		  GOTO 8000
8050	  INPUT "VERIFY ZEROING THE AMOUNTS FOR THESE ACCOUNTS (Y/N) ";V$ &
	\ GOTO 8000 UNLESS LEFT(V$,1%)="Y"
8100	  V%=FNG%(2%,S0$)
8105	  GOTO 8000 IF LEFT(FNL$,8%)>S1$
8110	  PRINT LEFT(FNL$,8%);" ZEROING" &
	\ STOP IF FNU%(2%,LEFT(FNL$,70%)+STRING$(186%,0%))
8120	  GOTO 8105 UNLESS FNN%(2%) &
	\ GOTO 8000
10000	  !
10010	  CLOSE 1% &
	\ CLOSE 12% &
	\ V%=FNX%("[1,3]CHART1.BAC",10%,"")
32767	  END
