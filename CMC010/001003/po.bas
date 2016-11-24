10	  ! &
	  ! Program name: po		Compiled with SCALE 0 on V06.C &
	  ! Decompiled on 24-Nov-16 at 02:11 AM
20	  OPEN "KB:" AS FILE 10% &
	\ FIELD #10%, 6% AS U$(1%),6% AS U$(2%),6% AS U$(3%),6% AS U$(4%),9% AS U$(5%),15% AS U$(6%),8% AS U$(7%),8% AS U$(8%) &
	\ FIELD #10%, 64% AS T3$
30	  V%=FNO%(2%,"PO.DAT","U","R") &
	\ IF V%=5% THEN &
		  GOTO 2500 &
	  ELSE &
		  IF V%<>0% THEN  &
			  PRINT "ERROR";V%;"IN OPENING PO.DAT" &
			\ GOTO 10000
60	  K1$="ENTDELCHAEXAFINPRISORENDINDJOB"
70	  S$(1%)="PO #     " &
	\ S$(2%)="JOB #    " &
	\ S$(3%)="VENDOR # " &
	\ S$(4%)="TYPE     " &
	\ S$(5%)="DATE     " &
	\ S$(6%)="VENDOR NAME" &
	\ S$(7%)="PO AMOUNT" &
	\ S$(8%)="INV AMT. "
80	  OPEN "KB:" FOR INPUT AS FILE 11%
1000	  !
1005	  GOTO 1020
1010	  PRINT  &
	\ PRINT "OPTIONS:  'ENTER' NEW PURCHASES" &
	\ PRINT "          'DELETE' EXISTING PURCHASES" &
	\ PRINT "          'CHANGE' PURCHASE INFORMATION" &
	\ PRINT "          'EXAMINE' PURCHASES" &
	\ PRINT "          'FIND' A SINGLE PURCHASE" &
	\ PRINT "          'PRINT' ALL PURCHASES" &
	\ PRINT "          'SORT'PURCHASE FILE" &
	\ PRINT "          'END' PROGRAM AND UPDATE FILES" &
	\ PRINT "	   'INDEX' OF PO'S BY JOB #" &
	\ PRINT "	   'JOB' EXAMINE"
1020	  V1$="N" &
	\ PRINT  &
	\ INPUT "OPTION ";K$ &
	\ FOR I%=1% TO 10% &
		\ IF LEFT(K$,3%)=MID(K1$,I%*3%-2%,3%) THEN  &
			  GOTO 1040
1030			  NEXT I% &
	\ GOTO 1010
1040	  ON I% GOTO 2000,3000,4000,5000,6000,7000,9000,10000,12000,13000
2000	  !
2010	  PRINT  &
	\ PRINT "NEW PO # "; &
	\ GOSUB 2200 &
	\ GOTO 1020 IF K$="" &
	\ W$(1%)=K$+"" &
	\ IF FNG%(2%,W$(1%))=0% THEN  &
		  PRINT "THAT # IS PRESENTLY IN USE." &
		\ GOTO 2010
2020	  PRINT "JOB # "; &
	\ GOSUB 2200 &
	\ GOTO 2080 IF K$="-" &
	\ W$(2%)=K$+"" &
	\ PRINT "VENDOR # "; &
	\ GOSUB 2200 &
	\ GOTO 2080 IF K$="-" &
	\ W$(3%)=K$+"" &
	\ PRINT "TYPE "; &
	\ GOSUB 2200 &
	\ GOTO 2080 IF K$="-" &
	\ W$(4%)=K$+"" &
	\ INPUT "DATE ";W$(5%) &
	\ GOTO 2080 IF W$(5%)="-" &
	\ W$(5%)="0"+W$(5%) IF INSTR(1%,W$(5%),".")=2% &
	\ W$(5%)=LEFT(W$(5%),3%)+"0"+RIGHT(W$(5%),4%) IF INSTR(4%,W$(5%),".")=5% &
	\ INPUT "PO AMOUNT ";K$ &
	\ GOTO 2080 IF K$="-" &
	\ W(1%)=VAL(K$) &
	\ INPUT "INV AMT. ";K$ &
	\ GOTO 2080 IF K$="-" &
	\ W(2%)=VAL(K$)
2040	  GOSUB 2100 &
	\ V%=FNA%(2%,T3$) &
	\ GOTO 2010
2050	  PRINT "ALL ENTRIES FOR THIS VENDOR ABORTED." &
	\ GOTO 2010
2070	  GOTO 2040
2080	  PRINT "ENTRY VOIDED." &
	\ GOTO 2010
2100	  !
2110	  LSET U$(I%)=W$(I%) FOR I%=1% TO 6% &
	\ LSET U$(I%)=CVTF$(W(I%-6%)) FOR I%=7% TO 8% &
	\ RETURN
2200	  !
2210	  INPUT LINE #11%, K$ &
	\ K$=CVT$$(K$,132%)
2220	  RETURN
2300	  !
2310	  LSET T3$=FNL$ &
	\ W$(I%)=U$(I%)+"" FOR I%=1% TO 6% &
	\ W(I%)=CVT$F(U$(I%+6%)) FOR I%=1% TO 2% &
	\ RETURN
2400	  !
2410	  IF X1%>60% THEN  &
		  PRINT  FOR X1%=X1% TO 71% &
		\ GOSUB 8000 &
		\ X1%=6%
2412	  X1%=X1%+1% &
	\ PRINT W$(L%);" "; FOR L%=1% TO 5%
2420	  FOR J%=1% TO 2% &
		\ PRINT USING " ########.##", W(J%); &
		\ T(J%)=T(J%)+W(J%) &
	\ NEXT J% &
	\ PRINT USING " ########.##", W(1%)-W(2%); &
	\ PRINT  &
	\ RETURN
2500	  !
2505	  PRINT "A PO FILE DOES NOT EXIST. SHALL I CREATE ONE "; &
	\ INPUT C$ &
	\ GOTO 10000 IF LEFT(C$,1%)<>"Y"
2510	  OPEN "PO.DAT" FOR OUTPUT AS FILE 2% &
	\ PRINT #2%, CVT%$(0%)+CVT%$(14%)+"S"+CHR$(128%); &
	\ CLOSE 2% &
	\ OPEN "PO.DA1" FOR OUTPUT AS FILE 2% &
	\ PRINT #2%, CVT%$(0%)+CVT%$(64%)+"S"+CHR$(128%); &
	\ CLOSE 2%
2520	  V%=FNO%(2%,"PO.DAT","","") &
	\ IF V%<>0% THEN  &
		  PRINT "ERROR";V%;"IN OPENING PO.DAT" &
		\ GOTO 10000
2530	  PRINT "CREATED." &
	\ GOTO 60
3000	  !
3010	  PRINT  &
	\ PRINT "DELETE - PO # "; &
	\ GOSUB 2200 &
	\ GOTO 1020 IF K$=""
3020	  IF FNG%(2%,K$) THEN  &
		  PRINT "P.O. NOT FOUND." &
		\ GOTO 3010
3030	  GOSUB 2300 &
	\ GOSUB 2400 &
	\ INPUT "CONFIRM (Y/N) ";K$ &
	\ V%=FND%(2%,"") IF LEFT(K$,1%)="Y" &
	\ GOTO 3010
4000	  !
4010	  PRINT  &
	\ PRINT "CHANGE - PO # "; &
	\ GOSUB 2200 &
	\ IF K$="" THEN  &
		  GOTO 1020
4020	  GOTO 4040 IF FNG%(2%,K$) &
	\ GOSUB 2300 &
	\ PRINT S$(1%);W$(1%);TAB(19%); &
	\ N1$=K$+"" &
	\ GOSUB 2200 &
	\ N9$=K$+"" &
	\ IF N9$<>"" THEN  &
		  GOTO 4050 IF FNG%(2%,N9$)=0%
4025	  FOR I%=2% TO 5% &
		\ PRINT S$(I%);W$(I%);TAB(18%);"? "; &
		\ GOSUB 2200 &
		\ W$(I%)=K$+"" IF K$<>"" &
	\ NEXT I% &
	\ FOR I%=1% TO 2% &
		\ PRINT USING "\       \ #####.##", S$(I%+6%),W(I%); &
		\ INPUT W4 &
		\ W(I%)=W4 IF W4<>0. &
	\ NEXT I% &
	\ IF N9$="" THEN  &
		  W$(1%)=U$(1%)+"" &
		\ GOSUB 2100 &
		\ IF FNU%(2%,T3$) THEN &
			  STOP &
		  ELSE &
			  GOTO 4010
4030	  W$(1%)=N9$+"" &
	\ GOSUB 2100 &
	\ STOP IF FNA%(2%,T3$) &
	\ STOP IF FND%(2%,N1$) &
	\ GOTO 4010
4040	  PRINT " NOT FOUND " &
	\ GOTO 4010
4050	  PRINT " THAT # IS ALREADY IN USE " &
	\ GOTO 4010
5000	  !
5010	  PRINT  &
	\ PRINT "EXAMINE FROM - PO # "; &
	\ GOSUB 2200 &
	\ IF K$="" THEN  &
		  GOTO 1020
5020	  N9$=K$+"" &
	\ PRINT "          TO - PO # "; &
	\ GOSUB 2200 &
	\ V%=FNG%(2%,"") IF FNG%(2%,N9$) &
	\ GOSUB 2300
5025	  IF U$(1%)<N9$ THEN  &
		  V%=FNN%(2%) &
		\ GOTO 5010 IF V% &
		\ GOSUB 2300 &
		\ GOTO 5025
5030	  IF U$(1%)>K$ THEN &
		  GOTO 5010 &
	  ELSE &
		  GOSUB 2400 &
		\ IF FNN%(2%) THEN  &
			  GOTO 5010
5040	  GOSUB 2300 &
	\ GOTO 5030
6000	  !
6010	  PRINT  &
	\ PRINT "FIND - PO # "; &
	\ GOSUB 2200 &
	\ GOTO 1020 IF K$="" &
	\ GOTO 6020 IF LEFT(K$,1%)="+" &
	\ GOTO 6030 IF LEFT(K$,1%)="-" &
	\ GOTO 6010 IF FNG%(2%,K$) &
	\ GOTO 6040
6020	  V%=FNN%(2%) &
	\ GOTO 6040
6030	  V%=FNN%(-2%)
6040	  GOSUB 2300 &
	\ GOSUB 2400 &
	\ GOTO 6010
7000	  !
7010	  X1%=66% &
	\ INPUT "SET PAGE ";I%
7050	  V%=FNG%(2%,"")
7060	  GOSUB 2300 &
	\ GOSUB 2400 &
	\ IF FNN%(2%) THEN &
		  GOTO 1020 &
	  ELSE &
		  GOTO 7060
8000	  !
8010	  PRINT "PO #  JOB #   VND # TYPE      DATE   "+"    PO AMOUNT    INV AMNT     TOTAL PO" &
	\ PRINT  &
	\ RETURN
9000	  V$=SYS(CHR$(7%)) &
	\ Q3$=CVT%$(9010%)+"!PO.BAC" &
	\ Q3$=Q3$+V$ IF ASCII(V$)=255% &
	\ CLOSE 10% &
	\ V%=FNC%(2%)+FNC%(0%) &
	\ CHAIN "!PO" 0%
9010	  V$=SYS(CHR$(7%)) &
	\ V$=RIGHT(V$,INSTR(1%,V$,".")+4%) &
	\ V$=SYS(CHR$(8%)+LEFT(V$,INSTR(1%,V$,CHR$(13%))-1%)) &
	\ GOTO 10
10000	  !
10010	  V%=FNC%(2%) &
	\ CLOSE 10% &
	\ CLOSE 11% &
	\ Q3$=CVT%$(8100%)+"!MENU.BAC"+SYS(CHR$(7%)) IF ASCII(SYS(CHR$(7%)))=255% &
	\ V%=FNC%(0%)
10020	  IF ASCII(SYS(CHR$(7%)))=255% THEN &
		  CHAIN "!MENU" 0. &
	  ELSE &
		  V$=SYS(CHR$(8%)) &
		\ GOTO 32767
12000	  !
12010	  OPEN "POJ.DAT" FOR OUTPUT AS FILE 4% &
	\ PRINT #4%, CVT%$(0%);CVT%$(14%);"S";CHR$(128%); &
	\ CLOSE 4% &
	\ OPEN "POJ.DA1" FOR OUTPUT AS FILE 4% &
	\ PRINT #4%, CVT%$(0%);CVT%$(16%);"S";CHR$(128%); &
	\ CLOSE 4%
12020	  STOP IF FNO%(4%,"POJ.DAT","","") &
	\ STOP IF FNG%(2%,"")
12030	  LSET T3$=FNL$ &
	\ PRINT "!"; &
	\ PRINT  IF POS(0%)>70% &
	\ STOP IF FNA%(4%,U$(2%)+U$(1%)) &
	\ IF FNN%(2%) THEN &
		  GOTO 12040 &
	  ELSE &
		  GOTO 12030
12040	  PRINT "DONE WITH INDEX" &
	\ STOP IF FNC%(4%) &
	\ GOTO 10000
13000	  !
13010	  GOTO 13120 IF FNO%(4%,"POJ.DAT","","")
13020	  T(1%),T(2%)=0. &
	\ INPUT #11%, "JOB#  ",J$ &
	\ GOTO 13120 IF J$="" &
	\ GOTO 13020 IF FNG%(4%,J$) &
	\ J$=LEFT(FNL$,6%)
13025	  P$=MID(FNL$,7%,6%)
13030	  STOP IF FNG%(2%,P$) &
	\ GOSUB 2300 &
	\ GOSUB 2400
13040	  GOTO 13100 IF FNN%(4%) &
	\ GOTO 13100 IF J$<>LEFT(FNL$,6%) &
	\ GOTO 13025
13100	  PRINT USING "\           \###,###,###,###.##", "TOTAL PO'S",T(1%) &
	\ PRINT USING "\           \###,###,###,###.##", "TOTAL INV'S",T(2%) &
	\ PRINT USING "\           \###,###,###,###.##", "DIFFERENCE",T(1%)-T(2%) &
	\ GOTO 13020
13110	  V%=FNC%(4%) &
	\ GOTO 10000
13120	  V%=FNC%(4%) &
	\ GOTO 1020
32767	  END
