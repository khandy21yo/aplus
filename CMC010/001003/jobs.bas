10	  ! &
	  ! Program name: jobs		Compiled with SCALE 0 on V07.0 &
	  ! Decompiled on 24-Nov-16 at 02:11 AM
20	  DIM P1$(25%), P$(17%), S$(25%)
25	  OPEN "NL:" AS FILE 1%, RECORDSIZE 512% &
	\ FIELD #1%, 6% AS P1$(1%),60% AS P1$(2%),6% AS P1$(3%),12% AS P1$(4%),1% AS P1$(5%),1% AS P1$(6%),20% AS P1$(7%),20% AS P1$(8%),20% AS P1$(9%),2% AS P1$(10.),8% AS P1$(11%),1% AS P1$(12%),4% AS P1$(13%),14% AS P1$(14%),9% AS P1$(15%),8% AS P1$(16%),8% AS P1$(17%),2% AS P1$(18%),2% AS P1$(19%),12% AS P1$(25%),8% AS P1$(20%),8% AS P1$(21%),8% AS P1$(22%),8% AS P1$(23%),8% AS P1$(24%) &
	\ FIELD #1., 256% AS T$
30	  V%=FNO%(2%,"JOB.DAT","","") &
	\ IF V%=5% THEN &
		  GOSUB 2900 &
	  ELSE &
		  IF V%<>0% THEN  &
			  PRINT "ERROR";V%;"IN OPENING JOB.DAT" &
			\ GOTO 10000
35	  FIELD #1%, 256% AS E$,6% AS C$(1%),30% AS C$(2%),30% AS C$(3%),30% AS C$(4%),30% AS C$(5%),8% AS C$(6%),8% AS C$(7%),12% AS C$(8%) &
	\ FIELD #1%, 256% AS T1$,256% AS T1$
40	  IF FNO%(4%,"CUSTOM.DAT","/RO","R") THEN  &
		  PRINT "ERROR";FNS%;"IN OPENING CUSTOM.DAT" &
		\ GOTO 10000
50	  OPEN "KB:" FOR INPUT AS FILE 11%
60	  S$(1%)="JOB NUMBER         " &
	\ S$(16%)="SUBSISTENCE 1    " &
	\ S$(2%)="JOB DESCRIPTION    " &
	\ S$(17%)="SUBSISTENCE 2    " &
	\ S$(3%)="CUSTOMER KEY       " &
	\ S$(18%)="SUBST. CODE 1    " &
	\ S$(4%)="TELEPHONE NUMBER   " &
	\ S$(19%)="SUBST. CODE 2    " &
	\ S$(5%)="BILLING TYPE       " &
	\ S$(20%)="SALES TAX RATE   " &
	\ S$(6%)="TAX CODE           " &
	\ S$(21%)="CONTRACT AMOUNT  " &
	\ S$(7%)="AUTHORITY          " &
	\ S$(22%)="PREV. BILLED AMT." &
	\ S$(8%)="CUSTOMER AUTHORITY " &
	\ S$(23%)="SPECIAL AMT. 1   " &
	\ S$(9%)="PURCHASE ORDER     " &
	\ S$(24%)="SPECIAL AMT. 2   " &
	\ S$(10%)="STATE CODE         " &
	\ S$(11%)="DATE (MM.DD.YY)    " &
	\ S$(12%)="OPEN/CLOSED        " &
	\ S$(13%)="ACCESSED           " &
	\ S$(14%)="BLD. SIZE (14 CHR.)" &
	\ S$(15%)="CO NOTES           "
100	  ON ERROR GOTO 110 &
	\ OPEN "SS0:UNIQUE.FIL" FOR INPUT AS FILE 12% &
	\ DIM #12%, A0$(255%)=64% &
	\ GOTO 130
110	  RESUME 111
111	  ON ERROR GOTO 120 &
	\ OPEN "SS0:#UNIQUE.FIL" FOR INPUT AS FILE 12% &
	\ GOTO 130
120	  RESUME 130
130	  ON ERROR GOTO 0
1000	  !
1020	  V1$="N" &
	\ PRINT  &
	\ INPUT "OPTION ";K$ &
	\ K$=LEFT(K$,3%)
1030	  GOTO 2000 IF K$="ENT" &
	\ GOTO 3000 IF K$="DEL" &
	\ GOTO 4000 IF K$="CHA" &
	\ GOTO 5000 IF K$="EXA" &
	\ GOTO 6000 IF K$="FIN" &
	\ GOTO 7000 IF K$="PRI" &
	\ GOTO 8000 IF K$="IND" &
	\ GOTO 9000 IF K$="BUD" &
	\ GOTO 10000 IF K$="END" &
	\ IF K$<>"" THEN  &
		  PRINT "TYPE <RETURN> FOR AN OPTIONS LIST." &
		\ GOTO 1020
1040	  PRINT  &
	\ PRINT "OPTIONS:  'ENTER' NEW JOBS" &
	\ PRINT "          'DELETE' EXISTING JOBS" &
	\ PRINT "          'CHANGE' JOB INFORMATION" &
	\ PRINT "          'EXAMINE' JOBS" &
	\ PRINT "          'FIND' A SINGLE JOB" &
	\ PRINT "          'PRINT' ALL JOBS" &
	\ PRINT "          'INDEX' OF JOBS BY CUSTOMER FOR CJSCAN" &
	\ PRINT "          'BUDGET' INFORMATION (C.O. NOTES)" &
	\ PRINT "          'END' PROGRAM AND UPDATE FILE" &
	\ GOTO 1020
2000	  !
2010	  PRINT  &
	\ PRINT "NEW JOB # "; &
	\ GOSUB 2200 &
	\ GOTO 2010 IF K$="VOID" OR K$="-" &
	\ GOTO 1020 IF K$="" &
	\ K$=K$+SPACE$(6%-LEN(K$)) &
	\ P$(1%)=K$+"" &
	\ IF FNG%(2%,P$(1%))=0% THEN  &
		  PRINT "JOB ALREADY EXISTS" &
		\ GOTO 2010
2012	  PRINT S$(2%);"? "; &
	\ GOSUB 2200 &
	\ GOTO 2010 IF K$="VOID" OR K$="-" &
	\ P$(2%)=K$+""
2014	  PRINT S$(3%);"? "; &
	\ GOSUB 2200 &
	\ GOTO 2010 IF K$="VOID" OR K$="-" &
	\ IF K$="" THEN  &
		  GOTO 2000
2015	  IF FNG%(4%,K$) THEN  &
		  PRINT "CUSTOMER NOT FOUND" &
		\ GOTO 2014
2016	  LSET T1$=FNL$ &
	\ IF K$=CVT$$(C$(1%),2%) THEN &
		  P$(3%)=K$ &
	  ELSE &
		  PRINT "PLEASE TYPE COMPLETE CUSTOMER # " &
		\ GOTO 2014
2017	  PRINT LEFT(T1$,6%) &
	\ PRINT C$(2%) &
	\ PRINT C$(3%) &
	\ PRINT C$(4%) &
	\ PRINT C$(5%) &
	\ PRINT C$(6%)
2018	  PRINT S$(4%); &
	\ P$(4%)=C$(8%) &
	\ PRINT P$(4%)
2020	  FOR I%=5% TO 11% &
		\ PRINT S$(I%);"? "; &
		\ GOSUB 2200 &
		\ GOTO 2010 IF K$="VOID" OR K$="-" &
		\ P$(I%)=K$+"" &
	\ NEXT I% &
	\ PRINT S$(14%);"? "; &
	\ GOSUB 2200 &
	\ GOTO 2010 IF K$="VOID" OR K$="-" &
	\ P$(14%)=K$+""
2030	  FOR I%=1% TO 2% &
		\ PRINT S$(I%+15%); &
		\ INPUT P(I%) &
		\ PRINT S$(I%+17%); &
		\ INPUT P$(I%+15%) &
	\ NEXT I%
2035	  FOR I%=3% TO 7% &
		\ PRINT S$(I%+17%); &
		\ INPUT P(I%) &
	\ NEXT I%
2037	  P$(12%)="O" &
	\ P$(13%)="1979"
2040	  GOSUB 2100 &
	\ STOP IF FNA%(2%,T$) &
	\ GOTO 2010
2100	  !
2110	  LSET P1$(I%)=P$(I%) FOR I%=1% TO 15% &
	\ FOR I%=1% TO 2% &
		\ LSET P1$(I%+15%)=CVTF$(P(I%)) &
		\ LSET P1$(I%+17%)=P$(I%+15%) &
	\ NEXT I% &
	\ LSET P1$(I%+17%)=CVTF$(P(I%)) FOR I%=3% TO 7% &
	\ RETURN
2200	  !
2210	  INPUT LINE #11%, K$ &
	\ K$=CVT$$(K$,132%)
2220	  RETURN
2300	  !
2310	  LSET T$=FNL$ &
	\ P$(I%)=P1$(I%)+"" FOR I%=1% TO 15% &
	\ FOR I%=1% TO 2% &
		\ P(I%)=CVT$F(P1$(I%+15%)) &
		\ P$(I%+15%)=P1$(I%+17%)+"" &
	\ NEXT I% &
	\ P(I%)=CVT$F(P1$(I%+17%)) FOR I%=3% TO 7% &
	\ RETURN
2400	  !
2410	  GOTO 2412 IF V1$<>"S"
2411	  PRINT P$(1%);" "; &
	\ PRINT P$(2%) &
	\ RETURN
2412	  PRINT P$(1%);" (";P$(12%);")  ";CVT$$(P$(2%),128%)
2414	  PRINT  &
	\ IF FNG%(4%,P$(3%)) THEN  &
		  PRINT P$(3%);" - ILLEGAL CUSTOMER NUMBER !!!" &
		\ X1%=X1%-3% &
		\ GOTO 2420
2416	  LSET T1$=FNL$ &
	\ PRINT C$(1%);" ";C$(2%);"     PHONE: ";P$(4%) &
	\ PRINT TAB(7%);CVT$$(C$(I%),128%) FOR I%=3% TO 5%
2420	  PRINT  &
	\ P$(7%)=CVT$$(P$(7%),136%) &
	\ P$(8%)=CVT$$(P$(8%),136%) &
	\ PRINT "AUTHORITY: ";P$(7%); UNLESS P$(7%)="" &
	\ PRINT "      CUSTOMER AUTHORITY: ";P$(8%); UNLESS P$(8%)="" &
	\ PRINT  &
	\ PRINT  &
	\ PRINT USING "   PO: \         \TYPE: !     TAX CODE: !", P$(9%),P$(5%),P$(6%)
2430	  PRINT USING " DATE: \      \  STATE: \\      ACCESS: \  \ "+"    SIZE: \            \ ", P$(11%),P$(10%),P$(13%),P$(14%) &
	\ PRINT 
2440	  PRINT USING "  TAX: ###.##                 CONTRACT:$$##,###,###.##-", P(3%),P(4%) &
	\ PRINT USING "SUB 1: ###.## \\                BILLED:$$##,###,###.##-", P(1%),P$(16%),P(5%)
2450	  PRINT USING "SUB 2: ###.## \\               SPECIAL:  "+" #,###,###.##  #,###,###.##", P(2%),P$(17%),P(6%),P(7%)
2460	  PRINT  FOR I%=1% TO 3% &
	\ X1%=X1%+18% &
	\ RETURN
2900	  !
2910	  PRINT "A JOB FILE DOES NOT EXIST. SHALL I CREATE ONE "; &
	\ INPUT C$ &
	\ GOTO 10000 IF LEFT(C$,1%)<>"Y"
2920	  OPEN "JOB.DAT" FOR OUTPUT AS FILE 2% &
	\ PRINT #2%, CVT%$(0%)+CVT%$(8%)+"S"+CHR$(128%); &
	\ CLOSE 2% &
	\ OPEN "JOB.DA1" FOR OUTPUT AS FILE 2% &
	\ PRINT #2%, CVT%$(0%)+CVT%$(256%)+"S"+CHR$(128%); &
	\ CLOSE 2%
2930	  IF FNO%(2%,"JOB.DAT","","") THEN  &
		  PRINT "ERROR";FNS%;"IN OPENING JOB.DAT." &
		\ GOTO 10000
2940	  PRINT "CREATED." &
	\ RETURN
3000	  !
3010	  PRINT  &
	\ PRINT "DELETE JOB # "; &
	\ GOSUB 2200 &
	\ GOTO 1020 IF K$="" &
	\ K$=K$+SPACE$(6%-LEN(K$))
3020	  GOTO 3030 IF FNG%(2%,K$) &
	\ GOSUB 2300 &
	\ GOSUB 2400 &
	\ INPUT "CONFIRM (Y/N) ";K$ &
	\ IF K$<>"Y" THEN &
		  PRINT "NOT DELETED" &
	  ELSE &
		  STOP IF FND%(2%,"") &
		\ PRINT "DELETED."
3021	  GOTO 3010
3030	  PRINT "JOB NOT FOUND " &
	\ GOTO 3010
4000	  !
4005	  S$="NU DE CU TE TY TC AU CA PO ST DA OP AC SI CO S1 S2 S1 S2 TA AM BI O1 O2"
4010	  PRINT  &
	\ PRINT "CHANGE JOB # "; &
	\ GOSUB 2200 &
	\ GOTO 1020 IF K$="" &
	\ K$=K$+SPACE$(6%-LEN(K$)) &
	\ IF FNG%(2%,K$)<>0% THEN  &
		  PRINT "CAN'T FIND JOB # ";K$ &
		\ GOTO 4010
4020	  GOSUB 2300
4030	  PRINT  &
	\ INPUT "ITEM TO CHANGE ( ? FOR HELP ) ";K$ &
	\ GOTO 4010 IF K$="" &
	\ K$=LEFT(K$,2%) &
	\ GOTO 4900 IF K$="?" &
	\ IF INSTR(1%,S$,K$)=0% THEN  &
		  PRINT  &
		\ PRINT "TYPE '?' FOR AN ITEMS LIST." &
		\ GOTO 4030
4040	  S%=INSTR(1%,S$,K$)/3%+1%
4050	  GOTO 4200 IF S%=16% OR S%=17% &
	\ GOTO 4300 IF S%>17%
4100	  !
4110	  PRINT CVT$$(S$(S%),128%);": ";CVT$$(P$(S%),128%)
4120	  PRINT "NEW ? "; &
	\ GOSUB 2200
4130	  IF K$="" THEN  &
		  PRINT "NO CHANGE MADE." &
		\ GOTO 4030
4140	  IF S%=1% THEN  &
		  P$(1%)=K$ &
		\ GOSUB 2100 &
		\ GOTO 4800 IF FND%(2%,"") &
		\ GOTO 4800 IF FNA%(2%,T$) &
		\ PRINT "CHANGED." &
		\ GOTO 4010
4150	  P$(S%)=K$ &
	\ GOSUB 2100 &
	\ GOTO 4800 IF FNU%(2%,T$) &
	\ PRINT "CHANGED." &
	\ GOTO 4030
4200	  !
4220	  PRINT S$(S%); &
	\ PRINT USING "##.##", P(S%-15%); &
	\ PRINT " ";P$(S%)
4230	  PRINT "NEW AMOUNT ? "; &
	\ GOSUB 2200 &
	\ IF K$="" THEN &
		  PRINT "AMOUNT NOT CHANGED." &
	  ELSE &
		  P(S%-15%)=VAL(K$) &
		\ PRINT "AMOUNT CHANGED."
4240	  PRINT "NEW CODE ? "; &
	\ GOSUB 2200 &
	\ IF K$="" THEN &
		  PRINT "CODE NOT CHANGED." &
	  ELSE &
		  P$(S%)=K$ &
		\ PRINT "CODE CHANGED."
4250	  GOSUB 2100 &
	\ GOTO 4800 IF FNU%(2%,T$) &
	\ GOTO 4030
4300	  !
4310	  PRINT CVT$$(S$(S%),128%);P(S%-17%)
4320	  PRINT "NEW AMOUNT ? "; &
	\ GOSUB 2200 &
	\ IF K$="" THEN &
		  PRINT "AMOUNT NOT CHANGED." &
	  ELSE &
		  P(S%-17%)=VAL(K$) &
		\ PRINT "AMOUNT CHANGED."
4330	  IF K$<>"" THEN  &
		  GOSUB 2100 &
		\ GOTO 4800 IF FNU%(2%,T$) &
		\ GOTO 4030
4340	  GOTO 4030
4800	  PRINT "FATAL ERROR HAS OCCURRED- CALL OR ZIP CMC." &
	\ GOTO 10000
4900	  PRINT  &
	\ PRINT "ENTER   TO CHANGE" &
	\ PRINT MID(S$,I%*3%-2%,2%);"     ";S$(I%) FOR I%=1% TO 24% &
	\ PRINT  &
	\ GOTO 4030
5000	  !
5005	  PRINT  &
	\ INPUT "FORM (S/L) ";V1$ &
	\ V1$=LEFT(V1$,1%) &
	\ IF V1$<>"S" AND V1$<>"L" THEN  &
		  PRINT  &
		\ PRINT "TYPE 'S' FOR SHORT FORM" &
		\ PRINT "AND 'L' FOR LONNG FORM" &
		\ GOTO 5005
5010	  PRINT  &
	\ PRINT "EXAMINE FROM JOB # "; &
	\ GOSUB 2200 &
	\ GOTO 1020 IF K$="" &
	\ K$=K$+SPACE$(6%-LEN(K$))
5020	  N9$=K$+"" &
	\ PRINT "          TO JOB # "; &
	\ GOSUB 2200 &
	\ K$=K$+SPACE$(6%-LEN(K$)) &
	\ V%=FNG%(2%,"") IF FNG%(2%,N9$) &
	\ GOSUB 2300
5025	  IF P1$(1%)<N9$ THEN  &
		  V%=FNN%(2%) &
		\ GOTO 5010 IF V% &
		\ GOSUB 2300 &
		\ GOTO 5025
5030	  IF P1$(1%)>K$ THEN &
		  GOTO 5010 &
	  ELSE &
		  GOSUB 2400 &
		\ IF FNN%(2%) THEN  &
			  GOTO 5010
5040	  GOSUB 2300 &
	\ GOTO 5030
6000	  !
6010	  V1$="L" &
	\ PRINT  &
	\ PRINT "FIND JOB # "; &
	\ GOSUB 2200 &
	\ GOTO 1020 IF K$="" &
	\ K$=K$+SPACE$(6%-LEN(K$)) &
	\ GOTO 6020 IF LEFT(K$,1%)="+" &
	\ GOTO 6030 IF LEFT(K$,1%)="-" &
	\ GOTO 6010 IF FNG%(2%,K$) &
	\ GOTO 6040
6020	  STOP IF FNN%(2%) &
	\ GOTO 6040
6030	  STOP IF FNN%(-2%)
6040	  GOSUB 2300 &
	\ GOSUB 2400 &
	\ GOTO 6010
7000	  !
7010	  PRINT  &
	\ INPUT "FORM (S/L) ";V1$ &
	\ V1$=LEFT(V1$,1%) &
	\ IF V1$<>"S" AND V1$<>"L" THEN  &
		  PRINT  &
		\ PRINT "TYPE 'S' FOR JUST JOB NUMBERS AND NAMES" &
		\ PRINT "TYPE 'L' FOR FULL PRINT OUT" &
		\ GOTO 7010
7020	  X1%=65% &
	\ INPUT #11%, "SET PAGE. . .";K$
7050	  STOP IF FNG%(2%,"")
7060	  IF X1%>50% THEN  &
		  PRINT  FOR I%=X1%+1% TO 66% &
		\ PRINT  &
		\ PRINT TAB(15%);"*** ";CVT$$(RIGHT(A0$(1%),2%),128%);" JOBS FILE ***" &
		\ PRINT  &
		\ X1%=3%
7070	  GOSUB 2300 &
	\ GOSUB 2400 &
	\ IF FNN%(2%) THEN &
		  GOTO 1020 &
	  ELSE &
		  GOTO 7060
8000	  !
8010	  OPEN "CJSCAN.DAT" FOR OUTPUT AS FILE 6% &
	\ PRINT #6%, CVT%$(0%);CVT%$(32%);"S";CHR$(128%); &
	\ CLOSE 6% &
	\ OPEN "CJSCAN.DA1" FOR OUTPUT AS FILE 6% &
	\ PRINT #6%, CVT%$(0%);CVT%$(128%);"S";CHR$(128%); &
	\ CLOSE 6% &
	\ STOP IF FNO%(6%,"CJSCAN.DAT","","")
8020	  STOP IF FNG%(2%,"")
8040	  GOSUB 2300 &
	\ PRINT "ERROR ON >";P$(1%);" ";P$(3%) IF FNG%(4%,P$(3%)) &
	\ LSET T1$=FNL$
8050	  STOP IF FNA%(6%,LEFT(C$(2%),23%)+" "+P$(1%)+" "+LEFT(P$(2%),46%)+" "+P$(5%)+P$(9%))
8060	  PRINT "!"; &
	\ PRINT  IF POS(0%)>70% &
	\ IF FNN%(2%) THEN &
		  GOTO 8070 &
	  ELSE &
		  GOTO 8040
8070	  STOP IF FNC%(6%) &
	\ GOTO 10000
9000	  PRINT  &
	\ PRINT "JOB # "; &
	\ GOSUB 2200 &
	\ GOTO 1020 IF K$="" &
	\ K$=K$+SPACE$(6%-LEN(K$)) &
	\ IF FNG%(2%,K$) THEN  &
		  PRINT "JOB NOT FOUND." &
		\ GOTO 9000
9010	  GOSUB 2300
9020	  PRINT "NOTES: ";P$(15%) &
	\ PRINT "CHANGE TO ? "; &
	\ GOSUB 2200 &
	\ P$(15%)=K$ UNLESS K$="" &
	\ PRINT USING "SPECIAL 1 ##,###,###.## ? ", P(6%); &
	\ GOSUB 2200 &
	\ P(6%)=VAL(K$) UNLESS K$="" &
	\ PRINT USING "SPECIAL 2 ##,###,###.## ? ", P(7%); &
	\ GOSUB 2200 &
	\ P(7%)=VAL(K$) UNLESS K$="" &
	\ GOSUB 2100 &
	\ STOP IF FNU%(2%,T$) &
	\ GOTO 9000
10000	  !
10010	  V%=FNC%(2%)+FNC%(4%) &
	\ CLOSE 1% &
	\ CLOSE 11% &
	\ CLOSE 12% &
	\ Q3$=CVT%$(8100%)+"!MENU.BAC"+SYS(CHR$(7%)) IF ASCII(SYS(CHR$(7%)))=255% &
	\ V%=FNC%(0%)
10020	  IF ASCII(SYS(CHR$(7%)))=255% THEN &
		  CHAIN "!MENU" 0. &
	  ELSE &
		  GOTO 32767
32767	  END
