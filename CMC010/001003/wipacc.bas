10	  ! &
	  ! Program name: wipacc		Compiled with SCALE 0 on V07.0 &
	  ! Decompiled on 24-Nov-16 at 02:11 AM
30	  OPEN "KB:" AS FILE 12% &
	\ FIELD #12%, 6% AS P$(1%),8% AS P$(2%),8% AS P$(3%),8% AS P$(4%),8% AS P$(5%),8% AS P$(6%),8% AS P$(7%),2% AS P$(8%),2% AS P$(9%) &
	\ FIELD #12%, 64% AS T1$
40	  OPEN "KB:" FOR INPUT AS FILE 11%
80	  V%=FNO%(4%,"WIPACC.DAT","U","R") &
	\ IF V%=5% THEN &
		  GOSUB 2400 &
	  ELSE &
		  IF V%<>0% THEN  &
			  PRINT "ERROR";V%;"IN OPENING WIPACC.DAT." &
			\ GOTO 10000
120	  H7$=" JOB #  ACCT. #       BUDGET      TO-DATE      CURRENT   MONTH" &
	\ H8$="\    \ \      \ #,###,###.## #,###,###.## #,###,###.##   \       \" &
	\ U$="###,###,###.##" &
	\ U1$="\                 \"+U$
800	  DIM M$(13%) &
	\ M$(1%)="JANUARY" &
	\ M$(2%)="FEBRUARY" &
	\ M$(3%)="MARCH" &
	\ M$(4%)="APRIL" &
	\ M$(5%)="MAY" &
	\ M$(6%)="JUNE" &
	\ M$(7%)="JULY" &
	\ M$(8%)="AUGUST" &
	\ M$(9%)="SEPTEMBER" &
	\ M$(10%)="OCTOBER" &
	\ M$(11%)="NOVEMBER" &
	\ M$(12%)="DECEMBER" &
	\ M$(0%)="CURRENT" &
	\ M$(13%)=""
1000	  !
1005	  GOTO 1020
1010	  PRINT  &
	\ PRINT "OPTIONS:  'ENTER' ACCOUNT CODES." &
	\ PRINT "          'DELETE' EXISTING ACCOUNT CODES." &
	\ PRINT "          'CHANGE' ACCOUNT INFORMATION." &
	\ PRINT "          'QUICK' CHANGE NUMERIC INFORMATION." &
	\ PRINT "          'EXAMINE' ACCOUNT CODES." &
	\ PRINT "          'KILL' ENTIRE JOB" &
	\ PRINT "          'END' PROGRAM." &
	\ PRINT "          'TOTAL' OF ALL COSTS IN FILE"
1020	  PRINT  &
	\ INPUT "OPTION ";K$ &
	\ K$=LEFT(K$,3%) &
	\ GOTO 2000 IF K$="ENT" &
	\ GOTO 4000 IF K$="DEL" &
	\ GOTO 5000 IF K$="CHA" &
	\ GOTO 5200 IF K$="QUI" &
	\ GOTO 6000 IF K$="EXA" &
	\ GOTO 7000 IF K$="KIL" &
	\ GOTO 10000 IF K$="END" &
	\ GOTO 12000 IF K$="TOT"
1030	  GOTO 1010
2000	  !
2010	  PRINT  &
	\ PRINT "NEW JOB # "; &
	\ GOSUB 2600 &
	\ GOTO 1020 IF K$=""
2020	  GOSUB 5100 &
	\ IF FNG%(4%,S$(1%)+S$(2%))=0% THEN  &
		  PRINT "ACC # ";S$(1%);"  ALREADY EXITS." &
		\ GOTO 2010
2030	  INPUT "BUDGET   ";S(1%) &
	\ S(I%)=0. FOR I%=2% TO 5% &
	\ S%(1%),S%(2%)=0%
2040	  INPUT "COST TO YEAR ";S(3%)
2050	  INPUT "COST-TO-DATE  ";S(4%) &
	\ GOSUB 2500 &
	\ STOP IF FNA%(4%,T1$) &
	\ GOTO 2010
2400	  !
2410	  PRINT "A W.I.P. ACCOUNTS FILE DOESN'T EXIST. SHALL I CREATE ONE "; &
	\ INPUT C$ &
	\ GOTO 10000 IF LEFT(C$,1%)<>"Y"
2420	  OPEN "WIPACC.DAT" FOR OUTPUT AS FILE 4% &
	\ PRINT #4%, CVT%$(0%)+CVT%$(16%)+"S"+CHR$(128%); &
	\ CLOSE 4% &
	\ OPEN "WIPACC.DA1" FOR OUTPUT AS FILE 4% &
	\ PRINT #4%, CVT%$(0%)+CVT%$(64%)+"S"+CHR$(128%); &
	\ CLOSE 4%
2430	  IF FNO%(4%,"WIPACC.DAT","","") THEN  &
		  PRINT "ERROR";FNS%;"IN OPENING WIPACC.DAT." &
		\ GOTO 10000
2440	  PRINT "FILE CREATED." &
	\ RETURN
2500	  !
2510	  LSET P$(1%)=S$(1%) &
	\ RSET P$(2%)=S$(2%) &
	\ LSET P$(I%+2%)=CVTF$(S(I%)) FOR I%=1% TO 5% &
	\ LSET P$(I%+8%)=CVT%$(S%(I%)) FOR I%=1% TO 2% &
	\ RETURN
2600	  !
2610	  INPUT #11%, K$ &
	\ S$(1%)=K$+SPACE$(6%-LEN(K$)) &
	\ RETURN
2700	  !
2710	  LSET T1$=FNL$ &
	\ S$(I%)=P$(I%)+"" FOR I%=1% TO 2% &
	\ S(I%)=CVT$F(P$(I%+2%)) FOR I%=1% TO 5% &
	\ S%(I%)=CVT$%(P$(I%+8%)) FOR I%=1% TO 2% &
	\ RETURN
2800	  !
2810	  FOR K%=1% TO 12% &
		\ IF LEFT(Y$,3%)=LEFT(M$(K%),3%) THEN  &
			  S%=K% &
			\ RETURN
2820			  NEXT K% &
	\ S%=0% &
	\ RETURN
4000	  !
4010	  PRINT  &
	\ PRINT "JOB # TO BE DELETED "; &
	\ GOSUB 2600 &
	\ IF K$="" THEN  &
		  GOTO 1020
4015	  GOSUB 5100 &
	\ IF FNG%(4%,S$(1%)+S$(2%)) THEN  &
		  PRINT "ACCOUNT DOES NOT EXIST." &
		\ GOTO 4010
4020	  GOSUB 2700 &
	\ PRINT  &
	\ PRINT USING H8$, S$(1%),S$(2%),S(1%),S(4%),S(5%),M$(S%) &
	\ INPUT "CONFIRM (Y/N) ";Y$ &
	\ V%=FND%(4%,"") IF LEFT(Y$,1%)="Y" &
	\ STOP IF FNS% &
	\ GOTO 4010
5000	  !
5010	  PRINT  &
	\ PRINT "JOB # TO BE MODIFIED "; &
	\ GOSUB 2600 &
	\ IF K$="" THEN  &
		  GOTO 1020
5020	  GOSUB 5100 &
	\ IF FNG%(4%,S$(1%)+S$(2%)) THEN  &
		  PRINT "THE ACCOUNT DOES NOT EXIST." &
		\ GOTO 5010
5030	  GOSUB 2700 &
	\ PRINT  &
	\ PRINT "JOB # ";S$(1%); &
	\ INPUT K$ &
	\ PRINT "ACCOUNT  #  : ";S$(2%); &
	\ INPUT " ";X$ &
	\ GOTO 5031 IF X$+K$="" &
	\ X$=X$+SPACE$(8%-LEN(X$)) &
	\ K$=SPACE$(6%-LEN(K$))+K$ &
	\ IF FNG%(4%,K$+X$)=0% THEN  &
		  PRINT "THAT ACCOUNT # IS USED." &
		\ GOTO 5020
5031	  PRINT USING "BUDGET    #,###,###.## : ", S(1%); &
	\ INPUT Y$ &
	\ IF Y$<>"" THEN  &
		  S(1%)=VAL(Y$)
5032	  PRINT USING "ADJUSTMENT#,###,###.## : ", S(2%); &
	\ INPUT Y$ &
	\ S(2%)=VAL(Y$) IF Y$<>""
5033	  PRINT USING "COST TO YEAR #,###,###.## : ", S(3%); &
	\ INPUT Y$ &
	\ S(3%)=VAL(Y$) IF Y$<>""
5034	  PRINT USING "COST-TO-DATE   #,###,###.## : ", S(4%); &
	\ INPUT Y$ &
	\ IF Y$<>"" THEN  &
		  S(4%)=VAL(Y$)
5035	  PRINT USING "CURRENT ##,###,###.## : ", S(5%); &
	\ INPUT Y$ &
	\ IF Y$<>"" THEN  &
		  S(5%)=VAL(Y$)
5036	  S9%=S%(1%) &
	\ S9%=13% IF S9%<0% OR S9%>13% &
	\ PRINT "MONTH       : ";M$(S%(1%)); &
	\ INPUT Y$ &
	\ IF Y$<>"" THEN  &
		  GOSUB 2800
5040	  IF K$+X$="" THEN  &
		  GOSUB 2510 &
		\ STOP IF FNU%(4%,T1$) &
		\ GOTO 5010
5050	  STOP IF FND%(4%,S$(1%)+S$(2%)) &
	\ S$(1%)=K$ IF K$<>SPACE$(6%) &
	\ S$(2%)=X$ IF X$<>"" &
	\ GOSUB 2500 &
	\ STOP IF FNA%(4%,T1$) &
	\ GOTO 5010
5100	  INPUT #11%, "ACCOUNT # ";A$ &
	\ A$=SPACE$(8%-LEN(A$))+A$ &
	\ S$(2%)=A$ &
	\ RETURN
5200	  !
5210	  PRINT "ITEM TO CHANGE: (1) BUDGET, (2) BUDGET ADJUSTMENT," &
	\ PRINT "                (3) COST, (4) BILLED, (5) PAID"; &
	\ INPUT X% &
	\ X$="" &
	\ GOTO 1020 IF X%=0%
5220	  IF X%=1% THEN  &
		  X1$="BUDGET" &
		\ X%=1% &
		\ GOTO 5240
5221	  IF X%=2% THEN  &
		  X1$="BUDGET ADJUSTMENT" &
		\ X%=2% &
		\ GOTO 5240
5222	  IF X%=3% THEN  &
		  X1$="COSTS" &
		\ X%=5% &
		\ GOTO 5240
5224	  IF X%=4% THEN  &
		  X1$="BILLED" &
		\ X%=4% &
		\ X$="  000.01" &
		\ GOTO 5240
5226	  IF X%=5% THEN  &
		  X1$="PAID" &
		\ X%=5% &
		\ X$="  000.01" &
		\ GOTO 5240
5228	  GOTO 5210
5240	  PRINT "JOB# "; &
	\ GOSUB 2600 &
	\ IF K$="" THEN  &
		  GOTO 5210
5250	  PRINT  IF POS(0%)>5% &
	\ IF X$<>"" THEN &
		  S$(2%),A$=X$+"" &
	  ELSE &
		  GOSUB 5100 &
		\ GOTO 5240 IF S$(2%)=""
5253	  V%=FNG%(4%,S$(1%)+S$(2%)) &
	\ IF V%=0% THEN  &
		  GOSUB 2700 &
		\ GOTO 5260
5255	  STOP IF V%<>88% &
	\ PRINT "JOB - ACCOUNT WILL BE ADDED " &
	\ S(I%)=0. FOR I%=1% TO 6% &
	\ S%=0%
5260	  PRINT USING U1$, X1$,S(X%); &
	\ PRINT " ADD: "; &
	\ Y$=SYS(CHR$(3%)) &
	\ INPUT " ";Y$ &
	\ K$=SYS(CHR$(2%)) &
	\ GOTO 5240 IF Y$="" AND X$="  000.01" &
	\ IF Y$="" THEN  &
		  X$=Y$ &
		\ GOTO 5250
5265	  S(X%)=VAL(Y$)/100.+S(X%) &
	\ PRINT USING U$, S(X%)
5270	  GOSUB 2510 &
	\ IF V% THEN &
		  STOP IF FNA%(4%,T1$) &
	  ELSE &
		  STOP IF FNU%(4%,T1$)
5280	  GOTO 5250
6000	  !
6010	  PRINT  &
	\ PRINT "LIST FROM JOB # "; &
	\ GOSUB 2600 &
	\ GOTO 1020 IF K$="" &
	\ T,T1,T2=0. &
	\ X$=S$(1%)+"" &
	\ PRINT "       TO JOB # "; &
	\ GOSUB 2600 &
	\ Y$=S$(1%)+""
6030	  V%=FNG%(4%,"") IF FNG%(4%,X$) &
	\ GOSUB 2700
6040	  IF S$(1%)<X$ THEN  &
		  V%=FNN%(4%) &
		\ GOTO 6010 IF V% &
		\ GOSUB 2700 &
		\ GOTO 6040
6050	  GOTO 6010 IF S$(1%)>Y$ &
	\ PRINT  &
	\ PRINT H7$ &
	\ PRINT 
6060	  S%=13% IF S%<0% OR S%>13% &
	\ IF S$(1%)>Y$ THEN  &
		  GOTO 6080
6062	  IF S$(2%)="  000.01" THEN &
		  PRINT S$(1%);" ";S$(2%) &
	  ELSE &
		  PRINT USING H8$, S$(1%),S$(2%),S(1%),S(4%),S(5%),M$(S%(1%)) &
		\ T=T+S(1%) &
		\ T1=T1+S(4%) &
		\ T2=T2+S(5%)
6070	  IF FNN%(4%) THEN &
		  GOTO 6080 &
	  ELSE &
		  GOSUB 2700 &
		\ GOTO 6060
6080	  PRINT  &
	\ PRINT SPACE$(7%);"TOTAL :"; &
	\ PRINT USING "  #,###,###.## #,###,###.## #,###,###.##", T,T1,T2 &
	\ GOTO 6010
7000	  !
7010	  PRINT "JOB # TO BE DELETED "; &
	\ GOSUB 2600 &
	\ GOTO 1020 IF K$="" &
	\ IF FNG%(4%,S$(1%)) THEN  &
		  PRINT "NOT FOUND" &
		\ GOTO 7010
7020	  K$="" &
	\ PRINT "VERIFY DELETION OF ENTIRE JOB# ";S$(1%); &
	\ INPUT "( Y OR N )"K$ UNTIL K$="Y" OR K$="N" &
	\ IF K$="N" THEN  &
		  PRINT "NOT DELETED" &
		\ GOTO 7010
7025	  K$=S$(1%)
7030	  GOSUB 2700 &
	\ GOTO 7050 IF K$<>S$(1%) &
	\ STOP IF FND%(4%,"") &
	\ GOTO 7030 IF FNN%(4%)=0%
7050	  PRINT "JOB # ";K$;" IS NOW DELETED" &
	\ GOTO 7010
10000	  !
10010	  V%=FNC%(4%) &
	\ CLOSE 12% &
	\ Q3$=CVT%$(8100%)+"!MENU.BAC"+SYS(CHR$(7%)) IF ASCII(SYS(CHR$(7%)))=255% &
	\ V%=FNC%(0%)
10020	  IF ASCII(SYS(CHR$(7%)))=255% THEN &
		  CHAIN "!MENU" 0. &
	  ELSE &
		  GOTO 32767
12000	  !
12010	  STOP IF FNG%(4%,"") &
	\ T(I%)=0. FOR I%=1% TO 6% &
	\ U1$="\                \ ###,###,###,###,###.##"
12040	  GOSUB 2700 &
	\ GOTO 12100 IF S$(2%)="  000.01" OR LEFT(S$(2%),5%)="  001" OR LEFT(S$(2%),5%)="  002" &
	\ T(I%)=T(I%)+S(I%) FOR I%=1% TO 6%
12050	  GOTO 12040 IF FNN%(4%)=0% &
	\ PRINT USING U1$, "BUDGET",T(1%) &
	\ PRINT USING U1$, "BUDGET ADJUSTMENTS",T(2%) &
	\ PRINT USING U1$, "COST TO YEAR",T(3%) &
	\ PRINT USING U1$, "COST TO MONTH",T(4%) &
	\ PRINT USING U1$, "TEMP COSTS",T(5%)
12060	  GOTO 1020
12100	  GOTO 12050
32767	  END
