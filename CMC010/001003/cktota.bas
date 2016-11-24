1	  ! &
	  ! Program name: cktota		Compiled with SCALE 0 on V07.0 &
	  ! Decompiled on 24-Nov-16 at 02:11 AM
10	  !
20	  PRINT "ENTER MONTH FOR FILE NAME (MMM)"; &
	\ INPUT LINE M$ &
	\ M$=CVT$$(M$,4%) &
	\ GOTO 10000 IF M$="" &
	\ GOTO 20 IF LEN(M$)<>3% AND INSTR(1%,M$,".")=0% &
	\ M$="CK"+M$+".DA1" IF INSTR(1%,M$,".")=0% &
	\ M$=LEFT(M$,LEN(M$)-1%)+"1" IF RIGHT(M$,LEN(M$))="T" &
	\ GOTO 20 IF FNO%(2%,M$,1%) &
	\ H1$="REC# DATE CK # ACCT # SR DESCRIPTION    UPDATE  JOB #     "+"AMOUNT  RUNNING TOTAL"
1000	  !
1020	  PRINT  &
	\ INPUT "OPTION ";K$ &
	\ K$=LEFT(K$,3%) &
	\ GOTO 2000 IF K$="LIS" &
	\ GOTO 3000 IF K$="TOT" &
	\ GOTO 4000 IF K$="CHE" &
	\ GOTO 2500 IF K$="SCA" &
	\ GOTO 10000 IF K$="END" &
	\ IF K$<>"" THEN  &
		  PRINT  &
		\ PRINT "TYPE <RETURN> FOR AN OPTIONS LIST." &
		\ GOTO 1020
1030	  PRINT "OPTIONS ARE:" &
	\ PRINT '   "LIST" CKDATA FILE WITH RUNNING TOTAL IN ORDER OF ENTRY' &
	\ PRINT '   "SCAN" A GIVEN RANGE OF THE CKDATA FILE' &
	\ PRINT '   "TOTAL" CKDATA FILE BY GENERAL LEDGER # AND PRINT' &
	\ PRINT '   "CHECK" ACCOUNTS AGAINST THOSE IN CHART (USE AFTER TOTAL)' &
	\ PRINT '   "END" PROGRAM' &
	\ GOTO 1020
2000	  !
2010	  F%=0% &
	\ S%=1% &
	\ T=0. &
	\ P%=0% &
	\ PRINT  &
	\ J$,J1$="" &
	\ INPUT "LIST HOW  (TYPE ? FOR HELP )";L$ &
	\ GOTO 1020 IF L$="" &
	\ GOTO 2020 IF L$="B" OR L$="F" &
	\ GOTO 2015 IF ASCII(L$)>=ASCII("0") AND ASCII(L$)<=ASCII("9") &
	\ PRINT "OPTIONS ARE:" &
	\ PRINT "?          PRINT THIS HELP MESSAGE." &
	\ PRINT "F          LIST ALL RECORDS IN THE ORDER THEY WERE ENTERED." &
	\ PRINT "B          LIST ALL RECORDS IN BACKWARD ORDER." &
	\ PRINT "A SEQ. #   LIST THE RECORD OF THE SEQUENTIAL NUMBER ENTERED." &
	\ PRINT '<CR>       RETURN TO "OPTION ?".' &
	\ PRINT  &
	\ GOTO 2010
2015	  F%=-1% &
	\ R2%=VAL(L$) &
	\ GOTO 2030
2020	  INPUT "LIST WHAT (TYPE ? FOR HELP) ";A2$ &
	\ GOTO 2100 IF A2$="?" &
	\ I%=INSTR(1%,A2$,"/") &
	\ SOURCE$="" &
	\ SOURCE$=RIGHT(A2$,I%+1%) IF I% &
	\ A2$=LEFT(A2$,I%-1%) IF I% &
	\ IF LEFT(A2$,1%)="J" THEN  &
		  J$=RIGHT(A2$,2%) &
		\ A2$="" &
		\ J$=LEFT(J$+"      ",6%)
2021	  IF LEFT(A2$,1%)="P" THEN  &
		  P$=RIGHT(A2$,2%) &
		\ A2$="" &
		\ P$=LEFT(P$+"      ",6%)
2022	  IF LEFT(A2$,1%)="C" OR LEFT(A2$,1%)="A" THEN  &
		  J2$=RIGHT(A2$,2%) &
		\ J1$=LEFT(A2$,1%) &
		\ A2$=""
2024	  IF A2$<>"" THEN  &
		  A2$=" "+A2$ UNLESS A2$="BAD"
2025	  INPUT "SET PAGE ";S$ &
	\ PRINT "TYPE <RETURN KEY> AFTER SETTING THE PAGE." IF S$="?" &
	\ PRINT 'TYPE "S" IF YOU ARE USING A SCOPE' IF S$="?" &
	\ PRINT  IF S$="?" &
	\ GOTO 2025 IF S$="?" &
	\ PRINT  FOR P%=1% TO 6% &
	\ PRINT H1$ &
	\ PRINT  &
	\ P%=4% &
	\ FIELD #2%, FNF%(2%,0%) AS E$,2% AS H$ IF L$="B" &
	\ R2%=CVT$%(H$) IF L$="B" &
	\ S%=-1% IF L$="B" &
	\ R2%=1% IF L$="F" &
	\ GOTO 2030
2030	  B%=FNF%(2%,R2%)
2053	  FIELD #2%, B% AS E$,6% AS X1$,8% AS X2$,2% AS X3$,8% AS A1$,2% AS X4$,2% AS X5$,2% AS X6$,27% AS X7$,1% AS X8$,6% AS X9$
2060	  R%=FNR%(2%) &
	\ GOTO 2080 IF X1$="DDDDDD" &
	\ IF A2$="BAD" THEN &
		  GOTO 2080 IF LEN(CVT$$(X2$,2%))=6% AND MID(X2$,5%,1%)="." &
	  ELSE &
		  GOTO 2080 IF LEFT(X2$,LEN(A2$))<>A2$ OR LEFT(X9$,LEN(J$))<>J$ &
		\ IF P$<>"" THEN  &
			  GOTO 2080 IF MID(X7$,21%,6%)<>P$ &
			\ PRINT "PO=";P$
2065	  GOTO 2080 IF (J1$="C" OR J1$="A") AND (LEFT(X7$,LEN(J2$))<>J2$ OR X3$<>"CR" AND X3$<>"AR" AND X3$<>"AP") &
	\ GOTO 2080 IF SOURCE$<>"" AND X3$<>SOURCE$
2070	  PRINT USING "#### ##", R%,CVT$%(X5$); &
	\ PRINT " ";X1$;X2$;X3$;" ";LEFT(X7$,20%);X8$;X9$; &
	\ A=CVT$F(A1$) &
	\ T=T+A &
	\ PRINT USING "#,###,###.## ###,###,###.##", A,T &
	\ IF S$<>"S" THEN  &
		  P%=P%+1% &
		\ IF P%>=56% THEN  &
			  PRINT  FOR P%=P% TO 63% &
			\ PRINT  FOR P%=1% TO 4% &
			\ PRINT H1$ &
			\ PRINT  &
			\ P%=4%
2080	  B%=FNN%(2%,S%) &
	\ GOTO 2010 IF FNS%<>0% OR F%<>0% &
	\ GOTO 2053
2100	  !
2110	  PRINT  &
	\ PRINT "YOUR CHOICES ARE:" &
	\ PRINT "___.__    PRINT ALL RECORDS WITH ACCOUNT NUMBER ___.__" &
	\ PRINT "J____     PRINT ALL RECORDS WITH JOB # ____" &
	\ PRINT "P____     PRINT ALL RECORDS WITH PO # ____" &
	\ PRINT "C______   PRINT ALL RECORDS FOR CUSTOMER # ______" &
	\ PRINT "A______   PRINT ALL RECORDS FOR VENDOR # ______" &
	\ PRINT "BAD       PRINT ALL RECORDS WITH BAD ACCOUNT NUMBER FORMAT" &
	\ PRINT "<RETURN>  PRINT ALL RECORDS" &
	\ PRINT  &
	\ PRINT "ADD A '/--' TO THE ABOVE ITEMS TO FURTHER LIMIT BY" &
	\ PRINT "SOURCE CODE. FOR EXAMPLE: 'J10122/PR' WOULD PRINT ONLY" &
	\ PRINT "ITEMS IN PAYROLL WITH JOB #10122" &
	\ PRINT  &
	\ GOTO 2020
2500	  !
2510	  S%=1% &
	\ T=0. &
	\ P%=0% &
	\ PRINT  &
	\ J$,J1$="" &
	\ INPUT "LIST SEQ  (TYPE ? FOR HELP )";L$ &
	\ GOTO 1020 IF L$="" &
	\ GOTO 2520 UNLESS L$="?"
2515	  PRINT "OPTIONS ARE:" &
	\ PRINT "?          PRINT THIS HELP MESSAGE." &
	\ PRINT "A SEQ #    LIST THE RECORD OF THE SEQUENTIAL NUMBER ENTERED" &
	\ PRINT "XXX-XXX    LIST THE RECORDS BETWEEN THE TWO NUMBERS ENTERED" &
	\ PRINT "-XXX       PRINT FROM START OF FILE TO XXX" &
	\ PRINT "XXX-       PRINT FROM XXX TO END OF FILE" &
	\ PRINT "-          PRINT ALL ITEMS IN FILE" &
	\ PRINT '<CR>       RETURN TO "OPTION ?".' &
	\ PRINT  &
	\ GOTO 2510
2520	  F%=0% &
	\ I%=INSTR(1%,L$,"-") &
	\ R2%=VAL(LEFT(L$,I%-1%)) &
	\ A1%=VAL(RIGHT(L$,I%+1%)) &
	\ R2%=A1% IF I%=0% &
	\ S%=SGN(A1%-R2%) &
	\ S%=1% IF S%=0% &
	\ R2%=1% IF LEFT(L$,1%)="-"
2530	  INPUT "LIST WHAT (TYPE ? FOR HELP) ";A2$ &
	\ GOTO 2620 IF A2$="?" &
	\ IF LEFT(A2$,1%)="J" THEN  &
		  J$=RIGHT(A2$,2%) &
		\ A2$="" &
		\ J$=LEFT(J$+"      ",6%)
2531	  IF LEFT(A2$,1%)="C" OR LEFT(A2$,1%)="A" THEN  &
		  J2$=RIGHT(A2$,2%) &
		\ J1$=LEFT(A2$,1%) &
		\ A2$=""
2532	  A3$="" &
	\ IF LEFT(A2$,1%)="B" THEN  &
		  J=FIX(VAL(RIGHT(A2$,2%))*100.) &
		\ A2$="" &
		\ A3$="B"
2540	  IF LEFT(A2$,1%)="P" THEN  &
		  P$=RIGHT(A2$,2%) &
		\ A2$="" &
		\ P$=LEFT(P$+"      ",6%)
2550	  IF A2$<>"" THEN  &
		  A2$=" "+A2$ UNLESS A2$="BAD"
2560	  INPUT "SET PAGE ";S$ &
	\ PRINT "TYPE <RETURN KEY> AFTER SETTING THE PAGE." IF S$="?" &
	\ PRINT 'TYPE "S" IF YOU ARE USING A SCOPE' IF S$="?" &
	\ PRINT  IF S$="?" &
	\ GOTO 2560 IF S$="?" &
	\ PRINT  FOR P%=1% TO 6% &
	\ PRINT H1$ &
	\ PRINT  &
	\ P%=4% &
	\ FIELD #2%, FNF%(2%,0%) AS E$,2% AS H$ &
	\ I%=CVT$%(H$) &
	\ R2%=I% IF R2%>I% &
	\ A1%=I% IF A1%>I% OR RIGHT(L$,LEN(L$))="-" &
	\ S%=SGN(A1%-R2%) &
	\ S%=1% IF S%=0%
2570	  B%=FNF%(2%,R2%)
2580	  FIELD #2%, B% AS E$,6% AS X1$,8% AS X2$,2% AS X3$,8% AS A1$,2% AS X4$,2% AS X5$,2% AS X6$,27% AS X7$,1% AS X8$,6% AS X9$
2590	  R%=FNR%(2%) &
	\ GOTO 2610 IF X1$="DDDDDD" &
	\ IF A2$="BAD" THEN &
		  GOTO 2610 IF LEN(CVT$$(X2$,2%))=6% AND MID(X2$,5%,1%)="." &
	  ELSE &
		  GOTO 2610 IF LEFT(X2$,LEN(A2$))<>A2$ OR LEFT(X9$,LEN(J$))<>J$ &
		\ IF P$<>"" THEN  &
			  GOTO 2610 IF MID(X7$,21%,6%)<>P$ &
			\ PRINT "PO=";P$
2595	  A=CVT$F(A1$) &
	\ T=T+A &
	\ GOTO 2610 IF A3$="B" AND FIX(T*100.)<>J
2597	  GOTO 2610 IF (J1$="C" OR J1$="A") AND (LEFT(X7$,LEN(J2$))<>J2$ OR X3$<>"CR" AND X3$<>"AR" AND X3$<>"AP")
2600	  PRINT USING "#### ##", R%,CVT$%(X5$); &
	\ PRINT " ";X1$;X2$;X3$;" ";LEFT(X7$,20%);X8$;X9$; &
	\ PRINT USING "#,###,###.## ###,###,###.##", A,T &
	\ IF S$<>"S" THEN  &
		  P%=P%+1% &
		\ IF P%>=56% THEN  &
			  PRINT  FOR P%=P% TO 63% &
			\ PRINT  FOR P%=1% TO 4% &
			\ PRINT H1$ &
			\ PRINT  &
			\ P%=4%
2610	  R2%=R2%+S% &
	\ GOTO 2510 IF R2%=A1%+S% &
	\ B%=FNF%(2%,R2%) &
	\ GOTO 2580
2620	  !
2630	  PRINT  &
	\ PRINT "YOUR CHOICES ARE:" &
	\ PRINT "___.__    PRINT ALL RECORDS WITH ACCOUNT NUMBER ___.__" &
	\ PRINT "J____     PRINT ALL RECORDS WITH JOB # ____" &
	\ PRINT "P____     PRINT ALL RECORDS WITH PO # ____" &
	\ PRINT "B_____    PRINT ALL RECORDS WITH RUNNING TOTAL = _____" &
	\ PRINT "C______   PRINT ALL RECORDS FOR CUSTOMER # ______" &
	\ PRINT "A______   PRINT ALL RECORDS FOR VENDOR # ______" &
	\ PRINT "BAD       PRINT ALL RECORDS WITH BAD ACCOUNT NUMBER FORMAT" &
	\ PRINT "<RETURN>  PRINT ALL RECORDS" &
	\ PRINT  &
	\ GOTO 2530
3000	  !
3005	  INPUT "PRINT ALL ACCOUNT TOTALS (Y/N) ";C$ &
	\ C$=LEFT(C$,1%) &
	\ G%=0% &
	\ R2%=1% &
	\ T(I%)=0. FOR I%=1% TO 400% &
	\ B%=0% &
	\ T=0. &
	\ DIM T(400%), G$(400%) &
	\ B%=FNF%(2%,R2%)
3030	  FIELD #2%, B% AS E$,6% AS X1$,8% AS X2$,2% AS X3$,8% AS A1$ &
	\ GOTO 3070 IF X1$="DDDDDD"
3035	  IF C$<>"Y" THEN  &
		  T=T+CVT$F(A1$) &
		\ GOTO 3070
3040	  FOR I%=1% TO G% &
		\ GOTO 3060 IF X2$=G$(I%) &
	\ NEXT I% &
	\ G%=G%+1% &
	\ G$(G%)=X2$+"" &
	\ I%=G%
3060	  T(I%)=T(I%)+CVT$F(A1$)
3070	  B%=FNN%(2%,1%) &
	\ GOTO 3100 IF FNS% &
	\ GOTO 3030
3100	  PRINT  &
	\ FOR I%=1% TO G% &
		\ PRINT USING "\        \ ###,###,###.##", G$(I%),T(I%) &
		\ T=T+T(I%) &
	\ NEXT I% &
	\ PRINT USING "    GRAND TOTAL ###,###,###.##", T &
	\ GOTO 1020
4000	  !
4010	  IF G%=0% THEN  &
		  PRINT "YOU MUST RUN THE TOTAL OPTION AND SPECIFY 'Y'" &
		\ PRINT "FOR THE 'PRINT ALL ACCOUNT TOTALS' QUESTION BEFORE" &
		\ PRINT "USING THIS OPTION !!!!" &
		\ GOTO 1020
4020	  IF FNO%(4%,"CHART.DAT",1%) THEN  &
		  PRINT "I CANNOT FIND CHART FILE" &
		\ GOTO 1020
4030	  INPUT "SET PAGE";K$ &
	\ X1%=66% &
	\ F%=0%
4040	  FOR I%=1% TO G% &
		\ E2$=" "+LEFT(G$(I%),7%) &
		\ V%=FNB%(4%,E2$)
4045		  FIELD #4%, V% AS E$,8% AS E$,2% AS E1$ &
		\ GOTO 4050 IF E$=E2$ AND CVT$%(E1$)<>-1% &
		\ IF E$=E2$ THEN  &
			  V%=FNN%(4%,-1%) &
			\ GOTO 4045 UNLESS FNS%=-1%
4047		  F%=-1% &
		\ GOSUB 4100 IF X1%>60% &
		\ PRINT USING "\      \  ###,###,##.##", G$(I%),T(I%) &
		\ X1%=X1%+1%
4050			  NEXT I% &
	\ PRINT  &
	\ PRINT "NO BAD ACCOUNTS FOUND !!! IF F%=0% &
	\ PRINT  &
	\ V%=FNC%(4%) &
	\ GOTO 1020
4100	  PRINT  FOR X1%=X1% TO 65% &
	\ PRINT  &
	\ PRINT  &
	\ PRINT  &
	\ PRINT "LISTING OF BAD ACCOUNT NUMBERS" &
	\ PRINT  &
	\ X1%=5% &
	\ RETURN
10000	  CLOSE 1% &
	\ CLOSE 2% &
	\ E$=SYS(CHR$(2%)) &
	\ IF ASCII(SYS(CHR$(7%)))=255% THEN &
		  CHAIN "!MENU" 0. &
	  ELSE &
		  V$=SYS(CHR$(8%)) &
		\ GOTO 32767
32767	  END
