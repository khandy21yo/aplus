10	  ! &
	  ! Program name: ckdata		Compiled with SCALE 0 on V08.0 &
	  ! Decompiled on 24-Nov-16 at 02:11 AM
20	  OPEN "KB:" AS FILE 10% &
	\ B%=-1% &
	\ IF FNO%(6%,"CHART.DAT","/RO","") THEN  &
		  B%=0%
30	  INPUT "MONTH NAME (MMM)";V$ &
	\ GOTO 10000 IF V$="" &
	\ C$=V$ &
	\ C$=LEFT(C$,3%) IF INSTR(1%,C$,".")=0% &
	\ GOTO 50 IF INSTR(1%,V$,".")<>0% &
	\ C$="CK"+V$+" .DAT" &
	\ GOTO 50 IF LEN(V$)<4%
40	  PRINT "BAD MONTH FOR CKDATA FILE NAME --"; &
	\ PRINT " USE 3 CHARACTERS OF MONTH NAME" &
	\ GOTO 30
50	  F$=C$ &
	\ IF FNO%(2%,C$,"/RW","")=0. THEN  &
		  PRINT "THAT FILE EXISTS."; &
		\ PRINT "  IT IS CURRENTLY EMPTY." IF FNG%(2%,"") &
		\ PRINT  &
		\ GOTO 100
60	  PRINT "Error";FNS%;"while opening ";C$;"  Aborting. . . ";FNX%("",0%,"") IF FNS%<>5% &
	\ V%=FNC%(2%) &
	\ GOTO 40 IF INSTR(1%,V$,".")<>0% &
	\ PRINT "THAT FILE DOES NOT EXIST.  "; &
	\ INPUT "SHOULD A NEW FILE BE OPENED (Y/N)";V$ &
	\ GOTO 10000 IF V$<>"Y"
70	  IF FNO%(2%,C$,"/CR:16,64","") THEN  &
		  PRINT "Error";FNS%;"while opening ";C$ &
		\ PRINT "Aborting. . . ";FNX%("",0%,"")
100	  !
110	  OPEN "KB:" AS FILE 12%, RECORDSIZE 64% &
	\ FIELD #12%, 6% AS P$,8% AS P1$,2% AS P2$,8% AS P3$,2% AS P4$,2% AS P5$,2% AS P6$,28% AS P7$,6% AS P8$ &
	\ FIELD #12%, 64% AS T$
120	  H4$="##/##/##  \\  \    \ \"+SPACE$(26%)+"\  \    \ \     \" &
	\ H5$="#####,###.##  " &
	\ H6$="#####,###.##CR" &
	\ H7$="MO DA YR SRC CHECK # DESCRIPTION"+SPACE$(19%)+"JOB #   ACCT #      AMOUNT"
1000	  !
1020	  PRINT  &
	\ INPUT "OPTION ";K$ &
	\ K$=LEFT(K$,3%) &
	\ IF K$<>"" THEN  &
		  GOTO 3000 IF K$="ENT" &
		\ GOTO 5000 IF K$="CHA" &
		\ GOTO 6000 IF K$="EXA" &
		\ GOTO 7000 IF K$="PRI" &
		\ GOTO 9000 IF K$="MIS" &
		\ GOTO 10000 IF K$="END" &
		\ PRINT  &
		\ PRINT "TYPE <CR> FOR OPTIONS." &
		\ GOTO 1020
1030	  PRINT "NOTE--USE THE CHANGE OPTION TO DELETE A RECORD." &
	\ PRINT "OPTIONS:" &
	\ PRINT '  "ENTER" NEW RECORDS' &
	\ PRINT '  "CHANGE" OR DELETE EXISTING INFORMATION' &
	\ PRINT '  "EXAMINE" RECORDS' &
	\ PRINT '  "PRINT" CHECK REGISTER (BY SOURCE CODE)' &
	\ PRINT '  "MISSING" CHECK NUMBER LISTING' &
	\ PRINT '  "END" PROGRAM AND UPDATE FILES' &
	\ GOTO 1020
2000	  !
2010	  RSET P$=L$ &
	\ RSET P1$=A$ &
	\ LSET P2$=L2$ &
	\ LSET P3$=CVTF$(L) &
	\ LSET P4$=CVT%$(M1%) &
	\ LSET P5$=CVT%$(D1%) &
	\ LSET P6$=CVT%$(Y1%) &
	\ LSET P7$=L1$ &
	\ LSET P8$=L5$ &
	\ RETURN
2100	  !
2110	  LSET T$=FNL$ &
	\ L$=P$+"" &
	\ L1$=P7$+"" &
	\ L2$=P2$+"" &
	\ A$=P1$+"" &
	\ L=CVT$F(P3$) &
	\ M1%=CVT$%(P4$) &
	\ D1%=CVT$%(P5$) &
	\ Y1%=CVT$%(P6$) &
	\ L5$=P8$+"" &
	\ RETURN
2200	  !
2210	  PRINT USING H4$, M1%,D1%,Y1%,L2$,L$,L1$,L5$,A$; &
	\ PRINT USING H5$, L IF L>=0. &
	\ PRINT USING H6$, -L IF L<0. &
	\ RETURN
3000	  !
3010	  T1=0. &
	\ PRINT  &
	\ PRINT 'TYPE "?" AT "CHECK #" FOR HELP MESSAGE.' &
	\ INPUT "SOURCE CODE";L2$ &
	\ LSET P8$="" &
	\ LSET P7$="" &
	\ PRINT  &
	\ PRINT "CHECK #   MO.DA.YR    ACCT #  JOB #      AMOUNT   DESCRIPTION"
3030	  ON ERROR GOTO 3170 &
	\ V$=SYS(CHR$(3%))
3040	  PRINT ">"; &
	\ INPUT #10%, K$ &
	\ K$=CVT$$(K$,-1%) &
	\ IF K$="." THEN  &
		  V$=SYS(CHR$(2%)) &
		\ PRINT  &
		\ ON ERROR GOTO 0 &
		\ GOTO 1020
3050	  K$=NUM1$(VAL(L$)+1.) IF K$="+" &
	\ K$=L$+"" IF K$="" &
	\ IF K$="?" THEN  &
		  GOSUB 3200 &
		\ GOTO 3040
3060	  K$=SPACE$(6.-LEN(K$))+K$ &
	\ PRINT USING "\    \   ", K$; &
	\ L$=K$ &
	\ INPUT #10%, V$ &
	\ IF V$="" THEN  &
		  PRINT USING "##/##/##  ", M1%,D1%,Y1%; &
		\ GOTO 3090
3070	  IF V$="-" THEN  &
		  PRINT "LINE ABORTED." &
		\ GOTO 3040
3080	  I%=INSTR(1%,V$,".") &
	\ V$=V$+"."+RIGHT(DATE$(0%),8%) IF INSTR(I%+1%,V$,".")=0% &
	\ X%=INSTR(I%+1%,V$,".") &
	\ M1%=VAL(LEFT(V$,I%-1%)) &
	\ D1%=VAL(MID(V$,I%+1%,X%-1%)) &
	\ Y1%=VAL(RIGHT(V$,X%+1%)) &
	\ PRINT USING "##/##/##  ", M1%,D1%,Y1%;
3090	  INPUT LINE #10%, A$ &
	\ A$=CVT$$(A$,-1%) &
	\ GOTO 3180 IF A$="-" &
	\ IF A$="" THEN  &
		  A$=P1$+"" &
		\ GOTO 3120
3110	  A$=FNA$(A$) &
	\ IF B% AND FNG%(6%,SPACE$(8%-LEN(CVT$$(A$,-1%)))+CVT$$(A$,-1%)) THEN  &
		  PRINT "NONEXISTENT G/L #" &
		\ GOTO 3180
3120	  PRINT USING " \     \  ", A$; &
	\ INPUT LINE #10%, L5$ &
	\ L5$=CVT$$(L5$,4%) &
	\ PRINT "LINE ABORTED." IF L5$="-" &
	\ GOTO 3030 IF L5$="-" &
	\ L5$=P8$+"" IF L5$="" &
	\ L5$=SPACE$(6%) IF L5$="0" &
	\ PRINT USING "\    \ ", L5$; &
	\ INPUT #10%, C$ &
	\ IF C$="-" THEN  &
		  PRINT "LINE ABORTED." &
		\ GOTO 3030
3130	  C$=NUM1$(L*100.) IF C$="" &
	\ N%=INSTR(1%,C$,"-") &
	\ N1%=INSTR(1%,C$,"CR") &
	\ IF N%<2% AND N1%=0% THEN  &
		  L=VAL(C$) &
		\ GOTO 3150
3140	  C$=LEFT(C$,N%-1%) IF N%>1% &
	\ C$=LEFT(C$,N1%-1%) IF N1%>1% &
	\ L=-VAL(C$)
3150	  L=L/100. &
	\ PRINT USING "###,###.##-  ", L; &
	\ INPUT LINE #10%, L1$ &
	\ L1$=CVT$$(L1$,4%) &
	\ PRINT "DESC.>27 CHAR. "; IF LEN(L1$)>27% &
	\ GOTO 3180 IF L1$="-" OR LEN(L1$)>27% &
	\ L1$=P7$+"" IF L1$="" &
	\ L1$="" IF L1$="0" &
	\ PRINT CVT$$(L1$,128%);TAB(59%); &
	\ GOSUB 3500 &
	\ GOSUB 2000 &
	\ STOP IF FNA%(2%,T$) &
	\ GOTO 3030
3170	  RESUME 3180
3180	  PRINT "INPUT ERROR!  "; IF ERR<>0% &
	\ PRINT "LINE ABORTED." &
	\ GOTO 3030
3200	  !
3210	  PRINT  &
	\ PRINT "THE FOLLOWING KEYS HAVE SPECIAL FUNCTIONS." &
	\ PRINT '?     AT "CHECK #" WILL GIVE YOU THIS HELP MESSAGE.' &
	\ PRINT '+     AT "CHECK #" WILL ADVANCE PREVIOUS CHECK NUMBER BY ONE.' &
	\ PRINT '<CR>  AT "CHECK #" WILL REPEAT THE PREVIOUS CHECK NUMBER.' &
	\ PRINT '0     AT "JOB #" OR "DESCRIPTION" MAKES BLANK ENTRY.' &
	\ PRINT '-     ANYWHERE BUT "CHECK #" WILL ABORT THE ENTRY LINE.' &
	\ PRINT '.     AT "CHECK #" WILL RETURN TO "OPTION ?".' &
	\ RETURN
3500	  !
3510	  PRINT STRING$(POS(0%)-59%,8%);CHR$(10%); IF POS(0%)>59% &
	\ T1=T1+L &
	\ PRINT USING "TOTAL:  #,###,###.##", ABS(T1); &
	\ PRINT "CR"; IF T1<0. &
	\ PRINT  &
	\ RETURN
5000	  !
5010	  N%=0% &
	\ PRINT  &
	\ INPUT "CHANGE WHICH CHECK # ";K$ &
	\ K$=CVT$$(K$,4%+8%+128%) &
	\ GOTO 1020 IF K$="" &
	\ L$=SPACE$(6%-LEN(K$))+K$ &
	\ PRINT "AND ACCT # ? "; &
	\ INPUT LINE #10%, A2$ &
	\ A2$=FNA$(A2$) &
	\ A$=A2$ &
	\ IF FNG%(2%,L$+A$)<>0% THEN  &
		  PRINT "NOT FOUND.  "; &
		\ PRINT "TRY A NUMBER NEAR THE DESIRED RECORD AND USE + OR -" &
		\ PRINT "TO FIND THE CORRECT ONE." &
		\ GOTO 5010
5020	  GOSUB 2100 &
	\ GOSUB 2200 &
	\ PRINT  &
	\ INPUT "Y=CHANGE RECORD; ?=HELP";V$ &
	\ GOTO 5200 IF V$="?" &
	\ GOTO 5010 IF V$="" &
	\ IF V$<>"Y" THEN  &
		  GOTO 5090 IF V$="D" &
		\ X%=ASCII(V$) &
		\ I%=ASCII(RIGHT(V$,2%)) &
		\ GOTO 5070 IF X%>47% AND X%<58% OR X%=45% AND (I%>47% AND I%<58%) &
		\ V%=FNN%(-2%) IF V$="-" &
		\ V%=FNN%(2%) IF V$<>"-" &
		\ GOTO 5010 IF V% &
		\ GOTO 5020
5040	  INPUT "CHANGE ITEM (?=HELP)";V$ &
	\ GOTO 5100 IF V$="?" &
	\ GOTO 5000 IF V$="" AND N%=0% &
	\ GOTO 5055 IF V$="" AND N%=-1% &
	\ GOTO 5040 IF INSTR(1%,"CH AC SO JO AM DA DE",V$)=0% &
	\ V%=INSTR(1%,"CH AC SO JO AM DA DE",V$) &
	\ N%=-1% &
	\ ON V%/3%+1% GOTO 5041,5042,5043,5044,5045,5046,5050
5041	  PRINT "CHECK # : ";L$; &
	\ INPUT "  ";L4$ &
	\ L4$=CVT$$(L4$,-1%) &
	\ L$=L4$+"" IF L4$<>"" &
	\ GOTO 5040
5042	  PRINT "ACCT # : ";A$; &
	\ INPUT "  ";A3$ &
	\ A3$=FNA$(A3$) IF A3$<>"" &
	\ V%=FNG%(6%,SPACE$(8%-LEN(CVT$$(A3$,-1%)))+CVT$$(A3$,-1%)) &
	\ PRINT "ILLEGAL ACCT #" IF B% AND V% &
	\ GOTO 5042 IF B% AND V% &
	\ A$=A3$+"" IF A3$<>"" &
	\ GOTO 5040
5043	  PRINT "SOURCE  : ";L2$; &
	\ INPUT "  ";K$ &
	\ L2$=K$ IF K$<>"" &
	\ GOTO 5040
5044	  PRINT "JOB #   : ";L5$; &
	\ INPUT "  ";L6$ &
	\ L5$=L6$ IF L6$<>"" &
	\ L5$=SPACE$(6%) IF L6$="0" &
	\ GOTO 5040
5045	  ON ERROR GOTO 5094 &
	\ PRINT USING "AMOUNT  : ####,###.##", L; &
	\ INPUT "  ";K$ &
	\ L=VAL(K$) IF K$<>"" &
	\ ON ERROR GOTO 0 &
	\ GOSUB 5300 IF K$<>"" &
	\ GOTO 5040
5046	  ON ERROR GOTO 5094 &
	\ PRINT "DATE (MM.DD.YY)   : ";NUM1$(M1%);".";NUM1$(D1%);".";NUM1$(Y1%); &
	\ INPUT "  ";K$ &
	\ GOTO 5040 IF K$="" &
	\ V%=INSTR(1%,K$,".") &
	\ PRINT "IMPROPER FORMAT!" IF V%=0% &
	\ GOTO 5046 IF V%=0% &
	\ M1%=VAL(LEFT(K$,V%-1%)) &
	\ D1%=VAL(MID(K$,V%+1%,INSTR(V%+1%,K$,".")-V%-1%)) &
	\ Y1%=VAL(RIGHT(K$,INSTR(V%+1%,K$,".")+1%)) &
	\ ON ERROR GOTO 0 &
	\ GOTO 5040
5050	  PRINT "DESC : ";L1$; &
	\ INPUT K$ &
	\ PRINT "TOO LONG--MAXIMUM OF 27 CHAR." IF LEN(K$)>27% &
	\ GOTO 5050 IF LEN(K$)>27% &
	\ L1$=K$ IF K$<>"" &
	\ L1$="" IF K$="0" &
	\ GOTO 5040
5055	  IF L4$+A3$="" THEN  &
		  GOSUB 2000 &
		\ V%=FNU%(2%,T$) &
		\ V%=FNN%(2%) &
		\ GOTO 5020
5060	  A1$=L$ &
	\ K$=A$ &
	\ L$=L4$ IF L4$<>"" &
	\ A$=A3$+"" IF A3$<>"" &
	\ GOSUB 2000 &
	\ STOP IF FNU%(-2%,T$) &
	\ V%=FNG%(2%,A1$+K$) &
	\ L4$="" &
	\ A3$="" &
	\ GOSUB 5300 &
	\ GOTO 5010
5070	  T1=VAL(L4$) &
	\ GOTO 5020 IF FNG%(2%,L$+A$) &
	\ GOSUB 2100
5080	  GOTO 5020 IF L=T1 &
	\ GOTO 5020 IF FNN%(2%) &
	\ GOSUB 2100 &
	\ GOTO 5080
5090	  INPUT "DELETE CHECK (Y OR N)";V$ &
	\ GOTO 5010 IF LEFT(V$,1%)<>"Y" &
	\ IF FND%(2%,"") THEN  &
		  PRINT "ERROR";FNS%;"DURING DELETE.  CALL CMC!" &
		\ GOTO 10000
5091	  PRINT "CHECK DELETED!" &
	\ GOSUB 5300 &
	\ V%=FNN%(2%) &
	\ GOTO 5010
5094	  PRINT "ILLEGAL RESPONSE!" &
	\ RESUME 
5100	  !
5110	  PRINT  &
	\ PRINT "YOUR CHOICES ARE:" &
	\ PRINT '<CR>  RETURN TO "ENTER # OF THE CHECK TO BE CHANGED ?"' &
	\ PRINT "?     PRINT THIS HELP MESSAGE." &
	\ PRINT "CH    CHECK #" &
	\ PRINT "AC    ACCOUNT #" &
	\ PRINT "SO    SOURCE CODE" &
	\ PRINT "JO    JOB #" &
	\ PRINT "AM    AMOUNT" &
	\ PRINT "DA    DATE" &
	\ PRINT "DE    DESCRIPTION" &
	\ PRINT "NOTES:  1.  TYPE <CR> (RETURN) AT ANY QUESTION FOR NO CHANGE." &
	\ PRINT "        2.  TYPE 0 (ZERO) AT JOB # OR DESRIPTION QUESTIONS" &
	\ PRINT "            TO ERASE THOSE ITEMS.  NO OTHERS MAY BE ERASED." &
	\ PRINT  &
	\ GOTO 5040
5200	  !
5210	  PRINT  &
	\ PRINT "YOUR CHOICES ARE:" &
	\ PRINT "?     PRINT THIS HELP MESSAGE." &
	\ PRINT "Y     YES, CHANGE THIS RECORD." &
	\ PRINT '<CR>  RETURN TO "CHECK NUMBER TO CHANGE?". (<CR>=RETURN KEY)' &
	\ PRINT "D     DELETE THIS RECORD." &
	\ PRINT "-     GO BACK ONE RECORD." &
	\ PRINT "+     GO FOREWARD ONE RECORD." &
	\ PRINT "Amt.  GET THE RECORD WITH THIS $ AMOUNT." &
	\ PRINT  &
	\ GOTO 5020
5300	  !
5310	  ON ERROR GOTO 5320 &
	\ KILL "TT0:GL.DAS"
5320	  RESUME 5330
5330	  ON ERROR GOTO 0 &
	\ RETURN
6000	  !
6010	  IF FNG%(2%,"") THEN  &
		  PRINT "FILE IS EMPTY!" &
		\ GOTO 1020
6020	  PRINT 'TYPE <CR> AT "FROM CK #" AND "TO CK #" TO PRINT WHOLE FILE.'
6030	  PRINT  &
	\ INPUT 'FROM CK # ("." TO EXIT) ';K$ &
	\ GOTO 1020 IF K$="." &
	\ K$=SPACE$(6%-LEN(K$))+K$ &
	\ T1=0. &
	\ X1%=0% &
	\ INPUT "TO CK #                 ";A2$ &
	\ GOTO 6030 IF A2$="." &
	\ X%=0% &
	\ X%=-1% IF A2$="" &
	\ A1$=SPACE$(6%-LEN(A2$))+A2$ &
	\ V%=FNG%(2%,K$) &
	\ GOSUB 2100 &
	\ GOTO 6030 IF L$>A1$ AND X%=0% &
	\ PRINT  FOR X1%=1% TO 6% &
	\ PRINT H7$ &
	\ PRINT  &
	\ X1%=4%
6040	  IF P$>A1$ AND X%=0% THEN &
		  GOTO 6060 &
	  ELSE &
		  GOSUB 6100 IF X1%>55% &
		\ GOSUB 2200 &
		\ T1=T1+L &
		\ X1%=X1%+1%
6050	  IF FNN%(2%)=0% THEN  &
		  GOSUB 2100 &
		\ GOTO 6040
6060	  PRINT SPACE$(60%);"TOTAL"; &
	\ IF T1<0. THEN  &
		  PRINT USING H6$, -T1 &
		\ GOTO 6030
6070	  PRINT USING H5$, T1 &
	\ GOTO 6030
6100	  !
6110	  PRINT  FOR X1%=X1% TO 67% &
	\ PRINT H7$ &
	\ PRINT  &
	\ X1%=4% &
	\ RETURN
7000	  !
7010	  PRINT  &
	\ INPUT "SOURCE  CODE   <RETURN FOR ALL> ";L4$ &
	\ INPUT "ACCOUNT NUMBER <RETURN FOR ALL> ";L9$ &
	\ IF L9$<>"" THEN  &
		  L9$=CVT$$(L9$,-1%) &
		\ L9$=L9$+".00" IF INSTR(1%,L9$,".")=0%
7020	  INPUT "REGISTER DATE (MM.DD.YY) ";K$ &
	\ N%=INSTR(2%,K$,".") &
	\ IF N% THEN  &
		  N1%=INSTR(4%,K$,".") &
		\ GOTO 7040
7030	  N%=INSTR(2%,K$,"/") &
	\ IF N% THEN &
		  N1%=INSTR(4%,K$,"/") &
	  ELSE &
		  GOTO 7020
7040	  M2%=VAL(LEFT(K$,N%-1%)) &
	\ D2%=VAL(MID(K$,N%+1%,N1%-N%-1%)) &
	\ Y2%=VAL(RIGHT(K$,N1%+1%)) &
	\ INPUT "REGISTER HEADING ";C$ &
	\ INPUT "PAGE LENGTH IN INCHES (8.5 OR 11) ";T1 &
	\ X5%=T1*6. &
	\ X1%=X5%+2% &
	\ INPUT "SET PAGE";X% &
	\ X%=0% &
	\ T1,T2,T3=0. &
	\ V%=FNG%(2%,"") &
	\ GOSUB 2100
7050	  IF X1%>X5%-6% THEN  &
		  GOSUB 7300 &
		\ L6$=""
7060	  V8%,V9%=0% &
	\ V8%=V8% OR L2$=L4$ &
	\ V8%=V8% OR L4$="" &
	\ V9%=V9% OR L9$=CVT$$(A$,-1%) &
	\ V9%=V9% OR L9$="" &
	\ GOTO 7070 IF V8%<>0% AND V9%<>0% &
	\ IF FNN%(2%) THEN &
		  GOTO 7415 &
	  ELSE &
		  GOSUB 2100 &
		\ GOTO 7060
7070	  PRINT USING H4$, M1%,D1%,Y1%,L2$,L$,L1$,L5$,A$; &
	\ IF L>=0. THEN  &
		  PRINT USING H5$+SPACE$(16%), L; &
		\ T1=T1+L &
		\ GOTO 7100
7080	  PRINT USING SPACE$(16%)+H6$, -L; &
	\ T2=T2+L
7100	  L6$=L$+"" &
	\ T3=T3+L &
	\ X1%=X1%+1% &
	\ A1$=L$
7110	  IF FNN%(2%) THEN &
		  GOTO 7400 &
	  ELSE &
		  GOSUB 2100 &
		\ V8%,V9%=0% &
		\ V8%=V8% OR L2$=L4$ &
		\ V8%=V8% OR L4$="" &
		\ V9%=V9% OR L9$=CVT$$(A$,-1%) &
		\ V9%=V9% OR L9$="" &
		\ GOTO 7110 UNLESS V8%<>0% AND V9%<>0%
7120	  GOSUB 7130 IF L$<>A1$ &
	\ PRINT  &
	\ GOTO 7050
7130	  PRINT SPACE$(6%); &
	\ PRINT USING H5$, T3; IF T3>=0. &
	\ PRINT USING H6$, -T3; IF T3<0. &
	\ T3=0. &
	\ RETURN
7300	  !
7310	  X%=X%+1% &
	\ PRINT  FOR I%=X1% TO X5%+5% &
	\ X1%=12% &
	\ PRINT TAB(45%);"C H E C K   R E G I S T E R" &
	\ PRINT  &
	\ PRINT USING "\"+SPACE$(38%)+"\"+SPACE$(11%)+"AS OF ##/##/##"+SPACE$(40%)+"PAGE ###", C$,M2%,D2%,Y2%,X% &
	\ PRINT TAB(69%);"---- CURRENT MONTH ----  -- NET CHECK --" &
	\ PRINT "MO DA YR SRC CHECK # DESCRIPTION                   "+"JOB #   ACCT #       DEBIT          CREDIT" &
	\ PRINT  &
	\ RETURN
7400	  !
7410	  GOSUB 7130
7415	  PRINT  &
	\ PRINT  &
	\ PRINT USING SPACE$(39%)+"FINAL TOTALS            $$#,###,###.##  $$#,###,###.##CR    ", T1,-T2; &
	\ IF T1+T2>=0. THEN  &
		  PRINT USING "  $$###,###.##*", T1+T2; &
		\ GOTO 7430
7420	  PRINT USING "  $$###,###.##CR*", -T1-T2;
7430	  PRINT  FOR I%=X1% TO X5% &
	\ GOTO 1020
9000	  !
9010	  IF FNG%(2%,"") THEN  &
		  PRINT "THERE ARE NO CHECKS IN THE FILE." &
		\ GOTO 1020
9020	  PRINT  &
	\ PRINT "MISSING CHECK NUMBERS :"
9030	  A1$=NUM1$(VAL(A2$)+1.) &
	\ A1$=SPACE$(6%-LEN(A1$))+A1$ &
	\ GOSUB 2100 &
	\ I%=ASCII(LEFT(CVT$$(L$,-1%),1%)) &
	\ IF I%>47% AND I%<58% THEN  &
		  A2$=L$ &
		\ IF A2$>A1$ THEN  &
			  PRINT USING "\    \ - \    \", A1$,NUM1$(VAL(A2$)-1.)
9060	  GOTO 9030 IF FNN%(2%)=0% &
	\ RSET A1$=NUM1$(VAL(A2$)+1.) &
	\ PRINT USING "\    \ - INFINITY", A1$ &
	\ GOTO 1020
10000	  !
10010	  CLOSE 10% &
	\ CLOSE 11% &
	\ CLOSE 12% &
	\ V%=FNC%(2%)+FNC%(6%) &
	\ V%=FNX%("",0%,"")
14100	  DEF FNA$(A$) &
	\ A$=CVT$$(A$,-1%) &
	\ GOTO 14110 IF A$="" &
	\ A$=LEFT(A$,LEN(A$)-2%)+"."+RIGHT(A$,LEN(A$)-1%) IF INSTR(1%,A$,".")=0% &
	\ A$=A$+SPACE$(3%-(LEN(A$)-INSTR(1%,A$,"."))) &
	\ A$=SPACE$(8%-LEN(A$))+A$
14110	  FNA$=A$ &
	\ FNEND
32767	  END
