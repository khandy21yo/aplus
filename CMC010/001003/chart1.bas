1	  ! &
	  ! Program name: chart1		Compiled with SCALE 0 on V08.0 &
	  ! Decompiled on 24-Nov-16 at 02:11 AM
10	  !
20	  DIM F$(29%), T$(3%), T%(4%), T(22%), T1(22%), T2$(14%)
25	  GOTO 1000
30	  GOTO 40 UNLESS FNO%(2%,"CHART.DAT","","") &
	\ UNLESS FNS%=5% THEN  &
		  PRINT "ERROR";FNS%;"IN OPENING CHART OF ACCOUNTS.  ABORT." &
		\ GOTO 10000
35	  INPUT "CREATE A FILE (Y/N)";K$ &
	\ GOTO 10000 IF LEFT(K$,1%)<>"Y" &
	\ IF FNO%(2%,"CHART.DAT","/CR:10,256","") THEN  &
		  PRINT "ERROR";FNS%;"IN CREATING CHART OF ACCOUNTS FILE.  ABORT." &
		\ GOTO 10000
40	  OPEN "KB:" FOR INPUT AS FILE 12% &
	\ OPEN "NL:" AS FILE 1%, RECORDSIZE 256% &
	\ FIELD #1%, 8% AS F$(1%),40% AS F$(2%),2% AS F$(3%),2% AS F$(4%),2% AS F$(5%) &
	\ FIELD #1%, 54%+(L%*8%-8%) AS V$,8% AS F$(L%+5%) FOR L%=1% TO 22% &
	\ FIELD #1%, 230%+(L%*1%-1%) AS V$,1% AS F$(L%+27%) FOR L%=1% TO 2% &
	\ FIELD #1%, 256% AS T0$
70	  UNLESS FNG%(2%,"ZZZZZ[1]") THEN  &
		  V%=CVT$%(MID(FNL$,13%,2%)) &
		\ IF V%<>0% THEN  &
			  PRINT "LAST UPDATE WAS NOT COMPLETED. "
80	  ON ERROR GOTO 90 &
	\ OPEN "SS0:UNIQUE.FIL" FOR INPUT AS FILE 4% &
	\ DIM #4%, A$(255%)=64% &
	\ C0$=A$(1%) &
	\ CLOSE 4% &
	\ GOTO 100
90	  PRINT "Unable to read company name from unique file" &
	\ C0$="" &
	\ RESUME 100
100	  !
300	  !
310	  IF FNG%(2%,"ZZZZZ[1]") THEN  &
		  PRINT "I need to ask a couple of questions before we get started." &
		\ INPUT "The current month # is (XX) ";Y1% &
		\ INPUT "The current year is (XX) ";Y% &
		\ INPUT "The month # for the start of the fiscal year (XX) ";C% &
		\ IF FNA%(2%,"ZZZZZ[1]"+CVT%$(Y1%)+STRING$(6%,0%)+CVT%$(C%)+STRING$(2%,0%)+CVT%$(Y%)) THEN  &
			  PRINT "ERROR";FNS%;"IN ADDING ZZZ RECORD.  ABORT." &
			\ GOTO 10000
315	  S1$="" &
	\ GOTO 1000 IF FNG%(2%,"ZZZZZ[1]") &
	\ Y1%=CVT$%(MID(FNL$,9%,2%)) &
	\ Y%=CVT$%(MID(FNL$,21%,2%)) &
	\ FOR I%=1% TO 15% &
		\ C$=NUM1$(Y1%)+"."+NUM1$(Y%) &
		\ C$="0"+C$ IF INSTR(1%,C$,".")=2% &
		\ Y1%=Y1%-1% &
		\ IF Y1%=0% THEN  &
			  Y1%=12% &
			\ Y%=Y%-1%
320		  S1$=C$+S1$ &
	\ NEXT I% &
	\ RETURN
1000	  !
1020	  PRINT  &
	\ INPUT "OPTION ";S$ &
	\ GOTO 1030 IF S$="" &
	\ S$=LEFT(S$,3%) &
	\ GOTO 10030 IF S$="TOT" OR S$="RES" OR S$="BYP" OR S$="ZZZ" &
	\ GOSUB 30 UNLESS F1% &
	\ F1%=-1% &
	\ GOTO 3000 IF S$="ENT" &
	\ GOTO 4000 IF S$="CHA" &
	\ GOTO 5000 IF S$="DEL" &
	\ GOTO 7000 IF S$="PRI" &
	\ GOTO 6000 IF S$="SEA" &
	\ GOTO 8000 IF S$="INP" &
	\ GOTO 10000 IF S$="END" &
	\ PRINT  &
	\ PRINT "TYPE <RETURN> FOR AN OPTIONS LIST." &
	\ GOTO 1020
1030	  PRINT  &
	\ PRINT "OPTIONS : " &
	\ PRINT "    ENTER accounts" &
	\ PRINT "    CHANGE accounts" &
	\ PRINT "    DELETE an account" &
	\ PRINT "    PRINT accounts" &
	\ PRINT "    TOTAL by account type; print totals" &
	\ PRINT "    SEARCH for a suffix" &
	\ PRINT "    INPUT budgets and balances" &
	\ PRINT "    RESET an update (i.e., reopen the books for a month)" &
	\ PRINT "    BYPASS an update" &
	\ PRINT "    ZZZ - special option" &
	\ PRINT  &
	\ PRINT "    END" &
	\ GOTO 1020
2000	  !
2010	  LSET T0$=FNL$ &
	\ T$(L%)=F$(L%)+"" FOR L%=1% TO 3% &
	\ T%(L%)=CVT$%(F$(L%+3%)) FOR L%=1% TO 2% &
	\ T(L%)=CVT$F(F$(L%+5%)) FOR L%=1% TO 22% &
	\ T%(L%+2%)=ASCII(F$(L%+27%)) FOR L%=1% TO 2% &
	\ RETURN
2100	  !
2110	  RSET F$(1%)=T$(1%) &
	\ LSET F$(L%)=T$(L%) FOR L%=2% TO 3% &
	\ LSET F$(L%+3%)=CVT%$(T%(L%)) FOR L%=1% TO 2% &
	\ LSET F$(L%+5%)=CVTF$(T(L%)) FOR L%=1% TO 22% &
	\ LSET F$(L%+27%)=CHR$(T%(L%+2%)) FOR L%=1% TO 2% &
	\ RETURN
2200	  !
2210	  ON ERROR GOTO 2220 &
	\ KILL "SS0:GL.DAS"
2220	  RESUME 2230
2230	  ON ERROR GOTO 0 &
	\ RETURN
3000	  !
3010	  F%=0% &
	\ T2$(I%)="" FOR I%=1% TO 14% &
	\ IF FNG%(2%,"ZZZZZ[1]") THEN  &
		  PRINT "PLEASE RUN THE ZZZ OPTION FIRST." &
		\ GOTO 1020
3020	  L1%=CVT$%(MID(FNL$,9%,2%)) &
	\ Y%,Y1%=CVT$%(MID(FNL$,21%,2%)) &
	\ PRINT "TYPE A DASH (-) AT ANY QUESTION TO VOID ENTRY OF THE G/L #." &
	\ PRINT "FOR EACH OF THE FOLLOWING, ";'TYPE "Y" IF YOU WISH TO MAKE AN ENTRY:' &
	\ INPUT "OVERHEAD (Y/N)";T2$(1%) &
	\ INPUT "BUDGETS (Y/N)";T2$(2%) &
	\ INPUT "CURRENT BALANCE (Y/N)";T2$(4%) &
	\ INPUT "HISTORY (Y/N)";T2$(5%)
3030	  C%=L1% &
	\ PRINT  &
	\ A$=FNI$("NEW ACCOUNT #? ",A$) &
	\ GOTO 1020 IF A$="" AND F%=0% &
	\ GOTO 10020 IF A$="" AND F%<>0% &
	\ GOTO 3300 IF A$="-" &
	\ A$=SPACE$(8%-LEN(A$))+A$ &
	\ IF FNG%(2%,A$)=0% THEN  &
		  PRINT A$;" IS IN USE." &
		\ GOTO 3030
3040	  T$(1%)=A$ &
	\ T(I%)=0. FOR I%=1% TO 22% &
	\ T$(2%)=FNI$("DESCRIPTION? ",K$) &
	\ GOTO 3300 IF T$(2%)="-" &
	\ IF LEN(T$(2%))>40% THEN  &
		  PRINT "MAXIMUM LENGTH IS 40." &
		\ GOTO 3040
3060	  T$(3%)=FNI$("ACCOUNT TYPE (2 CHARS.)? ",K$) &
	\ GOTO 3300 IF T$(3%)="-" &
	\ IF LEN(T$(3%))<>2% OR INSTR(1%,"A L O R E",LEFT(T$(3%),1%))=0% THEN  &
		  PRINT "TYPES MUST START WITH A,L,O,R OR E." &
		\ GOTO 3060
3080	  T%(2%)=0% &
	\ IF T2$(1%)="Y" THEN  &
		  INPUT "OVERHEAD ALLOCATION IN PERCENT";K$ &
		\ GOTO 3300 IF K$="-" &
		\ T%(2%)=INT(VAL(K$)*100.+6.8213185310482798e-15)
3090	  T(1%)=0. &
	\ IF T2$(2%)="Y" THEN  &
		  INPUT "MONTHLY BUDGET";K$ &
		\ GOTO 3300 IF K$="-" &
		\ T(1%)=VAL(K$)
3100	  T(2%)=0. &
	\ IF T2$(2%)="Y" THEN  &
		  INPUT "YEARLY BUDGET";K$ &
		\ GOTO 3300 IF K$="-" &
		\ T(2%)=VAL(K$)
3110	  T%(1%)=C% &
	\ T(I%)=0. FOR I%=3% TO 17% &
	\ IF T2$(4%)="Y" THEN  &
		  INPUT "CURRENT MONTH AMOUNT ";K$ &
		\ GOTO 3300 IF K$="-" &
		\ T(4%)=VAL(K$)
3120	  GOTO 3180 IF T2$(5%)<>"Y" &
	\ C1%=1%
3130	  C1%=C1%+1% &
	\ C%=C%-1% &
	\ IF C%=0% THEN  &
		  C%=12% &
		\ Y1%=Y1%-1%
3140	  C$=FNI$(CVT$$(FNM1$(C%),128%)+", 19"+NUM1$(Y1%)+" - $",C$) &
	\ GOTO 3300 IF C$="-" &
	\ GOTO 3180 IF C$="." &
	\ T1=VAL(C$) &
	\ IF C1%=2% THEN  &
		  T(3%)=T1 &
		\ GOTO 3130
3150	  IF C1%=15% THEN  &
		  T(5%)=T1 &
		\ GOTO 3180
3160	  T(C%+5%)=T1 &
	\ GOTO 3130
3180	  IF T2$(5%)="Y" THEN  &
		  PRINT  &
		\ FOR I%=Y%-1% TO Y%-5% STEP -1% &
			\ C$=FNI$("19"+NUM1$(I%)+" TOTAL - $",C$) &
			\ GOTO 3300 IF C$="-" &
			\ IF C$<>"." THEN  &
				  T(17%+Y%-I%)=VAL(C$) &
			\ NEXT I%
3200	  GOSUB 2100 &
	\ UNLESS FNA%(2%,T0$) THEN  &
		  PRINT "ACCOUNT ADDED." &
		\ GOSUB 2200 UNLESS F% &
		\ F%=-1% &
		\ GOTO 3030
3210	  PRINT "ERROR";FNS%;"IN ADDING RECORD.  ABORT." &
	\ GOTO 10000
3300	  PRINT "ENTRY OF THIS ACCOUNT IS ABORTED." &
	\ GOTO 3030
4000	  !
4020	  F%=0% &
	\ S$=S1$
4030	  F2%=0% &
	\ PRINT  &
	\ A$=FNI$("ACCOUNT # ",A$) &
	\ GOTO 1020 IF A$="" AND F%=0% &
	\ GOTO 10020 IF A$="" AND F%<>0% &
	\ A$=SPACE$(8%-LEN(A$))+A$ &
	\ IF FNG%(2%,A$) THEN  &
		  PRINT A$;" NOT FOUND." &
		\ GOTO 4030
4040	  GOSUB 2000 &
	\ GOSUB 7200
4050	  A$=LEFT(FNI$("ITEM TO CHANGE (TYPE ? FOR HELP)? ",A$),2%) &
	\ GOTO 4030 IF A$="" AND F2%=0% &
	\ GOTO 4055 IF A$<>"" AND A$<>"NU" OR F2%=0% &
	\ GOSUB 2100 &
	\ IF FNU%(2%,T0$) THEN  &
		  PRINT "ERROR";FNS%;"IN CHANGING RECORD.  ABORT." &
		\ GOTO 10000
4052	  PRINT "CHANGED." &
	\ F2%=0% &
	\ GOTO 4030 UNLESS A$="NU"
4055	  ON (INSTR(1%,"?  NU DE TY OV MB AB HI FL",A$)+2%)/3%+1% GOTO 4050,4070,4080,4090,4110,4130,4140,4150,4160,4200
4070	  PRINT "CODES ARE:" &
	\ PRINT "NU - ACCOUNT #" &
	\ PRINT "DE - DESCRIPTION" &
	\ PRINT "TY - TYPE" &
	\ PRINT "OV - OVERHEAD ALLOCATION" &
	\ PRINT "MB - MONTH BUDGET" &
	\ PRINT "AB - ANNUAL BUDGET" &
	\ PRINT "HI - HISTORY" &
	\ PRINT "FL - GENERAL LEDGER PRINT CHARACTERISTICS" &
	\ PRINT "?  - HELP MESSAGE" &
	\ GOTO 4050
4080	  C$=T$(1%) &
	\ INPUT "NEW ACCOUNT #";A$ &
	\ GOTO 4050 IF A$="" &
	\ A$=SPACE$(8%-LEN(A$))+A$ &
	\ IF FNG%(2%,A$)=0% THEN  &
		  PRINT A$;" IS IN USE." &
		\ GOTO 4080
4082	  IF FNG%(2%,C$) THEN  &
		  PRINT "ERROR";FNS%;"IN RECOVERING RECORD.  ABORT." &
		\ GOTO 10000
4083	  LSET T0$=FNL$ &
	\ LSET T0$=A$+RIGHT(T0$,9%) &
	\ IF FNU%(-2%,T0$) THEN  &
		  PRINT "ERROR";FNS%;"IN CHANGING RECORD.  ABORT." &
		\ GOTO 10000
4085	  GOSUB 2200 UNLESS F% &
	\ F%=-1% &
	\ PRINT "CHANGED." &
	\ GOTO 4030
4090	  PRINT "-> ";CVT$$(T$(2%),128%) &
	\ INPUT "NEW DESCRIPTION";T$ &
	\ GOTO 4050 IF T$="" &
	\ T$(2%)=LEFT(T$,40%) &
	\ F2%=-1% &
	\ GOTO 4050
4110	  PRINT "-> ";T$(3%) &
	\ INPUT "NEW TYPE";T$ &
	\ GOTO 4050 IF T$="" &
	\ IF LEN(T$)<>2% OR INSTR(1%,"A L O R E",LEFT(T$,1%))=0% THEN  &
		  PRINT "OPTIONS:  2 CHARACTERS, THE FIRST OF WHICH MUST BE A, L, O, R OR E." &
		\ GOTO 4110
4120	  T$(3%)=T$ &
	\ F2%=-1% &
	\ GOTO 4050
4130	  PRINT USING "-> ###.##%", T%(2%)/100. &
	\ INPUT "NEW PERCENTAGE ";T$ &
	\ GOTO 4050 IF T$="" &
	\ T%(2%)=INT(VAL(T$)*100.+6.8213185310482798e-15) &
	\ F2%=-1% &
	\ GOTO 4050
4140	  PRINT USING "-> #,###,###.##", T(1%) &
	\ INPUT "NEW MONTH BUDGET ";T$ &
	\ GOTO 4050 IF T$="" &
	\ T(1%)=VAL(T$) &
	\ F2%=-1% &
	\ GOTO 4050
4150	  PRINT USING "-> #,###,###.##", T(2%) &
	\ INPUT "NEW ANNUAL BUDGET ";T$ &
	\ GOTO 4050 IF T$="" &
	\ T(2%)=VAL(T$) &
	\ F2%=-1% &
	\ GOTO 4050
4160	  INPUT "MONTH TO CHANGE (MM.YY) ";T$ &
	\ GOTO 4050 IF T$="" &
	\ T$="0"+T$ IF INSTR(1%,T$,".")=2% &
	\ GOTO 4160 IF LEN(T$)<>5% &
	\ L1%=INSTR(1%,S$,T$) &
	\ IF L1%=0% OR L1%/5%*5%<>L1%-1% THEN  &
		  PRINT "ILLEGAL MONTH !" &
		\ GOTO 4160
4170	  L1%=(L1%+4%)/5% &
	\ IF L1%=1% THEN &
		  L1%=5% &
	  ELSE &
		  IF L1%=14% THEN &
			  L1%=3% &
		  ELSE &
			  IF L1%=15% THEN &
				  L1%=4% &
			  ELSE &
				  L1%=VAL(LEFT(T$,2%))+5%
4180	  PRINT USING "--> #,###,###.##", T(L1%) &
	\ INPUT "NEW AMOUNT";T$ &
	\ GOTO 4050 IF T$="" &
	\ T(L1%)=VAL(T$) &
	\ F2%=-1% &
	\ GOTO 4050
4200	  !
4210	  T%(4%)=0% IF T%(3%)<>255% &
	\ PRINT USING "-> ####", T%(4%) &
	\ INPUT "NEW PRINT FLAG ";T$ &
	\ GOTO 4050 IF T$="" &
	\ IF T$="1" OR T$="2" OR T$="3" THEN  &
		  T%(4%)=VAL(T$) &
		\ T%(3%)=255% &
		\ F2%=-1% &
		\ GOTO 4050
4220	  PRINT "Your choices are as follows:" &
	\ PRINT "     1.  Print all the detail transactions in the G/L" &
	\ PRINT "     2.  Print a summary of all the detail for this account" &
	\ PRINT "     3.  Print a summary by date for this account" &
	\ PRINT "Please try again." &
	\ PRINT  &
	\ GOTO 4200
5000	  !
5010	  PRINT  &
	\ A$=FNI$("DELETE ACCOUNT # ",A$) &
	\ GOTO 1020 IF A$="" &
	\ A$=SPACE$(8%-LEN(A$))+A$ &
	\ IF FNG%(2%,A$) THEN  &
		  PRINT A$;" NOT FOUND." &
		\ GOTO 5010
5020	  GOSUB 2000 &
	\ PRINT  &
	\ PRINT T$(1%);" ";T$(3%);" ";T$(2%) &
	\ IF LEFT(FNI$("CONFIRM DELETING (Y/N).  ",K$),1%)<>"Y" THEN  &
		  PRINT "NOT DELETED." &
		\ GOTO 5010
5030	  IF FND%(2%,A$) THEN  &
		  PRINT "ERROR";FNS%;"IN DELETING RECORD.  ABORT." &
		\ GOTO 10000
5040	  PRINT A$;" IS NOW DELETED." &
	\ GOSUB 2200 &
	\ GOTO 5010
6000	  !
6010	  C%=1% &
	\ C%=2% IF LEFT(FNI$("HISTORY (Y/N)? ",K$),1%)="Y" &
	\ IF C%=2% THEN  &
		  GOTO 1020 IF FNG%(2%,"ZZZZZ[1]") &
		\ M1%,V%=CVT$%(MID(FNL$,9%,2%)) &
		\ Y1%=CVT$%(MID(FNL$,21%,2%)) &
		\ T1(I%)=0. FOR I%=1% TO 22%
6020	  X%=66% &
	\ PRINT  &
	\ A$=FNI$("SUFFIX RANGE (FROM-TO)? ",A$) &
	\ L%=INSTR(1%,A$,"-") &
	\ GOTO 1020 IF A$="" &
	\ IF L%=0% THEN &
		  S$=A$ &
	  ELSE &
		  S$=RIGHT(A$,L%+1%) &
		\ A$=LEFT(A$,L%-1%)
6030	  L4%=9%-LEN(A$) &
	\ GOTO 6020 IF LEN(A$)<>LEN(S$) OR A$>S$ &
	\ T1,T2=0. &
	\ C$=FNI$("ACCOUNTS (FROM-TO) <CR> FOR ALL? ",C$) &
	\ C$=".00-99999.99" IF C$="" &
	\ L%=INSTR(1%,C$,"-") &
	\ IF L%=0% THEN &
		  T$=C$ &
	  ELSE &
		  T$=RIGHT(C$,L%+1%) &
		\ C$=LEFT(C$,L%-1%)
6040	  C$=SPACE$(8%-LEN(C$))+C$ &
	\ T$=SPACE$(8%-LEN(T$))+T$ &
	\ IF T$>=C$ THEN &
		  V%=FNG%(2%,C$) &
	  ELSE &
		  PRINT "SMALLER NUMBER FIRST!" &
		\ GOTO 6020
6050	  L1%=4% &
	\ L3%=2% &
	\ ON C% GOSUB 7300,7400
6060	  L2%=3% &
	\ L3%=2% &
	\ GOSUB 2010 &
	\ GOTO 6070 IF T$(1%)>T$ OR T$(1%)="ZZZZZ[1]" &
	\ ON C% GOSUB 7200,7500 IF RIGHT(T$(1%),L4%)>=A$ AND RIGHT(T$(1%),L4%)<=S$ &
	\ GOTO 6060 UNLESS FNN%(2%)
6070	  GOTO 7120 IF C%=2% &
	\ PRINT  &
	\ PRINT USING TAB(49%)+"###,###,###.## ###,###,###.##", FNZ(T1),FNZ(T2) &
	\ PRINT  &
	\ GOTO 6020
7000	  !
7010	  C%=1% &
	\ C%=2% IF LEFT(FNI$("ENTIRE HISTORY (Y/N)? ",K$),1%)="Y" &
	\ GOTO 1020 IF FNG%(2%,"ZZZZZ[1]") &
	\ M1%,V%=CVT$%(MID(FNL$,9%,2%)) &
	\ Y1%=CVT$%(MID(FNL$,21%,2%)) &
	\ T1(I%)=0. FOR I%=1% TO 22%
7020	  IF C%=1% THEN  &
		  INPUT "MONTH TO PRINT (MM.YY OR <CR> FOR CURRENT) ";T$ &
		\ K$=NUM1$(M1%)+"."+NUM1$(Y1%) IF T$="" &
		\ PRINT "CURRENT MONTH IS ";K$ IF T$="" &
		\ T$=K$ IF T$="" &
		\ T$="0"+T$ IF INSTR(1%,T$,".")=2% &
		\ GOTO 7020 IF LEN(T$)<>5% &
		\ L1%=INSTR(1%,S1$,T$) &
		\ IF L1%=0% OR L1%/5%*5%<>L1%-1% THEN  &
			  PRINT "ILLEGAL MONTH NUMBER!" &
			\ GOTO 7020
7030	  IF C%=1% THEN  &
		  L1%=(L1%+4%)/5% &
		\ IF L1%=1% THEN &
			  L1%=5% &
		  ELSE &
			  IF L1%=14% THEN &
				  L1%=3% &
			  ELSE &
				  IF L1%=15% THEN &
					  L1%=4% &
				  ELSE &
					  L1%=VAL(LEFT(T$,2%))+5%
7040	  GOTO 1020 IF FNG%(2%,"ZZZZZ[1]") &
	\ M1%=CVT$%(MID(FNL$,9%,2%)) &
	\ IF C%=1% THEN  &
		  INPUT "MONTH, YEAR-TO-DATE OR NO BALANCES (M/Y/N)";K$ &
		\ L3%=0% &
		\ L3%=1% IF LEFT(K$,1%)="M" &
		\ L3%=2% IF LEFT(K$,1%)="Y"
7050	  IF L1%=4% THEN &
		  L2%=3% &
	  ELSE &
		  IF L1%=3% THEN &
			  L2%=M1%+3% &
		  ELSE &
			  L2%=L1%-1% &
			\ L2%=17% IF L2%=5%
7060	  X%=66% &
	\ T$="" &
	\ F%,T1,T2=0. &
	\ PRINT  &
	\ C$=FNI$("FROM ACCOUNT # (or ALL) ? ",C$) &
	\ GOTO 1020 IF C$="" &
	\ T$=FNI$("TO ACCOUNT # ? ",T$) UNLESS C$="ALL" &
	\ F%=-1% IF T$="" &
	\ F%=F%*2% IF C$="ALL" &
	\ C$="" IF C$="ALL" &
	\ C$=SPACE$(8%-LEN(C$))+C$ IF C$<>"" &
	\ T$=SPACE$(8%-LEN(T$))+T$ &
	\ IF T$<C$ AND F%=0% THEN  &
		  GOTO 7060
7070	  PRINT  &
	\ IF F%<>0% AND FNG%(2%,C$) THEN  &
		  PRINT C$;" NOT FOUND." &
		\ GOTO 7060
7080	  K$=FNI$("SET PAGE.",K$) &
	\ ON C% GOSUB 7300,7400
7090	  GOSUB 2000 &
	\ GOTO 7110 IF T$(1%)>T$ AND F%=0% OR T$(1%)="ZZZZZ[1]" &
	\ IF F%=-1% THEN  &
		  ON C% GOSUB 7200,7500 &
		\ PRINT  &
		\ GOTO 7060
7100	  ON C% GOSUB 7200,7500 &
	\ UNLESS FNN%(2%) THEN  &
		  GOTO 7090
7110	  IF C%<>2% THEN  &
		  PRINT  &
		\ PRINT USING SPACE$(49%)+"###,###,###.##", FNZ(T1); &
		\ PRINT USING " ###,###,###.##", FNZ(T2); IF L1%=4% &
		\ PRINT  &
		\ PRINT  &
		\ GOTO 7060
7120	  T$(1%)="TOTAL" &
	\ T$(2%),T$(3%)="" &
	\ T(I%)=T1(I%) FOR I%=1% TO 22% &
	\ T%(1%),T%(2%)=0% &
	\ GOSUB 7500 &
	\ PRINT  &
	\ GOTO 1020
7200	  !
7210	  I%=0% &
	\ I%=T%(1%) IF L1%=4% &
	\ I%=T%(1%)-1% IF L1%=3% &
	\ I%=L1%-5% IF L1%>4% &
	\ I%=12% IF I%=0% &
	\ PRINT USING "\      \  \\  \"+SPACE$(28%)+"\  ", T$(1%),T$(3%),T$(2%); &
	\ IF L3% THEN  &
		  PRINT USING "## ###,###,###.##", I%,FNZ(T(L1%)); IF L3%=2% &
		\ PRINT USING "## ###,###,###.##", I%,FNZ(T(L1%)-T(L2%)); IF L3%=1% &
		\ PRINT USING " ###,###,###.##", FNZ(T(L3%)); IF L1%=4%
7220	  PRINT  &
	\ X%=X%+1% &
	\ GOSUB 7300 IF X%>55% AND T$(1%)<>"TOTAL" &
	\ T1=T1+T(L1%) IF L3%=2% &
	\ T1=T1+T(L1%)-T(L2%) IF L3%=1% &
	\ T2=T2+T(L3%) &
	\ RETURN
7300	  !
7310	  PRINT STRING$(68%-X%-2%,10%); &
	\ PRINT C0$ &
	\ PRINT  &
	\ PRINT "   G/L # TYPE DESCRIPTION"; &
	\ PRINT TAB(43%);"MONTH         AMOUNT"; IF L3% &
	\ PRINT "         BUDGET"; IF L1%=4% AND L3% &
	\ PRINT  &
	\ PRINT  IF C%=1% &
	\ X%=4% &
	\ RETURN
7400	  !
7410	  M%=M1% &
	\ Y%=Y1% &
	\ PRINT STRING$(65%-X%,10%); &
	\ PRINT C0$ &
	\ PRINT  &
	\ FOR L%=1% TO 3% &
		\ PRINT " "; &
		\ FOR L1%=1% TO 5% &
			\ IF L%=1% AND L1%=1% THEN  &
				  PRINT "      ";FNM1$(M%);",";Y%; &
				\ GOTO 7430
7420			  Y%=Y1% &
			\ M%=M1%-L1%*3%+4%-L% &
			\ UNTIL M%>0% &
				\ M%=12%+M% &
				\ Y%=Y%-1% &
			\ NEXT &
			\ PRINT "      ";FNM1$(M%);",";Y%;
7430				  NEXT L1% &
		\ PRINT  &
	\ NEXT L% &
	\ PRINT  &
	\ PRINT  &
	\ X%=6% &
	\ RETURN
7500	  !
7510	  PRINT USING "---\      \  \\  \"+SPACE$(38%)+"\", T$(1%),T$(3%),T$(2%) &
	\ FOR L%=1% TO 3% &
		\ FOR L1%=1% TO 5% &
			\ M%=M1%-L1%*3%+4%-L% &
			\ M%=12%+M% UNTIL M%>0% &
			\ V$="###,###,###.##" &
			\ IF L%=1% AND L1%=1% THEN &
				  PRINT USING V$, FNZ(T(4%)); &
			  ELSE &
				  IF L%=2% AND L1%=1% THEN &
					  PRINT USING V$, FNZ(T(3%)); &
				  ELSE &
					  IF L%=3% AND L1%=5% THEN &
						  PRINT USING V$, FNZ(T(5%)); &
					  ELSE &
						  PRINT USING V$, FNZ(T(M%+5%));
7520				  NEXT L1% &
		\ PRINT  &
	\ NEXT L% &
	\ T1(I%)=T1(I%)+T(I%) FOR I%=1% TO 22% &
	\ PRINT  &
	\ X%=X%+5% &
	\ GOSUB 7400 IF X%>55% AND T$(1%)<>"TOTAL" &
	\ RETURN
8000	  !
8010	  Y1%=0% &
	\ PRINT  &
	\ PRINT "TO ENTER THE FIGURE LISTED, TYPE THE LETTER INDICATED BELOW." &
	\ PRINT "     M       MONTH BUDGET" &
	\ PRINT "     Y       YEAR-TO-DATE BUDGET" &
	\ PRINT "     B       BOTH BUDGETS" &
	\ PRINT "     MM.YY   BALANCE FOR MONTH MM.YY" &
	\ PRINT  &
	\ INPUT "ENTER WHICH FIGURES";S$ &
	\ GOTO 1020 IF S$="" &
	\ L1%=0% &
	\ L1%=L1% OR 1% IF S$="M" OR S$="B" &
	\ L1%=L1% OR 2% IF S$="Y" OR S$="B" &
	\ C$=FNI$("FROM ACCOUNT # ",C$) &
	\ GOTO 8010 IF C$="" &
	\ A$=FNI$("TO ACCOUNT # ",A$) &
	\ GOTO 8010 IF A$="" &
	\ C$=SPACE$(8%-LEN(C$))+C$ &
	\ A$=SPACE$(8%-LEN(A$))+A$ &
	\ GOTO 8010 IF A$<C$ &
	\ V%=FNG%(2%,C$) &
	\ V$=SYS(CHR$(3%)) &
	\ IF INSTR(1%,S$,".")=0% THEN  &
		  PRINT TAB(10%);"       CURRENT"; &
		\ PRINT "     MONTH"; IF L1% AND 1% &
		\ PRINT "    ANNUAL"; IF L1% AND 2% &
		\ PRINT  &
		\ GOTO 8020
8015	  GOTO 1020 IF FNG%(2%,"ZZZZZ[1]") &
	\ M1%,V%=CVT$%(MID(FNL$,9%,2%)) &
	\ Y1%=CVT$%(MID(FNL$,21%,2%)) &
	\ V%=FNG%(2%,C$) &
	\ L1%=INSTR(1%,S1$,S$) &
	\ L1%=(L1%+4%)/5% &
	\ IF L1%=1% THEN &
		  L1%=5% &
	  ELSE &
		  IF L1%=14% THEN &
			  L1%=3% &
		  ELSE &
			  IF L1%=15% THEN &
				  L1%=4% &
			  ELSE &
				  L1%=VAL(LEFT(S$,2%))+5%
8017	  C%=-1% &
	\ PRINT TAB(15%);"AMOUNT"
8020	  GOSUB 2000 &
	\ GOTO 8050 IF T$(1%)>A$ OR T$(1%)="ZZZZZ[1]" &
	\ PRINT USING "\      \  ###,###,###.##-", T$(1%),T(L1%); &
	\ IF C%=-1% THEN  &
		  T$=FNI$("",T$) &
		\ T(L1%)=VAL(T$)/100. &
		\ PRINT USING "###,###,###.##", T(L1%); &
		\ GOTO 8040
8025	  IF L1% AND 1% THEN  &
		  T$=FNI$("",T$) &
		\ T(1%)=VAL(T$)/100. UNLESS T$="" &
		\ PRINT USING "###,###.##", T(1%);
8030	  IF L1% AND 2% THEN  &
		  T$=FNI$("",T$) &
		\ T(2%)=VAL(T$)/100. UNLESS T$="" &
		\ PRINT USING "###,###.##", T(2%);
8040	  PRINT  &
	\ GOSUB 2100 &
	\ IF FNU%(2%,T0$) THEN  &
		  PRINT "ERROR";FNS%;"IN CHANGING RECORD.  ABORT." &
		\ GOTO 10000
8045	  GOTO 8020 UNLESS FNN%(2%)
8050	  PRINT  &
	\ S$=SYS(CHR$(2%)) &
	\ GOTO 8010
10000	  !
10005	  K$,V$="" &
	\ I%=0%
10010	  CLOSE 1% &
	\ CLOSE 12% &
	\ V%=FNX%(V$,I%,K$)
10020	  V$="[1,3]CHART1.BAC" &
	\ I%=10% &
	\ K$="" &
	\ GOTO 10010
10030	  V$="[1,3]CHART2.BAC" &
	\ I%=10% &
	\ K$=S$ &
	\ GOTO 10010
14010	  DEF FNM1$(M%) &
	\ FNM1$=MID("JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC",M%*3%-2%,3%) &
	\ FNEND
14020	  DEF FNI$(V$,K$) &
	\ PRINT V$; &
	\ INPUT LINE #12%, K$ &
	\ FNI$=CVT$$(K$,140%) &
	\ FNEND
14030	  DEF FNZ(T1) &
	\ FNZ=INT(T1*100.+6.8213185310482798e-15)/100. &
	\ FNEND
32767	  END
