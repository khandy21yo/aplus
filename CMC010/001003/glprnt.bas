10	  ! &
	  ! Program name: glprnt		Compiled with SCALE 0 on V08.0 &
	  ! Decompiled on 24-Nov-16 at 02:11 AM
35	  !
36	  Y$=SYS(CHR$(12%)) &
	\ CHANGE Y$ TO Y% &
	\ PRGNUM$="["+NUM1$(Y%(6%))+","+NUM1$(Y%(5%))+"]" &
	\ PRGNAM$=RAD$(Y%(7%)+SWAP%(Y%(8%)))+RAD$(Y%(9%)+SWAP%(Y%(10%)))+"."+RAD$(Y%(11%)+SWAP%(Y%(12%))) &
	\ DEVNAM$=CHR$(Y%(23%))+CHR$(Y%(24%))+NUM1$(Y%(25%))+":"
50	  DIM F$(29%), N$(4%), T$(3%), T%(4%), T(22%)
60	  OPEN "NL:" AS FILE 1%, RECORDSIZE 64%+256%+16% &
	\ FIELD #1%, 6% AS R$,8% AS R1$,2% AS R2$,8% AS R3$,2% AS R4$,2% AS R5$,2% AS R6$,28% AS R7$,6% AS R8$ &
	\ FIELD #1%, 64% AS T$ &
	\ FIELD #1%, 64% AS K$,8% AS F$(1%),40% AS F$(2%),2% AS F$(3%),2% AS F$(4%),2% AS F$(5%) &
	\ FIELD #1%, 64% AS K$,54%+(L%*8%-8%) AS K$,8% AS F$(L%+5%) FOR L%=1% TO 22% &
	\ FIELD #1%, 64%+230%+(L%*1%-1%) AS D$,1% AS F$(L%+27%) FOR L%=1% TO 2% &
	\ FIELD #1%, 64% AS K$,256% AS T0$ &
	\ FIELD #1%, 64%+256% AS K$,8% AS N$(1%),4% AS K$,2% AS N$(2%) &
	\ FIELD #1%, 64%+256% AS K$,16% AS T2$
100	  !
102	  IF FNO%(4%,"CHART.DAT","/RW","") THEN &
		  PRINT "You must have a chart of accounts first !";FNX%("",0%,"") &
	  ELSE &
		  IF FNG%(4%,"ZZZZZ[1]") THEN &
			  PRINT "Better run the ZZZ option in CHART first.";FNX%("",0%,"") &
		  ELSE &
			  LSET T0$=FNL$ &
			\ PCNTRL$="Y" &
			\ FIND%=CVT$%(MID(T0$,11%,2%)) &
			\ YEAR%=CVT$%(MID(T0$,21%,2%)) &
			\ YEAR$=RIGHT(NUM1$(100%+YEAR%),2%) &
			\ LAST%=CVT$%(MID(T0$,9%,2%)) &
			\ IF YEAR%>99% THEN  &
				  INPUT "Year of last month closed ";Y$ &
				\ Y$=RIGHT(Y$,3%) IF LEN(Y$)=4% &
				\ YEAR%=VAL(Y$) &
				\ LSET T0$=FNL$ &
				\ LSET T0$=LEFT(T0$,20%)+CVT%$(YEAR%)+RIGHT(T0$,23%) &
				\ IF FNU%(4%,T0$) THEN  &
					  PRINT "Unable to change flag record in the chart file." &
					\ PRINT "Aborting this routine." &
					\ GOTO 10000
108	  IF FNO%(6%,"TT0:GL.DAS","/SF","")=0% AND FNG%(6%,"")=0% THEN  &
		  FILE$=MID(FNL$,2%,10%) &
		\ SIZE%=CVT$%(MID(FNL$,12%,2%)) &
		\ OPEN FILE$ FOR INPUT AS FILE 2% &
		\ GET #2% &
		\ FIELD #2%, 2% AS N$ &
		\ N%=CVT$%(N$) &
		\ CLOSE 2% &
		\ IF N%=SIZE% THEN  &
			  PRINT "A ledger file has been built for";FILE$;".  "; &
			\ PRINT "You may continue." IF FIND% &
			\ PRINT CHR$(13%)+CHR$(10%)+"Until all accounts have been validated, you may use"+CHR$(13%)+CHR$(10%)+"the EXAMINE option only."+CHR$(13%)+CHR$(10%) IF FIND%=0% &
			\ GOTO 300
120	  !
122	  PRINT "An error has occurred in this routine.  I will attempt to "+"recover." &
	\ ON ERROR GOTO 124 &
	\ KILL "TT0:GL.DAS"
124	  CHAIN PRGNUM$+"GL.BAC" 10%
300	  !
340	  IF FNO%(2%,FILE$,"/SF/NS","") THEN  &
		  PRINT "Error";FNS%;" has occurred while opening ";FILE$;"." &
		\ PRINT "Aborting this routine." &
		\ GOTO 10000
350	  IF INSTR(1%," 1234567890",MID(FILE$,5%,1%))=0% THEN  &
		  I%=INSTR(1%,"JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC",MID(FILE$,4%,3%)) &
		\ NEX%=(I%-1%)/3%+1%
370	  V%=FNG%(4%,"ZZZZZ[1]") &
	\ IF CVT$%(MID(FNL$,17%,2%))=NEX% THEN &
		  FIRST%=-1% &
	  ELSE &
		  FIRST%=0%
380	  GOSUB 600 IF FIRST% &
	\ YTDERN.TEMP=YTDERN &
	\ H1$="\"+SPACE$(38%)+"\  \"+SPACE$(34%)+"\     AS OF ##/##/##"+SPACE$(13%)+"PAGE ###" &
	\ H4$="\      \  ##/##/##  \\  \     \ \ "+SPACE$(21%)+"\ \      \    "
400	  !
410	  ON ERROR GOTO 420 &
	\ OPEN "SS0:UNIQUE.FIL/RO" FOR INPUT AS FILE 10% &
	\ DIM #10%, A0$(1%)=64% &
	\ S9$=CVT$$(LEFT(A0$(1%),61%),140%) &
	\ CLOSE 10% &
	\ GOTO 490 IF S9$<>""
420	  PRINT "Please enter the company name "; &
	\ INPUT LINE K$ &
	\ S9$=CVT$$(K$,4%) &
	\ IF ERR THEN  &
		  RESUME 490
490	  ON ERROR GOTO 0 &
	\ GOTO 1000
600	  !
610	  RETURN IF FNG%(4%,"") &
	\ Y8%=0%
620	  GOSUB 2400 &
	\ YTDERN=FNZ(YTDERN+T(4%)) IF LEFT(T$(3%),1%)="E" OR LEFT(T$(3%),1%)="R" &
	\ Y8%=-1% IF T$(3%)="OR" &
	\ GOTO 620 UNLESS FNN%(4%) &
	\ IF Y8%<>0% THEN &
		  RETURN &
	  ELSE &
		  PRINT "THE CHART OF ACCOUNTS MUST CONTAIN AN OWNERS'"+" EQUITY ACCOUNT WITH THE" &
		\ PRINT "ACCOUNT TYPE 'OR' (EG., YTD EARNINGS) BEFORE AN UPDATE CAN "+"BE COMPLETED." &
		\ GOTO 10000
1000	  !
1010	  !
1020	  PRINT  &
	\ YTDERN=YTDERN.TEMP &
	\ INPUT "OPTION ";K$ &
	\ K$=CVT$$(LEFT(K$,3%),-1%) &
	\ F$="" &
	\ GOTO 1030 IF K$="" &
	\ GOTO 7000 IF K$="PRI" AND FIND% &
	\ GOTO 5000 IF K$="WOR" AND FIND% &
	\ GOTO 4000 IF K$="TRI" AND FIND% &
	\ GOTO 3000 IF K$="EXA" &
	\ GOTO 9000 IF K$="UPD" AND FIND% &
	\ GOTO 10000 IF K$="END" &
	\ GOTO 1020
1030	  PRINT  &
	\ PRINT "OPTIONS:" &
	\ PRINT '     "PRINT" GENERAL LEDGER' IF FIND% &
	\ PRINT '     "EXAMINE" A RANGE OF ACCOUNTS' &
	\ PRINT '     "WORK SHEET" PRINTED IF FIND% &
	\ PRINT '     "TRIAL" BALANCE SHEET PRINTED' IF FIND% &
	\ PRINT '     "UPDATE" TOTALS (I.E., CLOSE BOOKS FOR THE MONTH)' IF FIND% &
	\ PRINT '     "END" PROGRAM' &
	\ GOTO 1020
2000	  !
2010	  X%=X%+1% &
	\ PRINT  FOR I%=X1% TO X5%+2% &
	\ X1%=6% &
	\ PRINT USING H1$, S9$,"G E N E R A L   L E D G E R",M2%,D2%,Y2%,X%
2020	  PRINT "ACCT #    MO DA YR SRC CHECK #  DESCRIPTION"+SPACE$(10%)+"JOB #            -- CURRENT  -     BUDGET--   "+"--YEAR-TO-DATE   -     BUDGET--" &
	\ PRINT  &
	\ RETURN
2050	  !
2055	  IF F$<>"T" THEN  &
		  X%=X%+1% &
		\ PRINT  FOR I%=X1% TO X5%+2% &
		\ X1%=7% &
		\ PRINT USING H1$, S9$,"W O R K    S H E E T",M2%,D2%,Y2%,X% &
		\ PRINT  &
		\ PRINT "DESCRIPTION OR ACCOUNT NAME               ACCNT #       ";" MONTH      YEAR-TO-DATE       JOURNAL ENTRIES          EXTENSIONS" &
		\ PRINT SPACE$(85%)+"DEBITS     CREDITS     DEBITS     CREDITS" &
		\ RETURN
2070	  X%=X%+1% &
	\ PRINT STRING$(X5%-X1%+3%,10%); &
	\ X1%=5% &
	\ PRINT S9$;TAB(35%); &
	\ PRINT USING "##/##/## TRIAL BALANCE", M2%,D2%,Y2%; &
	\ PRINT TAB(55%);"          PAGE";X% &
	\ PRINT "ACCT #  DESCRIPTION                 BEGINNING     ";"     CURRENT         ENDING" &
	\ RETURN
2120	  RETURN
2200	  !
2210	  GOSUB 2700 &
	\ X1%=X1%+3% &
	\ T1=FNZ(T1+L1) &
	\ T2=FNZ(T2+L1) &
	\ GOSUB 2000 IF X1%>X5%-1% &
	\ PRINT "??";CVT$$(L1$,-1%);TAB(10%);"????? UNDEFINED ACCOUNT ?????" &
	\ PRINT USING H4$, CVT$$(L1$,-1%),M1%,D1%,Y1%,L3$,L$,L2$,L5$; &
	\ V$=FNP$(L1) &
	\ PRINT  &
	\ PRINT  &
	\ RETURN
2300	  !
2310	  GOSUB 2700 &
	\ X1%=X1%+2% &
	\ T1=FNZ(T1+L1) &
	\ T2=FNZ(T2+L1) &
	\ GOSUB 2050 IF X1%>X5%-7% &
	\ PRINT  &
	\ PRINT USING "\"+SPACE$(25%)+"\", L2$; &
	\ PRINT "UNDEFINED # >>>"; &
	\ PRINT USING "\      \    ", L1$; &
	\ V$=FNP$(L1) &
	\ PRINT "  "; &
	\ V$=FNP$(L1) &
	\ PRINT  &
	\ RETURN
2400	  !
2410	  LSET T0$=FNL$ &
	\ T$(L%)=F$(L%)+"" FOR L%=1% TO 3% &
	\ T%(L%)=CVT$%(F$(L%+3%)) FOR L%=1% TO 2% &
	\ T(L%)=CVT$F(F$(L%+5%))*100. FOR L%=1% TO 22% &
	\ T%(L%+2%)=ASCII(F$(L%+27%)) FOR L%=1% TO 2% &
	\ RETURN
2500	  !
2510	  RSET F$(1%)=T$(1%) &
	\ LSET F$(I%)=T$(I%) FOR I%=2% TO 3% &
	\ LSET F$(I%+3%)=CVT%$(T%(I%)) FOR I%=1% TO 2% &
	\ LSET F$(I%+5%)=CVTF$(T(I%)/100.) FOR I%=1% TO 22% &
	\ RETURN
2600	  !
2610	  LSET T2$=FNL$ &
	\ X$=SPACE$(8%-LEN(CVT$$(N$(1%),-1%)))+CVT$$(N$(1%),-1%) &
	\ R%=CVT$%(N$(2%)) &
	\ IF FNG%(-2%,NUM1$(R%)) THEN  &
		  PRINT "Unable to find the data record for ";X$ &
		\ PRINT "Aborting. . ." &
		\ GOTO 10000
2620	  FIELD #2%, FNL% AS K$,64% AS T3$ &
	\ RETURN
2700	  !
2710	  LSET T$=T3$ &
	\ L$=R$+"" &
	\ L1$=CVT$$(R1$,-1%) &
	\ L3$=R2$+"" &
	\ L2$=R7$+"" &
	\ L5$=R8$+"" &
	\ L1=FNZ(CVT$F(R3$)*100.) &
	\ M1%=CVT$%(R4$) &
	\ D1%=CVT$%(R5$) &
	\ Y1%=CVT$%(R6$) &
	\ RETURN
2800	  !
2805	  INPUT "ENTER THE LEDGER DATE (MM.DD.YY)";L8$ &
	\ N%=INSTR(2%,L8$,".") &
	\ IF N% THEN  &
		  N1%=INSTR(4%,L8$,".") &
		\ GOTO 2820
2810	  N%=INSTR(2%,L8$,"/") &
	\ GOTO 2800 UNLESS N% &
	\ N1%=INSTR(4%,L8$,"/")
2820	  M2%=VAL(LEFT(L8$,N%-1%)) &
	\ D2%=VAL(MID(L8$,N%+1%,N1%-N%-1%)) &
	\ Y2%=VAL(RIGHT(L8$,N1%+1%)) &
	\ L1=0. &
	\ INPUT "ENTER PAGE LENGTH IN INCHES (8.5 OR 11)";L1 UNTIL L1=11. OR L1=8.5 UNLESS F$="T" &
	\ L1=11. IF F$="T" &
	\ IF L1>0. THEN &
		  X5%=L1*6. &
	  ELSE &
		  X5%=66%
2830	  X1%=X5%+1% &
	\ INPUT "SET PAGE";X$ &
	\ X$="" &
	\ T1,T2=0. &
	\ RETURN
2900	  !
2910	  IF FIRST%=0% THEN &
		  P=T(4%) &
	  ELSE &
		  IF LEFT(T$(3%),1%)="R" OR LEFT(T$(3%),1%)="E" THEN &
			  P=0. &
		  ELSE &
			  IF T$(3%)<>"OR" THEN &
				  P=T(4%) &
			  ELSE &
				  P=T(4%)+YTDERN &
				\ YTDERN=0.
2920	  RETURN
3000	  !
3010	  X%=0% &
	\ INPUT "FROM ACCT. # <ALL>";K$ &
	\ K$=CVT$$(K$,-1%) &
	\ K$=SPACE$(8%-LEN(K$))+K$ &
	\ K$="" IF K$=SPACE$(8%) &
	\ IF FNG%(4%,K$) THEN  &
		  PRINT "NOT A VALID NUMBER!" &
		\ GOTO 3010
3020	  INPUT "TO ACCT. # <ALL>";K3$ &
	\ K3$=CVT$$(K3$,-1%) &
	\ K3$=SPACE$(8%-LEN(K3$))+K3$ &
	\ K3$="ZZZZZ[1]" IF K3$=SPACE$(8%) &
	\ IF FNG%(4%,K3$) THEN  &
		  PRINT "NOT A VALID ACCT. NUMBER!" &
		\ GOTO 3020
3030	  INPUT "SET PAGE";K1$ &
	\ IF FNG%(4%,K$) THEN  &
		  PRINT K$;" is not a valid account number." &
		\ GOTO 3010
3035	  GOSUB 2020 &
	\ GOSUB 2400
3040	  K1$=T$(1%)+"" &
	\ V%=FNG%(6%,K1$) &
	\ GOTO 3050 IF V% &
	\ PRINT  &
	\ PRINT CVT$$(T$(1%),-1%);TAB(10%);CVT$$(T$(2%),128%) &
	\ PRINT TAB(32%);"BALANCE FORWARD";TAB(77%);"0.00";TAB(101%); &
	\ GOSUB 2910 &
	\ V$=FNP$(P) &
	\ PRINT  &
	\ T2=P &
	\ T5=FNZ(T5+T2)
3045	  GOSUB 2600 &
	\ GOSUB 2700 &
	\ PRINT USING H4$, CVT$$(T$(1%),-1%),M1%,D1%,Y1%,L3$,L$,L2$,L5$; &
	\ T=FNZ(T+L1) &
	\ V$=FNP$(L1) &
	\ PRINT  &
	\ X%=1% &
	\ V%=FNN%(6%) &
	\ UNLESS V% THEN  &
		  IF K1$=LEFT(FNL$,8%) THEN  &
			  GOTO 3045
3050	  V%=FNN%(4%) &
	\ GOTO 3060 IF V% &
	\ IF X% THEN  &
		  PRINT SPACE$(55%);"ACCOUNT TOTALS"; &
		\ V$=FNP$(T)+FNP$(T(1%)) &
		\ PRINT "*   "; &
		\ V$=FNP$(T2+T)+FNP$(T(2%)) &
		\ PRINT "*" &
		\ T3=T3+T(1%) &
		\ T4=T4+T(2%) &
		\ T1=T1+T &
		\ T=0. &
		\ X%=0%
3055	  GOSUB 2400 &
	\ IF T$(1%)<=K3$ THEN  &
		  GOTO 3040
3060	  PRINT SPACE$(57%);"GRAND TOTALS"; &
	\ V$=FNP$(T1)+FNP$(T3) &
	\ PRINT "    "; &
	\ V$=FNP$(T1+T5)+FNP$(T4) &
	\ PRINT  &
	\ T,T1,T2,T3,T4,T5=0. &
	\ GOTO 1020
4000	  !
4005	  F$="T" &
	\ PRINT "PAPER SIZE IS (8-1/2 BY 11) OR ('WIDER' BY 11)"
5000	  !
5010	  V%=FNG%(4%,"ZZZZZ[1]") &
	\ X%=0% &
	\ GOSUB 2800 &
	\ GOSUB 2050 &
	\ V%=FNG%(6%,"")+FNN%(6%) &
	\ GOSUB 2600 &
	\ V%=FNG%(4%,"") &
	\ GOSUB 2400 &
	\ GOSUB 2900 &
	\ GOTO 5030
5020	  IF FNN%(4%) THEN &
		  GOTO 5070 &
	  ELSE &
		  GOSUB 2400 &
		\ GOSUB 2900
5030	  GOSUB 2050 IF X1%>X5%-8% &
	\ IF K$<>"OUT" AND T$(1%)<>"ZZZZZ[1]" AND T$(1%)>X$ THEN  &
		  GOSUB 2300 &
		\ K$="OUT" IF FNN%(6%) &
		\ GOSUB 2600 &
		\ GOTO 5030
5040	  PRINT  UNLESS F$="T" &
	\ IF T$(1%)<>"ZZZZZ[1]" THEN  &
		  PRINT USING "\"+SPACE$(39%)+"\\      \ ", T$(2%),T$(1%); UNLESS F$="T" &
		\ PRINT USING "\      \ \"+SPACE$(20%)+"\", T$(1%),T$(2%); IF F$="T"
5045	  X1%=X1%+2%+(F$="T") &
	\ T=0. &
	\ IF T$(1%)<>X$ THEN  &
		  GOTO 5100
5050	  GOSUB 2700 &
	\ T=FNZ(T+L1) &
	\ IF FNN%(6%) THEN  &
		  K$="OUT" &
		\ GOTO 5100
5060	  GOSUB 2600 &
	\ IF X$=T$(1%) THEN &
		  GOTO 5050 &
	  ELSE &
		  GOTO 5100
5070	  IF K$="OUT" THEN  &
		  GOTO 5090
5080	  GOSUB 2300 &
	\ IF FNN%(6%)=0% THEN  &
		  GOSUB 2600 &
		\ GOTO 5080
5090	  GOSUB 2050 IF X1%>X5%-8% &
	\ PRINT  &
	\ PRINT  &
	\ IF F$<>"T" THEN  &
		  PRINT SPACE$(42%);"TOTAL : "; &
		\ V$=FNP$(T1) &
		\ PRINT "    "; &
		\ V$=FNP$(T2) &
		\ PRINT  FOR I%=X1% TO X5%+1% &
		\ GOTO 1020
5095	  PRINT TAB(31%); &
	\ V$=FNP$(T1)+FNP$(T2)+FNP$(T1+T2) &
	\ PRINT  FOR I%=X1% TO X5%+1% &
	\ GOTO 1020
5100	  IF F$<>"T" THEN  &
		  V$=FNP$(T)+FNP$(T+P) IF T$(1%)<>"ZZZZZ[1]" &
		\ PRINT  &
		\ T1=FNZ(T1+T) &
		\ T2=FNZ(T2+T+P) &
		\ GOTO 5020
5110	  V$=FNP$(P)+FNP$(T)+FNP$(T+P) IF T$(1%)<>"ZZZZZ[1]" &
	\ T1=FNZ(T1+P) &
	\ T2=FNZ(T2+T) &
	\ PRINT  &
	\ GOTO 5020
7000	  !
7010	  V%=FNG%(4%,"ZZZZZ[1]") &
	\ B$=MID(FNL$,19%,1%) &
	\ X%=0% &
	\ PRINT "Print accounts with a zero balance <N> "; &
	\ INPUT LINE ZERBAL$ &
	\ ZERBAL$=CVT$$(LEFT(ZERBAL$,1%),-1%) &
	\ ZERBAL$="N" IF ZERBAL$<>"Y" &
	\ PRINT "Use printer control feature <N> "; &
	\ INPUT LINE PCNTRL$ &
	\ PCNTRL$=CVT$$(LEFT(PCNTRL$,1%),-1%) &
	\ PCNTRL$="N" IF PCNTRL$<>"Y" &
	\ GOSUB 2800 &
	\ V%=FNG%(6%,"")+FNN%(6%) &
	\ GOSUB 2600 &
	\ V%=FNG%(4%,"") &
	\ GOSUB 2400 &
	\ T%(3%)=0% IF PCNTRL$<>"Y" &
	\ GOSUB 2900 &
	\ GOSUB 2000 &
	\ GOTO 7100
7020	  GOTO 7400 IF FNN%(4%) &
	\ GOSUB 2400 &
	\ T%(3%)=0% IF PCNTRL$<>"Y" &
	\ GOSUB 2900 &
	\ GOTO 7400 IF T$(1%)="ZZZZZ[1]"
7100	  !
7110	  GOSUB 2000 IF X1%>X5%-9% &
	\ IF K$<>"OUT" AND T$(1%)>X$ THEN  &
		  GOSUB 2200 &
		\ K$="OUT" IF FNN%(6%) &
		\ GOSUB 2600 &
		\ GOTO 7110
7130	  IF T$(1%)<>X$ AND T(4%)+P=0. AND ZERBAL$="N" THEN &
		  GOTO 7020 &
	  ELSE &
		  PRINT CVT$$(T$(1%),-1%);TAB(10%);CVT$$(T$(2%),128%) &
		\ PRINT TAB(32%);"BALANCE FORWARD"; &
		\ IF T(4%)<>0. THEN  &
			  PRINT TAB(77%);"0.00";TAB(101%); &
			\ V$=FNP$(P)
7140	  X1%=X1%+2% &
	\ PRINT  &
	\ T,T9=0. &
	\ D$,JOB$="" &
	\ IF T$(1%)<>X$ THEN  &
		  GOTO 7300
7200	  !
7210	  GOSUB 2700 &
	\ GOSUB 2000 IF X1%>X5%-9% &
	\ JOB$="VARIOUS" IF CVT$$(L5$,-1%)<>"" &
	\ A%=A%+1% &
	\ T=FNZ(T+L1) &
	\ IF T%(3%)=255% AND T%(4%)=2% THEN &
		  GOTO 7218 &
	  ELSE &
		  IF T%(3%)=255% AND T%(4%)=3% THEN  &
			  GOTO 7212
7211	  X1%=X1%+1% &
	\ PRINT USING H4$, CVT$$(T$(1%),-1%),M1%,D1%,Y1%,L3$,L$,L2$,L5$; &
	\ V$=FNP$(L1) &
	\ PRINT  &
	\ GOTO 7218
7212	  IF D$<>MID(T2$,9%,4%) THEN  &
		  IF D$="" THEN &
			  D$=MID(T2$,9%,4%)+"" &
		  ELSE &
			  D$=MID(T2$,9%,4%) &
			\ PRINT USING H4$, CVT$$(T$(1%),-1%),CVT$%(LEFT(D$,2%)),CVT$%(RIGHT(D$,3%)),Y1%,"VS","SUMMARY","TOTAL DETAIL  "+NUM1$(A%),JOB$; &
			\ V$=FNP$(T9) &
			\ T9=0. &
			\ JOB$="" &
			\ A%=0% &
			\ X1%=X1%+1% &
			\ PRINT 
7214	  T9=T9+L1
7218	  IF FNN%(6%) THEN  &
		  K$="OUT" &
		\ GOTO 7300
7220	  GOSUB 2600 &
	\ IF X$=T$(1%) THEN &
		  GOTO 7200 &
	  ELSE &
		  IF T%(3%)=255% THEN  &
			  IF T%(4%)=3% THEN  &
				  PRINT USING H4$, CVT$$(T$(1%),-1%),CVT$%(LEFT(D$,2%)),CVT$%(RIGHT(D$,3%)),Y1%,"VS","SUMMARY","TOTAL DETAIL  "+NUM1$(A%),JOB$; &
				\ V$=FNP$(T9) &
				\ PRINT  &
				\ X1%=X1%+1%
7230	  IF T%(3%)=255% AND T%(4%)=2% THEN  &
		  PRINT USING H4$, CVT$$(T$(1%),-1%),M2%,D2%,Y2%,"VS","SUMMARY","TOTAL DETAIL  "+NUM1$(A%),JOB$; &
		\ V$=FNP$(T) &
		\ PRINT  &
		\ X1%=X1%+1%
7300	  !
7310	  PRINT SPACE$(32%);"ACCOUNT TOTALS";SPACE$(23%); &
	\ V$=FNP$(T)+FNP$(T(1%)) &
	\ PRINT "*   "; &
	\ V$=FNP$(T+P) &
	\ V$=FNP$(T(2%)) UNLESS B$="V" &
	\ V$=FNP$(T(2%)+T(1%)) IF B$="V" &
	\ PRINT "*" &
	\ PRINT  &
	\ T1=FNZ(T1+T) &
	\ T2=FNZ(T2+T+P) &
	\ X1%=X1%+2% &
	\ GOTO 7020
7400	  !
7410	  IF K$="OUT" THEN  &
		  GOTO 7430
7420	  GOSUB 2200 &
	\ UNLESS FNN%(6%) THEN  &
		  GOSUB 2600 &
		\ GOTO 7420
7430	  GOSUB 2000 IF X1%>X5%-9% &
	\ PRINT  &
	\ PRINT  &
	\ PRINT SPACE$(32%);"GRAND TOTALS";SPACE$(25%); &
	\ V$=FNP$(T1) &
	\ PRINT "*                 "; &
	\ V$=FNP$(T2) &
	\ PRINT "*" &
	\ PRINT  FOR I%=X1% TO X5%+1% &
	\ GOTO 1020
9000	  !
9010	  JE%,COUNTER%,PERCENT%=0% &
	\ K$=MID(FILE$,INSTR(1%,FILE$,".")-1%,1%) &
	\ JE%=-1% IF K$>="0" AND K$<="9" &
	\ IF FNG%(4%,"ZZZZZ[1]") THEN  &
		  PRINT "The flag record is missing in the chart file." &
		\ PRINT "Please use the ZZZ option in chart to restore this flag." &
		\ PRINT "Aborting update. . ." &
		\ GOTO 10000
9020	  TOTAL%=FNT/10% &
	\ I%=CVT$%(MID(FNL$,13%,2%)) &
	\ B$=MID(FNL$,19%,1%) &
	\ IF I%<>0% THEN  &
		  PRINT "THE LAST UPDATE WAS NOT COMPLETED.  "+"CONTINUING. . . "
9040	  NEWMON%=LAST%+1% &
	\ NEWMON%=1% IF NEWMON%>12% &
	\ NEWMON%=LAST% IF JE%=-1% &
	\ N2%=LAST%-1% &
	\ N2%=12%+N2% IF N2%<=0% &
	\ NEWYEAR%=YEAR% &
	\ NEWYEAR%=YEAR%+1% IF LAST%=12% AND JE%=0% &
	\ NEWYEAR%=0% IF NEWYEAR%=100% &
	\ YEAR$=RIGHT(NUM1$(100%+NEWYEAR%),2%) &
	\ PRINT "CONFIRM CLOSING BOOKS FOR ";FNM1$(NEWMON%);".";YEAR$;"(Y/N)"; &
	\ INPUT K$ &
	\ IF LEFT(K$,1%)<>"Y" THEN  &
		  PRINT "NO UPDATE DONE." &
		\ GOTO 1020
9070	  IF FNG%(6%,"")+FNN%(6%) THEN &
		  PRINT "FILE IS EMPTY.  Aborting. . . ";FNX%("",0%,"") &
	  ELSE &
		  PRINT "Processing. . . " &
		\ PRINT "Percent complete   0%";
9080	  T=0. &
	\ IF FNG%(4%,"ZZZZZ[1]") THEN  &
		  PRINT "Unable to locate the flag record in chart." &
		\ PRINT "Please check chart to see what happened." &
		\ PRINT "Aborting this routine." &
		\ GOTO 10000
9085	  IF FNU%(4%,LEFT(FNL$,12%)+CVT%$(-1%)+RIGHT(FNL$,15%))=0% THEN &
		  V%=FNG%(4%,"") &
	  ELSE &
		  PRINT "Unable to change the "+"flag record in chart.  Aborting this routine." &
		\ GOTO 1010
9090	  COUNTER%=COUNTER%+1% &
	\ IF COUNTER%>TOTAL% THEN  &
		  PERCENT%=PERCENT%+10% &
		\ COUNTER%=0% &
		\ PRINT USING " ###%", PERCENT%;
9095	  GOSUB 2400 &
	\ GOSUB 2900 &
	\ ACCNUM$=CVT$$(T$(1%),-1%) &
	\ ACCNUM$=SPACE$(8%-LEN(ACCNUM$))+ACCNUM$ &
	\ GOTO 9110 IF T%(1%)=NEWMON% AND JE%=0% OR FNG%(6%,ACCNUM$)
9100	  GOSUB 2600 &
	\ GOSUB 2700 &
	\ IF CVT$$(L1$,2%)=CVT$$(T$(1%),2%) THEN  &
		  T=FNZ(T+L1) &
		\ GOTO 9100 UNLESS FNN%(6%)
9110	  GOSUB 9405 &
	\ T=0. &
	\ GOTO 9090 UNLESS FNN%(4%) &
	\ IF JE%=0% THEN &
		  GOTO 9130 &
	  ELSE &
		  PRINT  &
		\ PRINT "Combining closing journal file to monthly file." &
		\ V%=FNX%("[21,6]ISMCAP",30000%,"CP!"+LEFT(FILE$,LEN(FILE$)-1%)+"1/CK"+FNM1$(LAST%)+"."+YEAR$+"T@0-0$"+DEVNAM$+PRGNUM$+PRGNAM$+"#9120*"+CVT%$(NEWMON%)+CVT%$(NEWYEAR%)+FILE$)
9120	  K$=FNX$ &
	\ I%=INSTR(1%,K$,"*") &
	\ NEWMON%=CVT$%(MID(K$,I%+1%,2%)) &
	\ NEWYEAR%=CVT$%(MID(K$,I%+3%,2%)) &
	\ FILE$=RIGHT(K$,I%+5%) &
	\ JE%=-1% &
	\ IF FNO%(4%,"CHART.DAT","/RW","") THEN  &
		  PRINT "Unable to reopen chart to finish the update sequence." &
		\ PRINT "You must resolve this before continuing."; &
		\ PRINT "  Aborting. . . ";FNX%("",0%,"")
9130	  V%=FNG%(4%,"ZZZZZ[1]") &
	\ V%=FNU%(4%,LEFT(FNL$,8%)+CVT%$(NEWMON%)+STRING$(4%,0%)+CVT%$(-1%)+MID(FNL$,17%,4%)+CVT%$(NEWYEAR%)+RIGHT(FNL$,23%)) &
	\ PRINT  &
	\ PRINT "UPDATE COMPLETE FOR ";FNM1$(NEWMON%);NEWYEAR%;"AT ";TIME$(0%) &
	\ V%=FNC%(2%)+FNC%(10%) &
	\ FILE2$=LEFT(FILE$,INSTR(1%,FILE$,".")-1%) &
	\ NAME FILE2$+".DAT" AS FILE2$+"."+YEAR$+"T" UNLESS JE% &
	\ NAME FILE2$+".DA1" AS FILE2$+"."+YEAR$+"1" UNLESS JE% &
	\ KILL FILE$ IF JE% &
	\ KILL LEFT(FILE$,LEN(FILE$)-1%)+"T" IF JE% &
	\ KILL "TT0:GL.DAS" &
	\ GOTO 10000
9400	  !
9405	  IF JE%=-1% THEN  &
		  T(4%)=T(4%)+T &
		\ GOSUB 2500 &
		\ UNLESS FNU%(4%,T0$) THEN &
			  RETURN &
		  ELSE &
			  PRINT "ERROR ";FNS%;" IN FNU%.  CALL CMC!" &
			\ GOTO 10000
9410	  RETURN IF T%(1%)=NEWMON% &
	\ T(5%)=T(N2%+5%) &
	\ T(N2%+5%)=T(3%) &
	\ T(3%)=T(4%) &
	\ T(4%)=T+P &
	\ T%(1%)=NEWMON% &
	\ T(2%)=T(2%)+T(1%) IF B$="V" &
	\ T(2%)=T(1%) IF B$="V" AND FIRST%=-1% &
	\ GOSUB 2500 &
	\ UNLESS FNU%(4%,T0$) THEN &
		  RETURN &
	  ELSE &
		  PRINT "ERROR ";FNS%;" IN FNU%.  CALL CMC!" &
		\ GOTO 10000
10000	  !
10010	  V%=FNX%("",0%,"")
14010	  DEF FNM1$(ARG%) &
	\ FNM1$=MID("JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC",ARG%*3%-2%,3%) &
	\ FNEND
14020	  DEF FNZ(Y) &
	\ FNZ=INT(Y+6.8213185310482798e-15) &
	\ FNEND
14030	  DEF FNP$(Y) &
	\ PRINT "  "; IF F$="T" &
	\ PRINT USING "##,###,###.##-", FNZ(Y)/100.;
14040	  FNEND
15900	  DIM Y%(32%), Y1%(32%), Y$(32%)
32767	  END
