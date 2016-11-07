5	  EXTEND
10	! &
	! BANKBL   FOR ENTERING FROM BANK CLEARED CHECKS - BY CWR MAY 1978 &
	! &

15	  DIM D$(13%) &

100	! &
	! Initial open of files &
	! &

110	  SWITCH$="CL" &
		! CLEARED CHECKS file &

120	  INPUT "Multiple bank code (one digit-hit return if only one bank)";BANK$ &
	\ GOTO 120 IF LEN(BANK$)>1% &
	\ BANK$=LEFT(BANK$+" ",1%) &

130	  INPUT "Month ";MONTH$ &
	\ GOTO 130 IF LEN(MONTH$)<>3% &
	\ IF FNO%(2%,"BB"+MONTH$+BANK$+".DAS","/SF/RW","")=5% &
	  THEN	  INPUT "Should a new file be opened? (Y or N) ";K$ &
		\ GOTO 10000 IF K$<>"Y" &
		\ V%=FNO%(2%,"BB"+MONTH$+BANK$+".DAS","/CR:16/SF","") &

140	  IF FNS% &
	  THEN	  PRINT "Unable to open file!" &
		\ GOTO 10000 &

200	! &
	! Field for buffers &
	! &

205	  OPEN "KB:" AS FILE 12% &

210	  OPEN "NL:" AS FILE 1%,RECORDSIZE 64%+16% &

220	  FIELD #1%,6% AS C$(1%),8% AS C$(2%),8% AS C$(3%),8% AS C$(4%), &
		  8% AS C$(5%) &

230	  FIELD #1%,64% AS G$,6% AS T$(1%), 8% AS T$(2%) &
	\ FIELD #1%,64% AS T$,16% AS T2$ &

250	  GOTO 1000 &

500	! &
	! File seperation subroutines &
	! &

510	  FIELD #2%,B% AS C$,6% AS C$,8% AS C1$ &
	\ C=CVT$F(C1$) &
	\ RETURN &

520	  O=0. &
	\ RETURN IF FNG%(6%,J$) &
	\ B%=FNL% &

530	  FIELD #6%,B% AS O$,6% AS O$,8% AS O1$ &
	\ RETURN IF O$<>J$ &
	\ O=O+CVT$F(O1$) &
	\ RETURN IF FNN%(6%) &
	\ B%=FNL% &
	\ GOTO 530 &

540	  M=0. &
	\ RETURN IF FNG%(4%,J$) &
	\ B%=FNL% &

550	  FIELD #4%,B% AS M$,6% AS M$,8% AS M1$ &
	\ RETURN IF M$<>J$ &
	\ M=M+CVT$F(M1$) &
	\ RETURN IF FNN%(4%) &
	\ B%=FNL% &
	\ GOTO 550 &

560	  J$=SPACE$(6%-LEN(J$))+J$ &
	\ RETURN &

1000	! &
	! OPTIONS &
	! &

1005	  GOTO 1020 &

1010	  PRINT "To switch to a different file, enter" &
	\ PRINT "	SOU - Switch to outstanding file" &
	\ PRINT "	SCU - Switch to current file" &
	\ PRINT "	SCL - Switch to cleared file" &
	\ PRINT &
	\ PRINT "Options available in any file are:" &
	\ PRINT "	PRI - Print file contents" &
	\ PRINT "	ENT - Enter data" &
	\ PRINT "	DEL - Delete data" &
	\ PRINT "	CHA - Change data" &
	\ PRINT "	EXA - Examine data" &
	\ PRINT "	SOR - Sort any unsorted file" &
	\ PRINT "	ADD - Go to adding machine program" &
	\ PRINT "	CLE - Clean items with zero balance" &
	\ PRINT "	END - Exit from program" &
	\ PRINT &
	\ PRINT "Options available only in CLEARED file" &
	\ PRINT "	LOA - Load temporary current file from payroll" &
	\ PRINT "		or check register" &
	\ PRINT "	MAT - Match with outstanding" &
	\ PRINT "	OUT - Outstanding listing" &
	\ PRINT "	TRA - Create new outstanding file" &
	\ GOTO 1050 &

1020	  PRINT &
	\ PRINT "You are using the "; &
	\ IF SWITCH$="CL" &
	  THEN	  PRINT "cleared ";MONTH$;" file." &
	  ELSE	  IF SWITCH$="OU" &
		  THEN	  PRINT "outstanding file." &
		  ELSE	  IF SWITCH$="CU" &
			  THEN	  PRINT "current checks file." &
			  ELSE	  PRINT "*** ERROR *** SWITCH$=";SWITCH$ &
				\ PRINT "***Unable to continue***" &
				\ GOTO 10000 &

1050	  K$=SYS(CHR$(2%)) &
	\ PRINT &
	\ INPUT "Option:";I$ &
	\ PRINT &
	\ I$=LEFT(I$,3%) &
	\ GOTO 1100 IF I$="ENT" &
	\ GOTO 2000 IF I$="PRI" &
	\ GOTO 3000 IF I$="CHA" &
	\ GOTO 4000 IF I$="EXA" &
	\ GOTO 4100 IF I$="CLE" &
	\ GOTO 5000 IF I$="DEL" &
	\ GOTO 5500 IF I$="LOA" &
	\ GOTO 10000 IF I$="END" &
	\ GOTO 6000 IF I$="MAT" &
	\ GOTO 7000 IF I$="OUT" &
	\ GOTO 8000 IF I$="TRA" &
	\ GOTO 9000 IF I$="SOR" &
	\ GOTO 9100 IF I$="SOU" OR I$="SCU" OR I$="SCL" &
	\ GOTO 9500 IF I$="ADD" &

1070	  GOTO 1010 &

1100	! &
	! ENTER &
	! &

1103	  PRINT "Type 'TOTAL' to get sub total" &
	\ PRINT "Type '-' to void current line" &

1105	  GOSUB 2510 &
	\ T=0. &

1110	  K$=SYS(CHR$(3%)) &
	\ INPUT LINE #12%,K$ &
	\ K$=CVT$$(K$,4%) &
	\ GOTO 1150 IF K$="TOTAL" &
	\ GOTO 1020 IF K$="" &
	\ RSET C$(1%)=K$ &
	\ PRINT USING "\    \",C$(1%); &

1120	  FOR I%=2% TO 2% &
		\ INPUT LINE #12%,K$ &
		\ K$=CVT$$(K$,4%) &
		\ GOTO 1180 IF K$="-" &
		\ GOTO 1150 IF K$="TOTAL" &
		\ V=VAL(K$)/100. &
		\ PRINT USING "  ###,###,###.##",V; &
		\ T=T+V &
		\ LSET C$(I%)=CVTF$(V) &
	\ NEXT I% &
	\ PRINT &

1130	  IF FNA%(2%,T$) &
	  THEN	  PRINT "Unable to add to file!" &
		\ GOTO 10000 &

1140	  GOTO 1110 &

1150	  PRINT &
	\ PRINT "Total ====>";TAB(18%); &
	\ PRINT USING "###,###,###.##",T &
	\ T=0. &
	\ PRINT &
	\ GOTO 1110 &

1180	  PRINT "Line is void" &
	\ GOTO 1110 &

2000	! &
	! PRINT &
	! &

2005	  IF FNG%(2%,"") &
	  THEN	  PRINT "Nothing in file!" &
		\ GOTO 1000 &

2010	  INPUT "Set page - hit return";K$ &
	\ L%=63% &
	\ GOSUB 2500 &
	\ T(I%)=0. FOR I%=2% TO 5% &

2025	  LSET T$=FNL$ &
	\ L1%=LEN(CVT$$(C$(1%),2%)) &

2030	  LSET T$=FNL$ &
	\ IF LEN(CVT$$(C$(1%),2%))<>L1% &
	  THEN	  L1%=LEN(CVT$$(C$(1%),2%)) &
		\ GOTO 2060 &

2040	  PRINT C$(1%); &
	\ FOR I%=2% TO 2% &
		\ PRINT USING "  ###,###,###.##",CVT$F(C$(I%)); &
	\ NEXT I% &
	\ PRINT &
	\ L%=L%+1% &
	\ GOSUB 2500 IF L%>50% &
	\ T(I%)=T(I%)+CVT$F(C$(I%)) FOR I%=2% TO 2% &

2050	  GOTO 2030 IF FNN%(2%)=0% &

2060	  PRINT "Total "; &
	\ PRINT USING "  ###,###,###.##",T(I%); FOR I%=2% TO 2% &
	\ PRINT &
	\ PRINT &
	\ L%=L%+2% &
	\ T(I%)=0. FOR I%=2% TO 2% &
	\ GOTO 2040 UNLESS FNS% &
	\ GOTO 1020 &

2500	  PRINT STRING$(66%-L%,10%) &
	\ L%=3% &

2510	  PRINT "Month of ";MONTH$;"     ";DATE$(0%);"   ";TIME$(0%) &
	\ PRINT "Check #         Amount  " &

2520	  RETURN &

3000	! &
	! CHANGE &
	! &

3010	  V$=SYS(CHR$(3%)) &
	\ PRINT "Check#"; &
	\ INPUT LINE J$ &
	\ J$=CVT$$(J$,4%) &
	\ GOTO 1020 IF J$="" &
	\ GOTO 3100 IF J$="+" &
	\ GOTO 3200 IF J$="-" &
	\ GOSUB 560 &
	\ IF FNG%(2%,J$)<>0% &
	  THEN	  PRINT "Check ";J$;" not found " &
		\ GOTO 3010 &

3020	  LSET T$=FNL$ &
	\ PRINT C$(1%) &

3030	  PRINT USING "###,###,###.##",CVT$F(C$(2%)); &
	\ INPUT "  ";K$ &
	\ GOTO 3100 IF K$="+" &
	\ GOTO 3200 IF K$="-" &
	\ LSET C$(2%)=CVTF$(VAL(K$)/100.) IF K$<>"" &
	\ PRINT USING "###,###,###.##",CVT$F(C$(2%)) &

3040	  IF FNU%(-2%,T$) &
	  THEN	  PRINT "Unable to update file!" &
		\ GOTO 1000 &

3050	  GOTO 3010 &

3100	! (+) &

3110	  PRINT "(+)" &
	\ GOTO 3020 UNLESS FNN%(2%) &
	\ PRINT "End of file has been reached!" &
	\ GOTO 3010 &

3200	! (-) &

3210	  PRINT "(-)" &
	\ GOTO 3020 UNLESS FNN%(-2%) &
	\ PRINT "Beginning of file has been reached!" &
	\ GOTO 3010 &

4000	! &
	! EXAMINE &
	! &

4005	  GOSUB 2510 &

4010	  INPUT "Starting check#";J$ &
	\ GOTO 1020 IF J$="" &
	\ INPUT "Ending check#";E$ &
	\ T=0. &
	\ E$=SPACE$(6%-LEN(E$))+E$ &
	\ GOSUB 560 &
	\ I%=FNG%(2%,J$) &

4015	  LSET T$=FNL$ &
	\ GOTO 4030 IF C$(1%)>E$ &
	\ GOSUB 4020 &
	\ GOTO 4015 IF FNN%(2%)=0% &
	\ GOTO 4030 &

4020	  PRINT C$(1%); &
	\ FOR I%=2% TO 2% &
		\ PRINT USING "  ###,###,###.##",CVT$F(C$(I%)); &
		\ T=T+CVT$F(C$(I%)) &
	\ NEXT I% &
	\ PRINT &
	\ RETURN &

4030	  PRINT &
	\ PRINT "Total ++++>";TAB(18%); &
	\ PRINT USING "###,###,###.##",T &
	\ GOTO 1020 &

4100	! &
	! CLEAN ZERO BALANCES &
	! &

4110	  INPUT "CONFIRM CLEAN (Y OR N) ",J$ &
	\ GOTO 1020 IF J$<>"Y" &
	\ GOTO 1020 IF FNG%(2%,"") &
	\ E$="" &
	\ T=0. &

4120	  LSET T$=FNL$ &
	\ IF C$(1%)<>E$ AND E$<>"" THEN &
	  IF T<-.001 OR T>.001 THEN T=0. ELSE &
	  J$=C$(1%)+"" &
	\ PRINT E$ WHILE FND%(2%,E$)=0% &
	\ STOP IF FNG%(2%,J$) &
	\ T=0. &

4230	  E$=C$(1%)+"" &
	\ T=T+CVT$F(C$(2%)) &
	\ GOTO 4120 IF FNN%(2%)=0% &
	\ GOTO 1020 &

5000	! &
	! DELETE &
	! &

5010	  INPUT "Check# ";J$ &
	\ GOSUB 560 &
	\ GOTO 1020 IF J$="" &
	\ IF FNG%(2%,J$)<>0% &
	  THEN	  PRINT "Check not found " &
		\ GOTO 5010 &

5020	  LSET T$=FNL$ &
	\ GOSUB 4020 &
	\ INPUT "Confirm (Y or N)";K$ &
	\ GOTO 5100 IF K$="+" &
	\ GOTO 5200 IF K$="-" &
	\ GOTO 5010 IF K$<>"Y" &
	\ IF FND%(2%,"") &
	  THEN	  PRINT "Unable to delete record!" &
		\ GOTO 1000 &

5030	  PRINT "Deleted" &
	\ GOTO 5010 &

5100	  GOTO 5020 UNLESS FNN%(2%) &
	\ PRINT "End of file reached!" &
	\ GOTO 5010 &

5200	  GOTO 5020 UNLESS FNN%(-2%) &
	\ PRINT "Beginning of file reached!" &
	\ GOTO 5010 &

5500	! &
	! CREATE TEMPORARY FILE &
	! &

5505	  IF SWITCH$<>"CL" &
	  THEN	  PRINT "This option is only usable when in the CLEARED file" &
		\ GOTO 1000 &

5510	  INPUT "From payroll file (Y or N)";F$ &
	\ GOTO 5700 IF F$="Y" &
	\ INPUT "Check data file to create from (JAN, FEB , ETC.)";F$ &
	\ GOTO 5510 UNLESS INSTR(1%,"!JAN!FEB!MAR!APR!MAY!JUN!JUL!"+ &
		  "AUG!SEP!OCT!NOV!DEC",F$)>0% AND LEN(F$)>2% &
	\ V%=FNO%(4%,"CK"+F$+".DAT","","") &
	\ IF V%<>0% &
	  THEN	  STOP IF V%<>5% &
		\ X%=VAL(RIGHT(DATE$(0%),8%)) &
		\ X1%=X%-1% &
		\ V%=FNO%(4%,"CK"+F$+"."+NUM1$(X%)+"T","","") &
		\ IF V%<>0% &
		  THEN	  STOP IF V%<>5% &
			\ V%=FNO%(4%,"CK"+F$+"."+NUM1$(X1%)+"T","","") &
			\ IF V% &
			  THEN	  PRINT "Unable to find ckdata file" &
				\ GOTO 1020 &

5515	  INPUT "Start new loading process (Y or N)";K$ &
	\ IF K$="Y" &
	  THEN	  STOP IF FNO%(6%,"BBCCR"+BANK$+".DAS","/CR:16/SF","") &
	  ELSE	  GOTO 5515 IF FNO%(6%,"BBCCR"+BANK$+".DAS","/SF","") &

5520	  FOR X%=1% TO 2% &
		\ PRINT &
		\ R1%=FNL1%(MID("     0    1000",(X%-1%)*6%+1%,6%)) &
		\ R2%=FNL1%(MID("   999999999",(X%-1%)*6%+1%,6%)) &
		\ D$=MID("RPCK",(X%-1%)*2%+1%,2%) &
		\ PRINT "Enter ";MID("RecieptsChecks",(X%-1%)*8%+1%,8%); &
			  " (Y or N)"; &
		\ INPUT K$ &
		\ GOTO 5560 UNLESS LEFT(K$,1%)="Y" &
		\ PRINT"Type <CR> at Starting ";D$;"# and Ending ";D$; &
			  "# to include all ";MID("RecieptsChecks",(X%-1%)*8%+ &
			  1%,8%) &
		\ PRINT &
		\ PRINT "Starting ";D$;"#"; &
		\ INPUT S1$ &
		\ S1$=SPACE$(6%-LEN(S1$))+S1$ &
		\ PRINT "Ending ";D$;"#  "; &
		\ INPUT E$ &
		\ E$=SPACE$(6%-LEN(E$))+E$ &

5523		  INPUT "Source code (CK, PR, ECT. use 'RETURN' for ANY)",V$ &
		\ V1$="C" &
		\ INPUT "Account #  use 'RETURN' for ANY)";A1$ &
		\ GOTO 5525 IF X%=1% &
		\ INPUT "Check are in expense or checking account? (E or C)";V1$ &

5525		  GOTO 5530 IF FNG%(4%,S1$)=0% &
		\ STOP IF FNG%(4%,"") &

5530		  FIELD #5%,FNL% AS K$,6% AS M$(1%),8% AS A$,2% AS S$,8% AS M1$ &
		\ J%=FNL1%(CVT$$(M$(1%),2%)) &
		\ GOTO 5550 IF J%<R1% OR J%>R2% &
		\ GOTO 5550 IF M$(1%)<S1$ UNLESS S1$="" &
		\ GOTO 5550 IF M$(1%)>E$ UNLESS E$="" &
		\ GOTO 5550 IF S$<>V$ UNLESS V$="" &
		\ GOTO 5550 IF CVT$$(A$,2%)<>CVT$$(A1$,2%) UNLESS A1$="" &
		\ LSET T$(1%)=M$(1%) &
		\ LSET T$(2%)=M1$ &
		\ LSET T$(2%)=CVTF$(-CVT$F(M1$)) IF V1$="C" &
		\ STOP IF FNA%(6%,T2$) &

5550		  GOTO 5530 UNLESS FNN%(4%) &

5560	  NEXT X% &
	\ PRINT "File created !!"+STRING$(3%,7%) &
	\ GOTO 9000 &

5700	  INPUT "Payroll date (MM.DD.YY) ";F$ &
	\ GOTO 1020 IF F$="" &
	\ F$="0"+F$ IF INSTR(1%,F$,".")=2% &
	\ F$=LEFT(F$,3%)+"0"+RIGHT(F$,4%) IF INSTR(4%,F$,".")=5% &
	\ F$=LEFT(F$,2%)+RIGHT(F$,4%)+"T" &

5710	  IF FNO%(4%,"TX"+F$,"/RO","") &
	  THEN	  PRINT "No payroll found for that date" &
		\ GOTO 1020 &

5720	  INPUT "Start new file (Y or N)";K$ &
	\ IF K$="Y" &
	  THEN	  STOP IF FNO%(6%,"BBCCR"+BANK$+".DAS","/CR:16/SF","") &
	  ELSE	  GOTO 5720 IF FNO%(6%,"BBCCR"+BANK$+".DAS","/SF","") &

5721	  STOP IF FNG%(4%,"") &

5730	  FIELD #5%,FNL% AS M$,21% AS M$,8% AS M$,2% AS M1$,8% AS D$(1%), &
		  8% AS D$(2%),8% AS D$(3%),8% AS D$(4%),8% AS D$(5%), &
		  8% AS D$(6%),8% AS D$(7%),8% AS D$(8%),8% AS D$(9%), &
		  8% AS D$(10%),8% AS D$(11%),3% AS D$(12%), &
		  3% AS D$(13%) &

5740	  C=0. &
	\ C=C+CVT$F(D$(I%)) FOR I%=3% TO 11% &
	\ M=CVT$%(LEFT(D$(12%),2%))+ASCII(RIGHT(D$(12%),3%))/100. &
	\ C=CVT$F(D$(1%))-C-M &

5750	  M1$=CVT$$(M$,2%) &
	\ M1$=SPACE$(6%-LEN(M1$))+M1$ &
	\ STOP IF FNA%(6%,M1$+CVTF$(C)) &

5760	  GOTO 5730 IF FNN%(4%)=0% &
	\ PRINT "Done" &
	\ GOTO 9000 &
		! Sort file and return to option
6000	! &
	! MATCH CLEARED CHECKS WITH OUTSTANDING &
	! &

6002	  IF SWITCH$<>"CL" &
	  THEN	  PRINT "This option is usable only in the CLEARED file" &
		\ GOTO 1000 &

6005	  U1$="\    \ ###,###,###.## ###,###,###.## ###,###,###.##" &

6010	  GOTO 6070 IF FNO%(4%,"BBCCR"+BANK$+".DAS","/SF/RO","") &

6020	  STOP IF FNO%(6%,"BBOUT"+BANK$+".DAS","/SF/RO","") &
	\ FOR X%=1% TO 2% &
		\ R1%=FNL1%(MID("     0  1000",(X%-1%)*6%+1%,6%)) &
		\ R2%=FNL1%(MID("   999999999",(X%-1%)*6%+1%,6%)) &
		\ PRINT FOR Y%=1% TO 3% &
		\ PRINT "####### ";MID("ReceiptsChecks",(X%-1%)*8%+1%,8%); &
			  "   #######" &
		\ PRINT FOR Y%=1% TO 2% &
		\ PRINT "Check #       Cleared    Outstanding          Current" &
		\ STOP IF FNG%(2%,"") &
		\ B%=FNL% &

6030		  GOSUB 510 &
		\ J$=CVT$$(C$,2%) &
		\ J%=FNL1%(J$) &
		\ GOTO 6050 IF J%<R1% OR J%>R2% &
		\ GOSUB 560 &
		\ GOSUB 520 &
		\ GOSUB 540 &

6040		  GOTO 6050 IF C==O+M &
		\ PRINT &
		\ PRINT USING U1$,J$,C,O,M &

6050		  PRINT "!"; &
		\ PRINT IF POS(0%)>70% &
		\ GOTO 6060 IF FNN%(2%) &
		\ B%=FNL% &
		\ GOTO 6030 &

6060	  NEXT X% &
	\ V%=FNC%(4%)+FNC%(6%) &
	\ GOTO 1020 &

6070	  PRINT "Please create a current check register with the 'LOA' option" &
	\ PRINT "     Before running this option !!!" &
	\ GOTO 1020 &

7000	! &
	! PRINT OUTSTANDING CHECKS USING CLEARED, LAST MONTHS OUTSTANDING, AND &
	! THIS MONTH CHECKS &
	! &

7005	  IF SWITCH$<>"CL" &
	  THEN	  PRINT "This option is usable only in the CLEARED file" &
		\ GOTO 1000 &

7010	  U2$="\      \ ###,###,###.##" &
	\ U3$="\      \                     ###,###,###.##" &
	\ U4$="         ###,###,###.##      ###,###,###.##  ###,###,###.##" &
	\ H$= "Check #     Outstanding             Cleared           Total" &
	\ T,T1,T2,T3=0. &

7020	  S=1. &
	\ INPUT "Set page and press return";J$ &
	\ L%=63% &
	\ GOSUB 7170 &
	\ FOR X%=1% TO 2% &
		\ T,T1,T2,T3=0. &
		\ PRINT &
		\ GOTO 6070 IF FNO%(4%,"BBOUT"+BANK$+".DAS","/SF/RO","") &
		\ PRINT TAB(32%);"<<<<< ";MID("ReceiptsChecks",(X%-1%)*8%+1%, &
			  8%); " >>>>>" &
		\ PRINT &
		\ R1%=FNL1%(MID("     0  1000",(X%-1%)*6%+1%,6%)) &
		\ R2%=FNL1%(MID("   999999999",(X%-1%)*6%+1%,6%)) &
		\ GOSUB 7050 &
		\ L%=L%+1% &
		\ V%=FNC%(4%) &

7030		  PRINT TAB(10%);STRING$(60%,45%) &
		\ PRINT USING U4$,T,T1,T+T1 &
		\ PRINT &
		\ L%=L%+2% &
		\ T2=T &
		\ T3=T1 &
		\ T,T1=0. &

7040		  GOTO 6070 IF FNO%(4%,"BBCCR"+BANK$+".DAS","/SF/RO","") &
		\ S=1. &
		\ GOSUB 7050 &
		\ V%=FNC%(4%) &
		\ PRINT TAB(10%);STRING$(60%,45%) &
		\ PRINT USING U4$,T,T1,T+T1 &
		\ PRINT &
		\ PRINT "Total ";MID("RptCks",(X%-1%)*3%+1%,3%); &
		\ PRINT USING RIGHT(U4$,10%),T+T2,T1+T3,T+T2+T1+T3 &
		\ T(1%)=T(1%)+T+T2 &
		\ T(2%)=T(2%)+T1+T3 &
		\ T(3%)=T(3%)+T3+T+T2+T1 &
		\ PRINT &
		\ L%=L%+4% &
		\ T(X1%)=-T(X1%) FOR X1%=1% TO 3% IF X%=1% &
	\ NEXT X% &
	\ PRINT &
	\ PRINT "Gr. Total"; &
	\ PRINT USING RIGHT(U4$,10%),T(1%),T(2%),T(3%) &
	\ GOTO 1020 &

7050	  V%=FNG%(4%,"") &
	\ B%=FNL% &

7060	  GOSUB 7170 IF L%>52% &
	\ J$="" &
	\ M=0. &
	\ GOTO 7090 IF V% &

7070	  FIELD #4%,B% AS M$,6% AS M$,8% AS S1$ &
	\ M2$=CVT$$(M$,2%) &
	\ J%=FNL1%(M2$) &
	\ GOTO 7080 IF J%>R2% OR J%<R1% &
	\ GOTO 7090 IF M$<>J$ UNLESS J$="" &
	\ M=M+S*CVT$F(S1$) &
	\ J$=M$+"" &

7080	  V%=FNN%(4%) &
	\ GOTO 7090 IF V% &
	\ B%=FNL% &
	\ GOTO 7070 &

7090	  J$=CVT$$(J$,2%) &

7100	  J$=SPACE$(6%-LEN(J$))+J$ &

7110	  IF FNG%(2%,J$) &
	  THEN	  7120 &
	  ELSE	  7140 &

7120	  J%=FNL1%(CVT$$(J$,2%)) &
	\ GOTO 7130 IF J%>R2% OR J%<R1% &
	\ PRINT USING U2$,J$,M &
	\ L%=L%+1% &
	\ T=T+M &

7130	  IF V% &
	  THEN	  RETURN &
	  ELSE	  7060 &

7140	  FIELD #2%,FNL% AS M$,6% AS M$,8% AS M1$ &
	\ A=CVT$F(M1$) &
	\ J%=FNL1%(CVT$$(M$,2%)) &
	\ GOTO 7150 IF J%>R2% OR J%<R1% &
	\ PRINT USING U3$,J$,A &
	\ T1=T1+A &
	\ L%=L%+1% &

7150	  GOTO 7060 IF V%=0% &

7160	  RETURN &

7170	  PRINT STRING$(66%-L%,10%) &
	\ PRINT "** Outstanding Check Report **";" (";BANK$;") ";DATE$(0%), &
		  TIME$(0%) &
	\ PRINT &
	\ PRINT H$ &
	\ L%=L%+3% &

7180	  L%=4% &
	\ RETURN &

8000	! &
	! DELETE CLEARED CHECKS FROM OUTSTANDING FILE AND MOVE THIS MONTHS &
	! OUTSTANDING FROM THE CKDATA FILE TO OUTSTANDING (CKOUT.DAT) &

8005	  IF SWITCH$<>"CL" &
	  THEN	  PRINT "This option is usable only in the CLEARED file" &
		\ GOTO 1000 &

8010	  IF FNO%(6%,"BBCCR"+BANK$+".DAS","/SF/RO","") &
	  THEN	  PRINT "Unable to open cleared checks file!" &
		\ GOTO 1000 &

8015	  STOP IF FNO%(4%,"BBOUT"+BANK$+".DAS","/SF/RW","") &

8020	  FOR X%=1% TO 2% &
		\ R1%=FNL1%(MID("     0  1000",(X%-1%)*6%+1%,6%)) &
		\ R2%=FNL1%(MID("   999999999",(X%-1%)*6%+1%,6%)) &

8030		  STOP IF FNG%(4%,"") &
		\ B%=FNL% &

8040		  FIELD #4%,B% AS M$,6% AS M$ &
		\ J$=CVT$$(M$,2%) &

8050		  J$=SPACE$(6%-LEN(J$))+J$ &
		\ J%=FNL1%(CVT$$(J$,2%)) &
		\ IF J%>R2% OR J%<R1% &
		  THEN	  IF FNN%(4%) &
			  THEN	  8080 &
			  ELSE	  B%=FNL% &
				\ GOTO 8040 &

8060		  IF FNG%(2%,J$) &
		  THEN	  8070 &
		  ELSE	  PRINT "D"; &
			\ PRINT IF POS(0%)>70% &
			\ STOP IF FND%(4%,"") &

8070		  PRINT "'"; &
		\ PRINT IF POS(0%)>70% &
		\ IF FNN%(4%) &
		  THEN	  8080 &
		  ELSE	  B%=FNL% &
			\ GOTO 8040 &

8080		  STOP IF FNG%(6%,"") &
		\ B%=FNL% &

8090		  FIELD #6%,B% AS M$,6% AS M$,8% AS S$ &
		\ J$=CVT$$(M$,2%) &

8100		  J$=SPACE$(6%-LEN(J$))+J$ &
		\ J%=FNL1%(CVT$$(J$,2%)) &
		\ IF J%>R2% OR J%<R1% &
		  THEN	  IF FNN%(6%) &
			  THEN	  8140 &
			  ELSE	  B%=FNL% &
				\ GOTO 8090 &

8110		  IF FNG%(2%,J$) &
		  THEN	  8120 &
		  ELSE	  8130 &

8120		  PRINT "A"; &
		\ PRINT IF POS(0%)>70% &
		\ LSET T$=M$+S$+STRING$(50%,0%) &
		\ STOP IF FNA%(4%,T$) &

8130		  PRINT "!"; &
		\ PRINT IF POS(0%)>72% &
		\ IF FNN%(6%) &
		  THEN	  8140 &
		  ELSE	  B%=FNL% &
			\ GOTO 8090 &

8140	  NEXT X% &
	\ KILL "BBCCR"+BANK$+".DAS" &
	\ KILL "BB"+MONTH$+BANK$+".DAS" &
	\ PRINT "Done" &
	\ STOP IF FNX%("",0%,"") &

9000	! &
	! Sort files and return &
	! &
	  V%=FNX%("[1,201]BANKBL",30000,SWITCH$+BANK$+MONTH$) &

9100	! &
	! Switch to another file &
	! &

9110	  SWITCH1$=RIGHT(I$,2%) &
	\ GOSUB 9200 &
	\ IF SWITCH1$="**" &
	  THEN	  SWITCH1$=SWITCH$ &
		\ PRINT "Unable to switch... returning to pre-switch..." &
		\ GOSUB 9200 &
		\ IF SWITCH1$="**" &
		  THEN	  PRINT "Unable to switch back!!!" &
			\ GOTO 10000 &

9120	  GOTO 1000 &

9200	! &
	! Close out currently used file and open alternate &
	! &
	! File to switch to is given in SWITCH1$ &
	! On error, SWITCH1$ returns with '**' &
	! &

9210	  V%=FNC%(2%) &
	\ IF SWITCH1$="CL" &
	  THEN	  FILENAME$="BB"+MONTH$+BANK$+".DAS" &
	  ELSE	  IF SWITCH1$="CU" &
		  THEN	  FILENAME$="BBCCR"+BANK$+".DAS" &
		  ELSE	  IF SWITCH1$="OU" &
			  THEN	  FILENAME$="BBOUT"+BANK$+".DAS" &
			  ELSE	  PRINT "Illegal type (";SWITCH1$;")." &
				\ SWITCH1$="**" &
				\ RETURN &

9220	  GOTO 9230 IF FNO%(2%,FILENAME$,"/RW/SF","")=0% &
	\ PRINT "Unable to open ";FILENAME$;" (Type ";SWITCH1$;")" &
	\ IF SWITCH1$="CU" OR SWITCH1$="OU" &
	  THEN	  INPUT "Shall I create the file (y/N) ";K$ &
		\ IF LEFT(CVT$$(K$,-1%),1%)="Y" &
		  THEN	  IF FNO%(2%,FILENAME$,"/RW/SF/CR:16","") &
			  THEN	  PRINT "Unable to create file!" &
			  ELSE	  PRINT "New file created!" &
				\ GOTO 9230 &

9227	  SWITCH1$="**" &
	\ RETURN &

9230	  SWITCH$=SWITCH1$ &
	\ RETURN &

9500	! &
	! Go to adding machine &
	! &
	  V%=FNX%("[1,201]BANADD",0,SWITCH$+BANK$+MONTH$) &

10000	! &
	! End program &
	! &
	  V%=FNX%("",0%,"") &

11000	  DEF FNL1%(X$)=LEN(CVT$$(X$,2%)) &

30000	! &
	! Chain back in after sort & return to option &
	! &

30010	  G$=FNX$ &
	\ BANK$=MID(G$,3%,1%) &
	\ MONTH$=RIGHT(G$,4%) &
	\ SWITCH$,SWITCH1$=LEFT(G$,2%) &
	\ GOSUB 9200 &
	\ IF SWITCH1$="**" &
	  THEN	  PRINT "Unable to re-open file (Type ";SWITCH$;") !" &
		\ GOTO 10000 &

30020	  GOTO 200 &

32767	  END &

