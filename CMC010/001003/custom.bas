1	  ! &
	  ! Program name: custom		Compiled with SCALE 0 on V09.3 &
	  ! Decompiled on 24-Nov-16 at 02:11 AM
10	  !
50	  DIM U$(22%), C$(9%), C(3%), C1(10%), S$(22%)
100	  !
110	  OPEN "KB:" AS FILE 1%, RECORDSIZE 256% &
	\ V%=FNO%(4%,"RECHLD.DAT","/SF/RO","") &
	\ V%=FNO%(8%,"RECEIV.DAT","/SF/RO","") &
	\ B%=-1% IF FNO%(6%,"CHART.DAT","/SF/RO","") &
	\ IF FNO%(2%,"CUSTOM.DAT","/RW","")=0% THEN &
		  GOTO 130 &
	  ELSE &
		  IF FNS%=5% THEN  &
			  PRINT "A customer file does not exist.  "+"Shall I create one "; &
			\ INPUT LINE K$ &
			\ K$=CVT$$(LEFT(K$,1%),-1%) &
			\ GOTO 10000 IF K$<>"Y" &
			\ IF FNO%(2%,"CUSTOM.DAT","/CR:8,256","")=0% THEN  &
				  PRINT "A new file has been created." &
				\ GOTO 130
120	  PRINT "Error";FNS%;"while opening the customer file." &
	\ PRINT "Aborting. . ." &
	\ GOTO 10000
130	  ON ERROR GOTO 150 &
	\ OPEN "SS0:UNIQUE.FIL/RO" FOR INPUT AS FILE 12% &
	\ DIM #12%, A0$(255%)=64% &
	\ ON ERROR GOTO 0 &
	\ GOTO 170
150	  PRINT "YOU MUST CREATE A FILE CONTAINING YOUR SPECIFIC COMPANY'S INFORMATION" &
	\ PRINT "BEFORE THESE PROGRAMS CAN BE RUN. USE THE PROGRAM '!UNIQUE'" &
	\ PRINT "TO DO THIS." &
	\ RESUME 10000
170	  OPEN "KB:" FOR INPUT AS FILE 11% &
	\ U$="\    \   \"+SPACE$(28%)+"\                    ###,###.##"
200	  !
210	  FIELD #1%, 6% AS U$(1%),30% AS U$(2%),30% AS U$(3%),30% AS U$(4%),30% AS U$(5%),8% AS U$(6%),8% AS U$(7%),12% AS U$(8%) &
	\ FIELD #1%, 154%+I%*8% AS E$,8% AS U$(I%+9%) FOR I%=0% TO 10% &
	\ FIELD #1%, 242% AS E$,2% AS U$(20%),4% AS U$(21%),8% AS U$(22%) &
	\ FIELD #1%, 256% AS T3$
300	  !
320	  READ S$(I%) FOR I%=1% TO 22% &
	\ DATA	"CUSTOMER # ","NAME     ","ADDRESS  ","CITY,ST ","ZIP CODE ","RECEIV. G/L # ","SALES GL # ","PHONE ","LAST PMT DATE","PERCENT DISCOUNT ","PERCENT SALES TAX ","PERCENT INTEREST ","CURRENT ","30 DAY ","60 DAY ","90 DAY ","PAST 90 ","YTD SRVC ","CURRENT SRVC ","YTD SALES","CREDIT LIMIT","BALANCE"

370	  Z0$=MID(A0$(10%),2%,1%) &
	\ IF Z0$<>"B" AND Z0$<>"O" THEN  &
		  PRINT "PLEASE ENTER COMPLETE INFORMATION USING !UNIQUE." &
		\ GOTO 10000
1000	  !
1020	  V1$="" &
	\ PRINT  &
	\ INPUT "OPTION ";K$ &
	\ K$=LEFT(CVT$$(K$,-1%),3%) &
	\ GOTO 1030 IF K$="" &
	\ GOTO 2000 IF K$="ENT" &
	\ GOTO 3000 IF K$="DEL" &
	\ GOTO 4000 IF K$="CHA" &
	\ GOTO 5000 IF K$="EXA" &
	\ GOTO 6000 IF K$="FIN" &
	\ GOTO 7000 IF K$="PRI" &
	\ GOTO 9000 IF K$="SOR" &
	\ GOTO 10000 IF K$="END" &
	\ GOTO 8000 IF K$="LAB" &
	\ GOTO 8500 IF K$="YTD" &
	\ GOTO 8700 IF K$="ZER" &
	\ GOTO 1500 IF K$="BAL" &
	\ GOTO 5500 IF K$="REV" &
	\ PRINT  &
	\ PRINT "TYPE <RETURN> FOR AN OPTIONS LIST." &
	\ GOTO 1020
1030	  PRINT  &
	\ PRINT "OPTIONS:  'ENTER' NEW CUSTOMERS" &
	\ PRINT "          'DELETE' EXISTING CUSTOMERS" &
	\ PRINT "          'CHANGE' CUSTOMER INFORMATION" &
	\ PRINT "          'EXAMINE' CUSTOMERS" &
	\ PRINT "          'FIND' A SINGLE CUSTOMER" &
	\ PRINT "          'PRINT' ALL CUSTOMERS" &
	\ PRINT "          'LABEL' WILL PRINT LABLES" &
	\ PRINT "          'SORT'CUSTOMER FILE" &
	\ PRINT "          'YTD' PRINTING OF INTEREST" &
	\ PRINT "          'ZER'OO YTD INTEREST AND SALES" &
	\ PRINT "          'REV'ISE DISCOUNT, SALES TAX, OR INTEREST" &
	\ PRINT "          'END' PROGRAM AND UPDATE FILES" &
	\ PRINT "          'BALANCE' SETUP" &
	\ GOTO 1020
1500	  !
1510	  T=0. &
	\ PRINT "ENTER 'END' TO TERMINATE ENTRIES" &
	\ PRINT "START WITH CUSTOMER # "; &
	\ GOSUB 2200 &
	\ V%=FNG%(2%,K$) &
	\ FOR I%=1% TO 8% &
		\ PRINT "ENTER ";S$(12%+I%);" (Y OR N)"; &
		\ INPUT B$(I%) &
	\ NEXT I% &
	\ PRINT "ENTER ";S$(9%);" (Y OR N)"; &
	\ INPUT B$(I%+1%) &
	\ PRINT S$(12%+I%) IF B$(I%)="Y" FOR I%=1% TO 8% &
	\ PRINT S$(9%) IF B$(I%+1%)="Y" &
	\ INPUT "OK (Y OR N)",K$ &
	\ GOTO 1510 IF K$<>"Y" &
	\ V1$=SYS(CHR$(3%))
1520	  GOSUB 2300 &
	\ PRINT C$(1%);" ";C$(2%);" : "; &
	\ PRINT  &
	\ FOR I%=1% TO 8% &
		\ IF LEFT(B$(I%),1%)="Y" THEN  &
			  PRINT S$(12%+I%);":"; &
			\ GOSUB 1596 &
			\ IF K$="END" THEN  &
				  GOTO 1590
1525			  NEXT I% &
	\ IF LEFT(B$(I%+1%),1%)="Y" THEN  &
		  PRINT S$(9%);":"; &
		\ GOSUB 2200 &
		\ PRINT " ";K$ &
		\ IF K$="END" THEN &
			  GOTO 1590 &
		  ELSE &
			  C$(9%)=K$+""
1530	  GOSUB 2100 &
	\ IF FNU%(2%,T3$)=0% THEN &
		  PRINT  &
	  ELSE &
		  PRINT "Unable to change this record.  Error";FNS%;"has occurred."
1540	  GOTO 1520 IF FNN%(2%)=0%
1590	  PRINT USING "TOTAL ENTERED ###,###,###.##", T &
	\ V1$=SYS(CHR$(2%)) &
	\ GOTO 1020
1595	  PRINT "VOID - ENTER ";S$(11%+I%);" AGAIN: "; &
	\ RESUME 1597
1596	  ON ERROR GOTO 1595
1597	  GOSUB 2200 &
	\ IF K$="END" THEN &
		  RETURN &
	  ELSE &
		  C1(I%)=VAL(K$) &
		\ ON ERROR GOTO 0 &
		\ C1(I%)=C1(I%)/100. &
		\ PRINT USING "###,###.##", C1(I%) &
		\ T=T+C1(I%) &
		\ RETURN
2000	  !
2001	  PRINT "ENTER '-' TO START OVER ON A CUSTOMER"
2010	  PRINT  &
	\ PRINT "NEW CUSTOMER # "; &
	\ GOSUB 2200 &
	\ GOTO 9000 IF K$="" OR K$="." &
	\ C$(1%)=K$ &
	\ IF FNG%(2%,C$(1%))=0% THEN  &
		  GOSUB 2300 &
		\ PRINT  &
		\ PRINT CVT$$(C$(2%),128%);" ALREADY HAS THAT NUMBER." &
		\ GOTO 2010
2020	  FOR I%=2% TO 4% &
		\ PRINT S$(I%);"? "; &
		\ PRINT "(TYPE <RETURN> FOR YOUR CITY>.)"; IF I%=4% &
		\ GOSUB 2200 &
		\ GOTO 2050 IF K$="-" &
		\ C$(I%)=K$ &
	\ NEXT I% &
	\ IF C$(4%)="" THEN  &
		  C$(5%)=SPACE$(30%) &
		\ C$(4%)="IDAHO FALLS, IDAHO 83401" &
		\ C$(4%)=A0$(4%) IF CVT$$(A0$(4%),-1%)<>"" &
		\ GOTO 2035
2030	  PRINT S$(5%);"? "; &
	\ GOSUB 2200 &
	\ GOTO 2050 IF K$="-" &
	\ C$(5%)=K$+""
2035	  FOR I%=6% TO 7%
2037		  PRINT S$(I%);"? "; &
		\ GOSUB 2200 &
		\ PRINT "ENTRY REQUIRED!" IF K$="" AND B%=0% &
		\ GOTO 2037 IF K$="" AND B%=0% &
		\ GOTO 2050 IF K$="-" &
		\ V%=FNG%(6%,SPACE$(8%-LEN(K$))+K$ IF B%=0% &
		\ PRINT "NONEXISTENT G/L #." IF B%=0% AND V%<>0% &
		\ GOTO 2037 IF B%=0% AND V%<>0% &
		\ C$(I%)=K$ &
	\ NEXT I% &
	\ PRINT S$(8%);"? "; &
	\ GOSUB 2200 &
	\ GOTO 2050 IF K$="-" &
	\ C$(8%)=K$ &
	\ FOR I%=1% TO 3% &
		\ PRINT S$(I%+9%);"? "; &
		\ GOSUB 2200 &
		\ GOTO 2050 IF K$="-" &
		\ C(I%)=VAL(K$) &
	\ NEXT I% &
	\ C1(I%)=0. FOR I%=1% TO 8% &
	\ C$(9%)=""
2038	  PRINT S$(21%);"? "; &
	\ GOSUB 2200 &
	\ GOTO 2010 IF K$="-" &
	\ C1(9%)=VAL(K$) &
	\ C1(10%)=0.
2040	  GOSUB 2100 &
	\ V%=FNA%(2%,T3$) &
	\ GOTO 2010
2050	  PRINT "ALL ENTRIES FOR THIS CUSTOMER ABORTED." &
	\ GOTO 2010
2100	  !
2110	  LSET U$(I%)=C$(I%) FOR I%=1% TO 8% &
	\ LSET U$(I%+8%)=CVTF$(C(I%)) FOR I%=1% TO 3% &
	\ LSET U$(I%+11%)=CVTF$(C1(I%)) FOR I%=1% TO 8% &
	\ LSET U$(20%)=CVT%$(FND6%(FND7$(C$(9%)))) &
	\ LSET U$(21%)=FNN4$(C1(9%)) &
	\ LSET U$(22%)=CVTF$(C1(10%)) &
	\ RETURN
2200	  !
2210	  INPUT LINE #11%, K$ &
	\ K$=CVT$$(K$,140%)
2220	  RETURN
2300	  !
2310	  LSET T3$=FNL$ &
	\ C$(I%)=U$(I%)+"" FOR I%=1% TO 8% &
	\ C(I%)=CVT$F(U$(I%+8%)) FOR I%=1% TO 3% &
	\ C1(I%)=CVT$F(U$(I%+11%)) FOR I%=1% TO 8% &
	\ C$(9%)=FND6$(CVT$%(U$(20%))) &
	\ C1(9.)=FNN4(U$(21%)) &
	\ C1(10%)=CVT$F(U$(22%)) &
	\ RETURN
2400	  !
2410	  IF X1%>55% THEN  &
		  PRINT  FOR X1%=X1% TO 71% &
		\ X1%=6%
2412	  X1%=X1%+1% &
	\ PRINT TAB(10%);C$(1%);"  ";C$(2%);"  ";" ";CVT$$(C$(8%),128%); &
	\ IF B$="Y" THEN  &
		  C=C1(1%)+C1(2%)+C1(3%)+C1(4%)+C1(5%)+C1(7%) &
		\ PRINT TAB(65%); &
		\ PRINT USING "###,###.##", C;
2415	  PRINT  &
	\ T=T+C &
	\ RETURN IF V1$="S"
2420	  PRINT TAB(18%);CVT$$(C$(3%),128%) &
	\ PRINT TAB(18%);C$(4%);CVT$$(C$(6%),-1%);" - ";CVT$$(C$(7%),-1%) &
	\ PRINT TAB(18%);CVT$$(C$(5%),128%) UNLESS CVT$$(C$(5%),-1%)="" &
	\ X1%=X1%+1% UNLESS CVT$$(C$(5%),-1%)="" &
	\ PRINT USING "                  ###.##%, ###.##%, ###.##%", C(1%),C(2%),C(3%) &
	\ PRINT  &
	\ X1%=X1%+4% &
	\ RETURN
3000	  !
3010	  PRINT  &
	\ PRINT "DELETE - CUSTOMER # "; &
	\ GOSUB 2200 &
	\ F%=0% &
	\ IF K$="" THEN &
		  GOTO 1020 &
	  ELSE &
		  IF LEFT(K$,1%)="*" THEN  &
			  F%=-1% &
			\ K$=RIGHT(K$,2%)
3020	  GOTO 3100 IF FNG%(2%,K$) &
	\ GOSUB 2300 &
	\ IF F%=0% THEN  &
		  IF FNG%(4%,C$(1%))=0% OR FNG%(8%,C$(1%))=0% THEN  &
			  PRINT "CUSTOMER HAS CURRENT TRANSACTIONS!  DELETION NOT ALLOWED." &
			\ GOTO 3010
3030	  GOSUB 2400 &
	\ INPUT "CONFIRM (Y/N) ";K$ &
	\ V%=FND%(2%,"") IF LEFT(K$,1%)="Y" &
	\ GOTO 3010
3100	  PRINT "CUSTOMER NOT FOUND." &
	\ GOTO 3010
4000	  !
4005	  S$="CU NA AD CI ZI RE SA PH LP DI TA IN 00 30 60 90 PA YT SE YS CL BA "
4010	  PRINT  &
	\ PRINT "CUSTOMER # "; &
	\ GOSUB 2200 &
	\ F%=0% &
	\ GOTO 1020 IF K$="" &
	\ IF K$="+" OR K$="-" THEN  &
		  V%=FNN%(-2%) IF K$="-" &
		\ V%=FNN%(2%) IF K$="+" &
		\ IF V%=0% THEN &
			  GOTO 4020 &
		  ELSE &
			  PRINT "END OF FILE." &
			\ GOTO 4010
4015	  F%=-1% IF LEFT(K$,1%)="*" &
	\ K$=RIGHT(K$,2%) IF F%=-1% &
	\ K$=K$+SPACE$(6%-LEN(K$)) &
	\ IF FNG%(2%,K$)<>0% THEN  &
		  PRINT "CUSTOMER # ";CVT$$(K$,128%);" NOT FOUND." &
		\ GOTO 4010
4020	  GOSUB 2300
4030	  PRINT  &
	\ INPUT "ITEM TO CHANGE ( ? FOR HELP ) ";K$ &
	\ K$=LEFT(K$,2%) &
	\ GOTO 4010 IF K$="" &
	\ GOTO 4900 IF K$="?" &
	\ I1%=INSTR(1%,S$,K$) &
	\ I1%=I1%/3%+1% UNLESS I1%=0% &
	\ GOTO 4300 IF I1%>12% &
	\ GOTO 4200 IF I1%>9% &
	\ GOTO 4100 IF I1%>0% &
	\ PRINT  &
	\ PRINT "TYPE '?' FOR A LIST OF CHOICES." &
	\ GOTO 4030
4100	  !
4110	  PRINT S$(I1%);": ";CVT$$(C$(I1%),128%) &
	\ PRINT "REPLACE WITH ? "; &
	\ GOSUB 2200 &
	\ IF K$="" THEN  &
		  PRINT "NO CHANGE MADE." &
		\ GOTO 4030
4120	  K$="" IF K$="." &
	\ IF I1%=6% OR I1%=7% THEN  &
		  V%=FNG%(6%,SPACE$(8%-LEN(K$))+K$ IF B%=0% &
		\ IF V% AND B%=0% THEN  &
			  PRINT "Invalid account code.  Please re-enter." &
			\ GOTO 4110
4130	  IF I1%=1% THEN  &
		  K$=K$+SPACE$(6%-LEN(K$)) &
		\ V%=FNG%(2%,K$) &
		\ PRINT "Customer code of ";K$;" is in use.  Try another." IF V%=0% &
		\ GOTO 4010 IF V%=0% &
		\ V%=FNG%(4%,C$(1%)) &
		\ V1%=FNG%(8%,C$(1%)) &
		\ V%,V1%=1% IF F%<>0% &
		\ PRINT "Customer code in use in transaction file.  Change denied. IF V%=0% OR V1%=0% &
		\ GOTO 4010 IF V%=0% OR V1%=0% &
		\ GOTO 4800 IF FNG%(2%,C$(I1%)) &
		\ C$(I1%)=K$ &
		\ GOSUB 2100 &
		\ GOTO 4800 IF FNU%(-2%,T3$) &
		\ PRINT "CHANGED." &
		\ GOTO 4010
4140	  C$(I1%)=K$ &
	\ GOSUB 2100 &
	\ GOTO 4800 IF FNU%(2%,T3$) &
	\ PRINT "CHANGED." &
	\ GOTO 4030
4200	  !
4210	  PRINT S$(I1%);": "; &
	\ PRINT USING STRING$(LOG10(ABS(C(I1%-9%))+1%)+2%,ASCII("#"))+".##", C(I1%-9%) &
	\ PRINT "REPLACE WITH ? "; &
	\ GOSUB 2200 &
	\ IF K$="" THEN  &
		  PRINT "NO CHANGE MADE." &
		\ GOTO 4030
4220	  ON ERROR GOTO 4240 &
	\ C(I1%-9%)=VAL(K$) &
	\ ON ERROR GOTO 0 &
	\ GOSUB 2100 &
	\ GOTO 4800 IF FNU%(2%,T3$) &
	\ PRINT "CHANGED." &
	\ GOTO 4030
4240	  PRINT "ILLEGAL NUMBER ENTERED - NO CHANGE MADE." &
	\ RESUME 4030
4300	  !
4310	  PRINT S$(I1%);": "; &
	\ PRINT USING STRING$(LOG10(ABS(C1(I1%-12%))+1%)+2%,ASCII("#"))+".##", C1(I1%-12%) &
	\ PRINT "REPLACE WITH ? "; &
	\ GOSUB 2200 &
	\ IF K$="" THEN  &
		  PRINT "NO CHANGE MADE." &
		\ GOTO 4030
4320	  ON ERROR GOTO 4330 &
	\ C1(I1%-12%)=VAL(K$) &
	\ ON ERROR GOTO 0 &
	\ GOSUB 2100 &
	\ GOTO 4800 IF FNU%(2%,T3$) &
	\ PRINT "CHANGED." &
	\ GOTO 4030
4330	  PRINT "ILLEGAL NUMBER ENTERED - NO CHANGE MADE." &
	\ RESUME 4030
4800	  PRINT "AN ERROR HAS OCCURRED.  PLEASE ZIP OR CALL CMC." &
	\ V$=SYS(CHR$(5%))
4900	  !
4910	  PRINT "ENTER    TO CHANGE" &
	\ PRINT 
4920	  PRINT MID(S$,I1%*3%-2%,2%);"        ";S$(I1%) FOR I1%=1% TO 22% &
	\ PRINT  &
	\ GOTO 4030
5000	  !
5010	  PRINT  &
	\ PRINT "EXAMINE FROM - CUSTOMER # "; &
	\ GOSUB 2200 &
	\ IF K$="" OR K$="." THEN  &
		  GOTO 1020
5020	  N9$=K$ &
	\ PRINT "          TO - CUSTOMER # "; &
	\ GOSUB 2200 &
	\ V%=FNG%(2%,N9$) &
	\ GOSUB 2300 &
	\ PRINT 
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
5500	  CHAIN "[1,3]CUSREV" 0%
6000	  !
6010	  PRINT  &
	\ PRINT "FIND - CUSTOMER # "; &
	\ GOSUB 2200 &
	\ GOTO 1020 IF K$="" OR K$="." &
	\ PRINT  &
	\ GOTO 6020 IF LEFT(K$,1%)="+" &
	\ GOTO 6030 IF LEFT(K$,1%)="-" &
	\ GOTO 6010 IF FNG%(2%,K$) &
	\ GOTO 6040
6020	  V%=FNN%(2%) &
	\ GOTO 6040
6030	  V%=FNN%(-2%)
6040	  GOSUB 2300 &
	\ GOSUB 2400
6050	  PRINT TAB(18%); &
	\ PRINT USING "YTD SERVICE CHARGE $$##,###.##-", C1(6%) &
	\ PRINT USING "                  YTD SALES          $$##,###.##-", C1(8%) &
	\ PRINT TAB(18%);"LAST PAYMENT DATE     ";C$(9%) &
	\ PRINT 
6060	  PRINT "     CURRENT      30 DAY      60 DAY      90 DAY     OVER 90"+"   SRVC CHRG" &
	\ PRINT USING "#,###,###.##", C1(I%); UNLESS I%=6% FOR I%=1% TO 7% &
	\ PRINT  &
	\ GOTO 6010
7000	  !
7010	  T=0. &
	\ GOTO 1020 IF FNG%(2%,"") &
	\ X1%=70%
7020	  PRINT  &
	\ INPUT "FORM (S/L) ";V1$ &
	\ V1$=LEFT(V1$,1%) &
	\ IF V1$<>"S" AND V1$<>"L" THEN  &
		  PRINT  &
		\ PRINT "ENTER 'S' FOR SHORT FORM WITH JUST NAMES AND PHONE NUMBERS." &
		\ PRINT "ENTER 'L' FOR COMPLETE PRINTOUT" &
		\ GOTO 7020
7040	  PRINT  &
	\ INPUT #11%, "PRINT BALANCES (Y OR N) ";B$ &
	\ INPUT #11%, "SET PAGE. . . ";K$
7055	  PRINT SPACE$(35%-LEN(A0$(1%))/2%);A0$(1%) &
	\ PRINT SPACE$(22%); &
	\ PRINT "CUSTOMER ADDRESS REGISTER" &
	\ PRINT SPACE$(30%);DATE$(0%)
7060	  GOSUB 2300 &
	\ GOSUB 2400 &
	\ IF FNN%(2%) THEN &
		  GOTO 7070 &
	  ELSE &
		  GOTO 7060
7070	  PRINT USING "TOTAL BALANCE = ###,###,###.##", T &
	\ PRINT  &
	\ PRINT  &
	\ GOTO 1020
8000	  !
8005	  INPUT #11%, "LABELS OR ENVELOPES ? ";L$ &
	\ GOTO 8100 IF L$<>"E" AND L$<>"L" &
	\ GOTO 8200 IF LEFT(L$,1%)="E"
8006	  INPUT #11%, "PRINT CUST. AND TELEPHONE #'S (Y/N) ? ";F$ &
	\ F$=LEFT(F$,1%) &
	\ GOTO 8006 IF F$<>"Y" AND F$<>"N"
8010	  X1%=3% &
	\ GOTO 1020 IF FNG%(2%,"") &
	\ PRINT "SET PAGE. . ." &
	\ E$="" &
	\ V$=SYS(CHR$(3%)) &
	\ UNTIL E$="P" &
		\ PRINT "#";CHR$(13%); &
		\ INPUT LINE #11%, E$ &
		\ E$=CVT$$(E$,-1%) &
	\ NEXT &
	\ V$=SYS(CHR$(2%))
8020	  GOSUB 2300 &
	\ PRINT CVT$$(SPACE$(X1%)+C$(I%),128%) FOR I%=2% TO 5%
8030	  PRINT  UNLESS F$="Y" &
	\ PRINT "( ";CVT$$(C$(1%),128%);"  Phone ";CVT$$(C$(8%),128%);" ) " IF F$="Y" &
	\ PRINT  &
	\ GOTO 1020 IF FNN%(2%) &
	\ GOTO 8020
8100	  PRINT "ENTER EITHER AN 'L' OR AN 'E'" &
	\ PRINT  &
	\ GOTO 8005
8200	  !
8210	  PRINT "POSITION ENVELOPE AND ENTER CUSTOMER NUMBER." &
	\ PRINT "ENTER A PERIOD ('.') TO END" &
	\ PRINT 
8220	  V$=SYS(CHR$(3%))
8230	  PRINT TAB(30%); &
	\ PRINT STRING$(2%,7%); &
	\ INPUT LINE #11%, C$ &
	\ C$=CVT$$(C$,-1%) &
	\ GOTO 8250 IF C$="." &
	\ GOTO 8230 IF C$="" OR FNG%(2%,C$+SPACE$(6%-LEN(C$)))
8240	  GOSUB 2300 &
	\ PRINT CVT$$(TAB(30%)+C$(I%),128% FOR I%=2% TO 5% &
	\ PRINT  &
	\ PRINT  &
	\ GOTO 8230
8250	  PRINT  &
	\ V$=SYS(CHR$(2%)) &
	\ GOTO 1020
8500	  !
8510	  IF FNG%(2%,"") THEN  &
		  PRINT "The customer file is empty." &
		\ GOTO 1020
8520	  INPUT "Date for Printout ";D$ &
	\ INPUT "Set page ";K$ &
	\ L1%=1% &
	\ T6=0. &
	\ L%=0% &
	\ GOSUB 8650
8530	  GOSUB 2300 &
	\ T6=T6+C1(6%) &
	\ IF L%>55% THEN  &
		  GOSUB 8600 &
		\ GOSUB 8650
8540	  PRINT USING U$, C$(1%),C$(2%),C1(6%) &
	\ L%=L%+1% &
	\ GOTO 8550 IF FNN%(2%) &
	\ GOTO 8530
8550	  PRINT  &
	\ PRINT " -- Total -->";TAB(57%); &
	\ PRINT USING "#,###,###.##", T6 &
	\ L%=L%+2% &
	\ GOSUB 8600 &
	\ PRINT  &
	\ PRINT  &
	\ GOTO 1020
8600	  !
8620	  PRINT STRING$(61%-L%,10%) &
	\ PRINT TAB(35%);"- Page";L1%;"-" &
	\ L1%=L1%+1% &
	\ L%=0% &
	\ PRINT  FOR X%=1% TO 3% &
	\ RETURN
8650	  PRINT  FOR X%=1% TO 3% &
	\ PRINT TAB(25%);"** Interest Report For ";D$;" **" &
	\ PRINT  &
	\ PRINT "Code     Name                          "+"            Amount of Interest" &
	\ PRINT STRING$(79%,45%) &
	\ L%=L%+7% &
	\ RETURN
8700	  INPUT "ZERO YTD SERVICE AND SALES (Y OR N)",K$ &
	\ IF K$<>"Y" THEN  &
		  PRINT "OPERATION ABORTED" &
		\ GOTO 1020
8710	  IF FNG%(2%,"") THEN  &
		  PRINT "The customer file is empty." &
		\ GOTO 1020
8720	  GOSUB 2300 &
	\ C1(6%),C1(8%)=0. &
	\ GOSUB 2100 &
	\ GOTO 4800 IF FNU%(2%,T3$) &
	\ PRINT  IF POS(0%)>75% &
	\ PRINT "!"; &
	\ GOTO 8730 IF FNN%(2%) &
	\ GOTO 8720
8730	  PRINT  IF POS(0%)>70% &
	\ PRINT "COMPLETE !!!" &
	\ GOTO 1020
9000	  V%=FNX%("[1,3]CUSTOM.BAC",10%,"")
10000	  !
10010	  V%=FNX%("",0%,"")
14030	  DEF FND8%(D8) &
	\ FND8%=D8 &
	\ FNEND
14035	  !
14040	  DEF FND6%(D9$) &
	\ FND6%=VAL(MID(D9$,4%,2%))+VAL(LEFT(D9$,2%))*32%+FND8%(VAL(RIGHT(D9$,7%)))*512% &
	\ FNEND
14050	  DEF FND6$(D9%) &
	\ FND6$=RIGHT(NUM1$((D9% AND 15%*32%)/32%+100%),2%)+"."+RIGHT(NUM1$((D9% AND 31%)+100%),2%)+"."+RIGHT(NUM1$((SWAP%(D9%) AND 254%)/2%+100%),2%) &
	\ FNEND
14250	  !
14260	  DEF FND7$(D7$) &
	\ D7$=D7$+"."+RIGHT(DATE$(0%),8%) IF LEN(D7$)<6% &
	\ D7$="0"+D7$ IF INSTR(1%,D7$,".")=2% &
	\ D7$=LEFT(D7$,3%)+"0"+RIGHT(D7$,4%) IF INSTR(4%,D7$,".")=5% &
	\ FND7$=D7$ &
	\ FNEND
14400	  DEF FNN4(Y$) &
	\ FNN4=CVT$%(LEFT(Y$,2%))*256.+ASCII(MID(Y$,3%,1%))+ASCII(RIGHT(Y$,4%))/100. &
	\ FNEND
14410	  DEF FNN4$(Y) &
	\ FNN4$=CVT%$(INT(Y/256.))+CHR$(Y-INT(Y/256.)*256.)+CHR$((Y-INT(Y))*100.+0.5) &
	\ FNEND
19000	  !
32767	  END
