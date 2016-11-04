1	  EXTEND
10	! &
	!  CONERC - PRINT, DELETE, CHANGE PORTION OF CONE81 &
	!         - CHAINS TO AND FROM [1,8]CONE81 OR THE NONSTANDARD CONE81 &
	!         - ON ANY OTHER ACCOUNT (AS LONG AS THE FNX$ IS SET UP RIGHT). &
	! &

15	  EXTEND &

16	  Y$=SYS(CHR$(12%)) &
	\ CHANGE Y$ TO Y% &
	\ SYS.LOC$=CHR$(Y%(23%))+CHR$(Y%(24%))+NUM1$(Y%(25%))+":"+ &
		  "["+NUM1$(Y%(6%))+","+NUM1$(Y%(5%))+"]"+ &
		  RAD$(Y%(7%)+SWAP%(Y%(8%)))+RAD$(Y%(9%)+SWAP%(Y%(10%))) &

20 DIM  M1$(24%),M$(24%),M2$(44%),M(21%),D1$(25%),D$(11%),D(14%), &
	D%(1%),X(14%),S(10%),S1(10%),S1$(10%),T(5%),F$(10), &
	P1$(29%),P$(37%),P(17%),P%(18%) &

30 I%=INSTR(1%,FNX$,"*") : Z$=LEFT(FNX$,I%-1%) IF I% : Z$=FNX$ IF I%=0% &
	: Z2$=RIGHT(FNX$,I%+1%) IF I% : ON ERROR GOTO 106 &
	: OPEN "SS0:UNIQUE.FIL" FOR INPUT AS FILE 1% : DIM #1%,A0$(255%)=64% &
	: CONAME$=CVT$$(A0$(1%),140%) &
	: A0%=INSTR(1%,A0$(15%),"TIPS") : CLOSE #1% : GOTO 110 &

106 A0%=0% : RESUME 110 &

110 ON ERROR GOTO : IF FNO%(2%,"MSTRFL.DAT","","") THEN &
	PRINT "ERROR";FNS%;"IN OPENING MASTER.  ABORT." : GOTO 10000 &

130 INPUT "PAYROLL DATE (MM.DD.YY) ";Z$ IF Z$="" : GOTO 10000 IF Z$="" &
	: Z$=FND7$(Z$) : Z1$=LEFT(Z$,2%)+RIGHT(Z$,4%)+"T" : IF FND7%(Z$) THEN &
		PRINT Z$;" IS A BAD DATE!  RE-ENTER OR TYPE <RETURN> TO END." &
	: Z$="" : GOTO 130 &

140 IF FNO%(4%,"PR"+Z1$,"","") THEN PRINT &
		"PAYROLL FILE FOR ";Z$;" DOES NOT EXIST!" IF FNS%=5% &
	: Z$="" : GOTO 130 IF FNS%=5% &
	: PRINT "ERROR";FNS%;"IN OPENING PAYROLL FILE.  ABORTED." : GOTO 10000 &

150 IF FNO%(6%,"TX"+Z1$,"","") THEN PRINT &
	"THE EARNINGS AND DEDUCTIONS FILE FOR ";Z$;" DOES NOT YET EXIST!" &
		IF FNS%=5% &
	: IF FNS%<>5% THEN PRINT &
		"ERROR";FNS%;"IN OPENING EARNINGS FILE.  ABORT." : GOTO 10000 &

210 OPEN "KB:" AS FILE #1%, RECORDSIZE 512%+128% &
	: OPEN "KB:" FOR INPUT AS FILE #12% &
	: FIELD #1%, 6% AS M1$(1%), 30% AS M1$(2%), 30% AS M1$(3%), &
		    30% AS M1$(4%), 30% AS M1$(5%), 11% AS M1$(6%), &
		     8% AS M1$(7%),  1% AS M1$(8%),  2% AS M1$(9%), &
		    12% AS M1$(10%), 2% AS M1$(11%) &
	: FIELD #1%,152%+I%*10% AS E$,8% AS M2$(I%),2% AS M1$(I%+11%) &
		FOR I%=1% TO 9% &
	: FIELD #1%,252% AS E$,8% AS M1$(21%),8% AS M1$(22%) &
	: FIELD #1%,188%+I%*8% AS E$,8% AS M2$(I%) FOR I%=10% TO 36% &
	: FIELD #1%,484% AS E$,2% AS M1$(23%) &
	: FIELD #1%,486%+I%*2% AS E$,2% AS M2$(I%+37%) FOR I%=0% TO 7% &
							! MSTRFL.DAT &

240 FIELD #1%,512% AS E$,      6% AS D1$(1%), 1% AS D1$(2%), 2% AS D1$(3%), &
		2% AS D1$(4%), 2% AS D1$(5%), 2% AS D1$(6%), 2% AS D1$(7%), &
		2% AS D1$(8%), 2% AS D1$(9%), 8% AS D1$(10%),2% AS D1$(11%), &
		8% AS D1$(12%),8% AS D1$(13%),8% AS D1$(14%),8% AS D1$(15%), &
		8% AS D1$(16%),8% AS D1$(17%),8% AS D1$(18%),8% AS D1$(19%), &
		8% AS D1$(20%),8% AS D1$(21%),8% AS D1$(22%),3% AS D1$(23%), &
		3% AS D1$(24%),1% AS D1$(25%) &
	: FIELD #1%,512% AS T2$,128% AS T6$	! ERNDED.DAT &

250 H1$="UNTAXED EARNINGS" : H2$="NOTAX" : H3$="VACATION FUND" &
	: IF A0% THEN H1$="DECLARED TIPS   " : H2$="TIPS " &
	: H3$="MEAL DEDUCTIONS" &

260 READ F$(I%) FOR I%=1% TO 10% &
	: DATA "Federal Tax","FICA Tax","State Tax","Vacation","Insurance", &
		"Other 1","Other 2","Other 3","Other 4","Non-taxed earnings" &

1000	! &
	!  CONTROL &
	! &

1020 PRINT : INPUT "SELECTION";E$ : E$=LEFT(E$,3%) : GOTO 1030 IF E$="" &
	: GOTO 10020 IF E$="COM" : GOTO 3000 IF E$="DEL" OR E$="EXA" &
	: GOTO 6000 IF E$="CHA" : GOTO 6100 IF E$="CHE" : GOTO 7000 IF E$="PRI" &
	: GOTO 8000 IF E$="REG" : GOTO 10000 IF E$="END" : GOTO 1020 &

1030 PRINT : PRINT "OPTIONS:" &
	: PRINT "     COMPUTE     DEDUCTIONS AND NET PAY" &
	: PRINT "     CHANGE      WITHHOLDINGS AND NON-TAXED EARNINGS" &
	: PRINT "     CHECK       CHANGE CHECK NUMBERS AND DATES" &
	: PRINT "     DELETE      A RECORD" &
	: PRINT "     EXAMINE     RECORDS" &
	: PRINT "     PRINT       PAYROLL" &
	: PRINT "     REGISTER    PAYROLL REGISTER" &
	: PRINT "     END         PROGRAM" &
	: GOTO 1020 &

2000	! &
	!  INPUT EMPLOYEE # AND FORMAT FOR 6 CHR RSET &
	! &

2010 PRINT : PRINT "EMPLOYEE # "; : INPUT LINE #12%,B$ : B$=CVT$$(B$,132%) &
	: B$=B$+SPACE$(6%-LEN(B$)) IF B$<>"" : RETURN &

2300	! &
	!  SEPARATE MASTER &
	! &

2310 LSET T2$=FNL$ &
	: M$(I%)=M1$(I%)+"" FOR I%=1% TO 23% &
	: M(I%)=CVT$F(M2$(I%)) FOR I%=1% TO 21% : RETURN &

2400	  EXTEND &

2410	  F%=FNL% &
	\ FIELD #5%, F% AS E$,6% AS P1$(1%),6% AS P1$(2%),1% AS P1$(3%),1% &
		 AS P1$(4%),2% AS P1$(5%),2% AS P1$(6%),2% AS P1$(7%),7% AS &
		P1$(8%),2% AS P1$(9%) &
	\ FIELD #5%, F%+19%+I% AS E$,1% AS P1$(I%) FOR I%=10% TO 24% &
	\ FIELD #5%, F%+44% AS E$,8% AS P1$(25%),3% AS P1$(26%),3% AS P1$(27%), &
		3% AS P1$(28%),3% AS P1$(29%) &
	\ P$(I%)=P1$(I%)+"" FOR I%=1% TO 8% &
	\ P%(I%)=ASCII(P1$(I%+9%)) FOR I%=1% TO 15% &
	\ P(1%)=CVT$F(P1$(25%)) &
	\ P(I%)=FNN3(P1$(I%+24%)) FOR I%=2% TO 5% &
	\ P%(0%)=CVT$%(P1$(9%)) &
	\ P$(9%)=FND9$(P%(0%)) &
	\ IF P%(7%)=255% THEN &
		  P(6%)=FNN3(P1$(13%)+P1$(14%)+P1$(15%)) &
		\ P(7%)=FNN3(P1$(20%)+P1$(21%)+P1$(22%)) &
		\ P%(I%)=0% FOR I%=1% TO 14% &
		\ RETURN &

2420	  P(6%),P(7%)=0. &
	\ P(6%)=P(6%)+P%(I%) FOR I%=1% TO 7% &
	\ P(7%)=P(7%)+P%(I%) FOR I%=8% TO 14% &
	\ P(I%)=P(I%)/10. FOR I%=6% TO 7% &
	\ RETURN &

2500	! &
	!  SEPARATE ERNDED &
	! &

2510 LSET T6$=FNL$ &
	: D$(I%)=D1$(I%)+"" FOR I%=1% TO 10% &
	: D$(11%)=FND9$(CVT$%(D1$(11%))) &
	: D(I%)=CVT$F(D1$(I%+11%)) FOR I%=1% TO 11% &
	: D(I%+11%)=FNN3(D1$(I%+22%)) FOR I%=1% TO 2% &
	: D%(1%)=ASCII(D1$(25%)) &
	: D(14%)=D(1%)-D(12%) : D(14%)=D(14%)+D(2%) UNLESS A0% &
	: D(14%)=D(14%)-D(I%) FOR I%=3% TO 11% : RETURN &

2600	! &
	!  PREPARE ERNDED &
	! &

2610 LSET D1$(I%)=D$(I%) FOR I%=1% TO 10% &
	: LSET D1$(11%)=CVT%$(FND9%(D$(11%))) &
	: LSET D1$(I%+11%)=CVTF$(D(I%)) FOR I%=1% TO 11% &
	: LSET D1$(I%+22%)=FNN3$(D(I%+11%)) FOR I%=1% TO 2% &
	: LSET D1$(25%)=CHR$(D%(1%)) : RETURN &

3000	! &
	!  DELETE/EXAMINE &
	! &

3005 GOTO 1020 IF FNG%(6%,"") : IF E$="DEL" THEN GOSUB 2510 &
	: IF (D%(1%) AND 3%) THEN PRINT &
		"PAYROLL HAS BEEN UPDATED - DELETION CANNOT BE MADE." &
	: PRINT : GOTO 1020 &

3010 GOSUB 2010 : GOTO 1020 IF B$="" &
	: IF FNG%(6%,B$) THEN PRINT "EMPLOYEE # ";B$;" NOT FOUND." : GOTO 3010 &

3020 GOSUB 2510 : V%=FNG%(2%,D$(1%)) : GOSUB 2310 : GOSUB 7920 : GOSUB 4000 &
	: GOTO 3010 IF E$<>"DEL" : INPUT "CONFIRM DELETION (Y/N) ";E$ &
	: GOTO 3010 UNLESS E$="Y" : GOTO 3010 UNLESS FND%(6%,"") &
	: PRINT "ERROR";FNS%;"IN DELETING RECORD.  ABORT." : GOTO 10000 &

4000	! &
	!  PRINT RECORD &
	! &

4010 D$(0%)="" : D$(0%)="*" IF D%(1%)=1% : D$(0%)="#" IF D%(1%)=2% &
	: D$(0%)="!" IF D%(1%)=3% : PRINT &
	: PRINT USING "\    \!\            \\\\\ ####.## ###.## ####.## "+ &
		"####.## ###.## ###.## ####.##",D$(1%),D$(0%),M$(2%), &
		" "+D$(2%),D$(3%),D(1%),D(2%),D(6%)+D(7%)+D(8%)+D(9%)+D(10%)+ &
		D(11%)+D(12%),D(3%),D(4%),D(5%),D(14%) &
	: PRINT USING "    \\ \    \ \      \",M$(23%),D$(10%),D$(11%) &
	: X1%=X1%+3% : M$(23%)="00" IF M$(23%)="" : FOR I%=1% TO 10% &
	: IF S1$(I%)<>"" THEN GOTO 4040 IF M$(23%)=S1$(I%) : NEXT I% : STOP &

4030 S1$(I%)=M$(23%) &

4040 S(I%)=S(I%)+D(5%): S1(I%)=S1(I%)+D(1%): RETURN &

6000	! &
	!  CHANGE TAX ROUTINE &
	! &

6005 GOTO 1020 IF FNG%(6%,"") : GOSUB 2510 &
	: IF [(D%(1%) AND 1%) OR (D%(1%) AND 2%)] THEN PRINT &
	"EMPLOYEE HAS BEEN UPDATED.  NO CHANGE CAN BE MADE." : GOTO 1020 &

6010 GOSUB 2010 : GOTO 1020 IF B$="" &
	: IF FNG%(6%,B$) THEN PRINT "EMPLOYEE # ";B$;" NOT FOUND." : GOTO 6010 &

6020 GOSUB 2510 : V%=FNG%(2%,D$(1%)) : GOSUB 2310 : GOSUB 7920 : GOSUB 4000 &
	: V$="" &

6030 PRINT : INPUT "Item to change (? for help) ";C$ &
	: GOTO 6080 IF C$="" AND V$="1" : GOTO 6010 IF C$="" &
	: I%=INSTR(1%,"FE FI ST VA IN O1 O2 O3 O4 NO",C$) &
	: IF I%=0% THEN GOSUB 6800 : GOTO 6030 &

6040 I%=I%/3%+1% : I%=0% IF I%=10% : PRINT F$(I%);" of"; &
	: PRINT USING "$$###,###.##-",D(I%+2%); : PRINT TAB(30%); &
	: INPUT "New Amount ";K$ : K$="" IF K$="-" : V$="1" IF K$<>"" &
	: D(I%+2%)=VAL(K$) UNLESS K$="" : GOTO 6030 &

6080 GOSUB 2600 : STOP IF FNU%(6%,T6$) : GOTO 6010 &

6100	! &
	!  CHANGE CHECK #'S &
	! &

6110 GOTO 1020 IF FNG%(6%,"") : GOSUB 2510 &
	: IF [(D%(1%) AND 1%) OR (D%(1%) AND 2%)] THEN PRINT &
	"EMPLOYEE HAS BEEN UPDATED.  NO CHANGE CAN BE MADE." : GOTO 1020 &

6115 INPUT "Change check dates also (Y or N) ";K$ : K$=LEFT(K$,1%) &
	: IF K$="Y" THEN INPUT "Change to (MM.DD.YY) ";D$ : D$=FND7$(D$) &

6120 INPUT 'START WITH EMP # (ENTER "RETURN" FOR BEGINNING)';B$ &
	: GOTO 6120 IF FNG%(6%,B$) &
	: PRINT 'ENTER "END" AT ANY LINE TO END CHANGE OPTION.' &
	: PRINT 'ENTER "RETURN" TO MAKE NO CHANGE BUT ADVANCE ONE.' &

6130 GOSUB 2510 : STOP IF FNG%(2%,D$(1%)) : GOSUB 2310 &
	: PRINT USING "\    \ \                             \ \      \", &
	D$(1%),M$(2%),D$(10%); &

6140 INPUT "NEW # ";B$ : GOTO 1020 IF B$="END" : IF B$<>"" THEN D$(10%)=B$ &
	: D$(11%)=D$ IF K$="Y" : GOSUB 2600 : STOP IF FNU%(6%,T6$) &

6150 GOTO 6130 UNLESS FNN%(6%) : GOTO 1020 &

6800	! &
	! PRINT HELP MESSAGE &
	! &

6810 PRINT : PRINT "Choices are:" &
	: PRINT "  FE   for Federal Tax" &
	: PRINT "  FI   for FICA Tax" &
	: PRINT "  ST   for State Tax" &
	: PRINT "  VA   for Vacation" &
	: PRINT "  IN   for Insurance" &
	: PRINT "  NO   for Non-taxed earnings" &
	: PRINT "  O1,O2,O3,O4  for Other 1,2,3 and 4" : RETURN &

7000	! &
	!  PRINT &
	! &

7020 GOTO 1020 IF FNG%(6%,"") : X%=0% : INPUT "SET PAGE ";E$ : GOSUB 7900 &
	: FOR I%=1% TO 10% : S(I%),S1(I%)=0. : S1$(I%)="" : NEXT I% &
	: X(I%)=0. FOR I%=1% TO 14% &

7030 GOSUB 2510 : IF X1%>59% THEN PRINT STRING$(66%-X1%,10%); : GOSUB 7900 &

7040 V%=FNG%(2%,D$(1%)) : GOSUB 2310 : GOSUB 4000 : X%=X%+1% &
	: X(I%)=X(I%)+D(I%) FOR I%=1% TO 14% : GOTO 7030 UNLESS FNN%(6%) &
	: B$="\"+SPACE$(14%)+"\  #,###,###.##" : PRINT STRING$(66%-X1%,10%); &
	: GOSUB 7900 : T(0%)=X(1%)-X(12%) : T(0%)=T(0%)+X(2%) UNLESS A0% &
	: T(0%)=T(0%)-X(I%) FOR I%=3% TO 11% : PRINT &
	: PRINT USING "TOTAL EMPLOYEES = ####",X% : PRINT "REGISTER TOTALS :" &
	: PRINT USING B$,"TAXABLE EARNINGS",X(1%) &
	: PRINT USING B$,H1$,X(2%) &
	: PRINT USING B$,H3$,X(6%) &
	: PRINT USING B$,"INSURANCE FUND",X(7%) &
	: PRINT USING B$,"OTHER #1 FUND",X(8%) &
	: PRINT USING B$,"OTHER #2 FUND",X(9%) &
	: PRINT USING B$,"OTHER #3 FUND",X(10%) &
	: PRINT USING B$,"OTHER #4 FUND",X(11%) &
	: PRINT USING B$,"OTHER HAND DED",X(12%) &
	: PRINT USING B$,"OTHER EARNINGS",X(13%) &
	: PRINT USING B$,"FEDERAL TAX",X(3%) &
	: PRINT USING B$,"FICA",X(4%) &
	: PRINT USING B$,"STATE TAX",X(5%) : PRINT &
	: PRINT USING B$,"NET PAYROLL",T(0%) &
	: PRINT USING B$,"FED TAX DEPOSIT",X(3%)+(X(4%)/.067*.137) &
	: PRINT : PRINT &
	: T(0%),T(1%)=0. : FOR I%=1% TO 10% &
	: IF S1$(I%)<>"" THEN T(0%)=T(0%)+S(I%) : T(1%)=T(1%)+S1(I%) &
	: PRINT USING B$,"TAX BY STATE "+S1$(I%),S(I%); &
	: PRINT USING B$," EARNINGS FOR "+S1$(I%),S1(I%) : NEXT I% : STOP &

7050 PRINT USING B$,"TOTAL STATE TAX",T(0%); &
	: PRINT USING B$," TOTAL EARNINGS ",T(1%) : PRINT &
	: PRINT "*  UPDATED TO PAYROLL MASTER FILE" &
	: PRINT "#  JOURNAL ENTRY MADE TO GENERAL LEDGER" &
	: PRINT "!  BOTH * AND #" : GOTO 1020 &

7900	! &
	!  PRINT HEADER &
	! &

7910 PRINT STRING$(3%,10%); &

7920 PRINT : PRINT "PAYROLL DATE: ";Z$ &
	: PRINT "EMP #  NAME          M #    TAXED  ";H2$; &
		"   OTHER     FED   FICA   STATE  NETCK" &
	: PRINT : X1%=7% : RETURN &

8000	  !!  PRINT PAYROLL REGISTER &

8010	  IF FNG%(6%,"") THEN PRINT "The earnings and deduction file is empty." &
		\ GOTO 1020 &

8020	  INPUT "SET PAGE (WIDE PAPER) AND PRESS 'RETURN' TO PRINT ";K$ &
	\ X1%=0% &
	\ X(I%)=0. FOR I%=1% TO 14% &
	\ GOSUB 8100 &

8030	  IF X1%>59% THEN PRINT STRING$(66%-X1%,10%); &
	\ GOSUB 8100 &

8032	  GOSUB 2500 &
	\ P(6%),P(7%),H,O=0. &
	\ IF FNG%(4%,D$(1%)) THEN 8038 &

8034	  GOSUB 2400 &
	\ IF D$(1%)=P$(1%) THEN H=H+P(6%) &
	\ O=O+P(7%) &
	\ IF FNN%(4%)=0% THEN 8034 &

8038	  IF FNG%(2%,D$(1%))=0% THEN GOSUB 2300 ELSE &
		  M$(2%)="Undefined number" &
		\ M$(6%)="" &

8040	  X1%=X1%+1% &
	\ PRINT USING "\    \ \"+SPACE$(26%)+"\ !\\ \         \ "+ &
		  "###.## ###.## ####.## ###.## ####.## ####.## "+ &
		  "###.## ###.## ####.## \\ \    \", &
		  D$(1%),M$(2%),D$(2%),D$(3%),M$(6%),H,O,D(1%),D(2%), &
		  D(6%)+D(7%)+D(8%)+D(9%)+D(10%)+D(11%)+D(12%),D(3%), &
		  D(4%),D(5%),D(14%),M$(23%),D$(10%) &
	\ X(1%)=X(1%)+H &
	\ X(2%)=X(2%)+O &
	\ X(3%)=X(3%)+D(1%) &
	\ X(4%)=X(4%)+D(2%) &
	\ X(5%)=X(5%)+D(6%)+D(7%)+D(8%)+D(9%)+D(10%)+D(11%)+D(12%) &
	\ X(6%)=X(6%)+D(3%) &
	\ X(7%)=X(7%)+D(4%) &
	\ X(8%)=X(8%)+D(5%) &
	\ X(9%)=X(9%)+D(14%) &

8050	  IF FNN%(6%)=0% THEN 8030 ELSE &
	  PRINT &
	\ PRINT USING SPACE$(40%)+"TOTAL"+ &
		  "    ######.##      ######.##      ######.##"+ &
		  "      ######.##     #######.##",X(1%),X(3%), &
			  X(5%),X(7%),X(9%) &
	\ PRINT USING SPACE$(45%)+ &
		  "            #####.##      ######.##        "+ &
		  "#####.##     ######.##",X(2%),X(4%),X(6%),X(8%) &
	\ PRINT &
	\ GOTO 1020 &

8060	  GOTO 1020 &

8100	  PRINT STRING$(3%,10%); &
	\ PRINT SPACE$(65%-LEN(CONAME$)/2%);CONAME$ &
	\ PRINT SPACE$(58%-LEN(Z$)/2%);"PAYROLL DATE: ";Z$ &
	\ PRINT &
	\ PRINT "EMP$  NAME"+SPACE$(26%)+"M#  SSN         REG HR OVT HR "; &
		  "  TAXED    TIPS   OTHER    FED   FICA  STATE  NET CK"; &
		  " ST CK#" &
	\ PRINT &
	\ X1%=8% &
	\ RETURN &

10000	! &
	!  END &
	! &

10005 E$,Z1$="" : V%=0% &

10010 CLOSE #1%,12% : V%=FNX%(E$,V%,Z1$) &

10020 E$=LEFT(SYS.LOC$,LEN(SYS.LOC$)-3%)+"E"+RIGHT(SYS.LOC$, &
		  LEN(SYS.LOC$)-1%) : V%=10% : Z1$=Z$ &
	: GOTO 10010 &

14010 DEF FND9%(E$) : E$="0"+E$ IF INSTR(1%,E$,".")=2% &
	: E$=LEFT(E$,3%)+"0"+RIGHT(E$,4%) IF INSTR(4%,E$,".")=5% &
	: FND9%=VAL(LEFT(E$,2%))+VAL(MID(E$,4%,2%))*16%+ &
		FND8%(VAL(RIGHT(E$,7%)))*512% : FNEND &

14020 DEF FND9$(V%) &
	: FND9$=RIGHT(NUM1$((V% AND 15%)+100%),2%)+ &
		"."+RIGHT(NUM1$((V% AND 31%*16%)/16%+100%),2%)+ &
		"."+RIGHT(NUM1$(((SWAP%(V%) AND 254%)/2%)+100%),2%) : FNEND &

14030 DEF FND8%(X)=X &

14040 DEF FNZ(X)=INT(X*100.+.51)/100. &

14210 DEF FND7%(E$) : ON ERROR GOTO 14220 &
	: GOTO 14220 IF INSTR(1%,E$,".")<>3% OR INSTR(4%,E$,".")<>6% OR &
		INSTR(7%,E$,".")<>0% OR LEN(E$)<>8% &
	: V%=VAL(LEFT(E$,2%))   : GOTO 14220 IF V%<1% OR V%>12% &
	: V%=VAL(MID(E$,4%,2%)) : GOTO 14220 IF V%<1% OR V%>31% &
	: V%=VAL(RIGHT(E$,7%))  : GOTO 14220 IF V%<0%: FND7%=0% : GOTO 14230 &

14220 FND7%=-1% : RESUME 14230 &

14230 ON ERROR GOTO 0 : FNEND &

14260 DEF FND7$(E$) : E$=E$+"."+RIGHT(DATE$(0%),8%) IF LEN(E$)<6% &
	: E$="0"+E$ IF INSTR(1%,E$,".")=2% &
	: E$=LEFT(E$,3%)+"0"+RIGHT(E$,4%) IF INSTR(4%,E$,".")=5% &
	: FND7$=E$ : FNEND &

14310 DEF FNN3$(X)=CVT%$(INT(X))+CHR$((X-INT(X))*100.+.51) &

14350 DEF FNN3(E$)=CVT$%(LEFT(E$,2%))+ASCII(MID(E$,3%,1%))/100. &

15900	  DIM Y%(32%), Y1%(32%), Y$(32%) &

32767	  END

