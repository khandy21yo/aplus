10	! &
	! PAYROL - CONSTRUCTION GENERAL PAYROLL MODULE 1 &
	! &
	! 07/02/79 - BILL LOVEGROVE FOR GENERAL SYSTEM &
	!
50 DIM M1$(23%),M$(23%),M2$(44%),M(44%),P1$(29%),P$(9%),P%(15%),P(7%), &
	T2(16%),T(8%),T1(8%)
60 D$="MON  TUE  WED  THU  FRI  SAT  SUN  "
100	! &
	! OPEN FILES &
	!
110 OPEN "KB:" AS FILE 1%, RECORDSIZE 512%+64% &
	: OPEN "KB:" FOR INPUT AS FILE 12%
120 IF FNO%(2%,"MSTRFL.DAT","","") &
	THEN	PRINT "MASTER FILE NOT FOUND !!!" : GOTO 10000
130 INPUT "PAYROLL DATE (MM.DD.YY) ";Z$ UNLESS Z$<>"" : GOTO 10000 IF Z$="" &
	: Z$=FND7$(Z$) : Z1$=LEFT(Z$,2%)+RIGHT(Z$,4%)+"T" : IF FND7%(Z$) &
	THEN	PRINT "BAD DATE, PLEASE RE-ENTER." : Z$="" : GOTO 130
140 IF FNO%(4%,"PR"+Z1$,"","") THEN GOSUB 500
150 V1%=FNO%(8%,"JOB.DAT","","")
160 ON ERROR GOTO 170 : OPEN "SS0:UNIQUE.FIL" FOR INPUT AS FILE 11% &
	: DIM #11%,A0$(255%)=64% &
	: A0$=MID(A0$(15%),INSTR(1%,A0$(15%),"=")+1%,1%) : CLOSE 11% : GOTO 180
170 A0$="" : RESUME 180
180 A0$="" IF A0$<"0" OR A0$>"9" : A0%=VAL(A0$) : A0%=1% IF A0%=0%
200	! &
	! FIELDS &
	!
220 FIELD#1%, 6% AS M1$(1%), 30% AS M1$(2%), 30% AS M1$(3%), 30% AS M1$(4%), &
	     30% AS M1$(5%), 11% AS M1$(6%),  8% AS M1$(7%),  1% AS M1$(8%), &
	      2% AS M1$(9%), 12% AS M1$(10%), 2% AS M1$(11%)
221 FIELD#1%,152%+I%*10% AS E$,8% AS M2$(I%), 2% AS M1$(I%+11%) FOR I%=1% TO 9% &
	: FIELD#1%,252% AS E$,8% AS M1$(21%),8% AS M1$(22%) &
	: FIELD#1%,188%+I%*8% AS E$,8% AS M2$(I%) FOR I%=10% TO 36% &
	: FIELD#1%,484% AS E$,2% AS M1$(23%) &
	: FIELD#1%,486%+I%*2% AS E$,2% AS M2$(I%+37%) FOR I%=0% TO 7% &
		! MSTRFL.DAT
230 FIELD#1%, 512% AS E$,     6% AS P1$(1%),6% AS P1$(2%),1% AS P1$(3%), &
		1% AS P1$(4%),2% AS P1$(5%),2% AS P1$(6%),2% AS P1$(7%), &
		7% AS P1$(8%),2% AS P1$(9%) &
	: FIELD#1%, 512%+19%+I% AS E$, 1% AS P1$(I%) FOR I%=10% TO 24% &
	: FIELD#1%, 512%+44% AS E$,8% AS P1$(25%),3% AS P1$(26%), &
		3% AS P1$(27%),3% AS P1$(28%), 3% AS P1$(29%) &
		! PAYROL.DAT
290 FIELD #1%,512% AS T2$,64% AS T4$
320 GOTO 1000
500 GOTO 550 IF FNS%<>5% : INPUT "CONFIRM CREATING A NEW PAYROLL FILE";K$ &
	: GOTO 10000 UNLESS LEFT(K$,1%)="Y" &
	: OPEN "PR"+Z1$ FOR OUTPUT AS FILE 10% &
	: PRINT #10%,CVT%$(0%)+CVT%$(14%)+"S"+CHR$(128%) : CLOSE 10% &
	: OPEN "PR"+LEFT(Z1$,7%)+"1" FOR OUTPUT AS FILE 10% &
	: PRINT #10%, CVT%$(0%)+CVT%$(64%)+"S"+CHR$(128%) : CLOSE 10% &
	: PRINT "A NEW PAYROLL FILE HAS BEEN OPENED" &
	: IF FNO%(4%,"PR"+Z1$,"U","R") THEN PRINT "HELP." : GOTO 10000
510 RETURN
550 PRINT "ERROR";FNS% : GOTO 10000
990 DEF FNZ(Z)=INT(Z*100.+.51)/100.
1000	! &
	! PROGRAM CONTROL &
	!
1020 PRINT : INPUT "OPTION ";K$ : K$=LEFT(K$,3%) &
	: GOTO 1030 IF K$=""    : GOTO 8000 IF K$="ENT" : GOTO 3000 IF K$="DEL" &
	: GOTO 5000 IF K$="EXA" : GOTO 7000 IF K$="PRI" : GOTO 6000 IF K$="TOT" &
	: GOTO10000 IF K$="END" &
	: PRINT : PRINT "TYPE <RETURN> FOR AN OPTIONS LIST." : GOTO 1020
1030 PRINT &
	: PRINT "OPTIONS:  'ENTER' PAYROLL DATA" &
	: PRINT "          'DELETE' A FILE RECORD" &
	: PRINT "          'EXAMINE' ALL RECORDS FOR AN EMP #" &
	: PRINT "          'PRINT' FILE CONTENTS" &
	: PRINT "          'TOTAL' HOURS PRINTED" &
	: PRINT "          'END' PROGRAM AND UPDATE FILES" : GOTO 1020
2000	! &
	!  INPUT EMPLOYEE # &
	!
2010 PRINT: PRINT "EMPLOYEE # ";: INPUT LINE #12%,A$: A$=CVT$$(A$,4%) &
	: RETURN IF A$="": A$=A$+SPACE$(6%-LEN(A$)): RETURN
2300	! &
	! SEPARATE MASTER FILE &
	!
2310 LSET T2$=FNL$ &
	: M$(I%)=M1$(I%)+"" FOR I%=1% TO 23% &
	: M(I%)=CVT$F(M2$(I%)) FOR I%=1% TO 36% &
	: M(I%)=CVT$%(M2$(I%))/10. FOR I%=37% TO 44% : RETURN
2400	! &
	!	SEPARATE PAYROLL &
	!
2410 LSET T4$=FNL$ &
	: P$(I%)=P1$(I%)+"" FOR I%=1% TO 8% &
	: P%(I%)=ASCII(P1$(I%+9%)) FOR I%=1% TO 15% &
	: P(1%)=CVT$F(P1$(25%)) &
	: P(I%)=FNN3(P1$(I%+24%)) FOR I%=2% TO 5% &
	: P%(0%)=CVT$%(P1$(9%)) : P$(9%)=FND9$(P%(0%)) &
	: IF P%(7%)=255% THEN P(6%)=FNN3(P1$(13%)+P1$(14%)+P1$(15%)) &
	: P(7%)=FNN3(P1$(20%)+P1$(21%)+P1$(22%)) &
	: P%(I%)=0% FOR I%=1% TO 14% : RETURN
2420 P(6%),P(7%)=0% : P(6%)=P(6%)+P%(I%) FOR I%=1% TO 7% &
	: P(7%)=P(7%)+P%(I%) FOR I%=8% TO 14% &
	: P(I%)=P(I%)/10. FOR I%=6% TO 7% : RETURN
3000	! &
	!  DELETE &
	!
3002 GOTO 1020 IF FNG%(4%,"") : GOSUB 2400 : IF [P%(15%) AND 7%] THEN PRINT &
	"PAYROLL HAS BEEN UPDATED - NO CHANGES CAN BE MADE" : GOTO 1020
3010 GOSUB 2000 : GOTO 1020 IF A$="" &
	: IF FNG%(4%,A$) THEN PRINT "NO RECORDS FOUND." : GOTO 3010
3014 INPUT "JOB # ";K$ : K$=K$+SPACE$(6%-LEN(K$)) &
	: IF FNG%(4%,A$+K$) THEN PRINT "CLOSEST ONE IS. . ."
3020 GOSUB 2400 &
	: V%=FNG%(2%,P$(1%)): GOSUB 2300
3025 X1%=1%: E%=1%: GOSUB 7110
3030 INPUT "CONFIRM (? FOR HELP) ";K$ : GOTO 3010 IF K$="" &
	: GOTO 3100 IF K$="?" : GOTO 3200 IF K$="+" : GOTO 3300 IF K$="-" &
	: GOTO 3000 IF K$="N" : GOTO 3030 IF K$<>"Y" &
	: IF FND%(4%,"") THEN PRINT "ERROR";FNS%;"OCCURRED IN DELETION." &
	: GOTO 10000
3050 PRINT "DELETED." : GOTO 3010
3100 PRINT : PRINT "'Y'      DELETE THIS RECORD" : PRINT "'+'      NEXT RECORD" &
	: PRINT "'-'      PREVIOUS RECORD" : PRINT "'?'      HELP MESSAGE" &
	: PRINT "<RETURN> RETURN TO 'EMPLOYEE' QUESTION" : PRINT : GOTO 3030
3200 GOTO 3000 IF FNN%(4%) : GOSUB 2400 : GOTO 3025
3300 GOTO 3000 IF FNN%(-4%) : GOSUB 2400 : GOTO 3025
5000	! &
	!  EXAMINE &
	!
5010 INPUT "FORM (L/S) ";F$ : T(I%),T1(I%),T2(I%),T2(I%+8%)=0. FOR I%=1% TO 8% &
	: GOSUB 2000 : GOTO 1020 IF A$="" : V%=FNG%(4%,A$) : GOSUB 2400 &
	: X1%=70% : GOSUB 7900 : V%=FNG%(2%,P$(1%)) : GOSUB 2300 : E%=1%
5020 IF A$=P$(1%) THEN GOSUB 7110 : IF FNN%(4%)=0% THEN GOSUB 2400 : GOTO 5020
5030 GOSUB 7200 : GOTO 5010
6000	! &
	! PRINT TOTAL HOURS REPORT BY EMPLOYEE &
	!
6010 GOTO 1020 IF FNG%(4%,"") : INPUT "SET PAGE";K$ &
	: T(I%),T1(I%)=0. FOR I%=1% TO 8% : E$="" : X1%=66%
6020 GOSUB 2400 : GOSUB 6050 IF E$<>P$(1%) : T(1%)=T(1%)+P(6%) &
	: T(2%)=T(2%)+P(7%) : T1(1%)=T1(1%)+P(6%) : T1(2%)=T1(2%)+P(7%) &
	: GOTO 6020 UNLESS FNN%(4%) : GOSUB 6050 &
	: PRINT USING "       \"+SPACE$(20%)+"\ #####.## #####.##", &
		"*** Grand Totals ***",T1(1%),T1(2%) &
	: PRINT : PRINT : GOTO 1020
6050 GOTO 6060 IF E$="" : GOSUB 6080 IF X1%>60% : STOP IF FNG%(2%,E$) &
	: GOSUB 2300 : X1%=X1%+1% : PRINT USING &
	"\    \ \"+SPACE$(20%)+"\ #####.## #####.##",E$,M$(2%),T(1%),T(2%)
6060 E$=P$(1%)+"" : T(I%)=0. FOR I%=1% TO 6% : RETURN
6080 PRINT FOR X1%=X1% TO 65% : PRINT : PRINT : PRINT &
	: PRINT "EMP #    NAME                  REG HRS  OVT HRS" &
	: X1%=4% : RETURN
7000	! &
	!  PRINT &
	!
7010 INPUT "FORM (L/S) ";F$ : F$=LEFT(F$,1%) &
	: IF F$<>"S" AND F$<>"L" THEN PRINT &
	: PRINT '"L" - LONG FORM WITH HOURS BY INDIVIDUAL DAY' &
	: PRINT '"S" - SHORTER FORM WITH JUST TOTAL HOURS' : PRINT : GOTO 7010
7020 GOTO 1020 IF FNG%(4%,"") : E%=1% : P%=0% &
	: T(I%),T1(I%),T2(I%),T2(I%+8%)=0. FOR I%=1% TO 8% &
	: INPUT #12%,"SET PAGE. . .";K$ : X1%=66% : GOSUB 7900
7025 GOSUB 2400 : V%=FNG%(2%,P$(1%)) : GOSUB 2300 : GOSUB 7110 : GOSUB 7250 &
	: IF FNN%(4%) THEN GOSUB 7200 : GOSUB 7510 : P%=0% : F$="" : PRINT &
	: PRINT "*  UPDATED TO PAYROLL MASTER FILE" &
	: PRINT "#  JOURNAL ENTRY MADE TO GENERAL LEDGER" &
	: PRINT "!  BOTH * AND #" : GOTO 1020
7027 GOTO 7025
7100	! &
	! PRINT ONE RECORD &
	!
7110 GOSUB 7200 IF E$<>P$(1%) : E%=2% : GOSUB 7900 IF X1%>58% &
	: IF E1%=1% THEN PRINT : G$="" : G$="*" IF P%(15%)=1% &
	: G$="#" IF P%(15%)=2% : G$="!" IF P%(15%)=3% : PRINT G$;M$(2%) &
	: E1%=2% : X1%=X1%+2%
7115 GOTO 7300 IF F$="S" : X1%=X1%+1% &
	: PRINT P$(1%);" ";P$(2%);"  "; &
	: PRINT USING "####.## \\    ####.## \\",P(1%),P$(4%),P(2%),P$(3%); &
	: FOR I%=1 TO 7% &
	: IF P%(I%)=0% THEN PRINT "     "; ELSE PRINT USING " ##.#",P%(I%)/10.; &
	: T(I%)=T(I%)+P%(I%)/10.
7140 NEXT I% : PRINT USING " ##.##",P(6%) &
	: GOTO 7180 UNLESS P$(8%)<>SPACE$(7%) OR P(7%)>0% &
	: PRINT SPACE$(30%); : PRINT USING ".\     \.",P$(8%); UNLESS P$(8%)="" &
	: PRINT "         "; IF P$(8%)="" : FOR I%=1% TO 7% &
	: IF P%(I%+7%)=0. THEN PRINT "     "; ELSE &
		PRINT USING " ##.#",P%(I%+7%)/10.; &
	: T1(I%)=T1(I%)+P%(I%+7%)/10.
7170 NEXT I% : PRINT USING " ##.##",P(7%) : X1%=X1%+1%
7180 PRINT USING "DEDUCTION \\ :######.##",P$(5%),P(3%) UNLESS P(3%)=0. &
	: PRINT USING "DEDUCTION \\ :######.##",P$(6%),P(4%) UNLESS P(4%)=0. &
	: PRINT USING "EARNINGS  \\ :######.##",P$(7%),P(5%) UNLESS P(5%)=0. &
	: X1%=X1%+1% IF P(I%)<>0. FOR I%=3% TO 5% : RETURN
7200	! &
	! PRINT TOTALS &
	!
7205 E$=P$(1%) : E1%=1% : RETURN IF E%=1% &
	: IF F$="S" THEN PRINT USING  "TOTALS------->            "+ &
		"#####.##  #####.##  ######.##  ######.## $$###,###.##-", &
		T(1%),T(2%),T(3%),T(4%),T(5%) &
	: X1%=X1%+1% &
	: T1(I%)=T1(I%)+T(I%) FOR I%=1% TO 5% &
	: T(I%)=0. FOR I%=1% TO 5% : RETURN
7210 PRINT : PRINT SPACE$(17%);"TOTAL REGULAR  HOURS :"; : FOR I%=1% TO 7% &
	: IF T(I%)=0. THEN PRINT "     "; ELSE PRINT USING " ##.#",T(I%); &
	: T(8%)=T(8%)+T(I%)
7220 NEXT I% : PRINT USING "###.##",T(8%) &
	: T1(8%)=T1(8%)+T1(I%) FOR I%=1% TO 7% : X1%=X1%+3% &
	: GOTO 7240 IF T1(8%)=0% : PRINT SPACE$(17%);"TOTAL OVERTIME HOURS :"; &
	: FOR I%=1% TO 7% &
	: IF T1(I%)=0% THEN PRINT "     "; ELSE PRINT USING " ##.#",T1(I%);
7230 NEXT I% : X1%=X1%+1% : PRINT USING "###.##",T1(8%)
7240 FOR I%=1% TO 8% : T2(I%)=T2(I%)+T1(I%) : T2(I%+8%)=T2(I%+8%)+T(I%) &
	: T1(I%),T(I%)=0. : NEXT I% : PRINT : RETURN
7250	P$(2%)=CVT$$(P$(2%),128%) &
	: RETURN IF INSTR(1%,P$(2%),"-")=0% &
			OR V1%<>0% &
	: GOTO 7260 IF FNG%(8%,LEFT(P$(2%),3%)) &
	: J$=MID(FNL$,157%,1%) &
	: RETURN IF J$="O" OR J$="X"
7260 PRINT "     <<<<<< CHECK ABOVE JOB # >>>>>>" : X1%=X1%+1% : RETURN
7300	! &
	! PRINT SHORT FORM &
	!
7310 X1%=X1%+1% : PRINT P$(1%);" ";P$(2%);"  "; : O$=LEFT(P$(4%),1%) : O=1. &
	: O=1. : O=1.5 IF O$="H" : O=2. IF O$="D" &
	: T=FNZ(P(6%)*P(1%))+FNZ(P(7%)*P(1%)*O) &
	: T(I%)=T(I%)+P(I%+5%) FOR I%=1% TO 2% &
	: T(3%)=T(3%)+P(5%) : T(4%)=T(4%)+P(2%)+P(3%)+P(4%) &
	: T(5%)=T(5%)+T+P(5%)-P(2%)-P(3%)-P(4%) &
	: PRINT USING "####.## \\  ####.##   ####.##   #####.##"+ &
		"   #####.## $$###,###.##-",P(1%),P$(4%),P(6%),P(7%), &
		P(5%),P(2%)+P(3%)+P(4%),T+P(5%) : RETURN
7500	! &
	! GRAND TOTALS &
	!
7510 IF F$<>"S" THEN GOSUB 7900 IF X1%>60% : PRINT &
	: PRINT USING TAB(10%)+"GRAND TOTAL REGULAR HOURS#####.##",T2(16%) &
	: PRINT USING TAB(10%)+"GRAND TOTAL OVERTIME HOURS####.##",T2(8%) &
	: X1%=X1%+3% : RETURN
7520	  E%=0% &
	: PRINT &
	: PRINT &
	: X1%=X1%+2% &
	: GOSUB 7900 IF X1%>60% &
	: T(I%)=T1(I%) FOR I%=1% TO 5% &
	: GOSUB 7200 &
	: RETURN
7900	! &
	! PRINT REGISTER PAGE HEADER &
	!
7920 PRINT FOR I%=X1% TO 68% : P%=P%+1% &
	: PRINT "PAYROLL DATE :  ";Z$;TAB(55%);"PAGE";P% : PRINT : X1%=7% &
	: PRINT "EMP#   JOB #      RATE       SUBSIST     "; &
		RIGHT(D$,A0%*5%-4%);LEFT(D$,(A0%-1%)*5%);"HOUR" UNLESS F$="S" &
	: PRINT "EMP#   JOB #     RATE    REG HOURS  OT HOURS"+ &
		"  OTH. EARN  OTH. DEDUCT      GROSS" IF F$="S" &
	: PRINT : RETURN
8000	! &
	! ENTER OPTION &
	!
8005 V%=FNG%(4%,"") : GOTO 8010 IF V%=88% : STOP IF V% : GOSUB 2400 &
	: IF [P%(15%) AND 7%] THEN PRINT "PAYROLL HAS BEEN UPDATED, "; &
		"NO NEW ENTRIES CAN BE MADE." : GOTO 1020
8010 CLOSE #1%,12% : V%=FNX%("[1,8]CONPA2",1010,LEFT(Z1$,4%)+MID(Z1$,6%,2%)) &

8100	! &
	! RETURN FROM ENTER OPTION &
	!
8110 V$=FNX$
8120 Z$=LEFT(V$,2%)+"."+MID(V$,3%,2%)+"."+RIGHT(V$,5%) &
	\ GOTO 10
10000	! &
	!  TERMINATE PROGRAM &
	!
10010 V%=FNC%(2%)+FNC%(4%)+FNC%(8%) : CLOSE 1%,12% &
	: Q3$=CVT%$(8100%)+"!MENU.BAC"+SYS(CHR$(7%)) &
		IF ASCII(SYS(CHR$(7%)))=255% : V%=FNC%(0%)
10020 CHAIN "!MENU" 8100 IF ASCII(SYS(CHR$(7%)))=255% : GOTO 32767
14020 DEF FND9$(D9%) : FND9$=RIGHT(NUM1$((D9% AND 15%)+100%),2%)+ &
		"."+RIGHT(NUM1$((D9% AND 31%*16%)/16%+100%),2%)+ &
		"."+RIGHT(NUM1$(((SWAP%(D9%) AND 254%)/2%)+100%),2%) : FNEND
14210 DEF FND7%(D7$) : ON ERROR GOTO 14220 &
	: GOTO 14220 IF INSTR(1%,D7$,".")<>3% OR INSTR(4%,D7$,".")<>6% OR &
		INSTR(7%,D7$,".")<>0% OR LEN(D7$)<>8% &
	: D7%=VAL(LEFT(D7$,2%))   : GOTO 14220 IF D7%<1% OR D7%>12% &
	: D7%=VAL(MID(D7$,4%,2%)) : GOTO 14220 IF D7%<1% OR D7%>31% &
	: D7%=VAL(RIGHT(D7$,7%))  : GOTO 14220 IF D7%<0%: FND7%=0% : GOTO 14230
14220 FND7%=-1% : RESUME 14230
14230 ON ERROR GOTO 0 : FNEND
14260 DEF FND7$(D7$) : D7$=D7$+"."+RIGHT(DATE$(0%),8%) IF LEN(D7$)<6% &
	: D7$="0"+D7$ IF INSTR(1%,D7$,".")=2% &
	: D7$=LEFT(D7$,3%)+"0"+RIGHT(D7$,4%) IF INSTR(4%,D7$,".")=5% &
	: FND7$=D7$ : FNEND
14350 DEF FNN3(N3$)=CVT$%(LEFT(N3$,2%))+ASCII(MID(N3$,3%,1%))/100.
32767 END
