1	EXTEND
10	! &
	! Program name: [1,8]CONCE1		Compiled with SCALE 0 on V07.0 &
	! Decompiled on 24-Aug-83 at 11:33 AM by UNBAC Version 1 &
	! &
	! CERTIFIED PAYROLL REPORT &
	! FROM PRREP - CONSTRUCTION PAYROLL REPORT &
	! &
	! 07/10/79 - MODIFIED BY BILL LOVEGROVE FOR GENERAL SYSTEM &
	! 08/07/79 - MODIFIED BY KEVIN HANDY -- FORMAT CHANGES &
	! 08/29/84 - BY WENDELL RICHARDSON -- OTHER EARNINGS CORRECTION &
	! 05/30/85 - by Rick Owen - Fix to properly handle situation where &
	!			    more than one record for an employee in &
	!			    earnings and deduction file. &
	! &

50	DIM M1$(23%), M$(23%), P1$(29%), P$(9%), P%(17%), &
		P(5%), D1$(25%), D(14%), DD(14%) &

60	IF FNO%(9%,"PRREP.TMP" ,"" ,"" ) OR FNG%(9%,"" )<>0% OR LEFT(FNL$, &
		14%)<> STRING$(14%,0%) &
	THEN	PRINT "A TEMPORARY FILE DOES NOT YET EXIST.  PLEASE" &
\		PRINT "CREATE IT WITH THE CREATE OPTION IN PRREP PROGRAM" &
\		GOTO 10000 &

70	Z$=MID(FNL$,15%,8%) &
\	Z1$=LEFT(Z$,2%)+RIGHT(Z$,4%)+"T" &
\	PRINT " A FILE EXISTS FOR ";Z$ UNLESS F% &

100	! &
	! OPEN FILES &
	! &

110	OPEN "KB:" AS FILE 1%, RECORDSIZE 512%+64%+128% &

120	IF FNO%(2%,"MSTRFL.DAT" ,"/RO" ,"" ) &
	THEN	PRINT "MASTER FILE NOT FOUND !!!" &
\		GOTO 10000 &

140	IF FNO%(4%,"JOB.DAT" ,"/RO" ,"" ) &
	THEN	PRINT "JOB.DAT NOT FOUND !!!" &
\		GOTO 10000 &

150	IF FNO%(6%,"TX"+Z1$,"/RO" ,"" ) &
	THEN	PRINT "EARNINGS AND DEDUCTIONS FILE DOES NOT EXIST !!!" &
\		GOTO 10000 &

160	IF FNO%(11%,"CUSTOM.DAT" ,"/RO" ,"" ) &
	THEN	PRINT "CUSTOMER FILE NOT FOUND !!!" &
\		GOTO 10000 &

170	ON ERROR GOTO 180 &
\	OPEN "SS0:UNIQUE.FIL/RO" AS FILE 8% &
\	DIM #8%, A0$(255%)=64% &
\	A0$=A0$(15%) &
\	D%=INSTR(1%,A0$,"DAY=" ) &
\	D%=VAL(MID(A0$,D%+4%,1%)) IF D% &
\	J%=INSTR(1%,A0$,"LEN=" ) &
\	J%=VAL(MID(A0$,J%+4%,1%)) IF J% &
\	A0$="MON  TUE  WED  THR  FRI  SAT  SUN  " &
\	A0$=MID(A0$+A0$,(D%-1%)*5%+1%,35%) IF D% &
\	GOTO 185 &

180	PRINT "A UNIQUE FILE DOES NOT EXIST !!! " &
\	RESUME 10000 &

185	ON ERROR GOTO 0 &

200	! &
	! FIELD STATEMENTS &
	! &

220	FIELD #1%, 6% AS M1$(1%),30% AS M1$(2%),30% AS M1$(3%), &
		30% AS M1$(4%), 30% AS M1$(5%),11% AS M1$(6%),8% AS M1$(7%), &
		1% AS M1$(8%),2% AS M1$(9%),12% AS M1$(10%),2% AS M1$(11%) &

221	FIELD #1%, 160%+I%*10% AS E$,2% AS M1$(I%+11%) FOR I%=1% TO 9% &
\	FIELD #1%, 252% AS E$,8% AS M1$(21%),8% AS M1$(22%) &
\	FIELD #1%, 484% AS E$,2% AS M1$(23%) &

230	FIELD #1%, 512% AS E$,6% AS P1$(1%),6% AS P1$(2%),1% AS P1$(3%), &
		1% AS P1$(4%),2% AS P1$(5%),2% AS P1$(6%),2% AS P1$(7%), &
		7% AS P1$(8%),2% AS P1$(9%) &
\	FIELD #1%, 512%+19%+I% AS E$,1% AS P1$(I%) FOR I%=10% TO 24% &
\	FIELD #1%, 512%+44% AS E$,8% AS P1$(25%),3% AS P1$(26%), &
		3% AS P1$(27%), 3% AS P1$(28%),3% AS P1$(29%) &

240	FIELD #1%, 512%+64% AS E$,6% AS D1$(1%),1% AS D1$(2%),2% AS D1$(3%), &
		2% AS D1$(4%),2% AS D1$(5%),2% AS D1$(6%),2% AS D1$(7%), &
		2% AS D1$(8%),2% AS D1$(9%),8% AS D1$(10%),2% AS D1$(11%), &
		8% AS D1$( 12%),8% AS D1$(13%),8% AS D1$(14%),8% AS D1$(15%), &
		8% AS D1$(16%), 8% AS D1$(17%),8% AS D1$(18%),8% AS D1$(19%), &
		8% AS D1$(20%),8% AS D1$(21%),8% AS D1$(22%),3% AS D1$(23%), &
		3% AS D1$(24%),1% AS D1$(25%) &

290	FIELD #1%, 512% AS T2$,64% AS T4$,128% AS T6$ &

400	U1$=" RT/OT=###.###/###.###  O.T.=" &
\	U8$=" CLASS=\              \ R.T.=" &
\	J1$=SPACE$(72%) &
\	C$=SPACE$(36%) &

401	U2$="####.##" &

402	U3$="#####.##  #####.##  #####.##  * ####.## ####.## ####.## ####.## #####.##" &

990	DEF FNZ(Z2)=INT(Z2*100.0+0.5)/100.0 &

1000	! &
	! PROGRAM CONTROL SECTION &
	! &

1010	IF F% &
	THEN	GOTO 7300 IF F%=-1% &
\		GOTO 7330 IF F%=-2% &

1020	GOTO 6100 IF F% &
\	PRINT &
\	INPUT "OPTION ";K$ &
\	K$=LEFT(K$,3%) &
\	GOTO 7300 IF K$="CER" &
\	GOTO 7330 IF K$="SEL" &
\	GOTO 10000 IF K$="END" &

1030	PRINT "OPTIONS ARE:" &
\	PRINT "   CERTIFIED" &
\	PRINT "   SELECTIVE CERTIFIED" &
\	PRINT "   END PROGRAM" &
\	PRINT &
\	GOTO 1020 &

2300	! &
	! SEPERATE MASTER FILE &
	! &

2310	LSET T2$=FNL$ &
\	M$(I%)=M1$(I%)+"" FOR I%=1% TO 23% &
\	RETURN &

2400	! &
	! SEPERATE TEMP FILE &
	! &

2410	LSET T4$=FNL$ &
\	P$(I%)=P1$(I%)+"" FOR I%=1% TO 8% &
\	P%(I%)=ASCII(P1$(I%+9%)) FOR I%=1% TO 15% &
\	P(1%)=CVT$F(P1$(25%)) &
\	P(I%)=FNN3(P1$(I%+24%)) FOR I%=2% TO 5% &
\	P%(0%)=CVT$%(P1$(9%)) &
\	P$(9%)=FND9$(P%(0%)) &
\	P%(16%),P%(17%)=0% &
\	P%(16%)=P%(16%)+P%(I%) FOR I%=1% TO 7% &
\	P%(17%)=P%(17%)+P%(I%) FOR I%=8% TO 14% &
\	RETURN &

2450	! &
	! ADD A TEMP FILE RECORD &
	! &

2460	LSET T4$=FNL$ &
\	P%(I%)=P%(I%)+ASCII(P1$(I%+9%)) FOR I%=1% TO 15% &
\	P(I%)=P(I%)+FNN3(P1$(I%+24%)) FOR I%=2% TO 5% &
\	P%(16%),P%(17%)=0% &
\	P%(16%)=P%(16%)+P%(I%) FOR I%=1% TO 7% &
\	P%(17%)=P%(17%)+P%(I%) FOR I%=8% TO 14% &
\	RETURN &

2500	! &
	! SEPERATE ERNDED FILE &
	! &

2510	LSET T6$=FNL$ &
\	D(I%)=CVT$F(D1$(I%+11%)) FOR I%=1% TO 11% &
\	D(I%+11%)=FNN3(D1$(I%+22%)) FOR I%=1% TO 2% &
\	RETURN &

2520	! &
	!	If there is more than one record for an employee in &
	!	the earnings and deduction file we will will come here &
	!	to sum them up. &
	! &
	DD(I%) = D(I%) FOR I% = 1% TO 14% &

2530	RETURN IF FNN%(6%) &
\	RETURN IF LEFT(FNL$,LEN(A$)) <> A$ &
\	GOSUB 2500 &
\	DD(I%) = DD(I%) + D(I%) FOR I% = 1% TO 14% &
\	GOTO 2530 &

6000	! &
	! CHAIN IN FROM CONPRR &
	! &

6010	F%=-1% &
\	GOTO 6090 &

6020	F%=-2% &
\	GOTO 6090 &

6090	GOTO 10 &

6100	! &
	! CHAIN BACK TO PRREP &
	! &

6110	PRINT FOR I%=1% TO 66%-L% &
\	V%=FNX%("[1,8]CONPR2",0%,"") &

7140	! &
	! SEPERATE TEMP STRING &
	! &

7145	GOSUB 2400 &
\	RETURN IF LEFT(P$(1%),2%)="PR" &
\	B1$=P$(1%) &
\	B1$=LEFT(B1$,J%) IF J% &
\	B1$=LEFT(B1$,INSTR(1%,B1$,"-" )-1%) IF INSTR(1%,B1$,"-" )<>0% &
\	A$=P$(2%) &
\	P$(1%)=A$ &
\	P$(2%)=B1$ &

7147	V%=FNN%(9%) &
\	GOTO 7150 IF V%<>0% &
\	LSET T4$=FNL$ &
\	IF P1$(2%)<>P$(1%) OR CVT$F(P1$(25%))<>P(1%) OR P$(2%)<>P1$(1%) &
	THEN	V%=FNN%(-9%) &
\		GOTO 7150 &

7149	GOSUB 2450 &
\	GOTO 7147 &

7150	V%=FNG%(2%,A$) &
\	STOP IF V% &
\	GOSUB 2300 &
\	V%=FNG%(6%,A$) &
\	STOP IF V% &
\	GOSUB 2500 &
\	GOSUB 2520 &
\	RETURN &

7160	GOSUB 7220 IF B1$<>B2$ &
\	GOSUB 7220 IF L%>55% &
\	GOSUB 7170 &
\	RETURN &

7170	! &
	! PRINT ONE EMPLOYEE RECORD &
	! &

7172	PRINT LEFT(M$(2%),23%);TAB(24%);"SUB.="; &
\	PRINT "   "+MID(P$(8%),I%,1%)+" "; FOR I%=1% TO 7% &
\	PRINT USING "####.##" , P(2%) &

7175	N1,R1=P(1%) &
\	R1=R1*1.5 IF P$(4%)="O" OR P$(4%)="H" &
\	R1=R1*2. IF P$(4%)="D" &
\	PRINT USING U1$, N1,R1; &
\	FOR I%=1% TO 7% &
\		R%=P%(I%) &
\		O%=P%(I%+7%) &

7180		R%(I%)=R% &
\		IF O% &
		THEN	PRINT USING "###.#" , O%/10.; &
		ELSE	PRINT "     "; &

7183	NEXT I% &
\	PRINT USING "####.##" , P%(17%)/10. &
\	GOSUB 7700 &
\	D,D1=0. &
\	D=D+DD(I%) FOR I%=3% TO 12% &
\	D1=D1+DD(I%) FOR I%=6% TO 12% &

7185	PRINT USING U8$, M$(10%); &
\	FOR I%=1% TO 7% &
\		IF R%(I%) &
		THEN	PRINT USING "###.#" , R%(I%)/10.; &
		ELSE	PRINT "     "; &

7186	NEXT I% &
\	PRINT USING U2$, P%(16%)/10. &
\	PRINT "  SS#="; &
\	PRINT USING "\          \" , M$(6%) &
\	GOSUB 7260 &
\	PRINT TAB(5%); &

7187	PRINT USING U3$, E,DD(1%)+DD(2%)-E,DD(1%)+DD(2%),DD(4%),DD(3%), &
		DD(5%),D1,DD(1%)+DD(2%)-D &

7190	L%=L%+6% &

7195	RETURN &

7220	! &
	! PAGE &
	! &

7225	GOSUB 7800 IF B1$<>B2$ AND B2$<>"" &

7230	PRINT FOR I%=1% TO 66%-L% &
\	PRINT &
\	PRINT &
\	PRINT TAB(13%);"CERTIFIED PAYROLL FOR THE PERIOD ENDING ";Z$ &
\	PRINT &

7233	IF B1$<>B2$ &
	THEN	V%,V1%=0% &
\		IF FNG%(4%,LEFT(B1$,INSTR(1%,B1$+"-" ,"-" )-1%)) &
		THEN	V%=-1% &
		ELSE	LSET J1$=FNL$ &
\			IF FNG%(11%,MID(J1$,67%,6%)) &
			THEN	V1%=-1% &
			ELSE	LSET C$=FNL$ &

7236	B3$=B1$ &
\	PRINT "    JOB # ";B1$;"  "; &
\	PRINT CVT$$(MID(J1$,7%,60%),128%); UNLESS V% &
\	PRINT &
\	PRINT "    CUSTOMER # "; &
\	PRINT MID(J1$,67%,6%);"   "; UNLESS V% &
\	PRINT CVT$$(MID(C$,7%,30%),128%); UNLESS V1% OR V% &
\	PRINT &

7240	PRINT TAB(8%);"NAME";TAB(43%);"DAILY TIME" &

7250	PRINT TAB(31%);A0$;"TOTAL" &
\	PRINT &

7253	B2$=B1$ &
\	L%=9% &

7255	RETURN &

7260	PRINT TAB(6%);"THIS JOB  OTH JOBS    TOTAL  *    "; &
		"FICA    FED.   STATE   OTHER     NET" &

7270	RETURN &

7300	! &
	! CERTIFIED, SELECTIVE CERTIFIED &
	! &

7305	PRINT "SET PAGE. . ."; &
\	GOTO 1020 IF FNG%(9%,"" )+FNN%(9%) &
\	L%=63% &
\	B2$="" &
\	INPUT LINE #1%, J$ &

7310	GOSUB 7140 &
\	GOSUB 7160 &

7320	V%=FNN%(9%) &
\	GOTO 7310 IF V%=0% &
\	STOP IF V%<>11% &
\	GOSUB 7800 &
\	GOTO 1020 &

7330	! &
	! SELECTIVE REPORT &
	! &

7335	GOTO 1020 IF FNG%(9%,"" ) &
\	L%=66% &
\	V$=SYS(CHR$(3%)) &
\	PRINT "POSITION PAPER THEN ENTER JOBS SEPARATED BY RETURNS" &

7340	B2$="" &
\	PRINT CHR$(7%); &
\	INPUT LINE #1%, J$ &
\	J$=CVT$$(J$,6%) &
\	GOTO 7370 IF J$="" &
\	J$=J$+STRING$(5%-LEN(J$),32%) &
\	B1$=J$ &
\	V%=FNG%(9%,J$) &
\	GOTO 7340 IF V%=88% &
\	STOP IF V% &
\	IF P8%=0% &
	THEN	GOSUB 7230 &
	ELSE	GOSUB 7220 &

7345	STOP IF FNG%(9%,J$) &

7350	GOSUB 7140 &
\	P8%=1% IF B1$<>B2$ &
\	GOTO 7400 IF B1$<>B2$ &
\	GOSUB 7220 IF L%>42% &
\	GOSUB 7170 &

7360	V%=FNN%(9%) &
\	GOTO 7350 IF V%=0% &
\	STOP IF V%<>11% &
\	GOTO 7400 &

7370	V$=SYS(CHR$(2%)) &
\	GOTO 1020 &

7400	GOSUB 7800 &
\	PRINT FOR I%=1% TO 66%-L% &
\	L%=100% &
\	GOTO 7340 &

7700	! &
	! COMPUTE TOTAL EARNINGS FROM PAYROLL RECORD FOR ONE JOB &
	! &

7701	D=1. &
\	D=2. IF P$(4%)="D" &
\	D=1.5 IF P$(4%)="H" &
\	A1=P%(16%)/10. &
\	A2=P%(17%)/10. &
\	E=FNZ(A1*P(1%))+FNZ(A2*P(1%)*D)+P(5%) &

7720	RETURN &

7800	! &
	! SIGNING OF CERTIFICATION REPORTS &
	! &

7810	B4$=B1$ &
\	B1$=B3$ &
\	GOSUB 7230 IF L%>53% &
\	B1$=B4$ &
\	PRINT &
\	PRINT "It is hereby certified that this payroll  is correct"; &
		"  and complete and that the" &
\	PRINT "wage rates contained hereon are not less than those "; &
		"determined by the secretary" &
\	PRINT "of  labor,  and that the classifications set forth for"; &
		" each laborer or mechanic" &
\	PRINT "conform with the work performed by that individual and"; &
		" payments of fringe" &
\	PRINT "benefits as listed in contract will be made to"; &
		" appropriate programs" &
\	PRINT "except as otherwise noted." &
\	PRINT &

7820	PRINT CVT$$(A0$(1%),128%) &
\	PRINT CVT$$(A0$(2%),128%);TAB(30%);"BY____________________________"; &
		"  DATE ";DATE$(0%) &
\	PRINT CVT$$(A0$(3%),128%) &
\	PRINT CVT$$(A0$(4%),128%) &
\	L%=L%+12% &
\	RETURN &

10000	! &
	! END PROGRAM &
	! &

10010	V%=FNX%("",0%,"") &

14000	! &
	! STORE AND RETREIVE DATE IN INTEGER &
	! &

14020	DEF FND9$(D9%) &
\		FND9$=RIGHT(NUM1$((D9% AND 15%)+100%),2%)+"."+ &
			RIGHT(NUM1$((D9% AND 31%*16%)/16%+100%),2%)+"."+ &
			RIGHT(NUM1$((SWAP%(D9%) AND 254%)/2%+100%),2%) &
\	FNEND &

14300	! &
	! FLOATING POINT NUMBER TO THREE CHARACTERS AND BACK &
	! &

14350	DEF FNN3(N3$) &
\		FNN3=CVT$%(LEFT(N3$,2%))+ASCII(MID(N3$,3%,1%))/100. &
\	FNEND &

32767	END
