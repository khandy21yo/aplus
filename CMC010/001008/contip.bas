1	EXTEND &

5	! Program name: [1,8]CONTIP	Compiled with SCALE 0 on V08.0 &
	! Decompiled on 28-Dec-84 at 10:22 AM by UNBAC Version 1 &

10	! &

15	Y$="" &
\	GOTO 1000 &

20	F=42000. &

25	F1=.0715 &

26	R1=3.35 &

27	FUI=7000. &

30	DIM R$(24%), M$(6%), P$(37%), M(37%), M1$(6%), M2$(44%), T(12%), &
		U2$(12%), T9(37%), D$(11%), D(14%), D1$(25%), P1$(30%), P(10%), &
		P%(15%) &

40	OPEN "NL:" AS FILE 1%, RECORDSIZE 512%+256%+128%+64% &

70	FIELD #1%, 512% AS E$,6% AS U1$(1%),6% AS U1$(2%),8% AS U1$(3%), &
		8% AS H$(1%),8% AS H$(2%),8% AS H$(3%),8% AS H$(4%), 1% AS U1$(4%), &
		2% AS U1$(5%) &
\	FIELD #1%, 37%+32%+512%+I%*8% AS D$,8% AS U2$(I%) FOR I%=1% TO 12% &

120	X$="$$###,########.##" &

150	IF Y$="" &
	THEN	INPUT "YEAR (<RETURN> FOR CURRENT)";Y$ &
\		Y$=CVT$$(Y$,-1%) &
\		Y$="FL" IF Y$="" &
\		GOTO 150 IF LEN(Y$)<>2% &

155	IF FNO%(2%,"MSTR"+Y$+".DAT" ,"/RO" ,"" ) &
	THEN	PRINT "MSTR"+Y$+".DAT IS NONEXISTENT" &
\		GOTO 32767 &

200	! &

220	FIELD #1%, 6% AS M1$(1%),30% AS M1$(2%),30% AS M1$(3%), &
		30% AS M1$( 4%),30% AS M1$(5%),11% AS M1$(6%),8% AS G$, &
		1% AS G$, 2% AS G$, 12% AS G$,2% AS G$ &

221	FIELD #1%, 152%+I%*10% AS E$,8% AS M2$(I%),2% AS G$ FOR I%=1% TO 9% &
\	FIELD #1%, 252% AS E$,8% AS G$,8% AS G$ &
\	FIELD #1%, 188%+I%*8% AS E$,8% AS M2$(I%) FOR I%=10% TO 36% &
\	FIELD #1%, 484% AS E$,2% AS G$ &
\	FIELD #1%, 486%+I%*2% AS E$,2% AS M2$(I%+37%) FOR I%=0% TO 7% &

240	FIELD #1%, 512%+256% AS E$,6% AS D1$(1%),1% AS D1$(2%),2% AS D1$(3%), &
		2% AS D1$(4%),2% AS D1$(5%),2% AS D1$(6%),2% AS D1$(7%), &
		2% AS D1$(8%),2% AS D1$(9%),8% AS D1$(10%),2% AS D1$(11%), &
		8% AS D1$(12%),8% AS D1$(13%),8% AS D1$(14%),8% AS D1$(15%), &
		8% AS D1$(16%),8% AS D1$(17%),8% AS D1$(18%),8% AS D1$(19%), &
		8% AS D1$(20%),8% AS D1$(21%),8% AS D1$(22%),3% AS D1$(23%), &
		3% AS D1$(24%),1% AS D1$(25%) &

250	FIELD #1%, 512%+256%+128% AS E$,6% AS P1$(1%),6% AS P1$(2%), &
		1% AS P1$(3%),1% AS P1$(4%),2% AS P1$(5%),2% AS P1$(6%), &
		2% AS P1$( 7%),7% AS P1$(8%),2% AS P1$(9%) &
\	FIELD #1%, 512%+256%+19%+I% AS E$,1% AS P1$(I%) FOR I%=10% TO 24% &
\	FIELD #1%, 512%+256%+44% AS E$,8% AS P1$(25%),3% AS P1$(26%), &
		3% AS P1$(27%),3% AS P1$(28%),3% AS P1$(29%) &

290	FIELD #1%, 512% AS T2$,256% AS T4$,128% AS T6$,64% AS T8$ &

300	RETURN &

400	I%=INSTR(1%,Z$,"." ) &
\	Z$="0"+Z$ IF I%=2% &
\	Z$=LEFT(Z$,2%)+RIGHT(Z$,4%) &
\	Z$=LEFT(Z$,2%)+"0"+RIGHT(Z$,3%) IF LEN(Z$)=6% &
\	Z$="PR"+Z$+"T" &
\	RETURN &

1000	! &

1010	GOSUB 20 &

1020	PRINT &
\	INPUT "OPTION ";K$ &
\	K$=LEFT(K$,3%) &
\	GOTO 3000 IF K$="CRE" &
\	GOTO 4000 IF K$="CHE" &
\	GOTO 5000 IF K$="PRI" &
\	GOTO 10000 IF K$="END" &

1030	PRINT &
\	PRINT "OPTIONS ARE:     CREATE TEMPORARY SORT FILE FOR PAYROLLS" &
\	PRINT "                 PRINT QUARTERLY 941 REPORT" &
\	PRINT "                 END program" &
\	GOTO 1020 &

2000	! &

2040	! &

2050	LSET T4$=FNL$ &
\	T4$(X%)=U1$(X%) FOR X%=1% TO 5% &
\	T(I%)=CVT$F(U2$(I%)) FOR I%=1% TO 12% &
\	H1=CVT$F(H$(1%)) &
\	H2=CVT$F(H$(2%)) &
\	H3=CVT$F(H$(3%)) &
\	H4=CVT$F(H$(4%)) &
\	RETURN &

2300	! &

2310	LSET T2$=FNL$ &
\	M$(I%)=M1$(I%)+"" FOR I%=1% TO 6% &
\	M(I%)=CVT$F(M2$(I%)) FOR I%=1% TO 36% &
\	RETURN &

2500	! &

2510	LSET T6$=FNL$ &
\	D$(I%)=D1$(I%)+"" FOR I%=1% TO 10% &
\	D$(11%)=FND9$(CVT$%(D1$(11%))) &
\	D(I%)=CVT$F(D1$(I%+11%)) FOR I%=1% TO 11% &
\	D(I%+11%)=FNN3(D1$(I%+22%)) FOR I%=1% TO 2% &
\	D%(1%)=ASCII(D1$(25%)) &
\	D(14%)=D(1%)-D(12%) &
\	D(14%)=D(14%)+D(2%) UNLESS A0% &
\	D(14%)=D(14%)-D(I%) FOR I%=3% TO 11% &
\	RETURN &

2600	! &

2610	LSET T8$=FNL$ &
\	O$=LEFT(P1$(4%),1%) &
\	O=1. &
\	O=1.5 IF O$="H" &
\	O=2. IF O$="D" &
\	P(1%)=CVT$F(P1$(25%)) &
\	P%(I%)=ASCII(P1$(I%+9%)) FOR I%=1% TO 15% &
\	IF P%(7%)=255% &
	THEN	P(6%)=FNN3(P1$(13%)+P1$(14%)+P1$(15%)) &
\		P(7%)=FNN3(P1$(20%)+P1$(21%)+P1$(22%)) &
\		P%(I%)=0% FOR I%=1% TO 14% &
\		GOTO 2620 &

2615	P(6%),P(7%)=0. &
\	P(6%)=P(6%)+P%(I%) FOR I%=1% TO 7% &
\	P(7%)=P(7%)+P%(I%) FOR I%=8% TO 14% &
\	P(I%)=P(I%)/10. FOR I%=6% TO 7% &

2620	IF P(1%)<R1 &
	THEN	M1=R1-P(1%) &
\		D1=D1+M1*(P(6%)+P(7%)*O) &
\		H1=H1+P(6%)+P(7%) &

2630	RETURN &

3000	! &

3010	STOP IF FNX%("[1,8]CONTI1.BAC" ,10%,Y$) &

4000	! &

4010	IF FNO%(9%,"EMPCKR.TMP" ,"U" ,"R" ) &
	THEN	PRINT"YOU MUST RUN THE CREATE OPTION BEFORE PRINTING THE REPORT" &
\		GOTO 1020 &

4015	STOP IF FNG%(9%,"     1" ) &
\	Y$(I%)=MID(FNL$,(I%-1%)*8%+7%,8%) FOR I%=1% TO 10% &
\	STOP IF FNN%(9%) &

4020	INPUT "SET PAGE ";A0$ &
\	STOP IF FNG%(9%,"     1" ) &
\	STOP IF FNN%(9%) &
\	S5$="" &
\	X9%=200% &

4030	GOSUB 4200 IF X9%>45% &

4040	GOSUB 2040 &
\	STOP IF FNG%(2%,T4$(1%)) &
\	GOSUB 2000 &
\	T%=T%+1% &
\	GOSUB 4500 IF S5$<>T4$(1%) AND T%>1% &
\	IF S5$<>T4$(1%) &
	THEN	T%=0% &
\		S1(I%)=0. FOR I%=0% TO 10% &
\		PRINT &
\		X9%=X9%+1% &
\		E%=E%+1% &

4050	GOSUB 4300 &
\	S5$=T4$(1%)+"" &
\	GOSUB 4400 &
\	GOTO 4030 UNLESS FNN%(9%) &
\	GOSUB 4500 IF T%>0% &

4060	PRINT FOR I%=1% TO 51%-X9% &
\	PRINT "TOTALS REPORT:" &
\	PRINT FOR I%=1% TO 5% &

4070	PRINT "TOTAL PAGES";TAB(25%);P% &
\	PRINT "TOTAL EMPLOYEES LISTED";TAB(25%);E% &
\	PRINT "TOTAL HOURS";TAB(25%); &
\	PRINT USING X$, S2(10%) &
\	PRINT "TOTAL GROSS WAGES";TAB(25%); &
\	PRINT USING X$, S2(0%) &
\	PRINT "TOTAL TIPS";TAB(25%); &
\	PRINT USING X$, S2(1%) &
\	PRINT "TOTAL MEALS";TAB(25%); &
\	PRINT USING X$, S2(5%) &

4080	PRINT "TOTAL INSURANCE ONE";TAB(25%); &
\	PRINT USING X$, S2(7%) &
\	PRINT "TOTAL INSURANCE TWO";TAB(25%); &
\	PRINT USING X$, S2(8%) &
\	PRINT "TOTAL DEDUCTIONS A,B,C";TAB(25%); &
\	PRINT USING X$, S2(6%) &
\	PRINT "TOTAL FEDERAL TAX";TAB(25%); &
\	PRINT USING X$, S2(2%) &
\	PRINT "TOTAL FICA TAX";TAB(25%); &
\	PRINT USING X$, S2(3%) &
\	PRINT "TOTAL STATE TAX";TAB(25%); &
\	PRINT USING X$, S2(4%) &

4090	S2(0%)=S2(0%)-S2(I%) FOR I%=1% TO 8% &
\	PRINT "TOTAL CHECK AMOUNTS";TAB(25%); &
\	PRINT USING X$, S2(0%) &

4100	PRINT FOR I%=1% TO 5% &
\	STOP IF FNC%(9%) &
\	P%,E%,T%,X9%=0% &
\	S2(I%)=0. FOR I%=0% TO 10% &
\	GOTO 1020 &

4200	! &

4210	PRINT FOR L%=1% TO 51%-X9% &
\	PRINT &
\	PRINT &
\	PRINT &
\	PRINT TAB(38%);"TOTAL  GROSS";TAB(76%);"TOTAL";TAB(113%);"PAYROL" &

4220	PRINT "DATE     EMP#      NAME          M#   HOURS"+ &
		"  WAGES  TIPS MEAL  INS 1 INS 2  A,B,C   FED   FICA"+ &
		"  STATE  NET CK     CK #" &

4230	P%=P%+1% &
\	PRINT &
\	X9%=6% &
\	RETURN &

4300	! &

4310	PRINT USING "\      \  \   \\"+SPACE$(17%)+"\\ \" , T4$(8%),T4$(1%), &
		M$(2%),T4$(2%)+T4$(3%); &

4320	A=T(12%) &
\	GOSUB 4640 &
\	PRINT " "; &

4330	A=T(1%) &
\	GOSUB 4640 &
\	A=T(2%) &
\	GOSUB 4620 &
\	PRINT " "; &

4340	A=T(6%) &
\	GOSUB 4600 &
\	FOR I%=8% TO 9% &
\		A=T(I%) &
\		GOSUB 4620 &
\	NEXT I% &

4350	A=T(7%) &
\	GOSUB 4640 &

4360	FOR I%=3% TO 5% &
\		A=T(I%) &
\		GOSUB 4640 &
\	NEXT I% &

4370	A=T(1%) &
\	A=A-T(I%) FOR I%=2% TO 9% &
\	GOSUB 4640 &

4380	PRINT USING "    ######" , T(11%); &
\	PRINT &
\	X9%=X9%+1% &
\	RETURN &

4400	! &

4410	S2(X%)=S2(X%)+T(X%+1%) FOR X%=0% TO 8% &
\	S2(9%)=S2(9%)+T(11%) &
\	S2(10%)=S2(10%)+T(12%) &

4420	S1(X%)=S1(X%)+T(X%+1%) FOR X%=0% TO 8% &
\	S1(9%)=S1(9%)+T(11%) &
\	S1(10%)=S1(10%)+T(12%) &
\	RETURN &

4500	! &

4510	PRINT TAB(37%); &

4520	A=S1(10%) &
\	GOSUB 4640 &
\	PRINT " "; &

4530	A=S1(0%) &
\	GOSUB 4640 &
\	A=S1(1%) &
\	GOSUB 4620 &
\	PRINT " "; &

4540	A=S1(5%) &
\	GOSUB 4600 &
\	FOR I%=7% TO 8% &
\		A=S1(I%) &
\		GOSUB 4620 &
\	NEXT I% &

4550	A=S1(6%) &
\	GOSUB 4640 &

4560	FOR I%=2% TO 4% &
\		A=S1(I%) &
\		GOSUB 4640 &
\	NEXT I% &

4570	A=S1(0%) &
\	A=A-S1(I%) FOR I%=1% TO 8% &
\	GOSUB 4640 &

4580	PRINT &
\	X9%=X9%+1% &
\	RETURN &

4600	IF A=0. &
	THEN	PRINT "  *  "; &
	ELSE	PRINT USING "##.##" , A; &

4610	RETURN &

4620	IF A=0. &
	THEN	PRINT "   *  "; &
	ELSE	PRINT USING "###.##" , A; &

4630	RETURN &

4640	IF A=0. &
	THEN	PRINT "    *  "; &
	ELSE	PRINT USING "####.##" , A; &

4650	RETURN &

5000	! &

5005	IF FNO%(9%,"TT0:EMPCKR.DAS" ,"/SF" ,"" ) &
	THEN	PRINT "CREATE THE FILE BEFORE PRINTING THIS REPORT" &
\		GOTO 1020 &

5007	INPUT "STATE UI CUTOFF";A1 &

5010	INPUT "QUARTER NUMBER ( 1-4 ) ";N% &
\	GOTO 5010 IF N%>4% &
\	N$=MID(" FIRSTSECOND THIRDFOURTH" ,N%*6%-5%,6%) &

5020	PRINT "FEDERAL I.D. # "; &
\	INPUT LINE A$(1%) &
\	PRINT "STATE I.D. # "; &
\	INPUT LINE A$(2%) &
\	PRINT "DATE QUARTER ENDED"; &
\	INPUT LINE A$(3%) &
\	PRINT "NAME"; &
\	INPUT LINE A$(4%) &
\	PRINT "ADDRESS"; &
\	INPUT LINE A$(5%) &
\	PRINT "CITY, STATE"; &
\	INPUT LINE A$(6%) &
\	PRINT "ZIP CODE"; &
\	INPUT LINE A$(7%) &
\	PRINT "NUMBER OF EMPLOYEES"; &
\	INPUT A% &
\	A$(I%)=CVT$$(A$(I%),4%) FOR I%=1% TO 7% &

5025	S1(I%),S2(I%),S3(I%),S4(I%),S5(I%)=0. FOR I%=0% TO 9% &

5040	INPUT "SET PAGE (WIDE LONG) ";A0$ &
\	STOP IF FNG%(9%,"" ) &
\	S5$="" &
\	X9%=64% &
\	T9(I%)=0. FOR I%=1% TO 37% &
\	S3,S4=0. &

5050	GOSUB 5300 IF X9%>58% &

5060	GOSUB 2040 &
\	GOTO 5095 IF FNG%(2%,T4$(1%)) &
\	GOSUB 2300 &
\	T%=T%+1% &
\	GOSUB 5100 IF S5$<>T4$(1%) AND S5$<>"" &
\	IF S5$<>T4$(1%) &
	THEN	T%=0% &
\		S1(I%),S4(I%)=0. FOR I%=0% TO 10% &
\		PRINT &
\		X9%=X9%+1% &
\		E%=E%+1% &

5070	R=0. &
\	R=R+M(I%) FOR I%=11% TO 10%+N% &
\	S5$=T4$(1%)+"" &
\	GOSUB 5200 &
\	S$=M$(6%) &
\	GOTO 5050 UNLESS FNN%(9%) &

5075	H1=CVT$F(MID(FNL$,7%,8%)) &
\	D1=CVT$F(MID(FNL$,15%,8%)) &
\	GOSUB 5100 &

5080	GOSUB 5400 &
\	GOTO 1020 &

5090	PRINT FOR I%=1% TO 60% &
\	STOP IF FNC%(9%) &
\	P%,E%,T%,X9%=0% &
\	S2(I%),S1(I%),S3(I%),S4(I%)=0. FOR I%=0% TO 10% &
\	T9(I%)=0. FOR I%=1% TO 37% &
\	GOTO 1020 &

5095	PRINT "NO MSTRFL RECORD FOR " ,T4$(1%)," PLEASE CORRECT AND RERUN" &
\	GOTO 5050 UNLESS FNN%(9%) &
\	GOTO 5075 &

5100	! &

5110	PRINT USING "\         \ \                     \" , S$,S2$(2%); &

5120	A=R-F &
\	A=0. IF A<0. &
\	A=S1(0%) IF S1(0%)-A<0. &
\	A=S1(0%)-A &
\	S3=S3+A &
\	GOSUB 5700 &
\	PRINT " "; &

5150	A=S1(1%) &
\	GOSUB 5600 &
\	PRINT " "; &

5160	A=S1(0%) &
\	GOSUB 5700 &
\	PRINT " "; &
\	A=S4(1%) &
\	GOSUB 5600 &
\	PRINT " "; &
\	A=S4(2%) &
\	GOSUB 5600 &
\	PRINT " "; &
\	A=S4(3%) &
\	GOSUB 5700 &
\	PRINT " "; &
\	A=S4(4%) &
\	GOSUB 5600 &
\	PRINT " "; &

5170	A=S1(0%)+S4(3%) &
\	GOSUB 5700 &
\	PRINT " "; &
\	A2=A &
\	A=R-A1 &
\	A=0. IF A<0. &
\	A=A2 IF A2-A<0. &
\	A=A2-A+S1(1%) &
\	S4=S4+A &
\	GOSUB 5700 &
\	PRINT &
\	X9%=X9%+1% &
\	RETURN &

5200	! &

5210	S1(X%)=S1(X%)+T(X%+1%) FOR X%=0% TO 8% &

5220	S2(X%)=S2(X%)+T(X%+1%) FOR X%=0% TO 5% &
\	S4(1%)=S4(1%)+H1 &
\	S4(2%)=S4(2%)+H2 &
\	S4(3%)=S4(3%)+H3 &
\	S4(4%)=S4(4%)+H4 &
\	S5(3%)=S5(3%)+H3 &
\	S5(4%)=S5(4%)+H4 &

5230	S2$(1%)=T4$(1%)+"" &
\	S2$(2%)=M$(2%)+"" &
\	S2$(3%)=T4$(2%)+"" &
\	S2$(4%)=T4$(3%)+"" &

5240	RETURN &

5300	! &

5310	P%=P%+1% &
\	IF P%=1% &
	THEN	PRINT FOR L%=1% TO 69%-X9% &
\		PRINT TAB(23%);"EMPLOYER'S QUARTERLY REPORT" &
\		PRINT TAB(29%);N$;" QUARTER" &
\		PRINT TAB(34%);"19";RIGHT(A$(3%),LEN(A$(3%))-1%) &
\		PRINT &
\		PRINT &
\		X9%=11% &
\		GOTO 5330 &

5320	IF P%<>1% &
	THEN	PRINT &
\		PRINT TAB(33%); &
\		PRINT USING "##" , P%-1% &
\		PRINT FOR L%=1% TO 66%-X9% &
\		X9%=5% &

5330	PRINT TAB(41%);"FICA";TAB(60%);"GROSS";TAB(67%);"REGULAR";TAB(78.); &
		"O. T.";TAB(85.);"  $'S <";TAB(97%);"HOURS <";TAB(109.);"TOTAL"; &
		TAB(117%);"  SUI" &
\	PRINT "   SSN #         NAME";TAB(32%);"        WAGES    "+ &
		"TIPS       WAGES   HOURS    HOURS   MIN. WAGE   MIN. WAGE"+ &
		"  TAXABLE  TAXABLE" &

5340	PRINT &
\	RETURN &

5400	! &

5410	GOSUB 5500 &

5420	PRINT FOR I%=1% TO 66%-X9% &
\	PRINT TAB(27%),"EMPLOYER'S QUARTERLY REPORT" &
\	PRINT &
\	PRINT &
\	PRINT USING "FED I.D. NO. \             \" , A$(1%); &
\	PRINT SPACE$(10%),"DATE QUARTER" &
\	PRINT USING "STATE I.D. NO. \               \" , A$(2%); &
\	PRINT SPACE$(12%); &
\	PRINT USING "ENDED   \                \" , A$(3%) &

5430	PRINT &
\	PRINT &
\	PRINT USING "\                               \" , A$(4%) &
\	PRINT USING "\                            \" , A$(5%) &
\	PRINT USING "\                            \" , A$(6%) &
\	PRINT USING "\                            \" , A$(7%) &
\	PRINT &
\	PRINT &

5440	PRINT "TOTAL PAGES"; &
\	PRINT P%+1% &
\	PRINT "NUMBER OF EMPLOYEES ON REPORT"; &
\	PRINT TAB(46%); &
\	PRINT USING "####" , E% &
\	PRINT "NUMBER OF EMPLOYEES"; &
\	PRINT TAB(46%); &
\	PRINT USING "####" , A% &
\	PRINT &
\	PRINT "*** YEAR TO DATE ***" &
\	PRINT &
\	PRINT "EARNINGS (INCLUDING TIPS)"; &
\	A=0. &
\	A=A+T9(I%) FOR I%=11% TO 10%+N% &
\	A=A+S2(1%) ! QTD=YTD TIPS &
		!	T9(15%) &
\	PRINT TAB(40%); &
\	GOSUB 5700 &
\	PRINT &
\	PRINT "SICK PAY"; &
\	A=0. &
\	PRINT TAB(40%); &
\	GOSUB 5700 &
\	PRINT &
\	PRINT "EXCESS OVER FICA LIMIT"; &
\	A=S3(1%) &
\	PRINT TAB(40%); &
\	GOSUB 5700 &
\	PRINT &

5450	PRINT "EXCESS OVER FUTA LIMIT"; &
\	A=S3(2%) &
\	PRINT TAB(40%); &
\	GOSUB 5700 &
\	PRINT &
\	PRINT "EXCESS OVER STATE UI LIMIT"; &
\	A=S3(3%) &
\	PRINT TAB(40%); &
\	GOSUB 5700 &
\	PRINT &
\	PRINT "FICA"; &
\	A=0. &
\	A=A+T9(I%) FOR I%=22% TO 21%+N% &
\	PRINT TAB(40%); &
\	GOSUB 5700 &
\	PRINT &
\	PRINT "FEDERAL TAXES"; &
\	A=0. &
\	A=A+T9(I%) FOR I%=17% TO 16%+N% &
\	PRINT TAB(40%); &
\	GOSUB 5700 &
\	PRINT &
\	PRINT "STATE TAXES"; &
\	A=0. &
\	A=A+T9(I%) FOR I%=27% TO 26%+N% &
\	PRINT TAB(40%); &
\	GOSUB 5700 &
\	PRINT &
\	PRINT "TIPS (DECLARED)"; &
\	A=S2(1%) ! QTD=YTD TIPS &
		!	T9(15%) &
\	PRINT TAB(40%); &
\	GOSUB 5700 &

5460	PRINT &
\	PRINT &
\	PRINT "*** QUARTER TO DATE ***" &
\	PRINT &
\	PRINT "EARNINGS (INCLUDING TIPS)"; &
\	A=S2(0%)+S2(1%) &
\	PRINT TAB(40%); &
\	GOSUB 5700 &
\	PRINT &
\	PRINT "SICK PAY"; &
\	A=0. &
\	PRINT TAB(40%); &
\	GOSUB 5700 &
\	PRINT &
\	PRINT "FICA"; &
\	A=S2(3%) &
\	PRINT TAB(40%); &
\	GOSUB 5700 &
\	PRINT &
\	PRINT "FEDERAL TAXES"; &
\	A=S2(2%) &
\	PRINT TAB(40%); &
\	GOSUB 5700 &
\	PRINT &
\	PRINT "STATE TAXES"; &
\	A=S2(4%) &
\	PRINT TAB(40%); &
\	GOSUB 5700 &
\	PRINT &
\	PRINT "TIPS (DECLARED)"; &
\	A=S2(1%) &
\	PRINT TAB(40%); &
\	GOSUB 5700 &
\	PRINT &
\	PRINT "MEALS (DEDUCTION)"; &
\	A=S2(5%) &
\	PRINT TAB(40%); &
\	GOSUB 5700 &
\	PRINT &

5470	PRINT &
\	PRINT "*** TAXABLE ***" &
\	PRINT &
\	PRINT "TAXABLE FICA WAGES (EXCLUDES TIPS)"; &
\	A=S3 &
\	PRINT TAB(40%); &
\	GOSUB 5700 &
\	PRINT &
\	PRINT "TAXABLE TIPS"; &
\	A=S2(1%) &
\	PRINT TAB(40%); &
\	GOSUB 5700 &
\	PRINT &
\	PRINT "TAXABLE STATE (INCLUDES TIPS)"; &
\	A=S2(0%) + S2(1%) &
\	PRINT TAB(40%); &
\	GOSUB 5700 &
\	PRINT &

5475	PRINT &
\	PRINT &
\	PRINT &
\	PRINT "TOTAL HOURS WORKED AT A RATE LESS THAN ";R1;"= "; &
\	PRINT USING "##,###.##" , S5(4%) &
\	PRINT "TOTAL DOLLARS IN UNDER PAYMENTS              = "; &
\	PRINT USING "##,###.##" , S5(3%) &
\	PRINT "ADDITIONAL FICA DUE AT FICA RATE OF '";NUM1$(F1);"'   = "; &
\	PRINT USING "##,###.##" , S5(3%)*F1 &
\	PRINT "TAXABLE SUI                                  = "; &
\	PRINT USING "##,###,###.##" , S4 &

5480	PRINT FOR I%=1% TO 25% &
\	STOP IF FNC%(9%) &
\	P%,E%,T%,X9%=0% &
\	S2(I%)=0. FOR I%=0% TO 10% &
\	S3(I%)=0. FOR I%=1% TO 5% &
\	RETURN &

5500	! &

5510	T9(I%)=0. FOR I%=1% TO 36% &
\	STOP IF FNG%(2%,"" ) &

5520	GOSUB 2300 &
\	GOTO 5570 IF M$(1%)="ZPAYRL" &
\	T9(I%)=T9(I%)+M(I%) FOR I%=1% TO 36% &
\	R=0. &
\	R=R+M(I%) FOR I%=11% TO 10%+N% &
\	A=R-F &
\	A=0. IF A<0. &
\	S3(1%)=S3(1%)+A &

5530	A=R+M(15%) &
\	A=A-FUI &
\	A=0. IF A<0. &
\	S3(2%)=S3(2%)+A &

5540	A=R-A1 &
\	A=0. IF A<0. &
\	S3(3%)=S3(3%)+A &

5550	A=R &
\	A=F IF A>F &
\	S3(4%)=S3(4%)+A &

5560	A=R &
\	S3(5%)=S3(5%)+A &

5570	GOTO 5520 IF FNN%(2%)=0% &

5580	RETURN &

5600	IF A=0. &
	THEN	PRINT "     *  "; &
	ELSE	PRINT USING "#,###.##" , A; &

5610	RETURN &

5700	IF A=0. &
	THEN	PRINT "       *  "; &
	ELSE	PRINT USING "###,###.##" , A; &

5710	RETURN &

6000	! &

6010	Y$=FNX$ &
\	GOTO 1000 &

10000	! &

10010	STOP IF FNX%("" ,0%,"" ) &
\	STOP &

14020	DEF FND9$(V%) &
\		FND9$=RIGHT(NUM1$((V% AND 15%)+100%),2%)+"."+ &
			RIGHT(NUM1$((V% AND 31%*16%)/16%+100%),2%)+"."+ &
			RIGHT(NUM1$((SWAP%(V%) AND 254%)/2%+100%),2%) &
\	FNEND &

14350	DEF FNN3(E$) &
\		FNN3=CVT$%(LEFT(E$,2%))+ASCII(MID(E$,3%,1%))/100. &
\	FNEND &

32767	END &

