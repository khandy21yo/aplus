10	!*ISAM3 &
	! PRREP - CONSTRUCTION PAYROLL REPORT &
	! &
	! 07/10/79 - MODIFIED BY BILL LOVEGROVE FOR GENERAL SYSTEM &
	! 08/07/79 - MODIFIED BY KEVIN HANDY -- FORMAT CHANGES &
	! &

50	  DIM M1$(23%),M$(23%),P1$(29%),P$(9%),P%(17%),P(7%), &
		  D1$(25%),D(14%),T(7%),T1(7%),T2(7%) &

60	  IF FNO%(9%,"PRREP.TMP","","") &
	  THEN	  PRINT"A TEMPORARY FILE DOES NOT YET EXIST." &
	  ELSE	  IF FNG%(9%,"")=0% AND LEFT(FNL$,14%)=STRING$(14%,0%) &
		  THEN	  Z$=MID(FNL$,15%,8%) &
			\ PRINT"A FILE HAS BEEN CREATED FOR ";Z$ &
	\ INPUT "DO YOU PLAN TO PRINT WITH THIS PAYROLL DATE (Y OR N)",K$ &
			\ IF K$="N" &
			  THEN	  Z$="" &
				\ STOP IF FNC%(9%) &

100	! &
	! OPEN FILES TO FIELD ON &
	! &

110	  OPEN "KB:" AS FILE 1%, RECORDSIZE 512%+64%+128% &

120	  IF FNO%(2%,"MSTRFL.DAT","/RO","") &
	  THEN	  PRINT "MASTER FILE NOT FOUND !!!" &
		\ GOTO 10000 &

130	  PRINT &
	\ INPUT "PAYROLL DATE (MM.DD.YY) ";Z$ IF Z$="" &
	\ GOTO 10000 IF Z$="" &
	\ Z$=FND7$(Z$) &
	\ Z1$=LEFT(Z$,2%)+RIGHT(Z$,4%)+"T" &
	\ IF FND7%(Z$) &
	  THEN	  PRINT "BAD DATE !!! -- RE ENTER (TYPE <RETURN> TO END)" &
		\ Z$="" &
		\ GOTO 130 &

140	  IF FNO%(4%,"PR"+Z1$,"/RO","") &
	  THEN	  PRINT "PAYROLL FILE DOES NOT EXIST !!!" &
		\ Z$="" &
		\ GOTO 130 &

150	  IF FNO%(6%,"TX"+Z1$,"/RO","") &
	  THEN	  PRINT "EARNINGS AND DEDUCTIONS FILE DOES NOT EXIST !!!" &
		\ GOTO 10000 &

200	! &
	! FIELD STATEMENTS &
	! &

220	  FIELD#1%, 6% AS M1$(1%), 30% AS M1$(2%),30% AS M1$(3%), &
		  30% AS M1$(4%),30% AS M1$(5%),11% AS M1$(6%),8% AS M1$(7%), &
		  1% AS M1$(8%),2% AS M1$(9%), 12% AS M1$(10%), &
		  2% AS M1$(11%) &

221	  FIELD#1%,160%+I%*10% AS E$,2% AS M1$(I%+11%) FOR I%=1% TO 9% &
	\ FIELD#1%,252% AS E$,8% AS M1$(21%),8% AS M1$(22%) &
	\ FIELD#1%,484% AS E$,2% AS M1$(23%) &
	! MSTRFL.DAT &

230	  FIELD#1%, 512% AS E$, 6% AS P1$(1%),6% AS P1$(2%),1% AS P1$(3%), &
		  1% AS P1$(4%),2% AS P1$(5%),2% AS P1$(6%),2% AS P1$(7%), &
		  7% AS P1$(8%),2% AS P1$(9%) &
	\ FIELD#1%, 512%+19%+I% AS E$, 1% AS P1$(I%) FOR I%=10% TO 24% &
	\ FIELD#1%, 512%+44% AS E$,8% AS P1$(25%),3% AS P1$(26%), &
		  3% AS P1$(27%),3% AS P1$(28%), 3% AS P1$(29%) &
	! PAYROL.DAT &

240	  FIELD#1%, 512%+64% AS E$, 6% AS D1$(1%),1% AS D1$(2%),2% AS D1$(3%), &
		  2% AS D1$(4%),2% AS D1$(5%), 2% AS D1$(6%),2% AS D1$(7%), &
		  2% AS D1$(8%), 2% AS D1$(9%), 8% AS D1$(10%), &
		  2% AS D1$(11%),8% AS D1$(12%),8% AS D1$(13%), &
		  8% AS D1$(14%),8% AS D1$(15%),8% AS D1$(16%), &
		  8% AS D1$(17%),8% AS D1$(18%),8% AS D1$(19%),8% AS D1$(20%), &
		  8% AS D1$(21%),8% AS D1$(22%),3% AS D1$(23%),3% AS D1$(24%), &
		  1% AS D1$(25%) &
	! ERNDED.DAT &

290	  FIELD #1%,512% AS T2$,64% AS T4$, 128% AS T6$ &

404	  U5$="####.# ###.# #####.## ####.## ####.## ##,###.##" &

990	  DEF FNZ(Z2)=INT(Z2*100.+.51)/100. &

1000	! &
	! PROGRAM CONTROL SECTION &
	! &

1020	  PRINT &
	\ INPUT "Option ";K$ &
	\ K$=LEFT(K$,3%) &
	\ GOTO 6000 IF K$="CER" &
	\ GOTO 6000 IF K$="SEL" &
	\ GOTO 7000 IF K$="CRE" &
	\ GOTO 8000 IF K$="JOB" &
	\ GOTO 10000 IF K$="END" &

1030	  PRINT "OPTIONS ARE:" &
	\ PRINT"   CREATE TEMPORARY FILE" &
	\ PRINT"   JOB COST" &
	\ PRINT"   CERTIFIED REPORT" &
	\ PRINT"   SELECTIVE CERTIFIED REPORT" &
	\ PRINT"   END PROGRAM" &
	\ PRINT &
	\ GOTO 1020 &

2300	! &
	! SEPARATE MASTER FILE &
	! &

2310	  LSET T2$=FNL$ &
	\ M$(I%)=M1$(I%)+"" FOR I%=1% TO 23% &
	\ RETURN &

2400	! &
	!	SEPARATE PAYROLL &
	! &

2410	  LSET T4$=FNL$ &
	\ P$(I%)=P1$(I%)+"" FOR I%=1% TO 8% &
	\ P%(I%)=ASCII(P1$(I%+9%)) FOR I%=1% TO 15% &
	\ P(1%)=CVT$F(P1$(25%)) &
	\ P(I%)=FNN3(P1$(I%+24%)) FOR I%=2% TO 5% &
	\ P%(0%)=CVT$%(P1$(9%)) &
	\ P$(9%)=FND9$(P%(0%)) &
	\ IF P%(7%)=255% &
	  THEN	  P(6%)=FNN3(P1$(13%)+P1$(14%)+P1$(15%)) &
		\ P(7%)=FNN3(P1$(20%)+P1$(21%)+P1$(22%)) &
		\ P%(I%)=0% FOR I%=1% TO 14% &
		\ RETURN &

2420	  P(6%),P(7%)=0% &
	\ P(6%)=P(6%)+P%(I%) FOR I%=1% TO 7% &
	\ P(7%)=P(7%)+P%(I%) FOR I%=8% TO 14% &
	\ P(I%)=P(I%)/10. FOR I%=6% TO 7% &
	\ RETURN &

2500	! &
	! SEPARATE ERNDED FILE &
	! &

2510	  LSET T6$=FNL$ &
	\ D(I%)=CVT$F(D1$(I%+11%)) FOR I%=1% TO 11% &
	\ D(I%+11%)=FNN3(D1$(I%+22%)) FOR I%=1% TO 2% &
	\ D(14%)=D(1%)+D(2%)-D(12%) &
	\ D(14%)=D(14%)-D(I%) FOR I%=3% TO 11% &
	\ RETURN &

6000	! &
	! CHAIN TO [1,8]CONCER TO PRINT CERTIFIED REPORTS &
	! &

6010	  I%=6010% &
	\ I%=6020% IF K$="SEL" &
	\ V%=FNX%("[1,8]CONCER.BAC",I%,"") &

7000	! &
	!  CREATE PRREP.TMP FILE &
	! &

7001	  ON ERROR GOTO 7730 &
	\ OPEN "SS0:UNIQUE.FIL/RO" AS FILE 12% &
	\ DIM #12%,A0$(255%)=64% &
	\ J%=INSTR(1%,A0$(15%),"LEN=") &
	\ J%=VAL(MID(A0$(15%),J%+4%,1%)) IF J% &
	\ CLOSE #12% &

7005	  ON ERROR GOTO 0 &
	\ V%=FNC%(9%) &

7007	  IF FNO%(9%,"PRREP.TMP","/CR:16,64","") &
	  THEN	  PRINT"ERROR IN FILE CREATE." &
		\ GOTO 10000 &

7010	  STOP IF FNA%(9%,STRING$(14%,0%)+Z$) &
	\ GOTO 10000 IF FNG%(4%,"") &

7020	  LSET T4$=FNL$ &
	\ B1$=P1$(2%)+"" &
	\ B1$=LEFT(B1$,J%) IF J% &
	\ B1$=B1$+SPACE$(6%-LEN(B1$)) &
	\ V%=FNA%(9%,B1$+LEFT(T4$,6%)+RIGHT(FNL$,13%)) &
	\ STOP IF V% &
	\ PRINT"!"; &
	\ PRINT IF POS(0%)>70. &

7030	  V%=FNN%(4%) &
	\ GOTO 7020 IF V%=0% &
	\ STOP IF V%<>11% &

7040	  CLOSE 1% &
	\ V%=FNX%("[1,8]CONPRR",10%,"") &

7140	  GOSUB 2400 &
	\ B1$=P$(1%) &
	\ A$=P$(2%) &
	\ P$(1%)=A$ &
	\ P$(2%)=B1$ &

7150	  V%=FNG%(2%,A$) &
	\ STOP IF V% &
	\ GOSUB 2300 &
	\ V%=FNG%(6%,A$) &
	\ STOP IF V% &
	\ GOSUB 2500 &
	\ RETURN &

7700	!COMPUTE TOTAL EARNINGS FROM PAYROL RECORD FOR ONE JOB &

7701	  D=1. &
	\ D=2. IF P$(4%)="D" &
	\ D=1.5 IF P$(4%)="H" &
	\ A1=P(6%) &
	\ A2=P(7%) &
	\ T(5%)=P(5%)+P(2%) &
	\ T(1%)=A1 &
	\ T(2%)=A2 &
	\ T(3%)=FNZ((A1+A2)*P(1%)) &
	\ T(4%)=(A2*(D-1)*P(1%)) &
	\ T(7%)=A2*(D-1) &

7720	  RETURN &

7730	  J%=0% &
	\ RESUME 7005 &

8000	! &
	! PRINT JOB REPORT &
	! &

8005	  GOTO 10000 IF FNG%(9%,"")+FNN%(9%) &
	\ L%=66% &
	\ B2$="" &
	\ P1%=0% &
	\ INPUT"SET PAGE ";K$ &

8010	  GOSUB 7140 &
	\ IF B2$<>B1$ &
	  THEN	  GOSUB 8315 &
		\ B2$=B1$ &
		\ D1$=Z$ &

8020	  GOSUB 8195 IF L%>55% &
	\ GOSUB 8200 &

8030	  V%=FNN%(9%) &
	\ GOTO 8010 IF V%=0% &
	\ STOP IF V%<>11% &
	\ GOSUB 8345 &
	\ GOTO 10000 &

8170	! SUB TO PRINT JOB DETAIL &

8195	  P1%=P1%+1% &
	\ PRINT STRING$(65%-L%,10%) &
	\ PRINT TAB(29%);"JOB COST REPORT ";D1$;TAB(63%);"PAGE";P1% IF L%<>44% &
	\ PRINT" JOB   EMP #       NAME             REG    OT "; &
		  "  STAND.   PREM.   OTHER     GROSS" &

8196	  L%=2% &
	\ RETURN &

8200	  PRINT P$(2%);" ";P$(1%); &

8210	  PRINT LEFT(M$(2%),20%); &

8212	  GOSUB 7700 &

8213	! PRINT ONE LINE FOR EACH EARNING ENTERY &

8214	  PRINT USING U5$,T(1%),T(2%),T(3%),T(4%),T(5%),T(3%)+T(4%)+T(5%) &

8235	  L%=L%+1% &

8240	  T1(I%)=T1(I%)+T(I%) FOR I%=1% TO 7% &

8295	  RETURN &

8315	! SUB TO PRINT JOB TOTALS &

8320	  RETURN IF B2$="" &
	\ T2(I%)=T2(I%)+T1(I%) FOR I%=1% TO 7% &

8325	  PRINT "************** TOTALS ***"; &
	\ PRINT TAB(33); &
	\ PRINT USING U5$, T1(1%),T1(2%),T1(3%),T1(4%),T1(5%),T1(3%)+T1(4%)+ &
		  T1(5%) &
	\ PRINT &

8330	  T1(I%)=0. FOR I%=1% TO 7% &

8335	  L%=L%+2% &
	\ RETURN &

8345	! SUB TO PRINT GRAND TOTAL &

8350	  GOSUB 8315 &
	\ PRINT &
	\ PRINT "************** GRAND TOTALS ***"; &

8360	  PRINT TAB(33); &
	\ PRINT USING U5$,T2(1%),T2(2%),T2(3%),T2(4%),T2(5%),T2(3%)+T2(4%)+ &
		  T2(5%) &
	\ PRINT &
	\ RETURN &

10000	! &
	!  TERMINATE PROGRAM &
	! &

10010	  CLOSE 1% &
	\ V%=FNX%("",0%,"") &

14000	! &
	! STORE AND RETRIEVE DATE IN INTEGER (D9% <=> MM.DD.YY) &
	! &

14020	  DEF FND9$(D9%) &
		\ FND9$=RIGHT(NUM1$((D9% AND 15%)+100%),2%)+"."+ &
			  RIGHT(NUM1$((D9% AND 31%*16%)/16%+100%),2%)+"."+ &
			  RIGHT(NUM1$(((SWAP%(D9%) AND 254%)/2%)+100%), &
			  2%) &
	\ FNEND &

14200	! &
	! CHECK FOR VALID DATE &
	! &

14210	  DEF FND7%(D7$) &
		\ ON ERROR GOTO 14220 &
		\ GOTO 14220 IF INSTR(1%,D7$,".")<>3% OR INSTR(4%,D7$, &
			  ".")<>6% OR INSTR(7%,D7$,".")<>0% OR LEN(D7$)<>8% &
		\ D7%=VAL(LEFT(D7$,2%)) &
		\ GOTO 14220 IF D7%<1% OR D7%>12% &
		\ D7%=VAL(MID(D7$,4%,2%)) &
		\ GOTO 14220 IF D7%<1% OR D7%>31% &
		\ D7%=VAL(RIGHT(D7$,7%)) &
		\ GOTO 14220 IF D7%<0% &
		\ FND7%=0% &
		\ GOTO 14230 &

14220		  FND7%=-1% &
		\ RESUME 14230 &

14230		  ON ERROR GOTO 0 &
	\ FNEND &

14250	! &
	! FORMAT DATE TO MM.DD.YY , FILL WITH ZEROS &
	! &

14260	  DEF FND7$(D7$) &
		\ D7$=D7$+"."+RIGHT(DATE$(0%),8%) IF LEN(D7$)<6% &
		\ D7$="0"+D7$ IF INSTR(1%,D7$,".")=2% &
		\ D7$=LEFT(D7$,3%)+"0"+RIGHT(D7$,4%) IF INSTR(4%,D7$,".")=5% &
		\ FND7$=D7$ &
	\ FNEND &

14300	! &
	! FLOATING POINT NUMBER TO THREE CHARACTERS AND BACK &
	! &

14350	  DEF FNN3(N3$)=CVT$%(LEFT(N3$,2%))+ASCII(MID(N3$,3%,1%))/100. &

32767	  END &

