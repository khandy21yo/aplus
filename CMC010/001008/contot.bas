1	  ! Program name: CONTOT  -  Compiled with SCALE 0 on V07.0 &
	  ! Decompiled on 04-Jan-84 at 08:18 AM by UNBAC Version 1 &
	  ! &

10	  ! &

50	  DIM P1$(29%), P$(9%), P(7%), P%(15%), D1$(25%), D$(11%), D(14%), &
		  E(11%), B(11%) &

60	  F=.0705 &
	\ F1=.0705 &

70	  R=3.35 &

100	  ! &

105	  OPEN "SS0:UNIQUE.FIL" FOR INPUT AS FILE 1% &
	\ DIM #1%, A0$(255%)=64% &
	\ A0%=INSTR(1%,A0$(15%),"TIPS" ) &
	\ CLOSE #1% &
	\ A1$="VACATION" &
	\ A1$="MEALS" IF A0% &

110	  OPEN "NL:" AS FILE 1%, RECORDSIZE 64%+128% &

111	  OPEN "KB:" FOR INPUT AS FILE 12% &

130	  INPUT "PAYROLL DATE (MM.DD.YY) ";Z$ &
	\ GOTO 10000 IF Z$="" &
	\ Z$=FND7$(Z$) &
	\ Z1$=LEFT(Z$,2%)+RIGHT(Z$,4%)+"T" &
	\ IF FND7%(Z$) &
	  THEN	  PRINT "BAD DATE !!! -- RE ENTER (TYPE <RETURN> TO END)" &
		\ GOTO 130 &

140	  IF FNO%(4%,"PR"+Z1$,"/RO" ,"" ) &
	  THEN	  PRINT "PAYROLL FILE DOES NOT EXIST !!!" &
		\ GOTO 10000 &

150	  IF FNO%(6%,"TX"+Z1$,"" ,"" ) &
	  THEN	  PRINT "EARNINGS AND DEDUCTIONS FILE DOES NOT EXIST !!!" &
		\ GOTO 10000 &

200	  ! &

230	  FIELD #1%, 6% AS P1$(1%),6% AS P1$(2%),1% AS P1$(3%),1% AS P1$(4%), &
		  2% AS P1$(5%),2% AS P1$(6%),2% AS P1$(7%),7% AS P1$(8%),2% AS P1$(9%) &
	\ FIELD #1%, 19%+I% AS E$,1% AS P1$(I%) FOR I%=10% TO 24% &
	\ FIELD #1%, 44% AS E$,8% AS P1$(25%),3% AS P1$(26%),3% AS P1$(27%), &
		  3% AS P1$(28%),3% AS P1$(29%) &

240	  FIELD #1%, 64% AS E$,6% AS D1$(1%),1% AS D1$(2%),2% AS D1$(3%), &
		  2% AS D1$(4%),2% AS D1$(5%),2% AS D1$(6%),2% AS D1$(7%), &
		  2% AS D1$(8%),2% AS D1$(9%),8% AS D1$(10%),2% AS D1$(11%), &
		  8% AS D1$(12%),8% AS D1$(13%),8% AS D1$(14%),8% AS D1$(15%), &
		  8% AS D1$(16%),8% AS D1$(17%),8% AS D1$(18%),8% AS D1$(19%), &
		  8% AS D1$(20%),8% AS D1$(21%),8% AS D1$(22%),3% AS D1$(23%), &
		  3% AS D1$(24%),1% AS D1$(25%) &

290	  FIELD #1%, 64% AS T4$,128% AS T6$ &

310	  B(I%),E(I%),T2(I%),T3(I%)=0. FOR I%=1% TO 10% &
	\ E1%=0% &
	\ E5,E6=0. &
	\ L(I%),E(I%)=0. FOR I%=1% TO 8% &
	\ P(I%)=0. FOR I%=1% TO 7% &
	\ P2$,D2$="" &
	\ S$="\"+SPACE$(23%)+"\" &
	\ S1$="###,###.##" &

1000	  ! &

1020	  INPUT "OPTION";O$ &
	\ O$=LEFT(O$,3%) &
	\ GOTO 2000 IF O$="PRI" &
	\ GOTO 10000 IF O$="END" &

1030	  PRINT "	PRINT TOTALS FOR PAYROLL DATE" &
	\ PRINT "	END" &
	\ GOTO 1020 &

2000	  ! &

2010	  STOP IF FNG%(4%,"" ) &
	\ GOSUB 2400 &

2020	  A$=LEFT(P$(1%),6%) &
	\ E1%=E1%+1% &
	\ IF FNG%(6%,A$) &
	  THEN	  PRINT "Earnings and deduction not found for ";A$ &
		\ PRINT "Please correct this problem and run again." &
		\ PRINT "Most likely cause is not computing after" &
		\ PRINT "adding more records to detail." &
		\ GOTO 10000 &

2030	  GOSUB 2500 &
	\ GOSUB 6000 &
	\ GOTO 8000 IF V% &
	\ GOTO 2020 &

2400	  ! &

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

2420	  P(6%),P(7%)=0. &
	\ P(6%)=P(6%)+P%(I%) FOR I%=1% TO 7% &
	\ P(7%)=P(7%)+P%(I%) FOR I%=8% TO 14% &
	\ P(I%)=P(I%)/10. FOR I%=6% TO 7% &
	\ RETURN &

2500	  ! &

2510	  LSET T6$=FNL$ &
	\ D$(I%)=D1$(I%)+"" FOR I%=1% TO 10% &
	\ D$(11%)=FND9$(CVT$%(D1$(11%))) &
	\ D(I%)=CVT$F(D1$(I%+11%)) FOR I%=1% TO 11% &
	\ D(I%+11%)=FNN3(D1$(I%+22%)) FOR I%=1% TO 2% &
	\ D(14%)=D(1%)+D(2%)-D(12%) &
	\ D(14%)=D(14%)-D(I%) FOR I%=3% TO 11% &
	\ RETURN &

5999	  ! &

6000	  X%=1% &

6020	  GOSUB 6900 &
	\ GOSUB 7000 &
	\ V%=FNN%(4%) &
	\ UNLESS V% &
	  THEN	  GOSUB 2400 &
		\ IF A$=P$(1%) &
		  THEN	  X%=X%+1% &
			\ GOTO 6020 &

6030	  GOSUB 7100 &
	\ RETURN IF FNN%(6%) &
	\ GOSUB 2500 &
	\ IF A$=D$(1%) &
	  THEN	  GOTO 6030 &
	  ELSE	  RETURN &

6900	  ! &

6910	  B(10%),B(11%)=0. &
	\ B(1%)=P(6%) &
	\ B(2%)=P(7%) &
	\ B(3%)=P(1%) &
	\ B(4%)=FNZ(B(1%)*P(1%)) &
	\ B(9%)=1. &
	\ B(9%)=1.5 IF P$(4%)="H" &
	\ B(9%)=2. IF P$(4%)="D" &
	\ B(5%)=FNZ(B(9%)*B(2%)*P(1%)) &
	\ B(6%)=P(2%) &

6920	  IF P$(4%)="V" &
	  THEN	  B(10%)=B(1%) &
		\ B(11%)=B(4%) &
		\ B(1%),B(4%)=0. &

6925	  D,H=0. &
	\ IF P(1%)<R &
	  THEN	  M1=R-P(1%) &
		\ D=M1*B(1%) &
		\ H=B(1%) &

6926	  GOTO 6927 IF B(2%)=0. &
	\ IF P(1%)*B(9%)<R &
	  THEN	  M1=R-P(1%)*B(9%) &
		\ D=D+M1*B(2%) &
		\ H=H+B(2%) &

6927	  D1=D1+D &
	\ H1=H1+H &

6930	  B(7%)=0. &
	\ B(7%)=P(5%) UNLESS (P$(7%)="SB" AND A0%) OR (P$(7%)="TP" AND A0%) OR &
		(P$(7%) = "V") &
	\ B(8%)=B(4%)+B(5%)+B(6%)+B(7%) &

6940	  E(I%)=E(I%)+B(I%) FOR I%=1% TO 11% &
	\ RETURN &

7000	  IF P$(5%)="V" &
	  THEN	  E(11%)=E(11%)+P(3%) &
		\ GOTO 7030 &

7010	  I%=(INSTR(1%,D2$,P$(5%))+1%)/2% &
	\ IF I%=0% &
	  THEN	  D2$=D2$+P$(5%) &
		\ I%=LEN(D2$)/2% &

7020	  T3(I%)=T3(I%)+P(3%) &

7030	  IF P$(6%)="V" &
	  THEN	  E(11%)=E(11%)+P(4%) &
		\ GOTO 7050 &

7035	  I%=(INSTR(1%,D2$,P$(6%))+1%)/2% &
	\ IF I%=0% &
	  THEN	  D2$=D2$+P$(6%) &
		\ I%=LEN(D2$)/2% &

7040	  T3(I%)=T3(I%)+P(4%) &

7050	  IF P$(7%)="V" &
	  THEN	  E(11%)=E(11%)+P(5%) &
		\ GOTO 7100 &

7055	  I%=(INSTR(1%,P2$,P$(7%))+1%)/2% &
	\ IF I%=0% &
	  THEN	  P2$=P2$+P$(7%) &
		\ I%=LEN(P2$)/2% &

7060	  T2(I%)=T2(I%)+P(5%) &
	\ RETURN &

7100	  I%=(INSTR(1%,D2$,D$(6%))+1%)/2% &
	\ IF I%=0% &
	  THEN	  D2$=D2$+D$(6%) &
		\ I%=LEN(D2$)/2% &

7102	  T3(I%)=T3(I%)+D(8%) &

7140	  I%=(INSTR(1%,D2$,D$(7%))+1%)/2% &
	\ IF I%=0% &
	  THEN	  D2$=D2$+D$(7%) &
		\ I%=LEN(D2$)/2% &

7150	  T3(I%)=T3(I%)+D(9%) &

7160	  I%=(INSTR(1%,D2$,D$(8%))+1%)/2% &
	\ IF I%=0% &
	  THEN	  D2$=D2$+D$(8%) &
		\ I%=LEN(D2$)/2% &

7170	  T3(I%)=T3(I%)+D(10%) &

7180	  I%=(INSTR(1%,D2$,D$(9%))+1%)/2% &
	\ IF I%=0% &
	  THEN	  D2$=D2$+D$(9%) &
		\ I%=LEN(D2$)/2% &

7190	  T3(I%)=T3(I%)+D(11%) &

7200	  L(I%)=L(I%)+D(I%) FOR I%=1% TO 7% &
	\ RETURN &

8000	  INPUT "SET PAGE";O$ &
	\ PRINT FOR I%=1% TO 5% &
	\ PRINT "TOTALS FOR PAYROLL DATE ";Z$ &
	\ PRINT &
	\ PRINT TAB(30%);"          DEBIT    CREDIT    GENERAL LEDGER #" &
	\ PRINT &
	\ PRINT &
	\ PRINT USING "NUMBER OF EMPLOYEES PAID        ###" , E1% &
	\ PRINT USING S$,"REGULAR HOURS"; &
	\ A=E(1%) &
	\ GOSUB 8700 &
	\ PRINT USING S$,"OVERTIME HOURS"; &
	\ A=E(2%) &
	\ GOSUB 8700 &
	\ PRINT USING S$,"VACATION HOURS"; &
	\ A=E(10%) &
	\ GOSUB 8700 &
	\ PRINT USING S$,"REGULAR WAGES"; &
	\ A=E(4%) &
	\ GOSUB 8700 &
	\ PRINT USING S$,"OVERTIME WAGES"; &
	\ A=E(5%) &
	\ GOSUB 8700 &
	\ PRINT USING S$,"VACATION EARNINGS"; &
	\ A=E(11%) &
	\ GOSUB 8700 &
	\ PRINT USING S$,"OTHER EARNINGS"; &
	\ A=E(7%)+E(6%) &
	\ GOSUB 8700 &
	\ GOSUB 8800 IF A0% &
	\ PRINT &
	\ PRINT &

8002	  TDEP = 0.0 &
	\ PRINT USING S$,"DEPOSITS:" &
	\ PRINT USING S$,"  Fed. Withholding"; &
	\ A = L(3%) &
	\ TDEP = TDEP + A &
	\ GOSUB 8700 &
	\ PRINT USING S$,"  FICA Witholding"; &
	\ A = L(4%) &
	\ TDEP = TDEP + A &
	\ GOSUB 8700 &
	\ PRINT USING S$,"  FICA Employer"; &
	\ A = L(4%) &
	\ TDEP = TDEP + A &
	\ GOSUB 8700 &
	\ PRINT USING S$,"  Less: FICA on tips"; &
	\ A = -(F1 * L(2%)) &
	\ A = 0.0 UNLESS A0% &
	\ TDEP = TDEP + A &
	\ GOSUB 8700 &
	\ PRINT USING S$,"  Add: FICA on under pmnt."; &
	\ A = F1 * D1 &
	\ A = 0.0 UNLESS A0% &
	\ TDEP = TDEP + A &
	\ GOSUB 8700 &
	\ PRINT USING S$,"TOTAL DEPOSIT"; &
	\ A =  TDEP &
	\ GOSUB 8700 &
	\ PRINT &

8010	  PRINT USING S$, "STATE DEPOSIT"; &
	\ A=L(5%) &
	\ GOSUB 8700 &
	\ PRINT &
	\ PRINT &
	\ GOSUB 8400 &
	\ A2$="WAGES" &
	\ A2$=A2$+" & MEALS" IF A0% &
	\ PRINT USING S$, A2$ &
	\ A=E(8%)-A1+E(11%) &
	\ GOSUB 8755 &
	\ PRINT USING S$, "FICA"; &
	\ A=L(4%) &
	\ GOSUB 8750 &
	\ PRINT USING S$, "FEDERAL TAXES"; &
	\ A=L(3%) &
	\ GOSUB 8750 &
	\ PRINT USING S$, "STATE TAXES"; &
	\ A=L(5%) &
	\ GOSUB 8750 &
	\ PRINT USING S$, A1$ &
	\ A=L(6%) &
	\ GOSUB 8750 &
	\ PRINT USING S$, "INSURANCE"; &
	\ A=L(7%) &
	\ GOSUB 8750 &
	\ PRINT &
	\ PRINT &
	\ GOTO 8450 &

8400	  A1=0. &
	\ FOR S6%=1% TO LEN(P2$)/2% &
		\ GOTO 8420 IF T2(S6%)=0. &
		\ M3$=MID(P2$,2.*S6%-1%,2%) &
		\ GOTO 8420 IF M3$="SB" AND A0%<>0% OR M3$="TP" AND A0%<>0% &
		\ PRINT "EARNINGS CODE #";M3$; IF M3$<>"" &
		\ A=T2(S6%) &
		\ A1=A1+A &
		\ GOSUB 8755 &

8420	  NEXT S6% &
	\ RETURN &

8450	  E3=0. &
	\ FOR S6%=1% TO LEN(D2$)/2% &
		\ GOTO 8470 IF T3(S6%)=0. &
		\ M4$=MID(D2$,2%*S6%-1%,2%) &
		\ GOTO 8470 IF M4$="ME" AND A0%<>0% &
		\ PRINT "DEDUCTION CODE #";M4$; IF M4$<>"" &
		\ A=T3(S6%) &
		\ E3=E3+A &
		\ GOSUB 8750 &

8470	  NEXT S6% &

8490	  PRINT &
	\ PRINT &
	\ PRINT "NET PAY"; &
	\ A=L(1%)-L(4%)-L(3%)-L(5%)-L(6%)-L(7%)-E3 &
	\ A=A+L(2%) UNLESS A0% &
	\ GOSUB 8750 &
	\ PRINT "BALANCE";TAB(36%); &
	\ PRINT USING S1$+"  "+S1$, E6,E5 &
	\ PRINT "NOTE BALANCE PROBLEM ABOVE !!!" IF FNZ(E5)<>FNZ(E6) &
	\ PRINT &
	\ PRINT &
	\ IF A0% &
	  THEN	  PRINT USING S$, "TIPS DECLARED"; &
		\ A=L(2%) &
		\ GOSUB 8700 &
		\ PRINT &
		\ PRINT &
		\ PRINT &

8495	  GOTO 8500 IF H1=0. AND D1=0. &
	\ PRINT "TOTAL HOURS WORKED AT A RATE LESS THAN";R;"="; &
	\ PRINT USING "##,###.##" , H1 &
	\ PRINT "TOTAL DOLLARS IN UNDER PAYMENTS             ="; &
	\ PRINT USING "##,###.##" , D1 &
	\ PRINT "ADDITIONAL FICA DUE AT FICA RATE OF '";NUM1$(F1);"' ="; &
	\ PRINT USING "##,###.##" , D1*F1 &

8500	  GOTO 10000 &

8700	  IF A=0. &
	  THEN	  PRINT &
	  ELSE	  PRINT USING S1$, A &
		\ RETURN &

8710	  RETURN &

8720	  IF A=0. &
	  THEN	  PRINT &
	  ELSE	  PRINT USING SPACE$(10%)+S1$, A &
		\ RETURN &

8730	  RETURN &

8750	  IF A=0. &
	  THEN	  PRINT &
	  ELSE	  PRINT USING TAB(48%)+S1$, A; &
		\ E5=E5+A &
		\ PRINT TAB(60%);"________________" &
		\ RETURN &

8755	  IF A=0. &
	  THEN	  PRINT &
	  ELSE	  PRINT USING TAB(36%)+S1$, A; &
		\ E6=E6+A &
		\ PRINT TAB(60%);"________________" &
		\ RETURN &

8760	  RETURN &

8800	  FOR S6%=1% TO LEN(D2$)/2% &
		\ IF T3(S6%)<>0. &
		  THEN	  IF MID(D2$,2.*S6%-1%,2%)="ME" &
			  THEN	  A=T3(S6%) &
				\ PRINT USING S$, "MEALS"; &
				\ GOSUB 8700 &
				\ RETURN &

8810	  NEXT S6% &
	\ RETURN &

10000	  ! &

10010	  V%=FNX%("" ,0%,"" ) &

14000	  ! &

14005	  DEF FNZ(A) &
		\ FNZ=INT(A*100.+.51)/100. &
	\ FNEND &

14020	  DEF FND9$(I%) &
		\ FND9$=RIGHT(NUM1$((I% AND 15%)+100%),2%)+"."+ &
			  RIGHT(NUM1$((I% AND 31%*16%)/16%+100%),2%)+"."+ &
			  RIGHT(NUM1$((SWAP%(I%) AND 254%)/2%+100%),2%) &
	\ FNEND &

14200	  ! &

14210	  DEF FND7%(E$) &
		\ ON ERROR GOTO 14220 &
		\ GOTO 14220 IF INSTR(1%,E$,"." )<>3% OR INSTR(4%,E$, &
			  "." )<>6% OR INSTR(7%,E$,"." )<>0% OR LEN(E$)<>8% &
		\ D7%=VAL(LEFT(E$,2%)) &
		\ GOTO 14220 IF D7%<1% OR D7%>12% &
		\ D7%=VAL(MID(E$,4%,2%)) &
		\ GOTO 14220 IF D7%<1% OR D7%>31% &
		\ D7%=VAL(RIGHT(E$,7%)) &
		\ GOTO 14220 IF D7%<0% &
		\ FND7%=0% &
		\ GOTO 14230 &

14220		  FND7%=-1% &
		\ RESUME 14230 &

14230		  ON ERROR GOTO 0 &
	\ FNEND &

14250	  ! &

14260	  DEF FND7$(E$) &
		\ E$=E$+"."+RIGHT(DATE$(0%),8%) IF LEN(E$)<6% &
		\ E$="0"+E$ IF INSTR(1%,E$,"." )=2% &
		\ E$=LEFT(E$,3%)+"0"+RIGHT(E$,4%) IF INSTR(4%,E$,"." )=5% &
		\ FND7$=E$ &
	\ FNEND &

14300	  ! &

14350	  DEF FNN3(E$) &
		\ FNN3=CVT$%(LEFT(E$,2%))+ASCII(MID(E$,3%,1%))/100. &
	\ FNEND &

32767	  END &
	&
	&

