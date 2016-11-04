1	  EXTEND
10	  ! Program name: [1,8]CONE90 &
	  !  &

20	  DIM M1$(24%), M$(24%), M2$(44%), M(21%), P1$(29%), P$(37%), P(17%), &
		P%(18%), D1$(25%), D$(11%), D(14%), D%(1%), H(8%,2%), &
		P0(8%,2%), F(7%,2%), H1(9%,2%), P1(9%,2%), F1(9%,2%), T(5%), &
		H2(7%,2%), P2(7%,2%), F2(6%,2%), H3(4%,2%), P3(4%,2%), &
		F3(3%,2%) &

35	  ! &

36	  Y$=SYS(CHR$(12%)) &
	\ CHANGE Y$ TO Y% &
	\ PRGNUM$="["+NUM1$(Y%(6%))+","+NUM1$(Y%(5%))+"]" &
	\ PRGNAM$=RAD$(Y%(7%)+SWAP%(Y%(8%)))+RAD$(Y%(9%)+SWAP%(Y%(10%)))+"."+ &
		RAD$(Y%(11%)+SWAP%(Y%(12%))) &
	\ DEVNAM$=CHR$(Y%(23%))+CHR$(Y%(24%))+NUM1$(Y%(25%))+":" &

40	  IF FNX$="" THEN &
		  V%=FNX%(DEVNAM$+PRGNUM$+LEFT(PRGNAM$,3%)+"C90.BAC",10%,"") &

50	  I%=INSTR(1%,FNX$,"*") &
	\ Z$=FNX$ IF FNX$<>"" &
	\ Z$=LEFT(Z$,I%-1%) IF I% &
	\ Z2$=RIGHT(FNX$,I%+1%) IF I% &
	\ ON ERROR GOTO 106 &
	\ OPEN "SS0:UNIQUE.FIL" FOR INPUT AS FILE 1% &
	\ DIM #1%, A0$(255%)=64% &
	\ A0%=INSTR(1%,A0$(15%),"TIPS") &
	\ CLOSE 1% &
	\ GOTO 110 &

106	  A0%=0% &
	\ RESUME 110 &

110	  ON ERROR GOTO 0 &
	\ IF FNO%(2%,"MSTRFL.DAT","","") THEN &
		  PRINT "ERROR";FNS%;"IN OPENING MASTER.  ABORT." &
		\ GOTO 10000 &

130	  INPUT "PAYROLL DATE (MM.DD.YY) ";Z$ IF Z$="" &
	\ GOTO 10000 IF Z$="" &
	\ Z$=FND7$(Z$) &
	\ Z1$=LEFT(Z$,2%)+RIGHT(Z$,4%)+"T" &
	\ IF FND7%(Z$) THEN &
		  PRINT Z$; &
			" IS A BAD DATE!  RE-ENTER OR TYPE <RETURN> TO END." &
		\ Z$="" &
		\ GOTO 130 &

140	  IF FNO%(4%,"PR"+Z1$,"","") THEN &
		  PRINT "PAYROLL FILE FOR ";Z$;" DOES NOT EXIST!" IF FNS%=5% &
		\ Z$="" &
		\ GOTO 130 IF FNS%=5% &
		\ PRINT "ERROR";FNS%;"IN OPENING PAYROLL FILE.  ABORTED." &
		\ GOTO 10000 &

150	  PRINT "Verify computing the earnings and deductions for ";Z$; &
		" (Y/N) <Y>"; &
	\ INPUT K$ &
	\ IF CVT$$(LEFT(K$,1%),-1%)="N" THEN &
		  GOTO 10020 &
	  ELSE &
		  IF FNO%(6%,"TX"+Z1$,"/CR:8,128","") THEN &
			  PRINT "Error";FNS%;"while opening ";Z$; &
				" Aborting. . . ";FNX%("",0%,"") &

160	  OPEN "NL:" AS FILE 1%, RECORDSIZE 128% &
	\ FIELD #1%, 6% AS D1$(1%),1% AS D1$(2%),2% AS D1$(3%),2% AS D1$(4%), &
		2% AS D1$(5%),2% AS D1$(6%),2% AS D1$(7%),2% AS D1$(8%),2% &
		 AS D1$(9%),8% AS D1$(10%),2% AS D1$(11%),8% AS D1$(12%),8% &
		 AS D1$(13%),8% AS D1$(14%),8% AS D1$(15%),8% AS D1$(16%),8% &
		 AS D1$(17%),8% AS D1$(18%),8% AS D1$(19%),8% AS D1$(20%),8% &
		 AS D1$(21%),8% AS D1$(22%),3% AS D1$(23%),3% AS D1$(24%),1% &
		 AS D1$(25%) &
	\ FIELD #1%, 128% AS T6$ &

1000	  ! &

1020	  GOTO 8000 &

2300	  ! &

2310	  F%=FNL% &
	\ FIELD #3%, F% AS E$,6% AS M1$(1%),30% AS M1$(2%),30% AS M1$(3%),30% &
		 AS M1$(4%),30% AS M1$(5%),11% AS M1$(6%),8% AS M1$(7%),1% &
		 AS M1$(8%),2% AS M1$(9%),12% AS M1$(10%),2% AS M1$(11%) &
	\ FIELD #3%, F%+152%+I%*10% AS E$,8% AS M2$(I%),2% AS M1$(I%+11%) &
		FOR I%=1% TO 9% &
	\ FIELD #3%, F%+252% AS E$,8% AS M1$(21%),8% AS M1$(22%) &
	\ FIELD #3%, F%+188%+I%*8% AS E$,8% AS M2$(I%) FOR I%=10% TO 36% &
	\ FIELD #3%, F%+484% AS E$,2% AS M1$(23%) &
	\ FIELD #3%, F%+486%+I%*2% AS E$,2% AS M2$(I%+37%) FOR I%=0% TO 7% &
	\ M$(I%)=M1$(I%)+"" FOR I%=1% TO 23% &
	\ M(I%)=CVT$F(M2$(I%)) FOR I%=1% TO 21% &
	\ RETURN &

2400	  ! &

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

2600	  ! &

2610	  LSET D1$(I%)=D$(I%) FOR I%=1% TO 10% &
	\ LSET D1$(11%)=CVT%$(FND9%(D$(11%))) &
	\ LSET D1$(I%+11%)=CVTF$(D(I%)) FOR I%=1% TO 11% &
	\ LSET D1$(I%+22%)=FNN3$(D(I%+11%)) FOR I%=1% TO 2% &
	\ LSET D1$(25%)=CHR$(D%(1%)) &
	\ RETURN &

8000	  ! &

8015	  PRINT "WORKING.  PLEASE WAIT." &
	\ GOSUB 8800 &
	\ PRINT "FICA IS BEING CALCULATED AS FOLLOWS:" &
	\ PRINT USING "FICA RATE FOR 1990 (EMPLOYEE):   #.####",D7 &
	\ PRINT USING "FICA LIMIT FOR 1990            ####.##",D8 &
	\ PRINT "FEDERAL INCOME TAX WITHHOLDING IS COMPUTED BY" &
	\ PRINT "THE PERCENTAGE METHOD USING THE 1990 TABLE" &
	\ PRINT "PLEASE SPOT CHECK ONE OR TWO EMPLOYEES BY HAND." &
	\ X%=1% &
	\ IF FNG%(4%,"")=0% THEN GOSUB 2410 &
	  ELSE PRINT "NO DATA IN PAYROLL FILE" &
	\ GOTO 10000 &

8020	  T(0%),T(1%),T(3%),T(4%),H1,H2=0. &
	\ B$=P$(1%) &
	\ IF FNG%(2%,B$)=0% THEN &
		  GOSUB 2300 &
	  ELSE &
		  PRINT B$;" cannot be found in the employee master file." &
		\ PRINT "You must resolve this problem before I can continue." &
		\ INPUT "Type a 'cr' to continue. . . ";K$ &
		\ V$=SYS(CHR$(11%)) &
		\ V%=FNX%("",0%,"") &

8030	  ! &

8040	  P(0%)=1. &
	\ P(0%)=1.5 IF LEFT(P$(4%),1%)="H" &
	\ P(0%)=2. IF LEFT(P$(4%),1%)="D" &
	\ T(0%)=T(0%)+FNZ(P(6%)*P(1%))+FNZ(P(7%)*P(1%)*P(0%)) &
	\ T(0%)=T(0%)+M(4%)*(P(6%)+P(7%)) IF LEFT(M$(15%),1%)="#" &
	\ H1=H1+P(6%) &
	\ H2=H2+P(7%) &
	\ T(4%)=T(4%)+P(5%) &
	\ T(3%)=T(3%)+P(I%) FOR I%=3% TO 4% &
	\ IF P$(7%)="SB" THEN &
		  T(1%)=T(1%)+P(5%) &
	  ELSE &
		  T(0%)=T(0%)+P(5%) &

8050	  IF P$(3%)="T" THEN &
		  T(0%)=T(0%)+P(2%) &
	  ELSE &
		  T(1%)=T(1%)+P(2%) &

8070	  IF FNN%(4%) THEN &
		  X%=0% &
		\ GOTO 8100 &

8080	  GOSUB 2410 &
	\ GOTO 8030 IF P$(1%)=B$ &

8100	  ! &

8110	  D$(1%)=B$ &
	\ V%=FNG%(2%,B$) &
	\ GOSUB 2300 &
	\ D$(2%)=M$(8%) &
	\ D$(3%)=M$(9%) &
	\ ON ERROR GOTO 8400 &
	\ T(5%)=VAL(M$(11%)) &
	\ D(1%)=T(0%) &
	\ D(2%)=T(1%) &
	\ D$(I%)=M$(I%+11%) FOR I%=4% TO 9% &
	\ T(2%)=T(0%) &
	\ T(2%)=T(2%)+T(1%) IF A0% &
	\ D(0%)=VAL(M$(9%)) &
	\ X=T(2%)*T(5%)-D6*D(0%) &
	\ ON ERROR GOTO 0 &
	\ IF M$(8%)="M" OR M$(8%)="F" OR M$(8%)="Y" THEN &
		  V%=2% &
	  ELSE &
		  V%=1% &

8130	  FOR I%=1% TO 4% &
		\ GOTO 8140 IF X<F(I%,V%) &
	\ NEXT I% &
	\ I%=5% &

8140	  D(3%)=(H(I%,V%)+P0(I%,V%)*(X-F(I%-1%,V%)))/T(5%) &

8150	  D(4%)=0. &
	\ GOTO 8160 IF M(21%)>=D8 &
	\ IF T(2%)*D7+M(21%)<=D8 THEN &
		  D(4%)=T(2%)*D7 &
	  ELSE &
		  D(4%)=D8-M(21%) &

8160	  D(5%)=0. &
	\ GOTO 9000 IF M$(23%)="CO" &
	\ GOTO 9050 IF M$(23%)="OR" &
	\ GOTO 9100 IF M$(23%)="KY" &
	\ X=D1 &
	\ X=D2 IF V%=2% &

8170	  X=T(2%)*T(5%)-X &
	\ GOTO 8195 IF X<0. &
	\ X=X-D9*D(0%) &
	\ GOTO 8195 IF X<0. &

8180	  FOR I%=1% TO 8% &
		\ GOTO 8190 IF X<F1(I%,V%) &
	\ NEXT I% &
	\ I%=9% &

8190	  X=(H1(I%,V%)+P1(I%,V%)*(X-F1(I%-1%,V%)))/T(5%) &
	\ D(5%)=X IF X>0. &

8195	  D(3%),D(5%)=0. IF M$(8%)="E" OR M$(8%)="X" &
	\ D(4%)=0. IF M$(8%)="X" OR M$(8%)="F" OR M$(8%)="G" &
	\ D(5%)=0. IF M$(23%)="WY" OR M$(23%)="TX" OR M$(23%)="TN" OR M$(23%) &
		="WA" OR M$(23%)="NE" OR M$(23%)="NV" OR M$(23%)="SD" &
	\ D(5%)=.8*D(5%) IF M$(23%)="AL" &
	\ D(5%)=.33*D(3%) IF M$(23%)="UT" &
	\ D(5%)=.06*D(3%) IF M$(23%)="NM" &
	\ D(5%)=.17*D(3%) IF M$(23%)="AZ" &
	\ D(5%)=.25*D(3%) IF M$(23%)="MT" &
	\ D(5%)=.13*D(3%) IF M$(23%)="OH" &
	\ D(5%)=.6*D(5%) IF M$(23%)="CA" &
	\ D(5%)=0. IF M$(8%)="Y" OR M$(8%)="Z" &

8200	  ! &

8210	  D(12%)=T(3%) &
	\ D(13%)=T(4%) &
	\ D%(1%)=0% &
	\ D$(11%)=Z$ &
	\ D(I%)=M(I%-2%) FOR I%=6% TO 11% &
	\ D(I%)=D(I%)*D(1%)/100. IF LEFT(D$(I%-2%),1%)="%" FOR I%=6% TO 11% &
	\ D(I%)=D(I%)*(H1+H2) IF LEFT(D$(I%-2%),1%)="*" OR LEFT(D$(I%-2%),1%) &
		="#" FOR I%=6% TO 11% &
	\ D(I%)=FNZ(D(I%)) FOR I%=1% TO 13% &
	\ D$(10%)="" &
	\ GOSUB 2610 &
	\ IF FNA%(6%,T6$) THEN &
		  PRINT "ERROR";FNS%;"IN ADDING RECORD.  ABORT." &
		\ V%=FNC%(6%) &
		\ KILL "TX"+Z1$ &
		\ KILL "TX"+LEFT(Z1$,7%)+"1" &
		\ GOTO 10000 &

8220	  GOTO 8020 IF X% &
	\ PRINT "DONE." &
	\ GOTO 10020 &

8400	  IF ERL=8110% THEN &
		  PRINT "EMPLOYEE ";M$(1%)+" "+M$(2%) &
		\ PRINT &
			"HAS A BAD NUMBER IN EITHER EXEMPTIONS OR PAY PERIODS:" &
		\ PRINT "PLEASE CORRECT AND RE-CALCULATE" &
		\ PRINT "PAY PERIODS = ";M$(11%) &
		\ PRINT "EXEMPTIONS  = ";M$(9%) &
		\ V%=FNC%(6%) &
		\ KILL "TX"+Z1$ &
		\ KILL "TX"+LEFT(Z1$,7%)+"1" &
		\ RESUME 10000 &

8410	  ON ERROR GOTO 0 &
	\ GOTO 10000 &

8800	  ! &

8810	  D1=0.0 &
	\ D2=0.0 &
	\ D6=2050.		! Pers. Exemption Federal &
	\ D9=2000.		! Pers. Exemption State &
	\ D7=.0765		! FICA percentage &
	\ D8=3924.45		! FICA limit &
	\ COL.BASE=1950. &
	\ ORE.BASE=90. &
	\ KY.DED=650. &
	\ KY.EXEMP=20. &

8815	  READ H(I%,V%) FOR I%=1% TO 5% FOR V%=1% TO 2% &
	\ READ P0(I%,V%) FOR I%=1% TO 5% FOR V%=1% TO 2% &
	\ READ F(I%,V%) FOR I%=0% TO 4% FOR V%=1% TO 2% &
	\ READ H1(I%,V%) FOR I%=1% TO 9% FOR V%=1% TO 2% &
	\ READ P1(I%,V%) FOR I%=1% TO 9% FOR V%=1% TO 2% &
	\ READ F1(I%,V%) FOR I%=0% TO 8% FOR V%=1% TO 2% &
	\ READ H2(I%,V%) FOR I%=1% TO 7% FOR V%=1% TO 2% &
	\ READ P2(I%,V%) FOR I%=1% TO 7% FOR V%=1% TO 2% &
	\ READ F2(I%,V%) FOR I%=0% TO 6% FOR V%=1% TO 2% &
	\ READ H3(I%,V%) FOR I%=1% TO 3% FOR V%=1% TO 2% &
	\ READ P3(I%,V%) FOR I%=1% TO 3% FOR V%=1% TO 2% &
	\ READ F3(I%,V%) FOR I%=0% TO 2% FOR V%=1% TO 2% &
	\ RETURN &

8820	! Federal &
	DATA	0, 0, 2917.5, 10645.5, 31122.0, &
		0, 0, 4867.5, 17733.5, 56940.8, &
		0, .15, .28, .33, .28, &
		0, .15, .28, .33, .28, &
		0, 1200,20650,48250,110300, &
		0, 3400,35850,81800,2006100 &

8821	! Idaho &
	DATA	0,0,20, 60,105,160,225,412.5,1387.5, &
		0,0,40,120,210,320,450,825. ,2775, &
		0,.02,.04,.045,.055,.065,.075,.078,.082, &
		0,.02,.04,.045,.055,.065,.075,.078,.082, &
		0,1000,2000,3000,4000,5000,6000,8500,21000, &
		0,3000,5000,7000,9000,11000,13000,18000,43000 &

8830	! Colorado &
	DATA	0,0,156.75,476.75,1054.25,1459.25,1459.25, &
		0,0,201.00,516.00,1056.00,1633.50,2173.50, &
		0,.0275,.04,.0525,.045,.04,.04, &
		0,.03,.045,.06,.0525,.045,.04, &
		0,1300,7000,15000,26000,35000,35000, &
		0,1300,8000,15000,24000,35000,47000 &

8840	! Oregon &
	DATA	0,0,400, &
		0,0,710, &
		0,.07,.09, &
		0,.07,.09, &
		0,1090,6800, &
		0,2860,13000 &

9000	  ! &

9010	  X=T(2%)*T(5%)-COL.BASE*D(0%) &
	\ GOTO 8195 IF X<2100. &
	\ D(5%) = (X - 1050.0) * 0.05 IF V% = 1% &
	\ D(5%) = (X - 2100.0) * 0.05 IF V% = 2% &
	\ GOTO 8195 &
 ! &
 !	\ GOTO 8195 IF X<0. &
 !	\ GOTO 9020 IF X<F2(I%,V%) FOR I%=1% TO 6% &
 !	\ I%=7% &
 ! &
 ! 9020	  X=(H2(I%,V%)+P2(I%,V%)*(X-F2(I%-1%,V%)))/T(5%) &
 !	\ D(5%)=X IF X>0. &
 !	\ GOTO 8195 &
 !

9050	  ! &

9060	  TEMP%=1% &
	\ TEMP%=2% IF M$(8%)="M" OR D(0%)>=2% &
	\ X1=D(3%)*T(5%) &
	\ X1=3000 IF X1>3000 &
	\ X=T(2%)*T(5%)-ORE.BASE*D(0%)-X1 &
	\ GOTO 8195 IF X<0. &
	\ GOTO 9070 IF X<F3(I%,TEMP%) FOR I%=1% TO 2% &
	\ I%=3% &

9070	  X=(H3(I%,TEMP%)+P3(I%,TEMP%)*(X-F3(I%-1%,TEMP%)))/T(5%) &
	\ D(5%)=X IF X>0. &
	\ GOTO 8195 &

9100	! &

9110	  X=T(2%)*T(5%)-D(3%)*T(5%)-KY.DED &
	\ D(5%)=0. &
	\ Y=3000. &
	\ Y=X IF X<=3000. &
	\ D(5%)=.02*Y &
	\ X=X-3000. &
	\ IF X>0. THEN &
	  Y=1000. &
	\ Y=X IF X<=1000. &
	\ D(5%)=D(5%)+Y*.03 &
	\ X=X-1000. &
	\ IF X>0. THEN &
	  Y=X IF X<=1000. &
	\ D(5%)=D(5%)+Y*.04 &
	\ X=X-1000. &
	\ IF X>0. THEN &
	  Y=3000. &
	\ Y=X IF X<=3000. &
	\ D(5%)=D(5%)+Y*.04 &
	\ X=X-3000 &
	\ IF X>0. THEN &
	  D(5%)=D(5%)+X*.05 &

9120	  D(5%)=D(5%)-KY.EXEMP*D(0%) &
	\ D(5%)=D(5%)/T(5%) &
	\ D(5%)=0. IF D(5%)<0. &
	\ GOTO 8195 &

10000	  ! &

10005	  E$,Z1$="" &
	\ V%=0% &

10010	  CLOSE 1% &
	\ CLOSE 12% &
	\ V%=FNX%(E$,V%,Z1$) &

10020	  V%=FNX%(DEVNAM$+PRGNUM$+LEFT(PRGNAM$,3%)+"C"+RIGHT(PRGNAM$,5%),10%, &
		Z$) &

14010	  DEF FND9%(E$) &
	\ E$="0"+E$ IF INSTR(1%,E$,".")=2% &
	\ E$=LEFT(E$,3%)+"0"+RIGHT(E$,4%) IF INSTR(4%,E$,".")=5% &
	\ FND9%=VAL(LEFT(E$,2%))+VAL(MID(E$,4%,2%))*16%+FND8%(VAL(RIGHT(E$,7% &
		)))*512% &
	\ FNEND &

14020	  DEF FND9$(V%) &
	\ FND9$=RIGHT(NUM1$((V% AND 15%)+100%),2%)+"."+RIGHT(NUM1$((V% AND &
		31%*16%)/16%+100%),2%)+"."+RIGHT(NUM1$((SWAP%(V%) AND 254%)/ &
		2%+100%),2%) &
	\ FNEND &

14030	  DEF FND8%(X) &
	\ FND8%=X &
	\ FNEND &

14040	  DEF FNZ(X) &
	\ FNZ=INT(X*100.+.51)/100. &
	\ FNEND &

14210	  DEF FND7%(E$) &
	\ ON ERROR GOTO 14220 &
	\ GOTO 14220 IF INSTR(1%,E$,".")<>3% OR INSTR(4%,E$,".")<>6% OR &
		INSTR(7%,E$,".")<>0% OR LEN(E$)<>8% &
	\ V%=VAL(LEFT(E$,2%)) &
	\ GOTO 14220 IF V%<1% OR V%>12% &
	\ V%=VAL(MID(E$,4%,2%)) &
	\ GOTO 14220 IF V%<1% OR V%>31% &
	\ V%=VAL(RIGHT(E$,7%)) &
	\ GOTO 14220 IF V%<0% &
	\ FND7%=0% &
	\ GOTO 14230 &

14220	  FND7%=-1% &
	\ RESUME 14230 &

14230	  ON ERROR GOTO 0 &
	\ FNEND &

14260	  DEF FND7$(E$) &
	\ E$=E$+"."+RIGHT(DATE$(0%),8%) IF LEN(E$)<6% &
	\ E$="0"+E$ IF INSTR(1%,E$,".")=2% &
	\ E$=LEFT(E$,3%)+"0"+RIGHT(E$,4%) IF INSTR(4%,E$,".")=5% &
	\ FND7$=E$ &
	\ FNEND &

14310	  DEF FNN3$(X) &
	\ FNN3$=CVT%$(INT(X))+CHR$((X-INT(X))*100.+.51) &
	\ FNEND &

14350	  DEF FNN3(E$) &
	\ FNN3=CVT$%(LEFT(E$,2%))+ASCII(MID(E$,3%,1%))/100. &
	\ FNEND &

15900	  DIM Y%(32%), Y1%(32%), Y$(32%) &

32767	  END
