1	  ! &
	  ! Program name: conpru		Compiled with SCALE 0 on V07.0 &
	  ! Decompiled on 28-Nov-16 at 04:11 PM
10	  !
50	  DIM M1$(24%), M$(24%), M2$(44%), M(44%), D1$(25%), D$(11%), D(14%), D%(1%), P1$(29%), P$(9%), P%(15%)
100	  !
110	  OPEN "KB:" AS FILE 1%, RECORDSIZE 512%+64%+128%
120	  IF FNO%(2%,"MSTRFL.DAT","","") THEN  &
		  PRINT "MASTER FILE NOT FOUND !!!" &
		\ GOTO 10000
130	  INPUT "PAYROLL DATE (MM.DD.YY) ";Z$ &
	\ GOTO 10000 IF Z$="" &
	\ Z$=FND7$(Z$) &
	\ Z1$=LEFT(Z$,2%)+RIGHT(Z$,4%)+"T" &
	\ IF FND7%(Z$) THEN  &
		  PRINT "BAD DATE !!! -- RE ENTER (TYPE <RETURN> TO END)" &
		\ GOTO 130
140	  IF FNO%(4%,"PR"+Z1$,"","") THEN  &
		  PRINT "PAYROLL FILE DOES NOT EXIST !!!" &
		\ GOTO 10000
150	  V%=FNC%(6%) &
	\ IF FNO%(6%,"TX"+Z1$,"","") THEN  &
		  PRINT "EARNINGS AND DEDUCTIONS FILE DOES NOT EXIST !!!" &
		\ GOTO 10000
200	  !
220	  FIELD #1%, 6% AS M1$(1%),30% AS M1$(2%),30% AS M1$(3%),30% AS M1$(4%),30% AS M1$(5%),11% AS M1$(6%),8% AS M1$(7%),1% AS M1$(8%),2% AS M1$(9%),12% AS M1$(10%),2% AS M1$(11%)
221	  FIELD #1%, 152%+I%*10% AS E$,8% AS M2$(I%),2% AS M1$(I%+11%) FOR I%=1% TO 9% &
	\ FIELD #1%, 252% AS E$,8% AS M1$(21%),8% AS M1$(22%) &
	\ FIELD #1%, 188%+I%*8% AS E$,8% AS M2$(I%) FOR I%=10% TO 36% &
	\ FIELD #1%, 484% AS E$,2% AS M1$(23%) &
	\ FIELD #1%, 486%+I%*2% AS E$,2% AS M2$(I%+37%) FOR I%=0% TO 7%
230	  FIELD #1%, 512% AS E$,6% AS P1$(1%),6% AS P1$(2%),1% AS P1$(3%),1% AS P1$(4%),2% AS P1$(5%),2% AS P1$(6%),2% AS P1$(7%),7% AS P1$(8%),2% AS P1$(9%) &
	\ FIELD #1%, 512%+19%+I% AS E$,1% AS P1$(I%) FOR I%=10% TO 24% &
	\ FIELD #1%, 512%+44% AS E$,8% AS P1$(25%),3% AS P1$(26%),3% AS P1$(27%),3% AS P1$(28%),3% AS P1$(29%)
240	  FIELD #1%, 512%+64% AS E$,6% AS D1$(1%),1% AS D1$(2%),2% AS D1$(3%),2% AS D1$(4%),2% AS D1$(5%),2% AS D1$(6%),2% AS D1$(7%),2% AS D1$(8%),2% AS D1$(9%),8% AS D1$(10%),2% AS D1$(11%),8% AS D1$(12%),8% AS D1$(13%),8% AS D1$(14%),8% AS D1$(15%),8% AS D1$(16%),8% AS D1$(17%),8% AS D1$(18%),8% AS D1$(19%),8% AS D1$(20%),8% AS D1$(21%),8% AS D1$(22%),3% AS D1$(23%),3% AS D1$(24%),1% AS D1$(25%)
290	  FIELD #1%, 512% AS T2$,64% AS T4$,128% AS T6$
1000	  !
1010	  IF FNG%(2%,"ZPAYRL") THEN  &
		  V%=FNA%(2%,"ZPAYRL")
1015	  GOSUB 2300
1016	  V%=FNG%(6%,"") &
	\ PRINT "FILE EMPTY." IF V% &
	\ GOTO 130 IF V% &
	\ GOSUB 2500 &
	\ IF D%(1%) AND 1% THEN  &
		  F%=-1%
1020	  K$="N" IF F%=-1% &
	\ PRINT "PAYROLL HAS BEEN UPDATED. IF F% &
	\ C$="Y" &
	\ INPUT "CALCULATE TOTALS ANYWAY (Y OR N)";C$ IF F%=-1% &
	\ F%=0% IF C$="REDO" &
	\ C$="Y" IF C$="REDO" &
	\ GOTO 10000 IF LEFT(C$,1%)<>"Y" &
	\ I1%=(VAL(LEFT(Z$,2%))+2%)/3% &
	\ T(I%)=0. FOR I%=1% TO 9% &
	\ INPUT "SHALL THE MASTERFILE BE UPDATED (Y OR N)";K$ UNLESS F%=-1% &
	\ K$=LEFT(K$,1%)
1030	  PRINT "UPDATING. . ." IF K$="Y" &
	\ PRINT "CALCULATING TOTALS. . ." IF K$<>"Y"
1040	  PRINT "!"; &
	\ PRINT  IF POS(0%)>70% &
	\ H1,H2=0. &
	\ GOSUB 2500 &
	\ IF FNG%(2%,D$(1%)) THEN  &
		  PRINT "EMP # ";D$(1%);" IS NOT IN THE MASTERFILE." &
		\ STOP
1045	  GOSUB 2300
1050	  IF FNG%(4%,D$(1%)) THEN  &
		  PRINT "Employee ";D$(1%);" is not in payroll file!" &
		\ PRINT "*** He is, however in the tax file!" &
		\ STOP
1052	  GOSUB 2400 &
	\ UNTIL P$(1%)<>D$(1%) OR FNS%<>0% &
		\ H1=H1+P(6%) &
		\ H2=H2+P(7%) &
		\ V%=FNN%(4%) &
		\ GOSUB 2400 &
	\ NEXT
1055	  T=0. &
	\ T=T+D(I%) FOR I%=3% TO 12% &
	\ T=D(1%)+D(2%)-T
1070	  T(I%)=T(I%)+D(I%) FOR I%=1% TO 7% &
	\ T(8%)=T(8%)+D(8%)+D(9%)+D(10%)+D(11%)+D(12%) &
	\ T(9%)=T(9%)+T &
	\ GOTO 1072 IF K$="Y" &
	\ IF FNN%(6%) THEN &
		  GOTO 1120 &
	  ELSE &
		  GOTO 1040
1072	  M(10%)=M(10%)+D(1%) &
	\ M(10%+I1%)=M(10%+I1%)+D(1%) &
	\ M(15%)=M(15%)+D(2%) &
	\ FOR I%=3% TO 5% &
		\ M(1%+I%*5%)=M(1%+I%*5%)+D(I%) &
		\ M(1%+I%*5%+I1%)=M(1%+I%*5%+I1%)+D(I%) &
	\ NEXT I%
1074	  M(31%)=M(31%)+D(6%) &
	\ M(32%)=M(32%)+D(7%) &
	\ M(35%)=M(35%)+D(8%)+D(9%)+D(10%)+D(11%)+D(12%) &
	\ M(36%)=M(36%)+T &
	\ M(36%+I1%)=M(36%+I1%)+H1 &
	\ M(40%+I1%)=M(40%+I1%)+H2
1080	  GOSUB 2100 &
	\ IF FNU%(2%,T2$) THEN  &
		  PRINT "Unable to update master file with ";LEFT(T2$,6%) &
		\ STOP
1085	  GOSUB 3000 &
	\ IF FNN%(6%) THEN &
		  GOTO 1090 &
	  ELSE &
		  GOTO 1040
1090	  PRINT  &
	\ PRINT "UPDATE COMPLETE." &
	\ IF FNG%(2%,"ZPAYRL") THEN  &
		  PRINT "Unable to fin ZPAYRL flag in master file!" &
		\ STOP
1095	  GOSUB 2300 &
	\ M(10%)=T(1%) &
	\ M(15%)=T(2%) &
	\ M(1%+I%*5%)=T(I%) FOR I%=3% TO 6% &
	\ M(32%)=T(7%) &
	\ M(35%)=T(8%) &
	\ M(36%)=T(9%) &
	\ M$(2%)=Z$+"" &
	\ GOSUB 2100 &
	\ IF FNU%(2%,T2$) THEN  &
		  PRINT "Unable to update ZPAYRL record!" &
		\ STOP
1120	  INPUT "SET PAGE ";K$ &
	\ PRINT  FOR I%=1% TO 5% &
	\ PRINT SPACE$(27%);"PAYROL SUMMARY FOR ";Z$ &
	\ PRINT  &
	\ PRINT 
1150	  PRINT USING "TAXABLE EARNINGS       ###,###,###.##", T(1%) &
	\ PRINT USING "NON TAXABLE EARNINGS   ###,###,###.##", T(2%) &
	\ PRINT USING "FEDERAL TAX            ###,###,###.##", T(3%) &
	\ PRINT USING "FICA TAX               ###,###,###.##", T(4%) &
	\ PRINT USING "STATE TAX              ###,###,###.##", T(5%) &
	\ PRINT USING "VACATION               ###,###,###.##", T(6%) &
	\ PRINT USING "INSURANCE              ###,###,###.##", T(7%) &
	\ PRINT USING "OTHER DEDUCTIONS       ###,###,###.##", T(8%) &
	\ PRINT USING "NET PAID               ###,###,###.##", T(9%)
1160	  PRINT  &
	\ GOTO 10000
2100	  !
2110	  LSET M1$(I%)=M$(I%) FOR I%=1% TO 23% &
	\ LSET M2$(I%)=CVTF$(M(I%)) FOR I%=1% TO 36% &
	\ LSET M2$(I%)=CVT%$(M(I%)*10.+6.8213185310482798e-15) FOR I%=37% TO 44% &
	\ RETURN
2200	  !
2210	  LSET P1$(15%+9%)=CHR$(P%(15%)) &
	\ RETURN
2300	  !
2310	  LSET T2$=FNL$ &
	\ M$(I%)=M1$(I%)+"" FOR I%=1% TO 23% &
	\ M(I%)=CVT$F(M2$(I%)) FOR I%=1% TO 36% &
	\ M(I%)=CVT$%(M2$(I%))/10. FOR I%=37% TO 44% &
	\ RETURN
2400	  !
2410	  LSET T4$=FNL$ &
	\ P$(I%)=P1$(I%)+"" FOR I%=1% TO 8% &
	\ P%(I%)=ASCII(P1$(I%+9%)) FOR I%=1% TO 15% &
	\ P(1%)=CVT$F(P1$(25%)) &
	\ P(I%)=FNN3(P1$(I%+24%)) FOR I%=2% TO 5% &
	\ P%(0%)=CVT$%(P1$(9%)) &
	\ P$(9%)=FND9$(P%(0%)) &
	\ IF P%(7%)=255% THEN  &
		  P(6%)=FNN3(P1$(13%)+P1$(14%)+P1$(15%)) &
		\ P(7%)=FNN3(P1$(20%)+P1$(21%)+P1$(22%)) &
		\ P%(I%)=0% FOR I%=1% TO 14% &
		\ RETURN
2420	  P(6%),P(7%)=0% &
	\ P(6%)=P(6%)+P%(I%) FOR I%=1% TO 7% &
	\ P(7%)=P(7%)+P%(I%) FOR I%=8% TO 14% &
	\ P(I%)=P(I%)/10. FOR I%=6% TO 7% &
	\ RETURN
2500	  !
2510	  LSET T6$=FNL$ &
	\ D$(I%)=D1$(I%)+"" FOR I%=1% TO 10% &
	\ D$(11%)=FND9$(CVT$%(D1$(11%))) &
	\ D(I%)=CVT$F(D1$(I%+11%)) FOR I%=1% TO 11% &
	\ D(I%+11%)=FNN3(D1$(I%+22%)) FOR I%=1% TO 2% &
	\ D%(1%)=ASCII(D1$(25%)) &
	\ D(14%)=D(1%)+D(2%)-D(12%) &
	\ D(14%)=D(14%)-D(I%) FOR I%=3% TO 11% &
	\ RETURN
2600	  !
2610	  LSET D1$(I%)=D$(I%) FOR I%=1% TO 10% &
	\ LSET D1$(11%)=CVT%$(FND9%(D$(11%))) &
	\ LSET D1$(I%+11%)=CVTF$(D(I%)) FOR I%=1% TO 11% &
	\ LSET D1$(I%+22%)=FNN3$(D(I%+11%) FOR I%=1% TO 2% &
	\ LSET D1$(25%)=CHR$(D%(1%)) &
	\ RETURN
3000	  !
3010	  D%(1%)=D%(1%) OR 1% &
	\ GOSUB 2600 &
	\ STOP IF FNU%(6%,T6$)
3020	  RETURN IF FNG%(4%,D$(1%))
3030	  GOSUB 2400 &
	\ GOTO 3040 IF P$(1%)<>D$(1%) &
	\ P%(15%)=P%(15%) OR 1% &
	\ GOSUB 2200 &
	\ STOP IF FNU%(4%,T4$) &
	\ GOTO 3030 UNLESS FNN%(4%)
3040	  RETURN
10000	  !
10010	  CLOSE 1% &
	\ CLOSE 11% &
	\ V%=FNX%("",0%,"")
14000	  !
14010	  DEF FND9%(D9$) &
	\ D9$="0"+D9$ IF INSTR(1%,D9$,".")=2% &
	\ D9$=LEFT(D9$,3%)+"0"+RIGHT(D9$,4%) IF INSTR(4%,D9$,".")=5% &
	\ FND9%=VAL(LEFT(D9$,2%))+VAL(MID(D9$,4%,2%))*16%+FND8%(VAL(RIGHT(D9$,7%)))*512% &
	\ FNEND
14020	  DEF FND9$(D9%) &
	\ FND9$=RIGHT(NUM1$((D9% AND 15%)+100%),2%)+"."+RIGHT(NUM1$((D9% AND 31%*16%)/16%+100%),2%)+"."+RIGHT(NUM1$((SWAP%(D9%) AND 254%)/2%+100%),2%) &
	\ FNEND
14030	  DEF FND8%(D8) &
	\ FND8%=D8 &
	\ FNEND
14200	  !
14210	  DEF FND7%(D7$) &
	\ ON ERROR GOTO 14220 &
	\ GOTO 14220 IF INSTR(1%,D7$,".")<>3% OR INSTR(4%,D7$,".")<>6% OR INSTR(7%,D7$,".")<>0% OR LEN(D7$)<>8% &
	\ D7%=VAL(LEFT(D7$,2%)) &
	\ GOTO 14220 IF D7%<1% OR D7%>12% &
	\ D7%=VAL(MID(D7$,4%,2%)) &
	\ GOTO 14220 IF D7%<1% OR D7%>31% &
	\ D7%=VAL(RIGHT(D7$,7%)) &
	\ GOTO 14220 IF D7%<0% &
	\ FND7%=0% &
	\ GOTO 14230
14220	  FND7%=-1% &
	\ RESUME 14230
14230	  ON ERROR GOTO 0 &
	\ FNEND
14250	  !
14260	  DEF FND7$(D7$) &
	\ D7$=D7$+"."+RIGHT(DATE$(0%),8%) IF LEN(D7$)<6% &
	\ D7$="0"+D7$ IF INSTR(1%,D7$,".")=2% &
	\ D7$=LEFT(D7$,3%)+"0"+RIGHT(D7$,4%) IF INSTR(4%,D7$,".")=5% &
	\ FND7$=D7$ &
	\ FNEND
14300	  !
14310	  DEF FNN3$(N3) &
	\ FNN3$=CVT%$(INT(N3))+CHR$((N3-INT(N3))*100.+6.8213185310482798e-15) &
	\ FNEND
14350	  DEF FNN3(N3$) &
	\ FNN3=CVT$%(LEFT(N3$,2%))+ASCII(MID(N3$,3%,1%))/100. &
	\ FNEND
32767	  END
