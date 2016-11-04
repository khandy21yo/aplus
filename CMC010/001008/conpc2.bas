10	  ! &
	  ! Program name: [1,8]CONPC2		Compiled with SCALE 0 on V07.0 &
	  ! Decompiled on 04-May-84 at 09:24 AM by UNBAC Version 1
50	  DIM M1$(23%), M$(23%), M2$(44%), M(44%), P1$(29%), P$(9%), P(7%), &
		P%(17%), D1$(25%), D$(11%), D(14%), Y$(30%)
60	  READ Y$(I%) FOR I%=1% TO 30%
70	  DATA	" ONE"," TWO"," THREE"," FOUR"," FIVE"," SIX"," SEVEN", &
		" EIGHT"," NINE"," TEN"," ELEVEN"," TWELVE"," THIRTEEN", &
		" FOURTEEN"," FIFTEEN"," SIXTEEN"," SEVENTEEN"," EIGHTEEN", &
		" NINETEEN"," TWENTY"," THIRTY"," FORTY"," FIFTY"," SIXTY", &
		" SEVENTY"," EIGHTY"," NINETY"," HUNDRED"," THOUSAND"," MILLION" &

100	  !
110	  OPEN "KB:" AS FILE 1%, RECORDSIZE 512%+64%+128%
111	  OPEN "KB:" FOR INPUT AS FILE 12%
120	  IF FNO%(2%,"MSTRFL.DAT","/RO","") THEN &
	  PRINT "MASTER FILE NOT FOUND !!!" &
	\ GOTO 10000
130	  INPUT "PAYROLL DATE (MM.DD.YY) ";Z$ &
	\ GOTO 10000 IF Z$="" &
	\ Z$=FND7$(Z$) &
	\ Z1$=LEFT(Z$,2%)+RIGHT(Z$,4%)+"T" &
	\ IF FND7%(Z$) THEN &
	  PRINT "BAD DATE!  RE ENTER (TYPE <RETURN> TO END)" &
	\ GOTO 130
140	  IF FNO%(4%,"PR"+Z1$,"/RO","") THEN &
	  PRINT "PAYROLL FILE DOES NOT EXIST!" &
	\ GOTO 10000
150	  IF FNO%(6%,"TX"+Z1$,"","") THEN &
	  PRINT "EARNINGS AND DEDUCTIONS FILE DOES NOT EXIST!" &
	\ GOTO 10000
200	  !
220	  FIELD #1%, 6% AS M1$(1%),30% AS M1$(2%),30% AS M1$(3%),30% AS M1$(4%), &
		30% AS M1$(5%),11% AS M1$(6%),8% AS M1$(7%),1% AS M1$(8%),2% &
		 AS M1$(9%),12% AS M1$(10%),2% AS M1$(11%)
221	  FIELD #1%, 152%+I%*10% AS E$,8% AS M2$(I%),2% AS M1$(I%+11%) FOR I%= &
		1% TO 9% &
	\ FIELD #1%, 252% AS E$,8% AS M1$(21%),8% AS M1$(22%) &
	\ FIELD #1%, 188%+I%*8% AS E$,8% AS M2$(I%) FOR I%=10% TO 36% &
	\ FIELD #1%, 484% AS E$,2% AS M1$(23%) &
	\ FIELD #1%, 486%+I%*2% AS E$,2% AS M2$(I%+37%) FOR I%=0% TO 7%
230	  FIELD #1%, 512% AS E$,6% AS P1$(1%),6% AS P1$(2%),1% AS P1$(3%),1% &
		 AS P1$(4%),2% AS P1$(5%),2% AS P1$(6%),2% AS P1$(7%),7% AS &
		P1$(8%),2% AS P1$(9%) &
	\ FIELD #1%, 512%+19%+I% AS E$,1% AS P1$(I%) FOR I%=10% TO 24% &
	\ FIELD #1%, 512%+44% AS E$,8% AS P1$(25%),3% AS P1$(26%),3% AS P1$(27%), &
		3% AS P1$(28%),3% AS P1$(29%)
240	  FIELD #1%, 512%+64% AS E$,6% AS D1$(1%),1% AS D1$(2%),2% AS D1$(3%), &
		2% AS D1$(4%),2% AS D1$(5%),2% AS D1$(6%),2% AS D1$(7%),2% AS &
		D1$(8%),2% AS D1$(9%),8% AS D1$(10%),2% AS D1$(11%),8% AS D1$( &
		12%),8% AS D1$(13%),8% AS D1$(14%),8% AS D1$(15%),8% AS D1$(16%), &
		8% AS D1$(17%),8% AS D1$(18%),8% AS D1$(19%),8% AS D1$(20%),8% &
		 AS D1$(21%),8% AS D1$(22%),3% AS D1$(23%),3% AS D1$(24%),1% &
		 AS D1$(25%)
290	  FIELD #1%, 512% AS T2$,64% AS T4$,128% AS T6$
900	  IF FNG%(6%,"") THEN &
	  PRINT "FILE IS EMPTY." &
	\ GOTO 10000
902	  GOSUB 2500 &
	\ IF D%(1%) AND 1% OR D%(1%) AND 2% THEN &
	  PRINT "PAYROLL HAS BEEN UPDATED - NO CHECKS CAN BE WRITTEN." &
	\ GOTO 10000
905	  INPUT "SERIES OR INDIVIDUAL CHECKS (S/I) ";C1$ &
	\ C1$=LEFT(C1$,1%)
910	  GOTO 1000 IF C1$="S" &
	\ GOTO 1100 IF C1$="I" &
	\ GOTO 900
1000	  !
1010	  INPUT "START CHECKS WITH EMP CODE: (HIT RETURN TO GET ALL)",S$ &
	\ GOTO 10000 IF FNG%(4%,S$)
1020	  GOSUB 3000
1022	  PRINT &
	\ FOR I1=F TO B &
	\ GOSUB 2400 &
	\ A$=LEFT(P$(1%),6%) &
	\ V%=FNG%(2%,A$) &
	\ GOSUB 2300 &
	\ V%=FNG%(6%,A$) &
	\ GOSUB 2500 &
	\ LSET D1$(10%)=NUM1$(I1) &
	\ LSET D1$(11%)=CVT%$(FND9%(D$)) &
	\ STOP IF FNU%(6%,T6$) &
	\ GOSUB 4000 &
	\ GOSUB 6000 &
	\ GOSUB 7000 &
	\ IF FNS% THEN &
	  GOTO 1030
1025	  NEXT I1 &
	\ GOTO 1120 IF C1$="I" &
	\ INPUT LINE #12%, A$ &
	\ V$=SYS(CHR$(2%)) &
	\ GOTO 1020
1030	  GOTO 1120 IF C1$="I" &
	\ INPUT LINE #12%, A$ &
	\ V$=SYS(CHR$(2%)) &
	\ GOTO 10000
1100	  !
1110	  GOSUB 3012
1120	  V$=SYS(CHR$(3%)) &
	\ PRINT CHR$(7%); &
	\ INPUT LINE #12%, A$ &
	\ A$=CVT$$(A$,132%) &
	\ GOTO 1140 IF A$="" &
	\ A$=A$+SPACE$(6%-LEN(A$)) &
	\ IF FNG%(4%,A$) THEN &
	  PRINT CHR$(7%)+CHR$(13%); FOR I%=1% TO 5% &
	\ GOTO 1120
1130	  PRINT CHR$(7%); FOR I%=1% TO 4% &
	\ INPUT LINE #12%, F$ &
	\ F$=CVT$$(F$,132%) &
	\ F=VAL(F$) &
	\ F=B+1. IF F$="" &
	\ B=F &
	\ GOTO 1022
1140	  V$=SYS(CHR$(2%)) &
	\ GOTO 10000
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
	\ IF P%(7%)=255% THEN &
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
3000	  !
3010	  INPUT "FIRST CHECK # ";F &
	\ INPUT "BREAK CHECK # ";B &
	\ B=900000. IF B=0.
3012	  INPUT "ENTER DATE TO BE PRINTED ON CHECKS ";D$ &
	\ D$=FND7$(D$) &
	\ IF FND7%(D$) THEN &
	  PRINT "ILLEGAL DATE." &
	\ GOTO 3012
3015	  PRINT "POSITION # SIGN ON EDGE OF FIRST CHECK." &
	\ PRINT "PRESS 'P' AND RETURN TO PRINT CHECKS." &
	\ V$=SYS(CHR$(3%))
3020	  INPUT LINE #12%, A$ &
	\ A$=CVT$$(A$,4%) &
	\ RETURN IF A$="P" &
	\ PRINT "#";CHR$(8%); &
	\ GOTO 3020
4000	  !
4010	  PRINT &
	\ PRINT &
	\ PRINT &
	\ PRINT USING " \"+SPACE$(22%)+"\ \    \\\ !\\  \      \    #####"+ &
		"            \    \", M$(2%),M$(1%),M$(10%),M$(8%),M$(9%),D$,I1, &
		D1$ &
	\ PRINT &
	\ PRINT &
	\ RETURN
6000	  !
6010	  X%=1% &
	\ C$=P$(1%)+"" &
	\ T%=0% &
	\ A(0%,I%),B(I%)=0. FOR I%=1% TO 10% &
	\ B$(5%)=""
6020	  GOSUB 6800 &
	\ UNLESS FNN%(4%) THEN &
	  GOSUB 2400 &
	\ IF A$=P$(1%) THEN &
	  X%=X%+1% &
	\ GOTO 6020
6030	  B(I%)=0. FOR I%=1% TO 10% &
	\ IF X%<=7% THEN &
	  STOP IF FNG%(4%,C$) &
	\ FOR J%=1% TO X% &
	\ GOSUB 2400 &
	\ GOSUB 6900 &
	\ GOSUB 6500 &
	\ V%=FNN%(4%) &
	\ NEXT J% &
	\ RETURN
6050	  X%=T% &
	\ FOR J%=1% TO X% &
	\ B(I%)=A(J%,I%) FOR I%=1% TO 10% &
	\ B$(I%)=A$(J%,I%) FOR I%=1% TO 10% &
	\ B$(2%),B$(3%),B$(4%)="" &
	\ GOSUB 6500 &
	\ NEXT J% &
	\ RETURN
6500	  GOTO 6580 IF B(8%)=0. &
	\ PRINT USING "  \    \", B$(4%); &
	\ IF B(1%)=0. THEN &
	  PRINT "     "; &
	  ELSE &
	  PRINT USING "###.#", B(1%);
6510	  IF B(2%)=0. THEN &
	  PRINT "       "; &
	  ELSE &
	  PRINT USING "  ##.# ", B(2%);
6520	  PRINT USING "\\", B$(1%); &
	\ IF B(3%)=0. THEN &
	  PRINT "       "; &
	  ELSE &
	  PRINT USING "###.###", B(3%);
6530	  IF B(4%)=0. THEN &
	  PRINT "         "; &
	  ELSE &
	  PRINT USING "  ####.##", B(4%);
6540	  IF B(5%)=0. THEN &
	  PRINT "       "; &
	  ELSE &
	  PRINT USING " ###.##", B(5%);
6550	  IF B(6%)=0. THEN &
	  PRINT "        "; &
	  ELSE &
	  PRINT USING " ###.## ", B(6%);
6560	  PRINT USING "\\", B$(2%); &
	\ IF B(7%)=0. THEN &
	  PRINT "        "; &
	  ELSE &
	  PRINT USING "####.## ", B(7%);
6570	  PRINT USING "\\", B$(3%); &
	\ IF B(8%)<>0. THEN &
	  PRINT USING "##,###.##", B(8%);
6580	  PRINT &
	\ RETURN
6800	  !
6810	  FOR T1%=1% TO T% &
	\ GOTO 6830 IF A$(T1%,1%)=P$(3%) AND A(T1%,3%)=P(1%) &
	\ NEXT T1% &
	\ IF T%<8% THEN &
	  T%=T%+1% &
	\ A$(T%,1%)=P$(3%) &
	\ A$(T%,2%)=P$(4%) &
	\ A$(T%,4%)=P$(2%) &
	\ T1%=T% &
	\ A(T%,I%)=0. FOR I%=1% TO 10% &
	\ GOTO 6830
6820	  GOSUB 6900 &
	\ A(I%,0%)=A(I%,0%)+B(I%) FOR I%=1% TO 10% &
	\ RETURN
6830	  GOSUB 6900 &
	\ GOSUB 6950 &
	\ RETURN
6900	  !
6910	  B$(1%)=P$(3%) &
	\ B$(2%)=P$(4%) &
	\ B$(4%)=P$(2%) &
	\ B$(3%),B$(5%)="" &
	\ B(1%)=P(6%) &
	\ B(2%)=P(7%) &
	\ B(3%)=P(1%) &
	\ B(4%)=FNZ(B(1%)*P(1%)) &
	\ B(9%)=1. &
	\ B(9%)=1.5 IF P$(4%)="H" &
	\ B(9%)=2. IF P$(4%)="D" &
	\ B(5%)=FNZ(B(9%)*B(2%)*P(1%)) &
	\ B(6%)=P(2%) &
	\ B(7%),B(10%)=0. &
	\ B(7%)=B(7%)+P(5%) &
	\ B(10%)=B(10%)+P(3%)+P(4%) &
	\ B(8%)=B(4%)+B(5%)+B(6%)+B(7%) &
	\ RETURN
6950	  !
6960	  A$(T1%,3%)=B$(3%) &
	\ A$(T1%,5%)=B$(5%) &
	\ A(T1%,I%)=A(T1%,I%)+B(I%) FOR I%=1% TO 10% &
	\ A(T1%,3%)=B(3%) &
	\ A(0%,I%)=A(0%,I%)+B(I%) FOR I%=1% TO 10% &
	\ RETURN
7000	  !
7010	  PRINT  FOR K%=X% TO 6% &
	\ PRINT USING SPACE$(57%)+"TOTAL  :  ####.##", A(0%,8%) &
	\ PRINT &
	\ PRINT &
	\ PRINT USING " ###.## ##.##  ##.##\\ ", D(3%),D(4%),D(5%),M$(23%); &
	\ IF D(7%)=0. THEN &
	  PRINT "      "; &
	  ELSE &
	  PRINT USING " ##.##", D(7%);
7020	  U$="S" &
	\ GOSUB 7060 &
	\ U$="U" &
	\ PRINT " "; &
	\ GOSUB 7060 &
	\ IF D(6%)=0. THEN &
	  PRINT "       "; &
	  ELSE &
	  PRINT USING " ###.##", D(6%);
7030	  U$="C" &
	\ GOSUB 7060 &
	\ GOSUB 7080 &
	\ A1=A(0%,10%) &
	\ A1=A1+D(K%) FOR K%=3% TO 11% &
	\ Y=A(0%,8%)-A1 &
	\ Y=Y+D(6%) IF LEFT(M$(15%),1%)="#" &
	\ PRINT USING SPACE$(55%)+"####.## \\ #,###.##", -A1,"TD",Y &
	\ PRINT  FOR K%=1% TO 9% &
	\ IF Y<0. THEN &
	  PRINT "     VOID  VOID  VOID  VOID  VOID"+"  VOID VOID VOID VOID" &
		FOR K%=1% TO 8% &
	\ GOTO 7050
7040	  Y=A(0%,8%)-A1 &
	\ Y=Y+D(6%) IF LEFT(M$(15%),1%)="#" &
	\ GOSUB 9600 &
	\ PRINT "      ";Y$;TAB(58%); &
	\ PRINT USING "$$##,###,###.##", Y &
	\ PRINT &
	\ PRINT &
	\ PRINT TAB(38%); &
	\ PRINT USING " ######     \      \$$##,###,###.##", I1,D$,Y &
	\ PRINT TAB(8%);M$(2%) &
	\ PRINT TAB(8%);M$(3%) &
	\ PRINT TAB(8%);CVT$$(M$(4%),24%);"   ";CVT$$(M$(5%),24%) &
	\ PRINT
7050	  PRINT  FOR K%=1% TO 5% &
	\ PRINT  IF C1$="S" &
	\ RETURN
7060	  FOR I%=1% TO 4% &
	\ IF D$(I%+6%)=U$ AND D(I%+8%)<>0. THEN &
	  PRINT USING "###.##", D(I%+8%); &
	\ RETURN
7070	  NEXT I% &
	\ PRINT "      "; &
	\ RETURN
7080	  A1=0. &
	\ A1$="" &
	\ FOR I%=1% TO 4% &
	\ U$=D$(I%+5%) &
	\ GOTO 7090 IF U$="U" OR U$="C" OR U$="S" &
	\ A1=A1+D(I%+7%) &
	\ A1$=U$+""
7090	  NEXT I% &
	\ PRINT USING " ###.## \\", A1,A1$; IF A1<>0. &
	\ IF A(0%,10%)=0. THEN &
	  PRINT &
	\ PRINT &
	\ RETURN
7100	  PRINT &
	\ PRINT USING SPACE$(55%)+"####.## \\", -A(0%,10%),B$(5%) &
	\ RETURN
9600	  !
9610	  Y=FNZ(Y) &
	\ Y$="" &
	\ Y1=INT((Y-INT(Y))*100.+.5) &
	\ IF Y1=0. THEN &
	  Y$="NO/100---DOLLARS" &
	\ GOTO 9630
9620	  Y$=NUM1$(Y1)+"/100---DOLLARS"
9630	  Y1=INT(Y) &
	\ RETURN IF Y1=0. &
	\ Y1$=NUM1$(Y1) &
	\ GOSUB 9700 &
	\ Y$=Y3$+" AND "+Y$ &
	\ RETURN IF Y1$="" &
	\ GOSUB 9700 &
	\ IF Y3$<>"" THEN &
	  Y$=Y3$+Y$(29%)+Y$
9670	  RETURN IF Y1$="" &
	\ GOSUB 9700 &
	\ Y$=Y3$+Y$(30%)+Y$ &
	\ RETURN
9700	  Y2$=RIGHT(Y1$,LEN(Y1$)-2%) &
	\ Y1$=LEFT(Y1$,LEN(Y1$)-3%) &
	\ GOSUB 9800 &
	\ RETURN
9800	  Y3$="" &
	\ Y2$=NUM1$(VAL(Y2$)) &
	\ IF LEN(Y2$)=3% THEN &
	  Y2=VAL(LEFT(Y2$,1%)) &
	\ Y3$=Y$(Y2)+Y$(28%) &
	\ Y2$=RIGHT(Y2$,2%)
9810	  Y2=VAL(Y2$) &
	\ GOTO 9850 IF LEN(Y2$)<2% &
	\ IF Y2<20. THEN &
	  Y3$=Y3$+Y$(Y2) &
	\ RETURN
9830	  Y2=INT(Y2/10.)+18. &
	\ Y3$=Y3$+Y$(Y2) &
	\ Y2=VAL(RIGHT(Y2$,2%))
9850	  Y3$=Y3$+Y$(Y2) &
	\ RETURN
10000	  !
10010	  V%=FNC%(2%)+FNC%(4%)+FNC%(6%) &
	\ CLOSE 1% &
	\ CHAIN "!MENU" 8100. IF ASCII(SYS(CHR$(7%)))=255% &
	\ GOTO 32767
14000	  !
14010	  DEF FND9%(E$) &
	\ E$="0"+E$ IF INSTR(1%,E$,".")=2% &
	\ E$=LEFT(E$,3%)+"0"+RIGHT(E$,4%) IF INSTR(4%,E$,".")=5% &
	\ FND9%=VAL(LEFT(E$,2%))+VAL(MID(E$,4%,2%))*16%+FND8%(VAL(RIGHT(E$,7%)) &
		)*512% &
	\ FNEND
14020	  DEF FND9$(K%) &
	\ FND9$=RIGHT(NUM1$((K% AND 15%)+100%),2%)+"."+RIGHT(NUM1$((K% AND 31%* &
		16%)/16%+100%),2%)+"."+RIGHT(NUM1$((SWAP%(K%) AND 254%)/2%+100% &
		),2%) &
	\ FNEND
14030	  DEF FND8%(Z) &
	\ FND8%=Z &
	\ FNEND
14040	  DEF FNZ(Z) &
	\ FNZ=INT(Z*100.+.51)/100. &
	\ FNEND
14200	  !
14210	  DEF FND7%(E$) &
	\ ON ERROR GOTO 14220 &
	\ GOTO 14220 IF INSTR(1%,E$,".")<>3% OR INSTR(4%,E$,".")<>6% OR INSTR( &
		7%,E$,".")<>0% OR LEN(E$)<>8% &
	\ D7%=VAL(LEFT(E$,2%)) &
	\ GOTO 14220 IF D7%<1% OR D7%>12% &
	\ D7%=VAL(MID(E$,4%,2%)) &
	\ GOTO 14220 IF D7%<1% OR D7%>31% &
	\ D7%=VAL(RIGHT(E$,7%)) &
	\ GOTO 14220 IF D7%<0% &
	\ FND7%=0% &
	\ GOTO 14230
14220	  FND7%=-1% &
	\ RESUME 14230
14230	  ON ERROR GOTO 0 &
	\ FNEND
14250	  !
14260	  DEF FND7$(E$) &
	\ E$=E$+"."+RIGHT(DATE$(0%),8%) IF LEN(E$)<6% &
	\ E$="0"+E$ IF INSTR(1%,E$,".")=2% &
	\ E$=LEFT(E$,3%)+"0"+RIGHT(E$,4%) IF INSTR(4%,E$,".")=5% &
	\ FND7$=E$ &
	\ FNEND
14300	  !
14310	  DEF FNN3$(Z) &
	\ FNN3$=CVT%$(INT(Z))+CHR$((Z-INT(Z))*100.+.51) &
	\ FNEND
14350	  DEF FNN3(E$) &
	\ FNN3=CVT$%(LEFT(E$,2%))+ASCII(MID(E$,3%,1%))/100. &
	\ FNEND
32767	  END
