1	EXTEND
10	! &
	! PAYROL - CONSTRUCTION GENERAL PAYROLL MODULE 1 &
	! &
	! 07/02/79 - BILL LOVEGROVE FOR GENERAL SYSTEM &
	! 01/??/81 - MODIFIED FOR OVARD COLLINS BY MIKE FEINAUER &
	!	     (FOR TRANSFERING PAYROL TO CHECK DATA FILE) &
	!
50 DIM M1$(23%),M$(23%),M2$(44%),M(44%),P1$(29%),P$(9%),P%(15%),P(7%), &
	T2(16%),T(8%),T1(8%)
60 D$="MON  TUE  WED  THU  FRI  SAT  SUN  "
100	! &
	! OPEN FILES &
	!
110 OPEN "NL:" AS FILE 1%, RECORDSIZE 512% + 64% + 64% &
	: OPEN "KB:" FOR INPUT AS FILE 12%
120 IF FNO%(2%,"MSTRFL.DAT","/RO","") &
	THEN	PRINT "MASTER FILE NOT FOUND !!!" : GOTO 10000
130 INPUT "PAYROLL DATE (MM.DD.YY) ";Z$ UNLESS Z$<>"" : GOTO 10000 IF Z$="" &
	: Z$=FND7$(Z$) : Z1$=LEFT(Z$,2%)+RIGHT(Z$,4%)+"T" : IF FND7%(Z$) &
	THEN	PRINT "BAD DATE, PLEASE RE-ENTER." : Z$="" : GOTO 130
140 IF FNO%(4%,"PR"+Z1$,"","") THEN PRINT "PAYROL FOR ";Z1$;" NOT FOUND" &
	: Z$="" :  GOTO 130
150 OPEN "SS0:UNIQUE.FIL" FOR INPUT AS FILE 11% &
	: DIM #11%,A0$(255%)=64% &
	: A0%=INSTR(1%,A0$(15%),"TIPS")
155 C9$(1%)="WIP" : C9$(2%)="DED" : C9$(3%)="BNK" : C9$(4%)="FED" &
  : C9$(5%)="FIC" : C9$(6%)="BUR" : C9$(7%)="VAC" : C9$(8%)="INS" &
  : C9$(9%)="B/W" : C9$(10%)="B/E"
160 A$=CVT$$(A0$(22%)+A0$(27%)+A0$(28%),-1%) &
	: FOR I%=1% TO 10% &
	: A%=INSTR(1%,A$,C9$(I%)) &
	: C9$(I%)=MID(A$,A%+4%,6%) IF A% &
	: NEXT I% : A%=0% : S%=0% : D%=0% &
	: B1,B2=1. : B1=VAL(C9$( 9%)) UNLESS C9$( 9%)="B/W" &
	:            B2=VAL(C9$(10%)) UNLESS C9$(10%)="B/E"
170 A%=INSTR(A%+1%,A$,"D/") &
	: IF A% THEN D%=D%+1% &
	\ A1%=INSTR(A%+1%,A$,'=') &
	\ A1%=A%+4% IF A1%=0% OR A%+5%<A1% &
	\ D2$(D%)=MID(A$,A%+2%,A1%-(A%+2%)) &
	: D3$(D%)=MID(A$,A1%+1%,6%) &
	: GOTO 170
180 A%=INSTR(A%+1%,A$,"W/") &
	: IF A% THEN W%=W%+1% &
	: W$(W%)=MID(A$,A%+2%,2%) &
	: W0$(W%)=MID(A$,A%+5%,6%) &
	: GOTO 180
190 C9$(I%)=C9$(I%)+".00" IF LEN(C9$(I%))=3% FOR I%=1% TO 10% &
	: A%=INSTR(1%,A0$(15%),"LEN="):L%=0% &
	: L%=VAL(MID(A0$(15%),A%+4%,1%)) IF A%
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
240 FIELD #1%, 512% + 64% AS E$, 6% AS P$, 8% AS P1$, 2% AS P2$, &
		8% AS P3$, 2% AS P4$, 2% AS P5$, 2% AS P6$, 28% AS P7$, &
		6% AS P8$
290 FIELD #1%,512% AS T2$,64% AS T4$, 64% AS T6$
320 GOTO 1000
990 DEF FNZ(Z)=INT(Z*100.+.51)/100.
1000	! &
	! PROGRAM CONTROL &
	!
1020 PRINT : T5%=0% : INPUT "OPTION ";K$ : K$=LEFT(K$,3%) &
	: GOTO  1030 IF K$="" &
	: GOTO  7000 IF K$="TRA" &
	: GOTO  5000 IF K$="TOT" &
	: GOTO 10000 IF K$="END" &
	: PRINT
1030 PRINT &
	: PRINT "OPTIONS:" &
	: PRINT "	'TRA'NSFER DATA TO CHECK DATA FILE" &
	: PRINT "	'TOT'ALS OF PAYROLL TRANSFER PRINTED" &
	: PRINT "	'END' PROGRAM" : GOTO 1020
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
5000	! &
	! PRINT TOTALS &
	!
5010	T5%=-1% : GOTO 7020
7000	! &
	!  PRINT &
	!
7010	INPUT "Transfer to which 'CKDATA' file ( JAN, FEB, ETC. )";F$ &
	: UNLESS LEN(F$)=3% AND INSTR(1%,"!JAN!FEB!MAR!APR!MAY!JUN!"+ &
	"JUL!AUG!SEP!OCT!NOV!DEC",F$)>0% THEN PRINT "Invalid month !!"+ &
	STRING$(3%,7%) : GOTO 7010 &

7015 C1$=F$ : V%=FNO%(6%,"CK"+F$+".DAT","","") &
	: IF V%<>0% THEN PRINT "PLEASE CHECK THE MONTHLY TRANSACTION FILE" &
	: PRINT "THEN RE-RUN.  " \ GOTO 10000 &

7020 GOTO 1020 IF FNG%(4%,"") : T8,T9=0. &
	: T(I%),T1(I%),T2(I%),T2(I%+8%)=0. FOR I%=1% TO 8% &
	: GOSUB 2400 : IF [P%(15%) AND 2%] THEN IF T5%=-1% THEN 7025 &
	ELSE PRINT "This payroll has already been transfered !!(7025)"+ &
	STRING$(3%,7%) : GOTO 1020
7025 GOSUB 2400 : V%=FNG%(2%,P$(1%)) &
	: IF V%=0% THEN GOSUB 2300 ELSE PRINT &
	: PRINT "EMPLOYEE ";P$(1%);" NOT IN MASTER FILE" &
	: PRINT "PLEASE CHECK THE ASSOCIATED EXPENSE ACOUNT IN CKDATA" &
	: M$(10%)="" &

7030 GOTO 7040 IF W%=0% &
	: C9$(1%)="BAD.00" &
	: W$=CVT$$(M$(10%),2%) &
	: W$=CVT$$(P$(2%),2%) IF A0% &
	: C9$(1%)=W0$(I%) IF W$(I%)=W$ FOR I%=1% TO W% &

7040 GOSUB 7300 &
	: GOTO 7025 UNLESS FNN%(4%) &

7060 PRINT &
	: GOSUB 7500 &
	: P%=0% : F$="" : PRINT : GOTO 32767 IF T5% &
	: LSET P2$="PR" : LSET P7$="Payroll Burden" : LSET P1$=" "+C9$(6%) &
	: RSET P$="8" : LSET P8$="" : LSET P3$=CVTF$((P9-T(5%))*(-1)) &
	: STOP IF FNA%(6%,T6$) UNLESS CVT$F(P3$)=0. : STOP IF FNG%(4%,"")
7070 GOSUB 2400 : LSET P1$(24%)=CHR$(P%(15%) OR 2%) &
	: STOP IF FNU%(4%,T4$) : GOTO 7070 UNLESS FNN%(4%)
7080 STOP IF FNX%("[1,8]CONTRE.BAC",100%,Z$+C1$) : GOTO 32767
7300	! &
	! PRINT SHORT FORM &
	!
7305 P7=P(5%) : IF A0% AND P$(7%)="SB" THEN T7=T7+P7 : P7=0.
7310 X1%=X1%+1% : O$=LEFT(P$(4%),1%) : O=1. &
	: O=1. : O=1.5 IF O$="H" : O=2. IF O$="D" &
	: T=FNZ(P(6%)*P(1%))+FNZ(P(7%)*P(1%)*O) &
	: T(I%)=T(I%)+P(I%+5%) FOR I%=1% TO 2% &
	: T(3%)=T(3%)+P7 : T(4%)=T(4%)+P(3%)+P(4%) &
	: T(5%)=T(5%)+T+P7+P(2%)
7315 GOTO 7322 IF T5% : FOR I%=3% TO 4% &
	: GOTO 7318 IF P(I%)=0. &
	: RSET P$="8" &
	: LSET P2$="PR" : LSET P4$=CVT%$(VAL(LEFT(Z$,2%))) &
	: LSET P5$=CVT%$(VAL(MID(Z$,4%,2%))) &
	: LSET P6$=CVT%$(VAL(RIGHT(Z$,7%))) &
	: LSET P7$="MISCELLANEOUS DEDUCTION" : LSET P8$="" &
	: LSET P3$=CVTF$(-P(I%)) &
	: LSET P1$=" "+C9$(2%) : FOR K%=1% TO D% : IF P$(I%+2%)=D2$(K%) THEN &
	  LSET P1$=" "+D3$(K%) : GOTO 7317
7316 NEXT K% : LSET P1$=LEFT(P1$,5%)+P$(I%+2%) UNLESS P$(I%+2%)=""
7317 STOP IF FNA%(6%,T6$)
7318 NEXT I%
7320 RSET P$="8" : LSET P2$="PR" : LSET P8$="" : LSET P1$=" BAD.00" &
	: LSET P4$=CVT%$(VAL(LEFT(Z$,2%))) &
	: LSET P5$=CVT%$(VAL(MID(Z$,4%,2%))) &
	: LSET P6$=CVT%$(VAL(RIGHT(Z$,7%))) &
	: LSET P7$=P$(1%)+SPACE$(6%-LEN(NUM1$(P(6%))))+NUM1$(P(6%))+ &
	SPACE$(6%-LEN(NUM1$(P(7%))))+NUM1$(P(7%))+"  "+LEFT(M$(10%),6%) &
	: A1$=CVT$$(P$(2%),-1%) &
	: LSET P1$=" "+A1$ &
	: GOTO 7322 IF INSTR(1%,A1$,".") &
	: IF (LEN(A1$)=3% AND L%<>3%) THEN LSET P1$=" "+A1$+".00" ELSE &
	  LSET P1$=" "+C9$(1%) : LSET P8$=P$(2%) &
	: LSET P1$=" "+LEFT(C9$(1%),3%)+"."+RIGHT(A1$,L%+1%) &
	  IF (L%>0% AND LEN(A1$)>L%) &
	: LSET P8$=LEFT(P$(2%),L%) IF L%
7322 T9=T+P7+P(2%) : IF INSTR(1%,P$(2%),".") THEN T8=T9*B2 ELSE T8=T9*B1
7330 P9=P9+T8 : UNLESS T5% THEN LSET P3$=CVTF$(T8) &
	: STOP IF FNA%(6%,T6$) UNLESS T8=0. &
	: PRINT IF POS(0%)>75% : PRINT "!";
7340 RETURN
7500	! &
	! GRAND TOTALS &
	!
7520 PRINT : PRINT : PRINT TAB(22%);"Payroll Transfer Totals for "; &
	Z$ : PRINT : U$="\"+SPACE$(16%)+"\   #,###,###.##" &
	: PRINT USING U$,"Reg. Hours ",T(1%) &
	: PRINT USING U$,"O.T. Hours",T(2%) &
	: PRINT USING U$,"Other Earnings",T(3%) &
	: PRINT USING U$,"Other Deductions",T(4%) &
	: PRINT USING U$,"Gross Pay",T(5%) &
	: PRINT USING U$,"Burden",P9-T(5%) &
	: PRINT USING U$,"Total Payroll",P9 &
	: PRINT &
	: PRINT USING U$,"Tips",T7 IF A0% &
	: RETURN
10000	! &
	!  TERMINATE PROGRAM &
	!
10010 V%=FNC%(2%)+FNC%(4%)+FNC%(8%) : CLOSE 1%,12% &
	: STOP IF FNX%("",0%,"")
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
