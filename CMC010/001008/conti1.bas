10	! &
	! CONTI1 - CONTIP - CHECK REPORTS AND 941 REPORTS   V01-02 &
	! &
	! 01/??/78 -- BILL LOVEGROVE (JAN 78) &
	! 04/??/79 -- BILL LOVEGROVE (APR 79) TO CORRECTLY HANDLE &
	!              YTD TOTALS IF PRINTING 941 FOR QUARTER BEFORE CURRENT &
	! 07/26/79 -- CONVERTED TO STANDARD SYSTEM -- KEVIN HANDY &
	! &
	! 03/30/80 -- CONVERTED TO CONSTRUCTION SYSTEM FOR 249,* -- CWR &
	!
15 Y$=FNX$ : GOTO 1000
25 !SUBROUTINE FOR SET UP
26 R1=3.35 &
	! CURRENT MINIMUM WAGE
30 DIM R$(24%),M$(6%),P$(37%),M(37%),M1$(6%),M2$(44%),T(12%), &
	U2$(12%),Y$(100%),T9(37%),D$(11%),D(14%),D1$(25%),P1$(30%), &
	P(10%),P%(15%)
40 OPEN "NL:" AS FILE 1%,RECORDSIZE 512%+256%+128%+64%
70 FIELD #1%,512% AS E$,6% AS U1$(1%),6% AS U1$(2%),8% AS U1$(3%), &
	8% AS H$(1%),8% AS H$(2%),8% AS H$(3%),8% AS H$(4%), &
	1% AS U1$(4%),2% AS U1$(5%) &
	: FIELD #1%,(37%+512%+I%*8%) AS D$,8% AS U2$(I%) FOR I%=1% TO 12%
120 X$="$$###,########.##"
150 INPUT "YEAR (<RETURN> FOR CURRENT)";K$ : K$=CVT$$(K$,-1%) &
	: K$="FL" IF K$="" : GOTO 150 IF LEN(K$)<>2%
155 IF FNO%(2%,"MSTR"+K$+".DAT","/RO","R") THEN PRINT &
	"MSTR"+K$+".DAT IS NONEXISTENT" : GOTO 32767
160 E9=FNT ! SAVE FOR EXTENDING TEMP FILE
240 FIELD#1%, 512%+256% AS E$, 6% AS D1$(1%),1% AS D1$(2%),2% AS D1$(3%), &
		2% AS D1$(4%),2% AS D1$(5%), 2% AS D1$(6%),2% AS D1$(7%), &
		2% AS D1$(8%), 2% AS D1$(9%), 8% AS D1$(10%), 2% AS D1$(11%), &
		8% AS D1$(12%),8% AS D1$(13%),8% AS D1$(14%),8% AS D1$(15%), &
		8% AS D1$(16%),8% AS D1$(17%),8% AS D1$(18%),8% AS D1$(19%), &
		8% AS D1$(20%),8% AS D1$(21%),8% AS D1$(22%),3% AS D1$(23%), &
		3% AS D1$(24%),1% AS D1$(25%)		! ERNDED.DAT
250 FIELD #1%, 512%+256%+128% AS E$, 6% AS P1$(1%),6% AS P1$(2%),1% AS P1$(3%), &
		1% AS P1$(4%),2% AS P1$(5%),2% AS P1$(6%),2% AS P1$(7%), &
		7% AS P1$(8%),2% AS P1$(9%) &
	: FIELD #1%,512%+256%+128%+19%+I% AS E$,1% AS P1$(I%) FOR I%=10% TO 24% &
	: FIELD #1%,512%+256%+128%+44% AS E$,8% AS P1$(25%),3% AS P1$(26%), &
		3% AS P1$(27%),3% AS P1$(28%),3% AS P1$(29%) &
			! PAYROLL.DAT
290 FIELD #1%,512% AS T2$,256% AS T4$, 128% AS T6$, 64% AS T8$
300 RETURN
400 I%=INSTR(1%,Z$,".") : Z$="0"+Z$ IF I%=2% &
	: Z$=LEFT(Z$,2%)+RIGHT(Z$,4%) &
	: Z$=LEFT(Z$,2%)+"0"+RIGHT(Z$,3%) IF LEN(Z$)=6% &
	: Z$="PR"+Z$+"T" : RETURN
1000	! &
	!  PROGRAM CONTROL SECTION &
	!
1010 GOSUB 25
1020 GOTO 3000
2040	! &
	! ROUTINE TO SEPARATE TEMPORARY FILE &
	!
2050 LSET T4$=FNL$ : T4$(X%)=U1$(X%) FOR X%=1% TO 5% &
	: T(I%)=CVT$F(U2$(I%)) FOR I%=1% TO 12% &
	: RETURN
2300	! &
	!  SEPARATE MASTER &
	!
2310 LSET T2$=FNL$ &
	: M$(I%)=M1$(I%)+"" FOR I%=1% TO 6% &
	: M(I%)=CVT$F(M2$(I%)) FOR I%=1% TO 36% : RETURN
2500	! &
	!  SEPARATE ERNDED &
	!
2510 LSET T6$=FNL$ &
	: D$(I%)=D1$(I%)+"" FOR I%=1% TO 10% &
	: D$(11%)=FND9$(CVT$%(D1$(11%))) &
	: D(I%)=CVT$F(D1$(I%+11%)) FOR I%=1% TO 11% &
	: D(I%+11%)=FNN3(D1$(I%+22%)) FOR I%=1% TO 2% &
	: D%(1%)=ASCII(D1$(25%)) &
	: D(14%)=D(1%)-D(12%) : D(14%)=D(14%)+D(2%) UNLESS A0% &
	: D(14%)=D(14%)-D(I%) FOR I%=3% TO 11% : RETURN
2600	! &
	! SEPARATE PAYROLL &
	!
2610 LSET T8$=FNL$ &
	: O$=LEFT(P1$(4%),1%) &
	: O=1. &
	: O=1.5 IF O$="H" &
	: O=2. IF O$="D" &
	: P(1%)=CVT$F(P1$(25%)) &
	: P%(I%)=ASCII(P1$(I%+9%)) FOR I%=1% TO 15% &
	: IF P%(7%)=255% THEN P(6%)=FNN3(P1$(13%)+P1$(14%)+P1$(15%)) &
	: P(7%)=FNN3(P1$(20%)+P1$(21%)+P1$(22%)) &
	: P%(I%)=0% FOR I%=1% TO 14% : GOTO 2620
2615 P(6%),P(7%)=0. : P(6%)=P(6%)+P%(I%) FOR I%=1% TO 7% &
	: P(7%)=P(7%)+P%(I%) FOR I%=8% TO 14% &
	: P(I%)=P(I%)/10. FOR I%=6% TO 7%
2620 D1,H1=0. &
	\ IF P(1%)<R1 THEN M1=R1-P(1%) &
	\ D1=M1*P(6%) &
	\ H1=P(6%)
2625 GOTO 2630 IF P(7%)=0. &
	\ IF P(1%)*O<R1 THEN M1=R1-P(1%)*O &
	\ D1=D1+M1*P(7%) &
	\ H1=H1+P(7%)
2630 RETURN
3000	! &
	! CREATE A FILE &
	!
3020 STOP IF FNO%(9%,"TT0:EMPCKR.DAS","/CR:256/SF/EX:"+NUM1$(E9),"") &
	: PRINT"ENTER THE PAYROLL DATES. . ."
3030 Y$(I%)="" FOR I%=1% TO 100% &
	: FOR I%=1% TO 100% : PRINT"DATE FOR WEEK #";I%;"     "; &
	: INPUT Y$(I%) : GOTO 3050 IF Y$(I%)="" : NEXT I%
3050 FOR I1%=1% TO 100% &
	: GOTO 3110 IF Y$(I1%)="" : Z$=Y$(I1%)+"" : GOSUB 400
3060 IF FNO%(7%,"TX"+RIGHT(Z$,3%),"/RO","R") THEN PRINT" AN EARNINGS AND"+ &
	" DEDUCTIONS FILE FOR ";Y$(I1%);" DOES NOT EXIST !!!!!" &
	: GOTO 3100
3065 IF FNO%(5%,Z$,"/RO","") THEN PRINT " A PAYROLL FILE FOR ";Y$(I1%); &
	" DOES NOT EXIST !!!!!" &
	: GOTO 3100
3070 STOP IF FNG%(5%,"") \ GOSUB 2600 \ STOP IF FNG%(7%,"")
3080 GOSUB 2500 : G$=CVT$$(D$(10%),2%) : G$=SPACE$(6%-LEN(G$))+G$ &
	: Y$(I1%)=Y$(I1%)+SPACE$(8%-LEN(Y$(I1%))) &
	: GOSUB 3200 &
	: STOP IF FNA%(9%,D$(1%)+G$+Y$(I1%)+CVTF$(P1)+CVTF$(P2)+CVTF$(P3) &
	+CVTF$(P4)+RIGHT(T6$,7%))
3090 GOTO 3080 UNLESS FNN%(7%) : STOP IF FNC%(7%)+FNC%(5%)
3100 PRINT "Completed ";Y$(I1%) : NEXT I1%
3110  V%=FNC%(9%) : STOP IF V% &
	: STOP IF FNX%("[1,8]CONTIP.BAC",6000%,Y$) &
	: STOP
3200	! &
	! ADD UP ALL HOURS FOR ONE EMPLOYEE &
	!
3210 P1,P2,P3,P4=0. &
	: RETURN IF D$(1%)<>P1$(1%) &
	: P1=P(6%) &
	: P2=P(7%) &
	: P3=D1 &
	: P4=H1
3220 RETURN IF FNN%(5%) \ GOSUB 2600 &
	: RETURN IF D$(1%)<>P1$(1%) &
	: P1=P1+P(6%) &
	: P2=P2+P(7%) &
	: P3=P3+D1 &
	: P4=P4+H1 &
	: GOTO 3220
10000	! &
	!  TERMINATE PROGRAM &
	!
10010 STOP IF FNX%("",0%,"") : STOP
14020 DEF FND9$(V%) &
	: FND9$=RIGHT(NUM1$((V% AND 15%)+100%),2%)+ &
		"."+RIGHT(NUM1$((V% AND 31%*16%)/16%+100%),2%)+ &
		"."+RIGHT(NUM1$(((SWAP%(V%) AND 254%)/2%)+100%),2%) : FNEND
14350 DEF FNN3(E$)=CVT$%(LEFT(E$,2%))+ASCII(MID(E$,3%,1%))/100.
32767 END
