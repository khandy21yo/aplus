10	  !
	  ! Program name: [1,8]CONMON		Compiled with SCALE 0 on V07.0
	  ! Decompiled on 13-Jan-83 at 08:41 AM by UNBAC Version 1
50	  DIM M1$(23%), M$(23%), M2$(36%), M(36%), P1$(29%), P$(9%), P%(17%), 
		P(5%), D1$(25%), D$(11%), D(14%), U1$(10%), T(7%), T$(3%)
100	  !
110	  OPEN "KB:" AS FILE 1%, RECORDSIZE 512%+64%+128%+128%
120	  IF FNO%(2%,"MSTRFL.DAT","/RO","") THEN 
		  PRINT "MASTER FILE NOT FOUND !!!"
		\ GOTO 10000
200	  !
220	  FIELD #1%, 6% AS M1$(1%),30% AS M1$(2%),30% AS M1$(3%),30% AS M1$(4%),
		30% AS M1$(5%),11% AS M1$(6%),8% AS M1$(7%),1% AS M1$(8%),2%
		 AS M1$(9%),12% AS M1$(10%),2% AS M1$(11%)
221	  FIELD #1%, 152%+I%*10% AS E$,8% AS M2$(I%),2% AS M1$(I%+11%) FOR I%=
		1% TO 9%
	\ FIELD #1%, 252% AS E$,8% AS M1$(21%),8% AS M1$(22%)
	\ FIELD #1%, 188%+I%*8% AS E$,8% AS M2$(I%) FOR I%=10% TO 36%
	\ FIELD #1%, 484% AS E$,2% AS M1$(23%)
230	  FIELD #1%, 512% AS E$,6% AS P1$(1%),5% AS P1$(2%),1% AS P1$(3%),2%
		 AS P1$(4%),2% AS P1$(5%),2% AS P1$(6%),2% AS P1$(7%),7% AS 
		P1$(8%),2% AS P1$(9%)
	\ FIELD #1%, 512%+18%+I% AS E$,1% AS P1$(I%) FOR I%=10% TO 28%
	\ FIELD #1%, 512%+44% AS E$,8% AS P1$(25%),3% AS P1$(26%),3% AS P1$(27%),
		3% AS P1$(28%),3% AS P1$(29%)
240	  FIELD #1%, 512%+64% AS E$,6% AS D1$(1%),1% AS D1$(2%),2% AS D1$(3%),
		2% AS D1$(4%),2% AS D1$(5%),2% AS D1$(6%),2% AS D1$(7%),2% AS 
		D1$(8%),2% AS D1$(9%),8% AS D1$(10%),2% AS D1$(11%),8% AS D1$(
		12%),8% AS D1$(13%),8% AS D1$(14%),8% AS D1$(15%),8% AS D1$(16%),
		8% AS D1$(17%),8% AS D1$(18%),8% AS D1$(19%),8% AS D1$(20%),8%
		 AS D1$(21%),8% AS D1$(22%),3% AS D1$(23%),3% AS D1$(24%),1%
		 AS D1$(25%)
250	  FIELD #1%, 512%+64%+128% AS E$,6% AS U1$(1%),8% AS U1$(2%),6% AS U1$(
		3%),8% AS U1$(4%),8% AS U1$(5%),8% AS U1$(6%),8% AS U1$(7%),8%
		 AS U1$(8%),8% AS U1$(9%),8% AS U1$(10%)
290	  FIELD #1%, 512% AS T2$,64% AS T4$,128% AS T6$,128% AS T1$
400	  READ D$(I%) FOR I%=1% TO 7%
420	  DATA	"TAXED EARNINGS","UN-TAXED EARNINGS","FEDERAL","FICA","STATE",
		"OTHER","NET CHECK"

1000	  !
1020	  INPUT "SELECTION:",K$
	\ K$=LEFT(K$,3%)
	\ GOTO 1200 IF K$="PRI"
	\ GOTO 1100 IF K$="CRE"
	\ GOTO 10000 IF K$="END"
1030	  PRINT "CREATE	-	CREATE TEMPORARY SORT FILE"
	\ PRINT "PRINT	-	PRINT CHECK REGISTER"
	\ PRINT "END	-	END PROGRAM"
1040	  GOTO 1020
1100	  STOP IF FNO%(9%,"TT0:MONCKR.TAS","/CR:128/SF","")
1120	  PRINT "ENTER THE PAYROLL DATES. . ."
1130	  Y$(I%)="" FOR I%=1% TO 10%
	\ FOR I%=1% TO 10%
		\ PRINT "DATE FOR WEEK #";I%;"     ";
		\ INPUT Y$(I%)
		\ GOTO 1140 IF Y$(I%)=""
		\ Y$(I%)=FND7$(Y$(I%))
	\ NEXT I%
1140	  Z$="     X"
	\ Z$=Z$+SPACE$(8%-LEN(Y$(I%))) FOR I%=1% TO 10%
	\ STOP IF FNA%(9%,Z$)
1150	  FOR I1%=1% TO 10%
		\ GOTO 1195 IF Y$(I1%)=""
		\ Z$=Y$(I1%)+""
1160		  IF FNO%(6%,"TX"+LEFT(Z$,2%)+RIGHT(Z$,4%)+"T","/RO","") THEN 
			  PRINT " AN EARNINGS AND"+" DEDUCTIONS FILE FOR ";Y$(
				I1%);" DOES NOT EXIST !!!!!"
			\ GOTO 10000
1170		  STOP IF FNG%(6%,"")
1180		  GOSUB 2510
		\ LSET U1$(1%)=D$(10%)
		\ LSET U1$(2%)=D$(11%)
		\ LSET U1$(3%)=D$(1%)
		\ LSET U1$(I%+3%)=CVTF$(D(I%)) FOR I%=1% TO 5%
		\ T=0.
		\ T=T+D(I%) FOR I%=6% TO 12%
		\ LSET U1$(9%)=CVTF$(T)
		\ LSET U1$(10%)=CVTF$(D(14%))
		\ STOP IF FNA%(9%,T1$)
1190		  GOTO 1180 UNLESS FNN%(6%)
		\ STOP IF FNC%(6%)
	\ NEXT I1%
1195	  V%=FNC%(9%)
	\ STOP IF V%
	\ STOP IF FNX%("[1,8]CONMON.BAC",10%,"")
	\ STOP
1200	  !
1220	  GOTO 1300 IF FNO%(9%,"TT0:MONCKR.TAS","/SF/RO","")
	\ GOTO 1300 IF FNG%(9%,"     X")
	\ Y$(I%)=MID(FNL$,I%*8%-1%,8%) FOR I%=1% TO 10%
	\ GOTO 10000 IF FNG%(9%,"")
1230	  INPUT "ENTER PAPER LENGTH (11 OR 8.5) INCHES",A0$
	\ X8%=66%
	\ X8%=51% IF A0$="8.5"
	\ INPUT "SET (WIDE) PAGE ";A0$
	\ X9%=200%
1240	  GOSUB 5000 IF X9%>X8%-6%
1250	  GOSUB 2700
	\ GOTO 1260 IF T$(1%)="     X"
	\ STOP IF FNG%(2%,T$(3%))
	\ GOSUB 2300
	\ GOSUB 6000
	\ T1(I%)=T1(I%)+T(I%) FOR I%=1% TO 7%
1260	  GOTO 1240 UNLESS FNN%(9%)
	\ GOSUB 7000
	\ KILL "TT0:MONCKR.TAS"
	\ GOTO 10000
1300	  PRINT "PLEASE CREATE FILE"
	\ V%=FNC%(9%)
	\ GOTO 1020
2100	  !
2110	  LSET M1$(I%)=M$(I%) FOR I%=1% TO 23%
	\ LSET M2$(I%)=CVTF$(M(I%)) FOR I%=1% TO 36%
	\ RETURN
2200	  !
2210	  LSET P1$(I%)=P$(I%) FOR I%=1% TO 8%
	\ LSET P1$(9%)=CVT%$(FND9%(P$(9%)))
	\ LSET P1$(I%+9%)=CHR$(P%(I%)) FOR I%=1% TO 15%
	\ LSET P1$(25%)=CVTF$(P(1%))
	\ LSET P1$(I%+24%)=FNN3$(P(I%)) FOR I%=2% TO 5%
	\ RETURN
2300	  !
2310	  LSET T2$=FNL$
	\ M$(I%)=M1$(I%)+"" FOR I%=1% TO 23%
	\ M(I%)=CVT$F(M2$(I%)) FOR I%=1% TO 36%
	\ RETURN
2400	  !
2410	  LSET T4$=FNL$
	\ P$(I%)=P1$(I%)+"" FOR I%=1% TO 8%
	\ P%(I%)=ASCII(P1$(I%+9%)) FOR I%=1% TO 15%
	\ P(1%)=CVT$F(P1$(25%))
	\ P(I%)=FNN3(P1$(I%+24%)) FOR I%=2% TO 5%
	\ P%(0%)=CVT$%(P1$(9%))
	\ P$(9%)=FND9$(P%(0%))
	\ P%(16%),P%(17%)=0%
	\ P%(16%)=P%(16%)+P%(I%) FOR I%=1% TO 7%
	\ P%(17%)=P%(17%)+P%(I%) FOR I%=8% TO 14%
	\ RETURN
2500	  !
2510	  LSET T6$=FNL$
	\ D$(I%)=D1$(I%)+"" FOR I%=1% TO 10%
	\ D$(11%)=FND9$(CVT$%(D1$(11%)))
	\ D(I%)=CVT$F(D1$(I%+11%)) FOR I%=1% TO 11%
	\ D(I%+11%)=FNN3(D1$(I%+22%)) FOR I%=1% TO 2%
	\ D%(1%)=ASCII(D1$(25%))
	\ D(14%)=D(1%)+D(2%)-D(12%)
	\ D(14%)=D(14%)-D(I%) FOR I%=3% TO 11%
	\ RETURN
2600	  !
2610	  LSET D1$(I%)=D$(I%) FOR I%=1% TO 10%
	\ LSET D1$(11%)=CVT%$(FND9%(D$(11%)))
	\ LSET D1$(I%+11%)=CVTF$(D(I%)) FOR I%=1% TO 12%
	\ LSET D1$(24%)=FNN3$(D(13%))
	\ LSET D1$(25%)=CVT%$(D%(1%))
	\ RETURN
2700	  !
2710	  LSET T1$=FNL$
	\ T$(I%)=U1$(I%)+"" FOR I%=1% TO 3%
	\ T(I%)=CVT$F(U1$(I%+3%)) FOR I%=1% TO 7%
	\ RETURN
5000	  PRINT  FOR L%=1% TO X8%-X9%
	\ PRINT 
	\ PRINT 
	\ PRINT 
5010	  PRINT TAB(42%);"TAX  UN-TAX";TAB(90%);"NET"
	\ PRINT "CHECK #  EMP #  NAME              M#     EARN "+
		"   EARN     FED    FICA   STATE   OTHER   CHECK  DATE"
5020	  PRINT 
	\ X9%=6%
	\ RETURN
6000	  !
6010	  PRINT USING "\    \ \    \ \"+SPACE$(17%)+"\ !\\", T$(1%),M$(1%),M$(
		2%),M$(8%),M$(9%);
6020	  A=T(1%)
	\ GOSUB 6970
6030	  A=T(2%)
	\ GOSUB 6970
	\ FOR I%=3% TO 5%
		\ A=T(I%)
		\ GOSUB 6970
	\ NEXT I%
6040	  A=T(6%)
	\ GOSUB 6970
6050	  A=T(7%)
	\ GOSUB 6970
6100	  PRINT "  ";T$(2%)
	\ X9%=X9%+1%
	\ RETURN
6950	  IF A=0. THEN
		  PRINT "  *  ";
	  ELSE
		  PRINT USING "##.##", A;
6955	  RETURN
6960	  IF A=0. THEN
		  PRINT "   *  ";
	  ELSE
		  PRINT USING "###.##", A;
6965	  RETURN
6970	  IF A=0. THEN
		  PRINT "     *  ";
	  ELSE
		  PRINT USING "#####.##", A;
6975	  RETURN
7000	  !
7010	  PRINT  FOR I%=1% TO X8%-X9% IF X9%>X8%-17%
7020	  PRINT  FOR I%=1% TO 5%
	\ FOR I%=1% TO 7%
		\ PRINT TAB(5%);
		\ PRINT USING "\                      \    #,###,###.## ", 
			"TOTAL "+D$(I%),T1(I%)
	\ NEXT I%
	\ RETURN
10000	  !
10010	  STOP IF FNX%("",0%,"")
14000	  !
14010	  DEF FND9%(D9$)
	\ D9$="0"+D9$ IF INSTR(1%,D9$,".")=2%
	\ D9$=LEFT(D9$,3%)+"0"+RIGHT(D9$,4%) IF INSTR(4%,D9$,".")=5%
	\ FND9%=VAL(LEFT(D9$,2%))+VAL(MID(D9$,4%,2%))*16%+FND8%(VAL(RIGHT(D9$,
		7%)))*512%
	\ FNEND
14020	  DEF FND9$(D9%)
	\ FND9$=RIGHT(NUM1$((D9% AND 15%)+100%),2%)+"."+RIGHT(NUM1$((D9% AND 
		31%*16%)/16%+100%),2%)+"."+RIGHT(NUM1$((SWAP%(D9%) AND 254%)/2%
		+100%),2%)
	\ FNEND
14030	  DEF FND8%(D8)
	\ FND8%=D8
	\ FNEND
14100	  !
14110	  DEF FNA8$(X$)
	\ FNA8$=CVT%$(VAL(LEFT(X$,3%)))+CHR$(VAL(RIGHT(X$,5%)))
	\ FNEND
14120	  DEF FNA7$(X$)
	\ FNA7$=RIGHT(NUM1$(1000%+CVT$%(X$)),2%)+"."+RIGHT(NUM1$(100%+ASCII(
		RIGHT(X$,3%))),2%)
	\ FNEND
14200	  !
14210	  DEF FND7%(D7$)
	\ ON ERROR GOTO 14220
	\ GOTO 14220 IF INSTR(1%,D7$,".")<>3% OR INSTR(4%,D7$,".")<>6% OR 
		INSTR(7%,D7$,".")<>0% OR LEN(D7$)<>8%
	\ D7%=VAL(LEFT(D7$,2%))
	\ GOTO 14220 IF D7%<1% OR D7%>12%
	\ D7%=VAL(MID(D7$,4%,2%))
	\ GOTO 14220 IF D7%<1% OR D7%>31%
	\ D7%=VAL(RIGHT(D7$,7%))
	\ GOTO 14220 IF D7%<0%
	\ FND7%=0%
	\ GOTO 14230
14220	  FND7%=-1%
	\ RESUME 14230
14230	  ON ERROR GOTO 0
	\ FNEND
14250	  !
14260	  DEF FND7$(D7$)
	\ D7$=D7$+"."+RIGHT(DATE$(0%),8%) IF LEN(D7$)<6%
	\ D7$="0"+D7$ IF INSTR(1%,D7$,".")=2%
	\ D7$=LEFT(D7$,3%)+"0"+RIGHT(D7$,4%) IF INSTR(4%,D7$,".")=5%
	\ FND7$=D7$
	\ FNEND
14300	  !
14310	  DEF FNN3$(N3)
	\ FNN3$=CVT%$(INT(N3))+CHR$((N3-INT(N3))*100.+.51)
	\ FNEND
14350	  DEF FNN3(N3$)
	\ FNN3=CVT$%(LEFT(N3$,2%))+ASCII(MID(N3$,3%,1%))/100.
	\ FNEND
32767	  END

