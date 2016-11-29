10	  ! &
	  ! Program name: conded		Compiled with SCALE 0 on V07.0 &
	  ! Decompiled on 28-Nov-16 at 04:11 PM
50	  DIM M1$(23%), M$(23%), M2$(36%), M(36%), P1$(29%), P$(9%), P%(17%), P(5%), D1$(25%), D$(11%), D(14%), D%(1%), T(7%), T1(7%), T2(7%)
60	  IF FNO%(9%,"DEDUCT.TMP","","")=0% AND F%=-1% THEN  &
		  STOP IF FNG%(9%,"") &
		\ Z$=MID(FNL$,15%,8%)
100	  !
110	  OPEN "KB:" AS FILE 1%, RECORDSIZE 512%+64%+128%
120	  IF FNO%(2%,"MSTRFL.DAT","/RO","") THEN  &
		  PRINT "MASTER FILE NOT FOUND !!!" &
		\ GOTO 10000
130	  PRINT  &
	\ INPUT "PAYROLL DATE (MM.DD.YY) ";Z$ IF Z$="" &
	\ GOTO 10000 IF Z$="" &
	\ Z$=FND7$(Z$) &
	\ Z1$=LEFT(Z$,2%)+RIGHT(Z$,4%)+"T" &
	\ IF FND7%(Z$) THEN  &
		  PRINT "BAD DATE !!! -- RE ENTER (TYPE <RETURN> TO END)" &
		\ Z$="" &
		\ GOTO 130
140	  IF FNO%(4%,"PR"+Z1$,"/RO","") THEN  &
		  PRINT "PAYROLL FILE DOES NOT EXIST !!!" &
		\ GOTO 10000
150	  IF FNO%(6%,"TX"+Z1$,"/RO","") THEN  &
		  PRINT "EARNINGS AND DEDUCTIONS FILE DOES NOT EXIST !!!" &
		\ GOTO 10000
200	  !
220	  FIELD #1%, 6% AS M1$(1%),30% AS M1$(2%),30% AS M1$(3%),30% AS M1$(4%),30% AS M1$(5%),11% AS M1$(6%),8% AS M1$(7%),1% AS M1$(8%),2% AS M1$(9%),12% AS M1$(10%),2% AS M1$(11%)
221	  FIELD #1%, 152%+I%*10% AS E$,8% AS M2$(I%),2% AS M1$(I%+11%) FOR I%=1% TO 9% &
	\ FIELD #1%, 252% AS E$,8% AS M1$(21%),8% AS M1$(22%) &
	\ FIELD #1%, 188%+I%*8% AS E$,8% AS M2$(I%) FOR I%=10% TO 36% &
	\ FIELD #1%, 484% AS E$,2% AS M1$(23%)
230	  FIELD #1%, 512% AS E$,6% AS P1$(1%),6% AS P1$(2%),1% AS P1$(3%),1% AS P1$(4%),2% AS P1$(5%),2% AS P1$(6%),2% AS P1$(7%),7% AS P1$(8%),2% AS P1$(9%) &
	\ FIELD #1%, 512%+19%+I% AS E$,1% AS P1$(I%) FOR I%=10% TO 24% &
	\ FIELD #1%, 512%+44% AS E$,8% AS P1$(25%),3% AS P1$(26%),3% AS P1$(27%),3% AS P1$(28%),3% AS P1$(29%)
240	  FIELD #1%, 512%+64% AS E$,6% AS D1$(1%),1% AS D1$(2%),2% AS D1$(3%),2% AS D1$(4%),2% AS D1$(5%),2% AS D1$(6%),2% AS D1$(7%),2% AS D1$(8%),2% AS D1$(9%),8% AS D1$(10%),2% AS D1$(11%),8% AS D1$(12%),8% AS D1$(13%),8% AS D1$(14%),8% AS D1$(15%),8% AS D1$(16%),8% AS D1$(17%),8% AS D1$(18%),8% AS D1$(19%),8% AS D1$(20%),8% AS D1$(21%),8% AS D1$(22%),3% AS D1$(23%),3% AS D1$(24%),1% AS D1$(25%)
290	  FIELD #1%, 512% AS T2$,64% AS T4$,128% AS T6$
400	  U1$="   OT RATE=####.###     O.T.=" &
	\ U8$="   RT RATE=####.###     R.T.="
401	  U2$="####.##" &
	\ U7$="###,###,###.##"
402	  U3$="#####.##  #####.##  #####.##  * ####.## ####.## ####.## ####.## #####.##"
403	  U4$="###### ######"
404	  U5$="####.# ###.# #####.## ####.## ####.## ##,###.##" &
	\ U6$="####.# "
990	  DEF FNZ(Z2) &
	\ FNZ=INT(Z2*100.+0.5)/100. &
	\ FNEND
999	  RETURN IF F%
1000	  !
1020	  PRINT  &
	\ INPUT "PRINT DEDUCTIONS REPORT ";K$ &
	\ GOTO 10000 IF LEFT(K$,1%)<>"Y" &
	\ GOTO 6000
2300	  !
2310	  LSET T2$=FNL$ &
	\ M$(I%)=M1$(I%)+"" FOR I%=1% TO 23% &
	\ M(I%)=CVT$F(M2$(I%)) FOR I%=1% TO 36% &
	\ RETURN
2400	  !
2410	  LSET T4$=FNL$ &
	\ P$(I%)=P1$(I%)+"" FOR I%=1% TO 8% &
	\ P%(I%)=ASCII(P1$(I%+9%)) FOR I%=1% TO 15% &
	\ P(1%)=CVT$F(P1$(25%)) &
	\ P(I%)=FNN3(P1$(I%+24%)) FOR I%=2% TO 5% &
	\ P%(0%)=CVT$%(P1$(9%)) &
	\ P$(9%)=FND9$(P%(0%)) &
	\ P%(16%),P%(17%)=0% &
	\ P%(16%)=P%(16%)+P%(I%) FOR I%=1% TO 7% &
	\ P%(17%)=P%(17%)+P%(I%) FOR I%=8% TO 14% &
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
6000	  !
6005	  V%=FNC%(9%) &
	\ OPEN "DEDUCT.TMP" FOR OUTPUT AS FILE 9% &
	\ PRINT #9%, CVT%$(0%)+CVT%$(16%)+"S"+CHR$(128%); &
	\ CLOSE 9% &
	\ OPEN "DEDUCT.TM1" FOR OUTPUT AS FILE 9% &
	\ PRINT #9%, CVT%$(0%)+CVT%$(32%)+"S"+CHR$(128%); &
	\ CLOSE 9% &
	\ GOTO 10000 IF FNO%(9%,"DEDUCT.TMP","","")
6010	  STOP IF FNA%(9%,STRING$(14%,0%)+Z$) &
	\ IF FNG%(4%,"") THEN  &
		  PRINT "FILE IS EMPTY." &
		\ GOTO 10000
6030	  GOSUB 2400 &
	\ A$=P$(1%) &
	\ GOTO 6070 IF A$=A1$ &
	\ A1$=A$ &
	\ V%=FNG%(2%,A$) &
	\ GOSUB 2300 &
	\ V%=FNG%(6%,A$)
6035	  GOSUB 2500
6040	  FOR I%=4% TO 9% &
		\ GOTO 6060 IF D(I%+2%)=0. &
		\ B$=D$(I%) &
		\ V%=FNM%(B$+A$,D(I%+2%))
6060			  NEXT I% &
	\ V%=FNN%(6%) &
	\ IF V%=0% AND A$=LEFT(FNL$,6%) THEN  &
		  GOTO 6035
6070	  FOR I%=3% TO 4% &
		\ GOTO 6090 IF P(I%)=0% &
		\ B=P(I%) &
		\ V%=FNM%(P$(I%+2%)+A$,B)
6090			  NEXT I%
6100	  V%=FNN%(4%) &
	\ GOTO 6030 IF V%=0% &
	\ STOP IF V%<>11%
6110	  V%=FNX%("[1,8]CONDED",6120%,"")
6120	  V$=SYS(CHR$(7%)) &
	\ V%=INSTR(1%,V$,CHR$(255%)) &
	\ V$=SYS(CHR$(8%)+RIGHT(V$,V%)) UNLESS V%=0% &
	\ F%=-1% &
	\ GOSUB 10 &
	\ INPUT #1%, "SET PAGE. . . ";K$ &
	\ L%=60% &
	\ T1=0. &
	\ GOSUB 6230
6130	  IF FNG%(9%,"")+FNN%(9%)<>0% THEN  &
		  PRINT "NO DEDUCTIONS FOUND." &
		\ GOTO 10000
6140	  B$=FNL$ &
	\ A$=MID(B$,3%,6%) &
	\ STOP IF FNG%(2%,A$) &
	\ GOSUB 2300
6145	  B2$=LEFT(B$,2%) &
	\ L%=L%+1% &
	\ GOSUB 6230 IF L%>55% &
	\ GOSUB 6240 IF B1$<>B2$
6150	  PRINT LEFT(B$,2%);"  ";M$(1%);" ";LEFT(M$(2%),20%); &
	\ PRINT USING TAB(31%)+"##,###.##", CVT$F(RIGHT(B$,9%))
6155	  T=T+CVT$F(RIGHT(B$,9%))
6160	  V%=FNN%(9%) &
	\ GOTO 6140 IF V%=0% &
	\ STOP IF V%<>11% &
	\ GOSUB 6240 &
	\ PRINT  &
	\ PRINT "************* GRAND TOTAL ***";TAB(31%); &
	\ PRINT USING "##,###.##", T1 &
	\ PRINT  &
	\ GOTO 10000
6200	  DEF FNM%(B$,B1) &
	\ B$=B$+STRING$(8%-LEN(B$),32%)+CVTF$(B1) &
	\ V%=FNA%(9%,B$) &
	\ STOP IF V%
6210	  FNEND
6230	  PRINT STRING$(66%-L%,10%) &
	\ PRINT "      ";S5$;"     PAYROLL DEDUCTIONS    ";Z$ &
	\ PRINT "CD  EMP #       NAME               AMOUNT" &
	\ PRINT  &
	\ L%=3% &
	\ RETURN
6240	  IF B1$<>"" THEN  &
		  PRINT "************* TOTAL ***";TAB(31%); &
		\ PRINT USING "##,###.##", T &
		\ T1=T1+T &
		\ PRINT  &
		\ T=0. &
		\ L%=L%+2%
6250	  B1$=B2$ &
	\ RETURN
10000	  !
10010	  V%=FNC%(2%)+FNC%(9%)+FNC%(5%)+FNC%(7%) &
	\ CLOSE 1% &
	\ CLOSE 4% &
	\ CLOSE 11% &
	\ CLOSE 12% &
	\ V$=SYS(CHR$(7%)) &
	\ Q3$=CVT%$(8100%)+"!MENU"+V$ IF ASCII(V$)=255% &
	\ V%=FNC%(0%)
10020	  CHAIN "!MENU" 0. IF ASCII(V$)=255% &
	\ GOTO 32767
13010	  OPEN "PRREP.TMP" FOR OUTPUT AS FILE 9% &
	\ PRINT #9%, CVT%$(0%)+CVT%$(16%)+"S"+CHR$(128%); &
	\ CLOSE 9% &
	\ OPEN "PRREP.TM1" FOR OUTPUT AS FILE 9% &
	\ PRINT #9%, CVT%$(0%)+CVT%$(32%)+"S"+CHR$(128%); &
	\ CLOSE 9% &
	\ RETURN
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
