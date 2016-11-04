10	  ! &
	  ! Program name: [1,8]CONPA2		Compiled with SCALE 0 on V07.0 &
	  ! Decompiled on 24-Mar-84 at 06:30 PM by UNBAC Version 1
50	  DIM M1$(23%), M$(23%), M2$(44%), M(44%), P1$(29%), P$(9%), P%(15%), &
		P(7%), J$(12%), R(7%), O(7%), L$(5%), A(5%), A$(5%), R$(5%), &
		R1$(5%)
100	  !
110	  OPEN "KB:" AS FILE 1%, RECORDSIZE 512%+64%+64% &
	\ OPEN "KB:" FOR INPUT AS FILE 12%
120	  IF FNO%(2%,"MSTRFL.DAT","/RO","") THEN &
		  PRINT "CAN'T OPEN MASTER FILE.  ERROR ";FNS% &
		\ GOTO 10000
121	  IF FNG%(2%,"ZP")<>0% THEN &
		  GOTO 130 &
	  ELSE &
		  IF RIGHT(FNL$,512%)<>"5" THEN &
			  GOTO 130
125	  IF FNO%(6%,"RATE.DAS","/SF/RO","")=0% THEN &
		  B%=-1%
130	  INPUT "PAYROLL DATE (MM.DD.YY) ";Z$ UNLESS Z$<>"" &
	\ GOTO 10000 IF Z$="" &
	\ Z$=FND7$(Z$) &
	\ Z1$=LEFT(Z$,2%)+RIGHT(Z$,4%)+"T" &
	\ IF FND7%(Z$) THEN &
		  PRINT "BAD DATE, PLEASE RE-ENTER." &
		\ Z$="" &
		\ GOTO 130
140	  GOSUB 500 IF FNO%(4%,"PR"+Z1$,"","") &
	\ V%=FNO%(8%,"JOB.DAT","/RO","")
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
290	  FIELD #1%, 512% AS T2$,64% AS T4$
300	  IF B% THEN &
		  FIELD #1%, 582%+10%*(I%-1%) AS E$,8% AS R$(I%),2% AS R1$(I%) &
			FOR I%=1% TO 5% &
		\ FIELD #1%, 576% AS E$,64% AS T6$
400	  !
410	  ON ERROR GOTO 420 &
	\ OPEN "SS0:UNIQUE.FIL/RO" FOR INPUT AS FILE 10% &
	\ DIM #10%, A0$(255%)=64% &
	\ A%=INSTR(1%,A0$(15%),"LEN=") &
	\ L%=VAL(MID(A0$(15%),A%+4%,1%)) IF A% &
	\ L%=0% IF A%=0% &
	\ GOTO 430
420	  RESUME 430
430	  ON ERROR GOTO 0 &
	\ CLOSE 10% &
	\ GOTO 1000
500	  GOTO 550 IF FNS%<>5% &
	\ INPUT "CONFIRM CREATING A NEW PAYROLL FILE";K$ &
	\ GOTO 10000 UNLESS LEFT(K$,1%)="Y" &
	\ OPEN "PR"+Z1$ FOR OUTPUT AS FILE 10% &
	\ PRINT #10%, CVT%$(0%)+CVT%$(13%)+"S"+CHR$(128%) &
	\ CLOSE 10% &
	\ OPEN "PR"+LEFT(Z1$,7%)+"1" FOR OUTPUT AS FILE 10% &
	\ PRINT #10%, CVT%$(0%)+CVT%$(64%)+"S"+CHR$(128%) &
	\ CLOSE 10% &
	\ PRINT "A NEW PAYROLL FILE HAS BEEN OPENED" &
	\ IF FNO%(4%,"PR"+Z1$,"U","R") THEN &
		  PRINT "HELP." &
		\ GOTO 10000
510	  RETURN
550	  PRINT "ERROR";FNS% &
	\ GOTO 10000
1000	  !
1005	  GOTO 8000
1010	  V$=FNX$ &
	\ Z$=LEFT(V$,2%)+"."+MID(V$,3%,2%)+"."+RIGHT(V$,5%) &
	\ GOTO 10
2000	  !
2010	  PRINT &
	\ PRINT "EMPLOYEE # "; &
	\ INPUT LINE #12%, A$ &
	\ A$=CVT$$(A$,4%) &
	\ RETURN IF A$="" &
	\ IF A$<>"ECHO ON" AND A$<>"ECHO OFF" THEN &
		  A$=A$+SPACE$(6%-LEN(A$)) &
		\ RETURN
2020	  E9%=2% &
	\ E9%=3% IF A$="OFF" &
	\ GOTO 2010
2200	  !
2210	  LSET P1$(I%)=P$(I%) FOR I%=1% TO 8% &
	\ LSET P1$(9%)=CVT%$(FND9%(P$(9%))) &
	\ LSET P1$(I%+9%)=CHR$(P%(I%)) FOR I%=1% TO 15% &
	\ LSET P1$(25%)=CVTF$(P(1%)) &
	\ LSET P1$(I%+24%)=FNN3$(P(I%)) FOR I%=2% TO 5% &
	\ RETURN
2300	  !
2310	  LSET T2$=FNL$ &
	\ M$(I%)=M1$(I%)+"" FOR I%=1% TO 23% &
	\ M(I%)=CVT$F(M2$(I%)) FOR I%=1% TO 36% &
	\ M(I%)=CVT$%(M2$(I%))/10. FOR I%=37% TO 44% &
	\ RETURN
2400	  !
2410	  LSET T4$=FNL$ &
	\ P%(I%)=ASCII(P1$(I%+9%)) FOR I%=1% TO 15% &
	\ RETURN
2500	  !
2510	  LSET T6$=FNL$ &
	\ FOR I%=1% TO 5% &
		\ A(I%)=CVT$F(R$(I%)) &
		\ A$(I%)=R1$(I%)+"" &
	\ NEXT I% &
	\ RETURN
8000	  !
8002	  GOTO 8003 IF FNG%(4%,"") &
	\ GOSUB 2400 &
	\ IF P%(15%) AND 1% OR P%(15%) AND 2% THEN &
		  PRINT "PAYROLL HAS BEEN UPDATED." &
		\ GOTO 10000
8003	  E9%=2% IF E9%=0% &
	\ K2%=-1% &
	\ INPUT "DAY NUMBER (ANSWER ONLY IF ENTERING A SINGLE DAY)";K2% WHILE &
		K2%<0% OR K2%>7% &
	\ K2%=1% IF K2%=0%
8010	  V$=SYS(CHR$(2%)) &
	\ GOSUB 2000 &
	\ GOTO 8140 IF A$="" &
	\ IF FNG%(2%,A$) THEN &
		  PRINT "IMPROPER EMPLOYEE #" &
		\ GOTO 8010
8015	  V$=SYS(CHR$(E9%)) &
	\ GOSUB 2300 &
	\ A$=M$(1%)+"" &
	\ IF B% THEN &
		  STOP IF FNG%(6%,A$) &
		\ GOSUB 2500
8020	  J$(I%)="" FOR I%=1% TO 12% &
	\ L$(I%)="" FOR I%=1% TO 5% &
	\ PRINT M$(2%);" JOB # "; &
	\ INPUT LINE #12%, K$ &
	\ K$=CVT$$(M$(10%),2%)+K$ IF ASCII(K$)=32% &
	\ K$=CVT$$(K$,4%+128%) &
	\ PRINT K$ IF E9%=3% &
	\ GOTO 8010 IF K$="" &
	\ GOTO 8500 IF K$="?" &
	\ K$=K$+" " &
	\ K%=K2% &
	\ R(I%),O(I%)=0. FOR I%=1% TO 7% &
	\ P%(I%)=0% FOR I%=1% TO 15% &
	\ P(I%)=0. FOR I%=1% TO 5% &
	\ P$(8%)=SPACE$(7%) &
	\ I%=INSTR(1%,K$," ") &
	\ P$(1%)=A$ &
	\ P$(2%)=LEFT(K$,I%) &
	\ K$=RIGHT(K$,I%) &
	\ IF K$="" THEN &
		  PRINT "LAST INPUT ABORTED- NO COST DATA." &
		\ GOTO 8020
8022	  K1%=0% &
	\ FOR I2%=1% TO 3% &
		\ I%=0%
8024		  I%=INSTR(I%+1%,K$,MID("RED",I2%,1%)) &
		\ GOTO 8024 IF I% AND MID(K$,I%-1%,1%)<>" " &
		\ IF I% THEN &
			  I1%=INSTR(I%,K$," ") &
			\ K1%=K1%+1% &
			\ L$(K1%)=MID(K$,I%,I1%-I%) &
			\ K$=LEFT(K$,I%-1%)+RIGHT(K$,I1%+1%) &
			\ I%=0% &
			\ GOTO 8024
8026	  NEXT I2% &
	\ K$=RIGHT(K$,2%)
8030	  I%=INSTR(1%,K$," ") &
	\ IF I% THEN &
		  J$(K%)=LEFT(K$,I%-1%) &
		\ K$=RIGHT(K$,I%+1%) &
		\ K%=K%+1% &
		\ GOTO 8030
8040	  K%=K%-1% &
	\ FOR I1%=1% TO K% &
		\ I%=INSTR(1%,J$(I1%),"S") &
		\ U7$="S" &
		\ GOTO 8065 IF I% &
		\ U7$="Z" &
		\ I%=INSTR(1%,J$(I1%),"Z") &
		\ IF I%=0% THEN &
			  GOTO 8068
8065		  P$(8%)=LEFT(P$(8%),I1%-1%)+U7$+RIGHT(P$(8%),I1%+1%) &
		\ J$(I1%)=LEFT(J$(I1%),I%-1%)+RIGHT(J$(I1%),I%+1%)
8068	  NEXT I1% &
	\ IF K%>7% THEN &
		  PRINT "LAST INPUT ABORTED - TOO MANY DAYS." &
		\ GOTO 8020
8070	  O$=M$(12%) IF B%=0% &
	\ O$=A$(1%) IF B% &
	\ FOR I1%=1% TO K% &
		\ I%=INSTR(1%,J$(I1%),"O") &
		\ I%=INSTR(2%,J$(I1%),"H") IF I%=0% &
		\ I%=INSTR(2%,J$(I1%),"D") IF I%=0% &
		\ O$=MID(J$(I1%),I%,1%) IF I%<>0% AND (MID(J$(I1%),I%,1%)="D" &
			 OR MID(J$(I1%),I%,1%)="H") &
		\ O(I1%)=VAL(RIGHT(J$(I1%),I%+1%)) IF I% &
		\ J$(I1%)=LEFT(J$(I1%),I%-1%) IF I% &
		\ R(I1%)=VAL(J$(I1%)) &
	\ NEXT I1% &
	\ GOTO 8076 IF B%=0% AND M$(12%)<>"HF" AND M$(12%)<>"DF" OR B%<>0% AND &
		A$(1%)<>"HF" AND A$(1%)<>"DF" &
	\ P%(7%)=255% &
	\ R,O=0. &
	\ R=R+R(I%) FOR I%=1% TO 7% &
	\ O=O+O(I%) FOR I%=1% TO 7% &
	\ P%(5%)=INT(R) &
	\ P%(4%)=SWAP%(P%(5%)) &
	\ P%(6%)=(R-INT(R))*100.+.51 &
	\ P%(12%)=INT(O) &
	\ P%(11%)=SWAP%(P%(12%)) &
	\ P%(13%)=(O-INT(O))*100.+.51 &
	\ GOTO 8090
8076	  IF R(K2%)>25. THEN &
		  T1=8. &
		\ T1=INT((R(K2%)+6%)/7.) IF R(K2%)>56% &
		\ T=R(K2%) &
		\ FOR I%=1% UNTIL T<T1 &
			\ R(I%)=T1 &
			\ T=T-T1 &
		\ NEXT I% &
		\ IF I%<8% THEN &
			  R(I%)=T &
		  ELSE &
			  R(I%-1%)=R(I%-1%)+T
8077	  P(2%)=0. &
	\ P$(3%)="" &
	\ GOTO 8080 IF P$(8%)=SPACE$(7%) &
	\ I%=INSTR(1%,P$(2%),"-")-1% &
	\ I%=LEN(P$(2%)) IF I%<=0% &
	\ I%=L% IF L%<>0% &
	\ IF FNG%(8%,LEFT(P$(2%),I%)) THEN &
		  INPUT "TOTAL AMOUNT OF SUBSISTENCE ";T &
		\ P(2%)=T &
		\ GOTO 8080
8078	  W1=CVT$F(MID(FNL$,185%,8%)) &
	\ U7$=LEFT(M$(10%),2%) &
	\ W1=CVT$F(MID(FNL$,193%,8%)) IF U7$=MID(FNL$,203%,2%) UNLESS U7$="  " &
	\ IF W1=0. THEN &
		  INPUT "TOTAL AMOUNT OF SUBSISTENCE ",T &
		\ P(2%)=T &
		\ GOTO 8080
8079	  FOR I%=1% TO 7% &
		\ U7$=MID(P$(8%),I%,1%) &
		\ T=0. &
		\ T=R(I%)+O(I%) IF U7$="S" &
		\ T=8. IF T>8. OR U7$="Z" &
		\ P(2%)=P(2%)+FNZ(T/8.*W1) &
	\ NEXT I%
8080	  P$(3%)="T" IF INSTR(1%,M$(10%),"*") &
	\ FOR I%=1% TO 7% &
		\ P%(I%)=INT(R(I%)*10.+.51) &
		\ P%(I%+7%)=INT(O(I%)*10.+.51) &
	\ NEXT I%
8090	  P(1%)=M(1%) UNLESS B% &
	\ P(1%)=A(1%) IF B% &
	\ P$(4%)=O$ &
	\ FOR I1%=1% TO K1% &
		\ FOR I2%=1% TO 3% &
			\ GOTO 8130 IF LEFT(L$(I1%),1%)=MID("RED",I2%,1%) &
		\ NEXT I2%
8110	  NEXT I1% &
	\ GOSUB 2200 &
	\ STOP IF FNA%(4%,T4$) &
	\ GOTO 8020
8130	  ON I2% GOTO 8400,8200,8300
8140	  GOTO 10000
8200	  !
8220	  IF P(5%)<>0. THEN &
		  PRINT "ONLY ONE OTHER EARNING ALLOWED PER RECORD." &
		\ PRINT "PLEASE ENTER EACH OTHER EARNING ON A SEPARATE LINE." &
		\ GOTO 8020
8230	  I%=INSTR(3%,L$(I1%),"-") &
	\ P(5%)=VAL(MID(L$(I1%),2%,I%-2%)) &
	\ P$(7%)=RIGHT(L$(I1%),I%+1%) &
	\ GOTO 8110
8300	  !
8310	  I%=INSTR(3%,L$(I1%),"-") &
	\ IF P(3%)=0. THEN &
		  P(3%)=VAL(MID(L$(I1%),2%,I%-2%)) &
		\ P$(5%)=RIGHT(L$(I1%),I%+1%) &
		\ GOTO 8110
8320	  IF P(4%)=0. THEN &
		  P(4%)=VAL(MID(L$(I1%),2%,I%-2%)) &
		\ P$(6%)=RIGHT(L$(I1%),I%+1%) &
		\ GOTO 8110
8330	  PRINT "ONLY TWO OTHER DEDUCTIONS ALLOWED PER RECORD." &
	\ PRINT "PLEASE ENTER THE REST ON OTHER LINES." &
	\ GOTO 8020
8400	  !
8410	  GOTO 8420 IF LEN(L$(I1%))<>2% &
	\ I=VAL(RIGHT(L$(I1%),2%)) &
	\ IF B%=0% THEN &
		  P(1%)=M(I) &
		\ P$(4%)=M$(I+11%) &
		\ GOTO 8110
8415	  P(1%)=A(I) &
	\ P$(4%)=A$(I)+"" &
	\ GOTO 8110
8420	  I%=INSTR(1%,L$(I1%),".") &
	\ P(1%)=VAL(MID(L$(I1%),2%,I%+1%)) &
	\ I%=INSTR(1%,L$(I1%),"-") &
	\ P$(4%)=RIGHT(L$(I1%),I%+1%) IF I% &
	\ GOTO 8110
8500	  !
8510	  PRINT &
	\ PRINT "ENTER ANY COMBINATION OF THE FOLLOWING, SEPARATING EACH WITH" &
	\ PRINT "A SPACE (THE FIRST ITEM MUST BE JOB NUMBER ). . ." &
	\ PRINT &
	\ PRINT " JOB NUMBER          ENTER THE JOB NUMBER" &
	\ PRINT "                     ENTER JOB NUMBER-PHASE IF JOB IS" &
	\ PRINT "                       DIVIDED INTO PHASES" &
	\ PRINT "                     ENTER A G/L NUMBER TO APPLY LABOR" &
	\ PRINT "                       TO A SPECIFIC ACCOUNT" &
	\ PRINT &
	\ PRINT " HOURS               ENTER HOURS BY INDIVIDUAL DAY," &
	\ PRINT "                       SEPARATING EACH DAY BY A SPACE." &
	\ PRINT "                     OR ENTER JUST TOTAL HOURS; THE PROGRAM" &
	\ PRINT "                       WILL DIVIDE UP AS NECESSARY." &
	\ PRINT "                     ADD 'O','D' OR 'H'"+ &
		" AND OVERTIME HOURS FOR ANY DAY" &
	\ PRINT "                     ADD 'S' TO ADD SUBSISTENCE FOR ANY DAY" &
	\ PRINT &
	\ PRINT " RATE                ENTER 'R' AND A NUMBER 1-3 TO SPECIFY" &
	\ PRINT "                       MSTRFL RATE TO USE (1 IF NOT ENTERED)" &
	\ PRINT &
	\ PRINT "OTHER EARNINGS        ENTER 'E',AMOUNT,'-',CODE," &
	\ PRINT "                       AS IN 'E25.00-11'" &
	\ PRINT &
	\ PRINT " DEDUCTIONS           SAME AS EARNINGS EXCEPT USE 'D'" &
	\ PRINT &
	\ GOTO 8020
10000	  !
10010	  CLOSE 1% &
	\ CLOSE 12% &
	\ V%=FNX%("[1,8]CONPA1.BAC",8100%,LEFT(Z1$,4%)+MID(Z1$,6%,2%))
14010	  DEF FND9%(E$) &
	\ E$="0"+E$ IF INSTR(1%,E$,".")=2% &
	\ E$=LEFT(E$,3%)+"0"+RIGHT(E$,4%) IF INSTR(4%,E$,".")=5% &
	\ FND9%=VAL(LEFT(E$,2%))+VAL(MID(E$,4%,2%))*16%+FND8%(VAL(RIGHT(E$,7%)) &
		)*512% &
	\ FNEND
14020	  DEF FNZ(Z) &
	\ FNZ=INT(Z*100.+.51)/100. &
	\ FNEND
14030	  DEF FND8%(Z) &
	\ FND8%=Z &
	\ FNEND
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
14260	  DEF FND7$(E$) &
	\ E$=E$+"."+RIGHT(DATE$(0%),8%) IF LEN(E$)<6% &
	\ E$="0"+E$ IF INSTR(1%,E$,".")=2% &
	\ E$=LEFT(E$,3%)+"0"+RIGHT(E$,4%) IF INSTR(4%,E$,".")=5% &
	\ FND7$=E$ &
	\ FNEND
14310	  DEF FNN3$(Z) &
	\ FNN3$=CVT%$(INT(Z))+CHR$((Z-INT(Z))*100.+.51) &
	\ FNEND
32767	  END
