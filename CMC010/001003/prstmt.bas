10	  ! &
	  ! Program name: prstmt		Compiled with SCALE 0 on V08.0 &
	  ! Decompiled on 24-Nov-16 at 02:11 AM
50	  DIM U$(20%), C$(9%), C(3%), C1(8%), L1$(10%), L$(7%), L(3%)
100	  !
110	  OPEN "KB:" AS FILE 1%, RECORDSIZE 256%+64%+64%
120	  IF FNO%(2%,"CUSTOM.DAT","U","R") THEN  &
		  PRINT 'CUSTOMER FILE NONEXISTENT. USE "CUSTOM" TO CREATE.' &
		\ GOTO 10000
140	  IF FNO%(6%,"RECEIV.DAT","","") THEN  &
		  PRINT "RECEIV.DAT NONEXISTENT. USE THE RECEIV PROGRAM TO CREATE." &
		\ GOTO 10000
150	  ON ERROR GOTO 170
160	  OPEN "SS0:UNIQUE.FIL" FOR INPUT AS FILE 12% &
	\ DIM #12%, A0$(255%)=64% &
	\ GOTO 180
170	  IF ERR=5% THEN  &
		  PRINT "YOU MUST CREATE A FILE CONTAINING YOUR SPECIFIC COMPANY'S INFORMATION" &
		\ PRINT "BEFORE THESE PROGRAMS CAN BE USED. USE THE PROGRAM '!UNIQUE'" &
		\ PRINT "TO DO THIS." &
		\ RESUME 10000
180	  ON ERROR GOTO 0 &
	\ IF MID(A0$(24%),16%,1%)<>"=" THEN  &
		  PRINT "CHECK FOR BAD ACCOUNT NUMBERS IN UNIQUE."
182	  Z0$=MID(A0$(10%),2%,1%) &
	\ B%=-1% IF INSTR(1%,A0$(10%),"*")<>0% &
	\ IF Z0$<>"B" AND Z0$<>"O" THEN  &
		  PRINT "PLEASE USE !UNIQUE TO ENTER NECESSARY INFORMATION ON LINE 10." &
		\ GOTO 10000
185	  !
190	  IF FNO%(8%,"RECHLD.DAT","","")<>0% THEN  &
		  PRINT "ERROR";FNS%;"IN OPENING RECHLD.DAT." &
		\ GOTO 10000
200	  !
210	  FIELD #1%, 6% AS U$(1%),30% AS U$(2%),30% AS U$(3%),30% AS U$(4%),30% AS U$(5%),8% AS U$(6%),8% AS U$(7%),12% AS U$(8%) &
	\ FIELD #1%, 154%+I%*8% AS E$,8% AS U$(I%+9%) FOR I%=0% TO 10% &
	\ FIELD #1%, 242% AS E$,2% AS U$(20%) &
	\ FIELD #1%, 256% AS T3$
220	  FIELD #1%, 256% AS E$,6% AS L1$(1%),8% AS L1$(2%),2% AS L1$(3%),8% AS L1$(4%),8% AS L1$(5%),6% AS L1$(6%),8% AS L1$(7%),8% AS L1$(8%),8% AS L1$(9%),1% AS L1$(10%) &
	\ FIELD #1%, 256% AS E$,64% AS T4$
300	  !
1000	  !
1020	  PRINT  &
	\ INPUT "Option ";K$ &
	\ K$=LEFT(CVT$$(K$,-1%),3%) &
	\ GOTO 3000 IF K$="UPD" &
	\ GOTO 4000 IF K$="STA" &
	\ GOTO 10000 IF K$="END" &
	\ GOTO 5000 IF K$="AGE"
1030	  PRINT  &
	\ PRINT "Options : STATEMENTS printed" &
	\ PRINT "          AGEING reports printed" &
	\ PRINT "          UPDATE ageing" &
	\ PRINT "          END program" &
	\ GOTO 1020
2100	  !
2110	  LSET U$(I%)=C$(I%) FOR I%=1% TO 8% &
	\ LSET U$(I%+8%)=CVTF$(C(I%)) FOR I%=1% TO 3% &
	\ LSET U$(I%+11%)=CVTF$(C1(I%)) FOR I%=1% TO 8% &
	\ LSET U$(20%)=CVT%$(FND6%(C$(9%))) &
	\ RETURN
2300	  !
2310	  LSET T3$=FNL$ &
	\ C$(I%)=U$(I%)+"" FOR I%=1% TO 8% &
	\ C(I%)=CVT$F(U$(I%+8%)) FOR I%=1% TO 3% &
	\ C1(I%)=CVT$F(U$(I%+11%)) FOR I%=1% TO 8% &
	\ C$(9%)=FND6$(CVT$%(U$(20%))) &
	\ RETURN
2400	  !
2410	  LSET L1$(I%)=L$(I%) FOR I%=1% TO 6% &
	\ LSET L1$(I%+6%)=CVTF$(L(I%)) FOR I%=1% TO 3% &
	\ LSET L1$(10%)=L$(7%)+"" &
	\ RETURN
2450	  !
2460	  LSET T4$=FNL$ &
	\ L$(I%)=L1$(I%)+"" FOR I%=1% TO 6% &
	\ L(I%)=CVT$F(L1$(I%+6%)) FOR I%=1% TO 3% &
	\ L$(7%)=L1$(10%)+"" &
	\ RETURN
2600	  !
2610	  PRINT USING " \      \  \              \ ", D1$,D$; &
	\ IF A>=0. THEN &
		  PRINT USING "##,###.##          ", A; &
	  ELSE &
		  PRINT USING "          ##,###.##", -A;
2620	  T9=T9+A &
	\ C2(1%)=C2(1%)+A IF A>0. &
	\ C2(2%)=C2(2%)+ABS(A) IF A<0. &
	\ PRINT USING "#,###,###.##", T9 &
	\ X1%=X1%+1% &
	\ T1%=T1%+1% &
	\ RETURN
3000	  !
3002	  GOTO 3005 IF Z0$="O" &
	\ INPUT "MONTH FOR CKDATA ENTRIES ";M$ &
	\ M$=LEFT(M$,3%) &
	\ GOTO 10000 IF M$="" &
	\ IF FNO%(10%,"CK"+M$+".DAT","","")<>0% THEN  &
		  PRINT "ERROR";FNS%;"- PLEASE RE-ENTER." &
		\ GOTO 185
3005	  S1=0. &
	\ PRINT "NOTE: STATEMENTS MUST BE PRINTED BEFOREBEFORE AN UPDATE IS DONE."
3007	  GOSUB 3800 &
	\ INPUT "Verify the update (Y or N) ";K$ &
	\ GOTO 1020 IF LEFT(K$,1%)<>"Y"
3010	  O%=-1% &
	\ GOTO 10000 IF FNG%(2%,"")
3020	  GOSUB 8500 &
	\ GOSUB 3070 IF Z0$="O" &
	\ GOSUB 3080 IF Z0$="B" &
	\ GOSUB 2100 &
	\ STOP IF FNU%(2%,T3$) &
	\ GOTO 3020 UNLESS FNN%(2%)
3030	  GOSUB 3100 &
	\ GOSUB 8700 IF Z0$="B" &
	\ O%=0% &
	\ PRINT "UPDATE COMPLETE." &
	\ GOTO 10000
3070	  RETURN IF O%=0% OR S=0% &
	\ L$(1%)=C$(1%) &
	\ L$(2%)="SRVCHG" &
	\ L$(3%)="IN" &
	\ L$(4%)=MID(A0$(24%),17%,6%)+" " &
	\ L$(5%)=D9$ &
	\ L$(6%)="" &
	\ L(1%)=S &
	\ L(2%),L(3%)=0. &
	\ L$(7%)="N" &
	\ S1=S1+S &
	\ RETURN IF B%=-1% &
	\ GOSUB 2400 &
	\ STOP IF FNA%(8%,T4$) &
	\ RETURN
3080	  RETURN IF O%=0% OR S=0. &
	\ S1=S1+S &
	\ RETURN IF B%=-1% &
	\ C$(6%)=CVT$$(C$(6%),2%)+".00 " IF LEN(CVT$$(C$(6%),2%))=3% &
	\ STOP IF FNA%(10%,"     1 "+LEFT(C$(6%),7%)+"AR"+CVTF$(S)+CVT%$(VAL(LEFT(D9$,2%)))+CVT%$(VAL(MID(D9$,4%,2%)))+CVT%$(VAL(RIGHT(D9$,7%)))+"SERVICE CHARGE - "+C$(1%)+"") &
	\ RETURN
3100	  !
3105	  RETURN IF S1=0. &
	\ IF Z0$="O" THEN  &
		  PRINT "SERVICE CHARGES OF "; &
		\ PRINT USING "$$"+STRING$(INT(LOG10(ABS(S1)))+1.,ASCII("#"))+".##-", S1; &
		\ PRINT " ADDED TO THE HOLDING FILE." &
		\ RETURN
3110	  RETURN IF B%=-1% &
	\ STOP IF FNA%(10%,"     1"+" "+MID(A0$(24%),17%,6%)+" "+"AR"+CVTF$(-S1)+CVT%$(VAL(LEFT(D9$,2%)))+CVT%$(VAL(MID(D9$,4%,2%)))+CVT%$(VAL(RIGHT(D9$,7%)))+"SERVICE CHARGE                  ") &
	\ RETURN
3800	  INPUT "DATE (MM.DD.YY) ";D9$ &
	\ D9$="0"+D9$ IF INSTR(1%,D9$,".")=2% &
	\ D9$=LEFT(D9$,3%)+"0"+RIGHT(D9$,4%) IF INSTR(4%,D9$,".")=5% &
	\ GOTO 3800 IF FND7%(D9$) &
	\ D8$=CVT$$(MID("JANUARY   FEBRUARY  MARCH     APRIL     "+"MAY       JUNE      JULY      AUGUST    "+"SEPTEMBER OCTOBER   NOVEMBER  DECEMBER  ",VAL(LEFT(D9$,2%))*10%-9%,10%),128%)+" 19"+RIGHT(D9$,7%) &
	\ RETURN
4000	  !
4005	  PRINT  &
	\ S1%,T1%=0% &
	\ C2(Y%)=0. FOR Y%=1% TO 3% &
	\ GOSUB 3800
4010	  R%=0% &
	\ INPUT "PRINT INDIVIDUAL, ROUTE, OR ALL ";K$ &
	\ K$=LEFT(K$,1%) &
	\ K$="A" IF K$="" &
	\ GOTO 4300 IF K$="?" &
	\ GOTO 4010 IF K$<>"I" AND K$<>"A" AND K$<>"R" &
	\ GOTO 4100 IF K$="I" &
	\ GOTO 4060 IF K$="R"
4020	  INPUT "START AT (<RETURN> FOR BEGINNING)";K$ &
	\ K$=LEFT(K$+"      ",6%) IF K$<>"" &
	\ IF FNG%(2%,K$) THEN  &
		  PRINT "BAD CODE !!!" &
		\ GOTO 4020
4030	  GOSUB 8000
4040	  GOSUB 2300 &
	\ GOTO 4055 IF LEFT(C$(1%),LEN(R$))<>R$ AND R%=-1% &
	\ GOSUB 4200
4050	  GOTO 4040 UNLESS FNN%(2%)
4055	  V$=SYS(CHR$(2%)) &
	\ GOTO 4080
4060	  R%=-1%
4070	  INPUT "ROUTE CODE ";R$ &
	\ V%=FNG%(2%,R$) &
	\ GOSUB 2300 &
	\ GOTO 4070 IF LEFT(C$(1%),LEN(R$))<>R$ &
	\ GOTO 4030
4080	  PRINT  &
	\ PRINT  &
	\ INPUT "SET PAGE & HIT RETURN";K$ &
	\ PRINT  &
	\ PRINT  &
	\ PRINT "TOTAL # OF STATEMENTS   ===>"; &
	\ PRINT USING "  #,###", S1% &
	\ PRINT "TOTAL # OF TRANSACTIONS ===>"; &
	\ PRINT USING "  #,###", T1% &
	\ C2(1%)=C2(1%)-C2(3%) &
	\ FOR X%=1% TO 3% &
		\ PRINT "TOTAL AMOUNT OF "; &
		\ PRINT MID("CHARGES        CREDITS        SERVICE CHARGES",(X%-1%)*15%+1%,15%); &
		\ PRINT USING " ##,###,###.##", C2(X%) &
	\ NEXT X% &
	\ GOTO 1020
4100	  !
4110	  PRINT "AFTER SETTING PAGE, ENTER NUMBERS SEPARATED BY RETURNS"
4120	  GOSUB 8000
4130	  PRINT CHR$(7%); &
	\ INPUT #1%, K$ &
	\ IF K$="" THEN  &
		  V$=SYS(CHR$(2%)) &
		\ GOTO 1020
4140	  K$=LEFT(K$+"      ",6%) &
	\ IF FNG%(2%,K$) THEN  &
		  PRINT STRING$(7%,7%); &
		\ GOTO 4130
4150	  GOSUB 4200 &
	\ GOTO 4130
4200	  !
4210	  P1%=1% &
	\ GOSUB 8500 &
	\ RETURN IF T9+T+S+P=0. &
	\ S1%=S1%+1% &
	\ PRINT  FOR I%=1% TO 3% &
	\ PRINT "S T A T E M E N T ";TAB(28%);"PLEASE SEND PAYMENT TO:" &
	\ PRINT "- - - - - - - - - ";TAB(28%);A0$(1%) &
	\ PRINT "    - - - - -     ";TAB(28%);A0$(2%) &
	\ PRINT "        -         ";TAB(28%);A0$(3%) &
	\ PRINT "                  ";TAB(28%);A0$(4%) &
	\ PRINT TAB(28%);"TELEPHONE: ";A0$(5%) &
	\ PRINT  FOR I%=1% TO 3% &
	\ PRINT TAB(10%);"! ";C$(2%);"  ACCOUNT CODE: ";C$(1%) &
	\ PRINT TAB(10%);"! ";C$(3%);"  ";D8$ &
	\ PRINT TAB(10%);"! ";CVT$$(C$(I%),128%) FOR I%=4% TO 5%
4215	  PRINT  FOR I%=1% TO 5% &
	\ PRINT "     DATE     DESCRIPTION       CHARGES  PAYMENTS   BALANCE" &
	\ PRINT TAB(28%);"BALANCE FORWARD: "; UNLESS Z0$="O" &
	\ PRINT USING "###,###,###.##", T9 UNLESS Z0$="O" &
	\ PRINT  &
	\ IF Z0$="O" THEN  &
		  PRINT  &
		\ T9=0.
4220	  X1%=24% &
	\ GOTO 4250 IF FNG%(6%,C$(1%))
4230	  GOSUB 2450 &
	\ D$=L$(2%) &
	\ D1$=L$(5%) &
	\ A=L(1%)-L(2%)+L(3%) &
	\ GOSUB 2600
4235	  IF X1%>55% THEN  &
		  PRINT  FOR L%=X1% TO 65% &
		\ P1%=P1%+1% &
		\ PRINT "PAGE";P1% &
		\ PRINT  FOR L%=1% TO 3% &
		\ PRINT "     DATE     DESCRIPTION       CHARGES  PAYMENTS   BALANCE" &
		\ X1%=5%
4240	  IF FNN%(6%)=0% THEN  &
		  GOSUB 2450 &
		\ GOTO 4230 IF L$(1%)=C$(1%)
4250	  IF S<>0. THEN  &
		  D1$="" &
		\ D$="SERVICE CHARGE" &
		\ A=S &
		\ GOSUB 2600
4260	  PRINT  FOR X1%=X1% TO 55% &
	\ PRINT "    CURRENT      30 DAY      60 DAY      90 DAY  SRVC CHARGE" &
	\ PRINT USING " #######.## ########.## ########.## ########.## ########.##", C1(1%),C1(2%),C1(3%),C1(4%)+C1(5%),C1(7%) &
	\ PRINT  FOR X1%=1% TO 8% &
	\ RETURN
4300	  PRINT  &
	\ PRINT "ENTER 'A' TO PRINT A STATEMENT FOR ALL CUSTOMERS WITH INVOICES" &
	\ PRINT "ENTER 'R' TO PRINT THE STATEMENTS FOR ONE ROUTE" &
	\ PRINT "ENTER 'I' TO PRINT INDIVIDUAL STATEMENTS" &
	\ GOTO 4010
5000	  !
5010	  GOSUB 3800 &
	\ K1$,K2$="" &
	\ INPUT "WIDE OR NARROW FORM (W OR N)";K1$ UNLESS K1$="W" OR K1$="N" &
	\ X2=1. &
	\ INPUT "LENGTH OF FORM (8.5 OR 11)";X2 UNLESS X2=8.5 OR X2=11. OR X2=0. &
	\ X2%=X2*6. &
	\ X2%=66% IF X2%=0% AND K1$="N" &
	\ X2%=51% IF X2%=0% AND K1$="W" &
	\ INPUT "PRINT CUSTOMERS WITH ZERO BALANCE (Y OR N)";K2$ UNTIL K2$="Y" OR K2$="N"
5015	  INPUT "ROUTE CODE (RETURN FOR ALL)",R$ &
	\ R$="" IF R$="ALL" &
	\ GOTO 1020 IF FNG%(2%,R$) &
	\ INPUT "SALES GL# OR CATEGORY (RETURN FOR ALL)",K3$ &
	\ K3$=CVT$$(K3$,2%) &
	\ PRINT "Set page. . ."; &
	\ GET #0% &
	\ X1%=X2%-3%
5020	  GOSUB 8500 &
	\ GOTO 5040 IF R$<>LEFT(C$(1%),LEN(R$)) &
	\ GOSUB 5150 &
	\ GOTO 5020 UNLESS FNN%(2%)
5040	  PRINT  &
	\ LSET C$(1%)="" &
	\ LSET C$(2%)="*** GRAND TOTALS ***" &
	\ LSET C$(8%)="" &
	\ C1(I%)=T(I%) FOR I%=1% TO 7% &
	\ R=R1 &
	\ GOSUB 5150 &
	\ T(I%)=0. FOR I%=1% TO 7% &
	\ R1=0. &
	\ GOTO 1020
5100	  !
5110	  PRINT  FOR X1%=X1% TO X2%-1% &
	\ L%=40% &
	\ L%=65% IF K1$="W" &
	\ PRINT SPACE$(L%-33%/2%)+"ACCOUNTS RECEIVALBE AGEING REPORT" &
	\ PRINT SPACE$(L%-LEN(D8$)/2%)+D8$ &
	\ PRINT DATE$(0%);" ";RIGHT(A0$(1%),2%); &
	\ PRINT "  (EXCLUDING RETAINAGE)"; IF K1$="N" AND Z0$="O" &
	\ PRINT  &
	\ PRINT  &
	\ PRINT "CUST # CUSTOMER NAME         PHONE #      CURRENT    ";"30 DAY    60 DAY   "; &
	\ PRINT "OVER 60" IF K1$="N" &
	\ PRINT " 90 DAY   OVER 90 RETAINAGE   UNP SVC   BALANCE" IF K1$="W" &
	\ PRINT  &
	\ X1%=6% &
	\ RETURN
5150	  !
5160	  RETURN IF K3$<>"" AND K3$<>CVT$$(C$(7%),2%) &
	\ RETURN IF C1(1%)+C1(2%)+C1(3%)+C1(4%)+C1(5%)+C1(7%)=0. AND K2$="N" &
	\ T(I%)=T(I%)+C1(I%) FOR I%=1% TO 7% &
	\ R1=R1+R &
	\ T=0. &
	\ T=T+C1(I%) UNLESS I%=6% FOR I%=1% TO 7% &
	\ T=T+R &
	\ GOSUB 5100 IF X1%>X2%-6% &
	\ X1%=X1%+1% &
	\ PRINT C$(1%);" ";LEFT(C$(2%),19%);" ";C$(8%); &
	\ PRINT USING "#######.##", C1(I%); FOR I%=1% TO 3% &
	\ IF K1$="N" THEN  &
		  PRINT USING "#######.##", C1(4%)+C1(5%) &
		\ RETURN
5170	  PRINT USING "#######.##", C1(I%); FOR I%=4% TO 5% &
	\ PRINT USING "#######.##", R; &
	\ PRINT USING "#######.##", C1(7%); &
	\ PRINT USING "#######.##", T &
	\ RETURN
8000	  !
8010	  PRINT "POSITION FORMS ('?' FOR INSTRUCTIONS). . ." &
	\ PRINT "#";CHR$(13%); &
	\ K$="" &
	\ V$=SYS(CHR$(3%)) &
	\ WHILE K$<>"P" &
		\ PRINT CHR$(13%);"#"; &
		\ INPUT #1%, K$ &
		\ GOSUB 8100 IF K$="?" &
	\ NEXT &
	\ RETURN
8100	  PRINT  &
	\ PRINT "EACH <RETURN> ENTERED WILL RE-PRINT THE '#'" &
	\ PRINT "POSITION THE PAPER SUCH THAT THE '#' PRINTS ON THE TOP LINE." &
	\ PRINT "ENTER A 'P' TO PRINT THE STATEMENTS" &
	\ PRINT  &
	\ PRINT "#";CHR$(13%); &
	\ RETURN
8500	  !
8505	  R=0. &
	\ GOTO 9000 IF Z0$="O"
8510	  GOSUB 2300 &
	\ T9=C1(1%)+C1(2%)+C1(3%)+C1(4%)+C1(5%)+C1(7%) &
	\ C1(5%)=C1(5%)+C1(4%) &
	\ C1(I%)=C1(I%-1%) FOR I%=4% TO 2% STEP -1% &
	\ C1(1%)=0. &
	\ GOSUB 8600 &
	\ C1(1%)=T &
	\ C1(8%)=C1(8%)+T
8520	  C1(7%)=C1(7%)+P &
	\ P=0. &
	\ IF C1(7%)<0. THEN  &
		  P=C1(7%) &
		\ C1(7%)=0.
8540	  FOR I%=5% TO 2% STEP -1% &
		\ C1(I%)=C1(I%)+P &
		\ P=0. &
		\ IF C1(I%)<0. THEN  &
			  P=C1(I%) &
			\ C1(I%)=0.
8550			  NEXT I% &
	\ C1(1%)=C1(1%)+P &
	\ IF C1(1%)<0. AND C1(2%)+C1(3%)+C1(4%)+C1(5%)+C1(7%)<>0. THEN  &
		  P=C1(1%) &
		\ C1(1%)=0. &
		\ GOTO 8520
8560	  S=INT((C1(2%)+C1(3%)+C1(4%)+C1(5%))*C(3%))/100. &
	\ S=0. IF S<-1.5882272288890329e-23 &
	\ S=0.5 IF S<0.5 AND S>0. &
	\ C1(6%)=C1(6%)+S &
	\ C1(7%)=C1(7%)+S &
	\ C2(3%)=C2(3%)+S &
	\ C1(I%)=INT(C1(I%)*100.+0.5)/100. FOR I%=1% TO 7%
8570	  RETURN
8600	  !
8610	  T,P=0. &
	\ RETURN IF FNG%(6%,C$(1%))
8620	  GOSUB 2450 &
	\ RETURN IF L$(1%)<>C$(1%) &
	\ IF L$(3%)="RE" OR L(1%)-L(2%)+L(3%)<0. THEN &
		  P=P+L(1%)-L(2%)+L(3%) &
	  ELSE &
		  T=T+L(1%)-L(2%)+L(3%)
8625	  IF L$(3%)="RE" AND CVT%$(FND6%(L$(5%)))>CVT%$(FND6%(C$(9%))) THEN  &
		  C$(9%)=L$(5%)
8630	  GOTO 8620 UNLESS FNN%(6%) &
	\ RETURN
8640	  !
8650	  RETURN IF O%=0% &
	\ T1=0. &
	\ S%=0% &
	\ R$="" &
	\ RETURN IF FNG%(6%,C$(1%))
8660	  GOSUB 2450 &
	\ GOTO 8670 IF L$(1%)<>C$(1%) OR L$(2%)<>R$ AND R$<>"" &
	\ R$=L$(2%) &
	\ T1=T1+L(1%)-L(2%)+L(3%) &
	\ GOTO 8660 UNLESS FNN%(6%)
8665	  S%=FNS%
8670	  IF INT(T1*100.+6.8213185310482798e-15)/100.<>0. THEN  &
		  T1=0. &
		\ R$=L$(2%) &
		\ RETURN IF S%<>0% OR L$(1%)<>C$(1%) &
		\ GOTO 8660
8680	  V%=FND%(6%,C$(1%)+R$) &
	\ GOTO 8680 UNLESS V% &
	\ RETURN IF S%<>0% OR L$(1%)<>C$(1%) &
	\ STOP IF FNG%(6%,L$(1%)+L$(2%)) &
	\ GOSUB 2450 &
	\ R$=L$(2%) &
	\ GOTO 8660 UNLESS L$(1%)<>C$(1%) &
	\ RETURN
8700	  !
8710	  STOP IF FNC%(6%) &
	\ STOP IF FNO%(6%,"RECEIV.DAT","/CR:16,64/RW","") &
	\ RETURN
9000	  !
9010	  GOSUB 2300 &
	\ C1(I%)=0. UNLESS I%=6% FOR I%=1% TO 7%
9020	  GOTO 9100 IF FNG%(6%,C$(1%)) &
	\ R1$=""
9030	  GOSUB 2450 &
	\ GOTO 9100 IF L$(1%)<>C$(1%) &
	\ IF L$(2%)<>R1$ THEN  &
		  D1$=L$(5%) &
		\ R1$=L$(2%)
9040	  V%=FND2(D9$)-FND2(D1$) &
	\ V%=0% IF V%<0% &
	\ V%=120% IF V%>120% &
	\ IF INSTR(1%,L$(2%),"*") THEN &
		  R=R+L(1%)-L(2%)+L(3%) &
	  ELSE &
		  C1(V%/30%+1%)=C1(V%/30%+1%)+L(1%)-L(2%)+L(3%) UNLESS L$(2%)="SRVCHG" &
		\ C1(7%)=C1(7%)+L(1%)-L(2%)+L(3%) IF L$(2%)="SRVCHG"
9050	  IF L$(7%)="N" AND O%=-1% THEN  &
		  L$(7%)="U" &
		\ GOSUB 2400 &
		\ STOP IF FNU%(6%,T4$) &
		\ IF L$(3%)<>"RE" THEN  &
			  C1(8%)=C1(8%)+L(1%)-L(2%)+L(3%)
9055	  IF L$(3%)="RE" AND CVT%$(FND6%(L$(5%)))>CVT%$(FND6%(C$(9%))) THEN  &
		  C$(9%)=L$(5%)
9060	  GOTO 9030 UNLESS FNN%(6%)
9100	  T=C1(1%) &
	\ T9=C1(2%)+C1(3%)+C1(4%)+C1(5%) &
	\ S=0. &
	\ IF T+T9<=0. OR T9<=0. THEN &
		  GOTO 9110 &
	  ELSE &
		  S=INT(T9*C(3%)+6.8213185310482798e-15)/100. &
		\ C1(7%)=C1(7%)+S &
		\ C1(6%)=C1(6%)+S
9110	  GOSUB 8640 &
	\ RETURN
10000	  !
10010	  CLOSE 1% &
	\ CLOSE 12% &
	\ I%=FNC%(2%)+FNC%(6%)+FNC%(8%)+FNC%(10%) &
	\ I%=FNX%("",0%,"")
14030	  DEF FND8%(D8) &
	\ FND8%=D8 &
	\ FNEND
14035	  !
14040	  DEF FND6%(D9$) &
	\ FND6%=VAL(MID(D9$,4%,2%))+VAL(LEFT(D9$,2%))*32%+FND8%(VAL(RIGHT(D9$,7%)))*512% &
	\ FNEND
14050	  DEF FND6$(D9%) &
	\ FND6$=RIGHT(NUM1$((D9% AND 15%*32%)/32%+100%),2%)+"."+RIGHT(NUM1$((D9% AND 31%)+100%),2%)+"."+RIGHT(NUM1$((SWAP%(D9%) AND 254%)/2%+100%),2%) &
	\ FNEND
14200	  DEF FND7%(D7$) &
	\ ON ERROR GOTO 14290
14210	  GOTO 14290 IF MID(D7$,3%,1%)<>"." OR MID(D7$,6%,1%)<>"."
14220	  GOTO 14290 IF LEN(D7$)<>8%
14230	  D7%=VAL(LEFT(D7$,2%)) &
	\ GOTO 14290 IF D7%<1% OR D7%>12%
14240	  D7%=VAL(MID(D7$,4%,2%)) &
	\ GOTO 14290 IF D7%<1% OR D7%>31%
14250	  D7%=VAL(RIGHT(D7$,7%)) &
	\ GOTO 14290 IF D7%<0%
14260	  FND7%=0% &
	\ GOTO 14299
14290	  FND7%=-1% &
	\ RESUME 14299
14299	  ON ERROR GOTO 0 &
	\ FNEND
20100	  !
20110	  DEF FND2(X$) &
	\ P1%=INSTR(1%,X$,".") &
	\ P1%=INSTR(1%,X$,"/") UNLESS P1% &
	\ P2%=INSTR(P1%+1%,X$,".") &
	\ P2%=INSTR(P1%+1%,X$,"/") UNLESS P2%
20113	  Y6=VAL(RIGHT(X$,P2%+1%)) &
	\ M6=VAL(LEFT(X$,P1%-1%)) &
	\ D6=VAL(MID(X$,P1%+1%,P2%-P1%-1%)) &
	\ D6=D6+(Y6+0.)*0. &
	\ IF M6>2. THEN  &
		  D6=D6-INT(M6*-107376232.+1.0430812658057675e-08) &
		\ Y6=Y6+1.
20115	  FND2=M6*31.+INT((Y6+0.)/4.)+D6 &
	\ FNEND
32767	  END
