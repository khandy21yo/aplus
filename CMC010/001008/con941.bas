1	  ! Program name: [1,8]CON941		Compiled with SCALE 0 on V08.0 &
	  ! Decompiled on 21-Mar-85 at 08:48 PM by UNBAC Version 1
10	  !
12	  F2=53400. &
	\ FUI=7000. &
	\ FUI$=NUM1$(FUI)
15	  DIM M1$(23%), M2$(36%), M$(23%), M(36%), T(37%), S1(10%,37%), &
		S1$(10%)
100	  !
105	  OPEN "NL:" AS FILE 1%, RECORDSIZE 512%
120	  INPUT "YEAR FOR MASTER FILE ";F$ &
	\ F$="FL" IF F$="" &
	\ F$="MSTR"+F$+".DAT" &
	\ IF FNO%(2%,F$,"","") THEN &
		  PRINT "MASTER FILE NOT FOUND !!!" &
		\ GOTO 10000
130	  ON ERROR GOTO 135 &
	\ OPEN "SS0:UNIQUE.FIL" FOR INPUT AS FILE 12% &
	\ DIM #12%, A0$(255%)=64% &
	\ C$=A0$(1%) &
	\ GOTO 140
135	  PRINT "Please enter the name of the company "; &
	\ INPUT LINE C$ &
	\ C$=CVT$$(C$,4%) &
	\ RESUME 140
140	  ON ERROR GOTO 0 &
	\ CLOSE 12%
200	  FIELD #1%, 6% AS M1$(1%),30% AS M1$(2%),30% AS M1$(3%),30% AS M1$( &
		4%),30% AS M1$(5%),11% AS M1$(6%),8% AS M1$(7%),1% AS M1$(8%), &
		2% AS M1$(9%),12% AS M1$(10%),2% AS M1$(11%) &
	\ FIELD #1%, 152%+I%*10% AS E$,8% AS M2$(I%),2% AS M1$(I%+11%) FOR I% &
		=1% TO 9% &
	\ FIELD #1%, 252% AS E$,8% AS M1$(21%),8% AS M1$(22%) &
	\ FIELD #1%, 188%+I%*8% AS E$,8% AS M2$(I%) FOR I%=10% TO 36% &
	\ FIELD #1%, 484% AS E$,2% AS M1$(23%) &
	\ FIELD #1%, 512% AS T2$
1000	  !
1005	  GOTO 1020
1010	  PRINT "OPTIONS: TOTALS, YTD 941A"
1020	  GOTO 7000
2300	  !
2310	  LSET T2$=FNL$ &
	\ M$(I%)=M1$(I%)+"" FOR I%=1% TO 23% &
	\ M(I%)=CVT$F(M2$(I%)) FOR I%=1% TO 36% &
	\ RETURN
7000	  !
7005	  L$="" &
	\ INPUT "Page length ( 8.5 or 11 ) ";L$ UNTIL L$="8.5" OR L$="11" &
	\ L2%=VAL(L$)*6. &
	\ L%=L2%-3% &
	\ L1%=L2% &
	\ INPUT "QUARTER #?",F% &
	\ INPUT "DATE ENDING FOR HEADING",D1$ &
	\ INPUT "SET (WIDE!! 14-7/8 X 8-1/2) PAGE. EXPECT A PAUSE!",K$ &
	\ P%=0% &
	\ GOSUB 9820 &
	\ GOSUB 9710 &
	\ FOR I2%=1% TO 10% &
		\ GOTO 7010 IF S1$(I2%)="" &
		\ PRINT "ENTER UNEMPLOYMENT INSURANCE CUTOFF FOR ";S1$(I2%) &
		\ S1(I2%,I%)=0. FOR I%=1% TO 8% &
		\ E%(I%)=0% FOR I%=1% TO 10% &
		\ INPUT "-",U%(I2%) &
	\ NEXT I2% &
	\ STOP
7010	  INPUT "SET PAGE ",K$ &
	\ L%=L2%-4% &
	\ A$="\"+SPACE$(21%)+"\\         \" &
	\ B$="########.## ########.## ########.## ########.## ########.## " &
	\ B$=B$+B$ &
	\ FOR I2%=1% TO 10% &
		\ T(I%)=0. FOR I%=1% TO 15% &
		\ GOTO 7100 IF S1$(I2%)="" &
		\ GOSUB 7015 &
	\ NEXT I2% &
	\ STOP
7015	  GOSUB 7220 &
	\ V%=FNG%(2%,"") &
	\ STOP IF V%
7020	  GOSUB 2300 &
	\ GOTO 7050 IF M$(1%)="ZPAYRL" &
	\ M$(23%)="ID" IF M$(23%)="" &
	\ GOTO 7050 IF M$(23%)<>S1$(I2%) &
	\ Y1=0. &
	\ Y1=Y1+M(10%+I%) FOR I%=1% TO F% &
	\ Y=Y1-U%(I2%) &
	\ Y=M(10%+F%) IF Y1-M(10%+F%)>0. AND Y1-M(10%+F%)>U%(I2%) &
	\ Y=0. IF Y<0.
7025	  W=F2-(Y1-M(10%+F%)) &
	\ W=0. IF W<0. &
	\ IF Y1-F2<0. THEN &
		  W=M(10%+F%)
7027	  F=Y1-FUI &
	\ F=0. IF F<0. &
	\ F=M(10%+F%) IF Y1-M(10%+F%)>0. AND Y1-M(10%+F%)>FUI &
	\ F=0. IF F<0. &
	\ K$=M$(6%) &
	\ I%=INSTR(1%,K$,"-") &
	\ IF I%=0% AND MID(K$,4%,1%)<>" " THEN &
		  K$=LEFT(K$,3%)+"-"+MID(K$,4%,2%)+"-"+MID(K$,6%,4%)
7030	  PRINT USING A$, M$(2%),K$; &
	\ M(I%)=0. FOR I%=11%+F% TO 14% &
	\ PRINT USING B$, M(11%),M(12%),M(13%),M(14%),Y1,W,Y,F
7040	  T(I%)=T(I%)+INT(M(I%-1%)*100.+.5) FOR I%=12% TO F%+11% &
	\ T(11%)=T(11%)+INT(Y1*100.+.5) &
	\ T(1%)=T(1%)+INT(W*100.+.5) &
	\ T(2%)=T(2%)+INT(Y*100.+.5) &
	\ T(3%)=T(3%)+INT(F*100.+.5) &
	\ E%(I2%)=E%(I2%)+1% UNLESS M(10%+F%)=0. &
	\ GOSUB 7200
7050	  IF FNN%(2%) THEN &
		  GOTO 7060 &
	  ELSE &
		  GOTO 7020
7060	  PRINT &
	\ PRINT USING A$, "TOTALS"," "; &
	\ PRINT USING B$, T(12%)/100.,T(13%)/100.,T(14%)/100.,T(15%)/100.,T( &
		11%)/100.,T(1%)/100.,T(2%)/100.,T(3%)/100.
7070	  L%=L%+1% &
	\ S1(I2%,I%)=T(I%) FOR I%=11% TO 15% &
	\ S1(I2%,I%)=T(I%) FOR I%=1% TO 3% &
	\ RETURN
7100	  PRINT STRING$(L2%-L%,10%),"TOTALS BY STATE" &
	\ GOSUB 7230 &
	\ T(I%)=0. FOR I%=1% TO 15% &
	\ FOR I2%=1% TO 10% &
		\ GOTO 7110 IF S1$(I2%)="" &
		\ PRINT USING A$, "TOTALS FOR STATE OF ",S1$(I2%)+" EMP#"+ &
			NUM$(E%(I2%)); &
		\ PRINT USING B$, S1(I2%,12%)/100.,S1(I2%,13%)/100.,S1(I2%, &
			14%)/100.,S1(I2%,15%)/100.; &
		\ PRINT USING B$, S1(I2%,11%)/100.,S1(I2%,1%)/100.,S1(I2%,2%) &
			/100.,S1(I2%,3%)/100. &
		\ PRINT &
		\ T(I%)=T(I%)+S1(I2%,I%) FOR I%=1% TO 15% &
	\ NEXT I2%
7110	  PRINT USING A$, "GRAND TOTAL ",""; &
	\ PRINT USING B$, T(12%)/100.,T(13%)/100.,T(14%)/100.,T(15%)/100.,T( &
		11%)/100.,T(1%)/100.,T(2%)/100.,T(3%)/100. &
	\ PRINT &
	\ PRINT TAB(38%); &
	\ PRINT USING "Year-to-date compensation not includ"+ &
		"ed above =  ##,###,###.##", T(16%)/100. &
	\ GOTO 10000
7200	  !
7210	  IF L%<L2%-7% THEN &
		  L%=L%+1% &
		\ RETURN
7220	  PRINT STRING$(L2%-L%,10%) &
	\ PRINT "STATE CODE ";S1$(I2%)
7230	  PRINT "941A REPORT - ";CVT$$(C$,4%+128%);"  "; &
	\ P%=P%+1% &
	\ PRINT "QUARTER # ";F%;" ENDING ";D1$;TAB(100%);"PAGE# ";P%
7240	  PRINT TAB(35%);"FIRST  QTR  SECOND QTR   THIRD QTR  FOURTH QTR"; &
	\ PRINT "  YEAR TO DATE  THIS QTR    QTR EXCESS  QTR EXCESS"
7250	  PRINT TAB(36%);" EARNINGS    EARNINGS    EARNINGS    EARNINGS    "; &
		"EARNINGS    FICA WAGES  SUI ("; &
	\ PRINT USING "#####) FUI("+FUI$+")", U%(I2%)
7260	  L%=6% &
	\ RETURN
9700	  !
9705	  L%=63% &
	\ L1%=66% &
	\ INPUT "SET PAGE",K$ &
	\ GOSUB 9820 &
	\ GOSUB 9710 &
	\ GOTO 10000
9710	  T(I%)=0. FOR I%=1% TO 35% &
	\ V%=FNG%(2%,"") &
	\ S1(I1%,I%)=0. FOR I1%=1% TO 10% FOR I%=1% TO 15% &
	\ S1$(I1%)="" FOR I1%=1% TO 10%
9720	  GOSUB 2300 &
	\ GOTO 9721 IF M$(1%)="ZPAYRL" &
	\ T(I%)=T(I%)+INT(M(I%-1%)*100.+.51) FOR I%=11% TO 37% &
	\ M$(23%)="ID" IF M$(23%)="" &
	\ FOR I1%=1% TO 10% &
		\ GOTO 9722 IF S1$(I1%)="" &
		\ GOTO 9724 IF M$(23%)=S1$(I1%) &
	\ NEXT I1% &
	\ STOP
9721	  PRINT "LAST UPDATE WAS ";M$(2%) &
	\ GOTO 9725
9722	  S1$(I1%)=M$(23%)
9724	  S1(I1%,I%)=S1(I1%,I%)+INT(M(I%-1%)*100.+.51) FOR I%=11% TO 37%
9725	  IF FNN%(2%) THEN &
		  GOTO 9730 &
	  ELSE &
		  GOTO 9720
9730	  M$(I%)="" FOR I%=1% TO 23% &
	\ M(I%-1%)=T(I%)/100. FOR I%=1% TO 37% &
	\ K$="ALL STATES" &
	\ GOSUB 9735 &
	\ FOR I1%=1% TO 10% &
		\ RETURN IF S1$(I1%)="" &
		\ K$=S1$(I1%) &
		\ M(I%-1%)=S1(I1%,I%)/100. FOR I%=11% TO 37% &
		\ GOSUB 9735 &
	\ NEXT I1% &
	\ STOP
9735	  PRINT &
	\ PRINT "SUMMARY OF FILE DATA AS YEAR-TO-DATE TOTALS FOR ";K$ &
	\ PRINT SPACE$(10%);DATE$(0%)
9740	  PRINT &
	\ PRINT &
	\ PRINT "    EARNINGS     FED TAX        FICA       STATE"
9750	  A$="#,###,###.##  ###,###.##  ###,###.##  ###,###.##    " &
	\ B$="\          \ #,###,###.##" &
	\ PRINT USING A$, M(11%),M(17%),M(22%),M(27%); &
	\ PRINT USING B$, "VACATION",M(31%)
9760	  PRINT USING A$, M(12%),M(18%),M(23%),M(28%); &
	\ PRINT USING B$, "INSURANCE",M(32%)
9770	  PRINT USING A$, M(13%),M(19%),M(24%),M(29%); &
	\ PRINT USING B$, "TRAVEL",M(33%)
9780	  PRINT USING A$, M(14%),M(20%),M(25%),M(30%); &
	\ PRINT USING B$, "FRINGE",M(34%)
9790	  PRINT USING A$, M(15%); &
	\ PRINT SPACE$(38%); &
	\ PRINT USING B$, "OTHER DEDUCT",M(35%)
9800	  PRINT USING A$, M(10%),M(16%),M(21%),M(26%); &
	\ PRINT USING B$, "NET PAID",M(36%) &
	\ PRINT &
	\ GOSUB 9810 &
	\ RETURN
9810	  L%=L%+13% &
	\ RETURN IF L%<L1%-16%
9820	  PRINT STRING$(L1%-L%,10%); &
	\ L%=1% &
	\ RETURN
10000	  !
10010	  V%=FNC%(2%) &
	\ STOP IF FNX%("",0%,"")
32767	  END
