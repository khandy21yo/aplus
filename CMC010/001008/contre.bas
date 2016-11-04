5	  EXTEND
10	  !*ISAM3S--CONE81-WITH 1981 FICA RATES, FROM CONERN &
	  !  ERNDED - CONSTRUCTION EARNINGS AND DEDUCTIONS &
	  !  07/6/79 - BILL LOVEGROVE FOR GENERAL SYSTEM &
	  !  01/?/81 - MODIFIED BY MIKE FEINAUER FOR OVARD COLLINS &
	  !	     (FOR TRANSFERING TAXES TO CHECK DATA FILE) &
	  ! &

50	  DIM M1$(24%),M$(24%),M2$(44%),M(21%),P1$(29%),P$(37%),P(17%),P%(18%), &
		  D1$(25%),D$(11%),D(14%),D%(1%),X(14%),S(14%),S1(14%),S1$(10%) &

60	  C1$="" &
	\ INPUT "PAYROLL DATE (MM.DD.YY) ";Z$ IF Z$="" &
	\ GOTO 10000 IF Z$="" &
	\ Z$=FND7$(Z$) &
	\ Z1$=LEFT(Z$,2%)+RIGHT(Z$,4%)+"T" &
	\ IF FND7%(Z$) &
	  THEN	  PRINT "BAD DATE !!! -- RE ENTER (TYPE <RETURN> TO END)" &
		\ Z$="" &
		\ GOTO 60 &

70	  GOTO 110 &

100	  ! &
	  !  OPEN FILES &
	  ! &

105	  V$=FNX$ &
	\ Z$=LEFT(V$,8%) &
	\ C1$=RIGHT(V$,9%) &
	\ Z1$=LEFT(Z$,2%)+RIGHT(Z$,4%)+"T" &
	  ! GET DATA FROM CORE COMMON IF PROGRAM REFERENCED BY 'CONTRN' &

110	  OPEN "NL:" AS FILE 1%, RECORDSIZE 512%+64%+128%+64% &

120	  IF FNO%(2%,"MSTRFL.DAT" ,"" ,"" ) &
	  THEN	  PRINT "MASTER FILE NOT FOUND !!!" &
		\ GOTO 10000 &

130	  IF FNO%(6%,"TX"+Z1$,"" ,"" ) &
	  THEN	  PRINT "EARNINGS AND DEDUCTIONS FILE DOES NOT EXIST !!!" &
		\ GOTO 10000 &

150	  OPEN "SS0:UNIQUE.FIL" FOR INPUT AS FILE 11% &
	\ DIM #11%,A0$(255%)=64% &
	\ A0%=INSTR(1%,A0$(15%),"TIPS" ) &

155	  C9$(1%)="WIP" &
	\ C9$(2%)="DED" &
	\ C9$(3%)="BNK" &
	\ C9$(4%)="FED" &
	\ C9$(5%)="FIC" &
	\ C9$(6%)="BUR" &
	\ C9$(7%)="VAC" &
	\ C9$(8%)="INS" &

160	  A$=CVT$$(A0$(22%)+A0$(27%)+A0$(28%),-1%) &
	\ FOR I%=1% TO 8% &
		\ A%=INSTR(1%,A$,C9$(I%)) &
		\ C9$(I%)=MID(A$,A%+4%,6%) IF A% &
	\ NEXT I% &
	\ A%=0% &
	\ S%=0% &
	\ D%=0% &

170	  A%=INSTR(A%+1%,A$,"S/" ) &
	\ IF A% &
	  THEN	  S%=S%+1% &
		\ S$(S%)=MID(A$,A%+2%,2%) &
		\ S0$(S%)=MID(A$,A%+5%,6%) &
		\ GOTO 170 &

180	  A%=INSTR(A%+1%,A$,"D/" ) &
	\ IF A% &
	  THEN	  D%=D%+1% &
		\ A1%=INSTR(A%+1%,A$,'=') &
		\ A1%=A%+4% IF A1%=0% OR A%+5%<A1% &
		\ D2$(D%)=MID(A$,A%+2%,A1%-(A%+2%)) &
		\ D3$(D%)=MID(A$,A1%+1%,6%) &
		\ GOTO 180 &

190	  C9$(I%)=C9$(I%)+".00" IF LEN(C9$(I%))=3% FOR I%=1% TO 8% &
	\ A%=INSTR(1%,A0$(15%),"LEN=" ) &
	\ L%=0% &
	\ L%=VAL(MID(A0$(15%),A%+4%,1%)) IF A% &

200	  ! &
	  !  FIELD STATEMENTS &
	  ! &

220	  FIELD #1%, 6% AS M1$(1%), 30% AS M1$(2%), 30% AS M1$(3%), &
		  30% AS M1$(4%), 30% AS M1$(5%), 11% AS M1$(6%), 8% AS M1$(7%), &
		  1% AS M1$(8%), 2% AS M1$(9%), 12% AS M1$(10%), 2% AS M1$(11%) &

221	  FIELD #1%,152%+I%*10% AS E$,8% AS M2$(I%), 2% AS M1$(I%+ &
		  11%) FOR I%=1% TO 9% &
	\ FIELD #1%,252% AS E$,8% AS M1$(21%),8% AS M1$(22%) &
	\ FIELD #1%,188%+I%*8% AS E$,8% AS M2$(I%) FOR I%=10% TO 36% &
	\ FIELD #1%,484% AS E$,2% AS M1$(23%) &
	\ FIELD #1%,486%+I%*2% AS E$,2% AS M2$(I%+37%) FOR I%=0% TO 7% &
	  ! MSTRFL.DAT &

240	  FIELD #1%, 512%+64% AS E$, 6% AS D1$(1%),1% AS D1$(2%),2% AS D1$(3%), &
		  2% AS D1$(4%),2% AS D1$(5%), 2% AS D1$(6%),2% AS D1$(7%), &
		  2% AS D1$(8%), 2% AS D1$(9%), 8% AS D1$(10%), 2% AS D1$(11%), &
		  8% AS D1$(12%),8% AS D1$(13%),8% AS D1$(14%),8% AS D1$(15%), &
		  8% AS D1$(16%),8% AS D1$(17%),8% AS D1$(18%),8% AS D1$(19%), &
		  8% AS D1$(20%),8% AS D1$(21%),8% AS D1$(22%),3% AS D1$(23%), &
		  3% AS D1$(24%),1% AS D1$(25%) &
	  ! ERNDED.DAT &

250	  FIELD #1%,512%+64%+128% AS E$, 6% AS P$, 8% AS P1$, 2% AS P2$, &
		  8% AS P3$, 2% AS P4$, 2% AS P5$, 2% AS P6$, 28% AS P7$, 6% AS P8$ &
	  ! CKDATA &

290	  FIELD #1%,512% AS T2$,64% AS T4$, 128% AS T6$, 64% AS T8$ &

1000	  ! &
	  !  CONTROL &
	  ! &

1010	  GOTO 7012 UNLESS C1$="" &

1020	  PRINT &
	\ INPUT "OPTION ";K$ &
	\ K$=LEFT(K$,3%) &
	\ GOTO 7000 IF K$="TRA" &
	\ GOTO 10000 IF K$="END" &
	\ PRINT &

1030	  PRINT &
	\ PRINT "OPTIONS :" &
	\ PRINT "	'TRA'NSFER FILE CONTENTS TO CKDATA FILE" &
	\ PRINT "	'END' PROGRAM " &
	\ GOTO 1020 &

2300	  ! &
	  !  SEPARATE MASTER &
	  ! &

2310	  LSET T2$=FNL$ &
	\ M$(I%)=M1$(I%)+"" FOR I%=1% TO 23% &
	\ M(I%)=CVT$F(M2$(I%)) FOR I%=1% TO 21% &
	\ RETURN &

2400	  ! &
	  !  SEPARATE PAYROLL &
	  ! &

2410	  LSET T4$=FNL$ &
	\ P$(I%)=P1$(I%)+"" FOR I%=1% TO 8% &
	\ P%(I%)=ASCII(P1$(I%+9%)) FOR I%=1% TO 15% &
	\ P(1%)=CVT$F(P1$(25%)) &
	\ P(I%)=FNN3(P1$(I%+24%)) FOR I%=2% TO 5% &
	\ P%(0%)=CVT$%(P1$(9%)) &
	\ P$(9%)=FND9$(P%(0%)) &
	\ IF P%(7%)=255% &
	  THEN	  P(6%)=FNN3(P1$(13%)+P1$(14%)+P1$(15%)) &
		\ P(7%)=FNN3(P1$(20%)+P1$(21%)+P1$(22%)) &
		\ P%(I%)=0% FOR I%=1% TO 14% &
		\ RETURN &

2420	  P(6%),P(7%)=0. &
	\ P(6%)=P(6%)+P%(I%) FOR I%=1% TO 7% &
	\ P(7%)=P(7%)+P%(I%) FOR I%=8% TO 14% &
	\ P(I%)=P(I%)/10. FOR I%=6% TO 7% &
	\ RETURN &

2500	  ! &
	  !  SEPARATE ERNDED &
	  ! &

2510	  LSET T6$=FNL$ &
	\ D$(I%)=D1$(I%)+"" FOR I%=1% TO 10% &
	\ D$(11%)=FND9$(CVT$%(D1$(11%))) &
	\ D(I%)=CVT$F(D1$(I%+11%)) FOR I%=1% TO 11% &
	\ D(I%+11%)=FNN3(D1$(I%+22%)) FOR I%=1% TO 2% &
	\ D%(1%)=ASCII(D1$(25%)) &
	\ D(14%)=D(1%)-D(12%) &
	\ D(14%)=D(14%)+D(2%) UNLESS A0% &
	\ D(14%)=D(14%)-D(I%) FOR I%=3% TO 11% &
	\ RETURN &

2600	  ! &
	  !  PREPARE ERNDED &
	  ! &

2610	  LSET D1$(I%)=D$(I%) FOR I%=1% TO 10% &
	\ LSET D1$(11%)=CVT%$(FND9%(D$(11%))) &
	\ LSET D1$(I%+11%)=CVTF$(D(I%)) FOR I%=1% TO 11% &
	\ LSET D1$(I%+22%)=FNN3$(D(I%+11%)) FOR I%=1% TO 2% &
	\ LSET D1$(25%)=CHR$(D%(1%)) &
	\ RETURN &

4000	  ! &
	  !  PRINT RECORD &
	  ! &

4010	  T=D(14%) &
	\ F$="" &
	\ F$="*" IF D%(1%)=1% &
	\ F$="#" IF D%(1%)=2% &
	\ F$="!" IF D%(1%)=3% &
	\ X1%=X1%+3% &
	\ M$(23%)="00" IF M$(23%)="" &
	\ FOR I%=1% TO 10% &
		\ IF S1$(I%)<>"" &
		  THEN	  GOTO 4040 IF M$(23%)=S1$(I%) &
		\ NEXT I% &
		\ STOP &

4030	  S1$(I%)=M$(23%) &

4040	  S(I%)=S(I%)+D(5%) &
	\ S1(I%)=S1(I%)+D(1%) &
	\ RSET P$=CVT$$(D$(10%),2%) &
	\ LSET P1$=" "+C9$(3%) &
	\ LSET P2$="PR" &
	\ LSET P3$=CVTF$(-T) &
	\ LSET P4$=CVT%$(VAL(LEFT(Z$,2%))) &
	\ LSET P5$=CVT%$(VAL(MID(Z$,4%,2%))) &
	\ LSET P6$=CVT%$(VAL(RIGHT(Z$,7%))) &
	\ LSET P7$=M$(1%)+M$(2%) &
	\ LSET P8$="" &
	\ STOP IF FNA%(8%,T8$) &

4050	  PRINT IF POS(0%)>75% &
	\ PRINT "!"; &
	\ RETURN &

7000	  ! &
	  !  PRINT &
	  ! &

7010	  INPUT "CHECK DATA TO TRANSFER TO ( JAN, FEB, ETC. )";C1$ &
	\ UNLESS INSTR(1%,"!JAN!FEB!MAR!APR!MAY!JUN!JUL!AUG!SEP"+ &
		  "!OCT!NOV!DEC" ,C1$)>0% AND LEN(C1$)=3% &
	  THEN	  PRINT "Invalid month !!"+STRING$(3%,7%) &
		\ GOTO 7010 &

7012	  V%=FNO%(8%,"CK"+C1$+".DAT" ,"" ,"" ) &
	\ V%=FNO%(8%,"CK"+C1$+".DAT" ,"/CR:16,64" ,"" ) IF V%=5% &

7020	  GOTO 1020 IF FNG%(6%,"" ) &
	\ GOSUB 2500 &
	\ IF [D%(1%) AND 2%] &
	  THEN	  PRINT "THIS PAYROLL HAS BEEN TRANSFERED !!(7025)"+STRING$(3%, &
			  7%) &
		\ GOTO 1020 &

7025	  E%=0% &
	\ X1%=66% &
	\ S(I%),S1(I%),X(I%)=0. FOR I%=1% TO 14% &
	\ S1$(I%)="" FOR I%=1% TO 10% &
	\ A1$=" 100.01 " &
	  ! ACCOUNT NUMBER &

7030	  GOSUB 2500 &
	\ V%=FNG%(2%,D$(1%)) &
	\ GOSUB 2300 &
	\ GOSUB 4000 &
	\ E%=E%+1% &
	\ FOR I%=1% TO 14% &
		\ GOTO 7045 IF D(I%)=0. &
		\ GOTO 7040 IF I%<6% OR I%>11% &
		\ D$(I%-2%)="00" IF D$(I%-2%)="" &
		\ FOR K%=1% TO D% &
			\ IF D2$(K%)=D$(I%-2%) &
			  THEN	  D1(K%)=D(I%)+D1(K%) &
				\ GOTO 7045 &

7035		  NEXT K% &
		\ GOTO 7040 IF I%<8% &
		\ D%=D%+1% &
		\ D2$(D%)=D$(I%-2%) &
		\ D3$(D%)=LEFT(C9$(2%),4%)+D2$(D%) &
		\ D1(D%)=D1(D%)+D(I%) &
		\ GOTO 7045 &

7040		  X(I%)=X(I%)+D(I%) &

7045	  NEXT I% &

7050	  IF FNN%(6%) &
	  THEN	  7100 &
	  ELSE	  7030 &

7100	  ! &
	  !  PRINT TOTALS &
	  ! &

7110	  PRINT &
	\ B$="\"+SPACE$(14%)+"\  #,###,###.##" &
	\ T=X(1%)-X(12%) &
	\ T=T+X(2%) UNLESS A0% &
	\ T=T-X(I%) FOR I%=3% TO 11% &
	\ T=T-D1(I%) FOR I%=1% TO D% &
	\ PRINT &
	\ PRINT USING "TOTAL EMPLOYEES = ####" ,E% &
	\ PRINT "REGISTER TOTALS :" &
	\ PRINT USING B$,"TAXABLE EARNINGS" ,X(1%) &
	\ PRINT USING B$,"UNTAXED EARNINGS" ,X(2%) &
	\ PRINT USING B$,"VACTION FUND" ,X(6%) &
	\ PRINT USING B$,"INSURANCE FUND" ,X(7%) &
	\ FOR I%=1% TO D% &
		\ PRINT USING B$,"DED "+D2$(I%)+" "+D3$(I%),D1(I%) &
	\ NEXT I% &

7120	  PRINT USING B$,"OTHER HAND DED" ,X(12%) &
	\ PRINT USING B$,"OTHER EARNINGS" ,X(13%) &
	\ PRINT USING B$,"FEDERAL TAX" ,X(3%) &
	\ PRINT USING B$,"FICA" ,X(4%) &
	\ PRINT USING B$,"STATE TAX" ,X(5%) &
	\ PRINT &
	\ PRINT USING B$,"NET PAYROLL" ,T &
	\ PRINT USING B$,"FED TAX DEPOSIT" ,X(3%)+X(4%)*2. &
	\ PRINT &
	\ PRINT &
	\ T1,T=0. &
	\ FOR I%=1% TO 10% &
		\ IF S1$(I%)<>"" &
		  THEN	  T=T+S(I%) &
			\ T1=T1+S1(I%) &
			\ PRINT USING B$,"TAX BY STATE "+S1$(I%),S(I%); &
			\ PRINT USING B$," EARNINGS FOR "+S1$(I%),S1(I%) &
			\ F1$=" STA.00" &
			\ F1$=" "+S0$(K%) IF S$(K%)=S1$(I%) FOR K%=1% TO S% &
			\ F2$="STATE TAX" &
			\ A=-S(I%) &
			\ GOSUB 7150 &
		\ NEXT I% &
		\ STOP &

7130	  F1$=" "+C9$(4%) &
	\ F2$="FEDERAL TAX" &
	\ A=-X(3%) &
	\ GOSUB 7150 &
	  ! FED TAX &

7132	  F1$=" "+C9$(5%) &
	\ F2$="FICA TAX" &
	\ A=-X(4%) &
	\ GOSUB 7150 &
	  ! FICA TAX &

7134	  F1$=" "+C9$(7%) &
	\ F2$="VAC DED." &
	\ A=-X(6%) &
	\ GOSUB 7150 &
	  ! VAC DED. &

7136	  F1$=" "+C9$(8%) &
	\ F2$="INS DED." &
	\ A=-X(7%) &
	\ GOSUB 7150 &
	  ! INS DED. &

7138	  FOR K%=1% TO D% &
		\ F1$=" "+D3$(K%) &
		\ F2$="DED. "+D2$(K%) &
		\ A=-D1(K%) &
		\ GOSUB 7150 &
	\ NEXT K% &

7140	  STOP IF FNG%(6%,"" ) &

7142	  GOSUB 2500 &
	\ LSET D1$(25%)=CHR$(D%(1%) OR 2%) &
	\ STOP IF FNU%(6%,T6$) &
	\ GOTO 7142 UNLESS FNN%(6%) &

7145	  PRINT USING B$,"TOTAL STATE TAX" ,T; &
	\ PRINT USING B$," TOTAL EARNINGS " ,T1 &
	\ PRINT &
	\ GOTO 10000 &

7150	  ! &
	  ! PUT TAX RECORDS IN FILE &
	  ! &

7152	  RETURN IF A==0. &
	\ RSET P$="8" &
	\ LSET P1$=F1$ &
	\ LSET P2$="PR" &
	\ LSET P3$=CVTF$(A) &
	\ LSET P4$=CVT%$(VAL(LEFT(Z$,2%))) &
	\ LSET P5$=CVT%$(VAL(MID(Z$,4%,2%))) &
	\ LSET P6$=CVT%$(VAL(RIGHT(Z$,7%))) &
	\ LSET P7$=MID("VACATION FUND INSURANCE FUNDOTHER #1 FUND "+ &
		  "OTHER #2 FUND OTHER #3 FUND OTHER #4 FUND" ,(Y%-1%)*14%+1%, &
		  14%) IF Y% &
	\ LSET P7$=F2$ UNLESS Y% &
	\ LSET P8$="" &
	\ STOP IF FNA%(8%,T8$) &
	\ RETURN &

7900	  ! &
	  !  PRINT HEADER &
	  ! &

7910	  PRINT FOR I%=X1%+1% TO 66% &
	\ PRINT &
	\ PRINT "PAYROLL DATE: ";Z$ &
	\ PRINT "EMP #  NAME          M #    TAXED  ";H2$;"   OTHER"; &
		  "     FED   FICA   STATE  NETCK" &
	\ PRINT &
	\ X1%=4% &
	\ RETURN &

10000	  ! &
	  !  END &
	  ! &

10010	  STOP IF FNX%("" ,0%,"" ) &

14000	  ! &
	  ! STORE AND RETRIEVE DATE IN INTEGER (D9% <=> MM.DD.YY) &
	  ! &

14010	  DEF FND9%(E$) &
		\ E$="0"+E$ IF INSTR(1%,E$,"." )=2% &
		\ E$=LEFT(E$,3%)+"0"+RIGHT(E$,4%) IF INSTR(4%,E$,"." )=5% &
		\ FND9%=VAL(LEFT(E$,2%))+VAL(MID(E$,4%,2%))*16%+ &
			  FND8%(VAL(RIGHT(E$,7%)))*512% &
	\ FNEND &

14020	  DEF FND9$(V%) &
		\ FND9$=RIGHT(NUM1$((V% AND 15%)+100%),2%)+"."+ &
			  RIGHT(NUM1$((V% AND 31%*16%)/16%+100%),2%)+"."+ &
			  RIGHT(NUM1$(((SWAP%(V%) AND 254%)/2%)+100%),2%) &
	\ FNEND &

14030	  DEF FND8%(D8)=D8 &

14040	  DEF FNZ(X)=INT(X*100.+.51)/100. &

14200	  ! &
	  ! CHECK FOR VALID DATE &
	  ! &

14210	  DEF FND7%(E$) &
		\ ON ERROR GOTO 14220 &
		\ GOTO 14220 IF INSTR(1%,E$,"." )<>3% OR INSTR(4%,E$, &
			  "." )<>6% OR INSTR(7%,E$,"." )<>0% OR LEN(E$)<>8% &
		\ V%=VAL(LEFT(E$,2%)) &
		\ GOTO 14220 IF V%<1% OR V%>12% &
		\ V%=VAL(MID(E$,4%,2%)) &
		\ GOTO 14220 IF V%<1% OR V%>31% &
		\ V%=VAL(RIGHT(E$,7%)) &
		\ GOTO 14220 IF V%<0% &
		\ FND7%=0% &
		\ GOTO 14230 &

14220		  FND7%=-1% &
		\ RESUME 14230 &

14230		  ON ERROR GOTO 0 &
	\ FNEND &

14250	  ! &
	  ! FORMAT DATE TO MM.DD.YY , FILL WITH ZEROS &
	  ! &

14260	  DEF FND7$(E$) &
		\ E$=E$+"."+RIGHT(DATE$(0%),8%) IF LEN(E$)<6% &
		\ E$="0"+E$ IF INSTR(1%,E$,"." )=2% &
		\ E$=LEFT(E$,3%)+"0"+RIGHT(E$,4%) IF INSTR(4%,E$,"." )=5% &
		\ FND7$=E$ &
	\ FNEND &

14300	  ! &
	  ! FLOATING POINT NUMBER TO THREE CHARACTERS AND BACK &
	  ! &

14310	  DEF FNN3$(X)=CVT%$(INT(X))+CHR$((X-INT(X))*100.+.51) &

14350	  DEF FNN3(E$)=CVT$%(LEFT(E$,2%))+ASCII(MID(E$,3%,1%))/100. &

32767	  END &

