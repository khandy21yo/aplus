10	  ! &
	  ! Program name: [1,2]PRTWEK		Compiled with SCALE 0 on V07.0 &
	  ! Decompiled on 03-Feb-88 at 05:52 PM by UNBAC Version 1
1000	  Z%=0% &
	\ W%=1% &
	\ T%=2% &
	\ B%=3% &
	\ M%=4% &
	\ B%(T%),B%(B%)=-W% &
	\ ON ERROR GOTO 20000 &
	\ OPEN "$MONEY.ACT" FOR INPUT AS FILE T%, RECORDSIZE 8192. &
	\ OPEN "$MONEY.MST" FOR INPUT AS FILE B%, MODE W% &
	\ PRINT &
	\ GOSUB 4040 &
	\ PRINT "Output to"; &
	\ INPUT LINE S$
1010	  S$=CVT$$(S$,255%) &
	\ F%=INSTR(W%,S$,"/") &
	\ D$=RIGHT(S$,F%) &
	\ S$=LEFT(S$,F%-W%) IF F% &
	\ S$="KB:" IF S$="" &
	\ INPUT "Print final totals (Y/N)";O$ &
	\ O$=LEFT(CVT$$(O$,255%),W%) &
	\ IF N$="" OR N$="NONE" THEN &
		  F4%=O$="Y" &
	  ELSE &
		  F4%=(NOT O$="N")
1020	  O$=SYS(CHR$(6%)+CHR$(-10%)+S$) &
	\ IF F%=Z% AND (STATUS AND 256%)=Z% THEN &
		  OPEN S$ AS FILE W%, MODE T% &
	  ELSE &
		  IF D$="/O" OR STATUS AND 256% THEN &
			  OPEN S$ FOR OUTPUT AS FILE W% &
		  ELSE &
			  PRINT "Illegal option" &
			\ CLOSE T% &
			\ CLOSE B% &
			\ GOTO 32767
1030	  FIELD #B%, FNC%(M%,B%,Z%) AS E$,T% AS D$,T% AS D0$,8% AS E$,W% AS N0$
1035	D%=CVT$%(D$) &
	\ D0%=CVT$%(D0$) &
	\ F1%=-W% &
	\ PRINT "Total of";ASCII(N0$);"weeks in monthly portion!" &
	\ IF D%=Z% OR D0%=Z% THEN &
		  PRINT "No data in week" &
		\ CLOSE W% &
		\ GOTO 32767
1040	  PRINT "Do you want to transfer weekly portion to month at end "; &
		"of printing (Y/N)"; &
	\ INPUT O$ &
	\ F%=LEFT(CVT$$(O$,255%),W%)="Y" &
	\ INPUT "Separate Project no.'s by pages (Y/N)";O$ &
	\ F0%=(NOT LEFT(CVT$$(O$,255%),W%)="N") &
	\ OPEN "%DETENT" FOR INPUT AS FILE M%, MODE W%
1050	  U$="###!\ \ \    \ \         \########   \    \ \   \  #####  ### " &
	\ Q5$="      " &
	\ Q6$="           " &
	\ Q7$="        " &
	\ D5$="      " &
	\ Q1$="      " &
	\ FIELD #T%, FNC%(128%,T%,Z%) AS E$,T% AS E$ &
	\ E0%=CVT$%(E$) &
	\ L5%=66% &
	\ U1$="TOTAL:\      \ \         \ ######## \     \ \   \ #######"
1060	  GOSUB 4000 &
	\ IF K0%=-W% THEN &
		  GOSUB 6300 IF F5% &
		\ PRINT #W%, CHR$(10%); FOR L5%=L5%+W% TO 66% &
		\ GOTO 2500
1070	  F2%=Z% &
	\ FOR K%=K0% TO K1% &
		\ S$=CHR$(K%) &
		\ K2%=FNB%(E0%,W%,S$) &
		\ GOTO 1140 IF K2%=-W% &
		\ FIELD #T%, FNG%(K2%-W%) AS E$,W% AS Q$ FOR K2%=K2%+W% STEP - &
			W% UNTIL Q$<>S$ OR K2%=Z% &
		\ K2%=K2%+W%
1080		  FIELD #T%, FNG%(K2%) AS E$,T% AS A$,T% AS I$ &
		\ I0%=CVT$%(I$) &
		\ GOTO 1140 IF ASCII(A$)<>K% &
		\ FIELD #B%, FNC%(M%,B%,I0%) AS E$,W% AS F0$ &
		\ GOTO 1130 IF F0$="D" OR F0$="L" &
		\ IF F2%=Z% OR F0% AND ASCII(A$)<>A0% THEN &
			  GOSUB 6200 &
			\ F2%=-W% &
			\ A0%=ASCII(A$)
1090		  GOSUB 5000 &
		\ GOSUB 6100 &
		\ RSET Q5$=FNT$(FNV(C1$)) &
		\ C=CVT$F(C0$) &
		\ GOSUB 5250 &
		\ GOSUB 5010
1100		  RSET D5$=FNT$(FNV(D2$)) &
		\ RSET Q1$=NUM$(FNV(Q$)) &
		\ RSET Q1$="" IF Q1$="    0 " &
		\ D3%=CVT$%(D0$) &
		\ S$="" &
		\ S$="CREATED "+DATE$(D3%) IF D3%<>D% OR F0$="N" &
		\ D3%=CVT$%(D1$) &
		\ S$="DELETED "+DATE$(D3%) IF F0$="K" OR D3%<>D0% &
		\ S$="ERROR WITH ACCT!" IF D3%=Z%
1110		  IF F1% THEN &
			  I0%=FNS%(A$) &
			\ IF I0%<>-W% THEN &
				  FIELD #M%, FNC%(11%,M%,I0%+22%)+T% AS E$,T% &
					 AS E$ &
				\ S$="ZAPPED "+DATE$(CVT$%(E$)) IF LEFT(S$,W%) &
					<>"D" &
				\ UNLOCK #M%
1120		  PRINT #W%, USING U$, ASCII(A$),",",RIGHT(NUM$(CVT$%(A$) AND &
			255%),T%),Q5$,Q6$,CVT$F(K1$),D5$,Q1$,FNV(D3$),ASCII(C2$ &
			)+W%; &
		\ PRINT #W%, S$ &
		\ F5%=-W%
1130		  K2%=K2%+W% &
		\ GOTO 1080 UNLESS K2%>E0%
1140	  NEXT K% &
	\ GOTO 1060
2500	  IF F4%=Z% THEN &
		  CLOSE W% &
		\ GOTO 3000
2510	  FOR K%=W% TO E0% &
		\ I0%=K% &
		\ GOSUB 5000 &
		\ FIELD #B%, I1% AS E$,W% AS F$ &
		\ GOTO 2520 IF F$="D" OR F$="L" &
		\ GOSUB 5010
2520	  NEXT K% &
	\ PRINT #W% &
	\ PRINT #W%, TAB(7%);"Final totals for the week of ";DATE$(D%);" to "; &
		DATE$(D0%);" on ";DATE$(Z%) &
	\ PRINT #W%, TAB(36%);"Page";P5%+W%
2530	  PRINT #W%, CHR$(10%);TAB(11%); &
		"Connect     CPU-Time     Kilo-Core     ";"Device     Blocks" &
	\ PRINT #W%, TAB(12%); &
		"HH:MM     HH:MM:SECS      Ticks        HH:MM      Used"
2540	  O$="" &
	\ S$=FNT$(S(T%)) &
	\ O$=O$+SPACE$(17%-LEN(S$))+S$ &
	\ C=S(W%) &
	\ GOSUB 5250 &
	\ S$=X$+X1$ &
	\ O$=O$+SPACE$(15%-LEN(S$))+S$ &
	\ PRINT #W%, O$; &
	\ PRINT #W%, USING " ###########", S(Z%)» &
	\ S$=FNT$(S(B%)) &
	\ O$=SPACE$(12%-LEN(S$))+S$
2550	  PRINT #W%, O$; &
	\ PRINT #W%, USING "   #######", S(M%) &
	\ PRINT #W%, CHR$(10%); FOR L5%=8% TO 66% &
	\ CLOSE W% &
	\ GOTO 3000
3000	  GOTO 30000 UNLESS F% &
	\ PRINT "Beginning update." &
	\ S$=STRING$(33%,Z%) &
	\ FOR K%=W% TO E0% &
		\ FIELD #B%, FNC%(M%,B%,K%) AS E$,W% AS F$,33% AS X$ &
		\ GOTO 3050 IF F$="D" OR F$="L" &
		\ I0%=K% &
		\ GOSUB 5000
3010		  FIELD #B%, I1%+34% AS E$,T% AS D3$,T% AS D4$,W% AS N1$,8% AS &
			C5$,8% AS K5$,W% AS H6$,T% AS C6$,W% AS H5$,T% AS D5$, &
			M% AS E$,8% AS C7$,49% AS E$,T% AS A$ &
		\ Z%(B%)=-W% &
		\ LSET N1$=CHR$(ASCII(N1$)+W%) &
		\ LSET D3$=D0$ IF CVT$%(D3$)=Z% &
		\ LSET D4$=D1$
3020		  LSET C5$=CVTF$(CVT$F(C5$)+CVT$F(C0$)) &
		\ LSET K5$=CVTF$(CVT$F(K5$)+CVT$F(K1$)) &
		\ C=FNV(C6$)+65536.*ASCII(H6$)+FNV(C1$) &
		\ I1%=C/65536. &
		\ LSET C6$=CVT%$(FNV1%(C-I1%*65536.)) &
		\ LSET H6$=CHR$(I1%)
3030		  IF ASCII(A$)>10% THEN &
			  C=CVT$F(K1$)/100000.* &
			15.+8.*FNV(D2$)/60. &
		  ELSE &
			  C=CVT$F(K1$)/100000.*20.+10.*FNV(D2$)/60.
3040		  C=INT(C*100.+.5) &
		\ LSET C7$=CVTF$(CVT$F(C7$)+C) &
		\ C=FNV(D5$)+65536.*ASCII(H5$)+FNV(D2$) &
		\ I1%=C/65536. &
		\ LSET D5$=CVT%$(FNV1%(C-I1%*65536.)) &
		\ LSET H5$=CHR$(I1%) &
		\ LSET F$="M" IF F$="N" &
		\ LSET F$="L" IF F$="K" &
		\ LSET X$=S$
3050	  NEXT K% &
	\ FIELD #B%, FNC%(M%,B%,Z%) AS E$,T% AS D$,T% AS D0$,T% AS D1$,T% AS D2$ &
	\ LSET D1$=D$ UNLESS CVT$%(D1$) &
	\ LSET D2$=D0$ &
	\ LSET D$,D0$=CVT%$(Z%) &
	\ LSET N0$=CHR$(ASCII(N0$)+W%) &
	\ PUT #B%, RECORD W% &
	\ GOTO 30000
4000	  K0%=-W% &
	\ RETURN IF N$="" OR N$="NONE" &
	\ I1%=INSTR(W%,N$,",") &
	\ I1%=LEN(N$)+W% UNLESS I1% &
	\ A%=INSTR(W%,N$,"-") &
	\ K0%=Z% &
	\ K1%=255% &
	\ IF LEFT(N$,I1%-W%)<>"ALL" THEN &
		  IF A%=Z% OR A%>I1% THEN &
			  K0%,K1%=VAL(LEFT(N$,I1%-W%)) &
		  ELSE &
			  GOTO 4020
4010	  N$=RIGHT(N$,I1%+W%) &
	\ RETURN
4020	  K0%=VAL(LEFT(N$,A%-W%)) &
	\ K1%=VAL(MID(N$,A%+W%,I1%-A%-W%)) &
	\ GOTO 4010
4030	  PRINT "Error in proj. no. string."» &
	\ PRINT "  Will not update into monthly";" portion."; IF F% &
	\ F%=Z% &
	\ PRINT &
	\ PRINT "Please rerun this program!" &
	\ K0%=-W% &
	\ RETURN
4040	  PRINT "Project numbers to print"; &
	\ INPUT LINE N$ &
	\ N$=CVT$$(N$,255%) &
	\ RETURN UNLESS INSTR(W%,N$,"(") OR INSTR(W%,N$,")") OR INSTR(W%,N$, &
		",,") OR INSTR(W%,N$,".") &
	\ PRINT "Illegal proj. # format - ";N$ &
	\ GOTO 4040
4050	  RESUME  IF ERL=5230% &
	\ IF ERL=3050% OR ERL=4050% THEN &
		  SLEEP W% &
		\ PUT #B%, RECORD W% &
		\ GOTO 30000
4060	  IF ERL=5220% OR ERL=4060% THEN &
		  SLEEP W% &
		\ PUT #I1%, RECORD B%(I1%) &
		\ GOTO 5230
4070	  SLEEP W% &
	\ PRINT "Block interlocked at line";ERL UNLESS ERL=30000% &
	\ RESUME
5000	  I1%=FNC%(M%,B%,I0%) &
	\ FIELD #B%, I1%+W% AS E$,T% AS D0$,T% AS D1$,8% AS C0$,8% AS K1$,T% &
		 AS C1$,T% AS D2$,M% AS E$,T% AS Q$,T% AS D3$,W% AS C2$ &
	\ RETURN
5010	  S(Z%)=S(Z%)+CVT$F(K1$) &
	\ S(W%)=S(W%)+CVT$F(C0$) &
	\ S(T%)=S(T%)+FNV(C1$) &
	\ S(B%)=S(B%)+FNV(D2$) &
	\ S(M%)=S(M%)+FNV(D3$) &
	\ RETURN
5100	  DEF FNV(S$) &
	\ L9%,FNV=CVT$%(S$) &
	\ FNV=65536.+L9% IF L9%<Z% &
	\ FNEND
5110	  DEF FNT$(C) &
	\ C1=INT(C/60.) &
	\ FNT$=CVT$$(NUM$(C-C1*60.),T%) &
	\ FNT$=CVT$$(NUM$(C1),T%)+":"+MID(NUM$(100%+C-C1*60%),B%,T%) IF C1<>Z% &
	\ FNEND
5120	  DEF FNB%(K0%,I1%,S$) &
	\ K0%=K0%+W%
5130	  L9%=(I1%+K0%)/T% &
	\ FIELD #T%, FNG%(L9%) AS E$,W% AS Q$ &
	\ IF S$<>Q$ AND L9%=I1% THEN &
		  GOTO 5150 &
	  ELSE &
		  IF Q$>S$ THEN &
			  K0%=L9% &
		  ELSE &
			  IF Q$<S$ THEN &
				  I1%=L9% &
			  ELSE &
				  GOTO 5160
5140	  GOTO 5130
5150	  L9%=-W%
5160	  FNB%=L9% &
	\ FNEND
5170	  DEF FNG%(K0%) &
	\ FNG%=K0%*M% &
	\ FNEND
5180	  DEF FNV1%(C) &
	\ C=C-65536. IF C>32767. &
	\ FNV1%=C &
	\ FNEND
5220	  DEF FNC%(K9%,I1%,Y%) &
	\ L8%=Y%/K9% &
	\ GOTO 5240 IF L8%=B%(I1%) &
	\ PUT #I1%, RECORD B%(I1%)+W% IF Z%(I1%)
5230	  B%(I1%)=L8% &
	\ Z%(I1%)=Z% &
	\ GET #I1%, RECORD L8%+W%
5240	  FNC%=(Y%-L8%*K9%)*(512%/K9%) &
	\ FNEND
5250	  X$=FNT$(INT(C/600.))+":" &
	\ X$="" IF X$="0:" &
	\ X1$=MID(NUM$(100.01+(C-INT(C/600.)*600.)/10.),B%,M%) &
	\ X1$=" "+RIGHT(X1$,T%) IF LEFT(X1$,W%)="0" AND X$="" &
	\ RSET Q6$=X$+X1$ &
	\ RETURN
5260	  DEF FNS%(A$)
5270	  GET #M%, RECORD T% UNLESS B%(M%)=W% &
	\ B%(M%)=W% &
	\ UNLOCK #M% &
	\ FOR I0%=Z% TO 109% &
		\ FIELD #M%, I0%*T% AS E$,T% AS E$ &
		\ GOTO 5280 IF A$=E$ &
	\ NEXT I0% &
	\ I0%=-W%
5280	  FNS%=I0% &
	\ FNEND
6000	  PRINT #W%, CHR$(10%); FOR L5%=L5%+W% TO 67% &
	\ PRINT #W%, TAB(-5%*(NOT F0%));"Accounting dump "» &
	\ PRINT #W%, "of group";K%; IF F0% &
	\ PRINT #W%, "for the week of ";DATE$(D%);" to ";DATE$(D0%);" on "; &
		DATE$(Z%) &
	\ P5%=P5%+W% &
	\ PRINT #W%, TAB(36%);"Page";P5%
6010	  PRINT #W%, CHR$(10%);"Account Connect  CPU-Time  Kilo-Core  Device"+ &
		"       Blocks Disk       Additional" &
	\ PRINT #W%, "  No.    HH:MM  HH:MM:"; &
		"SECS   Ticks    HH:MM  Quota  Used  Clu.        Comments"
6020	  PRINT #W%, "-"; FOR L5%=W% TO 80% &
	\ L5%=7% &
	\ PRINT #W% &
	\ RETURN
6100	  L5%=L5%+W% &
	\ RETURN IF L5%<64%
6110	  L5%=L5%-W% &
	\ GOSUB 6000 &
	\ L5%=L5%+W% &
	\ RETURN
6200	  GOSUB 6300 IF F5% &
	\ GOTO 6110
6300	  PRINT #W% &
	\ C=S(W%) &
	\ GOSUB 5250 &
	\ RSET Q7$=FNT$(S(T%)) &
	\ RSET D5$=FNT$(S(B%)) &
	\ RSET Q1$="" &
	\ PRINT #W%, USING U1$, Q7$,Q6$,S(Z%),D5$,Q1$,S(M%) &
	\ L5%=L5%+T% &
	\ S(F5%)=Z% FOR F5%=Z% TO M% &
	\ F5%=Z% &
	\ RETURN
20000	  IF ERR=19% THEN &
		  RESUME 4050
20010	  IF ERR=10% AND ERL=1000% THEN &
		  PRINT "$MONEY.MST is open in normal mode,";" will wait!" IF &
			F%/8%*8%=F% &
		\ F%=F%+W% &
		\ SLEEP F% &
		\ RESUME
20020	  IF (ERR=5% OR ERR=10%) AND ERL=1040% THEN &
		  F1%=Z% &
		\ RESUME 1050
20030	  IF ERR=19% THEN &
		  SLEEP W% &
		\ IF ERL=5270% THEN &
			  RESUME 5270 &
		  ELSE &
			  IF ERL=5230% THEN &
				  RESUME
20100	  ON ERROR GOTO 0
30000	  GET #B%, RECORD W% &
	\ IF ASCII(N0$)>B% THEN &
		  PRINT "Time to transfer monthly "; &
			"portion to yearly portion of file." &
		\ PRINT "To print and transfer ";"monthly portion, run PRTMON!" &
		\ PRINT STRING$(7%,7%)»
32767	  CLOSE T% &
	\ CLOSE B% &
	\ END
