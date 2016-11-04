1	EXTEND
10	! &
	!  W2 INFORMATION ENTRY FOR NON-COMPUTERIZED PAYROLLS &
	! &
	! 01/18/91 - Kevin Handy &
	!	Formatted for new W2 format &
	! &
	! 01/16/92 - Kevin Handy &
	!	Reformatted source code &
	!	Added Medicare business &
	! &
	! 01/22/92 - Kevin Handy &
	!	Reformatted for 92 form &
	! &
	! 01/10/94 - Kevin Handy &
	!	Reformatted for 93 from. &
	! &
	! 01/01/95 - Kevin Handy &
	!	Change fica limit to 60600. &
	! &
	! 01/08/96 - Kevin Handy &
	!	Change FICA limit to 61200 &

15	! a$() &
	!	1 - Employee Code &
	!	2 - Name &
	!	3 - Address 1 &
	!	4 - Address 2 &
	!	5 - Address 3 &
	!	6 - Soc. Sec. Number &
	!	7 - Pension Plan flag &
	!	8 - State &
	! &
	! a() &
	!	1 - Federal Wages &
	!	2 - Federal Tax &
	!	3 - Fica Wage &
	!	4 - Fica Tips &
	!	5 - Fica Tax &
	!	6 - State Wages &
	!	7 - State Tax &
	!	8 - Advance EIC Payment &
	!	9 - Allocated Tips &
	!	10 - Medicare Wage &
	!	11 - Medicare Tax &

20	DIM A$(11%),A(11%),B$(19%),T(11%),T1(11%),D$(11%),A1$(8%), A1(11%) &
\	FICA = 61200. &

30	OPEN "KB:" AS FILE 1%, RECORDSIZE 256% &

40	FIELD #1%, &
		 6% AS B$(1%), &
		30% AS B$(2%), &
		30% AS B$(3%), &
		30% AS B$(4%), &
		30% AS B$(5%), &
		11% AS B$(6%), &
		 1% AS B$(7%), &
		 2% AS B$(8%) &
\	FIELD #1%,(140%+8%*(I%-1%)) AS E$,8% AS B$(I%+8%) FOR I%=1% TO 11% &
\	FIELD #1%,256% AS T$ &

50	C$=FNX$ &
\	INPUT "COMPANY CODE";C$ IF FNX$="" &
\	C$=LEFT(C$,4%) &
\	GOTO 10000 IF C$="" &
\	IF FNO%(2%,"W2"+C$+".DAT","","") &
	THEN &
		PRINT "FILE ";C$;" DOES NOT EXIST.  SHALL I CREATE IT"; &
\		INPUT E$ &
\		GOTO 10000 IF LEFT(E$,1%)<>"Y" &
\		IF FNO%(2%,"W2"+C$+".DAT","/CR:8,256","") &
		THEN &
			PRINT "ERROR;";FNS%;"IN OPEN.  CALL CMC IF NOT UNDERSTOOD." &
\			GOTO 10000 &

60	U$="        \"+SPACE$(18%)+"\=###,###.##" &
\	H$(1%)="EMPLOYEE CODE" &
\	H$(2%)="NAME" &
\	H$(3%)="ADDRESS LINE 1" &
\	H$(4%)="ADDRESS LINE 2" &
\	H$(5%)="ADDRESS LINE 3" &
\	H$(6%)="SOCIAL SECURITY NO." &
\	H$(7%)="PENSION PLAN (Y/N)" &
\	H$(8%)="STATE" &
\	D$(1%)="FEDERAL WAGES" &
\	D$(2%)="FEDERAL WITHHOLDING" &
\	D$(3%)="FICA WAGES" &
\	D$(4%)="FICA TIPS" &
\	D$(5%)="FICA WITHHOLDING" &
\	D$(6%)="STATE WAGES" &
\	D$(7%)="STATE WITHHOLDING" &
\	D$(8%)="ADVANCE EIC PAYMENT" &
\	D$(9%)="ALLOCATED TIPS" &
\	D$(10%)="MEDICARE WAGES" &
\	D$(11%)="MEDICARE TAXES" &

1000	! &
	!  CONTROL &
	! &

1020	PRINT &
\	INPUT "SELECTION";K$ &
\	K$=LEFT(K$,3%) &
\	GOTO 1030 IF K$="" &
\	GOTO 3000 IF K$="ENT" &
\	GOTO 4000 IF K$="CHA" &
\	GOTO 5000 IF K$="DEL" &
\	GOTO 6000 IF K$="PRI" OR K$="TOT" &
\	GOTO 7000 IF K$="FIN" &
\	GOTO 8000 IF K$="KIL" &
\	GOTO 9000 IF K$="W2" &
\	GOTO 10000 IF K$="END" &
\	GOTO 1020 &

1030	PRINT &
\	PRINT "OPTIONS:" &
\	PRINT "ENTER		EMPLOYEE INFORMATION" &
\	PRINT "CHANGE		INFORMATION" &
\	PRINT "DELETE		INFORMATION" &
\	PRINT "PRINT		INFORMATION" &
\	PRINT "FIND		ONE EMPLOYEE'S INFORMATION" &
\	PRINT "TOTAL		W2 FIGURES" &
\	PRINT "W2		PRINT W2'S" &
\	PRINT "KILL		THIS COMPANY'S FILE" &
\	PRINT "END		PROGRAM" &
\	GOTO 1020 &

2000	! &
	!  PREPARE W2.DAT &
	! &

2010	LSET B$(I%)=A$(I%) FOR I%=1% TO 8% &
\	LSET B$(I%+8%)=CVTF$(A(I%)) FOR I%=1% TO 11% &
\	RETURN &

2100	! &
	!  SEPARATE W2.DAT &
	! &

2110	LSET T$=FNL$ &
\	A$(I%)=B$(I%)+"" FOR I%=1% TO 8% &
\	A(I%)=CVT$F(B$(I%+8%)) FOR I%=1% TO 11% &
\	RETURN &

2200	! &
	!  INPUT EMPLOYEE CODE &
	! &

2210	PRINT H$(1%); &
\	INPUT A$(1%) &
\	A$(1%)=LEFT(A$(1%),6%) &
\	A$(1%)=A$(1%)+SPACE$(6%-LEN(A$(1%))) &
\	RETURN &

2300	! &
	!  PRINT RECORD &
	! &

2310	PRINT USING "\    \  \"+SPACE$(28%)+"\     \"+SPACE$(18%)+"\=\         \", &
		A$(1%),A$(2%),H$(6%),A$(6%) &
\	RETURN IF S$="Y" &
\	PRINT USING "        \"+SPACE$(28%)+"\     \"+SPACE$(18%)+ &
		"\=!",A$(3%),H$(7%),A$(7%) &
\	PRINT USING "        \"+SPACE$(28%)+"\     \"+SPACE$(18%)+"\=\\", &
		A$(4%),H$(8%),A$(8%) &
\	PRINT USING "        \"+SPACE$(28%)+"\",A$(5%) &
\	PRINT &
\	PRINT USING U$+"  \"+SPACE$(18%)+"\=###,###.##",D$(1%),A(1%),D$(2%),A(2%) &
\	PRINT USING U$+"  \"+SPACE$(18%)+"\=###,###.##",D$(3%),A(3%),D$(5%),A(5%) &
\	PRINT USING U$,D$(4%),A(4%) &
\	PRINT USING U$+"  \"+SPACE$(18%)+"\=###,###.##",D$(6%),A(6%),D$(7%),A(7%) &
\	PRINT USING U$,D$(8%),A(8%) &
\	PRINT USING U$,D$(9%),A(9%) &
\	PRINT USING U$+"  \"+SPACE$(18%)+"\=###,###.##",D$(10%),A(10%),D$(11%),A(11%) &
\	PRINT &
\	RETURN &

2400	! &
	! INPUT LINE &
	! &

2410	INPUT LINE K$ &
\	K$=CVT$$(K$,4%) &
\	RETURN &

2420	DEF *FNI$(K$) &
\		INPUT LINE ZZ$ &
\		ZZ$=CVT$$(ZZ$,4%) &
\		FNI$=ZZ$ &
\	FNEND &

3000	! &
	!  ENTER &
	! &

3010	GOSUB 2200 &
\	GOTO 1020 IF A$(1%)="" &
\	IF FNG%(2%,A$(1%)) = 0% &
	THEN	PRINT "THAT CODE IS IN USE." &
\		GOTO 3010 &

3020	FOR I%=2% TO 5% &
\	PRINT H$(I%); &
\	A$(I%)=FNI$(A$(I%)) &
\	NEXT I% &

3030	PRINT H$(6%); &
\	A$(6%)=FNI$(A$(6%)) &
\	IF A$(6%)="" &
	THEN	INPUT "CONFIRM BLANK SOC. SEC. NO. (Y/N)";K$ &
\		GOTO 3030 IF LEFT(K$,1%)="N" &

3040	PRINT H$(7%); &
\	A$(7%)=FNI$(A$(7%)) &
\	A$(7%)="N" IF LEFT(A$(7%),1%)<>"Y" &
\	PRINT H$(8%);" <ID>"; &
\	A$(8%)=FNI$(A$(8%)) &
\	A$(8%)="ID" IF A$(8%)="" &
\	FOR I%=1% TO 11% &
\		PRINT D$(I%); &
\		K$=FNI$(K$) &
\		A(I%)=0. &
\		A(I%)=FNZ(VAL(K$)) IF K$<>"" &
\		A(I%)=A(1%) IF K$="" AND (I%=3% OR I%=6%) &
\	NEXT I% &
\	PRINT &
\	A(3%)=FICA IF A(3%)>FICA &
\	GOSUB 2000 &
\	GOTO 3010 IF FNA%(2%,T$)= 0% &
\	PRINT "ERROR";FNS%;"IN ADDING RECORD.  CALL CMC!" &
\	GOTO 10000 &

4000	! &
	!  CHANGE &
	! &

4010	PRINT &
\	GOSUB 2200 &
\	GOTO 10020 IF CVT$$(A$(1%),132%)="" &
\	IF FNG%(2%,A$(1%)) &
	THEN	PRINT "CAN'T FIND THAT CODE" &
\		GOTO 4010 &

4020	GOSUB 2100 &
\	GOSUB 2300 &

4030	INPUT "ITEM TO CHANGE (? FOR HELP)";K$ &
\	K$=CVT$$(K$,-1%) &
\	GOTO 4100 IF K$="?" &
\	GOTO 4070 IF K$="" &
\	GOTO 4030 IF ASCII(K$)<49% OR ASCII(K$)>57% &
\	J%=VAL(K$) &
\	GOTO 4030 IF J%>17% &

4040	GOTO 4050 IF J%>8% &
\	PRINT H$(J%);"=";A$(J%) &
\	PRINT "CHANGE TO"; &
\	K$=FNI$(K$) &
\	A$(J%)=K$ IF K$<>"" &
\	GOTO 4030 IF J%>1% OR K$="" &
\	GOSUB 2000 &
\	IF FNU%(-2%,T$) &
	THEN	PRINT "ERROR";FNS%;"IN CHANGING RECORD." &
\		PRINT "CHANGE ABORTED.  CALL CMC IF YOU CANNOT CORRECT THE PROBLEM." &

4045	GOTO 4010 &

4050	ON ERROR GOTO 4200 &
\	PRINT USING "\          \=###,###.##  CHANGE TO",D$(J%-8%),A(J%-8%); &
\	K$=FNI$(K$) &
\	IF K$<>"" &
	THEN	A(J%-8%)=FNZ(VAL(K$)) &
\		ON ERROR GOTO 0 &
\		GOTO 4030 &

4070	GOSUB 2000 &
\	GOTO 4010 IF FNU%(2%,T$) = 0% &
\	PRINT "ERROR";FNS%;"IN CHANGING RECORD." &
\	PRINT "CHANGE ABORTED.  CALL CMC IF YOU CANNOT CORRECT THE PROBLEM." &
\	GOTO 4010 &

4100	! &
	!  HELP MESSAGE &
	! &

4110	PRINT "TO GET AN ITEM TO CHANGE, TYPE ITS ASSOCIATED NUMBER:" &
\	FOR I%=1% TO 10% &
\		PRINT I%;TAB(5%);H$(I%);TAB(40%);I%+8%;TAB(45%);D$(I%) &
\	NEXT I% &
\	PRINT TAB(40%);19%;TAB(45%);D$(11%) &
\	PRINT &
\	GOTO 4030 &

4200	PRINT "ITEM MUST BE A NUMBER!" &
\	ON ERROR GOTO 0 &
\	RESUME 4040 &

5000	! &
	!  DELETE &
	! &

5010	GOSUB 2200 &
\	GOTO 1020 IF CVT$$(A$(1%),132%)="" &
\	IF FNG%(2%,A$(1%)) &
	THEN	PRINT "NOT FOUND." &
\		GOTO 5010 &

5020	GOSUB 2100 &
\	GOSUB 2300 &
\	INPUT "CONFIRM DELETING (Y/N)";K$ &
\	GOTO 5010 IF LEFT(K$,1%)<>"Y" &
\	GOTO 5010 IF FND%(2%,"") = 0% &
\	PRINT "ERROR";FNS%;"IN DELETING." &
\	PRINT "CALL CMC IF YOU CANNOT CORRECT THE PROBLEM." &
\	GOTO 5010 &

6000	! &
	!  PRINT OR TOTAL &
	! &

6010	IF FNG%(2%,"") &
	THEN	PRINT "ERROR";FNS%;"ON FNG%" IF FNS%<>88% &
\		PRINT "THIS FILE IS EMPTY." IF FNS%=88% &
\		GOTO 1020 &

6015	S$="" &
\	INPUT "SHORT FORM (Y/N)";S$ IF K$="PRI" &
\	S$="Y" IF LEFT(S$,1%)<>"N" &
\	T(I%)=0. FOR I%=1% TO 11% &
\	INPUT "SET PAGE";E$ &

6020	GOSUB 2100 &
\	GOSUB 2300 IF K$="PRI" &
\	T(I%)=T(I%)+A(I%) FOR I%=1% TO 11% &
\	GOTO 6020 IF FNN%(2%) = 0% &
\	PRINT &
\	PRINT "TOTALS:" &
\	PRINT &
\	PRINT USING "\"+SPACE$(28%)+"\  ##,###,###.##",D$(I%),T(I%) &
	FOR I%=1% TO 11% &
\	PRINT &
\	S$="" &
\	GOTO 1020 &

7000	! &
	!  FIND &
	! &

7010	IF FNG%(2%,"") &
	THEN	PRINT "THIS FILE IS EMPTY." &
\		GOTO 1020 &

7020	PRINT &
\	GOSUB 2200 &
\	GOTO 1020 IF CVT$$(A$(1%),132%)="" &
\	IF FNG%(2%,A$(1%)) &
	THEN	PRINT "CAN'T FIND THAT CODE" &
\		GOTO 7020 &

7030	GOSUB 2100 &
\	GOSUB 2300 &
\	GOTO 7020 &

8000	! &
	!  KILL &
	! &

8010	PRINT "VERIFY KILLING THE FILE FOR COMPANY ";C$;" (Y/N)"; &
\	INPUT K$ &
\	GOTO 1020 IF LEFT(K$,1%)<>"Y" &
\	KILL "W2"+C$+".DAT" &
\	KILL "W2"+C$+".DA1" &
\	GOTO 10000 &

9000	! &
	!  W2 PRINTER &
	! &

9100	OPEN "KB:" AS FILE 12% &

9200	T(I%)=0. FOR I%=1% TO 11% &
\	H1$="   #########.##" &
\	H$(0%)="\"+SPACE$(28%)+"\   " &
\	H$(1%)="\    \"+SPACE$(62%)+"\    \" &
\	H$(2%)=H$(0%)+"\            \   \            \" &
\	H$(2%)=H$(2%)+"    "+H$(2%) &
\	H$(3%)=H$(0%)+SPACE$(35%)+H$(0%) &
\	H$(4%)=H$(0%)+"         \\" &
\	H$(4%)=H$(4%)+SPACE$(24%)+H$(4%) &
\	H$(5%)="\         \      "+H1$+" "+H1$+H1$ &
\	H$(5%)=H$(5%)+"     "+H$(5%) &
\	H$(6%)=H$(0%)+H1$+H1$+"     "+H$(0%)+H1$+H1$ &
\	H$(7%)="#######.##  ######.##  \      \"+SPACE$(33%) &
\	H$(7%)=H$(7%)+"    "+H$(7%) &
\	H$(8%)=H$(0%)+H1$+SPACE$(20%)+H$(0%)+H1$ &

9300	PRINT "ENTER 'Y' FOR THOSE WITH SSN OR 'N' FOR THOSE WITHOUT"; &
\	INPUT B$ &
\	B$="Y" IF B$="" &
\	GOTO 9300 IF B$<>"Y" AND B$<>"N" &
\	PRINT "EMPLOYEES' CONTROL NUMBER (Y/N)"; &
\	C1$=LEFT(FNI$(K$),1%) &
\	PRINT "FEDERAL ID #"; &
\	E1$(6%)=LEFT(FNI$(K$),14%) &

9310	S%(0%)=0% &

9320	PRINT "YOU WILL NOW BE ASK FOR STATE ABBREVIATIONS AND" &
\	PRINT "STATE TAX NUMBERS FOR EACH STATE YOU HAVE.  HIT" &
\	PRINT "RETURN WHEN ALL ARE IN." &
\	INPUT "State abbreviation ";S$ &
\	S$=CVT$$(S$,4%) &
\	IF S$<>"" &
	THEN	S%(0%)=S%(0%)+1% &
\		S$(S%(0%))=S$ &
\		INPUT "     State withholding tax number ";S$ &
\		S1$(S%(0%))=CVT$$(S$,4%) &
\		GOTO 9320 &

9330	PRINT &
\	PRINT "EMPLOYER'S NAME"; &
\	E1$(1%)=LEFT(FNI$(K$),32%) &
\	FOR I%=2% TO 5% &
\	PRINT "ADDRESS LINE";I%-1%; &
\	E1$(I%)=LEFT(FNI$(K$),32%) &
\	NEXT I% &

9400	! &
	!  PROGRAM CONTROL SECTION &
	! &

9410	GOTO 10000 IF FNG%(2%,"") &
\	INPUT "START WITH EMPLOYEE #";K$ &
\	GOTO 9410 IF FNG%(2%,K$) &
\	GOSUB 9500 &

9420	GOSUB 2100 &
\	F%=F%+1% &
\	GOSUB 9700 IF F%=42% &
\	GOSUB 9600 &
\	GOTO 9420 IF FNN%(2%) = 0% &

9430	F% = 1% - (F% - F% / 2% * 2%) &
\	PRINT STRING$(33%*F%,10%); &
\	GOSUB 9700 &
\	GOSUB 9800 &
\	PRINT STRING$(5%,7%); &
\	INPUT LINE #12%,A$ &
\	V$=SYS(CHR$(2%)) &
\	GOTO 10000 &

9500	!******************************************************************* &
	!  POSITION CHECKS IN PRINTER &
	!******************************************************************* &

9510	PRINT "POSITION # SIGN ON EDGE OF FIRST W-2 FORM." &
\	PRINT "PRESS 'P' AND RETURN TO PRINT W-2 FORMS." &
\	V$=SYS(CHR$(3%)) &

9520	INPUT LINE #12%,A$ &
\	A$=CVT$$(A$,4%) &
\	RETURN IF A$="P" &
\	PRINT "#";CHR$(8%); &
\	GOTO 9520 &

9600	!******************************************************************* &
	!  PRINT INFORMATION ON W-2 FORM &
	!******************************************************************* &

9610	IF A(1%)=0. OR (FNV%(A$(6%)) AND B$="Y") OR (FNV%(A$(6%))=0% AND B$="N") &
	THEN	F%=F%-1% &
\		RETURN &

9620	A(3%)=FICA IF A(3%)>FICA &
\	T(I%)=T(I%)+A(I%) FOR I% = 1% TO 11% &
\	S0$="IDAHO" &
\	S0$=A$(8%) IF A$(8%)<>"ID" &
\	N$=A$(6%) &
\	N$="" IF B$="N" &
\	FLAG$(I%) = "" FOR I% = 1% TO 9% &
\	FLAG$(3%) = "X"  IF A$(7%)="Y" &

9625	S$ = "" &
\	S$(1%) = A$(8%) IF S$(1%) = "" &
\	S$ = S1$(I%) IF A$(8%)=S$(I%) AND S1$(I%)<> "" &
		FOR I% = 1% TO S%(0%) &

9630	A1(I%) = A(I%) FOR I% = 1% TO 11% &
\	A1$(I%) = A$(I%) FOR I% = 1% TO 8% &
\	GOSUB 9650 &
\	RETURN &

9650	!-------------PRINT W2'S------------ &
	OUTPUT.CH% = 0% &
\	DW% = -1% &
	&
\	PRINT #OUTPUT.CH%, STRING$(2%,10%);'13'C; &
\	X% = FNDWPRINT%(OUTPUT.CH%, FORMAT$(A1$(1%), '\    \        ') + &
		FLAG$(8%), DW%) &
\	PRINT #OUTPUT.CH% &
\	X% = FNDWPRINT%(OUTPUT.CH%, FORMAT$(E1$(6%),'\                    \   ') + &
		"        " + &
		FORMAT$(A1(1%), '#########.##   ') + &
		FORMAT$(A1(2%), '#########.##'), DW%) &
\	PRINT #OUTPUT.CH% &
\	X% = FNDWPRINT%(OUTPUT.CH%, FORMAT$(E1$(1%), '\'+SPACE$(28%)+'\   ') + &
		FORMAT$(A1(3%),'#########.##   ')+ &
		FORMAT$(A1(5%),'#########.##'), DW%) &

9655	X% = FNDWPRINT%(OUTPUT.CH%, FORMAT$(E1$(2%), '\'+SPACE$(28%)+'\'), DW%) &
\	X% = FNDWPRINT%(OUTPUT.CH%, FORMAT$(E1$(3%), '\'+SPACE$(28%)+'\   ') + &
		FORMAT$(A1(10%), '#########.##   ')+ &
		FORMAT$(A1(11%), '#########.##'), DW%) &
\	PRINT #OUTPUT.CH% &
\	X% = FNDWPRINT%(OUTPUT.CH%, FORMAT$(E1$(4%),'\                               \') + &
		FORMAT$(A1(4%), '#########.##   ') + &
		FORMAT$(A1(9%), '#########.##'), DW%) &
\	PRINT #OUTPUT.CH% &

9660	X% = FNDWPRINT%(OUTPUT.CH%, FORMAT$(N$,'\'+SPACE$(28%)+'\   ') + &
		FORMAT$(A1(8%),'#########.##') + &
		FORMAT$(0.0, '   <%>########.##'), DW%) &
\	PRINT #OUTPUT.CH% &
\	X% = FNDWPRINT%(OUTPUT.CH%, FORMAT$(A1$(2%),'\'+SPACE$(28%)+'\') + &
		'               ' + &
		FORMAT$(0.0, '   <%>########.##'), DW%) &
\	X% = FNDWPRINT%(OUTPUT.CH%, FORMAT$(A1$(3%),'\'+SPACE$(28%)+'\'), DW%) &
\	X$ = FORMAT$(A1$(4%),'\'+SPACE$(28%)+'\') &
\	X% = FNDWPRINT%(OUTPUT.CH%, X$, DW%) &
\	X$ = FORMAT$(CVT$$(A1$(5%), 8%), '\'+SPACE$(28%)+'\') &
\	X% = FNDWPRINT%(OUTPUT.CH%, X$, DW%) &
\	X$ = FORMAT$(A1$(6%),'\'+SPACE$(28%)+'\') &
\	PRINT #OUTPUT.CH% &
\	X% = FNDWPRINT%(OUTPUT.CH%, SPACE$(33%)+ &
		FLAG$(1%)+'   ' +FLAG$(2%)+'   '+FLAG$(3%)+'   '+FLAG$(4%)+'   '+ &
		FLAG$(5%)+'   '+FLAG$(6%)+'   '+FLAG$(7%), DW%) &
\	PRINT #OUTPUT.CH% &

9665	PRINT #OUTPUT.CH% &
\	PRINT #OUTPUT.CH% &
\	X% = FNDWPRINT%(OUTPUT.CH%, &
		FORMAT$(A1$(8%),'\\  ') + &
		FORMAT$(S$,'\           \') + &
		FORMAT$(A1(6%), '#######.##') + &
		FORMAT$(A1(7%),'######.##') &
		, DW%) &
\	PRINT #OUTPUT.CH%, STRING$(8%,10%) &
\	RETURN &

9700	!******************************************************************* &
	! PRINT SUMMARY ON EVERY 42'ND FORM &
	!******************************************************************* &

9710	FLAG$(I%) = "" FOR I% = 1% TO 9% &
\	FLAG$(6%) = "X"  IF A$(7%)="Y" &
\	S$ = "" &
\	A1(I%) = T(I%) FOR I% = 1% TO 11% &
\	A1$(1%) = "" &
\	A1$(2%) = "********************" &
\	A1$(3%) = "** SUBTOTAL OF    **" &
\	A1$(4%) = "** W2'S PRINTED   **" &
\	A1$(5%) = "********************" &
\	A1$(6%) = "" &
\	A1$(7%) = "" &
\	A1$(8%) = "" &
\	T1(I%) = T1(I%) + T(I%) FOR I% = 1% TO 11% &
\	T(I%) = 0.0 FOR I% = 1% TO 11% &
\	GOSUB 9650 &
\	RETURN &

9800	!******************************************************************* &
	! PRINT GRAND TOTAL AT BOTTOM &
	!******************************************************************* &

9810	FLAG$(I%) = "" FOR I% = 1% TO 9% &
\	FLAG$(6%) = "X"  IF A$(7%)="Y" &
\	S$ = "" &
\	A1(I%) = T1(I%) FOR I% = 1% TO 11% &
\	A1$(1%) = "" &
\	A1$(2%) = "************************" &
\	A1$(3%) = "** THIS W2 FORM IS    **" &
\	A1$(4%) = "** THE GRAND TOTAL    **" &
\	A1$(5%) = "************************" &
\	A1$(6%) = "" &
\	A1$(7%) = "" &
\	A1$(8%) = "" &
\	GOSUB 9650 &
\	RETURN &

10000	! &
	!  END &
	! &

10005	K$="" &
\	I%=0% &
\	C$="" &

10010	CLOSE 1%,12% &
\	V%=FNX%(K$,I%,C$) &

10020	K$="[1,8]W2ENT.TSK" &
\	I%=10% &
\	GOTO 10010 &

14000	DEF FNZ(Z)=INT(Z*100.+0.51)/100. &

14100	! &
	!  CHECK FOR VALID SSN &
	! &

14110	DEF *FNV%(A$) &
\	GOTO 14130 IF LEN(CVT$$(A$,136%))<>9% &
\	ON ERROR GOTO 14120 &
\	A1=VAL(A$) &
\	FNV%=0% &
\	GOTO 14150 &

14120	RESUME 14130 &

14130	ON ERROR GOTO 0 &
\	A1$=CVT$$(LEFT(A$,3%)+MID(A$,5%,2%)+RIGHT(A$,8%),-1%) &
\	IF LEN(A1$)<>9% &
	THEN	FNV%=-1% &
\		GOTO 14150 &

14140	ON ERROR GOTO 14160 &
\	A1=VAL(A1$) &
\	A1=0. &
\	FNV%=0% &

14150	ON ERROR GOTO 0 &
\	FNEND &

14160	FNV%=-1% &
\	RESUME 14150 &

19000	ON ERROR GOTO 0 &

30700	!*************************************************************** &
	! Print out information (Double wide if necessary) &
	!*************************************************************** &
	DEF FNDWPRINT%(OUTPUT.CH%, TEXT$, DW%) &
\		ZZ% = INSTR(1%, TEXT$, '197'C) &
\		TEXT$ = LEFT(TEXT$, ZZ% - 1%) + " " + RIGHT(TEXT$, ZZ% + 1%) &
			IF ZZ% &
\		PRINT #OUTPUT.CH%, TEXT$; &
\		PRINT #OUTPUT.CH%, TAB(68%); CVT$$(TEXT$, 128%); IF DW% &
\		PRINT #OUTPUT.CH% &
\	FNEND &

32767	END
