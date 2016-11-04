10	! &
	! CHANGED JAN 29, 1985 BY WENDELL RICHARDSON &
	! TO TAKE OUT CHECK ON SOCIAL SECURITY NUMBERS &
	! ALSO TO CORRECT LOGIC ERROR WHEN STARTING IN THE &
	! MIDDLE AND THERE IS MORE THAN ONE STATE &
	! &
	! SEE VARIABLE YEAR$ TO SET UP FOR NEXR YEAR &
	! SEE VARIABLE FICA.LIMIT TO SET UP FOR NEXT YEAR &
	! &
	! NOTE:::::: Compile with [1,3]ISAMRD or you will get &
	! memory exceeded errors &
	! &
	!	01/14/92 - Kevin Handy &
	!		Cleaned out unused variables &
	!		Reformatted IF-THEN-ELSE &
	!		Reformatted '\'s. &
	! &
	!	12/23/92 - Kevin Handy &
	!		Changed rates for 92 &

36	Y$=SYS(CHR$(12%)) &
\	CHANGE Y$ TO Y% &
\	PRGNUM$="["+NUM1$(Y%(6%))+","+NUM1$(Y%(5%))+"]" &
\	PRGNAM$=RAD$(Y%(7%)+SWAP%(Y%(8%)))+RAD$(Y%(9%)+SWAP%(Y%(10%)))+"."+ &
		RAD$(Y%(11%)+SWAP%(Y%(12%))) &

37	YEAR$="92" &
\	FICA.LIMIT=55500. &
\	FICA.RATE=0.062 &
\	HI.LIMIT=130200. &

38	DEF FNZ(X) = INT(X*100.0+0.5)/100.0 &
	! Nobody better make a negitive wage &

40	PRINT "This program prints W2 forms.  Type 'Y' to continue" &
\	PRINT "or 'N' to exit this program "; &
\	INPUT LINE F1$ &
\	F1$=CVT$$(F1$,4%) &
\	F1$="" IF LEFT(F1$,1%)="Y" &
\	GOTO 10000 IF LEFT(F1$,1%)="N" &
\	F$=F1$ &
\	F$=F1$+"MSTR"+YEAR$+".DAT" IF RIGHT(F1$,LEN(F1$))="]" OR F1$="" &
\	IF FNO%(2%,F$,"/RO","") &
	THEN	F$=F1$+"MSTRFL.DAT" IF RIGHT(F1$,LEN(F1$))="]" OR F1$="" &
\		V%=FNO%(2%,F$,"/RO","") &

45	IF FNS% &
	THEN	PRINT &
\		PRINT "Unable to open the master payroll file." &
\		PRINT "Aborting. . . ";FNX%("",0%,"") &

50	F.O.FLAG%=-1% &
\	INPUT "Is there more than one state in the master file ";K$ &
\	IF CVT$$(LEFT(K$,1%),-1%)="Y" &
	THEN	I%=INSTR(1%,F$,".") &
\		I%=LEN(F$)+1% IF I%=0% &
\		F$=LEFT(F$,I%-1%)+".DA1" &
\		PRINT "Creating a new index file.  "; &
\		V%=FNX%("[21,6]ISMBLD",30000%,F$+"/I@485-2;1-6;$"+ &
			PRGNUM$+PRGNAM$+"#60") &

60	IF F.O.FLAG%=0% &
	THEN	F$=FNX$ &
\		IF FNO%(2%,F$,"/RO","") &
		THEN	PRINT "Error";FNS%,"  Try again. . ." &
\			GOTO 40 &

120	DIM H$(8%),T(8%),T1(8%),M$(23%),M1$(23%),M(36%),M2$(36%) &

1000	! &
	! Option control center &
	! &

1020	PRINT &
\	INPUT "Option ";K$ &
\	K$=LEFT(CVT$$(K$,-1%),3%) &
\	GOTO 1100 IF K$="ENT" &
\	GOTO 1200 IF K$="PRI" &
\	GOTO 10000 IF K$="END" &

1030	PRINT &
\	PRINT "Options are:" &
\	PRINT "	ENTER company information" &
\	PRINT "	PRINT W2 forms" &
\	PRINT "	END program" &
\	GOTO 1020 &

1100	! &
	! ENTER COMPANY INFORMATION &
	! &

1110	ON ERROR GOTO 1190 &
\	F%=INSTR(1%,F$,"]") &
\	F1%=INSTR(1%,F$,"[") &
\	F%=(F%+1%)-F1% &
\	F%=0% IF F%<2% &
\	F1$=MID(F$,F1%,F%)+"W2.FIL" &
\	OPEN "SS0:"+F1$ FOR INPUT AS FILE 11% &
\	ON ERROR GOTO 0 &

1120	PRINT "Company information already exists.  Do you want" &
\	PRINT "to replace it (Y or N) <N> "; &
\	INPUT K$ &
\	GOTO 1000 UNLESS LEFT(K$,1%)="Y" &

1125	ON ERROR GOTO 0 &
\	OPEN "SS0:"+F1$ FOR OUTPUT AS FILE 11% &
\	A0$(I%)="" FOR I%=1% TO 15% &
\	S$(I%),S1$(I%)="" FOR I%=1% TO 10% &

1130	PRINT "1. CONTROL NUMBER (Y/N)"; &
\	INPUT LINE A0$(1%) &
\	A0$(1%)=CVT$$(LEFT(A0$(1%),1%),-1%) &
\	PRINT "Employer's federal identification number"; &
\	INPUT LINE A0$(2%) &
\	A0$(2%)=CVT$$(LEFT(A0$(2%),14%),4%) &
\	K$="" &
\	INPUT "PENSION PLAN (Y/N)"; K$ UNTIL K$="Y" OR K$="N" &
\	A0$(4%)=K$ &
\	K$="" &
\	INPUT "Are there tips to be reported (Y/N) ";K$ &
		UNTIL K$="Y" OR K$="N" &
\	A0$(5%)=K$ &
\	IF K$="Y" &
	THEN	A0$(5%)="TIPS" &
	ELSE	INPUT "INCLUDE NON-TAXED EARNINGS IN WAGES (BOX 10 & 18) (Y/N)";K$ &
\		A0$(5%)=K$ &
\		PRINT &

1140	S%(0%)=0% &

1145	INPUT "State abbreviation ";S$ &
\	S$=CVT$$(S$,4%) &
\	IF S$<>"" &
	THEN	S%(0%)=S%(0%)+1% &
\		S$(S%(0%))=S$ &
\		INPUT "     State withholding tax number ";S$ &
\		S1$(S%(0%))=CVT$$(S$,4%) &
\		GOTO 1145 &

1150	PRINT "EMPLOYER'S NAME"; &
\	INPUT LINE A0$(6%) &
\	A0$(6%)=CVT$$(A0$(6%),4%) &
\	FOR I%=7% TO 10% &
\		PRINT "ADDRESS LINE";I%-6%; &
\		INPUT LINE A0$(I%) &
\		A0$(I%)=CVT$$(A0$(I%),4%) &
\	NEXT I% &

1160	CLOSE 11% &
\	GOTO 1000 &

1190	PRINT "No information currently exists!" &
\	RESUME 1125 &

1200	! &
	! PRINT OUT W2'S &
	! &

1220	H1$="   #########.##" &
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
 !\	HX$ = "\"+SPACE$(28%)+"\      #########.##   #########.##       " + &
 !		"\"+SPACE$(28%)+"\      #########.##   #########.##" &

1230 OPEN "NL:" AS FILE 1%, RECORDSIZE 512%+32% &
\	FIELD #1%,6% AS M1$(1%),30% AS M1$(2%),30% AS M1$(3%), &
		30% AS M1$(4%),30% AS M1$(5%),11% AS M1$(6%),8% AS M1$(7%), &
		1% AS M1$(8%),2% AS M1$(9%),12% AS M1$(10%),2% AS M1$(11%) &

1240 FIELD #1%,252% AS V$,8% AS M1$(21%),8% AS M1$(22%) &
\	FIELD #1%,188%+I%*8% AS V$,8% AS M2$(I%) FOR I%=10% TO 36% &
\	FIELD #1%,484% AS V$,2% AS M1$(23%) &
\	FIELD #1%,512% AS G$,6% AS T$(1%),2% AS T$(2%),8% AS T$(3%), &
	8% AS T$(4%) &
\	FIELD #1%,512% AS T2$,32% AS T4$ &
		! Field for Master file and for Year to date tips file &

1250	F%=INSTR(1%,F$,"]") &
\	F1%=INSTR(1%,F$,"[") &
\	F%=(F%+1%)-F1% &
\	F%=0% IF F%<2% &
\	F1$=MID(F$,F1%,F%)+"W2.FIL" &
\	ON ERROR GOTO 1260 &
\	OPEN "SS0:"+F1$ FOR INPUT AS FILE 11% &
\	DIM #11%, A0$(15%)=32%, S%(0%), S$(10%)=4%, S1$(10%)=16% &
\	PRINT "Control number ";A0$(1%) &
\	PRINT "Federal number ";A0$(2%) &
\	PRINT "Pension Plan   ";A0$(4%) &
\	PRINT "Include non-taxed earning in wages (Box 10 & 18) ";A0$(5%) &
\	PRINT "Employer's name  ";A0$(6%) &
\	PRINT "Address line 1   ";A0$(7%) &
\	PRINT "Address line 2   ";A0$(8%) &
\	PRINT "Address line 3   ";A0$(9%) &
\	PRINT "Address line 4   ";A0$(10%) &
\	PRINT &
\	PRINT "State ";S$(I%);"  Id code ";S1$(I%) FOR I%=1% TO S%(0%) &
\	C$=A0$(1%)+"" &
\	E1$(6%)=A0$(2%) &
\	P$="" &
\	P$="XX" IF A0$(4%)="Y" &
\	G$=A0$(5%) &
\	E1$(I%)=A0$(I%+5%) FOR I%=1% TO 5% &
\	PRINT "Please type a 'Y' if everything is correct "; &
\	INPUT LINE K$ &
\	K$=CVT$$(LEFT(K$,1%),-1%) &
\	GOTO 1000 UNLESS LEFT(K$,1%)="Y" &
\	GOTO 1270 &

1260	PRINT "Before printing W2's, you must enter your companies" &
\	PRINT "Information using the ENTER option in this program." &
\	RESUME 1000 &

1270	ON ERROR GOTO 0 &

1290	F%=INSTR(1%,F$,"]") &
\	F1%=INSTR(1%,F$,"[") &
\	F%=(F%+1%)-F1% &
\	F%=0% IF F%<2% &
\	F1$=MID(F$,F1%,F%)+"TIPYTD." &
\	YEAR%=VAL(RIGHT(DATE$(0%),8%))-1% &
\	V%=FNO%(4%,F1$+NUM1$(YEAR%)+"S","/SF/RO","") &
\	V%=FNO%(4%,F1$+".DAS","/SF/RO","") IF V%=5% &
\	ALLT%=-1% &
\	ALLT%=0% IF V%<>0% &
\	ALLT%=0% IF A0$(5%)<>"TIPS" &

2000	! &
	!  PROGRAM CONTROL SECTION &
	! &

2010	T(I%),T1(I%)=0. FOR I%=1% TO 6% &
\	F%,FORM.COUNT%,PRINT.COUNT%=0% &
\	STATE$="" &
\	PRINT "Constraints on employee numbers (XXXXXX) "; &
		"Press 'return' to print all "; &
\	INPUT LINE K$ &
\	TARGET$=CVT$$(K$,4%) &
\	PRINT "Employee number to start with  Press 'return' for "; &
		"the first one"; &
\	INPUT START$ &
\	START%=0% \ START%=-1% IF START$="" &
\	IF FNG%(2%,"") &
	THEN	PRINT "File is empty.  Aborting. . ." &
\		V%=FNX%("",0%,"") &

2020	P%=10% &
\	DEV$=FNOUTPUT$(P%) &
\	P%=0% IF DEV$="" &
\	GOSUB 3000 IF DEV$="" &
\	PRINT "Processing. . . "; IF DEV$<>"" &

2030	LSET T2$=FNL$ &
\	M$(I%)=M1$(I%)+"" FOR I%=1% TO 23% &
\	IF FNCOMP%(M$(1%),TARGET$)=0% AND TARGET$<>"" &
	THEN	GOTO 2050 &
	ELSE	M$(23%)="ID" IF M$(23%)="" &
\		IF STATE$<>M$(23%) &
		THEN	S$="" &
\			S$(1%)=M$(23%) IF S$(1%)="" &
\			S$=S1$(I%) IF M$(23%)=S$(I%) AND S1$(I%)<>"" &
				FOR I%=1% TO S%(0%) &

2040	STATE$=M$(23%) &
\	M(I%)=CVT$F(M2$(I%)) FOR I%=10% TO 36% &
\	F%=F%+1% IF M$(1%)<>"ZPAYRL" &
\	GOSUB 6000 IF F%=42% &
\	GOSUB 4000 IF M$(1%)<>"ZPAYRL" &

2050	GOTO 2030 IF FNN%(2%)=0% &
\	F%=1%-(F%-F%/2%*2%) &
\	PRINT #P%, STRING$(33%*F%,10%); IF PRINT.COUNT% &
\	GOSUB 6000 IF PRINT.COUNT% &
\	GOSUB 7000 IF PRINT.COUNT% &
\	PRINT #P%, STRING$(33%*1%,10%); IF PRINT.COUNT% &

2060	PRINT STRING$(5%,7%); &
\	INPUT LINE #10%,A$ IF DEV$="" &
\	V$=SYS(CHR$(2%)) &
\	CLOSE P% &
\	KILL F$ IF RIGHT(F$,3%)="DAI" &
\	V%=FNX%("",0%,"") &

3000	! &
	!  POSITION CHECKS IN PRINTER &
	! &

3015	OPEN "KB:" AS FILE 10% &
\	PRINT "POSITION # SIGN ON EDGE OF FIRST W-2 FORM." &
\	PRINT "PRESS 'P' AND RETURN TO PRINT W-2 FORMS." &
\	V$=SYS(CHR$(3%)) &

3020	INPUT LINE #10%,A$ &
\	A$=CVT$$(A$,4%) &
\	RETURN IF A$="P" &
\	PRINT "#";CHR$(8%); &
\	GOTO 3020 &

4000	! &
	!  PRINT INFORMATION ON W-2 FORM &
	! &
	!	T9		Allocated Tips &
	!	M(16)		Federal Income &
	!	M(10)+S		Federal Wages &
	!	M(21)		SS Tax &
	!	FICA.WAGE	SS Wage &
	!	TIPS		SS Tips &
	!	M(26)		State Tax &
	!	M(10)+s		State Wage &

4010	IF M(10%)=0. &
	THEN	F%=F%-1% &
\		RETURN &

4015	T9=0. &
\	GOTO 4020 UNLESS ALLT% &
\	GOTO 4020 IF FNG%(4%,M$(1%)) &

4016	IF LEFT(FNL$,6%)=M$(1%) &
	THEN	LSET T4$=FNL$ &
\		T9=T9+CVT$F(T$(3%)) &
\		GOTO 4016 UNLESS FNN%(4%) &

4020	PRINT.COUNT%=PRINT.COUNT%+1% &
\	FICA.WAGE=M(10%) &
\	FICA.WAGE=FICA.LIMIT IF FICA.WAGE>FICA.LIMIT &
\	HI.WAGE=M(10%) &
\	HI.WAGE=HI.LIMIT IF HI.WAGE>HI.LIMIT &
\	FICA.TAX=FNZ(FICA.RATE * FICA.WAGE) &
\	HI.TAX=M(21%)-FICA.TAX &
\	T(1%)=T(1%)+M(16%) &
\	S,TIPS=0. &
\	S=M(15%) IF G$="Y" OR G$="TIPS" &
\	TIPS=M(15%) IF G$="TIPS" &
\	T(2%)=T(2%)+M(10%)+S &
\	T(3%)=T(3%)+FICA.TAX &
\	T(4%)=T(4%)+FICA.WAGE &
\	T(5%)=T(5%)+M(26%) &
\	T(6%)=T(6%)+TIPS &
\	T(7%)=T(7%)+HI.TAX &
\	T(8%)=T(8%)+HI.WAGE &
\	START%=-1% IF START$=M$(1%) &
\	RETURN UNLESS START% &
\	S0$="IDAHO" &
\	S0$=M$(23%) IF M$(23%)<>"ID" &
\	N$=M$(6%) &

4030	FORM.COUNT%=FORM.COUNT%+1% &
\	PRINT #P% &
\	PRINT #P% &
\	PRINT #P% &
\	IF C$="Y" &
	THEN	PRINT #P%, USING"\    \"+SPACE$(62%)+"\    \",M$(1%),M$(1%) &
	ELSE	PRINT #P% &
	! 1. CONTROL # &

4040	PRINT #P% &
\	PRINT #P%, USING"\"+SPACE$(28%)+"\            \\"+SPACE$(24%)+"\"+ &
		SPACE$(28%)+"\            \\",E1$(1%),P$,E1$(1%),P$ &
\	PRINT #P%, USING"\"+SPACE$(28%)+"\   "+SPACE$(35%)+"\"+SPACE$(28%)+ &
		"\   ",E1$(2%),E1$(2%) &
\	IF ALLT% &
	THEN	PRINT #P%, USING "\"+SPACE$(28%)+ &
			"\      #########.##                    \"+SPACE$(28%)+ &
			"\      #########.##",E1$(3%),T9,E1$(3%),T9 &
	ELSE	PRINT #P%, USING "\"+SPACE$(28%)+ &
			"\                                      \"+ &
			SPACE$(28%)+"\                  ",E1$(3%),E1$(3%) &

4045	PRINT #P%, USING"\"+SPACE$(28%)+"\   "+SPACE$(35%)+"\"+SPACE$(28%)+ &
		"\   ",E1$(4%),E1$(4%) &
\	PRINT #P%, USING"\"+SPACE$(28%)+ &
		"\     ######.##      ######.##         \"+ &
		SPACE$(28%)+"\    #######.##     #######.##", &
		E1$(5%),M(16%),M(10%)+S, &
		E1$(5%),M(16%),M(10%)+S &

4046	ALLTIPS=ALLTIPS+T9 &
\	PRINT #P% &
\	PRINT #P%,USING "\         \     \              \"+ &
	"#########.##   #########.##         \         \ "+ &
	"    \              \#########.##   #########.##", &
	E1$(6%),S$,FICA.TAX,FICA.WAGE,E1$(6%),S$,FICA.TAX,FICA.WAGE &
\	PRINT #P% &
\	PRINT #P%,USING "\         \                    "+ &
	" #########.##   #########.##         \         \ "+ &
	"                    #########.##   #########.##", &
	N$,TIPS,hi.wage,N$,TIPS,hi.wage &
\	PRINT #P% &

4050	PRINT #P%,USING "\                            \ "+ &
	" #########.##   #########.##         \           "+ &
	"                 \  #########.##   #########.##", &
		M$(2%),hi.tax,0.0,M$(2%),hi.tax,0.0 &
\	PRINT #P%, USING "\"+SPACE$(28%)+"\   "+SPACE$(35%)+"\"+SPACE$(28%)+ &
		"\   ",M$(3%),M$(3%) &
\	PRINT #P%, USING"\"+SPACE$(28%)+"\   "+SPACE$(35%)+"\"+SPACE$(28%)+ &
		"\   ",M$(4%),M$(4%) &
\	PRINT #P%, USING"\"+SPACE$(28%)+ &
		"\                                      \"+SPACE$(28%)+"\", &
			M$(5%),M$(5%) &
\	PRINT #P% &
\	PRINT #P% &
\	PRINT #P% &
\	PRINT #P% &
\	PRINT #P% &
\	PRINT #P%, USING &
		"######.## #######.##   \      \       "+SPACE$(27%)+ &
		" #######.## #######.##   \      \", &
			M(26%),M(10%)+S,S0$,M(26%),M(10%)+S,S0$ &
\	PRINT #P% FOR I%=1% TO 8% &
\	RETURN &

6000	! &
	! PRINT SUMMARY ON EVERY 42'ND FORM &
	! &

6010	FORM.COUNT%=FORM.COUNT%+1% &
\	IF START% &
	THEN	PRINT #P% &
\		PRINT #P% &
\		PRINT #P% &
\		PRINT #P% &
\		PRINT #P% &
\		PRINT #P%, USING"\"+SPACE$(28%)+"\                        \\"+ &
			SPACE$(12%)+"\"+ &
			SPACE$(28%)+"\                        \\", &
			E1$(1%),"XX",E1$(1%),"XX" &
\		PRINT #P%, USING"\"+SPACE$(28%)+"\   "+SPACE$(35%)+"\"+SPACE$(28%)+ &
			"\   ",E1$(2%),E1$(2%) &
\		 PRINT #P%, USING "\"+SPACE$(28%)+ &
			"\      #########.##                    \"+SPACE$(28%)+ &
			"\      #########.##",E1$(3%),ALLTIPS,E1$(3%),ALLTIPS &
\		PRINT #P%, USING"\"+SPACE$(28%)+"\   "+SPACE$(35%)+"\"+SPACE$(28%)+ &
			"\   ",E1$(4%),E1$(4%) &
\		PRINT #P%, USING"\"+SPACE$(28%)+ &
			"\     ######.##      ######.##         \"+ &
			SPACE$(28%)+"\    #######.##     #######.##", &
			E1$(5%),T(1%),T(2%), &
			E1$(5%),T(1%),T(2%) &
\		PRINT #P% &
\		PRINT #P%,USING "\         \     \              \"+ &
		"#########.##   #########.##         \         \ "+ &
		"    \              \#########.##   #########.##", &
		E1$(6%),S$,T(3%),T(4%),E1$(6%),S$,T(3%),T(4%) &
\		PRINT #P% &
\		PRINT #P%,USING "\         \                    "+ &
		" #########.##   #########.##         \         \ "+ &
		"                    #########.##   #########.##", &
		"",T(6%),T(8%),"",T(6%),T(8%) &
\		PRINT #P% &
\		PRINT #P%,USING "\                            \ "+ &
		" #########.##   #########.##         \          "+ &
		"                  \  #########.##   #########.##", &
			"** SUBTOTAL OF      **",T(7%),0.0, &
			"** SUBTOTAL OF      **",T(7%),0.0 &
\		PRINT #P%, "** W2'S PRINTED     **" &
\		PRINT #P% &
\		PRINT #P% &
\		PRINT #P% &
\		PRINT #P% &
\		PRINT #P% &
\		PRINT #P% &
\		PRINT #P% &
\		PRINT #P%, USING &
			"######.## #######.##   \      \       "+SPACE$(27%)+ &
			" #######.## #######.##   \      \", &
			T(5%),T(2%),"",T(5%),T(2%),"" &
\		PRINT #P% FOR I%=1% TO 8% &

6020	F%=1% &
\	T1(I%)=T1(I%)+T(I%) FOR I%=1% TO 8% &
\	ALLTIPS.TOT=ALLTIPS.TOT+ALLTIPS &
\	T(I%)=0. FOR I%=1% TO 8% &
\	ALLTIPS=0. &
\	RETURN &

7000	! &
	! PRINT GRAND TOTAL AT BOTTOM &
	! &

7010	PRINT #P% &
\	PRINT #P% &
\	PRINT #P% &
\	PRINT #P% &
\	PRINT #P% &
\	PRINT #P%, USING"\"+SPACE$(28%)+"\                        \\"+ &
		SPACE$(12%)+"\"+ &
		SPACE$(28%)+"\                        \\", &
		E1$(1%),"XX",E1$(1%),"XX" &
\	PRINT #P%, USING"\"+SPACE$(28%)+"\   "+SPACE$(35%)+"\"+SPACE$(28%)+ &
		"\   ",E1$(2%),E1$(2%) &
\	 PRINT #P%, USING "\"+SPACE$(28%)+ &
		"\      #########.##                    \"+SPACE$(28%)+ &
		"\      #########.##",E1$(3%),ALLTIPS.TOT,E1$(3%),ALLTIPS.TOT &
\	PRINT #P%, USING"\"+SPACE$(28%)+"\   "+SPACE$(35%)+"\"+SPACE$(28%)+ &
		"\   ",E1$(4%),E1$(4%) &
\	PRINT #P%, USING"\"+SPACE$(28%)+ &
		"\     ######.##      ######.##         \"+ &
		SPACE$(28%)+"\    #######.##     #######.##", &
		E1$(5%),T1(1%),T1(2%), &
		E1$(5%),T1(1%),T1(2%) &
\	PRINT #P% &
\	PRINT #P%,USING "\         \     \              \"+ &
	"#########.##   #########.##         \         \ "+ &
	"    \              \#########.##   #########.##", &
	E1$(6%),S$,T1(3%),T1(4%),E1$(6%),S$,T1(3%),T1(4%) &
\	PRINT #P% &
\	PRINT #P%,USING "\         \                    "+ &
	" #########.##   #########.##         \         \ "+ &
	"                    #########.##   #########.##", &
	"",T1(6%),T1(8%),"",T1(6%),T1(8%) &
\	PRINT #P% &
\	PRINT #P%,USING "\         \                    "+ &
	" #########.##   #########.##         \         \ "+ &
	"                    #########.##   #########.##", &
	"** THIS W2 FORM IS A    **",T1(7%),0.0, &
	"** THIS W2 FORM IS A    **",T1(7%),0.0 &
\	PRINT #P%, "** GRAND TOTAL OF ALL   **" &
\	PRINT #P%, "** FORMS PRINTED        **" &
\	PRINT #P%, USING "   TOTAL OF ### FORMS", FORM.COUNT% + 1% &
\	PRINT #P% &
\	PRINT #P% &
\	PRINT #P% &
\	PRINT #P% &
\	PRINT #P% &
\	PRINT #P%, USING &
		"######.## #######.##   \      \       "+SPACE$(27%)+ &
		" #######.## #######.##   \      \", &
		T1(5%),T1(2%),"",T1(5%),T1(2%),"" &
\	PRINT #P% FOR I%=1% TO 8% &
\	RETURN &

9000	! &
	!  CHECK FOR VALID SSN &
	! &

9010	DEF FNV%(A$) = LEN(CVT$$(A$,140%))<>11% &

10000	! &
	!  TERMINATE PROGRAM &
	! &

10010	V%=FNX%("",0%,"") &

14700	!	********************************************************* &
	!		FNCOMP%(<input string>,<target string>) &
	!		Compare input string to target string. &
	!		 0% returned if input string is not in target string. &
	!		-1% returned if input string is in target string. &
	! &
	!		Written by Robert Peterson - June 1981 &
	!		Version 1 Edition 0 &
	!	********************************************************* &

14710	DEF FNCOMP%(Y$,Y2$) &
\		Y9% = 0% &
\		Y9% = -1% IF Y2$ = "*" &
\		Y2$ = Y2$ + "," &

14720		IF Y9%=0% &
		THEN	Y1$ = LEFT(Y2$,INSTR(1%,Y2$,",") - 1%) &
\			Y2$ = RIGHT(Y2$,LEN(Y1$) + 2%) &
\			Y1% = INSTR(1%,Y1$,"-") &
\			IF Y1% + INSTR(1%,Y1$,"?") = 0% &
			THEN	Y9% = (Y$ = Y1$) &
			ELSE	IF Y1% &
				THEN	Y9%= (LEFT(Y1$,Y1%- &
						1%)<=Y$) AND (Y$<=RIGHT(Y1$, &
						Y1%+1%)) &
				ELSE	CHANGE CVT$$( LEFT(Y$,30%),-1%) TO Y% &
\					CHANGE CVT$$( LEFT(Y1$,30%),- &
						1%) TO Y1% &
\					GOTO 14770 IF (Y%(Y3%)<>Y1%(Y3%))- &
						(Y1%(Y3%)=63%)FOR Y3%=1% TO Y1%(0%) &
\					Y9%=-1% &

14770		IF Y2$<>"" AND Y9%=0% &
		THEN	GOTO 14720 &
		ELSE	FNCOMP%=Y9% &
\		FNEND &

14800	!	************************************************************* &
	!		FNOUTPUT$(<channel #>) &
	!		Y%  = Channel # to print on &
	!		Y1% = Mode to open channel on &
	!		Y$  = Name of file or device &
	!		Y1$ = Error condition input &
	! &
	!		Written by Robert Peterson - June 1981 &
	!		Version 1 Edition 1 &
	!	************************************************************ &

14810	DEF FNOUTPUT$(Y%) &
\		Y1%=128% &

14820		ON ERROR GOTO 14840 &
\		PRINT "Enter the device or file for output (cr for keyboard) "; &
\		INPUT LINE Y$ &
\		Y$=CVT$$(Y$,4%) &
\		IF Y$<>"KB:" AND Y$<>"" &
		THEN	OPEN Y$ FOR OUTPUT AS FILE Y% MODE Y1% &

14830		GOSUB 21999 &
\		Y$="" IF Y$="KB:" &
\		FNOUTPUT$=Y$ &
\	FNEND &

14840	IF INSTR(1%,Y$,".")<>0% &
	THEN	IF ERR=16% &
		THEN	PRINT "That file presently exists." &
\			PRINT "Enter a '0' to supersede or a '2' to add to file "; &
\			INPUT LINE Y1$ &
\			Y1$=CVT$$(LEFT(Y1$,1%),-1%) &
\			OPEN Y$ AS FILE Y% MODE 0% IF Y1$="0" &
\			OPEN Y$ AS FILE Y% MODE 2% IF Y1$="2" &
\			RESUME 14830 IF Y1$="0" OR Y1$="2" &

14850	PRINT "Error-";CVT$$(RIGHT(SYS(CHR$(6%) +CHR$(9%)+CHR$(ERR)),2%),4%); &
\	PRINT ".  Please try again." &
\	RESUME 14820 &

15900	DIM Y%(30%), Y1%(30%) &

32767 END
