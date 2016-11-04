10	! &
	! WCREP -- TO PRINT WORKMANS COMP REPORT &
	! ??/??/?? -- WRITTEN BY WENDELL RICHARDSON &
	! 08/06/79 -- MODIFIED TO STANDARD SYSTEM BY KEVIN HANDY &
	! 02/16/81 -- MODIFIED BY MIKE FEINAUER ( PAGING CHANGES ) &
	!
11	  V9$="V1.0"
20 DIM P1$(29%),P$(9%),P%(17%), &
		P(5%), Y$(52%)
35	  EXTEND
36	  Y$=SYS(CHR$(12%)) &
	\ CHANGE Y$ TO Y% &
	\ PRGNUM$="["+NUM1$(Y%(6%))+","+NUM1$(Y%(5%))+"]" &
	\ PRGNAM$=RAD$(Y%(7%)+SWAP%(Y%(8%)))+RAD$(Y%(9%)+SWAP%(Y%(10%)))+ &
		  "."+RAD$(Y%(11%)+SWAP%(Y%(12%)))
120 Y$=FNX$ &
	\ IF Y$="" THEN Y$="FL" &
	\ INPUT "YEAR: (XX) HIT RETURN FOR CURRENT : ",Y1$ &
	: Y$=Y1$ IF Y1$<>""
130	  IF FNO%(2%,"MSTR"+Y$+".DAT","","") &
	  THEN	PRINT "MASTER FILE NOT FOUND !!!" : GOTO 10000
400	  !! &
	  !!	OPEN UNIQUE FILE FOR COMPANY NAME &
	  !!
410	  ON ERROR GOTO 420 &
	\ OPEN "SS0:UNIQUE.FIL/RO" FOR INPUT AS FILE 10% &
	\ DIM #10%, A0$(64%)=64% &
	\ CONAME$=A0$(1%) &
	\ GOTO 430
420	  PRINT "Please enter the company name "; &
	\ INPUT LINE CONAME$ &
	\ CONAME$=CVT$$(CONAME$,4%) &
	\ RESUME 430
430	  CLOSE 10% &
	\ GOSUB 21999
490	  U5$="\    \ \"+SPACE$(18%)+"\ \         \ #####.## \\" &
	\ U6$="#####.## ####.## #####.## #######.## #######.## #######.##"+ &
		  " #######.## #######.##"
1000 	! &
	! CONTROL SECTION &
	!
1020	  PRINT &
	\ INPUT "Selection ";K$ &
	\ K$=LEFT(CVT$$(K$,-1%),3%) &
	\ IF K$<>"" THEN &
	  IF K$="CRE" THEN 5000 ELSE &
	  IF K$="WOR" THEN 5100 ELSE &
	  IF K$="END" THEN V%=FNX%("",0%,"") ELSE &
	  	  PRINT "Type a (cr) for help." &
		\ GOTO 1020
1030 PRINT &
	: PRINT "OPTIONS : CREATE REPORT FILE" &
	: PRINT "          WORKMAN COMP REPORT" &
	: PRINT "          END REPORT" &
	: PRINT : GOTO 1020
5000	! &
	!  ROUTINE TO PRINT UNION HOUR REPORT &
	!
5010	  IF FNO%(9%,"TT0:PRWCR.TMP","/SF/CR:64","") THEN &
		  PRINT "Unable to open temporary sort file.  Aborting. . . " &
		\ V%=FNX%("",-1%,"")
5020	  PRINT "Please enter the payroll dates for this report:" &
	\ FOR I%=1% TO 52% &
		\ PRINT USING "Payroll !## - ","#",I%; &
		\ INPUT LINE Y$(I%) &
		\ Y$(I%)=CVT$$(Y$(I%),4%) &
		\ GOTO 5030 IF Y$(I%)="" &
	\ NEXT I%
5030	  Y%=0% &
	\ PRINT "This will take a little while.  Please be patient." &
	\ PRINT "Processing. . . "; &
	\ T=TIME(0%) &
	\ T%=TIME(1%) &
	\ R%=0%
5040	  Y%=Y%+1% &
	\ IF Y$(Y%)="" THEN PRINT &
		\ PRINT "Process completed." &
		\ PRINT "Elapse time ";TIME(0%)-T &
		\ PRINT "CPU time in seconds ";(TIME(1%)-T%)/10. &
		\ PRINT "Number of records processed ";R% &
		\ V%=FNX%(PRGNUM$+PRGNAM$,0%,Y$)
5045	  Z$=FND7$(Y$(Y%)) &
		\ Z$="PR"+LEFT(Z$,2%)+RIGHT(Z$,4%)+"1" &
		\ IF FNO%(5%,Z$,"/RO/SF/NS","") THEN PRINT &
		  "Unable to open the payroll dated ";Z$;"  Aborting. . . " &
		\ V%=FNX%("",-1%,"")
5050	  IF FNG%(5%,"") THEN PRINT Z$;" is empty.  Continuing. . . " &
		\ GOTO 5040
5060	  F%=FNL% &
	\ FIELD #5%, F% AS E$, 6% AS P1$(1%), 6% AS P1$(2%), 1% AS P1$(3%), &
		  1% AS P1$(4%), 2% AS P1$(5%), 2% AS P1$(6%), 2% AS P1$(7%), &
		  7% AS P1$(8%), 2% AS P1$(9%) &
	\ FIELD #5%, F%+19%+I% AS E$, 1% AS P1$(I%) FOR I%=10% TO 24% &
	\ FIELD #5%, F%+44% AS E$, 8% AS P1$(25%), 3% AS P1$(26%), &
		  3% AS P1$(27%), 3% AS P1$(28%), 3% AS P1$(29%) &
	\ GOTO 5080 IF P1$(1%)+P1$(2%)=STRING$(12%,ASCII("D")) &
	\ P$(I%)=P1$(I%)+"" FOR I%=1% TO 8% &
	\ P%(I%)=ASCII(P1$(I%+9%)) FOR I%=1% TO 15% &
	\ P(1%)=CVT$F(P1$(25%)) &
	\ P(I%)=FNN3(P1$(I%+24%)) FOR I%=2% TO 5% &
	\ P%(0%)=CVT$%(P1$(9%)) &
	\ P$(9%)=FND9$(P%(0%)) &
	\ REG,OT=0% &
	\ REG=REG+P%(I%) FOR I%=1% TO 7% &
	\ OT=OT+P%(I%) FOR I%=8% TO 14% &
	\ REG=FNN3(P1$(13%)+P1$(14%)+P1$(15%))*10. IF P%(7%)=255% &
	\ OT =FNN3(P1$(20%)+P1$(21%)+P1$(22%))*10. IF P%(7%)=255% &
	\ IF A$<>P$(1%) THEN V%=FNG%(2%,P$(1%)) &
		\ TRADE$=MID(FNL$,149%,12%) &
		\ STATE$=MID(FNL$,485%,2%) &
		\ IF V% THEN PRINT &
		  "Unable to find ";P$(1%);" in the master file.  Aborting." &
		\ V%=FNX%("",-1%,"")
5070	  R%=R%+1% &
	\ A$=P$(1%)+"" &
	\ P(2%)=P(5%) IF P$(7%)="SB" &
	\ P(5%)=0. IF P$(7%)="SB" &
	\ A1$=STATE$+TRADE$+P$(1%)+FNN3$(P(1%))+P$(4%)+ &
		  CVTF$(REG)+CVTF$(OT)+CVTF$(P(2%))+CVTF$(P(5%)) &
	\ IF FNA%(9%,A1$) THEN PRINT "Unable add record to file.  "; &
		  "Aborting. . . " &
		\ V%=FNX%("",-1%,"")
5080	  IF FNN%(5%)=0% THEN 5060 ELSE &
	  V%=FNC%(5%) &
	\ GOTO 5040
5100	! &
	! PRINT REPORT &
	!
5110	  IF FNO%(9%,"TT0:PRWCR.TMP","/SF/RW","") THEN PRINT &
		  "Unable to open the temporary work comp file.  Aborting." &
		\ V%=FNX%("",0%,"")
5120	  IF FNG%(9%,"") THEN PRINT &
		  "The temporary file is empty.  Aborting. . . " &
		\ V%=FNX%("",0%,"")
5130	  INPUT "Page length (11 or 8.5) ";K$ UNTIL K$="8.5" OR K$="11" &
	\ L%=VAL(K$)*6. &
	\ INPUT "Beginning payroll date ";D$ &
	\ INPUT "Ending payroll date ";D1$ &
	\ D$="Period "+D$+" to "+D1$ &
	\ PAGE.TOP$=STRING$(4%,10%)+ &
	  "DATE: "+DATE$(0%)+SPACE$(50%-LEN(CONAME$)/2%)+CONAME$+ &
	  SPACE$(56%-LEN(CONAME$)/2%)+"Page <<###>>"+CHR$(10%)+CHR$(13%)+ &
	  "TIME: "+TIME$(0%)+SPACE$(37%)+"WORKMENS COMPENSATION REPORT"+ &
	  SPACE$(35%)+"VERSION :"+V9$+CHR$(10%)+CHR$(13%)+ &
	  SPACE$(65%-LEN(D$)/2%)+D$+CHR$(10%)+CHR$(13%)+ &
	  SPACE$(57%)+"REG   O.T.   TOTAL <-----STANDARD------>"+SPACE$(17%)+ &
	  "OTHER      GROSS"+CHR$(10%)+CHR$(13%)+ &
	  "EMP #       NAME            SSN#            RATE CD    HOURS"+ &
	  "  HOURS   HOURS    REGULAR   OVERTIME    PREMIUM   EARNINGS"+ &
	  "   EARNINGS"+CHR$(10%)+CHR$(10%)+CHR$(13%) &
	\ PAGE.BOT$=STRING$(6%,10%)+CHR$(13%) &
	\ P%=12% &
	\ DEV$=FNOUTPUT$(P%) &
	\ P%=0% IF DEV$="" &
	\ T(I%),T1(I%)=0. FOR I%=1% TO 10% &
	\ A$,A1$,A2$="" &
	\ WC$="WORKMENS COMPENSATION CODE :" &
	\ INPUT "Set page (wide paper) ";K$ IF DEV$="" &
	\ PRINT "Working. . . "; IF DEV$<>"" &
	\ L1%=L%-9% &
	\ X1%=0% &
	\ X2%=1%
5160	  FIELD #9%, FNL% AS E$, 2% AS U$(1%), 12% AS U$(2%), 6% AS U$(3%), &
		  3% AS U$(4%), 1% AS U$(5%), 8% AS U$(6%), 8% AS U$(7%), &
		  8% AS U$(8%), 8% AS U$(9%) &
	\ F$(I%)=U$(I%)+"" FOR I%=1% TO 3% &
	\ F(4%)=FNN3(U$(4%)) &
	\ F$(5%)=U$(5%)+"" &
	\ F(I%)=CVT$F(U$(I%)) FOR I%=6% TO 9% &
	\ A$=U$(1%)+U$(2%)+U$(3%)+U$(4%)
5170	  IF A$<>A1$ AND A1$<>"" THEN V%=FNG%(2%,EMP$) &
		\ SSN$="" &
		\ EMP.NAME$="Undefined" &
		\ SSN$=MID(FNL$,127%,11%) IF V%=0% &
		\ EMP.NAME$=MID(FNL$,7%,20%) IF V%=0% &
		\ PRINT #P%, USING U5$+U6$,EMP$,EMP.NAME$,SSN$,RATE,CODE$, &
		  T(1%),T(2%),T(3%),T(4%),T(5%),T(6%),T(7%),T(8%) &
		\ T1(I%)=T1(I%)+T(I%) FOR I%=1% TO 10% &
		\ T(I%)=0. FOR I%=1% TO 10% &
		\ RETURN IF FLAG$="END" &
		\ X1%=X1%+1% &
		\ IF X1%>L1% THEN PRINT #P%, FNPAGE$(L%,X1%,FNPAGE%, &
		  PAGE.TOP$+"CONT> > > "+A2$+CHR$(10%)+CHR$(13%),PAGE.BOT$); &
		\ X1%=FNLINE%
5180	  GOSUB 5400 IF F$(1%)+F$(2%)<>A2$ &
	\ A1$=A$+"" &
	\ T(1%)=T(1%)+F(6%)/10. &
	\ T(2%)=T(2%)+F(7%)/10. &
	\ T(3%)=T(3%)+(F(6%)+F(7%))/10. &
	\ X=1. \ X=1.5 IF F$(5%)="H" \ X=2. IF F$(5%)="D" &
	\ T(4%)=T(4%)+FNZ((F(6%)/10.)*F(4%)) &
	\ TEMP=FNZ((F(7%)/10.)*X*F(4%)) &
	\ TEMP1=FNZ((F(7%)/10.)*F(4%)) &
	\ T(5%)=T(5%)+TEMP1 &
	\ T(6%)=T(6%)+(TEMP-TEMP1) &
	\ T(7%)=T(7%)+F(9%) &
	\ T(8%)=T(4%)+T(5%)+T(6%)+T(7%) &
	\ T(9%)=T(9%)+F(8%) &
	\ RATE=F(4%) &
	\ CODE$=F$(5%) &
	\ EMP$=F$(3%)
5190	  IF FNN%(9%)=0% THEN 5160 ELSE FLAG$="END" &
	\ A$="ZZZZZ" &
	\ GOSUB 5170 &
	\ GOSUB 5400 &
	\ PRINT #P%, FNPAGE$(L%,X1%,FNPAGE%,PAGE.TOP$+"GRAND TOTALS"+ &
		  CHR$(10%)+CHR$(10%)+CHR$(13%),""); &
	\ X1%=FNLINE% &
	\ T1(I%)=T2(I%) FOR I%=1% TO 10% &
	\ GOSUB 5400 &
	\ PRINT "Do you want to remove the temporary file <Y> "; &
	\ INPUT LINE K$ &
	\ KILL 'TT0:PRWCR.TMP' IF CVT$$(LEFT(K$,1%),-1%)<>"N" &
	\ PRINT "Temporary file has been removed." &
		  IF CVT$$(LEFT(K$,1%),-1%)<>"N" &
	\ V%=FNX%("",0%,"")
5400	  !! &
	  !!	PRINT NEW WORKMENS COMPENSATION CODE &
	  !!
5410	  IF A2$<>"" THEN &
	  PRINT #P%, FNPAGE$(L%,X1%,FNPAGE%,PAGE.TOP$+"CONT> > > "+ &
		  A2$+CHR$(10%)+CHR$(13%),PAGE.BOT$); &
		  IF X1%+13%>L1% &
	\ X1%=FNLINE% IF X1%+13%>L1% &
	\ PRINT #P% &
	\ PRINT #P%, USING "TOTAL REGULAR HOURS         #######.##",T1(1%) &
	\ PRINT #P%, USING "TOTAL OVERTIME HOURS        #######.##",T1(2%) &
	\ PRINT #P%, USING "STARDARD RT        ###,###,###.##",T1(4%) &
	\ PRINT #P%, USING "STARDARD OT        ###,###,###.##",T1(5%) &
	\ PRINT #P%, USING "                   --------------" &
	\ PRINT #P%, USING "TOTAL STANDARD     ###,###,###.##",T1(4%)+T1(5%) &
	\ PRINT #P%, USING "ADD: PREMIUM       ###,###,###.##",T1(6%) &
	\ PRINT #P%, USING "     OTHER EARN    ###,###,###.##",T1(7%) &
	\ PRINT #P%, USING "                   --------------" &
	\ PRINT #P%, USING "TOTAL EARNINGS     ###,###,###.##",T1(8%) &
	\ PRINT #P%, USING "                   ==============" &
	\ PRINT #P%, USING "SUBSISTENCE        ###,###,###.##",T1(9%) &
	\ PRINT #P%, FNPAGE$(L%,X1%+13%,FNPAGE%-1%,"",PAGE.BOT$); &
	\ X1%=FNLINE% &
	\ T2(I%)=T2(I%)+T1(I%) FOR I%=1% TO 10% &
	\ T1(I%)=0. FOR I%=1% TO 10% &
	\ RETURN IF FLAG$="END"
5420	  PRINT #P%, FNPAGE$(L%,X1%,FNPAGE%+X2%,PAGE.TOP$+WC$+F$(1%)+F$(2%)+ &
		  CHR$(10%)+CHR$(10%)+CHR$(13%),""); &
	\ X2%=0% &
	\ X1%=FNLINE% &
	\ A2$=F$(1%)+F$(2%) &
	\ RETURN
10000	! &
	!  TERMINATE PROGRAM &
	!
10010	  V%=FNX%("",0%,"")
14000	! &
	! STORE AND RETRIEVE DATE IN INTEGER (D9% <=> MM.DD.YY) &
	!
14010	DEF FND9%(D9$) \ D9$="0"+D9$ IF INSTR(1%,D9$,".")=2% &
	\ D9$=LEFT(D9$,3%)+"0"+RIGHT(D9$,4%) IF INSTR(4%,D9$,".")=5% &
	\ FND9%=VAL(LEFT(D9$,2%))+VAL(MID(D9$,4%,2%))*16%+ &
		FND8%(VAL(RIGHT(D9$,7%)))*512% &
	\ FNEND
14020	DEF FND9$(D9%) &
	\ FND9$=RIGHT(NUM1$((D9% AND 15%)+100%),2%)+ &
		"."+RIGHT(NUM1$((D9% AND 31%*16%)/16%+100%),2%)+ &
		"."+RIGHT(NUM1$(((SWAP%(D9%) AND 254%)/2%)+100%),2%) &
	\ FNEND
14030	DEF FND8%(D8)=D8
14200	! &
	! CHECK FOR VALID DATE &
	!
14210	DEF FND7%(D7$) : ON ERROR GOTO 14220 &
	: GOTO 14220 IF INSTR(1%,D7$,".")<>3% OR INSTR(4%,D7$,".")<>6% OR &
		INSTR(7%,D7$,".")<>0% OR LEN(D7$)<>8% &
	: D7%=VAL(LEFT(D7$,2%))   : GOTO 14220 IF D7%<1% OR D7%>12% &
	: D7%=VAL(MID(D7$,4%,2%)) : GOTO 14220 IF D7%<1% OR D7%>31% &
	: D7%=VAL(RIGHT(D7$,7%))  : GOTO 14220 IF D7%<0% &
	: FND7%=0% : GOTO 14230
14220	FND7%=-1% : RESUME 14230
14230	ON ERROR GOTO 0 : FNEND
14250	! &
	! FORMAT DATE TO MM.DD.YY , FILL WITH ZEROS &
	!
14260	DEF FND7$(D7$) : D7$=D7$+"."+RIGHT(DATE$(0%),8%) IF LEN(D7$)<6% &
	: D7$="0"+D7$ IF INSTR(1%,D7$,".")=2% &
	: D7$=LEFT(D7$,3%)+"0"+RIGHT(D7$,4%) IF INSTR(4%,D7$,".")=5% &
	: FND7$=D7$ : FNEND
14300	! &
	! FLOATING POINT NUMBER TO THREE CHARACTERS AND BACK &
	!
14310	DEF FNN3$(N3)=CVT%$(INT(N3))+CHR$((N3-INT(N3))*100.+.51)
14350	DEF FNN3(N3$)=CVT$%(LEFT(N3$,2%))+ASCII(MID(N3$,3%,1%))/100.
14500 DEF FNZ(Z)=INT(Z*100.+.51)/100.
14800	  !!	************************************************************* &
	  !!		FNOUTPUT$(<channel #>) &
	  !!		Y%  = Channel # to print on &
	  !!		Y1% = Mode to open channel on &
	  !!		Y$  = Name of file or device &
	  !!		Y1$ = Error condition input &
	  !! &
	  !!		Written by Robert Peterson - June 1981 &
	  !!		Version 1 Edition 1 &
	  !!	************************************************************
14810	  DEF FNOUTPUT$(Y%) &
	\ Y1%=128%
14820	  ON ERROR GOTO 14840 &
	\ PRINT "Enter the device or file for output (cr for keyboard) "; &
	\ INPUT LINE Y$ &
	\ Y$=CVT$$(Y$,4%) &
	\ IF Y$<>"KB:" AND Y$<>"" THEN  OPEN Y$ FOR OUTPUT AS FILE Y% MODE Y1%
14830	  GOSUB 21999 &
	\ Y$="" IF Y$="KB:" &
	\ FNOUTPUT$=Y$ &
	\ FNEND
14840	  IF INSTR(1%,Y$,".")<>0% THEN IF ERR=16% THEN &
		  PRINT "That file presently exists." &
		\ PRINT "Enter a '0' to supersede or a '2' to add to file "; &
		\ INPUT LINE Y1$ &
		\ Y1$=CVT$$(LEFT(Y1$,1%),-1%) &
		\ OPEN Y$ AS FILE Y% MODE 0% IF Y1$="0" &
		\ OPEN Y$ AS FILE Y% MODE 2% IF Y1$="2" &
		\ RESUME 14830 IF Y1$="0" OR Y1$="2"
14850	  PRINT "Error-";CVT$$(RIGHT(SYS(CHR$(6%) +CHR$(9%)+CHR$(ERR)),2%),4%); &
	\ PRINT ".  Please try again." &
	\ RESUME 14820
15200	  !!	************************************************************* &
	  !!	Print top and bottom of page &
	  !!	Format : &
	  !!	PRINT FNPAGE$(<LINES/PAGE>,<CURRENT LINE COUNT>,<PAGE COUNT>, &
			<TOP OF PAGE>,<BOTTOM OF PAGE>) &
	  !!	FNPAGE% = PAGE COUNT &
	  !!	FNLINE% = LINE COUNT &
	  !! &
	  !!	Written by Robert Peterson - July 1981 &
	  !!	Version 1 Edition 0 &
	  !!	***********************************************************
15210	  DEF FNPAGE$(Y0%,Y1%,Y2%,Y0$,Y1$) &
	\ Y2$="" &
	\ Y2$=STRING$(Y0%-(Y1%+ &
		  LEN(XLATE(Y1$,STRING$(10%,0%)+CHR$(10%)))),10%) IF Y1$<>"" &
	\ PAGE.LINE%=LEN(XLATE(Y0$,STRING$(10%,0%)+CHR$(10%))) &
	\ Y%=INSTR(1%,Y1$+Y0$,"<<#") &
	\ Y3%=INSTR(1%,Y1$+Y0$,"#>>") &
	\ Y$=RIGHT(NUM1$(100000+Y2%),8%-(Y3%-Y%)) &
	\ Y3%=-3% IF Y%=0% &
	\ FNPAGE$=Y2$+LEFT(Y1$+Y0$,Y%-1%)+Y$+RIGHT(Y1$+Y0$,Y3%+3%) &
	\ PAGE%=Y2%+1% &
	\ FNEND
15250	  DEF FNPAGE%=PAGE%
15260	  DEF FNLINE%=PAGE.LINE%
15900	  DIM Y%(32%),Y1%(32%)
21999	  ON ERROR GOTO 0 &
	\ RETURN
32767	  NO EXTEND &
	\ END
