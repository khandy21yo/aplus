10	  ! &
	  ! Program name: [1,8]CONCBN		Compiled with SCALE 0 on V07.0 &
	  ! Decompiled on 17-Oct-85 at 11:03 AM by UNBAC Version 1
20	  PRINT "BE SURE TO DELETE ANY VOID CHECKS BEFORE YOU COMBINE"
1020	  INPUT "DATE(MM.DD.YY) FOR NEW FINAL FILE",I$ &
	\ GOTO 10000 IF I$="" &
	\ I$=FND7$(I$) &
	\ GOTO 1020 IF FND7%(I$) &
	\ B$="PR"+LEFT(I$,2%)+MID(I$,4%,2%)+"."+RIGHT(I$,7%)+"T" &
	\ B1$="TX"+RIGHT(B$,3%) &
	\ V9%=FNM%(B$,15%,64%) &
	\ GOTO 1030 IF V9%=0% &
	\ GOTO 15000 IF V9%<>1% &
	\ INPUT " DO YOU REALLY WANT TO COMBINE WITH THIS FILE (Y OR N)?";K$ &
	\ IF K$<>"Y" THEN &
		  PRINT "COMBINATION HAS BEEN ABORTED" &
		\ C%=-1% &
		\ GOTO 10000
1025	  GOTO 1040
1030	  GOTO 15000 IF FNM%(B1$,8%,128%)
1040	  FOR I%=1% TO 10%
1045		  PRINT "DATE(MM.DD.YY) FOR FILE #";I%; &
		\ INPUT F$(I%) &
		\ GOTO 1050 IF F$(I%)="" &
		\ F$(I%)=FND7$(F$(I%)) &
		\ GOTO 1045 IF FND7%(F$(I%)) &
		\ IF F$(I%)=I$ THEN &
			  PRINT "Don't combine the ";F$(I%); &
				" payroll file with itself!" &
			\ PRINT &
				"That would double the amounts for that payroll date!" &
				+STRING$(3%,7%) &
			\ PRINT &
			\ GOTO 1045
1047	  NEXT I% &
	\ STOP
1050	  GOTO 15000 IF FNO%(2%,B$,"/RW","") &
	\ H$="PR" &
	\ GOSUB 2000
1080	  STOP IF FNC%(2%)
1090	  H$="TX" &
	\ GOTO 15000 IF FNO%(2%,B1$,"/RW","") &
	\ GOSUB 2000
1100	  STOP IF FNC%(2%) &
	\ GOTO 10000
2000	  I9%=0% &
	\ FOR I%=1% TO 10% &
		\ RETURN IF F$(I%)="" &
		\ F$=H$+LEFT(F$(I%),2%)+MID(F$(I%),4%,2%)+"."+RIGHT(F$(I%),7%)+ &
			"T" &
		\ PRINT "STARTING ";F$ &
		\ GOTO 2020 IF FNO%(6%,F$,"U","R") &
		\ GOTO 2030 IF FNG%(6%,"") = 88% &
		\ GOTO 15000 IF FNS% &
		\ IF I9%=0% THEN &
			  I9%=-1% &
			\ B%=ASCII(MID(FNL$,44%,1%)) IF LEFT(F$,2%)="PR" &
			\ B%=ASCII(MID(FNL$,126%,1%)) IF LEFT(F$,2%)="TX"
2005		  IF LEFT(F$,2%)="PR" AND ASCII(MID(FNL$,44%,1%))<>B% OR LEFT( &
				F$,2%)="TX" AND ASCII(MID(FNL$,126%,1%))<> &
				B% THEN &
			  PRINT &
			\ PRINT "ERROR!" &
			\ PRINT &
				"THE SPECIFIED PAYROLLS DO NOT HAVE THE SAME UPDATE STATUS." &
			\ PRINT &
				"THIS COMBINATION HAS BEEN ABORTED.  REFER TO THE UPDATE FLAGS" &
			\ PRINT &
				"ON THE PAYROLL PRINTOUTS TO DETERMINE THE BEST SOLUTION." &
			\ C%=-1% &
			\ GOTO 10000
2010		  D$=FNL$ &
		\ GOTO 2040 IF FNA%(2%,D$) &
		\ F%=FNN%(6%) &
		\ GOTO 2010 IF F%=0% &
		\ GOTO 2050 IF F%<>11%
2013		  STOP IF FNC%(6%) &
		\ PRINT "ENDING   ";F$
2015	  NEXT I% &
	\ STOP
2020	  STOP IF FNS%<>5% &
	\ PRINT "CAN'T OPEN ";F$;".  "; &
	\ PRINT "FILE NONEXISTENT." &
	\ PRINT "AND WILL BE LEFT OUT...COMBINING WILL CONTINUE" &
	\ GOTO 2015
2030	  PRINT "THIS FILE IS EMPTY!" &
	\ GOTO 2013
2040	  STOP
2050	  STOP
10000	  !
10010	  V%=FNC%(2%) &
	\ V%=FNC%(6%) &
	\ CLOSE 1% &
	\ CLOSE 4% &
	\ CLOSE 12% &
	\ Q3$=CVT%$(8100%)+"!MENU.BAC"+SYS(CHR$(7%)) IF ASCII(SYS(CHR$(7%)))= &
		255% &
	\ V%=FNC%(0%) UNLESS C%
10020	  IF ASCII(SYS(CHR$(7%)))=255% THEN &
		  CHAIN "!MENU" 8100. &
	  ELSE &
		  GOTO 32767
13000	  DEF FNM%(Z$,Z1%,Z2%) &
	\ FNM%=0% &
	\ ON ERROR GOTO 13055 &
	\ GOSUB 13050 &
	\ GOTO 13080 IF Z3% &
	\ LSET Z2$=CVT%$(Z1%) &
	\ GOSUB 13060 &
	\ Z$=LEFT(Z$,LEN(Z$)-1%)+"1" &
	\ GOSUB 13050 &
	\ GOTO 15000 IF Z3% &
	\ LSET Z2$=CVT%$(Z2%) &
	\ GOSUB 13060 &
	\ GOTO 13080
13050	  OPEN Z$ FOR INPUT AS FILE 9% &
	\ GOTO 13070
13055	  STOP IF ERR<>5% &
	\ RESUME 13056
13056	  OPEN Z$ FOR OUTPUT AS FILE 9% &
	\ FIELD #9%, 2% AS Z1$,2% AS Z2$,1% AS Z3$,1% AS Z4$ &
	\ RETURN
13060	  LSET Z1$=CVT%$(0%) &
	\ LSET Z3$="S" &
	\ LSET Z4$=CHR$(128%) &
	\ PUT #9%, RECORD 1% &
	\ CLOSE 9% &
	\ RETURN
13070	  FNM%=1% &
	\ PRINT Z$;" EXISTS " &
	\ CLOSE 9% &
	\ Z3%=-1% &
	\ RETURN
13080	  ON ERROR GOTO 0 &
	\ FNEND
14210	  DEF FND7%(D7$) &
	\ ON ERROR GOTO 14220 &
	\ GOTO 14220 IF INSTR(1%,D7$,".")<>3% OR INSTR(4%,D7$,".")<>6% OR &
		INSTR(7%,D7$,".")<>0% OR LEN(D7$)<>8% &
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
14260	  DEF FND7$(D7$) &
	\ D7$=D7$+"."+RIGHT(DATE$(0%),8%) IF LEN(D7$)<6% &
	\ D7$="0"+D7$ IF INSTR(1%,D7$,".")=2% &
	\ D7$=LEFT(D7$,3%)+"0"+RIGHT(D7$,4%) IF INSTR(4%,D7$,".")=5% &
	\ FND7$=D7$ &
	\ FNEND
15000	  PRINT "PLEASE CALL CMC (525-3000) NOW BECAUSE A FILE ERROR" &
	\ PRINT "HAS OCCURED" &
	\ C%=-1% &
	\ GOTO 10000
32767	  END
