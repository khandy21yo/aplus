10	!!
	!!	System name - Payroll
	!!	Program name - New year
	!!	Program function - Transfer payroll records to new file
	!!		for all employess currently employed
	!!
	!!	Rewritten by Robert Peterson Jan 1982
	!!

35	  EXTEND

105	  PRINT "THIS PROGRAM SETS UP A PAYROLL MASTER FILE FOR THE NEW YEAR."
	\ INPUT "DO YOU WISH TO CONTINUE (Y OR N)";D$
	\ IF CVT$$(LEFT(D$,1%),-1%)<>"Y"
	  THEN	  V%=FNX%("",0%,"")

110	  PRINT 'TYPE "A" TO TRANSFER ALL EMPLOYEES TO THE NEW YEAR'
	\ PRINT '     "B" TO TRANSFER ALL EMPLOYEES WITHOUT A TERMINATION DATE'
	\ PRINT '     "4" TO TRANSFER ALL WHO WORKED IN THE 4TH QTR'
	\ PRINT '     "I" TO EXAMINE EACH EMPLOYEE AND SELECTIVELY TRANSFER';
	\ PRINT
	\ D$=""
	\ INPUT "Selection  ";D$ UNTIL D$="A" OR D$="4" OR D$="I" OR D$="B"

500	!!	************************************************************
	!!	** Rename master file and build new master file           **
	!!	************************************************************

510	  I%=-1%
	\ I%=0% IF MID(DATE$(0%),4%,3%)="Dec"
	\ YEAR$=NUM1$(VAL(RIGHT(DATE$(0%),8%))+I%)
	\ YEAR1$=RIGHT(NUM1$(VAL(YEAR$)+101%),2%)
	\ OLD.FILE$="MSTR"+YEAR$+".DAT"
	\ NEW.FILE$="MSTRFL.DAT"

520	  ON ERROR GOTO 19000
	\ NAME NEW.FILE$ AS OLD.FILE$
	\ NAME LEFT(NEW.FILE$,LEN(NEW.FILE$)-1%)+"1" AS LEFT(OLD.FILE$,
		  LEN(OLD.FILE$)-1%)+"1"
	\ GOSUB 21999

540	  IF FNO%(2%,OLD.FILE$,"/RW","")
	  THEN	  PRINT "Error";FNS%;"while opening ";OLD.FILE$;
			  "  Aborting. . . ";FNX%("",0%,"")

550	  IF FNO%(4%,NEW.FILE$,"/CR:8,512","")
	  THEN	  PRINT "Error";FNS%;"while opening ";NEW.FILE$;
			  "  Aborting. . . ";FNX%("",0%,"")

1000	!!	**********************************************************
	!!	**  PROGRAM CONTROL SECTION                             **
	!!	**********************************************************

1020	  GOTO 5000 IF D$="4" OR D$="A" OR D$="B"
	\ GOTO 4000 IF D$="I"
	\ PRINT "Unable to determine routine to execute.  Aborting. . . ";
		  FNX%("",0%,"")

4000	!!	************************************************************
	!!	**  SELECTIVELY TRANSFER EMPLOYEES                        **
	!!	************************************************************

4010	  IF FNG%(2%,"")
	  THEN	  PRINT "The master file is empty.  Aborting. . .";FNX%("",0%,
			  "")

4020	  FIELD #3%,FNL% AS E$, 6% AS T$, 30% AS T2$, 232% AS T3$,216% AS T4$,
		  2% AS T5$, 16% AS T6$, 10% AS T7$
	\ K$=""
	\ IF T$<>"ZPAYRL"
	  THEN	  X%=X%+1%
		\ PRINT T$;" ";T2$;RIGHT(T3$,225%);" Transfer <Y> ";
		\ INPUT LINE K$
		\ K$=CVT$$(LEFT(K$,1%),-1%)
		\ X1%=X1%+1% IF K$<>"N"

4030	  LSET T3$=YEAR1$+RIGHT(T3$,3%) IF T$="ZPAYRL"
	\ IF K$<>"N"
	  THEN	  IF FNA%(4%,T$+T2$+T3$+STRING$(216%,0%)+T5$+STRING$(16%,0%)+T7$)
		  THEN	  PRINT "Unable to add record to file.  Aborting. . . ";
				  FNX%("",-1%,"")

4040	  IF FNN%(2%)=0%
	  THEN	  4020
	  ELSE	  PRINT
		\ PRINT X%;"Total employees"
		\ PRINT X1%;"Employees transferred"
		\ PRINT "You're now ready to start processing data for the new year."
		\ PRINT "The qtr to date and year to date totals have been set to zero."
		\ PRINT
		\ V%=FNX%("",0%,"")

5000	!!	*************************************************************
	!!	**  Transfer all records in the master file                **
	!!	**  Transfer all who worked in the fourth quarter          **
	!!	**  Transfer all without a termination date                **
	!!	*************************************************************

5010	  IF FNG%(2%,"")=0%
	  THEN	  PRINT "Processing. . . ";
	  ELSE	  PRINT "Master file is empty.  Aborting. . . ";FNX%("",0%,"")

5020	  FIELD #3%, FNL% AS E$, 512% AS T$
	\ X%=X%+1% IF LEFT(T$,6%)<>"ZPAYRL"
	\ IF D$="A" OR D$="4" AND CVT$F(MID(T$,301%,8%))<>0. OR
		  D$="B" AND MID(T$,261%,8%)="" OR LEFT(T$,6%)="ZPAYRL"
	  THEN	  X1%=X1%+1% IF LEFT(T$,6%)<>"ZPAYRL"
		\ LSET T$=LEFT(T$,36%)+YEAR1$+RIGHT(T$,39%) IF LEFT(T$,6%)="ZPAYRL"
		\ IF FNA%(4%,LEFT(T$,268%)+STRING$(216%,0%)+MID(T$,485%,2%)+
			  STRING$(16%,0%)+RIGHT(T$,503%))
		  THEN	  PRINT "Unable to add record to file.  ";
				  "Aborting. . . ";FNX%("",-1%,"")

5030	  IF FNN%(2%)=0%
	  THEN	  5020
	  ELSE	  PRINT
		\ PRINT X%;"TOTAL EMPLOYEES"
		\ PRINT X1%;"Employees transferred."
		\ PRINT "You're now ready to start processing data for the new year."
		\ PRINT "The qtr to date and year to date totals have been set to zero."
		\ V%=FNX%("",0%,"")

10000	!!	**********************************************************
	!!	**  Program termination section                         **
	!!	**********************************************************

10010	  V%=FNX%("",0%,"")

19000	!!	***********************************************************
	!!	**  ERROR TRAPPING SECTION                               **
	!!	***********************************************************

19010	  IF ERL=520%
	  THEN	  PRINT "File already exists.  Aborting. . . ";V%=FNX%("",0%,
			  "")

19100	  PRINT "Error - ";CVT$$(RIGHT(SYS(CHR$(6%)+CHR$(9%)+CHR$(ERR)),2%),
		  4%);" at line ";ERL
	\ PRINT "Please call CMC before continuing. . . ";
	\ V$=SYS(CHR$(5%))
	\ STOP

21999	  ON ERROR GOTO 0

32767	  NO EXTEND
	\ END

