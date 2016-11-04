10	  EXTEND &
	&
	  ! Program name: [1,8]PRUNUP		Compiled with SCALE 0 on V08.0 &
	  ! Decompiled on 20-Jan-93 at 03:28 PM by UNBAC Version 7.1
20	  !
30	  ON ERROR GOTO 19000
40	  OPEN "kb:" AS FILE 1% &
	\ DIM DATES$(100%) &
	\ OPEN "nl:" AS FILE 8%, RECORDSIZE 64%+128% &
	\ FIELD #8%, 43% AS PR$,1% AS PRFLAG$ &
	\ FIELD #8%, 64% AS E$,125% AS TX$,1% AS TXFLAG$ &
	\ FIELD #8%, 64% AS T3$,128% AS T5$
50	  PRINT "CMC - Payroll un-update. V1.0" &
	\ PRINT
60	  PRINT "Account specification> "; &
	\ INPUT LINE #1%, FILE.SPEC$ &
	\ FILE.SPEC$=CVT$$(FILE.SPEC$,4%) &
	\ IF FILE.SPEC$="" THEN &
		  PRINT "The current account will be used." &
	  ELSE &
		  IF FILE.SPEC$="?" THEN &
			  PRINT "Enter the account specification as:";"[p,pn]" &
			\ GOTO 60
70	  PRINT "To enter the payroll dates to un-update, type them as" &
	\ PRINT "MM.DD.YY.  You may enter multiple dates, separated by" &
	\ PRINT "commas.  When you have entered all the dates, type END" &
	\ PRINT "on a separate line." &
	\ DAT%=0%
80	  PRINT "Date>"; &
	\ INPUT LINE #1%, LIN$ &
	\ LIN$=CVT$$(LIN$,164%) &
	\ GOTO 100 IF LIN$="END" &
	\ IF LIN$="" THEN &
		  PRINT &
		\ PRINT "[exit]" &
		\ GOTO 32767
90	  LIN$=LIN$+"," UNLESS RIGHT(LIN$,LEN(LIN$))=","
92	  V%=INSTR(1%,LIN$,",") &
	\ GOTO 80 UNLESS V% &
	\ DAT%=DAT%+1% &
	\ DATES$(DAT%)=LEFT(LIN$,V%-1%) &
	\ LIN$=RIGHT(LIN$,V%+1%) &
	\ GOTO 92
100	  PRINT &
	\ PRINT "Enter the numeric value that you want the flag set to> "; &
	\ INPUT #1%, NEWFLAG% &
	\ PRINT &
	\ PRINT #1%, "processing . . ."
110	  FOR LOOP%=1% TO DAT% &
		\ DATES$(LOOP%)=FND7$(DATES$(LOOP%)) &
		\ IF FND7%(DATES$(LOOP%)) THEN &
			  PRINT DATES$(LOOP%);" - bad date - ignoring . . ." &
			\ GOTO 200
120		  PR.DATE$=FILE.SPEC$+"PR"+LEFT(DATES$(LOOP%),2%)+MID(DATES$( &
			LOOP%),4%,2%)+"."+MID(DATES$(LOOP%),7%,2%)+"T" &
		\ TX.DATE$=FILE.SPEC$+"TX"+LEFT(DATES$(LOOP%),2%)+MID(DATES$( &
			LOOP%),4%,2%)+"."+MID(DATES$(LOOP%),7%,2%)+"T" &
		\ PRINT "Processing date: ";DATES$(LOOP%); &
		\ V%=FNC%(2%)+FNC%(4%) &
		\ IF FNO%(2%,PR.DATE$,"/RW","")=0% THEN &
			  PRINT " <> ";PR.DATE$; &
		  ELSE &
			  PRINT &
			\ PRINT "Can't open ";PR.DATE$;" - skipping to next"; &
				" date . . ." &
			\ GOTO 200
130		  IF FNO%(4%,TX.DATE$,"","")=0% THEN &
			  PRINT " <> ";TX.DATE$ &
		  ELSE &
			  PRINT &
			\ PRINT "Can't open ";TX.DATE$;" - skipping to next"; &
				" date . . ." &
			\ GOTO 200
140		  IF FNG%(2%,"") THEN &
			  PRINT "Access error on ";PR.DATE$ &
			\ INPUT #1%, "Press RETURN to skip to next date> ";V$ &
			\ GOTO 200
150		  LSET T3$=FNL$ &
		\ LSET PRFLAG$=CHR$(NEWFLAG%) &
		\ IF FNU%(2%,T3$) THEN &
			  PRINT &
			\ PRINT "Update error!" &
			\ INPUT #1%, "Press RETURN to skip to next date> ";V$ &
			\ GOTO 200
155		  PRINT "."; &
		\ PRINT  IF POS(0%)>70% &
		\ GOTO 150 UNLESS FNN%(2%) &
		\ PRINT &
		\ PRINT STRING$(79%,ASCII("-"))
160		  IF FNG%(4%,"") THEN &
			  PRINT "Access error on ";TX.DATE$ &
			\ INPUT #1%, "Press RETURN to skip to next date> ";V$ &
			\ GOTO 200
162		  LSET T5$=FNL$ &
		\ LSET TXFLAG$=CHR$(NEWFLAG%) &
		\ IF FNU%(4%,T5$) THEN &
			  PRINT &
			\ PRINT "Update error!" &
			\ INPUT #1%, "Press RETURN to skip to next date> ";V$ &
			\ GOTO 200
165		  PRINT "."; &
		\ PRINT  IF POS(0%)>70% &
		\ GOTO 162 UNLESS FNN%(4%) &
		\ PRINT
170		  PRINT #1%, "Processing complete."
200	  NEXT LOOP%
10000	  V%=FNX%("",0%,"")
14200	  DEF FND7%(D7$) &
	\ ON ERROR GOTO 14220 &
	\ IF INSTR(1%,D7$,".")=3% AND INSTR(4%,D7$,".")=6% AND INSTR(7%,D7$, &
			".")=0% AND LEN(D7$)=8% THEN &
		  D7%=VAL(LEFT(D7$,2%)) &
		\ IF D7%>0% AND D7%<13% THEN &
			  D7%=VAL(MID(D7$,4%,2%)) &
			\ IF D7%>0% AND D7%<32% THEN &
				  D7%=VAL(RIGHT(D7$,7%)) &
				\ IF D7%>=0% THEN &
					  FND7%=0% &
					\ GOTO 14230
14220	  FND7%=-1% &
	\ RESUME 14230
14230	  ON ERROR GOTO 0 &
	\ FNEND
14250	  DEF FND7$(D7$) &
	\ D7$=D7$+"."+RIGHT(DATE$(0%),8%) IF LEN(D7$)<6% &
	\ D7$="0"+D7$ IF INSTR(1%,D7$,".")=2% &
	\ D7$=LEFT(D7$,3%)+"0"+RIGHT(D7$,4%) IF INSTR(4%,D7$,".")=5% &
	\ FND7$=D7$ &
	\ FNEND
32767	  END
