10	  ! &
	  ! Program name: prclen		Compiled with SCALE 0 on V07.0 &
	  ! Decompiled on 28-Nov-16 at 04:11 PM
15	  PRINT "THIS PROGRAM REMOVES PARTIAL PAYROLLS FOR SPECIFIED DATES." &
	\ PRINT "BE SURE YOU ARE DONE WITH A DATE'S PAYROLL BEFORE RUNNING!" &
	\ INPUT "CONFIRM CONTINUING (Y/N)";K$ &
	\ GOTO 10000 IF K$<>"Y"
20	  INPUT "MONTH # (EG. 01 FOR JAN) OR <RETURN> TO END";M$
30	  GOTO 10000 IF M$=""
40	  M$="0"+M$ IF LEN(M$)=1%
50	  PRINT M$;", DAY # OR <RETURN>"; &
	\ INPUT D$ &
	\ GOTO 20 IF D$="" &
	\ D$="0"+D$ IF LEN(D$)=1%
60	  PRINT M$;".";D$;", YEAR (YY) OR <RETURN>"; &
	\ INPUT Y$ &
	\ GOTO 50 IF Y$="" &
	\ Y$="0"+Y$ IF LEN(Y$)=1%
70	  D1$=M$+"."+D$+"."+Y$ &
	\ P$=M$+D$+"."+Y$ &
	\ IF Y$>"70" THEN  &
		  PRINT "DO NOT USE THIS PROGRAM TO DELETE"+" PAYROLLS WITH YEAR GREATER THAN '70." &
		\ GOTO 60
80	  PRINT TAB(5%);"VERIFY DELETING PAYROLL DATE ";D1$;" (Y/N) "; &
	\ INPUT K$
90	  IF K$<>"Y" THEN  &
		  PRINT "NOT DELETED" &
		\ GOTO 60
100	  ON ERROR GOTO 125 &
	\ KILL "PR"+P$+"T" &
	\ PRINT TAB(10%);"PR"+P$+"T DELETED." &
	\ GOTO 130
125	  PRINT TAB(10%);"FILE PR"+P$+"T DOES NOT EXIST." &
	\ RESUME 130
130	  ON ERROR GOTO 0 &
	\ ON ERROR GOTO 135 &
	\ KILL "PR"+P$+"1" &
	\ PRINT TAB(10%);"PR"+P$+"1 DELETED." &
	\ GOTO 140
135	  PRINT TAB(10%);"FILE PR"+P$+"1 DOES NOT EXIST." &
	\ RESUME 140
140	  ON ERROR GOTO 0 &
	\ ON ERROR GOTO 155 &
	\ KILL "TX"+P$+"T" &
	\ PRINT TAB(10%);"TX"+P$+"T DELETED." &
	\ GOTO 150
145	  PRINT TAB(10%);"FILE TX"+P$+"T DOES NOT EXIST." &
	\ RESUME 150
150	  ON ERROR GOTO 0 &
	\ ON ERROR GOTO 155 &
	\ KILL "TX"+P$+"1" &
	\ PRINT TAB(10%);"TX"+P$+"1 DELETED." &
	\ GOTO 160
155	  PRINT TAB(10%);"FILE TX"+P$+"1 DOES NOT EXIST." &
	\ RESUME 160
160	  ON ERROR GOTO 0 &
	\ GOTO 60
10000	  PRINT "THE END"
32767	  END
