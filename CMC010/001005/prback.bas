10	  P0%=-1%
 	\ PRINT "START AT ACCOUNT # EXAMPLE: [1,8]";
 	\ INPUT LINE S$
 	\ S$=CVT$$(S$,2%+4%+64%)
 	\ GOTO 20 IF S$=""
 	\ I%=INSTR(1%,S$,"(")
 	\ I1%=INSTR(1%,S$,",")
 	\ I2%=INSTR(1%,S$,")")
 	\ IF I%<>1% OR (I1%<3% OR I1%>I2%-2%) OR I2%<>LEN(S$)
 	  THEN	  PRINT "INVALID ACCOUNT NUMBER"
 		\ GOTO 10
 
15	  S$="["+MID(S$,2%,LEN(S$)-2%)+"]"
 	\ P0%=0%
 
20	  PRINT "END AT ACCOUNT #";
 	\ INPUT LINE E$
 	\ E$=CVT$$(E$,2%+4%+64%)
 	\ GOTO 27 IF E$=""
 	\ I%=INSTR(1%,E$,"(")
 	\ I1%=INSTR(1%,E$,",")
 	\ I2%=INSTR(1%,E$,")")
 	\ IF I%<>1% OR (I1%<3% OR I1%>I2%-2%) OR I2%<>LEN(S$)
 	  THEN	  PRINT "INVALID ACCOUNT NUMBER"
 		\ GOTO 20
 
25	  E$="["+MID(E$,2%,LEN(E$)-2%)+"]"
 
27	  E$="[ZZZ,ZZZ]" IF E$=""
 	\ INPUT "SUMMARIZE? (Y OR N)";Y$
 	\ Y$=LEFT(CVT$$(Y$,-1%),1%)
30	  L1%=66%
40	  L%=1%
45	  PRINT "FILE NAME:";
 	\ INPUT LINE F$
 	\ F$=CVT$$(F$,-1%)
50	  INPUT "KB? EXAMPLE:  KB8: ",D$
 	\ IF D$="" THEN
 		  D$="KB:"
 
55	  IF D$="KB:"
 	  THEN	  INPUT "DETACH (Y OR N) <Y> ";K$
 		\ D$=D$+"BACKUP.LST" IF LEFT(CVT$$(K$,-1%),1%)<>"N"
 
60	  INPUT "SET PAGE ";K$ IF LEFT(D$,3%)="KB:"
200	  OPEN F$ AS FILE 1%
 	\ OPEN D$ AS FILE 2%
 	\ IF D$<>"KB:"
 	  THEN	  PRINT "DETACHING . . ."
 		\ V$=SYS(CHR$(6%)+CHR$(7%))
210	  GOSUB 3000
 	\ GOSUB 2000
 	\ CLOSE 1%
 	\ GOTO 9000
250	  IF ERR<>5% THEN 
 		  PRINT "ERROR IS ";ERR
 		\ STOP
260	  RESUME 270
270	  GOTO 45
2000	  !
2010	  RETURN IF L%=1%
2020	  PRINT #2%, STRING$(L1%-L%,10%)
2030	  P%=P%+1%
 	\ L%=1%
2040	  RETURN
3000	  !
3005	  ON ERROR GOTO 4000
 	\ P%=1%
 	\ P1%=0%
3010	  !
3020	  INPUT LINE #1%, A$
 	\ GOTO 3020 IF CVT$$(A$,-1%)=""
3030	  IF A%=0% THEN 
 		  A%=INSTR(1%,A$,"Copyright")
 		\ PRINT #2%, A$;
 		\ L%=L%+1%
 		\ GOTO 3010
3040	  X4%=INSTR(1%,A$,"Beginning")<>0%
 	\ IF X4%<>0%
 	  THEN	  X%=INSTR(1%,A$,"[")
 		\ X1%=INSTR(X%,A$,"]")
 		\ A1$=MID(A$,X%,X1%-X%+1%)
 		\ A1$=SPACE$(8%-LEN(A1$))+A1$
 
3045	  IF X4%<>0% AND P0%=0% THEN 
 		  X%=INSTR(1%,A$,S$)
 		\ P0%=-1% IF X%
 
3047	  GOTO 3010 UNLESS P0%
 
3050	  X3%=INSTR(1%,A$,"volume")
 	\ IF X3%<>0% AND X4%<>0% THEN 
 		  A2$=MID(A$,X3%+7%,1%)
 		\ GOTO 3010 IF Y$="N"
 		\ PRINT #2%, " ";A1$;A2$;
 		\ IF POS(0%)<70% THEN
 			  GOTO 3010
 		  ELSE
 			  PRINT #2%
 			\ L%=L%+1%
 			\ GOSUB 6000
3060	  IF MID(A$,28%,5%)="      " THEN 
 		  A$=LEFT(A$,27%)+RIGHT(A$,33%)
3080	  GOTO 3010 IF Y$="Y"
 	\ PRINT #2%, CVT$$(A$,4%);TAB(70%);A1$;A2$
 	\ L%=L%+1%
 	\ GOSUB 6000
3085	  X%=INSTR(1%,A$,"Total of")
 	\ IF X%
 	  THEN	  IF INSTR(1%,A$,E$)
 		  THEN	  GOSUB 2000
 			\ CLOSE 1%
 			\ GOTO 9000
 
3090	  GOTO 3010
4000	  IF ERR<>11% THEN 
 		  PRINT "ERROR: ";ERR
 		\ STOP
4010	  RESUME 4020
4020	  RETURN
6000	  RETURN IF L%<L1%-2%
6010	  GOSUB 2000
6020	  PRINT #2%, "PAGE ";P%
6030	  L%=L%+1%
6040	  RETURN
9000	  J%=ASCII(SYS(CHR$(6%)+CHR$(9%)+CHR$(0%)))/2%
9030	  A$=SYS(CHR$(6%)+CHR$(8%)+CHR$(J%)+STRING$(23%,0%)+CHR$(0%)+CHR$(255%))
 		  IF D$<>"KB:"
9040	  GOTO 32767
 
32700	  GOSUB 2000
32767	  END
