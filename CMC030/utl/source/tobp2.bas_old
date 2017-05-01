500	DIM FLINE$(500%), FFLAG%(500%) &

1000	INPUT "Source file name"; SNAME$ &
\	OPEN SNAME$ FOR INPUT AS FILE 2%, &
		ACCESS READ, ALLOW MODIFY

1100	INPUT "Output file name"; ONAME$ &
\	OPEN ONAME$ FOR OUTPUT AS FILE 3% &
\	MARGIN #3%, 132%

2000	! &
	! Prime the pump &
	! &
	ON ERROR GOTO 19000 &
\	INPUT LINE #2%, NEXTLINE$ &
\	NEXTLINE$ = CVT$$(NEXTLINE$, 4%) &

2050	! &
	! Initiate a new line number group &
	! &
	I% = 0% &
\	I% = I% + 1% UNTIL I% > LEN(NEXTLINE$) OR &
		INSTR(1%, "0123456789", MID(NEXTLINE$, I%+1%, 1%)) = 0% &
\	LINENO$ = LEFT(NEXTLINE$, I%) &
\	NEXTLINE$ = RIGHT(NEXTLINE$, I% + 1%) &
\	COUNTER% = 1% &

2100	! &
	! Flip to a new line &
	! &
	FLINE$(COUNTER%) = NEXTLINE$ &
\	FFLAG%(COUNTER%) = 0% &
\	COUNTER% = COUNTER% + 1% &
\	INPUT LINE #2%, NEXTLINE$ &
\	NEXTLINE$ = CVT$$(NEXTLINE$, 4%) &
\	GOTO 2100 UNLESS INSTR(1%, "0123456789", LEFT(NEXTLINE$, 1%)) &

2150	COUNTER% = COUNTER% - 1% &

2200	! &
	! Search for the front of all statements &
	! &
	FFLAG%(1%) = -1% &
\	FFLAG%(COUNTER% + 1%) = -1% &
\	FOR I% = 2% TO COUNTER% &
\		XLINE$ = CVT$$(FLINE$(I%), -1%) &
\		FFLAG%(I%) = -1% IF LEFT(XLINE$, 1%) = "\" &
\		FFLAG%(I%) = -1% IF LEFT(XLINE$, 4%) = "THEN" &
\		FFLAG%(I%) = -1% IF LEFT(XLINE$, 4%) = "ELSE" &
\		FFLAG%(I%+1%) = -1% IF XLINE$ = "THEN&" OR XLINE$ = "ELSE&" &
\	NEXT I% &

2210	! &
	! Go backwards and flag all comment lines in front of a start line &
	! as a start line. &
	! &
	FOR I% = COUNTER% TO 2% STEP -1% &
\		IF LEFT(CVT$$(FLINE$(I%), -1%), 1%) = "!" &
		THEN &
			FFLAG%(I%) = -1% IF FFLAG%(I% + 1%) &

2215		IF FLINE$(I%) = "" OR CVT$$(FLINE$(I%), -1%) = "&" &
		THEN &
			FFLAG%(I%) = -1% IF FFLAG%(I% + 1%) &

2220	NEXT I% &

2250	! &
	! Go forewards and pick off all the starting comments &
	! &
	I% = 1% &

2260	IF LEFT(CVT$$(FLINE$(I%), -1%), 1%) = "!" &
	THEN &
		FFLAG%(I%+1%) = -1% &
\		I% = I% + 1% &
\		GOTO 2260 UNLESS I% >= COUNTER% &

4000	! &
	! Write out lines &
	! &

4010	PRINT #3%, LINENO$; &
\	FOR I% = 1% TO COUNTER% &
 &
\		XLINE$ = FLINE$(I%) &

4020		! &
		! Strip off foreward slash if it starts the line &
		! &
		IF LEFT(CVT$$(XLINE$, -1%), 1%) = "\" &
		THEN &
			J% = INSTR(1%, XLINE$, "\") &
\			XLINE$ = LEFT(XLINE$, J% - 1%) + RIGHT(XLINE$, J% + 1%) &

4030		! &
		! If there is a ampersand on the end of the line &
		! &
		ILEN% = LEN(XLINE$) &
\		IF RIGHT(XLINE$, ILEN%) = "&" &
		THEN &
			! &
			! Strip off ampersand if next line is the start of &
			! a statement. &
			! &
			XLINE$ = LEFT(XLINE$, ILEN% - 1%) IF FFLAG%(I%+1%) &

4100		! &
		! Convert CVT$$( to EDIT$( &
		! &
		J% = 0% &

4110		J% = INSTR(J%+1%, XLINE$, "CVT$$") &
\		IF J% &
		THEN &
			GOTO 4110 IF FNQUOTE%(XLINE$, J%) &
\			XLINE$ = LEFT(XLINE$, J%-1%) + "EDIT" + &
				RIGHT(XLINE$, J%+4%) &
\			GOTO 4110 &

4200		! &
		! Convert CHR$(constant) TO 'constant'C &
		! &
		J% = 0% &

4210		J% = INSTR(J%+1%, XLINE$, "CHR$(") &
\		GOTO 4300 UNLESS J% &
\		GOTO 4210 IF FNQUOTE%(XLINE$, J%) &

4220		K% = INSTR(J%, XLINE$, ")") &
\		GOTO 4210 UNLESS K% &

4230		K1% = K% - 1% &
\		K1% = K1% - 1% IF MID(XLINE$, K1%, 1%) = "%" &
\		K3% = VAL(MID(XLINE$, J%+5%, K1%-J%-4%)) &
\		GOTO 4210 IF K3% < 0% OR K3% > 255% &
\		XLINE$ = LEFT(XLINE$, J%-1%) + "'" + &
			NUM1$(K3%) + "'C" + RIGHT(XLINE$, K%+1%) &
\		GOTO 4210 &

4300		! &
		! Add a space to anything starting a line that is not &
		! a line number &
		! &
		XLINE$ = " " + XLINE$ UNLESS CVT$$(LEFT(XLINE$, 1%), -1%) = "" &

4500		! &
		! Dump out the line &
		! &
		PRINT #3%, CVT$$(XLINE$, 128%) &

4600	NEXT I% &

4650	GOTO 2050 UNLESS ENDFLAG% &

5000	CLOSE 2%, 3% &
\	GOTO 32767 &

8000	DEF FNQUOTE%(TEST$, TEST%) &
\		Z1% = 1% &

8010		GOTO 8020 IF INSTR(1%, "'" + '"', MID(TEST$, Z1%, 1%)) &
			FOR Z1% = Z1%+1% TO TEST% &
\		FNQUOTE% = 0% &
\		GOTO 8090 &

8020		Z1% = INSTR(Z1%+1%, TEST$, MID(TEST$, Z1%, 1%)) &
\		GOTO 8010 IF Z1% < TEST% AND Z1% <> 0% &
\		FNQUOTE% = -1% &

8090	FNEND &

19000	IF ERR=11% &
	THEN &
		ENDFLAG% = -1% &
\		RESUME 2150 &

19010	RESUME 4210 IF ERL=4230% &

19999	ON ERROR GOTO 0 &

32767	END
