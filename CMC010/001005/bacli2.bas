1	! &
	! Program to print out the backup listing in order to save &
	! reams of paper &
	! &
	! 07/20/87 - Kevin Handy &
	! &

50	! &
	! Array to store text in so can output in columns &
	! &
	DIM KEEP$(210%) &
\	ON ERROR GOTO 19000 &

100	! &
	! Open up the listing file &
	! &
	OPEN "BACKUP.LST" FOR INPUT AS FILE 5% &
	! &
	! Open the text file to write to
	! &
	OPEN "BACLIS.TXT" FOR OUTPUT AS FILE 10%
	! &
	! Start with the first volumn &
	! &
\	TEMP.VOL% = 1% &
	! &
	! Nothing in the array yet &
	! &
\	TEMP.COUNT% = 0% &

200	! &
	! Search for a file name &
	! &
	INPUT LINE #5%, TEXT$ &
\	TEXT$ = CVT$$(TEXT$, 4%) &
\	GOTO 200 UNLESS LEFT(TEXT$, 4%) = "File" AND &
		INSTR(1%, TEXT$, "written") &

210	! &
	! Split up file name and handle appropiately &
	! &
	TEMP% = INSTR(1%, TEXT$, "volume") + 6% &
\	VOL% = VAL(RIGHT(TEXT$, TEMP%)) &
\	GOSUB 2000 IF (VOL% <> TEMP.VOL%) OR (TEMP.COUNT% = 200%) &

220	! &
	! Store file name &
	! &
	TEMP.COUNT% = TEMP.COUNT% + 1% &
\	KEEP$(TEMP.COUNT%) = MID(TEXT$, 6%, 19%) &

290	! &
	! Loop back for more &
	! &
	GOTO 200 &

500	! &
	! Finish up &
	! &
	GOSUB 2000 &
\	CLOSE 5% &
\	PRINT #5%, CHR$(12%) &
\	GOTO 32767 &

2000	!******************************************************************* &
	! Handle a page &
	!******************************************************************* &

2010	! &
	! Print page title &
	! &
	PRINT #5%, CHR$(12%) &
\	PRINT #5%, &
\	PRINT #5% CHR$(155%); "[8w"; "Backup listing on "; DATE$(0%); &
		"   Volume"; TEMP.VOL%; &
		"     "; LEFT(KEEP$(1%), 9%); &
		"-"; LEFT(KEEP$(TEMP.COUNT%), 9%); &
		CHR$(155%);"[3w" &
\	PRINT #5% &

2020	! &
	! Print information &
	! &
	FOR I1% = 1% TO 50% &
\		FOR I2% = I1% TO 200% STEP 50% &
\			IF I2% <= TEMP.COUNT% &
			THEN &
				PRINT #5% KEEP$(I2%); "          "; &

2030		NEXT I2% &
\		PRINT #5% &
\	NEXT I1% &

2090	TEMP.VOL% = VOL% &
\	TEMP.COUNT% = 0% &
\	RETURN &

19000	RESUME 500 IF ERR = 11% &

32767	END &

