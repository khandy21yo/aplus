100	! &
	! Set the magtape mode variables (Non-file structured) &
	! &
	  D% = 256%	! 12% if 800 BPI, 256% if 1600 BPI &
	\ P% = 0%	! 0% if odd parity, 1% if even parity &
	\ S% = 0%	! 0% if not to retain MODE after close &
			! 8192% if  to retain MODE after close &
	\ DEV$ = "SY:[21,5]" &
	\ INPUT "Output to <KB:>"; OUT.DEV$ &
	\ OUT.DEV$ = "KB:" UNLESS LEN(OUT.DEV$) &
	\ OPEN OUT.DEV$ FOR OUTPUT AS FILE 2% &

200	! &
	! Let's open it up and see if we can get something done &
	! &
	  OPEN "MM0:" AS FILE 1%, RECORDSIZE 8192%, MODE D% + P% + S% &
	\ DUMMY% = MAGTAPE(3%, 0%, 1%) &
	\ ON ERROR GOTO 19000 &

300	! &
	! Read Volume Header information &
	! &
	  GET #1% &
	\ FIELD #1%, RECOUNT AS VOLUME$ &
	\ FIELD #1%, 3% AS VOL.LAB.ID$, 1% AS VOL.LAB.NUM$, 6% AS VOL.ID$, &
		  1% AS ACCESS$, 26% AS RESERVED.1$, 14% AS OWNER.ID$, &
		  28% AS RESERVED.2$, 1% AS LAB.VER$ &
	\ PRINT &
	\ PRINT #2%, "Volume information is as follows:" &
	\ PRINT &
	\ PRINT #2%, FNFORMAT$("Label identifier",	VOL.LAB.ID$) &
	\ PRINT #2%, FNFORMAT$("Label number",		VOL.LAB.NUM$) &
	\ PRINT #2%, FNFORMAT$("Volume identifier",	VOL.ID$) &
	\ PRINT #2%, FNFORMAT$("Accessibility",		ACCESS$) &
	\ PRINT #2%, FNFORMAT$("Reserved",		RESERVED.1$) &
	\ PRINT #2%, FNFORMAT$("Owner identifier",	OWNER.ID$) &
	\ PRINT #2%, FNFORMAT$("Reserved",		RESERVED.2$) &
	\ PRINT #2%, FNFORMAT$("Label Standard Version", LAB.VER$) &
	\ PRINT &

400	! &
	! Get Header 1 information &
	! &
	  GET #1% &
	\ FIELD #1%, RECOUNT AS HEADER.1$ &
	\ FIELD #1%, 3% AS HDR.1.LAB.ID$, 1% AS HDR.1.LAB.NUM$, &
		  17% AS FILE.ID$, 6% AS FILE.SET.ID$, 4% AS FILE.SECT.NUM$, &
		  4% AS FILE.SEQ.NUM$, 4% AS GEN.NUM$, 2% AS GEN.VER$, &
		  6% AS CREATE.DATE$, 6% AS EXPIRE.DATE$, 1% AS HDR.1.ACCESS$, &
		  6% AS BLK.CNT$, 13% AS SYS.CODE$, 7% AS RESERVED.3$ &
	\ PRINT #2%, STRING$(80%, ASCII("-")) &
	\ PRINT &
	\ PRINT #2%, "Header 1 information:" &
	\ PRINT &
	\ PRINT #2%, FNFORMAT$("Label Identifier",	HDR.1.LAB.ID$) &
	\ PRINT #2%, FNFORMAT$("Label Number",		HDR.1.LAB.NUM$) &
	\ PRINT #2%, FNFORMAT$("File Identifier",	FILE.ID$) &
	\ PRINT #2%, FNFORMAT$("File-set Identifier",	FILE.SET.ID$) &
	\ PRINT #2%, FNFORMAT$("File Section Number",	FILE.SECT.NUM$) &
	\ PRINT #2%, FNFORMAT$("File Sequence Number",	FILE.SEQ.NUM$) &
	\ PRINT #2%, FNFORMAT$("Generation Number",	GEN.NUM$) &
	\ PRINT #2%, FNFORMAT$("Generation Version",	GEN.VER$) &
	\ PRINT #2%, FNFORMAT$("Creation Date",		CREATE.DATE$) &
	\ PRINT #2%, FNFORMAT$("Expiration Date",	EXPIRE.DATE$) &
	\ PRINT #2%, FNFORMAT$("Accessibility",		HDR.1.ACCESS$) &
	\ PRINT #2%, FNFORMAT$("Block Count",		BLK.CNT$) &
	\ PRINT #2%, FNFORMAT$("System Code",		SYS.CODE$) &
	\ PRINT #2%, FNFORMAT$("Reserved",		RESERVED.3$) &
	\ PRINT &
	\ FILE.NAME$ = FILE.ID$ + "" &

500	! &
	! Read Header 2 information &
	! &
	  GET #1% &
	\ FIELD #1%, RECOUNT AS HEADER.2$ &
	\ FIELD #1%, 3% AS HDR.2.LAB.ID$, 1% AS HDR.2.LAB.NUM$, &
		  1% AS REC.FORMAT$, 5% AS BLK.LEN$, 5% AS REC.LEN$, &
		  21% AS RESERVED.4$, 1% AS CARRIAGE.CTRL$, &
		  13% AS RESERVED.5$, 2% AS BUFFER.OFFSET$, &
		  28% AS RESERVED.6$ &
	\ PRINT &
	\ PRINT #2%, "Header 2 information:" &
	\ PRINT &
	\ PRINT #2%, FNFORMAT$("Label Identifier",	HDR.2.LAB.ID$) &
	\ PRINT #2%, FNFORMAT$("Label Number",		HDR.2.LAB.NUM$) &
	\ PRINT #2%, FNFORMAT$("Record Format",		REC.FORMAT$) &
	\ PRINT #2%, FNFORMAT$("Block Length",		BLK.LEN$) &
	\ PRINT #2%, FNFORMAT$("Record Length",		REC.LEN$) &
	\ PRINT #2%, FNFORMAT$("Reserved?",		RESERVED.4$) &
	\ PRINT #2%, FNFORMAT$("Carriage Control",	CARRIAGE.CTRL$) &
	\ PRINT #2%, FNFORMAT$("Reserved?",		RESERVED.5$) &
	\ PRINT #2%, FNFORMAT$("Buffer offset",		BUFFER.OFFSET$) &
	\ PRINT #2%, FNFORMAT$("Reserved",		RESERVED.6$) &
	\ PRINT &

600	! &
	! Get a tape marker (I hope!) &
	! &
	  GET #1% &
	\ PRINT #2%, "Something is wrong - there should be a TAPE MARKER here!" &
	\ STOP &
	\ GOTO 32000 &

700	! &
	! Now process the file itself &
	! &
	  OPEN DEV$ + FILE.NAME$ FOR OUTPUT AS FILE 3% &

710	  DONE% = 0% &
	\ UNTIL DONE% &
		\ GET #1% &
		\ POINTER% = 1% &
		\ FIELD #1%, RECOUNT AS BLK$ &

720		  WHILE POINTER% < LEN(BLK$) &
			\ NEXT.LEN% = VAL(MID(BLK$, POINTER%, 4%)) &
			\ PRINT #3%, MID(BLK$, POINTER% + 4%, NEXT.LEN% - 4%) &
			\ POINTER% = POINTER% + NEXT.LEN% &
		\ NEXT &

730	  NEXT &
	\ CLOSE 3% &
	\ PRINT #2%, "******    "; &
	\ PRINT #2%, FILE.NAME$; "Copied to "; DEV$; FILE.NAME$; &
	\ PRINT #2%, "    ******" &

800	! &
	! Get End-of-file 1 information &
	! &
	  GET #1% &
	\ FIELD #1%, RECOUNT AS HDR.1$ &
	\ FIELD #1%, 3% AS HDR.1.LAB.ID$, 1% AS HDR.1.LAB.NUM$, &
		  17% AS FILE.ID$, 6% AS FILE.SET.ID$, 4% AS FILE.SECT.NUM$, &
		  4% AS FILE.SEQ.NUM$, 4% AS GEN.NUM$, 2% AS GEN.VER$, &
		  6% AS CREATE.DATE$, 6% AS EXPIRE.DATE$, 1% AS HDR.1.ACCESS$, &
		  6% AS BLK.CNT$, 13% AS SYS.CODE$, 7% AS RESERVED.3$ &
	\ PRINT &
	\ PRINT #2%, "End-of-File 1 information:" &
	\ PRINT &
	\ PRINT #2%, FNFORMAT$("Label Identifier",	HDR.1.LAB.ID$) &
	\ PRINT #2%, FNFORMAT$("Label Number",		HDR.1.LAB.NUM$) &
	\ PRINT #2%, FNFORMAT$("File Identifier",	FILE.ID$) &
	\ PRINT #2%, FNFORMAT$("File-set Identifier",	FILE.SET.ID$) &
	\ PRINT #2%, FNFORMAT$("File Section Number",	FILE.SECT.NUM$) &
	\ PRINT #2%, FNFORMAT$("File Sequence Number",	FILE.SEQ.NUM$) &
	\ PRINT #2%, FNFORMAT$("Generation Number",	GEN.NUM$) &
	\ PRINT #2%, FNFORMAT$("Generation Version",	GEN.VER$) &
	\ PRINT #2%, FNFORMAT$("Creation Date",		CREATE.DATE$) &
	\ PRINT #2%, FNFORMAT$("Expiration Date",	EXPIRE.DATE$) &
	\ PRINT #2%, FNFORMAT$("Accessibility",		HDR.1.ACCESS$) &
	\ PRINT #2%, FNFORMAT$("Block Count",		BLK.CNT$) &
	\ PRINT #2%, FNFORMAT$("System Code",		SYS.CODE$) &
	\ PRINT #2%, FNFORMAT$("Reserved",		RESERVED.3$) &
	\ PRINT &

900	! &
	! Read End-of-File 2 information &
	! &
	  GET #1% &
	\ FIELD #1%, RECOUNT AS HEADER.2$ &
	\ FIELD #1%, 3% AS HDR.2.LAB.ID$, 1% AS HDR.2.LAB.NUM$, &
		  1% AS REC.FORMAT$, 5% AS BLK.LEN$, 5% AS REC.LEN$, &
		  21% AS RESERVED.4$, 1% AS CARRIAGE.CTRL$, &
		  13% AS RESERVED.5$, 2% AS BUFFER.OFFSET$, &
		  28% AS RESERVED.6$ &
	\ PRINT &
	\ PRINT #2%, "End-of-File 2 information:" &
	\ PRINT &
	\ PRINT #2%, FNFORMAT$("Label Identifier",	HDR.2.LAB.ID$) &
	\ PRINT #2%, FNFORMAT$("Label Number",		HDR.2.LAB.NUM$) &
	\ PRINT #2%, FNFORMAT$("Record Format",		REC.FORMAT$) &
	\ PRINT #2%, FNFORMAT$("Block Length",		BLK.LEN$) &
	\ PRINT #2%, FNFORMAT$("Record Length",		REC.LEN$) &
	\ PRINT #2%, FNFORMAT$("Reserved?",		RESERVED.4$) &
	\ PRINT #2%, FNFORMAT$("Carriage Control",	CARRIAGE.CTRL$) &
	\ PRINT #2%, FNFORMAT$("Reserved?",		RESERVED.5$) &
	\ PRINT #2%, FNFORMAT$("Buffer offset",		BUFFER.OFFSET$) &
	\ PRINT #2%, FNFORMAT$("Reserved",		RESERVED.6$) &
	\ PRINT &

1000	! &
	! Get a tape marker (I hope!) &
	! &
	  GET #1% &
	\ PRINT #2%, "Something is wrong - there should be a TAPE MARKER here!" &
	\ STOP &
	\ GOTO 32000 &

19000	! &
	! Error processing.... &
	! &
	  IF ERR <> 11% &
	  THEN	  19200 &

19010	  IF ERL = 600% &
	  THEN	  RESUME 700 &

19020	  IF ERL = 400% &
	  THEN	  RESUME 32000 &

19030	  IF ERL = 710% &
	  THEN	  DONE% = -1% &
		\ RESUME 730 &

19040	  IF ERL = 1000% &
	  THEN	  RESUME 400% &

19200	  IF ERR = 52% AND ERL = 720% &
	  THEN	  PRINT &
		\ PRINT #2%, ">>>>>>>> ERROR READING BLOCK!!! <<<<<<<<" &
		\ PRINT #2%, "Length of this block was: "; LEN(BLK$) &
		\ PRINT #2%, "Pointer to bad portion:   "; POINTER% &
		\ PRINT #2%, BLK$ &
		\ PRINT &
		\ RESUME 730 &

19999	  ON ERROR GOTO 0 &

24000	! &
	! Functions &
	! &
	  DEF* FNFORMAT$(DESC$, VALUE$) &
		= LEFT(DESC$ + SPACE$(25% - LEN(DESC$)), 25%) &
		+ "    '" + VALUE$ + "'" &

32000	! &
	! Done processing - close up now &
	! &
	  CLOSE 1% &

32767	  END
