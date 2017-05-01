1	%TITLE "Sub Process to Extract File Open"
	%SBTTL "TK_SUBR_EXTRACTOPEN"
	%IDENT "V3.6a Calico"

	SUB TK_SUBR_EXTRACTOPEN(SMG_OPN_CDD OPN, STRING FILE_NAME, &
		STRING OPEN_TYPE, STRING LOCATION)

	!
	! COPYRIGHT (C) 1987 BY
	! Computer Management Center
	! Idaho Falls, Idaho.
	!
	! This software is furnished under a license and may be used and
	! copied only in accordance with terms of such license and with
	! the inclusion of the above copyright notice.  This software or
	! any other copies therof may not be provided or otherwise made
	! available to any other person.  No title to and ownership of
	! the software is hereby transferred.
	!
	! The information in this software is subject to change without
	! notice and should not be construed as a commitment by
	! Computer Management Center, Inc.
	!
	! CMC assumes no responsibility for the use or reliability of
	! its software on equipment which is not supported by CMC.
	!
	!++
	!
	! ABSTRACT:HELP
	!	.p
	!	A process to aid in the documentation of file.
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_SUBR_EXTRACTOPEN/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP TK_SUBR_EXTRACTOPEN
	!	$ DELETE TK_SUBR_EXTRACTOPEN.OBJ;*
	!
	! AUTHOR:
	!
	!	05/01/87 - Robert Peterson
	!
	! MODIFICATION HISTORY:
	!
	!	07/18/88 - Kevin Handy
	!		Modified to handle changes in the open
	!		statement, and the _PUR type files.
	!
	!	08/05/91 - Kevin Handy
	!		Added ACCESS READ to open statements.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	05/28/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/13/99 - Kevin Handy
	!		Use BASIC$STARLET for LIB$
	!
	!	12/01/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	%INCLUDE "SOURCE:[SMG.OPEN]SMG_OPN.HB"

	!
	! Declarations
	!
	DECLARE LONG TEXT_FILE.CH

	!
	! Get channels from VMS
	!
	SMG_STATUS% = LIB$GET_LUN(TEXT_FILE.CH)
	GOTO ErrorGetCh IF SMG_STATUS% = LIB$_INSLUN
	GOTO 200

 ErrorGetCh:
	CALL ENTR_3MESSAGE(SCOPE, "No free channels from VMS:  " + NUM1$(SMG_STATUS%), 0%)
	GOTO ExitProgram

200	!
	! Tell them what's hap'n
	!
	CALL ENTR_3MESSAGE(SCOPE, "Extracting OPEN data from " + TRM$(FILE_NAME), 1%)

400	!
	! Get all open include file names
	!
	WHEN ERROR IN
		OPEN TRM$(LOCATION) + TRM$(FILE_NAME) + "." + OPEN_TYPE &
			FOR INPUT AS FILE #TEXT_FILE.CH, &
			ACCESS READ, ALLOW MODIFY
	USE
		CONTINUE 500
	END WHEN

	KEYS% = 0%
	OPN::FILE_NAME = ""

410	!
	! Read in one entire line from the file
	!
	CURLINE$ = ""

415	WHEN ERROR IN
		LINPUT #TEXT_FILE.CH, TEMP$
	USE
		CONTINUE 420
	END WHEN

	TEMP$ = EDIT$(TEMP$, 4% + 8% + 16% + 32% + 128% + 256%)

	TEMP% = INSTR(1%, TEMP$, "!")
	TEMP$ = LEFT(CURLINE$, TEMP% - 1%) IF TEMP% <> 0%

	TEMP% = INSTR(1%, TEMP$, "&")
	IF TEMP% <> 0%
	THEN
		CURLINE$ = CURLINE$ + LEFT(TEMP$, TEMP% - 1%)
		GOTO 415
	ELSE
		CURLINE$ = CURLINE$ + TEMP$
	END IF

	!
	! If it is a file name statement, then pull off the file name
	!
	TEMP% = INSTR(1%, CURLINE$, ".NAME$ =")
	IF TEMP%
	THEN
		TEMP$ = EDIT$(RIGHT(CURLINE$, TEMP% + 8%), 2%)

		TEMP$ = LEFT(TEMP$, LEN(TEMP$) - 5%) + '"' &
			IF (RIGHT(TEMP$, LEN(TEMP$) - 4%) = '_OLD"') OR &
			(RIGHT(TEMP$, LEN(TEMP$) - 4%) = '_PUR"')

		TEMP$ = LEFT(TEMP$, LEN(TEMP$) - 5%) + "'" &
			IF (RIGHT(TEMP$, LEN(TEMP$) - 4%) = "_OLD'") OR &
			(RIGHT(TEMP$, LEN(TEMP$) - 4%) = "_PUR'")

		OPN::FILE_NAME = TEMP$

		TEMP% = LEN(OPN::FILE_NAME)
		WHILE TEMP%
			IF MID(OPN::FILE_NAME, TEMP%, 1%) = "."
			THEN
				OPN::EXTENSION = RIGHT(OPN::FILE_NAME, TEMP% + 1%)
				WORK% = INSTR(1%, OPN::EXTENSION, "'")
				WORK% = INSTR(1%, OPN::EXTENSION, '"') IF WORK% = 0%
				OPN::EXTENSION = LEFT(OPN::EXTENSION, WORK% - 1%) IF WORK%
				TEMP% = 0%
			ELSE
				TEMP% = TEMP% - 1%
			END IF
		NEXT

		GOTO 410
	END IF

	!
	! Handle a open statement
	!
	GOTO 410 IF LEFT(CURLINE$, 4%) <> "OPEN"

420	TEMP$ = "ORGANIZATION"
	TEMP% = INSTR(1%, CURLINE$, "ORGANIZATION")
	IF TEMP%
	THEN
		TEMP1% = INSTR(TEMP%, CURLINE$, ",")
		TEMP$ = EDIT$(MID(CURLINE$, TEMP% + LEN(TEMP$), &
				TEMP1% - (TEMP% + LEN(TEMP$))), 8% + 128%)
		TEMP% = INSTR(1%, TEMP$, " ")
		TEMP% = LEN(TEMP$) + 1% IF TEMP% = 0%
		OPN::ORGNIZATION = LEFT(TEMP$, TEMP% - 1%)
		OPN::STRCTURE = EDIT$(RIGHT(TEMP$,TEMP% + 1%), 8% + 128%)
	END IF

	TEMP$ = "OPEN "
	TEMP% =INSTR(1%, CURLINE$, TEMP$)
	TEMP1$ = "AS FILE"
	TEMP1% = INSTR(TEMP% + 1%, CURLINE$, TEMP1$)
	WORK% = INSTR(TEMP%, CURLINE$, "FOR")
	TEMP1$ = "FOR" IF WORK% < TEMP1% AND WORK% <> 0%
	TEMP1% = WORK% IF WORK% < TEMP1% AND WORK% <> 0%
	IF TEMP% <> 0% AND TEMP% < TEMP1%
	THEN
		!
		! If not using the new way, handle it the old way
		!
		IF OPN::FILE_NAME = ""
		THEN
			OPN::FILE_NAME = MID(CURLINE$, TEMP% + LEN(TEMP$), &
				TEMP1% - (TEMP% + LEN(TEMP$)))
			TEMP% = LEN(OPN::FILE_NAME)
			WHILE TEMP%
				IF MID(OPN::FILE_NAME, TEMP%, 1%) = "."
				THEN
					OPN::EXTENSION = RIGHT(OPN::FILE_NAME, TEMP% + 1%)
					WORK% = INSTR(1%, OPN::EXTENSION, "'")
					WORK% = INSTR(1%, OPN::EXTENSION, '"') IF WORK% = 0%
					OPN::EXTENSION = LEFT(OPN::EXTENSION, WORK% - 1%) IF WORK%
					TEMP% = 0%
				ELSE
					TEMP% = TEMP% - 1%
				END IF
			NEXT
		END IF
	END IF

	TEMP$ = "PRIMARY KEY "
	TEMP% = INSTR(1%, CURLINE$, TEMP$)

	IF TEMP%
	THEN
		TEMP1% = INSTR(TEMP% + 1%, CURLINE$, ",")
		TEMP2% = INSTR(TEMP% + 1%, CURLINE$, "(")

		IF TEMP1% > TEMP2%
		THEN
			TEMP2% = INSTR(TEMP% + 1%, CURLINE$, ")")
			TEMP1% = INSTR(TEMP2% + 1%, CURLINE$, ",") IF TEMP2%
		END IF

		OPN::KEYS(KEYS%) = MID(CURLINE$, TEMP%, TEMP1% - TEMP%)
	END IF


	TEMP$ = "ALTERNATE KEY "
	TEMP% = INSTR(1%, CURLINE$, TEMP$)

450	WHILE TEMP%
		TEMP1% = INSTR(TEMP% + 1%, CURLINE$, ",")
		TEMP2% = INSTR(TEMP% + 1%, CURLINE$, "(")

		IF TEMP1% > TEMP2%
		THEN
			TEMP2% = INSTR(TEMP% + 1%, CURLINE$, ")")
			TEMP1% = INSTR(TEMP2% + 1%, CURLINE$, ",") IF TEMP2%
		END IF

		KEYS% = KEYS% + 1%
		OPN::KEYS(KEYS%) = MID(CURLINE$, TEMP%, TEMP1% - TEMP%)
		TEMP% = INSTR(TEMP% + LEN(TEMP$), CURLINE$, TEMP$)
	NEXT

500	!
	! Determine file and key sizes
	!
	OPN::KEYS_NUM = KEYS%
	CLOSE #TEXT_FILE.CH

 ExitProgram:

	CALL ENTR_3MESSAGE(SCOPE, "", 1%)

	SMG_STATUS% = LIB$FREE_LUN(TEXT_FILE.CH)

	EXIT SUB

32767	END SUB
