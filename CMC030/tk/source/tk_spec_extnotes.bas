1	%TITLE "Check to see that all fields are in source code"
	%SBTTL "TK_SPEC_EXTNOTES"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1987, 1988 BY
	!
	! Computer Management Center, Inc.
	! Idaho Falls, Idaho.
	!
	! This software is furnished under a license and may be used and
	! copied only in accordance with terms of such license and with
	! the inclusion of the above copyright notice.  This software or
	! any other copies thereof may not be provided or otherwise made
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
	! Abstract:
	!	^*Check to see that all fields are in source code\*
	!	.p
	!	Check to see that all fields are in source code
	!
	! Index:
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_SPEC_EXTNOTES/LINE
	!	$ LINK/EXECUTABLE=TK_EXE: TK_SPEC_EXTNOTES, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE TK_SPEC_EXTNOTES.OBJ;*
	!
	! Author:
	!
	!	01/01/87 - Kevin Handy
	!
	! Modification history:
	!
	!	08/05/91 - Kevin Handy
	!		Added ACCESS READ to open statements.
	!
	!	06/14/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	06/23/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/28/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	DIM	FILENAME$(1000%), &
		DIR_NAME$(100%)

	ON ERROR GOTO 19000

	! Handle output file
	!
	CALL ASSG_CHANNEL(WRIT_FILE.CH%, STAT%)
	CALL ASSG_CHANNEL(READ_FILE.CH%, STAT%)

	!
	! Get Report
	!
510	INPUT "Directory: "; PREFIX$
	INPUT "Report: "; FILE_NAME$

	GOTO 510 IF PREFIX$ = ""

	CALL FIND_FILE("SOURCE:[" + PREFIX$ + ".SOURCE]" + &
		TRM$(FILE_NAME$) + ".BAS", &
		FILENAME$(), 16%, "", "")

	I.LOOP% = VAL%(FILENAME$(0%))

	ID.FLAG% = 0%

	FOR J% = 1% TO I.LOOP%

	FILE_NAME$ = FILENAME$(J%)

		!Open the new file we are going to create
515		OPEN "SOURCE:[" + PREFIX$ + ".SOURCE]" + FILE_NAME$ + ".BAS" &
			FOR INPUT AS FILE READ_FILE.CH%, &
			ACCESS READ, ALLOW MODIFY

		OPEN  "PROG.TMP" AS FILE WRIT_FILE.CH%, &
			RECORDSIZE 132%, &
			ACCESS APPEND

		GOTO 516 IF ID.FLAG% = -1%

		PRINT #WRIT_FILE.CH%, ".enable bolding"
		PRINT #WRIT_FILE.CH%, ".enable indexing"
		PRINT #WRIT_FILE.CH%, ".flags bold"
		ID.FLAG% = -1%

516		LINPUT #READ_FILE.CH%, TEXT$

		ST% = INSTR(1%, TEXT$, '"') + 1%
		ST1% = INSTR(ST%, TEXT$, '"') - ST%
		TEXT$ = MID$(TEXT$, ST%, ST1%)

		PRINT #WRIT_FILE.CH%, "^*" + TEXT$ + "\*"
		PRINT #WRIT_FILE.CH%, ".P"

		LINPUT #READ_FILE.CH%, TEXT$
		ST% = 1%
517		ST% = INSTR(ST% + 1%, TEXT$, "_") + 1%
		GOTO 518 IF ST% = 1%

		TEXT$ = LEFT$(TEXT$, ST% - 1%) + "_" + RIGHT$(TEXT$, ST%)
		GOTO 517


518		ST% = INSTR(1%, TEXT$, '"') + 1%
		ST1% = INSTR(ST%, TEXT$, '"') - ST%
		TEXT$ = MID$(TEXT$, ST%, ST1%)

		PRINT #WRIT_FILE.CH%, "^*" + TEXT$ + "\*"
		PRINT #WRIT_FILE.CH%, ".P"

		WHILE 1%
			LINPUT #READ_FILE.CH%, TEXT$

			GOTO 520 IF INSTR(1%, TEXT$, "++")
		NEXT

520		WHILE 1%

			WHEN ERROR IN
				LINPUT #READ_FILE.CH%, TEXT$
			USE
				CONTINUE 525 IF ERR = 11%
				FILENAME$ = FILE_NAME$
				CONTINUE HelpError
			END WHEN

			IF INSTR(1%, TEXT$, "!*****")
			THEN
				GOTO 520
			END IF

			IF INSTR(1%, TEXT$, "!")
			THEN
				GOSUB InsertID
			END IF

		NEXT

525		PRINT #WRIT_FILE.CH%, ".b"
		PRINT #WRIT_FILE.CH%, ".b"
		PRINT #WRIT_FILE.CH%, ".page"
		PRINT #WRIT_FILE.CH%, '.require "prog.rnx"'

		CLOSE READ_FILE.CH%
		CLOSE WRIT_FILE.CH%
 NextJ:

	NEXT J%

 ExitProgram:
	GOTO EndProgram

 InsertID:
	POSIT% = INSTR(1%, TEXT$, "!") + 1%

	GOTO 530 IF POSIT% > 8%

	TEXT$ = RIGHT$(TEXT$, POSIT%)
	TEXT$ = EDIT$(TEXT$, 8%)

	ST% = 1%
526	ST% = INSTR(ST% + 1%, TEXT$, "_") + 1%
	GOTO 527 IF ST% = 1%

	TEXT$ = LEFT$(TEXT$, ST% - 1%) + "_" + RIGHT$(TEXT$, ST%)
	GOTO 526


527	IF TEXT$ = ""
	THEN
		PRINT #WRIT_FILE.CH%, ".b"
	ELSE
		PRINT #WRIT_FILE.CH%, TEXT$
	END IF

530	RETURN

 HelpError:
	!*******************************************************************
	! Help Message for an Error
	!*******************************************************************
	PRINT ERR
	PRINT ERL
	!CALL HELP_3MESSAGE(SCOPE,FILENAME$ + " " + NUM1$(ERL) + " " + ERT$(ERR), &
	!	"ERR", ERN$, "ERROR" + NUM1$(ERR))

	GOTO EndProgram

19000	!*******************************************************************
	! Error trapping
	!*******************************************************************

	RESUME HelpError

 EndProgram:
32767	END
