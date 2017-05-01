1	%TITLE "Sub Process to Determine Help Library Key"
	%SBTTL "HELP_MENUHELPKEY"
	%IDENT "V3.6a Calico"

	SUB HELP_MENUHELPKEY(STRING COMMAND,	! Command &
		STRING TTYPE,			! Type H - help, P - prog &
		STRING SYSTEM_NAME,		! System name &
		STRING HELP_PRG_IDENT,		! Identification &
		STRING HELP_PRG_PROGRAM,	! Program name &
		STRING HELP_PRG_ITEM,		! Item &
		STRING PROGRAM_DEVICE,		! Device for program &
		LONG UTL_REPORT.LOC		! Report Device Location &
		)

	!
	! COPYRIGHT (C) 1987 BY
	! Computer Management Center
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
	! Abstract:HELP
	!	.p
	!	Determine name for help library key
	!
	! Parameters:
	!
	!	COMMAND
	!		Passed Command name
	!
	!	TTYPE
	!		Type H - help, P - prog
	!
	!	SYSTEM_NAME
	!		Passed System name
	!
	!	HELP_PRG_IDENT
	!		Passed Identification
	!
	!	HELP_PRG_PROGRAM
	!		Passed Program name
	!
	!	HELP_PRG_ITEM
	!		Passed Item
	!
	!	PROGRAM_DEVICE
	!		Passed Device for program
	!
	!	UTL_REPORT.LOC
	!		Passed Report Device Location
	!		0 = User directorfy
	!		1 = CMC: directory
	!
	!
	!	This subroutine determines the help library key.
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:HELP_MENUHELPKEY/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP HELP_MENUHELPKEY
	!	$ DELETE HELP_MENUHELPKEY.OBJ;*
	!
	! AUTHOR:
	!
	!	06/11/87 - B. Craig Larsen
	!
	! MODIFICATION HISTORY:
	!
	!	07/19/88 - Kevin Handy
	!		Modified so that both the CMC: and the users
	!		report file may be opened at the same time.
	!
	!	07/06/89 - Kevin Handy
	!		Removed IOBUF and IOBUF1 maps.
	!
	!	03/20/90 - Frank F. Starman
	!		Use suffix HELP just as a default,if there will
	!		be no $ sign.
	!
	!	09/28/90 - Kevin Handy
	!		Fix so it doesn't look into the users account,
	!		always look at CMC:.  This is for a bug caused
	!		when user types help on a report before ever
	!		running the report since Frank only partially
	!		implemented looking at CMC:.
	!
	!	03/14/92 - Kevin Handy
	!		Clean up vars (checkvar)
	!
	!	10/20/92 - Kevin Handy
	!		Stripped out commented out code so I could read it
	!		easier.  Also threw in random comments.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/17/96 - Kevin Handy
	!		Reformat source code
	!
	!	05/23/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/16/99 - Kevin Handy
	!		Use WHEN ERROR
	!
	!	04/08/99 - Kevin Handy
	!		Add definition of SCOPE to lose crashes.
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Map/Common Statements
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_MASTER_REPORT.HB"
	MAP (UTL_MASTER_REPORT) UTL_MASTER_REPORT_CDD UTL_MASTER_REPORT

	COM (CH_UTL_MASTER_REPORT) &
		UTL_MASTER_REPORT.CH%

	!
	! Create a couple of buffers
	!
	RECORD IOBUF_RECORD
		VARIANT
		CASE
			LONG IO_BUF(6%)
		CASE
			WORD IO_BUF_W(12%)
		END VARIANT
	END RECORD

	DECLARE IOBUF_RECORD IOBUF

	RECORD IOBUF1_RECORD
		STRING NAME.BUFFER = 50%
	END RECORD

	DECLARE IOBUF1_RECORD IOBUF1

	EXTERNAL LONG    FUNCTION SYS$FILESCAN

	!
	! Declare vars
	!
	DECLARE LONG CONSTANT FSCN$_NAME = 6

	%PAGE

	!******************************************************************
	! Determine key for help library
	!******************************************************************
	PROGRAM_DEVICE = ""

	IF INSTR(1%, COMMAND, "!")
	THEN
		COMMAND = LEFT(COMMAND, INSTR(1%, COMMAND, "!") - 1%)
	END IF

	IF TTYPE = "H"
	THEN
		!
		! Help if pointer to a menu level.
		!
		HELP_PRG_IDENT = "H"
		DOL% = INSTR(1%, COMMAND, "$")
		IF DOL%
		THEN
			HELP_PRG_PROGRAM = LEFT(COMMAND, DOL% - 1%)
			HELP_PRG_ITEM = RIGHT(COMMAND, DOL% + 1%)
		ELSE
			HELP_PRG_PROGRAM = SYSTEM_NAME
			HELP_PRG_ITEM    = COMMAND
		END IF
	ELSE
		!
		! Help if pointer to a program.
		!
		HELP_PRG_ITEM = "HELP"
		TEMP$ = COMMAND
		HELP_PRG_IDENT = "H"

		!
		! If using report, then pull off the report
		! number and hash out a name from that.
		!
		IF INSTR(1%, TEMP$, "UTL_REPORT")
		THEN
			I% = INSTR(1%, TEMP$, "/C")
			IF I%
			THEN
				TEMP$ = EDIT$(MID(TEMP$, I% + 2%, 6%), -1%)

				GOSUB GetReportProgram
			ELSE
				TEMP$ = "UTL_REPORT_" + SYSTEM_NAME
				SYS_STATUS% = 1%
			END IF
			HELP_PRG_IDENT = "H"
		ELSE
			!
			! Help if a pointer to a program
			! other than report.
			!
			IOBUF1::NAME.BUFFER = TEMP$
			I% = INSTR(1%, IOBUF1::NAME.BUFFER, "/")
			IOBUF1::NAME.BUFFER = &
				LEFT(IOBUF1::NAME.BUFFER, I% - 1%) IF I%

			!
			! Strip off all but the program name
			!
			IOBUF::IO_BUF_W(1%) = FSCN$_NAME
			IOBUF::IO_BUF_W(0%) = 0%
			IOBUF::IO_BUF(1%) = 0%
			IOBUF::IO_BUF(2%) = 0%
			IOBUF::IO_BUF(3%) = 0%
			SYS_STATUS% = SYS$FILESCAN( &
				IOBUF1::NAME.BUFFER BY DESC, &
				IOBUF::IO_BUF() BY REF, 0%)
			TEMP_LONG% = IOBUF::IO_BUF(1%)
			TEMP1_LONG% = LOC(IOBUF1::NAME.BUFFER)
			TEMP_LONG% = TEMP_LONG% - TEMP1_LONG% + 1%
			TEMP$ = MID(IOBUF1::NAME.BUFFER, &
				TEMP_LONG%, IOBUF::IO_BUF_W(0%))

			PROGRAM_DEVICE = LEFT(IOBUF1::NAME.BUFFER, &
				TEMP_LONG% - 1%)

		END IF

		!
		! For either report or program, give help
		! if the status isn't bad.
		!
		IF (SYS_STATUS% AND 1%) = 0%
		THEN
			HELP_PRG_PROGRAM = "BAD_FILE_NAME"
			HELP_PRG_ITEM = "BAD"
		ELSE
			HELP_PRG_PROGRAM= TEMP$
		END IF
	END IF

 ExitProgram:

	EXIT SUB

 GetReportProgram:
2000	!
	! Try to pull from system report file
	!
	IF UTL_MASTER_REPORT.CH% < 1%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_MASTER_REPORT.OPN"
		USE
			FILENAME$ = "UTL_REPORT"
			CONTINUE HelpError
		END WHEN
	END IF

2010	SYS_STATUS% = 1%

	WHEN ERROR IN
		FIND #UTL_MASTER_REPORT.CH%, KEY #0% EQ TEMP$, REGARDLESS
		GET #UTL_MASTER_REPORT.CH%, REGARDLESS
	USE
		IF ERR = 155%
		THEN
			TEMP$ = "REPORT_MISSING"
			HELP_PRG_ITEM = "REPORT_MISSING"
			CONTINUE 2020
		END IF
		FILENAME$ = "UTL_REPORT"
		CONTINUE HelpError
	END WHEN

	TEMP$ = TRM$(UTL_MASTER_REPORT::PRONAM)
	PROGRAM_DEVICE = TRM$(UTL_MASTER_REPORT::PRODEV)

2020	RETURN

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	SYS_STATUS% = (SYS_STATUS% OR 1%)

	GOTO ExitProgram

32767	END SUB
