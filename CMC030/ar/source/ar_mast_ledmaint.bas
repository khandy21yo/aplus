1	%TITLE "Accounts Receivable Ledger Maintenance"
	%SBTTL "AR_MAST_LEDMAINT"
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
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Accounts Receivable Ledger Maintenance\* initializes
	!	data pertinent to each customer when the Accounts Receivable system
	!	is first installed. The file is updated from both the Sales Journal and Cash
	!	Receipts Journal.
	!	.b
	!	^*Note:\* This program should ^*NOT\* be used to make changes
	!	to the ledger. Any changes made through
	!	this program will ^*NOT\* appear in the General
	!	Ledger, or anywhere else. The correct procedure
	!	to make changes is through the Journal programs
	!	(Sales Journal and Cash Receipts Journal).
	!	.lm -5
	!
	! Index:
	!	.x Maintain>Ledger File
	!	.x Ledger File>Maintain
	!
	! Option:
	!
	!	AR_MAIN_LEDMAINT$HELP
	!	AR_MAIN_CLOSEMAINT$HELP
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_MAST_LEDMAINT/LINE
	!	$ LINK/EXEC:AR_EXE AR_MAST_LEDMAINT,FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_MAST_LEDMAINT.OBJ;*
	!
	! Author:
	!
	!	02/22/88 - Aaron Redd
	!
	! Modification history:
	!
	!	05/31/91 - Kevin Handy
	!		Added code for distribution maintenance.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/11/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/10/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"

	!
	! Maps
	!
	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.HB"
	MAP	(AR_CONTROL)	AR_CONTROL_CDD	AR_CONTROL

	COM (CH_AR_CONTROL) AR_CONTROL.CH%

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION MAIN_WINDOW

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

1100	!
	! Read in information from control file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.OPN"
		GET #AR_CONTROL.CH%, RECORD 1%, REGARDLESS
	USE
		FILENAME$ = "AR_CONTROL"
		CONTINUE HelpError
	END WHEN

1200	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_WINDOW(AR_MAIN_LEDMAINT.ID, "")

	!******************************************************************
	! End of the program
	!******************************************************************

 ExitProgram:
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	FILENAME$ = ""
	RESUME HelpError

 HelpError:
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	RESUME ExitProgram

19990	END



20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"
	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	!
	! External Functions
	!
	EXTERNAL LONG FUNCTION AR_MAIN_LEDMAINT
	EXTERNAL LONG FUNCTION AR_MAIN_OPEN_DIST
	EXTERNAL LONG FUNCTION AR_MAIN_CLOSEMAINT
	EXTERNAL LONG FUNCTION AR_MAIN_35CUSTOM
	EXTERNAL LONG FUNCTION GL_MAIN_CHART

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE GL_MAIN_CHART.ID
		MAINT_GROUP = GL_MAIN_CHART(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AR_MAIN_35CUSTOM.ID
		MAINT_GROUP = AR_MAIN_35CUSTOM(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AR_MAIN_LEDMAINT.ID
		MAINT_GROUP = AR_MAIN_LEDMAINT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AR_MAIN_OPEN_DIST.ID
		MAINT_GROUP = AR_MAIN_OPEN_DIST(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AR_MAIN_CLOSEMAINT.ID
		MAINT_GROUP = AR_MAIN_CLOSEMAINT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
