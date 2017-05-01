1	%TITLE "Accounts Receivable Control File Maintenance"
	%SBTTL "AR_CNTR_CONTROL"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1987,1988 BY
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
	!	The ^*Accounts Receivable Control File Maintenance\* establishes
	!	the "Last Closed Period", validate the GL Accounts Receivable Accounts,
	!	establish aging intervals, and determine the retention period in
	!	reference to the A/R History file. Once this file is set up, it
	!	normally is not necessary to access it again.
	!	.lm -5
	!
	! Index:
	!	.x Maintain>Control Record
	!	.x Control Record>Maintain
	!	.x Maintain>Control
	!	.x Control>Maintain
	!
	! Option:
	!
	!	AR_MAIN_CONTROL$HELP
	!	AR_MAIN_CONTROL_ACCT$HELP
	!
	! Author:
	!
	!	02/11/88 - Aaron Redd
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_CNTR_CONTROL/LINE
	!	$ LINK/EXEC=AR_EXE:*.EXE AR_CNTR_CONTROL, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_CNTR_CONTROL.OBJ;*
	!
	! Modification history:
	!
	!	03/15/88 - Kevin Handy
	!		Added journal for account numbers.
	!
	!	05/14/88 - Lance Williams
	!		Modified to allow R/O open of file if R/W fails.
	!
	!	07/27/88 - Kevin Handy
	!		Added METHOD to control file.
	!
	!	11/28/88 - J. Shad Rydalch
	!		Split program into two parts:  _CNTR_ and _MAIN_
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	06/25/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"

	!
	! External Functions
	!
	EXTERNAL LONG		FUNCTION MAIN_WINDOW

	%PAGE

	!******************************************************************
	! Initialize maintainence
	!******************************************************************

	CALL READ_INITIALIZE

	!******************************************************************
	! Handle the main file
	!******************************************************************

	V% = MAIN_WINDOW(AR_MAIN_CONTROL.ID, "")

	!******************************************************************
	! End of the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19990	END

20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"
	%INCLUDE"FUNC_INCLUDE:GL_WINDOW.INC"
	%INCLUDE"FUNC_INCLUDE:AR_WINDOW.INC"

	EXTERNAL LONG		FUNCTION GL_MAIN_CHART
	EXTERNAL LONG		FUNCTION AR_MAIN_CONTROL
	EXTERNAL LONG		FUNCTION AR_MAIN_CONTROL_ACCT

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE GL_MAIN_CHART.ID

		MAINT_GROUP = GL_MAIN_CHART(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AR_MAIN_CONTROL_ACCT.ID

		MAINT_GROUP = AR_MAIN_CONTROL_ACCT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AR_MAIN_CONTROL.ID

		MAINT_GROUP = AR_MAIN_CONTROL(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
