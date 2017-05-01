1	%TITLE "CHART OF ACCOUNTS HISTORY MASTER"
	%SBTTL "GL_MAST_CHARTHIS"
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
	!	The ^*Chart of Accounts History Master\* program
	!	automatically maintains historical dollar, unit, and hour balances on each
	!	preceeding period.
	!	.b
	!	^*NOTE: Manual Changes to records in this file should not be
	!	considered.\*
	!	.lm -5
	!
	! Index:
	!	.x General Ledger>Account History
	!	.x Account History>General Ledger
	!	.x Account History>Change
	!	.x Change>Account History
	!
	! Option:
	!
	!	GL_MAIN_CHARTHIS$HELP
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_MAST_CHARTHIS/LINE
	!	$ LINK/EXECUTABLE=GL_EXE: GL_MAST_CHARTHIS, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_MAST_CHARTHIS.OBJ;*
	!
	! Author:
	!
	!	04/27/87 - Kevin Handy
	!
	! Modification history:
	!
	!	06/23/88 - Aaron Redd
	!		Split into two modules (_MAST_ and _MAIN_) in order
	!		to meet standardization requirements.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/18/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/22/2000 - Kevin Handy
	!		Lose useless error trap
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

	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION MAIN_WINDOW

	%PAGE

100	!******************************************************************
	! Initialization section - Prepare to do anything
	!******************************************************************

	!
	! Initialize all the standard stuff through an external call
	!
	CALL READ_INITIALIZE

1000	!******************************************************************
	! Handle the main function
	!******************************************************************

	V% = MAIN_WINDOW(GL_MAIN_CHARTHIS.ID, "")

	!******************************************************************
	! Exit GL_MAST_CHARTHIS
	!******************************************************************

 ExitProgram:
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19990	!******************************************************************
	! End of GL_MAST_CHARTHIS
	!******************************************************************
	END



20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION GL_MAIN_CHARTHIS

	!
	! CDD inclusions
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	!
	! Process the Chart of Accounts History maintenance window
	!
	CASE GL_MAIN_CHARTHIS.ID
		MAINT_GROUP = GL_MAIN_CHARTHIS(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	END SELECT

32767	!******************************************************************
	! End of MAINT_GROUP function
	!******************************************************************
	END FUNCTION
