1	%TITLE "General Ledger Control File"
	%SBTTL "GL_CNTR_CONTROL"
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
	!	.B
	!	.LM +5
	!	The ^*CONTROL\* file defines the number
	!	of periods in a fiscal year.
	!
	! Index:
	!	.X Change>Control File
	!	.x Control File>Change
	!	.x Control File>General Ledger
	!	.x General Ledger>Control File
	!
	! Option:
	!
	!	GL_MAIN_CONTROL$HELP
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_CNTR_CONTROL/LINE
	!	$ LINK/EXECUTABLE=GL_EXE: GL_CNTR_CONTROL, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_CNTR_CONTROL.OBJ;*
	!
	! Author:
	!
	!	02/26/87 - Kevin Handy
	!
	! Modification history:
	!
	!	11/29/88 - J. Shad Rydalch
	!		Split program into two parts:  _CNTR_ and _MAIN_
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/17/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/19/98 - Kevin Handy
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

	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION MAIN_WINDOW

	%PAGE

	!******************************************************************
	! Initialization maintainence
	!******************************************************************

	CALL READ_INITIALIZE

	!******************************************************************
	! Handle the main function
	!******************************************************************

	V% = MAIN_WINDOW(GL_MAIN_CONTROL.ID, "")

	!******************************************************************
	! End of program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19990	END


20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	EXTERNAL LONG	FUNCTION GL_MAIN_CONTROL

	%PAGE

	!
	!Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE GL_MAIN_CONTROL.ID
		MAINT_GROUP = GL_MAIN_CONTROL(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
