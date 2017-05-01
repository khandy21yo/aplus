1	%TITLE "CHART OF ACCOUNTS MASTER"
	%SBTTL "GL_MAST_TRICONTROL"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1995 BY
	!
	! Software Solutions, Inc.
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
	! Software Solutions, Inc.
	!
	! Software Solutions, Inc. assumes no responsibility for the use
	! or reliability of its software on equipment which is not supported
	! by Software Solutions, Inc.
	!
	!++
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Chart of Accounts Master\* maintains
	!	the Chart of Accounts.
	!	.lm -5
	!
	! Index:
	!	.x Control>Tri Spur
	!
	! Option:
	!
	!	GL_MAIN_CHART$HELP
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_MAST_TRICONTROL/LINE
	!	$ LINK/EXECUTABLE=GL_EXE: GL_MAST_TRICONTROL, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_MAST_TRICONTROL.OBJ;*
	!
	! Author:
	!
	!	06/06/95 - Kevin Handy
	!
	! Modification history:
	!
	!	07/03/96 - Kevin Handy
	!		Use GL_MAIN_CHART instead of GL_MAIN_CHARTEX.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/13/2000 - Kevin Handy
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

	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	!
	! External functions
	!
	EXTERNAL LONG FUNCTION MAIN_WINDOW

	%PAGE

100	!*******************************************************************
	! Initialization section - Prepare to do anything
	!*******************************************************************

	!
	! Initialize all the standard stuff through an external call
	!
	CALL READ_INITIALIZE

1000	!*******************************************************************
	! Handle the main function
	!*******************************************************************

	V% = MAIN_WINDOW(GL_MAIN_TRICONTROL.ID, "")

	!******************************************************************
	! Exit GL_MAST_TRICONTROL
	!******************************************************************

 ExitProgram:
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19990	!******************************************************************
	! End of GL_MAST_TRICONTROL
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
	EXTERNAL LONG FUNCTION GL_MAIN_CHART
	EXTERNAL LONG FUNCTION GL_MAIN_TRICONTROL
	EXTERNAL LONG FUNCTION GL_MAIN_CATEGORY

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
	! Process the Chart of Accounts maintenance window
	!
	CASE GL_MAIN_TRICONTROL.ID
		MAINT_GROUP = GL_MAIN_TRICONTROL(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE GL_MAIN_CHART.ID
		MAINT_GROUP = GL_MAIN_CHART(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE GL_MAIN_CATEGORY.ID
		MAINT_GROUP = GL_MAIN_CATEGORY(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	END SELECT

32767	!******************************************************************
	! End of MAINT_GROUP function
	!******************************************************************
	END FUNCTION
