1	%TITLE "MAINT - Maintain User Def Summary Layout Report"
	%SBTTL "GL_MAST_VENCOL"
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
	!	The ^*User Defined Summary Layout Report Table\*
	!	establishs a report containing user specified accounts. A user defined key
	!	makes access to the report easy and fast.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!	GL_MAIN_VENCOL$HELP
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_MAST_VENCOL/LINE
	!	$ LINK/EXECUTABLE=GL_EXE: GL_MAST_VENCOL, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_MAST_VENCOL.OBJ;*
	!
	! Author:
	!
	!	06/20/89 - Lance Williams
	!
	! Modification history:
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
	!	12/17/2000 - Kevin Handy
	!		Lose useless error trapping
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

	V% = MAIN_WINDOW(GL_MAIN_VENCOL.ID, "")

	!******************************************************************
	! Exit GL_MAST_VENCOL
	!******************************************************************

 ExitProgram:
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19990	!******************************************************************
	! End of GL_MAST_VENCOL
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
	EXTERNAL LONG	FUNCTION GL_MAIN_VENCOL

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
	! Process the Financial Statement maintenance window
	!
	CASE GL_MAIN_VENCOL.ID
		MAINT_GROUP = GL_MAIN_VENCOL(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	END SELECT

32767	!******************************************************************
	! End of MAINT_GROUP function
	!******************************************************************
	END FUNCTION
