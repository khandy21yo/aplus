1	%TITLE "Union/Pension Maintenance"
	%SBTTL "PR_MAST_UNPN"
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
	!	.p
	!	The ^*Union/Pension Maintenance\* option
	!	maintains the Union/Pension table.  This table contains
	!	the code, description, and the appropriate expense account.
	!
	! Index:
	!	.x Union/Pension Table>Maintain
	!	.x Maintain>Union/Pension Table
	!
	! Option:
	!
	!	PR_MAIN_UNPN_DESC$HELP
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_MAST_UNPN/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_MAST_UNPN, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_MAST_UNPN.OBJ;*
	!
	! Author:
	!
	!	12/11/87 - B. Craig Larsen
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/24/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
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

	%INCLUDE "FUNC_INCLUDE:PR_WINDOW.INC"

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION MAIN_WINDOW

	%PAGE

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_WINDOW(PR_MAIN_UNPN_DESC.ID, "")

 ExitProgram:
	!******************************************************************
	! End of the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19990	END



20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PR_WINDOW.INC"

	EXTERNAL LONG FUNCTION GL_MAIN_CHART
	EXTERNAL LONG FUNCTION PR_MAIN_UNPN_DESC
	EXTERNAL LONG FUNCTION PR_MAIN_UNPN_DEF

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	!
	! Chart of accounts maintainence
	!
	CASE GL_MAIN_CHART.ID

		MAINT_GROUP = GL_MAIN_CHART(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	!
	! Union/Pension Description file
	!
	CASE PR_MAIN_UNPN_DESC.ID

		MAINT_GROUP = PR_MAIN_UNPN_DESC(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	!
	! Union/Pension Definition File
	!
	CASE PR_MAIN_UNPN_DEF.ID

		MAINT_GROUP = PR_MAIN_UNPN_DEF(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
