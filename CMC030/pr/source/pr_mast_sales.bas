1	%TITLE "Sales Maintenance"
	%SBTTL "PR_MAST_SALES"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1987 BY
	!
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
	! Abstract:HELP
	!	.p
	!	The ^*Maintain Sales Information\* routine enters
	!	sales volume data for specified locations on specified dates.
	!
	! Index:
	!	.x Maintain>Sales Information
	!	.x Sales>Maintain
	!
	! Option:
	!
	!	PR_MAIN_SALES$HELP
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_MAST_SALES/LINE
	!	$ LINK/EXE=PR_EXE: PR_MAST_SALES,-
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_MAST_SALES.OBJ;*
	!
	! Author:
	!
	!	03/06/89 - Frank Starman
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!		Add PR_MAIN_SALES.ID to PR_WINDOW.INC.
	!
	!	10/24/96 - Kevin Handy
	!		Clean up (Check)
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

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:PR_WINDOW.INC"

	!
	! External functions
	!
	EXTERNAL LONG FUNCTION MAIN_WINDOW

	%PAGE

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_WINDOW(PR_MAIN_SALES.ID, "")

	!******************************************************************
	! End of the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19990	END



20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:PR_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	EXTERNAL LONG FUNCTION PR_MAIN_SALES

	EXTERNAL LONG FUNCTION UTL_MAIN_LOCATION
	EXTERNAL LONG FUNCTION UTL_MAIN_DEPARTMENT

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE PR_MAIN_SALES.ID

		MAINT_GROUP = PR_MAIN_SALES(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_LOCATION.ID

		MAINT_GROUP = UTL_MAIN_LOCATION(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_DEPARTMENT.ID

		MAINT_GROUP = UTL_MAIN_DEPARTMENT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
