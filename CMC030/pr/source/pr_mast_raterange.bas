1	%TITLE "Payroll Rate Range Maintenance"
	%SBTTL "PR_MAST_RATERANGE"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1987, 1988, 1989 BY
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
	!	The ^*Rate Range Table\* allows for the establishment of a range of rate
	!	parameters depending on the location of the business and the age of the
	!	employee.
	!
	! Index:
	!	.x Rate Range Table>Maintain
	!	.x Maintain>Rate Range Table
	!
	! Option:
	!
	!	PR_MAIN_RATERANGE$HELP
	!
	! Author:
	!
	!	06/01/89 - B. Craig Larsen
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_MAST_RATERANGE
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_MAST_RATERANGE, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_MAST_RATERANGE.OBJ;*
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

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:PR_WINDOW.INC"

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION MAIN_WINDOW

	%PAGE

400	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_WINDOW(PR_MAIN_RATERANGE.ID, "")

 ExitProgram:
	!******************************************************************
	! End of the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19990	END



20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:PR_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	!
	! External functions
	!
	EXTERNAL LONG FUNCTION PR_MAIN_RATERANGE
	EXTERNAL LONG FUNCTION UTL_MAIN_LOCATION

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE PR_MAIN_RATERANGE.ID

		MAINT_GROUP = PR_MAIN_RATERANGE(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_LOCATION.ID

		MAINT_GROUP = UTL_MAIN_LOCATION(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
