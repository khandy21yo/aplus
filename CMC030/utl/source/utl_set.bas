1	%TITLE "Set File Maintenance"
	%SBTTL "UTL_SET"
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
	!
	! Abstract:HELP
	!	.p
	! Index:
	! Option:
	!
	!	UTL_MAIN_SET$HELP
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UTL_SET/NOLINE
	!	$ LINK/EXECUTABLE=UTL_EXE: UTL_SET, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE UTL_SET.OBJ;*
	!
	! Author:
	!
	!	08/18/87 - Kevin Handy
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/30/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/22/98 - Kevin Handy
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

	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	!
	! Maps
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_SET.HB"
	MAP (UTL_SET)	UTL_SET_CDD	UTL_SET

	COM (CH_UTL_SET) &
		UTL_SET.CH%

	!
	! External functions
	!
	EXTERNAL LONG FUNCTION MAIN_WINDOW

	%PAGE

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_SET.CRE"

	CALL READ_INITIALIZE

	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_WINDOW(UTL_MAIN_SET.ID, "")

	!******************************************************************
	! End of the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19990	END

20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	EXTERNAL LONG FUNCTION UTL_MAIN_SET

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE UTL_MAIN_SET.ID
		MAINT_GROUP = UTL_MAIN_SET(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
