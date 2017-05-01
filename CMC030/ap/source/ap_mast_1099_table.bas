1	%TITLE "Accounts Payable 1099 Table Maintenance"
	%SBTTL "AP_MAST_1099_TABLE"
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
	!	The ^*1099 Table\* builds a table with a
	!	record for each type of payment to be reported on Form 1099's,
	!	i.e., interest payments to be reported on Form 1099-INT, or the various
	!	types of miscellaneous payments which are to be reported on Form
	!	1099-MISC, etc.
	!	.lm -5
	!
	! Index:
	!	.x 1099 Table
	!
	! Option:
	!	AP_MAIN_1099_TABLE$HELP
	!
	! Author:
	!
	!	08/03/87 - B. Craig Larsen
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_MAST_1099_TABLE
	!	$ LINK/EXECUTABLE=AP_EXE:*.EXE AP_MAST_1099_TABLE, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_MAST_1099_TABLE.OBJ;*
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/09/96 - Kevin Handy
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

	%INCLUDE "FUNC_INCLUDE:AP_WINDOW.INC"

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

	V% = MAIN_WINDOW(AP_MAIN_1099_TABLE.ID, "")

	!******************************************************************
	! End of the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19990	END



20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"
	%INCLUDE "FUNC_INCLUDE:AP_WINDOW.INC"

	EXTERNAL LONG FUNCTION AP_MAIN_1099_TABLE

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT
	CASE AP_MAIN_1099_TABLE.ID

		MAINT_GROUP = AP_MAIN_1099_TABLE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
