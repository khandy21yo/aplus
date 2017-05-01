1	%TITLE "MINMAX Table Maintenance"
	%SBTTL "BM_MAST_MAXMIN"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 2000 BY
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
	! or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	Maintains a table of minumum/maximum per part number used
	!	by the various MINMAX reports.
	!	This table is only used to generate the reports, it does not
	!	affect the normal use of any system.
	!	.lm -5
	!
	! Index:
	!	.x Minmax>Maintain
	!	.x Maintenance>Minmax
	!
	! Option:
	!
	!	BM_MAIN_MAXMIN$HELP
	!
	! Compile:
	!
	!	$ BAS BM_SOURCE:BM_MAST_MAXMIN/LINE
	!	$ LINK/EXECUTABLE=BM_EXE:*.EXE BM_MAST_MAXMIN, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE BM_MAST_MAXMIN.OBJ;*
	!
	! Author:
	!
	!	06/02/2000 - Kevin Handy
	!
	! Modification history:
	!
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:BM_WINDOW.INC"

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

	V% = MAIN_WINDOW(BM_MAIN_MAXMIN.ID, "")

	!******************************************************************
	! End of the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19990	END



20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include statements
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"
	%INCLUDE "FUNC_INCLUDE:BM_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"

	!
	! External Functions
	!
	EXTERNAL LONG FUNCTION BM_MAIN_MAXMIN
	EXTERNAL LONG FUNCTION PD_MAIN_PRODUCT

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE BM_MAIN_MAXMIN.ID

		MAINT_GROUP = BM_MAIN_MAXMIN(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE PD_MAIN_PRODUCT.ID
		MAINT_GROUP = PD_MAIN_PRODUCT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
