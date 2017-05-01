1	%TITLE "Asset Depreciation Control File"
	%SBTTL "AD_CNTR_CONTROL"
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
	!	The ^*Controlling File\* identifies the last period
	!	depreciation was calculated and an update
	!	was executed. The status flag indicates the current activity
	!	of the Asset Depreciation system.
	!	.b
	!	The Controlling File is also used to select the Object
	!	for the General Ledger.
	!	.lm -5
	!
	! Index:
	!	.x Controlling File
	!	.x Last Updated Period
	!	.x Last Calculation Period
	!	.x General Ledger Object
	!
	! Option:
	!	AD_MAIN_CONTROL$HELP
	!	AD_MAIN_CONTROLOBJ$HELP
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_CNTR_CONTROL/LINE
	!	$ LINK/EXE=AD_EXE: AD_CNTR_CONTROL,-
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AD_CNTR_CONTROL.OBJ;*
	!
	! Author:
	!
	!	12/09/87 - Frank F. Starman
	!
	! Modification history:
	!
	!	04/04/95 - Kevin Handy
	!		(V3.6)
	!		Updated for version 3.6
	!
	!	09/26/96 - Kevin Handy
	!		Reformat source code
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

	%INCLUDE "FUNC_INCLUDE:AD_WINDOW.INC"

	%PAGE

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_WINDOW(AD_MAIN_CONTROL.ID, "")

	!******************************************************************
	! End of the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19990	END



20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)


	%INCLUDE "FUNC_INCLUDE:AD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	EXTERNAL LONG FUNCTION AD_MAIN_CONTROL
	EXTERNAL LONG FUNCTION AD_MAIN_CONTROLOBJ
	EXTERNAL LONG FUNCTION AD_MAIN_OBJECT
	EXTERNAL LONG FUNCTION UTL_MAIN_ERA

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE AD_MAIN_CONTROL.ID

		MAINT_GROUP = AD_MAIN_CONTROL(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AD_MAIN_OBJECT.ID

		MAINT_GROUP = AD_MAIN_OBJECT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_ERA.ID

		MAINT_GROUP = UTL_MAIN_ERA(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AD_MAIN_CONTROLOBJ.ID

		MAINT_GROUP = AD_MAIN_CONTROLOBJ(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)
	END SELECT

32767	END FUNCTION
