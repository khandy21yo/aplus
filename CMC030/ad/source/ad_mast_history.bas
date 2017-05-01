1	%TITLE "Asset Depreciation Period History"
	%SBTTL "AD_MAST_HISTORY"
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
	!	.B
	!	.LM +5
	!	The ^*History\* file contains all history information concerning the
	!	Depreciation Ledger. This file is updated each time the Depreciation Ledger
	!	file is updated and allows printing of depreciation amounts for previous
	!	periods.
	!	.B
	!	The information contained in this file is not meant to be edited.
	!	.LM -5
	!
	! Index:
	!	.x Asset Depreciation History File
	!
	! Option:
	!
	!	AD_MAIN_HISTORY$HELP
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_MAST_HISTORY/LINE
	!	$ LINK/EXE=AD_EXE: AD_MAST_HISTORY, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AD_MAST_HISTORY.OBJ;*
	!
	! Author:
	!
	!	12/15/87 - Frank F. Starman
	!
	! Modification history:
	!
	!	04/10/95 - Kevin Handy
	!		(V3.6)
	!		Update to v3.6 coding standards.
	!
	!	10/03/96 - Kevin Handy
	!		Clean up (Check)
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

	!
	! Initialize all the standard stuff through an external call
	!
	CALL READ_INITIALIZE

	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_WINDOW(AD_MAIN_HISTORY.ID, "")

	!******************************************************************
	! End of the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19990	END

20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:AD_WINDOW.INC"
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	EXTERNAL LONG	FUNCTION AD_MAIN_ASSET
	EXTERNAL LONG	FUNCTION AD_MAIN_OBJECT
	EXTERNAL LONG	FUNCTION AD_MAIN_CONTROLOBJ
	EXTERNAL LONG	FUNCTION AD_MAIN_HISTORY

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE AD_MAIN_HISTORY.ID

		MAINT_GROUP = AD_MAIN_HISTORY(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AD_MAIN_ASSET.ID

		MAINT_GROUP = AD_MAIN_ASSET(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AD_MAIN_OBJECT.ID

		MAINT_GROUP = AD_MAIN_OBJECT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AD_MAIN_CONTROLOBJ.ID

		MAINT_GROUP = AD_MAIN_CONTROLOBJ(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
