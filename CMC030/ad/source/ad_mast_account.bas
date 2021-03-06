1	%TITLE "Depreciation Account Table"
	%SBTTL "AD_MAST_ACCOUNT"
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
	!	The ^*Depreciation Account\* table determines
	!	which General Ledger accounts will be affected when posting the calculated
	!	depreciation to the General Ledger.
	!	.LM -5
	!
	! Index:
	!	.x Depreciation Accounts
	!
	! Option:
	!
	!	AD_MAIN_ACCOUNT$HELP
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_MAST_ACCOUNT/LINE
	!	$ LINK/EXE=AD_EXE: AD_MAST_ACCOUNT,-
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AD_MAST_ACCOUNT.OBJ;*
	!
	! Author:
	!
	!	12/04/87 - Frank F. Starman
	!
	! Modification history:
	!
	!	04/07/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!
	!	07/03/96 - Kevin Handy
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

	%INCLUDE "FUNC_INCLUDE:AD_WINDOW.INC"

	%PAGE

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_WINDOW(AD_MAIN_ACCOUNT.ID, "")

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
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	EXTERNAL LONG FUNCTION AD_MAIN_ACCOUNT
	EXTERNAL LONG FUNCTION UTL_MAIN_LOCATION
	EXTERNAL LONG FUNCTION AD_MAIN_ASSTYPE
	EXTERNAL LONG FUNCTION GL_MAIN_CHART

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE AD_MAIN_ACCOUNT.ID

		MAINT_GROUP = AD_MAIN_ACCOUNT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_LOCATION.ID

		MAINT_GROUP = UTL_MAIN_LOCATION(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AD_MAIN_ASSTYPE.ID

		MAINT_GROUP = AD_MAIN_ASSTYPE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE GL_MAIN_CHART.ID

		MAINT_GROUP = GL_MAIN_CHART(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
