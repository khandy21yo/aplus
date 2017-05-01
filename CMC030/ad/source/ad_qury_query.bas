1	%TITLE "Asset Depreciation Query"
	%SBTTL "AD_QURY_QUERY"
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
	!	The ^*Asset Depreciation Query\* option
	!	makes online inquiries into Asset Depreciation files and
	!	shows the complete status of these items.
	!	.lm -5
	!
	! Index:
	!	.x Query
	!	.x Asset Depreciation Query
	!
	! Option:
	!	AD_MAIN_QUERYASSET$HELP
	!	AD_OUTP_DEPRECIATION$HELP
	!	AD_MAIN_QUERYASSET$DEP_HISTORY
	!	AD_MAIN_QUERYASSET$PROJECTED_DEP
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_QURY_QUERY/NOLINE
	!	$ LINK/EXE=AD_EXE: AD_QURY_QUERY,-
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AD_QURY_QUERY.OBJ;*
	!
	! Author:
	!
	!	09/14/88 - Frank Starman
	!
	! Modification History:
	!
	!	04/10/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!		Add Modification history section.
	!
	!	10/03/96 - Kevin Handy
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

400	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_WINDOW(AD_MAIN_QUERYASSET.ID, "")

 ExitProgram:
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

	EXTERNAL LONG FUNCTION AD_MAIN_QUERYASSET

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE AD_MAIN_QUERYASSET.ID
		MAINT_GROUP = AD_MAIN_QUERYASSET(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
