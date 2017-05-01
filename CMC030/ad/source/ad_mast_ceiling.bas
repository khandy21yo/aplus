1	%TITLE "Cost Recovery Ceiling Table"
	%SBTTL "AD_MAST_CEILING"
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
	!	The ^*Cost Recovery Ceiling Table\* sets a ceiling, or maximum, for the
	!	amount which can be depreciated each year. This ceiling usually causes the
	!	depreciation to last over more years. The tables are used mostly for luxury
	!	cars.
	!	.LM -5
	!
	! Index:
	!	.x Ceiling>Tables
	!	.x Tables>Ceiling
	!
	! Option:
	!	AD_MAIN_CEILING$HELP
	!	AD_MAIN_CEILINGONE$HELP
	!	AD_MAIN_CEILINGTWO$HELP
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_MAST_CEILING/LINE
	!	$ LINK/EXE=AD_EXE: AD_MAST_CEILING,-
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AD_MAST_CEILING.OBJ;*
	!
	! Author:
	!
	!	09/07/88 - Frank Starman
	!
	! Modification history:
	!
	!	04/07/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!
	!	10/03/96 - Kevin Handy
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

	V% = MAIN_WINDOW(AD_MAIN_CEILING.ID, "")

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

	EXTERNAL LONG FUNCTION AD_MAIN_CEILING
	EXTERNAL LONG FUNCTION AD_MAIN_CEILINGONE
	EXTERNAL LONG FUNCTION AD_MAIN_CEILINGTWO

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE AD_MAIN_CEILING.ID

		MAINT_GROUP = AD_MAIN_CEILING(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AD_MAIN_CEILINGONE.ID

		MAINT_GROUP = AD_MAIN_CEILINGONE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AD_MAIN_CEILINGTWO.ID

		MAINT_GROUP = AD_MAIN_CEILINGTWO(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
