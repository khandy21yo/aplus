1	%TITLE "Reconciliation File Maintenance"
	%SBTTL "CK_MAST_CKMNT"
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
	!	The ^*Maintain Reconciliation File\* option creates
	!	records for the outstanding checks and deposits in transit for each
	!	bank account.
	!	.lm -5
	!
	! Index:
	!	.x Maintenance>Reconciliation File
	!	.x Check Reconciliation File>Maintenance
	!	.x Initialize>Outstanding Checks
	!	.x Initialize>Deposits in Transit
	!	.x Outstanding Checks>Initialize
	!	.x Deposits in Transit>Initialize
	!
	! Option:
	!
	!	CK_MAIN_CKMNT$HELP
	!
	! Compile:
	!
	!	$ BAS CK_SOURCE:CK_MAST_CKMNT/LINE
	!	$ LINK/EXE=CK_EXE: CK_MAST_CKMNT,-
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE CK_MAST_CKMNT.OBJ;*
	!
	! Author:
	!
	!	01/18/88 - Frank F. Starman
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/17/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/19/98 - Kevin Handy
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

	%INCLUDE "FUNC_INCLUDE:CK_WINDOW.INC"

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

	V% = MAIN_WINDOW(CK_MAIN_CKMNT.ID, "")

	!******************************************************************
	! End of the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19990	END



20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"
	%INCLUDE "FUNC_INCLUDE:CK_WINDOW.INC"

	EXTERNAL LONG FUNCTION CK_MAIN_CKMNT

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE CK_MAIN_CKMNT.ID

		MAINT_GROUP = CK_MAIN_CKMNT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
