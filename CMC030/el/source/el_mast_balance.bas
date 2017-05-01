1	%TITLE "Equipment Ledger Balances"
	%SBTTL "EL_MAST_BALANCE"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1992 BY
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
	!	The ^*Equipment Ledger Balances\* option
	!	maintains the equipment ledger balances.  This file is a history
	!	file and shows all past activity.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	!	EL_MAIN_BALANCE$HELP
	!
	! Compile:
	!
	!	$ BAS EL_SOURCE:EL_MAST_BALANCE/LINE
	!	$ LINK/EXE=EL_EXE: EL_MAST_BALANCE,-
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE EL_MAST_BALANCE.OBJ;*
	!
	! Author:
	!
	!	10/13/92 - Dan Perkins
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/17/96 - Kevin Handy
	!		Clean up (Check)
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

	%INCLUDE "FUNC_INCLUDE:EL_WINDOW.INC"

	!
	! External functions
	!
	EXTERNAL LONG FUNCTION MAIN_WINDOW

	%PAGE

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

400	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_WINDOW(EL_MAIN_BALANCE.ID, "")

	!******************************************************************
	! End of the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19000	!******************************************************************
	! Error trapping
	!******************************************************************

19990	END



20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:EL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	EXTERNAL LONG FUNCTION EL_MAIN_BALANCE
	EXTERNAL LONG FUNCTION EL_MAIN_EQUIPMENT
	EXTERNAL LONG FUNCTION GL_MAIN_CHART

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE EL_MAIN_BALANCE.ID

		MAINT_GROUP = EL_MAIN_BALANCE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE GL_MAIN_CHART.ID

		MAINT_GROUP = GL_MAIN_CHART(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE EL_MAIN_EQUIPMENT.ID

		MAINT_GROUP = EL_MAIN_EQUIPMENT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
