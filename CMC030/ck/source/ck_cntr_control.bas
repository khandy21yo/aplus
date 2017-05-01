1	%TITLE "Check Reconciliation Control File"
	%SBTTL "CK_CNTR_CONTROL"
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
	! any other copies thereof may not be provided or otherwise mCKe
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
	!	The ^*Check/Deposit Control Maintenance\* option
	!	establishes the current fiscal year and the General Ledger or accounting
	!	period which identifies the accounting period immediately preceeding
	!	the period in which the Check Reconciliation System is initialized.
	!	.lm -5
	!
	! Index:
	!	.x Check/Deposit Maintenance>Control
	!	.x Control>Check/Deposit Maintenance
	!
	! Option:
	!	CK_MAIN_CONTROL$HELP
	!	CK_MAIN_CONTROLACC$HELP
	!
	! Compile:
	!
	!	$ BAS CK_SOURCE:CK_CNTR_CONTROL/LINE
	!	$ LINK/EXE=CK_EXE: CK_CNTR_CONTROL,-
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE CK_CNTR_CONTROL.OBJ;*
	!
	! Author:
	!
	!	12/09/87 - Lance Williams
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

	V% = MAIN_WINDOW(CK_MAIN_CONTROL.ID, "")

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
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	EXTERNAL LONG FUNCTION CK_MAIN_CONTROL
	EXTERNAL LONG FUNCTION CK_MAIN_CONTROLACC
	EXTERNAL LONG FUNCTION GL_MAIN_CHART

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE CK_MAIN_CONTROL.ID

		MAINT_GROUP = CK_MAIN_CONTROL(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE CK_MAIN_CONTROLACC.ID

		MAINT_GROUP = CK_MAIN_CONTROLACC(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE GL_MAIN_CHART.ID

		MAINT_GROUP = GL_MAIN_CHART(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
