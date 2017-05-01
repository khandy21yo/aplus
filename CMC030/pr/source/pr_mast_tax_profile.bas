1	%TITLE "Tax Profile Maintenance"
	%SBTTL "PR_MAST_TAX_PROFILE"
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
	!	.p
	!	The ^*Payroll Tax and GL Profile\* option
	!	maintains tables which assign General Ledger expense and
	!	liability account numbers to which the various taxes will be posted.
	!
	! Index:
	!	.x Tax Profile Maintenance
	!	.x Payroll Tax Profile
	!
	! Option:
	!
	!	PR_MAIN_TAX_PROFILE_C$HELP
	!	PR_MAIN_TAX_PROFILE_D$HELP
	!	PR_MAIN_TAX_PROFILE_E$HELP
	!	PR_MAIN_TAX_PROFILE_F$HELP
	!	PR_MAIN_TAX_PROFILE_S$HELP
	!	PR_MAIN_TAX_PROFILE_FRI$HELP
	!
	! Index:
	!	.x Payroll Tax>Profile
	!	files includes:
	!	.LIST "*"
	!	.LE
	!	Tax Identification Numbers
	!	.LE
	!	Unemployment Tax Rates
	!	.LE
	!	Unemployment Tax Maximum Earnings Limits
	!	.LE
	!	General Ledger Payroll Cash Account
	!	.LE
	!	Accrued Payroll Account
	!	.LE
	!	Minimum Hourly Wage Rates
	!	.ELS
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_MAST_TAX_PROFILE/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_MAST_TAX_PROFILE, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_MAST_TAX_PROFILE.OBJ;*
	!
	! Author:
	!
	!	09/22/87 - Kevin Handy
	!
	! Modification history:
	!
	!	01/09/89 - Kevin Handy
	!		Modified all window routines to pull up the proper
	!		help messages, instead of all pulling up the same
	!		messages.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/24/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/21/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:PR_WINDOW.INC"

	!
	! Map areas
	!
	%INCLUDE "SOURCE:[PR.OPEN]PR_CONTROL.HB"
	MAP (PR_CONTROL)	PR_CONTROL_CDD	PR_CONTROL

	MAP (PR_TAX_PROFILE) OH_APPLY_FLAG$ = 1%

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION MAIN_WINDOW

	ON ERROR GOTO 19000

	%PAGE

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

300	!
	! Open Control file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_CONTROL.OPN"
		GET #PR_CONTROL.CH%, RECORD 1%, REGARDLESS
	USE
		FILENAME$ = "PR_CONTROL"
		CONTINUE HelpError
	END WHEN

	OH_APPLY_FLAG$ = PR_CONTROL::OH_APPLY_FLAG

1000	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_WINDOW(PR_MAIN_TAX_PROFILE_F.ID, "")

 ExitProgram:
	!******************************************************************
	! End of the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitProgram

	%Page

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

19990	END


20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PR_WINDOW.INC"

	EXTERNAL LONG FUNCTION GL_MAIN_CHART
	EXTERNAL LONG FUNCTION PR_MAIN_TAX_PROFILE_F
	EXTERNAL LONG FUNCTION PR_MAIN_TAX_PROFILE_S
	EXTERNAL LONG FUNCTION PR_MAIN_TAX_PROFILE_C
	EXTERNAL LONG FUNCTION PR_MAIN_TAX_PROFILE_E
	EXTERNAL LONG FUNCTION PR_MAIN_TAX_PROFILE_D
	EXTERNAL LONG FUNCTION PR_MAIN_TAX_PROFILE_FRI

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	!
	! Chart of accounts maintainence
	!
	CASE GL_MAIN_CHART.ID

		MAINT_GROUP = GL_MAIN_CHART(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	!
	! Federal tax profile
	!
	CASE PR_MAIN_TAX_PROFILE_F.ID

		MAINT_GROUP = PR_MAIN_TAX_PROFILE_F(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	!
	! State tax profile
	!
	CASE PR_MAIN_TAX_PROFILE_S.ID

		MAINT_GROUP = PR_MAIN_TAX_PROFILE_S(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	!
	! City tax profile
	!
	CASE PR_MAIN_TAX_PROFILE_C.ID

		MAINT_GROUP = PR_MAIN_TAX_PROFILE_C(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	!
	! School tax profile
	!
	CASE PR_MAIN_TAX_PROFILE_E.ID

		MAINT_GROUP = PR_MAIN_TAX_PROFILE_E(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	!
	! County tax profile
	!
	CASE PR_MAIN_TAX_PROFILE_D.ID

		MAINT_GROUP = PR_MAIN_TAX_PROFILE_D(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	!
	! Tax fringe expense distribution table
	!
	CASE PR_MAIN_TAX_PROFILE_FRI.ID

		MAINT_GROUP = PR_MAIN_TAX_PROFILE_FRI(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVAULE)

	END SELECT

32767	END FUNCTION
