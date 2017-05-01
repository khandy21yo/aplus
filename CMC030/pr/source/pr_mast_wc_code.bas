1	%TITLE "PR Workmans Compensation Code Maintenance"
	%SBTTL "PR_MAST_WC_CODE"
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
	!	The ^*Maintain Workmen's Compensation Codes\* option
	!	accesses the Workmen's Compensation
	!	Table file where a record is maintained for each Workmen's
	!	Compensation code. There must be one or more General Ledger subject labor
	!	accounts and there
	!	may be one or more subject labor operations which relate to the specific
	!	Workmen's Compensation Code.
	!	.b
	!	The purpose of established Code records in the Workmen's Compensation Table
	!	is to enable the system to calculate accrued premiums.
	!	.b
	!	NOTE: A "SI" record must exist in the tax package definition file
	!	for the final post to be able to automatically create the Workman Comp
	!	distribution for the General Ledger.
	!
	! Index:
	!	.x Workmans Compensation Code Maintenance
	!
	! Option:
	!
	!	PR_MAIN_WC_DESCR$HELP
	!
	! Index:
	!	.x Workmen's Compensation>Table
	!	.x Tables>Workmen's Compensation
	!	The following data is maintained in each record in the file:
	!	.LIST "*"
	!	.LE
	!	Workmen's Compensation Code.
	!	.LE
	!	Code Description.
	!	.LE
	!	General Ledger account to which liability
	!	in reference to a specific Code will be
	!	credited.
	!	.LE
	!	General Ledger account(s) to which insurance
	!	liability calculations will be charged.
	!	.LE
	!	State codes for each State where insurance
	!	liability could feasibly exist relative to
	!	a specific Code.
	!	.LE
	!	Effective dates for each premium rate or
	!	combination of rates.
	!	.LE
	!	Insurance premium rates for Workmen's
	!	Compensation, Property Liability and
	!	Personal Injury Liability insurances.
	!	.LE
	!	Flags to indicate whether overtime premium
	!	labor costs are subject to Workmen's
	!	Compensation, Property Liability or
	!	Personal Injury Liability insurance
	!	premium calculations in reference to each
	!	Code.
	!	.ELS
	!	.p
	!	There must be one or more General Ledger subject labor accounts
	!	which relate to the specific Workmen's Compensation Code.  These are
	!	identified in the definition section of the Workmen's Compensation
	!	Table record.
	!	.p
	!	There may be one or more subject labor operations which relate
	!	to the specific Workmen's Compensation Code.  These are identified
	!	in the definition section of the Workmen's Compensation record.
	!	.p
	!	The purpose of establishing Code records in the Workmen's
	!	Compensation Table is to enable the system to calculate accrued
	!	premiums relative to Workmen's Compensation, Property Liability and
	!	Personal Injury insurances and post those calculations to specified
	!	General Ledger accounts.
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_MAST_WC_CODE/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_MAST_WC_CODE, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_MAST_WC_CODE.OBJ;*
	!
	! Author:
	!
	!	12/04/87 - B. Craig Larsen
	!
	! Modification history:
	!
	!	01/09/91 - Kevin Handy
	!		Removed the PR_WC_DEFINITION file.
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
	! External functions
	!
	EXTERNAL LONG   FUNCTION MAIN_WINDOW

	%PAGE

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_WINDOW(PR_MAIN_WC_DESCR.ID, "")

 ExitProgram:
	!******************************************************************
	! End of the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19990	END



20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PR_WINDOW.INC"

	EXTERNAL LONG FUNCTION GL_MAIN_CHART
	EXTERNAL LONG FUNCTION PR_MAIN_WC_DESCR
	EXTERNAL LONG FUNCTION PR_MAIN_WC_INSURANCE

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
	! WC Description file
	!
	CASE PR_MAIN_WC_DESCR.ID

		MAINT_GROUP = PR_MAIN_WC_DESCR(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	!
	! WC Insurance file
	!
	CASE PR_MAIN_WC_INSURANCE.ID

		MAINT_GROUP = PR_MAIN_WC_INSURANCE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
