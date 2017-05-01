1	%TITLE "EMPLOY - Maintain Employee File"
	%SBTTL "PR_MAST_EMPLOYEE"
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
	!	The ^*Maintain Employee File\* option
	!	maintains information relative to each employee.
	!
	! Index:
	!	.x Maintain>Employee Master File
	!	.x Maintain>Employee Master File
	!	.x Employee Master File>Maintain
	!	.x Employee Master File>Maintain
	!	.x Employee>Master File
	!	.x Master File>Employee
	!
	! Option:
	!
	!	PR_MAIN_EMPLOYEE$HELP
	!	PR_MAIN_EMP_STATUS$HELP
	!	PR_MAIN_EMP_RATE$HELP
	!	PR_MAIN_EMP_STD_ERNDED$HELP
	!	PR_MAIN_EMP_ACCRUAL$HELP
	!	PR_MAIN_EMP_DATES$HELP
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_MAST_EMPLOYEE/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_MAST_EMPLOYEE, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_MAST_EMPLOYEE.OBJ;*
	!
	! Author:
	!
	!	04/27/87 - Kevin Handy
	!
	! Modification history:
	!
	!	02/07/89 - Frank F. Starman
	!		Added Location and Department
	!
	!	05/22/91 - Kevin Handy
	!		Added code so that the PR_EMP_DATES file is
	!		created here in read/write mode, so that the
	!		PR_READ_DATES won't open it up in read/only
	!		mode.
	!
	!	12/26/91 - Kevin Handy
	!		Added ACCRUAL journal.
	!
	!	01/04/92 - Kevin Handy
	!		Added ACCRUAL_RATE journal.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/12/96 - Kevin Handy
	!		Add PR_MAIN_SKILLS to list
	!		Reformat source code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/06/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	01/11/2001 - Kevin Handy
	!		Fixed PR_EMP_RATES to PR_EMP_DATES in error message
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:PR_WINDOW.INC"

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_DATES.HB"
	MAP	(PR_EMP_DATES)		PR_EMP_DATES_CDD	PR_EMP_DATES

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	! This common area must be mapped in some of the MAIN programs,
	! PR_MAST_EMPLOYEE.BAS, and PR_MAST_WC_WORK.BAS.
	!
	COM (CH_PR_EMP_DATES) &
		PR_EMP_DATES.CH%, &
		PR_EMP_DATES.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION MAIN_WINDOW

	%PAGE

	!
	! Initialize all the standard stuff through an external call
	!
	CALL READ_INITIALIZE

100	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_DATES.CRE"
	USE
		FILENAME$ = "PR_EMP_DATES"
		CONTINUE HelpError
	END WHEN

	PR_EMP_DATES.READONLY% = 0%

1000	!******************************************************************
	! Handle the main file
	!******************************************************************

	!
	! Maintain file
	!
	V% = MAIN_WINDOW(PR_MAIN_EMPLOYEE.ID, "")

	!******************************************************************
	! End of the program
	!******************************************************************
 ExitProgram:

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitProgram

19990	END


20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"
	%INCLUDE "FUNC_INCLUDE:PR_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	!
	! External functions
	!
	EXTERNAL LONG FUNCTION GL_MAIN_CHART

	EXTERNAL LONG FUNCTION PR_MAIN_EMPLOYEE
	EXTERNAL LONG FUNCTION PR_MAIN_EMP_STATUS
	EXTERNAL LONG FUNCTION PR_MAIN_EMP_RATE
	EXTERNAL LONG FUNCTION PR_MAIN_EMP_STD_ERNDED
	EXTERNAL LONG FUNCTION PR_MAIN_EMP_DATES
	EXTERNAL LONG FUNCTION PR_MAIN_EMP_ACCRUAL
	EXTERNAL LONG FUNCTION PR_MAIN_EMP_ACCRUAL_RATE
	EXTERNAL LONG FUNCTION PR_MAIN_ERNDED_DEF
	EXTERNAL LONG FUNCTION PR_MAIN_OPER
	EXTERNAL LONG FUNCTION PR_MAIN_UNPN_DESC
	EXTERNAL LONG FUNCTION PR_MAIN_WC_DESCR
	EXTERNAL LONG FUNCTION PR_MAIN_TAX_PKG
	EXTERNAL LONG FUNCTION PR_MAIN_SKILLS
	EXTERNAL LONG FUNCTION UTL_MAIN_COUNTRY
	EXTERNAL LONG FUNCTION UTL_MAIN_LOCATION
	EXTERNAL LONG FUNCTION UTL_MAIN_DEPARTMENT

	%PAGE

	SELECT SMG_WINDOW::IDENT

	!
	! Chart of accounts maintainence
	!
	CASE GL_MAIN_CHART.ID

		MAINT_GROUP = GL_MAIN_CHART(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	!
	! Employee master file main screen
	!
	CASE PR_MAIN_EMPLOYEE.ID

		MAINT_GROUP = PR_MAIN_EMPLOYEE(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	!
	! Employee status
	!
	CASE PR_MAIN_EMP_STATUS.ID

		MAINT_GROUP = PR_MAIN_EMP_STATUS(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	!
	! Employee rates
	!
	CASE PR_MAIN_EMP_RATE.ID

		MAINT_GROUP = PR_MAIN_EMP_RATE(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	!
	! Employee ern/ded
	!
	CASE PR_MAIN_EMP_STD_ERNDED.ID

		MAINT_GROUP = PR_MAIN_EMP_STD_ERNDED(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	!
	! Employee diary
	!
	CASE PR_MAIN_EMP_DATES.ID

		MAINT_GROUP = PR_MAIN_EMP_DATES(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	!
	! Employee Accrual
	!
	CASE PR_MAIN_EMP_ACCRUAL.ID

		MAINT_GROUP = PR_MAIN_EMP_ACCRUAL(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	!
	! Employee Accrual
	!
	CASE PR_MAIN_EMP_ACCRUAL_RATE.ID

		MAINT_GROUP = PR_MAIN_EMP_ACCRUAL_RATE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	!
	! Ern/ded definition
	!
	CASE PR_MAIN_ERNDED_DEF.ID

		MAINT_GROUP = PR_MAIN_ERNDED_DEF(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	!
	! Tax package
	!
	CASE PR_MAIN_TAX_PKG.ID

		MAINT_GROUP = PR_MAIN_TAX_PKG(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	!
	! Tax package
	!
	CASE PR_MAIN_OPER.ID

		MAINT_GROUP = PR_MAIN_OPER(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	!
	! Tax package
	!
	CASE PR_MAIN_UNPN_DESC.ID

		MAINT_GROUP = PR_MAIN_UNPN_DESC(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	!
	! Tax package
	!
	CASE PR_MAIN_WC_DESCR.ID

		MAINT_GROUP = PR_MAIN_WC_DESCR(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	!
	! Location
	!
	CASE UTL_MAIN_LOCATION.ID

		MAINT_GROUP = UTL_MAIN_LOCATION(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	!
	! Department
	!
	CASE UTL_MAIN_DEPARTMENT.ID

		MAINT_GROUP = UTL_MAIN_DEPARTMENT(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	!
	! Country
	!
	CASE UTL_MAIN_COUNTRY.ID

		MAINT_GROUP = UTL_MAIN_COUNTRY(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	!
	! Skills
	!
	CASE PR_MAIN_SKILLS.ID

		MAINT_GROUP = PR_MAIN_SKILLS(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
