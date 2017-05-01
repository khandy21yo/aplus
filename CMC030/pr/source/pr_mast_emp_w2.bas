1	%TITLE "Maintain W2 file"
	%SBTTL "PR_MAST_EMP_W2"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 2001 BY
	!
	! Software Solutions, Inc.
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
	! Software Solutions, Inc.
	!
	! Software Solutions, Inc. assumes no responsibility for the use
	! or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	! Abstract:HELP
	!	.b
	!	This option enters the data to be printed on the W2 forms.
	!
	! Index:
	!
	! Option:
	!
	!	PR_MAIN_EMP_W2$HELP
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_MAST_EMP_W2/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_MAST_EMP_W2, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_MAST_EMP_W2.OBJ;*
	!
	! Author:
	!
	!	01/11/2001 - Kevin Handy
	!
	! Modification history:
	!
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
	! PR_MAST_EMP_W2.BAS, and PR_MAST_WC_WORK.BAS.
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

1000	!******************************************************************
	! Handle the main file
	!******************************************************************

	!
	! Maintain file
	!
	V% = MAIN_WINDOW(PR_MAIN_EMP_W2.ID, "")

	!******************************************************************
	! End of the program
	!******************************************************************
 ExitProgram:

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

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

	EXTERNAL LONG FUNCTION PR_MAIN_EMP_W2
	EXTERNAL LONG FUNCTION PR_MAIN_ERNDED_DEF
	EXTERNAL LONG FUNCTION PR_MAIN_TAX_PKG
	EXTERNAL LONG FUNCTION UTL_MAIN_COUNTRY
	EXTERNAL LONG FUNCTION UTL_MAIN_LOCATION

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
	CASE PR_MAIN_EMP_W2.ID

		MAINT_GROUP = PR_MAIN_EMP_W2(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

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
	! Location
	!
	CASE UTL_MAIN_LOCATION.ID

		MAINT_GROUP = UTL_MAIN_LOCATION(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	!
	! Country
	!
	CASE UTL_MAIN_COUNTRY.ID

		MAINT_GROUP = UTL_MAIN_COUNTRY(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
