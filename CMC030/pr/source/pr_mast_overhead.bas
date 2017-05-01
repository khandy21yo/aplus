1	%TITLE "PR Overhead Maintenance"
	%SBTTL "PR_MAST_OVERHEAD"
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
	!	The ^*Maintain Overhead Table\* option
	!	accesses the Overhead Table file.
	!	.p
	!	The Overhead Table file creates records which
	!	determine, depending on the General Ledger labor account and any
	!	associated operation, the following:
	!	.list "*"
	!	.le
	!	Overhead record key code
	!	.le
	!	Overhead record key description
	!	.le
	!	Rate at which overhead will be calculated
	!	.le
	!	Basis or method to be used in calculating overhead
	!	.le
	!	General Ledger account to which overtime premium
	!	costs will be charged, if applicable
	!	.le
	!	General Ledger account to which applied overhead
	!	will be credited
	!	.le
	!	General Ledger account to which overhead expense
	!	calculations will be charged
	!	.els
	!	.p
	!	There must be one or more General Ledger subject labor accounts
	!	which relate to the specific Overhead Key. These are identified in
	!	the definition section of the Overhead Table record.
	!	.p
	!	There may be one or more subject labor operations which relate to
	!	the specific Overhead Key. These are identified in the definition
	!	section of the Overhead Table record.
	!	.p
	!	The purpose of establishing Overhead Keys is to enable the system to
	!	calculate applied overhead, depending upon the labor accounts and/or
	!	labor operations, at varying rates and different methods and post
	!	those calculations to specified General Ledger accounts and costing
	!	locations.
	!
	! Index:
	!	.x Maintain Overhead>Table
	!	.x Table>Maintain Overhead
	!
	! Option:
	!
	!	PR_MAIN_OVERHEAD_DESC$HELP
	!	PR_MAIN_OVERHEAD_DEF$HELP
	!
	! Index:
	!	.x Overhead>Table>Maintain
	!	.x Maintain>Overhead Table
	!	associated operation, the following:
	!	.list "*"
	!	.le
	!	Overhead record key code
	!	.le
	!	Overhead record key description
	!	.le
	!	Rate at which overhead will be calculated
	!	.le
	!	Basis or method to be used in calculating overhead
	!	.le
	!	General Ledger account to which overtime premium
	!	costs will be charged, if applicable
	!	.le
	!	General Ledger account to which applied overhead
	!	will be credited
	!	.le
	!	General Ledger account to which overhead expense
	!	calculations will be charged
	!	.els
	!	.p
	!	There must be one or more General Ledger subject labor accounts
	!	which relate to the specific Overhead Key.  These are identified in
	!	the definition section of the Overhead Table record.
	!	.p
	!	There may be one or more subject labor operations which relate to
	!	the specific Overhead Key.  These are identified in the definition
	!	section of the Overhead Table record.
	!	.p
	!	The purpose of establishing Overhead Keys is to enable the system to
	!	calculate applied overhead, depending upon the labor accounts and/or
	!	labor operations, at varying rates and different methods, and post
	!	those calculations to specified General Ledger accounts and costing
	!	locations.
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_MAST_OVERHEAD/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_MAST_OVERHEAD, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_MAST_OVERHEAD.OBJ;*
	!
	! Author:
	!
	!	12/04/87 - B. Craig Larsen
	!
	! Modification history:
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

	V% = MAIN_WINDOW(PR_MAIN_OVERHEAD_DESC.ID, "")

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
	%INCLUDE "FUNC_INCLUDE:PR_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	EXTERNAL LONG FUNCTION GL_MAIN_CHART
	EXTERNAL LONG FUNCTION PR_MAIN_OVERHEAD_DESC
	EXTERNAL LONG FUNCTION PR_MAIN_OVERHEAD_DEF

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
	! OverHead Description file
	!
	CASE PR_MAIN_OVERHEAD_DESC.ID

		MAINT_GROUP = PR_MAIN_OVERHEAD_DESC(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	!
	! OverHead Definition File
	!
	CASE PR_MAIN_OVERHEAD_DEF.ID

		MAINT_GROUP = PR_MAIN_OVERHEAD_DEF(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
