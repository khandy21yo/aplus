1	%TITLE "Material Requisition Register Maintenance"
	%SBTTL "WP_MAST_REQREGISTER"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1991 BY
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
	!	.lm +5
	!	.b
	!	The ^*Material Requisition Register Maintenance\* option
	!	maintains Requisition Register records.
	!	.b
	!	.NOTE
	!	^*Editing records in this file is not advised.  Changes to the records could
	!	cause unexpected results!!\*
	!	.end note
	!	.lm -5
	!
	! Index:
	!	.x Material Requisition Register Maintenance
	!	.x Maintenance>Material Requisition Register
	!	.x Requisition>Register
	!
	! Option:
	!	WP_MAIN_REQREGISTER$HELP
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_MAST_REQREGISTER/LINE
	!	$ LINK/EXE=WP_EXE: WP_MAST_REQREGISTER,-
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE WP_MAST_REQREGISTER.OBJ;*
	!
	! Author:
	!
	!	07/22/91 - Craig Tanner
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/31/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/22/98 - Kevin Handy
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

	%INCLUDE "FUNC_INCLUDE:WP_WINDOW.INC"

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION MAIN_WINDOW

	%PAGE

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

400	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_WINDOW(WP_MAIN_REQREGISTER.ID, "")

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

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:WP_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:JC_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.HB"
	MAP	(WP_REQREGISTER)	WP_REQREGISTER_CDD	WP_REQREGISTER
	MAP	(WP_REQREGISTER_OLD)	WP_REQREGISTER_CDD	WP_REQREGISTER_OLD
	MAP	(WP_REQREGISTER_ONE)	WP_REQREGISTER_CDD	WP_REQREGISTER_ONE

	EXTERNAL LONG FUNCTION WP_MAIN_REQREGISTER
	EXTERNAL LONG FUNCTION JC_MAIN_JOB
	EXTERNAL LONG FUNCTION WP_MAIN_REGLINE
	EXTERNAL LONG FUNCTION UTL_MAIN_LOCATION
	EXTERNAL LONG FUNCTION PD_MAIN_PRODUCT

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE WP_MAIN_REQREGISTER.ID

		MAINT_GROUP = WP_MAIN_REQREGISTER(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE JC_MAIN_JOB.ID

		MAINT_GROUP = JC_MAIN_JOB(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE WP_MAIN_REGLINE.ID

		MAINT_GROUP = WP_MAIN_REGLINE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_LOCATION.ID

		MAINT_GROUP = UTL_MAIN_LOCATION(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE PD_MAIN_PRODUCT.ID

		MAINT_GROUP = PD_MAIN_PRODUCT(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
