1	%TITLE "Controlling File"
	%SBTTL "EL_CNTR_CONTROL"
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
	!	The ^*Controlling File\* initializes and shows the status
	!	of the system at the present time. This file indicates where the system
	!	is at the present time.
	!	.lm -5
	!
	! Index:
	!	.x Control File
	!
	! Option:
	!	EL_MAIN_CONTROL$HELP
	!
	! Compile:
	!
	!	$ BAS EL_SOURCE:EL_CNTR_CONTROL/LINE
	!	$ LINK/EXE=EL_EXE: EL_CNTR_CONTROL,-
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE EL_CNTR_CONTROL.OBJ;*
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

	V% = MAIN_WINDOW(EL_MAIN_CONTROL.ID, "")

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

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:EL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	EXTERNAL LONG FUNCTION EL_MAIN_CONTROL
	EXTERNAL LONG FUNCTION EL_MAIN_ACCOUNT
	EXTERNAL LONG FUNCTION GL_MAIN_CHART
	EXTERNAL LONG FUNCTION MAIN_WINDOW

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE EL_MAIN_CONTROL.ID

		MAINT_GROUP = EL_MAIN_CONTROL(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

		SELECT MOPTION

		CASE OPT_OPTLIST
			MVALUE = MVALUE + " accounT"

		CASE OPT_MOREMENU
			SELECT EDIT$(MVALUE, -1%)
			!
			! Line
			!
			CASE "ACCOUNT"
	!++
	! Abstract:ACCOUNT
	!--
			MAINT_GROUP = MAIN_WINDOW(EL_MAIN_ACCOUNT.ID, "")
			END SELECT

		END SELECT

	CASE EL_MAIN_ACCOUNT.ID

		SELECT MOPTION

		CASE OPT_RESETDEFAULT
			MVALUE = "EL"

		CASE OPT_SUBWIND
			MVALUE = "EL"
		END SELECT

		MAINT_GROUP = EL_MAIN_ACCOUNT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE GL_MAIN_CHART.ID

		MAINT_GROUP = GL_MAIN_CHART(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
