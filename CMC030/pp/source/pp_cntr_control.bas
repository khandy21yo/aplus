1	%TITLE "Pacific Price Controlling File"
	%SBTTL "PP_CNTR_CONTROL"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1992 BY
	!
	! Computer Management Center
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
	!	The ^*Controlling File\* identifies the System Customer Number,
	!	the AR General Ledger Account, the Host Number, and the Discount
	!	days for the Pacific Pride System.
	!	.b
	!	^*Note:\* After initialization, manual changes should ^*not\* be made to
	!	this file.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	!	PP_MAIN_CONTROL$HELP
	!
	! Compile:
	!
	!	$ BAS PP_SOURCE:PP_CNTR_CONTROL/LINE
	!	$ LINK/EXE=PP_EXE: PP_CNTR_CONTROL,-
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PP_CNTR_CONTROL.OBJ;*
	!
	! Author:
	!
	!	12/18/92 - Dan Perkins
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
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

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:PP_WINDOW.INC"

	COM (CH_PP_CONTROL) &
		PP_CONTROL.CH%

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

	V% = MAIN_WINDOW(PP_MAIN_CONTROL.ID, "")

	!******************************************************************
	! End of the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19990	END



20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:PP_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	EXTERNAL LONG FUNCTION PP_MAIN_CONTROL
	EXTERNAL LONG FUNCTION GL_MAIN_CHART

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE PP_MAIN_CONTROL.ID

		MAINT_GROUP = PP_MAIN_CONTROL(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE GL_MAIN_CHART.ID

		MAINT_GROUP = GL_MAIN_CHART(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
