1	%TITLE "POS Controlling File"
	%SBTTL "PS_CNTR_CONTROL"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1990 BY
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
	!	The ^*POS Controlling File\* identifies the last order number,
	!	the last purge date, the status flag which indicates the current
	!	activity of the POS system and the last invoice number.
	!	.lm -5
	!
	! Index:
	!	.x Controlling File
	!	.x Last Invoice Number
	!
	! Option:
	!
	!	PS_MAIN_CONTROL$HELP
	!
	! Compile:
	!
	!	$ BAS PS_SOURCE:PS_CNTR_CONTROL/LINE
	!	$ LINK/EXE=PS_EXE: PS_CNTR_CONTROL,-
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PS_CNTR_CONTROL.OBJ;*
	!
	! Author:
	!	11/02/91 - Frank F. Starman
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	11/14/95 - Kevin Handy
	!		Reformat closer to 80 columns.
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

	%INCLUDE "FUNC_INCLUDE:PS_WINDOW.INC"

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

	V% = MAIN_WINDOW(PS_MAIN_CONTROL.ID, "")

	!******************************************************************
	! End of the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19990	END



20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:PS_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PC_WINDOW.INC"
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	EXTERNAL LONG FUNCTION PS_MAIN_CONTROL
	EXTERNAL LONG FUNCTION PC_MAIN_PRCTYPE

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE PS_MAIN_CONTROL.ID

		MAINT_GROUP = PS_MAIN_CONTROL(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE PC_MAIN_PRCTYPE.ID

		MAINT_GROUP = PC_MAIN_PRCTYPE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
