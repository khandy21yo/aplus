1	%TITLE "FOB Codes"
	%SBTTL "UT_MAST_FOB"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1990 BY
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
	!	The ^*FOB\* option
	!	accesses the file where FOB Codes are defined.
	!	.lm -5
	!
	! Index:
	!	.x Table>FOB Codes
	!	.x FOB Codes>Table
	!	.y FOB
	!
	! Option:
	!	UT_MAIN_FOB$HELP
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UT_MAST_FOB/LINE
	!	$ LINK/EXE=UTL_EXE: UT_MAST_FOB, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE UT_MAST_FOB.OBJ;*
	!
	! Author:
	!
	!	05/31/90 - Aaron Redd
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/30/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--

	!++
	! Abstract:COMMAND
	!	^*CARRIER\*
	!	.b
	!	.lm +5
	!	Access the file where Carriers code are defined.
	!	.b
	!	^*Format: CARRIER\*
	!	.lm -5
	!
	! Index:
	!	.x FOB Codes
	!
	! Option:
	!	UT_MAIN_FOB$HELP
	!	UT_RPRT_FOB$COMMAND
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION MAIN_WINDOW

	%PAGE

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************
	CALL READ_INITIALIZE

1000	!*******************************************************************
	! Handle main program
	!*******************************************************************
	V% = MAIN_WINDOW(UT_MAIN_FOB.ID, "")

	!******************************************************************
	! End of the program
	!******************************************************************
 ExitProgram:
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19990	END



20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	EXTERNAL LONG FUNCTION UT_MAIN_FOB

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE UT_MAIN_FOB.ID
		MAINT_GROUP = UT_MAIN_FOB(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
