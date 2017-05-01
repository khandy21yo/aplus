1	%TITLE "Carrier Table "
	%SBTTL "UT_MAST_CARRIER"
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
	!	.lm +5
	!	The ^*Carrier Table\* option
	!	accesses the file where carriers (also referred to
	!	as ship-via) are defined.  This table will be provided with your CMC Software.
	!	.lm -5
	!
	! Index:
	!	.x Table>Carrier
	!	.x Carrier>Table
	!	.y CARRIER
	!	.y SHIPVIA
	!
	! Option:
	!	UT_MAIN_CARRIER$HELP
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UT_MAST_CARRIER/LINE
	!	$ LINK/EXEC:UTL_EXE UT_MAST_CARRIER,FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE UT_MAST_CARRIER.OBJ;*
	!
	! Author:
	!
	!	03/09/90 - Kevin Handy
	!
	! Modification history:
	!
	!	05/21/90 - Frank F. Starman
	!		Added COMMAND help message.
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
	!	Accesses the file where Carriers code are defined.
	!	.b
	!	^*Format: CARRIER\*
	!	.lm -5
	!
	! Index:
	!	.x CARRIER
	!	.y CARRIER
	!	.y SHIPVIA
	!
	! Option:
	!	UT_MAIN_CARRIER$HELP
	!	UT_RPRT_CARRIER$COMMAND
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

	V% = MAIN_WINDOW(UT_MAIN_CARRIER.ID, "")

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

	EXTERNAL LONG FUNCTION UT_MAIN_CARRIER

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE UT_MAIN_CARRIER.ID

		MAINT_GROUP = UT_MAIN_CARRIER(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
