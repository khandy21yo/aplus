1	%TITLE "Pack Form Description"
	%SBTTL "UTL_MAST_PACKFORM"
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
	!	The ^*Pack Form Description\* option
	!	accesses the file where the codes and descriptions for the different
	!	kinds of packaging forms are recorded. Packaging form descriptions include:
	!	bale, bag, can, tank, trunk, roll, etc.
	!
	! Index:
	!	.x ANSI Codes>Packaging>Form
	!	.x Packaging>Form>ANSI Codes
	!	.x Table>Packaging>Form
	!	.x Packaging>Form>Table
	!	.x Table>Packaging Form
	!	.x Packaging>Form>Table
	!
	! Option:
	!	UTL_MAIN_PACKFORM$HELP
	!
	! Compile:
	!	$ BAS UTL_SOURCE:UTL_MAST_PACKFORM/LINE
	!	$ LINK/EXE=UTL_EXE: UTL_MAST_PACKFORM,-
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE UTL_MAST_PACKFORM.OBJ;*
	!
	! Author:
	!
	!	10/30/87 - Frank Starman
	!
	! Modification history:
	!
	!	05/21/90 - Frank F. Starman
	!		Added COMMAND help mesage.
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
	!	^*PKFORM\*
	!	.p
	!	Access the file where the codes and descriptions for the different
	!	kinds of packaging forms are recorded.  Packaging form descriptions include:
	!	bale, bag, can, tank, trunk, roll, etc.
	!	.p
	!	^*Format: PKFORM\*
	!
	! Index:
	!	.x PKFORM
	!
	! Option:
	!
	!	UTL_MAIN_PACKFORM$HELP
	!	UTL_RPRT_PACKFORM$COMMAND
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
	EXTERNAL LONG FUNCTION MAIN_WINDOW

	%PAGE

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_WINDOW(UTL_MAIN_PACKFORM.ID, "")

	!******************************************************************
	! End of the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19990	END



20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	EXTERNAL LONG FUNCTION UTL_MAIN_PACKFORM

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE UTL_MAIN_PACKFORM.ID

		MAINT_GROUP = UTL_MAIN_PACKFORM(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
