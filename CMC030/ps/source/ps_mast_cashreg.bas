1	%TITLE "Cash Register Description Table"
	%SBTTL "PS_MAST_CASHREG"
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
	!	The ^*Cash Register Description Table\* option
	!	accesses the information
	!	concerning each cash register and location.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!	OE_MAIN_ACCOUNT$HELP
	!
	! Compile:
	!
	!	$ BAS SOURCE:[PS.SOURCE]PS_MAST_CASHREG/LINE
	!	$ LINK/EXEC:PS_EXE PS_MAST_CASHREG,FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PS_MAST_CASHREG.OBJ;*
	!
	! Author:
	!	11/03/91 - Frank F. Starman
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	11/14/95 - Kevin Handy
	!		Format source closer to 80 columns.
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
	EXTERNAL LONG   FUNCTION MAIN_WINDOW

	%PAGE

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

1000	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_WINDOW(PS_MAIN_CASHREG.ID, "")

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
	%INCLUDE "FUNC_INCLUDE:PS_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	EXTERNAL LONG FUNCTION PS_MAIN_CASHREG
	EXTERNAL LONG FUNCTION UTL_MAIN_LOCATION

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT


	CASE PS_MAIN_CASHREG.ID

		MAINT_GROUP = PS_MAIN_CASHREG(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_LOCATION.ID

		MAINT_GROUP = UTL_MAIN_LOCATION(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
