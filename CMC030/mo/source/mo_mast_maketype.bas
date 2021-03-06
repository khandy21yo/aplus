1	%TITLE "Make Type"
	%SBTTL "MO_MAST_MAKETYPE"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1991, BY
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
	!	Pertinent information concerning the Make Type is
	!	maintained by accessing the ^*Make Type\* option.
	!
	! Index:
	!	.x Make>Type
	!	.x Type>Make
	!
	! Option:
	!
	!	MO_MAIN_MAKETYPE$HELP
	!
	! Author:
	!
	!	02/27/91 - Craig Tanner
	!
	! Compile:
	!
	!	$ BAS MO_SOURCE:MO_MAST_MAKETYPE
	!	$ LINK/EXECUTABLE=MO_EXE:*.EXE MO_MAST_MAKETYPE, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE MO_MAST_MAKETYPE.OBJ;*
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/02/96 - Kevin Handy
	!		Reformat source code.
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

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:MO_WINDOW.INC"

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

	V% = MAIN_WINDOW(MO_MAIN_MAKETYPE.ID, "")

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
	%INCLUDE "FUNC_INCLUDE:MO_WINDOW.INC"

	EXTERNAL LONG FUNCTION MO_MAIN_MAKETYPE

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE MO_MAIN_MAKETYPE.ID

		MAINT_GROUP = MO_MAIN_MAKETYPE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
