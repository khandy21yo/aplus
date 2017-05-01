1	%TITLE "Maintain String Reports Setups"
	%SBTTL "UTL_MAST_STRING_PRINT"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1989 BY
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
	!	Provides the means to access the file where a variety of report string
	!	instructions can be established.  Once a report string has been setup, a
	!	number of selected reports can be printed in a predetermined sequence with
	!	a single instruction.
	!
	! Index:
	!	.x Maintain>String Reports
	!	.x String Reports>Maintain
	!	.x String Reports>Setups
	!
	! Option:
	!	UTL_MAIN_STRING_PRINT$HELP
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UTL_MAST_STRING_PRINT/LINE
	!	$ LINK/EXEC:UTL_EXE UTL_MAST_STRING_PRINT,FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE UTL_MAST_STRING_PRINT.OBJ;*
	!
	! Author:
	!
	!	01/13/88 - Kevin Handy
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--
	%PAGE

	!++
	! Abstract:COMMAND
	!	^*STRING\*
	!	.p
	!	^*String\* provides the means to access the file where a variety of report
	!	string instructions can be established. Once a report string has been setup, a
	!	number of selected reports can be printed in a predetermined sequence with
	!	a single instruction.
	!	.p
	!	^*Format:STRING\*
	!	.p
	!	^*Example:\*
	!	.literal
	!	Menu Command Level> /STRING
	!	.end literal
	!
	! Index:
	!	.x Maintain>String Reports
	!	.x String Reports>Maintain
	!	.x String Reports>Setups
	!
	! Option:
	!	UTL_MAIN_STRING_PRINT$HELP
	!	UTL_RPRT_STRING_PRINT$COMMAND
	!	UTL_SPEC_STRING_PRINT$COMMAND
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Map's
	!
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

	V% = MAIN_WINDOW(UTL_MAIN_STRING_PRINT.ID, "")

	!******************************************************************
	! End of the program
	!******************************************************************

 ExitProgram:
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19990	END



20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Includes
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	!
	! External Functions
	!
	EXTERNAL LONG FUNCTION UTL_MAIN_STRING_PRINT
	EXTERNAL LONG FUNCTION UTL_MAIN_PRINT

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE UTL_MAIN_STRING_PRINT.ID
		MAINT_GROUP = UTL_MAIN_STRING_PRINT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_PRINT.ID
		MAINT_GROUP = UTL_MAIN_PRINT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
