1	%TITLE "Batch Controlling File"
	%SBTTL "UTL_MAST_BATCH_CONTROL"
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
	!	Access the control file where records are created
	!	which contain information relative to a process such
	!	as posting, closing etc. It is not intended that the
	!	user modify or maintain any record in this file.
	!
	! Index:
	!	.x Controlling File
	!	.x Batch Control>File Maintenance>Utility
	!	.x Utility>Batch Control>File Maintenance
	!
	! Option:
	!	UTL_MAIN_BATCH_CONTROL$HELP
	!	UTL_FUNC_MTRBATCH$HELP
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UTL_MAST_BATCH_CONTROL/LINE
	!	$ LINK/EXECUTABLE=UTL_EXE: UTL_MAST_BATCH_CONTROL, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE UTL_MAST_BATCH_CONTROL.OBJ;*
	!
	! Author:
	!
	!	08/07/87 - Robert Peterson
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
	!	^*BATCH\*
	!	.p
	!	^*Batch\* provides access to the control file where records are created
	!	which contain information relative to a process such
	!	as posting, closing etc. It is not intended that the
	!	user modify or maintain any record in this file.
	!	.p
	!	^*Format: BATCH\*
	!	.p
	!	^*Example:\*
	!	.literal
	!	Menu Command Level> /BATCH
	!	.end literal
	!
	! Index:
	!	.x BATCH
	!
	! Option:
	!	UTL_MAIN_BATCH_CONTROL$HELP
	!	UTL_FUNC_MTRBATCH$COMMAND
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
	EXTERNAL LONG	FUNCTION MAIN_WINDOW

	%PAGE

	!
	! Initialize all the standard stuff through an external call
	!
	CALL READ_INITIALIZE

	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_WINDOW(UTL_MAIN_BATCH_CONTROL.ID, "")

	!******************************************************************
	! End of the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19990	END


20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	EXTERNAL LONG	FUNCTION UTL_MAIN_BATCH_CONTROL

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE UTL_MAIN_BATCH_CONTROL.ID

		MAINT_GROUP = UTL_MAIN_BATCH_CONTROL(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
