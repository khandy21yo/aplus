1	%TITLE "Device File"
	%SBTTL "UTL_MAST_DEVICE"
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
	!	The ^*Device File\* option
	!	maintains the protection and
	!	destination for user data files.
	!
	! Index:
	!	.x Device>File
	!	.x Protection>File
	!	.x Table>Device
	!	.x Device>Table
	!
	! Option:
	!
	!	UTL_MAIN_DEVICE$HELP
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UTL_MAST_DEVICE/LINE
	!	$ LINK/EXEC=UTL_EXE: UTL_MAST_DEVICE, FUNC_LIB:CMCLINK/OPTION
	!	$ DEL UTL_MAST_DEVICE.OBJ;*
	!
	! Author:
	!
	!	07/25/89 - Aaron Redd
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
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
	!	^*DEVICE\*
	!	.p
	!	^*Device\* maintains the protection and destination for user data files.
	!	.p
	!	Format: ^*DEVICE\*
	!	.p
	!	^*Example:\*
	!	.literal
	!	Menu Command Level> /DEVICE
	!	.end literal
	!
	! Index:
	!	.x DEVICE
	!	.x Device File
	!
	! Option:
	!	UTL_MAIN_DEVICE$HELP
	!	UTL_RPRT_DEVICE$COMMAND
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	%PAGE

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************
	!
	! Initialize the standard stuff through an external call
	!
	CALL READ_INITIALIZE

	!*******************************************************************
	! Handle main program
	!*******************************************************************
	V% = MAIN_WINDOW(UTL_MAIN_DEVICE.ID, "")

 ExitProgram:
	!******************************************************************
	! End of the program
	!******************************************************************
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19990	END



20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:TK_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	!
	! CDD inclusions
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION	TK_MAIN_FILEDICT
	EXTERNAL LONG	FUNCTION	UTL_MAIN_DEVICE

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	!
	! Get information from the Master file listing
	!
	CASE TK_MAIN_FILEDICT.ID

		MAINT_GROUP = TK_MAIN_FILEDICT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	!
	! Device file maintenance routine
	!
	CASE UTL_MAIN_DEVICE.ID

		MAINT_GROUP = UTL_MAIN_DEVICE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
