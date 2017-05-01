1	%TITLE "Units of Measure"
	%SBTTL "UTL_MAST_MEASURE"
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
	!	Access the file where ANSI codes and descriptions are recorded for all
	!	defined units of measure.
	!	.p
	!	Since the Units of Measure codes provided are internationally recognized
	!	standard ANSI codes, deviation from or modifications to the codes should not be
	!	considered.
	!
	! Index:
	!	.x ANSI Codes>Units of Measure
	!	.x Units of Measure>ANSI Codes
	!	.x Units of Measure>Table
	!	.x Table>Units of Measure
	!	.x Table>UOM
	!	.x UOM>Table
	!	.y ANSI
	!
	! Option:
	!
	!	UTL_MAIN_MEASURE$HELP
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UTL_MAST_MEASURE/LINE
	!	$ LINK/EXE=UTL_EXE: UTL_MAST_MEASURE,-
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE UTL_MAST_MEASURE.OBJ;*
	!
	! Author:
	!
	!	07/24/87 - Frank Starman
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
	!	^*UOM\*
	!	.p
	!	Access the file where ANSI codes and descriptions are recorded for all
	!	defined units of measure.
	!	.p
	!	Since the Units of Measure codes provided are internationally recognized
	!	standard ANSI codes, deviation from or modifications to the codes should not be
	!	considered.
	!	.b
	!	Format: ^*UOM\*
	! Index:
	!	.x UOM
	!	.x Units of Measure
	!	.y ANSI
	! Option:
	!	UTL_MAIN_MEASURE$HELP
	!	UTL_RPRT_MEASURE$COMMAND
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

	V% = MAIN_WINDOW(UTL_MAIN_MEASURE.ID, "")

	!******************************************************************
	! End of the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19990	END



20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	EXTERNAL LONG FUNCTION UTL_MAIN_MEASURE

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE UTL_MAIN_MEASURE.ID

		MAINT_GROUP = UTL_MAIN_MEASURE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
