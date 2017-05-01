1	%TITLE "Sub Account Account Table Maintenance"
	%SBTTL "JC_MAST_ACCOUNT"
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
	!
	! Index:
	!
	! Option:
	!
	!	SB_MAIN_ACCOUNT$HELP
	!
	! Compile:
	!
	!	$ BAS JC_SOURCE:JC_MAST_ACCOUNT/LINE
	!	$ LINK/EXECUTABLE=JC_EXE: JC_MAST_ACCOUNT, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE JC_MAST_ACCOUNT.OBJ;*
	!
	! Author:
	!
	!	07/03/90 - J. Shad Rydalch
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/20/96 - Kevin Handy
	!		Reformat source code
	!
	!	11/21/96 - Kevin Handy
	!		Change references to GL_MAIN_35CHART to
	!		GL_MAIN_CHART.
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

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:SB_WINDOW.INC"

	!
	! Com areas
	! Pass System through all maintainence
	!
	COM (TT_SYSTEM)	SYSTEM$ = 2%

	!
	! External functions
	!
	EXTERNAL LONG FUNCTION MAIN_WINDOW

	%PAGE

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	SYSTEM$ = "JC"

400	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_WINDOW(SB_MAIN_ACCOUNT.ID, "")

 ExitProgram:
	!******************************************************************
	! End of the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19990	END

20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "SOURCE:[SB.OPEN]SB_ACCOUNT.HB"
	MAP (SB_ACCOUNT_ONE)	SB_ACCOUNT_CDD		SB_ACCOUNT_ONE
	MAP (SB_ACCOUNT_TWO)	SB_ACCOUNT_CDD		SB_ACCOUNT_TWO

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"
	%INCLUDE "FUNC_INCLUDE:SB_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	EXTERNAL LONG FUNCTION SB_MAIN_ACCOUNT
	EXTERNAL LONG FUNCTION GL_MAIN_CHART

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE SB_MAIN_ACCOUNT.ID

		SELECT MOPTION
		CASE OPT_RESETDEFAULT
			MVALUE = SYSTEM$ + SB_ACCOUNT_TWO::ACCOUNT
		CASE OPT_SUBWIND
			SELECT MLOOP
			CASE 6%
				MVALUE = SYSTEM$ + SB_ACCOUNT_ONE::ACCOUNT
			CASE ELSE
				MVALUE = SYSTEM$ + SB_ACCOUNT_TWO::ACCOUNT
			END SELECT
		END SELECT

		MAINT_GROUP = SB_MAIN_ACCOUNT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE GL_MAIN_CHART.ID

		MAINT_GROUP = GL_MAIN_CHART(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
