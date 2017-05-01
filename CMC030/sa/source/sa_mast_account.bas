1	%TITLE "Sub Account Account Table Maintenance"
	%SBTTL "SA_MAST_ACCOUNT"
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
	!	SA_MAIN_ACCOUNT$HELP
	!
	! Compile:
	!
	!	$ BAS SA_SOURCE:SA_MAST_ACCOUNT/LINE
	!	$ LINK/EXECUTABLE=SA_EXE: SA_MAST_ACCOUNT, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE SA_MAST_ACCOUNT.OBJ;*
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
	!		Include SA_WINDOW.INC instead of SB_WINDOW.INC
	!
	!	10/29/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/22/98 - Kevin Handy
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

	%INCLUDE "FUNC_INCLUDE:SA_WINDOW.INC"

	!
	! Com areas
	! Pass System through all maintainence
	!
	COM (TT_SYSTEM)	SYSTEM$ = 2%

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION MAIN_WINDOW

	%PAGE

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	SYSTEM$ = "SA"

400	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_WINDOW(SA_MAIN_ACCOUNT.ID, "")

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
	MAP (SB_ACCOUNT_ONE)	SB_ACCOUNT_CDD SB_ACCOUNT_ONE
	MAP (SB_ACCOUNT_TWO)	SB_ACCOUNT_CDD SB_ACCOUNT_TWO

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP	(GL_CHART)	GL_CHART_CDD	GL_CHART

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"
	%INCLUDE "FUNC_INCLUDE:SA_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	EXTERNAL LONG FUNCTION SA_MAIN_ACCOUNT
	EXTERNAL LONG FUNCTION GL_MAIN_CHART

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE SA_MAIN_ACCOUNT.ID

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

		MAINT_GROUP = SA_MAIN_ACCOUNT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE GL_MAIN_CHART.ID

		MAINT_GROUP = GL_MAIN_CHART(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
