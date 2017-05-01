1	%TITLE "Customer Balance Maintenance"
	%SBTTL "AR_MAST_CUSBAL"
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
	!	This program allows maintenance of the customer balances for all
	!	"Balance Forward" customers, and the credit limits for all customers.
	!	.b
	!	You should not normally modify the customer balances for the
	!	"Balance Forward" customers using this routine, since any changes made
	!	will NOT be reflected in the General Leger, or in the AR Query.
	!	This program should only be used to initialize the balances when
	!	first installing this system, or for making extra-ordinary changes.
	!	.lm -5
	!
	! Index:
	!	.x Maintain>Balance Forward
	!	.x Balance Forward>Maintain
	!
	! Option:
	!
	!	AR_MAIN_CUSBAL$HELP
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_MAST_CUSBAL/LINE
	!	$ LINK/EXEC:AR_EXE AR_MAST_CUSBAL,FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_MAST_CUSBAL.OBJ;*
	!
	! Author:
	!
	!	02/10/88 - Aaron Redd
	!
	! Modification history:
	!
	!	04/13/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 source standard.
	!		Change AR_MAIN_CUBAL.ID to AR_MAIN_CUSBAL.ID.
	!
	!	10/11/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/10/2000 - Kevin Handy
	!		Use WHEN ERROR IN
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

	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"

	!
	! Maps
	!
	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.HB"
	MAP	(AR_CONTROL)	AR_CONTROL_CDD	AR_CONTROL

	MAP	(CH_AR_CONTROL) AR_CONTROL.CH%

	%PAGE

	ON ERROR GOTO 19000

1000	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	!
	! Open control file
	!
1100	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.OPN"
		GET #AR_CONTROL.CH%, RECORD 1%, REGARDLESS

	USE
		FILENAME$ = "AR_CONTROL"
		CONTINUE HelpError
	END WHEN

	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_WINDOW(AR_MAIN_CUSBAL.ID, "")

	!******************************************************************
	! End of the program
	!******************************************************************
 ExitProgram:
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	FILENAME$ = ""
	RESUME HelpError

 HelpError:
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	GOTO ExitProgram

19990	END



20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Includes
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"
	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	!
	! External Functions
	!
	EXTERNAL LONG FUNCTION AR_MAIN_35CUSTOM
	EXTERNAL LONG FUNCTION AR_MAIN_CUSBAL
	EXTERNAL LONG FUNCTION GL_MAIN_CHART

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE GL_MAIN_CHART.ID
		MAINT_GROUP = GL_MAIN_CHART(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AR_MAIN_35CUSTOM.ID
		MAINT_GROUP = AR_MAIN_35CUSTOM(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AR_MAIN_CUSBAL.ID
		MAINT_GROUP = AR_MAIN_CUSBAL(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
