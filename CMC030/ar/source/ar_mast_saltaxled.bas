1	%TITLE "Accounts Receivable Sales Tax Monthly Report File"
	%SBTTL "AR_MAST_SALTAXLED"
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
	!
	! Index:
	!
	! Option:
	!
	!	AR_MAIN_SALTAXLED$HELP
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_MAST_SALTAXLED/LINE
	!	$ LINK/EXECUTABLE=AR_EXE:*.EXE AR_MAST_SALTAXLED, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_MAST_SALTAXLED.OBJ;*
	!
	! Author:
	!
	!	03/10/88 - Aaron Redd
	!
	! Modification history:
	!
	!	03/11/88 - Kevin Handy
	!		Modified to pull Customer/Client/Patient from
	!		control file.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/10/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"

	!
	! Map's
	!
	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.HB"
	MAP	(AR_CONTROL)	AR_CONTROL_CDD	AR_CONTROL


	!
	! Common statements
	!
	COM (CH_AR_CONTROL) AR_CONTROL.CH%

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION MAIN_WINDOW

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

500	!
	! Open up control file and read in the control record
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.OPN"
		GET #AR_CONTROL.CH%, RECORD 1%
	USE
		FILENAME$ = "AR_CONTROL"
		CONTINUE HelpError
	END WHEN

	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_WINDOW(AR_MAIN_SALTAXLED.ID, "")

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
	RESUME ExitProgram

19990	END



20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include statements
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"
	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"

	!
	! External Functions
	!
	EXTERNAL LONG FUNCTION AR_MAIN_SALTAXLED
	EXTERNAL LONG FUNCTION AR_MAIN_35CUSTOM

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE AR_MAIN_SALTAXLED.ID

		MAINT_GROUP = AR_MAIN_SALTAXLED(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AR_MAIN_35CUSTOM.ID

		MAINT_GROUP = AR_MAIN_35CUSTOM(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)
	END SELECT

32767	END FUNCTION
