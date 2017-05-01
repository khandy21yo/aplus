1	%TITLE "Customer Sales Balances"
	%SBTTL "SA_MAST_SALCUST"
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
	!	.b
	!	.lm +5
	!	The ^*Customer Sales Balances\* option allows viewing of the current sales
	!	balances as applied to the customer.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	!	SA_MAIN_SALCUST$HELP
	!	SA_MAIN_BUDGET$HELP
	!
	! Compile:
	!
	!	$ BAS SA_SOURCE:SA_MAST_SALCUST/LINE
	!	$ LINK/EXE=SA_EXE: SA_MAST_SALCUST,-
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE SA_MAST_SALCUST.OBJ;*
	!
	! Author:
	!
	!	06/29/90 - Frank F. Starman
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!		Fix last param to entr_3choice.
	!
	!	10/29/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/26/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/06/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:SA_WINDOW.INC"

	%INCLUDE "SOURCE:[SB.OPEN]SB_CONTROL.HB"
	MAP (SB_CONTROL)	SB_CONTROL_CDD	SB_CONTROL

	!
	! Common Area
	!
	COM	(SA_SALCUST_COM) YYYY$ = 4%

	!
	! External functions
	!

	%PAGE

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE
	DIM SA_SALCUST_FILE$ (100)

	CALL READ_DEVICE("SA_SALCUST", SA_SALCUST.DEV$, STAT%)

	CUR.PERIOD$ = "      "

300	!
	! Open up control file, and grap record
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[SB.OPEN]SB_CONTROL.OPN"
		GET #SB_CONTROL.CH%, KEY #0% EQ "SA", REGARDLESS
		CLOSE SB_CONTROL.CH%
	USE
		CONTINUE 310
	END WHEN

	CUR.PERIOD$ = SB_CONTROL::PERIOD

310	!
	! Query user for year of file
	!
	CALL READ_DEVICE("SA_SALCUST", SA_SALCUST.DEV$, STAT%)

	CALL FIND_FILE(SA_SALCUST.DEV$ + "SA_SALCUST_*.HIS", &
		SA_SALCUST_FILE$(), 16%, "", "")

	SA_SALCUST_FILE% = VAL%(SA_SALCUST_FILE$(0%))

	IF SA_SALCUST_FILE%
	THEN
		SA_SALCUST_FILE$(LOOP%) = &
			MID(SA_SALCUST_FILE$(LOOP%), 12%, 4%) &
				FOR LOOP% = 1% TO SA_SALCUST_FILE%

		TEMP$ = "Customer Sales History Year"

		X% = ENTR_3CHOICE(SCOPE, "", "", SA_SALCUST_FILE$(), "", &
			0%, TEMP$, "", 0%)

		IF X% > 0%
		THEN
			YYYY$ = EDIT$(SA_SALCUST_FILE$(X%), -1%)
			GOTO 390
		END IF
	END IF

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		GOTO ExitProgram

	END SELECT


	!
	! Ask for Year number
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		20%, &
		80%, &
		SMG_SCREEN_DATA% &
	)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, &
		1%, &
		1% &
	)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Year number:", 11%, 30%)

320	!
	! Set up the help message
	!

	SCOPE::PRG_ITEM = "FLD01YEAR"
	!++
	! Abstract:FLD01YEAR
	!	^*Year\*
	!	.b
	!	.lm +5
	!	The ^*Year\* field enters the desired year for
	!	viewing and working.
	!	.b
	!	The format for entry is YYYY.
	!	.lm -5
	!
	! Index:
	!
	!--


	!
	! Assign default year number
	!
	YYYY$ = LEFT$(CUR.PERIOD$, 4%)

	SELECT ENTR_3ENTER(SCOPE, SMG_SCREEN_DATA%, 11%, 43%, YYYY$, -1%, 16%)

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		GOTO ExitProgram

	END SELECT

	YYYY$ = EDIT$(YYYY$, -1%)

	IF LEN(TRM$(YYYY$)) <> 4%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "Please enter the year number in XXXX format", 0%)
		GOTO 320
	END IF

390	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%)


400	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_WINDOW(SA_MAIN_SALCUST.ID, "")

	!******************************************************************
	! End of the program
	!******************************************************************
 ExitProgram:
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")


 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitProgram

19990	END


20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:SA_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	EXTERNAL LONG FUNCTION SA_MAIN_SALCUST
	EXTERNAL LONG FUNCTION AR_MAIN_35CUSTOM
	EXTERNAL LONG FUNCTION GL_MAIN_CHART

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE SA_MAIN_SALCUST.ID

		MAINT_GROUP = SA_MAIN_SALCUST(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AR_MAIN_35CUSTOM.ID

		MAINT_GROUP = AR_MAIN_35CUSTOM(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE GL_MAIN_CHART.ID

		MAINT_GROUP = GL_MAIN_CHART(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
