1	%TITLE "Accounts Payable Period File"
	%SBTTL "AP_MAST_37CLOSE"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 2000 BY
	!
	! Software Solutions, Inc.
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
	! Software Solutions, Inc.
	!
	! Software Solutions, Inc. assumes no responsibility for the
	! use or reliability of its software on equipment which is
	! not supported by Software Solutions, Inc.
	!
	!++
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	.lm -5
	!
	! Index:
	!	.x Close Ledger>Maintenance
	!	.x Maintain>Close Ledger
	!
	! Option:
	!	AP_MAST_37CLOSE$PERIOD
	!	AP_MAIN_30CLOSE$HELP
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_MAST_37CLOSE/LINE
	!	$ LINK/EXECUTABLE=AP_EXE: AP_MAST_37CLOSE, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_MAST_37CLOSE.OBJ;*
	!
	! Author:
	!
	!	07/05/2000 - Kevin Handy
	!
	! Modification history:
	!
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

	%INCLUDE "FUNC_INCLUDE:AP_WINDOW.INC"

	%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL.HB"
	MAP (AP_CONTROL) AP_CONTROL_CDD AP_CONTROL

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions
	!
	COM (CH_AP_37CLOSE) &
		AP_37CLOSE.CH%, &
		AP_37CLOSE.READONLY%, &
		YYYY_PP$ = 6%

	!
	! Dimension statements
	!
	DIM AP_37CLOSE_FILE$(1000%)

	%PAGE

100	!******************************************************************
	! Initialization section - Prepare to do anything
	!******************************************************************

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

	!
	! Initialize all the standard stuff through an external call
	!
	CALL READ_INITIALIZE

	CALL READ_DEVICE("AP_37CLOSE", AP_37CLOSE.DEV$, STAT%)


200	!******************************************************************
	! Select GL Period file  (Which YYYY and which PP?)
	!******************************************************************


	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL.OPN"
		GET #AP_CONTROL.CH%, RECORD 1%, REGARDLESS
		CLOSE #AP_CONTROL.CH%
	USE
		AP_CONTROL::YEAR = "000000"
		AP_CONTROL::LASTPERCLOSE = 0%
	END WHEN

	YYYY_PP$ = AP_CONTROL::YEAR + FORMAT$(AP_CONTROL::LASTPERCLOSE, "<0>#")

300	!
	! Get information needed to open GL Period file (AP_37CLOSE)
	!
	CALL FIND_FILE(AP_37CLOSE.DEV$ + "AP_37CLOSE_*.LED", AP_37CLOSE_FILE$(), &
		16%, "", "")

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram
	END SELECT

	AP_37CLOSE_FILE% = VAL%(AP_37CLOSE_FILE$(0%))

	IF AP_37CLOSE_FILE%
	THEN
		AP_37CLOSE_FILE$(LOOP%) = &
			MID(AP_37CLOSE_FILE$(LOOP%), 12%, 6%) &
			FOR LOOP% = 1% TO AP_37CLOSE_FILE%

		TEMP$ = "GL Period Files"

		X% = ENTR_3CHOICE(SCOPE, "", "", AP_37CLOSE_FILE$(), "", &
			0%, TEMP$, "", 0%)

		IF X% > 0%
		THEN
			YYYY_PP$ = EDIT$(AP_37CLOSE_FILE$(X%), -1%)
			GOTO 390
		END IF
	END IF

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		GOTO ExitProgram

	END SELECT

	!
	! Ask for period
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		18%, &
		80%, &
		SMG_SCREEN_DATA% &
	)

310	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"GL Period File Maintenance", 10%, 24%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Period: " + FORMAT$(TEMP_PP%, "<0>#"), &
		13%, 31%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, &
		1%, &
		1% &
	)

320	!
	! Query user for year of file
	!
	SCOPE::PRG_ITEM = "FLD001"

	!++
	!
	! Abstract:FLD001
	!	^*Year File Maintenance\*
	!	.b
	!	.lm +5
	!	To accept the ^*Year\* shown on the screen, press ^*Return\*. To enter a
	!	different ^*Year\*, type the year that corresponds to the file which is to
	!	be accessed and press ^*Return\*.
	!	.b
	!	The format for entry is YYYY.
	!	.lm 5
	!
	! Index:
	!
	!--

	SELECT ENTR_3ENTER(SCOPE, SMG_SCREEN_DATA%, &
		13%, 39%, YYYY_PP$, -1%, 0%)

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE SMG$K_TRM_UP
		GOTO 320

	CASE SMG$K_TRM_DOWN
		GOTO 330

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
		! Good key

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 320
	END SELECT

	IF LEN(EDIT$(YYYY_PP$, -1%)) <> 6%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Please enter the Period in YYYYPP format", 0%)
		GOTO 320
	END IF

330	!

390	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%)

	%PAGE

1000	!******************************************************************
	! Handle the main function
	!******************************************************************

	V% = MAIN_WINDOW(AP_MAIN_37CLOSE.ID, "")

	!******************************************************************
	! Exit AP_MAST_37CLOSE
	!******************************************************************

 ExitProgram:
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	FILENAME$ = ""
	RESUME HelpError

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	GOTO ExitProgram

19990	!******************************************************************
	! End of AP_MAST_37CLOSE
	!******************************************************************
	END


20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:AP_WINDOW.INC"

	!
	! External functions
	!
	EXTERNAL LONG FUNCTION GL_MAIN_CHART
	EXTERNAL LONG FUNCTION AP_MAIN_37CLOSE
	EXTERNAL LONG FUNCTION AP_MAIN_1099_TABLE
	EXTERNAL LONG FUNCTION AP_MAIN_VENDOR

	!
	! CDD inclusions
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	!
	! Process the Chart of Accounts maintenance window
	!
	CASE GL_MAIN_CHART.ID

		MAINT_GROUP = GL_MAIN_CHART(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	!
	! Process the Close Ledger maintenance window
	!
	CASE AP_MAIN_37CLOSE.ID

		MAINT_GROUP = AP_MAIN_37CLOSE(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE AP_MAIN_1099_TABLE.ID

		MAINT_GROUP = AP_MAIN_1099_TABLE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AP_MAIN_VENDOR.ID

		MAINT_GROUP = AP_MAIN_VENDOR(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	!******************************************************************
	! End of MAINT_GROUP function
	!******************************************************************
	END FUNCTION
	!+-+-+
	!++
	! Abstract:FLDPERIOD
	!	^*GL Period\*
	!	.b
	!	.lm +5
	!	If the ^*Period\* displayed on the screen is the
	!	correct period, it may be accepted by pressing ^*Return\*.
	!	.b
	!	To enter a different ^*Period\*, type the two (02) digit code
	!	that corresponds to the file which is to be accessed and press ^*Return\*.
	!	.lm -5
	!
	! Index:
	!
	!--
	!+-+-+
	!++
	! Abstract:PERIOD
	!	^*GL Period File Maintenance\*
	!	.b
	!	.lm +5
	!	If the ^*Period\* displayed on the screen is the
	!	correct period, it may be accepted by pressing ^*Return\*.
	!	.b
	!	To enter a different ^*Period\*, type the two (02) digit code
	!	that corresponds to the file which is to be accessed and press ^*Return\*.
	!	.lm -5
	!
	! Index:
	!
	!--
	!+-+-+
	!++
	! Abstract:YEAR
	!	^*GL Year File Maintenance\*
	!	.b
	!	.lm +5
	!	To accept the ^*Year\* shown on the screen, press ^*Return\*. To enter a
	!	different ^*Year\*, type the year that corresponds to the file which is to
	!	be accessed and press ^*Return\*.
	!	.lm -5
	!
	! Index:
	!
	!--
