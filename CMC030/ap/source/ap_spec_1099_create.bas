1	%TITLE "Create 1099 History File"
	%SBTTL "AP_SPEC_1099_CREATE"
	%IDENT "V3.6a Calico"

	!
	!	COPYRIGHT (C) 1987, 1988 BY
	!	Computer Management Center, Inc.
	!	Idaho Falls, Idaho.
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
	!	The ^*Create 1099 Work File from History\* option
	!	creates the subject file by copying the appropriate records
	!	from the Accounts Payable History (Closed) and the Accounts Payable
	!	open files.
	!	.b
	!	Since Form 1099 information must be reported on a calendar
	!	year basis, the accounting periods selected in creating the 1099
	!	Work File must begin with the period corresponding with January
	!	and end with the period corresponding with December of the selected
	!	calendar year.
	!	.b
	!	Only vendors that have the 1099 flag in the vendor file set to ^*Y\*
	!	will be added to the 1099 work file. Create will read the AP open
	!	file and then the AP closed file. If the retention period in the AP
	!	control file has been set to less than the number of periods in the
	!	1099 year then some of the history may not be stored. Also, if the
	!	retain 1099 history only has been set, then only vendors that have
	!	the 1099 flag in the vendor file will be stored in the history file.
	!	If the 1099 flag in the vendor file is changed, then all data before
	!	that point in time will not be in the history file.
	!	.lm -5
	!
	! Index:
	!	.x 1099 File Creation
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_SPEC_1099_CREATE/LINE
	!	$ LINK/EXECUTABLE=AP_EXE: AP_SPEC_1099_CREATE, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_SPEC_1099_CREATE.OBJ;*
	!
	! Author:
	!
	!	12/23/86 - Kevin Handy
	!
	! Modification history:
	!
	!	02/09/89 - Kevin Handy
	!		Modified for changes in ENTR_ENTER.
	!
	!	01/10/91 - Craig Tanner
	!		Changed where FILENAME$ = "AP_1099_YYYY" to =
	!		"AP_1099_" + YEAR_1099$ in error handler.
	!
	!	06/06/91 - Kevin Handy
	!		Unwound error trapping.
	!
	!	01/27/92 - Kevin Handy
	!		Modified so that there must be a "Y" in the
	!		1099 flag, so that blanks will not be allocated
	!		as 1099's.
	!
	!	01/27/91 - Kevin Handy
	!		Modified to fix bug in testing for previous
	!		vendor.  Would look up in master file only
	!		if same vendor as last time.
	!
	!	03/12/92 - Kevin Handy
	!		Removed duplicate error trapping (check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	12/14/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!		Change RIGHT(NUM1$()) to FORMAT$().
	!
	!	10/09/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	01/31/96 - Kevin Handy
	!		Clean up source code.
	!		Fix bug id "To Period", was displaying period as
	!		"%1996 01" instead of "199601"
	!
	!	05/21/97 - Kevin Handy
	!		Use integer in #key
	!
	!	08/23/97 - Kevin Handy
	!		Use 'val%' instead of 'val'
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/16/99 - Kevin Handy
	!		Fix parameters to unsolicited input
	!
	!	09/13/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!		Use LIB$DELETE_FILE instead of KILL
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:AP_WINDOW.INC"

	!
	! Map statements
	!
	%INCLUDE "SOURCE:[AP.OPEN]AP_1099_YYYY.HB"
	MAP (AP_1099_YYYY)	AP_1099_YYYY_CDD	AP_1099_YYYY

	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.HB"
	MAP (AP_OPEN)		AP_OPEN_CDD	AP_OPEN

	%INCLUDE "SOURCE:[AP.OPEN]AP_CLOSE.HB"
	MAP (AP_CLOSE)		AP_CLOSE_CDD	AP_CLOSE

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP (AP_VENDOR)	AP_VENDOR_CDD	AP_VENDOR

	%INCLUDE "SOURCE:[AP.OPEN]AP_1099_TABLE.HB"
	MAP (AP_1099_TABLE) AP_1099_TABLE_CDD AP_1099_TABLE

	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	MAP (GL_PERIOD)		GL_PERIOD_CDD	GL_PERIOD

	MAP (DP_OUTP_XUNSOL) RRR_FLAG%

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	MAP (CH_AP_1099_TABLE) &
		AP_1099_TABLE.CH%

	!
	! External functions
	!
	EXTERNAL LONG		FUNCTION MAIN_WINDOW
	EXTERNAL LONG		OUTP_XUNSOL ! (It's really an AST routine)

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	!***************************************************************
	! Open all of the files
	!***************************************************************

300	!
	! Open vendor master file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.OPN"
	USE
		FILENAME$ = "AP_VENDOR"
		CONTINUE HelpError
	END WHEN

310	!
	! Open AP open file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.OPN"
	USE
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

320	!
	! Open AP close file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_CLOSE.OPN"
	USE
		CONTINUE 330 IF ERR = 5%
		FILENAME$ = "AP_CLOSE"
		CONTINUE HelpError
	END WHEN

330	!
	! Figure out what in the world needs done (a whole lot)
	!
	GL_ENDPERIOD% = 13%
	GL_BEGPERIOD% = 1%

	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.OPN"
		GET #GL_PERIOD.CH%, RECORD 1%
		CLOSE GL_PERIOD.CH%
	USE
		CONTINUE 340 IF ERR = 5%
		FILENAME$ = "GL_PERIOD"
		CONTINUE HelpError
	END WHEN

	GL_BEGPERIOD% = GL_PERIOD::NEWYEAR
	GL_ENDPERIOD% = GL_BEGPERIOD% - 1%
	GL_ENDPERIOD% = GL_PERIOD::FPFY &
		IF GL_ENDPERIOD% < 1%

340	!
	! Ask for year
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

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "1099 Year:", 11%, 30%)

350	SCOPE::PRG_ITEM = "FLD01YEAR"
	!++
	! Abstract:FLD01YEAR
	!	^*1099 Year\*
	!	.b
	!	.lm +5
	!	The ^*Year\* identifies the 1099 work file.  The
	!	format for entry is YYYY.
	!	.lm -5
	!
	! Index:
	!	.x 1099 Year
	!
	!--
	!++
	! Abstract:YEAR
	!	^*Year\*
	!	.b
	!	.lm +5
	!	The ^*Year\* field refers to the year for which the report will be created.
	!	.b
	!	The format for entry is YYYY.
	!	.lm -5
	!
	! Index:
	!
	!--

	INP$ = LEFT(DATE_TODAY, 4%)

	SELECT ENTR_3ENTER(SCOPE, SMG_SCREEN_DATA%, 11%, 43%, INP$, -1%, 0%)

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		GOTO ExitProgram

	END SELECT

	YEAR_1099$ = EDIT$(INP$, -1%)

	YEAR_1099$ = LEFT(DATE_TODAY, 2%) + INP$ &
		IF LEN(INP$) = 2%

	IF LEN(YEAR_1099$) <> 4%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Please enter the 1099 register year in YYYY format", &
			0%)
		GOTO 350
	END IF

360	SCOPE::PRG_ITEM = "FLD02DEF"
	!++
	! Abstract:FLD02DEF
	!	^*Default 1099 Code\*
	!	.b
	!	.lm +5
	!	The ^*Default 1099 Code\* provides a default if a 1099 code
	!	is not found as the 1099 annual work file is created.  A 1099
	!	code is used to identify which box an amount will print in on
	!	the 1099 form.
	!	.b
	!	For a complete list of 1099 codes see the 1099 table in the
	!	accounts payable table maintenance section.
	!	.lm -5
	!
	! Index:
	!	.x Default>Create 1099
	!	.x Create 1099>Default
	!
	!--
	!++
	! Abstract:1099_DEFAULT
	!	^*1099 Default\*
	!	.b
	!	.lm +5
	!	The ^*1099 Default\* refers to the default code of the 1099 report to be
	!	printed.
	!	.lm -5
	!
	! Index:
	!
	!--
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"1099 Default        ", 11%, 30%)

	INP$ = "  "

	SELECT ENTR_3ENTER(SCOPE, SMG_SCREEN_DATA%, 11%, 43%, INP$, -1%, 0%)

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		GOTO ExitProgram

	END SELECT

	DEFAULT_1099$ = INP$

370	!
	! Open 1099 History file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_1099_YYYY.OPN"
	USE
		CONTINUE 390 IF ERR = 5%
		FILENAME$ = "AP_1099_" + YEAR_1099$
		CONTINUE HelpError
	END WHEN

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"File already exists.  Recreate file (Y/n) ", 11%, 10%)

380	SCOPE::PRG_ITEM = "KILL"
	!++
	! Abstract:KILL
	!	^*Recreate\*
	!	.b
	!	.lm +5
	!	The ^*Recreate\* field allows for user decision to recreate or abandon the
	!	creation if a report for the specified conditions already exists.
	!	.lm -5
	!
	! Index:
	!	.x Recreate
	!
	!--

	INP$ = "Y"

	SELECT ENTR_3ENTER(SCOPE, SMG_SCREEN_DATA%, 11%, 54%, INP$, -1%, 16%)

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		GOTO ExitProgram

	END SELECT

	IF INP$ <> "Y"
	THEN
		GOTO ExitProgram
	END IF

	SMG_STATUS% = LIB$DELETE_FILE(AP_1099_YYYY.DEV$ + "AP_1099_" + &
		YEAR_1099$ + ".HIS;*")

390	!
	! Create 1099 History file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_1099_YYYY.CRE"
	USE
		FILENAME$ = "AP_1099_" + YEAR_1099$
		CONTINUE HelpError
	END WHEN

400	!
	! Open 1099 Table file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_1099_TABLE.OPN"
	USE
		FILENAME$ = "AP_1099_TABLE"
		CONTINUE HelpError
	END WHEN

500	!
	! Paint the background
	!
	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%)

	BEG_PERIOD$ = YEAR_1099$ + FORMAT$(GL_BEGPERIOD%, "<0>#")

	WORK_YEAR$ = YEAR_1099$
	IF GL_BEGPERIOD% > 1%
	THEN
		WORK_YEAR$ = FORMAT$(VAL%(YEAR_1099$) + 1%, "<0>###")
	END IF

	END_PERIOD$ = WORK_YEAR$ + FORMAT$(GL_ENDPERIOD%, "<0>#")

	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		18%, &
		78%, &
		SMG_SCREEN_DATA%, &
		SMG$M_BORDER &
	)

	SMG_STATUS% = SMG$LABEL_BORDER &
	( &
		SMG_SCREEN_DATA%, &
		"Create 1099 History " + TRM$(SCOPE::PRG_COMPANY), &
		SMG$K_TOP &
	)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Beginning year and period  " + &
		BEG_PERIOD$, 05%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Ending year and period     " + &
		END_PERIOD$, 07%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Vendor #", 9%, 2%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, &
		2%, &
		2% &
	)

510	SCOPE::PRG_ITEM = "FLD04PER"
	!++
	! Abstract:FLD04PER
	!	^*Beginning year and period\*
	!	.b
	!	.lm +5
	!	The ^*Beginning year and Period\* field
	!	^&must\& be the period of the user's fiscal year
	!	which coincides with January of the selected year.
	!	.b
	!	The format for entry is YYYYPP.
	!	.lm -5
	!
	! Index:
	!	.x Beginning year and period
	!	.x Year and period>Beginning
	!
	!--
	!++
	! Abstract:PERIOD
	!	^*Beginning year and period\*
	!	.b
	!	.lm +5
	!	The ^*Beginning year and period\* field
	!	^&must\& be the period of the
	!	user's fiscal year which coincides with January of the selected
	!	year.
	!	.b 2
	!	^*Ending year and period\*
	!	.b
	!	The ^*Ending year and period\* field
	!	^&must\& be the period of the user's fiscal year
	!	which coincides with December of the selected year.
	!	.lm -5
	!
	! Index:
	!
	!--

	BEG_PERIOD$ = ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, "5;29", &
		"Change ", BEG_PERIOD$, 0%, "'E", BEG_PERIOD$)

	SELECT SCOPE::SCOPE_EXIT

	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

	CASE SMG$K_TRM_DOWN
		GOTO 520

	!
	! Good keys
	!
	CASE 0%, 10%, 12%, 13%, 87%, 73%, 65%, &
		69%, 70%, 87%, SMG$K_TRM_DO

	!
	! Bad Keys
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 510

	END SELECT

520	SCOPE::PRG_ITEM = "FLD05PER"
	!++
	! Abstract:FLD05PER
	!	^*Ending year and period\*
	!	.b
	!	.lm +5
	!	The ^*Ending year and period\* field
	!	^&must\& be the period of the user's fiscal year
	!	which coincides with December of the selected year.
	!	.b
	!	The format for entry is YYYYPP.
	!	.lm -5
	!
	! Index:
	!
	!--
	END_PERIOD$ = ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, "7;29", "Change ", &
		END_PERIOD$, 0%, "'E", END_PERIOD$)

	SELECT SCOPE::SCOPE_EXIT

	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

	CASE SMG$K_TRM_UP
		GOTO 510

	!
	! Good keys
	!
	CASE 0%, 10%, 12%, 13%, 87%, 73%, 65%, &
		69%, 70%, 87%, SMG$K_TRM_DO

	!
	! Bad Keys
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 520

	END SELECT

	%PAGE

600	SCOPE::PRG_ITEM = "CONFIRM"
	!++
	! Abstract:CONFIRM
	!	^*Confirm\*
	!	.b
	!	.lm +5
	!	Confirm 1099 creation process by pressing a ^*Y\* for yes or
	!	an ^*N\* for no.
	!	.lm -5
	!
	! Index:
	!	.x Confirm
	!
	!--
	INP$ = ENTR_3YESNO(SCOPE, SCOPE::SMG_OPTION, "", &
		"Confirm Creating 1099 History " + &
		YEAR_1099$ + " - then press <Do> ", "N", 0%, "", "")

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, SPACE$(80%), 1%, 1%)

	IF INP$ <> "Y"
	THEN
		CALL SUBR_3EXITPROGRAM(SCOPE, "", "")
	END IF

	%PAGE

2000	!******************************************************************
	! Create 1099 history file
	!******************************************************************

	TEST_VENNUM$ = ""

	!
	! Assign something impossible
	!
	TRANKEY$ = "ZZZZZZZZZZZZ"

	ADD_TO_1099% = 0%

	!
	! Set up to trap interrupt
	!
	SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
		LOC(OUTP_XUNSOL) BY VALUE, LOC(SCOPE::SMG_KBID) BY VALUE)

	RRR_FLAG% = 0%

	WHEN ERROR IN
		RESET #AP_OPEN.CH%
	USE
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

	CALL ENTR_3MESSAGE(SCOPE, "Reading AP open file", 1%)

2020	!
	! Main loop starts here
	!

	!
	! Set help to program help
	!
	SCOPE::PRG_ITEM = "HELP"

	!
	! Handle any special junk in RRR_FLAG%
	!
	SELECT RRR_FLAG%

	!
	! Repaint screen
	!
	CASE SMG$K_TRM_F11, SMG$K_TRM_CTRLW
		SMG_STATUS% = SMG$REPAINT_SCREEN(SCOPE::SMG_PBID)
		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)

	!
	! Help
	!
	CASE SMG$K_TRM_HELP
		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)
		CALL HELP_34MESSAGE(SCOPE, "", SCOPE::PRG_IDENT, &
			SCOPE::PRG_PROGRAM, "", SCOPE::PRG_ITEM)
		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)

	!
	! Interupt
	!
	CASE SMG$K_TRM_F6, SMG$K_TRM_F20
		SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

		CALL MENU_3INTERRUPT(SCOPE)

		SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
			LOC(OUTP_XUNSOL) BY VALUE, &
			LOC(SCOPE::SMG_KBID) BY VALUE)

	END SELECT

	RRR_FLAG% = 0%

	!
	! Get ap record
	!
	WHEN ERROR IN
		GET #AP_OPEN.CH%
	USE
		CONTINUE 3000 IF ERR = 11%
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

2025	IF TEST_VENNUM$ <> AP_OPEN::VENNUM
	THEN
		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, AP_OPEN::VENNUM, &
			09%, 10%)

		ADD_TO_1099% = 0%

		WHEN ERROR IN
			GET #AP_VENDOR.CH%, &
				KEY #0% EQ AP_OPEN::VENNUM, &
				REGARDLESS
		USE
			CONTINUE 2030 IF ERR = 155%
			FILENAME$ = "AP_VENDOR"
			CONTINUE HelpError
		END WHEN

		ADD_TO_1099% = -1% &
			IF EDIT$(AP_VENDOR::FLG1099, -1%) = "Y"

	END IF

2030	TEST_VENNUM$ = AP_OPEN::VENNUM

	GOTO 2040 &
		IF ADD_TO_1099% = 0%

	IF AP_OPEN::TRANKEY <> TRANKEY$
	THEN
		INVAMT = AP_OPEN::INVAMT
		AMT_1099 = AP_OPEN::AMT_1099
		CODE_1099$ = AP_OPEN::CODE_1099

	END IF

2035	TRANKEY$ = AP_OPEN::TRANKEY

	UPDATED$ = LEFT(AP_OPEN::UPDATED, 6%)

	IF UPDATED$ >= BEG_PERIOD$ AND UPDATED$ <= END_PERIOD$
	THEN
		IF AP_OPEN::CKAMT <> 0.0
		THEN
			IF CODE_1099$ = ""
			THEN
				SCOPE::PRG_ITEM = "1099_CODE"

				SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
					"Vendor # " + &
					AP_OPEN::VENNUM + " Ck # " + &
					AP_OPEN::CKNUM + " Date " + &
					PRNT_DATE(AP_OPEN::CKDAT, 8%) + " Ck Amount " + &
					FORMAT$(AP_OPEN::CKAMT, "#,###,###.##"), &
					11%, 2%)

				CODE_1099$ = DEFAULT_1099$

 CodeMissOpen:
				SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

				CODE_1099$ = ENTR_3STRING(SCOPE, &
					SCOPE::SMG_OPTION, "", &
					"Missing 1099 Code - Enter " + &
					"correct code - then press <Do> ", &
					CODE_1099$, 0%, "'E", CODE_1099$)

				SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
					LOC(OUTP_XUNSOL) BY VALUE, &
					LOC(SCOPE::SMG_KBID) BY VALUE)

				SELECT SCOPE::SCOPE_EXIT
				!
				! List Choices
				!
				CASE SMG$K_TRM_F14
					SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)
					IF (MAIN_WINDOW(AP_MAIN_1099_TABLE.ID, "V0") = 1%)
					THEN
						CODE_1099$ = &
							AP_1099_TABLE::CODE
					END IF

				SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
					LOC(OUTP_XUNSOL) BY VALUE, &
					LOC(SCOPE::SMG_KBID) BY VALUE)

					GOTO CodeMissOpen

				!
				! Control c
				!
				CASE 3%
					GOTO CodeMissOpen

				!
				! Exit key
				!
				CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
					GOTO ExitProgram

				END SELECT

				SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
					SPACE$(78%), 11%, 2%)

				SMG_STATUS% = &
					SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
			END IF

			IF AMT_1099 = 0.0
			THEN
				AMT_1099 = INVAMT
			END IF

			FACTOR = 1.0
			FACTOR = AP_OPEN::CKAMT / INVAMT &
				IF INVAMT <> 0.0

			AP_1099_YYYY::VENNUM = AP_OPEN::VENNUM
			AP_1099_YYYY::CODE = CODE_1099$
			AP_1099_YYYY::TRANKEY = AP_OPEN::TRANKEY
			AP_1099_YYYY::INVNUM = AP_OPEN::INVNUM
			AP_1099_YYYY::INVDAT = AP_OPEN::INVDAT
			AP_1099_YYYY::CKNUM = AP_OPEN::CKNUM
			AP_1099_YYYY::CKDAT = AP_OPEN::CKDAT
			AP_1099_YYYY::AMT1099 = &
				FUNC_ROUND(AMT_1099 * FACTOR, 2%)
			AP_1099_YYYY::CKAMT = AP_OPEN::CKAMT

			PUT #AP_1099_YYYY.CH%
		END IF
	END IF

2040	GOTO 2020

	%PAGE

3000	!******************************************************************
	! Read close file
	!******************************************************************

	TEST_VENNUM$ = ""

	!
	! Assign something impossible
	!
	TRANKEY$ = "ZZZZZZZZZZZZ"

	ADD_TO_1099% = 0%

	WHEN ERROR IN
		RESET #AP_CLOSE.CH%
	USE
		CONTINUE 5000 IF ERR = 11%
		FILENAME$ = "AP_CLOSE"
		CONTINUE HelpError
	END WHEN

	CALL ENTR_3MESSAGE(SCOPE, "Reading AP closing file", 1%)

3020	!
	! Main loop starts here
	!

	!
	! Set help to program help
	!
	SCOPE::PRG_ITEM = "HELP"

	!
	! Handle any special junk in RRR_FLAG%
	!
	SELECT RRR_FLAG%

	!
	! Repaint screen
	!
	CASE SMG$K_TRM_F11, SMG$K_TRM_CTRLW
		SMG_STATUS% = SMG$REPAINT_SCREEN(SCOPE::SMG_PBID)
		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)

	!
	! Help
	!
	CASE SMG$K_TRM_HELP
		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)
		CALL HELP_34MESSAGE(SCOPE, "", SCOPE::PRG_IDENT, &
			SCOPE::PRG_PROGRAM, "", SCOPE::PRG_ITEM)
		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)

	!
	! Interupt
	!
	CASE SMG$K_TRM_F6, SMG$K_TRM_F20
		SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

		CALL MENU_3INTERRUPT(SCOPE)

		SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
			LOC(OUTP_XUNSOL) BY VALUE, &
			LOC(SCOPE::SMG_KBID) BY VALUE)

	END SELECT

	RRR_FLAG% = 0%

	!
	! Get ap close record
	!
	WHEN ERROR IN
		GET #AP_CLOSE.CH%
	USE
		CONTINUE 5000 IF ERR = 11%
		FILENAME$ = "AP_CLOSE"
		CONTINUE HelpError
	END WHEN

3025	IF TEST_VENNUM$ <> AP_CLOSE::VENNUM
	THEN
		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			AP_CLOSE::VENNUM, 9%, 10%)

		ADD_TO_1099% = 0%

		WHEN ERROR IN
			GET #AP_VENDOR.CH%, &
				KEY #0% EQ AP_CLOSE::VENNUM, &
				REGARDLESS
		USE
			CONTINUE 3030 IF ERR = 155%
			FILENAME$ = "AP_VENDOR"
			CONTINUE HelpError
		END WHEN

		ADD_TO_1099% = -1% &
			IF EDIT$(AP_VENDOR::FLG1099, -1%) = "Y"

	END IF

3030	TEST_VENNUM$ = AP_CLOSE::VENNUM

	GOTO 3040 &
		IF ADD_TO_1099% = 0%

	IF AP_CLOSE::TRANKEY <> TRANKEY$
	THEN
		INVAMT = AP_CLOSE::INVAMT
		AMT_1099 = AP_CLOSE::AMT_1099
		CODE_1099$ = AP_CLOSE::CODE_1099

	END IF

	TRANKEY$ = AP_CLOSE::TRANKEY

	UPDATED$ = LEFT(AP_CLOSE::UPDATED, 6%)

	IF UPDATED$ >= BEG_PERIOD$ AND UPDATED$ <= END_PERIOD$
	THEN
		IF AP_CLOSE::CKAMT <> 0.0
		THEN
			IF CODE_1099$ = ""
			THEN
				SCOPE::PRG_ITEM = "1099_CODE"

				SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
					"Vendor # " + &
					AP_CLOSE::VENNUM + " Ck # " + &
					AP_CLOSE::CKNUM + " Date " + &
					PRNT_DATE(AP_CLOSE::CKDAT, 8%) + &
					" Ck Amount " + &
					FORMAT$(AP_CLOSE::CKAMT, &
					"#,###,###.##"), &
					11%, 2%)

				CODE_1099$ = DEFAULT_1099$

 CodeMissClose:
				SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

				CODE_1099$ = ENTR_3STRING(SCOPE, &
					SCOPE::SMG_OPTION, "", &
					"Missing 1099 Code - Enter " + &
					"correct code - then press <Do> ", &
					CODE_1099$, 0%, "'E", CODE_1099$)

				SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
					LOC(OUTP_XUNSOL) BY VALUE, &
					LOC(SCOPE::SMG_KBID) BY VALUE)

				SELECT SCOPE::SCOPE_EXIT
				!
				! List Choices
				!
				CASE SMG$K_TRM_F14
					SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)
					IF (MAIN_WINDOW(AP_MAIN_1099_TABLE.ID, "V0") = 1%)
					THEN
						CODE_1099$ = &
							AP_1099_TABLE::CODE
					END IF

					SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
						LOC(OUTP_XUNSOL) BY VALUE, &
						LOC(SCOPE::SMG_KBID) BY VALUE)

					GOTO CodeMissClose

				!
				! Control c
				!
				CASE 3%
					GOTO CodeMissClose

				!
				! Exit key
				!
				CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
					GOTO ExitProgram

				END SELECT

				SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
					SPACE$(78%), 11%, 2%)

				SMG_STATUS% = &
					SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
			END IF

			IF AMT_1099 = 0.0
			THEN
				AMT_1099 = INVAMT
			END IF

			FACTOR = 1.0
			FACTOR = AP_CLOSE::CKAMT / INVAMT &
				IF INVAMT <> 0.0

			AP_1099_YYYY::VENNUM = AP_CLOSE::VENNUM
			AP_1099_YYYY::CODE = CODE_1099$
			AP_1099_YYYY::TRANKEY = AP_CLOSE::TRANKEY
			AP_1099_YYYY::INVNUM = AP_CLOSE::INVNUM
			AP_1099_YYYY::INVDAT = AP_CLOSE::INVDAT
			AP_1099_YYYY::CKNUM = AP_CLOSE::CKNUM
			AP_1099_YYYY::CKDAT = AP_CLOSE::CKDAT
			AP_1099_YYYY::AMT1099 = &
				FUNC_ROUND(AMT_1099 * FACTOR, 2%)
			AP_1099_YYYY::CKAMT = AP_CLOSE::CKAMT

			WHEN ERROR IN
				PUT #AP_1099_YYYY.CH%
			USE
				FILENAME$ = "AP_1099_" + YEAR_1099$
				CONTINUE HelpError
			END WHEN
		END IF
	END IF

3040	GOTO 3020

	%PAGE

5000	!********************************************************************
	! End process
	!********************************************************************
	CLOSE AP_1099_YYYY.CH%, AP_OPEN.CH%, AP_CLOSE.CH%

	SCOPE::PRG_ITEM = "HELP"
	CALL ENTR_3MESSAGE(SCOPE, "Process completed", 0%)

 ExitProgram:
	!
	! Disable unsolicited input
	!
	SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitProgram

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	FILENAME$ = ""
	RESUME HelpError

19999	END

20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"
	%INCLUDE "FUNC_INCLUDE:AP_WINDOW.INC"

	EXTERNAL LONG FUNCTION AP_MAIN_1099_TABLE

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE AP_MAIN_1099_TABLE.ID

		MAINT_GROUP = AP_MAIN_1099_TABLE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
