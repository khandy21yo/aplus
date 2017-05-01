1	%TITLE "PR Time Keeper Maintenance"
	%SBTTL "PR_JOUR_TIMKEP_01"
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
	!	The ^*Timekeeper\* option
	!	accesses the Pay Journal, Deduction Journal and Check Journal.
	!	These journals provide the means to enter the employees' time worked,
	!	units produced, non-standard deductions and after-the-fact manual
	!	pay-off data.
	!	.lm -5
	!
	! Index:
	!	.x Timekeeper>Payroll Journal
	!	.x Payroll Journal>Timekeeper
	!
	! Option:
	!
	!	PR_JOUR_TIMKEP_01$PR_HIS_EXISTS
	!	PR_MAIN_TRN_TK01_PAY$HELP
	!	PR_MAIN_TRN_TK01_DED$HELP
	!	PR_MAIN_TRN_TK01_CHECK$HELP
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_JOUR_TIMKEP_01/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_JOUR_TIMKEP_01, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_JOUR_TIMKEP_01.OBJ;*
	!
	! Author:
	!
	!	12/03/87 - B. Craig Larsen
	!
	! Modification history:
	!
	!	06/03/91 - Kevin Handy
	!		Removed stupid code in error trapping.
	!
	!	10/08/92 - Kevin Handy
	!		Changed test open from .UPD to .OPN so multiple
	!		users can access file.
	!
	!	10/08/92 - Kevin Handy
	!		Modified to open up end date folder if available
	!		and different than folder date so we can get
	!		total hours to display on the entry screen.
	!
	!	02/03/95 - Kevin Handy
	!		Modified to try to open the chart of accounts
	!		in a read-only mode.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!		Fix last param to entr_choice
	!
	!	09/09/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/09/97 - Kevin Handy
	!		Lose SMG_BLANK1% display.
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/19/2000 - Kevin Handy
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

	%INCLUDE "FUNC_INCLUDE:PR_WINDOW.INC"

	!
	! Map areas
	!
	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.HB"
	MAP (PR_TRN_PAY)	PR_TRN_PAY_CDD	PR_TRN_PAY

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP	(GL_CHART)	GL_CHART_CDD	GL_CHART

	MAP (PR_DETAIL) &
		BATCH_NO$ = 8%, &
		END_DATE$ = 8%, &
		BATCH_ENTRY$ = 2%, &
		LOCATION$ = 4%

	COM (CH_PR_TRN_PAY_FINAL) &
		PR_TRN_PAY_FINAL.CH%

	COM (CH_GL_CHART) &
		GL_CHART.CH%, &
		GL_CHART.READONLY%

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	!
	! Dimension statements
	!
	DIM DATE_FILE$(200), REVERSE_LIST$(200%)

290	!
	! Open chart of accounts read/only, so it doesn't get open
	! read/write later.
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.OPN"
	USE
		CONTINUE 300
	END WHEN

	GL_CHART.READONLY% = -1%

300	!******************************************************************
	! Get Year for file name
	!******************************************************************

	!
	! Ask for year
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(18%, 80%, SMG_SCREEN_DATA%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, 1%, 1%)

	!
	! Look up device
	!
	CALL  READ_DEVICE("PR_TRN_PAY", PR_TRN_PAY.DEV$, STAT%)
	CALL  READ_DEVICE("PR_HIS_PAY", PR_HIS_PAY.DEV$, STAT%)

	CALL FIND_FILE(PR_TRN_PAY.DEV$ + "PR_TRN_PAY_*.JRL", DATE_FILE$(), &
		16%, "", "")

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram
	END SELECT

	DATE_FILE% = VAL%(DATE_FILE$(0%))

	IF DATE_FILE%
	THEN
		REVERSE_LIST$(DATE_FILE% - LOOP% + 1%) = &
			MID(DATE_FILE$(LOOP%), 16%, 2%) + "/" + &
			MID(DATE_FILE$(LOOP%), 18%, 2%) + "/" + &
			MID(DATE_FILE$(LOOP%), 12%, 4%) &
				FOR LOOP% = DATE_FILE% TO 1% STEP -1%

		TEMP$ = "Payroll Folder Dates"

		X% = ENTR_3CHOICE(SCOPE, "", "", REVERSE_LIST$(), &
			"", 0%, TEMP$, "", 0%)

		IF X% > 0%
		THEN
			BATCH_NO$ = RIGHT(REVERSE_LIST$(X%), 7%) + &
				LEFT(REVERSE_LIST$(X%), 2%) + &
				MID(REVERSE_LIST$(X%), 4%, 2%)
			PR_TRN_PAY_DATE$ = BATCH_NO$
			GOTO 340
		END IF
	END IF

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		GOTO ExitProgram

	END SELECT

320	!
	! Ask for the folder date
	!
	BATCH_NO$ = DATE_TODAY IF EDIT$(BATCH_NO$, -1%) = ""

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"(01) Payroll Folder Date", 6%, 20%)

	SCOPE::PRG_ITEM = "FLD001"

	!++
	! Abstract:FLD001
	!	.ts 55
	!	^*(01) Payroll Folder Date	MMDDYYYY or MMDDYY\*
	!	.b
	!	.lm +5
	!	The ^*Payroll Folder Date\* field
	!	selects an existing folder or to create a new one.
	!	.b
	!	When the Timekeeper option is accessed, the system automatically
	!	displays a ^*List Choices\* screen which includes all existing payroll
	!	folder dates. An existing folder can be accessed by positioning the
	!	pointer to the desired folder date and pressing ^*<Select>\*.
	!	.b
	!	A new payroll folder can be created by pressing ^*<Do>\* while the
	!	^*List Choices\* screen is displayed. The system then prompts for the
	!	new folder date to be entered.
	!	.lm -5
	!
	! Index:
	!	.x Payroll>Folder>Date
	!	.x Date>Payroll Folder
	!
	!--

	PR_TRN_PAY_DATE$ = ENTR_3DATE(SCOPE,  SMG_SCREEN_DATA%, "6;45", &
		"Enter Payroll Folder Date (MMDDYYYY) ", BATCH_NO$, 0%, &
		"8", "")

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
		! Good key

	CASE SMG$K_TRM_UP
		GOTO 320

	CASE SMG$K_TRM_DOWN
		GOTO 340

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 320
	END SELECT

	PR_TRN_PAY_DATE$ = EDIT$(PR_TRN_PAY_DATE$, -1%)

	IF LEN(EDIT$(PR_TRN_PAY_DATE$, -1%)) <> 8%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "Please enter the folder date in " + &
			"(MMDDYYYY) format", 0%)
		GOTO 320
	END IF

 FileExists:
	IF FIND_FILEEXISTS(PR_HIS_PAY.DEV$ + "PR_HIS_PAY_" + PR_TRN_PAY_DATE$ + ".ARC", 0%)
	THEN
		TEMP_ITEM$ = SCOPE::PRG_ITEM
		SCOPE::PRG_ITEM = "PR_HIS_EXISTS"
	!++
	! Abstract:PR_HIS_EXISTS
	!
	!
	! Index:
	!
	!
	!--

		CALL ENTR_3MESSAGE(SCOPE, "That Payroll Folder has been closed.", 0%)

		SCOPE::PRG_ITEM = TEMP_ITEM$

		SELECT SCOPE::SCOPE_EXIT
		!
		! Exit
		!
		CASE 3%, SMG$K_TRM_F8, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO ExitProgram

		!
		! Normal key typed
		!
		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

		!
		! Bad key typed
		!
		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO FileExists

		END SELECT

		PR_TRN_PAY_DATE$ = ""

		GOTO 320

	END IF

	!
	! Set Batch Number
	!
	BATCH_NO$ = PR_TRN_PAY_DATE$

340	!
	! Ask for the pay off date
	!
	END_DATE$ = BATCH_NO$ IF EDIT$(END_DATE$, -1%) = ""

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"(01) Payroll Folder Date ", 6%, 20%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, PRNT_DATE(BATCH_NO$, 8%), &
		6%, 45%, , SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "(02) Payroll End Date", &
		8%, 20%)

	SCOPE::PRG_ITEM = "FLD002"

	!++
	! Abstract:FLD002
	!	^*(02) Payroll End Date\*
	!	.b
	!	.lm +5
	!	The ^*Payroll End Date\* field
	!	enters a date which designates the end of a payroll period.
	!	Generally, the payroll end date and the payroll folder date would be
	!	equal. Pressing ^*<Do>\* at the payroll end date prompt, without
	!	entering any date, will result in the end date being equal to the
	!	folder date.
	!	.b
	!	The value in the Payroll End Date field can be used to cause the
	!	printing of more than one check for an employee in the same payroll
	!	period. Multiple checks per employee can be accomplished by following
	!	the following steps, assuming two checks are to be printed for
	!	employee "A":
	!	.lm +5
	!	.b
	!	Access the Pay Journal through the Timekeeper
	!	option.
	!	.b
	!	Use normal procedures in entering the value in
	!	the Payroll End Date field.
	!	.b
	!	Enter regular data in the Pay Journal, including
	!	the data which is to be included in one check for
	!	employee "A".
	!	.b
	!	Exit the Pay Journal routine.
	!	.b
	!	Re-access the Pay Journal through the Timekeeper
	!	option.
	!	.b
	!	When entering the value in the Payroll End Date
	!	field, enter a value different from the value
	!	entered during the first pass.
	!	.b
	!	Enter additional data in the Pay Journal for
	!	employee "A".
	!	.b
	!	.lm -5
	!	When the payroll checks printing routine is executed, two checks
	!	will be printed for employee "A".
	!	.b
	!	When the Calculate routine is accessed, the system prompts for
	!	entering a Payroll End Date. Though taxes will be calculated for
	!	all records in the payroll folder, standard deductions will be
	!	extracted only for records with payroll end date values equal to
	!	the Payroll End Date field when the Calculate
	!	routine was accessed. Thus, standard payroll deductions will ^&not\&
	!	be made on any checks which have a "non-standard" payroll end date.
	!
	! Index:
	!	.x Payroll>End Date
	!	.x End Date
	!
	!--

	END_DATE$ = ENTR_3DATE(SCOPE,  SMG_SCREEN_DATA%, "8;45", &
		"Enter Payroll End Date (MMDDYYYY) ", END_DATE$, 0%, "8", "")

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
		! Good key

	CASE SMG$K_TRM_UP
		GOTO 320

	CASE SMG$K_TRM_DOWN
		GOTO 350

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 340
	END SELECT

	END_DATE$ = EDIT$(END_DATE$, -1%)

	IF LEN(EDIT$(END_DATE$, -1%)) <> 8%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Please enter the end date in (MMDDYYYY) format", 0%)
		GOTO 340
	END IF

350	BATCH_ENTRY$ = SPACE$(2%) IF EDIT$(BATCH_ENTRY$, -1%) = ""

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "(03) Batch Entry #", &
		10%, 20%)

	SCOPE::PRG_ITEM = "FLD003"

	!++
	! Abstract:FLD003
	!	^*(03) Batch Entry _#\*
	!	.b
	!	.lm +5
	!	The ^*Batch Entry _#\* field enters a one or two
	!	alphanumeric character batch number which identifies the batch into
	!	which data will be entered.
	!	.b
	!	Subsequently, a Time Report can be printed which pertains to
	!	a specific batch only.
	!	.b
	!	This field may be left blank.
	!	.lm -5
	!
	! Index:
	!	.x Batch Entry
	!	.x Journal>Batch Entry
	!
	!--

	BATCH_ENTRY$ = ENTR_3STRING(SCOPE,  SMG_SCREEN_DATA%, "10;45", &
		"Enter Batch Entry # ", BATCH_ENTRY$, 0%, "'E", "")

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
		! Good key

	CASE SMG$K_TRM_UP
		GOTO 340

	CASE SMG$K_TRM_DOWN
		GOTO 360

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 350
	END SELECT

360	!
	! Enter location
	!
	LOCATION$ = SPACE$(4%) IF EDIT$(LOCATION$, -1%) = ""

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "(04) Location", &
		12%, 20%)

	SCOPE::PRG_ITEM = "FLD004"

	!++
	! Abstract:FLD004
	!	.ts 55
	!	^*(04) Location	4 Characters\*
	!	.b
	!	.lm +5
	!	The ^*Location\* field enters up to a four (4)
	!	alphanumeric character location code. The field may
	!	be left blank.
	!	.lm -5
	!
	! Index:
	!	.x Time Journal>Location
	!	.x Location>Time Journal
	!
	!--

	LOCATION$ = ENTR_3STRING(SCOPE,  SMG_SCREEN_DATA%, "12;45", &
		"Enter Location ", LOCATION$, 0%, "'E", "")

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
		! Good key

	CASE SMG$K_TRM_UP
		GOTO 350

	CASE SMG$K_TRM_DOWN
		GOTO 360

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 360
	END SELECT

390	!
	! Remove display
	!
	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)

395	!*******************************************************************
	! Open up payroll end date to see if it exists, so that we can
	! give them the total regular/overtime
	!*******************************************************************

	!
	! Skip if same folder
	!
	GOTO 400 IF END_DATE$ = PR_TRN_PAY_DATE$

	!
	! Set batch number
	!
	BATCH_NO$ = END_DATE$

	!
	! Open Pay folder
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.OPN"
	USE
		CONTINUE 400
	END WHEN

	PR_TRN_PAY_FINAL.CH% = PR_TRN_PAY.CH%

	PR_TRN_PAY.CH% = 0%

400	!*********************************************************************
	! Test to see if payroll has been closed or posted
	!	1 - Updated to Register
	!	2 - Accrued Post
	!	4 - Final Post
	!*********************************************************************

	!
	! Set batch number
	!
	BATCH_NO$ = PR_TRN_PAY_DATE$

	!
	! Open Pay folder
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.OPN"

		RESET #PR_TRN_PAY.CH%

		GET #PR_TRN_PAY.CH%, REGARDLESS
	USE
		CONTINUE 500
	END WHEN

	IF (PR_TRN_PAY::UPDATE_FLAG AND 1%)
	THEN
		CALL HELP_3MESSAGE(SCOPE, "PR Folder is Closed", "ERR", &
			"PR_CLOSED", "ERROR_PR_CLOSED")
		GOTO ExitProgram
	END IF

	IF (PR_TRN_PAY::UPDATE_FLAG AND 4%)
	THEN
		CALL HELP_3MESSAGE(SCOPE, "PR Folder has been Posted", &
			"ERR", "PR_POSTED", "ERROR_PR_POSTED")
		GOTO ExitProgram
	END IF

500	!
	! Resume after test and close channel
	!
	CLOSE #PR_TRN_PAY.CH%

	!
	! Change the width
	!
	SMG_STATUS% = SMG$CHANGE_PBD_CHARACTERISTICS(SCOPE::SMG_PBID, 132%)

	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_JOURNAL(PR_MAIN_TRN_TK01_PAY.ID, "")

 ExitProgram:
	!******************************************************************
	! End of the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitProgram

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

19990	END



20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"
	%INCLUDE "FUNC_INCLUDE:PR_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	EXTERNAL LONG FUNCTION GL_MAIN_CHART
	EXTERNAL LONG FUNCTION PR_MAIN_TK_EMP_QUERY
	EXTERNAL LONG FUNCTION PR_MAIN_TRN_TK01_PAY
	EXTERNAL LONG FUNCTION PR_MAIN_TRN_TK01_DED
	EXTERNAL LONG FUNCTION PR_MAIN_TRN_TK01_CHECK
	EXTERNAL LONG FUNCTION PR_MAIN_ERNDED_DEF

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	!
	! Chart of accounts maintainence
	!
	CASE GL_MAIN_CHART.ID

		MAINT_GROUP = GL_MAIN_CHART(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	!
	! (Special) Employee master file
	!
	CASE PR_MAIN_TK_EMP_QUERY.ID

		MAINT_GROUP = PR_MAIN_TK_EMP_QUERY(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	!
	! Pay File
	!
	CASE PR_MAIN_TRN_TK01_PAY.ID

		MAINT_GROUP = PR_MAIN_TRN_TK01_PAY(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	!
	! Deduction File
	!
	CASE PR_MAIN_TRN_TK01_DED.ID

		MAINT_GROUP = PR_MAIN_TRN_TK01_DED(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	!
	! Check File
	!
	CASE PR_MAIN_TRN_TK01_CHECK.ID

		MAINT_GROUP = PR_MAIN_TRN_TK01_CHECK(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	!
	! Ern/ded definition
	!
	CASE PR_MAIN_ERNDED_DEF.ID

		MAINT_GROUP = PR_MAIN_ERNDED_DEF(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
