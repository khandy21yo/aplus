1	%TITLE "MAINT - Maintain Payroll Journal"
	%SBTTL "PR_JOUR_DETAIL_X"
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
	!	The ^*Maintain Payroll Journal\* option in the Payroll Journal menu provides
	!	access to the records in a specified payroll folder file.
	!	.b
	!	^*Note:\* This option is ^~not\~ intended for entry of data. It is intended for editing
	!	purposes only.
	!	.lm -5
	!
	! Index:
	!	.x Maintain>Payroll Journal
	!	.x Detail
	!
	! Option:
	!	PR_MAIN_DETAIL_EMP_QUERY_X$HELP
	!	PR_MAIN_TRN_PAY_X$HELP
	!	PR_MAIN_TRN_DED$HELP
	!	PR_MAIN_TRN_CHECK$HELP
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_JOUR_DETAIL_X/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_JOUR_DETAIL_X, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_JOUR_DETAIL_X.OBJ;*
	!
	! Author:
	!
	!	11/25/87 - B. Craig Larsen
	!
	! Modification history:
	!
	!	08/25/89 - Kevin Handy
	!		Modified to allow reading of a file that has
	!		been posted, but not closed.
	!
	!	06/03/91 - Kevin Handy
	!		Removed stupid code in error trapping.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!		Fix last parameter to entr_3choice.
	!		Add PR_MAIN_DETAIL_EMP_QUERY_X.ID in PR_WINDOW.INC.
	!
	!	08/21/95 - Kevin Handy
	!		Fix so it will always allow editing folders, even
	!		if posted/closed.
	!
	!	09/09/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/20/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/15/2000 - Kevin Handy
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

	MAP (PR_DETAIL) &
		BATCH_NO$ = 8,		! Folder Date &
		CLOSE_FLAG%		! Is folder modifiable

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE
	CLOSE_FLAG% = 0%

	!
	! Dimension statements
	!
	DIM DATE_FILE$(200), REVERSE_LIST$(200%)

300	!******************************************************************
	! Get Year for file name
	!******************************************************************

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
			GOTO 390
		END IF
	END IF

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		GOTO ExitProgram

	END SELECT

	!
	! Ask for year
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(18%, 80%, SMG_SCREEN_DATA%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, 1%, 1%)

320	!
	! Ask for the folder date
	!
	BATCH_NO$ = DATE_TODAY IF EDIT$(BATCH_NO$, -1%) = ""

	SCOPE::PRG_ITEM = "FLD01FDATE"

	!++
	! Abstract:FLD01FDATE
	!	.ts 55
	!	^*Payroll Folder Date	MMDDYYYY or MMDDYY\*
	!	.b
	!	.lm +5
	!	The ^*Payroll Folder Date\* field provides
	!	the means to select an existing folder or to create a new one.
	!	.lm -5
	!
	! Index:
	!	.x Payroll>Folder>Date
	!	.x Date>Payroll Folder
	!
	!--

	PR_TRN_PAY_DATE$ = ENTR_3DATE(SCOPE,  SMG_SCREEN_DATA%, "", &
		"Enter Payroll Folder Date (MMDDYYYY) ", &
		BATCH_NO$, 64%, "8", "")

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
		! Good key

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

		BATCH_NO$, PR_TRN_PAY_DATE$ = ""

		GOTO 320

	END IF

	!
	! Set Batch No
	!
	BATCH_NO$ = PR_TRN_PAY_DATE$

390	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%)

400	!*********************************************************************
	! Test to see if payroll has been closed or posted
	!	1 - Updated to Register
	!	2 - Accrued Post
	!	4 - Final Post
	!*********************************************************************

	!
	! Open Pay folder
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.UPD"

		RESET #PR_TRN_PAY.CH%

		GET #PR_TRN_PAY.CH%, REGARDLESS
	USE
		CONTINUE 500
	END WHEN

 !	IF (PR_TRN_PAY::UPDATE_FLAG AND 1%)
 !	THEN
 !		CALL HELP_3MESSAGE(SCOPE, "PR Folder is Closed", "ERR", "PR_CLOSED", &
 !			"ERROR_PR_CLOSED")
 !		GOTO ExitProgram
 !		CALL ENTR_3MESSAGE(SCOPE, "NOTE: Folder has been closed, no changes allowed", 0%)
 !		CLOSE_FLAG% = -1%
 !		GOTO 500
 !	END IF

 !	IF (PR_TRN_PAY::UPDATE_FLAG AND 4%)
 !	THEN
 !		CALL HELP_3MESSAGE(SCOPE, "PR Folder has been Posted", "ERR", "PR_POSTED", &
 !			"ERROR_PR_POSTED")
 !		GOTO ExitProgram
 !		CALL ENTR_3MESSAGE(SCOPE, "NOTE: Folder has been posted, no changes allowed", 0%)
 !		CLOSE_FLAG% = -1%
 !	END IF

500	!
	! Resume after test and close channel
	!
	CLOSE #PR_TRN_PAY.CH%


	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_WINDOW(PR_MAIN_DETAIL_EMP_QUERY_X.ID, "")

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

	%Page

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
	EXTERNAL LONG FUNCTION PR_MAIN_DETAIL_EMP_QUERY_X
	EXTERNAL LONG FUNCTION PR_MAIN_TRN_PAY_X
	EXTERNAL LONG FUNCTION PR_MAIN_TRN_DED
	EXTERNAL LONG FUNCTION PR_MAIN_TRN_CHECK
	EXTERNAL LONG FUNCTION PR_MAIN_ERNDED_DEF
	EXTERNAL LONG FUNCTION PR_MAIN_TAX_PKG

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
	CASE PR_MAIN_DETAIL_EMP_QUERY_X.ID

		MAINT_GROUP = PR_MAIN_DETAIL_EMP_QUERY_X(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	!
	! Pay File
	!
	CASE PR_MAIN_TRN_PAY_X.ID

		MAINT_GROUP = PR_MAIN_TRN_PAY_X(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	!
	! Deduction File
	!
	CASE PR_MAIN_TRN_DED.ID

		MAINT_GROUP = PR_MAIN_TRN_DED(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	!
	! Check File
	!
	CASE PR_MAIN_TRN_CHECK.ID

		MAINT_GROUP = PR_MAIN_TRN_CHECK(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	!
	! Ern/ded definition
	!
	CASE PR_MAIN_ERNDED_DEF.ID

		MAINT_GROUP = PR_MAIN_ERNDED_DEF(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	!
	! Tax Package definition
	!
	CASE PR_MAIN_TAX_PKG.ID

		MAINT_GROUP = PR_MAIN_TAX_PKG(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
