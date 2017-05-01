1	%TITLE "HISTY - Examine Historical Folder"
	%SBTTL "PR_JOUR_HISTORY"
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
	!	The ^*Examine Historical Folder\* program allows for the maintaining and
	!	updating of the historical folders of the employees, both the Master file
	!	and the Journal file.
	!	.lm -5
	!
	! Index:
	!	.x Examine Historical Folder
	!	.x Folder>Examine Historical
	!
	! Option:
	!
	!	PR_MAIN_HISTORY_EMP_QUERY$HELP
	!	PR_MAIN_HIS_PAY$HELP
	!	PR_MAIN_HIS_DED$HELP
	!	PR_MAIN_HIS_CHECK$HELP
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_JOUR_HISTORY/LINE/NOOPT
	!	$ LINK/EXECUTABLE=PR_EXE: PR_JOUR_HISTORY, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_JOUR_HISTORY.OBJ;*
	!
	! Author:
	!
	!	11/25/87 - B. Craig Larsen
	!
	!	10/04/89 - Kevin Handy
	!		Taken from PR_JOUR_DETAIL
	!
	! Modification history:
	!
	!	08/25/89 - Kevin Handy
	!		Modified to allow reading of a file that has
	!		been posted, but not closed.
	!
	!	11/16/93 - Kevin Handy
	!		Increased dimension for folders from 200 to 500.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!		Fix last parameter to ENTR_3CHOICE.
	!		Add PR_MAIN_HISTORY_EMP_QUERY.ID to PR_WINDOW.INC.
	!
	!	10/22/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/14/99 - Kevin Handy
	!		Clean up formatting of source code
	!
	!	06/16/99 - Kevin Handy
	!		Add code to init DATE_FILE array to lose warning
	!		errors/crash on Alpha.
	!
	!	06/30/99 - Kevin Handy
	!		Compile /NOOPT to lose problems on Alpha
	!
	!	12/06/99 - Kevin Handy
	!		Increase file dimension from 500 to 1000. (LL)
	!
	!	01/07/2003 - Kevin Handy
	!		Increase file dimension from 1000 to 1500. (LL)
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
	! Map's
	!
	MAP (PR_DETAIL) &
		BATCH_NO$ = 8,		! Folder Date &
		CLOSE_FLAG%		! Is folder modifiable

	%PAGE

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE
	CLOSE_FLAG% = 0%

	!
	! Dimension statements
	!
	DIM DATE_FILE$(1500), REVERSE_LIST$(1500%)

300	!******************************************************************
	! Get Year for file name
	!******************************************************************

	!
	! Look up device
	!
	CALL READ_DEVICE("PR_HIS_PAY", PR_HIS_PAY.DEV$, STAT%)

	DATE_FILE$(0%) = "0"
	CALL FIND_FILE(PR_HIS_PAY.DEV$ + "PR_HIS_PAY_*.ARC", DATE_FILE$(), &
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
	!	The ^*Payroll Folder Date\* field
	!	selects an existing folder or to create a new one.
	!	.lm -5
	!
	! Index:
	!	.x Payroll>Folder>Date
	!	.x Date>Payroll Folder
	!
	!--

	PR_HIS_PAY_DATE$ = ENTR_3DATE(SCOPE, SMG_SCREEN_DATA%, "", &
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

	PR_HIS_PAY_DATE$ = EDIT$(PR_HIS_PAY_DATE$, -1%)

	IF LEN(EDIT$(PR_HIS_PAY_DATE$, -1%)) <> 8%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Please enter the folder date in (MMDDYYYY) format", 0%)
		GOTO 320
	END IF


	!
	! Set Batch No
	!
	BATCH_NO$ = PR_HIS_PAY_DATE$

390	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%)

	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_WINDOW(PR_MAIN_HISTORY_EMP_QUERY.ID, "")

 ExitProgram:
	!******************************************************************
	! End of the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19990	END



20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"
	%INCLUDE "FUNC_INCLUDE:PR_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	EXTERNAL LONG FUNCTION GL_MAIN_CHART
	EXTERNAL LONG FUNCTION PR_MAIN_HISTORY_EMP_QUERY
	EXTERNAL LONG FUNCTION PR_MAIN_HIS_PAY
	EXTERNAL LONG FUNCTION PR_MAIN_HIS_DED
	EXTERNAL LONG FUNCTION PR_MAIN_HIS_CHECK
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
	CASE PR_MAIN_HISTORY_EMP_QUERY.ID

		MAINT_GROUP = PR_MAIN_HISTORY_EMP_QUERY(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	!
	! Pay File
	!
	CASE PR_MAIN_HIS_PAY.ID

		MAINT_GROUP = PR_MAIN_HIS_PAY(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	!
	! Deduction File
	!
	CASE PR_MAIN_HIS_DED.ID

		MAINT_GROUP = PR_MAIN_HIS_DED(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	!
	! Check File
	!
	CASE PR_MAIN_HIS_CHECK.ID

		MAINT_GROUP = PR_MAIN_HIS_CHECK(SMG_WINDOW, &
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
