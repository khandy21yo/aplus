1	%TITLE "GENERAL LEDGER BUDGET MASTER"
	%SBTTL "GL_MAST_BUDGET"
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
	!	The ^*Budget Master\* routine sets
	!	up and maintains budgets.
	!	.lm -5
	!
	! Index:
	!	.x Budget>Maintenance
	!	.x Maintenance>Budget
	!
	! Option:
	!
	!	GL_MAIN_BUDGET$HELP
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_MAST_BUDGET/LINE
	!	$ LINK/EXECUTABLE=GL_EXE: GL_MAST_BUDGET, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_MAST_BUDGET.OBJ;*
	!
	! Author:
	!
	!	04/15/87 - Kevin Handy
	!
	! Modification history:
	!
	!	06/23/88 - Aaron Redd
	!		Split into two modules (_MAST_ and _MAIN_) in order
	!		to meet standardization requirements.
	!
	!	02/09/89 - Kevin Handy
	!		Modified for changes in ENTR_ENTER
	!
	!	05/24/91 - Kevin Handy
	!		Removed COM for GL_PERIOD, since it wasn't used.
	!
	!	04/14/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!		Fix last param to ENTR_3CHOICE.
	!
	!	10/18/96 - Kevin Handy
	!		Reformat source code
	!
	!	05/15/97 - Kevin Handy
	!		Reformat source code
	!
	!	08/25/97 - Kevin Handy
	!		Use 'val%' instead of 'val'
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/24/2000 - Kevin Handy
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

	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_GL_BUD_YYYY) &
		GL_BUD_YYYY.CH%, &
		GL_BUD_YYYY.READONLY%
	COM (TT_GL_BUD_YYYY) &
		GL_BUDGET_YEAR$ = 4%

	%PAGE

100	!******************************************************************
	! Initialization section - Prepare to do anything
	!******************************************************************

	!
	! Initialize all the standard stuff through an external call
	!
	CALL READ_INITIALIZE

200	!******************************************************************
	! Select budget file  (Which year?)
	!******************************************************************

	!
	! Get info required to open GL Budget file
	!
	CALL READ_DEVICE("GL_BUD_YYYY", GL_BUD_YYYY.DEV$, STAT%)

300	!
	! Query user for year of file
	!
	CALL FIND_FILE(GL_BUD_YYYY.DEV$ + "GL_BUD_*.MAS", GL_BUD_YYYY_FILE$(), &
		16%, "", "")

	GL_BUD_YYYY_FILE% = VAL%(GL_BUD_YYYY_FILE$(0%))

	IF GL_BUD_YYYY_FILE%
	THEN
		GL_BUD_YYYY_FILE$(LOOP%) = &
			MID(GL_BUD_YYYY_FILE$(LOOP%), 8%, 4%) &
				FOR LOOP% = 1% TO GL_BUD_YYYY_FILE%

		TEMP$ = "GL Budget Files"

		X% = ENTR_3CHOICE(SCOPE, "", "", GL_BUD_YYYY_FILE$(), "", &
			0%, TEMP$, "", 0%)

		IF X% > 0%
		THEN
			GL_BUDGET_YEAR$ = EDIT$(GL_BUD_YYYY_FILE$(X%), -1%)
			GOTO 400
		END IF
	END IF

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		GOTO ExitProgram

	END SELECT

	!
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

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Budget Year:", 11%, 30%)

320	SCOPE::PRG_ITEM = "YEAR"

	GL_BUDGET_YEAR$ = LEFT(DATE_TODAY, 4%)

	SELECT ENTR_3ENTER(SCOPE, SMG_SCREEN_DATA%, 11%, 43%, &
		GL_BUDGET_YEAR$, -1%, 0%)

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		GOTO ExitProgram

	END SELECT

	GL_BUDGET_YEAR$ = EDIT$(GL_BUDGET_YEAR$, -1%)

	GL_BUDGET_YEAR$ = LEFT(DATE_TODAY, 2%) + GL_BUDGET_YEAR$ &
		IF LEN(EDIT$(GL_BUDGET_YEAR$, -1%)) = 2%

	IF LEN(EDIT$(GL_BUDGET_YEAR$, -1%)) <> 4%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Please enter the budget year in YYYY format", 0%)
		GOTO 320
	END IF

400	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%)

1000	!******************************************************************
	! Handle the main function
	!******************************************************************

	V% = MAIN_WINDOW(GL_MAIN_BUDGET.ID, "")

	!******************************************************************
	! Exit GL_MAST_BUDGET
	!******************************************************************

 ExitProgram:
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19990	!******************************************************************
	! End of GL_MAST_BUDGET
	!******************************************************************
	END



20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION GL_MAIN_BUDGET
	EXTERNAL LONG	FUNCTION GL_MAIN_CHART

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
	! Process the GL Budget maintenance window
	!
	CASE GL_MAIN_BUDGET.ID
		MAINT_GROUP = GL_MAIN_BUDGET(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	END SELECT

32767	!******************************************************************
	! End of MAINT_GROUP function
	!******************************************************************
	END FUNCTION
