1	%TITLE "MAINT - Maintain Annual PR Ledger"
	%SBTTL "PR_MAST_REG_LEDGER"
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
	!	The ^*Maintain Payroll Annual Register\* menu option
	!	accesses the Payroll Register file
	!	where all relevant data pertaining to each employee's earnings, taxes
	!	withheld, non tax deductions, non compensation payments and memo
	!	entries are stored. This option is ^*not\* intended for regular entry or
	!	editing but prinmary purpost is to initialize existing data.
	!
	! Index:
	!	.x Payroll Register File
	!	.x Maintain>Payroll Register File
	!	.x Payroll Register Data>Initializing
	!	.x Initialization>Payroll Register Data
	!
	! Option:
	!
	!	PR_MAIN_REG_EMPLOYEE$HELP
	!	PR_MAIN_REG_TAXES$HELP
	!	PR_MAIN_REG_ERNDED$HELP
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_MAST_REG_LEDGER/LINE/NOOPT
	!	$ LINK/EXECUTABLE=PR_EXE: PR_MAST_REG_LEDGER, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_MAST_REG_LEDGER.OBJ;*
	!
	! Author:
	!
	!	10/15/87 - Kevin Handy
	!
	! Modification history:
	!
	!	02/09/89 - Kevin Handy
	!		Modified for changes in ENTR_ENTER.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!		Fix last parameter to entr_3choice.
	!
	!	10/24/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/30/99 - Kevin Handy
	!		Compile /NOOPT to lose problems on Alpha
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
	MAP (PR_REG_YYYY) PR_REG_YYYY$ = 4%	! Pass year through all maintainence

	%PAGE

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	!
	! Dimension statements
	!
	DIM YYYY_FILE$(100)

300	!******************************************************************
	! Get Year for file name
	!******************************************************************

	!
	! Look up device
	!
	CALL  READ_DEVICE("PR_REG_TAXES", PR_REG_TAXES.DEV$, STAT%)

	CALL FIND_FILE( PR_REG_TAXES.DEV$ + "PR_REG_TAXES_*.LED", &
		YYYY_FILE$(), 16%, "", "")

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram
	END SELECT

	YYYY_FILE% = VAL%(YYYY_FILE$(0%))

	IF YYYY_FILE%
	THEN
		YYYY_FILE$(LOOP%) = &
			MID(YYYY_FILE$(LOOP%), 14%, 4%) &
				FOR LOOP% = 1% TO YYYY_FILE%

		TEMP$ = "Payroll Register Year"

		X% = ENTR_3CHOICE(SCOPE, "", "", YYYY_FILE$(), "", &
			0%, TEMP$, "", 0%)

		IF X% > 0%
		THEN
			PR_REG_YYYY$ = EDIT$(YYYY_FILE$(X%), -1%)
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
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		18%, &
		80%, &
		SMG_SCREEN_DATA% &
	)

	!
	! Paint a prompt
	!
	SMG_STATUS% = SMG$PUT_CHARS &
	( &
		SMG_SCREEN_DATA%, &
		"Enter Year for Registers", &
		6%, &
		28% &
	)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, &
		1%, &
		1% &
	)

320	SCOPE::PRG_ITEM = "FLD01YEAR"

	!++
	! Abstract:FLD01YEAR
	!	^*Year\*
	!	.p
	!	The ^*Year\* field allows for entry of the year for which the report is
	!	to initialize the existing data.
	!
	! Index:
	!	.x Year>Maintain Annual Register
	!	.x Maintain Annual Register>Year
	!
	!--

	PR_REG_YYYY$ = LEFT(DATE_TODAY, 4%)

	SELECT ENTR_3ENTER(SCOPE, SMG_SCREEN_DATA%, 8%, 38%, PR_REG_YYYY$, 0%, 0%)

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
		! Good key

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 320
	END SELECT

	PR_REG_YYYY$ = EDIT$(PR_REG_YYYY$, -1%)

	IF LEN(EDIT$(PR_REG_YYYY$, -1%)) <> 4%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Please enter the Register year in YYYY format", 0%)
		GOTO 320
	END IF

390	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%)

400	!******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_WINDOW(PR_MAIN_REG_EMPLOYEE.ID, "")

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

	EXTERNAL LONG FUNCTION PR_MAIN_REG_EMPLOYEE
	EXTERNAL LONG FUNCTION PR_MAIN_REG_TAXES
	EXTERNAL LONG FUNCTION PR_MAIN_REG_ERNDED
	EXTERNAL LONG FUNCTION PR_MAIN_ERNDED_DEF

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	!
	! (Special) Employee master file
	!
	CASE PR_MAIN_REG_EMPLOYEE.ID

		MAINT_GROUP = PR_MAIN_REG_EMPLOYEE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	!
	! Ern/ded definition
	!
	CASE PR_MAIN_ERNDED_DEF.ID

		MAINT_GROUP = PR_MAIN_ERNDED_DEF(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	!
	! Tax file
	!
	CASE PR_MAIN_REG_TAXES.ID

		MAINT_GROUP = PR_MAIN_REG_TAXES(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	!
	! Ernded file
	!
	CASE PR_MAIN_REG_ERNDED.ID

		MAINT_GROUP = PR_MAIN_REG_ERNDED(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
