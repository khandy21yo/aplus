1	%TITLE "Clean Junk <Zero> records from Register"
	%SBTTL "PR_SPEC_CLEANREG"
	%IDENT "V3.6a Calico"

	!
	!	COPYRIGHT (C) 1992 BY
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
	!	.p
	!
	! Index:
	!	.X Clean Folder>Special
	!	.x Special>Clean Folder
	!
	! Option:
	!
	!	PR_SPEC_CLEANREG$CONFIRM
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_SPEC_CLEANREG
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_SPEC_CLEANREG, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_SPEC_CLEANREG.OBJ;*
	!
	! Author:
	!
	!	10/28/92 - Kevin Handy
	!
	! Modification history:
	!
	!	11/17/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!		Fix last parameter to entr_3choice
	!
	!	05/15/97 - Kevin Handy
	!		Reformat source code
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/10/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Define options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.HB"
	MAP (PR_REG_TAXES)	PR_REG_TAXES_CDD	PR_REG_TAXES

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.HB"
	MAP (PR_REG_ERNDED)	PR_REG_ERNDED_CDD	PR_REG_ERNDED

	%PAGE

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	!
	! Look up device
	!
	CALL  READ_DEVICE("PR_REG_TAXES", PR_REG_TAXES.DEV$, STAT%)

	CALL FIND_FILE(PR_REG_TAXES.DEV$ + "PR_REG_TAXES_*.LED", &
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
	!	The ^*Year\* field allows for entry of the year for which the folder is to
	!	be created. The format for entry is YYYY.
	!
	! Index:
	!	.x Year>Create Folder
	!	.x Create Folder>Year
	!
	!--
	PR_REG_YYYY$ = LEFT(DATE_TODAY, 4%)

	SELECT ENTR_3ENTER(SCOPE, SMG_SCREEN_DATA%, &
		8%, 38%, PR_REG_YYYY$, 0%, 0%)

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
	YYYY$ = PR_REG_YYYY$

	%PAGE

500	!*******************************************************************
	! Confirm creation of new TRN file.
	!*******************************************************************

	SCOPE::PRG_ITEM = "CONFIRM"
	INP$ = ENTR_3YESNO(SCOPE, SCOPE::SMG_OPTION, "", &
		"Confirm Payroll Clean process  - then press <Do> ", &
		"N", 0%, "", "")

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, SPACE$(80%), 1%, 1%)

	IF INP$ <> "Y"
	THEN
		CALL SUBR_3EXITPROGRAM(SCOPE, "", "")
	END IF

	!
	! Set help message
	!
	SCOPE::PRG_ITEM = "HELP"

	%PAGE

670	!
	! Open TaxWH register
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.MOD"
	USE
		FILENAME$ = "PR_REG_TAXES_" + YYYY$
		EXIT HANDLER
	END WHEN

680	!
	! Open ERNDED register
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.MOD"
	USE
		FILENAME$ = "PR_REG_ERNDED_" + YYYY$
		EXIT HANDLER
	END WHEN

	%PAGE

2000	!*******************************************************************
	! Copy over pay file
	!*******************************************************************

	RESET #PR_REG_ERNDED.CH%

	CALL ENTR_3MESSAGE(SCOPE, "Starting Pay    file", 1%)

2100	WHEN ERROR IN
		GET #PR_REG_ERNDED.CH%
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE 3000 IF ERR = 11%
		EXIT HANDLER
	END WHEN

2110	GOTO 2100 IF PR_REG_ERNDED::QTR_DOLL(QTR1%) <> 0.0 &
		FOR QTR1% = 0% TO 3%
	GOTO 2100 IF PR_REG_ERNDED::REG_HRS(QTR1%) <> 0.0 &
		FOR QTR1% = 0% TO 3%
	GOTO 2100 IF PR_REG_ERNDED::PRE_HRS(QTR1%) <> 0.0 &
		FOR QTR1% = 0% TO 3%
	GOTO 2100 IF PR_REG_ERNDED::UNITS(QTR1%) <> 0.0 &
		FOR QTR1% = 0% TO 3%

	DELETE #PR_REG_ERNDED.CH%

	GOTO 2100

	%PAGE

3000	!*******************************************************************
	! Handle deduction file
	!*******************************************************************

	RESET #PR_REG_TAXES.CH%

	CALL ENTR_3MESSAGE(SCOPE, "Starting Taxwh  file", 1%)

3100	WHEN ERROR IN
		GET #PR_REG_TAXES.CH%
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE 4000 IF ERR = 11%
		EXIT HANDLER
	END WHEN

3110	IF (INSTR(1%, "~CW~DW~EW~", PR_REG_TAXES::TTYPE) <> 0%)
	THEN
		GOTO 3100 IF (PR_REG_TAXES::TAX(QTR1%) <> 0.0) &
			FOR QTR1% = 0% TO 3%
		GOTO 3190
	END IF

	GOTO 3100 IF (PR_REG_TAXES::TAXABLE(QTR1%) <> 0.0) &
		FOR QTR1% = 0% TO 3%
	GOTO 3100 IF (PR_REG_TAXES::REPORTABLE(QTR1%) <> 0.0) &
		FOR QTR1% = 0% TO 3%
	GOTO 3100 IF (PR_REG_TAXES::TAX(QTR1%) <> 0.0) &
		FOR QTR1% = 0% TO 3%

3190	DELETE #PR_REG_TAXES.CH%

	GOTO 3100

	%PAGE

4000	!*******************************************************************
	! Fall through to exit program
	!*******************************************************************

 ExitProgram:
	!
	! Exit to next program or menu
	!
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitProgram

	END
