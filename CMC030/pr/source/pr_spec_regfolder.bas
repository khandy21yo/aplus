1	%TITLE "CRFLDR - Create a Folder from Quarter of Register"
	%SBTTL "PR_SPEC_REGFOLDER"
	%IDENT "V3.6a Calico"

	!
	!	COPYRIGHT (C) 1989 BY
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
	!	The ^*Create Folder\* routine is a special routine used to
	!	create a payroll folder from one quarter in the payroll
	!	register.
	!	.p
	!	This routine is designed so that some reports can be
	!	run including information from a quarter that has been initialized
	!	by manually setting the information in the first quarter(s)
	!	of a payroll register.
	!
	! Index:
	!	.X Create Folder>Special
	!	.x Special>Create Folder
	!
	! Option:
	!
	!	PR_SPEC_REGFOLDER$CONFIRM
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_SPEC_REGFOLDER
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_SPEC_REGFOLDER, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_SPEC_REGFOLDER.OBJ;*
	!
	! Author:
	!
	!	08/16/89 - Kevin Handy
	!
	! Modification history:
	!
	!	09/19/89 - Kevin Handy
	!		Modified to take items posted to the Pay file
	!		that do not have a type of "P", and have no
	!		hours, any place them in the deduction folder.
	!
	!	12/13/90 - Kevin Handy
	!		Added PR_HID_DED::ADDEXEMPT field.
	!
	!	01/14/91 - Craig Tanner
	!		Added YYYY$ to some filename$ in error trapping.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!		Fix last param to entr_3choices.
	!
	!	10/25/96 - Kevin Handy
	!		Reformat source code
	!
	!	03/13/97 - Kevin Handy
	!		Modified to handle FH codes
	!		Use integer for #key
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/11/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP	(PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_PAY.HB"
	MAP (PR_HIS_PAY)	PR_HIS_PAY_CDD	PR_HIS_PAY

	%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_DED.HB"
	MAP (PR_HIS_DED)	PR_HIS_DED_CDD	PR_HIS_DED

	%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_CHECK.HB"
	MAP (PR_HIS_CHECK)	PR_HIS_CHECK_CDD	PR_HIS_CHECK

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
	! List of Quarter
	!
	QTRTITLE$ = "QTR  Description"

	QTR$(0%) = "4"
	QTR$(1%) = "1    First Quarter"
	QTR$(2%) = "2    Second Quarter"
	QTR$(3%) = "3    Third Quarter"
	QTR$(4%) = "4    Fourth Quarter"

	!
	! Look up device
	!
	CALL  READ_DEVICE("PR_REG_TAXES", PR_REG_TAXES.DEV$, STAT%)

	CALL FIND_FILE(PR_REG_TAXES.DEV$ + "PR_REG_TAXES_*.LED", &
		YYYY_FILE$(), &
		16%, "", "")

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

	SELECT ENTR_3ENTER(SCOPE, SMG_SCREEN_DATA%, 8%, 38%, &
		PR_REG_YYYY$, 0%, 0%)

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
	! Ask for quarter
	!*******************************************************************

	!
	! Paint the background
	!
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
		"Payroll Folder Create " + TRM$(SCOPE::PRG_COMPANY), &
		SMG$K_TOP &
	)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Payroll Date", 4%, 5%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Quarter ", &
		6%, 5%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Employee # ", 10%, 5%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, &
		2%, &
		2% &
	)

	!*******************************************************************
	! Enter payroll date to create
	!*******************************************************************

	BATCH_NO$ = DATE_TODAY

505	PR_HIS_PAY_DATE$ = ENTR_3DATE(SCOPE,  SMG_SCREEN_DATA%, "4;35", &
		"Enter Payroll Folder Date (MMDDYYYY) ", &
		BATCH_NO$, 0%, "8", "")

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
		! Good key

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 505
	END SELECT

	PR_HIS_PAY_DATE$ = EDIT$(PR_HIS_PAY_DATE$, -1%)

	IF LEN(EDIT$(PR_HIS_PAY_DATE$, -1%)) <> 8%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "Please enter the folder date in " + &
			"(MMDDYYYY) format", 0%)
		GOTO 505
	END IF

	BATCH_NO$ = PR_HIS_PAY_DATE$

510	SCOPE::PRG_ITEM = "FLD02QUARTER"
	!++
	! Abstract:FLD02QUARTER
	!	^* Quarter\*
	!	.p
	!	The ^*Quarter\* field allows for entry of the calendar quarter for which
	!	the folder will be created. This field requires an entry and will accommodate
	!	a one (1) digit number.
	!
	! Index:
	!	.x Create Folder>Quarter
	!
	!--

	QTR% = (VAL%(MID(BATCH_NO$, 5%, 2%)) - 1%) / 3% + 1%

	QTR$ = NUM1$(QTR%)

	QTR$ = EDIT$(ENTR_3STRINGLIST(SCOPE,  SMG_SCREEN_DATA%, "6;35", &
		"Enter the Quarter", QTR$, 8%, "'", &
		"", QTR$(), QTRTITLE$, "005"), -1%)

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
		! Good key

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 510
	END SELECT

	QTR% = VAL%(QTR$)
	QTR1% = QTR% - 1%

	!*******************************************************************
	! Confirm creation of new TRN file.
	!*******************************************************************

	SCOPE::PRG_ITEM = "CONFIRM"
	INP$ = ENTR_3YESNO(SCOPE, SCOPE::SMG_OPTION, "", &
		"Confirm Payroll Update process  - then press <Do> ", &
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

620	!
	! Open employee master file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
	USE
		FILENAME$ = "PR_EMP_MASTER"
		EXIT HANDLER
	END WHEN

630	!
	! Open Pay folder
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_PAY.CRE"
	USE
		FILENAME$ = "PR_HIS_PAY"
		EXIT HANDLER
	END WHEN

635	WHEN ERROR IN
		RESET #PR_HIS_PAY.CH%
		GET #PR_HIS_PAY.CH%
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE 640 IF ERR = 11%
		FILENAME$ = "PR_HIS_PAY"
		EXIT HANDLER
	END WHEN

	CALL ENTR_3MESSAGE(SCOPE, "Payroll folder is not empty!", 0%)
	GOTO ExitProgram

640	!
	! Open Deduction folder
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_DED.CRE"
	USE
		FILENAME$ = "PR_HIS_DED"
		EXIT HANDLER
	END WHEN

650	!
	! Open Check folder
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_CHECK.CRE"
	USE
		FILENAME$ = "PR_HIS_CHECK"
		EXIT HANDLER
	END WHEN

670	!
	! Open TaxWH register
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.OPN"
	USE
		FILENAME$ = "PR_REG_TAXES_" + YYYY$
		EXIT HANDLER
	END WHEN

680	!
	! Open ERNDED register
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.OPN"
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

2110	IF (PR_REG_ERNDED::QTR_DOLL(QTR1%) <> 0.0) OR &
		(PR_REG_ERNDED::REG_HRS(QTR1%) <> 0.0) OR &
		(PR_REG_ERNDED::PRE_HRS(QTR1%) <> 0.0) OR &
		(PR_REG_ERNDED::UNITS(QTR1%) <> 0.0)
	THEN
		THIS_EMPLOYEE$ = PR_REG_ERNDED::EMPNUM
		GOSUB 16000

		IF (PR_REG_ERNDED::ETYPE = "P")
		THEN
			PR_HIS_PAY::EMPNUM	= PR_REG_ERNDED::EMPNUM
			PR_HIS_PAY::PR_END_DATE	= BATCH_NO$
			PR_HIS_PAY::EMP_SKILL	= PR_EMP_MASTER::EMP_SKILL
			PR_HIS_PAY::EMP_GRADE	= PR_EMP_MASTER::EMP_GRADE
			PR_HIS_PAY::ACCT	= PR_EMP_MASTER::ACCT
			PR_HIS_PAY::SUBACC	= PR_EMP_MASTER::SUBACC
			PR_HIS_PAY::OPER	= PR_EMP_MASTER::OPER
			PR_HIS_PAY::LOCATION	= PR_EMP_MASTER::LOCATION
			PR_HIS_PAY::DEPT	= PR_EMP_MASTER::DEPT
			PR_HIS_PAY::WORK_CENTER	= PR_EMP_MASTER::WORK_CENTER
			PR_HIS_PAY::UNION	= PR_EMP_MASTER::UNION
			PR_HIS_PAY::PTYPE	= "P"
			PR_HIS_PAY::RTYPE	= "S"
			PR_HIS_PAY::CODE	= PR_REG_ERNDED::CODE
			PR_HIS_PAY::PIECE_RATE	= 0.0

			IF PR_REG_ERNDED::REG_HRS(QTR1%) <> 0.0 OR &
				PR_REG_ERNDED::PRE_HRS(QTR1%) <> 0.0
			THEN
				PR_HIS_PAY::HOUR_RATE = &
					FUNC_ROUND(PR_REG_ERNDED::QTR_DOLL(QTR1%) / &
					(PR_REG_ERNDED::REG_HRS(QTR1%) + &
					PR_REG_ERNDED::PRE_HRS(QTR1%) * 1.5), 3%)
			ELSE
				PR_HIS_PAY::HOUR_RATE = 0.0
			END IF

			PR_HIS_PAY::REG_HR	= PR_REG_ERNDED::REG_HRS(QTR1%)
			PR_HIS_PAY::OVT_HR	= PR_REG_ERNDED::PRE_HRS(QTR1%)
			PR_HIS_PAY::PIECE	= PR_REG_ERNDED::UNITS(QTR1%)
			PR_HIS_PAY::FACTOR	= 150%
			PR_HIS_PAY::GROSS	= &
				FUNC_ROUND((PR_HIS_PAY::REG_HR + &
				PR_HIS_PAY::OVT_HR * 1.5) * &
				PR_HIS_PAY::HOUR_RATE, 2%)
			PR_HIS_PAY::TAX_PKG	= PR_EMP_MASTER::TAX_PKG
			PR_HIS_PAY::BATCH_ENTRY	= "999999"
			PR_HIS_PAY::UPDATE_FLAG	= 15%
			PR_HIS_PAY::SEQNUM	= ""
			PR_HIS_PAY::BATCH	= "999999"
			PR_HIS_PAY::WORKDATE	= ""

			IF PR_HIS_PAY::HOUR_RATE <> 0.0 OR &
				PR_HIS_PAY::REG_HR <> 0.0 OR &
				PR_HIS_PAY::OVT_HR <> 0.0 OR &
				PR_HIS_PAY::PIECE <> 0.0
			THEN
				PUT #PR_HIS_PAY.CH%
			END IF

			!
			! Hack to try to force final net amount correct.
			!
			OFFSET = FUNC_ROUND(PR_REG_ERNDED::QTR_DOLL(QTR1%) - &
				PR_HIS_PAY::GROSS, 2%)
			IF OFFSET <> 0.0
			THEN
				PR_HIS_PAY::PTYPE	= "O"
				PR_HIS_PAY::RTYPE	= "S"
				PR_HIS_PAY::CODE	= PR_REG_ERNDED::CODE
				PR_HIS_PAY::PIECE_RATE	= 0.0
				PR_HIS_PAY::HOUR_RATE	= 0.0
				PR_HIS_PAY::REG_HR	= 0.0
				PR_HIS_PAY::OVT_HR	= 0.0
				PR_HIS_PAY::PIECE	= 0.0
				PR_HIS_PAY::FACTOR	= 0%
				PR_HIS_PAY::GROSS	= OFFSET

				PUT #PR_HIS_PAY.CH%
			END IF

		ELSE
			PR_HIS_DED::EMPNUM	= PR_REG_ERNDED::EMPNUM
			PR_HIS_DED::PR_END_DATE	= BATCH_NO$
			PR_HIS_DED::DTYPE	= PR_REG_ERNDED::ETYPE
			PR_HIS_DED::CODE	= PR_REG_ERNDED::CODE
			PR_HIS_DED::AMOUNT	= PR_REG_ERNDED::QTR_DOLL(QTR1%)
			PR_HIS_DED::TAX_CODE	= ""
			PR_HIS_DED::SSTATUS	= ""
			PR_HIS_DED::EXEMPT	= 0%
			PR_HIS_DED::ADDEXEMPT	= 0%
			PR_HIS_DED::UPDATE_FLAG	= 15%
			PR_HIS_DED::BATCH	= "999999"

			PUT #PR_HIS_DED.CH%
		END IF
	END IF

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

3110	IF (PR_REG_TAXES::TAXABLE(QTR1%) <> 0.0) OR &
		(PR_REG_TAXES::REPORTABLE(QTR1%) <> 0.0) OR &
		(PR_REG_TAXES::TAX(QTR1%) <> 0.0)
	THEN
		PR_HIS_DED::EMPNUM	= PR_REG_TAXES::EMPNUM
		PR_HIS_DED::PR_END_DATE	= BATCH_NO$
		IF INSTR(1%, "SW!SX!CW!DW!SI!FI!FH!FW", PR_REG_TAXES::TTYPE)
		THEN
			PR_HIS_DED::DTYPE	= "C"
		ELSE
			PR_HIS_DED::DTYPE	= "D"
		END IF
		PR_HIS_DED::CODE	= PR_REG_TAXES::TTYPE
		PR_HIS_DED::AMOUNT	= PR_REG_TAXES::TAX(QTR1%)
		PR_HIS_DED::TAX_CODE	= PR_REG_TAXES::CODE
		PR_HIS_DED::SSTATUS	= ""
		PR_HIS_DED::EXEMPT	= 0%
		PR_HIS_DED::ADDEXEMPT	= 0%
		PR_HIS_DED::UPDATE_FLAG	= 15%
		PR_HIS_DED::BATCH	= "999999"

		PUT #PR_HIS_DED.CH%

	END IF

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

16000	!*******************************************************************
	! Search for one employee (THIS_EMPLOYEE$)
	!*******************************************************************

	IF (THIS_EMPLOYEE$ <> PR_EMP_MASTER::EMPNUM)
	THEN
		PR_EMP_MASTER::EMPNUM	= ""
		PR_EMP_MASTER::SUBACC	= ""
		PR_EMP_MASTER::ACCT	= ""
		PR_EMP_MASTER::OPER	= ""
		PR_EMP_MASTER::UNION	= ""
		PR_EMP_MASTER::LOCATION	= ""
		PR_EMP_MASTER::DEPT	= ""
		PR_EMP_MASTER::WORK_CENTER	= ""
		PR_EMP_MASTER::EMP_SKILL	= ""
		PR_EMP_MASTER::EMP_GRADE	= ""
		PR_EMP_MASTER::TAX_PKG	= ""

		WHEN ERROR IN
			GET #PR_EMP_MASTER.CH%, &
				KEY #0% EQ THIS_EMPLOYEE$, REGARDLESS
		USE
			CONTINUE 16010
		END WHEN
	END IF

16010	RETURN

	END
