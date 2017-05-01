1	%TITLE "Fix Payroll End Date in Folder."
	%SBTTL "PR_SPEC_FIXFICA"
	%IDENT "V3.6a Calico"

	!
	!	COPYRIGHT (C) 2001 BY
	!	Software Solutions, Inc.
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
	! Software Solutions, Inc.
	!
	! Software Solutions, Inc. assumes no responsibility for the use
	! or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	! Abstract:HELP
	!	.p
	!	This program is a quick and dirty to fix the FICA
	!	YTD witheld at the end of the year by trading FICA
	!	taxes with FEDERAL taxes.
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_SPEC_FIXFICA
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_SPEC_FIXFICA, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_SPEC_FIXFICA.OBJ;*
	!
	! Author:
	!
	!	01/18/2001 - Kevin Handy
	!
	! Modification history:
	!
	!	01/19/2001 - Kevin Handy
	!		Have to add a pay record too. (blick)
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.HB"
	MAP (PR_TRN_PAY)	PR_TRN_PAY_CDD	PR_TRN_PAY

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.HB"
	MAP (PR_TRN_DED)	PR_TRN_DED_CDD	PR_TRN_DED

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_TABLE.HB"
	MAP (PR_TAX_TABLE)	PR_TAX_TABLE_CDD PR_TAX_TABLE

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.HB"
	MAP (PR_REG_TAXES) PR_REG_TAXES_CDD PR_REG_TAXES

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP (PR_EMP_MASTER) PR_EMP_MASTER_CDD PR_EMP_MASTER

	%PAGE

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	!
	! Look up device
	!
	CALL  READ_DEVICE("PR_TRN_PAY", PR_TRN_PAY.DEV$, STAT%)

	!
	! Ask for the date for the Check Journal
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(18%, 80%, SMG_SCREEN_DATA%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, 1%, 1%)

	BATCH_NO$ = DATE_TODAY

100	PR_TRN_PAY_DATE$ = ENTR_3DATE(SCOPE,  SMG_SCREEN_DATA%, "", &
		"Enter Check Journal Date to Create (MMDDYYYY)", &
		BATCH_NO$, 64%, "8", "")

	SELECT SCOPE::SCOPE_EXIT

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
		! Good key

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 100

	END SELECT

	PR_TRN_PAY_DATE$ = EDIT$(PR_TRN_PAY_DATE$, -1%)

	IF LEN(EDIT$(PR_TRN_PAY_DATE$, -1%)) <> 8%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Please enter the journal date in (MMDDYYYY) format", &
			0%)
		GOTO 100
	END IF

	BATCH_NO$ = PR_TRN_PAY_DATE$
	YYYY$ = LEFT(PR_TRN_PAY_DATE$, 4%)

190	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%)

	%PAGE

	!***************************************************************
	! Open all of the files
	!***************************************************************

	!
	! Open Payroll Check information file
	!
300	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.CRE"
	USE
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.CRE"
	USE
		FILENAME$ = "PR_TRN_DED"
		CONTINUE 320
	END WHEN

320	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.OPN"
	USE
		FILENAME$ = "PR_REG_TAXES"
		CONTINUE HelpError
	END WHEN

330	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

380	!
	! Open Tax Table file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_TABLE.OPN"

		GET #PR_TAX_TABLE.CH%, KEY #0% EQ "F", REGARDLESS
	USE
		FICA_EMPE_PCT = 0.0
		FICA_LIMIT = 0.0
		FICA_EMPE_PCT_HI = 0.0
		FICA_LIMIT_HI = 0.0

		FILENAME$ = "PR_TAX_TABLE_" + YYYY$
		CONTINUE HelpError
	END WHEN

	FICA_EMPE_PCT = (PR_TAX_TABLE::FICA_EMPE_PCT) / 10000.0
	FICA_LIMIT = PR_TAX_TABLE::FICA_LIMIT
	FICA_EMPE_PCT_HI = (PR_TAX_TABLE::FICA_EMPE_PCT_HI) / 10000.0
	FICA_LIMIT_HI = PR_TAX_TABLE::FICA_LIMIT_HI

	IF FICA_EMPE_PCT > 0.10
	THEN
		FICA_EMPE_PCT = FICA_EMPE_PCT / 10.0
		FICA_EMPE_PCT_HI = FICA_EMPE_PCT_HI / 10.0
	END IF


	!***************************************************************
	! Begin actually doing something
	!***************************************************************

	!
	! Get the first check to be blanked
	!
	TOTAL_FICA = 0.0

1000	WHEN ERROR IN
		RESET #PR_REG_TAXES.CH%
	USE
		FILENAME$ = "PR_REG_TAXES"
		CONTINUE HelpError
	END WHEN

1100	WHEN ERROR IN
		GET #PR_REG_TAXES.CH%, REGARDLESS
	USE
		CONTINUE 2000 IF ERR = 11%
		FILENAME$ = "PR_REG_TAXES"
		CONTINUE HelpError
	END WHEN

	SELECT PR_REG_TAXES::TTYPE
	CASE "FI"

		TAX = PR_REG_TAXES::TAX(0%) + &
			PR_REG_TAXES::TAX(1%) + &
			PR_REG_TAXES::TAX(2%) + &
			PR_REG_TAXES::TAX(3%)
		TAXABLE = PR_REG_TAXES::TAXABLE(0%) + &
			PR_REG_TAXES::TAXABLE(1%) + &
			PR_REG_TAXES::TAXABLE(2%) + &
			PR_REG_TAXES::TAXABLE(3%)
		TAXABLE = MIN(TAXABLE, FICA_LIMIT)
		CALCTAX = FUNC_ROUND(TAXABLE * FICA_EMPE_PCT, 2%)

		IF ABS(TAX - CALCTAX) > .05
		THEN
			GOSUB DoFederal IF EMPLOYEE$ <> PR_REG_TAXES::EMPNUM

			CALCTAX = FUNC_ROUND(CALCTAX - TAX, 2%)
			TOTAL_FICA = TOTAL_FICA + CALCTAX
			EMPLOYEE$ = PR_REG_TAXES::EMPNUM

			PR_TRN_DED::EMPNUM = PR_REG_TAXES::EMPNUM
			PR_TRN_DED::PR_END_DATE = PR_TRN_PAY_DATE$
			PR_TRN_DED::DTYPE = "C"
			PR_TRN_DED::CODE = "FI"
			PR_TRN_DED::AMOUNT = CALCTAX
			PR_TRN_DED::TAX_CODE = ""
			PR_TRN_DED::SSTATUS = ""
			PR_TRN_DED::EXEMPT = 0%
			PR_TRN_DED::UPDATE_FLAG = 0%
			PR_TRN_DED::BATCH = "FICA"
			PR_TRN_DED::TAXABLE = 0.0
			PR_TRN_DED::REPORTABLE = 0.0
			PR_TRN_DED::ADDEXEMPT = 0%

			PUT #PR_TRN_DED.CH%
		END IF

	CASE  "FH"

		TAX = PR_REG_TAXES::TAX(0%) + &
			PR_REG_TAXES::TAX(1%) + &
			PR_REG_TAXES::TAX(2%) + &
			PR_REG_TAXES::TAX(3%)
		TAXABLE = PR_REG_TAXES::TAXABLE(0%) + &
			PR_REG_TAXES::TAXABLE(1%) + &
			PR_REG_TAXES::TAXABLE(2%) + &
			PR_REG_TAXES::TAXABLE(3%)
		TAXABLE = MIN(TAXABLE, FICA_LIMIT_HI)
		CALCTAX = FUNC_ROUND(TAXABLE * FICA_EMPE_PCT_HI, 2%)

		IF ABS(TAX - CALCTAX) > .05
		THEN
			GOSUB DoFederal IF EMPLOYEE$ <> PR_REG_TAXES::EMPNUM

			CALCTAX = FUNC_ROUND(CALCTAX - TAX, 2%)
			TOTAL_FICA = TOTAL_FICA + CALCTAX
			EMPLOYEE$ = PR_REG_TAXES::EMPNUM

			PR_TRN_DED::EMPNUM = PR_REG_TAXES::EMPNUM
			PR_TRN_DED::PR_END_DATE = PR_TRN_PAY_DATE$
			PR_TRN_DED::DTYPE = "C"
			PR_TRN_DED::CODE = "FH"
			PR_TRN_DED::AMOUNT = CALCTAX
			PR_TRN_DED::TAX_CODE = ""
			PR_TRN_DED::SSTATUS = ""
			PR_TRN_DED::EXEMPT = 0%
			PR_TRN_DED::UPDATE_FLAG = 0%
			PR_TRN_DED::BATCH = "FICA"
			PR_TRN_DED::TAXABLE = 0.0
			PR_TRN_DED::REPORTABLE = 0.0
			PR_TRN_DED::ADDEXEMPT = 0%

			PUT #PR_TRN_DED.CH%
		END IF

	END SELECT

	GOTO 1100

2000	GOTO ExitProgram

	%PAGE

	!*******************************************************************
	! Slap out a federal record to offset the FICA
	!*******************************************************************

 DoFederal:
	IF EMPLOYEE$ <> "" AND TOTAL_FICA <> 0.0
	THEN
		CALL ENTR_3MESSAGE(SCOPE, EMPLOYEE$, 1%)

		TOTAL_FICA = FUNC_ROUND(TOTAL_FICA, 2%)

		PR_TRN_DED::EMPNUM = EMPLOYEE$
		PR_TRN_DED::PR_END_DATE = PR_TRN_PAY_DATE$
		PR_TRN_DED::DTYPE = "C"
		PR_TRN_DED::CODE = "FW"
		PR_TRN_DED::AMOUNT = - TOTAL_FICA
		PR_TRN_DED::TAX_CODE = ""
		PR_TRN_DED::SSTATUS = ""
		PR_TRN_DED::EXEMPT = 0%
		PR_TRN_DED::UPDATE_FLAG = 0%
		PR_TRN_DED::BATCH = "FICA"
		PR_TRN_DED::TAXABLE = 0.0
		PR_TRN_DED::REPORTABLE = 0.0
		PR_TRN_DED::ADDEXEMPT = 0%

		PUT #PR_TRN_DED.CH%

		WHEN ERROR IN
			GET #PR_EMP_MASTER.CH%, KEY #0% EQ EMPLOYEE$, REGARDLESS
		USE
			! Ignore any employee lookup errors, since these
			! records shouldn'e matter (much)
		END WHEN

		PR_TRN_PAY::EMPNUM = EMPLOYEE$
		PR_TRN_PAY::PR_END_DATE = PR_TRN_PAY_DATE$
		PR_TRN_PAY::EMP_SKILL = ""
		PR_TRN_PAY::EMP_GRADE = ""
		PR_TRN_PAY::ACCT = PR_EMP_MASTER::ACCT
		PR_TRN_PAY::SUBACC = PR_EMP_MASTER::SUBACC
		PR_TRN_PAY::OPER = PR_EMP_MASTER::OPER
		PR_TRN_PAY::LOCATION = PR_EMP_MASTER::LOCATION
		PR_TRN_PAY::DEPT = PR_EMP_MASTER::DEPT
		PR_TRN_PAY::WORK_CENTER = PR_EMP_MASTER::WORK_CENTER
		PR_TRN_PAY::UNION = PR_EMP_MASTER::UNION
		PR_TRN_PAY::PTYPE = "O"
		PR_TRN_PAY::RTYPE = PR_EMP_MASTER::RATE_TYPE
		PR_TRN_PAY::CODE = PR_EMP_MASTER::RATE_CDE
		PR_TRN_PAY::PIECE_RATE = 0.0
		PR_TRN_PAY::HOUR_RATE = 0.0
		PR_TRN_PAY::REG_HR = 0.0
		PR_TRN_PAY::OVT_HR = 0.0
		PR_TRN_PAY::PIECE = 0.0
		PR_TRN_PAY::FACTOR = 0%
		PR_TRN_PAY::GROSS = 0%
		PR_TRN_PAY::TAX_PKG = PR_EMP_MASTER::TAX_PKG
		PR_TRN_PAY::BATCH_ENTRY = "FICA"
		PR_TRN_PAY::UPDATE_FLAG = 0%
		PR_TRN_PAY::SEQNUM = ""
		PR_TRN_PAY::BATCH = "FICA"
		PR_TRN_PAY::WORKDATE = ""
		PR_TRN_PAY::EQUIPMENT = ""
		PR_TRN_PAY::EQUIPHOUR = 0.0

		PUT #PR_TRN_PAY.CH%
	END IF

	EMPLOYEE$ = ""
	TOTAL_FICA = 0.0

	RETURN

	%PAGE

 ExitProgram:
	!******************************************************************
	! Exit the program
	!******************************************************************

	GOSUB DoFederal

	!
	! Erase Display
	!
	SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_SCREEN_DATA%)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE

 HelpError:
19990	!
	! This moved from inside error to outside so that errors occuring
	! at lower levels could be trapped.  Basic will not allow any
	! error to occur inside of an error no matter if it is in a
	! different module.
	!
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	GOTO ExitProgram

	END
