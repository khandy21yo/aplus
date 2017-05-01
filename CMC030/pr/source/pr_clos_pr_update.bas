1	%TITLE "UPDATE - Update Payroll Ledger"
	%SBTTL "PR_CLOS_PR_UPDATE"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1990 BY
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
	!	The ^*Update Payroll Ledger\* option
	!	accesses the routine which updates a specific
	!	payroll file folder into the Payroll Ledger file.
	!	.b
	!	A payroll register is always updated into a specific calendar
	!	quarter.
	!	.b
	!	A payroll folder must always be posted before the Update option is executed.
	!	.b
	!	^*NOTE:\* A record in a payroll file folder ^~cannot\~ be edited
	!	after the file folder has been updated.
	!	.lm -5
	!
	! Index:
	!	.x Update Payroll Register
	!
	! Option:
	!	PR_CLOS_PR_UPDATE$CONFIRM
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_CLOS_PR_UPDATE/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_CLOS_PR_UPDATE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_CLOS_PR_UPDATE.OBJ;*
	!
	! Author:
	!
	!	11/27/87 - Robert Peterson
	!
	!	12/31/90 - Kevin Handy
	!		Nearly a total re-write of previous version.
	!
	! Modification history:
	!
	!	04/11/88 - Robert Peterson
	!		Update counter for the pay deduction file was being
	!		set equal to the pr control counter before the
	!		test to see if they were equal.  Therefore the
	!		update of standard deductions and accrual payments
	!		was always skipped.
	!
	!	03/27/89 - Kevin Handy
	!		Fixed bug where is used CHW instead of CWH for
	!		the city witholding tax.
	!
	!	04/03/89 - Kevin Handy
	!		Modified to handle SI (WC Insurance).  Fixed loops
	!		to use length of SUBJECT_TYPE_TABLE$ instead of
	!		being hard-coded.
	!
	!	04/04/89 - Kevin Handy
	!		Fixed bug where would crash at 18300 with a subscript
	!		out of range.  TOTAL() was too small.
	!
	!	04/06/89 - Kevin Handy
	!		Modified display to add additional lines.
	!
	!	09/14/89 - Kevin Handy
	!		Untangled error trapping
	!
	!	04/09/90 - Kevin Handy
	!		Fixed bug where PKG_WH_CODE$() array wasn't being
	!		completely zeroed out, thus causing the wrong states
	!		to be updated to the registers for some employees.
	!
	!	10/22/90 - Kevin Handy
	!		Modified to be a report.
	!
	!	12/31/90 - Kevin Handy
	!		Massive changes due to the changes made in the
	!		layout of the tax register. (Update now much simpler)
	!
	!	01/11/91 - Craig Tanner
	!		Added YYYY$ to some filename$ in error trapping.
	!
	!	01/24/91 - Kevin Handy
	!		Modified to save type "O" in ernded file as type
	!		"P" in ERNDED file to match what was before.
	!
	!	07/12/91 - Kevin Handy
	!		Fixed bug at 2030 that caused multiple checks
	!		not roll up properly.
	!
	!	07/14/91 - Kevin Handy
	!		Modified to update hours instead of dollars
	!		for Northwest Center's odd accrual, to give me
	!		time to re-design the entire accrual system.
	!
	!	01/04/92 - Kevin Handy
	!		Modified for PR_EMP_ACCRUAL accruals.
	!
	!	04/13/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	02/17/92 - Kevin Handy
	!		Modify "NAM" so it has device on both "from"
	!		and "to" sides, so that it won't move file to
	!		users default directory.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	07/11/95 - Kevin Handy
	!		Skip over opening PR_EMP_STD_ERNDED if the file
	!		doesn't exist. Don't require it. (Jet)
	!
	!	01/15/96 - Kevin Handy
	!		Add a new accrual code "3".
	!
	!	03/12/97 - Kevin Handy
	!		Handle FH code like FI.
	!
	!	04/16/97 - Kevin Handy
	!		Only update taxable/reprtable on "C" records.
	!
	!	10/15/97 - Kevin Handy
	!		Clean up/reformat source code
	!
	!	11/12/97 - Kevin Handy
	!		Increase dimensions from 100 to 200.
	!
	!	05/29/98 - Kevin Handy
	!		Modified to handle 'F' final deduction code.
	!		Also handle "O" like "P".
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/21/99 - Kevin Handy
	!		Fix unsolicited input
	!
	!	12/04/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP (PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_CONTROL.HB"
	MAP (PR_CONTROL)	PR_CONTROL_CDD	PR_CONTROL

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_STD_ERNDED.HB"
	MAP (PR_EMP_STD_ERNDED)		PR_EMP_STD_ERNDED_CDD	PR_EMP_STD_ERNDED

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.HB"
	MAP (PR_TRN_PAY)	PR_TRN_PAY_CDD	PR_TRN_PAY

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.HB"
	MAP (PR_TRN_DED)	PR_TRN_DED_CDD	PR_TRN_DED

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_CHECK.HB"
	MAP (PR_TRN_CHECK)	PR_TRN_CHECK_CDD	PR_TRN_CHECK

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.HB"
	MAP (PR_REG_TAXES)	PR_REG_TAXES_CDD	PR_REG_TAXES

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.HB"
	MAP (PR_REG_ERNDED)	PR_REG_ERNDED_CDD	PR_REG_ERNDED

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_ACCRUAL.HB"
	MAP (PR_EMP_ACCRUAL)	PR_EMP_ACCRUAL_CDD	PR_EMP_ACCRUAL

	MAP (DP_OUTP_XUNSOL) RRR_FLAG%

	!
	! Records
	!
	RECORD TAX_RECORD
		STRING	TTYPE = 2
		STRING	CODE = 2
		GFLOAT	TAX
		GFLOAT	TAXABLE
		GFLOAT	REPORTABLE
	END RECORD

	DIM TAX_RECORD TAX(200%)

	RECORD ERNDED_RECORD
		STRING	ETYPE = 1
		STRING	CODE = 2
		GFLOAT	QTR_DOLL
		GFLOAT	REG_HRS
		GFLOAT	PRE_HRS
		GFLOAT	UNITS
	END RECORD

	DIM ERNDED_RECORD ERNDED(200%)

	!
	! External functions
	!
	EXTERNAL LONG	OUTP_XUNSOL ! (It's really an AST routine)

	!
	! Dimension
	!
	DIM TOTAL(16%), DISPLAY$(18%)

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize
	!*******************************************************************

	CALL OUTP_INITFROMFILE(UTL_REPORTX, 80%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)
	BATCH_NO$ = DATE_STOREDATE(BATCH_NO$) ! Reformat to (YYYYMMDD)
	YYYY$ = LEFT(BATCH_NO$, 4%)
	!++
	! Abstract:FLD01
	!	.x Folder Date>Reverse PR Ledger Update
	!	^*(01) Payroll Folder Date\*
	!	.b
	!	.lm +5
	!	The ^*Payroll Folder Date\* field selects an existing folder
	!	to update.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!	.x Reverse PR Ledger Update>Folder Date
	!
	!--

	QTR$ = LEFT(UTL_REPORTX::OPTDEF(1%), 1%)
	QTR% = VAL%(QTR$)

	!++
	! Abstract:FLD02
	!	.x Quarter>Reverse PR Ledger Update
	!	^*(02) Quarter\*
	!	.b
	!	.lm +5
	!	The ^*Quarter\* field selects the accounting quarter for which
	!	this report will print.
	!	.b
	!	Valid entries are:
	!	.table 3,25
	!	.te
	!	#####^*1\*
	!	.te
	!	#####^*2\*
	!	.te
	!	#####^*3\*
	!	.te
	!	#####^*4\*
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Reverse PR Ledger Update>Quarter
	!
	!--

	!
	! Set up titles and whatnot
	!
	TITLE$(1%) = "Payroll Update Process"
	TITLE$(2%) = "For the Payroll Folder Dated: " + &
		PRNT_DATE(BATCH_NO$, 8%)

	TITLE$(3%) = "Quarter " + QTR$
	TITLE$(4%) = ""
	TITLE$(5%) = ""

	!
	! Other Variables
	!
	TAX_TYPE_TABLE$ = "FI!FW!SW!SX!CW!DW!EW!SU!SI!FH!"

	%Page

300	!
	! Open Employee master file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.MOD"
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

310	!
	! Open Employee standard deductino file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_STD_ERNDED.MOD"
	USE
		CONTINUE 320 IF ERR = 5%
		FILENAME$ = "PR_EMP_STD_ERNDED"
		CONTINUE HelpError
	END WHEN

320	!
	! Open Payroll control file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_CONTROL.MOD"
		GET #PR_CONTROL.CH%, RECORD 1%
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		FILENAME$ = "PR_CONTROL"
		CONTINUE HelpError
	END WHEN

	IF PR_CONTROL::CLOSEFLAG = "2"
	THEN
		CALL HELP_3MESSAGE(SCOPE, "PR Reset in process", "ERR", &
			"PR_RESET", "ERROR_RESET")
		GOTO ExitProgram
	END IF

330	!
	! Open Pay folder
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.UPD"
	USE
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

340	!
	! Open Deduction folder
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.UPD"
	USE
		FILENAME$ = "PR_TRN_DED"
		CONTINUE HelpError
	END WHEN

350	!
	! Open Check folder
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_CHECK.UPD"
	USE
		FILENAME$ = "PR_TRN_CHECK"
		CONTINUE HelpError
	END WHEN

370	!
	! Open TaxES register
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.CRE"
	USE
		FILENAME$ = "PR_REG_TAXES_" + YYYY$
		CONTINUE HelpError
	END WHEN

380	!
	! Open ERNDED register
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.CRE"
	USE
		FILENAME$ = "PR_REG_ERNDED_" + YYYY$
		CONTINUE HelpError
	END WHEN

390	!
	! Open accrual file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_ACCRUAL.MOD"
	USE
		CONTINUE 500
	END WHEN

	%PAGE

500	!
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
		"Payroll Update " + TRM$(SCOPE::PRG_COMPANY), &
		SMG$K_TOP &
	)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Payroll Date", 4%, 5%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		PRNT_DATE(BATCH_NO$, 8%), &
		4%, 35%, , SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Quarter ", &
		6%, 5%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, &
		2%, &
		2% &
	)

	!
	! Set help message
	!
	SCOPE::PRG_ITEM = "HELP"

	%PAGE

1000	!******************************************************************
	! Updating payroll register
	!******************************************************************
	CALL ENTR_3MESSAGE(SCOPE, "Updating payroll register", 1%)

	TAX_COUNT%, ERNDED_COUNT% = 0%

	RESET #PR_TRN_PAY.CH%

	!
	! Set up to trap interrupt
	!
	SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
		LOC(OUTP_XUNSOL) BY VALUE, &
		LOC(SCOPE::SMG_KBID) BY VALUE)

	RRR_FLAG% = 0%

1010	!
	! Set close flag in period file
	!
	PR_CONTROL::UR_DATE = BATCH_NO$

	IF PR_CONTROL::CLOSEFLAG <> "1"
	THEN
		PR_CONTROL::UR_COUNT = PR_CONTROL::UR_COUNT + 1%
	END IF

	PR_CONTROL::CLOSEFLAG = "1"

	WHEN ERROR IN
		UPDATE #PR_CONTROL.CH%
	USE
		FILENAME$ = "PR_CONTROL"
		CONTINUE HelpError
	END WHEN

1020	!*******************************************************************
	! Main loop starts here for pay file
	!*******************************************************************

	!
	! Check for an interupt
	!
	GOSUB Interupt IF RRR_FLAG%

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #PR_TRN_PAY.CH%
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

1025	IF TEST_EMPNUM$ <> PR_TRN_PAY::EMPNUM
	THEN
		GOSUB ReadDedFile IF TEST_EMPNUM$ <> ""
		GOSUB WriteTotals IF TEST_EMPNUM$ <> ""

		TEST_EMPNUM$ = PR_TRN_PAY::EMPNUM

		CALL ENTR_3MESSAGE(SCOPE, &
			"Updating Employee " + TEST_EMPNUM$, 1%)

	END IF


1030	!
	! Scan for ERNDED record already exists
	!
	IF PR_TRN_PAY::PTYPE = "O"
	THEN
		PTYPE$ = "P"
	ELSE
		PTYPE$ = PR_TRN_PAY::PTYPE
	END IF

	GOTO 1035 &
		IF (ERNDED(I%)::ETYPE = PTYPE$) AND &
		(ERNDED(I%)::CODE = PR_TRN_PAY::CODE) &
		FOR I% = 1% TO ERNDED_COUNT%

	ERNDED_COUNT%, I% = ERNDED_COUNT% + 1%

	ERNDED(I%)::ETYPE	= PTYPE$
	ERNDED(I%)::CODE	= PR_TRN_PAY::CODE
	ERNDED(I%)::QTR_DOLL	= 0.0
	ERNDED(I%)::REG_HRS	= 0.0
	ERNDED(I%)::PRE_HRS	= 0.0
	ERNDED(I%)::UNITS	= 0.0

1035	ERNDED(I%)::QTR_DOLL	= &
		FUNC_ROUND(ERNDED(I%)::QTR_DOLL + &
		PR_TRN_PAY::GROSS, 2%)
	ERNDED(I%)::REG_HRS	= &
		FUNC_ROUND(ERNDED(I%)::REG_HRS + &
		PR_TRN_PAY::REG_HR, 2%)
	ERNDED(I%)::PRE_HRS = &
		FUNC_ROUND(ERNDED(I%)::PRE_HRS + &
		PR_TRN_PAY::OVT_HR, 2%)
	ERNDED(I%)::UNITS = &
		FUNC_ROUND(ERNDED(I%)::UNITS + &
		PR_TRN_PAY::PIECE, 2%)


1080	PR_TRN_PAY::UPDATE_FLAG = PR_TRN_PAY::UPDATE_FLAG OR 1%

	UPDATE #PR_TRN_PAY.CH%

	GOTO 1020

 ReadDedFile:
2000	!********************************************************************
	! Get Tax/Ded detail information
	!********************************************************************

	WHEN ERROR IN
		FIND #PR_TRN_DED.CH%, KEY #0% EQ TEST_EMPNUM$
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE 2100
	END WHEN

2020	WHEN ERROR IN
		GET #PR_TRN_DED.CH%
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE 2100
	END WHEN

	!
	! Skip out when done with this employee
	!
	GOTO 2100 IF TEST_EMPNUM$ <> PR_TRN_DED::EMPNUM

	!
	! See if this is a tax
	!
	TAX_TYPE% = (INSTR(1%, TAX_TYPE_TABLE$, PR_TRN_DED::CODE) + 2%) / 3%

	!
	! If this is a tax record, then sum it into the TAX file,
	! otherwise it must be summed into the ERNDED file.
	!
	GOTO 2040 IF TAX_TYPE% = 0%

2030	!
	! Scan for ERNDED record already exists
	!
	GOTO 2035 &
		IF (TAX(I%)::TTYPE = PR_TRN_DED::CODE) AND &
		(TAX(I%)::CODE = PR_TRN_DED::TAX_CODE) &
		FOR I% = 1% TO TAX_COUNT%

	TAX_COUNT%, I% = TAX_COUNT% + 1%

	TAX(I%)::TTYPE		= PR_TRN_DED::CODE
	TAX(I%)::CODE		= PR_TRN_DED::TAX_CODE
	TAX(I%)::TAX		= 0.0
	TAX(I%)::TAXABLE	= 0.0
	TAX(I%)::REPORTABLE	= 0.0

2035	TAX(I%)::TAX		= &
		FUNC_ROUND(TAX(I%)::TAX + &
		PR_TRN_DED::AMOUNT, 2%)

	IF PR_TRN_DED::DTYPE = "C"
	THEN
		TAX(I%)::TAXABLE	= &
			FUNC_ROUND(TAX(I%)::TAXABLE + &
			PR_TRN_DED::TAXABLE, 2%)

		TAX(I%)::REPORTABLE = &
			FUNC_ROUND(TAX(I%)::REPORTABLE + &
			PR_TRN_DED::REPORTABLE, 2%)
	END IF

	GOTO 2090

2040	!
	! Scan for ERNDED record already exists
	!
	GOTO 2045 &
		IF (ERNDED(I%)::ETYPE = PR_TRN_DED::DTYPE) AND &
		(ERNDED(I%)::CODE = PR_TRN_DED::CODE) &
		FOR I% = 1% TO ERNDED_COUNT%

	ERNDED_COUNT%, I% = ERNDED_COUNT% + 1%

	ERNDED(I%)::ETYPE	= PR_TRN_DED::DTYPE
	ERNDED(I%)::CODE	= PR_TRN_DED::CODE
	ERNDED(I%)::QTR_DOLL	= 0.0
	ERNDED(I%)::REG_HRS	= 0.0
	ERNDED(I%)::PRE_HRS	= 0.0
	ERNDED(I%)::UNITS	= 0.0

2045	ERNDED(I%)::QTR_DOLL	= &
		FUNC_ROUND(ERNDED(I%)::QTR_DOLL + &
		PR_TRN_DED::AMOUNT, 2%)


2090	PR_TRN_DED::UPDATE_FLAG = PR_TRN_DED::UPDATE_FLAG OR 1%

	UPDATE #PR_TRN_DED.CH%

	GOTO 2020

2100	RETURN

	%Page

 WriteTotals:
8000	!*********************************************************************
	! Update Employee master file
	!*********************************************************************

	WHEN ERROR IN
		GET #PR_EMP_MASTER.CH%, KEY #0% EQ TEST_EMPNUM$
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

	WKS% = PR_EMP_MASTER::PAYFREQ

	WKS% = 52% / WKS% IF WKS% <> 0%

	IF PR_EMP_MASTER::ACTIVE_FLAG <> "Y"
	THEN
		PR_EMP_MASTER::ACTIVE_FLAG = "Y"

		WHEN ERROR IN
			UPDATE #PR_EMP_MASTER.CH%
		USE
			FILENAME$ = "PR_EMP_MASTER"
			CONTINUE HelpError
		END WHEN
	END IF

	!*******************************************************************
	! Update Earnings/Deduction File
	!*******************************************************************

8005	FOR LOOP% = 1% TO ERNDED_COUNT%

		!
		! Accrual
		!
		GOTO 8010 IF ERNDED(LOOP%)::ETYPE = "A"

		!
		! Summarize for the totals
		!
		SELECT ERNDED(LOOP%)::ETYPE

		CASE "P", "O"
			TOTAL(1%) = FUNC_ROUND(TOTAL(1%) + &
				ERNDED(LOOP%)::QTR_DOLL, 2%)

		CASE "D", "F"
			TOTAL(2%) = FUNC_ROUND(TOTAL(2%) + &
				ERNDED(LOOP%)::QTR_DOLL, 2%)

		CASE "T"
			TOTAL(3%) = FUNC_ROUND(TOTAL(3%) + &
				ERNDED(LOOP%)::QTR_DOLL, 2%)

		CASE "M"
			TOTAL(4%) = FUNC_ROUND(TOTAL(4%) + &
				ERNDED(LOOP%)::QTR_DOLL, 2%)

		END SELECT

		TOTAL(5%) = FUNC_ROUND(TOTAL(5%) + ERNDED(LOOP%)::REG_HRS, 2%)
		TOTAL(6%) = FUNC_ROUND(TOTAL(6%) + ERNDED(LOOP%)::PRE_HRS, 2%)
		TOTAL(7%) = FUNC_ROUND(TOTAL(7%) + ERNDED(LOOP%)::UNITS, 2%)

8010		!
		! First, we try to load in an existing record
		!
		WHEN ERROR IN
			GET #PR_REG_ERNDED.CH%, &
				KEY #0% EQ TEST_EMPNUM$ + &
				ERNDED(LOOP%)::ETYPE + &
				ERNDED(LOOP%)::CODE
		USE
			IF ERR = 154%	! Locked Block
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 8015
		END WHEN

		PUT_FLAG% = 0%

		GOTO 8020

8015		!
		! Since it doesn't exist, we must create it
		!
		PUT_FLAG% = -1%

		PR_REG_ERNDED::EMPNUM	= TEST_EMPNUM$
		PR_REG_ERNDED::ETYPE	= ERNDED(LOOP%)::ETYPE
		PR_REG_ERNDED::CODE	= ERNDED(LOOP%)::CODE
		FOR I% = 0% TO 3%
			PR_REG_ERNDED::QTR_DOLL(I%)	= 0.0
			PR_REG_ERNDED::REG_HRS(I%)	= 0.0
			PR_REG_ERNDED::PRE_HRS(I%)	= 0.0
			PR_REG_ERNDED::UNITS(I%)	= 0.0
		NEXT I%
		PR_REG_ERNDED::UPDATE_COUNTER = 0%

8020		!
		! Add information to this record
		!
		PR_REG_ERNDED::QTR_DOLL(QTR% - 1%) = &
			FUNC_ROUND(PR_REG_ERNDED::QTR_DOLL(QTR% - 1%) + &
			ERNDED(LOOP%)::QTR_DOLL, 2%)
		PR_REG_ERNDED::REG_HRS(QTR% - 1%) = &
			FUNC_ROUND(PR_REG_ERNDED::REG_HRS(QTR% - 1%) + &
			ERNDED(LOOP%)::REG_HRS, 2%)
		PR_REG_ERNDED::PRE_HRS(QTR% - 1%) = &
			FUNC_ROUND(PR_REG_ERNDED::PRE_HRS(QTR% - 1%) + &
			ERNDED(LOOP%)::PRE_HRS, 2%)
		PR_REG_ERNDED::UNITS(QTR% - 1%) = &
			FUNC_ROUND(PR_REG_ERNDED::UNITS(QTR% - 1%) + &
			ERNDED(LOOP%)::UNITS, 2%)

8030		!
		! Update ERNDED file
		!
		IF PUT_FLAG% = 0%
		THEN
			IF PR_REG_ERNDED::UPDATE_COUNTER <> PR_CONTROL::UR_COUNT
			THEN
				PR_REG_ERNDED::UPDATE_COUNTER = &
					PR_CONTROL::UR_COUNT
				WHEN ERROR IN
					UPDATE #PR_REG_ERNDED.CH%
				USE
					FILENAME$ = "PR_REG_ERNDED_" + YYYY$
					CONTINUE HelpError
				END WHEN
			ELSE
				!
				! If we don't update this because it is already
				! updated, then we will assume that the accrual
				! has already been done and skip that too.
				!
				GOTO 8190
			END IF
		ELSE
			PR_REG_ERNDED::UPDATE_COUNTER = PR_CONTROL::UR_COUNT
			WHEN ERROR IN
				PUT #PR_REG_ERNDED.CH%
			USE
				FILENAME$ = "PR_REG_ERNDED_" + YYYY$
				CONTINUE HelpError
			END WHEN
		END IF

8035		!
		! Handle PR_EMP_ACCRUAL records special
		!
		IF ERNDED(LOOP%)::ETYPE = "A"
		THEN
			WHEN ERROR IN
				GET #PR_EMP_ACCRUAL.CH%, &
					KEY #0% EQ TEST_EMPNUM$ + ERNDED(LOOP%)::CODE
			USE
				IF ERR = 154%	! Locked Block
				THEN
					SLEEP 5%
					RETRY
				END IF

				CONTINUE 8040
			END WHEN

			SELECT PR_EMP_ACCRUAL::AVAILFLAG

			CASE "2"
				PR_EMP_ACCRUAL::HOURSUNA = &
					FUNC_ROUND(PR_EMP_ACCRUAL::HOURSUNA + &
					ERNDED(LOOP%)::REG_HRS, 2%)
				PR_EMP_ACCRUAL::DOLLARUNA = &
					FUNC_ROUND(PR_EMP_ACCRUAL::DOLLARUNA + &
					ERNDED(LOOP%)::QTR_DOLL, 2%)

			CASE "3"
				IF (PR_EMP_ACCRUAL::AVAILDATE >= BATCH_NO$)
				THEN
					PR_EMP_ACCRUAL::HOURSUNA = &
						FUNC_ROUND(PR_EMP_ACCRUAL::HOURSUNA + &
						ERNDED(LOOP%)::REG_HRS, 2%)
					PR_EMP_ACCRUAL::DOLLARUNA = &
						FUNC_ROUND(PR_EMP_ACCRUAL::DOLLARUNA + &
						ERNDED(LOOP%)::QTR_DOLL, 2%)
				ELSE
					PR_EMP_ACCRUAL::HOURSAVA = &
						FUNC_ROUND(PR_EMP_ACCRUAL::HOURSAVA + &
						ERNDED(LOOP%)::REG_HRS, 2%)
					PR_EMP_ACCRUAL::DOLLARAVA = &
						FUNC_ROUND(PR_EMP_ACCRUAL::DOLLARAVA + &
						ERNDED(LOOP%)::QTR_DOLL, 2%)
				END IF

			CASE ELSE
				PR_EMP_ACCRUAL::HOURSAVA = &
					FUNC_ROUND(PR_EMP_ACCRUAL::HOURSAVA + &
					ERNDED(LOOP%)::REG_HRS, 2%)
				PR_EMP_ACCRUAL::DOLLARAVA = &
					FUNC_ROUND(PR_EMP_ACCRUAL::DOLLARAVA + &
					ERNDED(LOOP%)::QTR_DOLL, 2%)
			END SELECT

			UPDATE #PR_EMP_ACCRUAL.CH%

			GOTO 8190
		END IF

8037		!
		! Handle PR_EMP_ACCRUAL records special
		!
		IF ERNDED(LOOP%)::ETYPE = "P"
		THEN
			WHEN ERROR IN
				GET #PR_EMP_ACCRUAL.CH%, &
					KEY #0% EQ TEST_EMPNUM$ + ERNDED(LOOP%)::CODE
			USE
				IF ERR = 154%	! Locked Block
				THEN
					SLEEP 5%
					RETRY
				END IF

				CONTINUE 8040
			END WHEN

			PR_EMP_ACCRUAL::HOURSAVA = &
				FUNC_ROUND(PR_EMP_ACCRUAL::HOURSAVA - &
				ERNDED(LOOP%)::REG_HRS, 2%)
			PR_EMP_ACCRUAL::DOLLARAVA = &
				FUNC_ROUND(PR_EMP_ACCRUAL::DOLLARAVA - &
				ERNDED(LOOP%)::QTR_DOLL, 2%)

			UPDATE #PR_EMP_ACCRUAL.CH%

			GOTO 8190
		END IF

8040		!
		! Check for standard ernings/deduction file
		!
		WHEN ERROR IN
			GET #PR_EMP_STD_ERNDED.CH%, &
				KEY #0% EQ TEST_EMPNUM$ + ERNDED(LOOP%)::ETYPE + &
				ERNDED(LOOP%)::CODE
		USE
			IF ERR = 154%	! Locked Block
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 8050
		END WHEN

8045		IF PR_EMP_STD_ERNDED::LIMIT <> 0.0
		THEN
			PR_EMP_STD_ERNDED::CTDBAL = &
				FUNC_ROUND(PR_EMP_STD_ERNDED::CTDBAL + &
				ERNDED(LOOP%)::QTR_DOLL, 2%)

			WHEN ERROR IN
				UPDATE #PR_EMP_STD_ERNDED.CH%
			USE
				FILENAME$ = "PR_EMP_STD_ERNDED"
				CONTINUE HelpError
			END WHEN
		ELSE
			UNLOCK #PR_EMP_STD_ERNDED.CH%
		END IF

8050		!
		! Is this an ernded accrual
		!
		GOTO 8190 IF ERNDED(LOOP%)::ETYPE <> "P"

		WHEN ERROR IN
			GET #PR_EMP_STD_ERNDED.CH%, &
				KEY #0% EQ TEST_EMPNUM$ + "A" + &
				ERNDED(LOOP%)::CODE
		USE
			IF ERR = 154%	! Locked Block
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 8190
		END WHEN

		PR_EMP_STD_ERNDED::CTDBAL = &
			FUNC_ROUND(PR_EMP_STD_ERNDED::CTDBAL + &
			ERNDED(LOOP%)::REG_HRS + &
			ERNDED(LOOP%)::PRE_HRS, 2%)

		UPDATE #PR_EMP_STD_ERNDED.CH%

8190	NEXT LOOP%

8200	!*******************************************************************
	! Tax Witholding
	!*******************************************************************

	FOR LOOP% = 1% TO TAX_COUNT%

		SELECT TAX(LOOP%)::TTYPE

		CASE "FI", "FH"
			TOTAL(8%) = FUNC_ROUND(TOTAL(8%) + TAX(LOOP%)::TAX, 2%)

		CASE "FW"
			TOTAL(9%) = FUNC_ROUND(TOTAL(9%) + TAX(LOOP%)::TAX, 2%)

		CASE "SW"
			TOTAL(10%) = FUNC_ROUND(TOTAL(10%) + &
				TAX(LOOP%)::TAX, 2%)

		CASE "SX", "SI"
			TOTAL(11%) = FUNC_ROUND(TOTAL(11%) + &
				TAX(LOOP%)::TAX, 2%)

		CASE "CW"
			TOTAL(12%) = FUNC_ROUND(TOTAL(12%) + &
				TAX(LOOP%)::TAX, 2%)

		CASE "DW"
			TOTAL(13%) = FUNC_ROUND(TOTAL(13%) + &
				TAX(LOOP%)::TAX, 2%)

		CASE "EW"
			TOTAL(14%) = FUNC_ROUND(TOTAL(14%) + &
				TAX(LOOP%)::TAX, 2%)

		CASE "SU"
			TOTAL(15%) = FUNC_ROUND(TOTAL(15%) + &
				TAX(LOOP%)::TAX, 2%)

		END SELECT

8210		!
		! Try to load an existing record
		!
		WHEN ERROR IN
			GET #PR_REG_TAXES.CH%, &
				KEY #0% EQ TEST_EMPNUM$ + &
				TAX(LOOP%)::TTYPE + &
				TAX(LOOP%)::CODE
		USE
			IF ERR = 154%	! Locked Block
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 8215
		END WHEN

		PUT_FLAG% = 0%

		GOTO 8220

8215		!
		! We must create a new record
		!
		PUT_FLAG% = -1%

		PR_REG_TAXES::EMPNUM	= TEST_EMPNUM$
		PR_REG_TAXES::TTYPE	= TAX(LOOP%)::TTYPE
		PR_REG_TAXES::CODE	= TAX(LOOP%)::CODE
		FOR I% = 0% TO 3%
			PR_REG_TAXES::TAX(I%)		= 0.0
			PR_REG_TAXES::TAXABLE(I%)	= 0.0
			PR_REG_TAXES::REPORTABLE(I%)	= 0.0
			PR_REG_TAXES::WKWRK(I%)		= 0%
		NEXT I%

8220		PR_REG_TAXES::TAX(QTR% - 1%)	= &
			FUNC_ROUND(PR_REG_TAXES::TAX(QTR% - 1%) + &
			TAX(LOOP%)::TAX, 2%)
		PR_REG_TAXES::TAXABLE(QTR% - 1%)	= &
			FUNC_ROUND(PR_REG_TAXES::TAXABLE(QTR% - 1%) + &
			TAX(LOOP%)::TAXABLE, 2%)
		PR_REG_TAXES::REPORTABLE(QTR% - 1%)= &
			FUNC_ROUND(PR_REG_TAXES::REPORTABLE(QTR% - 1%) + &
			TAX(LOOP%)::REPORTABLE, 2%)
		PR_REG_TAXES::WKWRK(QTR% - 1%)	= &
			PR_REG_TAXES::WKWRK(QTR% - 1%) + WKS%

8230		IF PUT_FLAG% = 0%
		THEN
			IF (PR_REG_TAXES::UPDATE_COUNTER <> &
				PR_CONTROL::UR_COUNT)
			THEN
				PR_REG_TAXES::UPDATE_COUNTER = &
					PR_CONTROL::UR_COUNT
				WHEN ERROR IN
					UPDATE # PR_REG_TAXES.CH%
				USE
					FILENAME$ = "PR_REG_TAXES_" + YYYY$
					CONTINUE HelpError
				END WHEN
			END IF
		ELSE
			PR_REG_TAXES::UPDATE_COUNTER = PR_CONTROL::UR_COUNT
			WHEN ERROR IN
				PUT # PR_REG_TAXES.CH%
			USE
				FILENAME$ = "PR_REG_TAXES_" + YYYY$
				CONTINUE HelpError
			END WHEN
		END IF

8390	NEXT LOOP%

8990	TAX_COUNT% = 0%
	ERNDED_COUNT% = 0%
	TEST_EMPNUM$ = ""

	RETURN

	%PAGE

	!*******************************************************************
	! Exit and display totals
	!*******************************************************************

 ExitTotal:
	GOSUB ReadDedFile IF TEST_EMPNUM$ <> ""
	GOSUB WriteTotals IF TEST_EMPNUM$ <> ""

	GET #PR_CONTROL.CH%, RECORD 1%

	PR_CONTROL::CLOSEFLAG = "0"

	UPDATE #PR_CONTROL.CH%

	!
	! Disable unsolicited input
	!
	SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

	SCOPE::PRG_ITEM = "HELP"

	DISPLAY$(1%)  = "Current Totals:"
	DISPLAY$(2%)  = "   Gross Pay       " + &
		FORMAT$(TOTAL(1%), "#,###,###.##")
	DISPLAY$(3%)  = "   Deductions      " + &
		FORMAT$(TOTAL(2%), "#,###,###.##")
	DISPLAY$(4%)  = "   Noncompensation " + &
		FORMAT$(TOTAL(3%), "#,###,###.##")
	DISPLAY$(5%)  = "   Memo Entry      " + &
		FORMAT$(TOTAL(4%), "#,###,###.##")
	DISPLAY$(6%)  = ""
	DISPLAY$(7%)  = "   Regular Hours   " + &
		FORMAT$(TOTAL(5%), "#,###,###.##")
	DISPLAY$(8%)  = "   Overtime Hours  " + &
		FORMAT$(TOTAL(6%), "#,###,###.##")
	DISPLAY$(9%)  = "   Units           " + &
		FORMAT$(TOTAL(7%), "#,###,###.##")
	DISPLAY$(10%) = ""
	DISPLAY$(11%) = "   FICA            " + &
		FORMAT$(TOTAL(8%), "#,###,###.##")
	DISPLAY$(12%) = "   Federal         " + &
		FORMAT$(TOTAL(9%), "#,###,###.##")
	DISPLAY$(13%) = "   State W/H       " + &
		FORMAT$(TOTAL(10%), "#,###,###.##")
	DISPLAY$(14%) = "   Other State Tax " + &
		FORMAT$(TOTAL(11%) + TOTAL(16%), "#,###,###.##")
	DISPLAY$(15%) = "   City W/H        " + &
		FORMAT$(TOTAL(12%), "#,###,###.##")
	DISPLAY$(16%) = "   Country W/H     " + &
		FORMAT$(TOTAL(13%), "#,###,###.##")
	DISPLAY$(17%) = "   School District " + &
		FORMAT$(TOTAL(14%), "#,###,###.##")
	DISPLAY$(18%) = "   SUI             " + &
		FORMAT$(TOTAL(15%), "#,###,###.##")

	FOR I% = 1% TO 18%
		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			LEFT(DISPLAY$(I%) + SPACE$(77%), 77%), I%, 1%)

		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), &
			DISPLAY$(I%), 0%)
	NEXT I%

	NAME PR_TRN_PAY.DEV$ + "PR_TRN_PAY_" + BATCH_NO$ + ".JRL" AS &
		PR_TRN_PAY.DEV$ + "PR_HIS_PAY_" + BATCH_NO$ + ".ARC"

	NAME PR_TRN_DED.DEV$ + "PR_TRN_DED_" + BATCH_NO$ + ".JRL" AS &
		PR_TRN_DED.DEV$ + "PR_HIS_DED_" + BATCH_NO$ + ".ARC"

	NAME PR_TRN_CHECK.DEV$ + "PR_TRN_CHECK_" + BATCH_NO$ + ".JRL" AS &
		PR_TRN_CHECK.DEV$ + "PR_HIS_CHECK_" + BATCH_NO$ + ".ARC"

	CALL ENTR_3MESSAGE(SCOPE, "Update completed", 0%)

 ExitProgram:
	CALL OUTP_FINISH(UTL_REPORTX)

	!
	! Exit to next program or menu
	!
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE


 Interupt:
	!********************************************************************
	! Handle any special junk in RRR_FLAG%
	!********************************************************************
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

	RETURN

	%Page

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

32767	END
	!+-+-+
	!++
	! Abstract:FLD01FDATE
	!	.x Payroll>Folder>Date
	!	^*Payroll Folder Date\*
	!	.b
	!	.lm +5
	!	The ^*Payroll Folder Date\* field selects an existing folder
	!	to update.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!	.x Date>Payroll Folder
	!
	!--
