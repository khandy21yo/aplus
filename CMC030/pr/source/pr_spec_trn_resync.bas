1	%TITLE "RESYNC - Resync Taxable & Reportable"
	%SBTTL "PR_SPEC_TRN_RESYNC"
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
	! ID:PR073
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Resync Folder\* option is used to re-calculate
	!	the ^~taxable\~ and ^~Reportable\~ fields in the deduction
	!	part of the payroll folder.
	!	.lm -5
	!
	! Index:
	!	.x Resync>Deduction
	!	.x Deduction>Resync
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_SPEC_TRN_RESYNC/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_SPEC_TRN_RESYNC, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_SPEC_TRN_RESYNC.OBJ;*
	!
	! Author:
	!
	!	05/21/90 - Kevin Handy
	!
	! Modification history:
	!
	!	06/27/90 - Kevin Handy
	!		Modified to fix bug that caused only one
	!		state to be updated instead of all
	!
	!	12/13/90 - Kevin Handy
	!		Added code for PR_EMP_STATUS::ADDEXEMPT and
	!		PR_TRN_DED::ADDEXEMPT.
	!
	!	01/15/91 - Craig Tanner
	!		Added YYYY$ to some filename$ in error trapping.
	!
	!	06/04/91 - Kevin Handy
	!		Removed junk in error trapping.
	!
	!	02/01/93 - Kevin Handy
	!		Modified to ignore accrual "A" records.
	!
	!	03/30/93 - Kevin Handy
	!		Fixed error trapping.  Trap EOF at 17190 instead of
	!		17110, which had no GET.
	!
	!	03/30/93 - Kevin Handy
	!		Sorted error traps numerically to make it easier
	!		to find things there.
	!
	!	03/30/93 - Kevin Handy
	!		Fixed bug with EOF_FLAG% where wasn't getting
	!		set when it should.
	!
	!	03/30/93 - Kevin Handy
	!		Removed 17170 error trap since there was nothing
	!		there to be trapped.
	!
	!	04/01/93 - Kevin Handy
	!		Modified so that CW,DW,... will not be dumped
	!		into deduction if not in tax package.  Added
	!		PKG_WH_CODE%() variable to handle it.
	!
	!	04/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	03/02/94 - Kevin Handy
	!		Modifications for completely ignoring the
	!		CW, DW, ... when the employee is not geven those
	!		codes, plus modifications to delete the records
	!		for these if they exist and should be zero.
	!
	!	12/16/94 - Kevin Handy
	!		Modified to handle a range of payroll dates.
	!
	!	12/16/94 - Kevin Handy
	!		Removed report output. Replaced with printing
	!		list of folders processed.
	!
	!	12/16/94 - Kevin Handy
	!		Fixed eternal loop when folder is empty.
	!
	!	04/11/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!		Removed unsolicited_input stuff.
	!
	!	12/21/95 - Kevin Handy
	!		Lose extra parameter in calls to PR_READ_SUBJTAX.
	!
	!	12/21/95 - Kevin Handy
	!		Fix so that records created in deduction file
	!		is flagged as posted and updated.
	!
	!	10/25/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/12/97 - Kevin Handy
	!		Lose lookup into PR_TAX_TABLE, since the info
	!		was never used.
	!
	!	06/02/97 - Kevin Handy
	!		Use integer for #key
	!
	!	06/27/97 - Kevin Handy
	!		Clean up titles
	!		Lose various print/total calculations which are
	!		never output.
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	06/18/98 - Kevin Handy
	!		Make sure YYYY$ is set up properly for
	!		register files
	!		Force taxes #1-3 instead of 1-2 so that
	!		FW is forced to calculate.
	!
	!	06/19/98 - Kevin Handy
	!		Additional repairs to FW calculation
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	07/08/99 - Kevin Handy
	!		Set it up for 80 columns inatead of 132
	!
	!	10/13/2000 - Kevin Handy
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
	DECLARE				UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP	(PR_EMP_MASTER)		PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.HB"
	MAP	(PR_TRN_PAY)		PR_TRN_PAY_CDD		PR_TRN_PAY
	MAP	(PR_HIS_PAY)		PR_TRN_PAY_CDD		PR_HIS_PAY

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.HB"
	MAP	(PR_TRN_DED)		PR_TRN_DED_CDD		PR_TRN_DED
	MAP	(PR_HIS_DED)		PR_TRN_DED_CDD		PR_HIS_DED

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.HB"
	MAP	(PR_REG_TAXES)		PR_REG_TAXES_CDD	PR_REG_TAXES

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.HB"
	MAP	(PR_REG_ERNDED)		PR_REG_ERNDED_CDD	PR_REG_ERNDED

	%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.HB"
	MAP	(PR_ERNDED_DEF)		PR_ERNDED_DEF_CDD	PR_ERNDED_DEF

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_TABLE.HB"
	MAP	(PR_TAX_TABLE)		PR_TAX_TABLE_CDD	PR_TAX_TABLE

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_S.HB"
	MAP	(PR_TAX_PROFILE_F)	PR_TAX_PROFILE_S_CDD	PR_TAX_PROFILE_S

	!
	! Need to include _F version so that variable length record
	! business will work.
	!
	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_F.HB"
	MAP	(PR_TAX_PROFILE_F)	PR_TAX_PROFILE_F_CDD	PR_TAX_PROFILE_F

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PKG.HB"
	MAP	(PR_TAX_PKG)		PR_TAX_PKG_CDD		PR_TAX_PKG

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_STATUS.HB"
	MAP	(PR_EMP_STATUS)		PR_EMP_STATUS_CDD	PR_EMP_STATUS

	!
	! Structures local to program
	!
	RECORD TOTAL_RECORD
		STRING CODE = 2%
		GFLOAT TAXABLE
		GFLOAT REPORTABLE
		GFLOAT TAX
		STRING STAT = 1%
		LONG EXEMPT
		LONG ADDEXEMPT
		LONG EMPCOUNT
	END RECORD

	DIM TOTAL_RECORD EMPLOYEE(10%, 10%)

	!
	! Dimension
	!
	DIM EMP_NT(10%), &
		EMP_NTR(10%), &
		EMPLOYEE_CODES%(10%), &
		TOTAL_WH_CODE%(10%)

	DIM DATA_FILE$(600%)

	%PAGE

	ON ERROR GOTO 19000

	!
	! Other Variables
	!
	SUBJECT_TYPE_TABLE$ = "FIE!FHE!FWH!SWH!OST!SUI!CWH!DWH!EWH!SWC"
	TAX_TYPE_TABLE$ = "FI!FH!FW!SW!SX!SU!CW!DW!EW!SI!"

 Init:	!
	! Initilize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 80%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)
	FROM_BATCH_NO$ = DATE_STOREDATE(FROM_BATCH_NO$)
	YYYY$ = LEFT(FROM_BATCH_NO$, 4%)

	!++
	! Abstract:FLD01
	!	^*(01) Start Payroll Date\*
	!	.p
	!	The ^*Start Payroll Date\* field is to contain the date of
	!	the payroll folder with which the report will begin printing.
	!	A blank field will cause the report to start
	!	with the first payroll folder date in the file.
	!
	! Index:
	!	.x Start Payroll Date>Employee Check Audit Report
	!	.x Employee Check Audit Report>Start Payroll Date
	!
	! Datatype:DATE
	! Size:8
	! Required:Y
	!--

	TO_BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	TO_BATCH_NO$ = DATE_STOREDATE(TO_BATCH_NO$)

	!++
	! Abstract:FLD02
	!	^*(02) End Payroll Date\*
	!	.p
	!	The ^*End Payroll Date\* field is to contain the date of
	!	the payroll folder with which the report is to end printing.
	!	A blank field will cause the report to end
	!	with the last payroll folder date in the file.
	!
	! Index:
	!	.x End Payroll Date>Employee Check Audit Report
	!	.x Employee Check Audit Report>End Payroll Date
	!
	! Datatype:DATE
	! Size:8
	! Required:Y
	!--

	!
	! Look up all folders
	!
	CALL READ_DEVICE("PR_TRN_PAY", PR_TRN_PAY.DEV$, STAT%)
	CALL READ_DEVICE("PR_HIS_PAY", PR_HIS_PAY.DEV$, STAT%)

	CALL PR_FIND_DETAILFILE(FROM_BATCH_NO$, &
		TO_BATCH_NO$, &
		PR_TRN_PAY.DEV$, &
		PR_HIS_PAY.DEV$, &
		DATA_FILE$())

	DATA_FILE% = VAL%(DATA_FILE$(0%))

300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

330	!
	! Open ERNDED Definition file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.OPN"
	USE
		FILENAME$ = "PR_ERNDED_DEF"
		CONTINUE HelpError
	END WHEN

340	!
	! Open TaxWH register
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.OPN"
	USE
		CONTINUE 350 IF ERR = 5%
		FILENAME$ = "PR_REG_TAXES_" + YYYY$
		CONTINUE HelpError
	END WHEN

350	!
	! Open ERNDED register
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.OPN"
	USE
		CONTINUE 370 IF ERR = 5%
		FILENAME$ = "PR_REG_ERNDED_" + YYYY$
		CONTINUE HelpError
	END WHEN

370	!
	! Open Profile
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_F.OPN"
	USE
		CONTINUE 380 IF ERR = 5%
		FILENAME$ = "PR_TAX_PROFILE"
		CONTINUE HelpError
	END WHEN

380	!
	! Open Tax package
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PKG.OPN"
	USE
		FILENAME$ = "PR_TAX_PKG"
		CONTINUE HelpError
	END WHEN

390	!
	! Open employee status file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_STATUS.OPN"
	USE
		FILENAME$ = "PR_EMP_STATUS"
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
	!
	! Set up titles and whatnot
	!
	TITLE$(1%) = "Resync Payroll Taxes"
	TITLE$(2%) = "For the Payroll Folder Dated: " + &
		MID(BATCH_NO$, 5%, 2%) + "/" + &
		MID(BATCH_NO$, 7%, 2%) + "/" + &
		LEFT(BATCH_NO$, 4%)
	TITLE$(3%) = "By Employee Number"

	TITLE$(4%) = ""
	TITLE$(5%) = ""

	%PAGE

16000	FOR PR_LOOP% = 1% TO DATA_FILE%

		BATCH_NO$ = DATA_FILE$(PR_LOOP%)
		CALL ENTR_3MESSAGE(SCOPE, "Processing: " + BATCH_NO$, 1%)

		YYYY$ = LEFT(BATCH_NO$, 4%)

		!
		! Open Pay folder
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.OPN"
		USE
			CONTINUE 16010 IF ERR = 5%
			FILENAME$ = "PR_TRN_PAY"
			CONTINUE HelpError
		END WHEN

		USE_HISTORY% = 0%

		GOTO 16020

16010		!
		! Open pay history folder if journal not there
		!
		%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_PAY.OPN"

		PR_TRN_PAY.CH% = PR_HIS_PAY.CH%
		USE_HISTORY% = -1%

16020		!
		! Open Deduction folder
		!
		IF USE_HISTORY% = 0%
		THEN
			WHEN ERROR IN
				%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.MOD"
			USE
				CONTINUE 16090 IF ERR = 5%
				FILENAME$ = "PR_TRN_DED"
				CONTINUE HelpError
			END WHEN
		ELSE
			WHEN ERROR IN
				%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_DED.MOD"
			USE
				CONTINUE 16090 IF ERR = 5%
				FILENAME$ = "PR_HIS_DED"
				CONTINUE HelpError
			END WHEN

			PR_TRN_DED.CH% = PR_HIS_DED.CH%
		END IF

16030		!

16040		TEXT$ = "Starting folder " + BATCH_NO$ + " " + &
			DATE$(0%) + " " + TIME$(0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		GOSUB 17000

		CLOSE PR_TRN_PAY.CH%
		CLOSE PR_TRN_DED.CH%

16090	NEXT PR_LOOP%

	TEXT$ = "Finished " + DATE$(0%) + " " + TIME$(0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO ExitProgram

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		RESET #PR_EMP_MASTER.CH%, KEY #0%
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

	EOF_FLAG% = 0%

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #PR_EMP_MASTER.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

	PR_END_DATE$ = "        "

17100	GOTO ExitTotal IF EOF_FLAG%

	EMP_NT(I%), EMP_NTR(I%) = 0.0 FOR I% = 1% TO 10%
	PAY, NON_COMP = 0.0

	EMPLOYEE_CODES%(I%) = 0% FOR I% = 1% TO 10%

	!
	! Set up for federal taxes
	!
	EMPLOYEE_CODES%(1%) = 1%
	EMPLOYEE(1%, 1%)::TAXABLE = 0.0
	EMPLOYEE(1%, 1%)::REPORTABLE = 0.0
	EMPLOYEE(1%, 1%)::TAX = 0.0
	EMPLOYEE(1%, 1%)::CODE = ""
	EMPLOYEE(1%, 1%)::STAT = ""
	EMPLOYEE(1%, 1%)::EXEMPT = 0%
	EMPLOYEE(1%, 1%)::ADDEXEMPT = 0%

	EMPLOYEE_CODES%(2%) = 1%
	EMPLOYEE(2%, 1%)::TAXABLE = 0.0
	EMPLOYEE(2%, 1%)::REPORTABLE = 0.0
	EMPLOYEE(2%, 1%)::TAX = 0.0
	EMPLOYEE(2%, 1%)::CODE = ""
	EMPLOYEE(2%, 1%)::STAT = ""
	EMPLOYEE(2%, 1%)::EXEMPT = 0%
	EMPLOYEE(2%, 1%)::ADDEXEMPT = 0%

	EMPLOYEE_CODES%(3%) = 1%
	EMPLOYEE(3%, 1%)::TAXABLE = 0.0
	EMPLOYEE(3%, 1%)::REPORTABLE = 0.0
	EMPLOYEE(3%, 1%)::TAX = 0.0
	EMPLOYEE(3%, 1%)::CODE = ""
	EMPLOYEE(3%, 1%)::STAT = ""
	EMPLOYEE(3%, 1%)::EXEMPT = 0%
	EMPLOYEE(3%, 1%)::ADDEXEMPT = 0%

	!
	! Get pay detail information
	!
	WHEN ERROR IN
		GET #PR_TRN_PAY.CH%, &
			KEY #0% GT PR_EMP_MASTER::EMPNUM + PR_END_DATE$, &
			REGARDLESS
	USE
		CONTINUE 17020 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

	!
	! If history then set history into journal map
	!
	IF USE_HISTORY%
	THEN
		PR_TRN_PAY = PR_HIS_PAY
	END IF

	PR_END_DATE$ = PR_TRN_PAY::PR_END_DATE

	IF (PR_TRN_PAY::EMPNUM <> PR_EMP_MASTER::EMPNUM)
	THEN
		GOTO 17020
	END IF

	CALL ENTR_3MESSAGE(SCOPE, "Processing: " + BATCH_NO$ + " " + &
		PR_EMP_MASTER::EMPNUM, 1%)

17110	IF (PR_TRN_PAY::EMPNUM <> PR_EMP_MASTER::EMPNUM) OR &
		(PR_TRN_PAY::PR_END_DATE <> PR_END_DATE$)
	THEN
		GOTO 17200
	END IF

	!
	! Skip if accrual record
	!
	GOTO 17190 IF PR_TRN_PAY::PTYPE = "A"

17120	PKG_WH_CODE$(I%) = "" FOR I% = 4% TO 7%
	PKG_WH_CODE%(I%) = -1% FOR I% = 0% TO 3%	! Force FW,FI
	PKG_WH_CODE%(I%) = 0% FOR I% = 4% TO 7%

	WHEN ERROR IN
		FIND #PR_TAX_PKG.CH%, KEY #0% EQ PR_TRN_PAY::TAX_PKG, REGARDLESS
	USE
		CONTINUE 17140 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_TAX_PKG"
		CONTINUE HelpError
	END WHEN

17130	WHEN ERROR IN
		GET #PR_TAX_PKG.CH%, REGARDLESS
	USE
		CONTINUE 17140 IF ERR = 11%
		FILENAME$ = "PR_TAX_PKG"
		CONTINUE HelpError
	END WHEN

	GOTO 17140 IF PR_TRN_PAY::TAX_PKG <> PR_TAX_PKG::TAX_PKG

	TAX_TYPE% = (INSTR(1%, TAX_TYPE_TABLE$, PR_TAX_PKG::STTYPE) + 2%) /3%

	PKG_WH_CODE$(TAX_TYPE%) = PR_TAX_PKG::CODE IF TAX_TYPE% > 3%
	PKG_WH_CODE%(TAX_TYPE%) = -1% IF TAX_TYPE% > 3%

	GOTO 17130

17140	FOR TAX_TYPE% = 1% TO LEN(SUBJECT_TYPE_TABLE$) / 4%
		!
		! Skip if user does not have this tax available
		!
		GOTO 17170 IF PKG_WH_CODE%(TAX_TYPE%) = 0%

		!
		! See if taxable
		!
		SUBJECT_CODE$ = MID(SUBJECT_TYPE_TABLE$, &
			(TAX_TYPE% - 1%) * 4% + 1%, 3%)

		CALL PR_READ_SUBJTAX(PR_ERNDED_DEF.CH%, &
			SUBJECT_CODE$, &
			"P", &
			PR_TRN_PAY::CODE, &
			TAXABLE%, &
			REPORTABLE%)

		WH_LOOP% = 1%

		IF TAX_TYPE% > 3%
		THEN
			GOTO 17150 IF PKG_WH_CODE$(TAX_TYPE%) = &
				EMPLOYEE(TAX_TYPE%, WH_LOOP%)::CODE &
				FOR WH_LOOP% = 1% TO EMPLOYEE_CODES%(TAX_TYPE%)

			EMPLOYEE_CODES%(TAX_TYPE%), WH_LOOP% = &
				EMPLOYEE_CODES%(TAX_TYPE%) + 1%

			EMPLOYEE(TAX_TYPE%, WH_LOOP%)::CODE = &
				PKG_WH_CODE$(TAX_TYPE%)
			EMPLOYEE(TAX_TYPE%, WH_LOOP%)::TAXABLE = 0.0
			EMPLOYEE(TAX_TYPE%, WH_LOOP%)::REPORTABLE = 0.0
			EMPLOYEE(TAX_TYPE%, WH_LOOP%)::TAX = 0.0
			EMPLOYEE(TAX_TYPE%, WH_LOOP%)::STAT = ""
			EMPLOYEE(TAX_TYPE%, WH_LOOP%)::EXEMPT = 0%
			EMPLOYEE(TAX_TYPE%, WH_LOOP%)::ADDEXEMPT = 0%

		END IF

17150		IF TAX_TYPE% < 4% OR PKG_WH_CODE$(TAX_TYPE%) <> ""
		THEN
			EMPLOYEE(TAX_TYPE%, WH_LOOP%)::TAXABLE = &
				FUNC_ROUND(EMPLOYEE(TAX_TYPE%, WH_LOOP%)::TAXABLE + &
				PR_TRN_PAY::GROSS, 2%) &
				IF TAXABLE% = 0%

			EMPLOYEE(TAX_TYPE%, WH_LOOP%)::REPORTABLE = &
				FUNC_ROUND(EMPLOYEE(TAX_TYPE%, WH_LOOP%)::REPORTABLE + &
				PR_TRN_PAY::GROSS, 2%) &
				IF REPORTABLE% = 0%

		END IF

17170	NEXT TAX_TYPE%

	PAY = FUNC_ROUND(PAY + PR_TRN_PAY::GROSS, 2%)

17190	WHEN ERROR IN
		GET #PR_TRN_PAY.CH%, REGARDLESS
	USE
		IF ERR = 11%
		THEN
			EOF_FLAG% = -1%
			CONTINUE 17200
		END IF
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

	!
	! If history then set history into journal map
	!
	IF USE_HISTORY%
	THEN
		PR_TRN_PAY = PR_HIS_PAY
	END IF

	GOTO 17110

17200	!
	! Get Tax/Ded detail information
	!
	WHEN ERROR IN
		FIND #PR_TRN_DED.CH%, &
			KEY #0% GE PR_EMP_MASTER::EMPNUM + PR_END_DATE$, &
			REGARDLESS
	USE
		CONTINUE 17300 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_TRN_DED"
		CONTINUE HelpError
	END WHEN

17210	WHEN ERROR IN
		GET #PR_TRN_DED.CH%, REGARDLESS
	USE
		CONTINUE 17300 IF ERR = 11%
		FILENAME$ = "PR_TRN_DED"
		CONTINUE HelpError
	END WHEN

	!
	! If history then set history into journal map
	!
	IF USE_HISTORY%
	THEN
		PR_TRN_DED = PR_HIS_DED
	END IF

	IF (PR_EMP_MASTER::EMPNUM <> PR_TRN_DED::EMPNUM) OR &
		(PR_TRN_DED::PR_END_DATE <> PR_END_DATE$)
	THEN
		GOTO 17300
	END IF

	!
	! See if this is a tax
	!
	TAX_TYPE% = (INSTR(1%, TAX_TYPE_TABLE$, PR_TRN_DED::CODE) + 2%) /3%

	!
	! Jump to deduction if not tax
	!
	GOTO 17250 IF TAX_TYPE% = 0%

	WH_LOOP% = 1%

	IF TAX_TYPE% > 3%
	THEN
		!
		! State, local tax
		!
		GOTO 17240 IF PR_TRN_DED::TAX_CODE = &
			EMPLOYEE(TAX_TYPE%, WH_LOOP%)::CODE &
			FOR WH_LOOP% = 1% TO EMPLOYEE_CODES%(TAX_TYPE%)

		EMPLOYEE_CODES%(TAX_TYPE%), WH_LOOP% = &
			EMPLOYEE_CODES%(TAX_TYPE%) + 1%

		EMPLOYEE(TAX_TYPE%, WH_LOOP%)::CODE = PR_TRN_DED::TAX_CODE
		EMPLOYEE(TAX_TYPE%, WH_LOOP%)::TAXABLE = 0.0
		EMPLOYEE(TAX_TYPE%, WH_LOOP%)::REPORTABLE = 0.0
		EMPLOYEE(TAX_TYPE%, WH_LOOP%)::TAX = 0.0
		EMPLOYEE(TAX_TYPE%, WH_LOOP%)::STAT = ""
		EMPLOYEE(TAX_TYPE%, WH_LOOP%)::EXEMPT = 0%
		EMPLOYEE(TAX_TYPE%, WH_LOOP%)::ADDEXEMPT = 0%
	END IF

17240	EMPLOYEE(TAX_TYPE%, WH_LOOP%)::TAX = &
		FUNC_ROUND(EMPLOYEE(TAX_TYPE%, WH_LOOP%)::TAX + &
		PR_TRN_DED::AMOUNT, 2%)

	EMPLOYEE(TAX_TYPE%,WH_LOOP%)::STAT = PR_TRN_DED::SSTATUS
	EMPLOYEE(TAX_TYPE%,WH_LOOP%)::EXEMPT = PR_TRN_DED::EXEMPT
	EMPLOYEE(TAX_TYPE%,WH_LOOP%)::ADDEXEMPT = PR_TRN_DED::ADDEXEMPT

	GOTO 17290

17250	!
	! Check Deductions
	!
	GOTO 17270 IF PR_TRN_DED::DTYPE = "T" OR PR_TRN_DED::DTYPE = "M"

	FOR TAX_TYPE% = 1% TO LEN(SUBJECT_TYPE_TABLE$) / 4%

		!
		! Skip if user does not have this tax available
		!
		GOTO 17260 IF PKG_WH_CODE%(TAX_TYPE%) = 0%

		!
		! See if taxable and reportable
		!
		SUBJECT_CODE$ = MID(SUBJECT_TYPE_TABLE$, &
			(TAX_TYPE% - 1%) * 4% + 1%, 3%)

		CALL PR_READ_SUBJTAX(PR_ERNDED_DEF.CH%, &
			SUBJECT_CODE$, &
			PR_TRN_DED::DTYPE, &
			PR_TRN_DED::CODE, &
			TAXABLE%, &
			REPORTABLE%)

		IF (TAXABLE% = 0% AND PR_TRN_DED::DTYPE <> "D" AND &
			PR_TRN_DED::DTYPE <> "F") OR &
			(TAXABLE% <> 0% AND (PR_TRN_DED::DTYPE = "D" OR &
			PR_TRN_DED::DTYPE = "F"))
		THEN
			EMP_NT(TAX_TYPE%) = FUNC_ROUND(EMP_NT(TAX_TYPE%) + &
				PR_TRN_DED::AMOUNT, 2%)
		END IF

		IF (REPORTABLE% = 0% AND PR_TRN_DED::DTYPE <> "D" AND &
			PR_TRN_DED::DTYPE <> "F") OR &
			(REPORTABLE% <> 0% AND (PR_TRN_DED::DTYPE = "D" OR &
			PR_TRN_DED::DTYPE = "F"))
		THEN
			EMP_NTR(TAX_TYPE%) = FUNC_ROUND(EMP_NTR(TAX_TYPE%) + &
				PR_TRN_DED::AMOUNT, 2%)
		END IF

17260	NEXT TAX_TYPE%

	GOTO 17290

17270	!
	! Add noncompensation items
	!


17290	!
	! Loop back for next deduction record
	!
	GOTO 17210

17300	!****************************************************************
	! Look up fica tax and sdi
	!****************************************************************

17400	!
	! Allocate taxable wages
	!
	FOR TAX_TYPE% = 1% TO 3%

		EMPLOYEE(TAX_TYPE%, 1%)::TAXABLE = &
			EMPLOYEE(TAX_TYPE%, 1%)::TAXABLE - EMP_NT(TAX_TYPE%)

		EMPLOYEE(TAX_TYPE%, 1%)::REPORTABLE = &
			EMPLOYEE(TAX_TYPE%, 1%)::REPORTABLE - EMP_NTR(TAX_TYPE%)

	NEXT TAX_TYPE%

	FOR TAX_TYPE% = 4% TO LEN(SUBJECT_TYPE_TABLE$) / 4%

		!
		! Allocate Taxable
		!
		TEMP = 0.0
		TEMP = TEMP + EMPLOYEE(TAX_TYPE%, LOOP%)::TAXABLE &
			FOR LOOP% = 1% TO EMPLOYEE_CODES%(TAX_TYPE%)

		FACTOR = 1.0
		FACTOR = 1.0 - (EMP_NT(TAX_TYPE%) / TEMP) &
			IF TEMP <> 0.0

		TOTAL_TO_DIST = FUNC_ROUND(TEMP - EMP_NT(TAX_TYPE%), 2%)

		FOR LOOP% = 1% TO EMPLOYEE_CODES%(TAX_TYPE%) - 1%
			EMPLOYEE(TAX_TYPE%, LOOP%)::TAXABLE = &
				FUNC_ROUND(EMPLOYEE(TAX_TYPE%, LOOP%)::TAXABLE * &
				FACTOR, 2%)
			TOTAL_TO_DIST = FUNC_ROUND(TOTAL_TO_DIST - &
				EMPLOYEE(TAX_TYPE%, LOOP%)::TAXABLE, 2%)
		NEXT LOOP%

		EMPLOYEE(TAX_TYPE%, EMPLOYEE_CODES%(TAX_TYPE%))::TAXABLE = &
			TOTAL_TO_DIST

		!
		! Allocate Reportable
		!
		TEMP = 0.0
		TEMP = TEMP + EMPLOYEE(TAX_TYPE%, LOOP%)::REPORTABLE &
			FOR LOOP% = 1% TO EMPLOYEE_CODES%(TAX_TYPE%)

		FACTOR = 1.0
		FACTOR = 1.0 - (EMP_NTR(TAX_TYPE%) / TEMP) &
			IF TEMP <> 0.0

		TOTAL_TO_DIST = FUNC_ROUND(TEMP - EMP_NTR(TAX_TYPE%), 2%)

		FOR LOOP% = 1% TO EMPLOYEE_CODES%(TAX_TYPE%) - 1%
			EMPLOYEE(TAX_TYPE%, LOOP%)::REPORTABLE = &
				FUNC_ROUND(EMPLOYEE(TAX_TYPE%, LOOP%)::REPORTABLE * &
				FACTOR, 2%)
			TOTAL_TO_DIST = FUNC_ROUND(TOTAL_TO_DIST - &
				EMPLOYEE(TAX_TYPE%, LOOP%)::REPORTABLE, 2%)
		NEXT LOOP%

		EMPLOYEE(TAX_TYPE%, EMPLOYEE_CODES%(TAX_TYPE%))::REPORTABLE = &
			TOTAL_TO_DIST

	NEXT TAX_TYPE%

	WORK_LOOP% = 1%

	WORK_LOOP% = EMPLOYEE_CODES%(LOOP%) &
		IF WORK_LOOP% < EMPLOYEE_CODES%(LOOP%) &
		FOR LOOP% = 4% TO LEN(SUBJECT_TYPE_TABLE$) / 4%

	FOR LOOP% = 1% TO WORK_LOOP%

		FOR TAX_TYPE% = 3% TO LEN(SUBJECT_TYPE_TABLE$) / 4%
			!
			! Taxes
			!
			IF EMPLOYEE(TAX_TYPE%, LOOP%)::TAXABLE <> 0.0 OR &
				EMPLOYEE(TAX_TYPE%, LOOP%)::TAX <> 0.0
			THEN
				GOSUB GetStatus &
					IF (EMPLOYEE(TAX_TYPE%, LOOP%)::STAT = "") AND &
					(EMPLOYEE(TAX_TYPE%, LOOP%)::EXEMPT = 0%)

			END IF

		NEXT TAX_TYPE%

17420	NEXT LOOP%

	!*******************************************************************
	! Force resyncronization
	!*******************************************************************

	GOSUB ResyncDed

	!
	! Total codes
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

17450	!
	! Next employee
	!
	GOTO 17100

	%Page

 ExitTotal:
	RETURN

 ExitProgram:
	CALL OUTP_FINISH(UTL_REPORTX)

	!
	! Exit to next program or menu
	!
	IF TRM$(UTL_REPORTX::NEXTRUN) = ""
	THEN
		CALL SUBR_3EXITPROGRAM(SCOPE, "", "")
	ELSE
		CALL SUBR_3EXITPROGRAM(SCOPE, "RUN " + UTL_REPORTX::NEXTRUN, "")
	END IF

	%PAGE

 GetStatus:
18600	!*******************************************************************
	! Search for status if item doesn't have one
	!*******************************************************************

	WHEN ERROR IN
		GET #PR_EMP_STATUS.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM + &
			MID(TAX_TYPE_TABLE$, TAX_TYPE% * 3% - 2%, 2%) + &
			EMPLOYEE(TAX_TYPE%, LOOP%)::CODE, &
			REGARDLESS
	USE
		CONTINUE 18690
	END WHEN

	EMPLOYEE(TAX_TYPE%, LOOP%)::STAT = PR_EMP_STATUS::STSTATUS
	EMPLOYEE(TAX_TYPE%, LOOP%)::EXEMPT = PR_EMP_STATUS::EXEMPT
	EMPLOYEE(TAX_TYPE%, LOOP%)::ADDEXEMPT = PR_EMP_STATUS::ADDEXEMPT

18690	RETURN

	%PAGE

18700	!*******************************************************************
	! Resyncronize routine for deduction folder.
	!
	! Forces TAXABLE and REPORTABLE amounts to be what they should
	! be calculated as.
	!*******************************************************************
 ResyncDed:

	!
	! For all tax types
	!
	FOR TAX_TYPE% = 1% TO LEN(SUBJECT_TYPE_TABLE$) / 4%

		TAX_TYPE$ = MID(TAX_TYPE_TABLE$, TAX_TYPE% * 3% - 2%, 2%)

		!
		! For all items under that tax type
		!
		FOR LOOP% = 1% TO EMPLOYEE_CODES%(TAX_TYPE%)

			!
			! Pull up an existing deduction (if one exists)
			!
18710			WHEN ERROR IN
				GET #PR_TRN_DED.CH%, &
					KEY #0% EQ PR_EMP_MASTER::EMPNUM + &
					PR_END_DATE$ + &
					"C" + &
					TAX_TYPE$
			USE
				IF ERR = 154%	! Locked Block
				THEN
					SLEEP 5%
					RETRY
				END IF

				CONTINUE 18720 IF ERR = 155%
				FILENAME$ = "PR_TRN_DED"
				CONTINUE HelpError
			END WHEN

18715			IF USE_HISTORY%
			THEN
				PR_TRN_DED = PR_HIS_DED
			END IF

			IF (PR_TRN_DED::EMPNUM = PR_EMP_MASTER::EMPNUM) AND &
				(PR_TRN_DED::PR_END_DATE = PR_END_DATE$) AND &
				(PR_TRN_DED::DTYPE = "C") AND &
				(PR_TRN_DED::CODE = TAX_TYPE$)
			THEN

				!
				! Delete the record if it already exists,
				! and they don't need it.
				!
				IF (PKG_WH_CODE%(TAX_TYPE%) = 0%)
				THEN
					WHEN ERROR IN
						DELETE #PR_TRN_DED.CH%
					USE
						CONTINUE 18720 IF ERR = 11%
						FILENAME$ = "PR_TRN_DED"
						CONTINUE HelpError
					END WHEN

					GOTO 18780
				END IF

				IF PR_TRN_DED::TAX_CODE = &
					EMPLOYEE(TAX_TYPE%, LOOP%)::CODE
				THEN
					PR_TRN_DED::TAXABLE = EMPLOYEE(TAX_TYPE%, LOOP%)::TAXABLE
					PR_TRN_DED::REPORTABLE = EMPLOYEE(TAX_TYPE%, LOOP%)::REPORTABLE
					IF USE_HISTORY%
					THEN
						PR_HIS_DED = PR_TRN_DED
					END IF
					WHEN ERROR IN
						UPDATE #PR_TRN_DED.CH%
					USE
						CONTINUE 18720 IF ERR = 11%
						FILENAME$ = "PR_TRN_DED"
						CONTINUE HelpError
					END WHEN

					GOTO 18780
				ELSE
					WHEN ERROR IN
						GET #PR_TRN_DED.CH%
					USE
						IF ERR = 154%	! Locked Block
						THEN
							SLEEP 5%
							RETRY
						END IF

						CONTINUE 18720 IF ERR = 11%
						FILENAME$ = "PR_TRN_DED"
						CONTINUE HelpError
					END WHEN

					GOTO 18715
				END IF
			END IF

			!
			! Create a new record (Unless EVERYTHING is zero)
			!
18720			IF (EMPLOYEE(TAX_TYPE%, LOOP%)::TAXABLE = 0.0) AND &
				(EMPLOYEE(TAX_TYPE%, LOOP%)::REPORTABLE = 0.0)
			THEN
				GOTO 18780
			END IF

18725			PR_TRN_DED::EMPNUM	= PR_EMP_MASTER::EMPNUM
			PR_TRN_DED::PR_END_DATE	= PR_END_DATE$
			PR_TRN_DED::DTYPE	= "C"
			PR_TRN_DED::CODE	= TAX_TYPE$
			PR_TRN_DED::AMOUNT	= 0.0
			PR_TRN_DED::TAX_CODE	= EMPLOYEE(TAX_TYPE%, LOOP%)::CODE
			PR_TRN_DED::SSTATUS	= ""
			PR_TRN_DED::EXEMPT	= 0%
			PR_TRN_DED::ADDEXEMPT	= 0%
			IF USE_HISTORY%
			THEN
				PR_TRN_DED::UPDATE_FLAG	= 7%
			ELSE
				PR_TRN_DED::UPDATE_FLAG	= 6%
			END IF
			PR_TRN_DED::BATCH	= ""
			PR_TRN_DED::TAXABLE	= EMPLOYEE(TAX_TYPE%, LOOP%)::TAXABLE
			PR_TRN_DED::REPORTABLE	= EMPLOYEE(TAX_TYPE%, LOOP%)::REPORTABLE

			IF USE_HISTORY%
			THEN
				PR_HIS_DED = PR_TRN_DED
			END IF

			PUT #PR_TRN_DED.CH%

18780		NEXT LOOP%

18790	NEXT TAX_TYPE%

	RETURN

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	UTL_REPORTX::STAT = -1%
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
