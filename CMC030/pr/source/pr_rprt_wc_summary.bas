1	%TITLE "Payroll Workman Compensation Summary Report"
	%SBTTL "PR_RPRT_WC_SUMMARY"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1986, 1988 BY
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
	! ID:PR028
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Workman's Compensation report\* consists of a report of the employees
	!	receiving compensation and how is it calculated. The following fields are
	!	contained in the ^*Workman's Compensation\* report:
	!	.table 3,25
	!	.te
	!	Type
	!	.te
	!	State
	!	.te
	!	Workman's Compensation
	!	.te
	!	Description
	!	.te
	!	Regular Time
	!	.te
	!	Overtime
	!	.te
	!	Gross
	!	.te
	!	Method
	!	.te
	!	Employer Subject
	!	.te
	!	Employer Premium
	!	.te
	!	Employee Subject
	!	.te
	!	Employee Premium
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Workmen's Compensation
	!	.x Workmen's Compensation>Report
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_WC_SUMMARY/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_WC_SUMMARY, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_WC_SUMMARY.OBJ;*
	!
	! Author:
	!
	!	04/03/88 - Robert Peterson
	!
	! Modification history:
	!
	!	06/23/89 - Kevin Handy
	!		Opened PR_TEMP file TEMPORARY instead of trying
	!		to remember to delete it.
	!
	!	01/25/90 - Kevin Handy
	!		Changed FACTOR in calculation of SUBJECT_PREM
	!		to PR_TRN_PAY::FACTOR.
	!
	!	06/19/90 - Aaron Redd
	!		Added line layout information so that the report could
	!		also be sent to either a spreadsheet or a DIF file.
	!
	!	10/25/90 - Kevin Handy
	!		Added pay type "X" (eXcess).
	!
	!	12/28/90 - Kevin Handy
	!		Added printing totals for columns 4 and 6.
	!		Attempted to speed up by putting an IF around the
	!		GET on the PR_TAX_PKG file.
	!
	!	01/09/91 - Kevin Handy
	!		Removed PR_WC_DEFINITION file.
	!
	!	06/04/91 - Kevin Handy
	!		Unwound error trapping.
	!
	!	12/18/91 - Kevin Handy
	!		Modified to ignore "A" records in PR_PAY.
	!
	!	01/06/92 - Kevin Handy
	!		Fixed calculation of SUBJECT_PREM to use
	!		(FACTOR-1) instead of (1-FACTOR).
	!
	!	07/14/92 - Kevin Handy
	!		Modified to use last insurance rate in date
	!		range instead of first one.
	!
	!	02/02/92 - Kevin Handy
	!		Changed PR_TEMP to use a RECORD instead of
	!		disassociated variables in a map.
	!
	!	02/02/92 - Kevin Handy
	!		Lost HOUR_RATE, PIECE, FACTOR% from temp
	!		file since they are not needed for the report.
	!
	!	02/04/92 - Kevin Handy
	!		Modified totals to there is some duplication
	!		in the inside of the if statements block, but much
	!		less in the conditional.  I couldn't trace through
	!		the mass of if-thens that resulted to find out what
	!		order they would operate in.
	!
	!	02/04/92 - Kevin Handy
	!		Added the SORTBY field to be able to sort the
	!		report by location.
	!
	!	04/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/10/95 - Kevin Handy
	!		(V3.6)
	!		Update source to v3.6 Calico coding standards.
	!		Took out code for unsolicited input disable.
	!		Removed SUBJECT% parameter from PR_READ_SUBJTAX.
	!
	!	04/11/95 - Kevin Handy
	!		Add open of PR_ERNDED_DEF file.
	!
	!	05/01/96 - Kevin Handy
	!		Reformat source code.
	!
	!	04/08/97 - Kevin Handy
	!		Display folder being scanned during create.
	!
	!	06/02/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/15/99 - Kevin Handy
	!		Use WHEN ERROR IN code
	!--
	%PAGE

	!
	! Set up compiling options
	!
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PKG.HB"
	MAP	(PR_TAX_PKG)		PR_TAX_PKG_CDD		PR_TAX_PKG

	%INCLUDE "SOURCE:[PR.OPEN]PR_WC_DESCR.HB"
	MAP	(PR_WC_DESCR)		PR_WC_DESCR_CDD		PR_WC_DESCR

	%INCLUDE "SOURCE:[PR.OPEN]PR_WC_INSURANCE.HB"
	MAP	(PR_WC_INSURANCE)	PR_WC_INSURANCE_CDD	PR_WC_INSURANCE

	%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.HB"
	MAP	(PR_ERNDED_DEF)		PR_ERNDED_DEF_CDD	PR_ERNDED_DEF

	RECORD PR_TEMP_CDD
		STRING	LOCATION = 4%
		STRING	INS_TYPE = 2%
		STRING	STATE = 4%
		STRING	WC = 6%
		REAL	REG_HR
		REAL	OVT_HR
		REAL	GROSS
		STRING	METHOD = 1%
		REAL	SUBJ_EMPLR
		REAL	INSURANCE_EMPLR
		REAL	SUBJ_EMPLE
		REAL	INSURANCE_EMPLE
	END RECORD

	MAP (PR_TEMP) PR_TEMP_CDD PR_TEMP

	!
	! Declare variables and constants
	!
	DECLARE	STRING	LYT_LINE

	!
	! Dimension arrays
	!
	DIM DATA_FILE$(2000%)

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

	!
	! Find a channel for the temporary file
	!
	CALL ASSG_CHANNEL(PR_TEMP.CH%, STAT%)

 Init:
	!
	! Initilize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)
	FROM_BATCH_NO$ = DATE_STOREDATE(FROM_BATCH_NO$)

	!++
	! Abstract:FLD01
	!	.ts 55
	!	^*(01) Start Payroll Date	MMDDYYYY or MMDDYY\*
	!	.b
	!	.lm +5
	!	The ^*Start Payroll Date\* field enters the
	!	date of the payroll for which the report is to start printing.
	!	.b
	!	A blank in this field will cause the report
	!	to start with the first date in the file.
	!	.lm -5
	!
	! Index:
	!	.x Start Payroll Date>Workman's Compensation Report
	!	.x Workman's Compensation Report>Start Payroll Date
	!
	!--

	TO_BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	TO_BATCH_NO$ = DATE_STOREDATE(TO_BATCH_NO$)

	!++
	! Abstract:FLD02
	!	.ts 55
	!	^*(02) End Payroll Date	MMDDYYYY or MMDDYY\*
	!	.b
	!	.lm +5
	!	The ^*End Payroll Date\* field causes the
	!	printing to end with that particular
	!	date.
	!	.b
	!	A blank field will cause the report to end with the last
	!	date in file.
	!	.lm -5
	!
	! Index:
	!	.x End Payroll Date>Workman's Compensation Report
	!	.x Workman's Compensation Report>End Payroll Date
	!
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!
	! Index:
	!	.x Sort By>Workman's Compensation Report
	!	.x Workman's Compensation Report>Sort By
	!
	!--

	CALL READ_DEVICE("UTL_WORK", UTL_WORK.DEV$, STAT%)
	CALL READ_DEVICE("PR_TRN_PAY", PR_TRN_PAY.DEV$, STAT%)
	CALL READ_DEVICE("PR_HIS_PAY", PR_HIS_PAY.DEV$, STAT%)

	CALL PR_FIND_DETAILFILE(FROM_BATCH_NO$, &
		TO_BATCH_NO$, &
		PR_TRN_PAY.DEV$, &
		PR_HIS_PAY.DEV$, &
		DATA_FILE$())

	DATA_FILE% = VAL%(DATA_FILE$(0%))

300	!
	! Employee Master
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

320	!
	! Open WC Insurance
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_WC_INSURANCE.OPN"
	USE
		FILENAME$ = "PR_WC_INSURANCE"
		CONTINUE HelpError
	END WHEN

330	!
	! Open Subledger subacct
	!
	!%INCLUDE "SOURCE:[SL.OPEN]SL_SUBACCT.OPN"

340	!
	! Open Tax package
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PKG.OPN"
	USE
		FILENAME$ = "PR_TAX_PKG"
		CONTINUE HelpError
	END WHEN

350	!
	! Open wc description file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_WC_DESCR.OPN"
	USE
		FILENAME$ = "PR_WC_DESCR"
		CONTINUE HelpError
	END WHEN

360	!
	! Open Earnings/deduction definition
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.OPN"
	USE
		FILENAME$ = "PR_ERNDED_DEF"
		CONTINUE HelpError
	END WHEN

410	!
	! Create wc audit file
	!
	CALL ENTR_3MESSAGE(SCOPE, &
		"Creating work file.  Reading Pay folder.", 1%)
	TEMP_NUM$ = ""

	WHEN ERROR IN
		OPEN UTL_WORK.DEV$ + "PR_TEMP.TMP" FOR OUTPUT &
			AS FILE PR_TEMP.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP PR_TEMP, &
			TEMPORARY, &
			BUFFER 32%, &
			PRIMARY KEY (PR_TEMP::LOCATION, &
				PR_TEMP::INS_TYPE, &
				PR_TEMP::STATE, &
				PR_TEMP::WC) &
			DUPLICATES, &
			ACCESS MODIFY, ALLOW NONE
	USE
		FILENAME$ = "PR_TEMP"
		CONTINUE HelpError
	END WHEN

	!
	! Open all payroll folder files
	!
	FOR PR_LOOP% = 1% TO DATA_FILE%
		BATCH_NO$ = DATA_FILE$(PR_LOOP%)

420		!
		! Open Pay folder
		!
		USE_HISTORY% = 0%

		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.OPN"
		USE
			CONTINUE 425 IF ERR = 5%
			FILENAME$ = "PR_TRN_PAY"
			CONTINUE HelpError
		END WHEN

		PR_TMP_PAY.CH% = PR_TRN_PAY.CH%

		GOTO 430

425		!
		! Open pay history folder if journal not there
		!
		USE_HISTORY% = -1%

		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_PAY.OPN"
		USE
			FILENAME$ = "PR_TRN_PAY"
			CONTINUE HelpError
		END WHEN

		PR_TMP_PAY.CH% = PR_HIS_PAY.CH%

430		!
		! Add to audit file
		!
		WHEN ERROR IN
			RESET #PR_TMP_PAY.CH%, KEY #0%
		USE
			CONTINUE 530
		END WHEN

440		WHEN ERROR IN
			GET #PR_TMP_PAY.CH%, REGARDLESS
		USE
			CONTINUE 530 IF ERR = 11%
			FILENAME$ = "PR_TRN_PAY"
			CONTINUE HelpError
		END WHEN

		!
		! If journal net there the set history map to journal
		!
		IF USE_HISTORY%
		THEN
			PR_TRN_PAY = PR_HIS_PAY
		END IF

		GOTO 440 IF PR_TRN_PAY::PTYPE = "A"

		IF TEMP_NUM$ <> PR_TRN_PAY::EMPNUM
		THEN
			CALL ENTR_3MESSAGE(SCOPE, &
				"Creating work file. Reading Pay folder. " + &
				PRNT_DATE(BATCH_NO$, 8%) + " " + &
				PR_TRN_PAY::EMPNUM, 1%)
			TEMP_NUM$ = PR_TRN_PAY::EMPNUM
		END IF

		CALL PR_READ_SUBJTAX(PR_ERNDED_DEF.CH%, &
			"WCI", &
			PR_TRN_PAY::PTYPE, &
			PR_TRN_PAY::CODE, &
			SUBJECT%, &
			WCTAXABLE%)

		CALL PR_READ_SUBJTAX(PR_ERNDED_DEF.CH%, &
			"OST", &
			PR_TRN_PAY::PTYPE, &
			PR_TRN_PAY::CODE, &
			TAXABLE%, &
			REPORTABLE%)

450		IF TEST_EMPNUM$ <> PR_TRN_PAY::EMPNUM
		THEN
			PR_EMP_MASTER::WC = ""
			PR_EMP_MASTER::LOCATION = ""

			WHEN ERROR IN
				GET #PR_EMP_MASTER.CH%, &
					KEY #0% EQ PR_TRN_PAY::EMPNUM, &
					REGARDLESS
			USE
				CONTINUE 460 IF ERR = 155%
				FILENAME$ = "PR_EMP_MASTER"
				CONTINUE HelpError
			END WHEN
		END IF

460		TEST_EMPNUM$ = PR_TRN_PAY::EMPNUM

		!
		! Look up state
		!
		STATE$ = "??"

		IF (PR_TRN_PAY::TAX_PKG <> PR_TAX_PKG::TAX_PKG) OR &
			(PR_TAX_PKG::STTYPE <> "SW")
		THEN
			WHEN ERROR IN
				GET #PR_TAX_PKG.CH%, &
					KEY #0% EQ PR_TRN_PAY::TAX_PKG + "SW", &
					REGARDLESS
			USE
				CONTINUE 490 IF ERR = 155%
				FILENAME$ = "PR_TAX_PKG"
				CONTINUE HelpError
			END WHEN

		END IF

		STATE$ = PR_TAX_PKG::CODE

490		!
		! Calculate subject pay
		!
		SUBJECT_PREM = 0.0

		IF PR_TRN_PAY::RTYPE = "H" OR PR_TRN_PAY::RTYPE = "S" OR &
			PR_TRN_PAY::RTYPE = "X"
		THEN
			SUBJECT_PREM = FUNC_ROUND(PR_TRN_PAY::OVT_HR * ( &
				(PR_TRN_PAY::FACTOR / 100.0 - 1.0) * &
				PR_TRN_PAY::HOUR_RATE), 2%)
			SUBJECT_PREM = 0.0 IF SUBJECT_PREM < 0.0
		END IF

		!
		! Set initial values in temp file
		!
		IF SORTBY$ = "LO"
		THEN
			PR_TEMP::LOCATION	= PR_EMP_MASTER::LOCATION
		ELSE
			PR_TEMP::LOCATION	= ""
		END IF
		PR_TEMP::STATE		= STATE$
		PR_TEMP::WC		= PR_EMP_MASTER::WC
		PR_TEMP::REG_HR		= PR_TRN_PAY::REG_HR
		PR_TEMP::OVT_HR		= PR_TRN_PAY::OVT_HR
		PR_TEMP::GROSS		= PR_TRN_PAY::GROSS

		!
		! Inital values for insurance file
		!
		TEST_INS_TYPE$ = ""
		INS_LOOP% = 0%
		SUBJ(LOOP%) = 0.0 FOR LOOP% = 1% TO 10%
		EMPLR_RATE(LOOP%) = 0.0 FOR LOOP% = 1% TO 10%
		EMPLE_RATE(LOOP%) = 0.0 FOR LOOP% = 1% TO 10%
		ADD_INS%(LOOP%) = 0% FOR LOOP% = 1% TO 10%

500		!
		! Look up insurance record
		!
		WHEN ERROR IN
			FIND #PR_WC_INSURANCE.CH%, &
				KEY #0% EQ PR_EMP_MASTER::WC + STATE$, &
				REGARDLESS
		USE
			CONTINUE 520 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "PR_WC_INSURANCE"
			CONTINUE HelpError
		END WHEN

 GetNextIns:
510		WHEN ERROR IN
			GET #PR_WC_INSURANCE.CH%, REGARDLESS
		USE
			CONTINUE 520 IF ERR = 11%
			FILENAME$ = "PR_WC_INSURANCE"
			CONTINUE HelpError
		END WHEN

		GOTO 520 IF PR_WC_INSURANCE::CODE + PR_WC_INSURANCE::STATE <> &
			PR_EMP_MASTER::WC + STATE$

		IF PR_WC_INSURANCE::EFFDAT <= BATCH_NO$
		THEN
			!
			! Increment only if we don't want to replace previous
			! rete (this rate has later date)
			!
			IF TEST_INS_TYPE$ <> PR_WC_INSURANCE::INS_TYPE &
				OR INS_LOOP% = 0%
			THEN
				INS_LOOP% = INS_LOOP% + 1%
			END IF

			TEST_INS_TYPE$ = PR_WC_INSURANCE::INS_TYPE
			WC_INS_TYPE$(INS_LOOP%) = TEST_INS_TYPE$
			WC_METHOD$(INS_LOOP%) = PR_WC_INSURANCE::METHOD
			ADD_INS%(INS_LOOP%) = 0%

			EMPLR_RATE(INS_LOOP%) = PR_WC_INSURANCE::EMPLR_RATE
			EMPLE_RATE(INS_LOOP%) = PR_WC_INSURANCE::EMPLE_RATE

			SELECT PR_WC_INSURANCE::METHOD

			CASE "1"
				SUBJ(INS_LOOP%) = PR_TRN_PAY::GROSS - &
					SUBJECT_PREM
				ADD_INS%(INS_LOOP%) = -1%

			CASE "2"
				SUBJ(INS_LOOP%) = PR_TRN_PAY::GROSS
				ADD_INS%(INS_LOOP%) = -1%

			CASE "3"
				SUBJ(INS_LOOP%) = PR_TRN_PAY::REG_HR + &
					PR_TRN_PAY::OVT_HR
				ADD_INS%(INS_LOOP%) = -1%

			CASE "4"
				SUBJ(INS_LOOP%) = PR_TRN_PAY::REG_HR
				ADD_INS%(INS_LOOP%) = -1%

			CASE "5"
				SUBJ(INS_LOOP%) = PR_TRN_PAY::REG_HR / 8.
				ADD_INS%(INS_LOOP%) = -1%

			END SELECT

		END IF

		GOTO GetNextIns

520		!
		! Insurance loop
		!
		FOR LOOP% = 1% TO INS_LOOP%
			IF ADD_INS%(INS_LOOP%)
			THEN
				PR_TEMP::INS_TYPE = WC_INS_TYPE$(LOOP%)
				PR_TEMP::METHOD = WC_METHOD$(LOOP%)

				PR_TEMP::INSURANCE_EMPLR, &
					PR_TEMP::INSURANCE_EMPLE = 0.0
				PR_TEMP::SUBJ_EMPLR, PR_TEMP::SUBJ_EMPLE = 0.0

				IF SUBJECT% = 0%
				THEN
					PR_TEMP::SUBJ_EMPLR = SUBJ(LOOP%)
					PR_TEMP::INSURANCE_EMPLR = &
						PR_TEMP::SUBJ_EMPLR * &
						EMPLR_RATE(LOOP%)
				END IF

				IF REPORTABLE% = 0%
				THEN
					PR_TEMP::SUBJ_EMPLE = SUBJ(LOOP%)
					PR_TEMP::INSURANCE_EMPLE = &
						PR_TEMP::SUBJ_EMPLE * &
						EMPLE_RATE(LOOP%)
				END IF

				PUT #PR_TEMP.CH%
			END IF
		NEXT LOOP%

525		!
		! Get next payroll record
		!
		GOTO 440

530		CALL ASSG_FREECHANNEL(PR_TRN_PAY.CH%)
		CALL ASSG_FREECHANNEL(PR_HIS_PAY.CH%)
		CLOSE PR_TMP_PAY.CH%
	!
	! Get next payroll
	!
	NEXT PR_LOOP%

	%PAGE

 ReportTitle:
 !	SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

	!
	! Set up titles
	!
	TITLE$(1%) = "Payroll Workmen Comp Summary Report"
	TITLE$(2%) = "For the Payroll Folders Dated From:  " + &
		PRNT_DATE(FROM_BATCH_NO$, 8%) + &
		"  To:  " + PRNT_DATE(TO_BATCH_NO$, 8%)
	TITLE$(3%) = ""

	!
	! Column headings
	!
	TITLE$(4%) = SPACE$(46%) + &
		SPACE$(41%) + &
		"-------Employer------ -------Employee------"
	TITLE$(5%) = "Ty  St  Wc       Description                  " + &
		"   RegTime   Overtime      Gross  Method " + &
		"      Subj    Premium       Subj    Premium"
	TITLE$(6%) = ""

	!
	! Line layouts
	!
	LYT_LINE = "$Type:004,$State:008,$WC:017,$Descr:046," + &
		"VRegTime:056,VOvertime:067,VGross:078,$TestMethod:083," + &
		"VEmployerSubj:097,VEmployerPrem:108,VEmployeeSubj:119," + &
		"VEmployeePrem:130"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	RESET #PR_TEMP.CH%, KEY #0%

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #PR_TEMP.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PR_TEMP"
		CONTINUE HelpError
	END WHEN

	IF PR_TEMP::LOCATION <> TEST.LOCATION$
	THEN
		IF PASS.1%
		THEN
			GOSUB LocTotal
			GOTO ExitProgram IF UTL_REPORTX::STAT
			GOSUB StateTotal
			GOTO ExitProgram IF UTL_REPORTX::STAT
			GOSUB InsTotal
			GOTO ExitProgram IF UTL_REPORTX::STAT
		END IF

		WORK_LOCATION$ = PR_TEMP::LOCATION
		WORK_INS_TYPE$ = PR_TEMP::INS_TYPE
		WORK_WC$ = PR_TEMP::WC
		WORK_STATE$ = PR_TEMP::STATE
	END IF

	IF PR_TEMP::INS_TYPE <> TEST.INS_TYPE$
	THEN
		IF PASS.1%
		THEN
			GOSUB WCTotal
			GOTO ExitProgram IF UTL_REPORTX::STAT
			GOSUB StateTotal
			GOTO ExitProgram IF UTL_REPORTX::STAT
			GOSUB InsTotal
			GOTO ExitProgram IF UTL_REPORTX::STAT
		END IF

		WORK_INS_TYPE$ = PR_TEMP::INS_TYPE
		WORK_WC$ = PR_TEMP::WC
		WORK_STATE$ = PR_TEMP::STATE
	END IF

	IF PR_TEMP::STATE <> TEST.STATE$
	THEN
		IF PASS.1%
		THEN
			GOSUB WCTotal
			GOTO ExitProgram IF UTL_REPORTX::STAT
			GOSUB StateTotal
			GOTO ExitProgram IF UTL_REPORTX::STAT
		END IF

		WORK_STATE$ = PR_TEMP::STATE
		WORK_WC$ = PR_TEMP::WC
	END IF

	IF PR_TEMP::WC <> TEST.WC$
	THEN
		IF PASS.1%
		THEN
			GOSUB WCTotal
			GOTO ExitProgram IF UTL_REPORTX::STAT
		END IF

		WORK_WC$ = PR_TEMP::WC
	END IF

	IF PRINT_LINE%
	THEN
		!
		! Print grand total
		!
		GOSUB PrintLine

		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	PRINT_LINE% = 0%

	!
	! Set test values
	!
	TEST.INS_TYPE$	= PR_TEMP::INS_TYPE
	TEST.STATE$	= PR_TEMP::STATE
	TEST.WC$	= PR_TEMP::WC
	TEST_METHOD$	= PR_TEMP::METHOD

	PASS.1% = -1%

	WC_TOTAL(1%) = WC_TOTAL(1%) + PR_TEMP::REG_HR
	WC_TOTAL(2%) = WC_TOTAL(2%) + PR_TEMP::OVT_HR
	WC_TOTAL(3%) = WC_TOTAL(3%) + PR_TEMP::GROSS
	WC_TOTAL(4%) = WC_TOTAL(4%) + PR_TEMP::SUBJ_EMPLR
	WC_TOTAL(5%) = WC_TOTAL(5%) + PR_TEMP::INSURANCE_EMPLR
	WC_TOTAL(6%) = WC_TOTAL(6%) + PR_TEMP::SUBJ_EMPLE
	WC_TOTAL(7%) = WC_TOTAL(7%) + PR_TEMP::INSURANCE_EMPLE

	!
	! Go to next record
	!
	GOTO 17020


 ExitTotal:
	!
	! Handle end of report
	!
	IF PASS.1%
	THEN
		GOSUB WCTotal
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	GOSUB StateTotal
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOSUB InsTotal
	GOTO ExitProgram IF UTL_REPORTX::STAT

	IF SORTBY$ = "LO"
	THEN
		GOSUB LocTotal
	END IF

	!
	! Print grand total
	!
	GOSUB PrintLine

	TEXT$ = "         Grand Total                          " + &
		FORMAT$(GRAND_TOTAL(1%), "#######.## ") + &
		FORMAT$(GRAND_TOTAL(2%), "#######.## ") + &
		FORMAT$(GRAND_TOTAL(3%), "#######.##         ") + &
		FORMAT$(GRAND_TOTAL(4%), "#######.## ") + &
		FORMAT$(GRAND_TOTAL(5%), "#######.## ") + &
		FORMAT$(GRAND_TOTAL(6%), "#######.## ") + &
		FORMAT$(GRAND_TOTAL(7%), "#######.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOSUB PrintLine

 ExitProgram:
17500	!
	! Kill temp file
	!
	CLOSE PR_TEMP.CH%

17510	CALL OUTP_FINISH(UTL_REPORTX)

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

 WCTotal:
	!******************************************************************
	! Print Total for wc code
	!******************************************************************
17700	!
	! Look up WC description
	!
	IF WORK_WC$ <> ""
	THEN
		WORK_DESC$ = STRING$(LEN(PR_WC_DESCR::DESCR), 63%)

		WHEN ERROR IN
			GET #PR_WC_DESCR.CH%, KEY #0% GE WORK_WC$, REGARDLESS
		USE
			CONTINUE 17710 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "PR_WC_DESCR"
			CONTINUE HelpError
		END WHEN

		WORK_DESC$ = PR_WC_DESCR::DESCR
	END IF

17710	SUM = 0.0
	SUM = FUNC_ROUND(SUM + WC_TOTAL(LOOP%), 2%) FOR LOOP% = 1% TO 7%

	IF SUM <> 0.0
	THEN
		TEXT$ = LEFT(WORK_INS_TYPE$ + SPACE$(4%), 4%) + &
			LEFT(WORK_STATE$ + SPACE$(4%), 4%) + &
			LEFT(WORK_WC$ + SPACE$(9%), 9%) + &
			LEFT(WORK_DESC$ + SPACE$(29%), 29%) + &
			FORMAT$(WC_TOTAL(1%), "#######.## ") + &
			FORMAT$(WC_TOTAL(2%), "#######.## ") + &
			FORMAT$(WC_TOTAL(3%), "#######.## ") + &
			"   " + LEFT(TEST_METHOD$ + " ", 1%) + "    " + &
			FORMAT$(WC_TOTAL(4%), "#######.## ") + &
			FORMAT$(WC_TOTAL(5%), "#######.## ") + &
			FORMAT$(WC_TOTAL(6%), "#######.## ") + &
			FORMAT$(WC_TOTAL(7%), "#######.##")

		CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)

		WORK_INS_TYPE$, WORK_STATE$, WORK_WC$, WORK_DESC$ = ""
	END IF

	STATE_TOTAL(LOOP%) = STATE_TOTAL(LOOP%) + WC_TOTAL(LOOP%) &
		FOR LOOP% = 1% TO 7%

	WC_TOTAL(LOOP%) = 0.0 FOR LOOP% = 1% TO 7%

	RETURN

	%Page

 StateTotal:
	!******************************************************************
	! Print Total for State
	!******************************************************************
	SUM = 0.0
	SUM = FUNC_ROUND(SUM + STATE_TOTAL(LOOP%), 2%) FOR LOOP% = 1% TO 7%

	IF SUM <> 0.0
	THEN
		GOSUB PrintLine

		TEXT$ = "     State Total                              " + &
			FORMAT$(STATE_TOTAL(1%), "#######.## ") + &
			FORMAT$(STATE_TOTAL(2%), "#######.## ") + &
			FORMAT$(STATE_TOTAL(3%), "#######.##         ") + &
			FORMAT$(STATE_TOTAL(4%), "#######.## ") + &
			FORMAT$(STATE_TOTAL(5%), "#######.## ") + &
			FORMAT$(STATE_TOTAL(6%), "#######.## ") + &
			FORMAT$(STATE_TOTAL(7%), "#######.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		PRINT_LINE% = -1%
	END IF

	INS_TOTAL(LOOP%) = INS_TOTAL(LOOP%) + STATE_TOTAL(LOOP%) &
		FOR LOOP% = 1% TO 7%

	LOCATION_TOTAL(LOOP%) = LOCATION_TOTAL(LOOP%) + STATE_TOTAL(LOOP%) &
		FOR LOOP% = 1% TO 7%

	STATE_TOTAL(LOOP%) = 0.0 FOR LOOP% = 1% TO 7%

	RETURN

	%Page

 InsTotal:
	!******************************************************************
	! Print Total for Insurance type
	!******************************************************************
	SUM = 0.0
	SUM = FUNC_ROUND(SUM + INS_TOTAL(LOOP%), 2%) FOR LOOP% = 1% TO 7%

	IF SUM <> 0.0
	THEN
		GOSUB PrintLine

		TEXT$ = "       Ins Total                              " + &
			FORMAT$(INS_TOTAL(1%), "#######.## ") + &
			FORMAT$(INS_TOTAL(2%), "#######.## ") + &
			FORMAT$(INS_TOTAL(3%), "#######.##         ") + &
			FORMAT$(INS_TOTAL(4%), "#######.## ") + &
			FORMAT$(INS_TOTAL(5%), "#######.## ") + &
			FORMAT$(INS_TOTAL(6%), "#######.## ") + &
			FORMAT$(INS_TOTAL(7%), "#######.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		PRINT_LINE% = -1%
	END IF

	GRAND_TOTAL(LOOP%) = GRAND_TOTAL(LOOP%) + INS_TOTAL(LOOP%) &
		FOR LOOP% = 1% TO 7%

	INS_TOTAL(LOOP%) = 0.0 FOR LOOP% = 1% TO 7%

	RETURN

	%Page

 LocTotal:
	!******************************************************************
	! Print Total for State
	!******************************************************************

	GOSUB PrintLine

	TEXT$ = "     Location Total                           " + &
		FORMAT$(LOCATION_TOTAL(1%), "#######.## ") + &
		FORMAT$(LOCATION_TOTAL(2%), "#######.## ") + &
		FORMAT$(LOCATION_TOTAL(3%), "#######.##         ") + &
		FORMAT$(LOCATION_TOTAL(4%), "#######.## ") + &
		FORMAT$(LOCATION_TOTAL(5%), "#######.## ") + &
		FORMAT$(LOCATION_TOTAL(6%), "#######.## ") + &
		FORMAT$(LOCATION_TOTAL(7%), "#######.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	PRINT_LINE% = -1%

	LOCATION_TOTAL(LOOP%) = 0.0 FOR LOOP% = 1% TO 7%

	RETURN

	%PAGE

 PrintLine:
	!*****************************************************************
	! Print underline
	!*****************************************************************

	TEXT$ = SPACE$(46%) + &
		"---------- ---------- ----------         " + &
		"---------- ---------- ---------- ----------"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3%)

	RETURN

	%Page

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
