1	%TITLE "Payroll Workman's Compensation Audit Report"
	%SBTTL "PR_RPRT_AUDT_WC"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1988 BY
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
	! ID:PR053
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Workman's Compensation Audit report\*
	!	gives the total gross,
	!	subject earnings, and premium for a specified
	!	time period. This report includes
	!	the following fields:
	!	.table 3,25
	!	.te
	!	Type
	!	.te
	!	State
	!	.te
	!	Workman's Compensation
	!	.te
	!	Employee Number
	!	.te
	!	Name
	!	.te
	!	Paydate
	!	.te
	!	Regular Time
	!	.te
	!	Over Time
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
	!	.x Report>Workmen's Compensation. Audit
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_AUDT_WC/LINE/NOOPT
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_AUDT_WC, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_AUDT_WC.OBJ;*
	!
	! Author:
	!
	!	03/27/88 - Robert Peterson
	!
	! Modification history:
	!
	!	06/23/89 - Kevin Handy
	!		Opened PR_TEMP as TEMPORARY instead of trying
	!		to remember to delete it.
	!
	!	01/25/90 - Kevin Handy
	!		Changed variable FACTOR in calculation of
	!		SUBJECT_PREM to PR_TRN_PAY::FACTOR.
	!
	!	03/05/90 - Kevin Handy
	!		Fixed bug where it was calculating premium
	!		by subtracting the ovt factor from 1, instead
	!		of subtracting 1 from the ovt factor.
	!
	!	06/15/90 - Aaron Redd
	!		Added line layout information so that the report
	!		can be sent to a spreadsheet or a DIF file.
	!
	!	10/25/90 - Kevin Handy
	!		Added pay type "X" (eXcess).
	!
	!	12/27/90 - Kevin Handy
	!		Attempt at a slight speed-up by placing an IF
	!		statement around the search on PR_TAX_PKG file.
	!
	!	12/28/90 - Kevin Handy
	!		Added printing totals for items 4 and 6.
	!
	!	01/09/91 - Kevin Handy
	!		Removed PR_WC_DEFINITION file.
	!
	!	06/03/91 - Kevin Handy
	!		Unwound error trapping.  Removed commented out code.
	!
	!	12/18/91 - Kevin Handy
	!		Modified to ignore "A" records in PR_PAY.
	!
	!	07/14/92 - Kevin Handy
	!		Modified to use last date in insurance file within
	!		range instead of first one.
	!
	!	10/23/92 - Kevin Handy
	!		Modified to display date being read while generating
	!		sort file.
	!
	!	10/23/92 - Kevin Handy
	!		Dimensioned various arrays, set at 50 using
	!		constant definition MAX_INS_RATES.
	!
	!	04/14/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/10/93 - Kevin Handy
	!		Fix goofy indentation.
	!		Increase buffer size on temp file from 32 to 64.
	!
	!	05/13/93 - Kevin Handy
	!		Modified to load PR_WC_INSURANCE records into
	!		memory for increased speed (less file searching
	!		on the innermost loop).
	!
	!	04/10/95 - Kevin Handy
	!		(V3.6)
	!		Updated for V3.6 coding standard.
	!		Removed SUBJECT% parameter from PR_READ_SUBJTAX.
	!		Lost disable of unsolicited input.
	!
	!	04/11/95 - Kevin Handy
	!		Added open for PR_ERNDED_DEF.
	!
	!	01/16/96 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!		Lose several lines of commented out code.
	!
	!	01/16/96 - Kevin Handy
	!		Change PR_TEMP to use record structure unstead of
	!		individual variables.
	!
	!	01/16/96 - Kevin Handy
	!		Add wildcard for ins type, state, and WC code.
	!
	!	10/25/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE's
	!
	!	12/11/2000 - Kevin Handy
	!		Use WHEN ERROR IN
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
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP (PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.HB"
	MAP (PR_TRN_PAY)	PR_TRN_PAY_CDD	PR_TRN_PAY
	MAP (PR_HIS_PAY)	PR_TRN_PAY_CDD	PR_HIS_PAY

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PKG.HB"
	MAP (PR_TAX_PKG)	PR_TAX_PKG_CDD	PR_TAX_PKG

	%INCLUDE "SOURCE:[PR.OPEN]PR_WC_INSURANCE.HB"
	MAP (PR_WC_INSURANCE)	PR_WC_INSURANCE_CDD	PR_WC_INSURANCE
	DIM PR_WC_INSURANCE_CDD PR_WC_INSURANCE_ARRAY(200%)

	%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.HB"
	MAP (PR_ERNDED_DEF)	PR_ERNDED_DEF_CDD	PR_ERNDED_DEF

	RECORD PR_TEMP_CDD
		STRING INS_TYPE = 2%
		STRING STATE = 4%
		STRING WC = 6%
		STRING EMPNUM = 10%
		STRING PR_END_DATE = 08%
		STRING SUBACC = 10%
		STRING OPER = 08%
		STRING ACCT = 18%
		GFLOAT HOUR_RATE
		GFLOAT REG_HR
		GFLOAT OVT_HR
		GFLOAT PIECE
		LONG FACTOR
		GFLOAT GROSS
		STRING METHOD = 1%
		GFLOAT SUBJ_EMPLR
		GFLOAT INSURANCE_EMPLR
		GFLOAT SUBJ_EMPLE
		GFLOAT INSURANCE_EMPLE
	END RECORD

	MAP (PR_TEMP) PR_TEMP_CDD PR_TEMP

	!
	! Dimension
	!
	DECLARE LONG CONSTANT MAX_INS_RATES = 50%

	DIM DATA_FILE$(200%)

	DIM WC_INS_TYPE$(MAX_INS_RATES), WC_METHOD$(MAX_INS_RATES), &
		EMPLR_RATE(MAX_INS_RATES), EMPLE_RATE(MAX_INS_RATES), &
		SUBJ(MAX_INS_RATES), ADD_INS%(MAX_INS_RATES)

	%PAGE

	ON ERROR GOTO 19000

	CALL ASSG_CHANNEL(PR_TEMP.CH%, STAT%)

 Init:	!
	! Initilize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)
	FROM_BATCH_NO$ = DATE_STOREDATE(FROM_BATCH_NO$) ! Reformat to (YYYYMMDD)

	!++
	! Abstract:FLD01
	!	.ts 55
	!	^*(01) From Payroll Date	MMDDYYYY or MMDDYY\*
	!	.b
	!	.lm +5
	!	The ^*From Payroll Date\* field enters the payroll date with
	!	which the report is to begin printing.
	!	.b
	!	A blank field will cause the report
	!	to begin with the first date in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Payroll Date>Workman's Compensation Audit Report
	!	.x Workman's Compensation Audit Report>From Payroll Date
	!
	!--

	TO_BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	TO_BATCH_NO$ = DATE_STOREDATE(TO_BATCH_NO$) ! Reformat to (YYYYMMDD)

	!++
	! Abstract:FLD02
	!	^*(02) To Payroll Date	MMDDYYYY or MMDDYY\*
	!	.b
	!	.lm +5
	!	The ^*To Payroll Date\* field enters the date with which the
	!	report is to end printing.
	!	.b
	!	A blank field will cause the report to print
	!	to the end of the file.
	!
	! Index:
	!	.x To Payroll Date>Workman's Compensation Audit Report
	!	.x Workman's Compensation Audit Report>To Payroll Date
	!
	!--

	WILD_INS_TYPE$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard Insurance Type	Wildcard\*
	!	.b
	!	.lm +5
	!	Used to select which insurance types will appear on
	!	the report
	!	.lm -5
	!
	! Index:
	!	.x Insurance Type>Workman's Compensation Audit Report
	!	.x Workman's Compensation Audit Report>Insurance Type
	!--

	WILD_STATE$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard State	Wildcard\*
	!	.b
	!	.lm +5
	!	Used to select which states will appear on the report.
	!	.lm -5
	!
	! Index:
	!	.x State>Workman's Compensation Audit Report
	!	.x Workman's Compensation Audit Report>State
	!--

	WILD_WC$ = EDIT$(UTL_REPORTX::OPTDEF(4%), 132%)

	!++
	! Abstract:FLD05
	!	^*(03) Wildcard Workman Comp Code	MMDDYYYY or MMDDYY\*
	!	.b
	!	.lm +5
	!	Used to select which workman comp codes will appear on
	!	the report
	!	.lm -5
	!
	! Index:
	!	.x Workman Comp Code>Workman's Compensation Audit Report
	!	.x Workman's Compensation Audit Report>Workman Comp Code
	!--

	CALL READ_DEVICE("PR_TRN_PAY", PR_TRN_PAY.DEV$, STAT%)
	CALL READ_DEVICE("PR_HIS_PAY", PR_HIS_PAY.DEV$, STAT%)

	CALL PR_FIND_DETAILFILE(FROM_BATCH_NO$, &
		TO_BATCH_NO$, &
		PR_TRN_PAY.DEV$, &
		PR_HIS_PAY.DEV$, &
		DATA_FILE$())

	DATA_FILE% = VAL%(DATA_FILE$(0%))

300	!
	! Open employee master file
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
		RESET #PR_WC_INSURANCE.CH%
	USE
		FILENAME$ = "PR_WC_INSURANCE"
		CONTINUE HelpError
	END WHEN

	PR_WC_INSURANCE_ARRAY% = 0%

325	WHEN ERROR IN
		GET #PR_WC_INSURANCE.CH%, REGARDLESS
	USE
		CONTINUE 340
	END WHEN

	PR_WC_INSURANCE_ARRAY% = PR_WC_INSURANCE_ARRAY% + 1%
	PR_WC_INSURANCE_ARRAY(PR_WC_INSURANCE_ARRAY%) = PR_WC_INSURANCE

	GOTO 325

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
	! Open Earning/Deduction definition
	!
	%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.OPN"

410	!
	! Create wc audit file
	!
	CALL ENTR_3MESSAGE(SCOPE, "Creating work file.  Reading Pay folder.", 1%)

	WHEN ERROR IN
		OPEN PR_TRN_PAY.DEV$ + "PR_TEMP.TMP" FOR OUTPUT AS FILE PR_TEMP.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP PR_TEMP, &
			TEMPORARY, &
			BUFFER 64%, &
			PRIMARY KEY (PR_TEMP::INS_TYPE, &
				PR_TEMP::STATE, &
				PR_TEMP::WC, &
				PR_TEMP::EMPNUM, &
				PR_TEMP::PR_END_DATE) &
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

		CALL ENTR_3MESSAGE(SCOPE, &
			"Creating work file.  Reading Pay folder. " + BATCH_NO$, 1%)

		TEST_EMPNUM$ = ""

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
			WHEN ERROR IN
				GET #PR_EMP_MASTER.CH%, &
					KEY #0% EQ PR_TRN_PAY::EMPNUM, &
					REGARDLESS
			USE
				PR_EMP_MASTER::WC = ""

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

		IF (PR_TAX_PKG::TAX_PKG <> PR_TRN_PAY::TAX_PKG) OR &
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
			SUBJECT_PREM = FUNC_ROUND(PR_TRN_PAY::OVT_HR * &
				(PR_TRN_PAY::FACTOR/100. - 1.) * &
				PR_TRN_PAY::HOUR_RATE, 2%)
			SUBJECT_PREM = 0.0 IF SUBJECT_PREM < 0.0
		END IF

		!
		! Set initial values in temp file
		!
		PR_TEMP::STATE		= STATE$
		PR_TEMP::WC		= PR_EMP_MASTER::WC
		PR_TEMP::EMPNUM		= PR_TRN_PAY::EMPNUM
		PR_TEMP::PR_END_DATE	= PR_TRN_PAY::PR_END_DATE
		PR_TEMP::SUBACC		= PR_TRN_PAY::SUBACC
		PR_TEMP::OPER		= PR_TRN_PAY::OPER
		PR_TEMP::ACCT		= PR_TRN_PAY::ACCT
		PR_TEMP::HOUR_RATE	= PR_TRN_PAY::HOUR_RATE
		PR_TEMP::REG_HR		= PR_TRN_PAY::REG_HR
		PR_TEMP::OVT_HR		= PR_TRN_PAY::OVT_HR
		PR_TEMP::PIECE		= PR_TRN_PAY::PIECE
		PR_TEMP::FACTOR		= PR_TRN_PAY::FACTOR
		PR_TEMP::GROSS		= PR_TRN_PAY::GROSS

		!
		! Inital values for insurance file
		!
		INS_LOOP% = 0%
		TEST_INS_TYPE$ = ""
		SUBJ(LOOP%) = 0.0 FOR LOOP% = 1% TO 10%
		EMPLR_RATE(LOOP%) = 0.0 FOR LOOP% = 1% TO 10%
		EMPLE_RATE(LOOP%) = 0.0 FOR LOOP% = 1% TO 10%
		ADD_INS%(LOOP%) = 0% FOR LOOP% = 1% TO 10%

500		!
		! Look up insurance record
		!
		! First binary search to create smaller search limits
		!
		MINIMUM% = 1%
		MAXIMUM% = PR_WC_INSURANCE_ARRAY%

		!
		! Sequential search to pin it down
		!
		FOR ITEM% = MAXIMUM% TO MINIMUM% STEP -1%
			IF PR_WC_INSURANCE_ARRAY(ITEM%)::CODE = &
				PR_EMP_MASTER::WC AND &
				PR_WC_INSURANCE_ARRAY(ITEM%)::STATE = STATE$
			THEN
				MAXIMUM% = ITEM%
			END IF
		NEXT ITEM%

 GetNextIns:
510		GOTO 520 IF MAXIMUM% > PR_WC_INSURANCE_ARRAY%

		GOTO 520 IF PR_WC_INSURANCE_ARRAY(MAXIMUM%)::CODE <> &
			PR_EMP_MASTER::WC OR &
			PR_WC_INSURANCE_ARRAY(MAXIMUM%)::STATE <> STATE$

		IF PR_WC_INSURANCE_ARRAY(MAXIMUM%)::EFFDAT <= BATCH_NO$
		THEN
			IF TEST_INS_TYPE$ <> &
				PR_WC_INSURANCE_ARRAY(MAXIMUM%)::INS_TYPE &
				OR INS_LOOP% = 0%
			THEN
				INS_LOOP% = INS_LOOP% + 1%
			END IF

			TEST_INS_TYPE$ = PR_WC_INSURANCE_ARRAY(MAXIMUM%)::INS_TYPE
			WC_INS_TYPE$(INS_LOOP%) = TEST_INS_TYPE$
			WC_METHOD$(INS_LOOP%) = PR_WC_INSURANCE_ARRAY(MAXIMUM%)::METHOD
			ADD_INS%(INS_LOOP%) = 0%

			EMPLR_RATE(INS_LOOP%) = PR_WC_INSURANCE_ARRAY(MAXIMUM%)::EMPLR_RATE
			EMPLE_RATE(INS_LOOP%) = PR_WC_INSURANCE_ARRAY(MAXIMUM%)::EMPLE_RATE

			SELECT PR_WC_INSURANCE_ARRAY(MAXIMUM%)::METHOD

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
				SUBJ(INS_LOOP%) = PR_TRN_PAY::REG_HR/8.
				ADD_INS%(INS_LOOP%) = -1%

			END SELECT

		END IF

		MAXIMUM% = MAXIMUM% + 1%

		GOTO GetNextIns

520		!
		! Insurance loop
		!
		FOR LOOP% = 1% TO INS_LOOP%
			IF ADD_INS%(INS_LOOP%)
			THEN
				PR_TEMP::INS_TYPE	= WC_INS_TYPE$(LOOP%)
				PR_TEMP::METHOD		= WC_METHOD$(LOOP%)

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

				PUTFLAG% = -1%

				IF (WILD_INS_TYPE$ <> "")
				THEN
					PUTFLAG% = 0% &
						IF COMP_STRING(PR_TEMP::INS_TYPE, &
						WILD_INS_TYPE$) = 0%
				END IF

				IF (WILD_STATE$ <> "")
				THEN
					PUTFLAG% = 0% &
						IF COMP_STRING(PR_TEMP::STATE, &
						WILD_STATE$) = 0%
				END IF

				IF (WILD_WC$ <> "")
				THEN
					PUTFLAG% = 0% &
						IF COMP_STRING(PR_TEMP::WC, &
						WILD_WC$) = 0%
				END IF

				PUT #PR_TEMP.CH% IF PUTFLAG%
			END IF
		NEXT LOOP%

525		!
		! Get next payroll record
		!
		GOTO 440

530		CLOSE PR_TMP_PAY.CH%
		CALL ASSG_FREECHANNEL(PR_TRN_PAY.CH%)
		CALL ASSG_FREECHANNEL(PR_HIS_PAY.CH%)
		PR_TRN_PAY.CH%, PR_HIS_PAY.CH% = 0%

	!
	! Get next payroll
	!
	NEXT PR_LOOP%

	%PAGE

 ReportTitle:

	!
	! Set up titles
	!
	TITLE$(1%) = "Payroll Workmen Comp Audit Report"
	TITLE$(2%) = "For the Payroll Folders Dated From: " + &
		PRNT_DATE(FROM_BATCH_NO$, 8%) + &
		" To: " + PRNT_DATE(TO_BATCH_NO$, 8%)
	TITLE$(3%) = ""

	!
	! Set column headings
	!
	TITLE$(4%) = SPACE$(93%) + "------Employer----- ------Employee-----"
	TITLE$(5%) = "Ty St Wc     Emp #      Name               " + &
		"Pay Date     Reg Time  Over Time      Gross Method     " + &
		"Subj   Premium      Subj   Premium"
	TITLE$(6%) = ""

	!
	! Define line layouts
	!
	LYT_LINE$ = "$Type:003,$State:006,$WC:013,$EmpNum:023," + &
		"$EmpName:042,DPRDate:053,VRegTime:064,VOverTime:075," + &
		"VGross:086,$Method:091,VEmployerSubj:102," + &
		"VEmployerPrem:112,VEmployeeSubj:122,VEmployeePrem:132"

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

	!
	! Check current record
	!
	GOTO 17060 IF PR_TEMP::EMPNUM = TEST.EMPNUM$ AND &
		PR_TEMP::PR_END_DATE = TEST.PR_END_DATE$ AND &
		PR_TEMP::WC = TEST.WC$ AND &
		PR_TEMP::STATE = TEST.STATE$ AND &
		PR_TEMP::INS_TYPE = TEST.INS_TYPE$ OR &
		PASS.1% = 0% &

	GOSUB EmpTotal

	GOTO ExitProgram IF UTL_REPORTX::STAT

17060	IF PR_TEMP::WC <> TEST.WC$ OR PR_TEMP::STATE <> TEST.STATE$ OR &
		PR_TEMP::INS_TYPE <> TEST.INS_TYPE$
	THEN
		IF PASS.1%
		THEN
			GOSUB WCTotal
			GOTO ExitProgram IF UTL_REPORTX::STAT
		END IF

		WORK_WC$ = PR_TEMP::WC
	END IF

	IF PR_TEMP::STATE <> TEST.STATE$ OR PR_TEMP::INS_TYPE <> TEST.INS_TYPE$
	THEN
		IF PASS.1%
		THEN
			GOSUB StateTotal
			GOTO ExitProgram IF UTL_REPORTX::STAT
		END IF

		WORK_STATE$ = PR_TEMP::STATE
	END IF

	IF PR_TEMP::INS_TYPE <> TEST.INS_TYPE$
	THEN
		IF PASS.1%
		THEN
			GOSUB InsTotal
			GOTO ExitProgram IF UTL_REPORTX::STAT
		END IF

		WORK_INS_TYPE$ = PR_TEMP::INS_TYPE
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
	TEST.INS_TYPE$ = PR_TEMP::INS_TYPE
	TEST.STATE$ = PR_TEMP::STATE
	TEST.WC$ = PR_TEMP::WC
	TEST.EMPNUM$ = PR_TEMP::EMPNUM
	TEST.PR_END_DATE$ = PR_TEMP::PR_END_DATE
	TEST.METHOD$ = PR_TEMP::METHOD

	PASS.1% = -1%

	EMP_TOTAL(1%) = EMP_TOTAL(1%) + PR_TEMP::REG_HR
	EMP_TOTAL(2%) = EMP_TOTAL(2%) + PR_TEMP::OVT_HR
	EMP_TOTAL(3%) = EMP_TOTAL(3%) + PR_TEMP::GROSS
	EMP_TOTAL(4%) = EMP_TOTAL(4%) + PR_TEMP::SUBJ_EMPLR
	EMP_TOTAL(5%) = EMP_TOTAL(5%) + PR_TEMP::INSURANCE_EMPLR
	EMP_TOTAL(6%) = EMP_TOTAL(6%) + PR_TEMP::SUBJ_EMPLE
	EMP_TOTAL(7%) = EMP_TOTAL(7%) + PR_TEMP::INSURANCE_EMPLE

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
		GOSUB EmpTotal
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	GOSUB WCTotal
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOSUB StateTotal
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOSUB InsTotal
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Print grand total
	!
	GOSUB PrintLine

	TEXT$ = SPACE$(18%) + "          Grand Total               " + &
		FORMAT$(GRAND_TOTAL(1%), "#######.## ") + &
		FORMAT$(GRAND_TOTAL(2%), "#######.## ") + &
		FORMAT$(GRAND_TOTAL(3%), "#######.##       ") + &
		FORMAT$(GRAND_TOTAL(4%), "######.## ") + &
		FORMAT$(GRAND_TOTAL(5%), "######.## ") + &
		FORMAT$(GRAND_TOTAL(6%), "######.## ") + &
		FORMAT$(GRAND_TOTAL(7%), "######.##")

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

 EmpTotal:
17700	!******************************************************************
	! Print employee total
	!******************************************************************
	!
	! Look up employee name
	!

	WORK_EMPNAME$ = STRING$(LEN(PR_EMP_MASTER::EMPNAME), 63%)

	WHEN ERROR IN
		GET #PR_EMP_MASTER.CH%, KEY #0% GE TEST.EMPNUM$, REGARDLESS
	USE
		CONTINUE 17710 IF ERR = 155%
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

	WORK_EMPNUM$ = TEST.EMPNUM$
	WORK_EMPNAME$ = PR_EMP_MASTER::EMPNAME

17710	SUM = 0.0
	SUM = FUNC_ROUND(SUM + EMP_TOTAL(LOOP%), 2%) FOR LOOP% = 1% TO 7%

	IF SUM <> 0.0
	THEN
		TEXT$ = LEFT(WORK_INS_TYPE$ + SPACE$(3%), 3%) + &
			LEFT(WORK_STATE$ + SPACE$(3%), 3%) + &
			LEFT(WORK_WC$ + SPACE$(7%), 7%) + &
			LEFT(WORK_EMPNUM$ + SPACE$(10%), 10%) + " " + &
			LEFT(WORK_EMPNAME$ + SPACE$(18%), 18%) + " " + &
			PRNT_DATE(TEST.PR_END_DATE$, 8%) + " " + &
			FORMAT$(EMP_TOTAL(1%), "#######.## ") + &
			FORMAT$(EMP_TOTAL(2%), "#######.## ") + &
			FORMAT$(EMP_TOTAL(3%), "#######.## ") + &
			"   " + LEFT(TEST.METHOD$ + " ", 1%) + "  " + &
			FORMAT$(EMP_TOTAL(4%), "######.## ") + &
			FORMAT$(EMP_TOTAL(5%), "######.## ") + &
			FORMAT$(EMP_TOTAL(6%), "######.## ") + &
			FORMAT$(EMP_TOTAL(7%), "######.## ")

		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)

		WORK_INS_TYPE$, WORK_STATE$, WORK_WC$, &
			WORK_EMPNUM$, WORK_EMPNAME$ = ""

	END IF

	WC_TOTAL(LOOP%) = WC_TOTAL(LOOP%) + EMP_TOTAL(LOOP%) &
			FOR LOOP% = 1% TO 7%

	EMP_TOTAL(LOOP%) = 0.0 FOR LOOP% = 1% TO 7%

	RETURN

	%PAGE

 WCTotal:
	!******************************************************************
	! Print Total for Account
	!******************************************************************
	SUM = 0.0
	SUM = FUNC_ROUND(SUM + WC_TOTAL(LOOP%), 2%) FOR LOOP% = 1% TO 7%

	IF SUM <> 0.0
	THEN
		GOSUB PrintLine

		TEXT$ = SPACE$(18%) + "    Workmen Comp Total              " + &
			FORMAT$(WC_TOTAL(1%), "#######.## ") + &
			FORMAT$(WC_TOTAL(2%), "#######.## ") + &
			FORMAT$(WC_TOTAL(3%), "#######.##       ") + &
			FORMAT$(WC_TOTAL(4%), "######.## ") + &
			FORMAT$(WC_TOTAL(5%), "######.## ") + &
			FORMAT$(WC_TOTAL(6%), "######.## ") + &
			FORMAT$(WC_TOTAL(7%), "######.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		PRINT_LINE% = -1%
	END IF

	STATE_TOTAL(LOOP%) = STATE_TOTAL(LOOP%) + WC_TOTAL(LOOP%) &
			FOR LOOP% = 1% TO 7%

	WC_TOTAL(LOOP%) = 0.0 FOR LOOP% = 1% TO 7%

	RETURN

	%PAGE

 StateTotal:
	!******************************************************************
	! Print Total for Operation
	!******************************************************************
	SUM = 0.0
	SUM = FUNC_ROUND(SUM + STATE_TOTAL(LOOP%), 2%) FOR LOOP% = 1% TO 7%

	IF SUM <> 0.0
	THEN
		GOSUB PrintLine

		TEXT$ = SPACE$(18%) + "      State Total                   " + &
			FORMAT$(STATE_TOTAL(1%), "#######.## ") + &
			FORMAT$(STATE_TOTAL(2%), "#######.## ") + &
			FORMAT$(STATE_TOTAL(3%), "#######.##       ") + &
			FORMAT$(STATE_TOTAL(4%), "######.## ") + &
			FORMAT$(STATE_TOTAL(5%), "######.## ") + &
			FORMAT$(STATE_TOTAL(6%), "######.## ") + &
			FORMAT$(STATE_TOTAL(7%), "######.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		PRINT_LINE% = -1%
	END IF

	INS_TOTAL(LOOP%) = INS_TOTAL(LOOP%) + STATE_TOTAL(LOOP%) &
		FOR LOOP% = 1% TO 7%

	STATE_TOTAL(LOOP%) = 0.0 FOR LOOP% = 1% TO 7%

	RETURN

	%PAGE

 InsTotal:
	!******************************************************************
	! Print Total for Insurance type
	!******************************************************************
	SUM = 0.0
	SUM = FUNC_ROUND(SUM + INS_TOTAL(LOOP%), 2%) FOR LOOP% = 1% TO 7%

	IF SUM <> 0.0
	THEN
		GOSUB PrintLine

		TEXT$ = SPACE$(18%) + "        Ins Total                   " + &
			FORMAT$(INS_TOTAL(1%), "#######.## ") + &
			FORMAT$(INS_TOTAL(2%), "#######.## ") + &
			FORMAT$(INS_TOTAL(3%), "#######.##       ") + &
			FORMAT$(INS_TOTAL(4%), "######.## ") + &
			FORMAT$(INS_TOTAL(5%), "######.## ") + &
			FORMAT$(INS_TOTAL(6%), "######.## ") + &
			FORMAT$(INS_TOTAL(7%), "######.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		PRINT_LINE% = -1%
	END IF

	GRAND_TOTAL(LOOP%) = GRAND_TOTAL(LOOP%) + INS_TOTAL(LOOP%) &
		FOR LOOP% = 1% TO 7%

	INS_TOTAL(LOOP%) = 0.0 FOR LOOP% = 1% TO 7%

	RETURN

	%PAGE

 PrintLine:
	!*****************************************************************
	! Print underline
	!*****************************************************************

	TEXT$ = SPACE$(54%) + &
		"---------- ---------- ----------       " + &
		"--------- --------- --------- ---------"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3%)

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
