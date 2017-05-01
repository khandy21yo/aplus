1	%TITLE "PR Pay Journal Maintenance"
	%SBTTL "PR_MAIN_TRN_PAY"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PR_MAIN_TRN_PAY(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

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
	!	The ^*Payroll Journal Maintenance\* option allows for the editing or erasing
	!	of the pay records. Pay records may also be entered although they are normally
	!	entered through the Timekeeper option.
	!
	! Index:
	!	.x Payroll Journal>Maintenance
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_MAIN_TRN_PAY/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN PR_MAIN_TRN_PAY
	!	$ DELETE PR_MAIN_TRN_PAY.OBJ;*
	!
	! Author:
	!
	!	11/27/87 - B. Craig Larsen
	!
	! Modification history:
	!
	!	06/01/88 - Lance Williams
	!		Modified to allow R/O open of file if R/W fails.
	!
	!	08/25/89 - Kevin Handy
	!		Modified to allow examining a closed folder.
	!
	!	08/25/89 - Kevin Handy
	!		Modified so that creation of EC$() and PT$()
	!		does not occur every call to this routine.
	!		(Should Speed up program)
	!
	!	10/25/90 - Kevin Handy
	!		Added rate type 'X' (eXcess).
	!
	!	03/18/91 - Kevin Handy
	!		Added eval_date field to pr_read_rate.
	!
	!	05/09/91 - Kevin Handy
	!		Increased the number of digits in the # of pieces
	!		field for Northwest Center.
	!
	!	05/13/91 - Kevin Handy
	!		Forced UPDATE_FLAG to be zero upon entry.
	!
	!	07/13/91 - Kevin Handy
	!		Added "A" Accrual type to pay type list.
	!
	!	04/23/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	05/13/92 - Kevin Handy
	!		Modified Opt_TestEntry routine to remove comments
	!		around CalcGross that someone added in there, which
	!		causes the wrong gross pay to be calculated after
	!		a change.
	!
	!	06/12/92 - Kevin Handy
	!		Clean up (check)
	!
	!	01/18/94 - Kevin Handy
	!		Added equipment number to maintenance.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	07/18/95 - Kevin Handy
	!		Modified to zero out batch number, instead of
	!		leaving it null.
	!
	!	09/10/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/29/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/08/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	03/22/2000 - Kevin Handy
	!		Added EFF_DATE$ parameter ro PR_READ_RATE
	!
	!	11/22/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	08/26/2002 - Kevin Handy
	!		Add field for BATCH_ENTRY
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "FUNC_INCLUDE:PR_WINDOW.INC"

	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.HB"
	MAP (PR_TRN_PAY)	PR_TRN_PAY_CDD	PR_TRN_PAY
	MAP (PR_TRN_PAY_OLD)	PR_TRN_PAY_CDD	PR_TRN_PAY_OLD, PR_TRN_PAY2

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP (PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD		GL_CHART

	%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.HB"
	MAP (PR_ERNDED_DEF)	PR_ERNDED_DEF_CDD	PR_ERNDED_DEF

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PKG.HB"
	MAP (PR_TAX_PKG)	PR_TAX_PKG_CDD	PR_TAX_PKG

	MAP (PR_DETAIL) &
		BATCH_NO$ = 8, &
		CLOSE_FLAG%

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_PR_TRN_PAY) &
		PR_TRN_PAY.CH%, &
		PR_TRN_PAY.READONLY%

	COM (PR_MAIN_TRN_PAY) &
		ECTITLE$ = 16%, &
		EC$(6%) = 16%, &
		PTTITLE$ = 16%, &
		PT$(3%)

	!
	! Create array to contain pointers and totals
	!
	RECORD RARRAY_RECORD
		RFA	LINRFA		! Rfa pointer for record
	END RECORD

	MAP (TT_PR_TRN_PAY) RARRAY_RECORD RARRAY(300%)

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION MAIN_WINDOW
	EXTERNAL LONG    FUNCTION FUNC_TESTENTRY

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	CASE OPT_INIT
		!
		! List of types
		!
		ECTITLE$ = "Type Description"
		EC$(0%) = "5"
		EC$(1%) = "H    Hourly"
		EC$(2%) = "S    Salary"
		EC$(3%) = "P    Piece"
		EC$(4%) = "M    Mileage"
		EC$(5%) = "X    eXcess"
		EC$(6%) = ""

		PTTITLE$ = "Type Description"
		PT$(0%) = "3"
		PT$(1%) = "P    Time/Unit"
		PT$(2%) = "O    Other Payments"
		PT$(3%) = "A    Accrual"

		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Line items"
		SMG_WINDOW::NHELP = "PR_MAIN_TRN_PAY"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 14%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 6%
		SMG_WINDOW::NITEMS= 23%
		IF CLOSE_FLAG%
		THEN
			SMG_WINDOW::FLAGS = 2%
		ELSE
			SMG_WINDOW::FLAGS = 0%
		END IF

		SMG_WINDOW::TOPLIN = 2%
		SMG_WINDOW::BOTLIN = 14%
		SMG_WINDOW::LINREC = 13%

		IF INSTR(1%, " QV", MVALUE) <= 1%
		THEN
			!
			! Load in defaults
			!
			CALL READ_DEFAULTS(SMG_WINDOW)
		END IF

700		!
		! Declare channels
		!
		IF PR_TRN_PAY.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PR_TRN_PAY.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		IF (SMG_WINDOW::FLAGS AND 2%) = 0%
		THEN
			WHEN ERROR IN
				%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.CRE"
			USE
				CONTINUE 760 IF ERR = 10%
				PR_MAIN_TRN_PAY = ERR
				CONTINUE 770
			END WHEN

			PR_TRN_PAY.READONLY% = 0%
			GOTO 790
		END IF

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.OPN"
		USE
			PR_MAIN_TRN_PAY = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PR_TRN_PAY.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PR_TRN_PAY.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = PR_TRN_PAY.CH%
		WHEN ERROR IN
			RESET #PR_TRN_PAY.CH%
			GET #PR_TRN_PAY.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

	!
	! Display the background
	!
20100	CASE OPT_BACKGROUND

	!
	! Extra display stuff
	!
	CASE OPT_DISPLAY

	!
	! Enter/Display/Default
	!
20200	CASE OPT_ENTRY
		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 E0Loop:	SCOPE::SCOPE_EXIT = 0%

		TFLAG% = MFLAG
		TVALUE$ = MVALUE

		SELECT MLOOP
		CASE 14% TO 20%

			IF PR_TRN_PAY::PTYPE = "O"
			THEN
				MFLAG = MFLAG OR 33%
				MFLAG = MFLAG OR NOT 64%
				MVALUE = ""
			END IF

 !		CASE 21%

			!IF PR_TRN_PAY::PTYPE = "P"
			!THEN
			!	MFLAG = MFLAG OR 1%
			!	MFLAG = MFLAG OR NOT 96%
			!END IF

		END SELECT

		SELECT MLOOP

		CASE 1%
	!++
	!
	! Abstract:FLD001
	!	^*(01) Payroll End Date\*
	!	.p
	!	The ^*Payroll End Date\* field refers
	!	to the date designated as a payroll end date. The values in the
	!	Payroll End Date fields of a specific payroll folder will ordinarily
	!	be identical unless more than one check were to be written for a
	!	particular employee, in which case a separate check would be written
	!	for every record or group of records which contained a Payroll End
	!	Date unequal to another record or records related to an individual
	!	employee.
	!
	! Index:
	!	.x End Date>Detail>Pay
	!	.x Detail>Pay>End Date
	!	.x Date>Detail>Pay
	!
	!--

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				"(01) Payroll End Date", &
				SMG_WINDOW::CURLIN, 3%)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				"     Batch: " + PR_TRN_PAY::BATCH + &
				" [" + NUM1$(PR_TRN_PAY::UPDATE_FLAG) + "]", &
				SMG_WINDOW::CURLIN + 1%, 3%)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				"(02) Employee Skill", &
				SMG_WINDOW::CURLIN + 2%, 3%)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				"(03) Employee Grade", &
				SMG_WINDOW::CURLIN + 3%, 3%)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				"(04) Union", &
				SMG_WINDOW::CURLIN + 4%, 3%)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				"(05) Location", &
				SMG_WINDOW::CURLIN + 5%, 3%)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				"(06) Department", &
				SMG_WINDOW::CURLIN + 6%, 3%)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				"(07) Work Center", &
				SMG_WINDOW::CURLIN + 7%, 3%)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				"(08) Account", &
				SMG_WINDOW::CURLIN + 8%, 3%)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				"(09) Sub Account", &
				SMG_WINDOW::CURLIN + 9%, 3%)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				"(10) Operation", &
				SMG_WINDOW::CURLIN + 10%, 3%)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				"(11) Pay Code", &
				SMG_WINDOW::CURLIN + 11%, 3%)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				"(12) Tax Package Code", &
				SMG_WINDOW::CURLIN + 12%, 3%)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				"(13) Pay  Type", &
				SMG_WINDOW::CURLIN, 41%)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				"(14) Rate Type", &
				SMG_WINDOW::CURLIN + 1%, 41%)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				"(15) Hourly Rate", &
				SMG_WINDOW::CURLIN + 3%, 41%)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				"(16) Overtime Factor", &
				SMG_WINDOW::CURLIN + 4%, 41%)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				"(17) Unit Rate", &
				SMG_WINDOW::CURLIN + 5%, 41%)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				"(18) Regular Hours", &
				SMG_WINDOW::CURLIN + 7%, 41%)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				"(19) Overtime Hours", &
				SMG_WINDOW::CURLIN + 8%, 41%)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				"(20) Number of Units", &
				SMG_WINDOW::CURLIN + 9%, 41%)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				"(21) Gross", &
				SMG_WINDOW::CURLIN + 10%, 41%)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				"(22) Equipment", &
				SMG_WINDOW::CURLIN + 11%, 41%)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				"(23) Entry Batch", &
				SMG_WINDOW::CURLIN + 12%, 41%)

			PR_TRN_PAY::PR_END_DATE = BATCH_NO$ &
				IF PR_TRN_PAY::PR_END_DATE = ""
			XLINE$ = NUM1$(SMG_WINDOW::CURLIN)
			PR_TRN_PAY::PR_END_DATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";26", TEMP$, &
				PR_TRN_PAY::PR_END_DATE, MFLAG, "8", MVALUE)

		CASE 2%
	!++
	!
	! Abstract:FLD002
	!	^*(02) Employee Skill\*
	!	.p
	!	The ^*Employee Skill\* field refers
	!	to the code for a specific skill, i.e. electrician, iron worker, etc.
	!	as related to the specific record.
	!
	! Index:
	!	.x Skill>Detail>Pay
	!	.x Detail>Pay>Skill
	!
	!--
			XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 2%)
			PR_TRN_PAY::EMP_SKILL = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";26", TEMP$, &
				PR_TRN_PAY::EMP_SKILL, MFLAG, "'E", MVALUE)

		CASE 3%
	!++
	!
	! Abstract:FLD003
	!	^*(03) Employee Grade\*
	!	.p
	!	The ^*Employee Grade\* field refers
	!	to the employee's specific grade pertaining to the record, i.e.,
	!	apprentice or journeyman.
	!
	! Index:
	!	.x Grade>Detail>Pay
	!	.x Detail>Pay>Grade
	!
	!--
			XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 3%)
			PR_TRN_PAY::EMP_GRADE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";26", TEMP$, &
				PR_TRN_PAY::EMP_GRADE, MFLAG, "'E", MVALUE)

		CASE 4%
	!++
	!
	! Abstract:FLD004
	!	^* (04) Union\*
	!	.p
	!	The ^*Union\* field refers to the
	!	union code to which an employee belongs as relates to a specific
	!	record.
	!
	! Index:
	!	.x Union>Detail>Pay
	!	.x Detail>Pay>Union
	!
	!--
			XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 4%)
			PR_TRN_PAY::UNION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";26", TEMP$, &
				PR_TRN_PAY::UNION, MFLAG, "'E", MVALUE)

		CASE 5%
	!++
	!
	! Abstract:FLD005
	!	^*(05) Location\*
	!	.p
	!	The ^*Location\* field refers to
	!	a location code indicating where an employee is assigned as
	!	relates to a specific record.
	!
	! Index:
	!	.x Location>Detail>Pay
	!	.x Detail>Pay>Location
	!
	!--
			XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 5%)
			PR_TRN_PAY::LOCATION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";26", TEMP$, &
				PR_TRN_PAY::LOCATION, MFLAG, "'E", MVALUE)

		CASE 6%
	!++
	!
	! Abstract:FLD006
	!	^*(06) Department\*
	!	.p
	!	The ^*Department\* field refers to the
	!	department code relative to the specific record.
	!
	! Index:
	!	.x Department>Detail>Pay
	!	.x Detail>Pay>Department
	!
	!--
			XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 6%)
			PR_TRN_PAY::DEPT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";26", TEMP$, &
				PR_TRN_PAY::DEPT, MFLAG, "'E", MVALUE)

		CASE 7%
	!++
	!
	! Abstract:FLD007
	!	^*(07) Work Center\*
	!	.p
	!	The ^*Work Center\* field refers to
	!	a work center code which would be applicable to the specific record.
	!
	! Index:
	!	.x Work Center>Detail>Pay
	!	.x Detail>Pay>Work Center
	!
	!--
			XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 7%)
			PR_TRN_PAY::WORK_CENTER = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";26", TEMP$, &
				PR_TRN_PAY::WORK_CENTER, MFLAG, "'E", MVALUE)

		CASE 8%
	!++
	!
	! Abstract:FLD008
	!	^*(08) Account\*
	!	.p
	!	The ^*Account\* field refers to the
	!	General Ledger account number which relates to the specific record.
	!
	! Index:
	!	.x Account>Detail>Pay
	!	.x Detail>Pay>Account
	!
	!--

 E0Loop1:		XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 8%)
			PR_TRN_PAY::ACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";20", TEMP$, &
				PR_TRN_PAY::ACCT, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX ") = 1%)
				THEN
					PR_TRN_PAY::ACCT = GL_CHART::ACCT
				END IF

				SCOPE::SCOPE_EXIT = 0%
				GOTO E0Loop1
			END IF

		CASE 9%
	!++
	!
	! Abstract:FLD009
	!	^*(09) Sub Account\*
	!	.p
	!	The ^*Sub Account\* field refers to
	!	the sub-account related to the specific record.
	!
	! Index:
	!	.x Sub Account>Detail>Pay
	!	.x Detail>Pay>Sub Account
	!
	!--
			XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 9%)
			PR_TRN_PAY::SUBACC = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";26", TEMP$, &
				PR_TRN_PAY::SUBACC, MFLAG, "'E", MVALUE)

		CASE 10%
	!++
	!
	! Abstract:FLD010
	!	^*(10) Operation\*
	!	.p
	!	The ^*Operation\* field refers to
	!	the code for the operation or task related to the specific record.
	!
	! Index:
	!	.x Operation>Detail>Pay
	!	.x Detail>Pay>Operation
	!
	!--
			XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 10%)
			PR_TRN_PAY::OPER = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";26", TEMP$, &
				PR_TRN_PAY::OPER, MFLAG, "'E", MVALUE)

		CASE 11%
	!++
	!
	! Abstract:FLD011
	!	^*(11) Pay Code\*
	!	.p
	!	The ^*Pay Code\* field refers to the
	!	specific type or kind of earnings represented in the record, i.e.
	!	a code for regular taxable earnings, bonuses, commission, etc.
	!
	! Index:
	!	.x Pay Code>Detail>Pay
	!	.x Detail>Pay>Pay Code
	!
	!--

 E0Loop2:		XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 11%)
			PR_TRN_PAY::CODE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";26", TEMP$, &
				PR_TRN_PAY::CODE, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(PR_MAIN_ERNDED_DEF.ID, "V0P") = 1%)
				THEN
					PR_TRN_PAY::CODE = &
						PR_ERNDED_DEF::CODE
				END IF

				SCOPE::SCOPE_EXIT = 0%
				GOTO E0Loop2
			END IF

		CASE 12%
	!++
	!
	! Abstract:FLD012
	!	^*(12) Tax Package Code\*
	!	.p
	!	The ^*Tax Package Code\* field refers
	!	to the specific Tax Package to which the earnings on the record are
	!	subject.
	!
	! Index:
	!	.x Tax Package>Detail>Pay
	!	.x Detail>Pay>Tax Package
	!
	!--

 E0Loop3:		XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 12%)
			PR_TRN_PAY::TAX_PKG = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";26", TEMP$, &
				PR_TRN_PAY::TAX_PKG, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(PR_MAIN_TAX_PKG.ID, "V0 ") = 1%)
				THEN
					PR_TRN_PAY::TAX_PKG = &
						PR_TAX_PKG::TAX_PKG
				END IF

				SCOPE::SCOPE_EXIT = 0%
				GOTO E0Loop3
			END IF

		CASE 13%
	!++
	!
	! Abstract:FLD013
	!	^*(13) Pay Type\*
	!	.p
	!	The ^*Pay Type\* field refers to the
	!	type or kind of payment which is being made to an employee in
	!	reference to a particular record. Valid types are:
	!	.b
	!	.lm +15
	!	.ls 0,"o"
	!	.LE
	!	P = Time or Units Related Payments
	!	.le
	!	O = Other Payments
	!	.ELS
	!	.lm -15
	!	.p
	!	The ^*P\* type payments would generally be considered earnings which
	!	are calculated by multiplying a rate by a factor such as hours, days,
	!	weeks, months or other periods worked; pieces produced; or miles
	!	driven, etc.
	!	.p
	!	The ^*O\* type payments could be earnings or non-earnings payments
	!	which may not be the result of as simple a calculation. Examples
	!	could be earnings such as bonuses or commissions, or non-earnings
	!	payments such as mileage and travel expense reimbursements, travel
	!	advances, etc.
	!
	! Index:
	!	.x Pay Type>Detail>Pay
	!	.x Detail>Pay>Pay Type
	!
	!--
			XLINE$ = NUM1$(SMG_WINDOW::CURLIN)
			PR_TRN_PAY::PTYPE = EDIT$(ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";68", TEMP$, &
				PR_TRN_PAY::PTYPE, MFLAG, "'", MVALUE, &
				PT$(), PTTITLE$, "005"), -1%)

		CASE 14%
	!++
	!
	! Abstract:FLD014
	!	^*(14) Rate Type\*
	!	.p
	!	The ^*Rate Type\* field refers to the
	!	type of rate for which an employee is being paid.  Valid rate types
	!	can be viewed by pressing ^*<List Choices>\* and are:
	!	.b
	!	.lm +5
	!	.ls 0,"o"
	!	.LE
	!	H = Hourly Rate (or time related rate)
	!	.LE
	!	S = Salary
	!	.LE
	!	P = Piece Rate
	!	.LE
	!	M = Mileage
	!	.ELS
	!	.lm -5
	!
	! Index:
	!	.x Rate Type>Detail>Pay
	!	.x Detail>Pay>Rate Type
	!
	!--
			XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 1%)
			PR_TRN_PAY::RTYPE = EDIT$(ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";68", &
				TEMP$, PR_TRN_PAY::RTYPE, MFLAG, "'", &
				MVALUE, EC$(), ECTITLE$, "005"), -1%)

		CASE 15%
	!++
	!
	! Abstract:FLD015
	!	^*(15) Hourly Rate\*
	!	.p
	!	The ^*Hourly Rate\* field refers to the
	!	appropriate hourly rate related to a specific record.
	!
	! Index:
	!	.x Hourly Rate>Detail>Pay
	!	.x Rate>Detail>Pay
	!	.x Detail>Pay>Hourly Rate
	!
	!--
			XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 3%)
			PR_TRN_PAY::HOUR_RATE = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";68", &
				TEMP$, PR_TRN_PAY::HOUR_RATE * 1.0, &
				MFLAG, "##,###.###", MVALUE)

		CASE 16%
	!++
	!
	! Abstract:FLD016
	!	^*(16) Overtime Factor\*
	!	.p
	!	The ^*Overtime Factor\* field refers to
	!	an overtime hourly pay rate expressed as a percentage of an hourly
	!	rate.  Hence, an overtime rate verbally expressed as "time and a half"
	!	is expressed in this field as "150".
	!
	! Index:
	!	.x Overtime Factor>Detail>Pay
	!	.x Detail>Pay>Overtime Factor
	!
	!--
			XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 4%)
			PR_TRN_PAY::FACTOR = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";75", &
				TEMP$, PR_TRN_PAY::FACTOR * 1.0, &
				MFLAG, "###", MVALUE)

		CASE 17%
	!++
	!
	! Abstract:FLD017
	!	^*(17) Unit Rate\*
	!	.p
	!	The ^*Unit Rate\* field refers to the
	!	appropriate unit (piece or mile) rate which relates to a specific
	!	record.
	!
	! Index:
	!	.x Unit Rate>Detail>Pay
	!	.x Rate>Detail>Pay
	!	.x Detail>Pay>Unit Rate
	!
	!--
			XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 5%)
			PR_TRN_PAY::PIECE_RATE = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";68", &
				TEMP$, PR_TRN_PAY::PIECE_RATE * 1.0, &
				MFLAG, "#,###.####", MVALUE)

		CASE 18%
	!++
	!
	! Abstract:FLD018
	!	^*(18) Regular Hours\*
	!	.p
	!	The ^*Regular Hours\* field refers to
	!	the number of hours (or other unit of time) which an employee worked
	!	or is to be credited.  The number of regular hours is exclusive of
	!	overtime hours.
	!	.p
	!	The field will accommodate an entry to the nearest 1/100th of an
	!	hour.
	!	.note
	!	The value in this field may refer to days, weeks,
	!	months or any other time period unit.
	!	.END NOTE
	!
	! Index:
	!	.x Hours>Detail>Pay
	!	.x Regular Hours>Detail>Pay
	!	.x Detail>Pay>Regular Hours
	!
	!--
			XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 7%)
			PR_TRN_PAY::REG_HR = ENTR_3TIMEKEEPER(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";70", TEMP$, &
				PR_TRN_PAY::REG_HR * 1.0, MFLAG, "#,###.##", &
				MVALUE)

		CASE 19%
	!++
	!
	! Abstract:FLD019
	!	^*(19) Overtime Hours\*
	!	.p
	!	The ^*Overtime Hours\* field refers to
	!	the number of overtime hours worked by an employee in reference to
	!	the specific record.
	!	.p
	!	The field will accommodate an entry to the nearest 1/100th of
	!	an hour.
	!
	! Index:
	!	.x Overtime Hours>Detail>Pay
	!	.x Detail>Pay>Overtime Hours
	!
	!--
			XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 8%)
			PR_TRN_PAY::OVT_HR = ENTR_3TIMEKEEPER(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";70", TEMP$, &
				PR_TRN_PAY::OVT_HR * 1.0, MFLAG, "#,###.##", &
				MVALUE)

		CASE 20%
	!++
	!
	! Abstract:FLD020
	!	^*(20) Number of Units\*
	!	.p
	!	The ^*Number of Units\* refers to pieces
	!	produced or miles driven, etc. for which an employee is being credited.
	!	.p
	!	The field will accommodate a value to the nearest 1/100th of a
	!	unit.
	!
	! Index:
	!	.x Units>Detail>Pay
	!	.x Detail>Pay>Units
	!
	!--
			XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 9%)
			PR_TRN_PAY::PIECE = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";68", TEMP$, &
				PR_TRN_PAY::PIECE * 1.0, MFLAG, &
				"###,###.##", MVALUE)

		CASE 21%
	!++
	!
	! Abstract:FLD021
	!
	!	^*(21) Gross\*
	!	.p
	!	This calculated field displays the gross pay due to the employee
	!	by calculation.
	!
	! Index:
	!	.x Gross Pay>Detail>Pay
	!	.x Pay>Detail>Pay
	!	.x Detail>Pay>Gross Pay
	!--
			IF PR_TRN_PAY::PTYPE = "P"
			THEN
				GOSUB CalcGross
			END IF

			XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 10%)
			PR_TRN_PAY::GROSS = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";64", TEMP$, &
				PR_TRN_PAY::GROSS * 1.0, MFLAG, &
				"###,###,###.##", MVALUE)

		CASE 22%
	!++
	!
	! Abstract:FLD022
	!
	!	^*(22) Equipment Number\*
	!--
			XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 11%)
			PR_TRN_PAY::EQUIPMENT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";64", TEMP$, &
				PR_TRN_PAY::EQUIPMENT, MFLAG, &
				"'E", MVALUE)

		CASE 23%
	!++
	!
	! Abstract:FLD023
	!	^*(23) Entry Batch\*
	!	.b
	!	The two character batch number that was entered on the
	!	first screen of the time units entry program when this
	!	batch was being entered.
	!
	! Index:
	!
	!--
			XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 12%)
			PR_TRN_PAY::BATCH_ENTRY = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";64", TEMP$, &
				PR_TRN_PAY::BATCH_ENTRY, MFLAG, &
				"'E", MVALUE)

		END SELECT

		MFLAG = TFLAG%
		MVALUE = TVALUE$
		SCOPE::PRG_ITEM= TEMP1$


	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		PR_MAIN_TRN_PAY = 0%

		TEMP% = SCOPE::SCOPE_EXIT

		SELECT MLOOP

		CASE 1%
			IF EDIT$(MVALUE, -1%) = "ADD"
			THEN
				PR_TRN_PAY::EMP_SKILL	= PR_EMP_MASTER::EMP_SKILL
				PR_TRN_PAY::EMP_GRADE	= PR_EMP_MASTER::EMP_GRADE
				PR_TRN_PAY::UNION	= PR_EMP_MASTER::UNION
				PR_TRN_PAY::LOCATION	= PR_EMP_MASTER::LOCATION
				PR_TRN_PAY::DEPT	= PR_EMP_MASTER::DEPT
				PR_TRN_PAY::WORK_CENTER	= PR_EMP_MASTER::WORK_CENTER
				PR_TRN_PAY::ACCT	= PR_EMP_MASTER::ACCT
				PR_TRN_PAY::SUBACC	= PR_EMP_MASTER::SUBACC
				PR_TRN_PAY::OPER	= PR_EMP_MASTER::OPER
				PR_TRN_PAY::TAX_PKG	= PR_EMP_MASTER::TAX_PKG
				PR_TRN_PAY::PTYPE	= "P"
				PR_TRN_PAY::EQUIPMENT	= ""
			END IF

		CASE 8%
			!
			! Is the input defined?
			!
			PR_MAIN_TRN_PAY = FUNC_TESTENTRY(SMG_WINDOW, &
				PR_TRN_PAY::ACCT, &
				GL_CHART::DESCR, &
				"PR", MLOOP, "PRG", &
				"Account", GL_MAIN_CHART.ID)

		CASE 10%
			!
			! Let's look up the employee rate file if
			! this is an add function
			!
			IF EDIT$(SCOPE::PRG_ITEM, -1%) = "ADD"
			THEN
				CALL PR_READ_RATE(PR_TRN_PAY::EMPNUM, &
					PR_TRN_PAY::OPER, &
					PR_TRN_PAY::PR_END_DATE, &
					PR_TRN_PAY::RTYPE, &
					PR_TRN_PAY::CODE, &
					PR_TRN_PAY::HOUR_RATE, &
					PR_TRN_PAY::PIECE_RATE, &
					PR_TRN_PAY::FACTOR, &
					STDEFF, &
					EVALDATE$, &
					EFF_DATE$)

				!
				! Set rate type and code if null
				!
				IF PR_TRN_PAY::RTYPE = "" AND PR_TRN_PAY::CODE = ""
				THEN
					PR_TRN_PAY::RTYPE = PR_EMP_MASTER::RATE_TYPE
					PR_TRN_PAY::CODE = PR_EMP_MASTER::RATE_CDE
				END IF

				!
				! Now let's display the default rate data
				! on the screen
				!
				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					PR_TRN_PAY::CODE, &
					SMG_WINDOW::CURLIN + 11%, 26%, , &
					SMG$M_BOLD)

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					PR_TRN_PAY::RTYPE, &
					SMG_WINDOW::CURLIN + 1%, 68%, , &
					SMG$M_BOLD)

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					FORMAT$(PR_TRN_PAY::HOUR_RATE, &
					"##,###.###"), &
					SMG_WINDOW::CURLIN + 3%, 68%, , &
					SMG$M_BOLD)

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					FORMAT$(PR_TRN_PAY::FACTOR, "###"), &
					SMG_WINDOW::CURLIN + 4%, 75%, , &
					SMG$M_BOLD)

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					FORMAT$(PR_TRN_PAY::PIECE_RATE, &
					"#,###.####"), &
					SMG_WINDOW::CURLIN + 5%, 68%, , &
					SMG$M_BOLD)

			END IF

		CASE 11%
			PR_MAIN_TRN_PAY = FUNC_TESTENTRY(SMG_WINDOW, &
				"P" + PR_TRN_PAY::CODE, &
				PR_ERNDED_DEF::DESCR, &
				"PR", MLOOP, "PRG", &
				"Code", PR_MAIN_ERNDED_DEF.ID)

		CASE 12%
			PR_MAIN_TRN_PAY = FUNC_TESTENTRY(SMG_WINDOW, &
				PR_TRN_PAY::TAX_PKG, &
				PR_TAX_PKG::TAX_PKG, &
				"PR", MLOOP, "PRG", &
				"Code", PR_MAIN_TAX_PKG.ID)

		CASE 13% TO 21%

			IF PR_TRN_PAY::PTYPE = "O"
			THEN
				TFLAG% = 33%	! Blank these fields

				XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 1%)

				PR_TRN_PAY::RTYPE = EDIT$(ENTR_3STRINGLIST(SCOPE, &
					SMG_WINDOW::WNUMBER, XLINE$ + ";68", &
					TEMP$, PR_TRN_PAY::RTYPE, TFLAG%, "'", &
					"", EC$(), ECTITLE$, "005"), -1%)

				XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 3%)

				PR_TRN_PAY::HOUR_RATE = ENTR_3NUMBER(SCOPE, &
					SMG_WINDOW::WNUMBER, XLINE$ + ";68", &
					TEMP$, PR_TRN_PAY::HOUR_RATE * 1.0, &
					TFLAG%, "##,###.###", "")

				XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 4%)

				PR_TRN_PAY::FACTOR = ENTR_3NUMBER(SCOPE, &
					SMG_WINDOW::WNUMBER, XLINE$ + ";75", &
					TEMP$, PR_TRN_PAY::FACTOR * 1.0, &
					TFLAG%, "###", "")

				XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 5%)

				PR_TRN_PAY::PIECE_RATE = ENTR_3NUMBER(SCOPE, &
					SMG_WINDOW::WNUMBER, XLINE$ + ";68", &
					TEMP$, PR_TRN_PAY::PIECE_RATE * 1.0, &
					TFLAG%, "#,###.####", "")

				XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 7%)

				PR_TRN_PAY::REG_HR = ENTR_3NUMBER(SCOPE, &
					SMG_WINDOW::WNUMBER, XLINE$ + ";70", TEMP$, &
					PR_TRN_PAY::REG_HR * 1.0, TFLAG%, "#,###.##", &
					"")

				XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 8%)

				PR_TRN_PAY::OVT_HR = ENTR_3NUMBER(SCOPE, &
					SMG_WINDOW::WNUMBER, XLINE$ + ";70", TEMP$, &
					PR_TRN_PAY::OVT_HR * 1.0, TFLAG%, "#,###.##", &
					"")

				XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 9%)

				PR_TRN_PAY::PIECE = ENTR_3NUMBER(SCOPE, &
					SMG_WINDOW::WNUMBER, XLINE$ + ";70", TEMP$, &
					PR_TRN_PAY::PIECE * 1.0, TFLAG%, &
					"#,###.##", "")

			END IF

			IF PR_TRN_PAY::PTYPE = "P"
			THEN
				GOSUB CalcGross

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					FORMAT$(PR_TRN_PAY::GROSS, "###,###,###.##"), &
					SMG_WINDOW::CURLIN + 11%, 64%,, SMG$M_BOLD)

			END IF

		END SELECT

		SCOPE::SCOPE_EXIT = TEMP%

	!
	! Test option
	!
	CASE OPT_TESTOPT
		FUNC_FLAG% = 0%

		IF EDIT$(SCOPE::PRG_ITEM, -1%) = "ERASE" OR EDIT$(SCOPE::PRG_ITEM, -1%) = "CHANGE" &
				OR EDIT$(SCOPE::PRG_ITEM, -1%) = "INITIALIZE" &
				OR EDIT$(SCOPE::PRG_ITEM, -1%) = "BLANK"
		THEN
			!***************************************************
			! Test to see if payroll has been closed or posted
			!	1 - Updated to Register
			!	2 - Accrued Post
			!	4 - Final Post
			!***************************************************

			IF (PR_TRN_PAY::UPDATE_FLAG AND 1%)
			THEN
				CALL HELP_3MESSAGE(SCOPE, "PR Folder is Closed", "ERR", "PR_CLOSED", &
						"ERROR_PR_CLOSED")
				FUNC_FLAG% = 2%
			END IF

			IF (PR_TRN_PAY::UPDATE_FLAG AND 2%) AND FUNC_FLAG% = 0%
			THEN
				CALL HELP_3MESSAGE(SCOPE, "PR Folder is Accrued", "ERR", "PR_ACCRUED", &
						"ERROR_PR_ACCRUED")
				FUNC_FLAG% = 2%
			END IF

			IF (PR_TRN_PAY::UPDATE_FLAG AND 4%) AND FUNC_FLAG% = 0%
			THEN
				CALL HELP_3MESSAGE(SCOPE, "PR Folder has been Posted", "ERR", "PR_POSTED", &
					"ERROR_PR_POSTED")
				FUNC_FLAG% = 2%
			END IF
		END IF

		PR_MAIN_TRN_PAY = FUNC_FLAG%

	!
	! Set PR_TRN_PAY_OLD value
	!
20500	CASE OPT_SETOLD
		PR_TRN_PAY_OLD = PR_TRN_PAY

	!
	! Restore PR_TRN_PAY_OLD value
	!
	CASE OPT_RESETOLD
		PR_TRN_PAY = PR_TRN_PAY_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PR_TRN_PAY::UPDATE_FLAG = 0%
		PR_TRN_PAY::BATCH = ""
		PR_TRN_PAY2 = PR_TRN_PAY

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PR_TRN_PAY = PR_TRN_PAY2

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!
		PR_TRN_PAY::EMPNUM = PR_EMP_MASTER::EMPNUM


	!
	! Find
	!
	CASE OPT_FIND
		FIND #PR_TRN_PAY.CH%, KEY #0% GE PR_TRN_PAY::EMPNUM + "", &
			REGARDLESS

	!
	! Handle array of records
	!
27000	CASE OPT_ARRAY

		!
		! Select sub-option of array
		!
		SELECT MLOOP

		!
		! Load array with line items
		!
		CASE 1%

			!
			! Empty array
			!
			SMG_WINDOW::TOTREC = 0%

27110			!
			! Search for first record
			!
			WHEN ERROR IN
				FIND #SMG_WINDOW::CHAN, &
					KEY #0% GE PR_EMP_MASTER::EMPNUM + "", &
					REGARDLESS
			USE
				CONTINUE 28000
			END WHEN

27120			!
			! Get a record
			!
			WHEN ERROR IN
				GET #SMG_WINDOW::CHAN
			USE
				CONTINUE 28000 IF ERR = 11%
				EXIT HANDLER
			END WHEN

			IF (PR_TRN_PAY::EMPNUM = PR_EMP_MASTER::EMPNUM)
			THEN
				!
				! Add information to array
				!
				SMG_WINDOW::TOTREC = SMG_WINDOW::TOTREC + 1%
				RARRAY(SMG_WINDOW::TOTREC)::LINRFA = &
					GETRFA(SMG_WINDOW::CHAN)
				GOTO 27120
			END IF

		!
		! Remove one element of the array
		!
		CASE 2%
			!
			! Remove item pointed to by Mflag
			!
			RARRAY(I%) = RARRAY(I% + 1%) &
				FOR I% = MFLAG TO SMG_WINDOW::TOTREC - 1%

		!
		! Set array item to current record
		!
		CASE 3%
			RARRAY(MFLAG)::LINRFA = GETRFA(SMG_WINDOW::CHAN)

		!
		! Load in current record, locked
		!
		CASE 4%
27200			GET #SMG_WINDOW::CHAN, RFA RARRAY(MFLAG)::LINRFA

		!
		! Load in current record, unlocked
		!
		CASE 5%
			GET #SMG_WINDOW::CHAN, RFA RARRAY(MFLAG)::LINRFA, &
				REGARDLESS

		!
		! Change the current record's key to match header.  The
		! new key is probibly passes through MVALUE, unless some
		! other means is devised.
		!
		CASE 6%
			PR_TRN_PAY::EMPNUM = MID(MVALUE, 2%, 10%)

		END SELECT
	END SELECT

28000	EXIT FUNCTION

	%PAGE

 CalcGross:
	!*******************************************************************
	! Calculate gross earnings
	!*******************************************************************

	SELECT PR_TRN_PAY::RTYPE

	CASE "H", "S"
		PR_TRN_PAY::GROSS = FUNC_ROUND( &
			(PR_TRN_PAY::REG_HR + &
			(PR_TRN_PAY::OVT_HR * &
			(PR_TRN_PAY::FACTOR / 100.))) * &
			PR_TRN_PAY::HOUR_RATE, 2%)

	CASE "X"
		TEMP1 = FUNC_ROUND( &
			(PR_TRN_PAY::REG_HR + &
			(PR_TRN_PAY::OVT_HR * &
			(PR_TRN_PAY::FACTOR / 100.))) * &
			PR_TRN_PAY::HOUR_RATE, 2%)
		TEMP2 = FUNC_ROUND( &
			PR_TRN_PAY::PIECE * &
			PR_TRN_PAY::PIECE_RATE, 2%)

		IF TEMP1 > TEMP2
		THEN
			PR_TRN_PAY::GROSS = TEMP1
		ELSE
			PR_TRN_PAY::GROSS = TEMP2
		END IF

	CASE ELSE
		PR_TRN_PAY::GROSS = FUNC_ROUND( &
			PR_TRN_PAY::PIECE * &
			PR_TRN_PAY::PIECE_RATE, 2%)

	END SELECT

	RETURN

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
