1	%TITLE "PR Pay Time Keeper Journal Maintenance"
	%SBTTL "PR_MAIN_TRN_TK02_PAY"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PR_MAIN_TRN_TK02_PAY(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	The ^*Timekeeper\* option
	!	accesses the Pay Journal, Deduction Journal and Check Journal.
	!	These journals provide the means to enter the employees' time worked,
	!	units produced, non-standard deductions and after-the-fact manual
	!	pay-off data.
	!
	! Index:
	!	.x Timekeeper
	!	.x Payroll Journal>Timekeeper
	!
	! Option:
	!
	!	PR_MAIN_TRN_TK02_PAY$DEDUCTION
	!	PR_MAIN_TRN_TK02_PAY$CHECK
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_MAIN_TRN_TK02_PAY/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN PR_MAIN_TRN_TK02_PAY
	!	$ DELETE PR_MAIN_TRN_TK02_PAY.OBJ;*
	!
	! Author:
	!
	!	01/05/88 - B. Craig Larsen
	!
	! Modification history:
	!
	!	06/01/88 - Lance Williams
	!		Modified to allow R/O open of file if R/W fails.
	!
	!	01/11/90 - Kevin Handy
	!		Moved initilization of List Choices arrays into
	!		into init section to decrease CPU time used for
	!		entry.
	!
	!	04/03/90 - Kevin Handy
	!		Added windows into Deduction and Check files.
	!
	!	10/25/90 - Kevin Handy
	!		Added pay type "X" (eXcess).
	!
	!	03/18/91 - Kevin Handy
	!		Added eval_date field to pr_read_rate.
	!
	!	12/18/91 - Kevin Handy
	!		Added code for "A" Accrual.
	!
	!	04/23/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	06/12/92 - Kevin Handy
	!		Clean up (check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	07/18/95 - Kevin Handy
	!		Initialize batch number.
	!
	!	09/10/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/20/98 - Kevin Handy
	!		Lose unused SLINE field in array, not used.
	!
	!	06/28/99 - Kevin Handy
	!		Use ENTR_3TIMEKEEPER function
	!
	!	03/22/2000 - Kevin Handy
	!		Add EFF_DATE parameter to PR_READ_RATE
	!
	!	11/22/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	04/13/2001 - Kevin Handy
	!		Put folder date in title bar
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

	MAP (PR_DETAIL) &
		BATCH_NO$ = 8, &
		END_DATE$ = 8, &
		BATCH_ENTRY$ = 2%, &
		LOCATION$ = 4%, &
		PTTITLE$ = 26%, &
		PT$(4%) = 26%

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_PR_TRN_PAY) &
		PR_TRN_PAY.CH%, &
		PR_TRN_PAY.READONLY%

	!
	! Create array to contain pointers and totals
	!
	RECORD RARRAY_RECORD
		RFA	LINRFA		! Rfa pointer for record
		REAL	REG_HR		! Total Regular time
		REAL	OVT_HR		! Total Overtime
		REAL	PIECE		! Total piece count
		REAL	GROSS		! Total gross pay
	END RECORD

	MAP (TT_PR_TRN_PAY) RARRAY_RECORD RARRAY(2000%)	! Allocate for 2000

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION MAIN_WINDOW
	EXTERNAL LONG    FUNCTION MAIN_JOURNAL
	EXTERNAL LONG    FUNCTION FUNC_TESTENTRY

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Pay Journal 02 " + &
			PRNT_DATE(BATCH_NO$, 8%)
		SMG_WINDOW::NHELP = "PR_MAIN_TRN_TK02_PAY"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 10%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::TOPLIN = 3%
		SMG_WINDOW::BOTLIN = 17%
		SMG_WINDOW::LINREC = 3%

		PTTITLE$ = "Type Description"
		PT$(0%) = "3"
		PT$(1%) = "P    Time/Unit"
		PT$(2%) = "O    Other Payments"
		PT$(3%) = "A    Accrual"

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
		! Initialize the total records only when file is opened.
		!
		SMG_WINDOW::TOTREC = 0%

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PR_MAIN_TRN_TK02_PAY = ERR
			CONTINUE 770
		END WHEN

		PR_TRN_PAY.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.OPN"
		USE
			PR_MAIN_TRN_TK02_PAY = ERR
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

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)


		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"  01     02       03              04 05       " + &
			"06       07   08     09       10 ", &
			1%, 1%,, SMG$M_REVERSE)
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"  Emp  # Oper Account             PT Cd " + &
			"Hr  Rate Reg  Hrs  OTF OT Hrs    Gross", &
			2%, 1%,, SMG$M_REVERSE)

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

	!
	! Modify the option list
	!
	CASE OPT_OPTLIST

		MVALUE = MVALUE + " dedUction checK"

	!
	! More menu items
	!
	CASE OPT_MOREMENU

		SELECT MVALUE
		CASE "dedUction"

			PR_MAIN_TRN_TK02_PAY = MAIN_JOURNAL(PR_MAIN_TRN_TK02_DED.ID, "")
	!++
	! Abstract:DEDUCTION
	!	^*Deduction\*
	!	.p
	!	The ^*Deduction\* worksheet allows for the input and manipulation of data
	!	concerning the deductions of each employee.
	!
	! Index:
	!	.x Time keeper>Deduction
	!	.x Deduction>Time keeper
	!
	!--

		CASE "checK"

			PR_MAIN_TRN_TK02_PAY = MAIN_JOURNAL(PR_MAIN_TRN_TK02_CHECK.ID, "")
	!++
	! Abstract:CHECK
	!	^*Check\*
	!	.p
	!	The ^*Check\* journal provides the information concerning the checks for
	!	each employee. The fields include: Employee Name, Employee Number, Check
	!	Number, and Check Date.
	!
	! Index:
	!	.x Time keeper>Check
	!	.x Check>Time keeper
	!
	!--

		END SELECT

	!
	! Extra display stuff
	!
	CASE OPT_DISPLAY

		!
		! Generate totals
		!
		TOTAL_REG_HR = 0.0
		TOTAL_OVT_HR = 0.0
		TOTAL_GROSS = 0.0

		FOR I% = 1% TO SMG_WINDOW::TOTREC
			TOTAL_REG_HR = TOTAL_REG_HR + RARRAY(I%)::REG_HR
			TOTAL_OVT_HR = TOTAL_OVT_HR + RARRAY(I%)::OVT_HR
			TOTAL_GROSS = TOTAL_GROSS + RARRAY(I%)::GROSS
		NEXT I%

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		!
		! Display totals
		!
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"Number of lines" + &
			FORMAT$(SMG_WINDOW::TOTREC, "####") + &
			SPACE$(23%) + "TOTAL" + &
			FORMAT$(TOTAL_REG_HR, "#######.##  ") + &
			FORMAT$(TOTAL_OVT_HR, "#######.##") + &
			FORMAT$(TOTAL_GROSS, "######.##"), &
			SMG_WINDOW::VSIZE, 1%, , SMG$M_REVERSE)

		!
		! Paint lines on screen
		!
		FOR I% = 1% TO 7%
			A% = VAL%(MID("034,037,040,049,058,063,070", &
				I% * 4% - 3%, 3%))

			SMG_STATUS% = SMG$DRAW_LINE(SMG_WINDOW::WNUMBER, &
				1%, A%, SMG_WINDOW::BOTLIN, A%)
		NEXT I%

		!
		! Draw horizontal lines
		!
		SMG_STATUS% = SMG$DRAW_LINE(SMG_WINDOW::WNUMBER, &
			SMG_WINDOW::CURLIN + 2%, 1%, &
			SMG_WINDOW::CURLIN + 2%, 78%)

		!
		! Print the % sign for the Over Time Factor
		!
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"%", SMG_WINDOW::CURLIN + 1%, 62%,, SMG$M_BOLD)

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

	!
	! Enter/Display/Default
	!
20200	CASE OPT_ENTRY
		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 E0Loop:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP
		CASE 6% TO 9%

			IF PR_TRN_PAY::PTYPE = "O"
			THEN
				MFLAG = MFLAG OR 33%
				MFLAG = MFLAG AND NOT 64%
				MVALUE = ""
			END IF

		CASE 10%

			IF PR_TRN_PAY::PTYPE = "P"
			THEN
				MFLAG = MFLAG OR 1%
				MFLAG = MFLAG AND NOT 96%
			END IF

		END SELECT

		SELECT MLOOP

		CASE 1%
	!++
	!
	! Abstract:FLD001
	!	^*(01) Employee Number\*
	!	.p
	!	The ^*Employee Number\* is the number assigned to each employee to represent
	!	them when dealing with the company. It may contain up to ten (10) alphanumeric
	!	characters.
	!
	! Index:
	!	.x Employee Number>Timekeeper Journal
	!	.x Employee Number>Timekeeper Journal
	!
	!--

			PR_TRN_PAY::PR_END_DATE = END_DATE$
			PR_TRN_PAY::UPDATE_FLAG = 0%
			PR_TRN_PAY::BATCH_ENTRY	= BATCH_ENTRY$
			PR_TRN_PAY::SEQNUM	= ""
			PR_TRN_PAY::BATCH	= ""

			XLINE$ = NUM1$(SMG_WINDOW::CURLIN)
			PR_TRN_PAY::EMPNUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";3", TEMP$, &
				PR_TRN_PAY::EMPNUM, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				PR_TRN_PAY::EMPNUM = PR_EMP_MASTER::EMPNUM &
					IF (MAIN_WINDOW(PR_MAIN_TK_EMP_QUERY.ID, "VX ") = 1%)

				GOTO E0Loop
			END IF

		CASE 2%
	!++
	!
	! Abstract:FLD002
	!	^*(02) Operation\*
	!	.p
	!	The ^*Operation\* field refers to
	!	the code for the operation or task related to the specific record.
	!
	! Index:
	!	.x Operation>Payroll Register>Maintenance
	!	.x Payroll Register>Maintenance>Operation
	!
	!--
			PR_TRN_PAY::OPER = PR_EMP_MASTER::OPER &
				IF PR_TRN_PAY::OPER = ""

			XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 1%)
			PR_TRN_PAY::OPER = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";7", TEMP$, &
				PR_TRN_PAY::OPER, MFLAG, "'E", MVALUE)

		CASE 3%
	!++
	!
	! Abstract:FLD003
	!	^*(03) Account\*
	!	.p
	!	The ^*Account\* field refers to the
	!	General Ledger account number which relates to the specific record.
	!
	! Index:
	!	.x Account>Payroll Register>Maintenance
	!	.x Payroll Register>Maintenance>Account
	!
	!--
			PR_TRN_PAY::ACCT = PR_EMP_MASTER::ACCT &
				IF PR_TRN_PAY::ACCT = ""

			XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 1%)
			PR_TRN_PAY::ACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";16", TEMP$, &
				PR_TRN_PAY::ACCT, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				PR_TRN_PAY::ACCT = GL_CHART::ACCT &
					IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX ") = 1%)

				GOTO E0Loop
			END IF

		CASE 4%
	!++
	!
	! Abstract:FLD004
	!	^*(04) Pay Type\*
	!	.p
	!	The ^*Pay Type\* field record refers to the
	!	type or kind of payment which is being made to an employee in
	!	reference to a particular record. Valid types are:
	!	.b
	!	.lm +5
	!	.ls 0,"o"
	!	.LE
	!	P = Time or Units Related Payments
	!	.le
	!	O = Other Payments
	!	.ELS
	!	.lm -5
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
	!	.x Pay>Type>Payroll Register>Maintenance
	!	.x Payroll Register>Maintenance>Pay Type
	!
	!--
			XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 1%)
			PR_TRN_PAY::PTYPE = EDIT$(ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";35", TEMP$, &
				PR_TRN_PAY::PTYPE, MFLAG, "'", MVALUE, &
				PT$(), PTTITLE$, "005"), -1%)

		CASE 5%
	!++
	!
	! Abstract:FLD005
	!	^*(05) Pay Code\*
	!	.p
	!	The ^*Pay Code\* field refers to the
	!	specific type or kind of earnings represented in the record, i.e.
	!	a code for regular taxable earnings, bonuses, commission, etc.
	!
	! Index:
	!	.x Pay>Code>Payroll Register>Maintenance
	!	.x Payroll Register>Maintenance>Pay Code
	!
	!--

			XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 1%)

			PR_TRN_PAY::CODE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";38", TEMP$, &
				PR_TRN_PAY::CODE, MFLAG, "'E", MVALUE)


			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				PR_TRN_PAY::CODE = PR_ERNDED_DEF::CODE &
					IF (MAIN_WINDOW(PR_MAIN_ERNDED_DEF.ID, "V0P") = 1%)

				GOTO E0Loop
			END IF

		CASE 6%
	!++
	!
	! Abstract:FLD006
	!	^*(15) Hourly Rate\*
	!	.p
	!	The ^*Hourly Rate\* field refers to the
	!	appropriate hourly rate related to a specific record.
	!
	! Index:
	!	.x Hourly Rate>Payroll Register>Maintenance
	!	.x Rate>Hourly>Payroll Register>Maintenance
	!	.x Payroll Register>Maintenance>Hourly Rate
	!
	!--
			XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 1%)
			PR_TRN_PAY::HOUR_RATE = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";41", &
				TEMP$, PR_TRN_PAY::HOUR_RATE * 1.0, &
				MFLAG, "####.###", MVALUE)

		CASE 7%
	!++
	!
	! Abstract:FLD007
	!	^*(07) Regular Hours\*
	!	.p
	!	The ^*Regular Hours\* field refers to
	!	the number of hours (or other unit of time) which an employee worked
	!	or is to be credited. The number of regular hours is exclusive of
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
	!	.x Hours>Regular>Payroll Register>Maintenance
	!	.x Payroll Register>Maintenance>Hours>Regular
	!
	!--
			XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 1%)
			PR_TRN_PAY::REG_HR = ENTR_3TIMEKEEPER(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";51", TEMP$, &
				PR_TRN_PAY::REG_HR * 1.0, MFLAG, "####.##", &
				MVALUE)

		CASE 8%
	!++
	!
	! Abstract:FLD008
	!	^*(08) Overtime Factor\*
	!	.p
	!	The ^*Overtime Factor\* field refers to
	!	an overtime hourly pay rate expressed as a percentage of an hourly
	!	rate. Hence, an overtime rate verbally expressed as "time and a half"
	!	is expressed in this field as "150".
	!
	! Index:
	!	.x Overtime>Factor>Payroll Register>Maintenance
	!	.x Payroll Register>Maintenance>Overtime>Factor
	!
	!--
			XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 1%)
			PR_TRN_PAY::FACTOR = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";59", &
				TEMP$, PR_TRN_PAY::FACTOR * 1.0, &
				MFLAG, "###", MVALUE)

		CASE 9%
	!++
	!
	! Abstract:FLD009
	!	^*(09) Overtime Hours\*
	!	.p
	!	The ^*Overtime Hours\* field refers to
	!	the number of overtime hours worked by an employee in reference to
	!	the specific record.
	!	.p
	!	The field will accommodate an entry to the nearest 1/100th of
	!	an hour.
	!
	! Index:
	!	.x Overtime Hours>Payroll Register>Maintenance
	!	.x Payroll Register>Maintenance>Overtime Hours
	!
	!--
			XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 1%)
			PR_TRN_PAY::OVT_HR = ENTR_3TIMEKEEPER(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";64", TEMP$, &
				PR_TRN_PAY::OVT_HR * 1.0, MFLAG, "###.##", &
				MVALUE)

		CASE 10%
	!++
	!
	! Abstract:FLD010
	!
	!	^*(10) Gross\*
	!	Displays the gross amount of pay.
	!
	! Index:
	!--
			IF PR_TRN_PAY::PTYPE = "P"
			THEN
				GOSUB CalcGross
			END IF

			XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 1%)
			PR_TRN_PAY::GROSS = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";71", TEMP$, &
				PR_TRN_PAY::GROSS * 1.0, MFLAG, &
				"#####.##", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM= TEMP1$


	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		TEMP_FLAG%, PR_MAIN_TRN_TK02_PAY = 0%

		SELECT MLOOP

		CASE 1%
			!
			! Is the input defined?
			!
			PR_EMP_MASTER::EMP_SKILL = &
				STRING$(LEN(PR_TRN_PAY::EMP_SKILL), 63%)
			PR_EMP_MASTER::EMP_GRADE = &
				STRING$(LEN(PR_TRN_PAY::EMP_GRADE), 63%)
			PR_EMP_MASTER::ACCT = &
				STRING$(LEN(PR_TRN_PAY::ACCT), 63%)
			PR_EMP_MASTER::SUBACC = &
				STRING$(LEN(PR_TRN_PAY::SUBACC), 63%)
			PR_EMP_MASTER::OPER = &
				STRING$(LEN(PR_TRN_PAY::OPER), 63%)
			PR_EMP_MASTER::LOCATION = &
				STRING$(LEN(PR_TRN_PAY::LOCATION), 63%)
			PR_EMP_MASTER::DEPT = &
				STRING$(LEN(PR_TRN_PAY::DEPT), 63%)
			PR_EMP_MASTER::WORK_CENTER = &
				STRING$(LEN(PR_TRN_PAY::WORK_CENTER), 63%)
			PR_EMP_MASTER::UNION = &
				STRING$(LEN(PR_TRN_PAY::UNION), 63%)
			PR_EMP_MASTER::TAX_PKG = &
				STRING$(LEN(PR_TRN_PAY::TAX_PKG), 63%)

			PR_MAIN_TRN_TK02_PAY = FUNC_TESTENTRY(SMG_WINDOW, &
				PR_TRN_PAY::EMPNUM, &
				PR_EMP_MASTER::EMPNAME, &
				"PR", MLOOP, "PRG", &
				"Employee Number", PR_MAIN_TK_EMP_QUERY.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(PR_EMP_MASTER::EMPNAME, 19%), &
				SMG_WINDOW::CURLIN, 14%,, SMG$M_BOLD)

			PR_TRN_PAY::EMP_SKILL	= PR_EMP_MASTER::EMP_SKILL
			PR_TRN_PAY::EMP_GRADE	= PR_EMP_MASTER::EMP_GRADE
			PR_TRN_PAY::ACCT	= PR_EMP_MASTER::ACCT
			PR_TRN_PAY::SUBACC	= PR_EMP_MASTER::SUBACC
			PR_TRN_PAY::OPER	= PR_EMP_MASTER::OPER
			PR_TRN_PAY::LOCATION	= PR_EMP_MASTER::LOCATION
			PR_TRN_PAY::DEPT	= PR_EMP_MASTER::DEPT
			PR_TRN_PAY::WORK_CENTER	= PR_EMP_MASTER::WORK_CENTER
			PR_TRN_PAY::UNION	= PR_EMP_MASTER::UNION
			PR_TRN_PAY::TAX_PKG	= PR_EMP_MASTER::TAX_PKG

			PR_TRN_PAY::PTYPE	= "P"
			PR_TRN_PAY::PIECE	= 0%

			IF TEMP_FLAG% = 0%
			THEN
				IF EDIT$(PR_EMP_MASTER::TERMDAY, -1%) > "00000000"
				THEN
					!
					! Has the employee been terminated.
					!
					TEMP_FLAG%, PR_MAIN_TRN_TK02_PAY = 1%
					CALL ENTR_3MESSAGE(SCOPE, "Employee has a termination date ", 0%)
				END IF
			END IF

			IF TEMP_FLAG% = 0%
			THEN
				IF EDIT$(PR_EMP_MASTER::LOCATION, -1%) <> &
					LOCATION$ AND EDIT$(LOCATION$, -1%) <> ""
				THEN
					!
					! Has the employee been terminated.
					!
					PR_MAIN_TRN_TK02_PAY = 1%
					CALL ENTR_3MESSAGE(SCOPE, &
						"Employee Does not work in this location ", 0%)
				END IF
			END IF

		CASE 2%
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
					SMG_WINDOW::CURLIN + 1%, 38%, , &
					SMG$M_BOLD)

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					FORMAT$(PR_TRN_PAY::HOUR_RATE, &
					"####.###"), &
					SMG_WINDOW::CURLIN + 1%, 41%, , &
					SMG$M_BOLD)

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					FORMAT$(PR_TRN_PAY::FACTOR, "###%"), &
					SMG_WINDOW::CURLIN + 1%, 59%, , &
					SMG$M_BOLD)

			END IF

		CASE 3%
			!
			! Is the input defined?
			!
			PR_MAIN_TRN_TK02_PAY = FUNC_TESTENTRY(SMG_WINDOW, &
				PR_TRN_PAY::ACCT, &
				GL_CHART::DESCR, &
				"PR", MLOOP, "PRG", &
				"Account", GL_MAIN_CHART.ID)

		CASE 5%
			PR_MAIN_TRN_TK02_PAY = FUNC_TESTENTRY(SMG_WINDOW, &
				"P" + PR_TRN_PAY::CODE, &
				PR_ERNDED_DEF::DESCR, &
				"PR", MLOOP, "PRG", &
				"Code", PR_MAIN_ERNDED_DEF.ID)

		CASE 6% TO 9%

			GOSUB CalcGross

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				FORMAT$(PR_TRN_PAY::GROSS, "#####.##"), &
				SMG_WINDOW::CURLIN + 1%, 71%,, SMG$M_BOLD)

		END SELECT

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
		PR_TRN_PAY2 = PR_TRN_PAY

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PR_TRN_PAY2::EMPNUM = PR_TRN_PAY::EMPNUM
		PR_TRN_PAY = PR_TRN_PAY2

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
			RARRAY(MFLAG)::LINRFA	= GETRFA(SMG_WINDOW::CHAN)

			RARRAY(MFLAG)::REG_HR	= PR_TRN_PAY::REG_HR
			RARRAY(MFLAG)::OVT_HR	= PR_TRN_PAY::OVT_HR
			RARRAY(MFLAG)::PIECE	= PR_TRN_PAY::PIECE
			RARRAY(MFLAG)::GROSS	= PR_TRN_PAY::GROSS

		!
		! Load in current record, locked
		!
		CASE 4%
27200			WHEN ERROR IN
				GET #SMG_WINDOW::CHAN, RFA RARRAY(MFLAG)::LINRFA
			USE
				CONTINUE ExitFunction IF ERR = 173% AND &
					SMG_WINDOW::TOTREC = 0%
				EXIT HANDLER
			END WHEN

		!
		! Load in current record, unlocked
		!
		CASE 5%
			WHEN ERROR IN
				GET #SMG_WINDOW::CHAN, RFA RARRAY(MFLAG)::LINRFA, &
					REGARDLESS
			USE
				CONTINUE ExitFunction IF ERR = 173% AND &
					SMG_WINDOW::TOTREC = 0%
				EXIT HANDLER
			END WHEN

		!
		! Handle anything extra
		!
		CASE 7%

			!
			! Is the input defined?
			!
			PR_EMP_MASTER::EMPNAME = STRING$(23%, 63%) &
				IF MAIN_WINDOW(PR_MAIN_TK_EMP_QUERY.ID, &
				"Q0" + PR_TRN_PAY::EMPNUM) <> 1%

			!
			! Print Employee Name
			!
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(PR_EMP_MASTER::EMPNAME, 23%), &
				SMG_WINDOW::CURLIN, 14%,, SMG$M_BOLD)

			!
			! Draw horizontal lines
			!
			SMG_STATUS% = SMG$DRAW_LINE(SMG_WINDOW::WNUMBER, &
				SMG_WINDOW::CURLIN + 2%, 1%, &
				SMG_WINDOW::CURLIN + 2%, 78%)

			!
			! Print the % for Over Time Factor
			!
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				"%", SMG_WINDOW::CURLIN + 1%, 62%,, SMG$M_BOLD)

		END SELECT
	END SELECT

 ExitFunction:
28000	EXIT FUNCTION

	%PAGE

 CalcGross:
	!*******************************************************************
	! Calculate Gross Pay
	!*******************************************************************

	SELECT PR_TRN_PAY::RTYPE

	CASE "H", "S"
		PR_TRN_PAY::GROSS = FUNC_ROUND( &
			(PR_TRN_PAY::REG_HR + &
			(PR_TRN_PAY::OVT_HR * &
			(PR_TRN_PAY::FACTOR / 100.0))) * &
			PR_TRN_PAY::HOUR_RATE, 2%)

	CASE "X"
		TEMP1 = FUNC_ROUND( &
			(PR_TRN_PAY::REG_HR + &
			(PR_TRN_PAY::OVT_HR * &
			(PR_TRN_PAY::FACTOR / 100.0))) * &
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
