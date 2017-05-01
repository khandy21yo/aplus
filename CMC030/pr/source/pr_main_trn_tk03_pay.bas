1	%TITLE "Pay Time Keeper Journal Maintenance"
	%SBTTL "PR_MAIN_TRN_TK03_PAY"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PR_MAIN_TRN_TK03_PAY(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	The ^*Time Keeper Journal Maintenance\*  program maintains the PR Pay Time
	!	Keeper Journal file.
	!
	! Index:
	!	.x Timekeeper Journal Maintenance
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_MAIN_TRN_TK03_PAY/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN PR_MAIN_TRN_TK03_PAY
	!	$ DELETE PR_MAIN_TRN_TK03_PAY.OBJ;*
	!
	! Author:
	!
	!	01/05/88 - B. Craig Larsen
	!
	! Modification history:
	!
	!	04/07/89 - Kevin Handy
	!		Modified from PR_MAIN_TRN_TK02_PAY
	!
	!	01/17/90 - Kevin Handy
	!		Moved initialization of List Choices array into
	!		init section to reduce CPU time.
	!
	!	04/03/90 - Kevin Handy
	!		Added deduction and check screens to program.
	!
	!	10/25/90 - Kevin Handy
	!		Added pay type "X" (eXcess).
	!
	!	03/18/91 - Kevin Handy
	!		Added eval_date field to pr_read_rate.
	!
	!	12/18/91 - Kevin Handy
	!		Added code for "A" accrual.
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
	!		Use ENTR_3TIMEKEEPER for hours
	!
	!	03/22/2000 - Kevin Handy
	!		Added EFF_DATE parameter to PR_READ_RATE
	!
	!	12/08/2000 - Kevin Handy
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
		PTTITLE$ = 25%, &
		PT$(5%) = 25%

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
		SMG_WINDOW::DESCR = "Pay Journal " + &
			PRNT_DATE(BATCH_NO$, 8%)
		SMG_WINDOW::NHELP = "PR_MAIN_TRN_TK03_PAY"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 11%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::TOPLIN = 3%
		SMG_WINDOW::BOTLIN = 17%
		SMG_WINDOW::LINREC = 4%

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
			PR_MAIN_TRN_TK03_PAY = ERR
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
			PR_MAIN_TRN_TK03_PAY = ERR
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
			"  01     02       03              05 06       " + &
			"07       08   09     10       11 ", &
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
	!++
	! Abstract:DEDUCTION
	!	^*Deduction\*
	!	.p
	!	The ^*Deduction\* option accesses
	!	the deduction menu where all deductions are entered and maintained.
	!
	! Index:
	!	.x Deduction
	!
	!--

			PR_MAIN_TRN_TK03_PAY = MAIN_JOURNAL(PR_MAIN_TRN_TK02_DED.ID, "")

		CASE "checK"
	!++
	! Abstract:CHECK
	!	^*Check\*
	!	.p
	!	The ^*Check\* option accesses
	!	check menu where check are entered when they are written.
	!
	! Index:
	!
	!--

			PR_MAIN_TRN_TK03_PAY = MAIN_JOURNAL(PR_MAIN_TRN_TK02_CHECK.ID, "")

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
			SMG_WINDOW::CURLIN + 3%, &
			1%, SMG_WINDOW::CURLIN + 3%, 78%)

		!
		! Print the % sign for the Over Time Factor
		!
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"%", SMG_WINDOW::CURLIN + 1%, 62%,, SMG$M_BOLD)
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"04 Location", SMG_WINDOW::CURLIN + 2%, 2%,, &
			SMG$M_REVERSE)

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

	!
	! Enter/Display/Default
	!
20200	CASE OPT_ENTRY
		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 E0Loop:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP
		CASE 7% TO 10%

			IF PR_TRN_PAY::PTYPE = "O"
			THEN
				MFLAG = MFLAG OR 33%
				MFLAG = MFLAG AND NOT 64%
				MVALUE = ""
			END IF

		CASE 11%

			IF PR_TRN_PAY::PTYPE = "P"
			THEN
				MFLAG = MFLAG OR 1%
				MFLAG = MFLAG AND NOT 96%
			END IF

		END SELECT

		SELECT MLOOP

		CASE 1%
	!++
	! Abstract:FLD001
	!	^*(01) Employee _#\*
	!	.p
	!	The ^*Employee _#\* column
	!	enters the employee number who is to be paid for hours
	!	worked or credited or for payments or earnings unrelated to time.
	!	.p
	!	The number must be a valid number, i.e., a record must be established
	!	in the Employee Master file.  If an invalid number is entered, the
	!	system will prompt, "Employee number is undefined," and a string of
	!	question marks will appear in the Employee Name column.  When a valid
	!	number is entered, the system will display the employee's name in the
	!	Employee Name column.
	!
	! Index:
	!	.x Employee>Number
	!
	!--
			PR_TRN_PAY::PR_END_DATE = END_DATE$
			PR_TRN_PAY::UPDATE_FLAG	= 0%
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
					IF (MAIN_WINDOW( &
					PR_MAIN_TK_EMP_QUERY.ID, "VX ") = 1%)

				GOTO E0Loop
			END IF

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Operation\*
	!	.p
	!	The ^*Operation\* field
	!	enters a specific operation in which an employee worked.  If there
	!	is a default Operation in an employee's master record, that default
	!	will automatically appear in this field.  It may be overridden if
	!	necessary.
	!	.p
	!	This field may be null.
	!
	! Index:
	!	.x Operation>Pay Journal
	!	.x Pay Journal>Operation
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
	! Abstract:FLD003
	!	^*(03) Account\*
	!	.p
	!	The ^*Account\* field enters
	!	the General Ledger account number to which an employees earnings will
	!	be charged.  If an employee's master record contains a default account
	!	number, that number will automatically appear in this field.  The default
	!	value may be overridden if necessary.
	!	.p
	!	This field cannot be null.
	!
	! Index:
	!	.x Pay Journal>Account
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
					IF (MAIN_WINDOW(GL_MAIN_CHART.ID, &
					"VX ") = 1%)

				GOTO E0Loop
			END IF

		CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Location\*
	!	.p
	!	The ^*Location\* field
	!	enters the location where an employee worked. If the Location field in
	!	the employee's master record contains a value, that value will automatically
	!	appear in this field. The default value may be overridden if necessary.
	!
	! Index:
	!	.x Location>Timekeeper
	!	.x Timekeeper>Location
	!
	!--
			XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 2%)
			PR_TRN_PAY::LOCATION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";14", TEMP$, &
				PR_TRN_PAY::LOCATION, MFLAG, "'E", MVALUE)


		CASE 5%
	!++
	! Abstract:FLD005
	!	^*(05) Pay Type\*
	!	.p
	!	The ^*Pay Type\* field
	!	enters a flag which designates the type of pay which relates to
	!	a record.  Valid Pay Types are:
	!	.p
	!	.lm +5
	!	.list 0,"o"
	!	.le
	!	P = Time
	!	.le
	!	O = Other
	!	.els
	!	.lm -5
	!	.p
	!	If the flag is a ^*P\* type, the system will permit the operator
	!	to enter regular and/or overtime hours.  If the flag is an ^*O\* type,
	!	the system will bypass the time fields and permit a value to be entered
	!	in the Gross field.
	!
	! Index:
	!	.x Pay>Type
	!
	!--
			XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 1%)
			PR_TRN_PAY::PTYPE = EDIT$(ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";35", TEMP$, &
				PR_TRN_PAY::PTYPE, MFLAG, "'", MVALUE, &
				PT$(), PTTITLE$, "005"), -1%)

		CASE 6%
	!++
	! Abstract:FLD006
	!	^*(06) Code\*
	!	.p
	!	The ^*Code\* field
	!	enters a code which identifies the specific kind of pay an
	!	employee is to be paid, such as regular pay, vacation pay, bonus, etc.
	!	The code is user defined and must be established in the Earnings and
	!	Deductions Definition file.  If the Pay Code field in an employee's master
	!	record has a value, that value will automatically appear in this field.
	!	The default value may be overridden, if necessary.
	!
	! Index:
	!	.x Pay Code
	!	.x Code>Pay
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

		CASE 7%
	!++
	! Abstract:FLD007
	!	^*(07) Hourly Rate\*
	!	.p
	!	The ^*Hourly Rate\* field
	!	enters the rate an employee is to be paid.  The field will
	!	accommodate three (3) positions to the left of the decimal point and
	!	will contain a number as large as 999.999. The rate recorded in an
	!	employee's master record will automatically appear in this field.
	!	The default rate may be overridden, if necessary.
	!	.p
	!	In order for a default rate to appear automatically, the value in
	!	the Operation field of this record must be equal to a value in the
	!	Operation field in the rate record in an employee's master file.  The
	!	value in this field may be null.
	!	.p
	!	Though this field is designated, "Hourly" Rate, it could be used
	!	as any "Time" Rate, such as "Weekly" Rate, "Monthly" Rate, etc.  The
	!	use of an hourly equivalent, however, for salaried personnel provides
	!	the ability to track vacation pay and/or other employee benefits.
	!
	! Index:
	!	.x Rate>Hourly
	!	.x Rate>Pay
	!
	!--
			XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 1%)
			PR_TRN_PAY::HOUR_RATE = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";41", &
				TEMP$, PR_TRN_PAY::HOUR_RATE * 1.0, &
				MFLAG, "####.###", MVALUE)

		CASE 8%
	!++
	! Abstract:FLD008
	!	^*(08) Regular Hours\*
	!	.p
	!	The ^*Regular Hours\* field
	!	enters the regular hours or time (as opposed to overtime) an employee
	!	worked or is being given credit.
	!	.p
	!	The field will accommodate a numerical value as large as 999.99.
	!
	! Index:
	!	.x Regular Hours
	!	.x Hours>Regular
	!	.x Regular Hours>Timekeeper
	!
	!--
			XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 1%)
			PR_TRN_PAY::REG_HR = ENTR_3TIMEKEEPER(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";51", TEMP$, &
				PR_TRN_PAY::REG_HR * 1.0, MFLAG, "####.##", &
				MVALUE)

		CASE 9%
	!++
	! Abstract:FLD009
	!	^*Overtime Factor\*
	!	.p
	!	The ^*Overtime Factor\* or ^*Overtime Factor\* field
	!	enters a factor to be used in the calculation of overtime
	!	pay.  An overtime factor of "150", for example, would calculate overtime
	!	earnings at one and one-half the regular rate, while a factor of "200"
	!	would calculate overtime earnings at two times the regular rate.
	!	.p
	!	If Overtime Factor field in an employee's master file contains
	!	an value, that value will automatically appear in this field.  The
	!	defaulted value may be overridden, if necessary.
	!
	! Index:
	!	.x Overtime>Factor
	!
	!--
			XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 1%)
			PR_TRN_PAY::FACTOR = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";59", &
				TEMP$, PR_TRN_PAY::FACTOR * 1.0, &
				MFLAG, "###", MVALUE)

		CASE 10%
	!++
	! Abstract:FLD010
	!	^*(10) Overtime Hours\*
	!	.p
	!	The ^*Overtime Hours\* field
	!	enters the overtime hours or time an employee
	!	worked or is being given credit.
	!	.p
	!	The field will accommodate a numerical value as large as 999.99.
	!
	! Index:
	!	.x Overtime Hours
	!	.x Hours>Overtime
	!
	!--
			XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 1%)
			PR_TRN_PAY::OVT_HR = ENTR_3TIMEKEEPER(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";64", TEMP$, &
				PR_TRN_PAY::OVT_HR * 1.0, MFLAG, "###.##", &
				MVALUE)

		CASE 11%
	!++
	!
	! ABSTRACT:FLD011
	!
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
		TEMP_FLAG%, PR_MAIN_TRN_TK03_PAY = 0%

		SELECT MLOOP

		CASE 1%
			!
			! Is the input defined?
			!
			PR_EMP_MASTER::EMP_SKILL= STRING$(LEN(PR_TRN_PAY::EMP_SKILL), 63%)
			PR_EMP_MASTER::EMP_GRADE= STRING$(LEN(PR_TRN_PAY::EMP_GRADE), 63%)
			PR_EMP_MASTER::ACCT	= STRING$(LEN(PR_TRN_PAY::ACCT), 63%)
			PR_EMP_MASTER::SUBACC	= STRING$(LEN(PR_TRN_PAY::SUBACC), 63%)
			PR_EMP_MASTER::OPER	= STRING$(LEN(PR_TRN_PAY::OPER), 63%)
			PR_EMP_MASTER::LOCATION	= STRING$(LEN(PR_TRN_PAY::LOCATION), 63%)
			PR_EMP_MASTER::DEPT	= STRING$(LEN(PR_TRN_PAY::DEPT), 63%)
			PR_EMP_MASTER::WORK_CENTER= STRING$(LEN(PR_TRN_PAY::WORK_CENTER), 63%)
			PR_EMP_MASTER::UNION	= STRING$(LEN(PR_TRN_PAY::UNION), 63%)
			PR_EMP_MASTER::TAX_PKG	= STRING$(LEN(PR_TRN_PAY::TAX_PKG), 63%)

			PR_MAIN_TRN_TK03_PAY = FUNC_TESTENTRY(SMG_WINDOW, &
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
					TEMP_FLAG%, PR_MAIN_TRN_TK03_PAY = 1%
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
					PR_MAIN_TRN_TK03_PAY = 1%
					CALL ENTR_3MESSAGE(SCOPE, "Employee Does not work in this location ", 0%)
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
			PR_MAIN_TRN_TK03_PAY = FUNC_TESTENTRY(SMG_WINDOW, &
				PR_TRN_PAY::ACCT, &
				GL_CHART::DESCR, &
				"PR", MLOOP, "PRG", &
				"Account", GL_MAIN_CHART.ID)

		CASE 6%
			PR_MAIN_TRN_TK03_PAY = FUNC_TESTENTRY(SMG_WINDOW, &
				"P" + PR_TRN_PAY::CODE, &
				PR_ERNDED_DEF::DESCR, &
				"PR", MLOOP, "PRG", &
				"Code", PR_MAIN_ERNDED_DEF.ID)

		CASE 7% TO 10%

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

	%PAGE

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
