1	%TITLE "Employee Pay-erned Maintenance"
	%SBTTL "PR_MAIN_EMP_STD_ERNDED"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PR_MAIN_EMP_STD_ERNDED(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	The ^*Pay-ernded\* function
	!	accesses the file where standard payroll
	!	benefit accruals, standard payments and standard payroll deductions
	!	are recorded.
	!
	! Index:
	!	.x Standard Payments
	!	.x Standard Deductions
	!	.x Standard Accruals
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_MAIN_EMP_STD_ERNDED/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN PR_MAIN_EMP_STD_ERNDED
	!	$ DELETE PR_MAIN_EMP_STD_ERNDED.OBJ;*
	!
	! Author:
	!
	!	09/17/87 - Kevin Handy
	!
	! Modification history:
	!
	!	06/01/88 - Aaron Redd
	!		Modified to allow R/O open of file if R/W open fails.
	!
	!	10/04/90 - Kevin Handy
	!		Added method 6 to method.
	!
	!	04/23/91 - Kevin Handy
	!		Added method 7.
	!
	!	04/22/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	04/29/92 - Kevin Handy
	!		Clean up (check)
	!
	!	08/03/92 - Kevin Handy
	!		Fix bug that gave undefined code for everything
	!		entered in field 2, even if it was defined.
	!		(Caused by 04/22/92 change)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/22/96 - Kevin Handy
	!		Reformat source code
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
	!	12/13/2000 - Kevin Handy
	!		Use WHEN ERROR IN
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

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_STD_ERNDED.HB"
	MAP (PR_EMP_STD_ERNDED)	PR_EMP_STD_ERNDED_CDD	PR_EMP_STD_ERNDED
	MAP (PR_EMP_STD_ERNDED_OLD) PR_EMP_STD_ERNDED_CDD &
		PR_EMP_STD_ERNDED_OLD, PR_EMP_STD_ERNDED2

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP	(PR_EMP_MASTER)		PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.HB"
	MAP	(PR_ERNDED_DEF)		PR_ERNDED_DEF_CDD	PR_ERNDED_DEF

	!
	! Create array to contain pointers and totals
	!
	RECORD RARRAY_RECORD
		RFA	LINRFA		! Rfa pointer for record
	END RECORD

	MAP (TT_PR_EMP_STD_ERNDED) RARRAY_RECORD RARRAY(300%)

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	! This common area must be mapped in some of the MAIN programs,
	! PR_MAST_EMPLOYEE.BAS, and PR_MAST_WC_WORK.BAS.
	!
	COM (CH_PR_EMP_STD_ERNDED) &
		PR_EMP_STD_ERNDED.CH%, &
		PR_EMP_STD_ERNDED.READONLY%

	COM (TT_EMP_STD_ERNDED) &
		PTTITLE$ = 20%, &
		PT$(4%) = 20%, &
		MTDTITLE$ = 24%, &
		MTD$(7%) = 24% &

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION MAIN_WINDOW
	EXTERNAL LONG	FUNCTION FUNC_TESTENTRY

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	!
	! Initialization
	!
	! This option is used to initialize the window structure,
	! set up the default values for add, and open all files
	! necessary that have not already been opened.
	!
	CASE OPT_INIT

		!
		! Define window
		!
		SMG_WINDOW::DESCR = &
			"Employee Standard Accrual/Pay/Deduction Rates"
		SMG_WINDOW::NHELP = "PR_MAIN_EMP_STD_ERNDED"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 15%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 5%
		SMG_WINDOW::NITEMS= 10%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::TOPLIN = 3%
		SMG_WINDOW::BOTLIN = 14%
		SMG_WINDOW::LINREC = 1%

		!
		! Pay types
		!
		PTTITLE$ = "Type Description"
		PT$(0%) = "4"
		PT$(1%) = "A    Accrual"
		PT$(2%) = "P    Payment"
		PT$(3%) = "D    Deduction"
		PT$(4%) = "F    Final (Deduction)"

		!
		! Methods
		!
		MTDTITLE$ = "Method Description"

		MTD$(0%) = "7"
		MTD$(1%) = "1      Per Hour"
		MTD$(2%) = "2      Per Mile"
		MTD$(3%) = "3      % of Gross"
		MTD$(4%) = "4      % of Net"
		MTD$(5%) = "5      Per Pay Period"
		MTD$(6%) = "6      Per Hour/Premium"
		MTD$(7%) = "7      Per Hour/Limit per Pay Period"

		!
		! Load in defaults
		!
		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF PR_EMP_STD_ERNDED.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PR_EMP_STD_ERNDED.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_STD_ERNDED.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PR_MAIN_EMP_STD_ERNDED = ERR
			CONTINUE 770
		END WHEN

		PR_EMP_STD_ERNDED.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_STD_ERNDED.OPN"
		USE
			PR_MAIN_EMP_STD_ERNDED = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PR_EMP_STD_ERNDED.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PR_EMP_STD_ERNDED.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = PR_EMP_STD_ERNDED.CH%
		WHEN ERROR IN
			RESET #PR_EMP_STD_ERNDED.CH%
			GET #PR_EMP_STD_ERNDED.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

	!
	! Display the background
	!
	! This option is used to display the background information on the
	! screen.  It must first clear any junk on the screen, and then
	! write the background onto it.
	!
20100	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"(01) (02)   (03)      (04)      (05)    " + &
			"   (06)      (07)    (08)fq (09) (10) ", &
			1%, 1%, , SMG$M_REVERSE)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"Type Code   Rate     Limit     To date  " + &
			" Accrued   End date  12345S Mthd User ", &
			2%, 1%, , SMG$M_REVERSE)

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

	!
	! Extra display stuff
	!
	CASE OPT_DISPLAY

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		!
		! Display totals
		!
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"Number of lines" + &
			FORMAT$(SMG_WINDOW::TOTREC, "####") + SPACE$(60%), &
			SMG_WINDOW::VSIZE, 1%, , SMG$M_REVERSE)

		!
		! Paint lines on screen
		!
		FOR I% = 1% TO 9%
			A% = VAL%(MID("005,010,020,030,040,050,061,068,073", &
				I% * 4% - 3%, 3%))

			SMG_STATUS% = SMG$DRAW_LINE(SMG_WINDOW::WNUMBER, &
				1%, A%, SMG_WINDOW::BOTLIN, A%)
		NEXT I%

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

	!
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user, display data,
	! set defaults, and return the data back according to MFLAG.
	!
20200	CASE OPT_ENTRY

		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

		XLINE$ = NUM1$(SMG_WINDOW::CURLIN)

 E0Loop:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Type\*
	!	.p
	!	The ^*Type\* column
	!	enters a code designating
	!	the type of transaction represented by the record. Valid transactions
	!	types are:
	!	.b
	!	.LIST 0,"*"
	!	.LE
	!	A = Accrual (Replaced by accrual table)
	!	.LE
	!	P = Payment
	!	.LE
	!	D = Deduction
	!	.ELS
	!	.p
	!	If the type code is a ^*P\*, the employee master file defaults
	!	will be used to determine the account number, subacct, location, dept,
	!	etc. for the pay record that will be created.
	!
	! Index:
	!	.x Pay/Deduction>Code
	!	.x Code>Pay/Deduction
	!
	!--

			PR_EMP_STD_ERNDED::RTYPE = &
				EDIT$(ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";3", TEMP$, &
				PR_EMP_STD_ERNDED::RTYPE, MFLAG, "'E", MVALUE, &
				PT$(), PTTITLE$, "005"), -1%)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Code\*
	!	.p
	!	The ^*Code\* column
	!	enters the code which
	!	identifies the specific kind of accrual, payment or deduction that
	!	relates to the transaction type. Valid codes are the user defined
	!	codes which exist in the PAYDED file.
	!
	! Index:
	!	.x Pay/Deduction>Deduction Code
	!	.x Deduction Code>Pay/Deduction
	!	.x Code>Pay/Deduction
	!
	!--

			PR_EMP_STD_ERNDED::CODE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";7", TEMP$, &
				PR_EMP_STD_ERNDED::CODE, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(PR_MAIN_ERNDED_DEF.ID, "V0" + &
					PR_EMP_STD_ERNDED::RTYPE) = 1%)
				THEN
					PR_EMP_STD_ERNDED::CODE = &
						PR_ERNDED_DEF::CODE
					GOTO E0Loop
				END IF
			END IF

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Rate\*
	!	.p
	!	The ^*Rate\* column
	!	enters a rate or amount
	!	which is expressed as a dollar amount, a percentage,or an hour
	!	amount. The field will accommodate five (5) positions to the left
	!	of the decimal and three (3) positions to the right of the decimal.
	!	.p
	!	The system understands if the value is a dollar amount, a
	!	percentage, or an hour amount depending the value in the Type field
	!	and/or upon the value in the Method column. If the method were "per
	!	pay period", and the type were a P or a D the value in column (3)
	!	would be understood as a dollar amount. If the method were "gross"
	!	and the type were a P or a D, the value in column (3) would be
	!	understood as a percentage of the gross. When the type is an A, the
	!	Rate field is always understood to be an hour amount.
	!
	! Index:
	!	.x Pay/Deduction>Deduction Rate
	!	.x Deduction Rate>Pay/Deduction
	!	.x Rate>Pay/Deduction
	!
	!--

			PR_EMP_STD_ERNDED::RATE = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";11", TEMP$, &
				PR_EMP_STD_ERNDED::RATE, &
				MFLAG, "#####.###", MVALUE)

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Deduction Limit\*
	!	.p
	!	The ^*Deduction Limit\* column
	!	enters a dollar limit
	!	for a payment or deduction type, or an hour limit for an accrual
	!	type. The sum of a payroll deduction or a payment type for all
	!	payroll periods will not exceed the value of this field except when
	!	the Limit field value is zero. Likewise, an accrual type will not
	!	accrue more than the value in the Limit field, unless the Limit
	!	field value is zero.
	!	.p
	!	The value in the Limit field is expressed in dollars or hours.
	!	If the transaction type is a P or a D, the Limit field value is
	!	expressed in dollars. If the transaction type is an A, the Limit
	!	field value is expressed in hours.
	!
	! Index:
	!	.x Pay/Deduction>Deduction Limit
	!	.x Deduction Limit>Pay/Deduction
	!	.x Limit>Pay/Deduction
	!
	!--

			PR_EMP_STD_ERNDED::LIMIT = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";21", TEMP$, &
				PR_EMP_STD_ERNDED::LIMIT, &
				MFLAG, "######.##", MVALUE)

		CASE 5%

	!++
	! Abstract:FLD005
	!	^*(05) To Date\*
	!	.p
	!	The ^*To Date\* field
	!	is system generated.
	!	The value is expressed in dollars if the type is a payment or deduction.
	!	If the type is an accrual, the value is expressed in hours.
	!	.p
	!	The amount in the To Date field is a historical accumulation and
	!	has no relationship to any year-to-date concept. Some examples follow:
	!	.p
	!	EXAMPLE 1. If an employee were to receive a payroll advance for
	!	$50. and the advance were to be deducted from his/her payroll at a
	!	determined amount or percentage of gross per payroll period, the value
	!	of 50 would be entered in the Limit field. As the amounts were
	!	deducted from the employee's payroll, the To Date field would be
	!	incremented. The system would deduct amounts from the employee only
	!	until the value of the To Date field equaled 50. The record could
	!	then be erased. If the record were not erased, and the employee
	!	received another payroll advance, it would be necessary to re-enter
	!	the value of the Limit field to equal the amount of the new payroll
	!	advance and set the value to the To Date field to zero.
	!	.p
	!	EXAMPLE 2. As an employee takes vacation, and a payroll update
	!	process is executed, the number of vacation hours paid is added by the
	!	system to the To Date field. On the employee's employment date or
	!	calendar year end (whichever is stored in the End Date field) it would
	!	be necessary to set the value of the To Date field to zero.
	!
	! Index:
	!	.x To Date>Pay/Deduction
	!	.x Pay/Deduction>To Date
	!	.x Date>Pay/Deduction
	!
	!--

			PR_EMP_STD_ERNDED::CTDBAL = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";31", TEMP$, &
				PR_EMP_STD_ERNDED::CTDBAL, &
				MFLAG, "######.##", MVALUE)

		CASE 6%

	!++
	! Abstract:FLD006
	!	^*(06) Accrued\*
	!	.p
	!	The ^*Accrued\* column
	!	enters, during the initializing
	!	procedures, the number of hours accrued for each payroll benefit type
	!	offered by the employer.
	!	.p
	!	Subsequent to initialization, when the payroll benefit accrual
	!	process is executed, the Accrued field in each accrual type record
	!	will be appropriately incremented. When each payroll file is updated,
	!	the To Date field in each accrual type record will be incremented by
	!	the number of benefit hours used by any related employee. The
	!	difference between the Accrued field and the To Date field equal the
	!	current hours balance of the subject payroll benefit. The hours
	!	balance and resulting dollars balance is printed on the Employees'
	!	Accrual Report.
	!	.p
	!	An accrual record will be inactivated if the value in the Accrued
	!	field equals the value in the Limit field.
	!
	! Index:
	!	.x Pay/Deduction>Accrued
	!	.x Accrued>Pay/Deduction
	!
	!--

			PR_EMP_STD_ERNDED::ACCRUED = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";41", TEMP$, &
				PR_EMP_STD_ERNDED::ACCRUED, &
				MFLAG, "######.##", MVALUE)

		CASE 7%

	!++
	! Abstract:FLD007
	!	^*(07) End Date\*
	!	.p
	!	The ^*End Date\* column
	!	enters a date after which
	!	an accrual, payment or deduction record would be inactivated. A
	!	blank value in this field will result in the record being active
	!	indefinitely, unless inactivated as a result of the To date field
	!	or Accrued field being equal to the Limit field.
	!
	! Index:
	!	.x Pay/Deduction>End Date
	!	.x End Date>Pay/Deduction
	!	.x Date>Pay/Deduction
	!
	!--

			PR_EMP_STD_ERNDED::ENDDAT = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";51", TEMP$, &
				PR_EMP_STD_ERNDED::ENDDAT, &
				MFLAG, "'E", MVALUE)

		CASE 8%

	!++
	! Abstract:FLD008
	!	^*(08) Frequency\*
	!	.p
	!	The ^*Frequency\* column
	!	flags specific payrolls in
	!	which any accrual, payment or deduction will be activated.
	!	.p
	!	The frequency codes are:
	!	.LIST "*"
	!	.LE
	!	1 = First payroll of an accounting period
	!	.LE
	!	2 = Second payroll of an accounting period
	!	.LE
	!	3 = Third payroll of an accounting period
	!	.LE
	!	4 = Fouth payroll of an accounting period
	!	.LE
	!	5 = Fifth payroll of an accounting period
	!	.LE
	!	S = Special payroll
	!	.ELS
	!	.p
	!	A "Y" flag (for yes) is to be entered below each code representing
	!	the payroll period in which the record is to be activated. A "N" flag
	!	(for no) is to be entered below each code representing the payroll
	!	period in which the record is to be inactivated.
	!
	! Index:
	!	.x Pay/Deduction>Frequency
	!	.x Frequency>Pay/Deduction
	!
	!--

			PR_EMP_STD_ERNDED::FREQ = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";62", TEMP$, &
				PR_EMP_STD_ERNDED::FREQ, MFLAG, "'E", MVALUE)

		CASE 9%

	!++
	! Abstract:FLD009
	!	^*(09) Method\*
	!	.p
	!	The ^*Method\* column
	!	determines the
	!	method which is to be used to accomplish a standard accrual, payment
	!	or deduction transaction. If this is a payment, the percent
	!	of net method can not be used. Valid methods are:
	!	.B
	!	.LIST 0,"*"
	!	.LE
	!	1 = Per Hour
	!	.LE
	!	2 = Per Mile
	!	.LE
	!	3 = Percent of Gross
	!	.LE
	!	4 = Percent of Net
	!	.LE
	!	5 = Per Pay Period
	!	.le
	!	6 = Per Hour/Premium
	!	.le
	!	7 = Per Hour, Limit works per pay period.
	!	.ELS
	!
	! Index:
	!	.x Pay/Deduction>Method
	!	.x Method>Pay/Deduction
	!
	!--

			PR_EMP_STD_ERNDED::METHOD = &
				EDIT$(ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";70", TEMP$, &
				PR_EMP_STD_ERNDED::METHOD, MFLAG, "'", MVALUE, &
				MTD$(), MTDTITLE$, "007"), -1%)

		CASE 10%

	!++
	! Abstract:FLD010
	!	^*(10) User\*
	!	.p
	!	The ^*User\* column
	!	enters a user defined memo
	!	information. For example, the field may contain a bank account
	!	number to which savings deposits are to be made for the employee.
	!	.p
	!	The field will accommodate thirty characters. Though only
	!	the five (5) left-most characters will be displayed on the APD
	!	Rate screen, all characters in the field will print on reports.
	!	.P
	!	When used as for direct deposit information, the format is
	!	as follows: 2 digits for the transaction type (22=Checking,
	!	32=Savings), a space, 8 digits for the bank DFI code, a space,
	!	and 17 for the DFI Account number.
	!
	! Index:
	!	.x User Defined Memo>Pay/Deduction
	!	.x Pay/Deduction>User Defined Memo
	!
	!--

			PR_EMP_STD_ERNDED::USERDEF = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";74", TEMP$, &
				PR_EMP_STD_ERNDED::USERDEF, MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		PR_MAIN_EMP_STD_ERNDED = 0%

		SELECT MLOOP

		CASE 2%
			TEMP_RTYPE$ = PR_EMP_STD_ERNDED::RTYPE
			TEMP_RTYPE$ = "P" IF TEMP_RTYPE$ = "A"

			PR_MAIN_EMP_STD_ERNDED = FUNC_TESTENTRY( SMG_WINDOW, &
				TEMP_RTYPE$ + PR_EMP_STD_ERNDED::CODE, &
				PR_ERNDED_DEF::DESCR, &
				"PR", MLOOP, "PRG", &
				"Code", PR_MAIN_ERNDED_DEF.ID)

		CASE 9%
			IF PR_EMP_STD_ERNDED::RTYPE = "P" AND &
				PR_EMP_STD_ERNDED::METHOD = "4"
			THEN
				CALL ENTR_3MESSAGE(SCOPE, &
					"Method 4 not allowed with pay item", 0%)

				PR_MAIN_EMP_STD_ERNDED = 1%
			END IF

		END SELECT

	!
	! Set PR_EMP_STD_ERNDED_OLD value
	!
20500	CASE OPT_SETOLD
		PR_EMP_STD_ERNDED_OLD = PR_EMP_STD_ERNDED

	!
	! Restore PR_EMP_STD_ERNDED_OLD value
	!
	CASE OPT_RESETOLD
		PR_EMP_STD_ERNDED = PR_EMP_STD_ERNDED_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PR_EMP_STD_ERNDED2 = PR_EMP_STD_ERNDED

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PR_EMP_STD_ERNDED = PR_EMP_STD_ERNDED2

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!
		PR_EMP_STD_ERNDED::EMPNUM = PR_EMP_MASTER::EMPNUM

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #PR_EMP_STD_ERNDED.CH%, &
				KEY #0% GE PR_EMP_STD_ERNDED::EMPNUM + "", &
				REGARDLESS
		END SELECT

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

			IF PR_EMP_STD_ERNDED::EMPNUM = PR_EMP_MASTER::EMPNUM
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
		! new key probably passes through MVALUE, unless some
		! other means is devised.
		!
		CASE 6%
			PR_EMP_STD_ERNDED::EMPNUM = RIGHT(MVALUE, 2%)

		END SELECT
	END SELECT

28000	EXIT FUNCTION

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
