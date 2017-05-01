1	%TITLE "Employee Master File Maintenance"
	%SBTTL "PR_MAIN_DETAIL_EMP_QUERY_X"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PR_MAIN_DETAIL_EMP_QUERY_X(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

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
	!	The ^*Maintain Payroll Journal\* option in the Payroll Journal menu
	!	accesses the records in a specified payroll folder
	!	file.
	!	.p
	!	This maintenance option is ^*not\* intended for normal entry of
	!	data, but to edit or correct data which has already been entered in
	!	the Timekeeper routine, include deductions and taxes added to the file as
	!	a result of executing the Calculate Taxes and Standard Deductions
	!	routine, or check related data posted as a result of executing the
	!	Print Payroll Check routine.
	!
	! Index:
	!	.x Maintain>Payroll Journal
	!	.x Payroll Journal>Maintain
	!
	! Option:
	!
	!	PR_MAIN_DETAIL_EMP_QUERY_X$PAY
	!	PR_MAIN_DETAIL_EMP_QUERY_X$DED
	!	PR_MAIN_DETAIL_EMP_QUERY_X$CHECK
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_MAIN_DETAIL_EMP_QUERY_X/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN PR_MAIN_DETAIL_EMP_QUERY_X
	!	$ DELETE PR_MAIN_DETAIL_EMP_QUERY_X.OBJ;*
	!
	! Author:
	!
	!	10/15/87 - Kevin Handy
	!
	! Modification history:
	!
	!	05/31/88 - Aaron Redd
	!		Modified to allow R/O open of file if R/W open fails.
	!
	!	10/03/89 - Kevin Handy
	!		Put in the include for MAIN_WINDOW.COM, which
	!		had mysteriously disappeared, causing this function
	!		to not work.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/09/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/29/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/28/97 - Kevin Handy
	!		Lose unecessary external definitions
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/10/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	11/20/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "FUNC_INCLUDE:PR_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP	(PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER
	MAP	(PR_EMP_MASTER2)	PR_EMP_MASTER_CDD	PR_EMP_MASTER_OLD, PR_EMP_MASTER2

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	! This common area must be mapped in some of the MAIN programs,
	! PR_MAST_EMPLOYEE.BAS, and PR_MAST_WC_WORK.BAS.
	!
	COM (CH_PR_EMPLOYEE) &
		PR_EMP_MASTER.CH%, &
		PR_EMP_MASTER.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION MAIN_JOURNAL

	!
	! Declare data types
	!
	DECLARE LONG XPOS, YPOS

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
		SMG_WINDOW::DESCR = "Payroll Register Maintenance"
		SMG_WINDOW::NHELP = "PR_MAIN_DETAIL_EMP_QUERY_X"
		SMG_WINDOW::CHAN  = PR_EMP_MASTER.CH%
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 4%

		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 4%
		SMG_WINDOW::KNAME(0%) = "Employee-number"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		SMG_WINDOW::KNAME(1%) = "Name"
			SMG_WINDOW::KFIELD(1%, 0%) = 1%
			SMG_WINDOW::KFIELD(1%, 1%) = 2%

		SMG_WINDOW::KNAME(2%) = "Alpha_sort"
			SMG_WINDOW::KFIELD(2%, 0%) = 1%
			SMG_WINDOW::KFIELD(2%, 1%) = 3%

		SMG_WINDOW::KNAME(3%) = "Soc-sec-num"
			SMG_WINDOW::KFIELD(3%, 0%) = 1%
			SMG_WINDOW::KFIELD(3%, 1%) = 4%

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF PR_EMP_MASTER.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PR_EMP_MASTER.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PR_MAIN_DETAIL_EMP_QUERY_X = ERR
			CONTINUE 770
		END WHEN

		PR_EMP_MASTER.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
		USE
			PR_MAIN_DETAIL_EMP_QUERY_X = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PR_EMP_MASTER.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PR_EMP_MASTER.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = PR_EMP_MASTER.CH%
		WHEN ERROR IN
			RESET #PR_EMP_MASTER.CH%
			GET #PR_EMP_MASTER.CH%, REGARDLESS
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

		DATA	1,  1, "(01) Emp #", &
			2,  1, "(02) Name", &
			3,  1, "(03) Sort", &
			1, 40, "(04) Soc Sec Num", &
			0,  0, ""

		RESTORE

		READ XPOS, YPOS, XSTR$
		I% = 0%
		WHILE (XPOS <> 0%)
			I% = I% + 1%
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				XSTR$, XPOS, YPOS) &
				IF (SMG_WINDOW::HFLAG(I%) AND 2%) = 0%
			READ XPOS, YPOS, XSTR$
		NEXT

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)


	!
	! More options
	!
	CASE OPT_OPTLIST
		MVALUE = "Find Next Restore View Help Pay Ded Check eXit"

	!
	! Handle additional options
	!
	CASE OPT_MOREMENU
		SELECT EDIT$(MVALUE, -1%)

		!
		! Pay
		!
		CASE "PAY"
			PR_MAIN_DETAIL_EMP_QUERY_X = &
				MAIN_JOURNAL(PR_MAIN_TRN_PAY_X.ID, "")

	!++
	! Abstract:PAY
	!	^*Pay\*
	!	.b
	!	.lm +5
	!	The ^*Pay\* function
	!	accesses the records entered in the Timekeeper option. Pay records
	!	may be erased or edited in the Payroll Journal Maintenance option.
	!	Though pay records in the Payroll Journal would normally be added by
	!	accessing the Timekeeper option, pay records may be added in the
	!	Payroll Journal Maintenance option.
	!	.b
	!	Fields in the "Pay" record of the Payroll Journal include:
	!	.table 3,25
	!	.te
	!	Payroll End Date
	!	.te
	!	Employee Skill
	!	.te
	!	Union
	!	.te
	!	Location
	!	.te
	!	Department
	!	.te
	!	Work Center
	!	.te
	!	Account
	!	.te
	!	Sub Account
	!	.te
	!	Operation
	!	.te
	!	Pay Code
	!	.te
	!	Tax Package Code
	!	.te
	!	Pay Type
	!	.te
	!	Rate Type
	!	.te
	!	Hourly Rate
	!	.te
	!	Overtime Factor
	!	.te
	!	Unit Rate
	!	.te
	!	Regular Hours
	!	.te
	!	Overtime Hours
	!	.te
	!	Number of Units
	!	.te
	!	Gross
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Pay>Payroll Journal
	!	.x Payroll Journal>Pay
	!
	!--
		!
		! Deductions
		!
		CASE "DED"
			PR_MAIN_DETAIL_EMP_QUERY_X = &
				MAIN_JOURNAL(PR_MAIN_TRN_DED.ID, "")

	!++
	! Abstract:DED
	!	^*Deduction\*
	!	.b
	!	.lm +5
	!	The ^*Deduction\* function
	!	accesses the Deduction Journal where non-standard deductions
	!	are entered. A "non-standard deduction" refers to the ^&frequency\& of a
	!	deduction rather than the ^&type\& of deduction.
	!	.b
	!	Additional withholding taxes may also be entered in the Deduction
	!	Journal.
	!	.b
	!	Manually calculated taxes withheld in the event of a manual payoff
	!	can also be entered or edited in the Deduction Journal.
	!	.b
	!	The fields in the Deduction Journal include:
	!	.table 3,25
	!	.te
	!	Employee Number
	!	.te
	!	Employee Name (system displayed)
	!	.te
	!	Type
	!	.te
	!	Code
	!	.te
	!	Amount
	!	.te
	!	Tax Code
	!	.end table
	!	The last field is used only when a deduction relates to State or
	!	local withholding taxes; otherwise, the field is automatically bypassed.
	!
	! Index:
	!	.x Functions>dedUction
	!	.x Payroll>Deductions
	!	.x Deductions>Payroll
	!
	!--

		!
		! Check
		!
		CASE "CHECK"
			PR_MAIN_DETAIL_EMP_QUERY_X = &
				MAIN_JOURNAL(PR_MAIN_TRN_CHECK.ID, "")

	!++
	! Abstract:CHECK
	!	^*Check\*
	!	.b
	!	.lm +5
	!	The ^*Check\* function
	!	accesses the Check Journal where data relative to manually
	!	prepared payroll checks can be entered.
	!	.b
	!	A record for an employee with a check number in the Check Journal
	!	will preclude a check being written for the subject employee during
	!	the execution of the payroll check printing routine.
	!	.lm -5
	!
	! Index:
	!	.x Check>Function
	!	.x Pay Journal>Check>Function
	!	.x Functions>checK
	!
	!--

		END SELECT

	!
	! Handle finishing various options specially
	!
	CASE OPT_AFTEROPT

		SELECT SCOPE::PRG_ITEM

		!
		! Change records
		!
		CASE "Change", "Blank", "Initialize"
			!
			! Change line items to match new header
			! if the key was changed.
			!
			! The original record must be the one in the
			! MAP for this to be able to work.  The new
			! key is passed through the QUERY$ variable.
			!
			IF PR_EMP_MASTER_OLD::EMPNUM <> PR_EMP_MASTER::EMPNUM
			THEN
				TEMP$ = PR_EMP_MASTER::EMPNUM + ""
				PR_EMP_MASTER = PR_EMP_MASTER_OLD
				PR_MAIN_DETAIL_EMP_QUERY_X = &
					MAIN_JOURNAL(PR_MAIN_TRN_PAY_X.ID, &
					"C" + TEMP$)
				PR_MAIN_DETAIL_EMP_QUERY_X = &
					MAIN_JOURNAL(PR_MAIN_TRN_DED.ID, &
					"C" + TEMP$)
				PR_MAIN_DETAIL_EMP_QUERY_X = &
					MAIN_JOURNAL(PR_MAIN_TRN_CHECK.ID, &
					"C" + TEMP$)
			END IF

		!
		! Erase record
		!
		CASE "Erase"
			!
			! Erase any line items under the header
			!
			PR_MAIN_DETAIL_EMP_QUERY_X = &
				MAIN_JOURNAL(PR_MAIN_TRN_PAY_X.ID, "E")
			PR_MAIN_DETAIL_EMP_QUERY_X = &
				MAIN_JOURNAL(PR_MAIN_TRN_DED.ID, "E")
			PR_MAIN_DETAIL_EMP_QUERY_X = &
				MAIN_JOURNAL(PR_MAIN_TRN_CHECK.ID, "E")

		END SELECT


	!
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user, display data,
	! set defaults, and return the data back according to MFLAG.
	!
20200	CASE OPT_ENTRY

		TEMP$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View "

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

		SCOPE::SCOPE_EXIT = 0%

 E0Loop:	SELECT MLOOP

		CASE 1%
	!++
	!
	! Abstract:FLD001
	!	.ts 55
	!	^*(01) Employee _#	10 Characters\*
	!	.b
	!	.lm +5
	!	The ^*Employee _#\* field will accommodate up to ten (10) alphanumeric
	!	characters, providing a field large enough to use Social Security
	!	numbers as employee numbers if desired. The format is user defined.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Employee>Number
	!
	!--
			PR_EMP_MASTER::EMPNUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "1;15", TEMP$, &
				PR_EMP_MASTER::EMPNUM, MFLAG, "'E", MVALUE)

		CASE 2%
	!++
	!
	! Abstract:FLD002
	!	.ts 55
	!	^*(02) Name	30 Characters\*
	!	.b
	!	.lm +5
	!	The ^*Name\* field in the Employee Master record will
	!	accommodate up to thirty (30) characters. The employee name
	!	should be entered first name first. (The Sort field is provided
	!	in order that alphabetical sorts can be accomplished.)
	!	.b
	!	The name entered in this field will print on payroll checks,
	!	quarterly payroll reports and W-2's.
	!	.lm -5
	!
	! Index:
	!	.x Employee>Name
	!
	!--
			PR_EMP_MASTER::EMPNAME = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "2;15", TEMP$, &
				PR_EMP_MASTER::EMPNAME, MFLAG, "'E", MVALUE)

		CASE 3%
	!++
	!
	! Abstract:FLD003
	!	.ts 55
	!	^*(03) Sort	14 Characters\*
	!	.b
	!	.lm +5
	!	The ^*Sort\* field is designed to contain the employee name with the
	!	last name first. Since the field contains fourteen (14) characters
	!	only, it is recommended that special characters, such as a comma after
	!	the last name, not be used.
	!	.lm -5
	!
	! Index:
	!	.x Employee>Name>Alpha Sort
	!
	!--
			PR_EMP_MASTER::SORT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "3;15", TEMP$, &
				PR_EMP_MASTER::SORT, MFLAG, "'E", MVALUE)

		CASE 4%
	!++
	!
	! Abstract:FLD004
	!	.ts 55
	!	^*(04) Social Security Number	9 Digits\*
	!	.b
	!	.lm +5
	!	The employee's nine (9) digit social security number is be entered
	!	in the ^*Social Security Number\* field without entering the associated
	!	dashes.
	!	.lm -5
	!
	! Index:
	!	.x Employee>Social Security Number
	!
	!--
			PR_EMP_MASTER::SSN = ENTR_3SSN(SCOPE, &
				SMG_WINDOW::WNUMBER, "1;58", TEMP$, &
				PR_EMP_MASTER::SSN, MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP$

20300	CASE OPT_TESTENTRY

		PR_MAIN_DETAIL_EMP_QUERY_X = 0%

20500	CASE OPT_SETOLD
		PR_EMP_MASTER_OLD = PR_EMP_MASTER

	CASE OPT_RESETOLD
		PR_EMP_MASTER = PR_EMP_MASTER_OLD

	CASE OPT_SETDEFAULT
		PR_EMP_MASTER2 = PR_EMP_MASTER

	CASE OPT_RESETDEFAULT
		PR_EMP_MASTER = PR_EMP_MASTER2

	CASE OPT_VIEW
		SELECT MLOOP

		CASE 1%
			MVALUE = " Emp number  Name                 " + &
				"Sort           SSN"

		CASE 2%
			MVALUE = "013,034,049"

		CASE 3%
			MVALUE = PR_EMP_MASTER::EMPNUM + " " + &
				LEFT(PR_EMP_MASTER::EMPNAME, 20%) + " " + &
				LEFT(PR_EMP_MASTER::SORT, 14%) + " " + &
				LEFT(PR_EMP_MASTER::SSN, 13%)
		END SELECT

	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #PR_EMP_MASTER.CH%, &
				KEY #0% GE PR_EMP_MASTER::EMPNUM + "", &
				REGARDLESS

		CASE 1%
			FIND #PR_EMP_MASTER.CH%, &
				KEY #1% GE PR_EMP_MASTER::EMPNAME + "", &
				REGARDLESS

		CASE 2%
			FIND #PR_EMP_MASTER.CH%, &
				KEY #2% GE PR_EMP_MASTER::SORT + "", &
				REGARDLESS

		CASE 3%
			FIND #PR_EMP_MASTER.CH%, &
				KEY #3% GE PR_EMP_MASTER::SSN + "", &
				REGARDLESS
		END SELECT

	END SELECT

	EXIT FUNCTION

29000	!*******************************************************************
	! Error Traping

	ON ERROR GO BACK

32767	END FUNCTION
