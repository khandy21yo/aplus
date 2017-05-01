1	%TITLE "PR Pay Time Keeper 01 Journal Maintenance"
	%SBTTL "PR_MAIN_TRN_TK01_PAY"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PR_MAIN_TRN_TK01_PAY(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	^*TIMKEP - Timekeeper\*
	!	.p
	!	The ^*Timekeeper\* option
	!	accesses the Pay Journal, Deduction Journal and Check Journal.
	!	These journals provide the means to enter the employees' time worked,
	!	units produced, non-standard deductions and after-the-fact manual
	!	pay-off data.
	!
	! Index:
	!	.x Timekeeper Journal
	!	.x Journal>Timekeeper
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_MAIN_TRN_TK01_PAY/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN PR_MAIN_TRN_TK01_PAY
	!	$ DELETE PR_MAIN_TRN_TK01_PAY.OBJ;*
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
	!	04/03/89 - Kevin Handy
	!		Added one more digit to the units field.
	!
	!	01/18/90 - Kevin Handy
	!		Moved initilization of List Choices arrays into
	!		in initilization section to improve speed of
	!		entry (Decrease CPU time).
	!
	!	10/25/90 - Kevin Handy
	!		Added pay type 'X' (eXcess).
	!
	!	11/06/90 - Kevin Handy
	!		Fixed bugs RE 10/25/90.
	!
	!	03/18/91 - Kevin Handy
	!		Added eval_date field to pr_read_rate.
	!
	!	03/18/91 - Kevin Handy
	!		Modified to print flag on screen if the evaluation
	!		date passes.  Moved initilization of fields from
	!		OPT_ENTER case 1 to OPT_SETDEFAULT.
	!
	!	12/18/91 - Kevin Handy
	!		Added "A" type to PR_PAY file.
	!
	!	04/23/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	06/12/92 - Kevin Handy
	!		Clean up (check)
	!
	!	10/07/92 - Kevin Handy
	!		Modified OPT_TESTENTRY to display all fields
	!		after changing them, in case of a default
	!		being set.
	!
	!	10/07/92 - Kevin Handy
	!		Modified not to pull information into individual
	!		fields from OPT_TESTENTRY(1) when hard defaults
	!		are set.
	!
	!	10/07/92 - Kevin Handy
	!		Added code to test subaccount.
	!
	!	10/08/92 - Kevin Handy
	!		Added code to look up total REG and OVT hours
	!		in this folder and in the final folder,
	!		so user will n=know when they go into overtime.
	!
	!	04/12/93 - Kevin Handy
	!		Modified so that 10/08/92 change will ignore
	!		accrual records.
	!
	!	05/28/93 - Kevin Handy
	!		Modified so that the user can decide if they want
	!		to allow undefined/closed subaccount numbers.
	!		This is done through "PR_ALLOW","SUBACC" in the
	!		SET file.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	07/18/95 - Kevin Handy
	!		Initialize batch number.
	!
	!	05/09/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/23/96 - Kevin Handy
	!		Added code to check subaccounts, so that
	!		blank subaccounts are not allowed for gl
	!		accounts defines as belonging to JC, unless
	!		set "ALLOW","BLANKSUB" is set.
	!
	!	05/29/96 - Kevin Handy
	!		Modified blank subaccount check to allow
	!		for wildcard account numbers id SB_ACCOUNT.
	!
	!	08/06/96 - Kevin Handy
	!		Fixed so that "PR_ALLOW","SUBACCT" does not
	!		require the allow in the field option also.
	!		I.e. make it an either/or option.
	!
	!	11/17/97 - Kevin Handy
	!		Ring the bell when an undefined SUBACCOUNT is used.
	!		Clean up source code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/20/98 - Kevin Handy
	!		Lose unused SLINE field in array, not used.
	!
	!	03/10/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	06/22/99 - Kevin Handy
	!		Modifications to use ENTR_3TIMEKEEPER
	!
	!	10/20/99 - Kevin Handy
	!		Modified employee total display at bottom to only
	!		include those items that match this batch entry
	!		number, and to display the gross also.
	!
	!	03/06/2000 - Kevin Handy
	!		Ring bell on blank sunaccounts also.
	!
	!	03/14/2000 - Kevin Handy
	!		Use WHEN ERROR IN for error trapping
	!		Add some REGARDLESS clauses
	!		Lose redundent GOTOs inside of WHEN clause in CalcHours
	!
	!	03/22/2000 - Kevin Handy
	!		Added EFF_DATE parameter to PR_READ_RATE
	!
	!	04/03/2000 - Kevin Handy
	!		Trap more IO errors
	!
	!	04/13/2001 - Kevin Handy
	!		Put folder date on title bar
	!
	!	12/05/2001 - Kevin Handy
	!		Respect default values in add. (KBJ)
	!
	!	07/19/2005 - Kevin Handy
	!		If they change the rate code in add, update
	!		the rate, rate type fields. Fix so the rate
	!		is calculated on the actual rate code if it
	!		is given.
	!
	!	07/27/2005 - Kevin Handy
	!		More fun games trying to get everyone happy
	!		with how rates are initialized. Now we load
	!		up a default rate if we can't read by code.
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

	%INCLUDE "SOURCE:[SB.OPEN]SB_ACCOUNT.HB"
	MAP (SB_ACCOUNT)	SB_ACCOUNT_CDD		SB_ACCOUNT

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	DECLARE SB_SUBACCOUNT_CDD SB_SUBACCOUNT_READ

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_SET.HB"
	DECLARE UTL_SET_CDD	UTL_SET_READ

	MAP (PR_DETAIL) &
		BATCH_NO$ = 8%, &
		END_DATE$ = 8%, &
		BATCH_ENTRY$ = 2%, &
		LOCATION$ = 4%, &
		ECTITLE$ = 20%, &
		EC$(6%) = 20%, &
		PTTITLE$ = 20%, &
		PT$(5%) = 20%

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_PR_TRN_PAY) &
		PR_TRN_PAY.CH%, &
		PR_TRN_PAY.READONLY%

	COM (CH_SB_SUBACCOUNT) &
		SB_SUBACCOUNT.CH%

	COM (CH_PR_TRN_PAY_FINAL) &
		PR_TRN_PAY_FINAL.CH%

	COM (CH_SB_ACCOUNT) &
		SB_ACCOUNT.CH%

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
	EXTERNAL LONG   FUNCTION MAIN_WINDOW
	EXTERNAL LONG	FUNCTION MAIN_JOURNAL
	EXTERNAL LONG	FUNCTION FUNC_TESTENTRY
	EXTERNAL LONG	FUNCTION SB_EXAM_SUBACCOUNT
	EXTERNAL LONG	FUNCTION READ_35SET

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Pay Journal " + PRNT_DATE(BATCH_NO$, 8%)
		SMG_WINDOW::NHELP = "PR_MAIN_TRN_TK01_PAY"
		SMG_WINDOW::HSIZE = 130%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 15%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::TOPLIN = 3%
		SMG_WINDOW::BOTLIN = 17%
		SMG_WINDOW::LINREC = 3%

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
			PR_MAIN_TRN_TK01_PAY = ERR
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
			PR_MAIN_TRN_TK01_PAY = ERR
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
			CONTINUE 800
		END WHEN

800		IF (SB_ACCOUNT.CH% <= 0%)
		THEN
			WHEN ERROR IN
				%INCLUDE "SOURCE:[SB.OPEN]SB_ACCOUNT.OPN"
			USE
				CONTINUE 32767
			END WHEN
		END IF

	!
	! Display the background
	!
20100	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"  02                 03         04       05     " + &
			"06 07 08    09      10     11         12      " + &
			"   13         14         15          ", &
			1%, 1%,, SMG$M_REVERSE)
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"  Account            Sub Acct   Oper     Dept   " + &
			"EC PT RT  Hrly Rate OTF  Unit Rate Reg  Hrs   " + &
			"OT Hours      Units   Gross          ", &
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

			PR_MAIN_TRN_TK01_PAY = &
				MAIN_JOURNAL(PR_MAIN_TRN_TK01_DED.ID, "")

		CASE "checK"

			PR_MAIN_TRN_TK01_PAY = &
				MAIN_JOURNAL(PR_MAIN_TRN_TK01_CHECK.ID, "")

		END SELECT


	!
	! Extra display stuff
	!
	CASE OPT_DISPLAY

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		!
		! Generate totals
		!
		TOTAL_REG_HR = 0.0
		TOTAL_OVT_HR = 0.0
		TOTAL_PIECE = 0.0
		TOTAL_GROSS = 0.0

		FOR I% = 1% TO SMG_WINDOW::TOTREC
			TOTAL_REG_HR = TOTAL_REG_HR + RARRAY(I%)::REG_HR
			TOTAL_OVT_HR = TOTAL_OVT_HR + RARRAY(I%)::OVT_HR
			TOTAL_PIECE = TOTAL_PIECE + RARRAY(I%)::PIECE
			TOTAL_GROSS = TOTAL_GROSS + RARRAY(I%)::GROSS
		NEXT I%

		!
		! Display totals
		!
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"Number of lines" + &
			FORMAT$(SMG_WINDOW::TOTREC, "####") + &
			SPACE$(58%) + "TOTAL  " + &
			FORMAT$(TOTAL_REG_HR, "#####.##   ") + &
			FORMAT$(TOTAL_OVT_HR, "#####.##   ") + &
			FORMAT$(TOTAL_PIECE, "#####.##  ") + &
			FORMAT$(TOTAL_GROSS, "###,###,###.##"), &
			SMG_WINDOW::VSIZE, 1%, , SMG$M_REVERSE)

		!
		! Paint lines on screen
		!
		FOR I% = 1% TO 6%
			A% = VAL%(MID("068,072,083,093,104,115", &
				I% * 4% - 3%, 3%))

			SMG_STATUS% = SMG$DRAW_LINE(SMG_WINDOW::WNUMBER, &
				1%, A%, SMG_WINDOW::BOTLIN, A%)
		NEXT I%

		!
		! Draw horizontal lines
		!
		SMG_STATUS% = SMG$DRAW_LINE(SMG_WINDOW::WNUMBER, &
			SMG_WINDOW::CURLIN + 2%, 1%, &
			SMG_WINDOW::CURLIN + 2%, 130%)

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

	!
	! Enter/Display/Default
	!
20200	CASE OPT_ENTRY
		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

		SCOPE::SCOPE_EXIT = 0%

		TFLAG% = MFLAG
		TVALUE$ = MVALUE

		SELECT MLOOP
		CASE 8% TO 14%

			IF PR_TRN_PAY::PTYPE = "O"
			THEN
				MFLAG = MFLAG OR 33%
				MFLAG = MFLAG AND NOT 64%
				MVALUE = ""
			END IF

		CASE 15%

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
	!	them when dealing with the company.
	!	It may contain up to ten (10) alphanumeric characters.
	!
	! Index:
	!	.x Employee Number>Timekeeper Journal
	!	.x Timekeeper Journal>Employee Number
	!
	!--
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				"(01) Emp #", SMG_WINDOW::CURLIN, 3%)

			XLINE$ = NUM1$(SMG_WINDOW::CURLIN)
 E0Loop:		PR_TRN_PAY::EMPNUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";13", TEMP$, &
				PR_TRN_PAY::EMPNUM, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(PR_MAIN_TK_EMP_QUERY.ID, &
					"VX ") = 1%)
				THEN
					PR_TRN_PAY::EMPNUM = &
						PR_EMP_MASTER::EMPNUM
				END IF

				SCOPE::SCOPE_EXIT = 0%
				GOTO E0Loop
			END IF

		CASE 2%
	!++
	!
	! Abstract:FLD002
	!	^*(02) Account\*
	!	.p
	!	The ^*Account\* field refers to the
	!	General Ledger account number which relates to the specific record.
	!
	! Index:
	!	.x Account>Timekeeper Journal
	!	.x Timekeeper Journal>Account
	!
	!--
 E0Loop1:		XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 1%)
			PR_TRN_PAY::ACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";3", TEMP$, &
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

		CASE 3%
	!++
	!
	! Abstract:FLD003
	!	^*(03) Sub Account\*
	!	.p
	!	The ^*Sub Account\* field refers to
	!	the sub-account related to the specific record.
	!
	! Index:
	!	.x Sub Account>Timekeeper Journal
	!	.x Timekeeper Journal>Sub Account
	!
	!--
			XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 1%)
			PR_TRN_PAY::SUBACC = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";22", TEMP$, &
				PR_TRN_PAY::SUBACC, MFLAG, "'E", MVALUE)

		CASE 4%
	!++
	!
	! Abstract:FLD004
	!	^*(04) Operation\*
	!	.p
	!	The ^*Operation\* field refers to
	!	the code for the operation or task related to the specific record.
	!
	! Index:
	!	.x Operation>Timekeeper Journal
	!	.x Timekeeper Journal>Operation
	!
	!--
			XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 1%)
			PR_TRN_PAY::OPER = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";33", TEMP$, &
				PR_TRN_PAY::OPER, MFLAG, "'E", MVALUE)

		CASE 5%
	!++
	!
	! Abstract:FLD005
	!	^*(05) Department\*
	!	.p
	!	The ^*Department\* field refers to the
	!	department code relative to the specific record.
	!
	! Index:
	!	.x Department>Timekeeper Journal
	!	.x Timekeeper Journal>Department
	!
	!--
			XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 1%)
			PR_TRN_PAY::DEPT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";42", TEMP$, &
				PR_TRN_PAY::DEPT, MFLAG, "'E", MVALUE)

		CASE 6%
	!++
	!
	! Abstract:FLD006
	!	^*(06) Pay Code\*
	!	.p
	!	The ^*Pay Code\* field refers to the
	!	specific type or kind of earnings represented in the record, i.e.
	!	a code for regular taxable earnings, bonuses, commission, etc.
	!
	! Index:
	!	.x Pay Code>Timekeeper Journal
	!	.x Timekeeper Journal>Pay Code
	!
	!--
 E0Loop2:		XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 1%)

			PR_TRN_PAY::CODE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";49", TEMP$, &
				PR_TRN_PAY::CODE, MFLAG, "'E", MVALUE)


			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(PR_MAIN_ERNDED_DEF.ID, &
					"V0P") = 1%)
				THEN
					PR_TRN_PAY::CODE = PR_ERNDED_DEF::CODE
				END IF

				SCOPE::SCOPE_EXIT = 0%
				GOTO E0Loop2
			END IF

		CASE 7%
	!++
	!
	! Abstract:FLD007
	!	^*(07) Pay Type\*
	!	.p
	!	The ^*Pay Type\* field refers to the
	!	type or kind of payment which is being made to an employee in
	!	reference to a particular record. Valid types are:
	!	.lm +5
	!	.ls 0, "o"
	!	.b
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
	!	.x Pay Type>Timekeeper Journal
	!	.x Timekeeper Journal>Pay Type
	!
	!--
			XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 1%)
			PR_TRN_PAY::PTYPE = EDIT$(ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";52", TEMP$, &
				PR_TRN_PAY::PTYPE, MFLAG, "'", MVALUE, &
				PT$(), PTTITLE$, "005"), -1%)

		CASE 8%
	!++
	!
	! Abstract:FLD008
	!	^*(08) Rate Type\*
	!	.p
	!	The ^*Rate Type\* field refers to the
	!	type of rate for which an employee is being paid. Valid rate types
	!	can be viewed by pressing ^*<List Choices>\* and are:
	!	.b
	!	.lm +5
	!	.ls 0,"*"
	!	.LE
	!	H = Hourly Rate (or time related rate)
	!	.LE
	!	S = Salary
	!	.LE
	!	P = Piece Rate
	!	.LE
	!	M = Mileage
	!	.ELS
	!
	! Index:
	!	.x Rate Type>Timekeeper Journal
	!	.x Timekeeper Journal>Rate Type
	!
	!--
			XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 1%)
			PR_TRN_PAY::RTYPE = EDIT$(ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";55", &
				TEMP$, PR_TRN_PAY::RTYPE, MFLAG, "'", &
				MVALUE, EC$(), ECTITLE$, "005"), -1%)

		CASE 9%
	!++
	!
	! Abstract:FLD009
	!	^*(09) Hourly Rate\*
	!	.p
	!	The ^*Hourly Rate\* field refers to the
	!	appropriate hourly rate related to a specific record.
	!	.note
	!	If the employee's evaluation date is past the folder date,
	!	then the evaluation date will be shown above the hourly
	!	rate (in blinking bold).
	!	.end note
	!
	! Index:
	!	.x Hourly Rate>Timekeeper Journal
	!	.x Rate>Hourly>Timekeeper Journal
	!	.x Timekeeper Journal>Hourly Rate
	!
	!--
			XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 1%)
			PR_TRN_PAY::HOUR_RATE = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";58", &
				TEMP$, PR_TRN_PAY::HOUR_RATE * 1.0, &
				MFLAG, "##,###.###", MVALUE)

		CASE 10%
	!++
	!
	! Abstract:FLD010
	!	^*(10) Overtime Factor\*
	!	.p
	!	The ^*Overtime Factor\* field refers to
	!	an overtime hourly pay rate expressed as a percentage of an hourly
	!	rate. Hence, an overtime rate verbally expressed as "time and a half"
	!	is expressed in this field as "150".
	!
	! Index:
	!	.x Overtime Factor>Timekeeper Journal
	!	.x Timekeeper Journal>Overtime Factor
	!
	!--
			XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 1%)
			PR_TRN_PAY::FACTOR = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";69", &
				TEMP$, PR_TRN_PAY::FACTOR * 1.0, &
				MFLAG, "###", MVALUE)

		CASE 11%
	!++
	!
	! Abstract:FLD011
	!	^*(11) Unit Rate\*
	!	.p
	!	The ^*Unit Rate\* field enters the pay rate for each
	!	unit completed. This field is needed which the employee is payed by the number
	!	of units completed.
	!
	! Index:
	!	.x Unit Rate>Timekeeper Journal
	!	.x Timekeeper Journal>Unit Rate
	!
	!--
			XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 1%)
			PR_TRN_PAY::PIECE_RATE = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";73", &
				TEMP$, PR_TRN_PAY::PIECE_RATE * 1.0, &
				MFLAG, "#,###.####", MVALUE)

		CASE 12%
	!++
	!
	! Abstract:FLD012
	!	^*(12) Regular Hours\*
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
	!	.x Hours>Regular>Timekeeper Journal
	!	.x Regular>Hours>Timekeeper Journal
	!	.x Timekeeper Journal>Hours>Regular
	!
	!--
			XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 1%)
			PR_TRN_PAY::REG_HR = ENTR_3TIMEKEEPER(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";84", TEMP$, &
				PR_TRN_PAY::REG_HR * 1.0, MFLAG, "#,###.##", &
				MVALUE)

		CASE 13%
	!++
	!
	! Abstract:FLD013
	!	^*(13) Overtime Hours\*
	!	.p
	!	The ^*Overtime Hours\* field refers to
	!	the number of overtime hours worked by an employee in reference to
	!	the specific record.
	!	.p
	!	The field will accommodate an entry to the nearest 1/100th of
	!	an hour.
	!
	! Index:
	!	.x Overtime>Hours>Timekeeper Journal
	!	.x Timekeeper Journal>Overtime>Hours
	!	.x Hours>Overtime>Timekeeper Journal
	!	.x Timekeeper Journal>Hours>Overtime
	!
	!--
			XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 1%)
			PR_TRN_PAY::OVT_HR = ENTR_3TIMEKEEPER(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";95", TEMP$, &
				PR_TRN_PAY::OVT_HR * 1.0, MFLAG, "#,###.##", &
				MVALUE)

		CASE 14%
	!++
	!
	! Abstract:FLD014
	!	^*(14) Number of Units\*
	!	.p
	!	The ^*Number of Units\* refers to pieces
	!	produced or miles driven, etc. for which an employee is being credited.
	!	.p
	!	The field will accommodate a value to the nearest 1/100th of a
	!	unit.
	!
	! Index:
	!	.x Units>Timekeeper Journal
	!	.x Timekeeper Journal>Units
	!
	!--
			XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 1%)
			PR_TRN_PAY::PIECE = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";105", TEMP$, &
				PR_TRN_PAY::PIECE * 1.0, MFLAG, &
				"##,###.##", MVALUE)

		CASE 15%
	!++
	!
	! Abstract:FLD015
	!	^*(15) Gross\*
	!	.p
	!	Displays the amount of gross pay.
	!	.note
	!	This field is calculated based on the rates and units entered for the record,
	!	and cannot be directly changed.
	!	.end note
	!
	! Index:
	!	.x Gross>Timekeeper Journal
	!	.x Timekeeper Journal>Gross
	!
	!--
			IF PR_TRN_PAY::PTYPE = "P"
			THEN
				GOSUB CalcGross
			END IF

			XLINE$ = NUM1$(SMG_WINDOW::CURLIN + 1%)
			PR_TRN_PAY::GROSS = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";117", TEMP$, &
				PR_TRN_PAY::GROSS * 1.0, MFLAG, &
				"###,###,###.##", MVALUE)

		END SELECT

		MFLAG = TFLAG%
		MVALUE = TVALUE$
		SCOPE::PRG_ITEM = TEMP1$


	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		TEMP_FLAG%, PR_MAIN_TRN_TK01_PAY = 0%

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
			PR_EMP_MASTER::RATE_TYPE = &
				STRING$(LEN(PR_TRN_PAY::RTYPE), 63%)
			PR_EMP_MASTER::RATE_CDE = &
				STRING$(LEN(PR_TRN_PAY::CODE), 63%)

			PR_MAIN_TRN_TK01_PAY = FUNC_TESTENTRY(SMG_WINDOW, &
				PR_TRN_PAY::EMPNUM, &
				PR_EMP_MASTER::EMPNAME, &
				"PR", MLOOP, "PRG", &
				"Employee Number", PR_MAIN_TK_EMP_QUERY.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(PR_EMP_MASTER::EMPNAME, 25%) + &
				"          ", &
				SMG_WINDOW::CURLIN, 33%,, SMG$M_BOLD)

			IF TEMP_FLAG% = 0%
			THEN
				IF EDIT$(PR_EMP_MASTER::TERMDAY, -1%) > &
					"00000000"
				THEN
					!
					! Has the employee been terminated.
					!
					TEMP_FLAG%, PR_MAIN_TRN_TK01_PAY = 1%
					CALL ENTR_3MESSAGE(SCOPE, &
						"Employee has a termination date ", 0%)
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
					PR_MAIN_TRN_TK01_PAY = 1%
					CALL ENTR_3MESSAGE(SCOPE, &
						"Employee Does not work in this location ", 0%)
				END IF
			END IF

			!
			! Set defaults is mvalue = "add"
			!
			IF EDIT$(MVALUE, -1%) = "ADD"
			THEN
				PR_TRN_PAY::EMP_SKILL = &
					PR_EMP_MASTER::EMP_SKILL
				PR_TRN_PAY::EMP_GRADE = &
					PR_EMP_MASTER::EMP_GRADE
				PR_TRN_PAY::ACCT = PR_EMP_MASTER::ACCT &
					IF (SMG_WINDOW::HFLAG(2%) AND 3%) = 0%
				PR_TRN_PAY::SUBACC = &
					PR_EMP_MASTER::SUBACC &
					IF (SMG_WINDOW::HFLAG(3%) AND 3%) = 0%
				PR_TRN_PAY::OPER = PR_EMP_MASTER::OPER &
					IF (SMG_WINDOW::HFLAG(4%) AND 3%) = 0%
				PR_TRN_PAY::LOCATION = &
					PR_EMP_MASTER::LOCATION
				PR_TRN_PAY::DEPT = PR_EMP_MASTER::DEPT &
					IF (SMG_WINDOW::HFLAG(5%) AND 3%) = 0%
				PR_TRN_PAY::WORK_CENTER = &
					PR_EMP_MASTER::WORK_CENTER
				PR_TRN_PAY::UNION = PR_EMP_MASTER::UNION
				PR_TRN_PAY::TAX_PKG = PR_EMP_MASTER::TAX_PKG

				PR_TRN_PAY::PTYPE = "P"

				GOSUB CalcHours
			END IF

			V% = PR_MAIN_TRN_TK01_PAY(SMG_WINDOW, &
				OPT_ENTRY, I%, 1%, MVALUE) &
				FOR I% = 1% TO SMG_WINDOW::NITEMS

		CASE 2%
			!
			! Is the input defined?
			!
			PR_MAIN_TRN_TK01_PAY = FUNC_TESTENTRY(SMG_WINDOW, &
				PR_TRN_PAY::ACCT, &
				GL_CHART::DESCR, &
				"PR", MLOOP, "PRG", &
				"Account", GL_MAIN_CHART.ID)

		CASE 3%
			!
			! Check subaccount (if they enter)
			!
			IF TRM$(PR_TRN_PAY::SUBACC) = ""
			THEN
				GOSUB TestBlankSubaccount
			ELSE
				V% = SB_EXAM_SUBACCOUNT("J", &
					PR_TRN_PAY::SUBACC, &
					SB_SUBACCOUNT_READ)

				IF (SB_SUBACCOUNT.CH% > 0%)
				THEN
					IF V% <> CMC$_NORMAL
					THEN
						TEXT$ = "Subaccount is Undefined! "
						GOSUB TestSubaccount
					ELSE
						IF SB_SUBACCOUNT_READ::SSTATUS = "C"
						THEN
							TEXT$ = "Subaccount is Closed! "
							GOSUB TestSubaccount
						END IF
					END IF
				END IF
			END IF


		CASE 4%
 !			IF EDIT$(SCOPE::PRG_ITEM, -1%) = "ADD" OR &
 !				EDIT$(SCOPE::PRG_ITEM, -1%) = "CHANGE"
 !			THEN
				!
				! Force in default code if none entered
				!
				IF PR_TRN_PAY::RTYPE = "" AND &
					PR_TRN_PAY::CODE = ""
				THEN
					PR_TRN_PAY::RTYPE = &
						PR_EMP_MASTER::RATE_TYPE
					PR_TRN_PAY::CODE = &
						PR_EMP_MASTER::RATE_CDE
				END IF

				!
				! Try to read rate based on entered code
				!
				CALL PR_READ_RATE_CODE(PR_TRN_PAY::EMPNUM, &
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
				! ? Try to read rate based on employee
				! preferences.
				!
				IF (PR_TRN_PAY::HOUR_RATE = 0.0 AND &
					PR_TRN_PAY::PIECE_RATE = 0.0)
				THEN
					CALL PR_READ_RATE_CODE(PR_TRN_PAY::EMPNUM, &
						PR_TRN_PAY::OPER, &
						PR_TRN_PAY::PR_END_DATE, &
						PR_EMP_MASTER::RATE_TYPE, &
						PR_EMP_MASTER::RATE_CDE, &
						PR_TRN_PAY::HOUR_RATE, &
						PR_TRN_PAY::PIECE_RATE, &
						PR_TRN_PAY::FACTOR, &
						STDEFF, &
						EVALDATE$, &
						EFF_DATE$)
				END IF

				!
				! ? Try to read anything.
				!
				IF (PR_TRN_PAY::HOUR_RATE = 0.0 AND &
					PR_TRN_PAY::PIECE_RATE = 0.0)
				THEN
					TEMP1$ = PR_TRN_PAY::RTYPE + ""
					TEMP2$ = PR_TRN_PAY::CODE + ""

					CALL PR_READ_RATE(PR_TRN_PAY::EMPNUM, &
						PR_TRN_PAY::OPER, &
						PR_TRN_PAY::PR_END_DATE, &
						TEMP1$, &
						TEMP2$, &
						PR_TRN_PAY::HOUR_RATE, &
						PR_TRN_PAY::PIECE_RATE, &
						PR_TRN_PAY::FACTOR, &
						STDEFF, &
						EVALDATE$, &
						EFF_DATE$)

					PR_TRN_PAY::RTYPE = TEMP1$ &
						IF PR_TRN_PAY::RTYPE = ""
					PR_TRN_PAY::CODE = TEMP2$ &
						IF PR_TRN_PAY::CODE = ""
				END IF

				!
				! Now let's display the default rate data
				! on the screen
				!
				V% = PR_MAIN_TRN_TK01_PAY(SMG_WINDOW, &
					OPT_ENTRY, I%, 1%, MVALUE) &
					FOR I% = 1% TO SMG_WINDOW::NITEMS

 !				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
 !					PR_TRN_PAY::CODE, &
 !					SMG_WINDOW::CURLIN + 1%, 49%, , &
 !					SMG$M_BOLD)
 !
 !				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
 !					PR_TRN_PAY::RTYPE, &
 !					SMG_WINDOW::CURLIN + 1%, 55%, , &
 !					SMG$M_BOLD)
 !
 !				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
 !					FORMAT$(PR_TRN_PAY::HOUR_RATE, &
 !					"##,###.###"), &
 !					SMG_WINDOW::CURLIN + 1%, 58%, , &
 !					SMG$M_BOLD)
 !
 !				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
 !					FORMAT$(PR_TRN_PAY::FACTOR, "###"), &
 !					SMG_WINDOW::CURLIN + 1%, 69%, , &
 !					SMG$M_BOLD)
 !
 !				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
 !					FORMAT$(PR_TRN_PAY::PIECE_RATE, &
 !					"#,###.####"), &
 !					SMG_WINDOW::CURLIN + 1%, 73%, , &
 !					SMG$M_BOLD)

				IF (EVALDATE$ < PR_TRN_PAY::PR_END_DATE) AND &
					(TRM$(EVALDATE$) <> "")
				THEN
					SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
						PRNT_DATE(EVALDATE$, 8%), &
						SMG_WINDOW::CURLIN, 58%, , &
						SMG$M_BOLD OR SMG$M_BLINK)
				ELSE
					SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
						"          ", &
						SMG_WINDOW::CURLIN, 58%, , &
						SMG$M_BOLD)
				END IF

 !			END IF

		CASE 6%
			TEMP%, PR_MAIN_TRN_TK01_PAY = FUNC_TESTENTRY(SMG_WINDOW, &
				"P" + PR_TRN_PAY::CODE, &
				PR_ERNDED_DEF::DESCR, &
				"PR", MLOOP, "PRG", &
				"Account", PR_MAIN_ERNDED_DEF.ID)

			IF TEMP% = 0%
			THEN
				!
				! Try to read rate based on entered code
				!
				CALL PR_READ_RATE_CODE(PR_TRN_PAY::EMPNUM, &
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
				! ? Try to read rate based on employee
				! preferences.
				!
				IF (PR_TRN_PAY::HOUR_RATE = 0.0 AND &
					PR_TRN_PAY::PIECE_RATE = 0.0)
				THEN
					CALL PR_READ_RATE_CODE(PR_TRN_PAY::EMPNUM, &
						PR_TRN_PAY::OPER, &
						PR_TRN_PAY::PR_END_DATE, &
						PR_EMP_MASTER::RATE_TYPE, &
						PR_EMP_MASTER::RATE_CDE, &
						PR_TRN_PAY::HOUR_RATE, &
						PR_TRN_PAY::PIECE_RATE, &
						PR_TRN_PAY::FACTOR, &
						STDEFF, &
						EVALDATE$, &
						EFF_DATE$)
				END IF

				!
				! ? Try to read anything.
				!
				IF (PR_TRN_PAY::HOUR_RATE = 0.0 AND &
					PR_TRN_PAY::PIECE_RATE = 0.0)
				THEN
					TEMP1$ = PR_TRN_PAY::RTYPE + ""
					TEMP2$ = PR_TRN_PAY::CODE + ""

					CALL PR_READ_RATE(PR_TRN_PAY::EMPNUM, &
						PR_TRN_PAY::OPER, &
						PR_TRN_PAY::PR_END_DATE, &
						TEMP1$, &
						TEMP2$, &
						PR_TRN_PAY::HOUR_RATE, &
						PR_TRN_PAY::PIECE_RATE, &
						PR_TRN_PAY::FACTOR, &
						STDEFF, &
						EVALDATE$, &
						EFF_DATE$)

					PR_TRN_PAY::RTYPE = TEMP1$ &
						IF PR_TRN_PAY::RTYPE = ""
					PR_TRN_PAY::CODE = TEMP2$ &
						IF PR_TRN_PAY::CODE = ""
				END IF

				V% = PR_MAIN_TRN_TK01_PAY(SMG_WINDOW, &
					OPT_ENTRY, I%, 1%, MVALUE) &
					FOR I% = 7% TO SMG_WINDOW::NITEMS
			END IF

		CASE 7% TO 14%
			IF PR_TRN_PAY::PTYPE = "O"
			THEN
				JUNK% = PR_MAIN_TRN_TK01_PAY(SMG_WINDOW, &
					OPT_ENTRY, I%, 33%, "") &
					FOR I% = 8% TO 15%
			END IF

			IF PR_TRN_PAY::PTYPE = "P"
			THEN
				GOSUB CalcGross

				JUNK% = PR_MAIN_TRN_TK01_PAY(SMG_WINDOW, &
					OPT_ENTRY, I%, 1%, "") &
					FOR I% = 8% TO 15%
			END IF

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
		PR_TRN_PAY::PR_END_DATE = END_DATE$
		PR_TRN_PAY::UPDATE_FLAG = 0%
		PR_TRN_PAY::BATCH_ENTRY	= BATCH_ENTRY$
		PR_TRN_PAY::SEQNUM	= ""
		PR_TRN_PAY::BATCH = ""

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
			RARRAY(MFLAG)::LINRFA = GETRFA(SMG_WINDOW::CHAN)

			RARRAY(MFLAG)::REG_HR	= PR_TRN_PAY::REG_HR
			RARRAY(MFLAG)::OVT_HR	= PR_TRN_PAY::OVT_HR
			RARRAY(MFLAG)::PIECE	= PR_TRN_PAY::PIECE
			RARRAY(MFLAG)::GROSS	= PR_TRN_PAY::GROSS

		!
		! Load in current record, locked
		!
		CASE 4%
27200			WHEN ERROR IN
				GET #SMG_WINDOW::CHAN, &
					RFA RARRAY(MFLAG)::LINRFA
			USE
				!
				! Invalid RFA
				!
				CONTINUE ExitFunction IF ERR = 173% AND &
					SMG_WINDOW::TOTREC = 0%
				EXIT HANDLER
			END WHEN

		!
		! Load in current record, unlocked
		!
		CASE 5%
			WHEN ERROR IN
				GET #SMG_WINDOW::CHAN, &
					RFA RARRAY(MFLAG)::LINRFA, &
					REGARDLESS
			USE
				!
				! Invalid RFA
				!
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
			PR_EMP_MASTER::EMPNAME = STRING$( &
				LEN(PR_EMP_MASTER::EMPNAME), 63%) &
				IF MAIN_WINDOW(PR_MAIN_TK_EMP_QUERY.ID, &
				"Q0" + PR_TRN_PAY::EMPNUM) <> 1%

			!
			! Print chart description
			!
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PR_EMP_MASTER::EMPNAME, &
				SMG_WINDOW::CURLIN, 33%,, SMG$M_BOLD)

			!
			! Draw horizontal lines
			!
			SMG_STATUS% = SMG$DRAW_LINE(SMG_WINDOW::WNUMBER, &
				SMG_WINDOW::CURLIN + 2%, 1%, &
				SMG_WINDOW::CURLIN + 2%, 130%)

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

 CalcHours:
28200	!*******************************************************************
	! Calculate total regular and overtime hours for currently
	! selected employee, and display it at bottom of screen
	!
	! ONLY CALL DURING ADD. Calling during change can put wrong record
	! in memory.
	!*******************************************************************

	!
	! Save current record
	!
	PR_TRN_PAY_OLD = PR_TRN_PAY

	TOTAL_REG = 0.0
	TOTAL_OVT = 0.0
	TOTAL_PAY = 0.0

28210	!
	! Look up in final folder if it is open
	!
	GOTO 28250 IF PR_TRN_PAY_FINAL.CH% = 0%

	WHEN ERROR IN
		GET #PR_TRN_PAY_FINAL.CH%, &
			KEY #0% EQ PR_TRN_PAY_OLD::EMPNUM + "", &
			REGARDLESS
	USE
		CONTINUE 28250
	END WHEN

28220	WHILE (PR_TRN_PAY::EMPNUM = PR_TRN_PAY_OLD::EMPNUM)

		IF (PR_TRN_PAY::PTYPE <> "A") AND &
			(PR_TRN_PAY::BATCH_ENTRY = BATCH_ENTRY$)
		THEN
			TOTAL_REG = FUNC_ROUND(TOTAL_REG + &
				PR_TRN_PAY::REG_HR, 2%)
			TOTAL_OVT = FUNC_ROUND(TOTAL_OVT + &
				PR_TRN_PAY::OVT_HR, 2%)
			TOTAL_PAY = FUNC_ROUND(TOTAL_PAY + &
				PR_TRN_PAY::GROSS, 2%)
		END IF

		!
		! Grab next record
		!
		WHEN ERROR IN
			GET #PR_TRN_PAY_FINAL.CH%, REGARDLESS
		USE
			CONTINUE 28250
		END WHEN

	NEXT

28250	!
	! Look up in current folder
	!
	WHEN ERROR IN
		GET #PR_TRN_PAY.CH%, &
			KEY #0% EQ PR_TRN_PAY_OLD::EMPNUM + "", &
			REGARDLESS
	USE
		CONTINUE 28290
	END WHEN

28260	WHILE (PR_TRN_PAY::EMPNUM = PR_TRN_PAY_OLD::EMPNUM)

		IF (PR_TRN_PAY::PTYPE <> "A") AND &
			(PR_TRN_PAY::BATCH_ENTRY = BATCH_ENTRY$)
		THEN
			TOTAL_REG = FUNC_ROUND(TOTAL_REG + &
				PR_TRN_PAY::REG_HR, 2%)
			TOTAL_OVT = FUNC_ROUND(TOTAL_OVT + &
				PR_TRN_PAY::OVT_HR, 2%)
			TOTAL_PAY = FUNC_ROUND(TOTAL_PAY + &
				PR_TRN_PAY::GROSS, 2%)
		END IF

		WHEN ERROR IN
			GET #PR_TRN_PAY.CH%, REGARDLESS
		USE
			CONTINUE 28290
		END WHEN

	NEXT

28290	!
	! Display, and reset back
	!
	PR_TRN_PAY = PR_TRN_PAY_OLD

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
		"Emp#" + PR_TRN_PAY::EMPNUM + &
		" Bth " + BATCH_ENTRY$ + &
		FORMAT$(TOTAL_REG, " Reg###.## ") + &
		FORMAT$(TOTAL_OVT, "Ovt###.## ") + &
		FORMAT$(TOTAL_GROSS, "Grs####.##"), &
		SMG_WINDOW::VSIZE, 22%, , SMG$M_REVERSE)

	RETURN

	%PAGE

 TestSubaccount:
	!*******************************************************************
	! Subaccount has a problem, see what we want to do about it
	!*******************************************************************

	CALL ENTR_3MESSAGE(SCOPE, TEXT$, 1%)
	CALL SMG$RING_BELL(SMG_WINDOW::WNUMBER, 1%)

	UTL_SET_READ::ALLOWUND = "N"

	!
	! See if they are allowing undefined inputs
	!
	IF READ_35SET(SCOPE::PRG_PROGRAM, NUM1$(MLOOP), UTL_SET_READ) <> &
		CMC$_NORMAL
	THEN
		UTL_SET_READ::ALLOWUND = "N"
	END IF

	IF UTL_SET_READ::ALLOWUND = "N"
	THEN
		IF READ_35SET("PR_ALLOW", "SUBACC", UTL_SET_READ) <> CMC$_NORMAL
		THEN
			UTL_SET_READ::ALLOWUND = "N"
		END IF
	END IF

	IF UTL_SET_READ::ALLOWUND = "N"
	THEN
		!
		! Don't let them get past if we don't allow
		! them to enter undefined values.
		!
		PR_MAIN_TRN_TK01_PAY = 1%
	ELSE
		PR_MAIN_TRN_TK01_PAY = 0%
	END IF

	RETURN

	%PAGE

 TestBlankSubaccount:
28400	!*******************************************************************
	! Check a blank subaccount to see if it is allowed for a given
	! gl account
	!*******************************************************************

	!
	! If set file says to allow it no matter what, say OK
	!
	IF READ_35SET("PR_ALLOW", &
		"BLANKSUB", UTL_SET_READ) <> CMC$_NORMAL
	THEN
		UTL_SET_READ::ALLOWUND = "N"
	END IF

	RETURN IF (UTL_SET_READ::ALLOWUND) <> "N"

28420	!
	! Look for first account
	!
	WHEN ERROR IN
		GET #SB_ACCOUNT.CH%, &
			KEY #0% GE "JC", &
			REGARDLESS
	USE
		CONTINUE 28490
	END WHEN

28430	!
	! Look at one account, as long as it is a JC account
	!
	IF SB_ACCOUNT::SYSTEM = "JC"
	THEN
		!
		! If it shows up, then blanl ones are not allowed
		!
		IF COMP_STRING(TRM$(PR_TRN_PAY::ACCT), SB_ACCOUNT::ACCOUNT)
		THEN
			CALL SMG$RING_BELL(SMG_WINDOW::WNUMBER, 1%)
			CALL ENTR_3MESSAGE(SCOPE, "Subaccount Required", 0%)
			PR_MAIN_TRN_TK01_PAY = 1%
			GOTO 28490
		END IF

		!
		! Look at next one
		!
		WHEN ERROR IN
			GET #SB_ACCOUNT.CH%, REGARDLESS
		USE
			CONTINUE 28490
		END WHEN

		GOTO 28430
	END IF


28490	RETURN

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
