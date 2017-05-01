1	%TITLE "Employee Master File Maintenance"
	%SBTTL "PR_MAIN_ERNDED_DEF"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PR_MAIN_ERNDED_DEF(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, &
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
	!	The ^*Payment/Deduction Code Definitions\* file
	!	enters a record for each possible type of payment and deduction
	!	and to specify the impact that any payment or deduction has on the
	!	various types of taxes.
	!
	! Index:
	!	.x Payments
	!	.x Deductions
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_MAIN_ERNDED_DEF/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN PR_MAIN_ERNDED_DEF
	!	$ DELETE PR_MAIN_ERNDED_DEF.OBJ;*
	!
	! Author:
	!
	!	09/18/87 - Kevin Handy
	!
	! Modification history:
	!
	!	06/01/88 - Aaron Redd
	!		Modified to allow R/O open of file if R/W open fails.
	!
	!	04/22/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	03/02/93 - Dan Perkins
	!		Changed "V0" to "VX" on chart of accounts to be
	!		able to list accounts starting at a particular
	!		account.
	!
	!	02/01/94 - Kevin Handy
	!		Added "W2 Location" field to file.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/22/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/29/97 - Kevin handy
	!		Use integer for #key
	!
	!	05/26/98 - Kevin Handy
	!		Added 'F' code as allowed deduction type
	!
	!	05/29/98 - Kevin Handy
	!		Handle 'F' code in the validation section.
	!
	!	08/18/98 - Kevin Handy
	!		Force field 1 to be upper case always.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/21/2000 - Kevin Handy
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

	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.HB"
	MAP	(PR_ERNDED_DEF)	PR_ERNDED_DEF_CDD	PR_ERNDED_DEF
	MAP (PR_ERNDED_DEF2) PR_ERNDED_DEF_CDD PR_ERNDED_DEF_OLD, PR_ERNDED_DEF2

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP	(GL_CHART)	GL_CHART_CDD		GL_CHART

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_PR_ERNDED_DEF) &
		PR_ERNDED_DEF.CH%, &
		PR_ERNDED_DEF.READONLY%

	COM (TT_PR_ERNDED_DEF) &
		PTTITLE$ = 20%, &
		PT$(6%) = 20%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION MAIN_WINDOW
	EXTERNAL LONG	FUNCTION FUNC_TESTENTRY

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
		SMG_WINDOW::DESCR = "Payroll Pay/Ded master file"
		SMG_WINDOW::NHELP = "PR_MAIN_ERNDED_DEF"
		SMG_WINDOW::CHAN  = PR_ERNDED_DEF.CH%
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 28%

		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Code"
			SMG_WINDOW::KFIELD(0%, 0%) = 2%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
			SMG_WINDOW::KFIELD(0%, 2%) = 2%

		!
		! Pay types
		!
		PTTITLE$ = "Type Description"
		PT$(0%) = "5"
		PT$(1%) = "P    Payment"
		PT$(2%) = "D    Deduction"
		PT$(3%) = "T    Noncompensation"
		PT$(4%) = "M    Memo"
		PT$(5%) = "F    Final (deduction)"

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF PR_ERNDED_DEF.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PR_ERNDED_DEF.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PR_MAIN_ERNDED_DEF = ERR
			CONTINUE 770
		END WHEN

		PR_ERNDED_DEF.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.OPN"
		USE
			PR_MAIN_ERNDED_DEF = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PR_ERNDED_DEF.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PR_ERNDED_DEF.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = PR_ERNDED_DEF.CH%
		WHEN ERROR IN
			RESET #PR_ERNDED_DEF.CH%
			GET #PR_ERNDED_DEF.CH%, REGARDLESS
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

		SELECT MLOOP

		!
		! Main screen
		!
		CASE 0%
			SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

			SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

			DATA	1,  1, "(01) Type", &
				2,  1, "(02) Code", &
				3,  1, "(03) Descr", &
				4,  1, "(04) CR Acct", &
				5,  1, "(05) Acc. Ex Acct", &
				6,  1, "(06) Summary", &
				8, 16, "Taxable   Reportable", &
				9,  1, "Federal        (07)      (17)", &
				10,  1, "FICA Employee  (08)      (18)", &
				11,  1, "FICA Employer  (09)      (19)", &
				12,  1, "Fed Unemp      (10)      (20)", &
				13,  1, "State          (11)      (21)", &
				14,  1, "State Unemp    (12)      (22)", &
				15,  1, "Other State    (13)      (23)", &
				16,  1, "City           (14)      (24)", &
				17,  1, "County         (15)      (25)", &
				18,  1, "School         (16)      (26)", &
				8, 56, "Subject", &
				9, 41, "Workmen Comp   (27)", &
				18, 41, "(28) W2 Location", &
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

 E0Loop:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Type\*
	!	.p
	!	The ^*Type\* field
	!	defines the type of transaction
	!	represented by a specific record. Valid types are:
	!	.b
	!	.lm +5
	!	.LIST 0,"o"
	!	.LE
	!	P = Payments
	!	.LE
	!	D = Deductions
	!	.le
	!	N = Non compensation
	!	.le
	!	M = Memo
	!	.ELS
	!	.lm -5
	!
	! Index:
	!	.x Payment Code>Type
	!	.x Deduction Code>Type
	!	.x Type>Payment Code
	!	.x Type>Deduction Code
	!
	!--

			PR_ERNDED_DEF::ETYPE  = EDIT$(ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, "1;18", TEMP$, &
				PR_ERNDED_DEF::ETYPE, MFLAG OR 16%, &
				"'", MVALUE, &
				PT$(), PTTITLE$, "005"), -1%)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Code\*
	!	.p
	!	The ^*Code\* field
	!	enters a two (2) character, user defined,
	!	alphanumeric code for each particular payment or deduction type.
	!	.p
	!	Some examples could be:
	!	.B
	!	.lm +5
	!	^&Type\&####^&Code\&####^&Description\&
	!	.BREAK
	!	#D#######GI#####Group Insurance
	!	.BREAK
	!	#D#######CU#####Credit Union
	!	.break
	!	#D#######UW#####United Way
	!	.break
	!	#D#######IR#####IRA Plan
	!	.BREAK
	!	#D#######4K#####401K Plan
	!	.BREAK
	!	#P#######TX#####Regular Earnings
	!	.BREAK
	!	#P#######BO#####Bonuses
	!	.BREAK
	!	#P#######VA#####Vacation Pay
	!	.BREAK
	!	#P#######SP#####Sick Pay
	!	.BREAK
	!	#P#######MI#####Mileage Reimbursement
	!	.LM -5
	!
	! Index:
	!	.x Payment Code>Code
	!	.x Deduction Code>Code
	!	.x Code>Payment Code
	!	.x Code>Deduction Code
	!
	!--

			PR_ERNDED_DEF::CODE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "2;18", TEMP$, &
				PR_ERNDED_DEF::CODE, MFLAG, "'E", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Description\*
	!	.p
	!	The ^*Description\* field
	!	describes each particular
	!	kind of payment or deduction.
	!	.p
	!	The field will contain up to thirty (30) alphanumeric characters.
	!	However, the fourteen (14) left-most characters only will be printed
	!	on the employee's check stub.
	!
	! Index:
	!	.x Payment Code>Description
	!	.x Deduction Code>Description
	!	.x Description>Payment Code
	!	.x Description>Deduction Code
	!
	!--

			PR_ERNDED_DEF::DESCR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "3;18", TEMP$, &
				PR_ERNDED_DEF::DESCR, MFLAG, &
				"'E", MVALUE)

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Credited Account\*
	!	.p
	!	The ^*Credited Account\* field
	!	records the General Ledger
	!	account number to be credited when a particular payroll deduction
	!	is made.
	!	.p
	!	Records for Payment Codes must contain blanks in this field. The
	!	appropriate General Ledger accounts are entered in the Timekeeper
	!	routine. A list of accounts may be seen by pressing the ^*<List Choices>\*
	!	key, moving the arrow to the desired selection, and pressing the ^*<Select>\*
	!	key.
	!
	! Index:
	!	.x Deduction Code>Credit Account
	!	.x Payment Code>Credit Account
	!	.x Credit Account>Deduction Code
	!	.x Credit Account>Payment Code
	!
	!--

			PR_ERNDED_DEF::DRCR_ACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "4;18", TEMP$, &
				PR_ERNDED_DEF::DRCR_ACCT, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX ") = 1%)
				THEN
					PR_ERNDED_DEF::DRCR_ACCT = &
						GL_CHART::ACCT
					GOTO E0Loop
				END IF
			END IF

		CASE 5%

	!++
	! Abstract:FLD005
	!	^*(05) Accrual Expenses Account\*
	!	.p
	!	The ^*Accrual Expense(s) Account\* field
	!	records
	!	the expense account(s) associated with a related accrual for a
	!	particular type of payment. For example, if the record represented
	!	Vacation Pay, this field should contain the General Ledger expense
	!	account for Vacation Pay, provided there were a single account for
	!	Vacation Pay. If there were Vacation Pay expense accounts for various
	!	departments, etc., this field should contain an account mask.
	!
	! Index:
	!	.x Accrual Expense Account>Payment Code
	!	.x Payment Code>Accrual Expense Account
	!	.x Accrual Expense Account>Deduction Code
	!	.x Deduction Code>Accrual Expense Account
	!
	!--

			PR_ERNDED_DEF::ACCRUAL_ACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "5;18", TEMP$, &
				PR_ERNDED_DEF::ACCRUAL_ACCT, MFLAG, &
				"'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX ") = 1%)
				THEN
					PR_ERNDED_DEF::ACCRUAL_ACCT = &
						GL_CHART::ACCT
					GOTO E0Loop
				END IF
			END IF

		CASE 6%

	!++
	! Abstract:FLD006
	!	^*(06) Summary\*
	!	.p
	!	The ^*Summary\* field
	!	specifies a summary flag (Yes or No). A
	!	^*Yes\* flag will cause the transactions for a particular deduction to be
	!	recorded in the General Ledger in summary as a one line item for each
	!	payroll period. A ^*No\* flag will cause the transactions for a particular
	!	deduction to be recorded in detail with a line item for each employee
	!	for whom there was the specific deduction.
	!
	! Index:
	!	.X Deduction Code>Summary
	!	.x Payment Code>Summary
	!	.x Summary>Deduction Code
	!	.x Summary>Payment Code
	!
	!--

			PR_ERNDED_DEF::SUMMARY = ENTR_3YESNO(SCOPE, &
				SMG_WINDOW::WNUMBER, "6;18", TEMP$, &
				PR_ERNDED_DEF::SUMMARY, MFLAG, "'E", MVALUE)


		CASE 7%

	!++
	! Abstract:FLD007TAX
	!	^*(07) - (16) Taxable\*
	!	.p
	!	The ^*Taxable\* flags, fields (07) through (16), of the Payments and
	!	Deductions Codes Definition file are to be set to ^*Y\* (for yes) or ^*N\*
	!	(for no), indicating whether a particular payment or earnings code is
	!	subject to each tax listed in the column to the left of the flags.
	!	.p
	!	If the record represents a deduction transaction, the flags are
	!	to be set to ^*Y\* (for yes) if the earnings from which the deduction
	!	is made are subject to each of the taxes listed. An ^*N\* flag (for
	!	no) indicates that the earnings from which the deduction is made are
	!	^&not\& subject to a kind of tax. An example of the latter could be
	!	a 401k or Cafeteria 125 deduction.
	!
	! Index:
	!	.x Deduction Code>Taxable
	!	.x Payment Code>Taxable
	!	.x Taxable>Deduction Code
	!	.x Taxable>Payment Code
	!
	!--

			PR_ERNDED_DEF::TAXABLE_FWH = "Y" &
				IF PR_ERNDED_DEF::TAXABLE_FWH = ""

			PR_ERNDED_DEF::TAXABLE_FWH = &
				ENTR_3YESNO(SCOPE, SMG_WINDOW::WNUMBER, &
				"9;21", TEMP$, &
				PR_ERNDED_DEF::TAXABLE_FWH, MFLAG, &
				"'E", MVALUE)

		CASE 8%
			SCOPE::PRG_ITEM = "FLD007TAX"

			PR_ERNDED_DEF::TAXABLE_FIE = "Y" &
				IF PR_ERNDED_DEF::TAXABLE_FIE = ""

			PR_ERNDED_DEF::TAXABLE_FIE = ENTR_3YESNO(SCOPE, &
				SMG_WINDOW::WNUMBER, "10;21", TEMP$, &
				PR_ERNDED_DEF::TAXABLE_FIE, MFLAG, "'E", MVALUE)

		CASE 9%

			SCOPE::PRG_ITEM = "FLD007TAX"

			PR_ERNDED_DEF::TAXABLE_FIR = "Y" &
				IF PR_ERNDED_DEF::TAXABLE_FIR = ""

			PR_ERNDED_DEF::TAXABLE_FIR = ENTR_3YESNO(SCOPE, &
				SMG_WINDOW::WNUMBER, "11;21", TEMP$, &
				PR_ERNDED_DEF::TAXABLE_FIR, MFLAG, &
				"'E", MVALUE)

		CASE 10%

			SCOPE::PRG_ITEM = "FLD007TAX"

			PR_ERNDED_DEF::TAXABLE_FUI = "Y" &
				IF PR_ERNDED_DEF::TAXABLE_FUI = ""

			PR_ERNDED_DEF::TAXABLE_FUI = ENTR_3YESNO(SCOPE, &
				SMG_WINDOW::WNUMBER, "12;21", TEMP$, &
				PR_ERNDED_DEF::TAXABLE_FUI, MFLAG, &
				"'E", MVALUE)

		CASE 11%

			SCOPE::PRG_ITEM = "FLD007TAX"

			PR_ERNDED_DEF::TAXABLE_SWH = "Y" &
				IF PR_ERNDED_DEF::TAXABLE_SWH = ""

			PR_ERNDED_DEF::TAXABLE_SWH = ENTR_3YESNO(SCOPE, &
				SMG_WINDOW::WNUMBER, "13;21", TEMP$, &
				PR_ERNDED_DEF::TAXABLE_SWH, MFLAG, &
				"'E", MVALUE)

		CASE 12%

			SCOPE::PRG_ITEM = "FLD007TAX"

			PR_ERNDED_DEF::TAXABLE_SUI = "Y" &
				IF PR_ERNDED_DEF::TAXABLE_SUI = ""

			PR_ERNDED_DEF::TAXABLE_SUI = ENTR_3YESNO(SCOPE, &
				SMG_WINDOW::WNUMBER, "14;21", TEMP$, &
				PR_ERNDED_DEF::TAXABLE_SUI, MFLAG, &
				"'E", MVALUE)

		CASE 13%

			SCOPE::PRG_ITEM = "FLD007TAX"

			PR_ERNDED_DEF::TAXABLE_OST = "Y" &
				IF PR_ERNDED_DEF::TAXABLE_OST = ""

			PR_ERNDED_DEF::TAXABLE_OST = ENTR_3YESNO(SCOPE, &
				SMG_WINDOW::WNUMBER, "15;21", TEMP$, &
				PR_ERNDED_DEF::TAXABLE_OST, MFLAG, &
				"'E", MVALUE)

		CASE 14%

			SCOPE::PRG_ITEM = "FLD007TAX"

			PR_ERNDED_DEF::TAXABLE_CWH = "Y" &
				IF PR_ERNDED_DEF::TAXABLE_CWH = ""

			PR_ERNDED_DEF::TAXABLE_CWH = ENTR_3YESNO(SCOPE, &
				SMG_WINDOW::WNUMBER, "16;21", TEMP$, &
				PR_ERNDED_DEF::TAXABLE_CWH, MFLAG, &
				"'E", MVALUE)

		CASE 15%

			SCOPE::PRG_ITEM = "FLD007TAX"

			PR_ERNDED_DEF::TAXABLE_DWH = "Y" &
				IF PR_ERNDED_DEF::TAXABLE_DWH = ""

			PR_ERNDED_DEF::TAXABLE_DWH = ENTR_3YESNO(SCOPE, &
				SMG_WINDOW::WNUMBER, "17;21", TEMP$, &
				PR_ERNDED_DEF::TAXABLE_DWH, MFLAG, &
				"'E", MVALUE)

		CASE 16%

			SCOPE::PRG_ITEM = "FLD007TAX"

			PR_ERNDED_DEF::TAXABLE_EWH = "Y" &
				IF PR_ERNDED_DEF::TAXABLE_EWH = ""

			PR_ERNDED_DEF::TAXABLE_EWH = ENTR_3YESNO(SCOPE, &
				SMG_WINDOW::WNUMBER, "18;21", TEMP$, &
				PR_ERNDED_DEF::TAXABLE_EWH, MFLAG, &
				"'E", MVALUE)

		CASE 17%

			SCOPE::PRG_ITEM = "FLD017REP"
	!++
	! Abstract:FLD017REP
	!	^*(17) - (26) Reportable\*
	!	.p
	!	The ^*Reportable\* flags, fields (17) through (26),
	!	are to be set to ^*Y\* (for yes) or
	!	^*N\* (for no), indicating whether a particular payment or earnings code
	!	is reportable (though not necessarily taxable) in regard to each tax
	!	listed in the column to the left of the flags.
	!	.p
	!	If the record represents a deduction transaction, the flags are
	!	to be set to ^*Y\* (for yes) if the earnings from which the deduction
	!	is made is reportable as earnings in regard to each of the taxes
	!	listed. An ^*N\* flag (for no) indicates that the earnings from which
	!	the deduction is made are ^&not\& reportable as earnings in reference
	!	to a kind of tax.
	!
	! Index:
	!	.x Payment Code>Reportable
	!	.x Reportable>Payment Code
	!	.x Deduction Code>Reportable
	!	.x Reportable>Deduction Code
	!
	!--

			PR_ERNDED_DEF::REPORTABLE_FWH = "Y" &
				IF PR_ERNDED_DEF::REPORTABLE_FWH = ""

			PR_ERNDED_DEF::REPORTABLE_FWH = ENTR_3YESNO(SCOPE, &
				SMG_WINDOW::WNUMBER, "9;31", TEMP$, &
				PR_ERNDED_DEF::REPORTABLE_FWH, MFLAG, &
				"'E", MVALUE)

		CASE 18%

			SCOPE::PRG_ITEM = "FLD017REP"

			PR_ERNDED_DEF::REPORTABLE_FIE = "Y" &
				IF PR_ERNDED_DEF::REPORTABLE_FIE = ""

			PR_ERNDED_DEF::REPORTABLE_FIE = ENTR_3YESNO(SCOPE, &
				SMG_WINDOW::WNUMBER, "10;31", TEMP$, &
				PR_ERNDED_DEF::REPORTABLE_FIE, MFLAG, &
				"'E", MVALUE)

		CASE 19%

			SCOPE::PRG_ITEM = "FLD017REP"

			PR_ERNDED_DEF::REPORTABLE_FIR = "Y" &
				IF PR_ERNDED_DEF::REPORTABLE_FIR = ""

			PR_ERNDED_DEF::REPORTABLE_FIR = ENTR_3YESNO(SCOPE, &
				SMG_WINDOW::WNUMBER, "11;31", TEMP$, &
				PR_ERNDED_DEF::REPORTABLE_FIR, MFLAG, &
				"'E", MVALUE)

		CASE 20%

			SCOPE::PRG_ITEM = "FLD017REP"

			PR_ERNDED_DEF::REPORTABLE_FUI = "Y" &
				IF PR_ERNDED_DEF::REPORTABLE_FUI = ""

			PR_ERNDED_DEF::REPORTABLE_FUI = ENTR_3YESNO(SCOPE, &
				SMG_WINDOW::WNUMBER, "12;31", TEMP$, &
				PR_ERNDED_DEF::REPORTABLE_FUI, MFLAG, &
				"'E", MVALUE)

		CASE 21%

			SCOPE::PRG_ITEM = "FLD017REP"

			PR_ERNDED_DEF::REPORTABLE_SWH = "Y" &
				IF PR_ERNDED_DEF::REPORTABLE_SWH = ""

			PR_ERNDED_DEF::REPORTABLE_SWH = ENTR_3YESNO(SCOPE, &
				SMG_WINDOW::WNUMBER, "13;31", TEMP$, &
				PR_ERNDED_DEF::REPORTABLE_SWH, MFLAG, &
				"'E", MVALUE)

		CASE 22%

			SCOPE::PRG_ITEM = "FLD017REP"

			PR_ERNDED_DEF::REPORTABLE_SUI = "Y" &
				IF PR_ERNDED_DEF::REPORTABLE_SUI = ""

			PR_ERNDED_DEF::REPORTABLE_SUI = ENTR_3YESNO(SCOPE, &
				SMG_WINDOW::WNUMBER, "14;31", TEMP$, &
				PR_ERNDED_DEF::REPORTABLE_SUI, MFLAG, &
				"'E", MVALUE)

		CASE 23%

			SCOPE::PRG_ITEM = "FLD017REP"

			PR_ERNDED_DEF::REPORTABLE_OST = "Y" &
				IF PR_ERNDED_DEF::REPORTABLE_OST = ""

			PR_ERNDED_DEF::REPORTABLE_OST = ENTR_3YESNO(SCOPE, &
				SMG_WINDOW::WNUMBER, "15;31", TEMP$, &
				PR_ERNDED_DEF::REPORTABLE_OST, MFLAG, &
				"'E", MVALUE)

		CASE 24%

			SCOPE::PRG_ITEM = "FLD017REP"

			PR_ERNDED_DEF::REPORTABLE_CWH = "Y" &
				IF PR_ERNDED_DEF::REPORTABLE_CWH = ""

			PR_ERNDED_DEF::REPORTABLE_CWH = ENTR_3YESNO(SCOPE, &
				SMG_WINDOW::WNUMBER, "16;31", TEMP$, &
				PR_ERNDED_DEF::REPORTABLE_CWH, MFLAG, &
				"'E", MVALUE)

		CASE 25%

			SCOPE::PRG_ITEM = "FLD017REP"

			PR_ERNDED_DEF::REPORTABLE_DWH = "Y" &
				IF PR_ERNDED_DEF::REPORTABLE_DWH = ""

			PR_ERNDED_DEF::REPORTABLE_DWH = ENTR_3YESNO(SCOPE, &
				SMG_WINDOW::WNUMBER, "17;31", TEMP$, &
				PR_ERNDED_DEF::REPORTABLE_DWH, MFLAG, &
				"'E", MVALUE)

		CASE 26%

			SCOPE::PRG_ITEM = "FLD017REP"

			PR_ERNDED_DEF::REPORTABLE_EWH = "Y" &
				IF PR_ERNDED_DEF::REPORTABLE_EWH = ""

			PR_ERNDED_DEF::REPORTABLE_EWH = ENTR_3YESNO(SCOPE, &
				SMG_WINDOW::WNUMBER, "18;31", TEMP$, &
				PR_ERNDED_DEF::REPORTABLE_EWH, MFLAG, &
				"'E", MVALUE)

		CASE 27%

	!++
	! Abstract:FLD027
	!	^*(27) Subject\*
	!	.p
	!	The ^*Subject\* field
	!	designates whether a specific type of pay will be subject to workmens
	!	compensation. If only time worked were subject, records for time not
	!	worked, such as vacation or sick pay, would contain a ^*No\* flag.
	!
	! Index:
	!	.x Subject>Payment Code
	!	.x Payment Code>Subject
	!	.x Deduction Code>Subject
	!	.x Subject>Payment Code
	!
	!--

			PR_ERNDED_DEF::SUBJ_WC = "Y" &
				IF PR_ERNDED_DEF::SUBJ_WC = ""

			PR_ERNDED_DEF::SUBJ_WC = ENTR_3YESNO(SCOPE, &
				SMG_WINDOW::WNUMBER, "9;61", TEMP$, &
				PR_ERNDED_DEF::SUBJ_WC, MFLAG, &
				"'E", MVALUE)

		CASE 28%

	!++
	! Abstract:FLD028
	!	^*(28) W2 Locaton\*
	!	.b
	!	This field is used to specify special items that have their own blanks
	!	on W2 forms, and W2 tapes.
	!	.b
	!	The valid possibilities for this field are:
	!	.lm +5
	!	.b
	!	.i -3
	!	*A*E Advance Earned Income Credit.
	!	.br
	!	.i -3
	!	*A*T Allocated Tips
	!	.br
	!	.i -3
	!	*C*D Dependent care benefits.
	!	.br
	!	.i -3
	!	*D*A Uncollected Social Security tax on Tips.
	!	.br
	!	.i -3
	!	*D*B Uncollected Medicare tax on tips.
	!	.br
	!	.i -3
	!	*D*C Cost of group term life insurance over $50,000.
	!	.br
	!	.i -3
	!	*D*D Section 401(k) contributions.
	!	.br
	!	.i -3
	!	*D*E Section 403(b) contributions.
	!	.br
	!	.i -3
	!	*D*F Section 408(k)(6) contributions.
	!	.br
	!	.i -3
	!	*D*G Section 457(b) contributions.
	!	.br
	!	.i -3
	!	*D*H Section 501(c)(18)(D) contributions.
	!	.br
	!	.i -3
	!	*D*J Sick pay not includible as income.
	!	.br
	!	.i -3
	!	*D*K Tax on excess golden parachute payments.
	!	.br
	!	.i -3
	!	*D*L Nontaxable part of employee business expense reimbursements.
	!	.br
	!	.i -3
	!	*D*M Uncollected social security tax on cost of group-term life
	!	insurance coverage over $50,000 (former employees only).
	!	.br
	!	.i -3
	!	*D*N Uncollected medicare tax on cost of group-term life
	!	insurance coverage over $50,000 (former employees only).
	!	.br
	!	.i -3
	!	*E*C Employer Contributions to a Health Savings Account.
	!	.br
	!	.i -3
	!	*F*D Employee portion of Social Security and Medicare tax that was
	!	paid by a third party Sick Pay Insurance.
	!	.br
	!	.i -3
	!	*F*R Fringe Benefits.
	!	.br
	!	.i -3
	!	*L*O^~xx\~ Local Tax, for locality ^~xx\~
	!	.i -3
	!	*N*Q Non qualified plan section 457.
	!	.br
	!	.i -3
	!	*O? Other (second character user defined) for box 14 (may change) of
	!	the w2 form.
	!	.br
	!	.i -3
	!	*Q*U Qualified plan section 457.
	!	.br
	!	.i -3
	!	*S*T Social Security Tips.
	!	.lm -5
	!
	! Index:
	!
	!--

			PR_ERNDED_DEF::W2LOCATION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "18;61", TEMP$, &
				PR_ERNDED_DEF::W2LOCATION, MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY

		PR_MAIN_ERNDED_DEF = 0%

		SELECT MLOOP

		CASE 4%
		!
		! Is the input defined?
		!
		IF PR_ERNDED_DEF::DRCR_ACCT <> "" OR &
			PR_ERNDED_DEF::ETYPE = "D" OR &
			PR_ERNDED_DEF::ETYPE = "F"
		THEN
			PR_MAIN_ERNDED_DEF = FUNC_TESTENTRY(SMG_WINDOW, &
				PR_ERNDED_DEF::DRCR_ACCT, &
				GL_CHART::DESCR, &
				"PR", MLOOP, "PRG", &
				"Account", GL_MAIN_CHART.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(GL_CHART::DESCR, 20%), &
				4%, 41%, , SMG$M_BOLD)
		END IF

		CASE 5%
		!
		! Is the input defined?
		!
		! This has been changed to accrual expense account
		!
		IF PR_ERNDED_DEF::ACCRUAL_ACCT <> "" AND INSTR(1%, &
			PR_ERNDED_DEF::ACCRUAL_ACCT, "?") = 0%
		THEN
			PR_MAIN_ERNDED_DEF = FUNC_TESTENTRY( SMG_WINDOW, &
				PR_ERNDED_DEF::ACCRUAL_ACCT, &
				GL_CHART::DESCR, &
				"PR", MLOOP, "PRG", &
				"Account", GL_MAIN_CHART.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(GL_CHART::DESCR, 20%), &
				5%, 41%, , SMG$M_BOLD)
		END IF

		END SELECT

	!
	! Set PR_ERNDED_DEF_OLD value
	!
20500	CASE OPT_SETOLD
		PR_ERNDED_DEF_OLD = PR_ERNDED_DEF

	!
	! Restore PR_ERNDED_DEF_OLD value
	!
	CASE OPT_RESETOLD
		PR_ERNDED_DEF = PR_ERNDED_DEF_OLD

	!
	! Display junk
	!
	CASE OPT_DISPLAY
		IF (SMG_WINDOW::HFLAG(4%) AND 2%)=0%
		THEN
			!
			! First account
			!
			!
			! Is the input defined?
			!
			IF MAIN_WINDOW(GL_MAIN_CHART.ID, &
				"Q0" + PR_ERNDED_DEF::DRCR_ACCT) <> 1%
			THEN
				GL_CHART::DESCR = &
					STRING$(LEN(GL_CHART::DESCR), 63%)
			END IF

			!
			! Print chart description
			!
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(GL_CHART::DESCR, 20%), &
				4%, 41%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(5%) AND 2%)=0%
		THEN
			!
			! Second account
			!
			!
			! Is the input defined?
			!
			IF MAIN_WINDOW(GL_MAIN_CHART.ID, &
				"Q0" + PR_ERNDED_DEF::ACCRUAL_ACCT) <> 1%
			THEN
				GL_CHART::DESCR = &
					STRING$(LEN(GL_CHART::DESCR), 63%)
			END IF

			!
			! Print chart description
			!
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(GL_CHART::DESCR, 20%), &
				5%, 41%, , SMG$M_BOLD)
		END IF

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PR_ERNDED_DEF2 = PR_ERNDED_DEF

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PR_ERNDED_DEF = PR_ERNDED_DEF2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "Type Code Description"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "005,010"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = PR_ERNDED_DEF::ETYPE + "  " + &
				PR_ERNDED_DEF::CODE + "   " + &
				PR_ERNDED_DEF::DESCR
		END SELECT

	!
	! Find
	!
	CASE OPT_FIND

		FIND #PR_ERNDED_DEF.CH%, &
			KEY #0% GE PR_ERNDED_DEF::ETYPE + PR_ERNDED_DEF::CODE, &
			REGARDLESS

	END SELECT

	EXIT FUNCTION

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
