1	%TITLE "GENERAL LEDGER PERIOD FILE MAINTENANCE"
	%SBTTL "GL_MAIN_GLMNT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG GL_MAIN_GLMNT(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
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
	! ID:1025
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*General Ledger Maintenance\* function allows correction
	!	of any record in the General Ledger file. It is strongly recommended
	!	that the ^*Amount\* field not be changed.
	!	.b
	!	This function is intended for fields other than the amount
	!	field, and should be used as an exception, not as a rule. An
	!	audit trail will be provided if the ^*LOG\* function in the ^*SET\*
	!	command has been set to ^*LOG\*. To set the ^*LOG\* function enter
	!	the following after referring to the
	!	CMC Software User and Reference Documentation.
	!	.B 1
	!	.lm +5
	!	(01) Program###^*GL__GLMNT\*
	!	.BREAK
	!	(02) Item######^*LOG\*
	!	.BREAK
	!	(03) System
	!	.BREAK
	!	(04) Hard/Soft
	!	.BREAK
	!	(05) Data######^*LOG\* (If not present then no log)
	!	.b
	!	.lm -5
	!	After accessing the ^*GLMNT - Maintain Files\* option, the system will allow
	!	the ^*Period\*, and the ^*Year\* to be entered.
	!
	! Index:
	!	.x General Ledger>Maintenance
	!	.x Maintain>General Ledger
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_MAIN_GLMNT/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN GL_MAIN_GLMNT
	!	$ DELETE GL_MAIN_GLMNT.OBJ;*
	!
	! Author:
	!
	!	01/12/86 - Kevin Handy and B. Craig Larsen
	!
	! Modification history:
	!
	!	05/10/88 - Lance Williams
	!		Modified to allow R/O open of file if R/W fails.
	!
	!	06/23/88 - Aaron Redd
	!		Split into two modules (_MAST_ and _MAIN_) in order
	!		to meet standardization requirements.
	!
	!	11/30/90 - Craig Tanner
	!		Modified view option to display more fields.
	!
	!	08/14/91 - Kevin Handy
	!		Removed A+.
	!
	!	03/22/92 - Kevin Handy
	!		Clean up (check)
	!
	!	03/02/93 - Dan Perkins
	!		Changed "V0" to "VX" on chart of accounts to be
	!		able to list accounts starting at a particular
	!		account.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/16/98 - Kevin Handy
	!		Lose an excessive number of %PAGE
	!
	!	03/09/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	07/03/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	08/06/2001 - Kevin Handy
	!		Increase size of dollars field in entry. (kbj)
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION FUNC_TESTENTRY
	EXTERNAL LONG	FUNCTION MAIN_WINDOW

	%PAGE

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP	(GL_CHART)		GL_CHART_CDD	GL_CHART
	MAP	(GL_CHART_BLAH)		GL_CHART_CDD	GL_CHART_OLD, &
						GL_CHART2

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP	(GL_YYYY_PP)		GL_YYYY_PP_CDD	GL_YYYY_PP
	MAP	(GL_YYYY_PP_BLAH)	GL_YYYY_PP_CDD	GL_YYYY_PP_OLD, &
		GL_YYYY_PP2

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_GL_GLMNT) &
		GL_YYYY_PP.CH%, &
		GL_YYYY_PP.READONLY%
	COM (TT_GL_GLMNT) &
		YYYY_PP$ = 7%

	!
	! Declare some variables
	!
	DECLARE LONG XPOS, YPOS

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 29000

	SELECT MOPTION

	!****************************************************************
	! Initialization
	!
	! This option is used to initialize the window structure,
	! set up the default values for add, and open all files
	! necessary that have not already been opened.
	!****************************************************************
	CASE OPT_INIT

		!
		! Define window
		!
		SMG_WINDOW::DESCR = "GL Period File " + YYYY_PP$ + &
			" Maintenance"
		SMG_WINDOW::NHELP = "GL_MAIN_GLMNT"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 130%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS = 16%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 5%
		SMG_WINDOW::KNAME(0%) = "Account"
			SMG_WINDOW::KFIELD(0%, 0%) = 2%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
			SMG_WINDOW::KFIELD(0%, 2%) = 4%
		SMG_WINDOW::KNAME(1%) = "Subaccount"
			SMG_WINDOW::KFIELD(1%, 0%) = 3%
			SMG_WINDOW::KFIELD(1%, 1%) = 12%
			SMG_WINDOW::KFIELD(1%, 2%) = 13%
			SMG_WINDOW::KFIELD(1%, 3%) = 1%
		SMG_WINDOW::KNAME(2%) = "xRef"
			SMG_WINDOW::KFIELD(2%, 0%) = 2%
			SMG_WINDOW::KFIELD(2%, 1%) = 7%
			SMG_WINDOW::KFIELD(2%, 2%) = 1%
		SMG_WINDOW::KNAME(3%) = "Check"
			SMG_WINDOW::KFIELD(3%, 0%) = 2%
			SMG_WINDOW::KFIELD(3%, 1%) = 10%
			SMG_WINDOW::KFIELD(3%, 2%) = 1%
		SMG_WINDOW::KNAME(4%) = "Batch"
			SMG_WINDOW::KFIELD(4%, 0%) = 1%
			SMG_WINDOW::KFIELD(4%, 1%) = 16%

		!
		! Load defaults for period file maintenance
		!
		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF GL_YYYY_PP.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF GL_YYYY_PP.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			GL_MAIN_GLMNT = ERR
			CONTINUE 770
		END WHEN

		GL_YYYY_PP.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.OPN"
		USE
			GL_MAIN_GLMNT = ERR
			CONTINUE 770
		END WHEN
		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		GL_YYYY_PP.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(GL_YYYY_PP.CH%)

		GOTO ExitFunction

790		SMG_WINDOW::CHAN  = GL_YYYY_PP.CH%
		WHEN ERROR IN
			RESET #GL_YYYY_PP.CH%
			GET #GL_YYYY_PP.CH%, REGARDLESS
		USE
			IF ERR = 154%
			THEN
				SLEEP 1%
				RETRY
			END IF

			CONTINUE 32767
		END WHEN

20100	!****************************************************************
	! Display the background
	!
	! This option is used to display the background information
	! on the screen.  It must first clear any junk on the screen,
	! and then write the background onto it.
	!****************************************************************
	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	1,  2, "(01) Account", &
			2,  2, "(02) Source", &
			3,  2, "(03) Reference", &
			4,  2, "(04) Tran Date", &
			5,  2, "(05) Description", &
			6,  2, "(06) Amount", &
			7,  2, "(07) Xref", &
			8,  2, "(08) Post Time", &
			9,  2, "(09) Post Date", &
			10,  2, "(10) Chk/Dep #", &
			11,  2, "(11) Tran Key", &
			12,  2, "(12) Sub account", &
			13,  2, "(13) Operation", &
			14,  2, "(14) Units", &
			15,  2, "(15) Hours", &
			16,  2, "(16) Batch", &
			1, 42, "Descr:", &
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

20200	!****************************************************************
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user, display
	! data, set defaults, and return the data back according to
	! MFLAG.
	!****************************************************************
	CASE OPT_ENTRY
		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View  "

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 E0Loop:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP
		CASE 1%

	!++
	! Abstract:FLD001
	!	.x Account>General Ledger
	!	^*(01) Account\*
	!	.b
	!	.lm +5
	!	This field refers to the account number in the General Ledger
	!	Chart of Accounts. If an account number which does not exist in
	!	the Chart of Accounts is entered in this field, the system will
	!	prompt the user to confirm the entry unless ^*ALLOW\* no invalid
	!	accounts has been set. If ^*ALLOW\* no invalid accounts has been
	!	set, the system will prompt the user to enter a valid account
	!	number. If the account number exists in the Chart of Accounts,
	!	the ^*Descr\* will display the name related to the account number.
	!	.b
	!	An example of an account would be ^*1000-000\*, which would
	!	represent a cash account in the Chart of Accounts. To enter this
	!	account, type ^*1000-000\* followed by a ^*<Ret>\*.
	!	.b
	!	The field will accommodate 18 characters.
	!	.lm -5
	!
	! Index:
	!	.x General Ledger>Account
	!
	!--

			GL_YYYY_PP::ACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "1;21", TEMP$, &
				GL_YYYY_PP::ACCT, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%)
				THEN
					GL_YYYY_PP::ACCT = GL_CHART::ACCT
				END IF
				GOTO E0Loop
			END IF

			IF MAIN_WINDOW(GL_MAIN_CHART.ID, &
				"Q0" + GL_YYYY_PP::ACCT) <> 1%
			THEN
				GL_CHART::DESCR = STRING$(LEN(GL_CHART::DESCR), 63%)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(GL_CHART::DESCR, 30%), &
				1%, 49%, , SMG$M_BOLD)

		CASE 2%

	!++
	! Abstract:FLD002
	!	.x Source>General Ledger
	!	^*(02) Source\*
	!	.b
	!	.lm +5
	!	The ^*Source\* is a code that represents the
	!	journal from which a particular entry came.
	!	This field will normally be defined
	!	by the system.
	!	.b
	!	Example: ^*PJ\* -  representing the Purchases Journal
	!	.br
	!	.i +9
	!	^*CD\* -  representing the Cash Disbursements Journal
	!	.b
	!	The field will accept up to four (04) characters.
	!	.lm -5
	!
	! Index:
	!	.x General Ledger>Source
	!
	!--

			GL_YYYY_PP::SOURCE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "2;21", TEMP$, &
				GL_YYYY_PP::SOURCE, MFLAG, "'E", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	.x Reference>General Ledger
	!	^*(03) Reference\*
	!	.b
	!	.lm +5
	!	This field is used for reference only.
	!	.b
	!	Example: The vendor invoice number will be shown
	!	if the entry is from the Purchases Journal, or it would be
	!	the customer invoice number if the entry came from the Sales
	!	Journal.
	!	.b
	!	Sixteen spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!	.x General Ledger>Reference
	!
	!--

			GL_YYYY_PP::REFNO = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "3;21", TEMP$, &
				GL_YYYY_PP::REFNO, MFLAG, "'E", MVALUE)

		CASE 4%

	!++
	! Abstract:FLD004
	!	.x Transaction>Date>General Ledger
	!	^*(04) Transaction Date\*
	!	.b
	!	.lm +5
	!	The ^*Transaction Date\* stores the date of the transaction.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!	.x General Ledger>Transaction>Date
	!	.x Date>Transaction>General Ledger
	!	.x General Ledger>Date>Transaction
	!
	!--

			GL_YYYY_PP::TRANDAT = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, "4;21", TEMP$, &
				GL_YYYY_PP::TRANDAT, MFLAG, "'E", MVALUE)

		CASE 5%

	!++
	! Abstract:FLD005
	!	.x Description>General Ledger
	!	^*(05) Description\*
	!	.b
	!	.lm +5
	!	The ^*Description\* field is entered with a brief description
	!	of the transaction. This field is used as a means of further
	!	identifying the transaction.
	!	.b
	!	Example:  An entry from the Purchase
	!	Journal would display the vendor name, or an entry from the
	!	Cash Receipts Journal would display the customer name.
	!	.b
	!	Thirty spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!	.x General Ledger>Description
	!
	!--

			GL_YYYY_PP::DESCR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "5;21", TEMP$, &
				GL_YYYY_PP::DESCR, MFLAG, "'E", MVALUE)

		CASE 6%

	!++
	! Abstract:FLD006
	!	^*(06) Amount\*
	!	.b
	!	.lm +5
	!	The ^*Amount\* refers to the amount of money involved in the
	!	transaction. This amount will show a (-) for a credit and
	!	a (+) for a debit. For example, a check written to a vendor
	!	will show as a (-) in the cash account, a cash receipt from
	!	a customer will show as a (+).
	!	.b
	!	The system will automatically show the amount in decimal
	!	form if the decimal is not entered.
	!	.LM -5
	!
	! Index:
	!	.x General Ledger>Amount
	!	.x Amount>General Ledger
	!
	!--

			GL_YYYY_PP::AMOUNT = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "6;21", TEMP$, &
				GL_YYYY_PP::AMOUNT, MFLAG, &
				"###,###,###,###.##", MVALUE)

		CASE 7%

	!++
	! Abstract:FLD007
	!	.x Cross Reference>General Ledger
	!	^*(07) Cross Reference\*
	!	.b
	!	.lm +5
	!	The ^*Cross Reference\* _# is the key of the employee, customer, vendor,
	!	etc., that originated the transaction.
	!	.B
	!	Ten spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!	.x General Ledger>Cross Reference
	!	.x General Ledger>Employee
	!	.x Employee>General Ledger
	!	.x General Ledger>Customer
	!	.x Customer>General Ledger
	!	.x Vendor>General Ledger
	!	.x General Ledger>Vendor
	!
	!--

			GL_YYYY_PP::XREFNO = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "7;21", TEMP$, &
				GL_YYYY_PP::XREFNO, MFLAG, "'E", MVALUE)

		CASE 8%

	!++
	! Abstract:FLD008
	!	.x Post>Time>General Ledger
	!	^*(08) Post Time\*
	!	.b
	!	.lm +5
	!	^*Post Time\* is set at the time the record is posted to
	!	the GL transaction file. This is used to determine when an
	!	event took place. The format for the time is ^*HHMMSS\* and is
	!	military time i.e. 24 hour clock.
	!	.b
	!	Example: To enter 3:00 PM, type ^*150000\*, followed by
	!	a ^*<Ret>\*. To enter 8:31 AM, type ^*083100\*, followed by a
	!	^*<Ret>\*. If seconds are not entered, ^*00\* will be assumed.
	!	.lm -5
	!
	! Index:
	!	.x Time>Post>General Ledger
	!	.x General Ledger>Post>Time
	!	.x General Ledger>Time>Post
	!
	!--

			GL_YYYY_PP::POSTIM = ENTR_3TIME(SCOPE, &
				SMG_WINDOW::WNUMBER, "8;21", TEMP$, &
				GL_YYYY_PP::POSTIM, MFLAG, "", MVALUE)

		CASE 9%

	!++
	! Abstract:FLD009
	!	.x Post>Date>General Ledger
	!	^*(09) Post Date\*
	!	.b
	!	.lm +5
	!	The ^*Post Date\* stores the date the transaction was posted.
	!	.b
	!	Example: If the user were to enter July 4, 1987,
	!	type either ^*07041987\* or ^*070487\*.
	!	.b
	!	No other format will be accepted.
	!
	! Index:
	!	.x Date>Post>General Ledger
	!	.x General Ledger>Post>Date
	!	.x General Ledger>Post>Time
	!
	!--

			GL_YYYY_PP::POSDAT = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, "9;21", TEMP$, &
				GL_YYYY_PP::POSDAT, MFLAG, "'E", MVALUE)

		CASE 10%

	!++
	! Abstract:FLD010
	!	.x Check Number>General Ledger
	!	^*(10) Check/Deposit _#\*
	!	.b
	!	.lm +5
	!	^*Check/Deposit _#\* represents the number of the check or
	!	deposit, depending on the type of transaction involved.
	!	If the source code is ^*CD\*, it is assumed to be a check, if
	!	the source code is ^*CR\*, it is assumed to be a receipt. If
	!	this field contains data and is not a cash account, it
	!	is for information only.
	!	.lm -5
	!
	! Index:
	!	.x General Ledger>Check Number
	!	.x Deposit Number>General Ledger
	!	.x General Ledger>Deposit Number
	!
	!--

			GL_YYYY_PP::CKNO = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "10;21", TEMP$, &
				GL_YYYY_PP::CKNO, MFLAG, "'E", MVALUE)

		CASE 11%

	!++
	! Abstract:FLD011
	!	.x Transaction>General Ledger
	!	^*(11) Transaction Key\*
	!	.b
	!	.lm +5
	!	The ^*Transaction Key\* is an internal reference number used
	!	to type transactions together. For example, in the Accounts
	!	Payable system a voucher number is used to type the charge
	!	invoice to the check payment of that invoice.
	!	.b
	!	This number is entered by the system.
	!	.lm -5
	!
	! Index:
	!	.x General Ledger>Transaction
	!
	!--

			GL_YYYY_PP::TRANKEY = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "11;21", TEMP$, &
				GL_YYYY_PP::TRANKEY, MFLAG, "'E", MVALUE)

		CASE 12%

	!++
	! Abstract:FLD012
	!	.x Sub Account>General Ledger
	!	^*(12) Sub account\*
	!	.b
	!	.lm +5
	!	The ^*Sub-account _#\* is used to create the cross matrix for
	!	a subsidiary ledger. This number can be a work order number,
	!	a job number, a fixed asset number, etc.
	!	.b
	!	Ten spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!	.x General Ledger>Sub Account
	!	.x Work Order>General Ledger
	!	.x General Ledger>Work Order
	!	.x Job Number>General Ledger
	!	.x General Ledger>Job Number
	!	.x Asset Number>General Ledger
	!	.x General Ledger>Asset Number
	!
	!--

			GL_YYYY_PP::SUBACC = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "12;21", TEMP$, &
				GL_YYYY_PP::SUBACC, MFLAG, "'E", MVALUE)

		CASE 13%

	!++
	! Abstract:FLD013
	!	.x Operation>General Ledger
	!	^*(13) Operation\*
	!	.b
	!	.lm +5
	!	The ^*Operation\* has reference to job phases.
	!	.b
	!	Eight spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!	.x General Ledger>Operation
	!
	!--

			GL_YYYY_PP::OPERATION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "13;21", TEMP$, &
				GL_YYYY_PP::OPERATION, MFLAG, "'E", MVALUE)

		CASE 14%

	!++
	! Abstract:FLD014
	!	^*(14) Units\*
	!	.b
	!	.lm +5
	!	The ^*Units\* (regular quantity) is used to store units.
	!	For example, hours come from the Payroll system, units
	!	sold may come from the Sales system, etc. This quantity
	!	will show a (-) for a credit and a (+) for a debit.
	!	For example, payroll vacation hours will show as a (+)
	!	as they are used and as a (-) as they are accrued.
	!	.b
	!	The system will automatically show the quantity in decimal
	!	form if a decimal is not entered.
	!	.lm -5
	!
	! Index:
	!	.x Unit>General Ledger
	!	.x General Ledger>Unit
	!
	!--

			GL_YYYY_PP::UNITS = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "14;21", TEMP$, &
				GL_YYYY_PP::UNITS, MFLAG, "###,###.##", MVALUE)

		CASE 15%

	!++
	! Abstract:FLD015
	!	^*(15) Hours\*
	!	.b
	!	.lm +5
	!	The ^*Hours\* is used to store hours. For example, hours
	!	come from the Payroll system, units sold may come from the
	!	Sales system, etc. This quantity will show a (-) for a credit
	!	and a (+) for a debit. For example, payroll vacation hours will
	!	show as a (+) as they are used and as a (-) as they are accrued.
	!	.b
	!	The system will automatically show the quantity in decimal
	!	form if a decimal is not entered.
	!	.lm -5
	!
	! Index:
	!	.x Hour>General Ledger
	!	.x General Ledger>Hour
	!
	!--

			GL_YYYY_PP::HOURS = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "15;21", TEMP$, &
				GL_YYYY_PP::HOURS, MFLAG, "###,###.##", MVALUE)

		CASE 16%

	!++
	! Abstract:FLD016
	!	^*(16) Batch\*
	!	.p
	!
	! Index:
	!
	!--
			GL_YYYY_PP::BTHNUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "16;21", TEMP$, &
				GL_YYYY_PP::BTHNUM, MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

20300	!****************************************************************
	! Test value
	!****************************************************************
	CASE OPT_TESTENTRY

		SELECT MLOOP

		CASE 1%
			GL_MAIN_GLMNT = 0%

			IF GL_YYYY_PP::ACCT = ""
			THEN
				GL_MAIN_GLMNT = 1%
				GOTO ExitFunction
			END IF

			!
			! Is the input defined?
			!
			GL_MAIN_GLMNT = FUNC_TESTENTRY( SMG_WINDOW, &
				GL_YYYY_PP::ACCT, GL_CHART::DESCR, &
				"GL", MLOOP, "ACCT", &
				"Account number", GL_MAIN_CHART.ID)

		END SELECT

	!****************************************************************
	! Set GL_CHART_OLD value
	!****************************************************************
	CASE OPT_SETOLD
		GL_YYYY_PP_OLD = GL_YYYY_PP

	!****************************************************************
	! Restore GL_YYYY_PP_OLD value
	!****************************************************************
	CASE OPT_RESETOLD
		GL_YYYY_PP = GL_YYYY_PP_OLD

	!****************************************************************
	! Set default value
	!****************************************************************
	CASE OPT_SETDEFAULT
		GL_YYYY_PP2 = GL_YYYY_PP

	!****************************************************************
	! Restore default value
	!****************************************************************
	CASE OPT_RESETDEFAULT
		GL_YYYY_PP = GL_YYYY_PP2

	!****************************************************************
	! View header
	!****************************************************************
	CASE OPT_VIEW
		SELECT MLOOP

		CASE 1%
			MVALUE = &
				"  Account            Subaccount Source " + &
				"Batch  CrossRef        Amount Tran Date " + &
				" Check # Description"

		CASE 2%
			MVALUE = "021,032,039,046,057,069,080,088"

		CASE 3%
			MVALUE = GL_YYYY_PP::ACCT + " " + &
				GL_YYYY_PP::SUBACC + " " + &
				GL_YYYY_PP::SOURCE + "   " + &
				GL_YYYY_PP::BTHNUM + " " + &
				GL_YYYY_PP::XREFNO + &
				FORMAT$(GL_YYYY_PP::AMOUNT, "#########.## ") + &
				PRNT_DATE(GL_YYYY_PP::TRANDAT, 8%) + " " + &
				GL_YYYY_PP::CKNO + "  " + &
				LEFT(GL_YYYY_PP::DESCR, 20%)

		END SELECT

	!****************************************************************
	! Find
	!****************************************************************
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			!
			! FIND according to Primary Key
			!	(Account number + Transaction date)
			!
			FIND #GL_YYYY_PP.CH%, &
				KEY #0% GE GL_YYYY_PP::ACCT + GL_YYYY_PP::TRANDAT, &
				REGARDLESS

		CASE 1%
			!
			! FIND according to First Alternate Key
			!	(Subaccount number + Operation code +
			!		Account number)
			!
			FIND #GL_YYYY_PP.CH%, &
				KEY #1% GE GL_YYYY_PP::SUBACC + &
				GL_YYYY_PP::OPERATION + GL_YYYY_PP::ACCT, &
				REGARDLESS

		CASE 2%
			!
			! FIND according to Second Alternate Key
			!	(Cross-reference number + Account number)
			!
			FIND #GL_YYYY_PP.CH%, &
				KEY #2% GE GL_YYYY_PP::XREFNO + GL_YYYY_PP::ACCT, &
				REGARDLESS

		CASE 3%
			!
			! FIND according to Third Alternate Key
			!	(Check number + Account number)
			!
			FIND #GL_YYYY_PP.CH%, &
				KEY #3% GE GL_YYYY_PP::CKNO + GL_YYYY_PP::ACCT, &
				REGARDLESS

		CASE 4%
			!
			! FIND according to Fourth Alternate Key
			!	(Batch number)
			!
			FIND #GL_YYYY_PP.CH%, &
				KEY #4% GE GL_YYYY_PP::BTHNUM + "", &
				REGARDLESS

		END SELECT

	END SELECT

 ExitFunction:
	EXIT FUNCTION

29000	!****************************************************************
	! Trap errors
	!****************************************************************

	ON ERROR GO BACK

32767	!****************************************************************
	! End of GL_MAIN_GLMNT function
	!****************************************************************
	END FUNCTION
