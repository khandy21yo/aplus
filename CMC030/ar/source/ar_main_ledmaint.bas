1	%TITLE "Maintain Ledger File"
	%SBTTL "AR_MAIN_LEDMAINT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AR_MAIN_LEDMAINT(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	.b
	!	.lm +5
	!	Provides access to ^*Maintain Ledger File\* which will initialize
	!	data pertinent to each customer when the Accounts Receivable system
	!	is first installed.  Therefore, the file will be updated with information
	!	pertaining to each invoice and cash receipt as it is updated
	!	from both the Sales Journal and Cash Receipts Journal.
	!	.lm -5
	!
	! Index:
	!	.x Maintain>Ledger File
	!	.x Ledger File>Maintain
	!
	! Option:
	!
	!	AR_MAIN_OPEN_DIST$HELP
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_MAIN_LEDMAINT/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP AR_MAIN_LEDMAINT
	!	$ DELETE AR_MAIN_LEDMAINT.OBJ;*
	!
	! Author:
	!
	!	02/23/88 - Aaron Redd
	!
	! Modification history:
	!
	!	05/16/88 - Aaron Redd
	!		Modified to allow R/O open of file if R/W open fails.
	!
	!	08/05/88 - Kevin Handy
	!		Fixed title so "Customer" showed up on proper
	!		line (01).
	!
	!	08/05/88 - Kevin Handy
	!		Added salesman field.
	!		Fixed so that test for account number occured in
	!		correct field.
	!
	!	01/09/89 - Kevin Handy
	!		Modified to allow searching for a batch number.
	!
	!	05/31/91 - Kevin Handy
	!		Added Distribution file.
	!
	!	10/10/91 - Kevin Handy
	!		Added search keys for salesman and invoice numbers.
	!		(Modified file for invoice number).
	!
	!	04/15/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	04/22/92 - Kevin Handy
	!		Clean up (check)
	!
	!	02/10/93 - Dan Perkins
	!		Added fields for DUEDATE and DISCOUNTDATE.
	!
	!	02/22/93 - Dan Perkins
	!		Changed "V0" to "VX" on chart of accounts to be
	!		able to list accounts starting at a particular
	!		account.
	!
	!	02/26/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	03/23/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/11/93 - Kevin Handy
	!		Removed comments in OPT_AFTEROPT since it's their
	!		fault if they duplicate, and most likely they won't.
	!
	!	05/11/93 - Kevin Handy
	!		Modified OPT_AFTEROPT CHANGE so that it passed
	!		change key in same order as AR_MAIN_OPEN_DIST
	!		expected to receive it (CUS,INV) not (INV,CUS)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	06/17/96 - Kevin Handy
	!		Add transaction type to view.
	!
	!	06/19/96 - Kevin Handy
	!		Add transaction type "11.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/09/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	04/05/2001 - Kevin Handy
	!		Fix display problem on FJ with bad invoice numbers
	!		(null character at end)
	!
	!	12/06/2002 - Kevin Handy
	!		Allow for million dollar entries
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	!
	! Map's
	!
	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.HB"
	MAP (AR_CONTROL)	AR_CONTROL_CDD		AR_CONTROL

	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.HB"
	MAP (AR_OPEN)		AR_OPEN_CDD		AR_OPEN
	MAP (AR_OPEN_OLD)	AR_OPEN_CDD		AR_OPEN_OLD, AR_OPEN2

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD		GL_CHART

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

	COM (TT_AR_LEDMAINT) &
		ARTITLE$ = 20%, &
		ARTYPE$(9%) = 20%

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_AR_OPEN) &
		AR_OPEN.CH%, &
		AR_OPEN.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION MAIN_WINDOW
	EXTERNAL LONG    FUNCTION MAIN_JOURNAL
	EXTERNAL LONG    FUNCTION FUNC_TESTENTRY

	!
	! Declare variables
	!
	DECLARE RFA TEMP_RFA

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
		SMG_WINDOW::DESCR = "AR Open Ledger Maintenance"
		SMG_WINDOW::NHELP = "AR_MAIN_LEDMAINT"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 130%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 17%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 4%
		SMG_WINDOW::KNAME(0%) = TRM$(AR_CONTROL::CTITLE)
			SMG_WINDOW::KFIELD(0%, 0%) = 2%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
			SMG_WINDOW::KFIELD(0%, 2%) = 2%
		SMG_WINDOW::KNAME(1%) = "Batch"
			SMG_WINDOW::KFIELD(1%, 0%) = 2%
			SMG_WINDOW::KFIELD(1%, 1%) = 15%
			SMG_WINDOW::KFIELD(1%, 2%) = 1%
		SMG_WINDOW::KNAME(2%) = "Salesman"
			SMG_WINDOW::KFIELD(2%, 0%) = 3%
			SMG_WINDOW::KFIELD(2%, 1%) = 17%
			SMG_WINDOW::KFIELD(2%, 2%) = 1%
			SMG_WINDOW::KFIELD(2%, 3%) = 2%
		SMG_WINDOW::KNAME(3%) = "Invoice"
			SMG_WINDOW::KFIELD(3%, 0%) = 1%
			SMG_WINDOW::KFIELD(3%, 1%) = 2%
			SMG_WINDOW::KFIELD(3%, 2%) = 1%

		!
		! AR type
		!
		ARTITLE$ = "Type   Description"
		ARTYPE$(0%) = "8"
		ARTYPE$(1%) = "01   Receivable"
		ARTYPE$(2%) = "02   Cash Sale"
		ARTYPE$(3%) = "03   Debit Memo"
		ARTYPE$(4%) = "04   Service Charge"
		ARTYPE$(5%) = "08   Credit Memo"
		ARTYPE$(6%) = "09   ROA"
		ARTYPE$(7%) = "10   Cash Receipt"
		ARTYPE$(8%) = "11   Adjustment"

		!
		! Load in defaults
		!
		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF AR_OPEN.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF AR_OPEN.READONLY%

			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			AR_MAIN_LEDMAINT = ERR
			CONTINUE 770
		END WHEN

		AR_OPEN.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.OPN"
		USE
			AR_MAIN_LEDMAINT = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		AR_OPEN.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(AR_OPEN.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = AR_OPEN.CH%
		WHEN ERROR IN
			RESET #AR_OPEN.CH%
			GET #AR_OPEN.CH%, REGARDLESS
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

		DATA	1,  3, "(01)", &
			2,  3, "(02) Invoice", &
			3,  3, "(03) Type", &
			4,  3, "(04) Tran Date", &
			5,  3, "(05) Due Date", &
			6,  3, "(06) Disc Date", &
			7,  3, "(07) Amount", &
			8,  3, "(08) Discount", &
			9,  3, "(09) Other", &
			10, 3, "(10) Receipt No", &
			11, 3, "(11) Check No", &
			12, 3, "(12) Account", &
			13, 3, "(13) Matter #", &
			14, 3, "(14) Description", &
			15, 3, "(15) Batch", &
			16, 3, "(16) Update", &
			17, 3, "(17) Salesman", &
			0,  0, ""

		RESTORE

		READ XPOS%, YPOS%, XSTR$
		I% = 0%

		WHILE (XPOS% <> 0%)
			I% = I% + 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				XSTR$, XPOS%, YPOS%) &
				IF (SMG_WINDOW::HFLAG(I%) AND 2%) = 0%

			READ XPOS%, YPOS%, XSTR$
		NEXT

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			LEFT(AR_CONTROL::CTITLE, 14%), 1%, 8%)

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

	!
	! Select function
	!
	CASE OPT_OPTLIST

		MVALUE = MVALUE + " cLosed diStribution"

	!
	! Direction
	!
	CASE OPT_MOREMENU

		SELECT SCOPE::PRG_ITEM
		CASE "cLosed"
	!++
	! Abstract:CLOSED
	!	^*Closed\*
	!	.b
	!	.lm +5
	!	The ^*Closed\* COMMAND accesses the register history file of all accounts
	!	which have been closed. The balance of all accounts which have been closed
	!	must be zero.
	!	.lm -5
	!
	! Index:
	!	.x Closed
	!
	!--
			!
			! Access closed file
			!
			V% = MAIN_WINDOW(AR_MAIN_CLOSEMAINT.ID, "")

		!
		! Line option
		!
		CASE "diStribution"
	!++
	! Abstract:DISTRIBUTION
	!	^*diStribution\*
	!	.b
	!	.lm +5
	!	The ^*Distribution\* COMMAND indicates how
	!	the transaction was allocated to various accounts and sub__codes.
	!	.b
	!	This portion of the screen will scroll, allowing as many as forty
	!	(40) line item distributions to be viewed.
	!	.lm -5
	!
	! Index:
	!	.x Sales Journal>Line Items
	!	.x Line Items>Sales Journal
	!
	!--
			!
			! Make sure there is a header
			!
			TEMP_RFA = GETRFA(AR_OPEN.CH%)

			AR_MAIN_LEDMAINT = MAIN_JOURNAL(AR_MAIN_OPEN_DIST.ID, "")

		END SELECT

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

		SCOPE::SCOPE_EXIT = 0%

 E0loop:	SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Customer\*
	!	.b
	!	.lm +5
	!	The ^*Customer\* number refers to the number assigned to
	!	a customer as recorded in the Customer Master File.
	!	.b
	!	^*Note:\* It is ^*not\* recommended that Customer Numbers
	!	be changed, nor that records be added or deleted
	!	in this option. Nevertheless, the ability to
	!	accomplish these kinds of edit functions is
	!	provided, but should be used in very unusual
	!	circumstances only.
	!	.lm -5
	!
	! Index:
	!	.x Customer>Open Ledger Maintenance
	!	.x Open Ledger Maintenance>Customer
	!
	!--

			AR_OPEN::CUSNUM = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"1;21", TEMP$, &
				AR_OPEN::CUSNUM, MFLAG, "'E", MVALUE)

				IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
				THEN
					IF (MAIN_WINDOW(AR_MAIN_35CUSTOM.ID, "VX  ") = 1%)
					THEN
						AR_OPEN::CUSNUM = &
							AR_35CUSTOM::CUSNUM
					END IF
					GOTO E0Loop
				END IF

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Invoice\*
	!	.b
	!	.lm +5
	!	The ^*Invoice\* number refers to a reference number whether
	!	the record represents an invoice or credit memo.
	!	.b
	!	^*Note:\*  It is ^*not\* recommended that Invoice Numbers be
	!	changed in this option. Nevertheless, the ability
	!	to accomplish this function is provided, but should
	!	be used in very unusual circumstances only.
	!	.lm -5
	!
	! Index:
	!	.x Invoice>Open Ledger Maintenance
	!	.x Open Ledger Maintenance>Invoice
	!
	!--

			AR_OPEN::INVNUM = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"2;21", TEMP$, &
				AR_OPEN::INVNUM, MFLAG, "'E", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Type\*
	!	.b
	!	.lm +5
	!	The ^*Type\* field contains a code which indicates the
	!	type of transaction this screen represents.
	!	.b
	!	Valid types are:
	!	.table 3,25
	!	.te
	!	^*01\*	Invoice - Entered through Sales Journal.
	!	.te
	!	^*02\*	Cash Sale - Entered through Sales Journal.
	!	.te
	!	^*03\*	Debit Memo - Entered through Sales Journal.
	!	.te
	!	^*04\*	Service Charge - Created
	!	by service
	!	charge calculation process.
	!	.te
	!	^*08\*	Credit Memo - Entered through Sales Journal.
	!	.te
	!	^*09\*	Cash Received on Account - Entered through Cash Receipts
	!	Journal.
	!	.tE
	!	^*10\*	Cash received - Cash received other.
	!	.end table
	!	.lm -5
	!	.NOTE
	!	^*It is not recommended that the type field be
	!	changed in this screen, even though the ability
	!	to accomplish this edit function is provided.\*
	!	.end note
	!
	! Index:
	!	.x Type>Open Ledger Maintenance
	!	.x Open Ledger Maintenance>Type
	!
	!--

			AR_OPEN::TRATYP = ENTR_3STRINGLIST(SCOPE, SMG_WINDOW::WNUMBER, &
				"3;21", TEMP$, &
				AR_OPEN::TRATYP, MFLAG, "'E", MVALUE, &
				ARTYPE$(), ARTITLE$, "005")

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Transaction Date\*
	!	.p
	!	The ^*Transaction Date\* field represents the date of a
	!	particular transaction.
	!
	! Index:
	!	.x Transaction Date>Open Ledger Maintenance
	!	.x Open Ledger Maintenance>Transaction Date
	!
	!--

			AR_OPEN::TRADAT = ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"4;21", TEMP$, &
				AR_OPEN::TRADAT, MFLAG, "'E", MVALUE)

		CASE 5%

	!++
	! Abstract:FLD005
	!	^*(05) Due Date\*
	!	.p
	!	The ^*Transaction Date\* field represents the date of a
	!	particular transaction on which the amount becomes due.
	!
	! Index:
	!	.x Due Date>Open Ledger Maintenance
	!	.x Open Ledger Maintenance>Due Date
	!
	!--

			AR_OPEN::DUEDATE = ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"5;21", TEMP$, &
				AR_OPEN::DUEDATE, MFLAG, "'E", MVALUE)

		CASE 6%

	!++
	! Abstract:FLD006
	!	^*(06) Discount Date\*
	!	.p
	!	The ^*Discount Date\* field represents the date of a particular
	!	transaction on which a discount may be taken.
	!
	! Index:
	!	.x Discount Date>Open Ledger Maintenance
	!	.x Open Ledger Maintenance>Discount Date
	!
	!--

			AR_OPEN::DISCOUNTDATE = ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"6;21", TEMP$, &
				AR_OPEN::DISCOUNTDATE, MFLAG, "'E", MVALUE)

		CASE 7%

	!++
	! Abstract:FLD007
	!	^*(07) Amount\*
	!	.b
	!	.lm +5
	!	The ^*Amount\* field displays the gross amount of the
	!	transaction, whether it is a "Sale" or a "Receipt".
	!	.b
	!	^*Note:\*  It is ^*not\* recommended that the Amount field be
	!	changed in this screen, even though the ability
	!	to do so exists.
	!	.lm -5
	!
	! Index:
	!	.x Amount>Open Ledger Maintenance
	!	.x Open Ledger Maintenance>Amount
	!
	!--

			AR_OPEN::SALAMT = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"7;21", TEMP$, &
				AR_OPEN::SALAMT, MFLAG, "########.##", MVALUE)

		CASE 8%

	!++
	! Abstract:FLD008
	!	^*(08) Discount\*
	!	.b
	!	.lm +5
	!	The ^*Discount\* field contains a discount amount
	!	if applicable.
	!	.b
	!	^*Note:\* It is ^*not\* recommended that the discount amount
	!	be changed in this screen, even though the ability
	!	to do so exists.
	!	.lm -5
	!
	! Index:
	!	.x Discount>Open Ledger Maintenance
	!	.x Open Ledger Maintenance>Discount
	!
	!--

			AR_OPEN::DISAMT = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"8;21", TEMP$, &
				AR_OPEN::DISAMT, MFLAG, "########.##", MVALUE)

		CASE 9%

	!++
	! Abstract:FLD009
	!	^*(09) Other\*
	!	.b
	!	.lm +5
	!	The ^*Other\* field contains any other charges that
	!	pertain to a "Sale".
	!	.b
	!	This may includes Sales Tax, Freight, etc.
	!	.b
	!	^*Note:\* It is ^*not\* recommended that changes be made
	!	in this screen, even though the ability to do
	!	so exists.
	!	.lm -5
	!
	! Index:
	!	.x Other>Open Ledger Maintenance
	!	.x Open Ledger Maintenance>Other
	!
	!--

			AR_OPEN::OTHCHG = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"9;21", TEMP$, &
				AR_OPEN::OTHCHG, MFLAG, "########.##", MVALUE)

		CASE 10%

	!++
	! Abstract:FLD010
	!	^*(10) Receipt\*
	!	.b
	!	.lm +5
	!	The ^*Receipt\* field contains the receipt number when
	!	the transaction is a cash receipt type.
	!	.b
	!	^*Note:\*  It is ^*not\* recommended that changes be made
	!	in this screen, even though the ability to do
	!	so exists.
	!	.lm -5
	!
	! Index:
	!	.x Receipt>Open Ledger Maintenance
	!	.x Open Ledger Maintenance>Receipt
	!
	!--

			AR_OPEN::RECNUM = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"10;21", TEMP$, &
				AR_OPEN::RECNUM, MFLAG, "'E", MVALUE)

		CASE 11%

	!++
	! Abstract:FLD011
	!	^*(11) Check Number\*
	!	.b
	!	.lm +5
	!	The ^*Check Number\* field contains the customers check number
	!	when the type of transaction is a cash receipt.
	!	.lm -5
	!
	! Index:
	!	.x Check Number>Open Ledger Maintenance
	!	.x Open Ledger Maintenance>Check Number
	!
	!--

			AR_OPEN::CHKNUM = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"11;21", TEMP$, &
				AR_OPEN::CHKNUM, MFLAG, "'E", MVALUE)

		CASE 12%

	!++
	! Abstract:FLD012
	!	^*(12) Account\*
	!	.b
	!	.lm +5
	!	The ^*Account\* number field contains the General Ledger
	!	Cash account number whenever the transaction is a cash sale,
	!	or the Accounts Receivable General Ledger account number if the
	!	transaction relates to an "on account" transaction.
	!	.b
	!	^*Note:\* It is ^*not\* recommended that changes be made
	!	in this screen, even though the ability to do
	!	so exists.
	!	.lm -5
	!
	! Index:
	!	.x Account>Open Ledger Maintenance
	!	.x Open Ledger Maintenance>Account
	!
	!--

			AR_OPEN::ARACCT = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"12;21", TEMP$, &
				AR_OPEN::ARACCT, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX  ") = 1%)
				THEN
					AR_OPEN::ARACCT = &
						GL_CHART::ACCT
				END IF
				GOTO E0Loop
			END IF

		CASE 13%

	!++
	! Abstract:FLD013
	!	^*(13) Matter _#\*
	!	.b
	!	.lm +5
	!	The ^*Matter _#\*  field contains the identification number of different
	!	transactions for each customer. They are used especially for billing when
	!	the customer has more than one transaction with the organization.
	!	.b
	!	^*Note:\* It is ^*not\* recommended that changes be made
	!	in this screen, even though the ability to do
	!	so exists.
	!	.lm -5
	!
	! Index:
	!	.x Matter _#>Open Ledger Maintenance
	!	.x Open Ledger Maintenance>Matter _#
	!
	!--

			AR_OPEN::SUBACC = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"13;21", TEMP$, &
				AR_OPEN::SUBACC, MFLAG, "~L0'E", MVALUE)

		CASE 14%

	!++
	! Abstract:FLD014
	!	^*(14) Description\*
	!	.b
	!	.lm +5
	!	The ^*Description\* field contains the description for
	!	a transaction if one were entered in the Receipts Journal
	!	or the Sales Journal.
	!	.lm -5
	!
	! Index:
	!	.x Description>Open Ledger Maintenance
	!	.x Open Ledger Maintenance>Description
	!
	!--

			AR_OPEN::DESCR = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"14;21", TEMP$, &
				AR_OPEN::DESCR, MFLAG, "'E", MVALUE)

		CASE 15%

	!++
	! Abstract:FLD015
	!	^*(15) Batch\*
	!	.b
	!	.lm +5
	!	The ^*Batch\* field refers to the posting batch number that was assigned
	!	during the posting process. The number contains six (6) characters and is
	!	assigned by the posting process. The user has no control over this number.
	!	.b
	!	^*Note:\*  It is ^*not\* recommended that changes be made
	!	in this screen, even though
	!	the ability to do so exists.
	!	.lm -5
	!
	! Index:
	!	.x Batch>Open Ledger Maintenance
	!	.x Open Ledger Maintenance>Batch
	!
	!--

			AR_OPEN::BATCH = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"15;21", TEMP$, &
				AR_OPEN::BATCH, MFLAG, "'E", MVALUE)

		CASE 16%

	!++
	! Abstract:FLD016
	!	^*(16) Update\*
	!	.b
	!	.lm +5
	!	The ^*Update\* field contains the month, day, and year,
	!	in which a transaction is updated.
	!	.b
	!	^*Note:\*  It is ^*not\* recommended that changes be made
	!	in this field, even though the ability to do
	!	so exists.
	!	.lm -5
	!
	! Index:
	!
	!--

			AR_OPEN::UPDATED = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"16;21", TEMP$, &
				AR_OPEN::UPDATED, MFLAG, "'E", MVALUE)

		CASE 17%
	!++
	! Abstract:FLD017
	!	^*(17) Salesman\*
	!	.b
	!	.lm +5
	!	The ^*Salesman\* field refers to the person who made and completed the sale
	!	of the merchandise or services.
	!	.lm -5
	!
	! Index:
	!	.x Salesman>Open Ledger Maintenance
	!	.x Open Ledger Maintenance>Salesman
	!
	!--
			AR_OPEN::SALNUM = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"17;21", TEMP$, &
				AR_OPEN::SALNUM, MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

20300	CASE OPT_TESTENTRY

		AR_MAIN_LEDMAINT = 0%

		SELECT MLOOP

		CASE 1%
	!++
	! Abstract:FLD001
	!	^*(01) Customer\*
	!	.b
	!	.lm +5
	!	The ^*Customer\* number refers to the number assigned to
	!	a customer as recorded in the Customer Master File.
	!	.b
	!	^*Note:\* It is ^*not\* recommended that Customer Numbers
	!	be changed, nor that records be added or deleted
	!	in this option. Nevertheless, the ability to
	!	accomplish these kinds of edit functions is
	!	provided, but should be used in very unusual
	!	circumstances only.
	!	.lm -5
	!
	! Index:
	!	.x Customer>Open Ledger Maintenance
	!	.x Open Ledger Maintenance>Customer
	!
	!--
			AR_MAIN_LEDMAINT = FUNC_TESTENTRY(SMG_WINDOW, &
				AR_OPEN::CUSNUM, AR_35CUSTOM::CUSNAM, &
				"AR", MLOOP, "CUST", &
				"Customer number", AR_MAIN_35CUSTOM.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(AR_35CUSTOM::CUSNAM, 30%), &
				1%, 45%, , SMG$M_BOLD)

		CASE 12%
			AR_MAIN_LEDMAINT = FUNC_TESTENTRY(SMG_WINDOW, &
				AR_OPEN::ARACCT, GL_CHART::DESCR, &
				"AR", MLOOP, "ACCT", &
				"Account number", GL_MAIN_CHART.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(GL_CHART::DESCR, 30%), &
				12%, 45%, , SMG$M_BOLD)

		END SELECT

	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(1%) AND 2%) = 0%
		THEN
			IF MAIN_WINDOW(AR_MAIN_35CUSTOM.ID, "Q0" + AR_OPEN::CUSNUM) <> 1%
			THEN
				AR_35CUSTOM::CUSNAM = "????????????????????"
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(AR_35CUSTOM::CUSNAM, 30%), &
				1%, 45%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(12%) AND 2%) = 0%
		THEN
			IF MAIN_WINDOW(GL_MAIN_CHART.ID, "Q0" + AR_OPEN::ARACCT) <> 1%
			THEN
				GL_CHART::DESCR = "????????????????????"
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(GL_CHART::DESCR, 30%), &
				12%, 45%, , SMG$M_BOLD)
		END IF

20500	CASE OPT_SETOLD
		AR_OPEN_OLD = AR_OPEN

	CASE OPT_RESETOLD
		AR_OPEN = AR_OPEN_OLD

	CASE OPT_SETDEFAULT
		AR_OPEN2 = AR_OPEN

	CASE OPT_RESETDEFAULT
		AR_OPEN = AR_OPEN2

	CASE OPT_VIEW
		SELECT MLOOP

		CASE 1%
			MVALUE = LEFT(AR_CONTROL::CTITLE, 10%) + &
				"   Inv Num  Tp Date      " + &
				"   Amount   Sales Tx Description" + &
				"               Batch"

		CASE 2%
			MVALUE = "013,022,025,036,047,056,082"

		CASE 3%
			!
			! Hack to lose nasty characters from invoice numbers
			! (Common problem on FJ due to PP bugs)
			!
			INVNUM$ = AR_OPEN::INVNUM
			LSET INVNUM$ = EDIT$(AR_OPEN::INVNUM, 4%)

			MVALUE = AR_OPEN::CUSNUM + " " + &
				INVNUM$ + " " + &
				AR_OPEN::TRATYP + " " + &
				PRNT_DATE(AR_OPEN::TRADAT, 8%) + &
				FORMAT$(AR_OPEN::SALAMT, "########.##") + &
				FORMAT$(AR_OPEN::OTHCHG, "######.##") + " " + &
				AR_OPEN::DESCR + " " + &
				AR_OPEN::BATCH

		END SELECT

	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #AR_OPEN.CH%, &
				KEY #0% GE AR_OPEN::CUSNUM + AR_OPEN::INVNUM, &
				REGARDLESS

		CASE 1%
			FIND #AR_OPEN.CH%, &
				KEY #1% GE AR_OPEN::BATCH + AR_OPEN::CUSNUM, &
				REGARDLESS

		CASE 2%
			FIND #AR_OPEN.CH%, &
				KEY #2% GE AR_OPEN::SALNUM + AR_OPEN::CUSNUM + &
				AR_OPEN::INVNUM, &
				REGARDLESS

		CASE 3%
			FIND #AR_OPEN.CH%, &
				KEY #3% GE AR_OPEN::INVNUM + AR_OPEN::CUSNUM, &
				REGARDLESS

		END SELECT

	!
	! Handle finishing various options specially
	!
	! Currently commented out because I'm, not sure exactly how to handle
	! it where it ain't exactly a 1-to-1 match.
	!
	CASE OPT_AFTEROPT

		SELECT SCOPE::PRG_ITEM

		!
		! Add records
		!
		CASE "Add"
			!
			! Add line items also
			!
			AR_MAIN_OPEN = MAIN_JOURNAL(AR_MAIN_OPEN_DIST.ID, "A")

		!
		! Change records
		!
		CASE "Change"
			!
			! Change line items to match new header
			! if the key was changed.
			!
			! The original record must be the one in the
			! MAP for this to be able to work.  The new
			! key is passed through the QUERY$ variable.
			!
			IF (AR_OPEN_OLD::INVNUM <> AR_OPEN::INVNUM) OR &
				(AR_OPEN_OLD::CUSNUM <> AR_OPEN::CUSNUM)
			THEN
				TEMP$ = AR_OPEN::CUSNUM + AR_OPEN::INVNUM
				AR_OPEN = AR_OPEN_OLD
				AR_MAIN_LEDMAINT = &
					MAIN_JOURNAL(AR_MAIN_OPEN_DIST.ID, &
					"C" + TEMP$)
			END IF

		!
		! Erase record
		!
		CASE "Erase"
			!
			! Erase any line items under the header
			!
			AR_MAIN_LEDMAINT = &
				MAIN_JOURNAL(AR_MAIN_OPEN_DIST.ID, "E")

		END SELECT

	END SELECT

	EXIT FUNCTION

29000	!***************************************************************
	! Trap Errors
	!***************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
