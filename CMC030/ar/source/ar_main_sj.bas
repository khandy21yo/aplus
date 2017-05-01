1	%TITLE "Sales Journal Header Maintenance"
	%SBTTL "AR_MAIN_SJ"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AR_MAIN_SJ(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	The ^*Maintain Sales Journal\* option
	!	maintains regular sales and service charge journals.
	!	.lm -5
	!
	! Index:
	!	.x Sales Journal>Maintain
	!	.x Maintain>Sales Journal
	!	.x Journals>Sales
	!	.x Add>Sales Journal
	!	.x Erase>Sales Journal
	!	.x Change>Sales Journal
	!	.x Sales Journal>Add
	!	.x Sales Journal>Change
	!	.x Sales Journal>Erase
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_MAIN_SJ/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP AR_MAIN_SJ
	!	$ DELETE AR_MAIN_SJ.OBJ;*
	!
	! Author:
	!
	!	07/22/87 - Kevin Handy
	!
	! Modification history:
	!
	!	02/23/88 - Aaron Redd
	!		Added hard table under AR Type
	!
	!	05/16/88 - Aaron Redd
	!		Modified to allow R/O open of file if R/W open fails.
	!
	!	08/08/89 - Kevin Handy
	!		Modified to that F17 would work while adding
	!		a customer number.
	!
	!	09/05/89 - Kevin Handy
	!		Modified so that list choices on customer number
	!		will ask for key to search in.
	!
	!	08/14/91 - Kevin Handy
	!		Removed A+.
	!
	!	04/15/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	04/22/92 - Kevin Handy
	!		Clean up (check)
	!
	!	11/04/92 - Kevin Handy
	!		Modified to show third address line.
	!
	!	01/26/93 - Dan Perkins
	!		Added option to print invoices from journal.
	!
	!	02/08/93 - Kevin Handy
	!		Added DUEDATE and DISCOUNTDATE to entry.
	!		Set SUBACCT to blank as default.
	!
	!	02/09/93 - Kevin Handy
	!		Modified to look up in terms file and calculate
	!		due date and discount date.
	!
	!	02/16/93 - Kevin Handy
	!		Modified to default customer number to previous
	!		one used, if no hard or soft default exists.
	!
	!	02/22/93 - Dan Perkins
	!		Changed "V0" to "VX" on chart of accounts to be
	!		able to list accounts starting at a particular
	!		account.
	!
	!	11/29/93 - Kevin Handy
	!		Added test on input for inactive customers.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	05/02/95 - Kevin Handy
	!		Lose FORM_LOADVAR here. Has been placed in
	!		AR_JOUR_SJ so doesn't get screwed up in the
	!		library.
	!
	!	11/20/95 - Kevin Handy
	!		Lose /NOWARN on compile statement.
	!		Lose unnecessary external definitions.
	!
	!	07/03/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/21/97 - Kevin Handy
	!		Use integer for #key
	!
	!	05/19/98 - Kevin Handy
	!		Open terms code file as 'MOD' instead of
	!		'OPN' so that you can add terms codes in
	!		using magic button.
	!
	!	05/19/98 - Kevin Handy
	!		Lose common for AR_SJL, which is not used in
	!		this module for anything.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/09/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	10/30/2000 - Kevin Handy
	!		Use A"x"B
	!		Use WHEN ERROR IN
	!
	!	02/06/2001 - Kevin Handy
	!		Add aging info to bottom of screen on customer
	!		entry.
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
	! Maps
	!
	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.HB"
	MAP (AR_CONTROL)	AR_CONTROL_CDD		AR_CONTROL

	%INCLUDE "SOURCE:[AR.OPEN]AR_SJH.HB"
	MAP (AR_SJH)		AR_SJH_CDD		AR_SJH
	MAP (AR_SJH_OLD)	AR_SJH_CDD		AR_SJH_OLD, AR_SJH2

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD		GL_CHART

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_TERMS.HB"
	MAP (UTL_TERMS)		UTL_TERMS_CDD		UTL_TERMS

	%INCLUDE "SOURCE:[AR.OPEN]AR_CUSBAL.HB"
	MAP (AR_CUSBAL)		AR_CUSBAL_CDD		AR_CUSBAL
	DIM			AR_CUSBAL_CDD		ARRAY_CUSBAL(50%)

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (TT_AR_SJH) &
		ARTITLE$ = 20%, &
		ARTYPE$(5%) = 20%

	COM (CH_AR_SJH) &
		AR_SJH.CH%, &
		AR_SJH.READONLY%

	COM (TT_AR_SJ) &
		BATCH_NO$ = 2%

	COM (CH_UTL_TERMS) &
		UTL_TERMS.CH%

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION FUNC_TESTENTRY
	EXTERNAL LONG   FUNCTION AR_OUTP_SJ
	EXTERNAL LONG	FUNCTION AR_FUNC_AGE

	!
	! Declare some variables
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
		SMG_WINDOW::DESCR = "Sales Journal " + BATCH_NO$ + &
					" Maintenance"
		SMG_WINDOW::NHELP = "AR_MAIN_SJ"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 12%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Invoice-number"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		!
		! Load in defaults
		!
		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

		!
		! AR type
		!
		ARTITLE$ = "Type   Description"
		ARTYPE$(0%) = "5"
		ARTYPE$(1%) = "01   Receivable"
		ARTYPE$(2%) = "02   Cash Sale"
		ARTYPE$(3%) = "03   Debit Memo"
		ARTYPE$(4%) = "04   Service Charge"
		ARTYPE$(5%) = "08   Credit Memo"

700		!
		! Declare channels
		!
		IF AR_SJH.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF AR_SJH.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_SJH.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			AR_MAIN_SJ = ERR
			CONTINUE 770
		END WHEN

		AR_SJH.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_SJH.OPN"
		USE
			AR_MAIN_SJ = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		AR_SJH.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(AR_SJH.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = AR_SJH.CH%
		WHEN ERROR IN
			RESET #AR_SJH.CH%
			GET #AR_SJH.CH%, REGARDLESS
		USE
			CONTINUE 795
		END WHEN

795		IF UTL_TERMS.CH% = 0%
		THEN
			WHEN ERROR IN
				%INCLUDE "SOURCE:[UTL.OPEN]UTL_TERMS.MOD"
			USE
				CONTINUE 30000
			END WHEN
		END IF

	!
	! Modify the menu
	!
	CASE OPT_OPTLIST
		MVALUE = MVALUE + " Line_items invforM invform_direcT"

	!
	! Optional menu items
	!
5000	CASE OPT_MOREMENU
		SELECT SCOPE::PRG_ITEM

		!
		! Line option
		!
		CASE "Line_items"
	!++
	! Abstract:LINE_ITEMS
	!	^*Line__items\*
	!	.b
	!	.lm +5
	!	The ^*Line__items\* portion of this screen indicates how
	!	the transaction is to be allocated to various accounts and sub__codes.
	!	.b
	!	This portion of the screen will scroll, allowing as many as forty
	!	(40) line item distributions to be made.
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
			TEMP_RFA = GETRFA(AR_SJH.CH%)

			AR_MAIN_SJ = MAIN_JOURNAL(AR_MAIN_SJ_LINE.ID, "")

		CASE "invforM"
	!++
	! Abstract:INVFORM
	!--
			V% = AR_OUTP_SJ(AR_SJH::INVNUM, BATCH_NO$, 0%)

		CASE "invform_direcT"
	!++
	! Abstract:INVFORM_DIRECT
	!--
			V% = AR_OUTP_SJ(AR_SJH::INVNUM, BATCH_NO$, 1%)

		END SELECT

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

		DATA	1,  1, "(01) Invoice", &
			2,  1, "(02)", &
			9,  1, "(03) Description", &
			1, 46, "(04) Trans date", &
			2, 46, "(05) AR/Cash", &
			4, 46, "(06) AR type", &
			5, 46, "(07) Receipt #", &
			6, 46, "(08) Check #", &
			7, 46, "(09) Deposit #", &
			9, 46, "(10) Amount", &
			10,46, "(11) Due Date", &
			11,46, "(12) Discount Date", &
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
			LEFT(AR_CONTROL::CTITLE, 10%), 2%, 6%)

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

		TEMP1% = SCOPE::SCOPE_EXIT

 E0Loop:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%
	!++
	! Abstract:FLD001
	!	.ts 55
	!	^*(01) Invoice	8 Characters\*
	!	.b
	!	.lm +5
	!	The ^*Invoice\* field maintains the invoice number or
	!	reference for a particular document.
	!	.lm -5
	!
	! Index:
	!	.x Sales Journal>Invoice Number
	!	.x Invoice>Number in Sales Journal
	!
	!--

			AR_SJH::INVNUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"1;16", TEMP$, &
				AR_SJH::INVNUM, MFLAG, "'E", MVALUE)

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Customer\*
	!	.b
	!	.lm +5
	!	The ^*Customer\* field maintains the customer code which
	!	will identify a particular customer.  This number should have previously
	!	been entered in the Customer Master Name and Address screen.
	!	.b
	!	If the code entered is valid, the customers name and
	!	address information will appear.
	!	.b
	!	Pressing ^*List Choices\* will cause a list of valid customer codes
	!	to be displayed.
	!	.b
	!	By pressing the ^*F17\* key, the system will display the screen where
	!	additional customer codes may be entered.  After the screen has been
	!	completed the system will automatically return to the
	!	Sales Journal maintenance routine.
	!	.lm -5
	!
	! Index:
	!	.x Sales Journal>Customer Number
	!	.x Customer>Customer Number in Sales Journal
	!
	!--
			AR_SJH::CUSNUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"2;17", TEMP$, &
				AR_SJH::CUSNUM, MFLAG, &
				"'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(AR_MAIN_35CUSTOM.ID, "VX  ") = 1%)
				THEN
					AR_SJH::CUSNUM = &
						AR_35CUSTOM::CUSNUM
				END IF
				GOTO E0Loop
			END IF

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F17)
			THEN
				ST% = MAIN_WINDOW(AR_MAIN_35CUSTOM.ID, &
					"M0" + AR_SJH::CUSNUM)
				GOTO E0loop
			END IF

		CASE 3%
	!++
	! Abstract:FLD003
	!	.ts 55
	!	^*(03) Description	26 Characters\*
	!	.b
	!	.lm +5
	!	The ^*Description\* field maintains the
	!	description in reference to the transaction represented by an
	!	invoice.
	!	.lm -5
	!
	! Index:
	!	.x Sales Journal>Description
	!	.x Description>Sales Journal
	!
	!--
			AR_SJH::DESCR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"9;20", TEMP$, &
				AR_SJH::DESCR, MFLAG, "'E", MVALUE)

		CASE 4%
	!++
	! Abstract:FLD004
	!	.ts 55
	!	^*(04) Transaction Date	MMDDYYYY or MMDDYY\*
	!	.b
	!	.lm +5
	!	The ^*Transaction Date\* field maintains the
	!	date for a particular transaction.
	!	.b
	!	This field requires an entry and will not default to the
	!	current date.
	!	.lm -5
	!
	! Index:
	!	.x Transaction Date>Sales Journal
	!	.x Sales Journal>Transaction Date
	!
	!--
			AR_SJH::TRADAT = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"1;64", TEMP$, &
				AR_SJH::TRADAT, MFLAG, "'E", MVALUE)

		CASE 5%
	!++
	! Abstract:FLD005
	!	.ts 55
	!	^*(05) Accounts Receivable/Cash	20 Characters\*
	!	.b
	!	.lm +5
	!	The ^*Accounts Receivable/Cash\* field maintains the appropriate
	!	"Accounts Receivable" or "Cash" account number established in the General Ledger
	!	Chart of Accounts.  The selection is dependent upon whether a sale is
	!	a charge sale or a cash sale to be recorded as a transaction in
	!	a customer's file.
	!	.b
	!	Pressing ^*List Choices\* will cause a list of General Ledger Chart
	!	of Account numbers to be displayed.
	!	.lm -5
	!
	! Index:
	!	.x Accounts Receivable Account>Sales Journal
	!	.x Cash Account>Sales Journal
	!	.x Sales Journal>Accounts Receivable Account
	!	.x Sales Journal>Cash Account
	!
	!--
			AR_SJH::ARACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"2;61", TEMP$, &
				AR_SJH::ARACCT, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX  ") = 1%)
				THEN
					AR_SJH::ARACCT = &
						GL_CHART::ACCT
				END IF
				GOTO E0Loop
			END IF

		CASE 6%
	!++
	! Abstract:FLD006
	!	.ts 55
	!	^*(06) Accounts Receivable Type	1,2,3,4,8\*
	!	.b
	!	.lm +5
	!	The ^*Accounts Receivable Type\* field defines the type of
	!	transaction represented by a record.
	!	.b
	!	Valid values are:
	!	.table 3,25
	!	.te
	!	^*01\* - Sale on Account
	!	.te
	!	^*02\* - Cash Sale
	!	.te
	!	^*03\* - Debit Memo
	!	.te
	!	^*04\* - Service Charge
	!	.te
	!	^*08\* - Credit Memo
	!	.end table
	!	Pressing ^*List Choices\* will cause a list of valid types to be displayed.
	!	.b
	!	^*Note:\* When adding a record, the next three fields
	!	(Receipt, Check, and Deposit) will be bypassed
	!	unless the value in field (06) is 02 (Cash) or 08
	!	(Credit memo).
	!	.lm -5
	!
	! Index:
	!	.x Account Receivable>Transaction Type
	!	.x Sales Journal>Account Receivable Transaction Type
	!	.x Transaction Type>Sales>Sale on Account
	!	.x Sale on Account>Transaction Type
	!	.x Transaction Type>Sales>Cash Sale
	!	.x Cash Sale>Transaction Type
	!	.x Transaction Type>Debit Memo
	!	.x Debit Memo>Transaction Type
	!	.x Transaction Type>Service Charge
	!	.x Service Charge>Transaction Type
	!	.x Transaction Type>Credit Memo
	!	.x Credit Memo>Transaction Type
	!
	!--
			AR_SJH::TRATYP = ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"4;64", TEMP$, &
				AR_SJH::TRATYP, MFLAG, "'E", MVALUE, &
				ARTYPE$(), ARTITLE$, "005")

		CASE 7%
	!++
	! Abstract:FLD007
	!	.ts 55
	!	^*(07) Receipt _#	8 Characters\*
	!	.b
	!	.lm +5
	!	The ^*Receipt _#\* field maintains the number of a
	!	receipt if issued for a cash sale. When cash sales are accounted for
	!	using cash register closing totals, a receipt number would not
	!	necessarily be applicable. This field may be bypassed.
	!	.b
	!	^*Note:\* During an add function, this field is bypassed unless the
	!	transaction type is either a Cash Sale (02) or a Credit Memo (08).
	!	.lm -5
	!
	! Index:
	!	.x Receipt #>Sales Journal
	!	.x Sales Journal>Receipt #
	!
	!--
			IF (TEMP$ = "Add") AND &
				(INSTR(1%, "02,08", AR_SJH::TRATYP) = 0%)
			THEN
				TEMP% = MFLAG OR 1%
			ELSE
				TEMP% = MFLAG
			END IF

			AR_SJH::RECNUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"5;64", TEMP$, &
				AR_SJH::RECNUM, TEMP%, "'E", MVALUE)

			SCOPE::SCOPE_EXIT = TEMP1% IF TEMP% AND 1%

		CASE 8%
	!++
	! Abstract:FLD008
	!	.ts 55
	!	^*(08) Check _#	6 Characters\*
	!	.b
	!	.lm +5
	!	The ^*Check _#\* field maintains the number of a check
	!	issued by a customer in the event of a cash sale.  If a check was
	!	not received for the sale, or if the record represents a summary of
	!	cash sales such as a register total, this field would appropriately
	!	be left blank.
	!	.b
	!	During an Add function, this field is bypassed unless the
	!	transaction type represents either a Cash Sale or a Credit Memo.
	!	.lm -5
	!
	! Index:
	!	.x Check #>Sales Journal
	!	.x Sales Journal>Check #
	!
	!--
			IF (TEMP$ = "Add") AND &
				(INSTR(1%, "02,08", AR_SJH::TRATYP) = 0%)
			THEN
				TEMP% = MFLAG OR 1%
			ELSE
				TEMP% = MFLAG
			END IF

			AR_SJH::CHECK = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"6;64", TEMP$, &
				AR_SJH::CHECK, TEMP%, "'E", MVALUE)

			SCOPE::SCOPE_EXIT = TEMP1% IF TEMP% AND 1%

		CASE 9%
	!++
	! Abstract:FLD009
	!	.ts 55
	!	^*(09) Deposit _#	6 Characters\*
	!	.b
	!	.lm +5
	!	The ^*Deposit _#\* field maintains the number
	!	of the bank deposit in which a cash receipt is included. If CMC's
	!	Check Reconciliation System is used, the deposit _# field ^&must\& contain
	!	a valid deposit number. The use of this field is optional.
	!	If the Check Reconciliation system is used, it is important that
	!	deposit numbers assigned be outside the parameters established for
	!	check numbers.
	!	.b
	!	^*Note:\* During an Add function, this field is bypassed unless the
	!	transaction type is either a Cash Sale or a Credit Memo.
	!	.lm -5
	!
	! Index:
	!	.x Deposit #>Sales Journal
	!	.x Sales Journal>Deposit #
	!
	!--
			IF (TEMP$ = "Add") AND &
				(INSTR(1%, "02,08", AR_SJH::TRATYP) = 0%)
			THEN
				TEMP% = MFLAG OR 1%
			ELSE
				TEMP% = MFLAG
			END IF

			AR_SJH::DEPOSIT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"7;64", TEMP$, &
				AR_SJH::DEPOSIT, TEMP%, "'E", MVALUE)

			SCOPE::SCOPE_EXIT = TEMP1% IF TEMP% AND 1%

		CASE 10%
	!++
	! Abstract:FLD010
	!	.x Sales Journal>Amount
	!	^*(10) Amount\*
	!	.b
	!	.lm +5
	!	The ^*Amount\* field in the Sales Journal maintains the
	!	total invoice amount, including any freight, sales taxes, etc.
	!	.b
	!	^*Note:\* Sales tax will be a separate line item in the distribution.
	!	.lm -5
	!
	! Index:
	!	.x Amount>Sales Journal
	!
	!--
			AR_SJH::AMOUNT = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"9;64", TEMP$, &
				AR_SJH::AMOUNT, MFLAG, "##,###,###.##", MVALUE)

		CASE 11%
	!++
	! Abstract:FLD011
	!	.ts 55
	!	^*(11) Due Date	MMDDYYYY or MMDDYY\*
	!
	! Index:
	!	.x Due Date>Sales Journal
	!	.x Sales Journal>Due Date
	!
	!--
			AR_SJH::DUEDATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"10;64", TEMP$, &
				AR_SJH::DUEDATE, MFLAG, "'E", MVALUE)

		CASE 12%
	!++
	! Abstract:FLD012
	!	.ts 55
	!	^*(11) Discount Date	MMDDYYYY or MMDDYY\*
	!
	! Index:
	!	.x Discount Date>Sales Journal
	!	.x Sales Journal>Discount Date
	!
	!--
			AR_SJH::DISCOUNTDATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"11;64", TEMP$, &
				AR_SJH::DISCOUNTDATE, MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		AR_MAIN_SJ = 0%

		SELECT MLOOP

		CASE 2%
			AR_MAIN_SJ, ST% = FUNC_TESTENTRY(SMG_WINDOW, &
				AR_SJH::CUSNUM, AR_35CUSTOM::CUSNAM, &
				"AR", MLOOP, "CUST", &
				"Customer number", AR_MAIN_35CUSTOM.ID)

			GOSUB DisplayBalances

			!
			! Is the customer inactive
			!
			IF (AR_35CUSTOM::SSTATUS = "I")
			THEN
				CALL ENTR_3MESSAGE(SCOPE, &
					"** This customer is marked inactive! **", &
					0%)
			END IF

		CASE 4%
20340			IF SCOPE::PRG_ITEM = "Add"
			THEN
				WHEN ERROR IN
					GET #UTL_TERMS.CH%, &
						KEY #0% EQ AR_35CUSTOM::TERMS + "", &
						REGARDLESS
				USE
					CONTINUE 30000
				END WHEN

				AR_SJH::DUEDATE = DATE_DUEDATE(AR_SJH::TRADAT, &
					UTL_TERMS::DUEDAYS * 1%, &
					UTL_TERMS::DUEDATE)

				AR_SJH::DISCOUNTDATE = &
					DATE_DUEDATE(AR_SJH::TRADAT, &
					UTL_TERMS::DISCOUNTDAYS * 1%, &
					UTL_TERMS::DISCOUNTDATE)

				JUNK% = AR_MAIN_SJ(SMG_WINDOW, &
					OPT_ENTRY, I%, 1%, MVALUE) &
					FOR I% = 11% TO 12%
			END IF

		CASE 5%
			!
			! Is the input defined?
			!
			AR_MAIN_SJ = FUNC_TESTENTRY(SMG_WINDOW, &
				AR_SJH::ARACCT, GL_CHART::DESCR, &
				"AR", MLOOP, "ACCT", &
				"Account number", GL_MAIN_CHART.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(GL_CHART::DESCR, 30%), &
				3%, 51%, , SMG$M_BOLD)

		END SELECT

	!
	! Display additional information
	!
	CASE OPT_DISPLAY

		AR_MAIN_SJ = 0%

		!
		! Is the input defined?
		!
		IF MAIN_WINDOW(GL_MAIN_CHART.ID, "Q0" + AR_SJH::ARACCT) <> 1%
		THEN
			GL_CHART::DESCR = "????????????????????"
		END IF

		!
		! Print chart description
		!
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			LEFT(GL_CHART::DESCR, 30%), &
			3%, 51%, , SMG$M_BOLD)

		GOSUB DisplayBalances

	!
	! Set AR_SJH_OLD value
	!
20500	CASE OPT_SETOLD
		AR_SJH_OLD = AR_SJH

	!
	! Restore AR_SJH_OLD value
	!
	CASE OPT_RESETOLD
		AR_SJH = AR_SJH_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		AR_SJH2 = AR_SJH

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		TEMP$ = EDIT$(AR_SJH::CUSNUM, 4%)
		AR_SJH = AR_SJH2
		AR_SJH::SUBACCT = ""
		AR_SJH::CUSNUM = TEMP$ IF AR_SJH::CUSNUM = ""

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = " Invoice   Cust#"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "011"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = AR_SJH::INVNUM + " " + &
				TRM$(AR_SJH::CUSNUM)

		END SELECT
	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #AR_SJH.CH%, &
				KEY #0% GE AR_SJH::INVNUM + "", &
				REGARDLESS
		END SELECT

	!
	! Handle finishing various options specially
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
			AR_MAIN_SJ = MAIN_JOURNAL(AR_MAIN_SJ_LINE.ID, "A")

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
			IF AR_SJH_OLD::INVNUM <> AR_SJH::INVNUM
			THEN
				TEMP$ = AR_SJH::INVNUM + ""
				AR_SJH = AR_SJH_OLD
				AR_MAIN_SJ = MAIN_JOURNAL(AR_MAIN_SJ_LINE.ID, &
					"C" + TEMP$)
			END IF

		!
		! Erase record
		!
		CASE "Erase"
			!
			! Erase any line items under the header
			!
			AR_MAIN_SJ = MAIN_JOURNAL(AR_MAIN_SJ_LINE.ID, "E")

		END SELECT

	END SELECT

	EXIT FUNCTION

 DisplayBalances:
	!*******************************************************************
	! Display customer balances
	!*******************************************************************

	!
	! Is the input defined?
	!
	ST% = 0%
	IF MAIN_WINDOW(AR_MAIN_35CUSTOM.ID, &
		"Q0" + AR_SJH::CUSNUM) <> 1%
	THEN
		ST% = -1%
		AR_35CUSTOM::CUSNUM = AR_SJH::CUSNUM
		AR_35CUSTOM::METHOD = "O"
		AR_35CUSTOM::ADD1 = STRING$(LEN(AR_35CUSTOM::ADD1), A"?"B)
		AR_35CUSTOM::ADD2 = STRING$(LEN(AR_35CUSTOM::ADD2), A"?"B)
		AR_35CUSTOM::ADD3 = STRING$(LEN(AR_35CUSTOM::ADD3), A"?"B)
		AR_35CUSTOM::CITY = STRING$(LEN(AR_35CUSTOM::CITY), A"?"B)
		AR_35CUSTOM::STATE = STRING$(LEN(AR_35CUSTOM::STATE), A"?"B)
		AR_35CUSTOM::ZIP = STRING$(LEN(AR_35CUSTOM::ZIP), A"?"B)
		AR_35CUSTOM::SSTATUS = "A"
	END IF

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
		LEFT(AR_35CUSTOM::CUSNAM, 30%), &
		3%, 6%, , SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
		LEFT(AR_35CUSTOM::ADD1, 30%), &
		4%, 6%, , SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
		LEFT(AR_35CUSTOM::ADD2, 30%), &
		5%, 6%, , SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
		LEFT(AR_35CUSTOM::ADD3, 30%), &
		6%, 6%, , SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
		LEFT(TRM$(AR_35CUSTOM::CITY) + ", " + &
			AR_35CUSTOM::STATE + " " + &
			AR_35CUSTOM::ZIP + SPACE$(30%), 30%), &
		7%, 6%, , SMG$M_BOLD)

	!
	! Zero balances
	!
	CUSBAL(J%) = 0.0 FOR J% = 0% TO 4%
	SRVCHG = 0.0

	IF AR_FUNC_AGE(AR_SJH::CUSNUM, &
		AR_35CUSTOM::METHOD, &
		DATE_TODAY, "", &
		NUM_ACCT%, ARRAY_CUSBAL()) = 0%
	THEN
		!
		! Accumulate aging information
		!
		FOR LOOP% = 1% TO NUM_ACCT%
			!
			! Customer total
			!
			CUSBAL(J%) = CUSBAL(J%) + &
				ARRAY_CUSBAL(LOOP%)::AGING(J%) &
				FOR J% = 0% TO 4%

			SRVCHG = SRVCHG + &
				ARRAY_CUSBAL(LOOP%)::CHARGE

		NEXT LOOP%

		TEXT$ = "      Cur       "
		DAYS% = 1%

		FOR I% = 1% TO 4%
			DAYS% = DAYS% + AR_CONTROL::AGEPER(I%)
			TEXT$ = TEXT$ + FORMAT$(DAYS%, "###") + "       "
		NEXT I%

		TEXT$ = TEXT$ + " Bal    CrLimit"

		SMG_STATUS% = SMG$PUT_CHARS( &
			SMG_WINDOW::WNUMBER, &
			TEXT$, 17%, 7%, , SMG$M_REVERSE)

		!
		! Accumulate balance
		!
		BALANCE = &
			FUNC_ROUND(CUSBAL(0%) + &
			CUSBAL(1%) + &
			CUSBAL(2%) + &
			CUSBAL(3%) + &
			CUSBAL(4%) + &
			SRVCHG, 2%)
	END IF

	TEXT$ = FORMAT$(CUSBAL(0%), " ######.##") + &
		FORMAT$(CUSBAL(1%), " ######.##") + &
		FORMAT$(CUSBAL(2%), " ######.##") + &
		FORMAT$(CUSBAL(3%), " ######.##") + &
		FORMAT$(CUSBAL(4%), " ######.##") + &
		FORMAT$(BALANCE, " #######.##") + &
		FORMAT$(AR_35CUSTOM::CREDITLIM, " #######.##")

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
		TEXT$, 18%, 6%)

	RETURN

29000	!***************************************************************
	! Trap errors
	!***************************************************************

	ON ERROR GO BACK

29900	!
	! Handle no header for Line function here
	!
	CALL ENTR_3MESSAGE(SCOPE, &
		"Sorry, but there is no current header item", 0%)
	GOTO 30000

30000	END FUNCTION
