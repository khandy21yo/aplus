1	%TITLE "Maintain Cash Receipt Journal"
	%SBTTL "AR_MAIN_CRJ"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AR_MAIN_CRJ(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1987,1988 BY
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
	!	The ^*Maintain Cash Receipts Journal\* option
	!	maintains cash receipt transactions.
	!	.lm -5
	!
	! Index:
	!	.x Cash Receipts>Maintenance
	!	.x Maintenance>Cash Receipts
	!	.x Journal>Cash Receipts
	!	.x Cash Receipts>Journal
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_MAIN_CRJ/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP AR_MAIN_CRJ
	!	$ DELETE AR_MAIN_CRJ.OBJ;*
	!
	! Author:
	!
	!	02/17/88 - Aaron Redd
	!
	! Modification history:
	!
	!	05/14/88 - Aaron Redd
	!		Modified to allow R/O open of file if R/W open fails.
	!
	!	08/05/88 - Kevin Handy
	!		Added OPT_DISPLAY section which had been deleted
	!		so that additional information could be displayed.
	!
	!	11/14/89 - Kevin Handy
	!		Fix bug where changing (02) to '10' could lock up
	!		the program in an eternal loop.
	!
	!	10/07/91 - Frank F. Starman
	!		Increase dimension from 400 to 800.
	!
	!	10/11/91 - Kevin Handy
	!		Added Search_invoice option to allow looking
	!		for a customer by invoice number.
	!
	!	02/24/92 - Kevin Handy
	!		Cleaned up (check)
	!
	!	02/12/93 - Dan Perkins
	!		Changed "V0" to "VX" on chart of accounts to be
	!		able to list accounts starting at a particular
	!		account.
	!
	!	03/23/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	09/16/93 - Kevin Handy
	!		Increased INV_ARRAY from 800 to 4800 (FJ).
	!
	!	10/04/93 - Kevin Handy
	!		Changed "=<" to "<="
	!
	!	10/28/93 - Kevin Handy
	!		Modified auTo_line_load into the two options
	!		auTo_date and autO_invoice.
	!
	!	11/29/93 - Kevin Handy
	!		Increased array from 4800 to 6000 for FJ.
	!
	!	01/26/94 - Kevin Handy
	!		Skip the "auto-load" after an add if we have a
	!		"balance forward" customer instead of an "open
	!		item".
	!
	!	06/27/94 - Kevin Handy
	!		Modified so that TRATYP "10" will not pull up
	!		invoice list.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	01/26/96 - Kevin Handy
	!		Reformat source code.
	!		Change STRING$(...,ASCII(" ")) to "" in several
	!		places.
	!
	!	03/12/96 - Kevin Handy
	!		Remove commented out code.
	!
	!	05/21/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/09/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	09/13/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	10/30/2000 - Kevin Handy
	!		Use A"x"B
	!
	!	02/06/2001 - Kevin Handy
	!		Add aging info to bottom of screen on customer
	!		entry.
	!
	!	02/11/2002 - Kevin Handy
	!		Added form to journal
	!
	!	09/18/2003 - Kevin Handy
	!		Added another digit for Jerry, just in case.
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
	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.HB"
	MAP (AR_CONTROL)	AR_CONTROL_CDD	AR_CONTROL

	%INCLUDE "SOURCE:[AR.OPEN]AR_CRJH.HB"
	MAP (AR_CRJH)		AR_CRJH_CDD	AR_CRJH
	MAP (AR_CRJH_OLD)	AR_CRJH_CDD	AR_CRJH_OLD, AR_CRJH2

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD	GL_CHART

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)		AR_35CUSTOM_CDD	AR_35CUSTOM

	%INCLUDE "SOURCE:[AR.OPEN]AR_CRJL.HB"
	MAP	(AR_CRJL)	AR_CRJL_CDD	AR_CRJL

	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.HB"
	MAP	(AR_OPEN)	AR_OPEN_CDD	AR_OPEN

	%INCLUDE "SOURCE:[AR.OPEN]AR_CUSBAL.HB"
	MAP (AR_CUSBAL)		AR_CUSBAL_CDD		AR_CUSBAL
	DIM			AR_CUSBAL_CDD		ARRAY_CUSBAL(50%)

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION FUNC_TESTENTRY
	EXTERNAL LONG   FUNCTION MAIN_WINDOW
	EXTERNAL LONG   FUNCTION MAIN_JOURNAL
	EXTERNAL LONG	FUNCTION AR_FUNC_AGE
	EXTERNAL LONG   FUNCTION AR_OUTP_CRJ

	DIM INV_ARRAY$(6000%)

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (TT_AR_CRJH) &
		TYPTITLE$ = 20%, &
		TYPTYPE$(2%) = 20%

	COM (CH_AR_CRJH) &
		AR_CRJH.CH%, &
		AR_CRJH.READONLY%

	COM (CH_AR_CRJL) &
		AR_CRJL.CH%, &
		AR_CRJL.READONLY%

	COM (TT_AR_CRJ) &
		BATCH_NO$ = 2%

	COM (CH_AR_OPEN) &
		AR_OPEN.CH%

	!
	! Declare some variables
	!
	DECLARE RFA TEMP_RFA

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	!********************************************************************
	! Initialization
	!
	! This option is used to initialize the window structure,
	! set up the default values for add, and open all files
	! necessary that have not already been opened.
	!
	!********************************************************************
	CASE OPT_INIT

		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Cash Receipt Journal " + BATCH_NO$ + &
			" Maintenance"
		SMG_WINDOW::NHELP = "AR_MAIN_CRJ"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 9%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Receipt-number"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		!
		! Type
		!
		TYPTITLE$ = "Type   Description"
		TYPTYPE$(0%) = "2"
		TYPTYPE$(1%) = "09  ROA."
		TYPTYPE$(2%) = "10  Cash receipt."

		!
		! Load in defaults
		!
		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF AR_CRJH.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF AR_CRJH.READONLY%
			GOTO 790
		END IF

		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_CRJH.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			AR_MAIN_CRJ = ERR
			CONTINUE 770
		END WHEN
		AR_CRJH.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_CRJH.OPN"
		USE
			AR_MAIN_CRJ = ERR
			CONTINUE 770
		END WHEN
		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		AR_CRJH.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(AR_CRJH.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = AR_CRJH.CH%
		WHEN ERROR IN
			RESET #AR_CRJH.CH%
			GET #AR_CRJH.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

	!
	! Modify the menu
	!
	CASE OPT_OPTLIST
		MVALUE = MVALUE + &
			" Line_items auTo_date autO_invoice Search_invoice invforM Ydirect"


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
	!	^*Line__Items\*
	!	.b
	!	.lm +5
	!	The ^*Line__Items\* option will provide a screen referred to as the
	!	"distribution section".
	!	.b
	!	Each customer is assigned a type or "method" of statement in the
	!	Customer Maintenance Master File. The "Method" is one of two
	!	types, (B) Balance Forward or (O) Open Item. If the cash receipt
	!	noted on the screen is received from a customer and assigned the
	!	"Open Method", the receipt will be applied to the customer's
	!	invoice(s), designated invoice or to the lowest order invoice.
	!	A "Balance Forward" customer, generally, would only have
	!	one line item to enter.
	!	.b
	!	All receipts from customers would be credited to the accounts
	!	receivable account number (the default number). In instances when
	!	money is received for other than accounts receivable, the default
	!	account number would be overridden.
	!	.lm -5
	!
	! Index:
	!	.x Cash Receipts>Line Items
	!	.x Line Items>Cash Receipts
	!
	!--
			!
			! Make sure there is a header
			!
			TEMP_RFA = GETRFA(AR_CRJH.CH%)

			AR_MAIN_CRJ = MAIN_JOURNAL(AR_MAIN_CRJ_LINE.ID, "")

		!
		! Auto load of lines
		!
		CASE "auTo_date"

	!++
	! Abstract:AUTO_LOAD_LINES
	!	^*auTo-load-lines\*
	!	.b
	!	.lm +5
	!	The ^*Auto Load Lines\* function adds
	!	information concerning invoices to be paid, rather than entering the
	!	information manually in the ^*Lines\* function.
	!	.b
	!	When executed, the screen will display a list of all current invoices for
	!	the customer, and allow selection of those invoices to be paid.
	!	.b
	!	Use the arrow keys to move through the list, and press ^*Select\* to
	!	choose the invoices that are to be paid.  Press ^*Return\*
	!	after selections are complete.
	!	.b
	!	Press the ^*F17\* key (\*Special Function\*) to automatically select
	!	oldest invoices to be paid.
	!	.lm -5
	!
	! Index:
	!	.x Auto Load>Cash Receipts
	!	.x Cash Receipts>Auto Load
	!
	!--

	!++
	!Abstract:AUTO_DATE
	!	.p
	!	^*Auto__date\* option will provide for selection
	!	of multiple invoices for cash receipts and will load the
	!	distribution section of the process with the selected
	!	invoices automatically.  User may then perform selected
	!	processes under the ^*Line__Items\* process.
	!--
			LISTORDER% = 0%
			GOSUB ListInvoices

		!
		! Auto load of lines
		!
		CASE "autO_invoice"

	!++
	!Abstract:AUTO_INVOICE
	!	.p
	!	^*Auto__invoice\* option will provide for selection
	!	of multiple invoices for cash receipts and will load the
	!	distribution section of the process with the selected
	!	invoices automatically.  User may then perform selected
	!	processes under the ^*Line__Items\* process.
	!--
			LISTORDER% = 1%
			GOSUB ListInvoices

		!
		! Auto load of lines
		!
		CASE "Search_invoice"

	!++
	!Abstract:SEARCH_INVOICE
	!	.p
	!--

			V% = MAIN_WINDOW(AR_MAIN_LEDMAINT.ID, "VX  ")

		CASE "invforM"
	!++
	! Abstract:INVFORM
	!--
			V% = AR_OUTP_CRJ(AR_CRJH::RECNUM, BATCH_NO$, 0%)

		CASE "Ydirect"
	!++
	! Abstract:YDIRECT
	!--
			V% = AR_OUTP_CRJ(AR_CRJH::RECNUM, BATCH_NO$, 1%)

			END SELECT

	!********************************************************************
	!
	! Display the background
	!
	! This option is used to display the background information on the
	! screen.  It must first clear any junk on the screen, and then
	! write the background onto it.
	!
	!********************************************************************
20100	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	1,  1, "(01) Receipt", &
			2,  1, "(02) A/R Type", &
			3,  1, "(03)", &
			9,  1, "(04) Description", &
			1, 46, "(05) Trans date", &
			2, 46, "(06) Account", &
			5, 46, "(07) Check #", &
			6, 46, "(08) Deposit #", &
			9, 46, "(09) Amount", &
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
			LEFT(AR_CONTROL::CTITLE, 10%), 3%, 6%)

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

20200	!********************************************************************
	!
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user, display data,
	! set defaults, and return the data back according to MFLAG.
	!
	!********************************************************************
	CASE OPT_ENTRY

		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

		TEMP1% = SCOPE::SCOPE_EXIT

 E0Loop:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%
	!++
	! Abstract:FLD001
	!	.x Cash Receipts>Receipt
	!	^*(01) Receipt\*
	!	.b
	!	.lm +5
	!	The ^*Receipt\* field is provided to enter the number of
	!	the receipt issued for payment received from the customer.
	!	.b
	!	The field will accept up to eight (8) characters.
	!	.lm -5
	!
	! Index:
	!	.x Receipt>Cash Receipts
	!
	!--
			AR_CRJH::RECNUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"1;16", TEMP$, &
				AR_CRJH::RECNUM, MFLAG, "'E", MVALUE)

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Accounts Receivable Type\*
	!	.b
	!	.lm +5
	!	The ^*Accounts Receivable Type\* field will designate whether the receipt is
	!	to be applied as a credit to an Account Receivable or whether
	!	the record is a miscellaneous receipt having no relationship
	!	to an Account Receivable.
	!	.b
	!	Valid values are:
	!	.table 3,25
	!	.te
	!	^*09\* - ROA - Apply as a credit to an Account
	!	################Receivable.
	!	.te
	!	^*10\* - Miscellaneous receipt, no relationship to an
	!	#########Account Receivable.
	!	.end table
	!	^*Note:\*  When the receipt type is "10", the cursor will skip
	!	to field 04 (Description), since the customer number
	!	for this type of receipt has no applicability.
	!	.lm -5
	!
	! Index:
	!	.x AR Type>Cash Receipts
	!	.x Cash Receipts>AR Type
	!
	!--
			AR_CRJH::TRATYP = &
				ENTR_3STRINGLIST(SCOPE, SMG_WINDOW::WNUMBER, &
				"2;16", "", &
				AR_CRJH::TRATYP, MFLAG, "'E", MVALUE, &
				TYPTYPE$(), TYPTITLE$, "005")

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Customer \*
	!	.b
	!	.lm +5
	!	The ^*Customer\* is to be entered with the customer which has been
	!	established in the Customer Master File.
	!	.b
	!	Pressing ^*List Choices\* will provide a
	!	list of valid customers.
	!	.b
	!	^*Note:\* This field will be bypassed if the type (field 02)
	!	is ^*10 - Miscellaneous Receipt\*.
	!	.lm -5
	!
	! Index:
	!	.x Cash Receipts>Customer
	!	.x Customer>Cash Receipts
	!
	!--
			IF (TEMP$ = "Add") AND (AR_CRJH::TRATYP = "10")
			THEN
				AR_CRJH::CUSNUM = ""
				TEMP% = MFLAG OR 1%
			ELSE
				TEMP% = MFLAG
			END IF

			AR_CRJH::CUSNUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"3;17", TEMP$, &
				AR_CRJH::CUSNUM, TEMP%, "'E", MVALUE)


			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_F14
				IF (MAIN_WINDOW(AR_MAIN_35CUSTOM.ID, &
					"VX  ") = 1%)
				THEN
					AR_CRJH::CUSNUM = &
						AR_35CUSTOM::CUSNUM
				END IF
				GOTO E0Loop


			CASE SMG$K_TRM_F17
				IF (MAIN_WINDOW(AR_MAIN_35CUSTOM.ID, &
					"M0" + AR_CRJH::CUSNUM) = 1%)
				THEN
					AR_CRJH::CUSNUM = &
						AR_35CUSTOM::CUSNUM
				END IF
				GOTO E0Loop

			END SELECT

			SCOPE::SCOPE_EXIT = TEMP1% IF TEMP% AND 1%


		CASE 4%
	!++
	! Abstract:FLD004
	!	.x Cash Receipts>Description
	!	^*(04) Description\*
	!	.b
	!	.lm +5
	!	The ^*Description\* field will contain a description of the
	!	transactions. If the transaction is an AR Type "09", a
	!	comment in this field would generally have little value.
	!	.b
	!	Twenty-five (25) spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!	.x Description>Cash Receipts
	!
	!--
			AR_CRJH::DESCR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"9;20", TEMP$, &
				AR_CRJH::DESCR, MFLAG, "'E", MVALUE)

		CASE 5%
	!++
	! Abstract:FLD005
	!	.x Transaction Date>Cash Receipts
	!	^*(05) Transaction date\*
	!	.b
	!	.lm +5
	!	The ^*Transaction date\* field will be entered with the date
	!	the receipt was received.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!	.x Cash Receipts>Transaction Date
	!
	!--
			AR_CRJH::TRADAT = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"1;64", TEMP$, &
				AR_CRJH::TRADAT, MFLAG, "'E", MVALUE)

		CASE 6%
	!++
	! Abstract:FLD006
	!	^*(06) Account\*
	!	.b
	!	.lm +5
	!	The ^*Account\* field is to be entered with the General
	!	Ledger cash account number to which this receipt will be debited.
	!	.b
	!	Pressing ^*List Choices\* will provide a list of valid
	!	General Ledger account numbers.
	!	.lm -5
	!
	! Index:
	!	.x Cash Receipts>Account
	!	.x Account>Cash Receipts
	!
	!--
			AR_CRJH::ACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"2;61", TEMP$, &
				AR_CRJH::ACCT, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%)
				THEN
					AR_CRJH::ACCT = &
						GL_CHART::ACCT
				END IF
				GOTO E0Loop
			END IF

		CASE 7%
	!++
	! Abstract:FLD007
	!	^*(07) Check _#\*
	!	.b
	!	.lm +5
	!	The ^*Check _#\* field is provided to enter the number
	!	of the check received for payment in this transaction. If
	!	payment were made in cash, CASH could be typed.
	!	.b
	!	Six (6) spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!	.x Check #>Cash Receipts
	!	.x Cash Receipts>Check #
	!
	!--
			AR_CRJH::CHECK = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"5;64", TEMP$, &
				AR_CRJH::CHECK, MFLAG, "'E", MVALUE)

		CASE 8%
	!++
	! Abstract:FLD008
	!	^*(08) Deposit _#\*
	!	.b
	!	.lm +5
	!	The ^*Deposit _#\* field refers to the number of the bank deposit
	!	in which a cash receipt is included.
	!	.lm -5
	!
	! Index:
	!	.x Cash Receipts>Deposit #
	!	.x Deposit #>Cash Receipts
	!
	!--
			AR_CRJH::DEPOSIT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"6;64", TEMP$, &
				AR_CRJH::DEPOSIT, MFLAG, "'E", MVALUE)

		CASE 9%
	!++
	! Abstract:FLD009
	!	^*(09) Amount\*
	!	.b
	!	.lm +5
	!	The ^*Amount\* field will contain the total amount received
	!	for a particular transaction.
	!	.b
	!	The field will accommodate a number as large as a plus (+) or
	!	a minus (-) 9999999.99.
	!	.lm -5
	!
	! Index:
	!	.x Cash Receipts>Amount
	!	.x Amount>Cash Receipts
	!
	!--
			AR_CRJH::AMNT = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"9;64", TEMP$, &
				AR_CRJH::AMNT, MFLAG, "#########.##", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		AR_MAIN_CRJ = 0%

		SELECT MLOOP

		CASE 1%
			IF AR_CRJH::RECNUM = ""
			THEN
				AR_MAIN_CRJ = 1%
			ELSE
				IF (MVALUE = "ADD")
				THEN
					WHEN ERROR IN
						GET #AR_CRJH.CH%, &
							KEY #0% EQ AR_CRJH::RECNUM + "", &
							REGARDLESS
					USE
						CONTINUE 32767 IF ERR = 155%
						EXIT HANDLER
					END WHEN

					AR_MAIN_CRJ = 2%
					CALL ENTR_3MESSAGE(SCOPE, &
						"Record Already Exists", 1%)
				END IF
			END IF

		CASE 3%
			!
			! Check customer if transaction type is not "10"
			!
			IF AR_CRJH::TRATYP = "10"
			THEN
				AR_35CUSTOM::CUSNAM = ""
				AR_35CUSTOM::ADD1 = ""
				AR_35CUSTOM::ADD2 = ""
				AR_35CUSTOM::CITY = ""
				AR_35CUSTOM::STATE = ""
				AR_35CUSTOM::ZIP = ""
				AR_35CUSTOM::SALESMAN = ""
			ELSE
				!
				! Is the input defined?
				!
				AR_MAIN_CRJ, ST% = FUNC_TESTENTRY(SMG_WINDOW, &
					AR_CRJH::CUSNUM, AR_35CUSTOM::CUSNAM, &
					"AR", MLOOP, "CUST", &
					"Customer number", AR_MAIN_35CUSTOM.ID)

			END IF

			GOSUB DisplayBalances

		CASE 6%
			!
			! Is the input defined?
			!
			AR_MAIN_CRJ = FUNC_TESTENTRY(SMG_WINDOW, &
				AR_CRJH::ACCT, GL_CHART::DESCR, &
				"AR", MLOOP, "ACCT", &
				"Account number", GL_MAIN_CHART.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(GL_CHART::DESCR, 30%), &
				3%, 51%, , SMG$M_BOLD)

		END SELECT

	!
	! Display information
	!
	CASE OPT_DISPLAY

		GOSUB DisplayBalances

		IF MAIN_WINDOW(GL_MAIN_CHART.ID, "Q0" + AR_CRJH::ACCT) <> 1%
		THEN
			GL_CHART::DESCR = STRING$(LEN(GL_CHART::DESCR), A"?"B)
		END IF

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			LEFT(GL_CHART::DESCR, 30%), &
			3%, 51%, , SMG$M_BOLD)

	!
	! Set AR_CRJH_OLD value
	!
20500	CASE OPT_SETOLD
		AR_CRJH_OLD = AR_CRJH

	!
	! Restore AR_CRJH_OLD value
	!
	CASE OPT_RESETOLD
		AR_CRJH = AR_CRJH_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		AR_CRJH2 = AR_CRJH

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		AR_CRJH = AR_CRJH2

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
			MVALUE = AR_CRJH::RECNUM + " " + &
				TRM$(AR_CRJH::CUSNUM)

		END SELECT
	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #AR_CRJH.CH%, &
				KEY #0% GE AR_CRJH::RECNUM + "", &
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
			! Force the auto load function
			! even if she doesn't like it!
			!
			IF (AR_35CUSTOM::METHOD <> "B") AND &
				(AR_CRJH::TRATYP <> "10")
			THEN
				LISTORDER% = 0%
				GOSUB ListInvoices
			END IF

			!
			! Go to  line items also
			!
			AR_MAIN_CRJ = MAIN_JOURNAL(AR_MAIN_CRJ_LINE.ID, "")

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
			IF AR_CRJH_OLD::RECNUM <> AR_CRJH::RECNUM
			THEN
				TEMP$ = AR_CRJH::RECNUM + ""
				AR_CRJH = AR_CRJH_OLD
				AR_MAIN_CRJ = &
					MAIN_JOURNAL(AR_MAIN_CRJ_LINE.ID, &
					"C" + TEMP$)
			END IF

		!
		! Erase record
		!
		CASE "Erase"
			!
			! Erase any line items under the header
			!
			AR_MAIN_CRJ = MAIN_JOURNAL(AR_MAIN_CRJ_LINE.ID, "E")

		END SELECT

	END SELECT

28000	EXIT FUNCTION

 ListInvoices:
28100	!*******************************************************************
	! Create a list of invoices and ask for which one they want.
	!*******************************************************************

	AR_OPEN.ENDFLAG% = 0%

	NEXTLINE$ = "000"

	AMOUNTLEFT = AR_CRJH::AMNT


	!
	! If the AR_OPEN file has not yet been opened, then open it.
	!
	GOTO 28105 IF AR_OPEN.CH% > 0%

	!
	! Open main file (existing) for modification
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.OPN"
	USE
		CONTINUE 28290
	END WHEN

28105	GOTO 28110 IF AR_CRJL.CH% > 0%

	!
	! Open line file for adding into the sucker
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CRJL.CRE"
	USE
		CONTINUE 28290
	END WHEN

	AR_CRJL.READONLY% = 0%

28110	!
	! The open item file should now be opened.  Search for
	! The first record for this customer.
	!
	INV_ARRAY% = 0%
	WHEN ERROR IN
		GET #AR_OPEN.CH%, KEY #0% GE AR_CRJH::CUSNUM + "", REGARDLESS
	USE
		CONTINUE 28200
	END WHEN

28120	!
	! Skip out if done with this customer
	!
	GOTO 28200 IF AR_OPEN::CUSNUM <> AR_CRJH::CUSNUM

	!
	! Keep track of the current invoice.
	!
	INV_DATE$, INV_DATE1$ = AR_OPEN::TRADAT + ""
	INV_DATE1$ = "00000000" IF AR_OPEN::TRATYP = "04"	! Sort to front
	INV_INVNUM$ = AR_OPEN::INVNUM + ""
	INV_DESCR$ = AR_OPEN::DESCR + ""
	IF AR_OPEN::TRATYP = "02"
	THEN
		INV_AMOUNT = 0.0
	ELSE
		INV_AMOUNT = AR_OPEN::SALAMT
	END IF

28130	!
	! Pull in additional information for this invoice
	!
	WHEN ERROR IN
		GET #AR_OPEN.CH%, REGARDLESS
	USE
		AR_OPEN.ENDFLAG% = -1%
		CONTINUE 28135 IF ERR = 11%
		EXIT HANDLER
	END WHEN

	IF (AR_OPEN::CUSNUM = AR_CRJH::CUSNUM) AND &
		(AR_OPEN::INVNUM = INV_INVNUM$)
	THEN
		INV_AMOUNT = INV_AMOUNT + AR_OPEN::SALAMT &
			UNLESS AR_OPEN::TRATYP = "02"
		GOTO 28130
	END IF

28135	!
	! Skip if paid off
	!
	GOTO 28200 IF INV_AMOUNT = 0.0 AND AR_OPEN.ENDFLAG% = -1%

	GOTO 28120 IF INV_AMOUNT = 0.0

	!
	! New user friendly stuff goes here
	!
	WHEN ERROR IN
		FIND #AR_CRJL.CH%, &
			KEY #0% GE AR_CRJH::RECNUM + "", &
			REGARDLESS
	USE
		CONTINUE 28139
	END WHEN

28136	WHEN ERROR IN
		GET #AR_CRJL.CH%
	USE
		CONTINUE 28139 IF ERR = 11%
		EXIT HANDLER
	END WHEN

	GOTO 28139 IF AR_CRJL::RECNUM <> AR_CRJH::RECNUM

	NEXTLINE$ = AR_CRJL::LLINE IF AR_CRJL::LLINE > NEXTLINE$

	GOTO 28136 IF AR_CRJL::INVNUM <> INV_INVNUM$

	INV_AMOUNT = INV_AMOUNT + AR_CRJL::AMOUNT

	AMOUNTLEFT = AMOUNTLEFT + AR_CRJL::AMOUNT

	GOTO 28136

28139	INV_AMOUNT = FUNC_ROUND(INV_AMOUNT, 2%)

	GOTO 28200 IF INV_AMOUNT = 0.0 AND AR_OPEN.ENDFLAG% = -1%

	GOTO 28120 IF INV_AMOUNT = 0.0

	TEXT$ = &
		INV_DATE1$ + INV_DATE$ + &
		INV_INVNUM$ + &
		INV_DESCR$ + &
		FORMAT$(INV_AMOUNT, "########.##")

	!
	! Insert into list in invoice number order
	!
	IF LISTORDER%
	THEN
		INV_ARRAY% = INV_ARRAY% + 1%
		INV_ARRAY$(INV_ARRAY%) = TEXT$
		GOTO 28150
	END IF

	!
	! Insert into list in invoice date order
	!
	GOTO 28140 IF TEXT$ < INV_ARRAY$(LOOP%) &
		FOR LOOP% = 1% TO INV_ARRAY%
	LOOP% = INV_ARRAY% + 1%

28140	INV_ARRAY$(LOOP1% + 1%) = INV_ARRAY$(LOOP1%) &
		FOR LOOP1% = INV_ARRAY% TO LOOP% STEP -1%
	INV_ARRAY$(LOOP%) = TEXT$
	INV_ARRAY% = INV_ARRAY% + 1%

28150	GOTO 28120 IF AR_OPEN.ENDFLAG% = 0%

28200	!
	! We should now have a good list.
	! Now reformat the list into a user readable format
	! and use enter_choices to select one of them.
	!
	INV_ARRAY$(0%) = NUM1$(INV_ARRAY%)
	FOR LOOP% = 1% TO INV_ARRAY%

		INV_ARRAY$(LOOP%) = &
			"  " + &
			MID(INV_ARRAY$(LOOP%), 17%, 8%) + " " + &
			PRNT_DATE(MID(INV_ARRAY$(LOOP%), 9%, 8%), 8%) + " " + &
			MID(INV_ARRAY$(LOOP%), 25%, 25%) + " " + &
			RIGHT(INV_ARRAY$(LOOP%), 50%)
	NEXT LOOP%

	TEMP% = ENTR_3CHOICE(SCOPE, "", &
		"", &
		INV_ARRAY$(), &
		SELECTED$, &
		32%, &
		"List of Current Invoices  - unapplied amount: " + &
		FORMAT$(AMOUNTLEFT, "##,###,###.##"), &
		"013,024,050", &
		0%)

	SELECT SCOPE::SCOPE_EXIT

	CASE SMG$K_TRM_F17

		TOTALINV = 0.0
		LOOP% = 0%
 TakeNext:
		GOTO 28290 IF TOTALINV >= AMOUNTLEFT

		LOOP% = LOOP% + 1%
		GOTO 28290 IF LOOP% > INV_ARRAY%
		LINEAMT = VAL(RIGHT(INV_ARRAY$(LOOP%), 49%))
		GOTO TakeNext IF LINEAMT <= 0.0

		LINEAMT = AMOUNTLEFT - TOTALINV &
			IF AMOUNTLEFT - TOTALINV < LINEAMT
		AR_CRJL::RECNUM = AR_CRJH::RECNUM
		AR_CRJL::TRATYP = "1"
		AR_CRJL::LLINE = FORMAT$(VAL%(NEXTLINE$) + 1%, "<0>##")
		AR_CRJL::INVNUM = MID(INV_ARRAY$(LOOP%), 3%, 8%)
		AR_CRJL::AMOUNT = -LINEAMT

		NEXTLINE$ = AR_CRJL::LLINE

		GOSUB LookupAccount

		PUT #AR_CRJL.CH%

		TOTALINV = TOTALINV + LINEAMT

		GOTO TakeNext

	END SELECT

28250	GOTO 28290 IF SELECTED$ = ""

	X% = POS(SELECTED$, ",", 1%)
	Y% = X% + 1%
	X% = X% - 1%

	GOTO 28290 IF X% <= 0%

	TEMP% = VAL%(LEFT(SELECTED$, X%))

	SELECTED$ = RIGHT(SELECTED$, Y%)

	AR_CRJL::RECNUM = AR_CRJH::RECNUM
	AR_CRJL::TRATYP = "1"
	AR_CRJL::LLINE = FORMAT$(VAL%(NEXTLINE$) + 1%, "<0>##")
	AR_CRJL::INVNUM = MID(INV_ARRAY$(TEMP%), 3%, 8%)
	AR_CRJL::AMOUNT = -VAL(RIGHT(INV_ARRAY$(TEMP%), 49%))

	NEXTLINE$ = AR_CRJL::LLINE

	GOSUB LookupAccount

	PUT #AR_CRJL.CH%

	GOTO 28250

28290	RETURN

	%PAGE

 LookupAccount:
	!*******************************************************************
	! Look up account number for this invoice in the accounts receivable
	! register, and place it in the crj line item if apporpriate.
	!*******************************************************************

28300	WHEN ERROR IN
		GET #AR_OPEN.CH%, &
			KEY #0% EQ AR_CRJH::CUSNUM + AR_CRJL::INVNUM, &
			REGARDLESS
	USE
		CONTINUE 28390
	END WHEN

	AR_CRJL::ACCT = AR_OPEN::ARACCT

	AR_CRJL::SALNUM = AR_OPEN::SALNUM

28390	RETURN

	%PAGE

 DisplayBalances:
	!*******************************************************************
	! Display customer balances
	!*******************************************************************

	!
	! Is the input defined?
	!
	ST% = 0%

	IF AR_CRJH::TRATYP = "10"
	THEN
		AR_35CUSTOM::CUSNAM = ""
		AR_35CUSTOM::ADD1 = ""
		AR_35CUSTOM::ADD2 = ""
		AR_35CUSTOM::CITY = ""
		AR_35CUSTOM::STATE = ""
		AR_35CUSTOM::ZIP = ""
		AR_35CUSTOM::SALESMAN = ""
	ELSE
		IF MAIN_WINDOW(AR_MAIN_35CUSTOM.ID, &
			"Q0" + AR_CRJH::CUSNUM) <> 1%
		THEN
			ST% = -1%
			AR_35CUSTOM::CUSNUM = AR_CRJH::CUSNUM
			AR_35CUSTOM::METHOD = "O"
			AR_35CUSTOM::CUSNAM = &
				STRING$(LEN(AR_35CUSTOM::CUSNAM), A"?"B)
			AR_35CUSTOM::ADD1 = &
				STRING$(LEN(AR_35CUSTOM::ADD1), A"?"B)
			AR_35CUSTOM::ADD2 = &
				STRING$(LEN(AR_35CUSTOM::ADD2), A"?"B)
			AR_35CUSTOM::ADD3 = &
				STRING$(LEN(AR_35CUSTOM::ADD3), A"?"B)
			AR_35CUSTOM::CITY = &
				STRING$(LEN(AR_35CUSTOM::CITY), A"?"B)
			AR_35CUSTOM::STATE = &
				STRING$(LEN(AR_35CUSTOM::STATE), A"?"B)
			AR_35CUSTOM::ZIP = &
				STRING$(LEN(AR_35CUSTOM::ZIP), A"?"B)
			AR_35CUSTOM::SSTATUS = "A"
		END IF
	END IF

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
		LEFT(AR_35CUSTOM::CUSNAM, 30%), &
		4%, 6%, , SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
		LEFT(AR_35CUSTOM::ADD1, 30%), &
		5%, 6%, , SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
		LEFT(AR_35CUSTOM::ADD2, 30%), &
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

	IF AR_CRJH::TRATYP <> "10"
	THEN
		IF AR_FUNC_AGE(AR_CRJH::CUSNUM, &
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
	END IF

	TEXT$ = "      Cur       "
	DAYS% = 1%

	FOR I% = 1% TO 4%
		DAYS% = DAYS% + AR_CONTROL::AGEPER(I%)
		TEXT$ = TEXT$ + FORMAT$(DAYS%, "###") + "       "
	NEXT I%

	TEXT$ = TEXT$ + "  Bal    CrLimit   "

	SMG_STATUS% = SMG$PUT_CHARS( &
		SMG_WINDOW::WNUMBER, &
		TEXT$, 17%, 7%, , SMG$M_REVERSE)

	TEXT$ = FORMAT$(CUSBAL(0%), "########.##") + &
		FORMAT$(CUSBAL(1%), "#######.##") + &
		FORMAT$(CUSBAL(2%), "#######.##") + &
		FORMAT$(CUSBAL(3%), "#######.##") + &
		FORMAT$(CUSBAL(4%), "#######.##") + &
		FORMAT$(BALANCE, "########.##")   + &
		FORMAT$(AR_35CUSTOM::CREDITLIM, "########.##")

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
		TEXT$, 18%, 5%)

	RETURN


29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
