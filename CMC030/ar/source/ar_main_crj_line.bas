1	%TITLE "Cash Receipt Journal Line Maintenance"
	%SBTTL "AR_MAIN_CRJ_LINE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AR_MAIN_CRJ_LINE(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	The ^*Line-Items\* option will provide a screen referred to as the
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
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_MAIN_CRJ_LINE/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP AR_MAIN_CRJ_LINE
	!	$ DELETE AR_MAIN_CRJ_LINE.OBJ;*
	!
	! Author:
	!
	!	02/18/88 - Aaron Redd
	!
	! Modification history:
	!
	!	02/23/88 - Aaron Redd
	!		Added hard table under Type field
	!
	!	05/16/88 - Aaron Redd
	!		Modified to allow R/O open of file if R/W open fails.
	!
	!	08/05/88 - Kevin Handy
	!		Added salesman field to journal, fixed bug when reach
	!		end of AR_OPEN file in invoice search.
	!
	!	04/20/90 - Kevin Handy
	!		Modified to put account number from AR Open file
	!		if it can find the invoice there at the time the
	!		invoice number is entered or changed.
	!
	!	04/30/90 - Kevin Handy
	!		Modified so that invoice type "2" will not stop
	!		for an invoice number, and will force the invoice
	!		number to be blank.
	!
	!	04/30/90 - Kevin Handy
	!		Fixed bug that caused an undefined invoice number
	!		would completely abort the add.
	!
	!	01/24/91 - Val James Allen
	!		Attempting to make this process "USER FRIENDLY"
	!		but who knows?
	!
	!	12/23/91 - Dan Perkins
	!		Have the salesman default to AR_35CUSTOM salesman
	!		if nothing is entered in the field.
	!
	!	02/01/93 - Dan Perkins
	!		Added one more digit to amount to compensate for
	!		negative numbers.
	!
	!	02/12/93 - Dan Perkins
	!		Changed "V0" to "VX" on chart of accounts to be
	!		able to list accounts starting at a particular
	!		account.
	!
	!	03/23/93 - Kevin Handy
	!		Modifications to display acount description for
	!		the account number entered.
	!
	!	05/24/93 - Kevin Handy
	!		Modified to that lookup in AR_OPEN will not change
	!		account or salesman to question marks if no ar found.
	!		This is because it messes up any defaults the user
	!		sets up for them, and blanks (normal default) are
	!		nicer than question marks.
	!
	!	01/04/93 - Kevin Handy
	!		Increased dimension from 300 to 1000.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	03/12/96 - Kevin Handy
	!		Lose commented out code.
	!
	!	06/20/96 - Kevin Handy
	!		Added line type "3", adjustment, which is to
	!		post to register as type "11" instead of "09".
	!		Reformat source code.
	!
	!	05/21/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/08/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	09/13/2000 - Kevin Handy
	!		Use WHEN ERROR IN
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
	%INCLUDE "FUNC_INCLUDE:SA_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[AR.OPEN]AR_CRJL.HB"
	MAP	(AR_CRJL)	AR_CRJL_CDD	AR_CRJL
	MAP	(AR_CRJL_OLD)	AR_CRJL_CDD	AR_CRJL_OLD, AR_CRJL2, AR_CRJL3

	%INCLUDE "SOURCE:[AR.OPEN]AR_CRJH.HB"
	MAP	(AR_CRJH)	AR_CRJH_CDD	AR_CRJH

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP	(GL_CHART)	GL_CHART_CDD	GL_CHART

	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.HB"
	MAP	(AR_OPEN)	AR_OPEN_CDD	AR_OPEN

	%INCLUDE "SOURCE:[SA.OPEN]SA_SALESMAN.HB"
	MAP (SB_SUBACCOUNT)	SA_SALESMAN_CDD		SA_SALESMAN

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)		AR_35CUSTOM_CDD	AR_35CUSTOM

	!
	! Create array to contain pointers and totals
	!
	RECORD RARRAY_RECORD
		RFA	LINRFA		! Rfa pointer for record
		STRING	SLINE = 3%	! Line number
		REAL	AMOUNT		! Amount for record
	END RECORD

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (TT_AR_CRJL) RARRAY_RECORD RARRAY(1000%)

	COM (CH_AR_CRJH) &
		AR_CRJH.CH%

	COM (TT_AR_CRJ) &
		BATCH_NO$ = 2%

	COM (CH_AR_OPEN) AR_OPEN.CH%

	COM (TT_AR_CRJL1) &
		TYPTITLE$ = 20%, &
		TYPTYPE$(4%) = 20%

	COM (CH_AR_CRJL) &
		AR_CRJL.CH%, &
		AR_CRJL.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION FUNC_TESTENTRY
	EXTERNAL LONG    FUNCTION MAIN_WINDOW


	!
	! Dimension statements
	!
	DIM INV_ARRAY$(400%)

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	!**********************************************************************
	! Initialization
	!
	! This option is used to initialize the window structure,
	! set up the default values for add, and open all files
	! necessary that have not already been opened.
	!
	!**********************************************************************
	CASE OPT_INIT

		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Line items"
		SMG_WINDOW::NHELP = "AR_MAIN_CRJ_LINE"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 8%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 12%
		SMG_WINDOW::NITEMS= 7%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::TOPLIN = 3%
		SMG_WINDOW::BOTLIN = 7%

		!
		! Type
		!
		TYPTITLE$ = "Type   Description"
		TYPTYPE$(0%) = "3"
		TYPTYPE$(1%) = "1   Apply to AR."
		TYPTYPE$(2%) = "2   Apply to other."
		TYPTYPE$(3%) = "3   Adjustment."

		!
		! Load in defaults
		!
		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF AR_CRJL.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF AR_CRJL.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_CRJL.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			AR_MAIN_CRJ_LINE = ERR
			CONTINUE 770
		END WHEN

		AR_CRJL.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_CRJL.OPN"
		USE
			AR_MAIN_CRJ_LINE = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		AR_CRJL.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(AR_CRJL.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = AR_CRJL.CH%
		WHEN ERROR IN
			RESET #AR_CRJL.CH%
			GET #AR_CRJL.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

20100	!**********************************************************************
	!
	! Display the background
	!
	! This option is used to display the background information on the
	! screen.  It must first clear any junk on the screen, and then
	! write the background onto it.
	!
	!**********************************************************************
	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"  (01)   (02)          (03)                                (04)    (05)" + &
			SPACE$(16%), 1%, 1%, , SMG$M_REVERSE)
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"  Type Invoice       Account                              Amount   Salesman" + &
			SPACE$(15%), 2%, 1%, , SMG$M_REVERSE)

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

	!
	! Extra display stuff
	!
	CASE OPT_DISPLAY

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		!
		! Generate totals
		!
		AMOUNT = AR_CRJH::AMNT

		FOR I% = 1% TO SMG_WINDOW::TOTREC

			AMOUNT = AMOUNT + RARRAY(I%)::AMOUNT

		NEXT I%

		!
		! Display totals
		!
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"Number of lines" + &
			FORMAT$(SMG_WINDOW::TOTREC, "###") + &
			SPACE$(17%) + &
			FORMAT$(AMOUNT, " ##,###,###.##") + SPACE$(31%), &
			SMG_WINDOW::VSIZE, 1%, , SMG$M_REVERSE)

		!
		! Paint lines on screen
		!
		FOR I% = 1% TO 5%

			A% = VAL%(MID("007,016,035,052,067", I% * 4% - 3%, 3%))

			SMG_STATUS% = SMG$DRAW_LINE(SMG_WINDOW::WNUMBER, &
				1%, A%, SMG_WINDOW::BOTLIN, A%)

		NEXT I%

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

20200	!**********************************************************************
	!
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user, display data,
	! set defaults, and return the data back according to MFLAG.
	!
	!**********************************************************************
	CASE OPT_ENTRY

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
	!	.b
	!	.lm +5
	!	The ^*Type\* field is provided to enter a code which will identify
	!	whether the payment (money) received pertains to an Accounts Receivable
	!	account or other type of account.
	!	.b
	!	Valid values are:
	!	.table 3,25
	!	.te
	!	^*1\* Apply to AR
	!	.te
	!	^*2\* Credit to Other
	!	.te
	!	*3 Adjustment.
	!	.end table
	!	Pressing ^*List Choices\* will provide a list of
	!	valid type codes.
	!	.lm -5
	!
	! Index:
	!	.x Type>Cash Receipts
	!	.x Cash Receipts>Type
	!
	!--
			AR_CRJL::TRATYP = ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				XLINE$ + ";3", TEMP$, &
				AR_CRJL::TRATYP, MFLAG, "'", MVALUE, &
				TYPTYPE$(), TYPTITLE$, "005")

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Invoice Number\*
	!	.b
	!	.lm +5
	!	The ^*Invoice Number\* field refers to the number of the
	!	invoice to which payment is to be applied. This field is
	!	used only for an "open item" customer.
	!	.b
	!	Pressing ^*List Choices\* will display a list
	!	of the customer's open invoices in the file.
	!	.lm -5
	!
	! Index:
	!	.x Invoice Number>Cash Receipts
	!	.x Cash Receipts>Invoice Number
	!
	!--
			IF (AR_CRJL::TRATYP = "2")
			THEN
				TEMP_MFLAG% = MFLAG OR 1%
				AR_CRJL::INVNUM = "" IF (MFLAG AND 1%) = 0%
			ELSE
				TEMP_MFLAG% = MFLAG
			END IF

			AR_CRJL::INVNUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				XLINE$ + ";8", TEMP$, &
				AR_CRJL::INVNUM, TEMP_MFLAG%, &
				"'LLLLLLL", MVALUE)

			IF (TEMP_MFLAG% AND 1%) = 0%
			THEN
				SELECT SCOPE::SCOPE_EXIT
				CASE SMG$K_TRM_F14
					GOSUB ListInvoices
					GOTO E0Loop

				END SELECT

				GOSUB LookupAccount
			END IF

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Account\*
	!	.b
	!	.lm +5
	!	The ^*Account\* number field will contain the General Ledger
	!	Chart of Accounts number to which this receipt is to be credited.
	!	If the receipt is being applied to an account receivable, this
	!	would be the Accounts Receivable GL number.
	!	.b
	!	Pressing ^*List Choices\* will provide a list of
	!	valid Chart of Account numbers.
	!	.lm -5
	!
	! Index:
	!	.x Cash Receipts>Account
	!	.x Account>Cash Receipts
	!
	!--
			AR_CRJL::ACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				XLINE$ + ";17", TEMP$, &
				AR_CRJL::ACCT, MFLAG, &
				"'LLLLLLLLLLLLLLLLL", MVALUE)

				IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
				THEN
					IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%)
					THEN
						AR_CRJL::ACCT = GL_CHART::ACCT
					END IF
					GOTO E0Loop
				END IF

				IF MFLAG AND 1%
				THEN
					!
					! Is the input defined?
					!
					TEMP% = FUNC_TESTENTRY(SMG_WINDOW, &
						AR_CRJL::ACCT, GL_CHART::DESCR, &
						"AR", MLOOP, "ACCT", &
						"Account number", GL_MAIN_CHART.ID)

					TEMP% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
						LEFT(GL_CHART::DESCR, 16%), SMG_WINDOW::CURLIN, 36%)
				END IF
		CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Amount\*
	!	.b
	!	.lm +5
	!	The ^*Amount\* field will contain the amount to be applied
	!	to a particular invoice if the receipt is being applied to an
	!	open item. The total amount owed on the invoice will be
	!	automatically displayed. The displayed amount may be
	!	accepted by pressing ^*Do\*, or overridden by typing the
	!	appropriate amount and pressing ^*Do\*.
	!	.b
	!	The field will accommodate an entry as large as a plus (+) or
	!	minus (-) 999999.99.
	!	.lm -5
	!
	! Index:
	!	.x Cash Receipts>Amount
	!	.x Amount>Cash Receipts
	!
	!--
			AR_CRJL::AMOUNT = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				XLINE$ + ";57", TEMP$, &
				AR_CRJL::AMOUNT, MFLAG, "#######.##", MVALUE)

		CASE 5%
	!++
	! Abstract:FLD005
	!	^*(05) Salesman\*
	!	.b
	!	.lm +5
	!	The ^*Salesman\* field refers to the person who completed the sale of the
	!	merchandise or services.
	!	.lm -5
	!
	! Index:
	!	.x Salesman>Maintain Cash Receipts Journal
	!	.x Maintain Cash Receipts Journal>Salesman
	!
	!--
			AR_CRJL::SALNUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				XLINE$ + ";68", TEMP$, &
				AR_CRJL::SALNUM, MFLAG, &
				"'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(SA_MAIN_SALESMAN.ID, "VX") = 1%
				THEN
					AR_CRJL::SALNUM = SA_SALESMAN::SALESMAN
				END IF
				GOTO E0Loop
			END IF

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		AR_MAIN_CRJ_LINE = 0%

		SELECT MLOOP

		CASE 3%
			!
			! Is the input defined?
			!
			AR_MAIN_CRJ_LINE = FUNC_TESTENTRY(SMG_WINDOW, &
				AR_CRJL::ACCT, GL_CHART::DESCR, &
				"AR", MLOOP, "ACCT", &
				"Account number", GL_MAIN_CHART.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(GL_CHART::DESCR, 16%), &
				SMG_WINDOW::CURLIN, 36%)
		END SELECT

	!
	! Set AR_CRJL_OLD value
	!
20500	CASE OPT_SETOLD
		AR_CRJL_OLD = AR_CRJL

	!
	! Restore AR_CRJL_OLD value
	!
	CASE OPT_RESETOLD
		AR_CRJL = AR_CRJL_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		AR_CRJL2 = AR_CRJL

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		AR_CRJL = AR_CRJL2

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!
		AR_CRJL::RECNUM = AR_CRJH::RECNUM

		IF AR_CRJL::SALNUM = ""
		THEN
			AR_CRJL::SALNUM = AR_35CUSTOM::SALESMAN
		END IF

		IF SMG_WINDOW::TOTREC = 0%
		THEN
			AR_CRJL::LLINE = "001"
		ELSE
			AR_CRJL::LLINE = &
				FORMAT$(VAL%(RARRAY(SMG_WINDOW::TOTREC)::SLINE) + 1%, &
				"<0>##")
		END IF
	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #AR_CRJL.CH%, &
				KEY #0% GE AR_CRJL::RECNUM + "", &
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
					KEY #0% GE AR_CRJH::RECNUM + "", &
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

			IF AR_CRJL::RECNUM = AR_CRJH::RECNUM
			THEN
				!
				! Add information to array
				!
				SMG_WINDOW::TOTREC = SMG_WINDOW::TOTREC + 1%
				RARRAY(SMG_WINDOW::TOTREC)::LINRFA = &
					GETRFA(SMG_WINDOW::CHAN)
				RARRAY(SMG_WINDOW::TOTREC)::SLINE = &
					AR_CRJL::LLINE
				RARRAY(SMG_WINDOW::TOTREC)::AMOUNT = &
					AR_CRJL::AMOUNT
				GOTO 27120
			END IF

		!
		! Remove one element of the array
		!
		CASE 2%
			!
			! Remove item pointed to by Mflag
			!
			FOR I% = MFLAG TO SMG_WINDOW::TOTREC - 1%
				RARRAY(I%) = RARRAY(I% + 1%)
			NEXT I%

		!
		! Set array item to current record
		!
		CASE 3%
			RARRAY(MFLAG)::LINRFA = GETRFA(SMG_WINDOW::CHAN)
			RARRAY(MFLAG)::SLINE = AR_CRJL::LLINE
			RARRAY(MFLAG)::AMOUNT = AR_CRJL::AMOUNT

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
		! new key is probibly passes through MVALUE, unless some
		! other means is devised.
		!
		CASE 6%
			AR_CRJL::RECNUM = RIGHT(MVALUE, 2%)

		END SELECT
	END SELECT

28000	EXIT FUNCTION

	%PAGE

 ListInvoices:
28100	!*******************************************************************
	! Create a list of invoices and ask for which one they want.
	!*******************************************************************

	AR_OPEN.ENDFLAG% = 0%

	AR_CRJL3 = AR_CRJL

	!
	! If the AR_OPEN file has not yet been opened, then open it.
	!
	GOTO 28110 IF AR_OPEN.CH% > 0%

	!
	! Open main file (existing) for modification
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.OPN"
	USE
		CONTINUE 28290
	END WHEN

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
		FIND #SMG_WINDOW::CHAN, &
			KEY #0% GE AR_CRJH::RECNUM + "", &
			REGARDLESS
	USE
		CONTINUE 28139
	END WHEN

28136	WHEN ERROR IN
		GET #SMG_WINDOW::CHAN
	USE
		CONTINUE 28139 IF ERR = 11%
		EXIT HANDLER
	END WHEN

	GOTO 28139 IF AR_CRJL::RECNUM <> AR_CRJH::RECNUM

	GOTO 28136 IF AR_CRJL::INVNUM <> INV_INVNUM$

	INV_AMOUNT = INV_AMOUNT + AR_CRJL::AMOUNT

	GOTO 28136

28139	INV_AMOUNT = FUNC_ROUND(INV_AMOUNT, 2%)

	GOTO 28200 IF INV_AMOUNT = 0.0 AND AR_OPEN.ENDFLAG% = -1%

	GOTO 28120 IF INV_AMOUNT = 0.0

	TEXT$ = &
		INV_DATE1$ + INV_DATE$ + &
		INV_INVNUM$ + &
		INV_DESCR$ + &
		FORMAT$(INV_AMOUNT, "#######.##")

	!
	! Add (insertion) to list
	!
	GOTO 28140 IF TEXT$ < INV_ARRAY$(LOOP%) &
		FOR LOOP% = 1% TO INV_ARRAY%
	LOOP% = INV_ARRAY% + 1%

28140	INV_ARRAY$(LOOP1% + 1%) = INV_ARRAY$(LOOP1%) &
		FOR LOOP1% = INV_ARRAY% TO LOOP% STEP -1%
	INV_ARRAY$(LOOP%) = TEXT$
	INV_ARRAY% = INV_ARRAY% + 1%

	GOTO 28120 IF AR_OPEN.ENDFLAG% = 0%

28200	!
	! We should now have a good list.
	! Now reformat the list into a user readable format
	! and use enter_choices to select one of them.
	!

	AR_CRJL = AR_CRJL3

	INV_ARRAY$(0%) = NUM1$(INV_ARRAY%)
	FOR LOOP% = 1% TO INV_ARRAY%
		INV_ARRAY$(LOOP%) = &
			MID(INV_ARRAY$(LOOP%), 17%, 8%) + " " + &
			PRNT_DATE(MID(INV_ARRAY$(LOOP%), 9%, 8%), 8%) + " " + &
			MID(INV_ARRAY$(LOOP%), 25%, 25%) + " " + &
			RIGHT(INV_ARRAY$(LOOP%), 50%)
	NEXT LOOP%

		AMOUNTLEFT = AR_CRJH::AMNT
		FOR I% = 1% TO SMG_WINDOW::TOTREC
			AMOUNTLEFT = AMOUNTLEFT + RARRAY(I%)::AMOUNT
		NEXT I%



	TEMP% = ENTR_3CHOICE(SCOPE, "", &
		"", &
		INV_ARRAY$(), &
		"", &
		0%, &
		"List of Current Invoices  - unapplied amount: " + &
		FORMAT$(AMOUNTLEFT, "###,###.##"), &
		"009,020,026", &
		0%)

	IF TEMP% > 0%
	THEN
		AR_CRJL::INVNUM = LEFT(INV_ARRAY$(TEMP%), 8%)
		AR_CRJL::AMOUNT = -VAL(RIGHT(INV_ARRAY$(TEMP%), 47%))

		AR_CRJL::AMOUNT = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
			XLINE$ + ";41", "", &
			AR_CRJL::AMOUNT, 1%, "######.##", "")

		GOSUB LookupAccount
	END IF

28290	RETURN

	%PAGE

 LookupAccount:
	!*******************************************************************
	! Look up account number for this invoice in the accounts receivable
	! register, and place it in the crj line item if apporpriate.
	!*******************************************************************

28300	!
	! If the AR_OPEN file has not yet been opened, then open it.
	!
	GOTO 28301 IF AR_OPEN.CH% > 0%

	!
	! Open main file (existing) for modification
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.OPN"
	USE
		CONTINUE 28390
	END WHEN

28301	!
	WHEN ERROR IN
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

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
