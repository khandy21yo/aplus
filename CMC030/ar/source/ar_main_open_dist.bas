1	%TITLE "Sales Journal Line Item Maintenance"
	%SBTTL "AR_MAIN_OPEN_DIST"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AR_MAIN_OPEN_DIST(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1991 BY
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
	!	The ^*Distribution\* portion of this screen is used to indicate how
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
	! Option:
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_MAIN_OPEN_DIST/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP AR_MAIN_OPEN_DIST
	!	$ DELETE AR_MAIN_OPEN_DIST.OBJ;*
	!
	! Author:
	!
	!	05/31/91 - Kevin Handy
	!
	! Modification history:
	!
	!	04/15/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	04/22/92 - Kevin Handy
	!		Clean up (check)
	!
	!	02/22/93 - Dan Perkins
	!		Changed "V0" to "VX" on chart of accounts to be
	!		able to list accounts starting at a particular
	!		account.
	!
	!	05/11/93 - Kevin Handy
	!		Modified to initialize the customer number in the
	!		file, and allow it to change.  Also removed
	!		numerous bugs in routines.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	07/03/96 - Kevin Handy
	!		Reformat source code.
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
	!	11/09/2000 - Kevin Handy
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
	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN_DIST.HB"
	MAP (AR_OPEN_DIST)	AR_OPEN_DIST_CDD	AR_OPEN_DIST
	MAP (AR_OPEN_DIST_OLD)	AR_OPEN_DIST_CDD AR_OPEN_DIST_OLD, AR_OPEN_DIST2

	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.HB"
	MAP (AR_OPEN)		AR_OPEN_CDD		AR_OPEN

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD		GL_CHART

	!
	! Create array to contain pointers and totals
	!
	RECORD RARRAY_RECORD
		RFA	LINRFA		! Rfa pointer for record
		STRING	SLINE = 3	! Line number
		REAL	AMOUNT		! Amount for record
		REAL	QTY		! Quanity for record
	END RECORD

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (TT_AR_OPEN_DIST_1) &
		CATTITLE$ = 20%, &
		CATTYPE$(8%) = 20%, &
		TAXTITLE$ = 20%, &
		TAXTYPE$(8%) = 40%

	COM (TT_AR_OPEN_DIST) RARRAY_RECORD RARRAY(300%)

	COM (CH_AR_OPEN) &
		AR_OPEN.CH%

	COM (CH_AR_OPEN_DIST) &
		AR_OPEN_DIST.CH%, &
		AR_OPEN_DIST.READONLY%

	COM (TT_AR_SJ) &
		BATCH_NO$ = 2%

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION MAIN_WINDOW
	EXTERNAL LONG    FUNCTION FUNC_TESTENTRY

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
		SMG_WINDOW::DESCR = "Distribution"
		SMG_WINDOW::NHELP = "AR_MAIN_OPEN_DIST"
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
		! Load in defaults
		!
		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

		!
		! Category
		!
		CATTITLE$ = "Type   Description"
		CATTYPE$(0%) = "8"
		CATTYPE$(1%) = "     Not Recorded"
		CATTYPE$(2%) = "S    Sales"
		CATTYPE$(3%) = "D    Discount"
		CATTYPE$(4%) = "F    Freight"
		CATTYPE$(5%) = "T    Sales Tax"
		CATTYPE$(6%) = "C    Cost of Sales"
		CATTYPE$(7%) = "I    Inventory"
		CATTYPE$(8%) = "O    Other"

		!
		! Tax type
		!
		TAXTITLE$ = "Type   Description"
		TAXTYPE$(0%) = "7"
		TAXTYPE$(1%) = "     Not recorded"
		TAXTYPE$(2%) = "1    Taxable"
		TAXTYPE$(3%) = "2    Service"
		TAXTYPE$(4%) = "3    Freight"
		TAXTYPE$(5%) = "4    Resale"
		TAXTYPE$(6%) = "5    Out of state"
		TAXTYPE$(7%) = "6    Church, School, and Government"

700		!
		! Declare channels
		!
		IF AR_OPEN_DIST.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF AR_OPEN_DIST.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN_DIST.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			AR_MAIN_OPEN_DIST = ERR
			CONTINUE 770
		END WHEN

		AR_OPEN_DIST.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN_DIST.OPN"
		USE
			AR_MAIN_OPEN_DIST = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		AR_OPEN_DIST.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(AR_OPEN_DIST.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = AR_OPEN_DIST.CH%
		WHEN ERROR IN
			RESET #AR_OPEN_DIST.CH%
			GET #AR_OPEN_DIST.CH%, REGARDLESS
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
			"        (01)      (02) (03)(04)       (05)  " + &
			"               (06)          (07) ", &
			1%, 1%, , SMG$M_REVERSE)
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"  Acct #      Sub Acct  Cat Tax  Description" + &
			"                 Qty        Amount", &
			2%, 1%, , SMG$M_REVERSE)

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

	!
	! Extra display stuff
	!
	CASE OPT_DISPLAY

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		!
		! Generate totals
		!
		AMOUNT = 0.0
		QTY = 0.0

		FOR I% = 1% TO SMG_WINDOW::TOTREC

			AMOUNT = AMOUNT + RARRAY(I%)::AMOUNT
			QTY    = QTY    + RARRAY(I%)::QTY

		NEXT I%

		!
		! Display totals
		!
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"Number of lines" + &
			FORMAT$(SMG_WINDOW::TOTREC, "###") + &
			"                                   " + &
			FORMAT$(QTY, "###,###.###") + &
			FORMAT$(AMOUNT, " ##,###,###.##"), &
			SMG_WINDOW::VSIZE, 1%, , SMG$M_REVERSE)

		!
		! Paint lines on screen
		!
		FOR I% = 1% TO 6%

			A% = VAL%(MID("014,023,028,032,055,065", &
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

		TEMP1% = SCOPE::SCOPE_EXIT

 E0Loop:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%
	!++
	! Abstract:FLD001
	!	^*(01) Account _#\*
	!	.b
	!	.lm +5
	!	The ^*Account _#\* field allows for entry of the General Ledger
	!	account number corresponding to a particular line item. The account
	!	number entered should have been established in the General Ledger
	!	Chart of Accounts. If the number has not been entered in the Chart
	!	of Accounts, it must be accomplished before the appropriate distribution
	!	can be made. (The journal will not post if there is an unidentified
	!	account number.)
	!	.b
	!	Pressing ^*List Choices\* will display a list of General Ledger account
	!	numbers.
	!	.lm -5
	!
	! Index:
	!	.x Account>Sales Journal
	!	.x Sales Journal>Account
	!
	!--
			AR_OPEN_DIST::ACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				XLINE$ + ";3", TEMP$, &
				AR_OPEN_DIST::ACCT, MFLAG, "'LLLLLLLLLL", &
				MVALUE)

				IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
				THEN
					IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX  ") = 1%)
					THEN
						AR_OPEN_DIST::ACCT = GL_CHART::ACCT
					END IF
					GOTO E0Loop
				END IF

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Sub Account\*
	!	.b
	!	.lm +5
	!	The ^*Sub Account\* field is provided to enter a user defined code to
	!	classify the information this line item represents. A Sub Account
	!	would generally refer to a job or process code.
	!	.b
	!	This field will accommodate up to ten (10) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Sub Account>Sales Journal
	!	.x Sales Journal>Sub Account
	!
	!--
			AR_OPEN_DIST::SUBACCT = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";15", TEMP$, &
				AR_OPEN_DIST::SUBACCT, MFLAG, &
				"'LLLLLLL", MVALUE)

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Category\*
	!	.b
	!	.lm +5
	!	The ^*Category\* field is provided to enter a code which identifies
	!	the category of a particular line item.  Utilization of this field
	!	is at the user's option, depending upon the results desired.
	!	.b
	!	If this field is utilized, valid values are:
	!	.table 3,25
	!	.te
	!	^*S\* - Sales
	!	.te
	!	^*D\* - Discount
	!	.te
	!	^*F\* - Freight
	!	.te
	!	^*T\* - Sales Tax
	!	.te
	!	^*C\* - Cost of Sales
	!	.te
	!	^*I\* - Inventory
	!	.te
	!	^*O\* - Other
	!	.end table
	!	Pressing ^*List Choices\* will cause a list of valid codes to be displayed.
	!	.lm -5
	!
	! Index:
	!	.x Category>Sales Journal
	!	.x Sales Journal>Category
	!	.x Sales Journal>Line Item Category>Sales
	!	.x Sales Journal>Line Item Category>Discount
	!	.x Sales Journal>Line Item Category>Freight
	!	.x Sales Journal>Line Item Category>Sales Tax
	!	.x Sales Journal>Line Item Category>Cost of Sales
	!	.x Sales Journal>Line Item Category>Inventory
	!	.x Sales Journal>Line Item Category>Other
	!
	!--
			AR_OPEN_DIST::LTYPE = &
				ENTR_3STRINGLIST(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";25", TEMP$, &
				AR_OPEN_DIST::LTYPE, MFLAG, "'", MVALUE, &
				CATTYPE$(), CATTITLE$, "005")

		CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Tax Type\*
	!	.b
	!	.lm +5
	!	The ^*Tax Type\* field refers to the taxability of a sale with
	!	regard to sales tax.
	!	.b
	!	This field is bypassed unless the category (field 03) contains an
	!	"*S" (Sales).
	!	.b
	!	Valid values are:
	!	.te
	!	.table 3,25
	!	^*1\* - Taxable
	!	.te
	!	^*2\* - Service
	!	.te
	!	^*3\* - Freight
	!	.te
	!	^*4\* - Resale
	!	.te
	!	^*5\* - Out of state
	!	.te
	!	^*6\* - Churches, Schools, and Government
	!	.end table
	!	Pressing ^*List Choices\* will cause a list of valid codes to be displayed.
	!	.lm -5
	!
	! Index:
	!	.x Tax Type>Sales Journal
	!	.x Sales Journal>Tax Type
	!
	!--
			IF (TEMP$ = "Add") AND (AR_OPEN_DIST::LTYPE <> "S")
			THEN
				TEMP% = MFLAG OR 1%
			ELSE
				TEMP% = MFLAG
			END IF

			AR_OPEN_DIST::TAXTYP = ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				XLINE$ + ";30", TEMP$, &
				AR_OPEN_DIST::TAXTYP, TEMP%, "'", MVALUE, &
				TAXTYPE$(), TAXTITLE$, "005")

			SCOPE::SCOPE_EXIT = TEMP1% IF TEMP% AND 1%

		CASE 5%
	!++
	! Abstract:FLD005
	!	^*(05) Description\*
	!	.b
	!	.lm +5
	!	The ^*Description\* field enters a brief
	!	description of the line item.  Depending upon user requirements,
	!	this field may be left blank.
	!	.b
	!	The field will accommodate an entry of twenty-one (21)
	!	alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Sales Journal>Description
	!	.x Description>Sales Journal
	!
	!--
			AR_OPEN_DIST::DESCR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				XLINE$ + ";33", TEMP$, &
				AR_OPEN_DIST::DESCR, MFLAG, &
				"'LLLLLLLLLLLLLLLLLLLL", MVALUE)

		CASE 6%
	!++
	! Abstract:FLD006
	!	^*(06) Quantity\*
	!	.b
	!	.lm +5
	!	The ^*Quantity\* field refers to the number of units sold for
	!	this particular line item.
	!	.b
	!	The format for entry is XXXX.XX.
	!	.lm -5
	!
	! Index:
	!	.x Sales Journal>Qty
	!	.x Qty>Sales Journal
	!
	!--
			AR_OPEN_DIST::QTY = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				XLINE$ + ";56", TEMP$, &
				AR_OPEN_DIST::QTY, MFLAG, "#,###.###", MVALUE)

		CASE 7%
	!++
	! Abstract:FLD007
	!	^*(07) Amount\*
	!	.b
	!	.lm +5
	!	The ^*Amount\* field is to be entered with the dollar amount of
	!	the line item.  A credit must be entered with a dash (-) preceding
	!	it.  If the amount is a debit, no sign is entered.
	!	.b
	!	^*Note:\* Sales tax will be a separate line item in the distribution.
	!	.b
	!	The field will accommodate an entry as large as a plus (+) or
	!	minus (-) 999999.99.
	!	.lm -5
	!
	! Index:
	!	.x Amount>Sales Journal
	!	.x Sales Journal>Amount
	!
	!--
			AR_OPEN_DIST::AMOUNT = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				XLINE$ + ";66", TEMP$, &
				AR_OPEN_DIST::AMOUNT, MFLAG, &
				"##,###,###.##", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		AR_MAIN_OPEN_DIST = 0%

		SELECT MLOOP

		CASE 1%
			!
			! Is the input defined?
			!
			AR_MAIN_OPEN_DIST = FUNC_TESTENTRY( SMG_WINDOW, &
				AR_OPEN_DIST::ACCT, GL_CHART::DESCR, &
				"AR", MLOOP, "ACCT", &
				"Account number", GL_MAIN_CHART.ID)

		END SELECT

	!
	! Set AR_OPEN_DIST_OLD value
	!
20500	CASE OPT_SETOLD
		AR_OPEN_DIST_OLD = AR_OPEN_DIST

	!
	! Restore AR_OPEN_DIST_OLD value
	!
	CASE OPT_RESETOLD
		AR_OPEN_DIST = AR_OPEN_DIST_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		AR_OPEN_DIST2 = AR_OPEN_DIST

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		AR_OPEN_DIST = AR_OPEN_DIST2

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!
		AR_OPEN_DIST::INVNUM = AR_OPEN::INVNUM
		AR_OPEN_DIST::CUSNUM = AR_OPEN::CUSNUM

		IF SMG_WINDOW::TOTREC = 0%
		THEN
			AR_OPEN_DIST::SLINE = "001"
		ELSE
			AR_OPEN_DIST::SLINE = &
				FORMAT$(VAL%(RARRAY(SMG_WINDOW::TOTREC)::SLINE) + 1%, &
				"<0>##")
		END IF

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #AR_OPEN_DIST.CH%, &
				KEY #0% GE AR_OPEN_DIST::INVNUM + "", &
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
					KEY #0% GE AR_OPEN::CUSNUM + AR_OPEN::INVNUM, &
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

			IF (AR_OPEN_DIST::INVNUM = AR_OPEN::INVNUM) AND &
				(AR_OPEN_DIST::CUSNUM = AR_OPEN::CUSNUM)
			THEN
				!
				! Add information to array
				!
				SMG_WINDOW::TOTREC = SMG_WINDOW::TOTREC + 1%
				RARRAY(SMG_WINDOW::TOTREC)::LINRFA = &
					GETRFA(SMG_WINDOW::CHAN)
				RARRAY(SMG_WINDOW::TOTREC)::SLINE = &
					AR_OPEN_DIST::SLINE
				RARRAY(SMG_WINDOW::TOTREC)::AMOUNT = &
					AR_OPEN_DIST::AMOUNT
				RARRAY(SMG_WINDOW::TOTREC)::QTY = &
					AR_OPEN_DIST::QTY
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
			RARRAY(MFLAG)::SLINE = AR_OPEN_DIST::SLINE
			RARRAY(MFLAG)::AMOUNT = AR_OPEN_DIST::AMOUNT
			RARRAY(MFLAG)::QTY = AR_OPEN_DIST::QTY

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
			AR_OPEN_DIST::CUSNUM = RIGHT(MVALUE, 2%)
			AR_OPEN_DIST::INVNUM = RIGHT(MVALUE, 12%)

		END SELECT
	END SELECT

28000	EXIT FUNCTION

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
