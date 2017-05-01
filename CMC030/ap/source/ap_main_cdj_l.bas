1	%TITLE "Cash Disbursements Journal Maintenance"
	%SBTTL "AP_MAIN_CDJ_L"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AP_MAIN_CDJ_L(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	.B
	!	.LM +5
	!	The ^*Cash Disbursements Journal Maintenance\* program maintains the Marketing
	!	file.
	!	.LM -5
	!
	! Index:
	!	.x Cash Disbursements Journal Maintenance
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_MAIN_CDJ_L/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN AP_MAIN_CDJ_L
	!	$ DELETE AP_MAIN_CDJ_L.OBJ;*
	!
	! Author:
	!
	!	10/12/87 - Kevin Handy
	!
	! Modification history:
	!
	!	05/19/88 - Lance Williams
	!		Modified to allow R/O open of file if R/W fails.
	!
	!	07/04/90 - Kevin Handy
	!		Modified to use formatted PO number.
	!
	!	03/05/92 - Dan Perkins
	!		Changed ENTR_3PO to ENTR_3STRING.
	!
	!	12/11/92 - Kevin Handy
	!		Added DISAMT, AP_ACCT, CASH_ACCT to available
	!		items to maintain.
	!
	!	01/04/93 - Kevin Handy
	!		Increased dimension for array from 300 to 2000.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	06/04/96 - Kevin Handy
	!		Reformat source code.
	!		Added batch number to AP_CDJ.
	!
	!	06/24/97 - Kevin Handy
	!		Lose unecessary definitions
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

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[AP.OPEN]AP_CDJ.HB"
	MAP (AP_CDJ)		AP_CDJ_CDD	AP_CDJ
	MAP (AP_CDJ_OLD)	AP_CDJ_CDD	AP_CDJ_OLD, AP_CDJ2

	MAP (AP_CDJ_H)		AP_CDJ_CDD	AP_CDJ_H

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD	GL_CHART

	!
	! Create array to contain pointers and totals
	!
	RECORD RARRAY_RECORD
		RFA	LINRFA		! Rfa pointer for record
		REAL	DISC_LOST_AMT	! Amount for record
		REAL	CKAMT		! Quanity for record
	END RECORD

	MAP (TT_AP_CDJ) RARRAY_RECORD RARRAY(2000%)

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_AP_CDJ) &
		AP_CDJ.CH%, &
		AP_CDJ.READONLY%, &
		CDJ_BATCH$ = 2%


	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION FUNC_TESTENTRY
	EXTERNAL LONG    FUNCTION MAIN_WINDOW

	!
	! Set up error trapping
	!
	ON ERROR GOTO 29000

	%PAGE


	SELECT MOPTION

	!******************************************************************
	! Initialization
	!
	! This option is used to initialize the window structure,
	! set up the default values for add, and open all files
	! necessary that have not already been opened.
	!******************************************************************
	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Line items"
		SMG_WINDOW::NHELP = "AP_MAIN_CDJ_L"
		SMG_WINDOW::HSIZE = 130%
		SMG_WINDOW::VSIZE = 12%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 8%
		SMG_WINDOW::NITEMS= 12%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::TOPLIN = 3%
		SMG_WINDOW::BOTLIN = 11%

		IF INSTR(1%, " QV", MVALUE) <= 1%
		THEN
			!
			! Load in defaults
			!
			CALL READ_DEFAULTS(SMG_WINDOW)
		END IF

700		!
		! Get info required for main file - Should already be open
		! This section is not required
		!
		IF AP_CDJ.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF AP_CDJ.READONLY%
			GOTO 790
		END IF

750		WHEN ERROR IN
			%INCLUDE "SOURCE:[AP.OPEN]AP_CDJ.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			AP_MAIN_CDJ_L = ERR
			CONTINUE 770
		END WHEN

		AP_CDJ.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AP.OPEN]AP_CDJ.OPN"
		USE
			AP_MAIN_CDJ_L = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		AP_CDJ.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(AP_CDJ.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = AP_CDJ.CH%
		WHEN ERROR IN
			RESET #AP_CDJ.CH%
			GET #AP_CDJ.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

	%PAGE

20100	!******************************************************************
	! Display the background
	!
	! This option is used to display the background information
	! on the screen.  It must first clear any junk on the screen,
	! and then write the background onto it.
	!******************************************************************
	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"  (01)     (02)      (03)    (04)   (05)    " + &
			" (06)     (07)   (08)      (09)   " + &
			" (10)     (11)               (12)                   ", &
			1%, 1%, , SMG$M_REVERSE)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"  Tran # Invoice #  Inv Date Po #  Disc Lst " + &
			" Lst Amt Check#   Date   Net Amt  " + &
			" Discount A/P Account        Cash Account           ", &
			2%, 1%, , SMG$M_REVERSE)

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)


	!
	! Extra information to be displayed
	!
	CASE OPT_DISPLAY
		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		!
		! Generate totals
		!
		DISC_LOST_AMT = 0.0
		CKAMT = 0.0

		FOR I% = 1% TO SMG_WINDOW::TOTREC

			DISC_LOST_AMT = DISC_LOST_AMT + RARRAY(I%)::DISC_LOST_AMT
			CKAMT = CKAMT + RARRAY(I%)::CKAMT

		NEXT I%

		!
		! Display totals
		!
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"Number of lines" + &
			FORMAT$(SMG_WINDOW::TOTREC, "###") + &
			"                          " + &
			FORMAT$(DISC_LOST_AMT, "#####.##                 ") + &
			FORMAT$(CKAMT, "######.##") + &
			"                                                    ", &
			SMG_WINDOW::VSIZE, 1%, , SMG$M_REVERSE)

		!
		! Paint lines on screen
		!
		FOR I% = 1% TO 11%

			A% = VAL%(MID("009,020,029,035,044,053,060,069,079,088,107", &
				I% * 4% - 3%, 3%))

			SMG_STATUS% = SMG$DRAW_LINE(SMG_WINDOW::WNUMBER, &
				1%, A%, SMG_WINDOW::BOTLIN, A%)

		NEXT I%

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

20200	!******************************************************************
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user, display
	! data, set defaults, and return the data back according to
	! MFLAG.
	!******************************************************************

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
	!	^*(01) Transaction Number\*
	!	.b
	!	.lm +5
	!	The ^*Transaction Number\* field
	!	refers to the system generated transaction number which was assigned at the
	!	time a purchase journal record was entered.
	!	.lm -5
	!
	! Index:
	!	.x Transaction Number>Lines
	!	.x Lines>Transaction Number
	!
	!--
			AP_CDJ::TRANKEY = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";3", TEMP$, &
				AP_CDJ::TRANKEY, MFLAG, "'LLLLL", MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	.x Invoice #>Lines
	!	^*(02) Invoice _#\*
	!	.b
	!	.lm +5
	!	The ^*Invoice _#\* field
	!	refers to the invoice number which was entered at the time a purchase journal
	!	record was entered.
	!	.b
	!	Fifteen (15) spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!	.x Lines>Invoice #
	!
	!--
			AP_CDJ::INVNUM=ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";10", TEMP$, &
				AP_CDJ::INVNUM, MFLAG, &
				"'LLLLLLLLL", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	.x Invoice Date>Lines
	!	^* (03) Invoice Date\*
	!	.b
	!	.lm +5
	!	The ^*Invoice Date\* field
	!	contains the date of a vendor's invoice.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!	.x Lines>Invoice Date
	!
	!--
			AP_CDJ::INVDAT= ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";21", TEMP$, &
				AP_CDJ::INVDAT, MFLAG, "6", MVALUE)

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Purchase Order Number\*
	!	.b
	!	.lm +5
	!	The ^*Purchase Order Number\* field
	!	refers to the purchase order related to a vendor's invoice as it was
	!	entered at the time a purchase journal record was entered.
	!	.lm -5
	!
	! Index:
	!	.x Purchase Order Number>Lines
	!	.x Lines>Purchase Order Number
	!
	!--
			AP_CDJ::PONUM=ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";30", TEMP$, &
				AP_CDJ::PONUM, MFLAG OR 2%, "~R 'E", MVALUE)

		CASE 5%

	!++
	! Abstract:FLD005
	!	^*(05) Discount Lost\*
	!	.b
	!	.lm +5
	!	The ^*Discount Lost\* field
	!	indicates the General Ledger account which will be debited in the
	!	event that a discount lost amount is recorded in field (06).
	!	.lm -5
	!
	! Index:
	!	.x Discount Lost>Lines
	!	.x Lines>Discount Lost
	!
	!--
			AP_CDJ::DISCLOST_ACCT = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";36", TEMP$, &
				AP_CDJ::DISCLOST_ACCT, MFLAG, &
				"'LLLLLLL", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%)
				THEN
					AP_CDJ::DISCLOST_ACCT= GL_CHART::ACCT
				END IF
				GOTO E0Loop
			END IF

		CASE 6%

	!++
	! Abstract:FLD006
	!	^*(06) Lost Amount\*
	!	.b
	!	.lm +5
	!	The ^*Lost Amount\* field
	!	enters the amount of a discount lost due to not paying a
	!	vendor's invoice on or before the discount date.
	!	.b
	!	If the amount of the discount lost is entered in this field, the value of the
	!	discount in field (10) must be blanked.
	!	.lm -5
	!
	! Index:
	!	.x Lost Amount>Lines
	!	.x Lines>Lost Amount
	!
	!--
			AP_CDJ::DISC_LOST_AMT = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";45", TEMP$, &
				AP_CDJ::DISC_LOST_AMT, MFLAG, &
				"#####.##", MVALUE)

		CASE 7%

	!++
	! Abstract:FLD007
	!	^*(07) Check _#\*
	!	.b
	!	.lm +5
	!	The ^*Check _#\* field refers to
	!	the number of the check used to make payment for a vendor's invoice.  This field
	!	is entered by the system during the check writing process.
	!	.b
	!	In the event a hand check is written for a vendor's invoice, the check number
	!	must be manually entered in this field.  A line item with a check number
	!	already in this field will be bypassed during the check writing process.
	!	.lm -5
	!
	! Index:
	!	.x Check #>Lines
	!	.x Lines>Check #
	!
	!--
			AP_CDJ::CKNUM = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";54", TEMP$, &
				AP_CDJ::CKNUM, MFLAG, &
				"'LLLLL", MVALUE)

		CASE 8%

	!++
	! Abstract:FLD008
	!	.x Check Date>Lines
	!	^*(08) Date\*
	!	.b
	!	.lm +5
	!	The ^*Date\* field is blank
	!	until the check writing process is executed, at which time the system enters
	!	the date of the check.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.b
	!	If a manual check is prepared for an vendor's invoice, this field must have
	!	the date of the check which was manually prepared.
	!	.lm -5
	!
	! Index:
	!	.x Lines>Check Date
	!
	!--
			AP_CDJ::CKDAT = ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";61", TEMP$, &
				AP_CDJ::CKDAT, MFLAG, &
				"6", MVALUE)

		CASE 9%

	!++
	! Abstract:FLD009
	!	^*(09) Net Amount\*
	!	.b
	!	.lm +5
	!	The ^*Net Amount\* field
	!	contains the net amount to be paid on a vendor's invoice.
	!	.b
	!	If field (10), ^*Discount\*, contains a value, the sum of field (09) and field
	!	(10) will be displayed on the check stub as the ^&Gross Amount\&.  Field (10)
	!	will be displayed in the ^&Discount\& column and field (09) will be displayed
	!	in the ^&Net Amount\& column.
	!	.b
	!	If field (06), ^*[Discount] Lost Amount\*, contains a value, the sum of field
	!	(06) and field (09) will be displayed on the check stub as the ^&Gross Amount\&
	!	and the ^&Net Amount\&. The ^&Discount\& column will display zeroes.
	!	.b
	!	If a partial payment is to be made, this field must be changed
	!	to reflect the amount which is to be paid.
	!	.lm -5
	!
	! Index:
	!	.x Net Amount>Lines
	!	.x Lines>Net Amount
	!
	!--
			AP_CDJ::CKAMT = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";70", TEMP$, &
				AP_CDJ::CKAMT, MFLAG, "######.##", MVALUE)

		CASE 10%
	!++
	! Abstract:FLD010
	!	^*(10) Discount Amount\*
	!	.b
	!	.lm +5
	!	The ^*Discount Amount\* field
	!	contains the amount of the discount to be taken when an invoice is paid.
	!	.b
	!	If a discount value is displayed in this field, but cannot be taken, the value
	!	must be moved to field (06) ^*[Discount] Lost Amount\* by accessing the
	!	^*Change\* function in the COMMAND menu. This field must be blanked or changed
	!	to zero.
	!	.lm -5
	!
	! Index:
	!	.x Discount>Lines
	!	.x Lines>Discount
	!
	!--
			AP_CDJ::DISAMT = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";80", TEMP$, &
				AP_CDJ::DISAMT, MFLAG, &
				"#####.##", MVALUE)

		CASE 11%
	!++
	! Abstract:FLD011
	!	^*(11) A/P Account\*
	!	.lm +5
	!	.b
	!	The ^*A/P Account\* field is
	!	automatically filled with the normal General Ledger Accounts Payable account.
	!	As a rule, it would not be necessary to edit this field, though the default
	!	account could be overridden.
	!	.lm -5
	!
	! Index:
	!	.x Line>Account Payable Account
	!	.x Account Payable Account>Line
	!
	!--

			AP_CDJ::AP_ACCT = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";89", TEMP$, &
				AP_CDJ::AP_ACCT, MFLAG, &
				"'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX ") = 1%)
				THEN
					AP_CDJ::AP_ACCT = GL_CHART::ACCT
				END IF
				GOTO E0Loop
			END IF

		CASE 12%
	!++
	! Abstract:FLD012
	!	^*(12) Cash Account\*
	!	.lm +5
	!	.b
	!	The ^*Cash Account\* field is
	!	automatically filled with the default General Ledger Cash account.  The
	!	default account can be overridden, though it would not normally be necessary
	!	to do so.
	!	.lm -5
	!
	! Index:
	!	.x Line>Cash Account
	!	.x Cash Account>Line
	!
	!--

			AP_CDJ::CASH_ACCT = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";108", TEMP$, &
				AP_CDJ::CASH_ACCT, MFLAG, &
				"'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX ") = 1%)
				THEN
					AP_CDJ::CASH_ACCT = GL_CHART::ACCT
				END IF
				GOTO E0Loop
			END IF

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!***********************************************************************
	! Test values
	!***********************************************************************
20300	CASE OPT_TESTENTRY

	AP_MAIN_CDJ_L = 0%

	SELECT MLOOP

	CASE 1%
		IF (MVALUE = "ADD")
		THEN
			WHEN ERROR IN
				FIND #AP_CDJ.CH%, &
					KEY #0% EQ AP_CDJ::VENNUM + AP_CDJ::TRANKEY, &
					REGARDLESS
			USE
				CONTINUE 32767 IF ERR = 155%
				EXIT HANDLER
			END WHEN

			AP_MAIN_CDJ_L = 1%
			CALL ENTR_3MESSAGE(SCOPE, "Record Already Exists", 1%)
		END IF

	CASE 5%
		IF AP_CDJ::DISCLOST_ACCT <> ""
		THEN
			!
			! Is the input defined?
			!
			AP_MAIN_CDJ_L = FUNC_TESTENTRY( SMG_WINDOW, &
				AP_CDJ::DISCLOST_ACCT, GL_CHART::DESCR, &
				"GL", MLOOP, "ACCT", &
				"Account number", GL_MAIN_CHART.ID)
		END IF

	CASE 11%
		!
		! Is the input defined?
		!
		AP_MAIN_CDJ_L = FUNC_TESTENTRY( SMG_WINDOW, &
			AP_CDJ::AP_ACCT, GL_CHART::DESCR, &
			"AP", MLOOP, "ACCT", &
			"Account number", GL_MAIN_CHART.ID)

	CASE 12%
		!
		! Is the input defined?
		!
		AP_MAIN_CDJ_L = FUNC_TESTENTRY( SMG_WINDOW, &
			AP_CDJ::CASH_ACCT, GL_CHART::DESCR, &
			"AP", MLOOP, "ACCT", &
			"Account number", GL_MAIN_CHART.ID)

	END SELECT

	!***********************************************************************
	! Set AP_CDJ_OLD value
	!***********************************************************************
20500	CASE OPT_SETOLD
		AP_CDJ_OLD = AP_CDJ

	!***********************************************************************
	! Restore AP_CDJ_OLD value
	!***********************************************************************
	CASE OPT_RESETOLD
		AP_CDJ = AP_CDJ_OLD

	!***********************************************************************
	! Set default value
	!***********************************************************************
	CASE OPT_SETDEFAULT
		AP_CDJ2 = AP_CDJ

	!***********************************************************************
	! Restore default value
	!***********************************************************************

	CASE OPT_RESETDEFAULT
		AP_CDJ = AP_CDJ2

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!
		AP_CDJ::VENNUM = AP_CDJ_H::VENNUM

	!***********************************************************************
	! Find
	!***********************************************************************
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #AP_CDJ.CH%, &
				KEY #0% GE AP_CDJ::VENNUM + "", &
				REGARDLESS
		END SELECT

	!***********************************************************************
	! Handle array of records
	!***********************************************************************
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
					KEY #0% GE AP_CDJ_H::VENNUM + "", &
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

			IF AP_CDJ::VENNUM = AP_CDJ_H::VENNUM
			THEN
				!
				! Add information to array
				!
				SMG_WINDOW::TOTREC = SMG_WINDOW::TOTREC + 1%
				RARRAY(SMG_WINDOW::TOTREC)::LINRFA = &
					GETRFA(SMG_WINDOW::CHAN)
				RARRAY(SMG_WINDOW::TOTREC)::DISC_LOST_AMT = &
					AP_CDJ::DISC_LOST_AMT
				RARRAY(SMG_WINDOW::TOTREC)::CKAMT = &
					AP_CDJ::CKAMT
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
			RARRAY(MFLAG)::DISC_LOST_AMT = AP_CDJ::DISC_LOST_AMT
			RARRAY(MFLAG)::CKAMT = AP_CDJ::CKAMT

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
			AP_CDJ::VENNUM = RIGHT(MVALUE, 2%)

		END SELECT
	END SELECT

28000	EXIT FUNCTION

	%PAGE

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
