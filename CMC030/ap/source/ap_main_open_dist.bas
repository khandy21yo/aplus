1	%TITLE "Accounts Payable Open Distribution Maintenance"
	%SBTTL "AP_MAIN_OPEN_DIST"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AP_MAIN_OPEN_DIST(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	The ^*Distribution\* option allows the distribution for this transaction
	!	to be edited.
	!	.lm -5
	!
	! Index:
	!	.x Open>Distribution
	!	.x Distribution>Open
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_MAIN_OPEN_DIST/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN AP_MAIN_OPEN_DIST
	!	$ DELETE AP_MAIN_OPEN_DIST.OBJ;*
	!
	! Author:
	!
	!	10/13/88 - Kevin Handy
	!
	! Modification history:
	!
	!	07/02/90 - Kevin Handy
	!		Modified to handle formatted PO number.
	!
	!	09/27/90 - Frank F. Starman
	!		Display only 9 characters from PO number.
	!
	!	11/05/91 - Kevin Handy
	!		Modified to include AP_OPEN information when
	!		calculating remaining amount.  Commented out
	!		goofy code playing with discount remaining.
	!
	!	03/05/92 - Dan Perkins
	!		Changed ENTR_3PO to ENTR_3STRING.
	!
	!	02/11/93 - Dan Perkins
	!		Changed "V0" to "VX" on chard of accounts to be
	!		able to list accounts starting at a particular
	!		account.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	05/20/96 - Kevin Handy
	!		Use integer for #key
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/08/99 - Kevin Handy
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
	%INCLUDE "FUNC_INCLUDE:AP_WINDOW.INC"

	!
	! CDD and Maps
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN_DIST.HB"
	MAP (AP_OPEN_DIST)		AP_OPEN_DIST_CDD	AP_OPEN_DIST
	MAP (AP_OPEN_DIST_OLD) AP_OPEN_DIST_CDD AP_OPEN_DIST_OLD, AP_OPEN_DIST2

	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.HB"
	MAP (AP_OPEN)		AP_OPEN_CDD	AP_OPEN

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP (AP_VENDOR)		AP_VENDOR_CDD	AP_VENDOR

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD	GL_CHART

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_AP_OPEN_DIST) &
		AP_OPEN_DIST.CH%

	!
	! Create array to contain pointers and totals
	!
	RECORD RARRAY_RECORD
		RFA	LINRFA		! Rfa pointer for record
		STRING	SLINE = 4%	! Line number
		REAL	AMOUNT		! Amount for record
		REAL	DISCAMT		! Discount Amount for record
	END RECORD

	COM (TT_AP_OPEN_DIST) RARRAY_RECORD RARRAY(300%)

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION FUNC_TESTENTRY
	EXTERNAL LONG    FUNCTION MAIN_WINDOW

	%PAGE

	ON ERROR GOTO 29000

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
		SMG_WINDOW::DESCR = "Line Items"
		SMG_WINDOW::NHELP = "AP_MAIN_OPEN_DIST"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 8%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 12%
		SMG_WINDOW::NITEMS= 9%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::TOPLIN = 3%
		SMG_WINDOW::BOTLIN = 7%

		!
		! Load in defaults for chart
		!
		CALL READ_DEFAULTS(SMG_WINDOW)

700		!
		! Declare channels
		!
		IF AP_OPEN_DIST.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF AP_OPEN_DIST.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN_DIST.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			AR_MAIN_OPEN_DIST = ERR
			CONTINUE 770
		END WHEN

		AP_OPEN_DIST.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN_DIST.OPN"
		USE
			AR_MAIN_OPEN_DIST = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		AP_OPEN_DIST.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(AP_OPEN_DIST.CH%)

		GOTO ExitFunction

790		SMG_WINDOW::CHAN  = AP_OPEN_DIST.CH%

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
			"  01         02   03           04     " + &
			"05       06 07       08         09      ", &
			1%, 1%, , SMG$M_REVERSE)
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"  PO Number  PO L Acct Num     S Acct " + &
			"Oper.    Ut Units        Amount Disc Amt", &
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
		AMOUNT = AP_OPEN::INVAMT
		DISCAMT = AP_OPEN::DISAMT

		ST% = MAIN_WINDOW(AP_MAIN_VENDOR.ID, "Q0" + AP_OPEN::VENNUM)

		IF ST% = 1%
		THEN
			VEN_DISCPER = &
				FUNC_ROUND((AP_VENDOR::DISCPER / 100.0), 2%)
		ELSE
			VEN_DISCPER = 0.0
		END IF

		AMOUNT = AMOUNT - RARRAY(I%)::AMOUNT &
			FOR I% = 1% TO SMG_WINDOW::TOTREC

		DISCAMT = FUNC_ROUND(DISCAMT - RARRAY(I%)::DISCAMT, 2%) &
			FOR I% = 1% TO SMG_WINDOW::TOTREC

		!
		! Display totals
		!
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"Number of lines" + &
			FORMAT$(SMG_WINDOW::TOTREC, "####") + &
			"                            REMAINING" + &
			FORMAT$(AMOUNT, " #########.##") + &
			FORMAT$(DISCAMT, " #####.##"), &
			SMG_WINDOW::VSIZE, 1%, , SMG$M_REVERSE)

		!
		! Paint lines on screen
		!
		FOR I% = 1% TO 8%
			A% = VAL%(MID("013,018,031,038,047,050,059,070", &
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

 Eloop:		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Purchase Order Number\*
	!	.b
	!	.lm +5
	!	The ^*Purchase Order Number\* field enters the number
	!	of the ordering form used when ordering the merchandise.
	!	.lm -5
	!
	! Index:
	!	.x Purchase Order Number>Line Items
	!	.x Line Items>Purchase Order Number
	!
	!--
			AP_OPEN_DIST::PONUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				XLINE$ + ";3", TEMP$, &
				AP_OPEN_DIST::PONUM, MFLAG OR 2%, &
				"~R 'E", MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Purchase Order Line\*
	!	.b
	!	.lm +5
	!	The ^*Purchase Order Line\* enters the line number on the
	!	purchase order which contains the desired item.
	!	.lm -5
	!
	! Index:
	!	.x Purchase Order Line>Line Items
	!	.x Line Items>Purchase Order Line
	!
	!--
			AP_OPEN_DIST::PO_LINE = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";14", TEMP$, &
				AP_OPEN_DIST::PO_LINE, MFLAG, "'E", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Account Number\*
	!	.b
	!	.lm +5
	!	The ^*Account Number\* field enters the General Ledger
	!	Account for which the order is placed.
	!	.lm -5
	!
	! Index:
	!	.x Account Number>Line Items
	!	.x Line Items>Account Number
	!
	!--
			AP_OPEN_DIST::ACCT = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";19", TEMP$, &
				AP_OPEN_DIST::ACCT, MFLAG, &
				"'LLLLLLLLLLL", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				AP_OPEN_DIST::ACCT = GL_CHART::ACCT &
					IF MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%
				GOTO ELoop
			END IF

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Sub Account\*
	!	.b
	!	.lm +5
	!	The ^*Sub Account\* field contains the work order or job number to which
	!	the Use Tax will be charged.
	!	.lm -5
	!
	! Index:
	!	.x Sub Account>Line Items
	!	.x Line Items>Sub Account
	!
	!--
			AP_OPEN_DIST::SUBACC = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";32", TEMP$, &
				AP_OPEN_DIST::SUBACC, MFLAG, "'LLLLL", MVALUE)

		CASE 5%

	!++
	! Abstract:FLD005
	!	^*(05) Operation\*
	!	.b
	!	.lm +5
	!	The ^*Operation\* field enters the process with in the Sub
	!	Accounts. For example, the Sub Account may be making a dress. The operations
	!	within the Sub Account may be cutting, pinning, and sewing.
	!	.lm -5
	!
	! Index:
	!	.x Operation>Line Items
	!	.x Line Items>Operation
	!
	!--
			AP_OPEN_DIST::OPERATION = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";39", TEMP$, &
				AP_OPEN_DIST::OPERATION, MFLAG, "'E", MVALUE)

		CASE 6%

	!++
	! Abstract:FLD006
	!	^*(06) Use Tax\*
	!	.b
	!	.lm +5
	!	The ^*Use Tax\* field allows for indication as to if there is an applicable
	!	Use Tax. A ^*Y\* indicates a Use tax, while an ^*N\* indicates no Use tax.
	!
	! Index:
	!	.x Use Tax>Line Items
	!	.x Line Items>Use Tax
	!
	!--
			AP_OPEN_DIST::USE_TAX_FLAG = &
				ENTR_3YESNO(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";48", TEMP$, &
				AP_OPEN_DIST::USE_TAX_FLAG, MFLAG, "!", MVALUE)

		CASE 7%

	!++
	! Abstract:FLD007
	!	^*(07) Units\*
	!	.b
	!	.lm +5
	!	The ^*Units\* field enters the number of units ordered for
	!	the line item.
	!	.lm -5
	!
	! Index:
	!	.x Units>Line Item
	!	.x Line Item>Units
	!
	!--
			AP_OPEN_DIST::UNITS = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";52", TEMP$, &
				AP_OPEN_DIST::UNITS, MFLAG, "####.##", MVALUE)

		CASE 8%

	!++
	! Abstract:FLD008
	!	^*(08) Amount\*
	!	.b
	!	.lm +5
	!	The ^*Amount\* field contains the total amount for the line item.
	!	.lm -5
	!
	! Index:
	!	.x Amount>Line Items
	!	.x Line Items>Amount
	!
	!--
			AP_OPEN_DIST::AMOUNT = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";60", TEMP$, &
				AP_OPEN_DIST::AMOUNT, MFLAG, &
				"#######.##", MVALUE)

		CASE 9%

	!++
	! Abstract:FLD009
	!	^*(09) Discount Amount\*
	!	.b
	!	.lm +5
	!	The discount percentage entered in the Vendor Master File
	!	for the subject vendor will cause the system to calculate the
	!	discount amount and automatically enter that amount in this field.
	!	If the discount percentage in the Vendor Master File is zero, the
	!	calculated result will be zero. The automatically entered amount
	!	may be overridden by typing a different amount and pressing ^*<Ent>\*.
	!	.lm -5
	!
	! Index:
	!	.x Discount Amount>Line Items
	!	.x Line Items>Discount Amount
	!
	!--
			AP_OPEN_DIST::DISCAMT = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";71", TEMP$, &
				AP_OPEN_DIST::DISCAMT, MFLAG, &
				"#####.##", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

20300	CASE OPT_TESTENTRY
		AP_MAIN_OPEN_DIST = 0%

		SELECT MLOOP
		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Account Number\*
	!	.b
	!	.lm +5
	!	The ^*Account Number\* field enters the General Ledger
	!	Account for which the order is placed.
	!	.lm -5
	!
	! Index:
	!	.x Account Number>Line Items
	!	.x Line Items>Account Number
	!
	!--
			AP_MAIN_OPEN_DIST = FUNC_TESTENTRY(SMG_WINDOW, &
				AP_OPEN_DIST::ACCT, GL_CHART::DESCR, &
				"GL", MLOOP, "ACCT", &
				"Account number", GL_MAIN_CHART.ID)

		END SELECT

20500	CASE OPT_SETOLD
		AP_OPEN_DIST_OLD = AP_OPEN_DIST

	CASE OPT_RESETOLD
		AP_OPEN_DIST = AP_OPEN_DIST_OLD

	CASE OPT_SETDEFAULT
		AP_OPEN_DIST2 = AP_OPEN_DIST

	CASE OPT_RESETDEFAULT
		AP_OPEN_DIST = AP_OPEN_DIST2

		AP_OPEN_DIST::TRANKEY = AP_OPEN::TRANKEY

		IF SMG_WINDOW::TOTREC = 0%
		THEN
			AP_OPEN_DIST::SLINE = "0001"
		ELSE
			AP_OPEN_DIST::SLINE = &
			FORMAT$(VAL%(RARRAY(SMG_WINDOW::TOTREC)::SLINE) + 1%, &
			"<0>###")
		END IF

		AMOUNT = 0.0
		DISCAMT = 0.0

		ST% = MAIN_WINDOW(AP_MAIN_VENDOR.ID, "Q0" + AP_OPEN::VENNUM)

		IF ST% = 1%
		THEN
			VEN_DISCPER = &
				FUNC_ROUND((AP_VENDOR::DISCPER / 100.0), 2%)
		ELSE
			VEN_DISCPER = 0.0
		END IF

		DISCAMT = FUNC_ROUND((AMOUNT * VEN_DISCPER), 2%) &
			IF VEN_DISCPER > 0%

		AMOUNT = AMOUNT - RARRAY(I%)::AMOUNT &
			FOR I% = 1% TO SMG_WINDOW::TOTREC

		AP_OPEN_DIST::AMOUNT = AMOUNT

		DISCAMT = FUNC_ROUND(DISCAMT - RARRAY(I%)::DISCAMT, 2%) &
			FOR I% = 1% TO SMG_WINDOW::TOTREC

		AP_OPEN_DIST::DISCAMT = DISCAMT

	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%

			FIND #AP_OPEN_DIST.CH%, &
				KEY #0% GE AP_OPEN_DIST::TRANKEY + "", &
				REGARDLESS
		END SELECT

27000	CASE OPT_ARRAY

		SELECT MLOOP

		CASE 1%

			SMG_WINDOW::TOTREC = 0%

27110			!
			WHEN ERROR IN
				FIND #SMG_WINDOW::CHAN, &
					KEY #0% GE AP_OPEN::TRANKEY + "", &
					REGARDLESS
			USE
				CONTINUE ExitFunction
			END WHEN

27120			!
			WHEN ERROR IN
				GET #SMG_WINDOW::CHAN
			USE
				CONTINUE ExitFunction IF ERR = 11%
				EXIT HANDLER
			END WHEN

			IF AP_OPEN_DIST::TRANKEY = AP_OPEN::TRANKEY
			THEN
				SMG_WINDOW::TOTREC = SMG_WINDOW::TOTREC + 1%
				RARRAY(SMG_WINDOW::TOTREC)::LINRFA = &
					GETRFA(SMG_WINDOW::CHAN)
				RARRAY(SMG_WINDOW::TOTREC)::SLINE = &
					AP_OPEN_DIST::SLINE
				RARRAY(SMG_WINDOW::TOTREC)::AMOUNT = &
					AP_OPEN_DIST::AMOUNT
				RARRAY(SMG_WINDOW::TOTREC)::DISCAMT = &
					AP_OPEN_DIST::DISCAMT
				GOTO 27120
			END IF

		CASE 2%

			RARRAY(I%) = RARRAY(I% + 1%) &
				FOR I% = MFLAG TO SMG_WINDOW::TOTREC - 1%

		CASE 3%

			RARRAY(MFLAG)::LINRFA	= GETRFA(SMG_WINDOW::CHAN)
			RARRAY(MFLAG)::SLINE	= AP_OPEN_DIST::SLINE
			RARRAY(MFLAG)::AMOUNT	= AP_OPEN_DIST::AMOUNT
			RARRAY(MFLAG)::DISCAMT	= AP_OPEN_DIST::DISCAMT

		CASE 4%

27200			GET #SMG_WINDOW::CHAN, RFA RARRAY(MFLAG)::LINRFA

		CASE 5%

			GET #SMG_WINDOW::CHAN, RFA RARRAY(MFLAG)::LINRFA, &
				REGARDLESS

		CASE 6%

			AP_OPEN_DIST::TRANKEY = RIGHT(MVALUE, 2%)

		END SELECT
	END SELECT

 ExitFunction:
	EXIT FUNCTION

	%PAGE

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
