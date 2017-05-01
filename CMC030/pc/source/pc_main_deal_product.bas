1	%TITLE "Accounts Payable Journal Line Maintenance"
	%SBTTL "PC_MAIN_DEAL_PRODUCT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PC_MAIN_DEAL_PRODUCT(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 2001 BY
	!
	! Software Solutions, Inc.
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
	! Software Solutions, Inc.
	!
	! Software Solutions, Inc. assumes no responsibility for the use
	! or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	! Abstract:HELP
	!	.P
	!	.b
	!	.lm +5
	!	The ^*Product Deal\* file maintains
	!	a list of which "deals" that a customer is subscribed to.
	!	.lm -5
	!
	! Index:
	!	.x Deal Maintenance
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS PC_SOURCE:PC_MAIN_DEAL_PRODUCT/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP PC_MAIN_DEAL_PRODUCT
	!	$ DELETE PC_MAIN_DEAL_PRODUCT.OBJ;*
	!
	! Author:
	!
	!	07/19/2001 - Kevin Handy
	!
	! Modification history:
	!
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

	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PC_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	!
	! CDD and Maps
	!
	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD	AR_35CUSTOM

	%INCLUDE "SOURCE:[PC.OPEN]PC_DEAL.HB"
	MAP (PC_DEAL)		PC_DEAL_CDD	PC_DEAL

	%INCLUDE "SOURCE:[PC.OPEN]PC_DEAL_PRODUCT.HB"
	MAP (PC_DEAL_PRODUCT)		PC_DEAL_PRODUCT_CDD	PC_DEAL_PRODUCT
	MAP (PC_DEAL_PRODUCT_OLD)	PC_DEAL_PRODUCT_CDD	PC_DEAL_PRODUCT_OLD
	MAP (PC_DEAL_PRODUCT2)	PC_DEAL_PRODUCT_CDD	PC_DEAL_PRODUCT2

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT) PD_PRODUCT_CDD PD_PRODUCT
	DECLARE PD_PRODUCT_CDD PD_PRODUCT_EXAM

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD		GL_CHART

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_PC_DEAL_PRODUCT) &
		PC_DEAL_PRODUCT.CH%

	COM (CH_SB_SUBACCOUNT) &
		SB_SUBACCOUNT.CH%

	!
	! Create array to contain pointers and totals
	!
	RECORD RARRAY_RECORD
		RFA	LINRFA		! Rfa pointer for record
		STRING	SLINE = 15	! Line number
	END RECORD

	COM (TT_PC_DEAL_PRODUCT) RARRAY_RECORD RARRAY(600%)	! Allocate for 300

	COM (PC_MAIN_DEAL_PRODUCT_FRM) FRM$(9%)

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION FUNC_TESTENTRY
	EXTERNAL LONG   FUNCTION MAIN_WINDOW
	EXTERNAL LONG   FUNCTION PD_EXAM_PRODUCT
	EXTERNAL LONG	FUNCTION READ_35SET

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
		SMG_WINDOW::DESCR = "Products"
		SMG_WINDOW::NHELP = "PC_MAIN_DEAL_PRODUCT"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 10%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 10%
		SMG_WINDOW::NITEMS= 4%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::TOPLIN = 3%
		SMG_WINDOW::BOTLIN = 9%
		SMG_WINDOW::LINREC = 1%

		!
		! Load in defaults for chart
		!
		CALL READ_DEFAULTS(SMG_WINDOW)

700		!
		! Declare channels
		!
		IF PC_DEAL_PRODUCT.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PC_DEAL_PRODUCT.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PC.OPEN]PC_DEAL_PRODUCT.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PC_MAIN_DEAL_PRODUCT = ERR
			CONTINUE 770
		END WHEN

		PC_DEAL_PRODUCT.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PC.OPEN]PC_DEAL_PRODUCT.OPN"
		USE
			PC_MAIN_DEAL_PRODUCT = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PC_DEAL_PRODUCT.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PC_DEAL_PRODUCT.CH%)

		GOTO ExitFunction

790		SMG_WINDOW::CHAN  = PC_DEAL_PRODUCT.CH%

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
			"  01                    02        03      " + &
			"04                                  ", &
			1%, 1%, , SMG$M_REVERSE)
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"  Product              Price    Percent   " + &
			"Account                             ", &
			2%, 1%, , SMG$M_REVERSE)

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

	!
	! Extra display stuff
	!
	CASE OPT_DISPLAY

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		!
		! Display totals
		!
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"Number of lines" + &
			FORMAT$(SMG_WINDOW::TOTREC, "####") + &
			"                                                         ", &
			SMG_WINDOW::VSIZE, 1%, , SMG$M_REVERSE)

		!
		! Paint lines on screen
		!
		FOR I% = 1% TO 3%
			A% = VAL%(MID("018,030,041", &
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

 ReEnter:	SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Product\*
	!	.b
	!	.lm +5
	!	Inventory Product Number.
	!	.lm -5
	!
	! Index:
	!	.x Product>Deal Maintenance
	!	.x Deal Maintenance>Price
	!
	!--

			PC_DEAL_PRODUCT::PRODUCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				XLINE$ + ";3", TEMP$, &
				PC_DEAL_PRODUCT::PRODUCT, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, "VX") = 1%
				THEN
					PC_DEAL_PRODUCT::PRODUCT = &
						PD_PRODUCT::PRODUCT_NUM
				END IF
				GOTO Reenter
			END IF

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Price\*
	!	.b
	!	.lm +5
	!	The price to give for this product for anyone having this deal.
	!	.lm -5
	!
	! Index:
	!	.x Price>Deal Maintenance
	!	.x Deal Maintenance>Price
	!
	!--

			PC_DEAL_PRODUCT::PRICE = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				XLINE$ + ";21", TEMP$, &
				PC_DEAL_PRODUCT::PRICE, MFLAG, "####.####", &
				MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Percent\*
	!	.b
	!	.lm +5
	!	A percentage discount to use on this deal.
	!	.lm -5
	!
	! Index:
	!	.x Percent>Deal Maintenance
	!	.x Deal Maintenance>Percent
	!
	!--

			PC_DEAL_PRODUCT::PERCENT = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				XLINE$ + ";32", TEMP$, &
				PC_DEAL_PRODUCT::PERCENT, MFLAG, "####.####", &
				MVALUE)

		CASE 4%

	!++
	! Abstract:FLD001
	!	^*(01) Product\*
	!	.b
	!	.lm +5
	!	Inventory Product Number.
	!	.lm -5
	!
	! Index:
	!	.x Product>Deal Maintenance
	!	.x Deal Maintenance>Price
	!
	!--

			PC_DEAL_PRODUCT::ACCOUNT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				XLINE$ + ";43", TEMP$, &
				PC_DEAL_PRODUCT::ACCOUNT, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX  ") = 1%)
				THEN
					PC_DEAL_PRODUCT::ACCOUNT = &
						GL_CHART::ACCT
				END IF
				GOTO ReEnter
			END IF

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

20300	CASE OPT_TESTENTRY
		PC_MAIN_DEAL_PRODUCT = 0%

		SELECT MLOOP

		CASE 1%
			IF PD_EXAM_PRODUCT(PC_DEAL_PRODUCT::PRODUCT, &
				PD_PRODUCT_EXAM) = CMC$_UNDEFINED
			THEN
				EXIT_STATUS% = FUNC_TESTENTRY(SMG_WINDOW, &
					PC_DEAL_PRODUCT::PRODUCT, &
					PD_PRODUCT::DESCRIPTION, &
					"OE", MLOOP, "PROD", &
					"Product", PD_MAIN_PRODUCT.ID)

				PC_MAIN_DEAL_PRODUCT = EXIT_STATUS%
			ELSE
				IF PD_PRODUCT_EXAM::SSTATUS <> "A"
				THEN
					SMG_STATUS% = SMG$RING_BELL(SCOPE::SMG_KBID)
					CALL ENTR_3MESSAGE(SCOPE, &
						"Product is not active", 1%)
					IF READ_35SET("IC_ALLOW", "PRODUCT", &
						UTL_SET_READ) <> CMC$_NORMAL
					THEN
						PC_MAIN_DEAL_PRODUCT = 1%
					END IF
				END IF
			END IF

		CASE 4%
			PC_MAIN_DEAL_PRODUCT = FUNC_TESTENTRY(SMG_WINDOW, &
				PC_DEAL_PRODUCT::ACCOUNT, GL_CHART::DESCR, &
				"AR", MLOOP, "ACCT", &
				"Account number", GL_MAIN_CHART.ID)

		END SELECT

20500	CASE OPT_SETOLD
		PC_DEAL_PRODUCT_OLD = PC_DEAL_PRODUCT

	CASE OPT_RESETOLD
		PC_DEAL_PRODUCT = PC_DEAL_PRODUCT_OLD

	CASE OPT_SETDEFAULT
		PC_DEAL_PRODUCT2 = PC_DEAL_PRODUCT

	CASE OPT_RESETDEFAULT
		PC_DEAL_PRODUCT = PC_DEAL_PRODUCT2
		PC_DEAL_PRODUCT::DEAL = PC_DEAL::DEAL

	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #PC_DEAL_PRODUCT.CH%, &
				KEY #0% GE PC_DEAL_PRODUCT::DEAL + "", &
				REGARDLESS
		END SELECT

27000	CASE OPT_ARRAY

		SELECT MLOOP

		CASE 1%

			SMG_WINDOW::TOTREC = 0%

27110			!
			WHEN ERROR IN
				FIND #SMG_WINDOW::CHAN, &
					KEY #0% GE PC_DEAL::DEAL + "", &
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

			IF PC_DEAL_PRODUCT::DEAL = PC_DEAL::DEAL
			THEN
				SMG_WINDOW::TOTREC = SMG_WINDOW::TOTREC + 1%
				RARRAY(SMG_WINDOW::TOTREC)::LINRFA = &
					GETRFA(SMG_WINDOW::CHAN)
				RARRAY(SMG_WINDOW::TOTREC)::SLINE = &
					PC_DEAL_PRODUCT::PRODUCT
				GOTO 27120
			END IF

		CASE 2%

			RARRAY(I%) = RARRAY(I% + 1%) &
				FOR I% = MFLAG TO SMG_WINDOW::TOTREC - 1%

		CASE 3%

			RARRAY(MFLAG)::LINRFA	= GETRFA(SMG_WINDOW::CHAN)
			RARRAY(MFLAG)::SLINE	= PC_DEAL_PRODUCT::PRODUCT

		CASE 4%

27200			GET #SMG_WINDOW::CHAN, RFA RARRAY(MFLAG)::LINRFA

		CASE 5%

			GET #SMG_WINDOW::CHAN, RFA RARRAY(MFLAG)::LINRFA, &
				REGARDLESS

		CASE 6%

			PC_DEAL_PRODUCT::DEAL = RIGHT(MVALUE, 2%)

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
