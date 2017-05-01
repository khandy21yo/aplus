1	%TITLE "Customer Deal Maintenance"
	%SBTTL "PC_MAIN_DEAL"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PC_MAIN_DEAL(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

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
	!	PC_MAIN_DEAL_PRODUCT$HELP
	!
	! Compile:
	!
	!	$ BAS PC_SOURCE:PC_MAIN_DEAL/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP PC_MAIN_DEAL
	!	$ DELETE PC_MAIN_DEAL.OBJ;*
	!
	! Author:
	!
	!	07/19/2001 - Kevin Handy
	!
	! Modification history:
	!
	!	10/05/2001 - Kevin Handy
	!		New field (description) kbj.
	!
	!	03/07/2002 - Kevin Handy
	!		Move handler for deal_product into this function.
	!--

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PC_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD	AR_35CUSTOM

	%INCLUDE "SOURCE:[PC.OPEN]PC_DEAL.HB"
	MAP (PC_DEAL)		PC_DEAL_CDD	PC_DEAL
	MAP (PC_DEAL_OLD)	PC_DEAL_CDD	PC_DEAL_OLD
	MAP (PC_DEAL_DEF)	PC_DEAL_CDD	PC_DEAL_DEF

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD	PD_PRODUCT

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_PC_DEAL) &
		PC_DEAL.CH%, &
		PC_DEAL.READONLY%

	DECLARE RFA TEST_RFA
	!
	! External functions
	!
	EXTERNAL LONG FUNCTION PC_MAIN_DEAL_PRODUCT

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

		!*************************************************************
		! Set up information
		!*************************************************************

		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Customer Deal List"
		SMG_WINDOW::CURREC = -2%
		SMG_WINDOW::NHELP = "PC_MAIN_DEAL"
		SMG_WINDOW::HSIZE = 76%
		SMG_WINDOW::VSIZE = 11%
		SMG_WINDOW::HPOS  = 3%
		SMG_WINDOW::VPOS  = 8%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS = 4%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Product"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		SMG_WINDOW::HVIEW = 76%
		SMG_WINDOW::VVIEW = 11%
		SMG_WINDOW::VVPOS = 8%
		SMG_WINDOW::VHPOS = 3%

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF PC_DEAL.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PC_DEAL.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PC.OPEN]PC_DEAL.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PC_MAIN_DEAL = ERR
			CONTINUE 770
		END WHEN

		PC_DEAL.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PC.OPEN]PC_DEAL.OPN"
		USE
			PC_MAIN_DEAL = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PC_DEAL.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PC_DEAL.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = PC_DEAL.CH%
		WHEN ERROR IN
			RESET #PC_DEAL.CH%
			GET #PC_DEAL.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

	CASE OPT_OPTLIST
		MVALUE = MVALUE + " Products"

	CASE OPT_MOREMENU
		SELECT EDIT$(MVALUE, -1%)
		CASE "PRODUCTS"
		!++
		! Abstract:Product
		!--
			WHEN ERROR IN
				TEST_RFA = GETRFA(PC_DEAL.CH%)
			USE
				CONTINUE BadRfa
			END WHEN

			MAINT_GROUP = &
				MAIN_JOURNAL(PC_MAIN_DEAL_PRODUCT.ID, "")

 BadRfa:
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


		DATA	03,05, "(01) Deal", &
			04,05, "(02) From Date", &
			05,05, "(03) To   Date", &
			06,05, "(04) Description", &
			0, 0, ""

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

 ReEnter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Deal\*
	!	.b
	!	.lm +5
	!	Contains the customer deal being defined.
	!	This field selects which special price list a customer
	!	will receive.
	!	When a product is listed under multiple deals, the
	!	first one found will be the one that is used.
	!	.lm -5
	!
	! Index:
	!	.x Deal>Deal Maintenance
	!	.x Deal Maintenance>Deal
	!
	!--

			PC_DEAL::DEAL = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "03;28", TEMP$, &
				PC_DEAL::DEAL, MFLAG, "'E", MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Start Date\*
	!	.b
	!	.lm +5
	!	Selects the date that this "deal" starts at.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!	.x Start Date>Deal Maintenance
	!	.x Deal Maintenance>Start Date
	!
	!--

			PC_DEAL::STARTD = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, "04;28", TEMP$, &
				PC_DEAL::STARTD, MFLAG, "'E", &
				MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) End Date\*
	!	.b
	!	.lm +5
	!	Selects the date that this "deal" ends on.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!	.x End Date>Deal Maintenance
	!	.x Deal Maintenance>End Date
	!
	!--


			PC_DEAL::ENDD = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, "05;28", TEMP$, &
				PC_DEAL::ENDD, MFLAG, "'E", &
				MVALUE)

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Description\*
	!	.b
	!	.lm +5
	!	Enters a short description.
	!	.lm -5
	!
	! Index:
	!	.x Description>Deal Maintenance
	!	.x Deal Maintenance>Description
	!
	!--


			PC_DEAL::DESCR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "06;28", TEMP$, &
				PC_DEAL::DESCR, MFLAG, "'E", &
				MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Set PC_DEAL_OLD value
	!
20500	CASE OPT_SETOLD
		PC_DEAL_OLD = PC_DEAL

	!
	! Restore PC_DEAL_OLD value
	!
	CASE OPT_RESETOLD
		PC_DEAL = PC_DEAL_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PC_DEAL_DEF = PC_DEAL

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PC_DEAL = PC_DEAL_DEF
		PC_DEAL::CUSTOMER = AR_35CUSTOM::CUSNUM

	!
	! View header
	!
	CASE OPT_VIEW

		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Location   From Date   To Date"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "013,024,035"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = PC_DEAL::DEAL + " " + &
				PRNT_DATE(PC_DEAL::STARTD, 8%) + " " + &
				PRNT_DATE(PC_DEAL::ENDD, 8%) + " " + &
				PC_DEAL::DESCR

		END SELECT

	!
	! Find
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #1% GE PC_DEAL::CUSTOMER + &
				PC_DEAL::DEAL, &
				REGARDLESS

		END SELECT

	!
	! Handle array of records
	!
	CASE OPT_SUBWIND

		SELECT MLOOP

		!
		! Find first record (if there is any)
		!
		CASE 1%
			!
			! Set init value
			!
			SMG_WINDOW::CURREC = -1%

			!
27110			!
			! Search for first record
			!
			WHEN ERROR IN
				FIND #SMG_WINDOW::CHAN, &
					KEY #1% EQ AR_35CUSTOM::CUSNUM, &
					REGARDLESS

				!
				! Get a record
				!
				GET #SMG_WINDOW::CHAN
				SMG_WINDOW::CURREC = 0%
			USE
				CONTINUE 28000 IF ERR = 155%
				EXIT HANDLER
			END WHEN

		!
		! Find starting record (if there is any)
		!
		CASE 2%
			!
			! Set init value
			!
			SMG_WINDOW::CURREC = -1%

27115			!
			! Search for starting record
			!
			SELECT MFLAG

			CASE 0%
				WHEN ERROR IN
					FIND #SMG_WINDOW::CHAN, &
						KEY #1% GE MVALUE + &
						PC_DEAL::DEAL, &
						REGARDLESS
				USE
					CONTINUE 28000 IF ERR = 155%
					EXIT HANDLER
				END WHEN
			END SELECT

			!
			! Get a record
			!
			SMG_WINDOW::CURREC = 0%

		!
		! Check if still right key
		!
		CASE 3%
			SMG_WINDOW::CURREC = -1%

			IF PC_DEAL::CUSTOMER = MVALUE
			THEN
				SMG_WINDOW::CURREC = 0%
			END IF

		!
		! Set the new key
		!
		CASE 6%
			PC_DEAL::CUSTOMER = MVALUE

		END SELECT


	END SELECT

28000	EXIT FUNCTION

29000	!
	! Trap errors
	!
	ON ERROR GO BACK

32767	END FUNCTION
