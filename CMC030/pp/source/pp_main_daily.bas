1	%TITLE "Pacific Pride Daily Journal Maintenance"
	%SBTTL "PP_MAIN_DAILY"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PP_MAIN_DAILY(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1993 BY
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
	!	.lm +5
	!	.b
	!	The ^*Daily Transaction Maintenance\* function maintains the
	!	Daily Transaction File.  Data in this file includes
	!	Customer Number, Vechile, Driver, Transaction Date, Transaction
	!	Time, Driver, Location, Host, Site, Product, Quantity, Sale
	!	Type, and other information pertaining to each transaction.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PP_SOURCE:PP_MAIN_DAILY/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN PP_MAIN_DAILY
	!	$ DELETE PP_MAIN_DAILY.OBJ;*
	!
	! Author:
	!
	!	01/05/93 - Dan Perkins
	!
	! Modification history:
	!
	!	01/11/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	02/02/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	02/02/93 - Kevin Handy
	!		Added line number 20300 so error trapping would
	!		make sense.
	!
	!	02/04/93 - Kevin Handy
	!		Changed PP_DAILY::DISCOUNT to a string.
	!
	!	02/17/93 - Kevin Handy
	!		Lose LOCAT field, add IDENTITY field.
	!
	!	04/13/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/09/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/29/97 - Kevin Handy
	!		Lose unecessary external definitions
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	08/28/98 - Kevin Handy
	!		Fix format of view
	!
	!	11/02/2000 - Kevin Handy
	!		Use A"x"B
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
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PP_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[PP.OPEN]PP_DAILY.HB"
	MAP (PP_DAILY)		PP_DAILY_CDD		PP_DAILY
	MAP (PP_DAILY_OLD)	PP_DAILY_CDD		PP_DAILY_OLD, &
							PP_DAILY2

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[PP.OPEN]PP_TRANTYPE.HB"
	MAP (PP_TRANTYPE)	PP_TRANTYPE_CDD		PP_TRANTYPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_MEASURE.HB"
	MAP (UTL_MEASURE)	UTL_MEASURE_CDD		UTL_MEASURE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_STATE.HB"
	MAP (UTL_STATE)		UTL_STATE_CDD		UTL_STATE

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION MAIN_WINDOW
	EXTERNAL LONG    FUNCTION FUNC_TESTENTRY

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_PP_DAILY) &
		PP_DAILY.CH%, &
		PP_DAILY.READONLY%

	COM (BATCH_NO) &
		BATCH_NO$ = 8%

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

	!******************************************************************
	! Set up information
	!******************************************************************

		!
		! Define SMG_WINDOW
		!
		SMG_WINDOW::DESCR = "Daily Transaction Maintenance " + &
			PRNT_DATE(BATCH_NO$, 8%)
		SMG_WINDOW::NHELP = "PP_MAIN_DAILY"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 33%

		SMG_WINDOW::NKEYS = 2%
		SMG_WINDOW::KNAME(0%) = "Customer"
			SMG_WINDOW::KFIELD(0%, 0%) = 4%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
			SMG_WINDOW::KFIELD(0%, 2%) = 2%
			SMG_WINDOW::KFIELD(0%, 3%) = 4%
			SMG_WINDOW::KFIELD(0%, 4%) = 5%
		SMG_WINDOW::KNAME(1%) = "Host"
			SMG_WINDOW::KFIELD(1%, 0%) = 5%
			SMG_WINDOW::KFIELD(1%, 1%) = 6%
			SMG_WINDOW::KFIELD(1%, 2%) = 7%
			SMG_WINDOW::KFIELD(1%, 3%) = 8%
			SMG_WINDOW::KFIELD(1%, 4%) = 4%
			SMG_WINDOW::KFIELD(1%, 5%) = 5%

		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%

		COM (PP_MAIN_DAILY_FRM) FRM$(34%)

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF PP_DAILY.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PP_DAILY.READONLY%

			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PP.OPEN]PP_DAILY.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PP_MAIN_DAILY = ERR
			CONTINUE 770
		END WHEN

		PP_DAILY.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PP.OPEN]PP_DAILY.OPN"
		USE
			PP_MAIN_DAILY = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PP_DAILY.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PP_DAILY.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = PP_DAILY.CH%
		WHEN ERROR IN
			RESET #PP_DAILY.CH%
			GET #PP_DAILY.CH%, REGARDLESS
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

		DATA	01,03, "(01) Customer #", &
			03,03, "(02) Vehicle #", &
			04,03, "(03) Driver #", &
			05,03, "(04) Tran Date", &
			06,03, "(05) Tran Time", &
			07,03, "(06) Host", &
			08,03, "(07) Site", &
			09,03, "(08) Site Type", &
			10,03, "(09) Identity", &
			11,03, "(10) Product", &
			13,03, "(11) UOM", &
			14,03, "(12) Quantity", &
			15,03, "(13) Odometer", &
			16,03, "(14) Sale Type", &
			17,03, "(15) F Type", &
			18,03, "(16) Selling Price", &
			01,40, "(17) Tran Cost", &
			02,40, "(18) Misc Key", &
			03,40, "(19) Tran Type", &
			04,40, "(20) Discount", &
			05,40, "(21) ICB Date", &
			06,40, "(22) Tran #", &
			07,40, "(23) Sales Tax", &
			08,40, "(24) Pump #", &
			09,40, "(25) Buyer Fran", &
			10,40, "(26) Cap Date", &
			11,40, "(27) Cap Time", &
			12,40, "(28) Post Num", &
			13,40, "(29) Tran Source", &
			14,40, "(30) Edit Action", &
			15,40, "(31) Julian Day", &
			16,40, "(32) R Station", &
			17,40, "(33) State", &
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
	!	^*(01) Customer Number\*
	!	.b
	!	.lm +5
	!	The ^*Customer Number\* field enters the customer who made the purchase.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_DAILY::CUSNUM = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"1;23", TEMP$, PP_DAILY::CUSNUM, &
				MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF MAIN_WINDOW(AR_MAIN_35CUSTOM.ID, "VX") = 1%
				THEN
					PP_DAILY::CUSNUM = AR_35CUSTOM::CUSNUM
				END IF

				GOTO ReEnter
			END IF

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Vehicle Card Number\*
	!	.b
	!	.lm +5
	!	This field contains the vehicle card number, if such a card was
	!	used in the transaction.
	!	.b
	!	This field will accept 8 alpha-numeric characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_DAILY::VEHICLE = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"3;23",	TEMP$, PP_DAILY::VEHICLE, &
				MFLAG, "'E", MVALUE)

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Driver Card Number\*
	!	.b
	!	.lm +5
	!	This field contains the driver card number, if such a card was used
	!	in the transaction.
	!	.b
	!	This field accepts 8 alpha-numeric characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_DAILY::DRIVER = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"4;23",	TEMP$, PP_DAILY::DRIVER, &
				MFLAG, "'E", MVALUE)

		CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Transaction Date\*
	!	.lm +5
	!	.b
	!	This field specifies the date of the transaction.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_DAILY::TRANDATE = ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"5;23", TEMP$, PP_DAILY::TRANDATE, &
				MFLAG, "'E", MVALUE)

		CASE 5%
	!++
	! Abstract:FLD005
	!	^*(05) Transaction Time\*
	!	.b
	!	.lm +5
	!	This field specifies the time of the transaction.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_DAILY::TRANTIME = ENTR_3TIME(SCOPE, SMG_WINDOW::WNUMBER, &
				"6;23", TEMP$, PP_DAILY::TRANTIME, &
				MFLAG, "", MVALUE)

		CASE 6%
	!++
	! Abstract:FLD006
	!	^*(01) Host Number\*
	!	.b
	!	.lm +5
	!	The ^*Host Number\* field identifies the host used to make the
	!	transaction.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_DAILY::HOST = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"7;23", TEMP$, PP_DAILY::HOST, &
				MFLAG, "'E", MVALUE)

		CASE 7%
	!++
	! Abstract:FLD007
	!	^*(07) Site Number\*
	!	.b
	!	.lm +5
	!	The ^*Site Number\* field enters the site within the host that
	!	made the transaction.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_DAILY::SITE = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"8;23", TEMP$, PP_DAILY::SITE, &
				MFLAG, "'E", MVALUE)

		CASE 8%
	!++
	! Abstract:FLD008
	!	^*(08) Site Type\*
	!	.b
	!	.lm +5
	!	The ^*Site Type\* field enters a code for
	!	the type of site.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_DAILY::STYPE = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"9;23", TEMP$, PP_DAILY::STYPE, &
				MFLAG, "'E", MVALUE)

		CASE 9%
	!++
	! Abstract:FLD009
	!	^*(09) Identity\*
	!	.b
	!	.lm +5
	!	The ^*Location\* field enters a code for
	!	the identity.
	!	.b
	!	This field will accept 2 alpha-numeric characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_DAILY::IDENTITY = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"10;23", TEMP$, PP_DAILY::IDENTITY, &
				MFLAG, "'E", MVALUE)

		CASE 10%
	!++
	! Abstract:FLD010
	!	^*(10) Product Number\*
	!	.b
	!	.lm +5
	!	The ^*Product Number\* field enters the product number purchased.
	!	.b
	!	This field will accept 14 alpha-numeric characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_DAILY::PRODUCT = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"11;23", TEMP$, PP_DAILY::PRODUCT, &
				MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, "VX") = 1%
				THEN
					PP_DAILY::PRODUCT = &
						PD_PRODUCT::PRODUCT_NUM
				END IF

				GOTO ReEnter
			END IF

		CASE 11%
	!++
	! Abstract:FLD011
	!	^*(11) Unit of Measure\*
	!	.b
	!	.lm +5
	!	The ^*Unit of Measure\* field enters a code for
	!	the unit of measure by which the product was sold.
	!	.b
	!	This field will accept 2 alpha-numeric characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_DAILY::UOM = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"13;23", TEMP$, PP_DAILY::UOM, &
				MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF MAIN_WINDOW(UTL_MAIN_MEASURE.ID, "VX") = 1%
				THEN
					PP_DAILY::UOM = UTL_MEASURE::CODE
				END IF

				GOTO ReEnter
			END IF

		CASE 12%
	!++
	! Abstract:FLD012
	!	^*(12) Quantity\*
	!	.b
	!	.lm +5
	!	The ^*Quantity\* field enters the amount of product purchased.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_DAILY::QUANTITY = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"14;23", TEMP$, PP_DAILY::QUANTITY, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)

		CASE 13%
	!++
	! Abstract:FLD013
	!	^*(13) Odometer Reading\*
	!	.b
	!	.lm +5
	!	The ^*Odometer Reading\* field enters the odomoter reading
	!	at the time of the purchase.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_DAILY::ODOM = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"15;23", TEMP$, PP_DAILY::ODOM, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)

		CASE 14%
	!++
	! Abstract:FLD014
	!	^*(14) Sale Type\*
	!	.lm +5
	!	.b
	!	This field specifies how the product was purchased.  The following table
	!	specifies legal values for this field.
	!	.table 3,25
	!	.te
	!	L	Local Sale
	!	.te
	!	P	Local Purchase
	!	.te
	!	F	Foreign Sale
	!	.end table
	!	.b
	!	This field will accept 1 alpha-numeric character.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_DAILY::SLTYPE = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"16;23", TEMP$, PP_DAILY::SLTYPE, &
				MFLAG, "'E", MVALUE)

		CASE 15%
	!++
	! Abstract:FLD015
	!	^*(15) F Type\*
	!	.b
	!	.lm +5
	!	The ^*F Type\* field enters a code for
	!	the F type.
	!	.b
	!	This field will accept 1 alpha-numeric character.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_DAILY::FTYPE = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"17;23", TEMP$, PP_DAILY::FTYPE, &
				MFLAG, "'E", MVALUE)

		CASE 16%
	!++
	! Abstract:FLD016
	!	^*(16) Selling Price\*
	!	.b
	!	.lm +5
	!	This field specifies the selling price per unit (ie. gallon).
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_DAILY::SELLPRICE = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"18;23", TEMP$, PP_DAILY::SELLPRICE, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)

		CASE 17%
	!++
	! Abstract:FLD017
	!	^*(17) Transaction Cost\*
	!	.b
	!	.lm +5
	!	This field specifies the transaction cost (transfer price) per unit
	!	(ie. gallons).
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_DAILY::TRANCOST = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"1;58", TEMP$, PP_DAILY::TRANCOST, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)

		CASE 18%
	!++
	! Abstract:FLD018
	!	^*(18) Misc Keyborad Entry\*
	!	.b
	!	.lm +5
	!	The ^*Misc Keyboard Entry\* field enters any miscelanous keyboard
	!	entry made by the customer.
	!	.b
	!	This field will accept 9 alpha-numeric characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_DAILY::MISCKEYB = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"2;58", TEMP$, PP_DAILY::MISCKEYB, &
				MFLAG, "'E", MVALUE)

		CASE 19%
	!++
	! Abstract:FLD019
	!	^*(19) Transaction Type\*
	!	.b
	!	.lm +5
	!	The ^*Transaction Type\* field enters a code for
	!	the type of transaction.
	!	.b
	!	This field will accept 2 alpha-numeric characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_DAILY::TRNTYPE = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"3;58", TEMP$, PP_DAILY::TRNTYPE, &
				MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF MAIN_WINDOW(PP_MAIN_TRANTYPE.ID, "VX") = 1%
				THEN
					PP_DAILY::TRNTYPE = &
						PP_TRANTYPE::TRANTYPE
				END IF

				GOTO ReEnter
			END IF

		CASE 20%
	!++
	! Abstract:FLD020
	!	^*(20) Discount\*
	!	.b
	!	.lm +5
	!	The ^*Discount\* field enters a code for
	!	the type of discount.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_DAILY::DISCOUNT = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"4;58", TEMP$, PP_DAILY::DISCOUNT, MFLAG, &
				"'E", MVALUE)

		CASE 21%
	!++
	! Abstract:FLD021
	!	^*(21) ICB Date\*
	!	.b
	!	.lm +5
	!	The ^*ICB Date\* field enters the ICB date.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_DAILY::ICBDATE = ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"5;58", TEMP$, PP_DAILY::ICBDATE, &
				MFLAG, "'E", MVALUE)

		CASE 22%
	!++
	! Abstract:FLD022
	!	^*(22) Transaction Number\*
	!	.b
	!	.lm +5
	!	The ^*Transaction Number\* field enters the Pacific Prode transaction
	!	number.
	!	.b
	!	This field will accept 5 alpha-numeric characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_DAILY::TRNNUM = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"6;58", TEMP$, PP_DAILY::TRNNUM, &
				MFLAG, "'E", MVALUE)

		CASE 23%
	!++
	! Abstract:FLD023
	!	^*(23) Sales Tax Rate\*
	!	.b
	!	.lm +5
	!	The ^*Sales Tax Rate\* field enters the rate of sales tax.
	!	This is a per gallon rate.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_DAILY::STAXRATE = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"7;58", TEMP$, PP_DAILY::STAXRATE, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)

		CASE 24%
	!++
	! Abstract:FLD024
	!	^*(24) Pump Number\*
	!	.b
	!	.lm +5
	!	The ^*Pump Number\* field enters a code for the pump used to
	!	pump gas/diesel/etc..
	!	.b
	!	This field will accept 2 alpha-numeric characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_DAILY::PUMP = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"8;58", TEMP$, PP_DAILY::PUMP, &
				MFLAG, "'E", MVALUE)

		CASE 25%
	!++
	! Abstract:FLD025
	!	^*(25) Buyer Franchise\*
	!	.b
	!	.lm +5
	!	The ^*Buyer Franchise\* field enters a code for
	!	the buying franchise.
	!	.b
	!	This field will accept 3 alpha-numeric characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_DAILY::BUYFRAN = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"9;58", TEMP$, PP_DAILY::BUYFRAN, &
				MFLAG, "'E", MVALUE)

		CASE 26%
	!++
	! Abstract:FLD026
	!	^*(26) Capture Date\*
	!	.b
	!	.lm +5
	!	The ^*Capture Date\* field enters the date the transaction was
	!	captured.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_DAILY::CAPDATE = ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"10;58", TEMP$, PP_DAILY::CAPDATE, &
				MFLAG, "'E", MVALUE)

		CASE 27%
	!++
	! Abstract:FLD027
	!	^*(27) Capture Time\*
	!	.b
	!	.lm +5
	!	The ^*Capture Time\* field enters the time that the transaction
	!	was captured.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_DAILY::CAPTIME = ENTR_3TIME(SCOPE, SMG_WINDOW::WNUMBER, &
				"11;58", TEMP$, PP_DAILY::CAPTIME, &
				MFLAG, "", MVALUE)

		CASE 28%
	!++
	! Abstract:FLD028
	!	^*(28) Post Batch Number\*
	!	.b
	!	.lm +5
	!	The ^*Post Batch Number\* field enters the posting batch number.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_DAILY::POSTBNUM = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"12;58", TEMP$, PP_DAILY::POSTBNUM, &
				MFLAG, "'E", MVALUE)

		CASE 29%
	!++
	! Abstract:FLD029
	!	^*(29) Transaction Source\*
	!	.b
	!	.lm +5
	!	The ^*Transaction Source\* field enters the source of the transaction.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_DAILY::TRANSOURCE = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"13;58", TEMP$, PP_DAILY::TRANSOURCE, &
				MFLAG, "'E", MVALUE)

		CASE 30%
	!++
	! Abstract:FLD030
	!	^*(30) Edit Action\*
	!	.b
	!	.lm +5
	!	The ^*Edit Action\* field enters a code for any edit actions.
	!	.b
	!	This field will accept 1 alpha-numeric character.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_DAILY::EDITACT= ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"14;58", TEMP$, PP_DAILY::EDITACT, &
				MFLAG, "'E", MVALUE)

		CASE 31%
	!++
	! Abstract:FLD031
	!	^*(31) Julian Date\*
	!	.b
	!	.lm +5
	!	The ^*Julian Date\* field enters the julian day number
	!	(jan 1 = 1, Jan 2 = 2, Feb 1 = 32, etc) that the transaction
	!	occurred.
	!	.b
	!	This field will accept 3 alpha-numeric characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_DAILY::JULIANDAY = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"15;58", TEMP$, PP_DAILY::JULIANDAY, &
				MFLAG, "'E", MVALUE)

		CASE 32%
	!++
	! Abstract:FLD032
	!	^*(32) \*
	!	.b
	!	.lm +5
	!	The ^*R Station\* field enters a code for
	!	the reading station.
	!	.b
	!	This field will accept 1 alpha-numeric character.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_DAILY::RSTATION = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"16;58", TEMP$, PP_DAILY::RSTATION, &
				MFLAG, "'E", MVALUE)

		CASE 33%
	!++
	! Abstract:FLD033
	!	^*(33) State\*
	!	.b
	!	.lm +5
	!	The ^*State\* field enters a code for
	!	the state that the transaction took place in.
	!	.b
	!	This field will accept 2 alpha-numeric characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_DAILY::STATE = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"17;58", TEMP$, PP_DAILY::STATE, &
				MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF MAIN_WINDOW(UTL_MAIN_STATE.ID, "VX") = 1%
				THEN
					PP_DAILY::STATE = UTL_STATE::STATE
				END IF

				GOTO ReEnter
			END IF

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test the Entry.
	!
20300	CASE OPT_TESTENTRY

		PP_MAIN_DAILY = 0%

		SELECT MLOOP

		CASE 1%
			!
			! Display the descriptions for Customer
			!
			PP_MAIN_DAILY = FUNC_TESTENTRY(SMG_WINDOW, &
				PP_DAILY::CUSNUM, &
				AR_35CUSTOM::CUSNAM, &
				"PP", MLOOP, "PROG", &
				"Customer Number", AR_MAIN_35CUSTOM.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(AR_35CUSTOM::CUSNAM, 30%), &
				2%, 8%, , SMG$M_BOLD)

		CASE 10%
			!
			! Display the descriptions for Product
			!
			PP_MAIN_DAILY = FUNC_TESTENTRY(SMG_WINDOW, &
				PP_DAILY::PRODUCT, &
				PD_PRODUCT::DESCRIPTION, &
				"PP", MLOOP, "PROG", &
				"Product Number", PD_MAIN_PRODUCT.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(PD_PRODUCT::DESCRIPTION, 30%), &
				12%, 8%, , SMG$M_BOLD)

		CASE 11%
			!
			! Test Unit of Measure
			!
			PP_MAIN_DAILY = FUNC_TESTENTRY(SMG_WINDOW, &
				PP_DAILY::UOM, &
				UTL_MEASURE::DESCRIPTION, &
				"PP", MLOOP, "PROG", &
				"Unit of Measure", UTL_MAIN_MEASURE.ID)

		CASE 19%
			!
			! Test Transaction Type
			!
			PP_MAIN_DAILY = FUNC_TESTENTRY(SMG_WINDOW, &
				PP_DAILY::TRNTYPE, &
				PP_TRANTYPE::DESCRIPTION, &
				"PP", MLOOP, "PROG", &
				"Transaction Type", PP_MAIN_TRANTYPE.ID)

		CASE 33%
			!
			! Display the descriptions for State
			!
			PP_MAIN_DAILY = FUNC_TESTENTRY(SMG_WINDOW, &
				"US" + PP_DAILY::STATE, &
				UTL_STATE::DESCR, &
				"PP", MLOOP, "PROG", &
				"State", UTL_MAIN_STATE.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_STATE::DESCR, 17%, 62%, , SMG$M_BOLD)

		END SELECT

	!
	! Display the descriptions
	!
	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(1%) AND 2%) = 0%
		THEN
			AR_35CUSTOM::CUSNAM = &
				STRING$(LEN(AR_35CUSTOM::CUSNAM), A"?"B) &
				IF MAIN_WINDOW(AR_MAIN_35CUSTOM.ID, &
				"Q0" + PP_DAILY::CUSNUM) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(AR_35CUSTOM::CUSNAM, 30%), &
				2%, 8%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(10%) AND 2%) = 0%
		THEN
			PD_PRODUCT::DESCRIPTION = &
				STRING$(LEN(PD_PRODUCT::DESCRIPTION), A"?"B) &
				IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, &
				"Q0" + PP_DAILY::PRODUCT) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(PD_PRODUCT::DESCRIPTION, 30%), &
				12%, 8%, , SMG$M_BOLD)

		END IF

		IF (SMG_WINDOW::HFLAG(33%) AND 2%) = 0%
		THEN
			UTL_STATE::DESCR = &
				STRING$(LEN(UTL_STATE::DESCR), A"?"B) &
				IF MAIN_WINDOW(UTL_MAIN_STATE.ID, &
				"Q0" + "US" + PP_DAILY::STATE) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_STATE::DESCR, 17%, 62%, , SMG$M_BOLD)
		END IF

	!
	! Set PP_DAILY_OLD value
	!
	CASE OPT_SETOLD
		PP_DAILY_OLD = PP_DAILY

	!
	! Restore PP_DAILY_OLD value
	!
	CASE OPT_RESETOLD
		PP_DAILY = PP_DAILY_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PP_DAILY2 = PP_DAILY

		IF MFLAG = 1%
		THEN
			SELECT MLOOP
			CASE 0%
				FRM$(12%) = "#,###,###.##"
				FRM$(13%) = "###,###.#"
				FRM$(16%) = "###,###.###"
				FRM$(17%) = "###,###.###"
				FRM$(20%) = "##.##"
				FRM$(23%) = "##.##"
			CASE ELSE
				FRM$(MLOOP) = MVALUE
			END SELECT
		END IF

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PP_DAILY = PP_DAILY2

	!
	! View header
	!
	CASE OPT_VIEW

		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  CusNum     CusNam               " + &
				"Vehicle  Host Site STyp TranDate TranTime"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "013,034,043,048,053,058,067"

		!
		! Convert current record into text
		!
		CASE 3%
			AR_35CUSTOM::CUSNAM = &
				STRING$(LEN(AR_35CUSTOM::CUSNAM), A"?"B) &
				IF MAIN_WINDOW(AR_MAIN_35CUSTOM.ID, &
				"Q0" + PP_DAILY::CUSNUM) <> 1%

			MVALUE = PP_DAILY::CUSNUM + " " + &
				LEFT(AR_35CUSTOM::CUSNAM, 20%) + " " + &
				PP_DAILY::VEHICLE + " " + &
				PP_DAILY::HOST + " " + &
				PP_DAILY::SITE + " " + &
				PP_DAILY::STYPE + "    " + &
				PRNT_DATE(PP_DAILY::TRANDATE, 6%) + " " + &
				PRNT_TIME(PP_DAILY::TRANTIME, 2048%)

		END SELECT

	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE PP_DAILY::CUSNUM + &
				PP_DAILY::VEHICLE + &
				PP_DAILY::TRANDATE + &
				PP_DAILY::TRANTIME, REGARDLESS

		CASE 1%
			FIND #SMG_WINDOW::CHAN, &
				KEY #1% GE PP_DAILY::HOST + &
				PP_DAILY::SITE + &
				PP_DAILY::STYPE + &
				PP_DAILY::TRANDATE + &
				PP_DAILY::TRANTIME, REGARDLESS

		END SELECT

	END SELECT

27000	EXIT FUNCTION

29000	!
	! Trap errors
	!
	ON ERROR GO BACK

32767	END FUNCTION
