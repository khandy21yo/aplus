1	%TITLE "Product Transaction History"
	%SBTTL "IC_35MAIN_HISTORY"
	%IDENT "V3.6a Calico"

	FUNCTION LONG IC_MAIN_35HISTORY(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

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
	!
	! Index:
	!	.x Transaction History
	!	.x Archive>Transaction History
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_MAIN_35HISTORY/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN IC_MAIN_35HISTORY
	!	$ DELETE IC_MAIN_35HISTORY.OBJ;*
	!
	! Author:
	!
	!	12/30/91 - Dan Perkins
	!
	! Modification history:
	!
	!	02/04/92 - Kevin Handy
	!		Cleaned out junk (check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	06/21/95 - Kevin Handy
	!		Added additional keys to find, fixed keys that
	!		were there.
	!
	!	10/18/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/30/2000 - Kevin Handy
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

	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[IC.OPEN]IC_35HISTORY.HB"
	MAP	(IC_35HISTORY)		IC_35HISTORY_CDD	IC_35HISTORY
	MAP	(IC_35HISTORY_OLD)	IC_35HISTORY_CDD	IC_35HISTORY_OLD, IC_35HISTORY2

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP	(PD_PRODUCT)		PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP	(UTL_LOCATION)		UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_TRANSTYPE.HB"
	MAP	(UTL_TRANSTYPE)		UTL_TRANSTYPE_CDD	UTL_TRANSTYPE

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_IC_35HISTORY) &
		IC_35HISTORY.CH%, &
		IC_35HISTORY.READONLY%

	COM (YEAR_IC_35HISTORY) YYYY$ = 4%, &
		ERA$ = 2%

	!
	! External functions
	!
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

		!******************************************************************
		! Set up information
		!******************************************************************

		!
		! Define SMG_WINDOW
		!
		SMG_WINDOW::DESCR = "Product Transaction History " + YYYY$
		SMG_WINDOW::NHELP = "IC_MAIN_35HISTORY"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 41%

		SMG_WINDOW::NKEYS = 4%
		SMG_WINDOW::KNAME(0%) = "Product_loc"
			SMG_WINDOW::KFIELD(0%, 0%) = 5%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
			SMG_WINDOW::KFIELD(0%, 2%) = 2%
			SMG_WINDOW::KFIELD(0%, 3%) = 3%
			SMG_WINDOW::KFIELD(0%, 4%) = 4%
			SMG_WINDOW::KFIELD(0%, 5%) = 5%
		SMG_WINDOW::KNAME(1%) = "Cross_ref"
			SMG_WINDOW::KFIELD(1%, 0%) = 5%
			SMG_WINDOW::KFIELD(1%, 1%) = 4%
			SMG_WINDOW::KFIELD(1%, 2%) = 1%
			SMG_WINDOW::KFIELD(1%, 3%) = 5%
			SMG_WINDOW::KFIELD(1%, 4%) = 3%
			SMG_WINDOW::KFIELD(1%, 5%) = 2%
		SMG_WINDOW::KNAME(2%) = "Subacct"
			SMG_WINDOW::KFIELD(2%, 0%) = 5%
			SMG_WINDOW::KFIELD(2%, 1%) = 5%
			SMG_WINDOW::KFIELD(2%, 2%) = 4%
			SMG_WINDOW::KFIELD(2%, 3%) = 1%
			SMG_WINDOW::KFIELD(2%, 4%) = 3%
			SMG_WINDOW::KFIELD(2%, 5%) = 2%
		SMG_WINDOW::KNAME(3%) = "pRoduct_subacc"
			SMG_WINDOW::KFIELD(3%, 0%) = 5%
			SMG_WINDOW::KFIELD(3%, 1%) = 1%
			SMG_WINDOW::KFIELD(3%, 2%) = 5%
			SMG_WINDOW::KFIELD(3%, 3%) = 4%
			SMG_WINDOW::KFIELD(3%, 4%) = 3%
			SMG_WINDOW::KFIELD(3%, 5%) = 2%

		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF IC_35HISTORY.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF IC_35HISTORY.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[IC.OPEN]IC_35HISTORY.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			IC_MAIN_35HISTORY = ERR
			CONTINUE 770
		END WHEN

		IC_35HISTORY.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[IC.OPEN]IC_35HISTORY.OPN"
		USE
			IC_MAIN_35HISTORY = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		IC_35HISTORY.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(IC_35HISTORY.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = IC_35HISTORY.CH%
		WHEN ERROR IN
			RESET #IC_35HISTORY.CH%

			GET #IC_35HISTORY.CH%, REGARDLESS
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


		DATA	01,08, "(01) Product #", &
			02,08, "(02) Location", &
			03,08, "(03) Trans Type", &
			04,08, "(04) Crossref", &
			05,08, "(05) Subaccount", &
			07,08, "(06)", &
			08,08, "(07)", &
			09,08, "(08)", &
			10,08, "(09)", &
			11,08, "(10)", &
			12,08, "(11)", &
			13,08, "(12)", &
			14,08, "(13)", &
			15,08, "(14)", &
			16,08, "(15)", &
			17,08, "(16)", &
			18,08, "(17)", &
			07,29, "(18)", &
			08,29, "(19)", &
			09,29, "(20)", &
			10,29, "(21)", &
			11,29, "(22)", &
			12,29, "(23)", &
			13,29, "(24)", &
			14,29, "(25)", &
			15,29, "(26)", &
			16,29, "(27)", &
			17,29, "(28)", &
			18,29, "(29)", &
			07,50, "(30)", &
			08,50, "(31)", &
			09,50, "(32)", &
			10,50, "(33)", &
			11,50, "(34)", &
			12,50, "(35)", &
			13,50, "(36)", &
			14,50, "(37)", &
			15,50, "(38)", &
			16,50, "(39)", &
			17,50, "(40)", &
			18,50, "(41)", &
			06,08, "           Quantity                Price                 Cost", &
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
	!	^*(01) Product _#\*
	!	.b
	!	.lm +5
	!	The ^*Product Number\* field enters a
	!	number which identifies a specific product.
	!	.b
	!	Valid Product Numbers may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x Product Number>Transaction History
	!	.x Transaction History>Product Number
	!	.x Number>Product
	!
	!--
			IC_35HISTORY::PRODUCT = ENTR_3STRING(SCOPE,  SMG_WINDOW::WNUMBER, &
				"1;26",TEMP$, IC_35HISTORY::PRODUCT, &
				MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, "VX")=1%
				THEN
					IC_35HISTORY::PRODUCT = &
						PD_PRODUCT::PRODUCT_NUM
				END IF
				GOTO Reenter
			END IF

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Location\*
	!	.b
	!	.lm +5
	!	The ^*Location\* field enters a location
	!	code for the product which is entered in field (01).
	!	.b
	!	Valid Location codes may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x Location>Transaction History
	!	.x Transaction History>Location
	!
	!--
			IC_35HISTORY::LOCATION = ENTR_3STRING(SCOPE,  SMG_WINDOW::WNUMBER, &
				"2;26",	TEMP$, IC_35HISTORY::LOCATION, &
				MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "V0")=1%
				THEN
					IC_35HISTORY::LOCATION = &
						UTL_LOCATION::LOCATION
				END IF
				GOTO Reenter
			END IF

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Transaction Type\*
	!	.b
	!	.lm +5
	!	The ^*Transaction Type\* field
	!	identifies this type of transaction. If the transaction type
	!	entered has been previously defined in the Company Maintenance
	!	record, the transaction description will automatically appear.
	!	If the transaction code is not valid, a message will appear
	!	on the screen: ^*Input undefined, enter anyway <Yes/No>: No.
	!	The default "No" may be accepted by pressing ^*Return\*, or the
	!	undefined type may be entered by pressing ^*"Y"\* and ^*Return\*.
	!	.b
	!	Valid transaction codes may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x Transaction Type>Transaction History
	!	.x Transaction History>Transaction Type
	!	.x Type>Transaction
	!
	!--
			IC_35HISTORY::TRANSTYPE = ENTR_3STRING(SCOPE,  SMG_WINDOW::WNUMBER, &
				"3;26",	TEMP$, IC_35HISTORY::TRANSTYPE, &
				MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UTL_MAIN_TRANSTYPE.ID, "V0")=1%
				THEN
					IC_35HISTORY::TRANSTYPE = &
						UTL_TRANSTYPE::CODE
				END IF
				GOTO Reenter
			END IF

		CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Cross Reference\*
	!	.b
	!	.lm +5
	!	The ^*Cross Reference\* field refers to the individual who originated the
	!	transaction. Examples of a ^*Cross Reference\* are a customer or vendor
	!	number. This entry connects the transaction to the
	!	person responsible.
	!	.lm -5
	!
	! Index:
	!	.x Cross Reference
	!
	!--
			IC_35HISTORY::CROSSREF = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"4;26", TEMP$, IC_35HISTORY::CROSSREF, MFLAG, "'E", &
				MVALUE)

		CASE 5%

	!++
	! Abstract:FLD005
	!	^*(05) Subaccount\*
	!	.b
	!	.lm +5
	!	The ^*Subaccount\* field creates a cross matrix for the sub
	!	ledger. Each entry refers to the specific job identification. Examples of
	!	a ^*Subaccount\* are job _#, work order, etc.
	!	.lm -5
	!
	! Index:
	!	.x Subaccount
	!
	!--
			IC_35HISTORY::SUBACCT = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"5;26", TEMP$, IC_35HISTORY::SUBACCT, MFLAG, "'E", &
				MVALUE)

		CASE 6% TO 17%
	!++
	! Abstract:FLD006
	!	^*(06-17) Period Quantity\*
	!	.b
	!	.lm +5
	!	The ^*Period Quantity\* field accesses the number of units sold,
	!	transferred, or exchanged, etc.,  depending on the transaction type entered.
	!	.lm -5
	!
	! Index:
	!	.x Period Quantity
	!
	!--
			IC_35HISTORY::PQUANTITY(MLOOP - 5%)= ENTR_3NUMBER(SCOPE,  SMG_WINDOW::WNUMBER, &
				NUM1$(MLOOP + 1%) + ";14",TEMP$, &
				IC_35HISTORY::PQUANTITY(MLOOP - 5%), MFLAG, &
				"##,###,###.##", MVALUE)

		CASE 18% TO 29%
	!++
	! Abstract:FLD018
	!	^*(18-29) Price Amount\*
	!	.b
	!	.lm +5
	!	The ^*Price Amount\* field accesses the price of units sold,
	!	transferred, or exchanged, etc., depending on the transaction type entered
	!	for the indicated period.
	!	.lm -5
	!
	! Index:
	!	.x Price Amount
	!
	!--
			IC_35HISTORY::PRICEAMT(MLOOP - 17%) = ENTR_3NUMBER(SCOPE,  SMG_WINDOW::WNUMBER, &
				NUM1$(MLOOP - 11%) + ";35",TEMP$, &
				IC_35HISTORY::PRICEAMT(MLOOP - 17%), MFLAG, &
				"##,###,###.##", MVALUE)

		CASE 30% TO 41%
	!++
	! Abstract:FLD030
	!	^*(30-41) Cost of Sale\*
	!	.b
	!	.lm +5
	!	The ^*Cost of Sale\* field enters the cost of units sold,
	!	transferred, or exchanged, etc., depending on the transaction type entered
	!	for the indicated period.
	!	.lm -5
	!
	! Index:
	!	.x Cost of Sale
	!
	!--
			IC_35HISTORY::COSTAMT(MLOOP - 29%) = ENTR_3NUMBER(SCOPE,  SMG_WINDOW::WNUMBER, &
				NUM1$(MLOOP - 23%) + ";56",TEMP$, &
				IC_35HISTORY::COSTAMT(MLOOP - 29%), MFLAG, &
				"##,###,###.##", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		IC_MAIN_35HISTORY = 0%

		SELECT MLOOP

		CASE 1%
			IC_MAIN_35HISTORY = FUNC_TESTENTRY(SMG_WINDOW, &
				IC_35HISTORY::PRODUCT, &
				PD_PRODUCT::DESCRIPTION, &
				"IC", MLOOP, "PROG", &
				"Product Number", PD_MAIN_PRODUCT.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_PRODUCT::DESCRIPTION, &
					1%, 41%, , SMG$M_BOLD)

		CASE 2%
			IC_MAIN_35HISTORY = FUNC_TESTENTRY(SMG_WINDOW, &
				IC_35HISTORY::LOCATION, &
				UTL_LOCATION::LOCNAME, &
				"IC", MLOOP, "PROG", &
				"Location", UTL_MAIN_LOCATION.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_LOCATION::LOCNAME, &
					2%, 41%, , SMG$M_BOLD)

		CASE 3%
			IC_MAIN_35HISTORY = FUNC_TESTENTRY(SMG_WINDOW, &
				IC_35HISTORY::TRANSTYPE, &
				UTL_TRANSTYPE::DESCRIPTION, &
				"IC", MLOOP, "PROG", &
				"Transaction Type", UTL_MAIN_TRANSTYPE.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_TRANSTYPE::DESCRIPTION, &
				3%, 41%, , SMG$M_BOLD)

		END SELECT

	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(1%) AND 2%) = 0%
		THEN
			PD_PRODUCT::DESCRIPTION = &
				STRING$(LEN(PD_PRODUCT::DESCRIPTION), A"?"B) &
				IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, "Q0" + &
					IC_35HISTORY::PRODUCT) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_PRODUCT::DESCRIPTION, 1%, 41%, , SMG$M_BOLD)
		END IF

	IF (SMG_WINDOW::HFLAG(2%) AND 2%) = 0%
	THEN
		UTL_LOCATION::LOCNAME = &
			STRING$(LEN(UTL_LOCATION::LOCNAME), A"?"B) &
			IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "Q0" + &
				IC_35HISTORY::LOCATION) <> 1%

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			UTL_LOCATION::LOCNAME, 2%, 41%, , SMG$M_BOLD)
	END IF

	IF (SMG_WINDOW::HFLAG(3%) AND 2%) = 0%
	THEN
		UTL_TRANSTYPE::DESCRIPTION = &
			STRING$(LEN(UTL_TRANSTYPE::DESCRIPTION), A"?"B) &
			IF MAIN_WINDOW(UTL_MAIN_TRANSTYPE.ID, "Q0" + &
				IC_35HISTORY::TRANSTYPE) <> 1%

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			UTL_TRANSTYPE::DESCRIPTION, 3%, 41%, , SMG$M_BOLD)
	END IF

	!
	! Set IC_35HISTORY_OLD value
	!
	CASE OPT_SETOLD
		IC_35HISTORY_OLD = IC_35HISTORY

	!
	! Restore IC_35HISTORY_OLD value
	!
	CASE OPT_RESETOLD
		IC_35HISTORY = IC_35HISTORY_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		IC_35HISTORY2 = IC_35HISTORY

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		IC_35HISTORY = IC_35HISTORY2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Product#       Loc " + &
				" TT CrossRef   Subacct"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "017,022,025,036,047"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = IC_35HISTORY::PRODUCT + " " + &
				IC_35HISTORY::LOCATION + " " + &
				IC_35HISTORY::TRANSTYPE + " " + &
				IC_35HISTORY::CROSSREF + " " + &
				IC_35HISTORY::SUBACCT

		END SELECT

	CASE OPT_FIND
		SELECT MLOOP
		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE IC_35HISTORY::PRODUCT + &
					IC_35HISTORY::LOCATION + &
					IC_35HISTORY::TRANSTYPE + &
					IC_35HISTORY::CROSSREF  + &
					IC_35HISTORY::SUBACCT, &
				REGARDLESS

		CASE 1%
			FIND #SMG_WINDOW::CHAN, &
				KEY #1% GE IC_35HISTORY::CROSSREF + &
					IC_35HISTORY::PRODUCT + &
					IC_35HISTORY::SUBACCT + &
					IC_35HISTORY::TRANSTYPE + &
					IC_35HISTORY::LOCATION, &
				REGARDLESS

		CASE 2%
			FIND #SMG_WINDOW::CHAN, &
				KEY #2% GE IC_35HISTORY::SUBACCT + &
					IC_35HISTORY::CROSSREF + &
					IC_35HISTORY::PRODUCT + &
					IC_35HISTORY::TRANSTYPE + &
					IC_35HISTORY::LOCATION, &
				REGARDLESS

		CASE 3%
			FIND #SMG_WINDOW::CHAN, &
				KEY #3% GE IC_35HISTORY::PRODUCT + &
					IC_35HISTORY::SUBACCT + &
					IC_35HISTORY::CROSSREF + &
					IC_35HISTORY::TRANSTYPE + &
					IC_35HISTORY::LOCATION, &
				REGARDLESS
		END SELECT

	END SELECT

27000	EXIT FUNCTION

29000	!
	! Trap errors
	!
	ON ERROR GO BACK

32767	END FUNCTION
