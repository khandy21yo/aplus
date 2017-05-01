1	%TITLE "Product Cost Maintenance"
	%SBTTL "PC_MAIN_COST"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PC_MAIN_COST(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

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
	!	The ^*Product Standard Cost\* file screen accesses
	!	routines where records for new or additional standard costs for
	!	particular products are entered and maintained.
	!	.lm -5
	!
	! Index:
	!	.x Cost Maintenance
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PC_SOURCE:PC_MAIN_COST/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP PC_MAIN_COST
	!	$ DELETE PC_MAIN_COST.OBJ;*
	!
	! Author:
	!
	!	06/21/88 - Frank Starman
	!
	! Modification history:
	!
	!	02/20/91 - Frank F. Starman
	!		Remove pack.
	!
	!	04/15/92 - Frank F. Starman
	!		Set today's date as a default if blank.
	!
	!	11/16/92 - Dan Perkins
	!		Added CASE 2 to OPT_SUBWIND so that VIEW would
	!		work properly.  Fixed NKEYS to find by product
	!		and date.  Fixed view screen.
	!
	!	11/17/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/08/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/20/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/01/2000 - Kevin Handy
	!		Use A"x"B
	!		Use WHEN ERROR IN
	!
	!	04/15/2003 - Kevin Handy
	!		Add a REGARDLESS to a GET
	!--

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

	%INCLUDE "SOURCE:[PC.OPEN]PC_COST.HB"
	MAP (PC_COST)		PC_COST_CDD	PC_COST
	MAP (PC_COST_OLD)	PC_COST_CDD	PC_COST_OLD
	MAP (PC_COST_DEF)	PC_COST_CDD	PC_COST_DEF

	%INCLUDE "SOURCE:[PC.OPEN]PC_PRCTYPE.HB"
	MAP (PC_PRCTYPE)	PC_PRCTYPE_CDD	PC_PRCTYPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD	PD_PRODUCT

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_PC_COST) &
		PC_COST.CH%, &
		PC_COST.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION FUNC_TESTENTRY
	EXTERNAL LONG    FUNCTION MAIN_WINDOW

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
		SMG_WINDOW::DESCR = "Product Standard Cost"
		SMG_WINDOW::CURREC = -2%
		SMG_WINDOW::NHELP = "PC_MAIN_COST"
		SMG_WINDOW::HSIZE = 76%
		SMG_WINDOW::VSIZE = 11%
		SMG_WINDOW::HPOS  = 3%
		SMG_WINDOW::VPOS  = 8%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 4%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Location"
			SMG_WINDOW::KFIELD(0%, 0%) = 2%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
			SMG_WINDOW::KFIELD(0%, 2%) = 2%

		SMG_WINDOW::HVIEW = 76%
		SMG_WINDOW::VVIEW = 11%
		SMG_WINDOW::VVPOS = 8%
		SMG_WINDOW::VHPOS = 3%

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF PC_COST.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PC_COST.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PC.OPEN]PC_COST.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PC_MAIN_COST = ERR
			CONTINUE 770
		END WHEN

		PC_COST.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PC.OPEN]PC_COST.OPN"
		USE
			PC_MAIN_COST = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PC_COST.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PC_COST.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = PC_COST.CH%
		WHEN ERROR IN
			RESET #PC_COST.CH%
			GET #PC_COST.CH%, REGARDLESS
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


		DATA	03,05, "(01) Location #", &
			04,05, "(02) Date", &
			05,05, "(03) Cost/Unit", &
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

	CASE OPT_MOREMENU

		PC_MAIN_COST = 16%

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
	!	.x Location Number>Cost Maintenance
	!	^*(01) Location Number\*
	!	.b
	!	.lm +5
	!	The ^*Location _#\* field enters a location code
	!	which is established in the Company Profile file located in the
	!	Utility System.
	!	.b
	!	Valid Location numbers may be viewed by pressing List Choices.
	!	.b
	!	Four spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!	.x Cost Maintenance>Location Number
	!
	!--

			PC_COST::LOCATION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"03;28", TEMP$, &
				PC_COST::LOCATION, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "V0 ") = 1%
				THEN
					PC_COST::LOCATION = &
						UTL_LOCATION::LOCATION
				END IF
				GOTO Reenter
			END IF

		CASE 2%

	!++
	! Abstract:FLD002
	!	.x Date>Cost Maintenance
	!	^*(02) Date\*
	!	.b
	!	.lm +5
	!	The ^*Date\* field enters the effective date for
	!	a particular product cost.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!	.x Cost Maintenance>Date
	!
	!--


			PC_COST::EFFDATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, "04;28", TEMP$, &
				PC_COST::EFFDATE, MFLAG, "'E", &
				MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Cost/Unit\*
	!	.b
	!	.lm +5
	!	The ^*Cost/Unit\* field enters the cost
	!	of a particular product.
	!	.b
	!	If a unique product is available in different packs, i.e.
	!	"case", "each", etc., that cost is entered in this field.
	!	.lm -5
	!
	! Index:
	!	.x Cost/Unit>Cost Maintenance
	!	.x Cost Maintenance>Cost/Unit
	!
	!--


			PC_COST::COST = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"05;28", TEMP$, PC_COST::COST, MFLAG, &
				"#,###,###.######", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		PC_MAIN_COST = 0%

		SELECT MLOOP

		CASE 1%
			!
			! Is the location defined?
			!
			PC_MAIN_COST = FUNC_TESTENTRY(SMG_WINDOW, &
				PC_COST::LOCATION, &
				UTL_LOCATION::LOCNAME, &
				"PC", MLOOP, "STORE", &
				"Location", UTL_MAIN_LOCATION.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_LOCATION::LOCNAME, &
				3%, 45%, , SMG$M_BOLD)

		END SELECT

	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(1%) AND 2%) = 0%
		THEN
			UTL_LOCATION::LOCNAME = &
				STRING$(LEN(UTL_LOCATION::LOCNAME), A"?"B) &
				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, &
				"Q0" + PC_COST::LOCATION) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_LOCATION::LOCNAME, &
				3%, 45%, , SMG$M_BOLD)
		END IF

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			PD_PRODUCT::UOM, 5%, 45%, , SMG$M_BOLD)

	!
	! Set PC_COST_OLD value
	!
20500	CASE OPT_SETOLD
		PC_COST_OLD = PC_COST

	!
	! Restore PC_COST_OLD value
	!
	CASE OPT_RESETOLD
		PC_COST = PC_COST_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PC_COST_DEF = PC_COST

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PC_COST = PC_COST_DEF
		PC_COST::PRODUCT = MVALUE

		IF MFLAG% = 1%
		THEN
			PC_COST::EFFDATE = DATE_TODAY &
				IF PC_COST::EFFDATE = ""
		END IF
	!
	! View header
	!
	CASE OPT_VIEW

		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Location Date                   Cost"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "011,022,039"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = PC_COST::LOCATION + "     " + &
				PRNT_DATE(PC_COST::EFFDATE, 8%) + " " + &
				FORMAT$(PC_COST::COST, "#,###,###.######")

		END SELECT

	!
	! Find
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE PC_COST::PRODUCT + &
				PC_COST::LOCATION + &
				PC_COST::EFFDATE, &
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
				GET #SMG_WINDOW::CHAN, &
					KEY #0% EQ MVALUE, &
					REGARDLESS

				!
				! Get a record
				!
				!GET #SMG_WINDOW::CHAN, REGARDLESS
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
						KEY #0% GE MVALUE + &
						PC_COST::LOCATION + &
						PC_COST::EFFDATE, &
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

			IF PC_COST::PRODUCT = MVALUE
			THEN
				SMG_WINDOW::CURREC = 0%
			END IF

		!
		! Set the new key
		!
		CASE 6%
			PC_COST::PRODUCT = MVALUE

		END SELECT


	END SELECT

28000	EXIT FUNCTION

29000	!
	! Trap errors
	!
	ON ERROR GO BACK

32767	END FUNCTION
	!+-+-+
	!++
	! Abstract:FLD004
	!	^*(04) Cost/Unit\*
	!	.b
	!	.lm +5
	!	The ^*Cost/Unit\* field enters the cost of
	!	a particular unit.
	!	.b
	!	If a unique product is available in a particular unit
	!	of measure, i.e. "gallon", "piece", "yard", that cost
	!	is entered in this field.
	!	.lm -5
	!
	! Index:
	!	.x Cost/Unit>Cost Maintenance
	!	.x Cost Maintenance>Cost/Unit
	!
	!--
	!+-+-+
	!++
	! Abstract:RECORD
	!	^*Record\*
	!	.p
	!	The ^*Record\* option provides faster access to the
	!	file. It makes it to possible scan or view for all products.
	!
	! Index:
	!	.x Record
	!
	!--
