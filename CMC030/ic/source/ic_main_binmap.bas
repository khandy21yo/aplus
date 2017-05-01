1	%TITLE "Product Bin Location and Cycle Count"
	%SBTTL "IC_MAIN_BINMAP"
	%IDENT "V3.6a Calico"

	FUNCTION LONG IC_MAIN_BINMAP(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	.lm -5
	!
	! Index:
	!	.x Product Bin Location and Cycle Count
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_MAIN_BINMAP/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP IC_MAIN_BINMAP
	!	$ DELETE IC_MAIN_BINMAP.OBJ;*
	!
	! Author:
	!
	!	05/12/88 - Frank Starman
	!
	! Modification history:
	!
	!	05/31/88 - Aaron Redd
	!		Modified to allow R/O open of file if R/W open fails.
	!
	!	06/14/88 - Frank Starman
	!		Change lay out (add Safety stock and Maximum level)
	!
	!	04/06/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	04/23/92 - Kevin Handy
	!		Clean up (check)
	!
	!	11/20/92 - Dan Perkins
	!		Added CASE 2 to OPT_SUBWIND so that VIEW would
	!		work properly.
	!
	!	03/31/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	11/30/94 - Kevin Handy
	!		Added an "X" (exempt from cycle count) code.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/18/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/19/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[IC.OPEN]IC_BINMAP.HB"
	MAP (IC_BINMAP)		IC_BINMAP_CDD		IC_BINMAP
	MAP (IC_BINMAP_OLD)	IC_BINMAP_CDD		IC_BINMAP_OLD
	MAP (IC_BINMAP_DEF)	IC_BINMAP_CDD		IC_BINMAP_DEF

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_IC_BINMAP) &
		IC_BINMAP.CH%, &
		IC_BINMAP.READONLY%

	COM (TABLE_IC_BINMAP) &
		TITLE$ = 45%, &
		ABC$(5%) = 45%

	!
	! External functions
	!
	EXTERNAL	LONG    FUNCTION &
		MAIN_WINDOW, &
		FUNC_TESTENTRY

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
		SMG_WINDOW::DESCR = "Bin Location and Cycle Count Map"
		SMG_WINDOW::CURREC = -2%
		SMG_WINDOW::NHELP = "IC_MAIN_BINMAP"
		SMG_WINDOW::HSIZE = 76%
		SMG_WINDOW::VSIZE = 11%
		SMG_WINDOW::HPOS  = 3%
		SMG_WINDOW::VPOS  = 8%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 9%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Location"
		SMG_WINDOW::KFIELD(0%, 0%) = 1%
		SMG_WINDOW::KFIELD(0%, 1%) = 1%

		SMG_WINDOW::HVIEW = 77%
		SMG_WINDOW::VVIEW = 11%
		SMG_WINDOW::VVPOS = 8%
		SMG_WINDOW::VHPOS = 3%

		!
		! ABC Classification
		!
		TITLE$ = "Class  Description"
		ABC$(0%) = "5"
		ABC$(1%) = "      Undefined"
		ABC$(2%) = "A     Few Units with Large Dollar Volume"
		ABC$(3%) = "B     Moderate Units and Dollar Volume"
		ABC$(4%) = "C     Large Units with Small Dollar Volume"
		ABC$(5%) = "X     Exempt from cycle count"

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF IC_BINMAP.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF IC_BINMAP.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[IC.OPEN]IC_BINMAP.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			IC_MAIN_BINMAP = ERR
			CONTINUE 770
		END WHEN

		IC_BINMAP.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[IC.OPEN]IC_BINMAP.OPN"
		USE
			IC_MAIN_BINMAP = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		IC_BINMAP.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		CALL ASSG_FREECHANNEL(IC_BINMAP.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = IC_BINMAP.CH%
		WHEN ERROR IN
			RESET #IC_BINMAP.CH%
			GET #IC_BINMAP.CH%, REGARDLESS
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


		DATA	01,10, "(01) Location #", &
			02,10, "(02) Bin Loc 1", &
			03,10, "(03) Bin Loc 2", &
			04,10, "(04) Bin Loc 3", &
			05,10, "(05) Bin Loc 4", &
			06,10, "(06) Safety Stock", &
			07,10, "(07) Maximum Level", &
			08,10, "(08) ABC Flag", &
			10,10, "(09) 1234567890123456789012345678901234567890123456789012 WEEK", &
			09,15, "         1         2         3         4         5   ", &
			11,68, "NUMBER", &
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
	!	^*(01) Location _#\*
	!	.b
	!	.lm +5
	!	The ^*Location\* field
	!	enters the location code which is established in the Company
	!	Profile file in the Utility system.
	!	.b
	!	The field will accommodate up to four (4) alphanumeric
	!	characters.
	!	.b
	!	Valid Location codes may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x Location Number
	!
	!--


			IC_BINMAP::LOCATION = ENTR_3STRING(SCOPE,  SMG_WINDOW::WNUMBER, &
				"1;30",	TEMP$, IC_BINMAP::LOCATION, &
				MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "V0")=1%
				THEN
					IC_BINMAP::LOCATION = &
						UTL_LOCATION::LOCATION
				END IF
				GOTO Reenter
			END IF

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Bin Location 1\*
	!	.b
	!	.lm +5
	!	The ^*Bin Location 1\* field enters the
	!	physical location where the inventory item is stored.
	!	.b
	!	The field will accept six (06) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Bin Location 1
	!
	!--


			IC_BINMAP::BIN(0%) = ENTR_3STRING(SCOPE,  SMG_WINDOW::WNUMBER, &
				"2;30", TEMP$, IC_BINMAP::BIN(0%), &
				MFLAG, "'E", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Bin Location 2\*
	!	.b
	!	.lm +5
	!	The ^*Bin Location 2\* field enters
	!	the second physical location where the inventory item
	!	is stored.
	!	.b
	!	The field will accept six (06) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Bin Location 2
	!
	!--


			IC_BINMAP::BIN(1%) = ENTR_3STRING(SCOPE,  SMG_WINDOW::WNUMBER, &
				"03;30",TEMP$, IC_BINMAP::BIN(1%), &
				MFLAG, "'E", MVALUE)

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Bin Location 3\*
	!	.b
	!	.lm +5
	!	The ^*Bin Location 3\* field enters
	!	the third physical location where the inventory item
	!	is stored.
	!	.b
	!	The field will accept six (06) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Bin Location 3
	!
	!--


			IC_BINMAP::BIN(2%) = ENTR_3STRING(SCOPE,  SMG_WINDOW::WNUMBER, &
				"04;30",TEMP$, IC_BINMAP::BIN(2%), &
				MFLAG, "'E", MVALUE)

		CASE 5%

	!++
	! Abstract:FLD005
	!	^*(05) Bin Location 4\*
	!	.b
	!	.lm +5
	!	The ^*Bin Location 4\* field entersd
	!	the forth physical location where the inventory item
	!	is stored.
	!	.b
	!	The field will accept six (06) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Bin Location 4
	!
	!--


			IC_BINMAP::BIN(3%) = ENTR_3STRING(SCOPE,  SMG_WINDOW::WNUMBER, &
				"05;30",TEMP$, IC_BINMAP::BIN(3%), &
				MFLAG, "'E", MVALUE)

		CASE 6%

	!++
	! Abstract:FLD006
	!	^*(06) Safety Stock\*
	!	.b
	!	.lm +5
	!	The ^*Safety Stock\* field enters the
	!	quantity of stock planned to be in inventory to protect
	!	against fluctuations in demand and/or supply.
	!	.lm -5
	!
	! Index:
	!	.x Safety Stock
	!
	!--


			IC_BINMAP::SAFETY = ENTR_3NUMBER(SCOPE,  SMG_WINDOW::WNUMBER, &
				"6;30",TEMP$, IC_BINMAP::SAFETY, MFLAG, &
				"#,###,###.##", MVALUE)

		CASE 7%

	!++
	! Abstract:FLD007
	!	^*(07) Maximum Level\*
	!	.b
	!	.lm +5
	!	The ^*Maximum Level\* field enters the
	!	maximum stock allowable for an inventory item.
	!	.lm -5
	!
	! Index:
	!	.x Maximum Level
	!
	!--


			IC_BINMAP::MAXLEVEL = ENTR_3NUMBER(SCOPE,  SMG_WINDOW::WNUMBER, &
				"7;30",	TEMP$, IC_BINMAP::MAXLEVEL, MFLAG, &
				"#,###,###.##", MVALUE)

		CASE 8%

	!++
	! Abstract:FLD008
	!	^*(08) ABC Flag\*
	!	.b
	!	.lm +5
	!	The ^*ABC Flag\* field enters the flag which
	!	will represent a value control record for a selected item.
	!	Inventory items with the highest value would be represented
	!	by an "A" flag, while inventory items with lesser values would
	!	be represented by either a "B" or "C" flag. "B" representing
	!	inventory items with the next highest value, and "C" representing
	!	items with the least value.
	!	.lm -5
	!
	! Index:
	!	.x ABC Flag
	!	.x Flag>ABC
	!
	!--


			IC_BINMAP::ABC = ENTR_3STRINGLIST(SCOPE,  SMG_WINDOW::WNUMBER, &
				"08;30", TEMP$,IC_BINMAP::ABC, &
				MFLAG, "'E", MVALUE, ABC$(),TITLE$, "006")

		CASE 9%

	!++
	! Abstract:FLD009
	!	^*(09) Week Number\*
	!	.b
	!	.lm +5
	!	The ^*Week Number\* field enters the
	!	week number in which a selected inventory item is
	!	to be counted.
	!	.b
	!	An inventory item may be counted as many weeks as
	!	desired. After each entry an arrow will indicate
	!	that selection after which additional entries may
	!	be made.
	!	.lm -5
	!
	! Index:
	!	.x Week Number
	!	.x Number>Week
	!
	!--


			IC_BINMAP::CYCLEMAP = ENTR_BIT(SMG_WINDOW::WNUMBER, &
				"11;15",TEMP$, IC_BINMAP::CYCLEMAP, &
				MFLAG, "##", MVALUE, "^", 8%, 52%)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		IC_MAIN_BINMAP = 0%

		SELECT MLOOP

		CASE 1%
			IF IC_BINMAP::LOCATION = ""
			THEN
				IC_MAIN_BINMAP = 1%
			ELSE
				!
				! Is the input defined?
				!
				IC_MAIN_BINMAP = FUNC_TESTENTRY(SMG_WINDOW, &
					IC_BINMAP::LOCATION, UTL_LOCATION::LOCNAME, &
					"IC", MLOOP, "PROD", &
					"Location", UTL_MAIN_LOCATION.ID)
			END IF

			IF (MVALUE = "ADD")
			THEN
				WHEN ERROR IN
					GET #SMG_WINDOW::CHAN, &
						KEY #0% EQ IC_BINMAP::PRODUCT + &
						IC_BINMAP::LOCATION, REGARDLESS
				USE
					CONTINUE 32767 IF ERR = 155%
					EXIT HANDLER
				END WHEN

				IC_MAIN_BINMAP = 2%
				CALL ENTR_3MESSAGE(SCOPE, "Record Already Exists", 0%)
			END IF

		END SELECT

	!
	! Set IC_BINMAP_OLD value
	!
20500	CASE OPT_SETOLD
		IC_BINMAP_OLD = IC_BINMAP

	!
	! Restore IC_BINMAP_OLD value
	!
	CASE OPT_RESETOLD
		IC_BINMAP = IC_BINMAP_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		IC_BINMAP_DEF = IC_BINMAP

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		IC_BINMAP = IC_BINMAP_DEF
		IC_BINMAP::PRODUCT = MVALUE

	!
	! View header
	!
	CASE OPT_VIEW

		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE ="  Loc  Bin1   Bin2   "                    + &
				"123456789-123456789-123456789-123456789-" + &
				"123456789-12"
		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "007,014,021"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = IC_BINMAP::LOCATION + " " + &
				IC_BINMAP::BIN(0%)   + " " + &
				IC_BINMAP::BIN(1%)   + " " + &
				FUNC_BITSTRING(8%, IC_BINMAP::CYCLEMAP, 52%, "*")
		END SELECT

	!
	! Find
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE IC_BINMAP::PRODUCT + &
				IC_BINMAP::LOCATION, REGARDLESS

		END SELECT

	!
	! Handle array of records
	!
27000	CASE OPT_SUBWIND

		SELECT MLOOP

		!
		! Find first record (if there is any)
		!
		CASE 1%

			!
			! Set init value
			!
			SMG_WINDOW::CURREC = -1%

27110			!
			! Search for first record
			!
			WHEN ERROR IN
				FIND #SMG_WINDOW::CHAN, &
					KEY #0% EQ MVALUE, REGARDLESS

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
						KEY #0% GE MVALUE + &
						IC_BINMAP::LOCATION, REGARDLESS
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

			IF IC_BINMAP::PRODUCT = MVALUE
			THEN
				SMG_WINDOW::CURREC = 0%
			END IF

		!
		! Set the new key
		!
		CASE 6%
			IC_BINMAP::PRODUCT = MVALUE

		END SELECT

	END SELECT

28000	EXIT FUNCTION

29000	!
	! Trap errors
	!
	ON ERROR GO BACK


32767	END FUNCTION
	!+-+-+
	! More menu option hidden in mast
	!++
	! Abstract:RECORD
	!	^*Record\*
	!	.b
	!	.lm +5
	!	The ^*Record\* option provides faster access to the
	!	file. It makes it possible to scan or view for all products.
	!	.lm -5
	!
	! Index:
	!	.x Record
	!
	!--
