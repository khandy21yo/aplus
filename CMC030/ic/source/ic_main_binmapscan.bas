1	%TITLE "Maintain Bin Location and Cycle Map"
	%SBTTL "IC_MAIN_BINMAPSCAN"
	%IDENT "V3.6a Calico"

	FUNCTION LONG IC_MAIN_BINMAPSCAN(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	.x Maintain Bin Location and Cycle Map
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_MAIN_BINMAPSCAN/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP IC_MAIN_BINMAPSCAN
	!	$ DELETE IC_MAIN_BINMAPSCAN.OBJ;*
	!
	! Author:
	!
	!	05/18/88 - Frank F. Starman
	!
	! Modification history:
	!
	!	05/31/88 - Aaron Redd
	!		Modified to allow R/O open of file if R/W open fails.
	!
	!	06/14/88 - Frank Starman
	!		Changed layout (added Safety stock and Maximum level).
	!
	!	04/06/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	04/23/92 - Kevin Handy
	!		Clean up (check)
	!
	!	03/30/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	05/27/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/13/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[IC.OPEN]IC_BINMAP.HB"
	MAP (IC_BINMAP)		IC_BINMAP_CDD		IC_BINMAP
	MAP (IC_BINMAP_OLD) IC_BINMAP_CDD IC_BINMAP_OLD, IC_BINMAP2

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

	!
	! External functions
	!
	EXTERNAL STRING  FUNCTION &
		ENTR_BIT, &
		FUNC_BITSTRING

	EXTERNAL LONG    FUNCTION &
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

		!*************************************************************
		! Set up information
		!*************************************************************

		!
		! Define SMG_WINDOW
		!
		SMG_WINDOW::DESCR = "Bin Location and Cycle Map"
		SMG_WINDOW::CURREC = -2%
		SMG_WINDOW::NHELP = "IC_MAIN_BINMAPSCAN"
		SMG_WINDOW::HSIZE = 68%
		SMG_WINDOW::VSIZE = 14%
		SMG_WINDOW::HPOS  = 8%
		SMG_WINDOW::VPOS  = 4%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 10%

		SMG_WINDOW::NKEYS = 2%
		SMG_WINDOW::KNAME(0%) = "Product_number"
			SMG_WINDOW::KFIELD(0%, 0%)	= 2%
			SMG_WINDOW::KFIELD(0%, 1%)	= 1%
			SMG_WINDOW::KFIELD(0%, 2%)	= 2%
		SMG_WINDOW::KNAME(1%) = "Location_number"
			SMG_WINDOW::KFIELD(1%, 0%)	= 2%
			SMG_WINDOW::KFIELD(1%, 1%)	= 2%
			SMG_WINDOW::KFIELD(1%, 2%)	= 1%

		SMG_WINDOW::HVIEW	= 130%
		SMG_WINDOW::VVIEW	= 18%
		!SMG_WINDOW::VHPOS	= 3%
		!SMG_WINDOW::VVPOS	= 8%

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
			IC_MAIN_BINMAPSCAN = ERR
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
			IC_MAIN_BINMAPSCAN = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		IC_BINMAP.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
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


		DATA	02,03, "(01) Product #", &
			03,03, "(02) Location #", &
			04,03, "(03) Bin Loc 1", &
			05,03, "(04) Bin Loc 2", &
			06,03, "(05) Bin Loc 3", &
			07,03, "(06) Bin Loc 4", &
			08,03, "(07) Safety Stock", &
			09,03, "(08) Maximum Level", &
			10,03, "(09) ABC Flag", &
			12,03, "(10) 1234567890123456789012345678901234567890123456789012 WEEK", &
			11,08, "         1         2         3         4         5   ", &
			13,61, "NUMBER", &
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
	!	The ^*Product _#\* field
	!	enters an assigned number which identifies a specific product.
	!	.b
	!	The field will accommodate up to fourteen (14) alphanumeric
	!	characters.
	!	.b
	!	Each product number must be unique. No duplicates are allowed.
	!	.lm -5
	!
	! Index:
	!	.x Product>Number
	!
	!--
			IC_BINMAP::PRODUCT = &
				ENTR_3STRING(SCOPE,  SMG_WINDOW::WNUMBER, &
				"2;20",	TEMP$, IC_BINMAP::PRODUCT, &
				MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, "VX") = 1%
				THEN
					IC_BINMAP::PRODUCT = &
						PD_PRODUCT::PRODUCT_NUM
				END IF
				GOTO Reenter
			END IF

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Location _#\*
	!	.b
	!	.lm +5
	!	The ^*Location _#\* field
	!	enters the location code which has been established in the Company
	!	Profile file which is located in the Utility system.
	!	.b
	!	The field will accommodate up to four (4) alphanumeric
	!	characters.
	!	.b
	!	Pressing ^*List Choices\* at this field will cause a list
	!	of valid locations to be displayed.
	!	.lm -5
	!
	! Index:
	!	.x Location Number
	!	.x Number>Location
	!
	!--
			IC_BINMAP::LOCATION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"3;20",	TEMP$, IC_BINMAP::LOCATION, &
				MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "V0") = 1%
				THEN
					IC_BINMAP::LOCATION = &
						UTL_LOCATION::LOCATION
				END IF
				GOTO Reenter
			END IF

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Bin Location 1\*
	!	.b
	!	.lm +5
	!	The ^*Bin Location 1\* field enters the
	!	physical location where this inventory item is stored.
	!	.b
	!	The field will accept six (06) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Bin Location 1
	!
	!--
			IC_BINMAP::BIN(0%) = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"4;20", TEMP$, IC_BINMAP::BIN(0%), &
				MFLAG, "'E", MVALUE)

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Bin Location 2\*
	!	.b
	!	.lm +5
	!	The ^*Bin Location 2\* field enters the second
	!	physical location where this inventory item is stored.
	!	.b
	!	The field will accept six (06) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Bin Location 2
	!
	!--
			IC_BINMAP::BIN(1%) = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"05;20",TEMP$, IC_BINMAP::BIN(1%), &
				MFLAG, "'E", MVALUE)

		CASE 5%

	!++
	! Abstract:FLD005
	!	^*(05) Bin Location 3\*
	!	.b
	!	.lm +5
	!	The ^*Bin Location 3\* field enters the third
	!	physical location where this inventory item is stored.
	!	.b
	!	The field will accept six (06) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Bin Location 3
	!
	!--
			IC_BINMAP::BIN(2%) = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"06;20",TEMP$, IC_BINMAP::BIN(2%), &
				MFLAG, "'E", MVALUE)

		CASE 6%

	!++
	! Abstract:FLD006
	!	^*(06) Bin Location 4\*
	!	.b
	!	.lm +5
	!	The ^*Bin Location 4\* field enters the fourth
	!	physical location where this inventory item is stored.
	!	.b
	!	The field will accept six (06) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Bin Location 4
	!
	!--
			IC_BINMAP::BIN(3%) = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"07;20",TEMP$, IC_BINMAP::BIN(3%), &
				MFLAG, "'E", MVALUE)

		CASE 7%

	!++
	! Abstract:FLD007
	!	^*(07) Safety Stock\*
	!	.b
	!	.lm +5
	!	The ^*Safety Stock\* field enters the
	!	quantity of stock planned to be in inventory to protect
	!	against fluctuations in demand and/or supply.
	!	.lm -5
	!
	! Index:
	!	.x Safety Stock
	!	.x Stock>Safety
	!
	!--
			IC_BINMAP::SAFETY = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"8;20",TEMP$, IC_BINMAP::SAFETY, MFLAG, &
				"#,###,###.##", MVALUE)

		CASE 8%

	!++
	! Abstract:FLD008
	!	^*(08) Maximum Level\*
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
			IC_BINMAP::MAXLEVEL = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"9;20",	TEMP$, IC_BINMAP::MAXLEVEL, MFLAG, &
				"#,###,###.##", MVALUE)
		CASE 9%

	!++
	! Abstract:FLD009
	!	^*(09) ABC Flag\*
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
	!
	!--
			IC_BINMAP::ABC = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"10;20", TEMP$,IC_BINMAP::ABC, &
				MFLAG, "'E", MVALUE)

		CASE 10%

	!++
	! Abstract:FLD010
	!	^*(10) Week Number\*
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
				"13;08",TEMP$, IC_BINMAP::CYCLEMAP, &
				MFLAG, "##", MVALUE, "^", 8%, 52%)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		IC_MAIN_BINMAPSCAN = 0%

		SELECT MLOOP

		CASE 1%
			IF IC_BINMAP::PRODUCT = ""
			THEN
				IC_MAIN_BINMAPSCAN = 1%
			ELSE
				!
				! Is the input defined?
				!
				IC_MAIN_BINMAPSCAN = FUNC_TESTENTRY(SMG_WINDOW, &
					IC_BINMAP::PRODUCT, PD_PRODUCT::DESCRIPTION, &
					"IC", MLOOP, "PROD", &
					"Product", PD_MAIN_PRODUCT.ID)
			END IF

		CASE 2%
			IF IC_BINMAP::LOCATION = ""
			THEN
				IC_MAIN_BINMAPSCAN = 1%
			ELSE
				!
				! Is the input defined?
				!
				IC_MAIN_BINMAPSCAN = FUNC_TESTENTRY(SMG_WINDOW, &
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

				IC_MAIN_BINMAPSCAN = 2%
				CALL ENTR_3MESSAGE(SCOPE, &
					"Record Already Exists", 0%)
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
		IC_BINMAP2 = IC_BINMAP

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		IC_BINMAP = IC_BINMAP2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
		MVALUE ="  Product#       Loc   BinLoc1 BinLoc2 BinLoc3 " + &
			"BinLoc4 ABC 123456789-123456789-123456789-123456789-" + &
			"123456789-12"
		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "017,023,031,039,047,055,059"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = IC_BINMAP::PRODUCT + " " + &
				IC_BINMAP::LOCATION + "  " + &
				IC_BINMAP::BIN(0%) + "  " + &
				IC_BINMAP::BIN(1%) + "  " + &
				IC_BINMAP::BIN(2%) + "  " + &
				IC_BINMAP::BIN(3%) + "  " + &
				IC_BINMAP::ABC + "   " + &
				FUNC_BITSTRING(8%, IC_BINMAP::CYCLEMAP, &
				52%, "*")
		END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP
		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE IC_BINMAP::PRODUCT + &
				IC_BINMAP::LOCATION, &
				REGARDLESS

		CASE 1%
			FIND #SMG_WINDOW::CHAN, &
				KEY #1% GE IC_BINMAP::LOCATION + &
				IC_BINMAP::PRODUCT, &
				REGARDLESS
		END SELECT

	END SELECT

28000	EXIT FUNCTION

29000	!
	! Trap errors
	!
	ON ERROR GO BACK

32767	END FUNCTION
