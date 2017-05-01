1	%TITLE "Sales Category Maintenance"
	%SBTTL "OE_MAIN_CATEGORY"
	%IDENT "V3.6a Calico"

	FUNCTION LONG OE_MAIN_CATEGORY(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1987, 1988 BY
	!
	! Computer Management Center
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
	! Computer Management Center.
	!
	! CMC assumes no responsibility for the use or reliability of
	! its software on equipment which is not supported by CMC.
	!
	!++
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	Sales Category is a user defined code which gives the ability
	!	to catagorize orders.
	!	.lm -5
	!
	! Index:
	!	.x Maintanence>Category
	!	.x Category>Maintanence
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_MAIN_CATEGORY/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP OE_MAIN_CATEGORY
	!	$ DELETE OE_MAIN_CATEGORY.OBJ;*
	!
	!
	! Author:
	!
	!	06/14/90 - Lance Williams
	!
	! Modification history:
	!
	!	11/03/91 - Frank F. Starman
	!		Change Order category to Sales category.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/20/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/10/99 - Kevin Handy
	!		Fix FIND bug
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

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[OE.OPEN]OE_CATEGORY.HB"
	MAP (OE_CATEGORY)	OE_CATEGORY_CDD	OE_CATEGORY
	MAP (OE_CATEGORY_OLD)	OE_CATEGORY_CDD	OE_CATEGORY_OLD, OE_CATEGORY2

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_OE_CATEGORY) &
		OE_CATEGORY.CH%, &
		OE_CATEGORY.READONLY%

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
		SMG_WINDOW::DESCR = "Sales Category Maintenance"
		SMG_WINDOW::NHELP = "OE_MAIN_CATEGORY"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 2%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 2%
		SMG_WINDOW::KNAME(0%) = "Category"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
		SMG_WINDOW::KNAME(1%) = "Description"
			SMG_WINDOW::KFIELD(1%, 0%) = 1%
			SMG_WINDOW::KFIELD(1%, 1%) = 2%

		!
		! Load in defaults for chart
		!
		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF OE_CATEGORY.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF OE_CATEGORY.READONLY%
			GOTO 790
		END IF

		!
		! Open main file  OE_CATEGORY
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[OE.OPEN]OE_CATEGORY.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			EXIT HANDLER
		END WHEN

		OE_CATEGORY.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[OE.OPEN]OE_CATEGORY.OPN"
		USE
			CONTINUE 770 IF ERR = 5% OR ERR = 10%
			EXIT HANDLER
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		OE_CATEGORY.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(OE_CATEGORY.CH%)

		GOTO ExitFunction

790		SMG_WINDOW::CHAN  = OE_CATEGORY.CH%
		WHEN ERROR IN
			RESET #OE_CATEGORY.CH%
			GET #OE_CATEGORY.CH%, REGARDLESS
		USE
			CONTINUE 32767 IF ERR = 11%
			EXIT HANDLER
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

		DATA	6,  1, "(01) Sales Category", &
			8,  1, "(02) Description", &
			0,  0, ""

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

		SCOPE::SCOPE_EXIT = 0%

 Eloop:		SELECT MLOOP

		CASE 1%
	!++
	! Abstract:FLD001
	!	.x Sales Category
	!	^*(01) Sales Category\*
	!	.b
	!	.lm +5
	!	The ^*Sales Category\* field enters a user defined
	!	code which will identify an order or a ticket category.
	!	.b
	!	Two spaces are available for the entry.
	!	.b
	!	Example:  ^*CA\* - Cash Sale
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_CATEGORY::ORDCAT = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"6;22", TEMP$, &
				OE_CATEGORY::ORDCAT, MFLAG, "'E", MVALUE)

		CASE 2%
	!++
	! Abstract:FLD002
	!	.x Description
	!	^*(02) Description\*
	!	.b
	!	.lm +5
	!	The ^*Description\* field enters
	!	a brief description of the Category code entered in field (01)
	!	.b
	!	Thirty spaces are available for the description.
	!	.b
	!	Example:  CA - ^*Cash Sale\*
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_CATEGORY::DESCRIPTION = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"8;22", TEMP$, &
				OE_CATEGORY::DESCRIPTION, MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Set OE_CATEGORY_OLD value
	!
20500	CASE OPT_SETOLD
		OE_CATEGORY_OLD = OE_CATEGORY

	!
	! Restore OE_CATEGORY_OLD value
	!
	CASE OPT_RESETOLD
		OE_CATEGORY = OE_CATEGORY_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		OE_CATEGORY2 = OE_CATEGORY

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		OE_CATEGORY = OE_CATEGORY2

	!
	! View the Record
	!
	CASE OPT_VIEW
		SELECT MLOOP

		CASE 1%
			MVALUE = "Cat    Description"

		CASE 2%
			MVALUE = "007"

		CASE 3%
			MVALUE = &
				OE_CATEGORY::ORDCAT + " " + &
				OE_CATEGORY::DESCRIPTION

		END SELECT
	!
	! Find the Record.
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			!
			! Find by the Primary key ORDCAT,
			! Order Category
			!
			FIND #OE_CATEGORY.CH%, &
				KEY #0% GE OE_CATEGORY::ORDCAT + "", &
				REGARDLESS

		CASE 1%
			!
			! Find by the secondary key DESCRIPTION,
			! Order Category Description
			!
			FIND #OE_CATEGORY.CH%, &
				KEY #1% GE OE_CATEGORY::DESCRIPTION + "", &
				REGARDLESS

		END SELECT

	END SELECT

 ExitFunction:
	EXIT FUNCTION

29000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Resume to display untrapped error
	!
	FILENAME$ = ""
	ON ERROR GO BACK

32767	END FUNCTION
