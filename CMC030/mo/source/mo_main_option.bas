1	%TITLE " Option Maintenance"
	%SBTTL "MO_MAIN_OPTION"
	%IDENT "V3.6a Calico"

	FUNCTION LONG MO_MAIN_OPTION(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1991, BY
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
	! Computer Management Center, Inc.
	!
	! CMC assumes no responsibility for the use or reliability of
	! its software on equipment which is not supported by CMC.
	!
	!++
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The Option screen contains the following fields:
	!	.lm +10
	!	.b
	!	.LIST 0,"*"
	!	.le
	!	(01) Option Group
	!	.le
	!	(02) Option
	!	.le
	!	(03) Description
	!	.le
	!	(04) Product #
	!	.els
	!	.lm -10
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS MO_SOURCE:MO_MAIN_OPTION /LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN MO_MAIN_OPTION
	!	$ DELETE MO_MAIN_OPTION.OBJ;*
	!
	! Author:
	!
	!	03/01/91 - Craig Tanner
	!
	! Modification history:
	!
	!	11/23/92 - Dan Perkins
	!		Added CASE 2 to OPT_SUBWIND so that VIEW would
	!		work properly.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	01/26/96 - Kevin Handy
	!		Reformat source code.
	!		Lose second CASE 0% in SELECT statement.
	!		Change STRING$(...,ASCII(" ")) to "" in several
	!		places.
	!
	!	08/28/97 - Kevin Handy
	!		Lose unecessary external definitions
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/31/2000 - Kevin Handy
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

	%INCLUDE "FUNC_INCLUDE:MO_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[MO.OPEN]MO_OPTION.HB"
	MAP (MO_OPTION)		MO_OPTION_CDD		MO_OPTION
	MAP (MO_OPTION_OLD)	MO_OPTION_CDD		MO_OPTION_OLD, MO_OPTION2

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_MO_OPTION) &
		MO_OPTION.CH%, &
		MO_OPTION.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION FUNC_TESTENTRY
	EXTERNAL LONG	FUNCTION MAIN_WINDOW

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	!
	! Initilization
	!
	! This option is used to initialize the window structure,
	! set up the default values for add, and open all files
	! necessary that have not already been opened.
	!
	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Options Attached to Group"
		SMG_WINDOW::NHELP = "MO_MAIN_OPTION"
		SMG_WINDOW::CURREC = -2%
		SMG_WINDOW::HSIZE = 76%
		SMG_WINDOW::VSIZE = 6%
		SMG_WINDOW::HPOS  = 3%
		SMG_WINDOW::VPOS  = 10%
		SMG_WINDOW::NITEMS= 3%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::HVIEW = 76%
		SMG_WINDOW::VVIEW = 11%
		SMG_WINDOW::VHPOS = 3%
		SMG_WINDOW::VVPOS = 10%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Option"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		IF INSTR(1%, " QV", MVALUE) <= 1%
		THEN
			!
			! Load in defaults
			!
			CALL READ_DEFAULTS(SMG_WINDOW)
		END IF

700		!
		! Declare channels
		!
		IF MO_OPTION.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF MO_OPTION.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[MO.OPEN]MO_OPTION.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			MO_MAIN_OPTION = ERR
			CONTINUE 770
		END WHEN

		MO_OPTION.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[MO.OPEN]MO_OPTION.OPN"
		USE
			MO_MAIN_OPTION = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		MO_OPTION.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(MO_OPTION.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = MO_OPTION.CH%

		WHEN ERROR IN
			RESET #MO_OPTION.CH%
			GET #MO_OPTION.CH%, REGARDLESS
		USE
			CONTINUE 32767 IF ERR = 11%
			EXIT HANDLER
		END WHEN

	!
	! Display the background
	!
	! This option is used to display the background information
	! on the screen.  It must first clear any junk on the screen,
	! and then write the background onto it.
	!

20100	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	02, 05, "(01) Option", &
			03, 05, "(02) Description", &
			04, 05, "(03) Product #", &
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

	!
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user, display
	! data, set defaults, and return the data back according to
	! MFLAG.
	!
20200	CASE OPT_ENTRY
		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 Reenter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Option\*
	!	.b
	!	.lm +5
	!	The ^*Option\* field enters a code indicating an option which
	!	is available in the corresponding grouping.  Examples of options include
	!	colors and styles.
	!	.b
	!	The field will accommodate four (4) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x OPTION>Option
	!	.x Option>OPTION
	!
	!--

			MO_OPTION::OPTN = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "2;25", TEMP$, &
				MO_OPTION::OPTN, MFLAG, "'E", &
				MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Description\*
	!	.b
	!	.lm +5
	!	The ^*Description\* field enters a description
	!	to identify the Option entered in field (02).
	!	.b
	!	The field will accommodate forty (40) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x OPTION>Description
	!	.x Description>OPTION
	!
	!--

			MO_OPTION::DESCR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "3;25", TEMP$, &
				MO_OPTION::DESCR, MFLAG, "'E", &
				MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Product _#\*
	!	.b
	!	.lm +5
	!	The ^*Product _#\* field enters the number of the product
	!	which is associated with the option and option group.
	!	.b
	!	The field will accommodate fourteen (14) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x OPTION>Product
	!	.x Product>OPTION
	!
	!--

			MO_OPTION::PRODUCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "4;25", TEMP$, &
				MO_OPTION::PRODUCT, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, "VX") = 1%
				THEN
					MO_OPTION::PRODUCT = &
						PD_PRODUCT::PRODUCT_NUM
				END IF
				GOTO Reenter
			END IF

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		MO_MAIN_OPTION = 0%

		SELECT MLOOP

		CASE 1%
			SELECT MVALUE

			CASE "ADD"
				WHEN ERROR IN
					FIND #MO_OPTION.CH%, KEY #0% EQ &
						MO_OPTION::OPTGROUP + &
						MO_OPTION::OPTN, REGARDLESS
				USE
					CONTINUE ExitProgram IF ERR = 155%
					EXIT HANDLER
				END WHEN

				MO_MAIN_OPTION = 2%
				CALL ENTR_3MESSAGE(SCOPE, "Duplicate key detected ", 0%)

			END SELECT

		CASE 3%
			!
			! Is the input defined?
			!
			IF MO_OPTION::PRODUCT <> ""
			THEN
				MO_MAIN_OPTION = FUNC_TESTENTRY (SMG_WINDOW, &
					MO_OPTION::PRODUCT, &
					PD_PRODUCT::DESCRIPTION, &
					"MO", MLOOP, "PROG", &
					"Product", PD_MAIN_PRODUCT.ID)
			ELSE
				PD_PRODUCT::DESCRIPTION = ""
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_PRODUCT::DESCRIPTION, &
				9%, 40%, , SMG$M_BOLD)

		END SELECT

	!
	! Display descriptions
	!
	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(3%) AND 2%) = 0%
		THEN
			IF MAIN_WINDOW(MO_MAIN_OPTGROUP.ID, &
				"Q0" + MO_OPTION::PRODUCT) <> 1%
			THEN
				PD_PRODUCT::DESCRIPTION = &
					STRING$(LEN(PD_PRODUCT::DESCRIPTION), A"?"B)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_PRODUCT::DESCRIPTION, &
				9%, 40%, , SMG$M_BOLD)
		END IF

	!
	! Set MO_OPTION_OLD value
	!
20500	CASE OPT_SETOLD
		MO_OPTION_OLD = MO_OPTION

	!
	! Restore MO_OPTION_OLD value
	!
	CASE OPT_RESETOLD
		MO_OPTION = MO_OPTION_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		MO_OPTION2 = MO_OPTION

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		MO_OPTION = MO_OPTION2
		MO_OPTION::OPTGROUP = MVALUE

	!
	! View header
	!
	CASE OPT_VIEW

		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  OptGroup  Option  Product        Description"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "012,020,035"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = MO_OPTION::OPTGROUP + "        " + &
				MO_OPTION::OPTN + "    "     + &
				MO_OPTION::PRODUCT + " "        + &
				MO_OPTION::DESCR

		END SELECT

	!
	! Find
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #MO_OPTION.CH%, KEY #0% &
				GE MO_OPTION::OPTGROUP + &
				MO_OPTION::OPTN, REGARDLESS

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

27110			!
			! Search for first record
			!
			WHEN ERROR IN
				FIND #SMG_WINDOW::CHAN, KEY #0% EQ MVALUE, REGARDLESS

				!
				! Get a record
				!
				GET #SMG_WINDOW::CHAN
				SMG_WINDOW::CURREC = 0%
			USE
				CONTINUE ExitProgram
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
					FIND #MO_OPTION.CH%, KEY #0% &
						GE MVALUE + &
						MO_OPTION::OPTN, REGARDLESS
				USE
					CONTINUE ExitProgram
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

			IF MO_OPTION::OPTGROUP = MVALUE
			THEN
				SMG_WINDOW::CURREC = 0%
			END IF

		!
		! Change key
		!
		CASE 6%
			MO_OPTION::OPTGROUP = MVALUE

		END SELECT

	END SELECT

 ExitProgram:
	EXIT FUNCTION

29000	!
	! Trap Errors
	!
	ON ERROR GO BACK

32767	END FUNCTION
