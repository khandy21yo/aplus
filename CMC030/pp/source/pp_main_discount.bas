1	%TITLE "Pacific Price Discount Maintenance"
	%SBTTL "PP_MAIN_DISCOUNT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PP_MAIN_DISCOUNT(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1992 BY
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
	!	The ^*Discount Maintenance\* function maintains the Discount File.
	!	Data in this file includes a discount code, discount method,
	!	and a discount table.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PP_SOURCE:PP_MAIN_DISCOUNT/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN PP_MAIN_DISCOUNT
	!	$ DELETE PP_MAIN_DISCOUNT.OBJ;*
	!
	! Author:
	!
	!	12/21/92 - Dan Perkins
	!
	! Modification history:
	!
	!	02/02/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/09/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/28/97 - Kevin Handy
	!		Lose unecessary external definitions
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/10/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	12/20/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[PP.OPEN]PP_DISCOUNT.HB"
	MAP (PP_DISCOUNT)	PP_DISCOUNT_CDD		PP_DISCOUNT
	MAP (PP_DISCOUNT_OLD)	PP_DISCOUNT_CDD		PP_DISCOUNT_OLD, &
							PP_DISCOUNT2

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_PP_DISCOUNT) &
		PP_DISCOUNT.CH%, &
		PP_DISCOUNT.READONLY%

	COM (TT_PP_DISCMETHOD) &
		DISCMETHOD_T$   = 20%, &
		DISCMETHOD$(2%) = 20%


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
		SMG_WINDOW::DESCR = "Pacific Pride Discount Maintenance"
		SMG_WINDOW::NHELP = "PP_MAIN_DISCOUNT"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 23%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "discount_Code"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%

		!
		! TAX FLAGS
		!
		DISCMETHOD_T$ = "Method Discount     "
		DISCMETHOD$(0%) = "2"
		DISCMETHOD$(1%) = "V    Volume         "
		DISCMETHOD$(2%) = "N    Non-volume     "

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF PP_DISCOUNT.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PP_DISCOUNT.READONLY%

			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PP.OPEN]PP_DISCOUNT.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PP_MAIN_DISCOUNT = ERR
			CONTINUE 770
		END WHEN

		PP_DISCOUNT.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PP.OPEN]PP_DISCOUNT.OPN"
		USE
			PP_MAIN_DISCOUNT = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PP_DISCOUNT.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PP_DISCOUNT.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = PP_DISCOUNT.CH%
		WHEN ERROR IN
			RESET #PP_DISCOUNT.CH%
			GET #PP_DISCOUNT.CH%, REGARDLESS
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

		DATA	02,03, "(01) Discount Type (A/I)", &
			03,03, "(02) Discount Description", &
			04,03, "(03) Discount Method (V/N)", &
			07,03, "(04)", &
			08,03, "(05)", &
			09,03, "(06)", &
			10,03, "(07)", &
			11,03, "(08)", &
			12,03, "(09)", &
			13,03, "(10)", &
			14,03, "(11)", &
			15,03, "(12)", &
			16,03, "(13)", &
			06,03, "     If over this many units          Amount / Unit", &
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
	!	^*(01) Discount Code\*
	!	.b
	!	.lm +5
	!	The ^*Discount Code\* field enters a code for
	!	the type of discount.
	!	.b
	!	This field will accept 4 alpha-numeric characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_DISCOUNT::CODE = ENTR_3STRING(SCOPE,  SMG_WINDOW::WNUMBER, &
				"2;31",TEMP$, PP_DISCOUNT::CODE, &
				MFLAG, "'E", MVALUE)

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Description\*
	!	.b
	!	.lm +5
	!	The ^*Description\* field enters a description
	!	for the type of discount entered in Field (01).
	!	.b
	!	This field will accept 40 alpha-numeric characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_DISCOUNT::DESCRIPTION = ENTR_3STRING(SCOPE,  SMG_WINDOW::WNUMBER, &
				"3;31",	TEMP$, PP_DISCOUNT::DESCRIPTION, &
				MFLAG, "'E", MVALUE)

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Method\*
	!	.b
	!	.lm +5
	!	The ^*Method\* field identifies the method of discount.
	!	Valid choices are: ^*V\* for volume discounts, or ^*N\* for non-volume
	!	discounts.
	!	.b
	!	Valid discount methods may be viewed by pressing ^*List Choices, F14\*.
	!	.b
	!	This field accepts 1 alpha character.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_DISCOUNT::METHOD = ENTR_3STRINGLIST(SCOPE, SMG_WINDOW::WNUMBER, &
				"4;31",	TEMP$, PP_DISCOUNT::METHOD, &
				MFLAG, "!", MVALUE, &
				DISCMETHOD$(), DISCMETHOD_T$, "007")

		CASE 4% TO 13%
	!++
	! Abstract:FLD004
	!	^*(04) Over - Amount per Unit\*
	!	.lm +5
	!	.b
	!	The ^*Over\* field refers to the product volume that must be
	!	obtained before a discount is given.
	!	.b
	!	The ^*Amount per Unit\* field refers to the discount amount per unit that is
	!	given once the volume has been reached.
	!	.lm -5
	!
	! Index:
	!
	!--

			PP_DISCOUNT::OVER(MLOOP - 4%)= ENTR_3NUMBER(SCOPE,  SMG_WINDOW::WNUMBER, &
				NUM1$(MLOOP + 3%) + ";18",TEMP$, &
				PP_DISCOUNT::OVER(MLOOP - 4%), MFLAG, &
				"##,###,###.##", MVALUE)

			PP_DISCOUNT::RATE(MLOOP - 4%) = ENTR_3NUMBER(SCOPE,  SMG_WINDOW::WNUMBER, &
				NUM1$(MLOOP + 3%) + ";42",TEMP$, &
				PP_DISCOUNT::RATE(MLOOP - 4%), MFLAG, &
				"###,###.####", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test the Entry.
	!
20300	CASE OPT_TESTENTRY

		PP_MAIN_DISCOUNT = 0%

		SELECT MLOOP

		CASE 1%
			!
			! Must have a card number
			!
			IF PP_DISCOUNT::CODE = ""
			THEN
				PP_MAIN_DISCOUNT = 1%
				GOTO 32767
			END IF

			!
			! See if the card number already exists
			!
			IF MVALUE = "ADD"
			THEN
				WHEN ERROR IN
					GET #PP_DISCOUNT.CH%, &
						KEY #0% EQ PP_DISCOUNT::CODE + "", &
						REGARDLESS
				USE
					CONTINUE 32767 IF ERR = 155%
					EXIT HANDLER
				END WHEN

				PP_MAIN_DISCOUNT = 2%
				CALL ENTR_3MESSAGE(SCOPE, "Record Already Exists", 1%)
				GOTO 32767
			END IF

		END SELECT

	!
	! Set PP_DISCOUNT_OLD value
	!
	CASE OPT_SETOLD
		PP_DISCOUNT_OLD = PP_DISCOUNT

	!
	! Restore PP_DISCOUNT_OLD value
	!
	CASE OPT_RESETOLD
		PP_DISCOUNT = PP_DISCOUNT_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PP_DISCOUNT2 = PP_DISCOUNT

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PP_DISCOUNT = PP_DISCOUNT2

	!
	! View header
	!
	CASE OPT_VIEW

		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Code Description         " + &
				"                     Method"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "007,048"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = PP_DISCOUNT::CODE + " " + &
				PP_DISCOUNT::DESCRIPTION + " " + &
				PP_DISCOUNT::METHOD

		END SELECT

	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE PP_DISCOUNT::CODE + "", &
				REGARDLESS

		END SELECT

	END SELECT

27000	EXIT FUNCTION

29000	!
	! Trap errors
	!
	ON ERROR GO BACK

32767	END FUNCTION
