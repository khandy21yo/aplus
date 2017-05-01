1	%TITLE "Pacific Pride Card Maintenance"
	%SBTTL "PP_MAIN_CARD"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PP_MAIN_CARD(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1992 BY
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
	!	The ^*Card Maintenance\* function maintains the Card File.
	!	Data in this file includes a user card number, card type,
	!	card description, and a field for an odometer reading depending
	!	on the type of card issued.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PP_SOURCE:PP_MAIN_CARD/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN PP_MAIN_CARD
	!	$ DELETE PP_MAIN_CARD.OBJ;*
	!
	! Author:
	!
	!	12/22/92 - Dan Perkins
	!
	! Modification history:
	!
	!	01/11/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	02/03/93 - Kevin Handy
	!		Added system customer number.
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
	!	08/28/97 - Kevin Handy
	!		Lose unecessary external definitions
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/15/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	!
	! Include Statements
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:PP_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[PP.OPEN]PP_CARD.HB"
	MAP (PP_CARD)		PP_CARD_CDD		PP_CARD
	MAP (PP_CARD_OLD)	PP_CARD_CDD		PP_CARD_OLD, &
							PP_CARD_DEF

	!
	! Common Statements
	!
	COM (CH_PP_CARD) &
		PP_CARD.CH%, &
		PP_CARD.READONLY%

	COM (TT_PP_CARDTYPE) &
		CARDTYPE_T$   = 20%, &
		CARDTYPE$(3%) = 20%

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR	= "Pacific Pride Card Maintenance"
		SMG_WINDOW::CURREC	= -2%
		SMG_WINDOW::NHELP	= "PP_MAIN_CARD"
		SMG_WINDOW::HSIZE	= 72%
		SMG_WINDOW::VSIZE	= 7%
		SMG_WINDOW::HPOS	= 3%
		SMG_WINDOW::VPOS	= 11%
		SMG_WINDOW::NITEMS	= 6%
		SMG_WINDOW::FLAGS	= 0%
		SMG_WINDOW::HVIEW	= 72%
		SMG_WINDOW::VVIEW	= 5%
		SMG_WINDOW::VHPOS	= 3%
		SMG_WINDOW::VVPOS	= 11%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Card_number"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		COM (PP_MAIN_CARD_FRM) FRM$(4%)

		!
		! Card Type Table
		!
		CARDTYPE_T$ = "Type Description    "
		CARDTYPE$(0%) = "3"
		CARDTYPE$(1%) = "1  Driver 1         "
		CARDTYPE$(2%) = "2  Driver 2         "
		CARDTYPE$(3%) = "3  Vechicle         "

		!
		! Load in defaults for chart
		!
		CALL READ_DEFAULTS(SMG_WINDOW)

700		!
		! Declare channels
		!
		IF PP_CARD.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PP_CARD.READONLY%

			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PP.OPEN]PP_CARD.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PP_MAIN_CARD = ERR
			CONTINUE 770
		END WHEN

		PP_CARD.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PP.OPEN]PP_CARD.OPN"
		USE
			PP_MAIN_CARD = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PP_CARD.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PP_CARD.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN = PP_CARD.CH%
		WHEN ERROR IN
			RESET #PP_CARD.CH%
			GET #PP_CARD.CH%, REGARDLESS
		USE
			CONTINUE 32767 IF ERR = 11%
			EXIT HANDLER
		END WHEN

	!******************************************************************
	! Display the background
	!
	! This option is used to display the background information on the
	! screen.  It must first clear any junk on the screen, and then
	! write the background onto it.
	!******************************************************************
20100	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	01,01, "(01) Card Number", &
			02,01, "(02) Type", &
			03,01, "(03) Description", &
			04,01, "(04) Odometer", &
			05,01, "(05) System Number", &
			06,01, "(06) Discount Code", &
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

 Reenter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%
	!++
	! Abstract:FLD001
	!	.x Card Number
	!	^*(01) Card Number\*
	!	.b
	!	.lm +5
	!	The ^*Card Number\* field enters a unique code
	!	identifying the card issued to a selected customer.
	!	A customer may have a nearly unlimited number of cards.
	!	.b
	!	This field will accept 8 alpha-numeric characters.
	!	.lm +5
	!
	! Index:
	!
	!--
			PP_CARD::CARD = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"1;19", TEMP$, PP_CARD::CARD, MFLAG, &
				"'E", MVALUE)

		CASE 2%
	!++
	! Abstract:FLD002
	!	.x Card Type
	!	^*(02) Card Type\*
	!	.b
	!	.lm +5
	!	The ^*Card type\* field identifies the type
	!	of card issued to the customer.
	!	.b
	!	Valid card types are:
	!	.table 3,33
	!	.te
	!	1	primary driver of the vehicle.
	!	.te
	!	2	secondary driver.
	!	.te
	!	3	the vehicle itself.
	!	.end table
	!	.b
	!	Odometer mileage readings are associated with
	!	a type 3 card.
	!	.b
	!	Valid codes may be viewed by pressing ^*List Choices, F14\*.
	!	.b
	!	This field will accept 1 numeric character.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_CARD::CTYPE = ENTR_3STRINGLIST(SCOPE, SMG_WINDOW::WNUMBER, &
				"2;19",	TEMP$, PP_CARD::CTYPE, &
				MFLAG, "!", MVALUE, &
				CARDTYPE$(), CARDTYPE_T$, "005")

		CASE 3%
	!++
	! Abstract:FLD003
	!	.x Card Description
	!	^*(03) Card Description\*
	!	.b
	!	.lm +5
	!	The ^*Card Description\* field enters
	!	a line describing the card that is issued.
	!	.b
	!	This field will accept 40 alpha-numeric characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_CARD::DESCRIPTION = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"3;19", TEMP$, PP_CARD::DESCRIPTION, MFLAG, &
				"'E", MVALUE)

		CASE 4%
	!++
	! Abstract:FLD004
	!	.x Odometer
	!	^*(04) Odometer\*
	!	.b
	!	.lm +5
	!	Depending on the Type of Card issued, the ^*Odometer\* field
	!	enters the odometer reading on the vechile for which the
	!	card is to be used.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_CARD::ODOMETER = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"4;19", TEMP$, PP_CARD::ODOMETER, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)

		CASE 5%
	!++
	! Abstract:FLD005
	!
	! Index:
	!
	!--
			PP_CARD::SYSCUS = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"5;19", TEMP$, PP_CARD::SYSCUS, MFLAG, &
				"'E", MVALUE)

		CASE 6%
	!++
	! Abstract:FLD006
	!
	! Index:
	!
	!--
			PP_CARD::DISCOUNT = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"6;19", TEMP$, PP_CARD::DISCOUNT, MFLAG, &
				"'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test the Entry.
	!
20300	CASE OPT_TESTENTRY

		PP_MAIN_CARD = 0%

		SELECT MLOOP

		CASE 1%

			!
			! Must have a card number
			!
			IF PP_CARD::CARD = ""
			THEN
				PP_MAIN_CARD = 1%
				GOTO 32767
			END IF

			!
			! See if the card number already exists
			!
			IF MVALUE = "ADD"
			THEN
				WHEN ERROR IN
					GET #PP_CARD.CH%, &
						KEY #0% EQ PP_CARD::CUSNUM + &
						PP_CARD::CARD, REGARDLESS
				USE
					CONTINUE 32767 IF ERR = 155%
					EXIT HANDLER
				END WHEN

				PP_MAIN_CARD = 2%
				CALL ENTR_3MESSAGE(SCOPE, "Record Already Exists", 1%)
				GOTO 32767
			END IF

		END SELECT

	!
	! Set PP_CARD_OLD value
	!
	CASE OPT_SETOLD
		PP_CARD_OLD = PP_CARD

	!
	! Restore PP_CARD_OLD value
	!
	CASE OPT_RESETOLD
		PP_CARD = PP_CARD_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PP_CARD_DEF = PP_CARD

		IF MFLAG = 1%
		THEN
			SELECT MLOOP

			CASE 0%
				FRM$(4%) = "###,###.#"

			CASE ELSE
				FRM$(MLOOP) = MVALUE

			END SELECT
		END IF

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PP_CARD = PP_CARD_DEF

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!
		PP_CARD::CUSNUM = MVALUE

	!
	! Find the next record
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE PP_CARD::CUSNUM + &
				PP_CARD::CARD, REGARDLESS

		END SELECT

	!
	! View header
	!
	CASE OPT_VIEW

		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Card     Type Description         " + &
				"                     Odometer"
		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "011,016,057"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = PP_CARD::CARD       + " "    + &
				PP_CARD::CTYPE       + "    " + &
				PP_CARD::DESCRIPTION + "  "   + &
				FORMAT$(PP_CARD::ODOMETER, "###,###")

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
				CONTINUE 28000
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
						PP_CARD::CARD, REGARDLESS
				USE
					CONTINUE 28000
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

			IF PP_CARD::CUSNUM = MVALUE
			THEN
				SMG_WINDOW::CURREC = 0%
			END IF

		!
		! Change key
		!
		CASE 6%
			PP_CARD::CUSNUM = MVALUE

		END SELECT

	END SELECT

28000	EXIT FUNCTION

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
