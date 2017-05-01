1	%TITLE "Pacific Pride Card Exemption Maintenance"
	%SBTTL "PP_MAIN_CARDEXEMPT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PP_MAIN_CARDEXEMPT(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	The ^*Card Exemption Maintenance\* function maintains
	!	information in the Card Exemption File including the exempting
	!	state, the exempting authority, and the exempted product.  A
	!	nearly unlimited number of exemptions are possible for each card.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PP_SOURCE:PP_MAIN_CARDEXEMPT/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN PP_MAIN_CARDEXEMPT
	!	$ DELETE PP_MAIN_CARDEXEMPT.OBJ;*
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
	!	02/02/93 - Kevin Handy
	!		Added line number 20300 so error trapping would
	!		make sense.
	!
	!	02/18/93 - Kevin Handy
	!		Fixed bug where was not looking at card number
	!		to see if done.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/09/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
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

	!
	! Include Statements
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:PP_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[PP.OPEN]PP_CARDEXEMPT.HB"
	MAP (PP_CARDEXEMPT)	PP_CARDEXEMPT_CDD	PP_CARDEXEMPT
	MAP (PP_CARDEXEMPT_OLD)	PP_CARDEXEMPT_CDD	PP_CARDEXEMPT_OLD, &
							PP_CARDEXEMPT_DEF

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_STATE.HB"
	MAP (UTL_STATE)		UTL_STATE_CDD		UTL_STATE

	!
	! Common Statements
	!
	COM (CH_PP_CARDEXEMPT) &
		PP_CARDEXEMPT.CH%, &
		PP_CARDEXEMPT.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION MAIN_WINDOW
	EXTERNAL LONG	FUNCTION FUNC_TESTENTRY

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR	= "Pacific Pride Card Exeption Maintenance"
		SMG_WINDOW::CURREC	= -2%
		SMG_WINDOW::NHELP	= "PP_MAIN_CARDEXEMPT"
		SMG_WINDOW::HSIZE	= 72%
		SMG_WINDOW::VSIZE	= 4%
		SMG_WINDOW::HPOS	= 4%
		SMG_WINDOW::VPOS	= 15%
		SMG_WINDOW::NITEMS	= 3%
		SMG_WINDOW::FLAGS	= 0%
		SMG_WINDOW::HVIEW	= 72%
		SMG_WINDOW::VVIEW	= 4%
		SMG_WINDOW::VHPOS	= 4%
		SMG_WINDOW::VVPOS	= 15%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%)	= "State"
			SMG_WINDOW::KFIELD(0%, 0%)	= 2%
			SMG_WINDOW::KFIELD(0%, 1%)	= 1%
			SMG_WINDOW::KFIELD(0%, 2%)	= 2%

		!
		! Load in defaults for chart
		!
		CALL READ_DEFAULTS(SMG_WINDOW)

700		!
		! Declare channels
		!
		IF PP_CARDEXEMPT.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PP_CARDEXEMPT.READONLY%

			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PP.OPEN]PP_CARDEXEMPT.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PP_MAIN_CARDEXEMPT = ERR
			CONTINUE 770
		END WHEN

		PP_CARDEXEMPT.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PP.OPEN]PP_CARDEXEMPT.OPN"
		USE
			PP_MAIN_CARDEXEMPT = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PP_CARDEXEMPT.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PP_CARDEXEMPT.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = PP_CARDEXEMPT.CH%
		WHEN ERROR IN
			RESET #PP_CARDEXEMPT.CH%
			GET #PP_CARDEXEMPT.CH%, REGARDLESS
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

		DATA	01,01, "(01) State", &
			02,01, "(02) Authority", &
			03,01, "(03) Product", &
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
	!	.x State
	!	^*(01) State\*
	!	.b
	!	.lm +5
	!	The ^*State\* field enters the state in which
	!	there are exemptions.  A state must be defined in the State
	!	Master File in order to be used.
	!	.b
	!	Valid state codes may be viewed by pressing ^*List Choices, F14\*.
	!	.b
	!	Two spaces are available for the entry.
	!	.lm +5
	!
	! Index:
	!
	!--
			PP_CARDEXEMPT::STATE = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"1;17", TEMP$, PP_CARDEXEMPT::STATE, MFLAG, &
				"'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF MAIN_WINDOW(UTL_MAIN_STATE.ID, "VX") = 1%
				THEN
					PP_CARDEXEMPT::STATE = &
						UTL_STATE::STATE
				END IF

				GOTO Reenter
			END IF

		CASE 2%
	!++
	! Abstract:FLD002
	!	.x Authority
	!	^*(02) Authority\*
	!	.b
	!	.lm +5
	!	The ^*Authority\* field enters the exempting
	!	authority.
	!	.b
	!	This field may contain any of the following:
	!	.table 3,25
	!	.te
	!	F	Federal
	!	.te
	!	S	State
	!	.te
	!	D	County
	!	.te
	!	C	City
	!	.te
	!	A	Sales Tax
	!	.end table
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_CARDEXEMPT::AUTHORITY = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"2;17", TEMP$, PP_CARDEXEMPT::AUTHORITY, MFLAG, &
				"'E", MVALUE)

		CASE 3%
	!++
	! Abstract:FLD003
	!	.x Product
	!	^*(03) Product\*
	!	.b
	!	.lm +5
	!	The ^*Product\* field enters the product number
	!	of the product that is exempt.  The product must be defined
	!	in the Product Master File in order to be used.
	!	.b
	!	Valid products may be viewed by pressing ^*List Choices, F14\*.
	!	.b
	!	This field will accept 14 alpha-numeric characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_CARDEXEMPT::PRODUCT = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"3;17", TEMP$, PP_CARDEXEMPT::PRODUCT, MFLAG, &
				"'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, "VX") = 1%
				THEN
					PP_CARDEXEMPT::PRODUCT = &
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

		PP_MAIN_CARDEXEMPT = 0%

		SELECT MLOOP

		CASE 1%
			!
			! Test PP_CARDEXEMPT product
			!
			PP_MAIN_CARDEXEMPT = FUNC_TESTENTRY(SMG_WINDOW, &
				"US" + PP_CARDEXEMPT::STATE, &
				UTL_STATE::DESCR, &
				"PP", MLOOP, "PROG", &
				"State", UTL_MAIN_STATE.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_STATE::DESCR, 1%, 33%, , SMG$M_BOLD)

		CASE 2%
			!
			! See if the card number already exists
			!
			IF MVALUE = "ADD"
			THEN
				WHEN ERROR IN
					GET #PP_CARDEXEMPT.CH%, &
						KEY #0% EQ PP_CARDEXEMPT::CUSNUM + &
						PP_CARDEXEMPT::CARD + &
						PP_CARDEXEMPT::STATE + &
						PP_CARDEXEMPT::AUTHORITY, REGARDLESS
				USE
					CONTINUE 32767 IF ERR = 155%
					EXIT HANDLER
				END WHEN

				PP_MAIN_CARDEXEMPT = 2%
				CALL ENTR_3MESSAGE(SCOPE, "Record Already Exists", 1%)
				GOTO 32767
			END IF

		CASE 3%
			!
			! Test PP_CARDEXEMPT product
			!
			PP_MAIN_CARDEXEMPT = FUNC_TESTENTRY(SMG_WINDOW, &
				PP_CARDEXEMPT::PRODUCT, &
				PD_PRODUCT::DESCRIPTION, &
				"PP", MLOOP, "PROG", &
				"Product", PD_MAIN_PRODUCT.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_PRODUCT::DESCRIPTION, 3%, 33%, , SMG$M_BOLD)

		END SELECT

	!
	! Display
	!
	CASE OPT_DISPLAY

		!
		! Display the descriptions
		!
		IF (SMG_WINDOW::HFLAG(1%) AND 2%) = 0%
		THEN
			UTL_STATE::DESCR = &
				STRING$(LEN(UTL_STATE::DESCR), A"?"B) &
				IF MAIN_WINDOW(UTL_MAIN_STATE.ID, &
				"Q0" + "US" + PP_CARDEXEMPT::STATE) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_STATE::DESCR, 1%, 33%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(3%) AND 2%) = 0%
		THEN
			PD_PRODUCT::DESCRIPTION = &
				STRING$(LEN(PD_PRODUCT::DESCRIPTION), A"?"B) &
				IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, &
				"Q0" + PP_CARDEXEMPT::PRODUCT) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_PRODUCT::DESCRIPTION, 3%, 33%, , SMG$M_BOLD)
		END IF

	!
	! Set PP_CARDEXEMPT_OLD value
	!
	CASE OPT_SETOLD
		PP_CARDEXEMPT_OLD = PP_CARDEXEMPT

	!
	! Restore PP_CARDEXEMPT_OLD value
	!
	CASE OPT_RESETOLD
		PP_CARDEXEMPT = PP_CARDEXEMPT_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PP_CARDEXEMPT_DEF = PP_CARDEXEMPT

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PP_CARDEXEMPT = PP_CARDEXEMPT_DEF

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!
		PP_CARDEXEMPT::CUSNUM  = LEFT(MVALUE, 10%)
		PP_CARDEXEMPT::CARD    = RIGHT(MVALUE, 11%)

	!
	! Find the next record
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE PP_CARDEXEMPT::CUSNUM + &
				PP_CARDEXEMPT::CARD + &
				PP_CARDEXEMPT::STATE + &
				PP_CARDEXEMPT::AUTHORITY, REGARDLESS

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
			MVALUE = "  State Authority Product        " + &
				"Description"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "008,018,033"

		!
		! Convert current record into text
		!
		CASE 3%
			PD_PRODUCT::DESCRIPTION = &
				STRING$(LEN(PD_PRODUCT::DESCRIPTION), A"?"B) &
				IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, &
				"Q0" + PP_CARDEXEMPT::PRODUCT) <> 1%

			MVALUE = PP_CARDEXEMPT::STATE    + "    "  + &
				PP_CARDEXEMPT::AUTHORITY + "     " + &
				PP_CARDEXEMPT::PRODUCT   + " "     + &
				PD_PRODUCT::DESCRIPTION

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
						PP_CARDEXEMPT::STATE + &
						PP_CARDEXEMPT::AUTHORITY, REGARDLESS
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

			IF PP_CARDEXEMPT::CUSNUM = LEFT(MVALUE, 10%) AND &
				PP_CARDEXEMPT::CARD = RIGHT(MVALUE, 11%)
			THEN
				SMG_WINDOW::CURREC = 0%
			END IF

		!
		! Change key
		!
		CASE 6%
			PP_CARDEXEMPT::CUSNUM = LEFT(MVALUE, 10%)
			PP_CARDEXEMPT::CARD   = RIGHT(MVALUE, 11%)

		END SELECT

	END SELECT

28000	EXIT FUNCTION

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
