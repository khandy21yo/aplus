1	%TITLE "Inventory Count Entry Journal"
	%SBTTL "IC_MAIN_JOURCOUNT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG IC_MAIN_JOURCOUNT(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	This program maintains the Inventory count entry journal.
	!	.lm -5
	!
	! Index:
	!	.x Cycle Count>Journal
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_MAIN_JOURCOUNT/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP IC_MAIN_JOURCOUNT
	!	$ DELETE IC_MAIN_JOURCOUNT.OBJ;*
	!
	! Author:
	!
	!	07/31/88 - Frank Starman
	!
	! Modification history:
	!
	!	02/04/92 - Dan Perkins
	!		Adjusted field size to handle quantities up to
	!		nine million.
	!
	!	02/25/92 - Kevin Handy
	!		Removed references to function PD_MAIN_PACK which
	!		Frank deleted, but did not fix references so that
	!		programs couldn't compile.
	!
	!	04/06/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	04/24/92 - Kevin Handy
	!		Clean up (check)
	!
	!	03/31/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/12/94 - Kevin Handy
	!		Increased record limits from 1000 to 1500.
	!
	!	02/02/95 - Kevin Handy
	!		Modified to handle defining number format
	!		from the set file (copied stuff from
	!		PS_MAIN_TICKETLINE)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	01/08/96 - Kevin Handy
	!		Increased dimension from 1500 to 3000.
	!		Reformat source closer to 80 columns.
	!
	!	10/18/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/27/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/20/98 - Kevin Handy
	!		Lose SLINE field in the array.
	!		Changes for new CONTROL and new keys.
	!		Increase array size to 6000.
	!
	!	03/08/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	10/28/2000 - Kevin Handy
	!		Increase linit from 16000 to 20000 (LL)
	!
	!	12/14/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:IC_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[IC.OPEN]IC_JOURCOUNT.HB"
	MAP (IC_JOURCOUNT)	IC_JOURCOUNT_CDD	IC_JOURCOUNT
	MAP (IC_JOURCOUNT_OLD)	IC_JOURCOUNT_CDD	IC_JOURCOUNT_OLD, &
							IC_JOURCOUNT2, &
							IC_JOURCOUNT3

	%INCLUDE "SOURCE:[IC.OPEN]IC_CYCLEJOUR.HB"
	MAP (IC_CYCLEJOUR)	IC_CYCLEJOUR_CDD	IC_CYCLEJOUR

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (BATCH_IC_CYCLEJOUR) &
		BATCH_NO$ = 2%

	COM (CH_IC_JOURCOUNT) &
		IC_JOURCOUNT.CH%, &
		IC_JOURCOUNT.READONLY%

	COM (IC_MAIN_JOURCOUNT_FRM) FRM$(2%)

	!
	! Create array to contain pointers and totals
	!
	RECORD RARRAY_RECORD
		RFA	LINRFA		! Rfa pointer for record
	END RECORD

	MAP (TT_IC_JOURCOUNT)	RARRAY_RECORD RARRAY(20000%)

	!
	! External functions
	!
	EXTERNAL LONG		FUNCTION MAIN_WINDOW
	EXTERNAL LONG		FUNCTION FUNC_TESTENTRY

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
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Product Cycle Count"
		SMG_WINDOW::NHELP = "IC_MAIN_JOURCOUNT"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 12%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 8%
		SMG_WINDOW::NITEMS= 3%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::TOPLIN = 3%
		SMG_WINDOW::BOTLIN = 11%

		!
		! Load in defaults
		!
		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE)

700		!
		! Declare channels
		!
		IF IC_JOURCOUNT.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF IC_JOURCOUNT.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[IC.OPEN]IC_JOURCOUNT.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			IC_MAIN_JOURCOUNT = ERR
			CONTINUE 770
		END WHEN

		IC_JOURCOUNT.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[IC.OPEN]IC_JOURCOUNT.OPN"
		USE
			IC_MAIN_JOURCOUNT = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		IC_JOURCOUNT.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(IC_JOURCOUNT.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = IC_JOURCOUNT.CH%
		WHEN ERROR IN
			RESET #IC_JOURCOUNT.CH%
			GET #IC_JOURCOUNT.CH%, REGARDLESS
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

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"  (01)                             (02)" + &
			"        (03)                               ", &
			1%, 1%, , SMG$M_REVERSE)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"  Product#                   QtyCounted" + &
			"        Control                            ", &
			2%, 1%, , SMG$M_REVERSE)

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

	!
	! Extra display stuff
	!
	CASE OPT_DISPLAY

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		!
		! Display totals
		!
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"Number of lines" + &
			FORMAT$(SMG_WINDOW::TOTREC, "#####") + &
			"                                             " + &
			"               ", &
			SMG_WINDOW::VSIZE, 1%, , SMG$M_REVERSE)

		!
		! Paint lines on screen
		!
		FOR I% = 1% TO 2%
			A% = VAL%(MID("017,040", &
				I% * 4% - 3%, 3%))

			SMG_STATUS% = SMG$DRAW_LINE(SMG_WINDOW::WNUMBER, &
				1%, A%, SMG_WINDOW::BOTLIN, A%)
		NEXT I%

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

		XLINE$ = NUM1$(SMG_WINDOW::CURLIN)

 Reentry:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Product _#\*
	!	.b
	!	.lm +5
	!	The ^*Product _#\* field enters an assigned
	!	number which identifies a specific product.
	!	.b
	!	The field will accommodate up to fourteen (14) alphanumeric
	!	characters.
	!	.b
	!	Valid Product _#'s may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x Product Number>Cycle Count
	!
	!--

			IC_JOURCOUNT::PRODUCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				XLINE$ + ";3", TEMP$, &
				IC_JOURCOUNT::PRODUCT, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, "VX") = 1%
				THEN
					IC_JOURCOUNT::PRODUCT = &
						PD_PRODUCT::PRODUCT_NUM
				END IF
				GOTO Reentry
			END IF

			IF TEMP$ = "Initialize" OR TEMP$ = "Change"
			THEN
				PRODUCT_FACTOR = 1.
			END IF

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Quantity Counted\*
	!	.b
	!	.lm +5
	!	The ^*Quantity Counted\* field enters
	!	the quantity of units contained in a particular pack, based
	!	on the unit of measure. A Unit refers to the lowest common
	!	or utilized in the user's end product,
	!	i.e. "each", "gram", "millimeter", etc.
	!	.lm -5
	!
	! Index:
	!	.x Quantity per Unit>Cycle Count
	!
	!--

			IC_JOURCOUNT::QUANTITY = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				XLINE$ + ";25",TEMP$, &
				IC_JOURCOUNT::QUANTITY, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(02) Control Number\*
	!	.b
	!	.lm +5
	!	.lm -5
	!
	! Index:
	!	.x Control>Cycle Count
	!
	!--

			IC_JOURCOUNT::CONTROL = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				XLINE$ + ";50",TEMP$, &
				IC_JOURCOUNT::CONTROL, MFLAG, &
				"'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY

		IC_MAIN_JOURCOUNT = 0%

		SELECT MLOOP

		CASE 1%
			IF IC_JOURCOUNT::PRODUCT = ""
			THEN
				IC_MAIN_JOURCOUNT = 1%
			ELSE
				!
				! Is the input defined?
				!
				IC_MAIN_JOURCOUNT = FUNC_TESTENTRY(SMG_WINDOW, &
					IC_JOURCOUNT::PRODUCT, &
					PD_PRODUCT::DESCRIPTION, &
					"IC", MLOOP, "PROD", &
					"Product", PD_MAIN_PRODUCT.ID)
			END IF

		END SELECT

	! Set IC_JOURCOUNT_OLD value
	!
20500	CASE OPT_SETOLD
		IC_JOURCOUNT_OLD = IC_JOURCOUNT

	!
	! Restore IC_JOURCOUNT_OLD value
	!
	CASE OPT_RESETOLD
		IC_JOURCOUNT = IC_JOURCOUNT_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		IC_JOURCOUNT2 = IC_JOURCOUNT

		IF MFLAG = 1%
		THEN
			SELECT MLOOP

			CASE 0%
				FRM$(2%)  = "#,###,###"
			CASE ELSE
				FRM$(MLOOP) = MVALUE
			END SELECT
		END IF

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		IC_JOURCOUNT = IC_JOURCOUNT2

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!

		IC_JOURCOUNT::LOCATION = IC_CYCLEJOUR::LOCATION

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #IC_JOURCOUNT.CH%, &
				KEY #0% GE IC_JOURCOUNT::LOCATION + "", &
				REGARDLESS
		END SELECT

	!
	! Handle array of records
	!
27000	CASE OPT_ARRAY

		!
		! Select sub-option of array
		!
		SELECT MLOOP

		!
		! Load array with line items
		!
		CASE 1%

			!
			! Empty array
			!
			SMG_WINDOW::TOTREC = 0%

27110			!
			! Search for first record
			!
			WHEN ERROR IN
				FIND #SMG_WINDOW::CHAN, &
					KEY #0% GE IC_CYCLEJOUR::LOCATION + "", &
					REGARDLESS
			USE
				CONTINUE 28000
			END WHEN

27120			!
			! Get a record
			!
			WHEN ERROR IN
				GET #SMG_WINDOW::CHAN
			USE
				CONTINUE 28000 IF ERR = 11%
				EXIT HANDLER
			END WHEN

			IF IC_JOURCOUNT::LOCATION = IC_CYCLEJOUR::LOCATION
			THEN
				!
				! Add information to array
				!
				SMG_WINDOW::TOTREC = SMG_WINDOW::TOTREC + 1%
				RARRAY(SMG_WINDOW::TOTREC)::LINRFA = &
					GETRFA(SMG_WINDOW::CHAN)
				GOTO 27120
			END IF

		!
		! Remove one element of the array
		!
		CASE 2%
			!
			! Remove item pointed to by Mflag
			!
			FOR I% = MFLAG TO SMG_WINDOW::TOTREC - 1%
				RARRAY(I%) = RARRAY(I% + 1%)
			NEXT I%

		!
		! Set array item to current record
		!
		CASE 3%
			RARRAY(MFLAG)::LINRFA = GETRFA(SMG_WINDOW::CHAN)


		!
		! Load in current record, locked
		!
		CASE 4%
27200			GET #SMG_WINDOW::CHAN, RFA RARRAY(MFLAG)::LINRFA

		!
		! Load in current record, unlocked
		!
		CASE 5%
			GET #SMG_WINDOW::CHAN, RFA RARRAY(MFLAG)::LINRFA, &
				REGARDLESS

		!
		! Change the current record's key to match header.  The
		! new key probably passes through MVALUE, unless some
		! other means is devised.
		!
		CASE 6%
			IC_JOURCOUNT::LOCATION  = MID(MVALUE, 2%, 4%)
		!
		! Print descriptions in journal window.
		!
		CASE 7%
			! Not right now

		END SELECT
	END SELECT

28000	EXIT FUNCTION

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
