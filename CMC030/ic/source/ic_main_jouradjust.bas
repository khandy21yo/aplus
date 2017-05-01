1	%TITLE "Inventory Adjust Journal"
	%SBTTL "IC_MAIN_JOURADJUST"
	%IDENT "V3.6a Calico"

	FUNCTION LONG IC_MAIN_JOURADJUST(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	This program maintains the Inventory adjust journal
	!	.lm -5
	!
	! Index:
	!	.x Inventory Adjust Journal
	!	.x Journal>Inventory Adjust
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_MAIN_JOURADJUST/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP IC_MAIN_JOURADJUST
	!	$ DELETE IC_MAIN_JOURADJUST.OBJ;*
	!
	! Author:
	!
	!	07/31/88 - Frank Starman
	!
	! Modification history:
	!
	!	04/06/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to text input.
	!
	!	04/24/92 - Kevin Handy
	!		Clean up (check)
	!
	!	10/26/92 - Frank Starman
	!		Added IC_WRIT_35BALANCE
	!
	!	03/02/93 - Dan Perkins
	!		Changed "V0" to "VX" on chart of accounts to be
	!		able to list accounts starting at a particular
	!		account.
	!
	!	03/31/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
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
	!		Lose unused field SLINE in local array, not used.
	!		Change array size from GL_MAIN_CHART.ID to 6000.
	!
	!	03/08/99 - Kevin Handy
	!		Fix FIND bug
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

	%INCLUDE "FUNC_INCLUDE:IC_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[IC.OPEN]IC_JOURADJUST.HB"
	MAP (IC_JOURADJUST)	IC_JOURADJUST_CDD	IC_JOURADJUST
	MAP (IC_JOURADJUST_OLD)	IC_JOURADJUST_CDD	IC_JOURADJUST_OLD, &
							IC_JOURADJUST2

	%INCLUDE "SOURCE:[IC.OPEN]IC_CYCLEJOUR.HB"
	MAP (IC_CYCLEJOUR)	IC_CYCLEJOUR_CDD	IC_CYCLEJOUR

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD		GL_CHART

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (BATCH_IC_CYCLEJOUR) &
		BATCH_NO$ = 2%

	COM (CH_IC_JOURADJUST) &
		IC_JOURADJUST.CH%, &
		IC_JOURADJUST.READONLY%

	!
	! Create array to contain pointers and totals
	!
	RECORD RARRAY_RECORD
		RFA	LINRFA		! Rfa pointer for record
		STRING	SLINE = 4%	! Line number
	END RECORD

	MAP (TT_IC_JOURADJUST)	RARRAY_RECORD RARRAY(16000%)

	!
	! External functions
	!
	EXTERNAL LONG		FUNCTION FUNC_TESTENTRY
	EXTERNAL LONG		FUNCTION IC_WRIT_35BALANCE

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
		SMG_WINDOW::DESCR = "Inventory Adjustment"
		SMG_WINDOW::NHELP = "IC_MAIN_JOURADJUST"
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
		IF IC_JOURADJUST.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF IC_JOURADJUST.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[IC.OPEN]IC_JOURADJUST.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			IC_MAIN_JOURADJUST = ERR
			CONTINUE 770
		END WHEN

		IC_JOURADJUST.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[IC.OPEN]IC_JOURADJUST.OPN"
		USE
			IC_MAIN_JOURADJUST = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		IC_JOURADJUST.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(IC_JOURADJUST.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = IC_JOURADJUST.CH%
		WHEN ERROR IN
			RESET #IC_JOURADJUST.CH%
			GET #IC_JOURADJUST.CH%, REGARDLESS
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
			"  (01)                     (02) (03)              " + &
			" Description                   ", &
			1%, 1%, , SMG$M_REVERSE)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"  Product#           QtyPerUnit Account#" + &
			"                                       ", &
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
			FORMAT$(SMG_WINDOW::TOTREC, "####") + &
			"                                             " + &
			"               ", &
			SMG_WINDOW::VSIZE, 1%, , SMG$M_REVERSE)

		!
		! Paint lines on screen
		!
		FOR I% = 1% TO 3%
			A% = VAL%(MID("017,032,051", &
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
	!	Valid Product numbers may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x Product Number>Inventory Adjust Journal
	!	.x Inventory Adjust Journal>Product Number
	!	.x Number>Product
	!
	!--

			IC_JOURADJUST::PRODUCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				XLINE$ + ";3", TEMP$, &
				IC_JOURADJUST::PRODUCT, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, "VX") = 1%
				THEN
				IC_JOURADJUST::PRODUCT = &
						PD_PRODUCT::PRODUCT_NUM
				END IF
				GOTO Reentry
			END IF

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Quantity per Unit\*
	!	.b
	!	.lm +5
	!	The ^*Quantity per Unit\* field enters
	!	the quantity of units contained in a particular pack, based
	!	on the unit of measure. A Unit refers to the lowest common
	!	unit in which a product is measured or utilized in the user's end
	!	product, i.e. "each", "gram", "millimeter", etc.
	!	.lm -5
	!
	! Index:
	!	.x Quantity per Unit
	!	.x Inventory Adjust Journal>Quantity per Unit
	!
	!--

			IC_JOURADJUST::QUANTITY = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				XLINE$ + ";18",TEMP$, &
				IC_JOURADJUST::QUANTITY, MFLAG, &
				"##,###,###.###", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Account\*
	!	.b
	!	.lm +5
	!	The ^*Account\* field enters the General Ledger
	!	Chart of Account number which this entry has reference to.
	!	.b
	!	If a valid Chart of Accounts number is entered, the description
	!	field will automatically contain the description for that
	!	particular number. If an invalid number is entered, a message
	!	will appear indicating ^*"Input is undefined, enter anyway
	!	(Y/N)\*.
	!	.b
	!	Valid General Ledger Chart of Account numbers may be viewed by
	!	pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x Account>Inventory Adjust Journal
	!	.x Inventory Adjust Journal>Account
	!
	!--

			IC_JOURADJUST::ACCOUNT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				XLINE$ + ";33", TEMP$, &
				IC_JOURADJUST::ACCOUNT, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%
				THEN
					IC_JOURADJUST::ACCOUNT = &
						GL_CHART::ACCT
				END IF
				GOTO Reentry
			END IF

			IF IC_JOURADJUST::ACCOUNT <> ""
			THEN
				ACCTDESCR$ = &
					STRING$(LEN(GL_CHART::DESCR), A"?"B)
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, &
					"Q0" + IC_JOURADJUST::ACCOUNT) = 1%
				THEN
					ACCTDESCR$ = GL_CHART::DESCR
				END IF

				ACCTDESCR$ = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
					XLINE$ + ";52", TEMP$, &
					LEFT(ACCTDESCR$, 28%), 1%, "'E", &
					MVALUE)
			END IF

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY

		IC_MAIN_JOURADJUST = 0%

		SELECT MLOOP

		CASE 1%
			IF IC_JOURADJUST::PRODUCT = ""
			THEN
				IC_MAIN_JOURADJUST = 1%
			ELSE
				!
				! Is the input defined?
				!
				IC_MAIN_JOURADJUST = FUNC_TESTENTRY(SMG_WINDOW, &
					IC_JOURADJUST::PRODUCT, &
					PD_PRODUCT::DESCRIPTION, &
					"IC", MLOOP, "PROD", &
					"Product", PD_MAIN_PRODUCT.ID)
			END IF

		CASE 3%
			IF IC_JOURADJUST::ACCOUNT <> ""
			THEN
				!
				! Is the input defined?
				!
				IC_MAIN_JOURADJUST = FUNC_TESTENTRY(SMG_WINDOW, &
					IC_JOURADJUST::ACCOUNT, GL_CHART::DESCR, &
					"IC", MLOOP, "PROD", &
					"Account", GL_MAIN_CHART.ID)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(GL_CHART::DESCR, 28%), &
				SMG_WINDOW::CURLIN, 52%, , SMG$M_BOLD)

		END SELECT

	! Set IC_JOURADJUST_OLD value
	!
20500	CASE OPT_SETOLD
		IC_JOURADJUST_OLD = IC_JOURADJUST

	!
	! Restore IC_JOURADJUST_OLD value
	!
	CASE OPT_RESETOLD
		IC_JOURADJUST = IC_JOURADJUST_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		IC_JOURADJUST2 = IC_JOURADJUST

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		IC_JOURADJUST = IC_JOURADJUST2

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!
		IC_JOURADJUST::LOCATION = IC_CYCLEJOUR::LOCATION

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #IC_JOURADJUST.CH%, &
				KEY #0% GE IC_JOURADJUST::LOCATION + "", &
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

			IF IC_JOURADJUST::LOCATION = IC_CYCLEJOUR::LOCATION
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
			IC_JOURADJUST::LOCATION  = MID(MVALUE, 2%, 4%)
		!
		! Print descriptions in journal window.
		!
		CASE 7%
			! Not right now

		END SELECT

	CASE OPT_AFTEROPT

		SELECT MVALUE

		CASE "Add"

			V% = IC_WRIT_35BALANCE (IC_JOURADJUST::PRODUCT, &
				IC_JOURADJUST::LOCATION, &
				IC_CYCLEJOUR::TRANSTYPE, &
				IC_JOURADJUST::QUANTITY)


		CASE "Change", "Blank", "Initialize"

			V% = IC_WRIT_35BALANCE (IC_JOURADJUST_OLD::PRODUCT, &
				IC_JOURADJUST_OLD::LOCATION, &
				IC_CYCLEJOUR::TRANSTYPE, &
				-IC_JOURADJUST_OLD::QUANTITY)

			V% = IC_WRIT_35BALANCE (IC_JOURADJUST::PRODUCT, &
				IC_JOURADJUST::LOCATION, &
				IC_CYCLEJOUR::TRANSTYPE, &
				IC_JOURADJUST::QUANTITY)

		CASE "Erase"

			V% = IC_WRIT_35BALANCE (IC_JOURADJUST::PRODUCT, &
				IC_JOURADJUST::LOCATION, &
				IC_CYCLEJOUR::TRANSTYPE, &
				-IC_JOURADJUST::QUANTITY)

		END SELECT

	END SELECT

28000	EXIT FUNCTION

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
