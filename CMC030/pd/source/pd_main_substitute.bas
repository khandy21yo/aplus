1	%TITLE "Maintain List of Substitute Part Numbers"
	%SBTTL "PD_MAIN_SUBSTITUTE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PD_MAIN_SUBSTITUTE(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1994 BY
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
	!	.B
	!	.LM +5
	!	This program maintains the substitute part number file.
	!	.LM -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PD_SOURCE:PD_MAIN_SUBSTITUTE/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN PD_MAIN_SUBSTITUTE
	!	$ DELETE PD_MAIN_SUBSTITUTE.OBJ;*
	!
	! Author:
	!
	!	05/27/94 - Kevin Handy
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/20/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/28/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/08/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	11/27/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	04/04/2002 - Kevin Handy
	!		Fix test for blank substitute number
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:AP_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	!
	! Map's
	!
	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP (AP_VENDOR)		AP_VENDOR_CDD		AP_VENDOR

	%INCLUDE "SOURCE:[PD.OPEN]PD_SUBSTITUTE.HB"
	MAP (PD_SUBSTITUTE)	PD_SUBSTITUTE_CDD	PD_SUBSTITUTE
	MAP (PD_SUBSTITUTE_OLD) PD_SUBSTITUTE_CDD PD_SUBSTITUTE_OLD, &
		PD_SUBSTITUTE2

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD	PD_PRODUCT

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_PD_SUBSTITUTE) &
		PD_SUBSTITUTE.CH%, &
		PD_SUBSTITUTE.READONLY%

	!
	! Create array to contain pointers and totals
	!
	RECORD RARRAY_RECORD
		RFA	LINRFA		! Rfa pointer for record
	END RECORD

	MAP (TT_PD_SUBSTITUTE) RARRAY_RECORD RARRAY(300%)

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION MAIN_WINDOW
	EXTERNAL LONG   FUNCTION FUNC_TESTENTRY

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
		SMG_WINDOW::DESCR = "Substitute Part Numbers"
		SMG_WINDOW::NHELP = "PD_MAIN_SUBSTITUTE"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 9%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 9%
		SMG_WINDOW::NITEMS= 2%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::TOPLIN = 3%
		SMG_WINDOW::BOTLIN = 8%

		!
		! Load in defaults for CONTACT
		!
		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF PD_SUBSTITUTE.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PD_SUBSTITUTE.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PD.OPEN]PD_SUBSTITUTE.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PD_MAIN_SUBSTITUTE = ERR
			CONTINUE 770
		END WHEN

		PD_SUBSTITUTE.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PD.OPEN]PD_SUBSTITUTE.OPN"
		USE
			PD_MAIN_SUBSTITUTE = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PD_SUBSTITUTE.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PD_SUBSTITUTE.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = PD_SUBSTITUTE.CH%
		WHEN ERROR IN
			RESET #PD_SUBSTITUTE.CH%
			GET #PD_SUBSTITUTE.CH%, REGARDLESS
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
			"  (01)" + SPACE$(29%) + "(02)" + SPACE$(17%) + &
			"    " + SPACE$(18%), &
			1%, 1%, , SMG$M_REVERSE)
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"  Their Product                    Vendor        " + &
			"                             ", &
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
			FORMAT$(SMG_WINDOW::TOTREC, "###") + SPACE$(60%), &
			SMG_WINDOW::VSIZE, 1%, , SMG$M_REVERSE)

		!
		! Paint lines on screen
		!
		FOR I% = 1% TO 1%

			A% = VAL%(MID("034", I% * 4% - 3%, 3%))

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

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

		XLINE$ = NUM1$(SMG_WINDOW::CURLIN)

 E0Loop:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%
	!++
	! Abstract:FLD001
	!	^*(01) Their Product\*
	!	.b
	!	Usually the vendors product number, the barcode number, etc.
	!
	! Index:
	!
	!--
			PD_SUBSTITUTE::THEIR_PRODUCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";3", TEMP$, &
				PD_SUBSTITUTE::THEIR_PRODUCT, MFLAG, &
				"'E", MVALUE)

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Vendor\*
	!
	! Index:
	!
	!--
			PD_SUBSTITUTE::VENDOR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";36", TEMP$, &
				PD_SUBSTITUTE::VENDOR, MFLAG, &
				"'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT
			CASE SMG$K_TRM_F14
				IF MAIN_WINDOW(AP_MAIN_VENDOR.ID, "VX  ") = 1%
				THEN
					PD_SUBSTITUTE::VENDOR = &
						AP_VENDOR::VENNUM
				END IF
				GOTO E0Loop

			CASE SMG$K_TRM_F17
				V% = MAIN_WINDOW(AP_MAIN_VENDOR.ID, "M")
				PD_SUBSTITUTE::VENDOR = AP_VENDOR::VENNUM
				GOTO E0Loop
			END SELECT
		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		PD_MAIN_SUBSTITUTE = 0%

		SELECT MLOOP

		CASE 1%
			IF PD_SUBSTITUTE::THEIR_PRODUCT = ""
			THEN
				PD_MAIN_SUBSTITUTE = 1%
			END IF

		CASE 2%
			IF PD_SUBSTITUTE::VENDOR <> ""
			THEN
				PD_MAIN_SUBSTITUTE = &
					FUNC_TESTENTRY(SMG_WINDOW, &
					PD_SUBSTITUTE::VENDOR, &
					AP_VENDOR::VENNAM, &
					"PD", MLOOP, "VENDOR NAME", &
					"Vendor Number", AP_MAIN_VENDOR.ID)
			END IF

		END SELECT

	!
	! Set PD_SUBSTITUTE_OLD value
	!
20500	CASE OPT_SETOLD
		PD_SUBSTITUTE_OLD = PD_SUBSTITUTE

	!
	! Restore PD_SUBSTITUTE_OLD value
	!
	CASE OPT_RESETOLD
		PD_SUBSTITUTE = PD_SUBSTITUTE_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PD_SUBSTITUTE2 = PD_SUBSTITUTE

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PD_SUBSTITUTE = PD_SUBSTITUTE2

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!
		PD_SUBSTITUTE::OUR_PRODUCT = PD_PRODUCT::PRODUCT_NUM

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #PD_SUBSTITUTE.CH%, &
				KEY #0% GE PD_PRODUCT::PRODUCT_NUM + "", &
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
					KEY #0% GE PD_PRODUCT::PRODUCT_NUM + "", &
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

			!
			! Add information to array
			!
			IF (PD_PRODUCT::PRODUCT_NUM = &
				PD_SUBSTITUTE::OUR_PRODUCT)
			THEN
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
		! new key is probibly passes through MVALUE, unless some
		! other means is devised.
		!
		CASE 6%
			PD_SUBSTITUTE::OUR_PRODUCT = MID(MVALUE, 2%, 10%)

		END SELECT
	END SELECT

28000	EXIT FUNCTION

	%PAGE

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
