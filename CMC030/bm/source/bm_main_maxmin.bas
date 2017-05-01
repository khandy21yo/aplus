1	%TITLE "MINMAX Table"
	%SBTTL "BM_MAIN_MAXMIN"
	%IDENT "V3.6a Calico"

	FUNCTION LONG BM_MAIN_MAXMIN(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 2000 BY
	!
	! Software Solutions, Inc.
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
	! Software Solutions, Inc.
	!
	! Software Solutions, Inc. assumes no responsibility for the use
	! or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	Maintains the minmax table which is used to generate
	!	special reports.
	!	.lm -5
	!
	! Index:
	!	.x Minmax>Maintain
	!	.x Maintenance>Minmax
	!
	! Option:
	!
	!
	! Input:
	!
	!
	! Output:
	!
	!
	! Example:
	!
	!
	! Compile:
	!
	!	$ BAS BM_SOURCE:BM_MAIN_MAXMIN/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP BM_MAIN_MAXMIN
	!	$ DELETE BM_MAIN_MAXMIN.OBJ;*
	!
	! Author:
	!
	!	06/02/2000 - Kevin Handy
	!
	! Modification history:
	!
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:BM_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[BM.OPEN]BM_MAXMIN.HB"
	MAP (BM_MAXMIN) BM_MAXMIN_CDD BM_MAXMIN
	MAP (BM_MAXMIN2) BM_MAXMIN_CDD BM_MAXMIN_OLD, BM_MAXMIN2

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_BM_MAXMIN) &
		BM_MAXMIN.CH%, &
		BM_MAXMIN.READONLY%

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
		SMG_WINDOW::DESCR = "AR Sales Tax Table Maintenance"
		SMG_WINDOW::NHELP = "BM_MAIN_MAXMIN"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 5%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 2%
		SMG_WINDOW::KNAME(0%) = "Product"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
			SMG_WINDOW::KFIELD(0%, 2%) = 2%
		SMG_WINDOW::KNAME(0%) = "Group"
			SMG_WINDOW::KFIELD(1%, 0%) = 1%
			SMG_WINDOW::KFIELD(1%, 1%) = 2%
			SMG_WINDOW::KFIELD(1%, 2%) = 1%

		!
		! Load in defaults
		!
		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF BM_MAXMIN.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF BM_MAXMIN.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[BM.OPEN]BM_MAXMIN.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			BM_MAIN_MAXMIN = ERR
			CONTINUE 770
		END WHEN

		BM_MAXMIN.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[BM.OPEN]BM_MAXMIN.OPN"
		USE
			BM_MAIN_MAXMIN = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		BM_MAXMIN.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(BM_MAXMIN.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = BM_MAXMIN.CH%

		WHEN ERROR IN
			RESET #BM_MAXMIN.CH%
			GET #BM_MAXMIN.CH%, REGARDLESS
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

		DATA	5, 1, "(01) Product", &
			7, 1, "(02) Group", &
			9, 1, "(03) Max", &
			11,1, "(04) Min", &
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

 E0Loop:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	.ts 55
	!	^*(01) Product	14 Characters\*
	!	.b
	!	.lm +5
	!	The ^*Product\* field specifies which inventory product
	!	this entry is defining.
	!	.lm -5
	!
	! Index:
	!	.x MinMax>Product
	!	.x Product>Minmax
	!
	!--

			BM_MAXMIN::PRODUCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "5;20", TEMP$, &
				BM_MAXMIN::PRODUCT, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, "VX") = 1%
				THEN
					BM_MAXMIN::PRODUCT = &
						PD_PRODUCT::PRODUCT_NUM
				END IF
				GOTO E0Loop
			END IF

		CASE 2%

	!++
	! Abstract:FLD002
	!	.ts 55
	!	^*(02) Group	2 Characters\*
	!	.b
	!	.lm +5
	!	This field is user defined, and is used to
	!	segment the report so that several different
	!	screnios can be defined at the same time.
	!	.lm -5
	!
	! Index:
	!	.x Sales Tax>State
	!	.x State>Sales Tax
	!
	!--

			BM_MAXMIN::MGROUP = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "7;20", TEMP$, &
				BM_MAXMIN::MGROUP, MFLAG, "'E", MVALUE)
		CASE 3%

	!++
	! Abstract:FLD003
	!	.ts 55
	!	^*(03) Maximum	Numeric\*
	!	.b
	!	.lm +5
	!	Specifies the maximum quantity that will be produced.
	!	.lm -5
	!
	! Index:
	!	.x Minmax>Maximum
	!	.x Maximum>Minmax
	!
	!--

			BM_MAXMIN::MAXQTY = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "9;20", TEMP$, &
				BM_MAXMIN::MAXQTY, MFLAG, "####.##", MVALUE)
		CASE 4%

	!++
	! Abstract:FLD004
	!	.ts 55
	!	^*(04) Minimum	Numeric\*
	!	.b
	!	.lm +5
	!	Specifies the minimum quantity that will be produced.
	!	.lm -5
	!
	! Index:
	!	.x Minimum>Minmax
	!	.x Minmax>Maximum
	!
	!--

			BM_MAXMIN::MINQTY = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "11;20", TEMP$, &
				BM_MAXMIN::MINQTY, MFLAG, "####.##", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY

		BM_MAIN_MAXMIN = 0%

		SELECT MLOOP

		CASE 1%
			IF BM_MAXMIN::PRODUCT = ""
			THEN
				BM_MAIN_MAXMIN = 1%
			ELSE
				!
				! Is the input defined?
				!
				BM_MAIN_MAXMIN = FUNC_TESTENTRY(SMG_WINDOW, &
					BM_MAXMIN::PRODUCT, &
					PD_PRODUCT::DESCRIPTION, &
					"IC", MLOOP, "PROD", &
					"Product", PD_MAIN_PRODUCT.ID)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(PD_PRODUCT::DESCRIPTION, 37%), &
				5%, 41%, , SMG$M_BOLD)

		END SELECT


	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(2%) AND 2%) = 0%
		THEN
			PRODDESC$ = STRING$(40%, 63%)
			PRODDESC$ = PD_PRODUCT::DESCRIPTION &
				IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, &
				"Q0" + BM_MAXMIN::PRODUCT) = 1%

			!
			! Display name (ALSO IN TESTENTRY)
			!
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(PRODDESC$, 37%), &
				5%, 41%, , SMG$M_BOLD)
		END IF


	!
	! Set BM_MAXMIN_OLD value
	!
20500	CASE OPT_SETOLD

		BM_MAXMIN_OLD = BM_MAXMIN

	!
	! Restore BM_MAXMIN_OLD value
	!
	CASE OPT_RESETOLD

		BM_MAXMIN = BM_MAXMIN_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT

		BM_MAXMIN2 = BM_MAXMIN

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT

		BM_MAXMIN = BM_MAXMIN2

	!
	! View header
	!
	CASE OPT_VIEW

		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "   Product              Group           " + &
				"Max          Min"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "024,032,042"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = "  " + BM_MAXMIN::PRODUCT + "      " + &
				BM_MAXMIN::MGROUP + "       " + &
				FORMAT$(BM_MAXMIN::MAXQTY, "#####.##       ") + &
				FORMAT$(BM_MAXMIN::MINQTY, "#####.##")

		END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP
		CASE 0%
			FIND #BM_MAXMIN.CH%, &
				KEY #0% GE BM_MAXMIN::PRODUCT + &
				BM_MAXMIN::MGROUP, &
				REGARDLESS
		END SELECT
	END SELECT

 ExitFunction:
	EXIT FUNCTION

29000	!*****************************************************************
	! Trap Errors
	!*****************************************************************

	RESUME ExitFunction

32767	END FUNCTION
