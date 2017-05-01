1	%TITLE "Pacific Pride Site Product Maintenance"
	%SBTTL "PP_MAIN_SITE_PRODUCT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PP_MAIN_SITE_PRODUCT(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	This function maintains the Pacific Pride Site Product File.
	!	Data in this file includes the product number, federal, state,
	!	county, city, and salestax INTP, and federal, state, county,
	!	city, and salestax general ledger account numbers.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PP_SOURCE:PP_MAIN_SITE_PRODUCT/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN PP_MAIN_SITE_PRODUCT
	!	$ DELETE PP_MAIN_SITE_PRODUCT.OBJ;*
	!
	! Author:
	!
	!	12/29/92 - Dan Perkins
	!
	! Modification history:
	!
	!	02/02/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	02/02/93 - Kevin Handy
	!		Added line number 20300 so error trapping would
	!		make sense.
	!
	!	06/28/93 - Kevin Handy
	!		Adjust position of account number on screen so
	!		it doesn't overlap rate.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/09/96 - Kevin Handy
	!		Reformat source code.
	!
	!	04/21/97 - Kevin Handy
	!		Fix a bug with the values passed through MVALUE.
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

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:PP_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[PP.OPEN]PP_SITE_PRODUCT.HB"
	MAP (PP_SITE_PRODUCT)	PP_SITE_PRODUCT_CDD	PP_SITE_PRODUCT
	MAP (PP_SITE_PRODUCT_OLD) PP_SITE_PRODUCT_CDD	PP_SITE_PRODUCT_OLD, &
							PP_SITE_PRODUCT2

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD		GL_CHART

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION MAIN_WINDOW
	EXTERNAL LONG	FUNCTION FUNC_TESTENTRY

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_PP_SITE_PRODUCT) &
		PP_SITE_PRODUCT.CH%, &
		PP_SITE_PRODUCT.READONLY%

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
		SMG_WINDOW::DESCR  = "Pacific Pride Site Product Maintenance"
		SMG_WINDOW::CURREC = -2%
		SMG_WINDOW::NHELP  = "PP_MAIN_SITE_PRODUCT"
		SMG_WINDOW::HSIZE  = 76%
		SMG_WINDOW::VSIZE  = 10%
		SMG_WINDOW::HPOS   = 3%
		SMG_WINDOW::VPOS   = 8%
		SMG_WINDOW::FLAGS  = 0%
		SMG_WINDOW::NITEMS = 16%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Product"
			SMG_WINDOW::KFIELD(0%, 0%)	= 1%
			SMG_WINDOW::KFIELD(0%, 1%)	= 1%

		SMG_WINDOW::HVIEW	= 128%
		SMG_WINDOW::VVIEW	= 10%
		SMG_WINDOW::VHPOS	= 3%
		SMG_WINDOW::VVPOS	= 8%

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF PP_SITE_PRODUCT.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PP_SITE_PRODUCT.READONLY%

			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PP.OPEN]PP_SITE_PRODUCT.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PP_MAIN_SITE_PRODUCT = ERR
			CONTINUE 770
		END WHEN

		PP_SITE_PRODUCT.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PP.OPEN]PP_SITE_PRODUCT.OPN"
		USE
			PP_MAIN_SITE_PRODUCT = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PP_SITE_PRODUCT.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PP_SITE_PRODUCT.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = PP_SITE_PRODUCT.CH%
		WHEN ERROR IN
			RESET #PP_SITE_PRODUCT.CH%
			GET #PP_SITE_PRODUCT.CH%, REGARDLESS
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

		DATA	02,01, "(01) Product", &
			05,05, "(02)", &
			05,15, "(03)", &
			05,29, "(04)", &
			06,05, "(05)", &
			06,15, "(06)", &
			06,29, "(07)", &
			07,05, "(08)", &
			07,15, "(09)", &
			07,29, "(10)", &
			08,05, "(11)", &
			08,15, "(12)", &
			08,29, "(13)", &
			09,05, "(14)", &
			09,15, "(15)", &
			09,29, "(16)", &
			04,01, "         INTP       Rate          Account", &
			05,01, "Fed", &
			06,01, "Sta", &
			07,01, "Cou", &
			08,01, "Cty", &
			09,01, "Sal", &
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
	!	^*(01) Product\*
	!	.b
	!	.lm +5
	!	The ^*Product\* field is provided to enter an
	!	assigned number which identifies a specific product.
	!	The product must be defined in the Product Master File
	!	in order to be used.
	!	.b
	!	Valid Product Numbers may be viewed by pressing ^*List Choices, F14\*.
	!	.b
	!	This field will accept 14 alpha-numeric numbers.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_SITE_PRODUCT::PRODUCT = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"2;15", TEMP$, PP_SITE_PRODUCT::PRODUCT, &
				MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, "VX") = 1%
				THEN
					PP_SITE_PRODUCT::PRODUCT = &
						PD_PRODUCT::PRODUCT_NUM
				END IF

				GOTO Reenter
			END IF

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Federal INTP\*
	!	.b
	!	.lm +5
	!	The ^*Federal INTP\* field enters
	!	a code for the product which is entered in field (01).
	!	A ^*Y\* or ^*N\* is the only response allowed.
	!	.b
	!	Valid responses may be viewed by pressing ^*List Choices, F14\*.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_SITE_PRODUCT::FED_INTP = &
				ENTR_3YESNO(SCOPE, SMG_WINDOW::WNUMBER, &
				"5;10", TEMP$, PP_SITE_PRODUCT::FED_INTP, &
				MFLAG, "'E", MVALUE)

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Federal Rate\*
	!	.b
	!	.lm +5
	!	The ^*Federal Rate\* field contains the Federal Tax Rate
	!	assigned to the product.  The rate is expressed in a percentage.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_SITE_PRODUCT::FED_RATE = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"5;20", TEMP$, PP_SITE_PRODUCT::FED_RATE, &
				MFLAG, "##.#####", MVALUE)

		CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Federal Account\*
	!	.b
	!	.lm +5
	!	The ^*Federal Account\* field refers to the General Ledger Account
	!	to which the rate will be assigned.
	!	.b
	!	Valid GL accounts may be viewed by pressing ^*List Choices, F14\*.
	!	.b
	!	This field will accept 18 alpha-numeric characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_SITE_PRODUCT::FED_ACCOUNT = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"5;35", TEMP$, PP_SITE_PRODUCT::FED_ACCOUNT, &
				MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%
				THEN
					PP_SITE_PRODUCT::FED_ACCOUNT = &
						GL_CHART::ACCT
				END IF

				GOTO Reenter
			END IF

		CASE 5%
	!++
	! Abstract:FLD005
	!	^*(05) State INTP\*
	!	.b
	!	.lm +5
	!	The ^*State INTP\* field enters
	!	a code for the product which is entered in field (01).
	!	A ^*Y\* or ^*N\* is the only response allowed.
	!	.b
	!	Valid responses may be viewed by pressing ^*List Choices, F14\*.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_SITE_PRODUCT::STA_INTP = &
				ENTR_3YESNO(SCOPE, SMG_WINDOW::WNUMBER, &
				"6;10", TEMP$, PP_SITE_PRODUCT::STA_INTP, &
				MFLAG, "'E", MVALUE)

		CASE 6%
	!++
	! Abstract:FLD006
	!	^*(06) State Rate\*
	!	.b
	!	.lm +5
	!	The ^*State Rate\* field contains the State Tax Rate
	!	assigned to the product.  The rate is expressed in a percentage.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_SITE_PRODUCT::STA_RATE = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"6;20", TEMP$, PP_SITE_PRODUCT::STA_RATE, &
				MFLAG, "##.#####", MVALUE)

		CASE 7%
	!++
	! Abstract:FLD007
	!	^*(07) State Account\*
	!	.b
	!	.lm +5
	!	The ^*State Account\* field refers to the General Ledger Account
	!	to which the rate will be assigned.
	!	.b
	!	Valid GL accounts may be viewed by pressing ^*List Choices, F14\*.
	!	.b
	!	This field will accept 18 alpha-numeric characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_SITE_PRODUCT::STA_ACCOUNT = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"6;35", TEMP$, PP_SITE_PRODUCT::STA_ACCOUNT, &
				MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%
				THEN
					PP_SITE_PRODUCT::STA_ACCOUNT = &
						GL_CHART::ACCT
				END IF

				GOTO Reenter
			END IF

		CASE 8%
	!++
	! Abstract:FLD008
	!	^*(08) County INTP\*
	!	.b
	!	.lm +5
	!	The ^*County INTP\* field enters
	!	a code for the product which is entered in field (01).
	!	A ^*Y\* or ^*N\* is the only response allowed.
	!	.b
	!	Valid responses may be viewed by pressing ^*List Choices, F14\*.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_SITE_PRODUCT::COU_INTP = &
				ENTR_3YESNO(SCOPE, SMG_WINDOW::WNUMBER, &
				"7;10", TEMP$, PP_SITE_PRODUCT::COU_INTP, &
				MFLAG, "'E", MVALUE)

		CASE 9%
	!++
	! Abstract:FLD009
	!	^*(09) County Rate\*
	!	.b
	!	.lm +5
	!	The ^*County Rate\* field contains the County Tax Rate
	!	assigned to the product.  The rate is expressed in a percentage.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_SITE_PRODUCT::COU_RATE = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"7;20", TEMP$, PP_SITE_PRODUCT::COU_RATE, &
				MFLAG, "##.#####", MVALUE)

		CASE 10%
	!++
	! Abstract:FLD010
	!	^*(10) County Account\*
	!	.b
	!	.lm +5
	!	The ^*County Account\* field refers to the General Ledger Account
	!	to which the rate will be assigned.
	!	.b
	!	Valid GL accounts may be viewed by pressing ^*List Choices, F14\*.
	!	.b
	!	This field will accept 18 alpha-numeric characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_SITE_PRODUCT::COU_ACCOUNT = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"7;35", TEMP$, PP_SITE_PRODUCT::COU_ACCOUNT, &
				MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%
				THEN
					PP_SITE_PRODUCT::COU_ACCOUNT = &
						GL_CHART::ACCT
				END IF

				GOTO Reenter
			END IF

		CASE 11%
	!++
	! Abstract:FLD011
	!	^*(11) City INTP\*
	!	.b
	!	.lm +5
	!	The ^*City INTP\* field enters
	!	a code for the product which is entered in field (01).
	!	A ^*Y\* or ^*N\* is the only response allowed.
	!	.b
	!	Valid responses may be viewed by pressing ^*List Choices, F14\*.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_SITE_PRODUCT::CTY_INTP = &
				ENTR_3YESNO(SCOPE, SMG_WINDOW::WNUMBER, &
				"8;10", TEMP$, PP_SITE_PRODUCT::CTY_INTP, &
				MFLAG, "'E", MVALUE)

		CASE 12%
	!++
	! Abstract:FLD012
	!	^*(12) City Rate\*
	!	.b
	!	.lm +5
	!	The ^*City Rate\* field contains the City Tax Rate
	!	assigned to the product.  The rate is expressed in a percentage.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_SITE_PRODUCT::CTY_RATE = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"8;20", TEMP$, PP_SITE_PRODUCT::CTY_RATE, &
				MFLAG, "##.#####", MVALUE)

		CASE 13%
	!++
	! Abstract:FLD013
	!	^*(13) City Account\*
	!	.b
	!	.lm +5
	!	The ^*City Account\* field refers to the General Ledger Account
	!	to which the rate will be assigned.
	!	.b
	!	Valid GL accounts may be viewed by pressing ^*List Choices, F14\*.
	!	.b
	!	This field will accept 18 alpha-numeric characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_SITE_PRODUCT::CTY_ACCOUNT = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"8;35", TEMP$, PP_SITE_PRODUCT::CTY_ACCOUNT, &
				MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%
				THEN
					PP_SITE_PRODUCT::CTY_ACCOUNT = &
						GL_CHART::ACCT
				END IF

				GOTO Reenter
			END IF

		CASE 14%
	!++
	! Abstract:FLD014
	!	^*(14) Sales Tax INTP\*
	!	.b
	!	.lm +5
	!	The ^*Sales Tax INTP\* field enters
	!	a code for the product which is entered in field (01).
	!	A ^*Y\* or ^*N\* is the only response allowed.
	!	.b
	!	Valid responses may be viewed by pressing ^*List Choices, F14\*.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_SITE_PRODUCT::STX_INTP = &
				ENTR_3YESNO(SCOPE, SMG_WINDOW::WNUMBER, &
				"9;10", TEMP$, PP_SITE_PRODUCT::STX_INTP, &
				MFLAG, "'E", MVALUE)

		CASE 15%
	!++
	! Abstract:FLD015
	!	^*(15) Sales Tax Rate\*
	!	.b
	!	.lm +5
	!	The ^*Sales Tax Rate\* field contains the Sales Tax Rate
	!	assigned to the product.  The rate is expressed in a percentage.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_SITE_PRODUCT::STX_RATE = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"9;20", TEMP$, PP_SITE_PRODUCT::STX_RATE, &
				MFLAG, "##.#####", MVALUE)

		CASE 16%
	!++
	! Abstract:FLD016
	!	^*(16) Sales Tax Account\*
	!	.b
	!	.lm +5
	!	The ^*Sales Tax Account\* field refers to the General Ledger Account
	!	to which the rate will be assigned.
	!	.b
	!	Valid GL accounts may be viewed by pressing ^*List Choices, F14\*.
	!	.b
	!	This field will accept 18 alpha-numeric characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_SITE_PRODUCT::STX_ACCOUNT = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"9;35", TEMP$, PP_SITE_PRODUCT::STX_ACCOUNT, &
				MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%
				THEN
					PP_SITE_PRODUCT::STX_ACCOUNT = &
						GL_CHART::ACCT
				END IF

				GOTO Reenter
			END IF

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test the Entry.
	!
20300	CASE OPT_TESTENTRY

		PP_MAIN_SITE_PRODUCT = 0%

		SELECT MLOOP

		CASE 1%
			!
			! Display the descriptions for product
			!
			PP_MAIN_SITE_PRODUCT = FUNC_TESTENTRY(SMG_WINDOW, &
				PP_SITE_PRODUCT::PRODUCT, &
				PD_PRODUCT::DESCRIPTION, &
				"PP", MLOOP, "PROG", &
				"Product", PD_MAIN_PRODUCT.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_PRODUCT::DESCRIPTION, 2%, 31%, , SMG$M_BOLD)

		CASE 4%
			!
			! Display the descriptions for account
			!
			IF PP_SITE_PRODUCT::FED_ACCOUNT = ""
			THEN
				GL_CHART::DESCR = ""
			ELSE
				PP_MAIN_SITE_PRODUCT = FUNC_TESTENTRY(SMG_WINDOW, &
					PP_SITE_PRODUCT::FED_ACCOUNT, &
					GL_CHART::DESCR, &
					"PP", MLOOP, "PROG", &
					"Federal Account", GL_MAIN_CHART.ID)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 5%, 52%, , SMG$M_BOLD)

		CASE 7%
			!
			! Display the descriptions for account
			!
			IF PP_SITE_PRODUCT::STA_ACCOUNT = ""
			THEN
				GL_CHART::DESCR = ""
			ELSE
				PP_MAIN_SITE_PRODUCT = FUNC_TESTENTRY(SMG_WINDOW, &
					PP_SITE_PRODUCT::STA_ACCOUNT, &
					GL_CHART::DESCR, &
					"PP", MLOOP, "PROG", &
					"State Account", GL_MAIN_CHART.ID)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 6%, 52%, , SMG$M_BOLD)

		CASE 10%
			!
			! Display the descriptions for account
			!
			IF PP_SITE_PRODUCT::COU_ACCOUNT = ""
			THEN
				GL_CHART::DESCR = ""
			ELSE
				PP_MAIN_SITE_PRODUCT = FUNC_TESTENTRY(SMG_WINDOW, &
					PP_SITE_PRODUCT::COU_ACCOUNT, &
					GL_CHART::DESCR, &
					"PP", MLOOP, "PROG", &
					"County Account", GL_MAIN_CHART.ID)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 7%, 52%, , SMG$M_BOLD)

		CASE 13%
			!
			! Display the descriptions for account
			!
			IF PP_SITE_PRODUCT::CTY_ACCOUNT = ""
			THEN
				GL_CHART::DESCR = ""
			ELSE
				PP_MAIN_SITE_PRODUCT = FUNC_TESTENTRY(SMG_WINDOW, &
					PP_SITE_PRODUCT::CTY_ACCOUNT, &
					GL_CHART::DESCR, &
					"PP", MLOOP, "PROG", &
					"City Account", GL_MAIN_CHART.ID)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 8%, 52%, , SMG$M_BOLD)


		CASE 16%
			!
			! Display the descriptions for account
			!
			IF PP_SITE_PRODUCT::STX_ACCOUNT = ""
			THEN
				GL_CHART::DESCR = ""
			ELSE
				PP_MAIN_SITE_PRODUCT = FUNC_TESTENTRY(SMG_WINDOW, &
					PP_SITE_PRODUCT::STX_ACCOUNT, &
					GL_CHART::DESCR, &
					"PP", MLOOP, "PROG", &
					"Sales Tax Account", GL_MAIN_CHART.ID)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 9%, 52%, , SMG$M_BOLD)

		END SELECT

	CASE OPT_DISPLAY

		!
		! Display the descriptions
		!
		IF (SMG_WINDOW::HFLAG(1%) AND 2%) = 0%
		THEN
			PD_PRODUCT::DESCRIPTION = &
				STRING$(LEN(PD_PRODUCT::DESCRIPTION), A"?"B) &
				IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, &
				"Q0" + PP_SITE_PRODUCT::PRODUCT) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_PRODUCT::DESCRIPTION, 2%, 31%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(4%) AND 2%) = 0%
		THEN
			GL_CHART::DESCR = &
				STRING$(LEN(GL_CHART::DESCR), A"?"B) &
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, &
				"Q0" + PP_SITE_PRODUCT::FED_ACCOUNT) <> 1%

			GL_CHART::DESCR = "" IF PP_SITE_PRODUCT::FED_ACCOUNT = ""

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 5%, 52%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(7%) AND 2%) = 0%
		THEN
			GL_CHART::DESCR = &
				STRING$(LEN(GL_CHART::DESCR), A"?"B) &
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, &
				"Q0" + PP_SITE_PRODUCT::STA_ACCOUNT) <> 1%

			GL_CHART::DESCR = "" IF PP_SITE_PRODUCT::STA_ACCOUNT = ""

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 6%, 52%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(10%) AND 2%) = 0%
		THEN
			GL_CHART::DESCR = &
				STRING$(LEN(GL_CHART::DESCR), A"?"B) &
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, &
				"Q0" + PP_SITE_PRODUCT::COU_ACCOUNT) <> 1%

			GL_CHART::DESCR = "" IF PP_SITE_PRODUCT::COU_ACCOUNT = ""

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 7%, 52%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(13%) AND 2%) = 0%
		THEN
			GL_CHART::DESCR = &
				STRING$(LEN(GL_CHART::DESCR), A"?"B) &
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, &
				"Q0" + PP_SITE_PRODUCT::CTY_ACCOUNT) <> 1%

			GL_CHART::DESCR = "" IF PP_SITE_PRODUCT::CTY_ACCOUNT = ""

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 8%, 52%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(16%) AND 2%) = 0%
		THEN
			GL_CHART::DESCR = &
				STRING$(LEN(GL_CHART::DESCR), A"?"B) &
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, &
				"Q0" + PP_SITE_PRODUCT::STX_ACCOUNT) <> 1%

			GL_CHART::DESCR = "" IF PP_SITE_PRODUCT::STX_ACCOUNT = ""

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 9%, 52%, , SMG$M_BOLD)
		END IF

	!
	! Set PP_SITE_PRODUCT_OLD value
	!
	CASE OPT_SETOLD
		PP_SITE_PRODUCT_OLD = PP_SITE_PRODUCT

	!
	! Restore PP_SITE_PRODUCT_OLD value
	!
	CASE OPT_RESETOLD
		PP_SITE_PRODUCT = PP_SITE_PRODUCT_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PP_SITE_PRODUCT2 = PP_SITE_PRODUCT

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PP_SITE_PRODUCT = PP_SITE_PRODUCT2

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!
		PP_SITE_PRODUCT::HOST  = LEFT(MVALUE, 4%)
		PP_SITE_PRODUCT::SITE  = MID(MVALUE, 5%, 4%)
		PP_SITE_PRODUCT::STYPE = RIGHT(MVALUE, 9%)

	!
	! View header
	!
	CASE OPT_VIEW

		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Product        "   + &
				"FI FRate FedAccount " + &
				"SI SRate StaAccount " + &
				"CI CRate CouAccount " + &
				"CI CRate CtyAccount " + &
				"SI SRate StxAccount "

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "017,020,026,037,040,046,057,060," + &
				"066,077,080,086,097,100,106"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = PP_SITE_PRODUCT::PRODUCT + " " + &
				PP_SITE_PRODUCT::FED_INTP + "  " + &
				FORMAT$(PP_SITE_PRODUCT::FED_RATE, "##.#####") + " "  + &
				LEFT(PP_SITE_PRODUCT::FED_ACCOUNT, 10%) + " " + &
				PP_SITE_PRODUCT::STA_INTP + "  " + &
				FORMAT$(PP_SITE_PRODUCT::STA_RATE, "##.#####") + " "  + &
				LEFT(PP_SITE_PRODUCT::STA_ACCOUNT, 10%) + " " + &
				PP_SITE_PRODUCT::COU_INTP + "  " + &
				FORMAT$(PP_SITE_PRODUCT::COU_RATE, "##.#####") + " "  + &
				LEFT(PP_SITE_PRODUCT::COU_ACCOUNT, 10%) + " " + &
				PP_SITE_PRODUCT::CTY_INTP + "  " + &
				FORMAT$(PP_SITE_PRODUCT::CTY_RATE, "##.#####") + " "  + &
				LEFT(PP_SITE_PRODUCT::CTY_ACCOUNT, 10%) + " " + &
				PP_SITE_PRODUCT::STX_INTP + "  " + &
				FORMAT$(PP_SITE_PRODUCT::STX_RATE, "##.#####") + " "  + &
				LEFT(PP_SITE_PRODUCT::STX_ACCOUNT, 10%)

		END SELECT

	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			WHEN ERROR IN
				FIND #SMG_WINDOW::CHAN, &
					KEY #0% GE PP_SITE_PRODUCT::HOST + &
					PP_SITE_PRODUCT::SITE + &
					PP_SITE_PRODUCT::STYPE + &
					PP_SITE_PRODUCT::PRODUCT, REGARDLESS
			USE
				CONTINUE 32767 IF ERR = 155%
				EXIT HANDLER
			END WHEN

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
						KEY #0% GE PP_SITE_PRODUCT::HOST + &
						PP_SITE_PRODUCT::SITE + &
						PP_SITE_PRODUCT::STYPE + &
						PP_SITE_PRODUCT::PRODUCT, REGARDLESS
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

			IF PP_SITE_PRODUCT::HOST + &
				PP_SITE_PRODUCT::SITE + &
				PP_SITE_PRODUCT::STYPE = MVALUE
			THEN
				SMG_WINDOW::CURREC = 0%
			END IF

		!
		! Change key
		!
		CASE 6%
			PP_SITE_PRODUCT::HOST  = LEFT(MVALUE, 3%)
			PP_SITE_PRODUCT::SITE  = MID(MVALUE, 4%, 2%)
			PP_SITE_PRODUCT::STYPE = RIGHT(MVALUE, 6%)

		END SELECT

	END SELECT

28000	EXIT FUNCTION

29000	!
	! Trap errors
	!
	ON ERROR GO BACK

32767	END FUNCTION
