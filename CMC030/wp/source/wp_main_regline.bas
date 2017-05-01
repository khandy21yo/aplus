1	%TITLE "WIP Register Line Journal"
	%SBTTL "WP_MAIN_REGLINE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG WP_MAIN_REGLINE(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1991 BY
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
	!	The Manufacturing WIP Order Register Line Journal
	!	is used to enter the line information.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_MAIN_REGLINE/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN WP_MAIN_REGLINE
	!	$ DELETE WP_MAIN_REGLINE.OBJ;*
	!
	! Author:
	!
	!	05/29/91 - Val James "Goofer" Allen
	!
	! Modification history:
	!
	!	08/02/91 - Craig Tanner
	!		First: Left-justified feild one with 0's, and
	!		. . . .
	!
	!	11/10/92 - Dan Perkins
	!		Added CASE 2 to OPT_SUBWIND so that VIEW would
	!		work properly.
	!
	!	11/17/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	06/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	01/29/96 - Kevin Handy
	!		Reformat source code.
	!		Change STRING$(...,ASCII(" ")) to "" in
	!		several places.
	!
	!	06/06/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/06/2000 - Kevin Handy
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

	%INCLUDE "FUNC_INCLUDE:WP_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.HB"
	MAP (WP_REGLINE)	WP_REGLINE_CDD		WP_REGLINE
	MAP (WP_REGLINE_OLD)	WP_REGLINE_CDD		WP_REGLINE_OLD, &
							WP_REGLINE_DEF

	%INCLUDE "SOURCE:[WP.OPEN]WP_REGHEADER.HB"
	MAP (WP_REGHEADER)	WP_REGHEADER_CDD	WP_REGHEADER

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP (SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	!
	! Common Statements
	!
	COM (CH_WP_REGHEADER) &
		SB_SUBACCOUNT.CH%, &
		WP_REGHEADER.READONLY%

	COM (CH_WP_REGLINE) &
		WP_REGLINE.CH%, &
		WP_REGLINE.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION MAIN_WINDOW
	EXTERNAL LONG	FUNCTION FUNC_TESTENTRY
	EXTERNAL REAL   FUNCTION PC_READ_COST

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "WIP Order Register Lines"
		SMG_WINDOW::CURREC = -2%
		SMG_WINDOW::NHELP = "WP_MAIN_REGLINE"
		SMG_WINDOW::HSIZE = 76%
		SMG_WINDOW::VSIZE = 12%
		SMG_WINDOW::HPOS  = 3%
		SMG_WINDOW::VPOS  = 6%
		SMG_WINDOW::NITEMS= 13%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::HVIEW = 76%
		SMG_WINDOW::VVIEW = 12%
		SMG_WINDOW::VHPOS = 3%
		SMG_WINDOW::VVPOS = 6%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Line"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		COM (WP_MAIN_REGLINE_FRM) FRM$(13%)

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

		!
		! Load in defaults for chart
		!
		CALL READ_DEFAULTS(SMG_WINDOW)

700		!
		! Declare channels
		!
		IF WP_REGLINE.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF WP_REGLINE.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			WP_MAIN_REGLINE = ERR
			CONTINUE 770
		END WHEN

		WP_REGLINE.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.OPN"
		USE
			WP_MAIN_REGLINE = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		WP_REGLINE.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(WP_REGLINE.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = WP_REGLINE.CH%
		WHEN ERROR IN
			RESET #WP_REGLINE.CH%
			GET #WP_REGLINE.CH%, REGARDLESS
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

		DATA    01,02, "(01) Line No", &
			02,02, "(02) Line Type", &
			03,02, "(03) Item Code", &
			04,02, "(04) Description", &
			05,02, "(05) Quantity", &
			06,02, "(06) Cost", &
			07,02, "(07) Start Date", &
			08,02, "(08) Comp Date", &
			09,02, "(09) Batch No", &
			10,02, "(10) Post Time", &
			11,02, "(11) Post Date", &
			12,02, "(12) Record Type", &
			06,40, "Ext. Cost", &
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

 Reentry:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%
	!++
	! Abstract:FLD001
	!	^*(01) Line Number\*
	!	.lm +5
	!	.b
	!	The ^*Line Number\* field reflects the line
	!	numbers as they were established relative to a job in a Job Order journal.
	!	.lm -5
	!
	! Index:
	!	.x Line Number>Register
	!
	!--
			WP_REGLINE::LLINE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"01;25",TEMP$, WP_REGLINE::LLINE, MFLAG, &
				"~L0'E", MVALUE)

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Line Type\*
	!	.lm +5
	!	.b
	!	The ^*Line Type\* field indicates the type
	!	of line which was established relative to a job in a Job Order journal.
	!	.b
	!	The only valid line types are:
	!	.table 3,25
	!	.te
	!	^*M\* = Material
	!	.te
	!	^*L\* = Labor
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Line Type>Register
	!	.x Register>Line Type
	!
	!--
			IF TEMP$ = "Add"
			THEN
				TEXT$ = "                         "

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					TEXT$, 2%, 40%, , SMG$M_BOLD)
			END IF

			WP_REGLINE::TTYPE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"02;25",TEMP$, WP_REGLINE::TTYPE, MFLAG, &
				"'E", MVALUE)

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Item Code\*
	!	.B
	!	.LM +5
	!	The ^*Item Code\* field is used to enter either
	!	the product to be used when the Line type is material
	!	or the operation when the Line type is labor.
	!	.lm -5
	!
	! Index:
	!	.x Item Code
	!
	!--
			WP_REGLINE::ITEMCODE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"03;25",TEMP$, WP_REGLINE::ITEMCODE, MFLAG, &
				"'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF WP_REGLINE::TTYPE = "M"
				THEN
					IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, "VX") = 1%
					THEN
						WP_REGLINE::ITEMCODE = &
							PD_PRODUCT::PRODUCT_NUM
					END IF
				END IF
				GOTO Reentry
			END IF

		CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Description\*
	!	.b
	!	.lm +5
	!	The ^*Description\* field is used to enter a description for the
	!	line item previously entered.
	!	.lm -5
	!
	! Index:
	!	.x Description
	!
	!--
			WP_REGLINE::DESCR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"04;25",TEMP$, WP_REGLINE::DESCR, MFLAG, &
				"'E", MVALUE)

		CASE 5%
	!++
	! Abstract:FLD005
	!	^*(05) Quantity\*
	!	.lm +5
	!	.b
	!	The ^*Quantity\* field reflects the quantity
	!	of product or labor hours which was originally established in a Job Order
	!	journal.
	!	.b
	!	^*NOTE: This field in a Job Register record must not be edited!!!!\*
	!	.lm -5
	!
	! Index:
	!	.x Quantity>Register
	!	.x Register>Quantity
	!
	!--
			WP_REGLINE::QTY = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"05;25",TEMP$, WP_REGLINE::QTY, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)

		CASE 6%
	!++
	! Abstract:FLD006
	!	^*(06) Cost\*
	!	.lm +5
	!	.b
	!	The ^*Cost\* field reflects the cost
	!	relative to a line item as it was established in a Job Order journal.
	!	.b
	!	^*NOTE:  This field in a Job Register record must not be edited!!!!\*
	!	.lm -5
	!
	! Index:
	!	.x Cost>Register
	!	.x Register>Cost
	!
	!--
			IF WP_REGLINE::TTYPE = "M"
			THEN
				IF (TEMP$ = "Add") AND (WP_REGLINE::COST = 0.0)
				THEN
					WP_REGLINE::COST = PC_READ_COST(WP_REGLINE::ITEMCODE,WP_REGHEADER::LOCATION, &
						WP_REGHEADER::BDATE, "")
				END IF
			END IF

			WP_REGLINE::COST = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"06;25",TEMP$, WP_REGLINE::COST, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)

		CASE 7%
	!++
	! Abstract:FLD007
	!	^*(07) Start Date\*
	!	.lm +5
	!	.b
	!	The ^*Start Date\* field indicates the anticipated
	!	date a product is to be needed or an operation is to be performed.
	!	.lm -5
	!
	! Index:
	!	.x Start Date>Register
	!	.x Register>Start Date
	!
	!--
			WP_REGLINE::START_DATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"07;25",TEMP$, WP_REGLINE::START_DATE, MFLAG, &
				"'E", MVALUE)

		CASE 8%
	!++
	! Abstract:FLD008
	!	^*(08) Completion Date\*
	!	.lm +5
	!	.b
	!	The ^*Completion Date\* field indicates the
	!	anticipated date an operation is to be completed.
	!	.lm -5
	!
	! Index:
	!	.x Completion Date>Register
	!	.x Register>Completion Date
	!
	!--
			WP_REGLINE::COMP_DATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"08;25",TEMP$, WP_REGLINE::COMP_DATE, MFLAG, &
				"'E", MVALUE)

		CASE 9%
	!++
	! Abstract:FLD009
	!	^*(09) Batch Number\*
	!	.b
	!	.lm +5
	!	The ^*Batch Number\* field is to be entered with the batch number of
	!	this transaction.
	!	.lm -5
	!
	! Index:
	!	.x Batch
	!
	!--
			WP_REGLINE::BATCH = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"09;25",TEMP$, WP_REGLINE::BATCH, MFLAG, &
				"'E", MVALUE)

		CASE 10%
	!++
	! Abstract:FLD010
	!	^*(10) Post Time\*
	!	.lm +5
	!	.b
	!	The ^*Post Time\* field indicates the time
	!	a record in a Job Order journal was posted to the Job Register.
	!	.lm -5
	!
	! Index:
	!	.x Post Time>Register
	!	.x Register>Post Time
	!
	!--
			WP_REGLINE::POST_TIME = ENTR_3TIME(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"10;25",TEMP$, WP_REGLINE::POST_TIME, MFLAG, &
				"'E", MVALUE)

		CASE 11%
	!++
	! Abstract:FLD011
	!	^*(11) Post Date\*
	!	.lm +5
	!	.b
	!	The ^*Post Date\* field indicates the date
	!	a Job Order journal, in which a job was created, was posted.
	!	.lm -5
	!
	! Index:
	!	.x Post Date>Register
	!	.x Register>Post Date
	!
	!--
			WP_REGLINE::POST_DATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"11;25",TEMP$, WP_REGLINE::POST_DATE, MFLAG, &
				"'E", MVALUE)

		CASE 12%
	!++
	! Abstract:FLD012
	!	^*(12) Record Type\*
	!	.lm +5
	!	.b
	!	The ^*Record Type\* field indicates the
	!	type of transaction a record represents.
	!	.b
	!	Valid record types are:
	!	.table 3,25
	!	.te
	!	01 = original order transaction
	!	.te
	!	02 = completed (posted) transaction
	!	.te
	!	03 = cancelled (posted) transaction
	!	.te
	!	12 = completed (buyoff register) transaction
	!	.te
	!	13 = cancelled (buyoff register) transaction
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Record Type>Register
	!	.x Register>Record Type
	!
	!--
			WP_REGLINE::REC_TYPE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"12;25",TEMP$, WP_REGLINE::REC_TYPE, MFLAG, &
				"'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY

		WP_MAIN_REGLINE = 0%

		SELECT MLOOP

		CASE 1%
			WP_MAIN_REGLINE = 1% IF WP_REGLINE::LLINE = ""

		CASE 2%
			!
			! Make sure that the fools only enter an M or an L
			!
			IF WP_REGLINE::TTYPE <> "M" AND WP_REGLINE::TTYPE <> "L"
			THEN
				WP_MAIN_REGLINE = 1%
				GOTO 28000
			END IF

			IF WP_REGLINE::TTYPE = "M"
			THEN
				TEXT$ = "MATERIAL LINE"

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					TEXT$, 2%, 40%, , SMG$M_BOLD)
			END IF

			IF WP_REGLINE::TTYPE = "L"
			THEN
				TEXT$ = "LABOR LINE   "

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					TEXT$, 2%, 40%, , SMG$M_BOLD)
			END IF

		CASE 3%
			IF WP_REGLINE::TTYPE = "M"
			THEN
				!
				! Test WP_REGLINE product
				!
				WP_MAIN_REGLINE = FUNC_TESTENTRY(SMG_WINDOW, &
					WP_REGLINE::ITEMCODE, &
					PD_PRODUCT::DESCRIPTION, &
					"WP", MLOOP, "PROG", &
					"Product", PD_MAIN_PRODUCT.ID)

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					PD_PRODUCT::DESCRIPTION, &
					3%, 40%, , SMG$M_BOLD)
			END IF

		CASE 5%
			IF WP_REGLINE::QTY < 0.0
			THEN
				WP_MAIN_REGLINE = 1%
			END IF


		CASE 6%
			IF WP_REGLINE::COST < 0.0
			THEN
				WP_MAIN_REGLINE = 1%
			ELSE
				TEXT$ = FORMAT$(WP_REGLINE::QTY * &
					WP_REGLINE::COST, "#,###,###.##")

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					TEXT$, 6%, 61%, , SMG$M_BOLD)
			END IF

		END SELECT

	CASE OPT_DISPLAY

		!
		! Display the descriptions
		!
		IF (SMG_WINDOW::HFLAG(2%) AND 2%) = 0%
		THEN
			IF WP_REGLINE::TTYPE = "M"
			THEN
				TEXT$ = "MATERIAL LINE"

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					TEXT$, 2%, 40%, , SMG$M_BOLD)
			END IF

			IF WP_REGLINE::TTYPE = "L"
			THEN
				TEXT$ = "LABOR LINE   "

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					TEXT$, 2%, 40%, , SMG$M_BOLD)
			END IF
		END IF

		IF (SMG_WINDOW::HFLAG(3%) AND 2%) = 0%
		THEN
			IF WP_REGLINE::TTYPE = "M"
			THEN
				PD_PRODUCT::DESCRIPTION = &
					STRING$(LEN(PD_PRODUCT::DESCRIPTION), A"?"B) &
					IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, "Q0" +WP_REGLINE::ITEMCODE) <> 1%

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_PRODUCT::DESCRIPTION, 3%, 40%, , SMG$M_BOLD)
			ELSE
				PD_PRODUCT::DESCRIPTION = ""

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					PD_PRODUCT::DESCRIPTION, 3%, 40%, , SMG$M_BOLD)
			END IF

		END IF

		IF (SMG_WINDOW::HFLAG(6%) AND 2%) = 0%
		THEN
			TEXT$ = FORMAT$(WP_REGLINE::QTY * &
				WP_REGLINE::COST, "#,###,###.##")

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				TEXT$, 6%, 61%, , SMG$M_BOLD)
		END IF

	! Set WP_REGLINE_OLD value
	!
20500	CASE OPT_SETOLD
		WP_REGLINE_OLD = WP_REGLINE

	!
	! Restore WP_REGLINE_OLD value
	!
	CASE OPT_RESETOLD
		WP_REGLINE = WP_REGLINE_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		WP_REGLINE_DEF = WP_REGLINE

		IF MFLAG = 1%
		THEN
			SELECT MLOOP
			CASE 0%
				FRM$(5%) = "#,###,###"
				FRM$(6%) = "#,###,###.##"
			CASE ELSE
				FRM$(MLOOP) = MVALUE
			END SELECT
		END IF

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		WP_REGLINE = WP_REGLINE_DEF

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!
		WP_REGLINE::JOB  = MVALUE

	!
	! Find the next record
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE WP_REGLINE::JOB + &
				WP_REGLINE::LLINE, REGARDLESS

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
			MVALUE = "  Line RT LT ItemCode        Quantity      Cost StartDate  CompDate"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "007,010,013,028,038,048,059"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = WP_REGLINE::LLINE + " "  + &
				WP_REGLINE::REC_TYPE + " "  + &
				WP_REGLINE::TTYPE + "  " + &
				WP_REGLINE::ITEMCODE + " "  + &
				FORMAT$(WP_REGLINE::QTY, "######.##") + " "  + &
				FORMAT$(WP_REGLINE::COST, "######.##") + " "  + &
				PRNT_DATE(WP_REGLINE::START_DATE, 8%) + " "  + &
				PRNT_DATE(WP_REGLINE::COMP_DATE, 8%)

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
					FIND #SMG_WINDOW::CHAN, KEY #0% GE MVALUE + &
						WP_REGLINE::LLINE, REGARDLESS
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
			IF WP_REGLINE::JOB = MVALUE
			THEN
				SMG_WINDOW::CURREC = 0%
			END IF

		!
		! Change key
		!
		CASE 6%
			WP_REGLINE::JOB = MVALUE

		END SELECT

	END SELECT

28000	EXIT FUNCTION

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
