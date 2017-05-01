1	%TITLE "Work In Process Order Line Journal"
	%SBTTL "WP_MAIN_JOBLINE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG WP_MAIN_JOBLINE(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	Manufacturing Work In Process Order Line Journal is used to enter the
	!	line information about the production order (ie: products,
	!	operations, etc.)
	!	.lm -5
	!
	! Index:
	!	.x Help>Manurfacturing WIP Order Line Journal
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_MAIN_JOBLINE/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN WP_MAIN_JOBLINE
	!	$ DELETE WP_MAIN_JOBLINE.OBJ;*
	!
	! Author:
	!
	!	05/28/91 - Val James "Gweedo" Allen
	!
	! Modification history:
	!
	!	03/17/92 - Dan Perkins
	!		Allow format option on Qty and Cost fields.
	!
	!	04/28/92 - Kevin Handy
	!		Clean up (check)
	!
	!	07/15/92 - Dan Perkins
	!		Cleaned program code.
	!
	!	08/18/92 - Dan Perkins
	!		Fill description field with spaces if item is labor.
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
	!		Update to V3.6 Calico coding standards
	!
	!	07/27/95 - Kevin Handy
	!		Reformat source closer to 80 columns
	!
	!	10/31/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	06/06/97 - Kevin Handy
	!		Use integer for #key
	!
	!	05/18/98 - Kevin Handy
	!		Change File Batch Number from 7 characters to 8.
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/29/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[WP.OPEN]WP_ORDERLINE.HB"
	MAP (WP_ORDERLINE)	WP_ORDERLINE_CDD	WP_ORDERLINE
	MAP (WP_ORDERLINE_OLD)	WP_ORDERLINE_CDD	WP_ORDERLINE_OLD, &
							WP_ORDERLINE_DEF

	%INCLUDE "SOURCE:[WP.OPEN]WP_JOB.HB"
	MAP (WP_JOB)		WP_JOB_CDD		WP_JOB
	MAP (WP_JOB_ONE)	WP_JOB_CDD		WP_JOB_ONE

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	MAP (TT_WP_MAIN_JOBJOUR) &
		TETITLE$ = 32%, &
		TE$(4%) = 64%
	!
	! Common Statements
	!
	COM (CH_WP_JOB) &
		WP_JOB.CH%, &
		SB_SUBACCOUNT.CH%, &
		XPERIOD$ = 6%, &
		AVAILABLE

	COM (BATCH_NO) &
		BATCH_NO$ = 8%

	COM (CH_WP_ORDERLINE) &
		WP_ORDERLINE.CH%, &
		WP_ORDERLINE.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION FUNC_TESTENTRY
	EXTERNAL LONG   FUNCTION IC_WRIT_35BALANCE
	EXTERNAL REAL   FUNCTION PC_READ_COST

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "WIP Order Entry Lines"
		SMG_WINDOW::CURREC = -2%
		SMG_WINDOW::NHELP = "WP_MAIN_JOBLINE"
		SMG_WINDOW::HSIZE =  76%
		SMG_WINDOW::VSIZE =  11%
		SMG_WINDOW::HPOS  =   3%
		SMG_WINDOW::VPOS  =   8%
		SMG_WINDOW::NITEMS=   7%
		SMG_WINDOW::FLAGS =   0%
		SMG_WINDOW::HVIEW = 128%
		SMG_WINDOW::VVIEW =  11%
		SMG_WINDOW::VHPOS =   3%
		SMG_WINDOW::VVPOS =   8%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Type"
			SMG_WINDOW::KFIELD(0%, 0%) = 2%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
			SMG_WINDOW::KFIELD(0%, 2%) = 2%

		COM (WP_MAIN_JOBLINE_FRM) FRM$(7%)

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

		!
		! Load in defaults for chart
		!
		CALL READ_DEFAULTS(SMG_WINDOW)

		!
		! Declare channels
		!
700		IF WP_ORDERLINE.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF WP_ORDERLINE.READONLY%

			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[WP.OPEN]WP_ORDERLINE.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			WP_MAIN_JOBLINE = ERR
			CONTINUE 770
		END WHEN

		WP_ORDERLINE.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[WP.OPEN]WP_ORDERLINE.OPN"
		USE
			WP_MAIN_JOBLINE = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		WP_ORDERLINE.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(WP_ORDERLINE.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = WP_ORDERLINE.CH%
		WHEN ERROR IN
			RESET #WP_ORDERLINE.CH%
			GET #WP_ORDERLINE.CH%, REGARDLESS
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


		DATA	02,02, "(01) Line Type", &
			03,02, "(02) Item Code", &
			04,02, "(03) Description", &
			05,02, "(04) Quantity", &
			06,02, "(05) Cost", &
			07,02, "(06) Start Date", &
			08,02, "(07) Comp Date", &
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

		TETITLE$ = "Type Description"
		TE$(0%) = "2"
		TE$(1%) = "M  Material Line Entry"
		TE$(2%) = "L  Labor Line Entry"

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
	!	^*(01) Line Type\*
	!	.b
	!	.lm +5
	!	The ^*Line Type\* field
	!	specifies whether the line record refers to material or labor.  The only valid
	!	entries are:
	!	.table 3,25
	!	.te
	!	^*M\* = Material
	!	.te
	!	^*L\* = Labor
	!	.end table
	!	Pressing ^*<List Choices>\* while the cursor is located at this field will
	!	cause a list of valid flags to be displayed.
	!	.lm -5
	!
	! Index:
	!	.x Line Type
	!
	!--
			IF TEMP$ = "Add"
			THEN
				TEXT$ = "                         "
				SMG_STATUS% = &
					SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					TEXT$, 2%, 40%, , SMG$M_BOLD)
			END IF

			WP_ORDERLINE::TTYPE = &
				ENTR_3STRINGLIST(SCOPE, SMG_WINDOW::WNUMBER, &
				"02;25",TEMP$, WP_ORDERLINE::TTYPE, MFLAG, &
				"'", MVALUE, TE$(), TETITLE$, "004")

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Item Code\*
	!	.lm +5
	!	.b
	!	The ^*Item Code\* field contains either
	!	the product number to be used, if Line type = ^*M\*, or the operation code, if
	!	Line type = ^*L\*
	!	.b
	!	Pressing ^*<List Choices>\* while the cursor is located at this field will cause
	!	a list of valid choices to be displayed.
	!	.lm -5
	!
	! Index:
	!	.x Item Code>Work Order
	!	.x Item Code>Job Order
	!	.x Job Order>Line>Item Code
	!	.x Work Order>Line>Item Code
	!
	!--
			WP_ORDERLINE::ITEMCODE = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"03;25",TEMP$, WP_ORDERLINE::ITEMCODE, MFLAG, &
				"'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF WP_ORDERLINE::TTYPE = "M"
				THEN
					IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, &
						"VX") = 1%
					THEN
						WP_ORDERLINE::ITEMCODE = &
							PD_PRODUCT::PRODUCT_NUM
					END IF
				END IF
				GOTO Reentry
			END IF

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Description\*
	!	.b
	!	.lm +5
	!	The ^*Description\* field contains the description of the product or the
	!	labor operation as they are defined in the Products or the Labor Operations
	!	tables.
	!	.b
	!	If the (02) Item Code field is left blank, this field can be
	!	manually entered.
	!	.lm -5
	!
	! Index:
	!	.x Description
	!
	!--
			MFLAG = MFLAG OR 1% IF WP_ORDERLINE::TTYPE = "M"

			WP_ORDERLINE::DESCR = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"04;25",TEMP$, WP_ORDERLINE::DESCR, MFLAG, &
				"'E", MVALUE)

		CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Quantity\*
	!	.b
	!	.lm +5
	!	The ^*Quantity\* field
	!	enters the number of units to be used, if Line type is = ^*M\*, or the number of
	!	labor hours, if Line Type is = ^*L\*.
	!	.b
	!	This field will contain a figure as large as 9,999,999.
	!	.lm -5
	!
	! Index:
	!	.x Quantity>Work Order>Lines
	!	.x Quantity>Job Order>Lines
	!	.x Work Order>Lines>Quantity
	!	.x Job Order>Lines>Quantity
	!
	!--
			WP_ORDERLINE::QTY = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"05;25",TEMP$, WP_ORDERLINE::QTY, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)

		CASE 5%
	!++
	! Abstract:FLD005
	!	^*(05) Cost/Unit\*
	!	.b
	!	.lm +5
	!	The ^*Cost/Unit\* field
	!	enters the unit cost of a product or the hourly rate if the line record
	!	represents labor.
	!	.b
	!	If the line record represents material, the cost of a product as recorded
	!	in the Inventory Product file will automatically appear in this field.
	!	.lm -5
	!
	! Index:
	!	.x Cost/Unit>Work Order>Line
	!	.x Cost/Unit>Job Order>Line
	!	.x Work Order>Cost/Unit>Line
	!	.x Job Order>Cost/Unit>Line
	!
	!--
			IF WP_ORDERLINE::TTYPE = "M"
			THEN
				IF (TEMP$ = "Add") AND &
					(WP_ORDERLINE::COST = 0.0)
				THEN
					WP_ORDERLINE::COST = &
						PC_READ_COST(WP_ORDERLINE::ITEMCODE, &
						WP_JOB::LOCATION, &
						WP_JOB::BDATE, "")
				END IF
			END IF

			WP_ORDERLINE::COST = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"06;25",TEMP$, WP_ORDERLINE::COST, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)

		CASE 6%
	!++
	! Abstract:FLD006
	!	^*(06) Start Date\*
	!	.b
	!	.lm +5
	!	The ^*Start Date\* field
	!	enters the date a work order is to be started.
	!	.B
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!	.x Start Date>Work Order>Line
	!	.x Start Date>Job Order>Line
	!	.x Work Order>Start Date>Line
	!	.x Job Order>Start Date>Line
	!
	!--
			WP_ORDERLINE::START_DATE = &
				ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"07;25",TEMP$, WP_ORDERLINE::START_DATE, &
				MFLAG, "'E", MVALUE)

		CASE 7%
	!++
	! Abstract:FLD007
	!	^*(07) Completion Date\*
	!	.b
	!	.lm +5
	!	The ^*Completion Date\* field
	!	enters the date the work order is expected to be completed.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!	.x Completion Date>Work Order>Lines
	!	.x Completion Date>Job Order>Lines
	!	.x Work Order>Completion Date>Lines
	!	.x Job Order>Completion Date>Lines
	!
	!--
			WP_ORDERLINE::COMP_DATE = &
				ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"08;25", TEMP$, WP_ORDERLINE::COMP_DATE, &
				MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY

		WP_MAIN_JOBLINE = 0%

		SELECT MLOOP

		CASE 1%
			!
			! Make sure that the fools only enter an M or an L
			!
			IF WP_ORDERLINE::TTYPE <> "M" AND &
				WP_ORDERLINE::TTYPE <> "L"
			THEN
				WP_MAIN_JOBLINE = 1%
				GOTO 28000
			END IF

			IF WP_ORDERLINE::TTYPE = "M"
			THEN
				TEXT$ = "MATERIAL LINE"

				SMG_STATUS% = &
					SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					TEXT$, 2%, 40%, , SMG$M_BOLD)
			END IF

			IF WP_ORDERLINE::TTYPE = "L"
			THEN
				TEXT$ = "LABOR LINE   "

				SMG_STATUS% = &
					SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					TEXT$, 2%, 40%, , SMG$M_BOLD)
			END IF

		CASE 2%
			IF WP_ORDERLINE::TTYPE = "M"
			THEN
				!
				! Test WP_ORDERLINE product
				!
				WP_MAIN_JOBLINE = FUNC_TESTENTRY(SMG_WINDOW, &
					WP_ORDERLINE::ITEMCODE, &
					PD_PRODUCT::DESCRIPTION, &
					"WP", MLOOP, "PROG", &
					"Product", PD_MAIN_PRODUCT.ID)

				WP_ORDERLINE::DESCR = PD_PRODUCT::DESCRIPTION

				SMG_STATUS% = &
					SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					PD_PRODUCT::DESCRIPTION, &
					4%, 25%, , SMG$M_BOLD)
			END IF

		CASE 4%
			IF WP_ORDERLINE::QTY < 0.0
			THEN
				WP_MAIN_JOBLINE = 1%
			END IF

		CASE 5%
			IF WP_ORDERLINE::COST < 0.0
			THEN
				WP_MAIN_JOBLINE = 1%
			ELSE
				TEXT$ = FORMAT$(WP_ORDERLINE::QTY * &
					WP_ORDERLINE::COST, "#,###,###.##")

				SMG_STATUS% = &
					SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					TEXT$, 6%, 52%, , SMG$M_BOLD)
			END IF

		END SELECT

	CASE OPT_DISPLAY

		!
		! Display the descriptions
		!
		IF (SMG_WINDOW::HFLAG(1%) AND 2%) = 0%
		THEN
			IF WP_ORDERLINE::TTYPE = "M"
			THEN
				TEXT$ = "MATERIAL LINE"

				SMG_STATUS% = &
					SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					TEXT$, 2%, 40%, , SMG$M_BOLD)
			END IF

			IF WP_ORDERLINE::TTYPE = "L"
			THEN
				TEXT$ = "LABOR LINE   "

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					TEXT$, 2%, 40%, , SMG$M_BOLD)
			END IF
		END IF

		IF (SMG_WINDOW::HFLAG(5%) AND 2%) = 0%
		THEN
			TEXT$ = FORMAT$(WP_ORDERLINE::QTY * WP_ORDERLINE::COST, "#,###,###.##")

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				TEXT$, 6%, 52%, , SMG$M_BOLD)
		END IF

	!
	! Set WP_ORDERLINE_OLD value
	!
20500	CASE OPT_SETOLD
		WP_ORDERLINE_OLD = WP_ORDERLINE

	!
	! Restore WP_ORDERLINE_OLD value
	!
	CASE OPT_RESETOLD
		WP_ORDERLINE = WP_ORDERLINE_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		WP_ORDERLINE_DEF = WP_ORDERLINE

		IF MFLAG = 1%
		THEN
			SELECT MLOOP

			CASE 0%
				FRM$(4%) = "#,###,###"
				FRM$(5%) = "#,###,###.##"

			CASE ELSE
				FRM$(MLOOP) = MVALUE

			END SELECT
		END IF

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		WP_ORDERLINE = WP_ORDERLINE_DEF

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!
		RANDOMIZE

		WP_ORDERLINE::JOB  = MVALUE
		WP_ORDERLINE::LLINE  = FORMAT$(RND * 1000, "<0>###")

	!
	! Find the next record
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #1% GE WP_ORDERLINE::TTYPE + &
				WP_ORDERLINE::ITEMCODE + &
				WP_ORDERLINE::JOB, REGARDLESS

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
			MVALUE = "  Type Item Code      Description              " + &
				"                 Quantity         Cost "         + &
				"Start Date Comp Date"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "007,022,063,073,086,097"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = WP_ORDERLINE::TTYPE + "    " + &
				WP_ORDERLINE::ITEMCODE + " " + &
				WP_ORDERLINE::DESCR + " " + &
				FORMAT$(WP_ORDERLINE::QTY, "#,###,###") + " " + &
				FORMAT$(WP_ORDERLINE::COST, "#,###,###.##") + " " + &
				PRNT_DATE(WP_ORDERLINE::START_DATE, 8%) + " " + &
				PRNT_DATE(WP_ORDERLINE::COMP_DATE, 8%)

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
			! Check if erasing the whole order's lines and if so
			! then reset the inventory quantities correctly?
			!
			SELECT MFLAG

			CASE -1%
				IF WP_ORDERLINE::TTYPE = "M"
				THEN
					V% = IC_WRIT_35BALANCE (WP_ORDERLINE::ITEMCODE, &
						WP_JOB::LOCATION, "WO", &
						-WP_ORDERLINE::QTY)
				END IF

			END SELECT

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
					FIND #SMG_WINDOW::CHAN, KEY #0% GE &
						MVALUE, REGARDLESS
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
			IF WP_ORDERLINE::JOB = MVALUE
			THEN
				SMG_WINDOW::CURREC = 0%
			END IF

		!
		! Change key
		!
		CASE 6%
			WP_ORDERLINE::JOB = MVALUE

		END SELECT

	CASE OPT_AFTEROPT

		SELECT MVALUE

		CASE "Add"

			IF WP_ORDERLINE::TTYPE = "M"
			THEN
				V% = IC_WRIT_35BALANCE (WP_ORDERLINE::ITEMCODE, &
					WP_JOB::LOCATION, "WO", &
					WP_ORDERLINE::QTY)
			END IF

		CASE "Change", "Blank", "Initialize"

			IF WP_ORDERLINE::TTYPE = "M"
			THEN
				V% = IC_WRIT_35BALANCE (WP_ORDERLINE_OLD::ITEMCODE, &
					WP_JOB_ONE::LOCATION, "WO", &
					-WP_ORDERLINE_OLD::QTY)

				V% = IC_WRIT_35BALANCE (WP_ORDERLINE::ITEMCODE, &
					WP_JOB::LOCATION, "WO", &
					WP_ORDERLINE::QTY)
			END IF

		CASE "Erase"

			IF MLOOP <> 1%
			THEN
				IF WP_ORDERLINE::TTYPE = "M"
				THEN
					V% = IC_WRIT_35BALANCE (WP_ORDERLINE::ITEMCODE, &
						WP_JOB::LOCATION, "WO", &
						-WP_ORDERLINE::QTY)
				END IF

			END IF

		END SELECT

	END SELECT

28000	EXIT FUNCTION

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
