1	%TITLE "Purchase Order Journal Subline Maintenance"
	%SBTTL "PO_MAIN_ORDERSLINE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PO_MAIN_ORDERSLINE(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	.p
	!	The ^*Line-Items\* option will provide a screen
	!
	! Index:
	!	.x Purchase Orders>Line Items
	!	.x Line Items>Purchase Orders
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS PO_SOURCE:PO_MAIN_ORDERSLINE/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP PO_MAIN_ORDERSLINE
	!	$ DELETE PO_MAIN_ORDERSLINE.OBJ;*
	!
	! Author:
	!
	!	02/12/92 - Dan Perkins
	!
	! Modification history:
	!
	!	02/24/92 - Kevin Handy
	!		Cleaned up (check)
	!
	!	03/17/92 - Dan Perkins
	!		Allow formatting options on OUR_QTY field.
	!
	!	11/16/92 - Dan Perkins
	!		Added CASE 2 to OPT_SUBWIND so that VIEW would
	!		work properly.
	!
	!	11/17/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	06/22/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!
	!	01/28/96 - Kevin Handy
	!		Changed STRING$(..,ASCII(" ")) to "" in several places.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE's
	!
	!	11/24/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--

	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PO_WINDOW.INC"

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD		GL_CHART

	%INCLUDE "SOURCE:[PO.OPEN]PO_ORDERJOUR.HB"
	MAP (PO_ORDERJOUR)	PO_ORDERJOUR_CDD	PO_ORDERJOUR

	%INCLUDE "SOURCE:[PO.OPEN]PO_ORDERLINE.HB"
	MAP (PO_ORDERLINE)	PO_ORDERLINE_CDD	PO_ORDERLINE

	%INCLUDE "SOURCE:[PO.OPEN]PO_ORDERSLINE.HB"
	MAP (PO_ORDERSLINE)	PO_ORDERSLINE_CDD	PO_ORDERSLINE
	MAP (PO_ORDERSLINE_OLD)	PO_ORDERSLINE_CDD	PO_ORDERSLINE_OLD, &
		PO_ORDERSLINE2

	MAP (GL_CHART_CH) &
		GL_CHART.CH%

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_PO_ORDERJOUR) &
		BATCH_NO$ = 2%, &
		PO_ORDERJOUR.CH%, &
		PO_ORDERJOUR.READONLY%

	COM (CH_PO_ORDERLINE) &
		PO_ORDERLINE.CH%, &
		PO_ORDERLINE.READONLY%

	COM (CH_PO_ORDERSLINE) &
		PO_ORDERSLINE.CH%, &
		PO_ORDERSLINE.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION FUNC_TESTENTRY
	EXTERNAL LONG   FUNCTION IC_WRIT_35BALANCE

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 29000

	SELECT MOPTION

	!**********************************************************************
	! Initialization
	!
	! This option is used to initialize the window structure,
	! set up the default values for add, and open all files
	! necessary that have not already been opened.
	!**********************************************************************
	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR  = "PO Order Line Detail"
		SMG_WINDOW::NHELP  = "PO_MAIN_ORDERSLINE"
		SMG_WINDOW::CURREC = -2%
		SMG_WINDOW::HSIZE  = 74%
		SMG_WINDOW::VSIZE  =  6%
		SMG_WINDOW::HPOS   =  4%
		SMG_WINDOW::VPOS   = 13%
		SMG_WINDOW::NITEMS =  6%
		SMG_WINDOW::FLAGS  =  0%
		SMG_WINDOW::HVIEW  = 74%
		SMG_WINDOW::VVIEW  =  6%
		SMG_WINDOW::VHPOS  =  4%
		SMG_WINDOW::VVPOS  = 13%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "req_Date"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		COM (FRM) FRM$(6%)

		!
		! Load in defaults
		!
		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF PO_ORDERSLINE.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PO_ORDERSLINE.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PO.OPEN]PO_ORDERSLINE.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PO_MAIN_ORDERSLINE = ERR
			CONTINUE 770
		END WHEN

		PO_ORDERSLINE.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PO.OPEN]PO_ORDERSLINE.OPN"
		USE
			PO_MAIN_ORDERSLINE = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PO_ORDERSLINE.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PO_ORDERSLINE.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = PO_ORDERSLINE.CH%
		WHEN ERROR IN
			RESET #PO_ORDERSLINE.CH%
			GET #PO_ORDERSLINE.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

	!**********************************************************************
	! Display the background
	!
	! This option is used to display the background information on the
	! screen.  It must first clear any junk on the screen, and then
	! write the background onto it.
	!**********************************************************************
20100	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	1, 2, "(01) Date Requested", &
			2, 2, "(02) Quantity", &
			3, 2, "(03) GL_Account", &
			4, 2, "(04) Subaccount", &
			5, 2, "(05) Notes 1", &
			6, 2, "(06) Notes 2", &
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

	!**********************************************************************
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user, display data,
	! set defaults, and return the data back according to MFLAG.
	!**********************************************************************
20200	CASE OPT_ENTRY

		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 E0Loop:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%
	!++
	! Abstract:FLD001
	!	^*(01) Date Requested\*
	!	.b
	!	.lm +5
	!	The ^*Date Requested\* field is used to enter the date
	!	on which the ordered items are to be received.
	!	.b
	!	The format for entry is MMDDYYYY.
	!	.lm -5
	!
	! Index:
	!
	!--
			PO_ORDERSLINE::RECEIVEDATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, "1;24", TEMP$, &
				PO_ORDERSLINE::RECEIVEDATE, MFLAG, "8", MVALUE)

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Order Quantity\*
	!	.b
	!	.lm +5
	!	The ^*Order Quantity\* field records number of items to be
	!	ordered.
	!	.b
	!	This field will accept a number as large as 9,999,999.99.
	!	.lm -5
	!
	! Index:
	!
	!--
			PO_ORDERSLINE::OUR_QTY = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "2;24", TEMP$, &
				PO_ORDERSLINE::OUR_QTY, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) GL__Account Number\*
	!	.p
	!	The ^*GL__Account Number\* field contains the General Ledger
	!	Account that will be debited by this purchase order line.
	!	.p
	!	Pressing the ^*List Choices\* key while the cursor is positioned on
	!	this field will cause the defined GL__Account Numbers to be displayed.
	!	.p
	!	This field will accommodate up to eighteen (18) alphanumeric
	!	characters.
	!
	! Index:
	!
	!--
			PO_ORDERSLINE::GL_ACCOUNT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "3;24", TEMP$, &
				PO_ORDERSLINE::GL_ACCOUNT, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%
				THEN
					PO_ORDERSLINE::GL_ACCOUNT = &
						GL_CHART::ACCT
				END IF
				GOTO E0Loop
			END IF

		CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Subaccount\*
	!	.p
	!	The ^*Subaccount\* field contains the subaccount or reference
	!	to which the ordered items can be associated, i.e. a job number,
	!	or a client order number, etc.
	!	.p
	!	This field will accomodate up to ten (10) alphanumeric
	!	characters.
	!
	! Index:
	!
	!--
			PO_ORDERSLINE::SUBACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "4;24", TEMP$, &
				PO_ORDERSLINE::SUBACCT, MFLAG, "'E", MVALUE)

		CASE 5% TO 6%
		SCOPE::PRG_ITEM = "FLD005"
	!++
	! Abstract:FLD005
	!	^*(05), (06)Notes\*
	!	.p
	!	The ^*Notes\* fields enter up to two lines of
	!	notes in reference to the order item.
	!	.p
	!	Each line will accommodate up to forty (40) alphanumeric
	!	characters.
	!
	! Index:
	!
	!--
			PO_ORDERSLINE::NOTES(MLOOP - 5%) = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				STR$(MLOOP) + ";24", TEMP$, &
				PO_ORDERSLINE::NOTES(MLOOP - 5%), &
				MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY

		SELECT MLOOP

		CASE 3%
			!
			! Is the input defined?
			!
			IF PO_ORDERSLINE::GL_ACCOUNT <> ""
			THEN
				PO_MAIN_ORDERSLINE = &
					FUNC_TESTENTRY(SMG_WINDOW, &
					PO_ORDERSLINE::GL_ACCOUNT, &
					GL_CHART::DESCR, &
					"PO", MLOOP, "GL_CHART", &
					"GL_Account number", GL_MAIN_CHART.ID)
			ELSE
				GL_CHART::DESCR = ""
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 3%, 44%,, SMG$M_BOLD)

		END SELECT

	!
	! Display additional information
	!
	CASE OPT_DISPLAY
		IF (SMG_WINDOW::HFLAG(3%) AND 2%) = 0%
		THEN
			IF MAIN_WINDOW(GL_MAIN_CHART.ID, &
				"Q0" + PO_ORDERSLINE::GL_ACCOUNT) <> 1%
			THEN
				GL_CHART::DESCR = ""
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 3%, 44%,, SMG$M_BOLD)

		END IF


	!
	! Set PO_ORDERSLINE_OLD value
	!
20500	CASE OPT_SETOLD
		PO_ORDERSLINE_OLD = PO_ORDERSLINE

	!
	! Restore PO_ORDERSLINE_OLD value
	!
	CASE OPT_RESETOLD
		PO_ORDERSLINE = PO_ORDERSLINE_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PO_ORDERSLINE2 = PO_ORDERSLINE

		IF MFLAG = 1%
		THEN
			SELECT MLOOP
			CASE 0%
				FRM$(2%) = "#,###,###"
			CASE ELSE
				FRM$(MLOOP) = MVALUE
			END SELECT
		END IF

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PO_ORDERSLINE = PO_ORDERSLINE2

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!
		PO_ORDERSLINE::PO = &
			LEFT(MVALUE, LEN(PO_ORDERSLINE::PO))

		PO_ORDERSLINE::PO_LINE = &
			RIGHT(MVALUE, LEN(PO_ORDERSLINE::PO) + 1%)

	! Find
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #PO_ORDERSLINE.CH%, &
				KEY #0% GE PO_ORDERSLINE::PO + &
				PO_ORDERSLINE::PO_LINE + &
				PO_ORDERSLINE::RECEIVEDATE, REGARDLESS
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
			MVALUE = "  ReqDate          Qty " + &
				"GL_Account         SubAccount"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "013,023,042"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = PRNT_DATE(PO_ORDERSLINE::RECEIVEDATE, 8%) &
				+ " " + &
				FORMAT$(PO_ORDERSLINE::OUR_QTY, "#,###,###") + &
				" " + &
				PO_ORDERSLINE::GL_ACCOUNT + " " + &
				PO_ORDERSLINE::SUBACCT

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
			IF MFLAG = -1%
			THEN
				V% = IC_WRIT_35BALANCE( &
					PO_ORDERLINE::OUR_PRODUCT, &
					PO_ORDERJOUR::FROMLOCATION, "PO", &
					-PO_ORDERSLINE::OUR_QTY) &
					IF PO_ORDERLINE::OUR_PRODUCT <> ""
			END IF

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
				FIND #PO_ORDERSLINE.CH%, &
					KEY #0% GE PO_ORDERSLINE::PO + &
					PO_ORDERSLINE::PO_LINE + &
					PO_ORDERSLINE::RECEIVEDATE, REGARDLESS

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
			IF PO_ORDERSLINE::PO + &
				PO_ORDERSLINE::PO_LINE = MVALUE
			THEN
				SMG_WINDOW::CURREC = 0%
			END IF
		!
		! Change key
		!
		CASE 6%
			PO_ORDERSLINE::PO = &
				LEFT(MVALUE, LEN(PO_ORDERSLINE::PO))

			PO_ORDERSLINE::PO_LINE = &
				RIGHT(MVALUE, LEN(PO_ORDERSLINE::PO) + 1%)

		END SELECT


	CASE OPT_AFTEROPT

		SELECT MVALUE

		CASE "Add"

			V% = IC_WRIT_35BALANCE (PO_ORDERLINE::OUR_PRODUCT, &
				PO_ORDERJOUR::FROMLOCATION, "PO", &
				PO_ORDERSLINE::OUR_QTY) &
				IF PO_ORDERLINE::OUR_PRODUCT <> ""


		CASE "Change", "Blank", "Initialize"

			V% = IC_WRIT_35BALANCE (PO_ORDERLINE::OUR_PRODUCT, &
				PO_ORDERJOUR::FROMLOCATION, "PO", &
				-PO_ORDERSLINE_OLD::OUR_QTY) &
				IF PO_ORDERLINE::OUR_PRODUCT <> ""

			V% = IC_WRIT_35BALANCE (PO_ORDERLINE::OUR_PRODUCT, &
				PO_ORDERJOUR::FROMLOCATION, "PO", &
				PO_ORDERSLINE::OUR_QTY) &
				IF PO_ORDERLINE::OUR_PRODUCT <> ""


		CASE "Erase"

			IF MLOOP <> 1%
			THEN
				V% = IC_WRIT_35BALANCE( &
					PO_ORDERLINE::OUR_PRODUCT, &
					PO_ORDERJOUR::FROMLOCATION, "PO", &
					-PO_ORDERSLINE::OUR_QTY) &
					IF PO_ORDERLINE::OUR_PRODUCT <> ""
			END IF

		END SELECT

	END SELECT

28000	EXIT FUNCTION

	%PAGE

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
