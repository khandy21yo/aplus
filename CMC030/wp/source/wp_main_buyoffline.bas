1	%TITLE "WIP Buyoff Line Journal"
	%SBTTL "WP_MAIN_BUYOFFLINE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG WP_MAIN_BUYOFFLINE(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	The ^*Work In Process Buyoff Line Journal\* enters the
	!	number of products completed and cancelled.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_MAIN_BUYOFFLINE/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN WP_MAIN_BUYOFFLINE
	!	$ DELETE WP_MAIN_BUYOFFLINE.OBJ;*
	!
	! Author:
	!
	!	06/11/91 - Val James "Spooky" Allen
	!
	! Modification history:
	!
	!	03/17/92 - Dan Perkins
	!		Allow format option on quantity and cost fields.
	!
	!	04/28/92 - Kevin Handy
	!		Clean up (check)
	!
	!	05/06/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	06/12/92 - Kevin Handy
	!		Clean up (check)
	!
	!	09/09/92 - Dan Perkins
	!		Added LoadLines option which I cut from the
	!		WP_MAIN_BUYOFF program.
	!		Also modified for updates in WP_READ_REGLINE function.
	!
	!	09/22/92 - Kevin Handy
	!		Clean up (check)
	!
	!	09/30/92 - Dan Perkins
	!		Switched Buyoff Date and Cost fields to read cost
	!		using PC_READ_COST.  Use this function to read cost
	!		instead of bringing cost from REGLINE.
	!
	!	10/26/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	11/03/92 - Dan Perkins
	!		Commented out calls to WP_WRIT_REGLINE and
	!		WP_WRIT_REQREGISTER.
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
	!	11/01/93 - Frank F. Starman
	!		Disable MLOOP = -1% in OPT_SUBWIND
	!
	!	04/13/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!		Change last parameter on ENTR_3CLOICE from "" to 0%
	!
	!	12/22/95 - Kevin Handy
	!		Remove a lot of commented out code, if statements
	!		around commented out code, variables set up for
	!		commented out code.
	!		Reformatted source code closer to 80 columns.
	!
	!	12/26/95 - Kevin Handy
	!		Always Use JC_JOB::LOCATION instead of
	!		WP_BUYOFF::LOCATION some of the time.
	!		Lose common field LOCATION$, which is never used.
	!
	!	05/21/98 - Kevin Handy
	!		Increase batch number from 2 to 8 characters.
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/17/2000 - Kevin Handy
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

	%INCLUDE "FUNC_INCLUDE:SB_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:WP_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[WP.OPEN]WP_BUYOFFLINE.HB"
	MAP (WP_BUYOFFLINE)	WP_BUYOFFLINE_CDD	WP_BUYOFFLINE
	MAP (WP_BUYOFFLINE_OLD)	WP_BUYOFFLINE_CDD	WP_BUYOFFLINE_OLD, &
							WP_BUYOFFLINE_DEF

	%INCLUDE "SOURCE:[WP.OPEN]WP_BUYOFF.HB"
	MAP (WP_BUYOFF)		WP_BUYOFF_CDD		WP_BUYOFF

	%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.HB"
	MAP (WP_REGLINE)	WP_REGLINE_CDD		WP_REGLINE
	COM (WP_REGLINE_READ)	WP_REGLINE_CDD		WP_REGLINE_READ

	%INCLUDE "SOURCE:[JC.OPEN]JC_JOB.HB"
	MAP (SB_SUBACCOUNT)	JC_JOB_CDD		JC_JOB

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP (SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD		GL_CHART

	%INCLUDE "SOURCE:[PD.OPEN]PD_ACCOUNT.HB"
	DECLARE			PD_ACCOUNT_CDD		PD_ACCOUNT_READ

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	!
	! Common Statements
	!
	COM (BATCH_NO) &
		BATCH_NO$ = 8%

	COM (CH_WP_BUYOFF) &
		WP_BUYOFF.CH%

	COM (CH_WP_BUYOFFLINE) &
		WP_BUYOFFLINE.CH%, &
		WP_BUYOFFLINE.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION FUNC_TESTENTRY
	EXTERNAL LONG   FUNCTION WP_READ_REGLINE
	EXTERNAL LONG   FUNCTION IC_WRIT_35BALANCE
	EXTERNAL LONG   FUNCTION PD_READ_ACCOUNT
	EXTERNAL LONG   FUNCTION PD_EXAM_PRODUCT
	EXTERNAL REAL   FUNCTION PC_READ_COST

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR  = "Buy off Journal Lines"
		SMG_WINDOW::CURREC = -2%
		SMG_WINDOW::NHELP  = "WP_MAIN_BUYOFFLINE"
		SMG_WINDOW::HSIZE  =  76%
		SMG_WINDOW::VSIZE  =  11%
		SMG_WINDOW::HPOS   =   3%
		SMG_WINDOW::VPOS   =   8%
		SMG_WINDOW::NITEMS =   7%
		SMG_WINDOW::FLAGS  =   0%
		SMG_WINDOW::HVIEW  = 128%
		SMG_WINDOW::VVIEW  =  11%
		SMG_WINDOW::VHPOS  =   3%
		SMG_WINDOW::VVPOS  =   8%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%)	= "Line_number"
			SMG_WINDOW::KFIELD(0%, 0%)	= 1%
			SMG_WINDOW::KFIELD(0%, 1%)	= 1%

		COM (WP_MAIN_BUYOFFLINE_FRM) FRM$(7%)

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

		!
		! Load in defaults for chart
		!
		CALL READ_DEFAULTS(SMG_WINDOW)

700		!
		! Declare channels
		!
		IF WP_BUYOFFLINE.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF WP_BUYOFFLINE.READONLY%

			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[WP.OPEN]WP_BUYOFFLINE.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			WP_MAIN_BUYOFFLINE = ERR
			CONTINUE 770
		END WHEN

		WP_BUYOFFLINE.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[WP.OPEN]WP_BUYOFFLINE.OPN"
		USE
			WP_MAIN_BUYOFFLINE = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		WP_BUYOFFLINE.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(WP_BUYOFFLINE.CH%)

		EXIT FUNCTION

790		IF MVALUE = "A"
		THEN
			GOSUB LoadLines
			IF X% = 1%
			THEN
				WP_MAIN_BUYOFFLINE = 1%
			END IF
		END IF

		SMG_WINDOW::CHAN  = WP_BUYOFFLINE.CH%
		WHEN ERROR IN
			RESET #WP_BUYOFFLINE.CH%
			GET #WP_BUYOFFLINE.CH%, REGARDLESS
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

		DATA	01,03, "(01) Line #", &
			03,03, "(02) Qty Completed", &
			04,03, "(03) Qty Cancelled", &
			05,03, "(04) Completed Date", &
			06,03, "(05) Unit Cost", &
			07,03, "(06) FG Id No.", &
			08,03, "(07) Inv/Cos Account #", &
			02,03, "     Item Code", &
			06,48, "Ext. Cost", &
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
	!	^*(01) Line\*
	!	.b
	!	.lm +5
	!	The ^*Line\* field enters the
	!	number of the line of a job or work order which is completed.
	!	.b
	!	Pressing ^*<List Choices>\* while the cursor is located at this field will cause
	!	a list of lines for a job order in the register file to be displayed.
	!	.lm -5
	!
	! Index:
	!	.x Line>Buy off Journal
	!	.x Buy off Journal>Line
	!
	!--
			WP_BUYOFFLINE::LLINE = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"01;27",TEMP$, WP_BUYOFFLINE::LLINE, MFLAG, &
				"~L0'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF MAIN_WINDOW(WP_MAIN_REGLINE.ID, "VX" + &
					WP_BUYOFFLINE::JOB) = 1%
				THEN
					WP_BUYOFFLINE::LLINE = WP_REGLINE::LLINE
				END IF
				GOTO ReEntry
			END IF

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Qty Completed\*
	!	.b
	!	.lm +5
	!	The ^*Qty Completed\* field
	!	enters the number of units which have been completed.
	!	.b
	!	The field may contain a figure as large as 9,999,999.
	!	.lm -5
	!
	! Index:
	!	.x Quantity Completed>Buy off Journal
	!	.x Buy off Journal>Quantity Completed
	!
	!--
			WP_BUYOFFLINE::COMPQTY = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"03;27",TEMP$, WP_BUYOFFLINE::COMPQTY, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Qty Cancelled\*
	!	.b
	!	.lm +5
	!	The ^*Qty Cancelled\* field
	!	enters the number of units which are to be cancelled.
	!	.b
	!	The field may contain a figure as large as 9,999,999.
	!	.lm -5
	!
	! Index:
	!	.x Quantity Cancelled>Buy off Journal
	!	.x Cancelled Quantity>Buy off Journal
	!	.x Buy off Journal>Quantity Cancelled
	!
	!--
			WP_BUYOFFLINE::CANCELQTY = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"04;27",TEMP$, WP_BUYOFFLINE::CANCELQTY, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)

		CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Completed Date\*
	!	.b
	!	.lm +5
	!	The ^*Completed Date\* field
	!	enters the date of completion for the line item represented on the
	!	screen.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!	.x Completed Date>Buy off Journal
	!	.x Buy off Journal>Completed Date
	!
	!--
			WP_BUYOFFLINE::BDATE = ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"05;27",TEMP$, WP_BUYOFFLINE::BDATE, MFLAG, &
				"'E", MVALUE)

		CASE 5%
	!++
	! Abstract:FLD005
	!	^*(05) Unit Cost\*
	!	.b
	!	.lm +5
	!	The ^*Unit Cost\* field should automatically
	!	display the unit cost of a product as it is recorded in the Inventory Control
	!	System.
	!	.lm -5
	!
	! Index:
	!	.x Unit Cost>Buy off Journal
	!	.x Buy off Journal>Unit Cost
	!
	!--
			WP_BUYOFFLINE::COST = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"06;27",TEMP$, WP_BUYOFFLINE::COST, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)

		CASE 6%
	!++
	! Abstract:FLD006
	!	^*(06) FG Id Number\*
	!	.b
	!	.lm +5
	!	The ^*Finished Goods Identification Number\* field
	!	enters a finished goods
	!	identification number (serial number) to be associated with a
	!	completed product when that product is identified with a
	!	serial number in the Equipment Ledger rather than a product number in the
	!	Inventory Ledger.
	!	.lm -5
	!
	! Index:
	!	.x Buy off Journal>Serial Number
	!	.x Buy off Journal>Identification Number
	!	.x Serial Number>Buy off Journal
	!	.x Identification Number>Buy off Journal
	!
	!--
			WP_BUYOFFLINE::FG_ID_NO = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"7;27", TEMP$, &
				WP_BUYOFFLINE::FG_ID_NO, MFLAG, "'E", MVALUE)

		CASE 7%
	!++
	! Abstract:FLD007
	!	^*(07) Inv/Cos Account\*
	!	.b
	!	.lm +5
	!	The ^*Inv/Cos Account\* field
	!	enters the General Ledger Finished Goods Inventory account number which
	!	is to be debited with a buy off transaction.
	!	.b
	!	Pressing ^*<List Choices>\* while the cursor is located at this field will
	!	cause a list of the General Ledger chart of accounts to be displayed.
	!	.lm -5
	!
	! Index:
	!	.x Finished Goods Inventory Account>Buy off Journal
	!	.x Account Number>Finished Goods
	!	.x Buy off Journal>Finished Goods Inventory
	!
	!--
			WP_BUYOFFLINE::ACCT = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"8;27", TEMP$, &
				WP_BUYOFFLINE::ACCT, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%)
				THEN
					WP_BUYOFFLINE::ACCT = &
						GL_CHART::ACCT
				END IF
				GOTO Reentry
			END IF

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY

		WP_MAIN_BUYOFFLINE = 0%

		SELECT MLOOP

		CASE 1%
			!
			! Look up the product number in the WP_REGLINE file
			!
			WP_REGLINE_READ::ITEMCODE = ""
			WP_REGLINE_READ::DESCR =  ""

			V% = WP_READ_REGLINE(WP_BUYOFFLINE::JOB, &
				WP_BUYOFFLINE::LLINE, "EQ", &
				WP_REGLINE_READ, QTY())

			GOTO Undef IF WP_REGLINE_READ::LLINE <> WP_BUYOFFLINE::LLINE

			GOTO Gotit

 Undef:
			CALL HELP_34MESSAGE(SCOPE, "undefined WIP order line", &
				"W", SCOPE::PRG_PROGRAM, "", "UNDORDLIN")

	!++
	! Warning:UNDORDLIN
	!	^*Undefined Order Line\*
	!	.b
	!	.lm +5
	!	^*Explanation\*
	!	.b
	!	Selected order line doesn't exist in Order Register
	!	file.
	!	.b
	!	^*User Action\*
	!	.b
	!	Select order line from the Order Register file. Pressing
	!	<List Choices> displays a list of all valid lines for
	!	the particular order.
	!	.lm -5
	!
	! Index:
	!	.x Undefined Order Line
	!
	!--

				WP_MAIN_BUYOFFLINE = 1%
				GOTO ExitFunction

 Gotit:
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				WP_REGLINE_READ::ITEMCODE, &
				02%, 27%,, SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				WP_REGLINE_READ::DESCR, &
				02%, 48%,, SMG$M_BOLD)

			IF MVALUE = "ADD"
			THEN
				WP_BUYOFFLINE::COMPQTY = QTY(6%)

				V% = PD_EXAM_PRODUCT(WP_REGLINE_READ::ITEMCODE, &
					PD_PRODUCT_EXAM)

				V% = PD_READ_ACCOUNT(JC_JOB::LOCATION, &
					PD_PRODUCT_EXAM::PROD_TYPE, &
					PD_ACCOUNT_READ)

				WP_BUYOFFLINE::ACCT = PD_ACCOUNT_READ::INVACCT

				MASKX$ = FORMAT$(WP_BUYOFFLINE::COMPQTY, "#,###,###")

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					MASKX$, 3%, 27%,, SMG$M_BOLD)

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					WP_BUYOFFLINE::ACCT, 8%, 27%,, SMG$M_BOLD)

			END IF

		CASE 4%
			!
			! Read cost
			!
			WP_BUYOFFLINE::COST = PC_READ_COST( &
				WP_REGLINE_READ::ITEMCODE, &
				JC_JOB::LOCATION, &
				WP_BUYOFFLINE::BDATE, "")

			!
			! Calculate the extended cost
			!
			DISC = FUNC_ROUND(WP_BUYOFFLINE::COMPQTY * &
				WP_BUYOFFLINE::COST, 2%)

			DISC$ = FORMAT$(DISC, "#,###,###.##")

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				DISC$, 6%, 58%,, SMG$M_BOLD)

		CASE 7%
			WP_MAIN_BUYOFFLINE = FUNC_TESTENTRY(SMG_WINDOW, &
				WP_BUYOFFLINE::ACCT, &
				GL_CHART::DESCR, &
				"WP", MLOOP, "PRG", &
				"Account", GL_MAIN_CHART.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(GL_CHART::DESCR, 30%), &
				8%, 48%,, SMG$M_BOLD)

		END SELECT

	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(1%) AND 2%) = 0%
		THEN
			!
			! Look up the product number in the WP_REGLINE file
			!
			WP_REGLINE_READ::ITEMCODE = ""
			WP_REGLINE_READ::DESCR = ""

			V% = WP_READ_REGLINE(WP_BUYOFFLINE::JOB, &
				WP_BUYOFFLINE::LLINE, &
				"EQ", WP_REGLINE_READ, QTY())

			!
			! Display the information that we looked up.
			!
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				WP_REGLINE_READ::ITEMCODE, &
				02%, 27%,, SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				WP_REGLINE_READ::DESCR, &
				02%, 48%,, SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(4%) AND 2%) = 0%
		THEN
			DISC = FUNC_ROUND(WP_BUYOFFLINE::COMPQTY * &
				WP_BUYOFFLINE::COST, 2%)

			DISC$ = FORMAT$(DISC, "#,###,###.##")

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				DISC$, 6%, 58%,, SMG$M_BOLD)

		END IF

		IF (SMG_WINDOW::HFLAG(7%) AND 2%) = 0%
		THEN
			IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "Q0" + &
				WP_BUYOFFLINE::ACCT) <> 1%)
			THEN
				GL_CHART::DESCR = "??????????????????????????????"
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(GL_CHART::DESCR, 30%), &
				8%, 48%,, SMG$M_BOLD)
		END IF


	! Set WP_BUYOFFLINE_OLD value
	!
20500	CASE OPT_SETOLD
		WP_BUYOFFLINE_OLD = WP_BUYOFFLINE

	!
	! Restore WP_BUYOFFLINE_OLD value
	!
	CASE OPT_RESETOLD
		WP_BUYOFFLINE = WP_BUYOFFLINE_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		WP_BUYOFFLINE_DEF = WP_BUYOFFLINE

		IF MFLAG = 1%
		THEN
			SELECT MLOOP
			CASE 0%
				FRM$(2%) = "#,###,###"
				FRM$(3%) = "#,###,###"
				FRM$(5%) = "#,###,###.##"
			CASE ELSE
				FRM$(MLOOP) = MVALUE
			END SELECT
		END IF

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		WP_BUYOFFLINE = WP_BUYOFFLINE_DEF

		IF MFLAG = 1%
		THEN
			WP_BUYOFFLINE::BDATE = DATE_TODAY &
				IF WP_BUYOFFLINE_DEF::BDATE = ""
		END IF

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!
		WP_BUYOFFLINE::JOB = MVALUE

	!
	! Find the next record
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE WP_BUYOFFLINE::JOB + &
				WP_BUYOFFLINE::LLINE, REGARDLESS

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
			MVALUE = "  Line# CompletedQty CancelQty     "    + &
				"ItemCost    ExtendedCost CompletedDate" + &
				" G/LAccount#        SerialNumber"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "008,021,031,044,060,074,093"

		!
		! Convert current record into text
		!
		CASE 3%
			DISC = FUNC_ROUND(WP_BUYOFFLINE::COMPQTY * &
				WP_BUYOFFLINE::COST, 2%)

			DISC$ = FORMAT$(DISC, "#,###,###.##")

			MVALUE = WP_BUYOFFLINE::LLINE + "     " + &
				FORMAT$(WP_BUYOFFLINE::COMPQTY, "#,###,###") + " "     + &
				FORMAT$(WP_BUYOFFLINE::CANCELQTY, "#,###,###") + " "     + &
				FORMAT$(WP_BUYOFFLINE::COST, "#,###,###.##") + "    "  + &
				DISC$ + " "     + &
				PRNT_DATE(WP_BUYOFFLINE::BDATE, 8%) + "    " + &
				WP_BUYOFFLINE::ACCT + " " + &
				WP_BUYOFFLINE::FG_ID_NO

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
			USE
				CONTINUE ExitFunction IF ERR = 155%
				EXIT HANDLER
			END WHEN

			SMG_WINDOW::CURREC = 0%

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
						WP_BUYOFFLINE::LLINE, REGARDLESS
				USE
					CONTINUE ExitFunction IF ERR = 155%
					EXIT HANDLER
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

			IF WP_BUYOFFLINE::JOB = MVALUE
			THEN
				SMG_WINDOW::CURREC = 0%
			END IF

		!
		! Change key
		!
		CASE 6%
			WP_BUYOFFLINE::JOB  = MVALUE

		END SELECT

	CASE OPT_AFTEROPT

		SELECT MVALUE

		CASE "Add"

			GOSUB GetRemain

			IF WP_REGLINE_READ::TTYPE = "M"
			THEN
				V% = IC_WRIT_35BALANCE (WP_REGLINE_READ::ITEMCODE, &
					JC_JOB::LOCATION, "WO", &
					-ALLOCATE_QTY)

				V% = IC_WRIT_35BALANCE (WP_REGLINE_READ::ITEMCODE, &
					JC_JOB::LOCATION, "MA", &
					WP_BUYOFFLINE::COMPQTY)
			END IF

			!
			! stuff written into the register line file
			!
			KINGDOMKEY$ = WP_REGLINE_READ::JOB + &
				WP_REGLINE_READ::LLINE

		CASE "Change", "Blank", "Initialize"

			GOSUB GetRemainOld

			!
			! Reverse stuff written into the register line file
			!
			KINGDOMKEY$ = WP_REGLINE_READ::JOB + &
				WP_REGLINE_READ::LLINE

			!
			! Reverse stuff written into the register line file
			!
			KINGDOMKEY$ = WP_REGLINE_READ::JOB + &
				WP_REGLINE_READ::LLINE

			IF WP_REGLINE_READ::TTYPE = "M"
			THEN
				V% = IC_WRIT_35BALANCE (WP_REGLINE_READ::ITEMCODE, &
					JC_JOB::LOCATION, "WO", &
					ALLOCATE_QTY)

				V% = IC_WRIT_35BALANCE (WP_REGLINE_READ::ITEMCODE, &
					JC_JOB::LOCATION, "MA", &
					-WP_BUYOFFLINE_OLD::COMPQTY)

				GOSUB GetRemain

				V% = IC_WRIT_35BALANCE (WP_REGLINE_READ::ITEMCODE, &
					JC_JOB::LOCATION, "WO", &
					-ALLOCATE_QTY)

				V% = IC_WRIT_35BALANCE (WP_REGLINE_READ::ITEMCODE, &
					JC_JOB::LOCATION, "MA", &
					WP_BUYOFFLINE::COMPQTY)
			END IF

		CASE "Erase"

			IF MLOOP <> 1%
			THEN
				GOSUB GetRemain

				IF WP_REGLINE_READ::TTYPE = "M"
				THEN
					V% = IC_WRIT_35BALANCE (WP_REGLINE_READ::ITEMCODE, &
						JC_JOB::LOCATION, "WO", &
						ALLOCATE_QTY)

					V% = IC_WRIT_35BALANCE (WP_REGLINE_READ::ITEMCODE, &
						JC_JOB::LOCATION, "MA", &
						-WP_BUYOFFLINE::COMPQTY)
				END IF

				!
				! Reverse stuff written into the register line file
				!
				KINGDOMKEY$ = WP_REGLINE_READ::JOB + &
					WP_REGLINE_READ::LLINE

			END IF

		END SELECT

	END SELECT

 ExitFunction:
28000	EXIT FUNCTION

	%PAGE

 GetRemain:
	EXIT_STATUS% = WP_READ_REGLINE(WP_BUYOFFLINE::JOB, &
		WP_BUYOFFLINE::LLINE, "EQ", &
		WP_REGLINE_READ, QTY())

	!
	! Calculate remaining on-order for new or current line
	!
	ALLOCATE_QTY = QTY(6%)

	ALLOCATE_QTY = (WP_BUYOFFLINE::COMPQTY + WP_BUYOFFLINE::CANCELQTY) &
		IF (WP_BUYOFFLINE::COMPQTY + WP_BUYOFFLINE::CANCELQTY) <= ALLOCATE_QTY

	RETURN

 GetRemainOld:
	EXIT_STATUS% = WP_READ_REGLINE(WP_BUYOFFLINE::JOB, &
		WP_BUYOFFLINE::LLINE, "EQ", &
		WP_REGLINE_READ, QTY())

	!
	! Calculate remaining on-order for old qty's shipped and cancelled
	!
	ALLOCATE_QTY = QTY(6%)

	ALLOCATE_QTY = (WP_BUYOFFLINE_OLD::COMPQTY + WP_BUYOFFLINE_OLD::CANCELQTY) &
		IF (WP_BUYOFFLINE_OLD::COMPQTY + WP_BUYOFFLINE_OLD::CANCELQTY)  <= ALLOCATE_QTY

	RETURN

 LoadLines:
	!
	!=======================================================================
	!
	SOPTION$(1%) = "Auto Load Lines"
	SOPTION$(2%) = "Auto Cancel Lines"
	SOPTION$(3%) = "Manually Enter Lines"

 SelectSOption:
	X% = ENTR_3CHOICE(SCOPE, "", "", SOPTION$(), "", &
		0%, "Select Option", "", 0%)

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		GOTO ExitLoadLines
	END SELECT

	SELECT X%

	CASE 0%
		GOTO SelectSOption

	CASE 3%
		GOTO ExitLoadLines

	END SELECT

	CALL ENTR_3MESSAGE(SCOPE, "Loading Lines", 1% + 16%)

	TESTLINE$ = SPACE$(LEN(WP_REGLINE_READ::LLINE) + 1%)

 ReadRegLine:
	GOTO ExitLoadLines IF WP_READ_REGLINE(WP_BUYOFF::JOB, &
		TESTLINE$, "GT", WP_REGLINE_READ, QTY()) <> CMC$_NORMAL

	TESTLINE$ = WP_REGLINE_READ::LLINE

	GOTO ReadRegLine IF WP_REGLINE_READ::TTYPE = "L"
	GOTO ReadRegLine IF QTY(6%) <= 0.0

	WP_BUYOFFLINE::COST = PC_READ_COST( &
		WP_REGLINE_READ::ITEMCODE, &
		JC_JOB::LOCATION, &
		DATE_TODAY, "")

	!
	! Build part of BUYOFFLINE from register line
	!
	WP_BUYOFFLINE::LLINE	= WP_REGLINE_READ::LLINE
	WP_BUYOFFLINE::JOB	= WP_BUYOFF::JOB
	WP_BUYOFFLINE::BDATE	= DATE_TODAY
	WP_BUYOFFLINE::ACCT	= ""
	WP_BUYOFFLINE::FG_ID_NO	= ""

	SELECT X%

	CASE 1%
		WP_BUYOFFLINE::CANCELQTY = 0.0
		WP_BUYOFFLINE::COMPQTY = QTY(6%)

	CASE 2%
		WP_BUYOFFLINE::COMPQTY = 0.0
		WP_BUYOFFLINE::CANCELQTY = QTY(6%)

	END SELECT

	ITEMCODE$ = WP_REGLINE_READ::ITEMCODE
	PD_ACCOUNT_READ::INVACCT = ""

	V% = PD_EXAM_PRODUCT(ITEMCODE$, PD_PRODUCT_EXAM)

	V% = PD_READ_ACCOUNT(JC_JOB::LOCATION, &
		PD_PRODUCT_EXAM::PROD_TYPE, PD_ACCOUNT_READ)

	WP_BUYOFFLINE::ACCT = PD_ACCOUNT_READ::INVACCT

	V% = IC_WRIT_35BALANCE (ITEMCODE$, JC_JOB::LOCATION, "WO", &
		-WP_BUYOFFLINE::COMPQTY)

	V% = IC_WRIT_35BALANCE (ITEMCODE$, JC_JOB::LOCATION, "MA", &
		WP_BUYOFFLINE::COMPQTY)

28460	PUT #WP_BUYOFFLINE.CH%

	GOTO ReadRegLine

 ExitLoadLines:
	CALL ENTR_3MESSAGE(SCOPE, "", 1%)

	RETURN

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
