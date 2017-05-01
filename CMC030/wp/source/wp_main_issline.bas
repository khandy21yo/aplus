1	%TITLE "Inventory Issue Line Journal"
	%SBTTL "WP_MAIN_ISSLINE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG WP_MAIN_ISSLINE(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	Inventory Issue Line Journal is used to make multiple issues of
	!	products for one requisition.
	!	.lm -5
	!
	! Index:
	!	.x Inventory Issue Line Journal
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_MAIN_ISSLINE/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN WP_MAIN_ISSLINE
	!	$ DELETE WP_MAIN_ISSLINE.OBJ;*
	!
	! Author:
	!
	!	07/19/91 - Craig Tanner
	!
	! Modification history:
	!
	!	09/14/91 - Frank F. Starman
	!		Clean source code.
	!
	!	09/17/91 - Deborah K. Fries
	!		Added fields & resized View window
	!
	!	06/12/92 - Kevin Handy
	!		Clean up (check)
	!
	!	07/22/92 - Dan Perkins
	!		Reworked LOADLINES subroutine to get it to
	!		work properly.
	!
	!	08/26/92 - Dan Perkins
	!		Added IC_WRIT_35BALANCE to inform inventory of
	!		goings on.
	!
	!	09/25/92 - Dan Perkins
	!		Display Product Description.  Allow to issue
	!		lines not in REQREGISTER.
	!
	!	10/09/92 - Dan Perkins
	!		Set PROD_FLAG = "N" in OPT_RESETDEFAULT to avoid
	!		problems down the road.
	!
	!	11/03/92 - Dan Perkins
	!		Commented out calls to WP_WRIT_REQREGISTER.
	!
	!	11/17/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	12/11/92 - Dan Perkins
	!		Fixed Canceled Qty and Remain Qty displays to display
	!		the proper quantities.  Changed Canceled Qty to
	!		Issued Qty.
	!
	!		NOTE:	Call to subroutine GetRemainOld is not used
	!			at this time.
	!
	!	06/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	01/11/95 - Kevin Handy
	!		Modified to fill in LOCATION$ so that the
	!		update of the running balance may work.
	!
	!	04/13/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards.
	!		Change last parameter on entr_3choice from "" to 0%
	!
	!	08/07/95 - Kevin Handy
	!		Commented out subroutine GetRemainOld, since it isn't
	!		used for/by anything.
	!		Fix weird indentation in select statements.
	!
	!	08/10/95 - Kevin Handy
	!		Modified to explicitely set a blank date to
	!		todays date, instead of it sneaking in through
	!		PC_READ_COST.
	!
	!	08/11/95 - Kevin Handy
	!		Reformat source code closer to 80 columns.
	!
	!	08/11/95 - Kevin Handy
	!		Removed lots of commented out code, that I didn't
	!		know what it used to do, or if any of the sections
	!		were related, or what they were.
	!
	!	08/15/95 - Kevin Handy
	!		Modified to assign line numbers automatically
	!		if entered without one.
	!
	!	07/16/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/30/96 - Kevin Handy
	!		Test the REQ::LLINE to see if it changes while
	!		creating issue lines automatically.
	!
	!	05/19/98 - Kevin Handy
	!		Increase batch number from 2 characters to 8
	!
	!	06/16/98 - Kevin Handy
	!		Clean up goofy spacing around some functions
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/12/99 - Kevin Handy
	!		Fix array size for SOPTION so it isn't too small.
	!
	!	05/02/2000 - Kevin Handy
	!		Handle PRODUCT_FACTOR on the cost.
	!
	!	05/04/2000 - Kevin Handy
	!		Fix PRODUCT_FACTOR bug in LoadLines:
	!
	!	05/24/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	06/12/2000 - Kevin Handy
	!		Fix reference of PD_PRODUCT to PD_PRODUCT_EXAM in load
	!		(ROBSON)
	!
	!	11/06/2000 - Kevin Handy
	!		Use A"x"B
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
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:WP_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[WP.OPEN]WP_ISSJOUR.HB"
	MAP (WP_ISSJOUR)	WP_ISSJOUR_CDD		WP_ISSJOUR

	%INCLUDE "SOURCE:[WP.OPEN]WP_ISSLINE.HB"
	MAP (WP_ISSLINE)	WP_ISSLINE_CDD		WP_ISSLINE
	MAP (WP_ISSLINE_OLD)	WP_ISSLINE_CDD		WP_ISSLINE_OLD, &
							WP_ISSLINE_DEF

	%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.HB"
	MAP(WP_REQREGISTER)	WP_REQREGISTER_CDD	WP_REQREGISTER
	DECLARE			WP_REQREGISTER_CDD	WP_REQREGISTER_READ

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP(PD_PRODUCT)		PD_PRODUCT_CDD		PD_PRODUCT
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	!
	! Common Statements
	!
	COM (BATCH_NO) &
		BATCH_NO$ = 8%

	COM (LOCATION) &
		LOCATION$ = 4%

	COM (CH_WP_ISSLINE) &
		WP_ISSLINE.CH%

	COM (WP_ISSLINE_KEEPSAKE) &
		STRING LAST_ISSLINE = 4%

	COM (CH_WP_REQREGISTER_READ) WP_REQREGISTER.CH%

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION FUNC_TESTENTRY
	EXTERNAL LONG   FUNCTION IC_WRIT_35BALANCE
	EXTERNAL LONG   FUNCTION WP_READ_REQREGISTER
	EXTERNAL LONG   FUNCTION PD_EXAM_PRODUCT
	EXTERNAL REAL   FUNCTION PC_READ_COST

	DIM STRING SOPTION(4%)

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Inventory Issue Line Journal"
		SMG_WINDOW::CURREC = -2%
		SMG_WINDOW::NHELP = "WP_MAIN_ISSLINE"
		SMG_WINDOW::HSIZE = 77%
		SMG_WINDOW::VSIZE = 12%
		SMG_WINDOW::HPOS  = 3%
		SMG_WINDOW::VPOS  = 7%
		SMG_WINDOW::NITEMS= 7%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::HVIEW = 77%
		SMG_WINDOW::VVIEW = 12%
		SMG_WINDOW::VHPOS = 3%
		SMG_WINDOW::VVPOS = 6%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Line_number"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		COM (WP_MAIN_ISSJOUR_FRM) FRM$(7%)

		CALL READ_DEFAULTS(SMG_WINDOW)

		!
		! Declare channels
		!
700		IF WP_ISSLINE.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF WP_ISSLINE.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[WP.OPEN]WP_ISSLINE.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			WP_MAIN_ISSLINE = ERR
			CONTINUE 770
		END WHEN

		WP_ISSLINE.READONLY% = 0%

		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[WP.OPEN]WP_ISSLINE.OPN"
		USE
			WP_MAIN_ISSLINE = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		WP_ISSLINE.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(WP_ISSLINE.CH%)

		EXIT FUNCTION

790		IF MVALUE = "A"
		THEN
			GOSUB LoadLines

			IF X% = 1% OR X% = 2%
			THEN
				WP_MAIN_ISSLINE = 1%
			END IF
		END IF

		SMG_WINDOW::CHAN  = WP_ISSLINE.CH%

		WHEN ERROR IN
			RESET #WP_ISSLINE.CH%
			GET #WP_ISSLINE.CH%, REGARDLESS
		USE
			CONTINUE 32767
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

		DATA	01,05, "(01) Req Line", &
			02,05, "(02) Product", &
			03,05, "(03) Issue Date", &
			04,05, "(04) Cost", &
			05,05, "(05) Qty Issue", &
			06,05, "(06) Qty Cancel", &
			09,05, "     Original Req Qty", &
			10,05, "     Issued Qty", &
			11,05, "     Remaining Qty", &
			05,45, "Ext. Cost", &
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
		TEMP$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 Reenter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%
	!++
	! Abstract:FLD001
	!	^*(01) Req[uisition] Line\*
	!	.lm +5
	!	.b
	!	The ^*Req[uisition] Line\* field
	!	enters the requisition line against which material is being issued.
	!	.b
	!	Pressing ^*<List Choices>\* while the cursor is located at this field will
	!	cause register records on previous materials issues to be displayed.
	!	.lm -5
	!
	! Index:
	!	.x Requisition Line>Issue Journal
	!	.x Issue Journal>Requisition Line
	!
	!--
			WP_ISSLINE::REQLINE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"01;25",TEMP$, WP_ISSLINE::REQLINE, &
				MFLAG, "~L0'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(WP_MAIN_REQREGISTER.ID, "V0" + &
					WP_ISSLINE::JOB + WP_ISSLINE::LLINE + &
					WP_ISSLINE::REQNUM) = 1%
				THEN
					WP_ISSLINE::REQLINE = &
						WP_REQREGISTER::REQLIN
				END IF
				GOTO Reenter
			END IF

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Product\*
	!	.lm +5
	!	.b
	!	The ^*Product\* field enters the
	!	inventory product number for material issued.
	!	.lm -5
	!
	! Index:
	!	.x Product Number>Issue Journal
	!	.x Issue Journal>Product Number
	!
	!--
			MFLAG = MFLAG OR 1% IF WP_ISSLINE::PROD_FLAG = "Y"

			WP_ISSLINE::PRODUCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"02;25",TEMP$, WP_ISSLINE::PRODUCT, &
				MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, "VX") = 1%
				THEN
					WP_ISSLINE::PRODUCT = &
						PD_PRODUCT::PRODUCT_NUM
				END IF
				GOTO Reenter
			END IF

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Issue Date\*
	!	.lm +5
	!	.b
	!	The ^*Issue Date\* field enters the
	!	date the material is issued.
	!	.b
	!	The format for this field is MMDDYY.
	!	.lm -5
	!
	! Index:
	!	.x Issue Date>Issue Journal
	!	.x Issue Journal>Issue Date
	!
	!--
			WP_ISSLINE::ISSDATE = &
				ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"03;25",TEMP$, WP_ISSLINE::ISSDATE, &
				MFLAG, "'E", MVALUE)

		CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Cost\*
	!	.lm +5
	!	.b
	!	The ^*Cost\* field displays the cost of the product
	!	represented by the inventory product number entered in field (02) Product.
	!	.b
	!	The field may contain a figure as large as 9,999,999.99.
	!	.lm -5
	!
	! Index:
	!	.x Issue Journal>Cost
	!	.x Cost>Issue Journal
	!
	!--
			WP_ISSLINE::COST = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"04;25",TEMP$, WP_ISSLINE::COST, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)

		CASE 5%
	!++
	! Abstract:FLD005
	!	^*(05) Quantity Issue\*
	!	.lm +5
	!	.b
	!	The ^*Quantity Issue\* field enters
	!	the number of units issued.
	!	.b
	!	The field may contain a figure as large as 9,999,999.
	!	.lm -5
	!
	! Index:
	!	.x Issued Journal>Quantity
	!	.x Quantity>Issue Journal
	!
	!--
			WP_ISSLINE::QTYISSUE = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"05;25",TEMP$, WP_ISSLINE::QTYISSUE, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)

		CASE 6%
	!++
	! Abstract:FLD006
	!	^*(06) Quantity Cancel\*
	!	.lm +5
	!	.b
	!	The ^*Quantity Cancel\* field
	!	enters the number of units cancelled on a requisition.
	!	.b
	!	The field may contain a figure as large as 9,999,999.
	!	.lm -5
	!
	! Index:
	!	.x Cancelled Quantity>Issue Journal
	!	.x Quantity Cancelled>Issue Journal
	!	.x Issue Journal>Quantity Cancelled
	!
	!--
			WP_ISSLINE::QTYCANCEL = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"06;25",TEMP$, WP_ISSLINE::QTYCANCEL, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY

		WP_MAIN_ISSLINE = 0%

		SELECT MLOOP

		CASE 1%
			!
			! Try to fix up a line number
			!
			IF ((WP_ISSLINE::REQLINE = "") OR &
				(WP_ISSLINE::REQLINE = "0000")) AND &
				(MVALUE = "ADD")
			THEN
				GOSUB CalcLine
				JUNK% = WP_MAIN_ISSLINE(SMG_WINDOW, &
					OPT_ENTRY, 1%, 1%, "")
			END IF

			QTY(I%) = 0.0 FOR I% = 1% TO 6%

			!
			! Look up data in the WP_REQREGISTER file
			!
			IF WP_READ_REQREGISTER(WP_ISSLINE::JOB, &
				WP_ISSLINE::LLINE, WP_ISSLINE::REQNUM + &
				WP_ISSLINE::REQLINE, "EQ", &
				WP_REQREGISTER_READ, QTY()) = CMC$_NORMAL
			THEN
				WP_ISSLINE::PROD_FLAG = "Y"
				WP_ISSLINE::PRODUCT = &
					WP_REQREGISTER_READ::PRODUCT

				QTY(6%) = 0.0 IF QTY(6%) < 0.0

				WP_ISSLINE::QTYISSUE = QTY(6%)

			END IF

			MASKX$ = FORMAT$(QTY(1%), "#,###,###.##")

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				MASKX$, 9%, 30%,, SMG$M_BOLD)

			MASKX$ = FORMAT$(QTY(2%), "#,###,###.##")

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				MASKX$, 10%, 30%,, SMG$M_BOLD)

			MASKX$ = FORMAT$(QTY(6%), "#,###,###.##")

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				MASKX$, 11%, 30%,, SMG$M_BOLD)

		CASE 2%
			WP_MAIN_ISSLINE = FUNC_TESTENTRY(SMG_WINDOW, &
				WP_ISSLINE::PRODUCT, &
				PD_PRODUCT::DESCRIPTION, &
				"WP", MLOOP, "PROG", &
				"Product", PD_MAIN_PRODUCT.ID)

			WP_MAIN_ISSLINE = 0% IF	WP_ISSLINE::PROD_FLAG = "Y"

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_PRODUCT::DESCRIPTION, 2%, 45%,, SMG$M_BOLD)

		CASE 3%
			IF MVALUE = "ADD"
			THEN
				WP_ISSLINE::COST = &
					PC_READ_COST(WP_ISSLINE::PRODUCT, &
					LOCATION$, &
					WP_ISSLINE::ISSDATE, "")

				FLAG% = FUNC_TESTENTRY(SMG_WINDOW, &
					WP_ISSLINE::PRODUCT, &
					PD_PRODUCT::DESCRIPTION, &
					"WP", MLOOP, "PROG", &
					"Product", PD_MAIN_PRODUCT.ID)

				!
				! Adjust by product factor
				!
				IF PD_PRODUCT::PRODUCT_FACTOR <> 0.0 AND &
					FLAG% = 0%
				THEN
					WP_ISSLINE::COST = &
						FUNC_ROUND(WP_ISSLINE::COST / &
						PD_PRODUCT::PRODUCT_FACTOR, 4%)
				END IF

			END IF

		CASE 5%
			GOSUB GetRemain
			WP_ISSLINE::QTYRUN = RQ_BAL

			EXT_COST = FUNC_ROUND(WP_ISSLINE::COST * &
				WP_ISSLINE::QTYISSUE, 2%)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				FORMAT$(EXT_COST, "#,###,###.##"), &
				5%, 55%,, SMG$M_BOLD)

		CASE 6%
			GOSUB GetRemain
			WP_ISSLINE::QTYRUN = RQ_BAL

		END SELECT

	CASE OPT_DISPLAY
		IF (SMG_WINDOW::HFLAG(1%) AND 2%) = 0%
		THEN
			IF WP_READ_REQREGISTER(WP_ISSLINE::JOB, &
				WP_ISSLINE::LLINE, WP_ISSLINE::REQNUM + &
				WP_ISSLINE::REQLINE, "EQ", &
				WP_REQREGISTER_READ, QTY()) <> CMC$_NORMAL
			THEN
				QTY(I%) = 0.0 FOR I% = 1% TO 6%
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				WP_ISSLINE::PRODUCT, 2%, 25%,, SMG$M_BOLD)

			MASKX$ = FORMAT$(QTY(1%), "#,###,###.##")

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				MASKX$, 9%, 30%,, SMG$M_BOLD)

			MASKX$ = FORMAT$(QTY(2%) + &
				WP_ISSLINE::QTYISSUE, "#,###,###.##")

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				MASKX$, 10%, 30%,, SMG$M_BOLD)

			REMAIN = QTY(6%) - &
				WP_ISSLINE::QTYISSUE - WP_ISSLINE::QTYCANCEL
			REMAIN = 0.0 IF REMAIN < 0.0

			MASKX$ = FORMAT$(REMAIN, "#,###,###.##")

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				MASKX$, 11%, 30%,, SMG$M_BOLD)

			EXT_COST = FUNC_ROUND(WP_ISSLINE::COST * &
				WP_ISSLINE::QTYISSUE, 2%)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				FORMAT$(EXT_COST, "#,###,###.##"), &
				5%, 55%,, SMG$M_BOLD)

		END IF

		IF (SMG_WINDOW::HFLAG(2%) AND 2%) = 0%
		THEN
			IF MAIN_WINDOW (PD_MAIN_PRODUCT.ID, "Q0" + &
				WP_ISSLINE::PRODUCT) <> 1%
			THEN
				PD_PRODUCT::DESCRIPTION = &
					STRING$(LEN(PD_PRODUCT::DESCRIPTION), &
					A"?"B)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_PRODUCT::DESCRIPTION, 2%, 45%,, SMG$M_BOLD)
		END IF


	!
	! Set WP_ISSLINE_OLD value
	!
20500	CASE OPT_SETOLD
		WP_ISSLINE_OLD = WP_ISSLINE

	!
	! Restore WP_ISSLINE_OLD value
	!
	CASE OPT_RESETOLD
		WP_ISSLINE = WP_ISSLINE_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		WP_ISSLINE_DEF = WP_ISSLINE

		IF MFLAG = 1%
		THEN
			SELECT MLOOP

			CASE 0%
				FRM$(4%) = "##,###,###.##"
				FRM$(5%) = "##,###,###"
				FRM$(6%) = "##,###,###"
			CASE ELSE
				FRM$(MLOOP) = MVALUE

			END SELECT
		END IF

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		WP_ISSLINE = WP_ISSLINE_DEF
		WP_ISSLINE::PROD_FLAG = "N"

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!
		WP_ISSLINE::REQNUM = MID$(MVALUE, 1%, LEN(WP_ISSLINE::REQNUM))

		WP_ISSLINE::JOB = MID$(MVALUE, 1% + LEN(WP_ISSLINE::REQNUM), &
			LEN(WP_ISSLINE::JOB))

		WP_ISSLINE::LLINE  = &
			MID$(MVALUE, 1% + LEN(WP_ISSLINE::REQNUM + &
			WP_ISSLINE::JOB), LEN(WP_ISSLINE::LLINE))

		!
		! Force something into the date
		!
		IF WP_ISSLINE::ISSDATE = ""
		THEN
			WP_ISSLINE::ISSDATE = DATE_TODAY
		END IF


	!
	! Find the next record
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #WP_ISSLINE.CH%, &
				KEY #0% GE WP_ISSLINE::REQNUM + &
				WP_ISSLINE::JOB + WP_ISSLINE::LLINE + &
				WP_ISSLINE::REQLINE, &
				REGARDLESS

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
			MVALUE = "  Line# Product        " + &
				"IssDate            Cost IssuedQty CancelQty"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "008,023,034,047,057"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = " " + WP_ISSLINE::REQLINE + " " + &
				WP_ISSLINE::PRODUCT + " " + &
				PRNT_DATE(WP_ISSLINE::ISSDATE, 8%) + &
				FORMAT$(WP_ISSLINE::COST, "##,###,###.##") + &
				FORMAT$(WP_ISSLINE::QTYISSUE, "##,###,###") + &
				FORMAT$(WP_ISSLINE::QTYCANCEL, "##,###,###")

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
			LAST_ISSLINE = "0000"

			!
			! Check if erasing the whole order's lines and if so
			! then reset the Register quantities correctly?
			!
			SELECT MFLAG

			CASE -1%

				V% = WP_READ_REQREGISTER(WP_ISSJOUR::JOB, &
					WP_ISSJOUR::LLINE, &
					WP_ISSJOUR::REQNUM + REQLIN$, &
					"EQ", WP_REQREGISTER_READ, QTY())

				LOCATION$ = WP_REQREGISTER_READ::LOCATION

			END SELECT

			!
			! Set init value
			!
			SMG_WINDOW::CURREC = -1%

27105			!
			! Calculate last line number
			!
			WHEN ERROR IN
				FIND #SMG_WINDOW::CHAN, &
					KEY #0% EQ MVALUE, &
					REGARDLESS
			USE
				CONTINUE 27110
			END WHEN

27107			WHEN ERROR IN
				GET #SMG_WINDOW::CHAN
			USE
				CONTINUE 27110
			END WHEN

			IF WP_ISSLINE::REQNUM + WP_ISSLINE::JOB + &
				WP_ISSLINE::LLINE = MVALUE
			THEN
				LAST_ISSLINE = WP_ISSLINE::REQLINE &
					IF LAST_ISSLINE < WP_ISSLINE::REQLINE
				GOTO 27107
			END IF

27110			!
			! Search for first record
			!
			WHEN ERROR IN
				FIND #SMG_WINDOW::CHAN, &
					KEY #0% EQ MVALUE, &
					REGARDLESS

				!
				! Get a record
				!
				GET #SMG_WINDOW::CHAN
			USE
				CONTINUE 28000
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
					FIND #SMG_WINDOW::CHAN, &
						KEY #0% GE MVALUE + &
						WP_ISSLINE::REQLINE, &
						REGARDLESS
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

			IF WP_ISSLINE::REQNUM + WP_ISSLINE::JOB + &
				WP_ISSLINE::LLINE = MVALUE
			THEN
				SMG_WINDOW::CURREC = 0%
			END IF

		!
		! Change key
		!
		CASE 6%
			WP_ISSLINE::REQNUM = MID$(MVALUE, 1%, 10%)
			WP_ISSLINE::JOB    = MID$(MVALUE, 11%, 10%)
			WP_ISSLINE::LLINE  = MID$(MVALUE, 21%, 4%)

		END SELECT

	CASE OPT_AFTEROPT

		SELECT MVALUE

		CASE "Add"

			GOSUB GetRemain

			V% = IC_WRIT_35BALANCE (WP_ISSLINE::PRODUCT, &
				LOCATION$, "RQ", &
				RQ_BAL)

			V% = IC_WRIT_35BALANCE (WP_ISSLINE::PRODUCT, &
				LOCATION$, "IS", &
				-WP_ISSLINE::QTYISSUE)

		CASE "Change", "Blank", "Initialize"

			GOSUB GetRemain

			V% = IC_WRIT_35BALANCE (WP_ISSLINE::PRODUCT, &
				LOCATION$, "RQ", &
				-WP_ISSLINE_OLD::QTYRUN)

			V% = IC_WRIT_35BALANCE (WP_ISSLINE::PRODUCT, &
				LOCATION$, "IS", &
				WP_ISSLINE_OLD::QTYISSUE)

			V% = IC_WRIT_35BALANCE (WP_ISSLINE::PRODUCT, &
				LOCATION$, "RQ", &
				RQ_BAL)

			V% = IC_WRIT_35BALANCE (WP_ISSLINE::PRODUCT, &
				LOCATION$, "IS", &
				-WP_ISSLINE::QTYISSUE)

		CASE "Erase"

			IF MLOOP <> 1%
			THEN
				V% = WP_READ_REQREGISTER(WP_ISSJOUR::JOB, &
					WP_ISSJOUR::LLINE, &
					WP_ISSJOUR::REQNUM + REQLIN$, &
					"EQ", WP_REQREGISTER_READ, QTY())

				LOCATION$ = WP_REQREGISTER_READ::LOCATION

				V% = IC_WRIT_35BALANCE (WP_ISSLINE::PRODUCT, &
					LOCATION$, "RQ", &
					-WP_ISSLINE::QTYRUN)

				V% = IC_WRIT_35BALANCE (WP_ISSLINE::PRODUCT, &
					LOCATION$, "IS", &
					WP_ISSLINE::QTYISSUE)

			END IF

		END SELECT

	END SELECT

 ExitFunction:
28000	EXIT FUNCTION

 LoadLines:
	!=====================================================================
	SOPTION(1%) = "Auto Issue Rem Qty"
	SOPTION(2%) = "Auto Cancel Rem Qty"
	SOPTION(3%) = "Manually Enter Lines"
	SOPTION(4%) = ""

 SelectSOption:
	X% = ENTR_3CHOICE(SCOPE, "", "", SOPTION(), "", &
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

	CALL ENTR_3MESSAGE(SCOPE, "Loading Issue Lines", 1% + 16%)

	REQLIN$ = SPACE$(LEN(WP_REQREGISTER_READ::REQLIN) + 1%)

 ReqLine:
	!
	! Get quantity from requisition register
	!
	GOTO ExitLoadLines IF WP_READ_REQREGISTER(WP_ISSJOUR::JOB, &
		WP_ISSJOUR::LLINE, &
		WP_ISSJOUR::REQNUM + REQLIN$, &
		"GT", WP_REQREGISTER_READ, QTY()) <> CMC$_NORMAL

	GOTO ExitLoadLines &
		IF WP_REQREGISTER_READ::REQNUM <> WP_ISSJOUR::REQNUM OR &
		WP_REQREGISTER_READ::LLINE <> WP_ISSJOUR::LLINE

	LOCATION$ = WP_REQREGISTER_READ::LOCATION
	REQLIN$ = WP_REQREGISTER_READ::REQLIN

	GOTO ReqLine IF QTY(6%) = 0.0

	!
	! Build most of issue line here from register line
	!
	WP_ISSLINE::JOB       = WP_ISSJOUR::JOB
	WP_ISSLINE::LLINE     = WP_ISSJOUR::LLINE
	WP_ISSLINE::REQNUM    = WP_ISSJOUR::REQNUM
	WP_ISSLINE::REQLINE   = WP_REQREGISTER_READ::REQLIN
	WP_ISSLINE::PRODUCT   = WP_REQREGISTER_READ::PRODUCT
	WP_ISSLINE::ISSDATE   = DATE_TODAY
	WP_ISSLINE::PROD_FLAG = "Y"

	V% = PD_EXAM_PRODUCT(WP_ISSLINE::PRODUCT, PD_PRODUCT_EXAM)

	WP_ISSLINE::COST = PC_READ_COST(WP_ISSLINE::PRODUCT, &
		LOCATION$, &
		WP_ISSLINE::ISSDATE, "")

	!
	! Adjust by product factor
	!
	IF PD_PRODUCT_EXAM::PRODUCT_FACTOR <> 0.0 AND V% = CMC$_NORMAL
	THEN
		WP_ISSLINE::COST = &
			FUNC_ROUND(WP_ISSLINE::COST / &
			PD_PRODUCT_EXAM::PRODUCT_FACTOR, 4%)
	END IF

	SELECT X%

	CASE 1%
		WP_ISSLINE::QTYCANCEL = 0.0
		WP_ISSLINE::QTYISSUE  = QTY(6%)
		WP_ISSLINE::QTYRUN    = QTY(6%)

		V% = IC_WRIT_35BALANCE(WP_ISSLINE::PRODUCT, &
			LOCATION$, "RQ", QTY(6%))

		V% = IC_WRIT_35BALANCE(WP_ISSLINE::PRODUCT, &
			LOCATION$, "IS", -QTY(6%))

	CASE 2%
		WP_ISSLINE::QTYCANCEL = QTY(6%)
		WP_ISSLINE::QTYISSUE  = 0.0
		WP_ISSLINE::QTYRUN    = QTY(6%)

		V% = IC_WRIT_35BALANCE(WP_ISSLINE::PRODUCT, &
			LOCATION$, "RQ", QTY(6%))
	END SELECT

28100	PUT #WP_ISSLINE.CH%

	GOTO ReqLine

 ExitLoadLines:
	CALL ENTR_3MESSAGE(SCOPE, "", 1%)

	RETURN

	%PAGE

	!===================================================================
	! Subroutine to calculate remaining quantities
	!===================================================================
 GetRemain:
	V% = WP_READ_REQREGISTER(WP_ISSJOUR::JOB, &
		WP_ISSJOUR::LLINE, &
		WP_ISSJOUR::REQNUM + WP_ISSLINE::REQLINE, &
		"EQ", WP_REQREGISTER_READ, QTY())

	LOCATION$ = WP_REQREGISTER_READ::LOCATION

	!
	! Calculate remaining on-order for new or current line
	!
	RQ_BAL = QTY(6%)

	RQ_BAL = WP_ISSLINE::QTYISSUE + WP_ISSLINE::QTYCANCEL &
		IF ABS(WP_ISSLINE::QTYISSUE + WP_ISSLINE::QTYCANCEL) < &
		ABS(RQ_BAL) AND ABS(QTY(1%)) <> 0.0

	RETURN

 CalcLine:
28300	!*******************************************************************
	! Subroutine to look for the next available line number
	!*******************************************************************

	LAST_REQLINE$ = LAST_ISSLINE

	IF WP_REQREGISTER.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.OPN"
		USE
			CONTINUE 28360
		END WHEN
	END IF

28310	WHEN ERROR IN
		FIND #WP_REQREGISTER.CH%, KEY #0% EQ WP_ISSLINE::JOB + &
			WP_ISSLINE::LLINE + WP_ISSLINE::REQNUM, REGARDLESS
	USE
		CONTINUE 28360
	END WHEN

	!
	! Get WP_REQREGISTER file
	!
28320	WHEN ERROR IN
		GET #WP_REQREGISTER.CH%, REGARDLESS
	USE
		CONTINUE 28360
	END WHEN

	GOTO 28360 IF WP_REQREGISTER::JOB <> WP_ISSLINE::JOB OR &
		WP_REQREGISTER::LLINE <> WP_ISSLINE::LLINE OR &
		WP_REQREGISTER::REQNUM <> WP_ISSLINE::REQNUM

	IF LAST_REQLINE$ < WP_REQREGISTER::REQLIN
	THEN
		LAST_REQLINE$ = WP_REQREGISTER::REQLIN
	END IF

	GOTO 28320

28360	JUNK% = FUNC_INCREMENT(LAST_REQLINE$)
	WP_ISSLINE::REQLINE = LAST_REQLINE$
	LAST_ISSLINE = LAST_REQLINE$

	RETURN

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK


32767	END FUNCTION
	!+-+-+
	!++
	! Warning:UNDORDLIN
	!	^*Undefined Order Line\*
	!	.b
	!	.lm +5
	!	^*Explanation\*
	!	.b
	!	Selected order line doesn't exist in the Order Register
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
	!+-+-+
	!++
	! Abstract:FLD007
	!	^*(07) WIP Account\*
	!	.b
	!	.lm +5
	!	The ^*WIP Account\* field enters the account number
	!	the issue is to be debited against.
	!	.lm -5
	!
	! Index:
	!	.x WIP Account
	!
	!--
