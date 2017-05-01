1	%TITLE "Maintain Purchase Order Reciept Line"
	%SBTTL "PO_MAIN_RECLINE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PO_MAIN_RECLINE(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS PO_SOURCE:PO_MAIN_RECLINE/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP PO_MAIN_RECLINE
	!	$ DELETE PO_MAIN_RECLINE.OBJ;*
	!
	! Author:
	!
	!	02/14/92 - Dan Perkins
	!
	! Modification history:
	!
	!	02/24/92 - Kevin Handy
	!		Cleaned up (check)
	!
	!	03/03/92 - Kevin Handy
	!		Changed "CMC$NORMAL" to "CMC$_NORMAL"
	!
	!	03/12/92 - Kevin Handy
	!		Cleaned up (check)
	!
	!	03/12/92 - Dan Perkins
	!		Added F-14 key to PO number.  Fill Product,
	!		Description, UOM, and RECQTY fields if line is in
	!		register.
	!
	!	03/13/92 - Dan Perkins
	!		Automatically load lines.
	!
	!	03/17/92 - Dan Perkins
	!		Added formatting option to numbers - RECQTY, and
	!		CANQTY.
	!
	!	04/28/92 - Kevin Handy
	!		Clean up (check)
	!
	!	11/25/92 - Dan Perkins
	!		Added CASE 2 to OPT_SUBWIND so that VIEW would
	!		work properly.
	!
	!	11/04/94 - Kevin Handy
	!		Added parameter to PO_READ_REGLINE
	!
	!	04/14/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards.
	!		Change last param on entr_3choices from "" to 0%
	!
	!	07/12/95 - Kevin Handy
	!		Modified to handle Running Balance (On Order)
	!		the same way as the PO_POST_RECEIVE program does.
	!		(LL)
	!
	!	10/20/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/19/98 - Kevin Handy
	!		Modified to check for autocreate where must be
	!		greater than a near non-zero rather than zero.
	!		(Trying to lose extra lines with zero qty's)
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE's
	!
	!	03/10/99 - Kevin Handy
	!		Clean up SOPTION array
	!
	!	11/21/2000 - Kevin Handy
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
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PO_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[PO.OPEN]PO_RECJOUR.HB"
	MAP (PO_RECJOUR)	PO_RECJOUR_CDD		PO_RECJOUR

	%INCLUDE "SOURCE:[PO.OPEN]PO_RECLINE.HB"
	MAP (PO_RECLINE)	PO_RECLINE_CDD		PO_RECLINE
	MAP (PO_RECLINE_OLD)	PO_RECLINE_CDD		PO_RECLINE_OLD, PO_RECLINE2

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_MEASURE.HB"
	MAP (UTL_MEASURE)	UTL_MEASURE_CDD		UTL_MEASURE

	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.HB"
	MAP (PO_REG_LINE)	PO_REG_LINE_CDD		PO_REG_LINE
	DECLARE			PO_REG_LINE_CDD		PO_REG_LINE_READ

	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_SUB_LINE.HB"
	DECLARE			PO_REG_SUB_LINE_CDD	PO_REG_SUB_LINE_READ

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_PO_RECJOUR) &
		BATCH_NO$ = 2%, &
		PO_RECJOUR.CH%, &
		PO_RECJOUR.READONLY%

	COM (CH_PO_RECLINE) &
		PO_RECLINE.CH%, &
		PO_RECLINE.READONLY%

	COM (CH_PO_REG_LINE) &
		PO_REG_LINE.CH%, &
		PO_REG_LINE.READONLY%

	!
	! External functions
	!
	EXTERNAL	LONG	FUNCTION FUNC_TESTENTRY
	EXTERNAL	LONG	FUNCTION PO_READ_REG_LINE
	EXTERNAL	LONG	FUNCTION IC_WRIT_35BALANCE

	DIM STRING SOPTION(4%)

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
		SMG_WINDOW::DESCR  = "Line items"
		SMG_WINDOW::NHELP  = "PO_MAIN_RECLINE"
		SMG_WINDOW::CURREC = -2%
		SMG_WINDOW::HSIZE  = 74%
		SMG_WINDOW::VSIZE  =  8%
		SMG_WINDOW::HPOS   =  3%
		SMG_WINDOW::VPOS   =  12%
		SMG_WINDOW::NITEMS =  6%
		SMG_WINDOW::FLAGS  =  0%
		SMG_WINDOW::HVIEW  = 74%
		SMG_WINDOW::VVIEW  =  8%
		SMG_WINDOW::VHPOS  =  3%
		SMG_WINDOW::VVPOS  =  12%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Line_number"
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
		IF PO_RECLINE.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PO_RECLINE.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PO.OPEN]PO_RECLINE.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PO_MAIN_RECLINE = ERR
			CONTINUE 770
		END WHEN

		PO_RECLINE.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PO.OPEN]PO_RECLINE.OPN"
		USE
			PO_MAIN_RECLINE = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PO_RECLINE.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PO_RECLINE.CH%)

		EXIT FUNCTION

790		GOSUB LoadLines IF MVALUE = "A"
		SMG_WINDOW::CHAN  = PO_RECLINE.CH%
		WHEN ERROR IN
			RESET #PO_RECLINE.CH%
			GET #PO_RECLINE.CH%, REGARDLESS
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


		DATA	2, 2, "(01) Line#", &
			3, 2, "(02) Product", &
			4, 2, "(03) Description", &
			5, 2, "(04) Unit of Measure", &
			6, 2, "(05) Qty Received", &
			7, 2, "(06) Qty Canceled", &
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
	!	^*(01) Line Number\*
	!	.p
	!	The ^*Line Number\* field is used to identify each line item in the body of a
	!	Purchase Order.  This number is a key number used to record receipts in the
	!	Receiving Journal and to record invoices in the Accounts Payable Purchases
	!	Journal.  Consequently, the system tracks products ordered, received, and
	!	invoiced.
	!	.p
	!	Pressing the ^*List Choices\* key while the cursor is positioned on
	!	this field will cause the Line Numbers which are currently
	!	in the register to be displayed.
	!	.p
	!	This field will accept up to four (4) characters.
	!
	! Index:
	!
	!--
			PO_RECLINE::PO_LINE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "2;24", TEMP$, &
				PO_RECLINE::PO_LINE, MFLAG, "~L0'LLL", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF MAIN_WINDOW(PO_MAIN_REGLINE.ID, "VX") = 1%
				THEN
					PO_RECLINE::PO = &
						PO_REG_LINE::PO
				END IF
				GOTO E0Loop
			END IF

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Product Number\*
	!	.p
	!	The ^*Product Number\* field records the stock number used by the user to
	!	identify the item being ordered. If no part number is specified here, this
	!	line will not be interfaced into the Inventory System.
	!	.p
	!	Pressing the ^*List Choices\* key while the cursor is positioned on
	!	this field will cause the defined Product Numbers to be displayed.
	!	.p
	!	This field accepts up to fourteen (14) alphanumeric characters.
	!
	! Index:
	!
	!--
			MFLAG = MFLAG OR 1% IF PO_RECLINE::LINEFLAG = "Y"

			PO_RECLINE::PRODUCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "3;24", TEMP$, &
				PO_RECLINE::PRODUCT, MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, "VX") = 1%
				THEN
					PO_RECLINE::PRODUCT = &
						PD_PRODUCT::PRODUCT_NUM
				END IF
				GOTO E0Loop
			END IF

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Description\*
	!	.p
	!	The ^*Description\* field describes the item being ordered.  The description
	!	is automatically displayed if the stock number for the item is identified, i.e.
	!	the item is recorded in the Inventory Control system.  If the item is not
	!	identified, the user may manually enter a description in this field.
	!	.p
	!	This field will accommodate up to forty (40) alphanumeric characters.
	!
	! Index:
	!
	!--
			MFLAG = MFLAG OR 1% IF PO_RECLINE::LINEFLAG = "Y"

			PO_RECLINE::DESCRIPTION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "4;24", TEMP$, &
				PO_RECLINE::DESCRIPTION, MFLAG, "'E", MVALUE)

		CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Unit Of Measure\*
	!	.p
	!	The ^*Unit Of Measure\* field should contain the unit of measure used by
	!	the user relative to the item being ordered.
	!	.p
	!	This field will default to the unit of measure defined in the
	!	product description file and automatically display that default.
	!	.p
	!	Pressing the ^*List Choices\* key while the cursor is positioned on
	!	this field will cause the defined Units Of Measure to be displayed.
	!	.p
	!	This field will contain two (2) alphanumeric characters.
	!
	! Index:
	!
	!--
			MFLAG = MFLAG OR 1% IF PO_RECLINE::LINEFLAG = "Y"

			PO_RECLINE::UOM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "5;24", TEMP$, &
				PO_RECLINE::UOM, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UTL_MAIN_MEASURE.ID, "VX") = 1%
				THEN
					PO_RECLINE::UOM = &
						UTL_MEASURE::CODE
				END IF
				GOTO E0Loop
			END IF

		CASE 5%
	!++
	! Abstract:FLD005
	!	^*(05) Quantity Received\*
	!	.p
	!	The ^*Quantity Received\* field enters the
	!	quantity or number of items being received.
	!	.p
	!	This field will accept a number as large as 9,999,999.99
	!
	! Index:
	!
	!--
			PO_RECLINE::RECQTY = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "6;24", TEMP$, &
				PO_RECLINE::RECQTY, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)

		CASE 6%
	!++
	! Abstract:FLD006
	!	^*(06) Quantity Cancelled\*
	!	.p
	!	The ^*Quantity Cancelled\* field enters the
	!	quantity or number of items being canceled.
	!	.p
	!	This field will accept a number as large as 9,999,999.99
	!
	! Index:
	!
	!--
			PO_RECLINE::CANQTY = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "7;24", TEMP$, &
				PO_RECLINE::CANQTY, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY

		SELECT MLOOP

		CASE 1%
			IF MVALUE = "ADD"
			THEN
				WHEN ERROR IN
					GET #PO_RECLINE.CH%, &
						KEY #0% EQ PO_RECLINE::PO + &
						PO_RECLINE::PO_LINE, REGARDLESS
				USE
					CONTINUE 20310 IF ERR = 155%
					EXIT HANDLER
				END WHEN

				GOTO 28000
			END IF

20310			PO_RECLINE::LINEFLAG = "N"

			IF PO_READ_REG_LINE(PO_RECLINE::PO, &
				PO_RECLINE::PO_LINE, "EQ", &
				PO_REG_LINE_READ, PO_REG_SUB_LINE_READ, &
				QTY(), "") = CMC$_NORMAL
			THEN
				PO_RECLINE::LINEFLAG = "Y"

				PO_RECLINE::PRODUCT = &
					PO_REG_LINE_READ::PRODUCT

				PO_RECLINE::DESCRIPTION = &
					PO_REG_LINE_READ::DESCRIPTION

				PO_RECLINE::UOM = &
					PO_REG_LINE_READ::UOM

				PO_RECLINE::RECQTY = QTY(0%)

			END IF

		CASE 2%
			!
			! Is the input defined?
			!
			IF MVALUE = "ADD" AND PO_RECLINE::PO_LINE = "0000"
			THEN
				GOSUB FindProduct
			END IF

			IF PO_RECLINE::PRODUCT <> "" AND PO_RECLINE::LINEFLAG = "N"
			THEN
				PO_MAIN_RECLINE = FUNC_TESTENTRY(SMG_WINDOW, &
					PO_RECLINE::PRODUCT, PD_PRODUCT::PRODUCT_NUM, &
					"PD", MLOOP, "PRODUCT", &
					"Product number", PD_MAIN_PRODUCT.ID)

				PO_RECLINE::DESCRIPTION = PD_PRODUCT::DESCRIPTION
				PO_RECLINE::UOM = PD_PRODUCT::UOM

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					PO_RECLINE::DESCRIPTION, 4%, 24%,, SMG$M_BOLD)

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					PO_RECLINE::UOM, 5%, 24%,, SMG$M_BOLD)
			END IF

		CASE 3%
			IF PO_RECLINE::DESCRIPTION = "" AND PO_RECLINE::LINEFLAG = "N"
			THEN
				PO_MAIN_RECLINE = 1%
			END IF

		CASE 4%
			IF PO_RECLINE::LINEFLAG = "N"
			THEN
				PO_MAIN_RECLINE = FUNC_TESTENTRY(SMG_WINDOW, &
					PO_RECLINE::UOM, UTL_MEASURE::CODE, &
					"UTL", MLOOP, "UOM", &
					"Unit of Measure", UTL_MAIN_MEASURE.ID)
			END IF

		END SELECT

	!
	! Set PO_RECLINE_OLD value
	!
20500	CASE OPT_SETOLD
		PO_RECLINE_OLD = PO_RECLINE

	!
	! Restore PO_RECLINE_OLD value
	!
	CASE OPT_RESETOLD
		PO_RECLINE = PO_RECLINE_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PO_RECLINE2 = PO_RECLINE

		IF MFLAG = 1%
		THEN
			SELECT MLOOP
			CASE 0%
				FRM$(5%)="#,###,###"
				FRM$(6%)="#,###,###"
			CASE ELSE
				FRM$(MLOOP) = MVALUE
			END SELECT
		END IF

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PO_RECLINE = PO_RECLINE2

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!
		PO_RECLINE::PO = MVALUE

	!
	! Find
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #PO_RECLINE.CH%, &
				KEY #0% GE PO_RECLINE::PO + &
				PO_RECLINE::PO_LINE, REGARDLESS

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
			MVALUE = "  Line Product        "    + &
				"Description               " + &
				"UOM    RecQty    CanQty"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "007,022,048,052,062"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = PO_RECLINE::PO_LINE + " "  + &
				PO_RECLINE::PRODUCT + " "  + &
				LEFT(PO_RECLINE::DESCRIPTION, 25%) + " "  + &
				PO_RECLINE::UOM + "  " + &
				FORMAT$(PO_RECLINE::RECQTY, "#,###,###") + " "  + &
				FORMAT$(PO_RECLINE::CANQTY, "#,###,###")
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
				GOSUB GetRemain

				V% = IC_WRIT_35BALANCE (PO_RECLINE::PRODUCT, &
					INVLOCATION$, "PO", ALLOCATE_QTY)

				V% = IC_WRIT_35BALANCE (PO_RECLINE::PRODUCT, &
					INVLOCATION$, "RE", -PO_RECLINE::RECQTY)
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
				WHEN ERROR IN
					FIND #PO_RECLINE.CH%, &
						KEY #0% GE MVALUE + &
						PO_RECLINE::PO_LINE, REGARDLESS
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

			IF PO_RECLINE::PO = MVALUE
			THEN
				SMG_WINDOW::CURREC = 0%
			END IF

		!
		! Change key
		!
		CASE 6%
			PO_RECLINE::PO = MVALUE

		END SELECT

	CASE OPT_AFTEROPT

		SELECT MVALUE

		CASE "Add"
			GOSUB GetRemain

			V% = IC_WRIT_35BALANCE (PO_RECLINE::PRODUCT, &
				INVLOCATION$, "PO", -ALLOCATE_QTY)

			V% = IC_WRIT_35BALANCE (PO_RECLINE::PRODUCT, &
				INVLOCATION$, "RE", PO_RECLINE::RECQTY)

		CASE "Change", "Blank", "Initialize"
			GOSUB GetRemainOld

			V% = IC_WRIT_35BALANCE (PO_RECLINE::PRODUCT, &
				INVLOCATION$, "PO", ALLOCATE_QTY)

			V% = IC_WRIT_35BALANCE (PO_RECLINE::PRODUCT, &
				INVLOCATION$, "RE", -PO_RECLINE_OLD::RECQTY)

			GOSUB GetRemain

			V% = IC_WRIT_35BALANCE (PO_RECLINE::PRODUCT, &
				INVLOCATION$, "PO", -ALLOCATE_QTY)

			V% = IC_WRIT_35BALANCE (PO_RECLINE::PRODUCT, &
				INVLOCATION$, "RE", PO_RECLINE::RECQTY)

		CASE "Erase"
			IF MLOOP <> 1%
			THEN
				GOSUB GetRemain

				V% = IC_WRIT_35BALANCE(PO_RECLINE::PRODUCT, &
					INVLOCATION$, "PO", ALLOCATE_QTY)

				V% = IC_WRIT_35BALANCE(PO_RECLINE::PRODUCT, &
					INVLOCATION$, "RE", -PO_RECLINE::RECQTY)
			END IF

		END SELECT

	END SELECT

28000	EXIT FUNCTION

 LoadLines:
	!=======================================================================
	SOPTION(0%) = "3"
	SOPTION(1%) = "Auto Receive Rem Qty"
	SOPTION(2%) = "Auto Cancel Rem Qty"
	SOPTION(3%) = "Manually Enter Lines"

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

	CALL ENTR_3MESSAGE(SCOPE, "Loading Lines", 1% + 16%)

	POLIN$ = "    "

 ReqLine:
	! Get quantity from requisition register
	!
	IF PO_READ_REG_LINE(PO_RECJOUR::PO, POLIN$, "GT", &
		PO_REG_LINE_READ, PO_REG_SUB_LINE_READ, &
		QTY(), "") <> CMC$_NORMAL
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "", 1%)
		GOTO ExitLoadLines
	END IF

	POLIN$ = PO_REG_LINE_READ::PO_LINE

	GOTO ReqLine IF QTY(0%) <= 0.00001

	!
	! Build most of issue line here from register line
	!
	PO_RECLINE::PO		= PO_RECJOUR::PO
	PO_RECLINE::PO_LINE	= PO_REG_LINE_READ::PO_LINE
	PO_RECLINE::PRODUCT	= PO_REG_LINE_READ::PRODUCT
	PO_RECLINE::DESCRIPTION = PO_REG_LINE_READ::DESCRIPTION
	PO_RECLINE::UOM		= PO_REG_LINE_READ::UOM

	SELECT X%
	CASE 1%
		PO_RECLINE::CANQTY = 0.0
		PO_RECLINE::RECQTY = QTY(0%)
	CASE 2%
		PO_RECLINE::CANQTY = QTY(0%)
		PO_RECLINE::RECQTY = 0.0
	END SELECT

28100	PUT #PO_RECLINE.CH%

	V% = IC_WRIT_35BALANCE (PO_RECLINE::PRODUCT, &
		PO_REG_LINE_READ::FROMLOCATION, "PO", &
		-QTY(0%))

	V% = IC_WRIT_35BALANCE (PO_RECLINE::PRODUCT, &
		PO_REG_LINE_READ::FROMLOCATION, "RE", &
		PO_RECLINE::RECQTY)

	GOTO ReqLine

 ExitLoadLines:
	RETURN

	%PAGE

 FindProduct:
28200	IF PO_REG_LINE.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.OPN"
		USE
			CONTINUE ExitFindProduct
		END WHEN
	END IF

	WHEN ERROR IN
		GET #PO_REG_LINE.CH%, KEY #4% EQ PO_RECLINE::PRODUCT + &
			PO_RECLINE::PO, REGARDLESS
	USE
		CONTINUE ExitFindProduct
	END WHEN

	PO_RECLINE::LINEFLAG	= "Y"
	PO_RECLINE::PO_LINE	= PO_REG_LINE::PO_LINE
	PO_RECLINE::PRODUCT	= PO_REG_LINE::PRODUCT
	PO_RECLINE::DESCRIPTION	= PO_REG_LINE::DESCRIPTION
	PO_RECLINE::UOM		= PO_REG_LINE::UOM

	V% = PO_READ_REG_LINE(PO_RECLINE::PO, &
		PO_RECLINE::PO_LINE, "EQ", &
		PO_REG_LINE_READ, PO_REG_SUB_LINE_READ, &
		QTY(), "")

	PO_RECLINE::RECQTY = QTY(0%)
	PO_RECLINE::CANQTY = 0.0

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
		PO_RECLINE::PO_LINE, 2%, 24%,, SMG$M_BOLD)

 ExitFindProduct:
	RETURN

	%PAGE

28500
 GetRemain:
	EXIT_STATUS = PO_READ_REG_LINE(PO_RECLINE::PO, PO_RECLINE::PO_LINE, &
		"EQ", PO_REG_LINE_READ, PO_REG_SUB_LINE_READ, QTY(), "")

	INVLOCATION$ = PO_REG_LINE_READ::FROMLOCATION

	!
	! Calculate remaining on-order for new or current line
	!
	TEST_BALANCE = QTY(1%) - QTY(2%) - QTY(3%)
	IF TEST_BALANCE > 0.0
	THEN
		BEFORE_BALANCE = TEST_BALANCE
	ELSE
		BEFORE_BALANCE = 0.0
	END IF

	IF BEFORE_BALANCE > QTY(1%)
	THEN
		BEFORE_BALANCE = QTY(1%)
	END IF

	AFTER_BALANCE = TEST_BALANCE - PO_RECLINE::RECQTY
	IF AFTER_BALANCE < 0.0
	THEN
		AFTER_BALANCE = 0.0
	END IF

	IF AFTER_BALANCE > QTY(1%)
	THEN
		AFTER_BALANCE = QTY(1%)
	END IF

	ALLOCATE_QTY = BEFORE_BALANCE - AFTER_BALANCE

 !	ALLOCATE_QTY = QTY(0%)
 !
 !	ALLOCATE_QTY = (PO_RECLINE::RECQTY + PO_RECLINE::CANQTY) &
 !		IF (PO_RECLINE::RECQTY + PO_RECLINE::CANQTY) <= ALLOCATE_QTY

	RETURN

 GetRemainOld:
	EXIT_STATUS = PO_READ_REG_LINE(PO_RECLINE_OLD::PO, PO_RECLINE_OLD::PO_LINE, &
		"EQ", PO_REG_LINE_READ, PO_REG_SUB_LINE_READ, QTY(), "")

	INVLOCATION$ = PO_REG_LINE_READ::FROMLOCATION

	!
	! Calculate remaining on-order for old qty's shipped and cancelled
	!
	TEST_BALANCE = QTY(1%) - QTY(2%) - QTY(3%)
	IF TEST_BALANCE > 0.0
	THEN
		BEFORE_BALANCE = TEST_BALANCE
	ELSE
		BEFORE_BALANCE = 0.0
	END IF

	IF BEFORE_BALANCE > QTY(1%)
	THEN
		BEFORE_BALANCE = QTY(1%)
	END IF

	AFTER_BALANCE = TEST_BALANCE - PO_RECLINE::RECQTY + &
		PO_RECLINE_OLD::RECQTY
	IF AFTER_BALANCE < 0.0
	THEN
		AFTER_BALANCE = 0.0
	END IF

	IF AFTER_BALANCE > QTY(1%)
	THEN
		AFTER_BALANCE = QTY(1%)
	END IF

	ALLOCATE_QTY = BEFORE_BALANCE - AFTER_BALANCE

 !	ALLOCATE_QTY = QTY(0%)
 !
 !	ALLOCATE_QTY = (PO_RECLINE_OLD::RECQTY + PO_RECLINE_OLD::CANQTY) &
 !		IF (PO_RECLINE_OLD::RECQTY + PO_RECLINE_OLD::CANQTY) <= &
 !		ALLOCATE_QTY

	RETURN

	%PAGE

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
