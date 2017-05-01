1	%TITLE "Requisition Line Journal"
	%SBTTL "WP_MAIN_REQLINE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG WP_MAIN_REQLINE(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	The Requisition Line Journal enters the
	!	inventory products required in manufacturing to complete a job.
	!	.lm -5
	!
	! Index:
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_MAIN_REQLINE/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN WP_MAIN_REQLINE
	!	$ DELETE WP_MAIN_REQLINE.OBJ;*
	!
	! Author:
	!
	!	07/19/91 - Jeff Beard
	!
	! Modification history:
	!
	!	02/27/92 - Kevin Handy
	!		Changed "SOPTION(" to "SOPTION$(" so that the
	!		function would compile without syntax errors.
	!
	!	02/27/92 - Kevin Handy
	!		Added "EXTERNAL ENTR_3CHOICE".
	!
	!	02/27/92 - Kevin Handy
	!		Added "EXTERNAL WP_READ_REGISTER".
	!		And it still doesn't compile due to syntax errors.
	!
	!	04/03/92 - Dan Perkins
	!		Added more map statements, commented out WP_REQLINE
	!		fields which were incorrect.  Program may compile,
	!		but it is a genuine bet that it won't do whatever
	!		it was supposed to do.
	!
	!	06/12/92 - Kevin Handy
	!		Clean up (check)
	!
	!	07/22/92 - Danster and Frankster
	!		Added a whole bunch of junk in LOADLINE.
	!
	!	08/26/92 - Dan Perkins
	!		Added IC_WRIT_35BALANCE to inform inventory of
	!		goings on.
	!
	!	09/01/92 - Kevin Handy
	!		Clean up (check)
	!
	!	09/08/92 - Dan Perkins
	!		Added IC_WRIT_35BALANCE to inform inventory of
	!		goings on inside of LoadLines.
	!
	!	09/22/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	09/25/92 - Dan Perkins
	!		Display product description when loading lines.
	!
	!	09/30/92 - Dan Perkins
	!		Check product type with product types defined in
	!		BM_CONTROL file when loading lines.
	!
	!	10/26/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	11/10/92 - Dan Perkins
	!		Added CASE 2 to OPT_SUBWIND so that VIEW would
	!		work properly.
	!
	!	11/17/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	12/04/92 - Frank F. Starman
	!		Added FUNC_ROUND to REM.QTY calculation.
	!
	!	01/14/93 - Frank F. Starman
	!		Modify to allow call this function from WO header.
	!
	!	02/02/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	03/24/93 - Frank F. Starman
	!		Use Product_factor for autoload.
	!
	!	05/18/93 - Dan Perkins
	!		Change variable in IC_WRITE at line 28110 from
	!		BM_RELATION::COMPONENT to WP_REQLINE::PRODUCT.
	!
	!	06/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/13/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards.
	!		Change last param on entr_3choice from "" to 0%.
	!
	!	07/27/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!
	!	10/31/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/18/98 - Kevin Handy
	!		Change batch number (file) from 7 to 8 characters.
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/10/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	07/28/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	07/31/2000 - Kevin Handy
	!		Correctly handle PD_PRODUCT::PRODUCT_FACTOR
	!		Lose lots of commented out code.
	!		Display BOMUOM instead of UOM.
	!
	!	08/07/2000 - Kevin Handy
	!		Don't use PRODUCT_FACTOR to set up REQLINE::QTY,
	!		from BM, since it should already have been handled.
	!
	!	08/29/2000 - Kevin Handy
	!		Lots more work on factor conversions.
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
	%INCLUDE "FUNC_INCLUDE:WP_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[WP.OPEN]WP_REQLINE.HB"
	MAP (WP_REQLINE)	WP_REQLINE_CDD		WP_REQLINE
	MAP (WP_REQLINE_OLD)	WP_REQLINE_CDD		WP_REQLINE_OLD, &
							WP_REQLINE_DEF

	%INCLUDE "SOURCE:[WP.OPEN]WP_REQJOUR.HB"
	MAP (WP_REQJOUR)	WP_REQJOUR_CDD		WP_REQJOUR

	%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.HB"
	DECLARE			WP_REGLINE_CDD		WP_REGLINE_READ

	%INCLUDE "SOURCE:[JC.OPEN]JC_JOB.HB"
	MAP (SB_SUBACCOUNT)	JC_JOB_CDD		JC_JOB

	%INCLUDE "SOURCE:[WP.OPEN]WP_JOB.HB"
	MAP (WP_JOB)		WP_JOB_CDD		WP_JOB

	%INCLUDE "SOURCE:[WP.OPEN]WP_ORDERLINE.HB"
	MAP (WP_ORDERLINE)	WP_ORDERLINE_CDD	WP_ORDERLINE

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	%INCLUDE "SOURCE:[BM.OPEN]BM_RELATION.HB"
	MAP (BM_RELATION)	BM_RELATION_CDD		BM_RELATION
	DECLARE			BM_RELATION_CDD		BM_RELATION_ORIG

	%INCLUDE "SOURCE:[BM.OPEN]BM_CONTROL.HB"
	MAP (BM_CONTROL)	BM_CONTROL_CDD		BM_CONTROL

	!
	! Common Statements
	!
	COM (BATCH_NO) &
		BATCH_NO$ = 8%

	COM (CH_BM_RELATION_READ) &
		BM_RELATION.CH%

	COM (CH_BM_CONTROL_READ) &
		BM_CONTROL.CH%

	COM (CH_WP_REQJOUR) &
		WP_REQJOUR.CH%

	COM (CH_WP_REQLINE) &
		WP_REQLINE.CH%

	COM (X) X%

	RECORD COMPONENT_RECORD
		STRING NUMBER = 14%, &
		REAL   QUANTITY
	END RECORD

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION FUNC_TESTENTRY
	EXTERNAL LONG	FUNCTION WP_READ_REGLINE
	EXTERNAL LONG	FUNCTION IC_READ_35BALANCE
	EXTERNAL LONG   FUNCTION IC_WRIT_35BALANCE
	EXTERNAL LONG   FUNCTION PD_EXAM_PRODUCT

	DIM COMPONENT_RECORD	COMPONENT(5000%)
	DIM RFA			RFA_LEVEL(200%)
	DIM REAL		QTY_LEVEL(200%)
	DIM STRING		TEST_PRODUCT(200%)

	DECLARE LONG SMG_X2

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR  = "Material Requisition Lines"
		SMG_WINDOW::CURREC = -2%
		SMG_WINDOW::NHELP  = "WP_MAIN_REQLINE"
		SMG_WINDOW::HSIZE  = 76%
		SMG_WINDOW::VSIZE  = 6%
		SMG_WINDOW::HPOS   = 3%
		SMG_WINDOW::VPOS   = 12%
		SMG_WINDOW::NITEMS = 5%
		SMG_WINDOW::FLAGS  = 0%
		SMG_WINDOW::HVIEW  = 77%
		SMG_WINDOW::VVIEW  = 7%
		SMG_WINDOW::VHPOS  = 3%
		SMG_WINDOW::VVPOS  = 12%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Operation"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		COM (WP_MAIN_REQLINE_FRM) FRM$(5%)

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

		!
		! Open BM_CONTROL channel
		!
680		IF BM_CONTROL.CH% <= 0%
		THEN
			WHEN ERROR IN
				%INCLUDE "SOURCE:[BM.OPEN]BM_CONTROL.OPN"
				GET #BM_CONTROL.CH%, RECORD 1%, REGARDLESS
				CLOSE #BM_CONTROL.CH%
			USE
				CONTINUE 690 IF ERR = 5%
				EXIT HANDLER
			END WHEN

			CALL ASSG_FREECHANNEL(BM_CONTROL.CH%)
		END IF

		!
		! Open BM_RELATION channel
		!
690		IF BM_RELATION.CH% <= 0%
		THEN
			WHEN ERROR IN
				%INCLUDE "SOURCE:[BM.OPEN]BM_RELATION.OPN"
			USE
				CONTINUE 700 IF ERR = 5%
				EXIT HANDLER
			END WHEN
		END IF

		!
		! Declare channels
		!
700		IF WP_REQLINE.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF WP_REQLINE.READONLY%

			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[WP.OPEN]WP_REQLINE.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			WP_MAIN_REQLINE = ERR
			CONTINUE 770
		END WHEN

		WP_REQLINE.READONLY% = 0%
		GOTO 790

		!
		! If unable to open for modify, try to open with read
		! access only.
		!
760		WHEN ERROR IN
			%INCLUDE "SOURCE:[WP.OPEN]WP_REQLINE.OPN"
		USE
			WP_MAIN_REQLINE = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		WP_REQLINE.READONLY% = -1%

		GOTO 790

		!
		! File not open, so reset channel
		!
770		CALL ASSG_FREECHANNEL(WP_REQLINE.CH%)

		EXIT FUNCTION

790		IF MVALUE = "A"
		THEN
			GOSUB LoadLines
			IF X% = 1% OR X% = 2% OR X% = 3%
			THEN
				WP_MAIN_REQLINE = 1%
			END IF
		END IF

		SMG_WINDOW::CHAN  = WP_REQLINE.CH%
		WHEN ERROR IN
			RESET #WP_REQLINE.CH%
			GET #WP_REQLINE.CH%, REGARDLESS
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

		DATA	01, 05, "(01)  Operation", &
			02, 05, "(02)  Req Number", &
			03, 05, "(03)  Req Line", &
			04, 05, "(04)  Product", &
			05, 05, "(05)  Quantity", &
			0, 0,  ""

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

 Reenter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%
	!++
	! Abstract:FLD001
	!	^*(01) Operation\*
	!	.b
	!	.lm +5
	!	The ^*Operation\* field enters the designation
	!	for an operation in reference to a particular product where
	!	the component is first used.
	!	.lm -5
	!
	! Index:
	!	.x Operation>Requistion Line
	!	.x Requistion Line>Operation
	!
	!--
			WP_REQLINE::OPERATION = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"01;26",TEMP$, WP_REQLINE::OPERATION, MFLAG, &
				"'E", MVALUE)

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Requisition Number\*
	!	.b
	!	.lm +5
	!	The ^*Requisition Number\* field assigns
	!	an identification number to requisition lines that have been assigned to
	!	a single operation. The field is left blank when requisitions
	!	are entered.  The system automatically enters this field.
	!	.lm -5
	!
	! Index:
	!	.x Requisition Number>Requisition Line
	!	.x Requisition Line>Requisition Number
	!
	!--
			WP_REQLINE::REQNUM = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"02;26",TEMP$, WP_REQLINE::REQNUM, &
				MFLAG OR 2%, "~R 'E", MVALUE)

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Requisition Line\*
	!	.b
	!	.lm +5
	!	The ^*Requisition Line Number\* field assigns
	!	an identification number to requisition lines that have been assigned to
	!	a single operation. The field is left blank when requisitions
	!	are entered.  Entry in this field is system generated.
	!	.lm -5
	!
	! Index:
	!	.x Requisition Line>Requisition Line
	!	.x Requisition Line>Requisition Line
	!
	!--
			WP_REQLINE::REQLINE = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"03;26",TEMP$, WP_REQLINE::REQLINE, MFLAG, &
				"~L0'E", MVALUE)

		CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Product\*
	!	.b
	!	.lm +5
	!	The ^*Product\* field enters a code which
	!	identifies a particular component in a product.
	!	.b
	!	If a valid product is entered as defined in the Product
	!	Description file the description will automatically appear. If the Product
	!	code is not valid, a message will appear on the screen: ^*Input
	!	undefined, enter anyway <Yes/No>: No.\* The invalid code may be
	!	entered by typing "Y" and pressing ^*<Do>\*.
	!	.b
	!	Pressing ^*<List Choices>\* while the cursor is located at this
	!	field causes a list of valid component numbers to be
	!	displayed.
	!	.lm -5
	!
	! Index:
	!	.x Product>Requistion Line
	!	.x Requistion Line>Product
	!
	!--
			WP_REQLINE::PRODUCT = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"04;26",TEMP$, WP_REQLINE::PRODUCT, MFLAG, &
				"'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, "VX") = 1%
				THEN
					WP_REQLINE::PRODUCT = &
						PD_PRODUCT::PRODUCT_NUM
				END IF
				GOTO Reenter

			END IF

		CASE 5%
	!++
	! Abstract:FLD005
	!	^*(05) Quantity\*
	!	.b
	!	.lm +5
	!	The ^*Quantity\* field enters the quantity needed for the
	!	materials requisition.
	!	.lm -5
	!
	! Index:
	!	.x Quantity
	!
	!--
			WP_REQLINE::QTY = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"05;26",TEMP$, WP_REQLINE::QTY, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY

		WP_MAIN_REQLINE = 0%

		SELECT MLOOP

		CASE 4%
			WP_MAIN_REQLINE = FUNC_TESTENTRY(SMG_WINDOW, &
				WP_REQLINE::PRODUCT, &
				PD_PRODUCT::DESCRIPTION, &
				"WP", MLOOP, "PROG", &
				"Product ", PD_MAIN_PRODUCT.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_PRODUCT::DESCRIPTION, 4%, 40%,, SMG$M_BOLD)

		END SELECT

	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(4%) AND 2%) = 0%
		THEN
			PD_PRODUCT::DESCRIPTION = &
				SPACE$(LEN(PD_PRODUCT::DESCRIPTION))

			IF WP_REQLINE::PRODUCT <> ""
			THEN
				V% = MAIN_WINDOW(PD_MAIN_PRODUCT.ID, "Q0" + &
					WP_REQLINE::PRODUCT)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_PRODUCT::DESCRIPTION, 4%, 40%,, SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_PRODUCT::BOMUOM, 5%, 40%,, SMG$M_BOLD)

		END IF

	!
	! Set WP_REQLINE_OLD value
	!
20500	CASE OPT_SETOLD
		WP_REQLINE_OLD = WP_REQLINE

	!
	! Restore WP_REQLINE_OLD value
	!
	CASE OPT_RESETOLD
		WP_REQLINE = WP_REQLINE_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		WP_REQLINE_DEF = WP_REQLINE

		IF MFLAG = 1%
		THEN
			SELECT MLOOP

			CASE 0%
				FRM$(5%) = "#,###,###"

			CASE ELSE
				FRM$(MLOOP) = MVALUE

			END SELECT
		END IF

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		WP_REQLINE = WP_REQLINE_DEF

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!
		WP_REQLINE::JOB = LEFT(MVALUE, LEN(WP_REQLINE::JOB))
		WP_REQLINE::LLINE = RIGHT(MVALUE, LEN(WP_REQLINE::JOB) + 1%)

	!
	! Find the next record
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE WP_REQLINE::JOB + &
				WP_REQLINE::LLINE + &
				WP_REQLINE::OPERATION, REGARDLESS

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
			MVALUE = "  Reqnum     Reqlin Operation " + &
				"Product              Qty"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "013,020,030,044"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = WP_REQLINE::REQNUM    + " "   + &
				WP_REQLINE::REQLINE   + "   " + &
				WP_REQLINE::OPERATION + "  "  + &
				WP_REQLINE::PRODUCT   + " "   + &
				FORMAT$(WP_REQLINE::QTY, "#,###,###")

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
			! Check if erasing the whole requistion's lines
			! and if so then reset the inventory quantities
			! correctly.
			!
			SELECT MFLAG

			CASE -1%
				IF PD_EXAM_PRODUCT(WP_REQLINE::PRODUCT, &
					PD_PRODUCT_EXAM) <> CMC$_NORMAL
				THEN
					PD_PRODUCT_EXAM::PRODUCT_FACTOR = 1.0
				END IF

				PD_PRODUCT_EXAM::PRODUCT_FACTOR = 1.0 &
					IF PD_PRODUCT_EXAM::PRODUCT_FACTOR = 0.0

				V% = IC_WRIT_35BALANCE(WP_REQLINE::PRODUCT, &
					JC_JOB::LOCATION, "RQ", &
					FUNC_ROUND(WP_REQLINE::QTY * &
					PD_PRODUCT_EXAM::PRODUCT_FACTOR, 3%))

			END SELECT

			!
			! Set init value
			!
			SMG_WINDOW::CURREC = -1%

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
			WHEN ERROR IN
				FIND #SMG_WINDOW::CHAN, KEY #0% GE MVALUE + &
					WP_REQLINE::OPERATION, REGARDLESS
			USE
				CONTINUE 28000
			END WHEN

			!
			! Get a record
			!
			SMG_WINDOW::CURREC = 0%

		!
		! Check if still right key
		!
		CASE 3%
			SMG_WINDOW::CURREC = -1%
			IF WP_REQLINE::JOB + &
				WP_REQLINE::LLINE = MVALUE
			THEN
				SMG_WINDOW::CURREC = 0%
			END IF

		!
		! Change key
		!
		CASE 6%
			WP_REQLINE::JOB = SEG$(MVALUE, 1%, LEN(WP_REQLINE::JOB))
			WP_REQLINE::LLINE = &
				MID$(MVALUE, LEN(WP_REQLINE::JOB) + 1, &
				LEN(WP_REQLINE::LLINE))

		END SELECT

	CASE OPT_AFTEROPT

		SELECT MVALUE

		CASE "Add"
			IF PD_EXAM_PRODUCT(WP_REQLINE::PRODUCT, &
				PD_PRODUCT_EXAM) <> CMC$_NORMAL
			THEN
				PD_PRODUCT_EXAM::PRODUCT_FACTOR = 1.0
			END IF

			PD_PRODUCT_EXAM::PRODUCT_FACTOR = 1.0 &
				IF PD_PRODUCT_EXAM::PRODUCT_FACTOR = 0.0


			V% = IC_WRIT_35BALANCE(WP_REQLINE::PRODUCT, &
				JC_JOB::LOCATION, "RQ", &
				FUNC_ROUND(-WP_REQLINE::QTY * &
				PD_PRODUCT_EXAM::PRODUCT_FACTOR, 3%))

		CASE "Change", "Blank", "Initialize"

			IF PD_EXAM_PRODUCT(WP_REQLINE_OLD::PRODUCT, &
				PD_PRODUCT_EXAM) <> CMC$_NORMAL
			THEN
				PD_PRODUCT_EXAM::PRODUCT_FACTOR = 1.0
			END IF

			PD_PRODUCT_EXAM::PRODUCT_FACTOR = 1.0 &
				IF PD_PRODUCT_EXAM::PRODUCT_FACTOR = 0.0

			V% = IC_WRIT_35BALANCE(WP_REQLINE_OLD::PRODUCT, &
				JC_JOB::LOCATION, "RQ", &
				FUNC_ROUND(WP_REQLINE_OLD::QTY * &
				PD_PRODUCT_EXAM::PRODUCT_FACTOR, 3%))

			IF PD_EXAM_PRODUCT(WP_REQLINE::PRODUCT, &
				PD_PRODUCT_EXAM) <> CMC$_NORMAL
			THEN
				PD_PRODUCT_EXAM::PRODUCT_FACTOR = 1.0
			END IF

			PD_PRODUCT_EXAM::PRODUCT_FACTOR = 1.0 &
				IF PD_PRODUCT_EXAM::PRODUCT_FACTOR = 0.0

			V% = IC_WRIT_35BALANCE(WP_REQLINE::PRODUCT, &
				JC_JOB::LOCATION, "RQ", &
				FUNC_ROUND(-WP_REQLINE::QTY * &
				PD_PRODUCT_EXAM::PRODUCT_FACTOR, 3%))

		CASE "Erase"

			IF MLOOP <> 1%
			THEN
				IF PD_EXAM_PRODUCT(WP_REQLINE::PRODUCT, &
					PD_PRODUCT_EXAM) <> CMC$_NORMAL
				THEN
					PD_PRODUCT_EXAM::PRODUCT_FACTOR = 1.0
				END IF

				PD_PRODUCT_EXAM::PRODUCT_FACTOR = 1.0 &
					IF PD_PRODUCT_EXAM::PRODUCT_FACTOR = 0.0

				V% = IC_WRIT_35BALANCE(WP_REQLINE::PRODUCT, &
					JC_JOB::LOCATION, "RQ", &
					FUNC_ROUND(WP_REQLINE::QTY * &
					PD_PRODUCT_EXAM::PRODUCT_FACTOR, 3%))
			END IF

		END SELECT

	END SELECT

 ExitFunction:
28000	EXIT FUNCTION

	%PAGE

 LoadLines:
	!=======================================================================
	SOPTION$(1%) = "BOM lowest level"
	SOPTION$(2%) = "BOM highest level"
	SOPTION$(3%) = "BOM highest level no stop"
	SOPTION$(4%) = "Manually enter lines"

 SelectSOption:

	X% = ENTR_3CHOICE(SCOPE, "5;45", "", SOPTION$(), "", &
		0%, "Select Option", "", 0%) IF X% = 0%

	SELECT SCOPE::SCOPE_EXIT

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?

		X% = 1%
		GOTO ExitLoadLines

	END SELECT

	SELECT X%

	CASE 0%
		GOTO SelectSOption

	CASE 4%
		GOTO ExitLoadLines

	CASE ELSE
		!
		! Get information from work order register
		!
		IF WP_REQJOUR.CH%
		THEN
			IF WP_READ_REGLINE(WP_REQJOUR::JOB, &
				WP_REQJOUR::LLINE, &
				"EQ", WP_REGLINE_READ, &
				QTY()) <> CMC$_NORMAL
			THEN
				GOTO ExitLoadLines
			END IF
		ELSE
			WP_REQJOUR::JOB		= WP_ORDERLINE::JOB
			WP_REQJOUR::LLINE	= WP_ORDERLINE::LLINE
			JC_JOB::LOCATION	= WP_JOB::LOCATION
			WP_REGLINE_READ::JOB	= WP_ORDERLINE::JOB
			WP_REGLINE_READ::ITEMCODE	= WP_ORDERLINE::ITEMCODE
			QTY(0%), QTY(6%)	= WP_ORDERLINE::QTY
			WP_REGLINE_READ::DESCR	= WP_ORDERLINE::DESCR
		END IF

		GOTO PrintMessage IF X% = 3%
	END SELECT

	!
	! Display some info in a window
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		3%, &
		72%, &
		SMG_X2, &
		SMG$M_BORDER &
	)

	!
	! Label the display
	!
	SMG_STATUS% = SMG$LABEL_BORDER(SMG_X2, "Select Requisition Quantity")

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_X2, &
		SCOPE::SMG_PBID, &
		12%, &
		5% &
	)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_X2, &
		"ItemCode       Descripton                 OnOrder   ReqConf", &
		1%, 5%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_X2, &
		WP_REGLINE_READ::ITEMCODE         + " " + &
		LEFT(WP_REGLINE_READ::DESCR, 25%) + &
		FORMAT$(QTY(6%), "######.##"), &
		2%, 5%)

	REQCONF = ENTR_3NUMBER(SCOPE, SMG_X2, &
		"2;55", "Qty Requested", QTY(6%), MFLAG, &
		"######.##", MVALUE)

	SELECT SCOPE::SCOPE_EXIT

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?

		GOTO ExitLoadLines

	END SELECT

	REQCONF = 0.0 IF REQCONF < 0.0

	GOTO ExitLoadLines IF REQCONF = 0.0

	SMG_STATUS% = SMG$POP_VIRTUAL_DISPLAY(SMG_X2, SCOPE::SMG_PBID)

	QTY(0%), QTY(6%) = REQCONF

 PrintMessage:
	CALL ENTR_3MESSAGE(SCOPE, "Loading Lines", 1% + 16%)

	SELECT X%

	CASE 2%
		SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
		( &
			3%, &
			72%, &
			SMG_X2, &
			SMG$M_BORDER &
		)

		!
		! Label the display
		!
		SMG_STATUS% = SMG$LABEL_BORDER(SMG_X2, "Select Higher Level")

		SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
		( &
			SMG_X2, &
			SCOPE::SMG_PBID, &
			12%, &
			5% &
		)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_X2, &
			"    OnHand   Alloc    Avail  OnOrder      " + &
			"Req     ReqConf", &
			1%, 15%)

	END SELECT

28007	TOTAL_PRODUCT = 0.0

	WHEN ERROR IN
		FIND #BM_RELATION.CH%, &
			KEY #0% EQ WP_REGLINE_READ::ITEMCODE + "", &
			REGARDLESS
	USE
		CONTINUE ExitLoadLines IF ERR = 9% OR ERR=155%
		FILENAME$ = "BM_RELATION"
		EXIT HANDLER
	END WHEN

28010	!
	! Main loop starts here
	!
	!
	! Get next record
	!
	WHEN ERROR IN
		GET #BM_RELATION.CH%, REGARDLESS
	USE
		CONTINUE ExitLoadLines IF ERR = 11%
		FILENAME$ = "BM_RELATION"
		EXIT HANDLER
	END WHEN

	ITEM_LOOP% = 0%
	QTY_LEVEL(0%) = QTY(6%)
	LEVEL% = 1%

 GoDownTree:
	TEST_PRODUCT(LEVEL%) = BM_RELATION::PRODUCT
	QTY_LEVEL(LEVEL%) = QTY_LEVEL(LEVEL% - 1%) * BM_RELATION::QUANTITY
	RFA_LEVEL(LEVEL%) = GETRFA(BM_RELATION.CH%)

	BM_RELATION_ORIG = BM_RELATION
	REM.QTY = 0.0

28020	WHEN ERROR IN
		GET #BM_RELATION.CH%, &
			KEY #0% EQ BM_RELATION::COMPONENT + "", &
			REGARDLESS
	USE
		CONTINUE 28040 IF ERR = 155%
		FILENAME$ = "BM_RELATION"
		EXIT HANDLER
	END WHEN

	SELECT X%

	CASE 2%
		V% = IC_READ_35BALANCE(BM_RELATION::PRODUCT, &
			JC_JOB::LOCATION, XBALANCE(,))

		BAL(J%) = 0.0 FOR J% = 1% TO 3%

		FOR Z% = 1% TO 3%
			FOR Y% = 1% TO 3%
				BAL(Z%) = BAL(Z%) + XBALANCE(Z%, Y%)
			NEXT Y%
		NEXT Z%

		V% = PD_EXAM_PRODUCT(BM_RELATION::PRODUCT, &
			PD_PRODUCT_EXAM)

		PD_PRODUCT_EXAM::PRODUCT_FACTOR = 1.0 &
			IF PD_PRODUCT_EXAM::PRODUCT_FACTOR = 0.0

		AVAILABLE = (BAL(1%) + BAL(2%)) / &
			PD_PRODUCT_EXAM::PRODUCT_FACTOR
		AVAILABLE = 0.0 IF AVAILABLE < 0.0

 !		REQ = FUNC_ROUND(QTY_LEVEL(LEVEL%) / &
 !			PD_PRODUCT_EXAM::PRODUCT_FACTOR, 2%)

		REQ = FUNC_ROUND(QTY_LEVEL(LEVEL%), 2%)

		REQ = AVAILABLE IF AVAILABLE < REQ

		SMG_STATUS% = SMG$PUT_CHARS(SMG_X2, &
			BM_RELATION::PRODUCT + &
			FORMAT$(BAL(1%), " ######## ")  + &
			FORMAT$(-BAL(2%), "####### ")  + &
			FORMAT$(AVAILABLE, "######## ") + &
			FORMAT$(BAL(3%), "########") + &
			FORMAT$(QTY_LEVEL(LEVEL%), "######.##"), &
			2%, 2%)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_X2, &
			PD_PRODUCT_EXAM::DESCRIPTION, 3%, 2%)

		CALL ENTR_3MESSAGE(SCOPE, "", 1%)

		REQCONF = ENTR_3NUMBER(SCOPE, SMG_X2, &
			"2;63", "Qty Requested", REQ, MFLAG, &
			"######.##", MVALUE)

		SELECT SCOPE::SCOPE_EXIT

		CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?

			GOTO ExitLoadLines

		END SELECT

		CALL ENTR_3MESSAGE(SCOPE, "Loading Lines", 1% + 16%)

		IF REQCONF <> 0.0
		THEN
			WP_REQLINE::OPERATION = BM_RELATION_ORIG::OPERATION
			WP_REQLINE::PRODUCT   = BM_RELATION::PRODUCT
			WP_REQLINE::QTY       = REQCONF

			REM.QTY = FUNC_ROUND(QTY_LEVEL(LEVEL%) - REQCONF, 2%)

			GOTO 28050
		END IF

	CASE 3%
		V% = PD_EXAM_PRODUCT(BM_RELATION::PRODUCT, &
			PD_PRODUCT_EXAM)

 !
 ! Should already be in BOM qty
 !
 !		PD_PRODUCT_EXAM::PRODUCT_FACTOR = 1.0 &
 !			IF PD_PRODUCT_EXAM::PRODUCT_FACTOR = 0.0
 !
 !		REQCONF = FUNC_ROUND(QTY_LEVEL(LEVEL%) / &
 !			PD_PRODUCT_EXAM::PRODUCT_FACTOR, 2%)

		WP_REQLINE::OPERATION = BM_RELATION_ORIG::OPERATION
		WP_REQLINE::PRODUCT   = BM_RELATION::PRODUCT
		WP_REQLINE::QTY       = REQCONF

		REM.QTY = 0.0
		GOTO 28050
	END SELECT

	LEVEL% = LEVEL% + 1%

	GOTO GoDownTree

 GoUpTree:
	GOTO ExitLoadLines IF LEVEL% - 1% = 0%

	LEVEL% = LEVEL% - 1%

28030	WHEN ERROR IN
		GET #BM_RELATION.CH%, RFA RFA_LEVEL(LEVEL%), REGARDLESS

		GET #BM_RELATION.CH%, REGARDLESS
	USE
		IF ERR = 155% OR ERR = 11%
		THEN
			CONTINUE GoUpTree
		END IF
		FILENAME$ = "BM_RELATION"
		EXIT HANDLER
	END WHEN

	IF BM_RELATION::PRODUCT <> TEST_PRODUCT(LEVEL%)
	THEN
		GOTO GoUpTree
	ELSE
		GOTO GoDownTree
	END IF

28040	!
	! Get some product info, so we can compare product types
	!
	V% = PD_EXAM_PRODUCT(BM_RELATION::COMPONENT, PD_PRODUCT_EXAM)

	GOTO 28030 IF COMP_STRING(EDIT$( &
		PD_PRODUCT_EXAM::PROD_TYPE, -1%), BM_CONTROL::PRODTYPE) = 0% &
		AND BM_CONTROL::PRODTYPE <> ""

	PD_PRODUCT_EXAM::PRODUCT_FACTOR = 1.0 &
		IF PD_PRODUCT_EXAM::PRODUCT_FACTOR = 0.0

 !	REQCONF = FUNC_ROUND(QTY_LEVEL(LEVEL%) / &
 !		PD_PRODUCT_EXAM::PRODUCT_FACTOR, 3%)

	REQCONF = FUNC_ROUND(QTY_LEVEL(LEVEL%), 3%)

	!
	! Build REQLINE record
	!
	WP_REQLINE::OPERATION = BM_RELATION::OPERATION
	WP_REQLINE::PRODUCT   = BM_RELATION::COMPONENT

28050	WHEN ERROR IN
		FIND #WP_REQLINE.CH%, &
			KEY #0% EQ WP_REQJOUR::JOB + WP_REQJOUR::LLINE + &
			WP_REQLINE::OPERATION + WP_REQLINE::PRODUCT

		GET #WP_REQLINE.CH%
	USE
		CONTINUE 28100 IF ERR = 155%
		FILENAME$ = "BM_REQLINE"
		EXIT HANDLER
	END WHEN

	WP_REQLINE::QTY = WP_REQLINE::QTY + REQCONF

	UPDATE #WP_REQLINE.CH%
	GOTO 28110

28100	WP_REQLINE::JOB       = WP_REQJOUR::JOB
	WP_REQLINE::LLINE     = WP_REQJOUR::LLINE
	WP_REQLINE::REQNUM    = ""
	WP_REQLINE::REQLINE   = ""
	WP_REQLINE::QTY       = REQCONF

	PUT #WP_REQLINE.CH%

28110	!
	! Write to the register
	!

	!
	! Tell inventory what we did
	!
	IF PD_EXAM_PRODUCT(WP_REQLINE::PRODUCT, &
		PD_PRODUCT_EXAM) <> CMC$_NORMAL
	THEN
		PD_PRODUCT_EXAM::PRODUCT_FACTOR = 1.0
	END IF

	PD_PRODUCT_EXAM::PRODUCT_FACTOR = 1.0 &
		IF PD_PRODUCT_EXAM::PRODUCT_FACTOR = 0.0

	V% = IC_WRIT_35BALANCE(WP_REQLINE::PRODUCT, &
		JC_JOB::LOCATION, "RQ", &
		FUNC_ROUND(-WP_REQLINE::QTY * &
		PD_PRODUCT_EXAM::PRODUCT_FACTOR, 3%))

	IF REM.QTY > 0.0
	THEN
		QTY_LEVEL(LEVEL%) = REM.QTY
		LEVEL% = LEVEL% + 1%
		GOTO GoDownTree
	ELSE
		GOTO 28030
	END IF

 ExitLoadLines:
	SMG_STATUS% = SMG$POP_VIRTUAL_DISPLAY(SMG_X2, SCOPE::SMG_PBID)

	CALL ENTR_3MESSAGE(SCOPE, "", 1%)

	X% = 0%
	RETURN

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
