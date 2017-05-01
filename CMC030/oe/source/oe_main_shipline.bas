1	%TITLE "Invoice Line Journal"
	%SBTTL "OE_MAIN_SHIPLINE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG OE_MAIN_SHIPLINE(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1990 BY
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
	!	The ^*Invoice Line Journal\* maintains the
	!	number of products to be invoiced.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_MAIN_SHIPLINE/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN OE_MAIN_SHIPLINE
	!	$ DELETE OE_MAIN_SHIPLINE.OBJ;*
	!
	! Author:
	!	Probably Val Allen.
	!
	! Modification history:
	!
	!	08/09/91 - Craig Tanner
	!		Updated call to OE_READ_REGLINE. Now third parameter
	!		is NEXT_GET, used to be some meaningless date.
	!
	!	11/07/91 - Dan Perkins
	!		Added F14 key to line number field so operator can
	!		view all line numbers pertaining to order.
	!
	!	04/16/92 - Dan Perkins
	!		Allow formatting of numeric fields using /SET.
	!
	!	04/05/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/20/96 - Kevin Handy
	!		Reformat source code
	!
	!	05/28/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/10/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	12/07/2000 - Kevin Handy
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

	%INCLUDE "FUNC_INCLUDE:OE_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[OE.OPEN]OE_SHIPLINE.HB"
	MAP (OE_SHIPLINE)	OE_SHIPLINE_CDD		OE_SHIPLINE
	MAP (OE_SHIPLINE_OLD)	OE_SHIPLINE_CDD		OE_SHIPLINE_OLD, &
							OE_SHIPLINE_DEF

	%INCLUDE "SOURCE:[OE.OPEN]OE_SHIPJOUR.HB"
	MAP (OE_SHIPJOUR)	OE_SHIPJOUR_CDD		OE_SHIPJOUR

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.HB"
	MAP (OE_REGLINE)	OE_REGLINE_CDD		OE_REGLINE
	DECLARE			OE_REGLINE_CDD		OE_REGLINE_READ

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.HB"
	DECLARE			OE_REGHEADER_CDD	OE_REGHEADER_READ

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	!
	! Common Statements
	!
	COM (CH_OE_SHIPJOUR) &
		INVLOCATION$ = 4%, &
		BATCH_NO$ = 2%

	COM (CH_OE_SHIPLINE) &
		OE_SHIPLINE.CH%, &
		OE_SHIPLINE.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION OE_READ_REGLINE
	EXTERNAL LONG	FUNCTION PD_EXAM_PRODUCT
	EXTERNAL LONG   FUNCTION IC_WRIT_35BALANCE
	EXTERNAL LONG   FUNCTION OE_READ_REGHEADER
	EXTERNAL LONG	FUNCTION MAIN_WINDOW

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Order Shipping Lines"
		SMG_WINDOW::CURREC = -2%
		SMG_WINDOW::NHELP = "OE_MAIN_SHIPLINE"
		SMG_WINDOW::HSIZE = 76%
		SMG_WINDOW::VSIZE = 11%
		SMG_WINDOW::HPOS  = 3%
		SMG_WINDOW::VPOS  = 8%
		SMG_WINDOW::NITEMS= 7%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::HVIEW = 76%
		SMG_WINDOW::VVIEW = 11%
		SMG_WINDOW::VHPOS = 3%
		SMG_WINDOW::VVPOS = 8%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(1%) = "Line Number"
			SMG_WINDOW::KFIELD(1%, 0%) = 1%
			SMG_WINDOW::KFIELD(1%, 1%) = 1%

		COM (OE_MAIN_SHIPLINE_FRM) FRM$(7%)

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

		!
		! Load in defaults for chart
		!
		CALL READ_DEFAULTS(SMG_WINDOW)


700		!
		! Declare channels
		!
		IF OE_SHIPLINE.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF OE_SHIPLINE.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[OE.OPEN]OE_SHIPLINE.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			OE_MAIN_SHIPLINE = ERR
			CONTINUE 770
		END WHEN

		OE_SHIPLINE.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[OE.OPEN]OE_SHIPLINE.OPN"
		USE
			OE_MAIN_SHIPLINE = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		OE_SHIPLINE.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(OE_SHIPLINE.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = OE_SHIPLINE.CH%
		WHEN ERROR IN
			RESET #OE_SHIPLINE.CH%
			GET #OE_SHIPLINE.CH%, REGARDLESS
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


		DATA	02,05, "(01) Line Number", &
			05,05, "(02) Quantity to Ship", &
			06,05, "(03) Canceled Quantity", &
			03,05, "     Product Number", &
			04,05, "     Product Description", &
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
	!	The ^*Line\* field enters the line number of the
	!	order for this shipment.
	!	.b
	!	Valid line numbers may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x Line
	!
	!--
			OE_SHIPLINE::LIN = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"02;33",TEMP$, OE_SHIPLINE::LIN, MFLAG, &
				"~L0'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_F14
				IF MAIN_WINDOW(OE_MAIN_REGLINE.ID, "VX") = 1%
				THEN
					OE_SHIPLINE::LIN = OE_REGLINE::LIN
				END IF
				GOTO Reentry

			END SELECT

		CASE 2%
	!++
	! Abstract:FLD002
	!	.x Order Quantity
	!	^*(02) Shipped Quantity\*
	!	.b
	!	.lm +5
	!	The ^*Shipped Quantity\* field enters the number of
	!	units which have been shipped on this particular date for this
	!	particular product.
	!	.b
	!	The field may contain a figure as large as 99,999.99.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_SHIPLINE::SHPQTY = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"05;33",TEMP$, OE_SHIPLINE::SHPQTY, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Cancelled Quantity\*
	!	.b
	!	.lm +5
	!	The ^*Cancelled Quantity\* field enters the
	!	quantity that will not be backordered.
	!	.lm -5
	!
	! Index:
	!	.x Cancelled Quantity
	!
	!--
			OE_SHIPLINE::CANCELQTY = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"06;33",TEMP$, OE_SHIPLINE::CANCELQTY, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY

		OE_MAIN_SHIPLINE = 0%

		SELECT MLOOP
		CASE 1%
			!
			! Look up the product number in the OE_REGLINE file
			!
			IF OE_READ_REGLINE(OE_SHIPLINE::ORDNUM, &
				OE_SHIPLINE::LIN, "EQ", &
				OE_REGLINE_READ, QTY()) <> CMC$_NORMAL
			THEN

				CALL HELP_34MESSAGE(SCOPE, &
					"undefined order line", &
					"W", SCOPE::PRG_PROGRAM, "OE", "UNDORDLIN")

	!++
	! Warning:UNDORDLIN
	!	^*Undefined Order Line\*
	!	.b
	!	^*Explanation\*
	!	.b
	!	.lm +5
	!	Selected order line doesn't exist in Order Register
	!	file.
	!	.b
	!	.lm -5
	!	^*User Action\*
	!	.b
	!	.lm +5
	!	Select order line from the Order Register file. Pressing
	!	^*List Choices\* will display a list of all valid lines for
	!	the particular order.
	!	.lm -5
	!
	! Index:
	!	.x Undefined Order Line
	!
	!--

				OE_MAIN_SHIPLINE = 1%

			END IF

			IF MVALUE = "ADD"
			THEN
				IF OE_SHIPLINE::ORDNUM + OE_SHIPLINE::LIN <> &
					OE_REGLINE_READ::ORDNUM + &
					OE_REGLINE_READ::LIN
				THEN
					CALL HELP_34MESSAGE(SCOPE, &
						"undefined order line", &
						"W", SCOPE::PRG_PROGRAM, &
						"OE", "UNDORDLIN")
					OE_MAIN_SHIPLINE = 1%
				END IF
			END IF

			V% = PD_EXAM_PRODUCT(OE_REGLINE_READ::PRODUCT, &
				PD_PRODUCT_EXAM)

			!
			! Display the information that we looked up.
			!
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_PRODUCT_EXAM::PRODUCT_NUM, &
				03%, 33%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_PRODUCT_EXAM::DESCRIPTION, &
				04%, 33%, , SMG$M_BOLD)
		END SELECT

	CASE OPT_DISPLAY

			!
			! Look up the product number in the OE_REGLINE file
			!
			V% = OE_READ_REGLINE (OE_SHIPLINE::ORDNUM, &
				OE_SHIPLINE::LIN, &
				"EQ", &
				OE_REGLINE_READ, &
				QTY())

			V% = PD_EXAM_PRODUCT(OE_REGLINE_READ::PRODUCT, &
				PD_PRODUCT_EXAM)

			!
			! Display the information that we looked up.
			!
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_PRODUCT_EXAM::PRODUCT_NUM, &
				03%, 33%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_PRODUCT_EXAM::DESCRIPTION, &
				04%, 33%, , SMG$M_BOLD)

	!
	! Set OE_SHIPLINE_OLD value
	!
20500	CASE OPT_SETOLD
		OE_SHIPLINE_OLD = OE_SHIPLINE

	!
	! Restore OE_SHIPLINE_OLD value
	!
	CASE OPT_RESETOLD
		OE_SHIPLINE = OE_SHIPLINE_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		OE_SHIPLINE_DEF = OE_SHIPLINE

		IF MFLAG = 1%
		THEN
			SELECT MLOOP
			CASE 0%
				FRM$(2%) = "#,###,###.##"
				FRM$(3%) = "#,###,###.##"
			CASE ELSE
				FRM$(MLOOP) = MVALUE
			END SELECT
		END IF

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		OE_SHIPLINE = OE_SHIPLINE_DEF

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!
		OE_SHIPLINE::ORDNUM  = MVALUE

	!
	! Find the next record
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE OE_SHIPLINE::ORDNUM + &
				OE_SHIPLINE::LIN, REGARDLESS

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
			MVALUE = "  Line#        InvQty     UnitPrice   Discount%      ExtPrice"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "009,023,038,050"

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

				GOSUB GetRemain

				V% = IC_WRIT_35BALANCE(OE_REGLINE_READ::PRODUCT, &
					INVLOCATION$, "SO", &
					-ALLOCATE_QTY)

				V% = IC_WRIT_35BALANCE(OE_REGLINE_READ::PRODUCT, &
					INVLOCATION$, "SA", &
					OE_SHIPLINE::SHPQTY)

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
		! Check if still right key
		!
		CASE 3%
			SMG_WINDOW::CURREC = -1%
			IF OE_SHIPLINE::ORDNUM = MVALUE
			THEN
				SMG_WINDOW::CURREC = 0%
			END IF

		!
		! Change key
		!
		CASE 6%
			OE_SHIPLINE::ORDNUM = MVALUE

		END SELECT


	CASE OPT_AFTEROPT

		SELECT MVALUE

		CASE "Add"

			GOSUB GetRemain

			V% = IC_WRIT_35BALANCE (OE_REGLINE_READ::PRODUCT, &
				INVLOCATION$, "SO", &
				ALLOCATE_QTY)

			V% = IC_WRIT_35BALANCE (OE_REGLINE_READ::PRODUCT, &
				INVLOCATION$, "SA", &
				-OE_SHIPLINE::SHPQTY)

		CASE "Change", "Blank", "Initialize"

			GOSUB GetRemainOld

			V% = IC_WRIT_35BALANCE (OE_REGLINE_READ::PRODUCT, &
				INVLOCATION$, "SO", &
				-ALLOCATE_QTY)

			V% = IC_WRIT_35BALANCE (OE_REGLINE_READ::PRODUCT, &
				INVLOCATION$, "SA", &
				OE_SHIPLINE_OLD::SHPQTY)

			GOSUB GetRemain

			V% = IC_WRIT_35BALANCE (OE_REGLINE_READ::PRODUCT, &
				INVLOCATION$, "SO", &
				ALLOCATE_QTY)

			V% = IC_WRIT_35BALANCE (OE_REGLINE_READ::PRODUCT, &
				INVLOCATION$, "SA", &
				-OE_SHIPLINE::SHPQTY)

		CASE "Erase"

			IF MLOOP <> 1%
			THEN

				GOSUB GetRemain

				V% = IC_WRIT_35BALANCE (OE_REGLINE_READ::PRODUCT, &
					INVLOCATION$, "SO", &
					-ALLOCATE_QTY)

				V% = IC_WRIT_35BALANCE (OE_REGLINE_READ::PRODUCT, &
					INVLOCATION$, "SA", &
					OE_SHIPLINE::SHPQTY)
			END IF

		END SELECT

	END SELECT



28000	EXIT FUNCTION

	%PAGE

28500
 GetRemain:
	V% = OE_READ_REGLINE(OE_SHIPJOUR::ORDNUM, &
		OE_SHIPLINE::LIN, "EQ", &
		OE_REGLINE_READ, QTY())

	V% = OE_READ_REGHEADER(OE_SHIPJOUR::ORDNUM, OE_REGHEADER_READ)

	INVLOCATION$ = OE_REGHEADER_READ::LOCATION


	!
	! Calculate remaining on-order for new or current line
	!
	ALLOCATE_QTY = QTY(1%) - (QTY(2%) + QTY(3%))
	ALLOCATE_QTY = (OE_SHIPLINE::SHPQTY + OE_SHIPLINE::CANCELQTY) &
		IF (OE_SHIPLINE::SHPQTY + OE_SHIPLINE::CANCELQTY) <= &
		ALLOCATE_QTY
	RETURN


 GetRemainOld:
	V% = OE_READ_REGLINE(OE_SHIPJOUR::ORDNUM, &
		OE_SHIPLINE::LIN, "EQ", &
		OE_REGLINE_READ, QTY())

	V% = OE_READ_REGHEADER(OE_SHIPJOUR::ORDNUM, &
		OE_REGHEADER_READ)

	INVLOCATION$ = OE_REGHEADER_READ::LOCATION

	!
	! Calculate remaining on-order for old qty's shipped and cancelled
	!
	ALLOCATE_QTY = QTY(1%) - (QTY(2%) + QTY(3%))
	ALLOCATE_QTY = (OE_SHIPLINE_OLD::SHPQTY + OE_SHIPLINE_OLD::CANCELQTY) &
		IF (OE_SHIPLINE_OLD::SHPQTY + OE_SHIPLINE_OLD::CANCELQTY) <= &
		ALLOCATE_QTY
	RETURN


29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
