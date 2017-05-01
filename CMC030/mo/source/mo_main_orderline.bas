1	%TITLE "Manufacturing Order Line Journal"
	%SBTTL "MO_MAIN_ORDERLINE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG MO_MAIN_ORDERLINE(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	Manufacturing Order Line Journal is used to enter the models
	!	and associated options.
	!	.lm -5
	!
	! Index:
	!	.x Help>Order Line Journal
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS MO_SOURCE:MO_MAIN_ORDERLINE/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN MO_MAIN_ORDERLINE
	!	$ DELETE MO_MAIN_ORDERLINE.OBJ;*
	!
	! Author:
	!
	!
	! Modification history:
	!
	!	04/27/92 - Kevin Handy
	!		Clean up (check)
	!
	!	06/10/92 - Dan Perkins
	!		Reworked program code.
	!
	!	06/12/92 - Kevin Handy
	!		Clean up (check)
	!
	!	11/23/92 - Dan Perkins
	!		Added CASE 2 to OPT_SUBWIND so that VIEW would
	!		work properly.
	!
	!	02/02/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	02/08/93 - Dan Perkins
	!		Added line number and notes, resulting from new
	!		file layout.
	!
	!	02/26/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/01/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/05/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/14/93 - Dan Perkins
	!		Call SUBR_TRANTYPE in OPT_TESTENTRY so the order
	!		status changes immediately.
	!
	!	06/09/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	07/21/93 - Frank F. Starman
	!		Added a serial number field.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	06/26/96 - Kevin Handy
	!		Reformat source code.
	!
	!	07/30/97 - Kevin Handy
	!		Change XAGE parameter of READ_PERIOD to integer
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/10/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	10/31/2000 - Kevin Handy
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

	%INCLUDE "FUNC_INCLUDE:MO_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[MO.OPEN]MO_ORDERLINE.HB"
	MAP (MO_ORDERLINE)	MO_ORDERLINE_CDD	MO_ORDERLINE
	MAP (MO_ORDERLINE_OLD)	MO_ORDERLINE_CDD	MO_ORDERLINE_OLD, &
							MO_ORDERLINE_DEF

	%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERJOUR.HB"
	MAP (OE_ORDERJOUR)	OE_ORDERJOUR_CDD	OE_ORDERJOUR

	%INCLUDE "SOURCE:[MO.OPEN]MO_REGLINE.HB"
	MAP (MO_REGLINE)	MO_REGLINE_CDD		MO_REGLINE
	DECLARE			MO_REGLINE_CDD		MO_REGLINE_READ

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

	%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.HB"
	MAP (IC_CONTROL)	IC_CONTROL_CDD		IC_CONTROL

	%INCLUDE "SOURCE:[MO.OPEN]MO_MAKELINE.HB"
	MAP (MO_MAKELINE)	MO_MAKELINE_CDD		MO_MAKELINE

	%INCLUDE "SOURCE:[MO.OPEN]MO_MAKE.HB"
	MAP (MO_MAKE)		MO_MAKE_CDD		MO_MAKE
	COM (MO_MAKE_READ)	MO_MAKE_CDD		MO_MAKE_READ

	%INCLUDE "SOURCE:[MO.OPEN]MO_MODELCODE.HB"
	MAP (MO_MODELCODE)	MO_MODELCODE_CDD	MO_MODELCODE

	%INCLUDE "SOURCE:[MO.OPEN]MO_MODEL.HB"
	MAP (MO_MODEL)		MO_MODEL_CDD		MO_MODEL

	%INCLUDE "SOURCE:[MO.OPEN]MO_MAKETYPE.HB"
	MAP (MO_MAKETYPE)	MO_MAKETYPE_CDD		MO_MAKETYPE

	%INCLUDE "SOURCE:[MO.OPEN]MO_MAKESIZE.HB"
	MAP (MO_MAKESIZE)	MO_MAKESIZE_CDD		MO_MAKESIZE

	%INCLUDE "SOURCE:[MO.OPEN]MO_MODELLINE.HB"
	MAP (MO_MODELLINE)	MO_MODELLINE_CDD	MO_MODELLINE

	%INCLUDE "SOURCE:[MO.OPEN]MO_OPTGROUP.HB"
	MAP (MO_OPTGROUP)	MO_OPTGROUP_CDD		MO_OPTGROUP

	!
	! Common Statements
	!
	COM (CH_OE_ORDERJOUR) &
		OE_ORDERJOUR.CH%, &
		XPERIOD$ = 6%

	COM (BATCH_NO) &
		BATCH_NO$ = 7%

	COM (CH_MO_ORDERLINE) &
		MO_ORDERLINE.CH%, &
		MO_ORDERLINE.READONLY%, &
		MO_MODELLINE.CH%, &
		MO_OPTGROUP.CH%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION FUNC_TESTENTRY
	EXTERNAL LONG   FUNCTION IC_WRIT_35BALANCE
	EXTERNAL LONG	FUNCTION MAIN_WINDOW
	EXTERNAL LONG	FUNCTION MO_READ_MAKE
	EXTERNAL LONG	FUNCTION MO_READ_REGLINE
	EXTERNAL REAL   FUNCTION PC_READ_COST
	EXTERNAL REAL   FUNCTION PC_READ_PRICE

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR	= "Manufacturing Order Entry Lines"
		SMG_WINDOW::CURREC	= -2%
		SMG_WINDOW::NHELP	= "MO_MAIN_ORDERLINE"
		SMG_WINDOW::HSIZE	= 76%
		SMG_WINDOW::VSIZE	= 11%
		SMG_WINDOW::HPOS	= 3%
		SMG_WINDOW::VPOS	= 8%
		SMG_WINDOW::NITEMS	= 15%
		SMG_WINDOW::FLAGS	= 0%
		SMG_WINDOW::HVIEW	= 76%
		SMG_WINDOW::VVIEW	= 11%
		SMG_WINDOW::VHPOS	= 3%
		SMG_WINDOW::VVPOS	= 8%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Line"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 0%
			SMG_WINDOW::KFIELD(0%, 1%) = 1% &
			IF OE_ORDERJOUR::REG_FLAG = "Y"
		SMG_WINDOW::KNAME(1%) = "Make"
			SMG_WINDOW::KFIELD(1%, 0%) = 1%
			SMG_WINDOW::KFIELD(1%, 1%) = 2%

		COM (MO_MAIN_ORDERLINE_FRM) FRM$(15%)

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

		!
		! Load in defaults for chart
		!
		CALL READ_DEFAULTS(SMG_WINDOW)

510		%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.OPN"


		GET #IC_CONTROL.CH%, RECORD 1%, REGARDLESS
		CLOSE IC_CONTROL.CH%

		XSTART.DATE$ = OE_ORDERJOUR::SHIPDATE

		V% = READ_PERIOD("DATE", IC_CONTROL::ERA, XPERIOD$, &
			XPER.DESC$, XSTATUS$, XSTART.DATE$, &
			XFINISH.DATE$, XAGE%)


		!
		! Declare channels
		!
700		IF MO_ORDERLINE.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF MO_ORDERLINE.READONLY%

			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[MO.OPEN]MO_ORDERLINE.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			MO_MAIN_ORDERLINE = ERR
			CONTINUE 770
		END WHEN

		MO_ORDERLINE.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[MO.OPEN]MO_ORDERLINE.OPN"
		USE
			MO_MAIN_ORDERLINE = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		MO_ORDERLINE.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(MO_ORDERLINE.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = MO_ORDERLINE.CH%
		WHEN ERROR IN
			RESET #MO_ORDERLINE.CH%
			GET #MO_ORDERLINE.CH%, REGARDLESS
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

		DATA	01, 58, "(01) Line #", &
			01, 02, "(02) Make", &
			02, 02, "(03) Make Year", &
			03, 02, "(04) Make Type", &
			04, 02, "(05) Make Size", &
			05, 02, "(06) Model Code", &
			05, 58, "(07) SN", &
			06, 02, "(08) Qty Requested", &
			07, 02, "(09) Qty Invoiced", &
			08, 02, "(10) Qty Back", &
			06, 48, "(11) Unit Price", &
			07, 48, "(12) Discount", &
			08, 48, "(13) Unit Cost", &
			09, 02, "(14) Req Date", &
			10, 02, "(15) Notes", &
			09, 53, "Ext. Price", &
			0,   0, ""

		RESTORE

		READ XPOS%, YPOS%, XSTR$

		I% = 0%

		WHILE XPOS% <> 0%
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

 ReEnter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%
	!++
	! Abstract:FLD001
	!	^*(01) Line\*
	!	.b
	!	.lm +5
	!	The ^*Line\* field enters the line number to attach to.
	!	Use ^*NEWL\* for a brand new line.
	!	.lm -5
	!
	! Index:
	!	.x Make
	!
	!--
			MFLAG = MFLAG OR 1% IF OE_ORDERJOUR::REG_FLAG <> "Y"

			MO_ORDERLINE::LIN = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"1;71", TEMP$, MO_ORDERLINE::LIN, MFLAG, &
				"~L0'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_F14
				IF MAIN_WINDOW(MO_MAIN_REGLINE.ID, &
					"V0" + MO_ORDERLINE::ORDNUM) = 1%
				THEN
					MO_ORDERLINE::LIN = &
						MO_REGLINE::LIN
				END IF
				GOTO ReEnter

			END SELECT

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Make\*
	!	.b
	!	.lm +5
	!	The ^*Make\* field enters the Make of an auto
	!	manufacturer's vehicle (ie: MAZDA, FORD, DODGE, etc.)
	!	.b
	!	The field will accommodate ten (10) alphanumeric characters.
	!	.b
	!	Pressing ^*<List Choices>\* while the cursor is located at this field
	!	will cause valid choices to be displayed.
	!	.lm -5
	!
	! Index:
	!	.x Make
	!
	!--
			MFLAG = MFLAG OR 1% IF OE_ORDERJOUR::REG_FLAG = "Y" &
				AND MO_ORDERLINE::LIN <> "NEWL"

			MO_ORDERLINE::MAKE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"1;20", TEMP$, MO_ORDERLINE::MAKE, &
				MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF (MAIN_WINDOW(MO_MAIN_MAKE.ID, "VX") = 1%)
				THEN
					MO_ORDERLINE::MAKE  = MO_MAKE::MAKE
					MO_ORDERLINE::YEAR  = MO_MAKE::YEAR
					MO_ORDERLINE::MTYPE = MO_MAKE::MTYPE
					MO_ORDERLINE::MSIZE = MO_MAKE::MSIZE
				END IF

				GOTO ReEnter
			END IF

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Make Year\*
	!	.b
	!	.lm +5
	!	The ^*Make Year\* field enters the make year of a
	!	vehicle.
	!	.b
	!	The field will accommodate four (4) numeric characters. The format for this
	!	field is YYYY.
	!	.lm -5
	!
	! Index:
	!	.x Make Year
	!
	!--
			MFLAG = MFLAG OR 1% IF OE_ORDERJOUR::REG_FLAG = "Y" &
				AND MO_ORDERLINE::LIN <> "NEWL"

			MO_ORDERLINE::YEAR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"2;20", TEMP$, MO_ORDERLINE::YEAR, &
				MFLAG, "'E", MVALUE)

		CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Make Type\*
	!	.b
	!	.lm +5
	!	The ^*Make Type\* field enters the vehicle make type.
	!	The types are established in the Make Type Table.
	!	.b
	!	Pressing ^*List Choices\* while the cursor is located at this field will
	!	cause valid choices to be displayed.  The field will accommodate two (2)
	!	alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Make Type
	!
	!--
			MFLAG = MFLAG OR 1% IF OE_ORDERJOUR::REG_FLAG = "Y" &
				AND MO_ORDERLINE::LIN <> "NEWL"

			MO_ORDERLINE::MTYPE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"3;20", TEMP$, MO_ORDERLINE::MTYPE, &
				MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF (MAIN_WINDOW(MO_MAIN_MAKETYPE.ID, "VX") = 1%)
				THEN
					MO_ORDERLINE::MTYPE = MO_MAKETYPE::MTYPE
				END IF

				GOTO ReEnter
			END IF

		CASE 5%
	!++
	! Abstract:FLD005
	!	^*(05) Make Size\*
	!	.lm +5
	!	.b
	!	The ^*Make Size\* field enters the size
	!	of the Make.  Sizes are entered in the Make Size Table.
	!	.b
	!	Pressing ^*List Choices\* while the cursor is located at this field
	!	will cause valid choices to be displayed.
	!	.b
	!	The field will accommodate four (4) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Make Size
	!
	!--
			MFLAG = MFLAG OR 1% IF OE_ORDERJOUR::REG_FLAG = "Y" &
				AND MO_ORDERLINE::LIN <> "NEWL"

			MO_ORDERLINE::MSIZE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"4;20", TEMP$, MO_ORDERLINE::MSIZE, &
				MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF (MAIN_WINDOW(MO_MAIN_MAKESIZE.ID, "VX") = 1%)
				THEN
					MO_ORDERLINE::MSIZE = MO_MAKESIZE::MSIZE
				END IF

				GOTO ReEnter
			END IF

		CASE 6%
	!++
	! Abstract:FLD006
	!	^*(06) Model Code\*
	!	.lm +5
	!	.b
	!	The ^*Model Code\* field enters the model code related to a
	!	vehicle.
	!	.b
	!	Pressing ^*<List Choices>\* while the cursor is located at this field will
	!	cause valid model codes to be displayed.
	!	.b
	!	This field will accommodate up to four (4) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Model Code
	!
	!--
			MFLAG = MFLAG OR 1% IF OE_ORDERJOUR::REG_FLAG = "Y" &
				AND MO_ORDERLINE::LIN <> "NEWL"

			MO_ORDERLINE::MODELCODE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"5;20", TEMP$, MO_ORDERLINE::MODELCODE, &
				MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF (MAIN_WINDOW(MO_MAIN_MAKE_LINE.ID, "V0" + &
					MO_ORDERLINE::MAKE + &
					MO_MAKE_READ::YEAR + &
					MO_ORDERLINE::MTYPE + &
					MO_ORDERLINE::MSIZE) = 1%)
				THEN
					MO_ORDERLINE::MODELCODE = &
						MO_MAKELINE::MODELCODE
				END IF

				GOTO ReEnter
			END IF

		CASE 7%
	!++
	! Abstract:FLD007
	!	^*(07) Serial Number\*
	!	.b
	!	.lm +5
	!	The ^*Serial Number\* field enters a serial number
	!	associated with the make.
	!	.lm -5
	!
	! Index:
	!	.x Serial Number
	!
	!--
			MO_ORDERLINE::IDNUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"5;66", TEMP$, MO_ORDERLINE::IDNUM, &
				MFLAG, "'E", MVALUE)

		CASE 8%
	!++
	! Abstract:FLD008
	!	^*(08) Quantity Requested\*
	!	.b
	!	.lm +5
	!	The ^*Quantity Requested\* field enters the number of
	!	units which have been ordered on a particular order.
	!	.b
	!	The field will assume a whole number without entering any decimal point.
	!	.lm -5
	!
	! Index:
	!	.x Quantity
	!
	!--
			MFLAG = MFLAG OR 1% IF OE_ORDERJOUR::REG_FLAG = "Y" &
				AND MO_ORDERLINE::LIN <> "NEWL"

			MO_ORDERLINE::ORDQTY = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"6;20",TEMP$, MO_ORDERLINE::ORDQTY, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)

		CASE 9%
	!++
	! Abstract:FLD009
	!	^*(09) Quantity Invoiced\*
	!	.b
	!	.lm +5
	!	The ^*Quantity Invoiced\* field enters the number of
	!	units which have been sold.
	!	.b
	!	The field may contain a figure as large as 99,999.99.
	!	.lm -5
	!
	! Index:
	!	.x Quantity Sold
	!
	!--
			MO_ORDERLINE::SHPQTY = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"7;20", TEMP$, MO_ORDERLINE::SHPQTY, &
				MFLAG, TRM$(FRM$(MLOOP)), MVALUE)

		CASE 10%
	!++
	! Abstract:FLD010
	!	^*(10) Quantity on Backorder\*
	!	.b
	!	.lm +5
	!	The Quantity on Backorder field enters the number of
	!	units which have been back ordered for this
	!	particular product.
	!	.b
	!	The field may contain a figure as large as 99,999.99.
	!	.lm -5
	!
	! Index:
	!	.x Quantity on Backorder
	!
	!--
			MO_ORDERLINE::BCKQTY = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"8;20", TEMP$, MO_ORDERLINE::BCKQTY, &
				MFLAG, TRM$(FRM$(MLOOP)), MVALUE)

		CASE 11%
	!++
	! Abstract:FLD011
	!	^*(11) Requested Date\*
	!	.b
	!	.lm +5
	!	The ^*Requested Date\* field enters the date a particular
	!	Manufacturing Order Entry Line Item is requested.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!	.x Requested Date
	!
	!--
			MO_ORDERLINE::PRICE = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"6;65", TEMP$, MO_ORDERLINE::PRICE, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)

		CASE 12%
	!++
	! Abstract:FLD012
	!	.x Discount
	!	^*(12) Discount\*
	!	.b
	!	.lm +5
	!	The Discount field is to be entered if there is a discount on
	!	this particular product.
	!	.b
	!	The field may contain a discount percent as large as 99.99%.
	!	.lm -5
	!
	! Index:
	!
	!--
			MO_ORDERLINE::DISCOUNT = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"7;70", TEMP$, MO_ORDERLINE::DISCOUNT, &
				MFLAG, TRM$(FRM$(MLOOP)), MVALUE)

		CASE 13%
	!++
	! Abstract:FLD013
	!	^*(13) Unit Cost\*
	!	.b
	!	.lm +5
	!	The ^*Unit Cost\* field enters the unit cost of
	!	a particular Manufacturing Order Entry Line Item.
	!	.p
	!	.lm -5
	!
	! Index:
	!	.x Unit Cost
	!
	!--
			MO_ORDERLINE::COST = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"8;65",TEMP$, MO_ORDERLINE::COST, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)

		CASE 14%
	!++
	! Abstract:FLD014
	!	^*(14) Requested Date\*
	!	.b
	!	.lm +5
	!	The ^*Requested Date\* field enters the date a particular
	!	Manufacturing Order Entry Line Item is requested.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!	.x Requested Date
	!
	!--
			MFLAG = MFLAG OR 1% IF OE_ORDERJOUR::REG_FLAG = "Y" &
				AND MO_ORDERLINE::LIN <> "NEWL"

			MO_ORDERLINE::REQDATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"9;20",TEMP$, MO_ORDERLINE::REQDATE, MFLAG, &
				"'E", MVALUE)

		CASE 15%
	!++
	! Abstract:FLD015
	!	^*(15) Line Notes\*
	!	.b
	!	.lm +5
	!	The ^*Line Notes\* field enters up to forty (40)
	!	characters, which can be printed on the ticket form.
	!	.lm -5
	!
	! Index:
	!	.x Line Notes
	!
	!--

 FirstNote:
			MO_ORDERLINE::NOTES(0%) = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"10;20", TEMP$, MO_ORDERLINE::NOTES(0%), &
				MFLAG, "'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT
			CASE SMG$K_TRM_UP
				GOTO BypassNotes
			END SELECT

			GOTO BypassNotes IF MO_ORDERLINE::NOTES(0%) = "" &
				AND MO_ORDERLINE::NOTES(1%) = ""

 SecondNote:
			MO_ORDERLINE::NOTES(1%) = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"11;20", TEMP$, MO_ORDERLINE::NOTES(1%), &
				MFLAG, "'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT
			CASE SMG$K_TRM_UP
				GOTO FirstNote
			END SELECT

 BypassNotes:

		END SELECT

		SCOPE::PRG_ITEM = TEMP$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY

		MO_MAIN_ORDERLINE = 0%

		SELECT MLOOP

		CASE 1%
			!
			! See if the line is in the REGLINE file
			!
			IF OE_ORDERJOUR::REG_FLAG = "Y" AND &
				MO_ORDERLINE::LIN <> "NEWL"
			THEN
				IF MO_READ_REGLINE(MO_ORDERLINE::ORDNUM, &
					MO_ORDERLINE::LIN, "EQ", &
					MO_REGLINE_READ, QTY()) <> CMC$_NORMAL
				THEN
					MO_MAIN_ORDERLINE = 1%
					GOTO 28000
				END IF

				REMAIN_QTY = QTY(1%) - (QTY(2%) + QTY(3))

				MO_ORDERLINE::MAKE      = MO_REGLINE_READ::MAKE
				MO_ORDERLINE::YEAR      = MO_REGLINE_READ::YEAR
				MO_ORDERLINE::MTYPE     = MO_REGLINE_READ::MTYPE
				MO_ORDERLINE::MSIZE     = MO_REGLINE_READ::MSIZE
				MO_ORDERLINE::MODELCODE = MO_REGLINE_READ::MODELCODE
				MO_ORDERLINE::PRODUCT   = MO_REGLINE_READ::PRODUCT
				MO_ORDERLINE::ORDQTY    = QTY(1%)
				MO_ORDERLINE::SHPQTY    = REMAIN_QTY
				MO_ORDERLINE::BCKQTY    = 0.0
				MO_ORDERLINE::PRICE     = MO_REGLINE_READ::PRICE
				MO_ORDERLINE::DISCOUNT  = MO_REGLINE_READ::DISCOUNT
				MO_ORDERLINE::COST      = &
					PC_READ_COST(MO_REGLINE_READ::IDNUM, &
					OE_ORDERJOUR::LOCATION, &
					OE_ORDERJOUR::ORDDATE, "")
				MO_ORDERLINE::REQDATE   = MO_REGLINE_READ::TDATE
				MO_ORDERLINE::NOTES(0%) = MO_REGLINE_READ::NOTES(0%)
				MO_ORDERLINE::NOTES(1%) = MO_REGLINE_READ::NOTES(1%)
				MO_ORDERLINE::IDNUM     = MO_REGLINE_READ::IDNUM

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					FORMAT$(MO_ORDERLINE::PRICE,TRM$(FRM$(12%))) , 6%, 65%, , SMG$M_BOLD)

			ELSE
				MO_ORDERLINE::IDNUM = CONV_STRING(OE_ORDERJOUR::ORDNUM,CMC$_LEFT) &
					IF SMG_WINDOW::HFLAG(7%) <> 1%
			END IF

		CASE 2%
			MO_MAIN_ORDERLINE = FUNC_TESTENTRY(SMG_WINDOW, &
				MO_ORDERLINE::MAKE, &
				MO_MAKE::DESCR, &
				"MO", MLOOP, "PROG", &
				"Make", MO_MAIN_MAKE.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(MO_MAKE::DESCR, 25%), &
				1%, 32%, , SMG$M_BOLD)

		CASE 3%
			IF MO_ORDERLINE::YEAR = ""
			THEN
				MO_MAIN_ORDERLINE = 1%
			END IF

		CASE 4%
			MO_MAIN_ORDERLINE = FUNC_TESTENTRY(SMG_WINDOW, &
				MO_ORDERLINE::MTYPE, &
				MO_MAKETYPE::DESCR, &
				"MO", MLOOP, "PROG", &
				"Make Type", MO_MAIN_MAKETYPE.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				MO_MAKETYPE::DESCR, 3%, 25%, , SMG$M_BOLD)

		CASE 5%
			MO_MAIN_ORDERLINE = FUNC_TESTENTRY(SMG_WINDOW, &
				MO_ORDERLINE::MSIZE, &
				MO_MAKESIZE::DESCR, &
				"MO", MLOOP, "PROG", &
				"Make Size", MO_MAIN_MAKESIZE.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				MO_MAKESIZE::DESCR, 4%, 25%, , SMG$M_BOLD)

			IF MO_READ_MAKE(MO_ORDERLINE::MAKE, &
				MO_ORDERLINE::YEAR, MO_ORDERLINE::MTYPE, &
				MO_ORDERLINE::MSIZE, MO_MAKE_READ) <> CMC$_NORMAL
			THEN
				CALL HELP_34MESSAGE(SCOPE, &
					"Invalid Make, Year, Type, or Size", &
					"W", SCOPE::PRG_PROGRAM, "", "INVMAKE")

				MO_MAIN_ORDERLINE = 1%
				GOTO 28000
			END IF

		CASE 6%
			MO_MAIN_ORDERLINE = FUNC_TESTENTRY(SMG_WINDOW, &
				MO_ORDERLINE::MODELCODE, &
				MO_MODELCODE::DESCR, &
				"MO", MLOOP, "PROG", &
				"Model Code", MO_MAIN_MODELCODE.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(MO_MODELCODE::DESCR, &
				LEN(MO_MODELCODE::DESCR) - 8%), &
				5%, 25%, , SMG$M_BOLD)

			IF MAIN_WINDOW(MO_MAIN_MAKE_LINE.ID, "Q0" + &
				MO_ORDERLINE::MAKE + MO_MAKE_READ::YEAR + &
				MO_ORDERLINE::MTYPE + MO_ORDERLINE::MSIZE + &
				MO_ORDERLINE::MODELCODE) <> 1%
			THEN
				CALL HELP_34MESSAGE(SCOPE, &
					"Invalid Model Code for Make," + &
					" Year, Type, or Size", &
					"W", SCOPE::PRG_PROGRAM, &
					"", "INVMODELCODE")

				GOTO 28000
			END IF

			IF MAIN_WINDOW(MO_MAIN_MODEL.ID, "Q0" + &
				MO_ORDERLINE::MODELCODE + &
				MO_ORDERLINE::MSIZE + MO_MAKE_READ::CLASS) <> 1%
			THEN
				MO_ORDERLINE::PRODUCT = ""
			ELSE
				MO_ORDERLINE::PRODUCT = MO_MODEL::PRODUCT
			END IF

			IF MO_ORDERLINE::LIN = "NEWL"
			THEN
				MO_ORDERLINE::PRICE = PC_READ_PRICE( &
					MO_ORDERLINE::PRODUCT, &
					OE_ORDERJOUR::LOCATION, &
					AR_35CUSTOM::TTYPE, &
					OE_ORDERJOUR::ORDDATE, "", "", "")

			END IF

		CASE 7%
			IF MO_ORDERLINE::IDNUM <> ""
			THEN
				MO_ORDERLINE::COST = PC_READ_COST( &
					MO_ORDERLINE::IDNUM, &
					OE_ORDERJOUR::LOCATION, &
					OE_ORDERJOUR::ORDDATE, "")
			END IF

			IF MO_ORDERLINE::COST = 0.0
			THEN
				MO_ORDERLINE::COST = PC_READ_COST( &
					MO_ORDERLINE::PRODUCT, &
					OE_ORDERJOUR::LOCATION, &
					OE_ORDERJOUR::ORDDATE, "")
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				FORMAT$(MO_ORDERLINE::COST,TRM$(FRM$(13%))), &
				8%, 65%, , SMG$M_BOLD)

		CASE 9%
			EXTENT = FUNC_ROUND(MO_ORDERLINE::ORDQTY * &
				MO_ORDERLINE::PRICE, 2%)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				FORMAT$(EXTENT, "#,###,###.##"), 9%, 63%, , &
				SMG$M_BOLD)

		CASE 10%
			IF MVALUE = "ADD"
			THEN
				CALL SUBR_TRANTYPE(OE_ORDERJOUR::CUSNUM, &
					MO_ORDERLINE::LIN, &
					MO_ORDERLINE::ORDQTY, &
					MO_ORDERLINE::SHPQTY, &
					MO_ORDERLINE::BCKQTY, &
					TRANTYPE$(), TRANQTY())

				FOR I% = 1% TO 3%

					SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
						RIGHT(TRANTYPE$(I%), 3%), I% + 5%, 32%, , SMG$M_BOLD)

				NEXT I%
			END IF

		END SELECT

	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(2%) AND 2%) = 0%
		THEN
			IF MAIN_WINDOW(MO_MAIN_MAKE.ID, "Q0" + &
				MO_ORDERLINE::MAKE) <> 1%
			THEN
				MO_MAKE::DESCR = &
					STRING$(LEN(MO_MAKE::DESCR), A"?"B)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(MO_MAKE::DESCR, 25%), 1%, 32%, , &
				SMG$M_BOLD)

		END IF

		IF (SMG_WINDOW::HFLAG(4%) AND 2%) = 0%
		THEN
			IF MAIN_WINDOW(MO_MAIN_MAKETYPE.ID, "Q0" + &
				MO_ORDERLINE::MTYPE) <> 1%
			THEN
				MO_MAKETYPE::DESCR = &
					STRING$(LEN(MO_MAKETYPE::DESCR), A"?"B)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				MO_MAKETYPE::DESCR, 3%, 25%, , SMG$M_BOLD)

		END IF

		IF (SMG_WINDOW::HFLAG(5%) AND 2%) = 0%
		THEN
			IF MAIN_WINDOW(MO_MAIN_MAKESIZE.ID, "Q0" + &
				MO_ORDERLINE::MSIZE) <> 1%
			THEN
				MO_MAKESIZE::DESCR = &
					STRING$(LEN(MO_MAKESIZE::DESCR), A"?"B)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				MO_MAKESIZE::DESCR, 4%, 25%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(6%) AND 2%) = 0%
		THEN
			IF MAIN_WINDOW(MO_MAIN_MODELCODE.ID, "Q0" + &
				MO_ORDERLINE::MODELCODE) <> 1%
			THEN
				MO_MODELCODE::DESCR = &
					STRING$(LEN(MO_MODELCODE::DESCR), A"?"B)
			END IF

			V% = MO_READ_MAKE(MO_ORDERLINE::MAKE, &
				MO_ORDERLINE::YEAR, MO_ORDERLINE::MTYPE, &
				MO_ORDERLINE::MSIZE, MO_MAKE_READ)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(MO_MODELCODE::DESCR, &
				LEN(MO_MODELCODE::DESCR) - 8%), 5%, 25%, , &
				SMG$M_BOLD)
		END IF

		CALL SUBR_TRANTYPE(OE_ORDERJOUR::CUSNUM, MO_ORDERLINE::LIN, &
			MO_ORDERLINE::ORDQTY, &
			MO_ORDERLINE::SHPQTY, MO_ORDERLINE::BCKQTY, &
			TRANTYPE$(), TRANQTY())

		FOR I% = 1% TO 3%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				RIGHT(TRANTYPE$(I%), 3%), I% + 5%, 32%, , &
				SMG$M_BOLD)
		NEXT I%

		IF (SMG_WINDOW::HFLAG(8%) AND 2%) = 0%
		THEN
			EXTENT = FUNC_ROUND(MO_ORDERLINE::ORDQTY * &
				MO_ORDERLINE::PRICE, 2%)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				FORMAT$(EXTENT, "#,###,###.##"), 9%, 63%, , &
				SMG$M_BOLD)
		END IF

	!
	! Set MO_ORDERLINE_OLD value
	!
20500	CASE OPT_SETOLD
		MO_ORDERLINE_OLD = MO_ORDERLINE

	!
	! Restore MO_ORDERLINE_OLD value
	!
	CASE OPT_RESETOLD
		MO_ORDERLINE = MO_ORDERLINE_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		MO_ORDERLINE_DEF = MO_ORDERLINE

		IF MFLAG = 1%
		THEN
			SELECT MLOOP

			CASE 0%
				FRM$(8%)  = "###,###.##"
				FRM$(9%)  = "###,###.##"
				FRM$(10%)  = "###,###.##"
				FRM$(11%) = "###,###.##"
				FRM$(12%) = "##.##%"
				FRM$(13%) = "###,###.##"

			CASE ELSE
				FRM$(MLOOP) = MVALUE
			END SELECT
		END IF

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		MO_ORDERLINE = MO_ORDERLINE_DEF

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!
		MO_ORDERLINE::ORDNUM = MVALUE

		IF MFLAG = 1%
		THEN
			MO_ORDERLINE::LIN = "NEWL"

			MO_ORDERLINE::REQDATE = OE_ORDERJOUR::SHIPDATE &
				IF MO_ORDERLINE::REQDATE=""
		END IF

	!
	! Find the next record
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			IF OE_ORDERJOUR::REG_FLAG = "Y"
			THEN
				FIND #SMG_WINDOW::CHAN, &
					KEY #0% GE MO_ORDERLINE::ORDNUM + &
					MO_ORDERLINE::LIN, REGARDLESS
			ELSE
				FIND #SMG_WINDOW::CHAN, &
					KEY #0% GE MO_ORDERLINE::ORDNUM + "", &
					REGARDLESS
			END IF

		CASE 1%
			FIND #SMG_WINDOW::CHAN, &
				KEY #1% GE MO_ORDERLINE::MAKE + &
				MO_ORDERLINE::ORDNUM + &
				MO_ORDERLINE::LIN, REGARDLESS

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
			MVALUE = "  Make       Year Type Size Model" + &
				" OrderQty  UnitPrice     ExtPrice"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "013,018,023,028,034,043,054"

		!
		! Convert current record into text
		!
		CASE 3%
			EXTENT = FUNC_ROUND(MO_ORDERLINE::ORDQTY * &
				MO_ORDERLINE::PRICE, 2%)

			EXTENT$ = FORMAT$(EXTENT, "##,###,###.##")

			MVALUE = MO_ORDERLINE::MAKE + " "   + &
				MO_ORDERLINE::YEAR + " "   + &
				MO_ORDERLINE::MTYPE + "   " + &
				MO_ORDERLINE::MSIZE + " "   + &
				MO_ORDERLINE::MODELCODE + "  "  + &
				FORMAT$(MO_ORDERLINE::ORDQTY, "#####.##") + " "   + &
				FORMAT$(MO_ORDERLINE::PRICE, "###,###.##") + &
				EXTENT$
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
				IF MO_ORDERLINE::PRODUCT <> ""
				THEN
					CALL SUBR_TRANTYPE(OE_ORDERJOUR::CUSNUM,MO_ORDERLINE::LIN, &
						MO_ORDERLINE::ORDQTY, &
						MO_ORDERLINE::SHPQTY, MO_ORDERLINE::BCKQTY, &
						TRANTYPE$(), TRANQTY())

					FOR I% = 1% TO VAL%(TRANTYPE$(0%))

						V% = IC_WRIT_35BALANCE (MO_ORDERLINE::PRODUCT, &
							OE_ORDERJOUR::LOCATION, &
							LEFT(TRANTYPE$(I%), 2%), &
							-TRANQTY(I%))
					NEXT I%

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
					FIND #SMG_WINDOW::CHAN, &
						KEY #0% GE MVALUE + &
						MO_ORDERLINE::LIN, REGARDLESS
				USE
					CONTINUE 28000
				END WHEN

			CASE 1%
				WHEN ERROR IN
					FIND #SMG_WINDOW::CHAN, &
						KEY #1% GE MO_ORDERLINE::MAKE + &
						MVALUE + &
						MO_ORDERLINE::LIN, REGARDLESS
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

			IF MO_ORDERLINE::ORDNUM = MVALUE
			THEN
				SMG_WINDOW::CURREC = 0%
			END IF

		!
		! Change key
		!
		CASE 6%
			MO_ORDERLINE::ORDNUM = MVALUE

		END SELECT

	CASE OPT_AFTEROPT

		SELECT MVALUE

		CASE "Add"
			IF MO_ORDERLINE::PRODUCT <> ""
			THEN
				CALL SUBR_TRANTYPE(OE_ORDERJOUR::CUSNUM, &
					MO_ORDERLINE::LIN, &
					MO_ORDERLINE::ORDQTY, &
					MO_ORDERLINE::SHPQTY, &
					MO_ORDERLINE::BCKQTY, &
					TRANTYPE$(), TRANQTY())

				FOR I% = 1% TO VAL%(TRANTYPE$(0%))

					V% = IC_WRIT_35BALANCE(MO_ORDERLINE::PRODUCT, &
						OE_ORDERJOUR::LOCATION, &
						LEFT(TRANTYPE$(I%), 2%), &
						TRANQTY(I%))
				NEXT I%
			END IF

		CASE "Change", "Blank", "Initialize"
			IF MO_ORDERLINE_OLD::PRODUCT <> ""
			THEN
				CALL SUBR_TRANTYPE(OE_ORDERJOUR::CUSNUM, &
					MO_ORDERLINE::LIN, &
					MO_ORDERLINE_OLD::ORDQTY, &
					MO_ORDERLINE_OLD::SHPQTY, &
					MO_ORDERLINE_OLD::BCKQTY, &
					TRANTYPE$(), TRANQTY())

				FOR I% = 1% TO VAL%(TRANTYPE$(0%))

					V% = IC_WRIT_35BALANCE(MO_ORDERLINE::PRODUCT, &
						OE_ORDERJOUR::LOCATION, &
						LEFT(TRANTYPE$(I%), 2%), &
						-TRANQTY(I%))
				NEXT I%
			END IF

			IF MO_ORDERLINE::PRODUCT <> ""
			THEN
				CALL SUBR_TRANTYPE(OE_ORDERJOUR::CUSNUM, &
					MO_ORDERLINE::LIN, &
					MO_ORDERLINE::ORDQTY, &
					MO_ORDERLINE::SHPQTY, &
					MO_ORDERLINE::BCKQTY, &
					TRANTYPE$(), TRANQTY())

				FOR I% = 1% TO VAL%(TRANTYPE$(0%))

					V% = IC_WRIT_35BALANCE(MO_ORDERLINE::PRODUCT, &
						OE_ORDERJOUR::LOCATION, &
						LEFT(TRANTYPE$(I%), 2%), &
						TRANQTY(I%))

				NEXT I%
			END IF

		CASE "Erase"
			IF MLOOP <> 1%
			THEN
				IF MO_ORDERLINE::PRODUCT <> ""
				THEN
					CALL SUBR_TRANTYPE(OE_ORDERJOUR::CUSNUM,MO_ORDERLINE::LIN, &
						MO_ORDERLINE::ORDQTY, &
						MO_ORDERLINE::SHPQTY, &
						MO_ORDERLINE::BCKQTY, &
						TRANTYPE$(), TRANQTY())

					FOR I% = 1% TO VAL%(TRANTYPE$(0%))

						V% = IC_WRIT_35BALANCE(MO_ORDERLINE::PRODUCT, &
							OE_ORDERJOUR::LOCATION, &
							LEFT(TRANTYPE$(I%), 2%), &
							-TRANQTY(I%))

					NEXT I%
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
	!+-+-+
	!++
	! Abstract:OPTIONS
	!	^*Option\*
	!	.lm +5
	!	.b
	!	^*Option\* enters options for a specific order.
	!	.b
	!	The fields in the option screen include:
	!	.table 3,25
	!	.te
	!	Option Group
	!	.te
	!	Option
	!	.te
	!	Description
	!	.te
	!	Quantity
	!	.te
	!	Unit Price
	!	.te
	!	Unit Cost
	!	.end table
	!	.b
	!	The Extended Price is calculated and displayed.
	!	.lm -5
	!
	! Index:
	!
	!--
