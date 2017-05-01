1	%TITLE "Manufacturing Order Line Register"
	%SBTTL "MO_MAIN_QUERYLINE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG MO_MAIN_QUERYLINE(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	Manufacturing Order Line Query is used to view the models
	!	and associated options in the Order Register.
	!	.lm -5
	!
	! Index:
	!	.x Help>Manufacturing Order Line Query
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS MO_SOURCE:MO_MAIN_QUERYLINE/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN MO_MAIN_QUERYLINE
	!	$ DELETE MO_MAIN_QUERYLINE.OBJ;*
	!
	! Author:
	!
	!	03/28/91 - Val James Allen
	!
	! Modification history:
	!
	!
	!	04/27/92 - Kevin Handy
	!		Clean up (check)
	!
	!	11/24/92 - Dan Perkins
	!		Added CASE 2 to OPT_SUBWIND so that VIEW would
	!		work properly.
	!
	!	04/02/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	06/26/96 - Kevin Handy
	!		Reformat source code.
	!
	!	07/25/96 - Kevin Handy
	!		Change references from MO_REGHEADER to
	!		OE_REGHEADER.
	!
	!	07/30/97 - Kevin Handy
	!		Change XAGE parameter of READ_PERIOD to integer
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/10/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	06/09/99 - Kevin Handy
	!		Lose line 281?? (Dead code)
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
	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[MO.OPEN]MO_REGLINE.HB"
	MAP (MO_REGLINE)	MO_REGLINE_CDD		MO_REGLINE
	MAP (MO_REGLINE_OLD)	MO_REGLINE_CDD		MO_REGLINE_OLD, &
							MO_REGLINE_DEF, MO_REGLINE_HOLD

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.HB"
	MAP (OE_REGHEADER)	OE_REGHEADER_CDD	OE_REGHEADER
	MAP (OE_REGHEADER_ONE)	OE_REGHEADER_CDD	OE_REGHEADER_ONE

	%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.HB"
	MAP (IC_CONTROL)	IC_CONTROL_CDD		IC_CONTROL

	%INCLUDE "SOURCE:[MO.OPEN]MO_MAKELINE.HB"
	MAP (MO_MAKELINE)	MO_MAKELINE_CDD		MO_MAKELINE

	%INCLUDE "SOURCE:[MO.OPEN]MO_MAKE.HB"
	MAP(MO_MAKE)		MO_MAKE_CDD		MO_MAKE

	%INCLUDE "SOURCE:[MO.OPEN]MO_MODELCODE.HB"
	MAP (MO_MODELCODE)	MO_MODELCODE_CDD	MO_MODELCODE

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
	COM (CH_OE_REGHEADER) &
		BATCH_NO$ = 2%, &
		OE_REGHEADER.CH%, &
		XPERIOD$ = 6%

	COM (CH_MO_REGLINE) &
		MO_REGLINE.CH%, &
		MO_REGLINE.READONLY%, &
		MO_MODELLINE.CH%, &
		MO_OPTGROUP.CH%

	COM (CH_MO_YEARCLASS) &
		MAKE_YEAR$ = 4%, &
		MAKE_CLASS$ = 4%

	COM (CH_MO_ARRAY) &
		INV_ARRAY$(100%), &
		INV_COUNT%

	COM (TT_MO_MAIN_QUERYLINE) &
		TAXTITLE$ = 20%, &
		TAXTYPE$(7%) = 40%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION MAIN_WINDOW
	EXTERNAL LONG   FUNCTION IC_WRIT_35BALANCE

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Manufacturing Order Register Lines Query"
		SMG_WINDOW::CURREC = -2%
		SMG_WINDOW::NHELP = "MO_MAIN_QUERYLINE"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS = 2%
		SMG_WINDOW::VPOS = 2%
		SMG_WINDOW::NITEMS = 16%
		SMG_WINDOW::FLAGS =  0%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::VHPOS = 2%
		SMG_WINDOW::VVPOS = 2%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Line"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		TAXTITLE$ = "Type    Description"
		TAXTYPE$(0%) = "03"
		TAXTYPE$(1%) = "01      ON ORDER"
		TAXTYPE$(2%) = "02      SHIPPING"
		TAXTYPE$(3%) = "03      CANCEL"

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

		!
		! Load in defaults for chart
		!
		CALL READ_DEFAULTS(SMG_WINDOW)

		%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.OPN"


510		GET #IC_CONTROL.CH%, RECORD 1%, REGARDLESS

		XSTART.DATE$ = OE_REGHEADER::ORDDATE

		V% = READ_PERIOD("DATE", IC_CONTROL::ERA, XPERIOD$, &
			XPER.DESC$, XSTATUS$, XSTART.DATE$, &
			XFINISH.DATE$, XAGE%)


		!
		! Declare channels
		!
700		IF MO_REGLINE.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF MO_REGLINE.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[MO.OPEN]MO_REGLINE.OPN"
		USE
			CONTINUE 760 IF ERR = 10%
			MO_MAIN_QUERYLINE = ERR
			CONTINUE 770
		END WHEN

		MO_REGLINE.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[MO.OPEN]MO_REGLINE.OPN"
		USE
			MO_MAIN_QUERYLINE = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		MO_REGLINE.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(MO_REGLINE.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = MO_REGLINE.CH%
		WHEN ERROR IN
			RESET #MO_REGLINE.CH%
			GET #MO_REGLINE.CH%, REGARDLESS
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


		DATA	02,02, " Line", &
			03,02, " Trans Type", &
			04,02, " Trans Date", &
			05,02, " Make", &
			06,02, " Make Year", &
			07,02, " Make Type", &
			08,02, " Make Size", &
			09,02, " Model Code", &
			10,02, " Qty Ordered", &
			11,02, " Unit Price", &
			12,02, " Unit Cost", &
			13,02, " Req Date", &
			11,40, "Ext. Price", &
			14,02, " Post Date", &
			15,02, " Post Time", &
			16,02, " Batch #", &
			17,02, " Ship No", &
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

 ReEnter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%
	!++
	! Abstract:FLD001
	!	^*Line Number\*
	!	.b
	!	.lm +5
	!	The ^*Line Number\* field indicates the Line Number
	!	of the order register line.
	!	.lm -5
	!
	! Index:
	!	.x Line Number
	!
	!--
		MO_REGLINE::LIN = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
			"2;25", TEMP$, &
			MO_REGLINE::LIN, MFLAG, "~L0'E", MVALUE)

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*Transaction Type\*
	!	.b
	!	.lm +5
	!	The ^*Transaction Type\* field indicates the Transaction Type
	!	for the line.
	!	.b
	!	Valid Transaction Types are:
	!	.table 3,25
	!	.te
	!	^*01\* = Ordered
	!	.te
	!	^*02\* = Shipping
	!	.te
	!	^*03\* = Cancel
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Transaction Type
	!
	!--
		MO_REGLINE::TRANTYPE = EDIT$(ENTR_3STRINGLIST(SCOPE, SMG_WINDOW::WNUMBER, &
			"3;25", TEMP$, &
			MO_REGLINE::TRANTYPE, MFLAG, "'E", MVALUE, TAXTYPE$(), TAXTITLE$, "008"), -1%)

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*Transaction Date\*
	!	.b
	!	.lm +5
	!	The ^*Transaction Date\* field indicates the date of the
	!	transaction.
	!	.lm -5
	!
	! Index:
	!	.x Transaction Date
	!
	!--
		MO_REGLINE::TDATE = ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
			"4;25", TEMP$, &
			MO_REGLINE::TDATE, MFLAG, "'E", MVALUE)

		CASE 4%
	!++
	! Abstract:FLD004
	!	^*Make\*
	!	.b
	!	.lm +5
	!	The ^*Make\* field shows the Make of the Dealer's
	!	Model (ie: MAZDA, FORD, DODGE, etc.)
	!	.lm -5
	!
	! Index:
	!	.x Make
	!
	!--
		IF TEMP$ = "Add" AND (MFLAG AND 1%) = 0%
		THEN
			GOSUB 28500
		END IF

		MO_REGLINE::MAKE = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
			"5;25", TEMP$, &
			MO_REGLINE::MAKE, MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF (MAIN_WINDOW(MO_MAIN_MAKE.ID, "VX") = 1%)
				THEN
					MO_REGLINE::MAKE = MO_MAKE::MAKE
					MO_REGLINE::YEAR = MO_MAKE::YEAR
					MO_REGLINE::MTYPE = MO_MAKE::MTYPE
					MO_REGLINE::MSIZE = MO_MAKE::MSIZE
				END IF
				GOTO ReEnter
			END IF

		CASE 5%
	!++
	! Abstract:FLD005
	!	^*Make Year\*
	!	.b
	!	.lm +5
	!	The ^*Make Year\* field shows the year of the Make.
	!	.lm -5
	!
	! Index:
	!	.x Make Year
	!
	!--
		MO_REGLINE::YEAR = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
			"6;25", TEMP$, &
			MO_REGLINE::YEAR, MFLAG, "'E", MVALUE)

		CASE 6%
	!++
	! Abstract:FLD006
	!	^*Make Type\*
	!	.b
	!	.lm +5
	!	The ^*Make Type\* field indicates the type
	!	of the Make.
	!	.lm -5
	!
	! Index:
	!	.x Make Type
	!
	!--
		MO_REGLINE::MTYPE = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
			"7;25", TEMP$, &
			MO_REGLINE::MTYPE, MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF (MAIN_WINDOW(MO_MAIN_MAKETYPE.ID, "VX") = 1%)
				THEN
					MO_REGLINE::MTYPE = MO_MAKETYPE::MTYPE
				END IF
				GOTO ReEnter
			END IF

		CASE 7%
	!++
	! Abstract:FLD007
	!	^*Make Size\*
	!	.b
	!	.lm +5
	!	The ^*Make Size\* field indicates the size
	!	of the Make.
	!	.lm -5
	!
	! Index:
	!	.x Make Size
	!
	!--
		MO_REGLINE::MSIZE = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
			"8;25", TEMP$, &
			MO_REGLINE::MSIZE, MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF (MAIN_WINDOW(MO_MAIN_MAKESIZE.ID, "VX") = 1%)
				THEN
					MO_REGLINE::MSIZE = MO_MAKESIZE::MSIZE
				END IF
				GOTO ReEnter
			END IF

		CASE 8%
	!++
	! Abstract:FLD008
	!	^*Model Code\*
	!	.b
	!	.lm +5
	!	The ^*Model Code\* field indicates the model code
	!	of the Make.
	!	.lm -5
	!
	! Index:
	!	.x Model Code
	!
	!--
		MO_REGLINE::MODELCODE = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
			"9;25", TEMP$, &
			MO_REGLINE::MODELCODE, MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF (MAIN_WINDOW(MO_MAIN_MAKE_LINE.ID, "V0" + &
					MO_REGLINE::MAKE + MAKE_YEAR$ + &
					MO_REGLINE::MTYPE + MO_REGLINE::MSIZE) = 1%)
				THEN
					MO_REGLINE::MODELCODE = MO_MAKELINE::MODELCODE
				END IF
				GOTO ReEnter
			END IF

		CASE 9%
	!++
	! Abstract:FLD009
	!	^*Order Quantity\*
	!	.b
	!	.lm +5
	!	The ^*Order Quantity\* field indicates the number of
	!	units which have been ordered on the particular date for this
	!	particular model.
	!	.lm -5
	!
	! Index:
	!	.x Quantity
	!
	!--
			MO_REGLINE::QTY = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"10;25",TEMP$, MO_REGLINE::QTY, MFLAG, &
				"#,###,###.##", MVALUE)

		CASE 10%
	!++
	! Abstract:FLD010
	!	^*Unit Price\*
	!	.b
	!	.lm +5
	!	The Unit Price field indicates the price of
	!	a particular product.  If a unique product is available in a
	!	particular unit of measure, i.e. "gallon", "piece", "yard", etc.,
	!	that price is indicated in this field.
	!	.lm -5
	!
	! Index:
	!	.x Unit Price
	!
	!--
			MO_REGLINE::PRICE = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"11;25",TEMP$, MO_REGLINE::PRICE, MFLAG, &
				"#,###,###.##", MVALUE)

		CASE 11%
	!++
	! Abstract:FLD011
	!	^*Cost/Unit\*
	!	.b
	!	.lm +5
	!	The ^*Cost/Unit\* field shows the cost of
	!	a particular unit.
	!	.b
	!	If a unique product is available in a particular unit
	!	of measure, i.e. "gallon", "piece", "yard", that cost
	!	is indicated in this field.
	!	.lm -5
	!
	! Index:
	!	.x Cost/Unit
	!
	!--
			MO_REGLINE::COST = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"12;25",TEMP$, MO_REGLINE::COST, MFLAG, &
				"#,###,###.##", MVALUE)

		CASE 12%
	!++
	! Abstract:FLD012
	!	^*Request Date\*
	!	.b
	!	.lm +5
	!	The Request Date field indicates the date
	!	the product is requested.
	!	.lm -5
	!
	! Index:
	!	.x Request Date
	!
	!--
			IF (TEMP$ = "Add") AND (MO_REGLINE::TDATE = "")
			THEN
				MO_REGLINE::TDATE = OE_REGHEADER::ORDDATE
			END IF

			MO_REGLINE::TDATE = ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"13;25",TEMP$, MO_REGLINE::TDATE, MFLAG, &
				"'E", MVALUE)

		CASE 13%
	!++
	! Abstract:FLD013
	!	^*(13) Post Date\*
	!	.b
	!	.lm +5
	!	The Post Date field indicates the date the
	!	line transaction was posted to this file.
	!	.lm -5
	!
	! Index:
	!	.x Post Date
	!
	!--
			MO_REGLINE::POSTDATE = ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"14;25",TEMP$, MO_REGLINE::POSTDATE, MFLAG, &
				"'E", MVALUE)

		CASE 14%
	!++
	! Abstract:FLD014
	!	^*(14) Post Time\*
	!	.b
	!	.lm +5
	!	The Post Time field shows the particular time
	!	the line transaction was posted to this file.
	!	.lm -5
	!
	! Index:
	!	.x Post Time
	!
	!--
			MO_REGLINE::POSTTIME = ENTR_3TIME(SCOPE, SMG_WINDOW::WNUMBER, &
				"15;25",TEMP$, MO_REGLINE::POSTTIME, MFLAG, &
				"'E", MVALUE)

		CASE 15%
	!++
	! Abstract:FLD015
	!	^*Batch Number\*
	!	.b
	!	.lm +5
	!	The ^*Batch Number\* field indicates the Batch
	!	Nunber of the line transaction that was posted to this file.
	!	.lm -5
	!
	! Index:
	!	.x Batch Number
	!
	!--
			MO_REGLINE::BATCH = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"16;25",TEMP$, MO_REGLINE::BATCH, MFLAG, &
				"'E", MVALUE)

		CASE 16%
	!++
	! Abstract:FLD016
	!	^*Ship Number\*
	!	.b
	!	.lm +5
	!	The ^*Ship Number\* field shows the particular Shipment
	!	Nunber of the line transaction that was posted to this file.
	!	.lm -5
	!
	! Index:
	!	.x Ship Number
	!
	!--
			MO_REGLINE::SHIPNO = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"17;25",TEMP$, MO_REGLINE::SHIPNO, MFLAG, &
				"'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(6%) AND 2%) = 0%
		THEN
			IF MAIN_WINDOW(MO_MAIN_MAKETYPE.ID, "Q0" + MO_REGLINE::MTYPE) <> 1%
			THEN
				MO_MAKETYPE::DESCR = STRING$(40%, A"?"B)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				MO_MAKETYPE::DESCR, 7%, 35%,, SMG$M_BOLD)

		END IF

		IF (SMG_WINDOW::HFLAG(7%) AND 2%) = 0%
		THEN
			IF MAIN_WINDOW(MO_MAIN_MAKESIZE.ID, "Q0" + MO_REGLINE::MSIZE) <> 1%
			THEN
				MO_MAKESIZE::DESCR = STRING$(40%, A"?"B)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				MO_MAKESIZE::DESCR, 8%, 35%,, SMG$M_BOLD)

		END IF

		IF (SMG_WINDOW::HFLAG(8%) AND 2%) = 0%
		THEN
			IF MAIN_WINDOW(MO_MAIN_MODELCODE.ID, "Q0" + MO_REGLINE::MODELCODE) <> 1%
			THEN
				MO_MODELCODE::DESCR = STRING$(40%, A"?"B)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				MO_MODELCODE::DESCR, 9%, 35%,, SMG$M_BOLD)

		END IF

		IF (SMG_WINDOW::HFLAG(10%) AND 2%) = 0%
		THEN
			EXTENT = FUNC_ROUND(MO_REGLINE::QTY*MO_REGLINE::PRICE, 2%)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				FORMAT$(EXTENT, "#,###,###.##"), 11%, 61%, , SMG$M_BOLD)
		END IF

	!
	! Set MO_REGLINE_OLD value
	!
20500	CASE OPT_SETOLD
		MO_REGLINE_OLD = MO_REGLINE

	!
	! Restore MO_REGLINE_OLD value
	!
	CASE OPT_RESETOLD
		MO_REGLINE = MO_REGLINE_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		MO_REGLINE_DEF = MO_REGLINE

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		MO_REGLINE = MO_REGLINE_DEF

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!
		MO_REGLINE::ORDNUM = MVALUE

	!
	! Find the next record
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE MO_REGLINE::ORDNUM + &
				MO_REGLINE::LIN, REGARDLESS

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
			MVALUE = "  Line TType Make       Year Type Size Model TransQty  UnitPrice    ExtPrice"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "007,013,024,029,034,039,045,054,065"

		!
		! Convert current record into text
		!
		CASE 3%
			EXTENT  = FUNC_ROUND(MO_REGLINE::QTY*MO_REGLINE::PRICE, 2%)
			EXTENT$ = FORMAT$(EXTENT, "#,###,###.##")

			MVALUE = MO_REGLINE::LIN                   + " "     + &
				MO_REGLINE::TRANTYPE                 + "    "  + &
				MO_REGLINE::MAKE                     + " "     + &
				MO_REGLINE::YEAR                     + " "     + &
				MO_REGLINE::MTYPE                    + "   "   + &
				MO_REGLINE::MSIZE                    + " "     + &
				MO_REGLINE::MODELCODE                + " "     + &
				FORMAT$(MO_REGLINE::QTY, "#####.##") + " "     + &
				FORMAT$(MO_REGLINE::PRICE, "###,###.##") + " " + &
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
				IF MO_REGLINE::PRODUCT <> ""
				THEN
					V% = IC_WRIT_35BALANCE (MO_REGLINE::PRODUCT, &
						OE_REGHEADER::LOCATION, "MO", &
						MO_REGLINE::QTY)
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
						MO_REGLINE::LIN, REGARDLESS
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
			IF MO_REGLINE::ORDNUM = MVALUE
			THEN
				SMG_WINDOW::CURREC = 0%
			END IF

		!
		! Change key
		!
		CASE 6%
			MO_REGLINE::ORDNUM = MVALUE

		END SELECT

	CASE OPT_AFTEROPT

		SELECT MVALUE

		CASE "Add"
			IF MO_REGLINE::PRODUCT <> ""
			THEN
				V% = IC_WRIT_35BALANCE (MO_REGLINE::PRODUCT, &
					OE_REGHEADER::LOCATION, "MO", &
					-MO_REGLINE::QTY)
			END IF

		CASE "Change", "Blank", "Initialize"
			IF MO_REGLINE_OLD::PRODUCT <> ""
			THEN
				V% = IC_WRIT_35BALANCE(MO_REGLINE_OLD::PRODUCT, &
					OE_REGHEADER_ONE::LOCATION, "MO", &
					MO_REGLINE_OLD::QTY)
			END IF

			IF MO_REGLINE::PRODUCT <> ""
			THEN
				V% = IC_WRIT_35BALANCE (MO_REGLINE::PRODUCT, &
				OE_REGHEADER::LOCATION, "MO", &
				-MO_REGLINE::QTY)
			END IF

		CASE "Erase"
			IF MLOOP <> 1%
			THEN
				IF MO_REGLINE::PRODUCT <> ""
				THEN
					V% = IC_WRIT_35BALANCE(MO_REGLINE::PRODUCT, &
						OE_REGHEADER::LOCATION, "SO", &
						MO_REGLINE::QTY)
				END IF
			END IF

		END SELECT

	END SELECT

28000	EXIT FUNCTION

 !28100
	!*******************************************************************
	! Create a list of options and pass to option lines
	!*******************************************************************

	!
	! If the MO_MODELLINE file has not yet been opened, then open it.
	!
 !	GOTO 28105 IF MO_MODELLINE.CH% > 0%
 !
	!
	! Open main file (existing) for use
	!
 !	%INCLUDE "SOURCE:[MO.OPEN]MO_MODELLINE.OPN"


 !28105
	!
	! If the MO_OPTGROUP file has not yet been opened, then open it.
	!
 !	GOTO 28110 IF MO_OPTGROUP.CH% > 0%
 !
	!
	! Open main file (existing) for use
	!
 !	%INCLUDE "SOURCE:[MO.OPEN]MO_OPTGROUP.OPN"
 !
 !28110
	!
	! The model line item file should now be opened.  Search for
	! The first record for this modelcode, size and class.
	!
 !	INV_ARRAY% = 0%
 !
 !	INV_COUNT% = 0%
 !
 !	LASTTURKEY$ = ""
 !
 !	FIND #MO_MODELLINE.CH%, KEY #0% GE MO_REGLINE::MODELCODE + &
 !		MO_REGLINE::MSIZE + MAKE_CLASS$, REGARDLESS
 !
 !28120
	!
	! Skip out if done with these options
	!
 !	GET #MO_MODELLINE.CH%
 !
 !	GOTO 28200 IF MO_MODELLINE::MODELCODE + MO_MODELLINE::MSIZE + &
 !		MO_MODELLINE::CLASS <> MO_REGLINE::MODELCODE + &
 !		MO_REGLINE::MSIZE + MAKE_CLASS$
 !
 !	GOTO 28120 IF LASTTURKEY$ = MO_MODELLINE::OPTGROUP
 !
 !	LASTTURKEY$ = MO_MODELLINE::OPTGROUP
 !
 !	SEQNUM$ = "9999"
 !
 !28130	GET #MO_OPTGROUP.CH%, KEY #0% EQ MO_MODELLINE::OPTGROUP + "", REGARDLESS
 !
 !	SEQNUM$ = MO_OPTGROUP::SEQUENCE
 !
 !28140	TEXT$ = SEQNUM$ + MO_MODELLINE::OPTGROUP
 !
	!
	! Add (insertion) to list
	!
 !	GOTO 28150 IF TEXT$ < INV_ARRAY$(LOOP%) &
 !		FOR LOOP% = 1% TO INV_ARRAY%
 !
 !	LOOP% = INV_ARRAY% + 1%
 !
 !28150	INV_ARRAY$(LOOP1% + 1%) = INV_ARRAY$(LOOP1%) &
 !		FOR LOOP1% = INV_ARRAY% TO LOOP% STEP -1%
 !
 !	INV_ARRAY$(LOOP%) = TEXT$
 !	INV_ARRAY% = INV_ARRAY% + 1%
 !	INV_COUNT% = INV_COUNT% + 1%
 !
 !	GOTO 28120
 !
 !28200	RETURN

28500	!
	! Assign new line number
	!
	WORK_ITEM$      = "0"
	MO_REGLINE_HOLD = MO_REGLINE
	CHECK_ORDNUM$   = MO_REGLINE::ORDNUM

	WHEN ERROR IN
		FIND #MO_REGLINE.CH%, KEY #0% GE MO_REGLINE::ORDNUM + "", REGARDLESS
	USE
		CONTINUE 28600
	END WHEN

28510	WHEN ERROR IN
		GET #MO_REGLINE.CH%, REGARDLESS
	USE
		CONTINUE 28600
	END WHEN

	GOTO 28600 IF MO_REGLINE::ORDNUM <> CHECK_ORDNUM$

	WORK_ITEM$ = MO_REGLINE::LIN

	GOTO 28510

28600	MO_REGLINE = MO_REGLINE_HOLD

	MO_REGLINE::LIN = FORMAT$(VAL%(WORK_ITEM$) + 1%, "<0>###")

	RETURN

	%PAGE

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
	!+-+-+
	!++
	! Abstract:OPTIONS
	!	^*Manufacturing Order Option Register\*
	!	.b
	!	.lm +5
	!	^*Manufacturing Order Option Query\* is used to view options in the Order
	!	Register.
	!	.lm -5
	!
	! Index:
	!	.x Help>Manufacturing Order Option Query
	!
	!--
