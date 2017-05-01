1	%TITLE "Manufacturing Order Line Option Register"
	%SBTTL "MO_MAIN_REGLINEOPT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG MO_MAIN_REGLINEOPT(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	.p
	!	Order Line Option Register is used to enter options for
	!	a model.
	!
	! Index:
	!	.x Help>Order Line Option Register
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS MO_SOURCE:MO_MAIN_REGLINEOPT/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN MO_MAIN_REGLINEOPT
	!	$ DELETE MO_MAIN_REGLINEOPT.OBJ;*
	!
	! Author:
	!
	!	03/28/91 - Val James Allen
	!
	! Modification history:
	!
	!	09/17/91 - Deborah K. Fries
	!		Rearranged fields & resized windows
	!
	!	10/26/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	11/17/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	11/25/92 - Dan Perkins
	!		Added CASE 2 to OPT_SUBWIND so that VIEW would
	!		work properly.
	!
	!	02/11/93 - Dan Perkins
	!		Added new fields resulting in change in file layout.
	!
	!	02/28/93 - Kevin Handy
	!		Clean up (Check)
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
	!		Lose several commented out lines.
	!		Reformat source code.
	!		Changed references from MO_REGHEADER
	!		to OE_REGHEADER.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
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
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[MO.OPEN]MO_REGLINEOPT.HB"
	MAP (MO_REGLINEOPT)	MO_REGLINEOPT_CDD	MO_REGLINEOPT
	MAP (MO_REGLINEOPT_OLD)	MO_REGLINEOPT_CDD	MO_REGLINEOPT_OLD, &
							MO_REGLINEOPT_DEF

	%INCLUDE "SOURCE:[MO.OPEN]MO_REGLINE.HB"
	MAP (MO_REGLINE)	MO_REGLINE_CDD		MO_REGLINE

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.HB"
	MAP (OE_REGHEADER)	OE_REGHEADER_CDD	OE_REGHEADER

	%INCLUDE "SOURCE:[MO.OPEN]MO_OPTION.HB"
	MAP (MO_OPTION)		MO_OPTION_CDD		MO_OPTION

	%INCLUDE "SOURCE:[MO.OPEN]MO_OPTGROUP.HB"
	MAP (MO_OPTGROUP)	MO_OPTGROUP_CDD		MO_OPTGROUP

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[MO.OPEN]MO_MODELLINE.HB"
	MAP (MO_MODELLINE)	MO_MODELLINE_CDD	MO_MODELLINE

	%INCLUDE "SOURCE:[MO.OPEN]MO_MAKE.HB"
	DECLARE			MO_MAKE_CDD		MO_MAKE_READ

	!
	! Common Statements
	!
	COM (CH_MO_REGLINEOPT) &
		MO_REGLINEOPT.CH%, &
		MO_REGLINEOPT.READONLY%

	COM (TT_MO_MAIN_REGLINEOPT) &
		TRANTITLE$ = 20%, &
		TRANTYPE$(7%) = 40%

	COM (CH_MO_CLASS) &
		MAKE_CLAS$ = 4%, &
		COUNTSTUFF%

	COM (CH_MO_ARRAY) &
		INV_ARRAY$(100%), &
		INV_COUNT%

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION MAIN_WINDOW
	EXTERNAL LONG    FUNCTION FUNC_TESTENTRY
	EXTERNAL LONG    FUNCTION IC_WRIT_35BALANCE
	EXTERNAL LONG    FUNCTION MO_READ_MAKE

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Model Order Options"
		SMG_WINDOW::CURREC = -2%
		SMG_WINDOW::NHELP = "MO_MAIN_REGLINEOPT"
		SMG_WINDOW::HSIZE = 74%
		SMG_WINDOW::VSIZE = 15%
		SMG_WINDOW::HPOS  = 4%
		SMG_WINDOW::VPOS  = 4%
		SMG_WINDOW::NITEMS= 15%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::HVIEW = 74%
		SMG_WINDOW::VVIEW = 15%
		SMG_WINDOW::VHPOS = 4%
		SMG_WINDOW::VVPOS = 4%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Line"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		!
		! Transaction type
		!
		TRANTITLE$ = "Type    Description"
		TRANTYPE$(0%) = "3"
		TRANTYPE$(1%) = "01    On Order"
		TRANTYPE$(2%) = "02    Shipping"
		TRANTYPE$(3%) = "03    Cancel"

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

		!
		! Load in defaults for chart
		!
		CALL READ_DEFAULTS(SMG_WINDOW)

		V% = MO_READ_MAKE(MO_REGLINE::MAKE, &
			MO_REGLINE::YEAR, MO_REGLINE::MTYPE, &
			MO_REGLINE::MSIZE, MO_MAKE_READ)

		MAKE_CLAS$ = MO_MAKE_READ::CLASS

		COUNTSTUFF% = 0%

		!
		! Declare channels
		!
700		IF MO_REGLINEOPT.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF MO_REGLINEOPT.READONLY%

			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[MO.OPEN]MO_REGLINEOPT.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			MO_MAIN_REGLINEOPT = ERR
			CONTINUE 770
		END WHEN

		MO_REGLINEOPT.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[MO.OPEN]MO_REGLINEOPT.OPN"
		USE
			MO_MAIN_REGLINEOPT = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		MO_REGLINEOPT.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(MO_REGLINEOPT.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = MO_REGLINEOPT.CH%
		WHEN ERROR IN
			RESET #MO_REGLINEOPT.CH%
			GET #MO_REGLINEOPT.CH%, REGARDLESS
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

		DATA	01,02, "(01) Opt Line No", &
			02,02, "(02) Trans Type", &
			03,02, "(03) Trans Date", &
			04,02, "(04) Opt Group", &
			05,02, "(05) Option", &
			06,02, "(06) Description", &
			07,02, "(07) Quantity", &
			08,02, "(08) Unit Price", &
			09,02, "(09) Unit Cost", &
			10,02, "(10) Post Date", &
			11,02, "(11) Post Time", &
			12,02, "(12) Batch", &
			13,02, "(13) Period", &
			14,02, "(14) Ship No", &
			15,02, "(15) Product", &
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
	!	^*(01) Option Line Number\*
	!	.p
	!	The ^*Option Line Number\* field enters
	!	the Line Number associated with this option.
	!
	! Index:
	!	.x Option Line Number
	!
	!--
			MO_REGLINEOPT::OPTLIN = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"1;20", TEMP$, MO_REGLINEOPT::OPTLIN, &
				MFLAG, "~L0'E", MVALUE)

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Transaction Type\*
	!	.p
	!	The ^*Transacation Type\* field indicates the type of transaction
	!	contained in the record.
	!	.p
	!	The only valid trans types are:
	!	.p
	!	.list 0,"*"
	!	.le
	!	01 = Ordered
	!	.le
	!	02 = Shipping
	!	.le
	!	03 = Cancel
	!	.els
	!
	! Index:
	!
	!--
			MO_REGLINEOPT::TRANTYPE = &
				EDIT$(ENTR_3STRINGLIST(SCOPE, SMG_WINDOW::WNUMBER, &
				"2;20", TEMP$, MO_REGLINEOPT::TRANTYPE, MFLAG, &
				"'E", MVALUE, &
				TRANTYPE$(), TRANTITLE$, "008"), -1%)

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Transaction Date\*
	!	.p
	!	The ^*Transaction Date\* field enters the date of the
	!	transaction.
	!
	! Index:
	!	.x Transaction Date
	!
	!--
			MO_REGLINEOPT::TDATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"3;20", TEMP$, MO_REGLINEOPT::TDATE, &
				MFLAG, "'E", MVALUE)

		CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Option Group\*
	!	.p
	!	The ^*Option Group\* field enters the group to which
	!	the line number is assoiciated.  The ^*<List Choices>\* key displays a listing
	!	of the valid groups.
	!
	! Index:
	!
	!--
			MO_REGLINEOPT::OPTGROUP = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"4;20", TEMP$, MO_REGLINEOPT::OPTGROUP, &
				MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF COUNTSTUFF% > INV_COUNT%
				THEN
					IF (MAIN_WINDOW(MO_MAIN_MODEL_LINE.ID, "V0" + &
						MO_REGLINE::MODELCODE + MO_REGLINE::MSIZE + &
						MAKE_CLAS$) = 1%)
					THEN
						MO_REGLINEOPT::OPTGROUP = &
							MO_MODELLINE::OPTGROUP

						MO_REGLINEOPT::OPTN = &
							MO_MODELLINE::OPTN
					END IF
					GOTO Reentry
				ELSE
					IF (MAIN_WINDOW(MO_MAIN_MODEL_LINE.ID, "V0" + &
						MO_REGLINE::MODELCODE + MO_REGLINE::MSIZE + &
						MAKE_CLAS$ + MO_REGLINEOPT::OPTGROUP) = 1%)
					THEN
						MO_REGLINEOPT::OPTGROUP = &
							MO_MODELLINE::OPTGROUP

						MO_REGLINEOPT::OPTN = &
							MO_MODELLINE::OPTN
					END IF
					GOTO Reentry

				END IF
			END IF

		CASE 5%
	!++
	! Abstract:FLD005
	!	^*(05) Option\*
	!	.p
	!	The ^*Option\* field is the option associated with the group
	!	above that is required to complete the model. The Option
	!	group master file contains all options associated with the
	!	option group and this must be one of the associated options.
	!
	! Index:
	!
	!--
			MO_REGLINEOPT::OPTN = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"5;20",TEMP$, MO_REGLINEOPT::OPTN, &
				MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(MO_MAIN_MODEL_LINE.ID, "V0" + &
					MO_REGLINE::MODELCODE + MO_REGLINE::MSIZE + &
					MAKE_CLAS$ + MO_REGLINEOPT::OPTGROUP) = 1%)
				THEN
					MO_REGLINEOPT::OPTN = &
						MO_MODELLINE::OPTN
				END IF
				GOTO Reentry
			END IF

		CASE 6%
	!++
	! Abstract:FLD006
	!	^*(06) Description\*
	!	.p
	!	The ^*Description\* field is the description of the group
	!	above in field (03).
	!
	! Index:
	!
	!--
			MO_REGLINEOPT::OPTDESCR = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"6;20",TEMP$, MO_REGLINEOPT::OPTDESCR, &
				MFLAG, "'E", MVALUE)

		CASE 7%
	!++
	! Abstract:FLD007
	!	^*(07) Quantity\*
	!	.p
	!	The ^*Quantity Ordered\* field enters the number of
	!	units which have been ordered on the particular date for this
	!	particular product.
	!	.p
	!	The field may contain a figure as large as 9,999,999.99.
	!
	! Index:
	!	.x Quantity
	!
	!--
			MO_REGLINEOPT::QTY = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"7;20", TEMP$, MO_REGLINEOPT::QTY, &
				MFLAG, "#,###,###.##", MVALUE)

		CASE 8%
	!++
	! Abstract:FLD008
	!
	!	^*(08) Unit Price\*
	!	.p
	!	If a unique product is available in a particular unit of
	!	measure, i.e. "gallon", "piece", "yard", etc., that price is
	!	entered in this field.
	!
	! Index:
	!	.x Unit Price
	!--
			MO_REGLINEOPT::PRICE = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"8;20", TEMP$, MO_REGLINEOPT::PRICE, &
				MFLAG, "#,###,###.##", MVALUE)

		CASE 9%
	!++
	! Abstract:FLD009
	!	^*(09) Unit Cost\*
	!	.p
	!	The ^*Unit Cost\* field enters the cost of
	!	a particular unit.
	!	.p
	!	If a unique product is available in a particular unit
	!	of measure, i.e. "gallon", "piece", "yard", that cost
	!	is entered in this field.
	!
	! Index:
	!	.x Cost/Unit
	!
	!--
			MO_REGLINEOPT::COST = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"9;20",TEMP$, MO_REGLINEOPT::COST, &
				MFLAG, "#,###,###.##", MVALUE)

		CASE 10%
	!++
	! Abstract:FLD010
	!	^*(10) Post Date\*
	!	.p
	!	The ^*Post Date\* field enters the date
	!	the line transaction was posted to this file.
	!
	! Index:
	!	.x Post Date
	!
	!--
			MO_REGLINEOPT::POSTDATE = ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"10;20", TEMP$, MO_REGLINEOPT::POSTDATE, &
				MFLAG, "'E", MVALUE)

		CASE 11%
	!++
	! Abstract:FLD011
	!	^*(11) Post Time\*
	!	.p
	!	The ^*Post Time\* field enters the time
	!	the line transaction was posted to this file.
	!
	! Index:
	!	.x Post Time
	!
	!--
			MO_REGLINEOPT::POSTTIME = ENTR_3TIME(SCOPE, SMG_WINDOW::WNUMBER, &
				"11;20", TEMP$, MO_REGLINEOPT::POSTTIME, &
				MFLAG, "'E", MVALUE)

		CASE 12%
	!++
	! Abstract:FLD012
	!	^*(12) Batch Number\*
	!	.p
	!	The ^*Batch Number\* field enters the
	!	Batch Number of the line transaction which was posted to this file.
	!
	! Index:
	!	.x Batch No
	!
	!--
			MO_REGLINEOPT::BATCH = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"12;20", TEMP$, MO_REGLINEOPT::BATCH, &
				MFLAG, "'E", MVALUE)

		CASE 13%
	!++
	! Abstract:FLD013
	!	^*(13) Period\*
	!	.p
	!	The ^*Period\* field provides reference to the fiscal
	!	year and cycle into which this transaction was posted.  The
	!	format of this field is YYYYPP.
	!
	! Index:
	!	.x Period
	!
	!--
			MO_REGLINEOPT::PERIOD = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"13;20", TEMP$, MO_REGLINEOPT::PERIOD, &
				MFLAG, "'E", MVALUE)

		CASE 14%
	!++
	! Abstract:FLD014
	!	^*(14) Ship Number\*
	!	.p
	!	The ^*Ship Number\* field enters the
	!	Shipment Number of the line transaction which was posted to this file.
	!
	! Index:
	!	.x Ship Number
	!
	!--
			MO_REGLINEOPT::SHIPNO = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"14;20", TEMP$, MO_REGLINEOPT::SHIPNO, &
				MFLAG, "'E", MVALUE)

		CASE 15%
	!++
	! Abstract:FLD015
	!	^*(15) Product\*
	!	.p
	!	The ^*Product\* field enters the product number for the
	!	record.
	!	.p
	!	The field will accommodate two (2) alphanumeric characters. Pressing the
	!	^*<List Choices>\* key displays a list of the valid make types.
	!
	! Index:
	!
	!--
			MO_REGLINEOPT::PRODUCT = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"15;20", TEMP$, MO_REGLINEOPT::PRODUCT, &
				MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF (MAIN_WINDOW(PD_MAIN_PRODUCT.ID, "VX") = 1%)
				THEN
					MO_REGLINEOPT::PRODUCT = &
						PD_PRODUCT::PRODUCT_NUM
				END IF
				GOTO Reentry
			END IF

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY

		MO_MAIN_REGLINEOPT = 0%

		SELECT MLOOP

		CASE 1%
			MO_MAIN_REGLINEOPT = 1% IF MO_REGLINEOPT::OPTLIN = ""

		CASE 4%
			!
			! Test OPTGROUP
			!
			MO_MAIN_REGLINEOPT = FUNC_TESTENTRY(SMG_WINDOW, &
				MO_REGLINEOPT::OPTGROUP, &
				MO_OPTGROUP::DESCR, &
				"MO", MLOOP, "PROG", &
				"Option group", MO_MAIN_OPTGROUP.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				MO_OPTGROUP::DESCR, &
				4%, 30%, , SMG$M_BOLD)

		CASE 5%
			IF MAIN_WINDOW(MO_MAIN_MODEL_LINE.ID, "Q0" + &
				MO_REGLINE::MODELCODE + MO_REGLINE::MSIZE + &
				MAKE_CLAS$ + MO_REGLINEOPT::OPTGROUP + &
				MO_REGLINEOPT::OPTN) <> 1%
			THEN
				CALL ENTR_3MESSAGE(SCOPE, &
					"Invalid Option Group/option for this Model!", 0%)

				MO_MAIN_REGLINEOPT = 1%
				GOTO 28000
			END IF

			MO_REGLINEOPT::PRODUCT = MO_OPTION::PRODUCT

		CASE 15%
			!
			! Test product
			!
			IF MO_REGLINEOPT::PRODUCT <> ""
			THEN
				MO_MAIN_REGLINEOPT = FUNC_TESTENTRY(SMG_WINDOW, &
					MO_REGLINEOPT::PRODUCT, &
					PD_PRODUCT::DESCRIPTION, &
					"MO", MLOOP, "PROG", &
					"Product", PD_MAIN_PRODUCT.ID)

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					PD_PRODUCT::DESCRIPTION, &
					15%, 36%, , SMG$M_BOLD)
			END IF

		END SELECT

	CASE OPT_DISPLAY

		!
		! Display the descriptions
		!
		IF (SMG_WINDOW::HFLAG(4%) AND 2%) = 0%
		THEN
			MO_OPTGROUP::DESCR = &
				STRING$(LEN(MO_OPTGROUP::DESCR), A"?"B) &
				IF MAIN_WINDOW(MO_MAIN_OPTGROUP.ID, "Q0" + &
				MO_REGLINEOPT::OPTGROUP) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				MO_OPTGROUP::DESCR, 4%, 30%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(15%) AND 2%) = 0%
		THEN
			PD_PRODUCT::DESCRIPTION = ""

			IF MO_REGLINEOPT::PRODUCT <> ""
			THEN
				IF MAIN_WINDOW (PD_MAIN_PRODUCT.ID, "Q0" + &
					MO_REGLINEOPT::PRODUCT) <> 1%
				THEN
					PD_PRODUCT::DESCRIPTION = &
						STRING$(LEN(PD_PRODUCT::DESCRIPTION), A"?"B)
				END IF
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_PRODUCT::DESCRIPTION, 15%, 36%,, SMG$M_BOLD)

		END IF

	!
	! Set MO_REGLINEOPT_OLD value
	!
20500	CASE OPT_SETOLD
		MO_REGLINEOPT_OLD = MO_REGLINEOPT

	!
	! Restore MO_REGLINEOPT_OLD value
	!
	CASE OPT_RESETOLD
		MO_REGLINEOPT = MO_REGLINEOPT_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		MO_REGLINEOPT_DEF = MO_REGLINEOPT

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		MO_REGLINEOPT = MO_REGLINEOPT_DEF

		!
		! Setup and check array of options from line
		!
		COUNTSTUFF% = COUNTSTUFF% + 1%

		IF COUNTSTUFF% <= INV_COUNT%
		THEN
			MO_REGLINEOPT::OPTGROUP = MID(INV_ARRAY$(COUNTSTUFF%), 5%, 2%)
		END IF

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!
		MO_REGLINEOPT::ORDNUM = &
			LEFT(MVALUE, LEN(MO_REGLINEOPT::ORDNUM))

		MO_REGLINEOPT::LIN = &
			RIGHT(MVALUE, LEN(MO_REGLINEOPT::ORDNUM) + 1%)

		V% = MO_READ_MAKE(MO_REGLINE::MAKE, &
			MO_REGLINE::YEAR, MO_REGLINE::MTYPE, &
			MO_REGLINE::MSIZE, MO_MAKE_READ)

		MAKE_CLAS$ = MO_MAKE_READ::CLASS

	!
	! Find the next record
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE MO_REGLINEOPT::ORDNUM + &
				MO_REGLINEOPT::LIN + &
				MO_REGLINEOPT::OPTLIN, REGARDLESS

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
			MVALUE = "  Line TType OptGroup Option Description               TransQty     ExtPrice"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "007,013,022,029,051,064"

		!
		! Convert current record into text
		!
		CASE 3%
			MO_OPTION::DESCR = &
				STRING$(LEN(MO_OPTION::DESCR), A"?"B) &
				IF MAIN_WINDOW(MO_MAIN_OPTION.ID, "Q0" + &
				MO_REGLINEOPT::OPTGROUP+MO_REGLINEOPT::OPTN) <> 1%

			EXTENT = FUNC_ROUND(MO_REGLINEOPT::QTY * &
				MO_REGLINEOPT::PRICE, 2%)

			MVALUE = MO_REGLINEOPT::OPTLIN + " " + &
				MO_REGLINEOPT::TRANTYPE + "    " + &
				MO_REGLINEOPT::OPTGROUP + "       " + &
				MO_REGLINEOPT::OPTN + "   "     + &
				LEFT$(MO_OPTION::DESCR, 20%) + "  "      + &
				FORMAT$(MO_REGLINEOPT::QTY, "#,###,###.##") + " " + &
				FORMAT$(EXTENT, "#,###,###.##")

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
				IF MO_REGLINEOPT::PRODUCT <> ""
				THEN
					V% = IC_WRIT_35BALANCE (MO_REGLINEOPT::PRODUCT, &
						OE_REGHEADER::LOCATION, "MO", &
						MO_REGLINEOPT::QTY)
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
						MO_REGLINEOPT::OPTLIN, REGARDLESS
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

			IF MO_REGLINEOPT::ORDNUM + MO_REGLINEOPT::LIN = MVALUE
			THEN
				SMG_WINDOW::CURREC = 0%
			END IF

		!
		! Change key
		!
		CASE 6%
			MO_REGLINEOPT::ORDNUM = &
				LEFT(MVALUE, LEN(MO_REGLINEOPT::ORDNUM))

			MO_REGLINEOPT::LIN = &
				RIGHT(MVALUE, LEN(MO_REGLINEOPT::ORDNUM) + 1%)

		END SELECT

	CASE OPT_AFTEROPT

		SELECT MVALUE

		CASE "Add"
			IF MO_REGLINEOPT::PRODUCT <> ""
			THEN
				V% = IC_WRIT_35BALANCE(MO_REGLINEOPT::PRODUCT, &
					OE_REGHEADER::LOCATION, "MO", &
					-MO_REGLINEOPT::QTY)
			END IF

		CASE "Change", "Blank", "Initialize"
			IF MO_REGLINEOPT_OLD::PRODUCT <> ""
			THEN
				V% = IC_WRIT_35BALANCE(MO_REGLINEOPT_OLD::PRODUCT, &
					OE_REGHEADER::LOCATION, "MO", &
					MO_REGLINEOPT_OLD::QTY)
			END IF

			IF MO_REGLINEOPT::PRODUCT <> ""
			THEN
				V% = IC_WRIT_35BALANCE(MO_REGLINEOPT::PRODUCT, &
					OE_REGHEADER::LOCATION, "MO", &
					-MO_REGLINEOPT::QTY)
			END IF

		CASE "Erase"
			IF MLOOP <> 1%
			THEN
				IF MO_REGLINEOPT::PRODUCT <> ""
				THEN
					V% = IC_WRIT_35BALANCE(MO_REGLINEOPT::PRODUCT, &
						OE_REGHEADER::LOCATION, "MO", &
						MO_REGLINEOPT::QTY)
				END IF
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

