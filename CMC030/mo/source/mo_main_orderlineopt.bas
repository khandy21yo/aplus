1	%TITLE "Order Line Option Journal"
	%SBTTL "MO_MAIN_ORDERLINEOPT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG MO_MAIN_ORDERLINEOPT(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	Order Line Option Journal is used to enter options for
	!	a model.
	!
	! Index:
	!	.x Help>Order Line Option Journal
	! Option:
	!
	! Compile:
	!
	!	$ BAS MO_SOURCE:MO_MAIN_ORDERLINEOPT/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN MO_MAIN_ORDERLINEOPT
	!	$ DELETE MO_MAIN_ORDERLINEOPT.OBJ;*
	!
	! Author:
	!
	!	03/05/91 - Frank F. Starman
	!
	! Modification history:
	!	04/29/91 - J. Shad Rydalch
	!		Added description feild.
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
	!	01/08/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	02/26/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/02/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/14/93 - Dan Perkins
	!		Call SUBR_TRANTYPE in OPT_TESTENTRY so the order
	!		status changes immediately.
	!
	!	06/24/93 - Dan Perkins
	!		Fixed find and view options so they would work.
	!
	!	04/14/94 - Kevin Handy
	!		Modified closer to 80 columns.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	06/26/96 - Kevin Handy
	!		Reformat source code.
	!		Lose commented out code.
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

	%INCLUDE "SOURCE:[MO.OPEN]MO_ORDERLINEOPT.HB"
	MAP (MO_ORDERLINEOPT)	MO_ORDERLINEOPT_CDD	MO_ORDERLINEOPT
	MAP (MO_ORDERLINEOPT_OLD) MO_ORDERLINEOPT_CDD	MO_ORDERLINEOPT_OLD, &
							MO_ORDERLINEOPT_DEF

	%INCLUDE "SOURCE:[MO.OPEN]MO_ORDERLINE.HB"
	MAP (MO_ORDERLINE)	MO_ORDERLINE_CDD	MO_ORDERLINE

	%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERJOUR.HB"
	MAP (OE_ORDERJOUR)	OE_ORDERJOUR_CDD	OE_ORDERJOUR

	%INCLUDE "SOURCE:[MO.OPEN]MO_OPTION.HB"
	MAP (MO_OPTION)		MO_OPTION_CDD		MO_OPTION

	%INCLUDE "SOURCE:[MO.OPEN]MO_OPTGROUP.HB"
	MAP (MO_OPTGROUP)	MO_OPTGROUP_CDD		MO_OPTGROUP

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

	%INCLUDE "SOURCE:[MO.OPEN]MO_MODELLINE.HB"
	MAP (MO_MODELLINE)	MO_MODELLINE_CDD	MO_MODELLINE

	%INCLUDE "SOURCE:[MO.OPEN]MO_MAKE.HB"
	COM (MO_MAKE_READ)	MO_MAKE_CDD		MO_MAKE_READ

	%INCLUDE "SOURCE:[MO.OPEN]MO_REGLINEOPT.HB"
	MAP (MO_REGLINEOPT)	MO_REGLINEOPT_CDD	MO_REGLINEOPT
	DECLARE			MO_REGLINEOPT_CDD	MO_REGLINEOPT_READ

	!
	! Common Statements
	!
	COM (BATCH_NO) &
		BATCH_NO$ = 7%

	COM (CH_MO_ORDERLINEOPT) &
		MO_ORDERLINEOPT.CH%, &
		MO_ORDERLINEOPT.READONLY%

	COM (CH_MO_ARRAY) &
		INV_ARRAY$(100%), &
		INV_COUNT%, &
		COUNTSTUFF%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION MAIN_WINDOW
	EXTERNAL LONG	FUNCTION IC_WRIT_35BALANCE
	EXTERNAL LONG	FUNCTION MO_READ_MAKE
	EXTERNAL LONG	FUNCTION MO_READ_REGLINEOPT
	EXTERNAL LONG	FUNCTION FUNC_TESTENTRY
	EXTERNAL REAL	FUNCTION PC_READ_COST
	EXTERNAL REAL	FUNCTION PC_READ_PRICE

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR  = "Manufacturing Line Options"
		SMG_WINDOW::CURREC = -2%
		SMG_WINDOW::NHELP  = "MO_MAIN_ORDERLINEOPT"
		SMG_WINDOW::HSIZE  = 76%
		SMG_WINDOW::VSIZE  =  6%
		SMG_WINDOW::HPOS   =  4%
		SMG_WINDOW::VPOS   = 14%
		SMG_WINDOW::NITEMS =  9%
		SMG_WINDOW::FLAGS  =  0%
		SMG_WINDOW::HVIEW  = 76%
		SMG_WINDOW::VVIEW  =  6%
		SMG_WINDOW::VHPOS  =  4%
		SMG_WINDOW::VVPOS  = 14%

		SMG_WINDOW::NKEYS  =  1%
		SMG_WINDOW::KNAME(0%) = "LINE"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		COM (MO_MAIN_ORDERLINEOPT_FRM) FRM$(9%)

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

		!
		! Load in defaults for chart
		!
		CALL READ_DEFAULTS(SMG_WINDOW)

		V% = MO_READ_MAKE(MO_ORDERLINE::MAKE, &
			MO_ORDERLINE::YEAR, &
			MO_ORDERLINE::MTYPE, &
			MO_ORDERLINE::MSIZE, &
			MO_MAKE_READ)

		COUNTSTUFF% = 0%

		!
		! Declare channels
		!
700		IF MO_ORDERLINEOPT.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF MO_ORDERLINEOPT.READONLY%

			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[MO.OPEN]MO_ORDERLINEOPT.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			MO_MAIN_ORDERLINEOPT = ERR
			CONTINUE 770
		END WHEN

		MO_ORDERLINEOPT.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[MO.OPEN]MO_ORDERLINEOPT.OPN"
		USE
			MO_MAIN_ORDERLINEOPT = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		MO_ORDERLINEOPT.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(MO_ORDERLINEOPT.CH%)

		EXIT FUNCTION

790		IF MVALUE = "A"
		THEN
			GOSUB 28100
		END IF

		SMG_WINDOW::CHAN  = MO_ORDERLINEOPT.CH%
		WHEN ERROR IN
			RESET #MO_ORDERLINEOPT.CH%
			GET #MO_ORDERLINEOPT.CH%, REGARDLESS
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

		DATA	01,48, "(01) Line", &
			01,02, "(02) Opt Group", &
			02,02, "(03) Option", &
			03,02, "(04) Description", &
			04,02, "(05) Qty Requested", &
			05,02, "(06) Qty Sold", &
			06,02, "(07) Qty Back", &
			04,48, "(08) Unit Price", &
			05,48, "(09) Unit Cost", &
			06,48, "Ext. Price", &
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

 Reentry:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%
	!++
	! Abstract:FLD001
	!	.x Line
	!	^*(01) Line\*
	!	.b
	!	.lm +5
	!	The ^*Line\* field enters the line number
	!	of the option that was ordered.
	!	.b
	!	Four spaces are available for the entry.
	!	.b
	!	Valid Line numbers may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!
	!--
			MFLAG = MFLAG OR 1% IF OE_ORDERJOUR::REG_FLAG <> "Y"

			MO_ORDERLINEOPT::LINOPT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"01;60", TEMP$, MO_ORDERLINEOPT::LINOPT, &
				MFLAG, &
				"~L0'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_F14
				IF MAIN_WINDOW(MO_MAIN_REGLINEOPT.ID, &
					"V0" + OE_ORDERJOUR::ORDNUM) = 1%
				THEN
					MO_ORDERLINEOPT::LINOPT = &
						MO_REGLINEOPT::OPTLIN
				END IF
				GOTO Reentry

			END SELECT

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Option Group\*
	!	.b
	!	.lm +5
	!	The ^*Option Group\* field enters a code which
	!	identifies a particular "Option Group".
	!	.b
	!	Pressing ^*List Choices\* while the cursor is located at this field will
	!	cause valid option groups to be displayed.
	!	.lm -5
	!
	! Index:
	!	.x Option Group>Maintenance
	!	.x Maintenance>Option Group
	!
	!--
			MFLAG = MFLAG OR 1% IF OE_ORDERJOUR::REG_FLAG = "Y" &
				AND MO_ORDERLINEOPT::LIN <> "NEWL"

			MO_ORDERLINEOPT::OPTGROUP = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"01;25",TEMP$, MO_ORDERLINEOPT::OPTGROUP, &
				MFLAG, &
				"'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF COUNTSTUFF% > INV_COUNT%
				THEN
					IF MAIN_WINDOW(MO_MAIN_MODEL_LINE.ID, &
						"V0" + &
						MO_ORDERLINE::MODELCODE + &
						MO_ORDERLINE::MSIZE + &
						MO_MAKE_READ::CLASS) = 1%
					THEN
						MO_ORDERLINEOPT::OPTGROUP = &
							MO_MODELLINE::OPTGROUP

						MO_ORDERLINEOPT::OPTN = &
							MO_MODELLINE::OPTN
					END IF

					GOTO Reentry
				ELSE
					IF MAIN_WINDOW(MO_MAIN_MODEL_LINE.ID, &
						"V0" + &
						MO_ORDERLINE::MODELCODE + &
						MO_ORDERLINE::MSIZE + &
						MO_MAKE_READ::CLASS + &
						MO_ORDERLINEOPT::OPTGROUP) = 1%
					THEN
						MO_ORDERLINEOPT::OPTGROUP = &
							MO_MODELLINE::OPTGROUP

						MO_ORDERLINEOPT::OPTN = &
							MO_MODELLINE::OPTN
					END IF

					GOTO Reentry

				END IF
			END IF

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Option\*
	!	.lm +5
	!	.b
	!	The ^*Option\* field enters an option associated with
	!	the option group.  If the option group in field (01) is selected from the
	!	<List Choices> display, the data in this field will be automatically
	!	inserted.
	!	.b
	!	Pressing ^*List Choices\* while the cursor is located at this field will
	!	cause valid option groups and the related options to be displayed.
	!	.lm -5
	!
	! Index:
	!	.x Option
	!
	!--
			MFLAG = MFLAG OR 1% IF OE_ORDERJOUR::REG_FLAG = "Y" &
				AND MO_ORDERLINEOPT::LIN <> "NEWL"

			MO_ORDERLINEOPT::OPTN = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"02;25",TEMP$, MO_ORDERLINEOPT::OPTN, MFLAG, &
				"'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(MO_MAIN_MODEL_LINE.ID, "V0" + &
					MO_ORDERLINE::MODELCODE + &
					MO_ORDERLINE::MSIZE + &
					MO_MAKE_READ::CLASS + &
					MO_ORDERLINEOPT::OPTGROUP) = 1%)
				THEN
					MO_ORDERLINEOPT::OPTN = &
						MO_MODELLINE::OPTN
				END IF

				GOTO Reentry
			END IF

		CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Description\*
	!	.b
	!	.lm +5
	!	The ^*Description\* field is automatically filled with the description of the
	!	option as contained in the option table.
	!	.lm -5
	!
	! Index:
	!	.x Description
	!
	!--
			MFLAG = MFLAG OR 1% IF OE_ORDERJOUR::REG_FLAG = "Y" &
				AND MO_ORDERLINEOPT::LIN <> "NEWL"

			MO_ORDERLINEOPT::OPTDESCR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"03;25",TEMP$, MO_ORDERLINEOPT::OPTDESCR, &
				MFLAG, &
				"'E", MVALUE)

		CASE 5%
	!++
	! Abstract:FLD005
	!	^*(05) Quantity Requested\*
	!	.b
	!	.lm +5
	!	The ^*Quantity Requested\* field enters the number of units
	!	ordered relative to a particular option group.
	!	.lm -5
	!
	! Index:
	!	.x Quantity
	!
	!--
			MFLAG = MFLAG OR 1% IF OE_ORDERJOUR::REG_FLAG = "Y" &
				AND MO_ORDERLINEOPT::LIN <> "NEWL"

			MO_ORDERLINEOPT::ORDQTY = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"04;22",TEMP$, MO_ORDERLINEOPT::ORDQTY, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)

		CASE 6%
	!++
	! Abstract:FLD006
	!	^*(06) Quantity Sold\*
	!	.b
	!	.lm +5
	!	The ^*Quantity Sold\* field enters the number of units
	!	shipped relative to a particular option group.
	!	.lm -5
	!
	! Index:
	!	.x Quantity
	!
	!--
			MO_ORDERLINEOPT::SHPQTY = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"05;22",TEMP$, MO_ORDERLINEOPT::SHPQTY, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)


		CASE 7%
	!++
	! Abstract:FLD007
	!	^*(07) Back Order Quantity\*
	!	.b
	!	.lm +5
	!	The ^*Back Order Quantity\* field
	!	enters the number of units ordered relative to a particular option group.
	!	.lm -5
	!
	! Index:
	!	.x Quantity
	!
	!--
			MO_ORDERLINEOPT::BCKQTY = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"06;22",TEMP$, MO_ORDERLINEOPT::BCKQTY, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)

		CASE 8%
	!++
	! Abstract:FLD008
	!	^*(08) Unit Price\*
	!	.b
	!	.lm +5
	!	The ^*Unit Price\* field enters the unit price of
	!	an option.
	!	.lm -5
	!
	! Index:
	!	.x Unit Price
	!
	!--
			MO_ORDERLINEOPT::PRICE = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"04;66",TEMP$, MO_ORDERLINEOPT::PRICE, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)

		CASE 9%
	!++
	! Abstract:FLD009
	!	^*(09) Unit Cost\*
	!	.b
	!	.lm +5
	!	The ^*Unit Cost\* field enters the cost of
	!	a particular option.
	!	.lm -5
	!
	! Index:
	!	.x Unit Cost
	!
	!--
			MO_ORDERLINEOPT::COST = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"05;66",TEMP$, MO_ORDERLINEOPT::COST, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY

		MO_MAIN_ORDERLINEOPT = 0%

		SELECT MLOOP

		CASE 1%
			!
			! See if the line is in the REGLINEOPT file
			!
			IF OE_ORDERJOUR::REG_FLAG = "Y" AND &
				MO_ORDERLINEOPT::LIN <> "NEWL"
			THEN
				IF MO_READ_REGLINEOPT(MO_ORDERLINE::ORDNUM, &
					MO_ORDERLINE::LIN, &
					MO_ORDERLINEOPT::LINOPT, "EQ", &
					MO_REGLINEOPT_READ, QTY()) <> &
					CMC$_NORMAL
				THEN
					MO_MAIN_ORDERLINEOPT = 1%
					GOTO 28000
				END IF

				REMAIN_QTY = QTY(1%) - (QTY(2%) + QTY(3))

				MO_ORDERLINEOPT::ORDNUM		= MO_REGLINEOPT_READ::ORDNUM
				MO_ORDERLINEOPT::LIN		= MO_REGLINEOPT_READ::LIN
				MO_ORDERLINEOPT::LINOPT		= MO_REGLINEOPT_READ::OPTLIN
				MO_ORDERLINEOPT::OPTGROUP	= MO_REGLINEOPT_READ::OPTGROUP
				MO_ORDERLINEOPT::OPTN		= MO_REGLINEOPT_READ::OPTN
				MO_ORDERLINEOPT::OPTDESCR	= MO_REGLINEOPT_READ::OPTDESCR
				MO_ORDERLINEOPT::ORDQTY		= QTY(1%)
				MO_ORDERLINEOPT::SHPQTY		= REMAIN_QTY
				MO_ORDERLINEOPT::BCKQTY		= 0.0
				MO_ORDERLINEOPT::PRICE		= MO_REGLINEOPT_READ::PRICE
				MO_ORDERLINEOPT::COST		= MO_REGLINEOPT_READ::COST
				MO_ORDERLINEOPT::PRODUCT	= MO_REGLINEOPT_READ::PRODUCT
			END IF

		CASE 2%
			!
			! Test MO_ORDERLINEOPT product
			!
			MO_MAIN_ORDERLINEOPT = FUNC_TESTENTRY(SMG_WINDOW, &
				MO_ORDERLINEOPT::OPTGROUP, &
				MO_OPTGROUP::DESCR, &
				"MO", MLOOP, "PROG", &
				"Option group", MO_MAIN_OPTGROUP.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT$(MO_OPTGROUP::DESCR, 17%), &
				1%, 30%, , SMG$M_BOLD)

		CASE 3%
			MO_MAIN_ORDERLINEOPT = FUNC_TESTENTRY(SMG_WINDOW, &
				MO_ORDERLINEOPT::OPTGROUP + &
				MO_ORDERLINEOPT::OPTN, &
				MO_OPTION::DESCR, &
				"MO", MLOOP, "PROG", &
				"Option", MO_MAIN_OPTION.ID)

			IF MAIN_WINDOW(MO_MAIN_MODEL_LINE.ID, "Q0" + &
				MO_ORDERLINE::MODELCODE + &
				MO_ORDERLINE::MSIZE + &
				MO_MAKE_READ::CLASS + &
				MO_ORDERLINEOPT::OPTGROUP + &
				MO_ORDERLINEOPT::OPTN) <> 1%
			THEN
				CALL HELP_34MESSAGE(SCOPE, &
					"Invalid Option Group or Opton for this Model", &
					"W", SCOPE::PRG_PROGRAM, "", &
					"INVOPTION")

				MO_MAIN_ORDERLINEOPT = 1%
				GOTO 28000
			END IF

			MO_ORDERLINEOPT::PRODUCT = MO_OPTION::PRODUCT

			MO_ORDERLINEOPT::OPTDESCR = MO_OPTION::DESCR

			MO_ORDERLINEOPT::PRICE = PC_READ_PRICE( &
				MO_ORDERLINEOPT::PRODUCT, &
				OE_ORDERJOUR::LOCATION, &
				AR_35CUSTOM::TTYPE, &
				OE_ORDERJOUR::ORDDATE, "", "", "")

			MO_ORDERLINEOPT::COST = PC_READ_COST( &
				MO_ORDERLINEOPT::PRODUCT, &
				OE_ORDERJOUR::LOCATION, &
				OE_ORDERJOUR::ORDDATE, "")

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				MO_ORDERLINEOPT::OPTDESCR, &
				3%, 25%, , SMG$M_BOLD)

		CASE 6%
			EXTENT = FUNC_ROUND(MO_ORDERLINEOPT::ORDQTY * &
				MO_ORDERLINEOPT::PRICE, 2%)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				FORMAT$(EXTENT, "#,###,###.##"), &
				6%, 64%, , SMG$M_BOLD)

		CASE 7%
			IF MVALUE = "ADD"
			THEN
				CALL SUBR_TRANTYPE(OE_ORDERJOUR::CUSNUM, &
					MO_ORDERLINEOPT::LINOPT, &
					MO_ORDERLINEOPT::ORDQTY, &
					MO_ORDERLINEOPT::SHPQTY, &
					MO_ORDERLINEOPT::BCKQTY, &
					TRANTYPE$(), TRANQTY())

				FOR I% = 1% TO 3%

					SMG_STATUS% = &
						SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
						RIGHT(TRANTYPE$(I%), 3%), &
						I% + 3%, 32%, , SMG$M_BOLD)
				NEXT I%
			END IF

		END SELECT

	CASE OPT_DISPLAY

		!
		! Display the descriptions
		!
		IF (SMG_WINDOW::HFLAG(2%) AND 2%) = 0%
		THEN
			MO_OPTGROUP::DESCR = &
				STRING$(LEN(MO_OPTGROUP::DESCR), A"?"B) &
				IF MAIN_WINDOW(MO_MAIN_OPTGROUP.ID, &
				"Q0" + MO_ORDERLINEOPT::OPTGROUP) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT$(MO_OPTGROUP::DESCR, 17%), &
				1%, 30%, , SMG$M_BOLD)
		END IF

		CALL SUBR_TRANTYPE(OE_ORDERJOUR::CUSNUM, &
			MO_ORDERLINEOPT::LINOPT, &
			MO_ORDERLINEOPT::ORDQTY, &
			MO_ORDERLINEOPT::SHPQTY, MO_ORDERLINEOPT::BCKQTY, &
			TRANTYPE$(), TRANQTY())

		FOR I% = 1% TO 3%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				RIGHT(TRANTYPE$(I%), 3%), &
				I% + 3%, 32%, , SMG$M_BOLD)
		NEXT I%

		IF (SMG_WINDOW::HFLAG(8%) AND 2%) = 0%
		THEN
			EXTENT = FUNC_ROUND(MO_ORDERLINEOPT::ORDQTY * &
				MO_ORDERLINEOPT::PRICE, 2%)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				FORMAT$(EXTENT, "#,###,###.##"), &
				6%, 64%, , SMG$M_BOLD)
		END IF

	!
	! Set MO_ORDERLINEOPT_OLD value
	!
20500	CASE OPT_SETOLD
		MO_ORDERLINEOPT_OLD = MO_ORDERLINEOPT

	!
	! Restore MO_ORDERLINEOPT_OLD value
	!
	CASE OPT_RESETOLD
		MO_ORDERLINEOPT = MO_ORDERLINEOPT_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		MO_ORDERLINEOPT_DEF = MO_ORDERLINEOPT

		IF MFLAG = 1%
		THEN
			SELECT MLOOP
			CASE 0%
				FRM$(5%) = "###,###"
				FRM$(6%) = "###,###"
				FRM$(7%) = "###,###"
				FRM$(8%) = "###,###.##"
				FRM$(9%) = "###,###.##"

			CASE ELSE
				FRM$(MLOOP) = MVALUE
			END SELECT
		END IF

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		MO_ORDERLINEOPT = MO_ORDERLINEOPT_DEF

		MO_ORDERLINEOPT::LINOPT = "NEWL" IF MFLAG = 1%

		!
		! Setup and check array of options from line
		!
		COUNTSTUFF% = COUNTSTUFF% + 1%

		IF COUNTSTUFF% <= INV_COUNT%
		THEN
			MO_ORDERLINEOPT::OPTGROUP = &
				MID(INV_ARRAY$(COUNTSTUFF%), 5%, 2%)
		END IF

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!
		MO_ORDERLINEOPT::ORDNUM = &
			LEFT(MVALUE, LEN(MO_ORDERLINEOPT::ORDNUM))

		MO_ORDERLINEOPT::LIN = &
			MID(MVALUE, 11%,LEN(MO_ORDERLINEOPT::LIN))

		MO_ORDERLINEOPT::MAKE = &
			MID(MVALUE, 15%,LEN(MO_ORDERLINEOPT::MAKE))

		MO_ORDERLINEOPT::MODELCODE = &
			RIGHT(MVALUE, 25%)

	!
	! Find the next record
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			IF OE_ORDERJOUR::REG_FLAG = "Y"
			THEN
				FIND #SMG_WINDOW::CHAN, &
					KEY #0% GE MO_ORDERLINEOPT::ORDNUM + &
					MO_ORDERLINEOPT::LIN, REGARDLESS
			ELSE
				FIND #SMG_WINDOW::CHAN, &
					KEY #0% GE MO_ORDERLINEOPT::ORDNUM + "", &
					REGARDLESS
			END IF

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
			MVALUE = "  OptGroup Option Description" + &
				"               OrderQty     ExtPrice"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "011,018,040,053"

		!
		! Convert current record into text
		!
		CASE 3%

			EXTENT = FUNC_ROUND(MO_ORDERLINEOPT::ORDQTY * &
				MO_ORDERLINEOPT::PRICE, 2%)

			MVALUE = MO_ORDERLINEOPT::OPTGROUP + "       " + &
				MO_ORDERLINEOPT::OPTN + "   "     + &
				LEFT$(MO_ORDERLINEOPT::OPTDESCR, 20%) + "  " + &
				FORMAT$(MO_ORDERLINEOPT::ORDQTY, &
					"#,###,###.##") + " " + &
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
				IF MO_ORDERLINEOPT::PRODUCT <> ""
				THEN
					CALL SUBR_TRANTYPE(OE_ORDERJOUR::CUSNUM,MO_ORDERLINEOPT::LINOPT, &
						MO_ORDERLINEOPT::ORDQTY, &
						MO_ORDERLINEOPT::SHPQTY, &
						MO_ORDERLINEOPT::BCKQTY, &
						TRANTYPE$(), TRANQTY())

					FOR I% = 1% TO VAL%(TRANTYPE$(0%))

						V% = IC_WRIT_35BALANCE(MO_ORDERLINEOPT::PRODUCT, &
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
				FIND #SMG_WINDOW::CHAN, &
					KEY #0% EQ MVALUE, &
					REGARDLESS

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
						KEY #0% GE MVALUE, REGARDLESS
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

			IF MO_ORDERLINEOPT::ORDNUM + &
				MO_ORDERLINEOPT::LIN + &
				MO_ORDERLINEOPT::MAKE + &
				MO_ORDERLINEOPT::MODELCODE = MVALUE
			THEN
				SMG_WINDOW::CURREC = 0%
			END IF

		!
		! Change key
		!
		CASE 6%
			MO_ORDERLINEOPT::ORDNUM = &
				LEFT(MVALUE, LEN(MO_ORDERLINEOPT::ORDNUM))

			MO_ORDERLINEOPT::LIN = &
				MID(MVALUE, 11%,LEN(MO_ORDERLINEOPT::LIN))

			MO_ORDERLINEOPT::MAKE = &
				MID(MVALUE, 15%,LEN(MO_ORDERLINEOPT::MAKE))

			MO_ORDERLINEOPT::MODELCODE = &
				RIGHT(MVALUE, 25%)

		END SELECT

	CASE OPT_AFTEROPT

		SELECT MVALUE

		CASE "Add"
			IF MO_ORDERLINEOPT::PRODUCT <> ""
			THEN
				CALL SUBR_TRANTYPE(OE_ORDERJOUR::CUSNUM, &
					MO_ORDERLINEOPT::LINOPT, &
					MO_ORDERLINEOPT::ORDQTY, &
					MO_ORDERLINEOPT::SHPQTY, &
					MO_ORDERLINEOPT::BCKQTY, &
					TRANTYPE$(), TRANQTY())

				FOR I% = 1% TO VAL%(TRANTYPE$(0%))

					V% = IC_WRIT_35BALANCE(MO_ORDERLINEOPT::PRODUCT, &
						OE_ORDERJOUR::LOCATION, &
						LEFT(TRANTYPE$(I%), 2%), &
						TRANQTY(I%))
				NEXT I%
			END IF

		CASE "Change", "Blank", "Initialize"
			IF MO_ORDERLINEOPT_OLD::PRODUCT <> ""
			THEN
				CALL SUBR_TRANTYPE(OE_ORDERJOUR::CUSNUM, &
					MO_ORDERLINEOPT::LINOPT, &
					MO_ORDERLINEOPT_OLD::ORDQTY, &
					MO_ORDERLINEOPT_OLD::SHPQTY, &
					MO_ORDERLINEOPT_OLD::BCKQTY, &
					TRANTYPE$(), TRANQTY())

				FOR I% = 1% TO VAL%(TRANTYPE$(0%))

					V% = IC_WRIT_35BALANCE(MO_ORDERLINEOPT::PRODUCT, &
						OE_ORDERJOUR::LOCATION, &
						LEFT(TRANTYPE$(I%), 2%), &
						-TRANQTY(I%))
				NEXT I%
			END IF

			IF MO_ORDERLINEOPT::PRODUCT <> ""
			THEN
				CALL SUBR_TRANTYPE(OE_ORDERJOUR::CUSNUM, &
					MO_ORDERLINEOPT::LINOPT, &
					MO_ORDERLINEOPT::ORDQTY, &
					MO_ORDERLINEOPT::SHPQTY, &
					MO_ORDERLINEOPT::BCKQTY, &
					TRANTYPE$(), TRANQTY())

				FOR I% = 1% TO VAL%(TRANTYPE$(0%))

					V% = IC_WRIT_35BALANCE(MO_ORDERLINEOPT::PRODUCT, &
						OE_ORDERJOUR::LOCATION, &
						LEFT(TRANTYPE$(I%), 2%), &
						TRANQTY(I%))
				NEXT I%
			END IF

		CASE "Erase"
			IF (MLOOP <> 1%) AND (MO_ORDERLINEOPT::PRODUCT <> "")
			THEN
				CALL SUBR_TRANTYPE(OE_ORDERJOUR::CUSNUM, &
					MO_ORDERLINEOPT::LINOPT, &
					MO_ORDERLINEOPT::ORDQTY, &
					MO_ORDERLINEOPT::SHPQTY, &
					MO_ORDERLINEOPT::BCKQTY, &
					TRANTYPE$(), TRANQTY())

				FOR I% = 1% TO VAL%(TRANTYPE$(0%))

					V% = IC_WRIT_35BALANCE(MO_ORDERLINEOPT::PRODUCT, &
						OE_ORDERJOUR::LOCATION, &
						LEFT(TRANTYPE$(I%), 2%), &
						-TRANQTY(I%))
				NEXT I%
			END IF

		END SELECT

	END SELECT

28000	EXIT FUNCTION

	%PAGE

28100	!*******************************************************************
	! Create a list of options and pass to option lines
	!*******************************************************************

	!
	! If the MO_MODELLINE file has not yet been opened, then open it.
	!
	IF MO_MODELLINE.CH% <= 0%
	THEN
		!
		! Open main file (existing) for use
		!
		%INCLUDE "SOURCE:[MO.OPEN]MO_MODELLINE.OPN"
	END IF

28105	!
	! If the MO_OPTGROUP file has not yet been opened, then open it.
	!
	IF MO_OPTGROUP.CH% <= 0%
	THEN
		!
		! Open main file (existing) for use
		!
		%INCLUDE "SOURCE:[MO.OPEN]MO_OPTGROUP.OPN"
	END IF

28110	!
	! The model line item file should now be opened.  Search for
	! The first record for this modelcode, size and class.
	!
	INV_ARRAY%  = 0%
	INV_COUNT%  = 0%
	LASTTURKEY$ = ""

	WHEN ERROR IN
		FIND #MO_MODELLINE.CH%, KEY #0% GE MO_ORDERLINE::MODELCODE + &
			MO_ORDERLINE::MSIZE + MO_MAKE_READ::CLASS, REGARDLESS
	USE
		CONTINUE 28200 IF ERR = 155%
		EXIT HANDLER
	END WHEN

28120	!
	! Skip out if done with these options
	!
	WHEN ERROR IN
		GET #MO_MODELLINE.CH%
	USE
		CONTINUE 28200 IF ERR= 11%
		EXIT HANDLER
	END WHEN

	GOTO 28200 IF MO_MODELLINE::MODELCODE + MO_MODELLINE::MSIZE + &
		MO_MODELLINE::CLASS <> MO_ORDERLINE::MODELCODE + &
		MO_ORDERLINE::MSIZE + MO_MAKE_READ::CLASS

	GOTO 28120 IF LASTTURKEY$ = MO_MODELLINE::OPTGROUP

	LASTTURKEY$ = MO_MODELLINE::OPTGROUP

	SEQNUM$ = "9999"

28130	WHEN ERROR IN
		GET #MO_OPTGROUP.CH%, &
			KEY #0% EQ MO_MODELLINE::OPTGROUP + "", &
			REGARDLESS
	USE
		CONTINUE 28140 IF ERR = 155%
		EXIT HANDLER
	END WHEN

	SEQNUM$ = MO_OPTGROUP::SEQUENCE

28140	TEXT$ = &
		SEQNUM$ + &
		MO_MODELLINE::OPTGROUP

	!
	! Add (insertion) to list
	!
	GOTO 28150 IF TEXT$ < INV_ARRAY$(LOOP%) &
		FOR LOOP% = 1% TO INV_ARRAY%

	LOOP% = INV_ARRAY% + 1%

28150	INV_ARRAY$(LOOP1% + 1%) = INV_ARRAY$(LOOP1%) &
		FOR LOOP1% = INV_ARRAY% TO LOOP% STEP -1%

	INV_ARRAY$(LOOP%) = TEXT$
	INV_ARRAY% = INV_ARRAY% + 1%
	INV_COUNT% = INV_COUNT% + 1%

	GOTO 28120

28200	RETURN

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
