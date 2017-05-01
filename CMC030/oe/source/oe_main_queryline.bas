1	%TITLE "Register Line Query Journal"
	%SBTTL "OE_MAIN_QUERYLINE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG OE_MAIN_QUERYLINE(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	The Register Line Query Journal is used to view lines
	!	and reflects the product, quantity shipped, quantity ordered and
	!	quantity canceled with the remaining balance.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_MAIN_QUERYLINE/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN OE_MAIN_QUERYLINE
	!	$ DELETE OE_MAIN_QUERYLINE.OBJ;*
	!
	! Author:
	!
	!	07/16/91 - Val James "Dazed an' Confused" Allen
	!
	! Modification history:
	!
	!	08/02/91 - Craig Tanner
	!		Moved display of feilds from case OPT_ENTRY
	!		to case OPT_DISPLAY. Also added more items
	!		to the display.
	!
	!	08/27/91 - Dan Perkins
	!		Modified OPT_DISPLAY so Next would function
	!		properly.
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
	!		use integer for #key
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/21/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.HB"
	MAP (OE_REGLINE)	OE_REGLINE_CDD		OE_REGLINE
	MAP (OE_REGLINE_OLD)	OE_REGLINE_CDD		OE_REGLINE_OLD, &
							OE_REGLINE_DEF
	DECLARE			OE_REGLINE_CDD		OE_REGLINE_READ

	%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERJOUR.HB"
	MAP (OE_ORDERJOUR)	OE_ORDERJOUR_CDD	OE_ORDERJOUR

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	!
	! Common Statements
	!
	COM (CH_OE_ORDERJOUR) &
		BATCH_NO$ = 2%

	COM (CH_OE_REGLINE) &
		OE_REGLINE.CH%, &
		OE_REGLINE.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION OE_READ_REGLINE

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Order Entry Lines"
		SMG_WINDOW::CURREC = -2%
		SMG_WINDOW::NHELP = "OE_MAIN_QUERYLINE"
		SMG_WINDOW::HSIZE = 40%
		SMG_WINDOW::VSIZE = 6%
		SMG_WINDOW::HPOS  = 35%
		SMG_WINDOW::VPOS  = 10%
		SMG_WINDOW::NITEMS= 1%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::HVIEW = 91%
		SMG_WINDOW::VVIEW = 12%
		SMG_WINDOW::VHPOS = 22%
		SMG_WINDOW::VVPOS = 7%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Line_number"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		!
		! Load in defaults for chart
		!
		CALL READ_DEFAULTS(SMG_WINDOW)


700		!
		! Declare channels
		!
		IF OE_REGLINE.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF OE_REGLINE.READONLY%
			GOTO 790
		END IF

		!
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.OPN"
		USE
			CONTINUE 760 IF ERR = 10%
			OE_MAIN_QUERYLINE = ERR
			CONTINUE 770
		END WHEN

		OE_REGLINE.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.OPN"
		USE
			OE_MAIN_QUERYLINE = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		OE_REGLINE.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(OE_REGLINE.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = OE_REGLINE.CH%
		WHEN ERROR IN
			RESET #OE_REGLINE.CH%
			GET #OE_REGLINE.CH%, REGARDLESS
		USE
			CONTINUE 32767 IF ERR = 11%
			EXIT HANDLER
		END WHEN

	CASE OPT_OPTLIST

		MVALUE = "Find Next Restore Help eXit"



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


		DATA	01,02, "Line #", &
			02,02, "Product", &
			03,02, "Qty Ordered", &
			04,02, "Qty Shipped", &
			05,02, "Qty Cancelled", &
			06,02, "Qty Remaining", &
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
	!	The ^*Line\* field enters the line number
	!	of the order which is to be accessed.
	!	.lm -5
	!
	! Index:
	!	.x Line
	!
	!--
			OE_REGLINE::LIN = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"01;25",TEMP$, OE_REGLINE::LIN, MFLAG, &
				"~L0'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	CASE OPT_DISPLAY

		V% = OE_READ_REGLINE(OE_REGLINE::ORDNUM, &
			OE_REGLINE::LIN, "EQ", OE_REGLINE_READ, QTY())

		OE_REGLINE = OE_REGLINE_READ

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			OE_REGLINE_READ::PRODUCT, &
			2%, 16%, , SMG$M_BOLD)

		ORDQTY$ = FORMAT$(QTY(1%), "###,###.##")
		SHPQTY$ = FORMAT$(QTY(2%), "###,###.##")
		CANQTY$ = FORMAT$(QTY(3%), "###,###.##")
		SHPQTY2$ = FORMAT$(QTY(4%), "###,###.##")
		CANQTY2$ = FORMAT$(QTY(5%), "###,###.##")

		BALANCE = QTY(1%) - QTY(2%) - QTY(3%)
		BALANCE = 0.0 IF BALANCE < 0.0
		BALQTY$ = FORMAT$(BALANCE, "###,###.##")

		BALANCE = QTY(1%) - QTY(2%) - QTY(3%) - QTY(4%) - &
			QTY(5%)
		BALANCE = 0.0 IF BALANCE < 0.0
		BALQTY2$ = FORMAT$(BALANCE, "###,###.##")

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			ORDQTY$, &
			3%, 16%, , SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			SHPQTY$, &
			4%, 16%, , SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			CANQTY$, &
			5%, 16%, , SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			BALQTY$, &
			6%, 16%, , SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			SHPQTY2$, &
			4%, 27%, , SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			CANQTY2$, &
			5%, 27%, , SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			BALQTY2$, &
			6%, 27%, , SMG$M_BOLD)

	!
	! Set OE_REGLINE_OLD value
	!
20500	CASE OPT_SETOLD
		OE_REGLINE_OLD = OE_REGLINE

	!
	! Restore OE_REGLINE_OLD value
	!
	CASE OPT_RESETOLD
		OE_REGLINE = OE_REGLINE_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		OE_REGLINE_DEF = OE_REGLINE

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		OE_REGLINE = OE_REGLINE_DEF

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!
		OE_REGLINE::ORDNUM  = MVALUE

	!
	! Find the next record
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE OE_REGLINE::ORDNUM + &
				OE_REGLINE::LIN, REGARDLESS

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
				SMG_WINDOW::CURREC = 0%
			USE
				CONTINUE 28000
			END WHEN

		!
		! Check if still right key
		!
		CASE 3%
			SMG_WINDOW::CURREC = -1%
			IF OE_REGLINE::ORDNUM = MVALUE
			THEN
				SMG_WINDOW::CURREC = 0%
			END IF

		!
		! Change key
		!
		CASE 6%
			OE_REGLINE::ORDNUM = MVALUE

		END SELECT

	END SELECT

28000	EXIT FUNCTION

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
