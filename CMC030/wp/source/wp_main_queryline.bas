1	%TITLE "Register Line Query Journal"
	%SBTTL "WP_MAIN_QUERYLINE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG WP_MAIN_QUERYLINE(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	and reflect the product, qty shipped, qty ordered and
	!	qty cancelled with the remaining balance.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_MAIN_QUERYLINE/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN WP_MAIN_QUERYLINE
	!	$ DELETE WP_MAIN_QUERYLINE.OBJ;*
	!
	! Author:
	!
	!	07/31/91 - Craig Tanner
	!
	! Modification history:
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
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/31/96 - Kevin Handy
	!		Reformat source code
	!
	!	06/06/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/09/99 - Kevin Handy
	!		Lose line 770 (Dead Code)
	!
	!	11/24/2000 - Kevin Handy
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

	%INCLUDE "FUNC_INCLUDE:WP_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.HB"
	MAP (WP_REGLINE)	WP_REGLINE_CDD		WP_REGLINE
	MAP (WP_REGLINE_OLD)	WP_REGLINE_CDD		WP_REGLINE_OLD, &
							WP_REGLINE_DEF
	DECLARE			WP_REGLINE_CDD		WP_REGLINE_READ

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	!
	! Common Statements
	!
	COM (CH_WP_REGLINE) &
		WP_REGLINE.CH%, &
		WP_REGLINE.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION WP_READ_REGLINE

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Work in Process Lines"
		SMG_WINDOW::CURREC= -2%
		SMG_WINDOW::NHELP = "WP_MAIN_QUERYLINE"
		SMG_WINDOW::HSIZE = 40%
		SMG_WINDOW::VSIZE = 8%
		SMG_WINDOW::HPOS  = 35%
		SMG_WINDOW::VPOS  = 10%
		SMG_WINDOW::NITEMS= 3%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::HVIEW = 71%
		SMG_WINDOW::VVIEW = 12%
		SMG_WINDOW::VHPOS = 3%
		SMG_WINDOW::VVPOS = 7%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Line_number"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		CALL READ_DEFAULTS(SMG_WINDOW)

700		!
		! Declare channels
		!
		IF WP_REGLINE.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF WP_REGLINE.READONLY%
			GOTO 790
		END IF

		!
		!
750		%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.OPN"
		WP_REGLINE.READONLY% = 0%
		GOTO 790

 !770
		!
		! File not open, so reset channel
		!
 !		CALL ASSG_FREECHANNEL(WP_REGLINE.CH%)
 !
 !		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = WP_REGLINE.CH%
		WHEN ERROR IN
			RESET #WP_REGLINE.CH%
			GET #WP_REGLINE.CH%, REGARDLESS
		USE
			CONTINUE 32767 IF ERR = 11%
			EXIT HANDLER
		END WHEN

	CASE OPT_OPTLIST

		MVALUE = "Find Next Restore Help View eXit"

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
			02,02, "Item Code", &
			03,02, "Description", &
			04,02, "Qty Ordered", &
			05,02, "Qty Complete", &
			06,02, "Qty Cancelled", &
			07,02, "Balance", &
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
			WP_REGLINE::LLINE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"01;16",TEMP$, WP_REGLINE::LLINE, MFLAG, &
				"~L0'E", MVALUE)

		CASE 2%
			WP_REGLINE::ITEMCODE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"02;16",TEMP$, WP_REGLINE::ITEMCODE, MFLAG, &
				"'E", MVALUE)

		CASE 3%
			WP_REGLINE::DESCR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"03;16",TEMP$, WP_REGLINE::DESCR, MFLAG, &
				"'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Display all the other junk
	!
	CASE OPT_DISPLAY
		V% = WP_READ_REGLINE(WP_REGLINE::JOB, &
			WP_REGLINE::LLINE, "EQ", WP_REGLINE_READ, QTY())

		WP_REGLINE = WP_REGLINE_READ

		ORDQTY$  = FORMAT$(QTY(1%), "###,###.##")
		SHPQTY$  = FORMAT$(QTY(2%), "###,###.##")
		CANQTY$  = FORMAT$(QTY(3%), "###,###.##")
		SHPQTY2$ = FORMAT$(QTY(5%), "###,###.##")
		CANQTY2$ = FORMAT$(QTY(6%), "###,###.##")

		BALANCE  = QTY(1%) - QTY(2%) - QTY(3%)
		BALANCE  = 0.0 IF BALANCE < 0.0
		BALQTY$  = FORMAT$(BALANCE, "###,###.##")

		BALANCE  = QTY(1%) - QTY(2%) - QTY(3%) - QTY(5%) - QTY(6%)

		BALANCE  = 0.0 IF BALANCE < 0.0
		BALQTY2$ = FORMAT$(BALANCE, "###,###.##")

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			ORDQTY$, 4%, 16%, , SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			SHPQTY$, 5%, 16%, , SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			CANQTY$, 6%, 16%, , SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			BALQTY$, 7%, 16%, , SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			SHPQTY2$, 5%, 27%, , SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			CANQTY2$, 6%, 27%, , SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			BALQTY2$, 7%, 27%, , SMG$M_BOLD)

	!
	! Set WP_REGLINE_OLD value
	!
20500	CASE OPT_SETOLD
		WP_REGLINE_OLD = WP_REGLINE

	!
	! Restore WP_REGLINE_OLD value
	!
	CASE OPT_RESETOLD
		WP_REGLINE = WP_REGLINE_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		WP_REGLINE_DEF = WP_REGLINE

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		WP_REGLINE = WP_REGLINE_DEF

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!
		WP_REGLINE::JOB = MVALUE

	!
	! Find the next record
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #SMG_WINDOW::CHAN, KEY #0% GE WP_REGLINE::JOB + &
				WP_REGLINE::LLINE, REGARDLESS

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
			MVALUE = "  Line Type ItemCode       StartDate  Quantity UnitCost Batch"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "007,012,027,038,047,056"

		!
		! Convert current record into text
		!
		CASE 3%
			STUFFS$ = " " + WP_REGLINE::REC_TYPE
			STUFFS$ = "ORD" IF WP_REGLINE::REC_TYPE = "01"
			STUFFS$ = "COM" IF WP_REGLINE::REC_TYPE = "02"
			STUFFS$ = "CAN" IF WP_REGLINE::REC_TYPE = "03"

			MVALUE = WP_REGLINE::LLINE + " "  + &
				STUFFS$ + "  " + &
				WP_REGLINE::ITEMCODE + " "  + &
				PRNT_DATE(WP_REGLINE::START_DATE, 8%) + " "  + &
				FORMAT$(WP_REGLINE::QTY, "#####.##") + " "  + &
				FORMAT$(WP_REGLINE::COST, "#####.##") + " "  + &
				WP_REGLINE::BATCH

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
				FIND #SMG_WINDOW::CHAN, KEY #0% EQ MVALUE,REGARDLESS

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
					FIND #SMG_WINDOW::CHAN, KEY #0% GE MVALUE + &
						WP_REGLINE::LLINE, REGARDLESS
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
			IF WP_REGLINE::JOB = MVALUE
			THEN
				SMG_WINDOW::CURREC = 0%
			END IF

		!
		! Change key
		!
		CASE 6%
			WP_REGLINE::JOB = MVALUE

		END SELECT

	END SELECT

28000	EXIT FUNCTION

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
