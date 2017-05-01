1	%TITLE "Purchase Order Type Definition File Maintenance"
	%SBTTL "PO_MAIN_CALCORDER"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PO_MAIN_CALCORDER(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1987, 1988 BY
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
	! Computer Management Center.
	!
	! CMC assumes no responsibility for the use or reliability of
	! its software on equipment which is not supported by CMC.
	!
	!++
	! Abstract:HELP
	!	.p
	!	The Purchase Order Types are determined in the ^*Maintain PO Type Table\*
	!	option in the PO Master Tables menu.
	!
	! Index:
	!	.x Table>PO Type
	!	.x PO Type>Table
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS PO_SOURCE:PO_MAIN_CALCORDER/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP PO_MAIN_CALCORDER
	!	$ DELETE PO_MAIN_CALCORDER.OBJ;*
	!
	!
	! Author:
	!
	!	03/14/90 - Kevin Handy
	!
	! Modification history:
	!
	!	04/13/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Standards.
	!		Include IC_WINDOW.COM.
	!
	!	07/01/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/28/97 - Kevin Handy
	!		Lose unecessary external definitions
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/05/2000 - Kevin Handy
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
	%INCLUDE "FUNC_INCLUDE:AP_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:IC_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP (AP_VENDOR)		AP_VENDOR_CDD	AP_VENDOR

	%INCLUDE "SOURCE:[PO.OPEN]PO_CALCORDER.HB"
	MAP (PO_CALCORDER)	PO_CALCORDER_CDD	PO_CALCORDER
	MAP (PO_CALCORDER_OLD)	PO_CALCORDER_CDD	PO_CALCORDER_OLD, PO_CALCORDER2

	%INCLUDE "SOURCE:[PO.OPEN]PO_CONTROL.HB"
	MAP (PO_CONTROL)	PO_CONTROL_CDD		PO_CONTROL

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_PO_CALCORDER) &
		PO_CALCORDER.CH%, &
		PO_CALCORDER.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION MAIN_WINDOW

	%PAGE

	!
	! Give text description of formula parts
	!
	DEF FNFORM_TEXT$(X$)

		SELECT(X$)
		CASE "A"
			FNFORM_TEXT$ = "  Allocated"
		CASE "H"
			FNFORM_TEXT$ = "    On Hand"
		CASE "O"
			FNFORM_TEXT$ = "   On Order"
		CASE "S"
			FNFORM_TEXT$ = "     Safety"
		CASE "1"
			FNFORM_TEXT$ = "       Qtr1"
		CASE "2"
			FNFORM_TEXT$ = "       Qtr2"
		CASE "3"
			FNFORM_TEXT$ = "       Qtr3"
		CASE "4"
			FNFORM_TEXT$ = "       Qtr4"
		CASE "5"
			FNFORM_TEXT$ = "       Qtr5"
		CASE ELSE
			FNFORM_TEXT$ = "           "
		END SELECT

	FNEND

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	!******************************************************************
	! Initialization
	!
	! This option is used to initialize the window structure,
	! set up the default values for add, and open all files
	! necessary that have not already been opened.
	!******************************************************************
	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "PO Type Maintenance"
		SMG_WINDOW::NHELP = "PO_MAIN_CALCORDER"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 5%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 2%
		SMG_WINDOW::KNAME(0%) = "Vendor"
			SMG_WINDOW::KFIELD(0%, 0%) = 3%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
			SMG_WINDOW::KFIELD(0%, 2%) = 2%
			SMG_WINDOW::KFIELD(0%, 3%) = 3%
		SMG_WINDOW::KNAME(1%) = "Product"
			SMG_WINDOW::KFIELD(1%, 0%) = 2%
			SMG_WINDOW::KFIELD(1%, 1%) = 3%
			SMG_WINDOW::KFIELD(1%, 2%) = 1%

		!
		! Load in defaults for chart
		!
		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF PO_CALCORDER.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PO_CALCORDER.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PO.OPEN]PO_CALCORDER.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PO_MAIN_CALCORDER  = ERR
			CONTINUE 770
		END WHEN

		PO_CALCORDER.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PO.OPEN]PO_CALCORDER.OPN"
		USE
			PO_MAIN_CALCORDER = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PO_CALCORDER.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PO_CALCORDER.CH%)

		GOTO ExitFunction

790		SMG_WINDOW::CHAN  = PO_CALCORDER.CH%
		WHEN ERROR IN
			RESET #PO_CALCORDER.CH%
			GET #PO_CALCORDER.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

	%PAGE

20100	!******************************************************************
	! Display the background
	!
	! This option is used to display the background information
	! on the screen.  It must first clear any junk on the screen,
	! and then write the background onto it.
	!******************************************************************

	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	2,  1, "(01) Location", &
			3,  1, "(02) Vendor", &
			4,  1, "(03) Product", &
			5,  1, "(04) Order Qty", &
			6,  1, "(05) Cost", &
			0,  0, ""

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

		!
		! Display Titles for Load Formula
		!
		TEXT$ = ""
		FOR I% = 1% TO 5%
			TEXT$ = TEXT$ + &
				FNFORM_TEXT$(MID(PO_CONTROL::LOAD_FORMULA, I%, 1%))
		NEXT I%

		TEXT$ = TEXT$ + SPACE$(78% - LEN(TEXT$))

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			TEXT$, 12%, 1%, 0%, SMG$M_REVERSE)

		TEXT$ = ""
		FOR I% = 6% TO 10%
			TEXT$ = TEXT$ + &
				FNFORM_TEXT$(MID(PO_CONTROL::LOAD_FORMULA, I%, 1%))
		NEXT I%

		TEXT$ = TEXT$ + SPACE$(78% - LEN(TEXT$))

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			TEXT$, 14%, 1%, 0%, SMG$M_REVERSE)

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

	!
	! Display additional information
	!
	CASE OPT_DISPLAY

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		TOTAL = 0.0

		!
		! Display Titles for Load Formula
		!
		TEXT$ = ""
		FOR I% = 1% TO 5%
			TEXT$ = TEXT$ + &
				FORMAT$(PO_CALCORDER::QUANITY(I%), &
				" <%>#####.###")
			TOTAL = TOTAL + PO_CALCORDER::QUANITY(I%)
		NEXT I%

		TEXT$ = TEXT$ + SPACE$(78% - LEN(TEXT$))

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			TEXT$, 13%, 1%)

		TEXT$ = ""
		FOR I% = 6% TO 10%
			TEXT$ = TEXT$ + &
				FORMAT$(PO_CALCORDER::QUANITY(I%), &
				" <%>#####.###")
			TOTAL = TOTAL + PO_CALCORDER::QUANITY(I%)
		NEXT I%

		TEXT$ = TEXT$ + SPACE$(78% - LEN(TEXT$))

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			TEXT$, 15%, 1%)

		TEXT$ = "Total of all Items: " + &
			FORMAT$(TOTAL, "######.###")

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			TEXT$, 17%, 45%)

		GOSUB LookupMast

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			UTL_LOCATION::LOCNAME, 2%, 40%)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			AP_VENDOR::VENNAM, 3%, 40%)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			PD_PRODUCT::DESCRIPTION, 4%, 40%)

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

		SCOPE::SCOPE_EXIT = 0%

 Eloop:		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Location\*
	!
	! Index:
	!
	!--

			PO_CALCORDER::LOCATION = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"2;22", TEMP$, &
				PO_CALCORDER::LOCATION, MFLAG, "'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_F14

				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "VX") = 1%
				THEN
					PO_CALCORDER::LOCATION = &
						UTL_LOCATION::LOCATION
				END IF
				GOTO ELoop

			CASE SMG$K_TRM_F17

				V% = MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "M")
				PO_CALCORDER::LOCATION = &
					UTL_LOCATION::LOCATION
				GOTO ELoop

			END SELECT

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Vendor\*
	!
	! Index:
	!
	!--

			PO_CALCORDER::VENDOR = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"3;22", TEMP$, &
				PO_CALCORDER::VENDOR, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(AP_MAIN_VENDOR.ID, "VX") = 1%)
				THEN
					PO_CALCORDER::VENDOR = AP_VENDOR::VENNUM
				END IF
				GOTO ELoop
			END IF

		CASE 3%

	!++
	! Abstract:FLD003
	!
	!	^*(03) Product\*
	!
	! Index:
	!--

			PO_CALCORDER::PRODUCT = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"4;22", TEMP$, &
				PO_CALCORDER::PRODUCT, MFLAG, "'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_F14
				IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, "VX") = 1%
				THEN
					PO_CALCORDER::PRODUCT = &
						PD_PRODUCT::PRODUCT_NUM
				END IF
				GOTO ELoop

			CASE SMG$K_TRM_F17

				V% = MAIN_WINDOW(IC_WRIT_PRODUCT.ID, &
					"M0" + PO_CALCORDER::PRODUCT)

				PO_CALCORDER::PRODUCT = &
					PD_PRODUCT::PRODUCT_NUM
				GOTO ELoop
			END SELECT

		CASE 4%

	!++
	! Abstract:FLD004
	!
	!	^*(04) Order Quanity\*
	!
	! Index:
	!--

			PO_CALCORDER::ORDER = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"5;22", TEMP$, &
				PO_CALCORDER::ORDER, MFLAG, "#####.###", MVALUE)

		CASE 5%

	!++
	! Abstract:FLD005
	!
	!	^*(04) Cost\*
	!
	! Index:
	!--

			PO_CALCORDER::COST = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"6;22", TEMP$, &
				PO_CALCORDER::COST, MFLAG, "#####.##", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

20300	CASE OPT_TESTENTRY
		PO_MAIN_CALCORDER = 0%

		SELECT MLOOP

		CASE 1%
			CALL PO_FUNC_CALCORDER(PO_CALCORDER::PRODUCT, &
				PO_CALCORDER::LOCATION, &
				PO_CALCORDER::VENDOR, &
				PO_CONTROL::LOAD_FORMULA, &
				PO_CALCORDER)

		CASE 2%
			CALL PO_FUNC_CALCORDER(PO_CALCORDER::PRODUCT, &
				PO_CALCORDER::LOCATION, &
				PO_CALCORDER::VENDOR, &
				PO_CONTROL::LOAD_FORMULA, &
				PO_CALCORDER)

		CASE 3%
			CALL PO_FUNC_CALCORDER(PO_CALCORDER::PRODUCT, &
				PO_CALCORDER::LOCATION, &
				PO_CALCORDER::VENDOR, &
				PO_CONTROL::LOAD_FORMULA, &
				PO_CALCORDER)

		END SELECT

		!
		! Re-fresh screen
		!
		TEMP% = PO_MAIN_CALCORDER(SMG_WINDOW, &
			OPT_ENTRY, LOOP%, 1%, MVALUE) &
			FOR LOOP% = 1% TO SMG_WINDOW::NITEMS

		TEMP% = PO_MAIN_CALCORDER(SMG_WINDOW, &
			OPT_DISPLAY, LOOP%, 1%, MVALUE)

20500	CASE OPT_SETOLD
		PO_CALCORDER_OLD = PO_CALCORDER

	CASE OPT_RESETOLD
		PO_CALCORDER = PO_CALCORDER_OLD

	CASE OPT_SETDEFAULT
		PO_CALCORDER2 = PO_CALCORDER

	CASE OPT_RESETDEFAULT
		PO_CALCORDER = PO_CALCORDER2

	CASE OPT_VIEW

		SELECT MLOOP

		CASE 1%
			MVALUE = "  Location Vendor     Product    Order"

		CASE 2%
			MVALUE = "011,023,035"

		CASE 3%
			MVALUE = &
				PO_CALCORDER::LOCATION + "     " + &
				PO_CALCORDER::VENDOR + "  " + &
				PO_CALCORDER::PRODUCT + " " + &
				FORMAT$(PO_CALCORDER::ORDER, "######.####")

		END SELECT

	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #PO_CALCORDER.CH%, &
				KEY #0% GE PO_CALCORDER::LOCATION + &
					PO_CALCORDER::VENDOR + &
					PO_CALCORDER::PRODUCT, &
				REGARDLESS

		CASE 1%
			FIND #PO_CALCORDER.CH%, &
				KEY #1% GE PO_CALCORDER::PRODUCT + &
					PO_CALCORDER::LOCATION, &
				REGARDLESS

		END SELECT

	END SELECT

 ExitFunction:
	EXIT FUNCTION

 LookupMast:
	!*******************************************************************
	! Look up information in master files
	!*******************************************************************

	!
	! Vendor
	!
	IF AP_VENDOR::VENNUM <> PO_CALCORDER::VENDOR
	THEN
		ST% = MAIN_WINDOW(AP_MAIN_VENDOR.ID, "Q0" + PO_CALCORDER::VENDOR)

		IF ST% <> 1%
		THEN
			AP_VENDOR::VENNAM = ""
		END IF
	END IF

	!
	! Product
	!
	IF PD_PRODUCT::PRODUCT_NUM <> PO_CALCORDER::PRODUCT
	THEN
		ST% = MAIN_WINDOW(PD_MAIN_PRODUCT.ID, "Q0" + PO_CALCORDER::PRODUCT)

		IF ST% <> 1%
		THEN
			PD_PRODUCT::DESCRIPTION = ""
		END IF
	END IF

	!
	! Location
	!
	IF UTL_LOCATION::LOCATION <> PO_CALCORDER::LOCATION
	THEN
		ST% = MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "Q0" + PO_CALCORDER::LOCATION)

		IF ST% <> 1%
		THEN
			UTL_LOCATION::LOCNAME = ""
		END IF
	END IF

	RETURN

29000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
