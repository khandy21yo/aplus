1	%TITLE "Maintain Purchase Order Receipt Journal"
	%SBTTL "PO_MAIN_RECJOUR"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PO_MAIN_RECJOUR(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	Quantities of items which have been ordered and subsequently received are
	!	entered through the ^*Maintain Receiving Journal\* option.
	!
	! Index:
	!
	! Option:
	!
	!	PO_MAIN_RECJOUR$LINE_ITEMS
	!
	! Compile:
	!
	!	$ BAS PO_SOURCE:PO_MAIN_RECJOUR/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP PO_MAIN_RECJOUR
	!	$ DELETE PO_MAIN_RECJOUR.OBJ;*
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
	!	03/03/92 - Dan Perkins
	!		Changed from ENTR_3PO to ENTR_3STRING function after
	!		discussion with powers that be.  ENTR_3STRING is RSET
	!		by using (MVALUE OR 2%).
	!
	!	03/13/92 - Dan Perkins
	!		Display Vendor info.  Test for PO in Journal and
	!		Register.  F-14 lookup added to field 1.
	!
	!	03/22/92 - Kevin Handy
	!		Cleaned up (check)
	!
	!	11/04/94 - Kevin Handy
	!		Added parameter to PO_READ_REGLINE.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/20/96 - Kevin Handy
	!		Reformat source code
	!
	!	05/29/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/10/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	08/03/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	11/02/2000 - Kevin Handy
	!		Use A"x"B
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
	%INCLUDE "FUNC_INCLUDE:PO_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[PO.OPEN]PO_RECJOUR.HB"
	MAP (PO_RECJOUR)	PO_RECJOUR_CDD		PO_RECJOUR
	MAP (PO_RECJOUR_OLD) PO_RECJOUR_CDD PO_RECJOUR_OLD, PO_RECJOUR2

	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.HB"
	MAP (PO_REG_LINE)	PO_REG_LINE_CDD		PO_REG_LINE
	DECLARE			PO_REG_LINE_CDD		PO_REG_LINE_READ

	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_SUB_LINE.HB"
	DECLARE			PO_REG_SUB_LINE_CDD	PO_REG_SUB_LINE_READ

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	DECLARE			AP_VENDOR_CDD		AP_VENDOR_EXAM

	!
	! External functions
	!
	EXTERNAL STRING	FUNCTION CONV_STRING
	EXTERNAL LONG	FUNCTION MAIN_WINDOW
	EXTERNAL LONG	FUNCTION PO_READ_REG_LINE
	EXTERNAL LONG	FUNCTION AP_EXAM_VENDOR

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

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 29000

	!
	! Decide what the user wants to do
	!
	SELECT MOPTION

	!********************************************************************
	! Initialization
	!
	! This option is used to initialize the window structure,
	! set up the default values for add, and open all files
	! necessary that have not already been opened.
	!********************************************************************
	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Order Receipt Journal Maintenance"
		SMG_WINDOW::NHELP = "PO_MAIN_RECJOUR"
		SMG_WINDOW::HSIZE = 74%
		SMG_WINDOW::VSIZE =  8%
		SMG_WINDOW::HPOS  =  2%
		SMG_WINDOW::VPOS  =  8%
		SMG_WINDOW::NITEMS=  4%
		SMG_WINDOW::FLAGS =  0%
		SMG_WINDOW::HVIEW = 74%
		SMG_WINDOW::VVIEW =  8%
		SMG_WINDOW::VHPOS =  2%
		SMG_WINDOW::VVPOS =  8%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Purchase_order"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		!
		! Load in defaults
		!
		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF PO_RECJOUR.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PO_RECJOUR.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PO.OPEN]PO_RECJOUR.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PO_MAIN_RECJOUR = ERR
			CONTINUE 770
		END WHEN

		PO_RECJOUR.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PO.OPEN]PO_RECJOUR.OPN"
		USE
			PO_MAIN_RECJOUR = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PO_RECJOUR.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PO_RECJOUR.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = PO_RECJOUR.CH%
		WHEN ERROR IN
			RESET #PO_RECJOUR.CH%
			GET #PO_RECJOUR.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

	!********************************************************************
	!
	! Display the background
	!
	! This option is used to display the background information on the
	! screen.  It must first clear any junk on the screen, and then
	! write the background onto it.
	!
	!********************************************************************
20100	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	2,  2, "(01) PO Number", &
			5,  2, "(02) Receive Date", &
			6,  2, "(03) Reference #", &
			7,  2, "(04) Received By", &
			3,  2, "     Vendor Num", &
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

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

20200	!********************************************************************
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user, display data,
	! set defaults, and return the data back according to MFLAG.
	!********************************************************************
	CASE OPT_ENTRY

		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 E0Loop:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%
	!++
	! Abstract:FLD001
	!	^*(01) Purchase Order Number\*
	!	.p
	!	The ^*Purchase Order Number\* field
	!	enters the number of a purchase order on which an item has been received.
	!	This field may be blank if an inventory item has been received for which no
	!	purchase order exists.
	!	.p
	!	Pressing the ^*List Choices\* key while the cursor is positioned on
	!	this field will cause the Purchase Order Numbers which are currently
	!	in the register to be displayed.
	!	.p
	!	The field will accommodate up to ten (10) alphanumeric characters.
	!
	! Index:
	!
	!--
			PO_RECJOUR::PO = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "2;21", TEMP$, &
				PO_RECJOUR::PO, MFLAG OR 2%, "~R 'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF MAIN_WINDOW(PO_MAIN_REGLINE.ID, "VX") = 1%
				THEN
					PO_RECJOUR::PO = &
						PO_REG_LINE::PO
				END IF
				GOTO E0Loop
			END IF

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Receive Date\*
	!	.p
	!	The ^*Date\* field enters the date on which the Purchase
	!	Order is received.  It will default to today's date but can be overwritten
	!	with any desired date.
	!	.p
	!	The format for this field is a standard date format, i.e. MMDDYYYY.
	!
	! Index:
	!
	!--
			PO_RECJOUR::RECDATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, "5;21", TEMP$, &
				PO_RECJOUR::RECDATE, MFLAG, "8", MVALUE)

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Reference Number\*
	!	.p
	!	The ^*Reference Number\* field enters
	!	the reference number of a packing list or other items relative to the item which has been
	!	received.  This field may be blank if an inventory item which has been received
	!	has no related items with which to reference.
	!	.p
	!	The field will accommodate sixteen (16) alphanumeric characters.
	!
	! Index:
	!
	!--
			PO_RECJOUR::REFNO = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "6;21", TEMP$, &
				PO_RECJOUR::REFNO, MFLAG, "'E", MVALUE)

		CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Received By\*
	!	.p
	!	The ^*Received By\* field identifies
	!	the person who received the items.
	!	.p
	!	An entry in this field is required.
	!	.p
	!	The field will accommodate up to ten (10) alphanumeric characters.
	!
	! Index:
	!
	!--
			PO_RECJOUR::OPERATOR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "7;21", TEMP$, &
				PO_RECJOUR::OPERATOR, MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY

		PO_MAIN_RECJOUR = 0%

		SELECT MLOOP

		CASE 1%
			IF PO_RECJOUR::PO = ""
			THEN
				PO_MAIN_RECJOUR = 1%
				GOTO 32767
			END IF

			!
			! See if PO Number is already in use in Journal
			!
			IF MVALUE = "ADD"
			THEN
				WHEN ERROR IN
					GET #PO_RECJOUR.CH%, &
						KEY #0% EQ PO_RECJOUR::PO + "", &
						REGARDLESS
				USE
					CONTINUE 20350 IF ERR = 155%
					EXIT HANDLER
				END WHEN

				PO_MAIN_RECJOUR = 1%
				CALL ENTR_3MESSAGE(SCOPE, &
					"Order Already in Use", 1%)
				GOTO 32767
			END IF

20350			!
			! Check if this order number already in register file
			!
			IF MVALUE = "ADD"
			THEN
				PO_REG_LINE_READ::VENDOR = &
					STRING$(LEN(PO_REG_LINE_READ::VENDOR), &
					A"?"B)

				IF PO_READ_REG_LINE(PO_RECJOUR::PO, &
					"", "EQ", PO_REG_LINE_READ, &
					PO_REG_SUB_LINE_READ, QTY(), &
					"") <> CMC$_NORMAL
				THEN
					CALL ENTR_3MESSAGE(SCOPE, &
						"Order Not in Register", 1%)

					PO_MAIN_RECJOUR = 1%
					GOTO 32767
				END IF

				CALL ENTR_3MESSAGE(SCOPE, "", 1%)

				V% = AP_EXAM_VENDOR(PO_REG_LINE_READ::VENDOR, &
					AP_VENDOR_EXAM)

				CITYLINE$ = TRM$(AP_VENDOR_EXAM::POCITY) + ", " + &
					AP_VENDOR_EXAM::POSTATE  + " "  + &
					TRM$(AP_VENDOR_EXAM::POZIP)

				CITYLINE$ = CITYLINE$ + SPACE$(30% - LEN(CITYLINE$))

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					PO_REG_LINE_READ::VENDOR, 3%, 21%,, SMG$M_BOLD)

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					AP_VENDOR_EXAM::VENNAM, 3%, 35%,, SMG$M_BOLD)

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					CITYLINE$, 4%, 35%,, SMG$M_BOLD)

			END IF

		CASE 4%
			IF PO_RECJOUR::OPERATOR = ""
			THEN
				PO_MAIN_RECJOUR = 1%
			END IF

		END SELECT

	!
	! Display all the junk on the screen
	!
	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(1%) AND 2%) = 0%
		THEN
			PO_REG_LINE_READ::VENDOR = &
				STRING$(LEN(PO_REG_LINE_READ::VENDOR), &
				A"?"B)

			V% = PO_READ_REG_LINE(PO_RECJOUR::PO, &
				"", "EQ", PO_REG_LINE_READ, &
				PO_REG_SUB_LINE_READ, QTY(), "")

			V% = AP_EXAM_VENDOR(PO_REG_LINE_READ::VENDOR, &
				AP_VENDOR_EXAM)

			CITYLINE$ = TRM$(AP_VENDOR_EXAM::POCITY) + ", " + &
				AP_VENDOR_EXAM::POSTATE  + " "  + &
				TRM$(AP_VENDOR_EXAM::POZIP)

			CITYLINE$ = CITYLINE$ + SPACE$(30% - LEN(CITYLINE$))

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PO_REG_LINE_READ::VENDOR, 3%, 21%,, SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AP_VENDOR_EXAM::VENNAM, 3%, 35%,, SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				CITYLINE$, 4%, 35%,, SMG$M_BOLD)

		END IF

	!
	! Set PO_RECJOUR_OLD value
	!
20500	CASE OPT_SETOLD
		PO_RECJOUR_OLD = PO_RECJOUR

	!
	! Restore PO_RECJOUR_OLD value
	!
	CASE OPT_RESETOLD
		PO_RECJOUR = PO_RECJOUR_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PO_RECJOUR2 = PO_RECJOUR

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PO_RECJOUR = PO_RECJOUR2

		IF MFLAG = 1%
		THEN
			PO_RECJOUR::RECDATE = DATE_TODAY &
				IF PO_RECJOUR::RECDATE = ""
		END IF

	!
	! View header
	!
	CASE OPT_VIEW

		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  PoNumber     RecDate     RefNo" + &
				"             Received By"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "015,027,045"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = CONV_STRING(PO_RECJOUR::PO, CMC$_LEFT) + "   " + &
				PRNT_DATE(PO_RECJOUR::RECDATE, 8%) + "  "  + &
				PO_RECJOUR::REFNO + "  "  + &
				PO_RECJOUR::OPERATOR

		END SELECT

	!
	! Find
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #PO_RECJOUR.CH%, &
				KEY #0% GE PO_RECJOUR::PO + "", &
				REGARDLESS

		END SELECT

	END SELECT

	EXIT FUNCTION

	%PAGE

29000	!
	! Trap errors
	!
	ON ERROR GO BACK

32767	END FUNCTION
