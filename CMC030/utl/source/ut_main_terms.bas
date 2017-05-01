1	%TITLE "Terms Table File Maintenance"
	%SBTTL "UT_MAIN_TERMS"
	%IDENT "V3.6a Calico"

	FUNCTION LONG UT_MAIN_TERMS(CDD_WINDOW_CDD SMG_WINDOW, &
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
	! any other copies therof may not be provided or otherwise made
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
	!	.b
	!	.lm +5
	!	The file where Term Types are maintained is accessed through
	!	the ^*Maintain Terms Table\* option.
	!	.lm -5
	!
	! Index:
	!	.x Terms>Maintenance
	!	.x Maintenance>Terms
	!	.Y TERMS
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UT_MAIN_TERMS/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP UT_MAIN_TERMS
	!	$ DELETE UT_MAIN_TERMS.OBJ;*
	!
	!
	! Author:
	!
	!	03/09/90 - Kevin Handy
	!
	! Modification history:
	!
	!	08/28/91 - Dan Perkins
	!		Realigned View Screen.
	!
	!	02/05/93 - Kevin Handy
	!		Added discount date, discount days, due date, and
	!		due days fields.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/30/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/10/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	12/01/2000 - Kevin Handy
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

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_TERMS.HB"
	MAP (UTL_TERMS)	UTL_TERMS_CDD	UTL_TERMS
	MAP (UTL_TERMS_OLD)	UTL_TERMS_CDD	UTL_TERMS_OLD, UTL_TERMS2

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_UTL_TERMS) &
		UTL_TERMS.CH%, &
		UTL_TERMS.READONLY%

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
		SMG_WINDOW::DESCR = "Terms Maintenance"
		SMG_WINDOW::NHELP = "UT_MAIN_TERMS"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 7%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 2%
		SMG_WINDOW::KNAME(0%) = "Code"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
		SMG_WINDOW::KNAME(1%) = "Description"
			SMG_WINDOW::KFIELD(1%, 0%) = 1%
			SMG_WINDOW::KFIELD(1%, 1%) = 2%

		!
		! Load in defaults for chart
		!
		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF UTL_TERMS.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF UTL_TERMS.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_TERMS.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			UT_MAIN_TERMS  = ERR
			CONTINUE 770
		END WHEN

		UTL_TERMS.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_TERMS.OPN"
		USE
			UT_MAIN_TERMS = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		UTL_TERMS.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(UTL_TERMS.CH%)

		GOTO ExitFunction

790		SMG_WINDOW::CHAN  = UTL_TERMS.CH%
		WHEN ERROR IN
			RESET #UTL_TERMS.CH%
			GET #UTL_TERMS.CH%, REGARDLESS
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

		DATA	6,  1, "(01) Terms Code", &
			8,  1, "(02) Description", &
			10, 1, "(03) Discount", &
			11, 1, "(04) Discount Days", &
			12, 1, "(05) Discount Date", &
			14, 1, "(06) Due Days", &
			15, 1, "(07) Due Date", &
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
	!	.x Terms>Code
	!	^*(01) Terms Code\*
	!	.b
	!	.lm +5
	!	The ^*Terms Code\* field enters a user defined
	!	code which identifies a particular terms.
	!	.b
	!	Example:  ^*EM\* - End of Month
	!	.b
	!	Up to two (02) characters may be entered.
	!	.lm -5
	!
	! Index:
	!	.x Code>Terms
	!
	!--

			UTL_TERMS::CODE = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"6;22", TEMP$, &
				UTL_TERMS::CODE, MFLAG, "'E", MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	.x Terms>Description
	!	^*(02) Description\*
	!	.b
	!	.lm +5
	!	The ^*Description\* field enters a brief description
	!	for the terms code entered in field (01).
	!	.b
	!	Example: EM - ^*End of Month\*
	!	.b
	!	Forty (40) spaces are available to enter the description.
	!	.lm -5
	!
	! Index:
	!	.x Description>Terms
	!
	!--

			UTL_TERMS::DESCR = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"8;22", TEMP$, &
				UTL_TERMS::DESCR, MFLAG, "'E", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	.x Discount>Terms
	!	^*(03) Discount\*
	!	.b
	!	.lm +5
	!	The ^*Discount\* field enters a discount percentage
	!	for the Terms Code entered in field (01).
	!	.b
	!	If the discount percentage is to be 10%, the entry would be made as 10.00.
	!	.lm -5
	!
	! Index:
	!	.x Terms>Discount
	!	.y DISCOUNT
	!
	!--

			UTL_TERMS::DISCOUNT = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"10;22", TEMP$, &
				UTL_TERMS::DISCOUNT, MFLAG, "###.###", MVALUE)

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Discount Days\*
	!	.b
	!	.lm +5
	!	The ^*Discount Days\* field enters a vendor's
	!	discount terms when discounts may be taken if payment is made within
	!	a specified number of days after the invoice date, as in "2/10". A
	!	significant value in this field will cause the system to automatically
	!	calculate the discount date (by adding the field to
	!	the invoice date) and enter the calculated date in the purchases
	!	journal record. The calculated date may be overridden in the
	!	purchases journal by entering an alternative date.
	!	.b
	!	This field may contain up to four (4) numeric characters.
	!	.note
	!	The terms specifying a vendor's discount policy should
	!	be entered in either this field ^&or\& field (24) Discount
	!	Date, but ^&not\&#^&both\&.
	!	.end note
	!
	! Index:
	!	.x Discount Days>Vendor
	!	.x Vendor>Discount Days
	!
	!--

			UTL_TERMS::DISCOUNTDAYS = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "11;22", TEMP$, &
				UTL_TERMS::DISCOUNTDAYS * 1.0, MFLAG, &
				"####", MVALUE)

		CASE 5%

	!++
	! Abstract:FLD005
	!	^*(05) Discount Date\*
	!	.b
	!	.lm +5
	!	The ^*Discount Date\* field enters a vendor's
	!	discount terms when discounts may be taken if payment is made on or
	!	before a specified date, as in "2/10th". A significant value in
	!	this field will cause the system to automatically determine the
	!	discount date to be the next occurrence of the designated date and
	!	enter that date in the purchases journal record. The determined
	!	date may be overridden in the purchases journal by entering an
	!	alternative date.
	!	.b
	!	This field may contain up to two (2) numeric characters.
	!	.note
	!	The terms specifying a vendor's discount policy should
	!	be entered in either this field ^&or\& field (23) Discount
	!	Days, but ^&not\&#^&both\&.
	!	.end note
	!
	! Index:
	!	.x Discount Date>Vendor
	!	.x Vendor>Discount Date
	!
	!--

			UTL_TERMS::DISCOUNTDATE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "12;22", TEMP$, &
				UTL_TERMS::DISCOUNTDATE, MFLAG, "'E", &
				MVALUE)

		CASE 6%

	!++
	! Abstract:FLD006
	!	^*(06) Due Days\*
	!	.b
	!	.lm +5
	!	The ^*Due Days\* field enters a vendor's terms in
	!	reference to the number of elapsed days until an invoice is due, as in "N/10".
	!	A significant value in this field will cause the system to automatically
	!	calculate the due date (by adding the field to the invoice
	!	date) and enter the calculated date in the "due date" field in the purchases
	!	journal record. The calculated date may be overridden in the purchases journal
	!	by entering an alternative date.
	!	.b
	!	This field may contain up to four (4) numeric characters.
	!	.note
	!	The terms specifying when a vendor's invoices are due should be entered in
	!	either this field ^&or\& field (07) Due Date, but ^&not\&#^&both\&.
	!	.end note
	!
	! Index:
	!	.x Due Days>Terms>Vendor
	!	.x Terms>Due Days>Vendor
	!	.x Vendor>Terms>Due Days
	!
	!--

			UTL_TERMS::DUEDAYS = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "14;22", TEMP$, &
				UTL_TERMS::DUEDAYS * 1.0, MFLAG, &
				"####", MVALUE)

		CASE 7%

	!++
	! Abstract:FLD007
	!	^*(07) Due Date\*
	!	.b
	!	.lm +5
	!	The ^*Due Date\* field enters a vendor's terms
	!	when charges are due on a specified day of the month, as in "N/10th".
	!	A significant value in this field will cause the system to automatically
	!	determine the due date to be the specified day of the month following
	!	the invoice date and enter that date in the purchases journal record.
	!	The calculated date may be overridden in the purchases journal by
	!	entering an alternative date.
	!	.b
	!	This field will contain up to two (2) numeric characters.
	!	.note
	!	The terms specifying when a vendor's invoices are due
	!	should be entered in either this field ^&or\& field (21)
	!	Due Days, but ^&not\&#^&both\&.
	!	.end note
	!
	! Index:
	!	.x Due Date>Terms>Vendor
	!	.x Terms>Due Date>Vendor
	!	.x Vendor>Terms>Due Date
	!
	!--

			UTL_TERMS::DUEDATE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "15;22", TEMP$, &
				UTL_TERMS::DUEDATE, MFLAG, "'E", &
				MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

20300	CASE OPT_TESTENTRY
		UT_MAIN_TERMS = 0%

20500	CASE OPT_SETOLD
		UTL_TERMS_OLD = UTL_TERMS

	CASE OPT_RESETOLD
		UTL_TERMS = UTL_TERMS_OLD

	CASE OPT_SETDEFAULT
		UTL_TERMS2 = UTL_TERMS

	CASE OPT_RESETDEFAULT
		UTL_TERMS = UTL_TERMS2

	CASE OPT_VIEW
		SELECT MLOOP

		CASE 1%
			MVALUE = "Code Description                       " + &
				"       Discount"

		CASE 2%
			MVALUE = "005,046"

		CASE 3%
			MVALUE = &
				UTL_TERMS::CODE + " " + &
				UTL_TERMS::DESCR + "  " + &
				FORMAT$(UTL_TERMS::DISCOUNT, "###.###")

		END SELECT

	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #UTL_TERMS.CH%, &
				KEY #0% GE UTL_TERMS::CODE + "", &
				REGARDLESS

		CASE 1%
			FIND #UTL_TERMS.CH%, &
				KEY #1% GE UTL_TERMS::DESCR + "", &
				REGARDLESS

		END SELECT

	END SELECT

 ExitFunction:
	EXIT FUNCTION

29000	!******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
