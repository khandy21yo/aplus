1	%TITLE "Material Requisition Register Maintenance"
	%SBTTL "WP_MAIN_REQREGISTER"
	%IDENT "V3.6a Calico"

	FUNCTION LONG WP_MAIN_REQREGISTER(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1991 BY
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
	!	.b
	!	.lm +5
	!	The ^*Manufacturing WIP Order Register Maintenance\* option maintains
	!	Orders in the register file.
	!	.lm -5
	!
	! Index:
	!	.x Material Requisition Register Maintenance
	!	.x Maintenance>Material Requisition Register
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_MAIN_REQREGISTER/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN WP_MAIN_REQREGISTER
	!	$ DELETE WP_MAIN_REQREGISTER.OBJ;*
	!
	! Author:
	!
	!	07/22/91 - Craig Tanner
	!
	! Modification history:
	!
	!	08/09/91 - Craig Tanner
	!		Modified so that when program is called by MAIN_WINDOW
	!		for a list of choises, etc., will display only first
	!		key feild.
	!
	!	06/12/92 - Kevin Handy
	!		Clean up (check)
	!
	!	07/22/92 - Dan Perkins
	!		RSET Requisition Number.  Don't pad Job Nuber with
	!		zeros.  Cleaned program code.
	!
	!	06/22/93 - Kevin Handy
	!		Clean up (check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/31/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/20/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:WP_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:JC_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.HB"
	MAP (WP_REQREGISTER)	WP_REQREGISTER_CDD	WP_REQREGISTER
	MAP (WP_REQREGISTER_OLD) WP_REQREGISTER_CDD	WP_REQREGISTER_OLD
	MAP (WP_REQREGISTER_DEF) WP_REQREGISTER_CDD	WP_REQREGISTER_DEF

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[JC.OPEN]JC_JOB.HB"
	MAP (SB_SUBACCOUNT)	JC_JOB_CDD		JC_JOB

	%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.HB"
	MAP (WP_REGLINE)	WP_REGLINE_CDD		WP_REGLINE

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_WP_REQREGISTER) &
		WP_REQREGISTER.READONLY%

	COM (TT_WP_REQREGISTER) &
		TTITLE$ = 30%, &
		TSTAT$(5%) = 30%

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION MAIN_WINDOW
	EXTERNAL LONG    FUNCTION FUNC_TESTENTRY

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	!
	! Initialization
	!
	! This option is used to initialize the window structure,
	! set up the default values for add, and open all files
	! necessary that have not already been opened.
	!
	CASE OPT_INIT

	!******************************************************************
	! Set up information
	!******************************************************************

		!
		! Define SMG_WINDOW
		!
		SMG_WINDOW::DESCR = "Material Requisition Register"
		SMG_WINDOW::NHELP = "WP_MAIN_REQREGISTER"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 15%

		SMG_WINDOW::NKEYS = 4%

		IF MVALUE <> ""
		THEN
			SMG_WINDOW::KNAME(0%) = "Job"
				SMG_WINDOW::KFIELD(0%, 0%) = 1%
				SMG_WINDOW::KFIELD(0%, 1%) = 1%
			SMG_WINDOW::KNAME(1%) = "Req_num"
				SMG_WINDOW::KFIELD(1%, 0%) = 1%
				SMG_WINDOW::KFIELD(1%, 1%) = 3%
			SMG_WINDOW::KNAME(2%) = "Product"
				SMG_WINDOW::KFIELD(2%, 0%) = 1%
				SMG_WINDOW::KFIELD(2%, 1%) = 6%
			SMG_WINDOW::KNAME(3%) = "Batch"
				SMG_WINDOW::KFIELD(3%, 0%) = 1%
				SMG_WINDOW::KFIELD(3%, 1%) = 15%
		ELSE
			SMG_WINDOW::KNAME(0%) = "Job"
				SMG_WINDOW::KFIELD(0%, 0%) = 5%
				SMG_WINDOW::KFIELD(0%, 1%) = 1%
				SMG_WINDOW::KFIELD(0%, 2%) = 2%
				SMG_WINDOW::KFIELD(0%, 3%) = 3%
				SMG_WINDOW::KFIELD(0%, 4%) = 4%
				SMG_WINDOW::KFIELD(0%, 5%) = 5%
			SMG_WINDOW::KNAME(1%) = "Req_num"
				SMG_WINDOW::KFIELD(1%, 0%) = 5%
				SMG_WINDOW::KFIELD(1%, 1%) = 3%
				SMG_WINDOW::KFIELD(1%, 2%) = 4%
				SMG_WINDOW::KFIELD(1%, 3%) = 1%
				SMG_WINDOW::KFIELD(1%, 4%) = 2%
				SMG_WINDOW::KFIELD(1%, 5%) = 5%
			SMG_WINDOW::KNAME(2%) = "Product"
				SMG_WINDOW::KFIELD(2%, 0%) = 5%
				SMG_WINDOW::KFIELD(2%, 1%) = 6%
				SMG_WINDOW::KFIELD(2%, 2%) = 7%
				SMG_WINDOW::KFIELD(2%, 3%) = 1%
				SMG_WINDOW::KFIELD(2%, 4%) = 3%
				SMG_WINDOW::KFIELD(2%, 5%) = 5%
			SMG_WINDOW::KNAME(3%) = "Batch"
				SMG_WINDOW::KFIELD(3%, 0%) = 2%
				SMG_WINDOW::KFIELD(3%, 1%) = 15%
				SMG_WINDOW::KFIELD(3%, 2%) = 1%
		END IF

		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%

		TTITLE$ = "RecType Description"
		TSTAT$(0%) = "3"
		TSTAT$(1%) = "01      Required Amount"
		TSTAT$(2%) = "02      Issued Amount"
		TSTAT$(3%) = "03      Canceled Amount"

		COM (WP_MAIN_REQREGISTER_FRM) FRM$(15%)

		!
		! Read Defaults
		!
		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF WP_REQREGISTER.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if it was
			! that way from the last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF WP_REQREGISTER.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			WP_MAIN_REQREGISTER = ERR
			CONTINUE 770
		END WHEN

		WP_REQREGISTER.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.OPN"
		USE
			WP_MAIN_REQREGISTER = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		WP_REQREGISTER.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(WP_REQREGISTER.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = WP_REQREGISTER.CH%
		WHEN ERROR IN
			RESET #WP_REQREGISTER.CH%
			GET #WP_REQREGISTER.CH%, REGARDLESS
		USE
			CONTINUE 32767 IF ERR = 11%
			EXIT HANDLER
		END WHEN

	!***************************************************************
	! Display the background
	!
	! This option is used to display the background information on the
	! screen.  It must first clear any junk on the screen, and then
	! write the background onto it.
	!***************************************************************
20100	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	03,05, "(01) Job Number", &
			04,05, "(02) Job Line", &
			05,05, "(03) Req Number", &
			06,05, "(04) Req Line", &
			07,05, "(05) Record Type", &
			08,05, "(06) Product", &
			09,05, "(07) Location", &
			10,05, "(08) Quantity", &
			11,05, "(09) Amount", &
			12,05, "(10) Tran Date", &
			13,05, "(11) Operator", &
			14,05, "(12) Period", &
			15,05, "(13) Post Time", &
			16,05, "(14) Post Date", &
			17,05, "(15) Batch", &
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

	!****************************************************************
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user, display data,
	! set defaults, and return the data back according to MFLAG.
	!****************************************************************
20200	CASE OPT_ENTRY

		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 ReEnter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%
	!++
	! Abstract:FLD001
	!	^*(01) Job Number\*
	!	.b
	!	.lm +5
	!	The ^*Job Number\* field
	!	identifies the job to which a record relates.
	!	.lm -5
	!
	! Index:
	!	.x Job Number>Material Requisition Register
	!	.x Material Requisition Register>Job Number
	!
	!--
			WP_REQREGISTER::JOB = ENTR_3STRING(SCOPE,  SMG_WINDOW::WNUMBER, &
				"3;30",TEMP$, WP_REQREGISTER::JOB, &
				MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(JC_MAIN_JOB.ID, "VX") = 1%
				THEN
					WP_REQREGISTER::JOB = &
						JC_JOB::JOB
				END IF
				GOTO ReEnter
			END IF

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Line\*
	!	.b
	!	.lm +5
	!	The ^*Line\* field indicates the
	!	line number to which the record relates.
	!	.lm -5
	!
	! Index:
	!	.x Line>Material Requisition Register
	!	.x Material Requisition Register>Line
	!
	!--
			WP_REQREGISTER::LLINE = ENTR_3STRING(SCOPE,  SMG_WINDOW::WNUMBER, &
				"4;30",	TEMP$, WP_REQREGISTER::LLINE, &
				MFLAG, "~L0'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(WP_MAIN_REGLINE.ID, "V0" + &
						WP_REQREGISTER::JOB) = 1%
				THEN
					WP_REQREGISTER::LLINE = &
						WP_REGLINE::LLINE
				END IF
				GOTO ReEnter
			END IF

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Requisition Number\*
	!	.b
	!	.lm +5
	!	The ^*Requisition Number\* field
	!	indicates the number created at the time a Material Requisition journal was
	!	initiated.
	!	.lm -5
	!
	! Index:
	!	.x Requisition Number>Material Requisition Register
	!	.x Material Requisition Register>Requisition Number
	!
	!--
			WP_REQREGISTER::REQNUM = ENTR_3STRING(SCOPE,  SMG_WINDOW::WNUMBER, &
				"5;30",	TEMP$, WP_REQREGISTER::REQNUM, &
				MFLAG OR 2%, "~R 'E", MVALUE)

		CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Requisition Line\*
	!	.b
	!	.lm +5
	!	The ^*Requisition Line\* field indicates the
	!	line number created when a Material Requisition journal was initiated.
	!	.lm -5
	!
	! Index:
	!	.x Requisition Line>Material Requisition Register
	!	.x Material Requisition Register>Requisition Line
	!
	!--
			WP_REQREGISTER::REQLIN = ENTR_3STRING(SCOPE,  SMG_WINDOW::WNUMBER, &
				"06;30", TEMP$, WP_REQREGISTER::REQLIN, &
				MFLAG, "~L0'E", MVALUE)

		CASE 5%
	!++
	! Abstract:FLD005
	!	^*(05) Record Type\*
	!	.b
	!	.lm +5
	!	The ^*Record Type\* field indicates
	!	the type of transaction to which a record relates.
	!	.b
	!	The following are valid record types:
	!	.table 3,25
	!	.te
	!	^*01\* - Required
	!	.te
	!	^*02\* - Issued
	!	.te
	!	^*03\* - Cancelled
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Record Type>Material Requisition Register
	!	.x Material Requisition Register>Record Type
	!
	!--
			WP_REQREGISTER::RECTYP = EDIT$(ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, "07;30", TEMP$, &
				WP_REQREGISTER::RECTYP, MFLAG, "'E", MVALUE, &
				TSTAT$(), TTITLE$, "008"), -1%)

		CASE 6%
	!++
	! Abstract:FLD006
	!	^*(06) Product\*
	!	.b
	!	.lm +5
	!	The ^*Product\* field enters the product number
	!	contained in the requisition line.
	!	.b
	!	Pressing ^*List Choices\* while the cursor is located at this field
	!	will provide a list of valid products.
	!	.lm -5
	!
	! Index:
	!	.x Product>Material Requisition Register Maintenance Screen
	!
	!--
			WP_REQREGISTER::PRODUCT = ENTR_3STRING(SCOPE,  SMG_WINDOW::WNUMBER, &
				"08;30", TEMP$, WP_REQREGISTER::PRODUCT, &
				MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, "VX") = 1%
				THEN
					WP_REQREGISTER::PRODUCT = &
						PD_PRODUCT::PRODUCT_NUM
				END IF
				GOTO ReEnter
			END IF

		CASE 7%
	!++
	! Abstract:FLD007
	!	^*(07) Location\*
	!	.b
	!	.lm +5
	!	The ^*Location\* field enters the company location to
	!	which the requisition refers.
	!	.b
	!	Valid Location codes may be viewed by pressing ^*List Choices\*.
	!	Additional Location codes may be entered by pressing the F17 key.
	!	.lm -5
	!
	! Index:
	!	.x Location>Material Requisition Register Maintenance Screen
	!
	!--
			WP_REQREGISTER::LOCATION = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"09;30",TEMP$, WP_REQREGISTER::LOCATION, &
				MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "V0") = 1%
				THEN
					WP_REQREGISTER::LOCATION = &
						UTL_LOCATION::LOCATION
				END IF
				GOTO ReEnter
			END IF

		CASE 8%
	!++
	! Abstract:FLD008
	!	^*(08) Quantity\*
	!	.b
	!	.lm +5
	!	The ^*Quantity\* field enters the number of products
	!	requisitioned through the specific requisition record.
	!	.lm -5
	!
	! Index:
	!	.x Quantity>Material Requisition Register Maintenance Screen
	!
	!--
			WP_REQREGISTER::QTY = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"10;30", TEMP$, WP_REQREGISTER::QTY, &
				MFLAG, TRM$(FRM$(MLOOP)), MVALUE)

		CASE 9%
	!++
	! Abstract:FLD009
	!	^*(09) Amount\*
	!	.b
	!	.lm +5
	!	The ^*Amount\* field enters the total amount of the record.
	!	This field is automatically entered from the information in the Product
	!	record and the number of products on the requisition record.
	!	.lm -5
	!
	! Index:
	!	.x Amount>Material Requisition Register Maintenance Screen
	!
	!--
			WP_REQREGISTER::AMT = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"11;30", TEMP$, WP_REQREGISTER::AMT, &
				MFLAG, TRM$(FRM$(MLOOP)), MVALUE)

		CASE 10%
	!++
	! Abstract:FLD010
	!	^*(10) Transaction Date\*
	!	.b
	!	.lm +5
	!	The ^*Transaction Date\* field enters the date the transaction
	!	occurred.  The format for entry is MMDDYY or MMDDYYYY.
	!	.lm -5
	!
	! Index:
	!	.x Transaction Date>Material Requisition Register Maintenance Screen
	!
	!--
			WP_REQREGISTER::TRANDATE = ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"12;30",TEMP$, WP_REQREGISTER::TRANDATE, MFLAG, &
				"'E", MVALUE)

		CASE 11%
	!++
	! Abstract:FLD011
	!	^*(11) Operator\*
	!	.b
	!	.lm +5
	!	The ^*Operator\* field enters
	!	the name or identification of the operator entering the order.
	!	.lm -5
	!
	! Index:
	!	.x Operator>Material Requisition Register Maintenance Screen
	!
	!--
			WP_REQREGISTER::OPERATOR = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"13;30", TEMP$, &
				WP_REQREGISTER::OPERATOR, MFLAG, "'E", MVALUE)

		CASE 12%
	!++
	! Abstract:FLD012
	!	^*(12) Period\*
	!	.b
	!	.lm +5
	!	The ^*Period\* field enters the period when the
	!	transation was posted.
	!	.b
	!	The format for entry is YYYYPP.
	!	.lm -5
	!
	! Index:
	!	.x Period>Material Requisition Register Maintenance Screen
	!
	!--
			WP_REQREGISTER::PERIOD = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"14;30", TEMP$, &
				WP_REQREGISTER::PERIOD, MFLAG, "'E", MVALUE)

		CASE 13%
	!++
	! Abstract:FLD013
	!	^*(13) Post Time\*
	!	.b
	!	.lm +5
	!	The ^*Post Time\* field enters the time of day the
	!	particular transaction was posted.
	!	.lm -5
	!
	! Index:
	!	.x Post Time>Material Requisition Register Maintenance Screen
	!
	!--
			WP_REQREGISTER::POSTTIME = ENTR_3TIME(SCOPE, SMG_WINDOW::WNUMBER, &
				"15;30",TEMP$, WP_REQREGISTER::POSTTIME, MFLAG, &
				"'E", MVALUE)

		CASE 14%
	!++
	! Abstract:FLD014
	!	^*(14) Post Date\*
	!	.b
	!	.lm +5
	!	The ^*Post Date\* field enters the date the
	!	specified transaction was posted.
	!	.b
	!	The format for entry is MMDDYY or MMDDYYYY.
	!	.lm -5
	!
	! Index:
	!	.x Post Date>Material Requisition Register Maintenance Screen
	!
	!--
			WP_REQREGISTER::POSTDATE = ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"16;30",TEMP$, WP_REQREGISTER::POSTDATE, MFLAG, &
				"'E", MVALUE)

		CASE 15%
	!++
	! Abstract:FLD015
	!	^*(15) Batch\*
	!	.b
	!	.lm +5
	!	The ^*Batch\* field enters the number of the batch which is assigned
	!	to the transaction during the posting process.
	!	.lm -5
	!
	! Index:
	!	.x Batch>Material Requisition Register Maintenance Screen
	!
	!--
			WP_REQREGISTER::BATCH = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"17;30",TEMP$, WP_REQREGISTER::BATCH, MFLAG, &
				"'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		WP_MAIN_REQREGISTER = 0%

		SELECT MLOOP

		CASE 1%
			WP_MAIN_REQREGISTER = FUNC_TESTENTRY(SMG_WINDOW, &
				"J" + WP_REQREGISTER::JOB, &
				JC_JOB::DESCR, &
				"WP", MLOOP, "PROG", &
				"Job Number", JC_MAIN_JOB.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				JC_JOB::DESCR, 3%, 45%,, SMG$M_BOLD)

		CASE 2%
			IF WP_REQREGISTER::LLINE = ""
			THEN
				WP_MAIN_REQREGISTER = 1%
			ELSE
				IF MAIN_WINDOW (WP_MAIN_REGLINE.ID, &
					"Q0" + WP_REQREGISTER::JOB + &
					WP_REQREGISTER::LLINE) <> 1%
				THEN
					WP_MAIN_REQREGISTER = 1%
				END IF
			END IF

		CASE 6%
			IF WP_REQREGISTER::PRODUCT <> ""
			THEN
				WP_MAIN_REQREGISTER = FUNC_TESTENTRY(SMG_WINDOW, &
					WP_REQREGISTER::PRODUCT, &
					PD_PRODUCT::DESCRIPTION, &
					"WP", MLOOP, "PROG", &
					"Product", PD_MAIN_PRODUCT.ID)

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					PD_PRODUCT::DESCRIPTION, 8%, 45%,, SMG$M_BOLD)
			ELSE
				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					SPACE$(40%), 8%, 45%,, SMG$M_BOLD)
			END IF

		CASE 7%
			IF WP_REQREGISTER::LOCATION <> ""
			THEN
				WP_MAIN_REQREGISTER = FUNC_TESTENTRY(SMG_WINDOW, &
					WP_REQREGISTER::LOCATION, &
					UTL_LOCATION::LOCNAME, &
					"UTL", MLOOP, "PROG", &
					"Location", UTL_MAIN_LOCATION.ID)

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					UTL_LOCATION::LOCNAME, 9%, 45%,, SMG$M_BOLD)
			ELSE
				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					SPACE$(40%), 9%, 45%,, SMG$M_BOLD)
			END IF

		END SELECT

	!
	! Display descriptions, as in test entry
	!
	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(6%) AND 2%) = 0%
		THEN
			PD_PRODUCT::DESCRIPTION = SPACE$(40%)

			IF WP_REQREGISTER::PRODUCT <> ""
			THEN
				V% = MAIN_WINDOW (PD_MAIN_PRODUCT.ID, "Q0" + &
					WP_REQREGISTER::PRODUCT)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_PRODUCT::DESCRIPTION, 8%, 45%,, SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(7%) AND 2%) = 0%
		THEN
			UTL_LOCATION::LOCNAME = SPACE$(40%)

			IF WP_REQREGISTER::LOCATION <> ""
			THEN
				V% = MAIN_WINDOW (UTL_MAIN_LOCATION.ID, "Q0" + &
					WP_REQREGISTER::LOCATION)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_LOCATION::LOCNAME, 9%, 45%,, SMG$M_BOLD)
		END IF

	!
	! Set WP_REQREGISTER_OLD value
	!
20500	CASE OPT_SETOLD
		WP_REQREGISTER_OLD = WP_REQREGISTER

	!
	! Restore WP_REQREGISTER_OLD value
	!
	CASE OPT_RESETOLD
		WP_REQREGISTER = WP_REQREGISTER_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		WP_REQREGISTER_DEF = WP_REQREGISTER

		IF MFLAG = 1%
		THEN
			SELECT MLOOP
			CASE 0%
				FRM$(8%) = "#,###,###.##"
				FRM$(9%) = "#,###,###.##"
			CASE ELSE
				FRM$(MLOOP) = MVALUE
			END SELECT
		END IF

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		WP_REQREGISTER = WP_REQREGISTER_DEF

	!
	! View header
	!
	CASE OPT_VIEW

		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  JobNumber  Line ReqNumber  ReqLin " + &
				"RecTy Product        Location Batch"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "013,018,029,036,042,057,066"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = WP_REQREGISTER::JOB + " " + &
				WP_REQREGISTER::LLINE + " " + &
				WP_REQREGISTER::REQNUM + " " + &
				WP_REQREGISTER::REQLIN + "   " + &
				WP_REQREGISTER::RECTYP + "    "  + &
				WP_REQREGISTER::PRODUCT + " " + &
				WP_REQREGISTER::LOCATION + "     " + &
				WP_REQREGISTER::BATCH

		END SELECT

	!
	! Find
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE WP_REQREGISTER::JOB + &
					WP_REQREGISTER::LLINE + &
					WP_REQREGISTER::REQNUM + &
					WP_REQREGISTER::REQLIN + &
					WP_REQREGISTER::RECTYP, &
				REGARDLESS

		CASE 1%
			FIND #SMG_WINDOW::CHAN, &
				KEY #1% GE WP_REQREGISTER::REQNUM + &
					WP_REQREGISTER::REQLIN + &
					WP_REQREGISTER::JOB + &
					WP_REQREGISTER::LLINE + &
					WP_REQREGISTER::RECTYP, &
				REGARDLESS

		CASE 2%
			FIND #SMG_WINDOW::CHAN, &
				KEY #2% GE WP_REQREGISTER::PRODUCT + &
					WP_REQREGISTER::LOCATION + &
					WP_REQREGISTER::JOB + &
					WP_REQREGISTER::REQNUM + &
					WP_REQREGISTER::RECTYP, &
				REGARDLESS

		CASE 3%
			FIND #SMG_WINDOW::CHAN, &
				KEY #3% GE WP_REQREGISTER::BATCH + &
					WP_REQREGISTER::JOB, &
				REGARDLESS
		END SELECT

	END SELECT

28000	EXIT FUNCTION

29000	!***************************************************************
	! Trap errors
	!***************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
