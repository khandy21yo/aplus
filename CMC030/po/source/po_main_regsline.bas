1	%TITLE "Maintain Purchase Order Register Subline"
	%SBTTL "PO_MAIN_REGSLINE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PO_MAIN_REGSLINE(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PO_SOURCE:PO_MAIN_REGSLINE/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP PO_MAIN_REGSLINE
	!	$ DELETE PO_MAIN_REGSLINE.OBJ;*
	!
	! Author:
	!
	!	02/20/92 - Dan Perkins
	!
	! Modification history:
	!
	!	02/24/92 - Kevin Handy
	!		Cleaned up (check)
	!
	!	11/25/92 - Dan Perkins
	!		Added CASE 2 to OPT_SUBWIND so that VIEW would
	!		work properly.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/28/97 - Kevin Handy
	!		Lose unecessary external definitions
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/02/2000 - Kevin Handy
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
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PO_WINDOW.INC"

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.HB"
	MAP (PO_REG_LINE) PO_REG_LINE_CDD PO_REG_LINE

	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_SUB_LINE.HB"
	MAP (PO_REG_SUB_LINE)	PO_REG_SUB_LINE_CDD	PO_REG_SUB_LINE
	MAP (PO_REG_SUB_LINE_OLD) PO_REG_SUB_LINE_CDD	PO_REG_SUB_LINE_OLD, PO_REG_SUB_LINE2

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD		GL_CHART

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_PO_REG_LINE) &
		PO_REG_LINE.CH%, &
		PO_REG_LINE.READONLY%

	COM (CH_PO_REG_SUB_LINE) &
		PO_REG_SUB_LINE.CH%, &
		PO_REG_SUB_LINE.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION FUNC_TESTENTRY
	EXTERNAL LONG   FUNCTION MAIN_WINDOW

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 29000

	SELECT MOPTION

	!**********************************************************************
	! Initialization
	!
	! This option is used to initialize the window structure,
	! set up the default values for add, and open all files
	! necessary that have not already been opened.
	!**********************************************************************
	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR  = "Line items"
		SMG_WINDOW::NHELP  = "PO_MAIN_REGSLINE"
		SMG_WINDOW::CURREC = -2%
		SMG_WINDOW::HSIZE  = 76%
		SMG_WINDOW::VSIZE  = 11%
		SMG_WINDOW::HPOS   =  3%
		SMG_WINDOW::VPOS   =  9%
		SMG_WINDOW::NITEMS =  9%
		SMG_WINDOW::FLAGS  =  0%
		SMG_WINDOW::HVIEW  = 76%
		SMG_WINDOW::VVIEW  = 11%
		SMG_WINDOW::VHPOS  =  3%
		SMG_WINDOW::VVPOS  =  9%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%)	= "Action"
			SMG_WINDOW::KFIELD(0%, 0%)	= 1%
			SMG_WINDOW::KFIELD(0%, 1%)	= 1%
		SMG_WINDOW::KNAME(1%)	= "Batch"
			SMG_WINDOW::KFIELD(1%, 0%)	= 1%
			SMG_WINDOW::KFIELD(1%, 1%)	= 9%

		!
		! Load in defaults
		!
		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF PO_REG_SUB_LINE.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PO_REG_SUB_LINE.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PO.OPEN]PO_REG_SUB_LINE.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PO_MAIN_RECLINE = ERR
			CONTINUE 770
		END WHEN

		PO_REG_SUB_LINE.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PO.OPEN]PO_REG_SUB_LINE.OPN"
		USE
			PO_MAIN_RECLINE = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PO_REG_SUB_LINE.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PO_REG_SUB_LINE.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = PO_REG_SUB_LINE.CH%
		WHEN ERROR IN
			RESET #PO_REG_SUB_LINE.CH%
			GET #PO_REG_SUB_LINE.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

	!**********************************************************************
	! Display the background
	!
	! This option is used to display the background information on the
	! screen.  It must first clear any junk on the screen, and then
	! write the background onto it.
	!**********************************************************************
20100	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	2, 2, "(01) Action", &
			3, 2, "(02) Action Date", &
			4, 2, "(03) Quantity", &
			5, 2, "(04) Price", &
			6, 2, "(05) GL Account", &
			7, 2, "(06) Subaccount", &
			8, 2, "(07) Post Date", &
			9, 2, "(08) Post Time", &
			10,2, "(09) Batch", &
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

	!**********************************************************************
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user, display data,
	! set defaults, and return the data back according to MFLAG.
	!**********************************************************************
20200	CASE OPT_ENTRY

		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 E0Loop:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%
	!++
	! Abstract:FLD001
	!	^*(01) Action\*
	!	.p
	!	The ^*Action\* field is used to identify the type of purchase order action
	!	resulting in this transaction.
	!	.p
	!	Valid actions are:
	!	.table 5,30
	!	.te
	!	^*01\*	Ordered
	!	.te
	!	^*02\*	Received
	!	.te
	!	^*03\*	Cancled
	!	.te
	!	^*09\*	Invoiced
	!	.end table
	!	.p
	!	This field will accept up to four (2) characters.
	!
	! Index:
	!
	!--
			PO_REG_SUB_LINE::PO_ACTION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "2;20", TEMP$, &
				PO_REG_SUB_LINE::PO_ACTION, MFLAG, "'E", MVALUE)

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Action Date\*
	!	.p
	!	The ^*Action Date\* field enters the date on which the
	!	transaction occurred.
	!	.p
	!	The format for this field is MMDDYYYY.
	!
	! Index:
	!
	!--
			PO_REG_SUB_LINE::ACTION_DATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, "3;20", TEMP$, &
				PO_REG_SUB_LINE::ACTION_DATE, MFLAG, "8", MVALUE)

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Item Quantity\*
	!	.p
	!	The ^*Item Quantity\* field enters the
	!	item quantity of the transaction.
	!	.p
	!	This field will accept a number as large as 9,999,999.99
	!
	! Index:
	!
	!--
			PO_REG_SUB_LINE::QTY = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "4;20", TEMP$, &
				PO_REG_SUB_LINE::QTY, &
				MFLAG, "#######.##", MVALUE)

		CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Price\*
	!	.p
	!	The ^*Price\* field enters the
	!	price of the items in the transaction.
	!	.p
	!	This field will accept a number as large as 9,999,999.99
	!
	! Index:
	!
	!--
			PO_REG_SUB_LINE::PRICE = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "5;20", TEMP$, &
				PO_REG_SUB_LINE::PRICE, &
				MFLAG, "#######.##", MVALUE)

		CASE 5%
	!++
	! Abstract:FLD005
	!	^*(05) GL Account\*
	!	.p
	!	The ^*General Ledger Account Number\* field contains the General Ledger
	!	Account that will be effected by this purchase order line.
	!	.p
	!	Pressing the ^*List Choices\* key while the cursor is positioned on
	!	this field will cause the defined GL Account Numbers to be displayed.
	!	.p
	!	This field will accommodate up to eighteen (18) alphanumeric
	!	characters.
	!
	! Index:
	!
	!--
			PO_REG_SUB_LINE::ACCOUNT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "6;20", TEMP$, &
				PO_REG_SUB_LINE::ACCOUNT, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%
				THEN
					PO_REG_SUB_LINE::ACCOUNT = &
						GL_CHART::ACCT
				END IF
				GOTO E0Loop
			END IF

		CASE 6%
	!++
	! Abstract:FLD006
	!	^*(01) Subaccount\*
	!	.p
	!	The ^*Subaccount\* field contains the subaccount or reference
	!	to which the ordered items can be associated, i.e. a job number,
	!	or a client order number, etc.
	!	.p
	!	This field will accomodate up to ten (10) alphanumeric
	!	characters.
	!
	! Index:
	!
	!--
			PO_REG_SUB_LINE::SUBACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "7;20", TEMP$, &
				PO_REG_SUB_LINE::SUBACCT, MFLAG, "'E", MVALUE)

		CASE 7%
	!++
	! Abstract:FLD007
	!	^*(08) Post Date\*
	!	.p
	!	The Post Date field is to be entered to tell the user when
	!	this particular item was posted to the REGISTER subline.
	!
	! Index:
	!
	!--
			PO_REG_SUB_LINE::POSTDATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, "8;20",TEMP$, &
				PO_REG_SUB_LINE::POSTDATE, MFLAG, "'E", MVALUE)

		CASE 8%
	!++
	! Abstract:FLD008
	!
	!	^*(08) Post Time\*
	!	.p
	!	The Post Time field is to be entered to tell the user what time
	!	this particular transaction was posted to the REGISTER subline.
	!
	! Index:
	!
	!--
			PO_REG_SUB_LINE::POSTTIME = ENTR_3TIME(SCOPE, &
				SMG_WINDOW::WNUMBER, "9;20",TEMP$, &
				PO_REG_SUB_LINE::POSTTIME, MFLAG, "'E", MVALUE)

		CASE 9%
	!++
	! Abstract:FLD009
	!	^*(09) Batch\*
	!	.p
	!	The Batch field is to be entered to tell the batch number of
	!	the transaction posted to the REGISTER subline.
	!
	! Index:
	!
	!--
			PO_REG_SUB_LINE::BATCH = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "10;20",TEMP$, &
				PO_REG_SUB_LINE::BATCH, MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY

		SELECT MLOOP

		CASE 5%
			!
			! Is the input defined?
			!
			PO_MAIN_REGSLINE = FUNC_TESTENTRY(SMG_WINDOW, &
				PO_REG_SUB_LINE::ACCOUNT, GL_CHART::DESCR, &
				"PO", MLOOP, "ACCOUNT", &
				"GL Account  number", GL_MAIN_CHART.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 6%, 40%,, SMG$M_BOLD)

		END SELECT

	!
	! Display additional information
	!
	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(5%) AND 2%) = 0%
		THEN
			GL_CHART::DESCR = &
				STRING$(LEN(GL_CHART::DESCR), A"?") &
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, "Q0" + &
					PO_REG_SUB_LINE::ACCOUNT) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 6%, 40%,, SMG$M_BOLD)

		END IF

	!
	! Set PO_REG_SUB_LINE_OLD value
	!
20500	CASE OPT_SETOLD
		PO_REG_SUB_LINE_OLD = PO_REG_SUB_LINE

	!
	! Restore PO_REG_SUB_LINE_OLD value
	!
	CASE OPT_RESETOLD
		PO_REG_SUB_LINE = PO_REG_SUB_LINE_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PO_REG_SUB_LINE2 = PO_REG_SUB_LINE

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PO_REG_SUB_LINE = PO_REG_SUB_LINE2

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!
		PO_REG_SUB_LINE::PO = &
			LEFT(MVALUE, LEN(PO_REG_SUB_LINE::PO))

		PO_REG_SUB_LINE::PO_LINE = &
			RIGHT(MVALUE, LEN(PO_REG_SUB_LINE::PO) + 1%)

	!
	! Find
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #PO_REG_SUB_LINE.CH%, &
				KEY #0% GE PO_REG_SUB_LINE::PO + &
				PO_REG_SUB_LINE::PO_LINE + &
				PO_REG_SUB_LINE::PO_ACTION, REGARDLESS

		CASE 1%
			FIND #PO_REG_SUB_LINE.CH%, &
				KEY #1% GE PO_REG_SUB_LINE::BATCH + &
				PO_REG_SUB_LINE::PO + &
				PO_REG_SUB_LINE::PO_LINE, REGARDLESS

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
			MVALUE = "  Act Date              Qty        Price " + &
				"GL_Account      Subaccount Batch"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "006,015,028,041,057,068"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = PO_REG_SUB_LINE::PO_ACTION                     + "  " + &
				PRNT_DATE(PO_REG_SUB_LINE::ACTION_DATE, 6%)     + " "  + &
				FORMAT$(PO_REG_SUB_LINE::QTY, "#,###,###.##")   + " "  + &
				FORMAT$(PO_REG_SUB_LINE::PRICE, "#,###,###.##") + " "  + &
				LEFT(PO_REG_SUB_LINE::ACCOUNT, 15%)             + " "  + &
				PO_REG_SUB_LINE::SUBACCT                        + " "  + &
				PO_REG_SUB_LINE::BATCH

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
					FIND #PO_REG_SUB_LINE.CH%, &
						KEY #0% GE MVALUE + &
						PO_REG_SUB_LINE::PO_ACTION, REGARDLESS
				USE
					CONTINUE 28000
				END WHEN

			CASE 1%
				WHEN ERROR IN
					FIND #PO_REG_SUB_LINE.CH%, &
						KEY #1% GE PO_REG_SUB_LINE::BATCH + &
						MVALUE, REGARDLESS
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

			IF PO_REG_SUB_LINE::PO + &
				PO_REG_SUB_LINE::PO_LINE = MVALUE
			THEN
				SMG_WINDOW::CURREC = 0%
			END IF

		!
		! Change key
		!
		CASE 6%
			PO_REG_SUB_LINE::PO = &
				LEFT(MVALUE, LEN(PO_REG_SUB_LINE::PO))

			PO_REG_SUB_LINE::PO_LINE = &
				RIGHT(MVALUE, LEN(PO_REG_SUB_LINE::PO) + 1%)

		END SELECT

	END SELECT

28000	EXIT FUNCTION

	%PAGE

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
