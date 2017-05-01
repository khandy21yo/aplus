1	%TITLE "Maintain Sales Tax Ledger"
	%SBTTL "AR_MAIN_SALTAXLED"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AR_MAIN_SALTAXLED(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1987, 1988 BY
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
	!	The ^*Maintain Sales Tax Ledger\* contains information
	!	relative to each sale transaction and its taxable status.
	!	.lm -5
	!
	! Index:
	!	.x Sales Tax>Maintain
	!	.x Maintain>Sales Tax
	!
	! Option:
	!
	!
	! Input:
	!
	!
	! Output:
	!
	!
	! Example:
	!
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_MAIN_SALTAXLED/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP AR_MAIN_SALTAXLED
	!	$ DELETE AR_MAIN_SALTAXLED.OBJ;*
	!
	! Author:
	!
	!	03/10/88 - Aaron Redd
	!
	! Modification history:
	!
	!	05/16/88 - Aaron Redd
	!		Modified to allow R/O open of file if R/W open fails.
	!
	!	04/15/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	04/22/92 - Kevin Handy
	!		Clean up (check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/20/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/21/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/09/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	!
	! Maps
	!
	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.HB"
	MAP (AR_CONTROL)	AR_CONTROL_CDD		AR_CONTROL

	%INCLUDE "SOURCE:[AR.OPEN]AR_SALTAXLED.HB"
	MAP (AR_SALTAXLED)	AR_SALTAXLED_CDD	AR_SALTAXLED
	MAP (AR_SALTAXLED2) AR_SALTAXLED_CDD	AR_SALTAXLED_OLD, AR_SALTAXLED2

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

	!
	! Common Areas
	!
	! These areas store information that is re-used between calls to
	! these functions.
	!
	COM (TT_AR_SALTAXLEDGER) &
		TAXTITLE$ = 20%, &
		TAXTYPE$(7%) = 40%

	COM (CH_AR_SALTAXLED) &
		AR_SALTAXLED.CH%, &
		AR_SALTAXLED.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION MAIN_WINDOW
	EXTERNAL LONG   FUNCTION FUNC_TESTENTRY

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

		!
		! Define window
		!
		SMG_WINDOW::DESCR = "AR Monthly Sales Tax Ledger"
		SMG_WINDOW::NHELP = "AR_MAIN_SALTAXLED"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 5%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Tax-type"
			SMG_WINDOW::KFIELD(0%, 0%) = 3%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
			SMG_WINDOW::KFIELD(0%, 2%) = 2%
			SMG_WINDOW::KFIELD(0%, 3%) = 3%


		!
		! Load in defaults
		!
		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

		!
		! Tax type
		!
		TAXTITLE$ = "Type   Description"
		TAXTYPE$(0%) = "7"
		TAXTYPE$(1%) = "1    Taxable"
		TAXTYPE$(2%) = "2    Service"
		TAXTYPE$(3%) = "3    Freight"
		TAXTYPE$(4%) = "4    Resale"
		TAXTYPE$(5%) = "5    Out of state"
		TAXTYPE$(6%) = "6    Church, School, and Government"

700		!
		! Declare channels
		!
		IF AR_SALTAXLED.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF AR_SALTAXLED.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_SALTAXLED.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			AR_MAIN_SALTAXLED = ERR
			CONTINUE 770
		END WHEN

		AR_SALTAXLED.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_SALTAXLED.OPN"
		USE
			AR_MAIN_SALTAXLED = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		AR_SALTAXLED.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(AR_SALTAXLED.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = AR_SALTAXLED.CH%
		WHEN ERROR IN
			RESET #AR_SALTAXLED.CH%
			GET #AR_SALTAXLED.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

	!
	! Display the background
	!
	! This option is used to display the background information on the
	! screen.  It must first clear any junk on the screen, and then
	! write the background onto it.
	!
20100	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	5, 1, "(01) Tax Type", &
			7, 1, "(02)", &
			9, 1, "(03) Invoice", &
			11,1, "(04) Amount", &
			13,1, "(05) Date", &
			0,0, ""

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

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			LEFT(AR_CONTROL::CTITLE, 8%), 7%, 6%)

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

	!
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user, display data,
	! set defaults, and return the data back according to MFLAG.
	!
20200	CASE OPT_ENTRY

		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 E0Loop:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Tax Type\*
	!	.b
	!	.lm +5
	!	The ^*Tax Type\* field contains a code to identify
	!	whether a particular sale is subject to sales tax.
	!	.b
	!	Valid Tax Types are:
	!	.table 3,25
	!	.te
	!	^*1\* - Taxable
	!	.te
	!	^*2\* - Service
	!	.te
	!	^*3\* - Freight
	!	.te
	!	^*4\* - Resale
	!	.te
	!	^*5\* - Out of State
	!	.te
	!	^*6\* - Church, School, and Government
	!	.end table
	!	Pressing ^*List Choices\* will display a
	!	list of valid types.
	!	.lm -5
	!
	! Index:
	!	.x Tax Type>Monthly Sales Tax Ledger
	!	.x Monthly Sales Tax Ledger>Tax Type
	!
	!--

			AR_SALTAXLED::TAXTYP = ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, "5;25", TEMP$, &
				AR_SALTAXLED::TAXTYP, MFLAG, "'", MVALUE, &
				TAXTYPE$(), TAXTITLE$, "005")

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Customer\*
	!	.b
	!	.lm +5
	!	The ^*Customer\* field contains the number for a particular
	!	customer.
	!	.b
	!	Pressing ^*List Choices\* will provide a list
	!	of valid numbers.
	!	.lm -5
	!
	! Index:
	!	.x Customer>Monthly Sales Tax Ledger
	!	.x Monthly Sales Tax Ledger>Customer
	!
	!--

			AR_SALTAXLED::CUSNUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "7;25", TEMP$, &
				AR_SALTAXLED::CUSNUM, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(AR_MAIN_35CUSTOM.ID, "V0  ") = 1%)
				THEN
					AR_SALTAXLED::CUSNUM = &
						AR_35CUSTOM::CUSNUM
				END IF
				GOTO E0Loop
			END IF

		CASE 3%

	!++
	! Abstract:FLD003
	!	.ts 55
	!	^*(03) Invoice	8 Characters\*
	!	.b
	!	.lm +5
	!	The ^*Invoice\* field contains the invoice number for which a
	!	sale is recorded.
	!	.lm -5
	!
	! Index:
	!	.x Invoice>Monthly Sales Tax Ledger
	!	.x Monthly Sales Tax Ledger>Invoice
	!
	!--

			AR_SALTAXLED::INVNUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "9;25", TEMP$, &
				AR_SALTAXLED::INVNUM, MFLAG, "'E", MVALUE)

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Amount\*
	!	.b
	!	.lm +5
	!	The ^*Amount\* field contains the amount of the sale that
	!	relates to the tax type code entered in field (01).
	!	.b
	!	The field will contain a number as large as a plus (+) or
	!	a minus (-) 99999.99.
	!	.lm -5
	!
	! Index:
	!	.x Amount>Monthly Sales Tax Ledger
	!	.x Monthly Sales Tax Ledger>Amount
	!
	!--

			AR_SALTAXLED::AMOUNT = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "11;25", TEMP$, &
				AR_SALTAXLED::AMOUNT, MFLAG, "#####.##", MVALUE)
		CASE 5%

	!++
	! Abstract:FLD005
	!	.ts 55
	!	^*(05) Date	MMDDYYYY or MMDDYY\*
	!	.b
	!	.lm +5
	!	The ^*Date\* field contains the date for a particular
	!	invoice.
	!	.lm -5
	!
	! Index:
	!	.x Date>Monthly Sales Tax Ledger
	!	.x Monthly Sales Tax Ledger>Date
	!
	!--

			AR_SALTAXLED::TRADAT = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, "13;25", TEMP$, &
				AR_SALTAXLED::TRADAT, MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY

		AR_MAIN_SALTAXLED = 0%

		SELECT MLOOP

		CASE 1%
			IF (AR_SALTAXLED::TAXTYP = "") AND &
				(AR_SALTAXLED::CUSNUM = "") AND &
				(AR_SALTAXLED::INVNUM = "")
			THEN
				AR_MAIN_SALTAXLED = 1%
			ELSE
				IF (MVALUE = "ADD")
				THEN
					GET #AR_SALTAXLED.CH%, &
						KEY #0% EQ (AR_SALTAXLED::TAXTYP + AR_SALTAXLED::CUSNUM + AR_SALTAXLED::INVNUM = ""), &
						REGARDLESS

					AR_MAIN_SALTAXLED = 2%
					CALL ENTR_3MESSAGE(SCOPE, &
						"Record Already Exists", 1%)
				END IF
			END IF

		CASE 2%
			!
			! Is the input defined?
			!
			AR_MAIN_SALTAXLED = FUNC_TESTENTRY(SMG_WINDOW, &
				AR_SALTAXLED::CUSNUM, AR_35CUSTOM::CUSNAM, &
				"AR", MLOOP, "CUST", &
				"Customer number", AR_MAIN_35CUSTOM.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(AR_35CUSTOM::CUSNAM, 30%), &
				7%, 40%, , SMG$M_BOLD)

		END SELECT

	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(2%) AND 2%) = 0%
		THEN
			!
			! Is the input defined?
			!
			IF MAIN_WINDOW(AR_MAIN_35CUSTOM.ID, &
				"Q0" + AR_SALTAXLED::CUSNUM) <> 1%
			THEN
				AR_35CUSTOM::CUSNAM = "????????????????????"
			END IF

			!
			! Print chart description
			!
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(AR_35CUSTOM::CUSNAM, 30%), &
				7%, 40%, , SMG$M_BOLD)
		END IF

	!
	! Set AR_SALTAXLED_OLD value
	!
20500	CASE OPT_SETOLD

		AR_SALTAXLED_OLD = AR_SALTAXLED

	!
	! Restore AR_SALTAXLED_OLD value
	!
	CASE OPT_RESETOLD

		AR_SALTAXLED = AR_SALTAXLED_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT

		AR_SALTAXLED2 = AR_SALTAXLED

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT

		AR_SALTAXLED = AR_SALTAXLED2

	!
	! View header
	!
	CASE OPT_VIEW

		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "   Type    " + &
				LEFT(AR_CONTROL::CTITLE, 12%) + &
				"Invoice     Amount    Transaction Date"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "009,022,033,044"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = "  " + AR_SALTAXLED::TAXTYP + "     " + &
				AR_SALTAXLED::CUSNUM + "   " + &
				AR_SALTAXLED::INVNUM + "   " + &
				FORMAT$(AR_SALTAXLED::AMOUNT, "#####.##  ") + "   " + &
				PRNT_DATE(AR_SALTAXLED::TRADAT, 8%)

		END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP
		CASE 0%
			FIND #AR_SALTAXLED.CH%, &
				KEY #0% GE AR_SALTAXLED::TAXTYP + &
				AR_SALTAXLED::CUSNUM + &
				AR_SALTAXLED::INVNUM, &
				REGARDLESS
		END SELECT
	END SELECT

 ExitFunction:
	EXIT FUNCTION

29000	!***************************************************************
	! Trap Errors
	!***************************************************************

	RESUME ExitFunction

32767	END FUNCTION
