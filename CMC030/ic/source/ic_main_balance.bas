1	%TITLE "Asset Depreciated Balances"
	%SBTTL "IC_MAIN_BALANCE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG IC_MAIN_BALANCE(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

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
	!	.p
	!
	! Index:
	!	.x Location Balance
	!	.x Archive>Location Balances
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_MAIN_BALANCE/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN IC_MAIN_BALANCE
	!	$ DELETE IC_MAIN_BALANCE.OBJ;*
	!
	! Author:
	!
	!	05/10/88 - Frank F. Starman
	!
	! Modification history:
	!
	!	05/31/88 - Aaron Redd
	!		Modified to allow R/O open of file if R/W open fails.
	!
	!	12/18/90 - Val James Allen
	!		Modified for new file control IC_35BALANCE formats.
	!
	!	04/06/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	04/23/92 - Kevin handy
	!		Clean up (check)
	!
	!	03/31/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	06/11/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/18/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/08/99 - Kevin Handy
	!		Lose goofy parameter to SMG$PUT_CHARS
	!
	!	12/17/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:IC_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[IC.OPEN]IC_35BALANCE.HB"
	MAP	(IC_35BALANCE)		IC_35BALANCE_CDD	IC_35BALANCE
	MAP	(IC_35BALANCE_OLD)	IC_35BALANCE_CDD	IC_35BALANCE_OLD, IC_35BALANCE2

	%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.HB"
	MAP	(IC_CONTROL)		IC_CONTROL_CDD	IC_CONTROL

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP	(PD_PRODUCT)		PD_PRODUCT_CDD	PD_PRODUCT

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP	(UTL_LOCATION)		UTL_LOCATION_CDD UTL_LOCATION

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_TRANSTYPE.HB"
	MAP	(UTL_TRANSTYPE)		UTL_TRANSTYPE_CDD UTL_TRANSTYPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_PERIOD.HB"
	MAP	(UTL_PERIOD)		UTL_PERIOD_CDD	UTL_PERIOD

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_IC_35BALANCE) &
		IC_35BALANCE.CH%, &
		IC_35BALANCE.READONLY%, &
		YYYYPP$

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
		SMG_WINDOW::DESCR = "Inventory Product Balances"
		SMG_WINDOW::NHELP = "IC_MAIN_BALANCE"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 6%

		SMG_WINDOW::NKEYS = 2%
		SMG_WINDOW::KNAME(0%) = "Product_number"
			SMG_WINDOW::KFIELD(0%, 0%) = 3%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
			SMG_WINDOW::KFIELD(0%, 2%) = 2%
			SMG_WINDOW::KFIELD(0%, 3%) = 3%
		SMG_WINDOW::KNAME(1%) = "Location"
			SMG_WINDOW::KFIELD(1%, 0%) = 2%
			SMG_WINDOW::KFIELD(1%, 1%) = 2%
			SMG_WINDOW::KFIELD(1%, 2%) = 1%

		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF IC_35BALANCE.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF IC_35BALANCE.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[IC.OPEN]IC_35BALANCE.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			IC_MAIN_BALANCE = ERR
			CONTINUE 770
		END WHEN

		IC_35BALANCE.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[IC.OPEN]IC_35BALANCE.OPN"
		USE
			IC_MAIN_BALANCE = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		IC_35BALANCE.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(IC_35BALANCE.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = IC_35BALANCE.CH%
		WHEN ERROR IN
			RESET #IC_35BALANCE.CH%
			GET #IC_35BALANCE.CH%, REGARDLESS
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


		DATA	04,08, "(01) Product #", &
			05,08, "(02) Location", &
			07,08, "(03) Trans Type", &
			10,08, "(04) Beginning Balance", &
			13,08, "(05) Posted Balance", &
			14,08, "(06) Running Balance", &
			09,08, "     *Current Period ", &
			09,35, "*", &
			12,08, "     *Current Period and Future Periods*", &
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
 ReEnter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%
		!
		! Display current period now
		!
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				IC_CONTROL::PERIOD, 9%, 30%)
	!++
	! Abstract:FLD001
	!	.ts 55
	!	^*(01) Product _#\*
	!	.b
	!	.lm +5
	!	The ^*Product _#\* field enters a
	!	number which identifies a specific product.
	!	.b
	!	Valid Product _#'s may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x Product Number
	!	.x Number>Product
	!
	!--
			IC_35BALANCE::PRODUCT = ENTR_3STRING(SCOPE,  SMG_WINDOW::WNUMBER, &
				"4;26",TEMP$, IC_35BALANCE::PRODUCT, &
				MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, "VX")=1%
				THEN
					IC_35BALANCE::PRODUCT = &
						PD_PRODUCT::PRODUCT_NUM
				END IF
				GOTO Reenter
			END IF

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Location\*
	!	.b
	!	.lm +5
	!	The ^*Location\* field enters a
	!	location code pertaining to a selected record.
	!	.b
	!	Valid Location codes may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x Location>Location Balances
	!	.x Location Balances>Location
	!
	!--
			IC_35BALANCE::LOCATION = ENTR_3STRING(SCOPE,  SMG_WINDOW::WNUMBER, &
				"5;26",	TEMP$, IC_35BALANCE::LOCATION, &
				MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "V0") = 1%
				THEN
					IC_35BALANCE::LOCATION = &
						UTL_LOCATION::LOCATION
				END IF
				GOTO Reenter
			END IF

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Transaction Type\*
	!	.b
	!	.lm +5
	!	The ^*Transaction Type\* field
	!	enters a code which will identify the type of
	!	transaction.
	!	.b
	!	Valid Transaction Types may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x Transaction>Type
	!
	!--
			IC_35BALANCE::TRANSTYPE = ENTR_3STRING(SCOPE,  SMG_WINDOW::WNUMBER, &
				"07;26",TEMP$, IC_35BALANCE::TRANSTYPE, &
				MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UTL_MAIN_TRANSTYPE.ID, "V1") = 1%
				THEN
					IC_35BALANCE::TRANSTYPE = &
						UTL_TRANSTYPE::CODE
				END IF
				GOTO Reenter
			END IF

		CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Beginning Balance\*
	!	.b
	!	.lm +5
	!	The ^*Beginning Balance\* field enters the
	!	beginning balance quantity value for the record type indicated.
	!	.lm -5
	!
	! Index:
	!	.x Beginning Balance
	!
	!--
			IC_35BALANCE::BBALANCE = ENTR_3NUMBER(SCOPE,  SMG_WINDOW::WNUMBER, &
				"10;36",TEMP$, IC_35BALANCE::BBALANCE, MFLAG, &
				"##,###,###.##", MVALUE)

		CASE 5%
	!++
	! Abstract:FLD005
	!	^*(05) Posted Balance\*
	!	.b
	!	.lm +5
	!	The ^*Posted Balance\* field enters the
	!	current posted quantity value for the record type indicated.
	!	.lm -5
	!
	! Index:
	!	.x Posted Balance
	!
	!--
			IC_35BALANCE::PBALANCE = ENTR_3NUMBER(SCOPE,  SMG_WINDOW::WNUMBER, &
				"13;36",TEMP$, IC_35BALANCE::PBALANCE, MFLAG, &
				"##,###,###.##", MVALUE)

		CASE 6%
	!++
	! Abstract:FLD006
	!	^*(06) Running Balance\*
	!	.b
	!	.lm +5
	!	The ^*Running Balance\* field enters the
	!	current running balance quantity value for the record type indicated.
	!	.lm -5
	!
	! Index:
	!	.x Running Balance
	!
	!--
			IC_35BALANCE::RBALANCE = ENTR_3NUMBER(SCOPE,  SMG_WINDOW::WNUMBER, &
				"14;36",TEMP$, IC_35BALANCE::RBALANCE, MFLAG, &
				"##,###,###.##", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		IC_MAIN_BALANCE = 0%

		SELECT MLOOP

		CASE 1%
			IF IC_35BALANCE::PRODUCT = ""
			THEN
				IC_MAIN_BALANCE = 1%
			ELSE
				!
				! Is the input defined?
				!
				IC_MAIN_BALANCE = FUNC_TESTENTRY(SMG_WINDOW, &
					IC_35BALANCE::PRODUCT, PD_PRODUCT::DESCRIPTION, &
					"IC", MLOOP, "PROD", &
					"Product", PD_MAIN_PRODUCT.ID)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(PD_PRODUCT::DESCRIPTION, 37%), &
				4%, 41%, , SMG$M_BOLD)

		CASE 2%
			IF IC_35BALANCE::LOCATION = ""
			THEN
				IC_MAIN_BALANCE = 1%
			ELSE
				!
				! Is the input defined?
				!
				IC_MAIN_BALANCE = FUNC_TESTENTRY(SMG_WINDOW, &
					IC_35BALANCE::LOCATION, UTL_LOCATION::LOCNAME, &
					"IC", MLOOP, "PROD", &
					"Location", UTL_MAIN_LOCATION.ID)
			END IF

		END SELECT

	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(1%) AND 2%) = 0%
		THEN
			PRODDESC$ = STRING$(40%, 63%)
			PRODDESC$ = PD_PRODUCT::DESCRIPTION &
				IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, &
				"Q0" + IC_35BALANCE::PRODUCT) = 1%
			!
			! Display name (ALSO IN TESTENTRY)
			!
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(PRODDESC$, 37%), &
					4%, 41%, , SMG$M_BOLD)
		END IF

	!
	! Set IC_35BALANCE_OLD value
	!
20500	CASE OPT_SETOLD
		IC_35BALANCE_OLD = IC_35BALANCE

	!
	! Restore IC_35BALANCE_OLD value
	!
	CASE OPT_RESETOLD
		IC_35BALANCE = IC_35BALANCE_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		IC_35BALANCE2 = IC_35BALANCE

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		IC_35BALANCE = IC_35BALANCE2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Product        Loc  TT " + &
				" BeginningBal     PostedBal    RunningBal"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "017,022,025,039,053"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = IC_35BALANCE::PRODUCT + " " + &
				IC_35BALANCE::LOCATION + " " + &
				IC_35BALANCE::TRANSTYPE + " " + &
				FORMAT$(IC_35BALANCE::BBALANCE, &
					"##,###,###.##") + " " + &
				FORMAT$(IC_35BALANCE::PBALANCE, &
					"##,###,###.##") + " " + &
				FORMAT$(IC_35BALANCE::RBALANCE, &
					"##,###,###.##")

		END SELECT

	!
	! Find
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #IC_35BALANCE.CH%, &
				KEY #0% GE IC_35BALANCE::PRODUCT + &
				IC_35BALANCE::LOCATION + &
				IC_35BALANCE::TRANSTYPE, &
				REGARDLESS

		CASE 1%
			FIND #IC_35BALANCE.CH%, &
				KEY #1% GE IC_35BALANCE::LOCATION + &
				IC_35BALANCE::PRODUCT, &
				REGARDLESS
		END SELECT

	END SELECT

27000	EXIT FUNCTION

29000	!****************************************************************
	! Trap errors
	!****************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
	!+-+-+
	!++
	! Abstract:FLD007
	!	^*(07) Running Balance\*
	!	.b
	!	.lm +5
	!	The ^*Running Balance\* field enters the
	!	current running balance quantity value for the record type indicated.
	!	.lm -5
	!
	! Index:
	!	.x Running Balance
	!
	!--
