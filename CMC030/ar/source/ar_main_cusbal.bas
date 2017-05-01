1	%TITLE "Maintain Balance Forward"
	%SBTTL "AR_MAIN_CUSBAL"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AR_MAIN_CUSBAL(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	The ^*Maintain Balance Forward\* file
	!	initializes balances on accounts for each "Balance Forward"
	!	customer when the AR System is installed. Records for "Open
	!	Item" customers are initialized in the ^*ARLED - Maintain Ledger
	!	File\* option.
	!	.lm -5
	!
	! Index:
	!	.x Maintain>Balance Forward
	!	.x Balance Forward>Maintain
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_MAIN_CUSBAL/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP AR_MAIN_CUSBAL
	!	$ DELETE AR_MAIN_CUSBAL.OBJ;*
	!
	! Author:
	!
	!	03/04/88 - Kevin Handy
	!
	! Modification history:
	!
	!	05/16/88 - Aaron Redd
	!		Modified to allow R/O open of file if R/W open fails.
	!
	!	02/22/93 - Dan Perkins
	!		Changed "V0" to "VX" on chart of accounts to be
	!		able to list accounts starting at a particular
	!		account.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	12/04/95 - Kevin Handy
	!		Changed PTDSALES to LAST_PAID.
	!
	!	06/26/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/30/2000 - Kevin Handy
	!		Use A"x"B
	!
	!	04/05/2001 - Kevin Handy
	!		Added OPT_DISPLAY section
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
	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[AR.OPEN]AR_CUSBAL.HB"
	MAP (AR_CUSBAL)		AR_CUSBAL_CDD	AR_CUSBAL
	MAP (AR_CUSBAL_OLD)	AR_CUSBAL_CDD	AR_CUSBAL_OLD, AR_CUSBAL2

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD	AR_35CUSTOM

	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.HB"
	MAP (AR_CONTROL)	AR_CONTROL_CDD	AR_CONTROL

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD	GL_CHART

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_AR_CUSBAL) &
		AR_CUSBAL.CH%, &
		AR_CUSBAL.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION FUNC_TESTENTRY
	EXTERNAL LONG   FUNCTION MAIN_WINDOW

	ON ERROR GOTO 29000

	%PAGE

	!
	! List of types
	!
	SELECT MOPTION

	!*********************************************************************
	! Initialization
	!
	! This option is used to initialize the window structure,
	! set up the default values for add, and open all files
	! necessary that have not already been opened.
	!*********************************************************************
	CASE OPT_INIT

		!
		! Define window
		!
		SMG_WINDOW::DESCR = TRM$(AR_CONTROL::CTITLE) + &
			" Balance Maintenance"
		SMG_WINDOW::NHELP = "AR_MAIN_CUSBAL"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 130%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 16%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = TRM$(AR_CONTROL::CTITLE)
			SMG_WINDOW::KFIELD(0%, 0%) = 2%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
			SMG_WINDOW::KFIELD(0%, 2%) = 2%

		!
		! Load in defaults for cusbal
		!
		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF AR_CUSBAL.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF AR_CUSBAL.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_CUSBAL.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			AR_MAIN_CUSBAL = ERR
			CONTINUE 770
		END WHEN

		AR_CUSBAL.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_CUSBAL.OPN"
		USE
			AR_MAIN_CUSBAL = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		AR_CUSBAL.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(AR_CUSBAL.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN = AR_CUSBAL.CH%
		WHEN ERROR IN
			RESET #AR_CUSBAL.CH%
			GET #AR_CUSBAL.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

20100	!***********************************************************************
	! Display the background
	!
	! This option is used to display the background information on the
	! screen.  It must first clear any junk on the screen, and then
	! write the background onto it.
	!
	!***********************************************************************
	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	1,  1, "(01)", &
			8,  1, "(02) Account", &
			1, 40, "(03)", &
			2, 40, "(04)", &
			3, 40, "(05)", &
			4, 40, "(06)", &
			5, 40, "(07)", &
			7, 40, "(08) Service", &
			8, 40, "(09) Future", &
			12, 40, "(10) Credit Lm", &
			13, 40, "(11) Last charge", &
			14, 40, "(12) Last payment", &
			15, 40, "(13) Last update", &
			16, 40, "(14) YTD Service", &
			17, 40, "(15) Last Paid", &
			18, 40, "(16) YTD Sales", &
			10, 45, "Balance", &
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
		! Print variable titles
		!
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			LEFT(AR_CONTROL::CTITLE, 11%), &
			1%, 6%)


		FOR LOOP% = 0% TO 4%
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(AR_CONTROL::AGENAM(LOOP%), 12%), &
				LOOP% + 1%, 45%)
		NEXT LOOP%


		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

	CASE OPT_DISPLAY
		AR_MAIN_CUSBAL, ST% = FUNC_TESTENTRY(SMG_WINDOW, &
			AR_CUSBAL::CUSNUM, AR_35CUSTOM::CUSNAM, &
			"AR", MLOOP, "CUST", &
			"Customer number", AR_MAIN_35CUSTOM.ID)

		IF ST% <> 0%
		THEN
			AR_35CUSTOM::ADD1 = &
				STRING$(LEN(AR_35CUSTOM::ADD1), A"?"B)

			AR_35CUSTOM::ADD2 = &
				STRING$(LEN(AR_35CUSTOM::ADD2), A"?"B)

			AR_35CUSTOM::CITY = &
				STRING$(LEN(AR_35CUSTOM::CITY), A"?"B)

			AR_35CUSTOM::STATE = &
				STRING$(LEN(AR_35CUSTOM::STATE), A"?"B)

			AR_35CUSTOM::ZIP = &
				STRING$(LEN(AR_35CUSTOM::ZIP), A"?"B)

		END IF

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			LEFT(AR_35CUSTOM::CUSNAM, 30%), &
			2%, 6%, , SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			LEFT(AR_35CUSTOM::ADD1, 30%), &
			3%, 6%, , SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			LEFT(AR_35CUSTOM::ADD2, 30%), &
			4%, 6%, , SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			LEFT(TRM$(AR_35CUSTOM::CITY) + ", " + &
				AR_35CUSTOM::STATE + " " + &
				AR_35CUSTOM::ZIP + SPACE$(30%), 30%), &
			5%, 6%, , SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			FORMAT$(AR_CUSBAL::AGING(0%) + &
				AR_CUSBAL::AGING(1%) + &
				AR_CUSBAL::AGING(2%) + &
				AR_CUSBAL::AGING(3%) + &
				AR_CUSBAL::AGING(4%) + &
				AR_CUSBAL::CHARGE + &
				AR_CUSBAL::FUTURE, &
				"#######.##"), &
			10%, 58%, , SMG$M_BOLD)

20200	!***********************************************************************
	!
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user, display data,
	! set defaults, and return the data back according to MFLAG.
	!
	!***********************************************************************
	CASE OPT_ENTRY

		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

		SCOPE::SCOPE_EXIT = 0%

 Eloop:		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Customer/Client/Patient number\*
	!	.b
	!	.lm +5
	!	The ^*Customer/Client/Patient number\* field refers to the number
	!	assigned to a customer/client/patient, as recorded in the
	!	Master File.
	!	.b
	!	^*Note:\* The title displayed here will change according to the
	!	title defined in the control file.\*
	!	.b
	!	Pressing ^*List Choices\* will provide a list
	!	of valid numbers.
	!	.lm -5
	!
	! Index:
	!	.x Customer Number>Balance Forward
	!	.x Balance Forward>Customer Number
	!	.x Client Number>Balance Forward
	!	.x Balance Forward>Client Number
	!	.x Patient Number>Balance Forward
	!	.x Balance Forward>Patient Number
	!
	!--

			AR_CUSBAL::CUSNUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "1;18", TEMP$, &
				AR_CUSBAL::CUSNUM, MFLAG, "'E", MVALUE)

				IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
				THEN
					IF (MAIN_WINDOW(AR_MAIN_35CUSTOM.ID, "VX  ") = 1%)
					THEN
						AR_CUSBAL::CUSNUM = &
							AR_35CUSTOM::CUSNUM
					END IF
					GOTO ELoop
				END IF

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Account\*
	!	.b
	!	.lm +5
	!	The ^*Account\* field refers to the Accounts Receivable account
	!	number. This number must be established in the General Ledger
	!	Chart of Accounts as well as in the Accounts Receivable Control
	!	file.
	!	.b
	!	Pressing ^*List Choices\* will display a list
	!	of valid account numbers.
	!	.lm -5
	!
	! Index:
	!	.x Account>Customer Balance Maintenance
	!	.x Customer Balance Maintenance>Account
	!
	!--

			AR_CUSBAL::ACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "8;18", TEMP$, &
				AR_CUSBAL::ACCT, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX  ") = 1%)
				THEN
					AR_CUSBAL::ACCT = GL_CHART::ACCT
				END IF
				GOTO ELoop
			END IF

		CASE 3%
		SCOPE::PRG_ITEM = "FLD003AP"

	!++
	! Abstract:FLD003AP
	!	^*(03) - (07) Aging periods\*
	!	.b
	!	.lm +5
	!	The ^*Aging periods\* fields are provided to enter the
	!	aged outstanding balances due from a customer.
	!	.b
	!	^*Note:\* The aging intervals displayed for these fields
	!	are defined in the control file.
	!	.lm -5
	!
	! Index:
	!	.x Aging Periods>Balance Forward
	!	.x Balance Forward>Aging Periods
	!
	!--

			AR_CUSBAL::AGING(0%) = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "1;58", TEMP$, &
				AR_CUSBAL::AGING(0%), MFLAG, "#######.##", &
				MVALUE)

		CASE 4%
			SCOPE::PRG_ITEM = "FLD003AP"

			AR_CUSBAL::AGING(1%) = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "2;58", TEMP$, &
				AR_CUSBAL::AGING(1%), MFLAG, "#######.##", &
				MVALUE)

		CASE 5%
			SCOPE::PRG_ITEM = "FLD003AP"

			AR_CUSBAL::AGING(2%) = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "3;58", TEMP$, &
				AR_CUSBAL::AGING(2%), MFLAG, "#######.##", &
				MVALUE)

		CASE 6%
			SCOPE::PRG_ITEM = "FLD003AP"

			AR_CUSBAL::AGING(3%) = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "4;58", TEMP$, &
				AR_CUSBAL::AGING(3%), MFLAG, "#######.##", &
				MVALUE)

		CASE 7%
			SCOPE::PRG_ITEM = "FLD003AP"

			AR_CUSBAL::AGING(4%) = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "5;58", TEMP$, &
				AR_CUSBAL::AGING(4%), MFLAG, "#######.##", &
				MVALUE)

		CASE 8%

	!++
	! Abstract:FLD008
	!	^*(08) Service charge\*
	!	.b
	!	.lm +5
	!	The ^*Service Charge\* field allows for entry of the
	!	^*current\* amount of service charge which is due on this
	!	account.
	!	.lm -5
	!
	! Index:
	!	.x Service Charge>Balance Forward
	!	.x Balance Forward>Service Charge
	!
	!--

			AR_CUSBAL::CHARGE = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "7;58", TEMP$, &
				AR_CUSBAL::CHARGE, MFLAG, "#######.##", MVALUE)

		CASE 9%

	!++
	! Abstract:FLD009
	!	^*(09) Future\*
	!	.b
	!	.lm +5
	!	The ^*Future\* field enters an amount on
	!	an account which is not due until a date in the future, i.e.
	!	balloon payments. The amount entered here will be included in
	!	the total of the "balance" field.
	!	.lm -5
	!
	! Index:
	!	.x Future>Balance Forward
	!	.x Balance Forward>Future
	!
	!--

			AR_CUSBAL::FUTURE = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "8;58", TEMP$, &
				AR_CUSBAL::FUTURE, MFLAG, "#######.##", MVALUE)

		CASE 10%

	!++
	! Abstract:FLD010
	!	^*(10) Credit Limit\*
	!	.b
	!	.lm +5
	!	The ^*Credit Limit\* field enters the
	!	credit limit which is established for a particular customer.
	!	.b
	!	The field will accommodate a number as large as 999999.99.
	!	.lm -5
	!
	! Index:
	!	.x Credit Limit>Customer Balance Maintenance
	!	.x Customer Balance Maintenance>Credit Limit
	!
	!--

			AR_CUSBAL::CREDIT = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "12;58", TEMP$, &
				AR_CUSBAL::CREDIT, MFLAG, "#######.##", MVALUE)

		CASE 11%

	!++
	! Abstract:FLD011
	!	^*(11) Last Charge\*
	!	.b
	!	.lm +5
	!	The ^*Last Charge\* field enters the date
	!	when a customer last had a charge transaction. This field will
	!	be updated automatically each time an invoice is posted to a
	!	customer's account.
	!	.lm -5
	!
	! Index:
	!	.x Last Charge>Customer Balance Maintenance
	!	.x Customer Balance Maintenance>Last Charge
	!
	!--

			AR_CUSBAL::LAST_CHARGE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, "13;58", TEMP$, &
				AR_CUSBAL::LAST_CHARGE, MFLAG, "'E", MVALUE)

		CASE 12%

	!++
	! Abstract:FLD012
	!	^*(12) Last Payment\*
	!	.b
	!	.lm +5
	!	The ^*Last Payment\* enters the date when a
	!	customer last had a cash receipt transaction. This field will
	!	be automatically updated each time a cash receipt is posted
	!	to a particular customer's account.
	!	.lm -5
	!
	! Index:
	!	.x Last Payment>Customer Balance Maintenance
	!	.x Customer Balance Maintenance>Last Payment
	!
	!--

			AR_CUSBAL::LAST_PAYMENT = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, "14;58", TEMP$, &
				AR_CUSBAL::LAST_PAYMENT, MFLAG, "'E", MVALUE)

		CASE 13%

	!++
	! Abstract:FLD013
	!	^*(13) Last Update\*
	!	.b
	!	.lm +5
	!	The ^*Last Update\* field will automatically reflect the date
	!	an update was last made in the Accounts Receivable Register
	!	file regardless of whether that update had an influence on any
	!	particular customer's account.
	!	.lm -5
	!
	! Index:
	!	.x Last Update>Customer Balance Maintenance
	!	.x Customer Balance Maintenance>Last Update
	!
	!--

			AR_CUSBAL::LAST_UPDATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, "15;58", TEMP$, &
				AR_CUSBAL::LAST_UPDATE, MFLAG, "'E", MVALUE)

		CASE 14%

	!++
	! Abstract:FLD014
	!	^*(14) Year To Date Service Charge\*
	!	.b
	!	.lm +5
	!	The ^*Year to Date Service Charge\* field
	!	enters the year to date service charge which has been
	!	charged to a particular account.
	!	.b
	!	It is important to enter this information if the customer
	!	is to receive a statement at year end showing the total
	!	service charges paid.
	!	.lm -5
	!
	! Index:
	!	.x Year to Date Service Charge>Customer Balance Maintenance
	!	.x Customer Balance Maintenance>Year to Date Service CHarge
	!
	!--

			AR_CUSBAL::YTDSERVICE = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "16;58", TEMP$, &
				AR_CUSBAL::YTDSERVICE, MFLAG, "#######.##", &
				MVALUE)

		CASE 15%

	!++
	! Abstract:FLD015
	!	^*(15) Period To Date Sales\*
	!	.b
	!	.lm +5
	!	The ^*Period to Date Sales\* field enters
	!	the total amount of sales, for a particular customer, during
	!	the accounting period in which the system is initialized.
	!	.lm -5
	!
	! Index:
	!	.x Period to Date Sales>Customer Balance Maintenance
	!	.x Customer Balance Maintenance>Period to Date Sales
	!
	!--

			AR_CUSBAL::LAST_PAID = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"17;58", TEMP$, &
				AR_CUSBAL::LAST_PAID, MFLAG, &
				"#######.##", MVALUE)

		CASE 16%

	!++
	! Abstract:FLD016
	!	^*(16) Year to Date Sales\*
	!	.b
	!	.lm +5
	!	The ^*Year to Date Sales\* field enters
	!	the total amount of sales, for a particular customer, during
	!	the calendar year in which the system is initialized.
	!	.lm -5
	!
	! Index:
	!	.x Year to Date Sales>Customer Balance Maintenance
	!	.x Customer Balance Maintenance>Year to Date Sales
	!
	!--

			AR_CUSBAL::YTDSALES = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "18;58", TEMP$, &
				AR_CUSBAL::YTDSALES, MFLAG, "#######.##", &
				MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

20300	CASE OPT_TESTENTRY
		AR_MAIN_CUSBAL = 0%

		SELECT MLOOP

		CASE 1%
			AR_MAIN_CUSBAL, ST% = FUNC_TESTENTRY(SMG_WINDOW, &
				AR_CUSBAL::CUSNUM, AR_35CUSTOM::CUSNAM, &
				"AR", MLOOP, "CUST", &
				"Customer number", AR_MAIN_35CUSTOM.ID)

			IF ST% <> 0%
			THEN
				AR_35CUSTOM::ADD1 = &
					STRING$(LEN(AR_35CUSTOM::ADD1), A"?"B)

				AR_35CUSTOM::ADD2 = &
					STRING$(LEN(AR_35CUSTOM::ADD2), A"?"B)

				AR_35CUSTOM::CITY = &
					STRING$(LEN(AR_35CUSTOM::CITY), A"?"B)

				AR_35CUSTOM::STATE = &
					STRING$(LEN(AR_35CUSTOM::STATE), A"?"B)

				AR_35CUSTOM::ZIP = &
					STRING$(LEN(AR_35CUSTOM::ZIP), A"?"B)

			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(AR_35CUSTOM::CUSNAM, 30%), &
				2%, 6%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(AR_35CUSTOM::ADD1, 30%), &
				3%, 6%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(AR_35CUSTOM::ADD2, 30%), &
				4%, 6%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(TRM$(AR_35CUSTOM::CITY) + ", " + &
					AR_35CUSTOM::STATE + " " + &
					AR_35CUSTOM::ZIP + SPACE$(30%), 30%), &
				5%, 6%, , SMG$M_BOLD)

		CASE 2%
			AR_MAIN_CUSBAL = FUNC_TESTENTRY(SMG_WINDOW, &
				AR_CUSBAL::ACCT, GL_CHART::DESCR, &
				"AR", MLOOP, "ACCT", &
				"Account number", GL_MAIN_CHART.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(GL_CHART::DESCR, 30%), &
				9%, 6%, , SMG$M_BOLD)

		CASE 3% TO 9%
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				FORMAT$(AR_CUSBAL::AGING(0%) + &
					AR_CUSBAL::AGING(1%) + &
					AR_CUSBAL::AGING(2%) + &
					AR_CUSBAL::AGING(3%) + &
					AR_CUSBAL::AGING(4%) + &
					AR_CUSBAL::CHARGE + &
					AR_CUSBAL::FUTURE, &
					"#######.##"), &
				10%, 58%, , SMG$M_BOLD)

		END SELECT


20500	CASE OPT_SETOLD
		AR_CUSBAL_OLD = AR_CUSBAL

	CASE OPT_RESETOLD
		AR_CUSBAL = AR_CUSBAL_OLD

	CASE OPT_SETDEFAULT
		AR_CUSBAL2 = AR_CUSBAL

	CASE OPT_RESETDEFAULT
		AR_CUSBAL = AR_CUSBAL2

	CASE OPT_VIEW
		SELECT MLOOP

		CASE 1%

			MVALUE = "  Cust #     Account"

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Account\*
	!	.b
	!	.lm +5
	!	The ^*Account\* field refers to the Accounts Receivable account
	!	number. This number must be established in the General Ledger
	!	Chart of Accounts as well as in the Accounts Receivable Control
	!	file.
	!	.b
	!	Pressing ^*List Choices\* will display a list
	!	of valid account numbers.
	!	.lm -5
	!
	! Index:
	!	.x Account>Customer Balance Maintenance
	!	.x Customer Balance Maintenance>Account
	!
	!--

			MVALUE = "013"

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) - (07) Aging periods\*
	!	.b
	!	.lm +5
	!	The ^*Aging periods\* fields are provided to enter the
	!	aged outstanding balances due from a customer.
	!	.b
	!	^*Note:\* The aging intervals displayed for these fields
	!	are defined in the control file.
	!	.lm -5
	!
	! Index:
	!	.x Aging Periods>Balance Forward
	!	.x Balance Forward>Aging Periods
	!
	!--


			MVALUE = &
				AR_CUSBAL::CUSNUM + " " + &
				AR_CUSBAL::ACCT

		END SELECT

	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #AR_CUSBAL.CH%, &
				KEY #0% GE AR_CUSBAL::CUSNUM + AR_CUSBAL::ACCT, &
				REGARDLESS

		END SELECT

	END SELECT

	EXIT FUNCTION

29000	!***************************************************************
	! Error Trapping
	!***************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
