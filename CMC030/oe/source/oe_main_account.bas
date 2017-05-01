1	%TITLE "Order Entry Account"
	%SBTTL "OE_MAIN_ACCOUNT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG OE_MAIN_ACCOUNT(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	The ^*G/L Account Maintenance\*
	!	associates the G/L distribution
	!	of order information by customer type, sale type and
	!	inventory location.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_MAIN_ACCOUNT/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP OE_MAIN_ACCOUNT
	!	$ DELETE OE_MAIN_ACCOUNT.OBJ;*
	!
	! Author:
	!
	!	07/05/90 - Lance Williams
	!
	! Modification history:
	!
	!	09/17/91 - Dan Perkins
	!		Modified program to use FUNC_TESTENTRY
	!		in test option portion of program.
	!		Force input on field #4, A/R account #.
	!		Allow blank field entry on fields 5 - 7.
	!		Added GL Handling Account to view screen.
	!
	!	04/23/92 - Frank F. Starman
	!		Added Sales and COS accounts.
	!
	!	04/29/92 - Kevin Handy
	!		Clean up (check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	01/26/96 - Kevin Handy
	!		Reformat source code.
	!		Change STRING$(...,ASCII(" ")) to "" in
	!		several places.
	!
	!	08/28/97 - Kevin Handy
	!		Lose unecessary external definitions
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/10/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	11/01/2000 - Kevin Handy
	!		Use A"x"B
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include scope.com
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Include cdd
	!
	%INCLUDE "FUNC_INCLUDE:OE_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[OE.OPEN]OE_ACCOUNT.HB"
	MAP (OE_ACCOUNT)	OE_ACCOUNT_CDD		OE_ACCOUNT
	MAP (OE_ACCOUNT_OLD)	OE_ACCOUNT_CDD		OE_ACCOUNT_OLD, OE_ACCOUNT2

	%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERTYPE.HB"
	MAP (OE_ORDERTYPE)	OE_ORDERTYPE_CDD	OE_ORDERTYPE

	%INCLUDE "SOURCE:[AR.OPEN]AR_CUSTYPE.HB"
	MAP (AR_CUSTYPE)	AR_CUSTYPE_CDD		AR_CUSTYPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD		GL_CHART

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_OE_ACCOUNT) &
		OE_ACCOUNT.CH%, &
		OE_ACCOUNT.READONLY%

	!
	! External functions
	!
	EXTERNAL	LONG    FUNCTION MAIN_WINDOW
	EXTERNAL	LONG	FUNCTION FUNC_TESTENTRY

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
		SMG_WINDOW::DESCR	= "Order Entry Account"
		SMG_WINDOW::CURREC	= -2%
		SMG_WINDOW::NHELP	= "OE_MAIN_ACCOUNT"
		SMG_WINDOW::HSIZE	= 78%
		SMG_WINDOW::VSIZE	= 18%
		SMG_WINDOW::HPOS	= 2%
		SMG_WINDOW::VPOS	= 2%
		SMG_WINDOW::FLAGS	= 0%
		SMG_WINDOW::NITEMS	= 10%
		SMG_WINDOW::HVIEW	= 130%
		SMG_WINDOW::VVIEW	= 18%
		SMG_WINDOW::VHPOS	= 2%
		SMG_WINDOW::VVPOS	= 2%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Customer_type"
			SMG_WINDOW::KFIELD(0%, 0%)	= 1%
			SMG_WINDOW::KFIELD(0%, 1%)	= 1%
			SMG_WINDOW::KFIELD(1%, 1%)	= 1%
			SMG_WINDOW::KFIELD(2%, 1%)	= 1%

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, "QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF OE_ACCOUNT.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF OE_ACCOUNT.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[OE.OPEN]OE_ACCOUNT.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			EXIT HANDLER
		END WHEN

		OE_ACCOUNT.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[OE.OPEN]OE_ACCOUNT.OPN"
		USE
			CONTINUE 770 IF ERR = 10%
			EXIT HANDLER
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		OE_ACCOUNT.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(OE_ACCOUNT.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = OE_ACCOUNT.CH%
		WHEN ERROR IN
			RESET #OE_ACCOUNT.CH%
			GET #OE_ACCOUNT.CH%, REGARDLESS
		USE
			CONTINUE 32767 IF ERR = 11%
			EXIT HANDLER
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

		DATA	02,03, "(01) Cust. Type", &
			03,03, "(02) Sale Type", &
			04,03, "(03) Location ", &
			05,03, "(04) AR Account #", &
			06,03, "(05) Disc Acct #", &
			07,03, "(06) Freight Acct", &
			08,03, "(07) Hand Acct #", &
			09,03, "(08) Sales Acct #", &
			10,03, "(09) COS Acct #", &
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

	!*********************************************************************
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user, display data,
	! set defaults, and return the data back according to MFLAG.
	!*********************************************************************
20200	CASE OPT_ENTRY

		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")
 ReEnter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	.x Accounts Receivable>Customer Type
	!	^*(01) Customer Type\*
	!	.b
	!	.lm +5
	!	The ^*Customer Type\* field enters a selected
	!	customer type which has been established in the Customer Type
	!	file.
	!	.b
	!	The field will accommodate a two character code.
	!	.b
	!	Valid Customer Types may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!
	!--

			OE_ACCOUNT::CUSTTYPE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "02;25", TEMP$, &
				OE_ACCOUNT::CUSTTYPE, MFLAG, "'E", &
				MVALUE)


			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(AR_MAIN_CUSTYPE.ID, "V0") = 1%
				THEN
					OE_ACCOUNT::CUSTTYPE = &
						AR_CUSTYPE::CUSTYPE
				END IF
				GOTO Reenter
			END IF


		CASE 2%

	!++
	! Abstract:FLD002
	!	.x Sale Type
	!	^*(02) Sale Type\*
	!	.b
	!	.lm +5
	!	The ^*Sale Type\* field enters a selected
	!	sale type which has been established in the Sale
	!	Type Table.
	!	.b
	!	The field will accommodate a two character code.
	!	.b
	!	Valid Sale Types may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x Type>Sale
	!
	!--
			OE_ACCOUNT::ORDTYPE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "03;25", TEMP$, &
				OE_ACCOUNT::ORDTYPE, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(OE_MAIN_ORDERTYPE.ID, "V0  ") = 1%)
				THEN
					OE_ACCOUNT::ORDTYPE = &
						OE_ORDERTYPE::ORDTYPE
				END IF
				GOTO Reenter
		END IF

		CASE 3%

	!++
	! Abstract:FLD003
	!	.x Location
	!	^*(03) Location\*
	!	.b
	!	.lm +5
	!	The ^*Location\* field enters a selected
	!	inventory location as identified for a particular inventory
	!	item.
	!	.b
	!	The field will accommodate four characters.
	!	.b
	!	Valid Location codes may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_ACCOUNT::LOCATION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "04;25", TEMP$, &
				OE_ACCOUNT::LOCATION, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "VX") = 1%)
				THEN
					OE_ACCOUNT::LOCATION = &
						UTL_LOCATION::LOCATION
				END IF
				GOTO Reenter
			END IF

		CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Accounts Receivable General Ledger Account Number\*
	!	.b
	!	.lm +5
	!	The ^*Accounts Receivable General Ledger Account Number\* field
	!	enters a specific General Ledger account number which
	!	relates to a given customer type, sale type and location.
	!	.b
	!	Each customer type, sale type and location combination may, if
	!	desired, be associated with a specific General Ledger account number,
	!	or customer type only, sale type only or location only may be associated with
	!	designated General Ledger account numbers.
	!	.b
	!	The General Ledger account number entered must be a valid account
	!	established in the Chart of Accounts.
	!	.b
	!	Valid Chart of Account numbers may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x General Ledger>A/R Account Number
	!	.x A/R G/L Account>General Ledger
	!
	!--

			OE_ACCOUNT::ACCOUNT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "05;21", TEMP$, &
				OE_ACCOUNT::ACCOUNT, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%)
				THEN
					OE_ACCOUNT::ACCOUNT = &
						GL_CHART::ACCT
				END IF
				GOTO Reenter
			END IF

		CASE 5%
	!++
	! Abstract:FLD005
	!	^*(05) Discount General Ledger Account Number\*
	!	.b
	!	.lm +5
	!	The ^*Discount General Ledger Account Number\* field
	!	enters a specific General Ledger account number which
	!	relates to a given customer type, sale type and location.
	!	.b
	!	Each customer type, sale type and location combination may, if
	!	desired, be associated with a specific General Ledger account number,
	!	or customer type only, sale type only or location only may be associated with
	!	designated General Ledger account numbers.
	!	.b
	!	The General Ledger account number entered must be a valid account
	!	established in the Chart of Accounts.
	!	.b
	!	Valid Chart of Account numbers may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x General Ledger>Account Number
	!	.x Disc G/L Account>General Ledger Account Number
	!
	!--

			OE_ACCOUNT::DISACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "06;21", TEMP$, &
				OE_ACCOUNT::DISACCT, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%)
				THEN
					OE_ACCOUNT::DISACCT = &
						GL_CHART::ACCT
				END IF
				GOTO Reenter
			END IF

		CASE 6%
	!++
	! Abstract:FLD006
	!	^*(06) Freight General Ledger Account Number\*
	!	.b
	!	.lm +5
	!	The ^*Freight General Ledger Account Number\* field
	!	enters a specific General Ledger account number which
	!	relates to a given customer type, sale type and location.
	!	.b
	!	Each customer type, sale type and location combination may, if
	!	desired, be associated with a specific General Ledger account number,
	!	or customer type only, sale type only or location only may be associated with
	!	designated General Ledger account numbers.
	!	.b
	!	The General Ledger account number entered must be a valid account
	!	established in the Chart of Accounts.
	!	.b
	!	Valid Chart of Account numbers may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x General Ledger>Account Number
	!	.x Freight G/L Account>General Ledger Account Number
	!
	!--

			OE_ACCOUNT::FRACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "07;21", TEMP$, &
				OE_ACCOUNT::FRACCT, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%)
				THEN
					OE_ACCOUNT::FRACCT = &
						GL_CHART::ACCT
				END IF
				GOTO Reenter
			END IF

		CASE 7%
	!++
	! Abstract:FLD007
	!	^*(07) Handling General Ledger Account Number\*
	!	.b
	!	.lm +5
	!	The ^*Handling General Ledger Account Number\* field
	!	enters a specific General Ledger account number which
	!	relates to a given customer type, sale type and location.
	!	.b
	!	Each customer type, sale type and location combination may, if
	!	desired, be associated with a specific General Ledger account number,
	!	or customer type only, sale type only or location only may be associated with
	!	designated General Ledger account numbers.
	!	.b
	!	The General Ledger account number entered must be a valid account
	!	established in the Chart of Accounts.
	!	.b
	!	Valid Chart of Account numbers may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x General Ledger>Account Number
	!	.x Handling G/L Account>General Ledger Account Number
	!
	!--

			OE_ACCOUNT::HANDLING = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "08;21", TEMP$, &
				OE_ACCOUNT::HANDLING, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%)
				THEN
					OE_ACCOUNT::HANDLING = &
						GL_CHART::ACCT
				END IF
				GOTO Reenter
			END IF

		CASE 8%
	!++
	! Abstract:FLD008
	!	^*(08) Sales Account _#\*
	!	.lm +5
	!	.b
	!	The ^*Sales Account General Ledger Account Number\* field
	!	enters a specific General Ledger account number which
	!	relates to a given customer type, sale type and location.
	!	.b
	!	Each customer type, sale type and location combination may, if desired,
	!	be associated with a specific General Ledger account number, or customer
	!	type only, sale type only or location only may be associated with
	!	designated General Ledger account numbers.
	!	.b
	!	The General Ledger account number entered must be a valid account
	!	established in the Chart of Accounts.
	!	.b
	!	Valid Chart of Account numbers may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!
	!--

			OE_ACCOUNT::SALES= ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "09;21", TEMP$, &
				OE_ACCOUNT::SALES, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%)
				THEN
					OE_ACCOUNT::SALES = &
						GL_CHART::ACCT
				END IF
				GOTO Reenter
			END IF

		CASE 9%
	!++
	! Abstract:FLD009
	!	^*(09) COS Account _#\*
	!	.lm +5
	!	.b
	!	The Cost of Sale General Ledger Account Number field
	!	enters a specific General Ledger account number which relates to a
	!	given customer type, sale type and location.
	!	.b
	!	Each customer type, sale type and location combination may, if desired,
	!	be associated with a specific General Ledger account number, or customer
	!	type only, sale type only or location only may be associated with
	!	designated General Ledger account numbers.
	!	.b
	!	The General Ledger account number entered must be a valid account
	!	established in the Chart of Accounts.
	!	.b
	!	Valid Chart of Account numbers may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!
	!--

			OE_ACCOUNT::COSACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "10;21", TEMP$, &
				OE_ACCOUNT::COSACCT, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%)
				THEN
					OE_ACCOUNT::COSACCT = &
						GL_CHART::ACCT
				END IF
				GOTO Reenter
			END IF

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test the data
	!
20300	CASE OPT_TESTENTRY
		OE_MAIN_ACCOUNT = 0%

		SELECT MLOOP

		CASE 1%
			!
			! Test AR_CUSTYPE
			!
			OE_MAIN_ACCOUNT = FUNC_TESTENTRY(SMG_WINDOW, &
				OE_ACCOUNT::CUSTTYPE, &
				AR_CUSTYPE::DESCRIPTION, &
				"OE", MLOOP, "PROG", &
				"Customer Type", &
				AR_MAIN_CUSTYPE.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AR_CUSTYPE::DESCRIPTION, 2%, 30%, , SMG$M_BOLD)

		CASE 2%
			!
			! Test OE_ORDERTYPE
			!
			OE_MAIN_ACCOUNT = FUNC_TESTENTRY(SMG_WINDOW, &
				OE_ACCOUNT::ORDTYPE, &
				OE_ORDERTYPE::DESCRIPTION, &
				"OE", MLOOP, "PROG", &
				"Sale Type", &
				OE_MAIN_ORDERTYPE.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				OE_ORDERTYPE::DESCRIPTION, 3%, 30%, , SMG$M_BOLD)

		CASE 3%
			!
			! Test UTL_LOCATION
			!
			OE_MAIN_ACCOUNT = FUNC_TESTENTRY(SMG_WINDOW, &
				OE_ACCOUNT::LOCATION, &
				UTL_LOCATION::LOCNAME, &
				"OE", MLOOP, "PROG", &
				"Location", &
				UTL_MAIN_LOCATION.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_LOCATION::LOCNAME, 4%, 30%, , SMG$M_BOLD)

		CASE 4%
			!
			! Test Account
			!
			OE_MAIN_ACCOUNT = FUNC_TESTENTRY(SMG_WINDOW, &
				OE_ACCOUNT::ACCOUNT, &
				GL_CHART::DESCR, &
				"OE", MLOOP, "PROG", &
				"A/R Account #", &
				GL_MAIN_CHART.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 5%, 40%, , SMG$M_BOLD)

		CASE 5%
			IF OE_ACCOUNT::DISACCT = ""
			THEN
				GL_CHART::DESCR = ""
			ELSE
				OE_MAIN_ACCOUNT = FUNC_TESTENTRY(SMG_WINDOW, &
					OE_ACCOUNT::DISACCT, &
					GL_CHART::DESCR, &
					"OE", MLOOP, "PROG", &
					"Discount Account #", &
					GL_MAIN_CHART.ID)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 6%, 40%, , SMG$M_BOLD)

		CASE 6%
			IF OE_ACCOUNT::FRACCT = ""
			THEN
				GL_CHART::DESCR = ""
			ELSE
				OE_MAIN_ACCOUNT = FUNC_TESTENTRY(SMG_WINDOW, &
					OE_ACCOUNT::FRACCT, &
					GL_CHART::DESCR, &
					"OE", MLOOP, "PROG", &
					"Freight Account #", &
					GL_MAIN_CHART.ID)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 7%, 40%, , SMG$M_BOLD)

		CASE 7%
			IF OE_ACCOUNT::HANDLING = ""
			THEN
				GL_CHART::DESCR = ""
			ELSE
				OE_MAIN_ACCOUNT = FUNC_TESTENTRY(SMG_WINDOW, &
					OE_ACCOUNT::HANDLING, &
					GL_CHART::DESCR, &
					"OE", MLOOP, "PROG", &
					"Handling Account #", &
					GL_MAIN_CHART.ID)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 8%, 40%, , SMG$M_BOLD)

		CASE 8%
			IF OE_ACCOUNT::SALES = ""
			THEN
				GL_CHART::DESCR = ""
			ELSE
				OE_MAIN_ACCOUNT = FUNC_TESTENTRY(SMG_WINDOW, &
					OE_ACCOUNT::SALES, &
					GL_CHART::DESCR, &
					"OE", MLOOP, "PROG", &
					"Sales Account #", &
					GL_MAIN_CHART.ID)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 9%, 40%, , SMG$M_BOLD)

		CASE 9%
			IF OE_ACCOUNT::COSACCT = ""
			THEN
				GL_CHART::DESCR = ""
			ELSE
				OE_MAIN_ACCOUNT = FUNC_TESTENTRY(SMG_WINDOW, &
					OE_ACCOUNT::COSACCT, &
					GL_CHART::DESCR, &
					"OE", MLOOP, "PROG", &
					"COS Account #", &
					GL_MAIN_CHART.ID)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 10%, 40%, , SMG$M_BOLD)

		END SELECT

	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(1%) AND 2%) = 0%
		THEN
			!
			! Display description for Customer Type
			!
			AR_CUSTYPE::DESCRIPTION = STRING$(LEN( &
				AR_CUSTYPE::DESCRIPTION), A"?"B) &
				IF MAIN_WINDOW(AR_MAIN_CUSTYPE.ID, "Q0" + &
					OE_ACCOUNT::CUSTTYPE) <> 1%

			IF INSTR(1%, OE_ACCOUNT::CUSTTYPE, "?") > 0%
			THEN
				AR_CUSTYPE::DESCRIPTION = "Customer Type Overlay Mask"
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AR_CUSTYPE::DESCRIPTION, 2%, 30%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(2%) AND 2%) = 0%
		THEN
			!
			! Display description for Sale Type
			!
			OE_ORDERTYPE::DESCRIPTION = STRING$(LEN( &
				OE_ORDERTYPE::DESCRIPTION), A"?"B) &
				IF MAIN_WINDOW(OE_MAIN_ORDERTYPE.ID, "Q0" + &
					OE_ACCOUNT::ORDTYPE) <> 1%

				IF INSTR(1%, OE_ACCOUNT::ORDTYPE, "?") > 0%
				THEN
					OE_ORDERTYPE::DESCRIPTION = "Sale Type Overlay Mask"
				END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				OE_ORDERTYPE::DESCRIPTION, 3%, 30%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(3%) AND 2%) = 0%
		THEN
			UTL_LOCATION::LOCNAME = STRING$(LEN( &
				UTL_LOCATION::LOCNAME), A"?"B) &
				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "Q0" + &
					OE_ACCOUNT::LOCATION) <> 1%

			IF INSTR(1%, OE_ACCOUNT::LOCATION, "?") > 0%
			THEN
				UTL_LOCATION::LOCNAME = "Location Overlay Mask"
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_LOCATION::LOCNAME, 4%, 30%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(4%) AND 2%) = 0%
		THEN
			GL_CHART::DESCR = STRING$(LEN( &
				GL_CHART::DESCR), A"?"B) &
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, "Q0" + &
					OE_ACCOUNT::ACCOUNT) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 5%, 40%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(5%) AND 2%) = 0%
		THEN
			IF OE_ACCOUNT::DISACCT = ""
			THEN
				GL_CHART::DESCR = ""
			ELSE
				GL_CHART::DESCR = STRING$(LEN( &
					GL_CHART::DESCR), A"?"B) &
					IF MAIN_WINDOW(GL_MAIN_CHART.ID, &
					"Q0" + OE_ACCOUNT::DISACCT) <> 1%
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 6%, 40%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(6%) AND 2%) = 0%
		THEN
			IF OE_ACCOUNT::FRACCT = ""
			THEN
				GL_CHART::DESCR = ""
			ELSE
				GL_CHART::DESCR = STRING$(LEN( &
					GL_CHART::DESCR), A"?"B) &
					IF MAIN_WINDOW(GL_MAIN_CHART.ID, &
					"Q0" + OE_ACCOUNT::FRACCT) <> 1%
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 7%, 40%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(7%) AND 2%) = 0%
		THEN
			IF OE_ACCOUNT::HANDLING = ""
			THEN
				GL_CHART::DESCR = ""
			ELSE
				GL_CHART::DESCR = STRING$(LEN( &
					GL_CHART::DESCR), A"?"B) &
					IF MAIN_WINDOW(GL_MAIN_CHART.ID, &
					"Q0" + OE_ACCOUNT::HANDLING) <> 1%
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 8%, 40%, , SMG$M_BOLD)

		IF (SMG_WINDOW::HFLAG(8%) AND 2%) = 0%
		THEN
			IF OE_ACCOUNT::SALES = ""
			THEN
				GL_CHART::DESCR = ""
			ELSE
				GL_CHART::DESCR = STRING$(LEN( &
					GL_CHART::DESCR), A"?"B) &
					IF MAIN_WINDOW(GL_MAIN_CHART.ID, &
					"Q0" + OE_ACCOUNT::SALES) <> 1%
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 9%, 40%, , SMG$M_BOLD)

		IF (SMG_WINDOW::HFLAG(9%) AND 2%) = 0%
		THEN
			IF OE_ACCOUNT::COSACCT = ""
			THEN
				GL_CHART::DESCR = ""
			ELSE
				GL_CHART::DESCR = STRING$(LEN( &
					GL_CHART::DESCR), A"?"B) &
					IF MAIN_WINDOW(GL_MAIN_CHART.ID, &
					"Q0" + OE_ACCOUNT::COSACCT) <> 1%
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 10%, 40%, , SMG$M_BOLD)

		END IF

	!
	! Set OE_ACCOUNT_OLD value
	!
20500	CASE OPT_SETOLD
		OE_ACCOUNT_OLD = OE_ACCOUNT

	!
	! Restore OE_ACCOUNT_OLD value
	!
	CASE OPT_RESETOLD
		OE_ACCOUNT = OE_ACCOUNT_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		OE_ACCOUNT2 = OE_ACCOUNT

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		OE_ACCOUNT = OE_ACCOUNT2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  CT ST Loc  AR_Account         Disc_Account" + &
				"       Freight Account    Handling Account "

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "005,008,013,032,051,070,089,108"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = OE_ACCOUNT::CUSTTYPE + " " + &
				OE_ACCOUNT::ORDTYPE + " " + &
				OE_ACCOUNT::LOCATION + " " + &
				OE_ACCOUNT::ACCOUNT + " " + &
				OE_ACCOUNT::DISACCT + " " + &
				OE_ACCOUNT::FRACCT  + " " + &
				OE_ACCOUNT::HANDLING + " " + &
				OE_ACCOUNT::SALES  + " " + &
				OE_ACCOUNT::COSACCT


		END SELECT

	!
	! Find the next record
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE OE_ACCOUNT::CUSTTYPE + ""

		END SELECT

	END SELECT

 ExitFunction:
	EXIT FUNCTION


29000	!******************************************************************
	! Trap errors
	!******************************************************************

	!
	! Resume to display untrapped error
	!
	ON ERROR GO BACK

32767	END FUNCTION
