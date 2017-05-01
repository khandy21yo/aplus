1	%TITLE "Product Type"
	%SBTTL "PD_MAIN_ACCOUNT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PD_MAIN_ACCOUNT(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
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
	!	The ^*Product Type\* option
	!	establishes a relationship between the product number and the account number
	!	for each product entered in the files.
	!
	! Index:
	!	.x Product Type
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS PD_SOURCE:PD_MAIN_ACCOUNT/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP PD_MAIN_ACCOUNT
	!	$ DELETE PD_MAIN_ACCOUNT.OBJ;*
	!
	! Author:
	!
	!	06/22/88 - Frank Starman
	!
	! Modification history:
	!
	!	03/26/91 - Craig Tanner
	!		Added fields Sales account, COS account, and discount
	!		account.
	!
	!	08/05/91 - Craig Tanner
	!		Added field (06) WIP account.
	!
	!	01/10/92 - Frank F. Starman
	!		Added field (07) Misc Charges
	!
	!	02/04/92 - Kevin Handy
	!		Cleaned out junk (check)
	!
	!	08/28/92 - Dan Perkins
	!		Added field (08) Price Variance account.
	!
	!	06/15/94 - Kevin Handy
	!		Took over UNUSED field for MISCH2ACCT.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	01/27/96 - Kevin Handy
	!		Reformat source code.
	!		Changed STRING$(...,ASCII(" ")) to "" in several places.
	!
	!	08/14/96 - Kevin Handy
	!		Lose extra '&' befire 'end if'.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/01/2000 - Kevin Handy
	!		Use A"x"B
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[PD.OPEN]PD_ACCOUNT.HB"
	MAP (PD_ACCOUNT)	PD_ACCOUNT_CDD		PD_ACCOUNT
	MAP (PD_ACCOUNT_OLD)	PD_ACCOUNT_CDD		PD_ACCOUNT_OLD, &
							PD_ACCOUNT2

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODTYPE.HB"
	MAP (PD_PRODTYPE)	PD_PRODTYPE_CDD		PD_PRODTYPE

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
	COM (CH_PD_ACCOUNT) &
		PD_ACCOUNT.CH%, &
		PD_ACCOUNT.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION FUNC_TESTENTRY

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
		! Define window
		!
		SMG_WINDOW::DESCR = "Product GL Account Table"
		SMG_WINDOW::NHELP = "PD_MAIN_ACCOUNT"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 9%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Type"
			SMG_WINDOW::KFIELD(0%, 0%) = 2%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
			SMG_WINDOW::KFIELD(0%, 2%) = 2%

		SMG_WINDOW::HVIEW = 130%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::VHPOS = 2%
		SMG_WINDOW::VVPOS = 2%

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF PD_ACCOUNT.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PD_ACCOUNT.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PD.OPEN]PD_ACCOUNT.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PD_MAIN_ACCOUNT = ERR
			CONTINUE 770
		END WHEN

		PD_ACCOUNT.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PD.OPEN]PD_ACCOUNT.OPN"
		USE
			PD_MAIN_ACCOUNT = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PD_ACCOUNT.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PD_ACCOUNT.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = PD_ACCOUNT.CH%
		WHEN ERROR IN
			RESET #PD_ACCOUNT.CH%
			GET #PD_ACCOUNT.CH%, REGARDLESS
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


		DATA	02,  03, "(01) Product Type", &
			03,  03, "(02) Location", &
			04,  03, "(03) Invent Acct", &
			05,  03, "(04) COS Acct", &
			06,  03, "(05) Disc Acct", &
			07,  03, "(06) WIP Acct", &
			08,  03, "(07) Misc Acct", &
			09,  03, "(08) Price Var Acct", &
			10,  03, "(09) Misc 2 Acct", &
			0,   0, ""

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

	!++
	! Abstract:FLD001
	!	^*(01) Product Type\*
	!	.LM +5
	!	.B
	!	The ^*Product Type\* field enters a selected
	!	product type which has been established in the Product
	!	Type Table.
	!	.B
	!	Valid product types may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x Product Type
	!	.x Type>Product
	!
	!--

			PD_ACCOUNT::PRODTYPE = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"02;23", TEMP$, PD_ACCOUNT::PRODTYPE, MFLAG, &
				"'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(PD_MAIN_PRODTYPE.ID, "V0") = 1%
				THEN
					PD_ACCOUNT::PRODTYPE = &
						PD_PRODTYPE::CODE
				END IF
				GOTO Reenter
			END IF

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Location\*
	!	.p
	!	The ^*Location\* field enters a code identifying
	!	this location. If the location has been previously defined in
	!	the Company Profile Maintenance record, the name of the location
	!	will automatically appear.
	!	.p
	!	Pressing ^*<List Choices>\* at this field will provide a list
	!	of valid location codes.
	!
	! Index:
	!	.x Location
	!
	!--


			PD_ACCOUNT::LOCATION = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"03;23", TEMP$, &
				PD_ACCOUNT::LOCATION, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "V0") = 1%
				THEN
					PD_ACCOUNT::LOCATION = &
						UTL_LOCATION::LOCATION
				END IF
				GOTO Reenter
			END IF

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Inventory General Ledger Account _#\*
	!	.p
	!	The ^*Inventory General Ledger Account _#\* field
	!	enters a specific Inventory General Ledger account number which
	!	relates to a given product type, and location.
	!	.p
	!	Each product type and location combination, if
	!	desired, may be associated with a specific General Ledger account number,
	!	or with product type only or with location only and then may be associated with
	!	designated General Ledger account numbers.
	!	.p
	!	The General Ledger account number entered must be a valid account
	!	established in the Chart of Accounts. Pressing ^*<List Choices>\*,
	!	at this field, will cause valid choices to be displayed.
	!
	! Index:
	!	.x Inventory Account Number
	!
	!--

			PD_ACCOUNT::INVACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "04;23", TEMP$, &
				PD_ACCOUNT::INVACCT, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%)
				THEN
					PD_ACCOUNT::INVACCT = &
						GL_CHART::ACCT
				END IF
				GOTO Reenter
			END IF

		CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Cost Of Goods Sold General Ledger Account _#\*
	!	.p
	!	The ^*Cost Of Good Sold General Ledger Account _#\* field
	!	enters a specific Cost of Goods Sold General Ledger account number which
	!	relates to a given product type and location.
	!	.p
	!	Each product type and location combination, if desired, may be associated
	!	with a specific General Ledger account number, or with product type only or
	!	with location only and then may be associated with designated General Ledger
	!	Account numbers.
	!	.p
	!	The General Ledger account number entered must be a valid account established
	!	in the Chart of Accounts.  Pressing the ^*<List Choices>\* key will cause
	!	valid choices to be displayed.
	!
	! Index:
	!	.x Cost Of Goods Sold Account Number
	!
	!--

			PD_ACCOUNT::COSACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "05;23", TEMP$, &
				PD_ACCOUNT::COSACCT, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%)
				THEN
					PD_ACCOUNT::COSACCT = &
						GL_CHART::ACCT
				END IF
				GOTO Reenter
			END IF

		CASE 5%
	!++
	! Abstract:FLD005
	!	^*(05) Inventory Discount General Ledger Account _#\*
	!	.p
	!	The ^*Inventory Discount General Ledger Account _#\* field
	!	enters a specific Inventory Discount General Ledger account number which
	!	relates to a given product type and location.
	!	.p
	!	Each product type and location combination, if desired, may be associated
	!	with a specific General Ledger account number, or with product type only or
	!	with location only and then may be associated with designated General Ledger
	!	account numbers.
	!	.p
	!	The General Ledger account number entered must be a valid account established
	!	in the Chart of Accounts.  Pressing ^*<List Choices>\* at this field will cause
	!	valid choices to be displayed.
	!
	! Index:
	!	.x Inventory Discount Account Number
	!
	!--

			PD_ACCOUNT::DISCACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "06;23", TEMP$, &
				PD_ACCOUNT::DISCACCT, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%)
				THEN
					PD_ACCOUNT::DISCACCT = &
						GL_CHART::ACCT
				END IF
				GOTO Reenter
			END IF

		CASE 6%
	!++
	! Abstract:FLD006
	!	^*(06) Work in Process General Ledger Account _#\*
	!	.p
	!	The ^*Work in Process General Ledger Account _#\* field
	!	enters a specific Work in Process General Ledger account number which
	!	relates to a given product type and location.
	!	.p
	!	Each product type and location combination, if desired, may be associated
	!	with a specific General Ledger account number, or with product type only or
	!	with location only and then may be associated with designated General Ledger
	!	account numbers.
	!	.p
	!	The General Ledger account number entered must be a valid account established
	!	in the Chart of Accounts.  Pressing ^*<List Choices>\* at this field will cause
	!	valid choices to be displayed.
	!
	! Index:
	!	.x Work in Process Account Number
	!
	!--

			PD_ACCOUNT::WIPACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "07;23", TEMP$, &
				PD_ACCOUNT::WIPACCT, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%)
				THEN
					PD_ACCOUNT::WIPACCT = &
						GL_CHART::ACCT
				END IF
				GOTO Reenter
			END IF

		CASE 7%
	!++
	! Abstract:FLD007
	!	^*(07) Miscellaneous Charges General Ledger Account _#\*
	!	.p
	!	The ^*Miscellaneous Charges General Ledger Account _#\* field
	!	enters a specific General Ledger account number which
	!	relates to a given charges such as core charges.
	!	.p
	!	The General Ledger account number entered must be a valid account established
	!	in the Chart of Accounts.  Pressing ^*<List Choices>\* at this field will cause
	!	valid choices to be displayed.
	!
	! Index:
	!	.x Miscellaneous Charges Account Number
	!
	!--

			PD_ACCOUNT::MISCHACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "08;23", TEMP$, &
				PD_ACCOUNT::MISCHACCT, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%)
				THEN
					PD_ACCOUNT::MISCHACCT = &
						GL_CHART::ACCT
				END IF
				GOTO Reenter
			END IF

		CASE 8%
	!++
	! Abstract:FLD008
	!	^*(07) Product Price Variance General Ledger Account _#\*
	!	.p
	!	The ^*Product Price Variance General Ledger Account _#\*
	!	field enters a specific General Ledger
	!	account number which relates to a variance in product price.
	!	.p
	!	The General Ledger account number entered must be a valid account established
	!	in the Chart of Accounts.  Pressing ^*<List Choices>\* at this field will cause
	!	valid choices to be displayed.
	!
	! Index:
	!	.x Product Price Variance Account Number
	!
	!--

			PD_ACCOUNT::PRICEVARACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "09;23", TEMP$, &
				PD_ACCOUNT::PRICEVARACCT, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%)
				THEN
					PD_ACCOUNT::PRICEVARACCT = &
						GL_CHART::ACCT
				END IF
				GOTO Reenter
			END IF

		CASE 9%
	!++
	! Abstract:FLD009
	!	^*(09) Miscellaneous Charges (2) General Ledger Account _#\*
	!	.p
	!	The ^*Miscellaneous Charges General Ledger Account _#\* field
	!	enters a specific General Ledger account number which
	!	relates to a given charges such as core charges.
	!	.p
	!	The General Ledger account number entered must be a valid account established
	!	in the Chart of Accounts.  Pressing ^*<List Choices>\* at this field will cause
	!	valid choices to be displayed.
	!
	! Index:
	!	.x Miscellaneous Charges Account Number
	!
	! Index:
	!
	!--

			PD_ACCOUNT::MISCH2ACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "10;23", TEMP$, &
				PD_ACCOUNT::MISCH2ACCT, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%)
				THEN
					PD_ACCOUNT::MISCH2ACCT = &
						GL_CHART::ACCT
				END IF
				GOTO Reenter
			END IF

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

20300	CASE OPT_TESTENTRY
		PD_MAIN_ACCOUNT = 0%

		SELECT MLOOP

		CASE 1%
			PD_MAIN_ACCOUNT = FUNC_TESTENTRY(SMG_WINDOW, &
				PD_ACCOUNT::PRODTYPE, &
				PD_PRODTYPE::DESCRIPTION, &
				"PD", MLOOP, "PRODTYPE", &
				"Product Type", PD_MAIN_PRODTYPE.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_PRODTYPE::DESCRIPTION, 2%, 40%,, SMG$M_BOLD)

		CASE 2%
			PD_MAIN_ACCOUNT = FUNC_TESTENTRY(SMG_WINDOW, &
				PD_ACCOUNT::LOCATION, &
				UTL_LOCATION::LOCNAME, &
				"PD", MLOOP, "PROG", &
				"Location", UTL_MAIN_LOCATION.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_LOCATION::LOCNAME, 3%, 40%,, SMG$M_BOLD)

			IF (MVALUE = "ADD")
			THEN
				WHEN ERROR IN
					GET #SMG_WINDOW::CHAN, &
						KEY #0% EQ PD_ACCOUNT::PRODTYPE + &
						PD_ACCOUNT::LOCATION, &
						REGARDLESS
				USE
					CONTINUE 32767 IF ERR = 155%
					EXIT HANDLER
				END WHEN

				PD_MAIN_ACCOUNT = 2%
				CALL ENTR_3MESSAGE(SCOPE, "Record Already Exists", 0%)
			END IF

		CASE 3%
			IF PD_ACCOUNT::INVACCT <> ""
			THEN
				PD_MAIN_ACCOUNT = FUNC_TESTENTRY(SMG_WINDOW, &
					PD_ACCOUNT::INVACCT, &
					GL_CHART::DESCR, &
					"PD", MLOOP, "PROG", &
					"Inv Account", GL_MAIN_CHART.ID)

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					LEFT$(GL_CHART::DESCR, 35%), 4%, 42%,, &
					SMG$M_BOLD)
			ELSE
				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					SPACE$(35%), 4%, 42%,, SMG$M_BOLD)
			END IF

		CASE 4%
			IF PD_ACCOUNT::COSACCT <> ""
			THEN
				PD_MAIN_ACCOUNT = FUNC_TESTENTRY(SMG_WINDOW, &
					PD_ACCOUNT::COSACCT, &
					GL_CHART::DESCR, &
					"PD", MLOOP, "PROG", &
					"COS Account", GL_MAIN_CHART.ID)

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					LEFT$(GL_CHART::DESCR, 35%), 5%, 42%,, &
					SMG$M_BOLD)
			ELSE
				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					SPACE$(35%), 5%, 42%,, SMG$M_BOLD)
			END IF

		CASE 5%
			IF PD_ACCOUNT::DISCACCT <> ""
			THEN
				PD_MAIN_ACCOUNT = FUNC_TESTENTRY(SMG_WINDOW, &
					PD_ACCOUNT::DISCACCT, &
					GL_CHART::DESCR, &
					"PD", MLOOP, "PROG", &
					"Inv Disc Account", GL_MAIN_CHART.ID)

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					LEFT$(GL_CHART::DESCR, 35%), 6%, 42%,, &
					SMG$M_BOLD)
			ELSE
				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					SPACE$(35%), 6%, 42%,, SMG$M_BOLD)
			END IF

		CASE 6%
			IF PD_ACCOUNT::WIPACCT <> ""
			THEN
				PD_MAIN_ACCOUNT = FUNC_TESTENTRY(SMG_WINDOW, &
					PD_ACCOUNT::WIPACCT, &
					GL_CHART::DESCR, &
					"PD", MLOOP, "PROG", &
					"WIP Account", GL_MAIN_CHART.ID)

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					LEFT$(GL_CHART::DESCR, 35%), 7%, 42%,, &
					SMG$M_BOLD)
			ELSE
				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					SPACE$(35%), 7%, 42%,, SMG$M_BOLD)
			END IF

		CASE 7%
			IF PD_ACCOUNT::MISCHACCT <> ""
			THEN
				PD_MAIN_ACCOUNT = FUNC_TESTENTRY(SMG_WINDOW, &
					PD_ACCOUNT::MISCHACCT, &
					GL_CHART::DESCR, &
					"PD", MLOOP, "PROG", &
					"Misc Account", GL_MAIN_CHART.ID)

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					LEFT$(GL_CHART::DESCR, 35%), 8%, 42%,, &
					SMG$M_BOLD)
			ELSE
				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					SPACE$(35%), 8%, 42%,, SMG$M_BOLD)
			END IF

		CASE 8%
			IF PD_ACCOUNT::PRICEVARACCT <> ""
			THEN
				PD_MAIN_ACCOUNT = FUNC_TESTENTRY(SMG_WINDOW, &
					PD_ACCOUNT::PRICEVARACCT, &
					GL_CHART::DESCR, &
					"PD", MLOOP, "PROG", &
					"Price Variance Account", GL_MAIN_CHART.ID)

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					LEFT$(GL_CHART::DESCR, 35%), 9%, 42%,, &
					SMG$M_BOLD)
			ELSE
				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					SPACE$(35%), 9%, 42%,, SMG$M_BOLD)
			END IF


		CASE 9%
			IF PD_ACCOUNT::MISCH2ACCT <> ""
			THEN
				PD_MAIN_ACCOUNT = FUNC_TESTENTRY(SMG_WINDOW, &
					PD_ACCOUNT::MISCH2ACCT, &
					GL_CHART::DESCR, &
					"PD", MLOOP, "PROG", &
					"Misc Account", GL_MAIN_CHART.ID)

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					LEFT$(GL_CHART::DESCR, 35%), 10%, 42%,, &
					SMG$M_BOLD)
			ELSE
				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					SPACE$(35%), 10%, 42%,, SMG$M_BOLD)
			END IF

		END SELECT

	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(1%) AND 2%) = 0%
		THEN
			!
			! Display description for Product Type
			!
			IF MAIN_WINDOW(PD_MAIN_PRODTYPE.ID, "Q0" + &
				PD_ACCOUNT::PRODTYPE) <> 1%
			THEN
				IF INSTR(1%, PD_ACCOUNT::PRODTYPE, "?") > 0%
				THEN
					PD_PRODTYPE::DESCRIPTION = "Product Type Overlay Mask"
				ELSE
					PD_PRODTYPE::DESCRIPTION = STRING$(LEN( &
						PD_PRODTYPE::DESCRIPTION), A"?"B)
				END IF
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_PRODTYPE::DESCRIPTION, 2%, 40%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(2%) AND 2%) = 0%
		THEN
			IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "Q0" + &
				PD_ACCOUNT::LOCATION) <> 1%
			THEN

				IF INSTR(1%, PD_ACCOUNT::LOCATION, "?") > 0%
				THEN
					UTL_LOCATION::LOCNAME = "Location Overlay Mask"
				ELSE
					UTL_LOCATION::LOCNAME = STRING$(LEN( &
						UTL_LOCATION::LOCNAME), A"?"B)
				END IF

			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_LOCATION::LOCNAME, 3%, 40%, , SMG$M_BOLD)
		END IF

		!
		! Display description for Inventory Account Number
		!
		IF (SMG_WINDOW::HFLAG(3%) AND 2%) = 0%
		THEN
			IF MAIN_WINDOW(GL_MAIN_CHART.ID, "Q0" + &
				PD_ACCOUNT::INVACCT) <> 1%
			THEN
				GL_CHART::DESCR = STRING$(LEN( &
					GL_CHART::DESCR), A"?"B)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT$(GL_CHART::DESCR, 35%), 4%, 42%, , &
				SMG$M_BOLD)
		END IF

		!
		! Display description for Cost of Sale Account Number
		!
		IF (SMG_WINDOW::HFLAG(4%) AND 2%) = 0%
		THEN
			IF MAIN_WINDOW(GL_MAIN_CHART.ID, "Q0" + &
				PD_ACCOUNT::COSACCT) <> 1%
			THEN
				GL_CHART::DESCR = STRING$(LEN( &
					GL_CHART::DESCR), A"?"B)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT$(GL_CHART::DESCR, 35%), 5%, 42%, , &
				SMG$M_BOLD)
		END IF

		!
		! Display description for Discount Account Number
		!
		IF (SMG_WINDOW::HFLAG(5%) AND 2%) = 0%
		THEN
			IF MAIN_WINDOW(GL_MAIN_CHART.ID, "Q0" + &
				PD_ACCOUNT::DISCACCT) <> 1%
			THEN
				GL_CHART::DESCR = STRING$(LEN( &
					GL_CHART::DESCR), A"?"B)

				GL_CHART::DESCR = "" &
					IF PD_ACCOUNT::DISCACCT = ""
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT$(GL_CHART::DESCR, 35%), 6%, 42%, , &
				SMG$M_BOLD)
		END IF

		!
		! Display description for WIP Account Number
		!
		IF (SMG_WINDOW::HFLAG(6%) AND 2%) = 0%
		THEN
			IF MAIN_WINDOW(GL_MAIN_CHART.ID, "Q0" + &
				PD_ACCOUNT::WIPACCT) <> 1%
			THEN
				GL_CHART::DESCR = STRING$(LEN( &
					GL_CHART::DESCR), A"?"B)

				GL_CHART::DESCR = "" &
					IF PD_ACCOUNT::WIPACCT = ""
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT$(GL_CHART::DESCR, 35%), 7%, 42%, , &
				SMG$M_BOLD)
		END IF

		!
		! Display description for Miscellaneous Account Number
		!
		IF (SMG_WINDOW::HFLAG(7%) AND 2%) = 0%
		THEN
			IF MAIN_WINDOW(GL_MAIN_CHART.ID, "Q0" + &
				PD_ACCOUNT::MISCHACCT) <> 1%
			THEN
				GL_CHART::DESCR = STRING$(LEN( &
					GL_CHART::DESCR), A"?"B)

				GL_CHART::DESCR = "" &
					IF PD_ACCOUNT::MISCHACCT = ""
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT$(GL_CHART::DESCR, 35%), 8%, 42%, , &
				SMG$M_BOLD)
		END IF

		!
		! Display description for Product Price Variance Account Number
		!
		IF (SMG_WINDOW::HFLAG(8%) AND 2%) = 0%
		THEN
			IF MAIN_WINDOW(GL_MAIN_CHART.ID, "Q0" + &
				PD_ACCOUNT::PRICEVARACCT) <> 1%
			THEN
				GL_CHART::DESCR = STRING$(LEN( &
					GL_CHART::DESCR), A"?"B)

				GL_CHART::DESCR = "" &
					IF PD_ACCOUNT::PRICEVARACCT = ""
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT$(GL_CHART::DESCR, 35%), 9%, 42%, , &
				SMG$M_BOLD)
		END IF

		!
		! Display description for Miscellaneous Account Number
		!
		IF (SMG_WINDOW::HFLAG(9%) AND 2%) = 0%
		THEN
			IF MAIN_WINDOW(GL_MAIN_CHART.ID, "Q0" + &
				PD_ACCOUNT::MISCH2ACCT) <> 1%
			THEN
				GL_CHART::DESCR = STRING$(LEN( &
					GL_CHART::DESCR), A"?"B)

				GL_CHART::DESCR = "" &
					IF PD_ACCOUNT::MISCH2ACCT = ""
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT$(GL_CHART::DESCR, 35%), 10%, 42%, , &
				SMG$M_BOLD)
		END IF

	!
	! Set PD_ACCOUNT_OLD value
	!
20500	CASE OPT_SETOLD
		PD_ACCOUNT_OLD = PD_ACCOUNT

	!
	! Restore PD_ACCOUNT_OLD value
	!
	CASE OPT_RESETOLD
		PD_ACCOUNT = PD_ACCOUNT_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PD_ACCOUNT2 = PD_ACCOUNT

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PD_ACCOUNT = PD_ACCOUNT2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  PT Loc  InvAcct            "         + &
				"WIPAcct            CosAcct            " + &
				"DiscAcct           MiscAcct           " + &
				"PriceVarAcct"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "005,010,029,048,067,086,105"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = PD_ACCOUNT::PRODTYPE + " " + &
				PD_ACCOUNT::LOCATION + " " + &
				PD_ACCOUNT::INVACCT + " " + &
				PD_ACCOUNT::WIPACCT + " " + &
				PD_ACCOUNT::COSACCT + " " + &
				PD_ACCOUNT::DISCACCT + " " + &
				PD_ACCOUNT::MISCHACCT + " " + &
				PD_ACCOUNT::PRICEVARACCT

		END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP
		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE PD_ACCOUNT::PRODTYPE + &
				PD_ACCOUNT::LOCATION, &
				REGARDLESS

		END SELECT

	END SELECT

	EXIT FUNCTION

29000	!
	! Trap errors
	!
	ON ERROR GO BACK

32767	END FUNCTION
	!+-+-+
	!++
	! Abstract:RECORD
	!	^*Record\*
	!	.p
	!	The ^*Record\* option provides faster access to the
	!	file. It makes it to possible scan or view for all products.
	!
	! Index:
	!	.x Record
	!
	!--
