1	%TITLE "Product Account"
	%SBTTL "PD_MAIN_PRODACCT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PD_MAIN_PRODACCT(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	.p
	!	The ^*Product Account Table Report\*
	!	provides a list of Product GL Accounts.
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PD_SOURCE:PD_MAIN_PRODACCT/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP PD_MAIN_PRODACCT
	!	$ DELETE PD_MAIN_PRODACCT.OBJ;*
	!
	! Author:
	!
	!	07/05/90 - Lance Williams
	!
	! Modification history:
	!
	!	04/06/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	11/21/96 - Kevin Handy
	!		use GL_CHART instead of GL_35CHART.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/01/2000 - Kevin Handy
	!		Use A"x"B
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
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODACCT.HB"
	MAP (PD_PRODACCT)	PD_PRODACCT_CDD		PD_PRODACCT
	MAP (PD_PRODACCT_OLD)	PD_PRODACCT_CDD		PD_PRODACCT_OLD, PD_PRODACCT2

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODTYPE.HB"
	MAP (PD_PRODTYPE)	PD_PRODTYPE_CDD		PD_PRODTYPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)	GL_CHART_CDD		GL_CHART

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_PD_PRODACCT) &
		PD_PRODACCT.CH%, &
		PD_PRODACCT.READONLY%

	!
	! External functions
	!
	EXTERNAL	STRING  FUNCTION &
					ENTR_3STRING

	EXTERNAL	LONG    FUNCTION &
					MAIN_WINDOW, &
					FUNC_TESTENTRY
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
		SMG_WINDOW::DESCR	= "Product GL Account"
		SMG_WINDOW::CURREC	= -2%
		SMG_WINDOW::NHELP	= "PD_MAIN_PRODACCT"
		SMG_WINDOW::HSIZE	= 78%
		SMG_WINDOW::VSIZE	= 18%
		SMG_WINDOW::HPOS	= 2%
		SMG_WINDOW::VPOS	= 2%
		SMG_WINDOW::FLAGS	= 0%
		SMG_WINDOW::NITEMS	= 5%
		SMG_WINDOW::HVIEW	= 78%
		SMG_WINDOW::VVIEW	= 18%
		SMG_WINDOW::VHPOS	= 2%
		SMG_WINDOW::VVPOS	= 2%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Location"
			SMG_WINDOW::KFIELD(0%, 0%)	= 1%
			SMG_WINDOW::KFIELD(0%, 1%)	= 1%

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, "QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF PD_PRODACCT.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PD_PRODACCT.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PD.OPEN]PD_PRODACCT.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PD_MAIN_PRODACCT = ERR
			CONTINUE 770
		END WHEN

		PD_PRODACCT.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PD.OPEN]PD_PRODACCT.OPN"
		USE
			PD_MAIN_PRODACCT = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PD_PRODACCT.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PD_PRODACCT.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = PD_PRODACCT.CH%
		WHEN ERROR IN
			RESET #PD_PRODACCT.CH%
			GET #PD_PRODACCT.CH%, REGARDLESS
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

		DATA	02,03, "(01) Product Type", &
			03,03, "(02) Location", &
			04,03, "(03) Inventory Acct", &
			05,03, "(04) Cost Acct", &
			06,03, "(05) Discount Acct", &
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
	!	^*(01) Product Type\*
	!	.p
	!	The ^*Product Type\* field enters a selected
	!	product type which has been established in the Product
	!	Type Table.
	!
	! Index:
	!
	!--

			PD_PRODACCT::PRODTYPE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "02;23", TEMP$, &
				PD_PRODACCT::PRODTYPE, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(PD_MAIN_PRODTYPE.ID, "V0  ") = 1%)
				THEN
					PD_PRODACCT::PRODTYPE = &
						PD_PRODTYPE::CODE
				END IF
				GOTO Reenter
			END IF


		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Location\*
	!	.p
	!	The ^*Location\* field enters a selected
	!	location number which has been established in the Utilities
	!	system.
	!
	! Index:
	!
	!--

			PD_PRODACCT::LOCATION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "03;23", TEMP$, &
				PD_PRODACCT::LOCATION, MFLAG, "'E", &
				MVALUE)


			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "V0") = 1%
				THEN
					PD_PRODACCT::LOCATION = &
						UTL_LOCATION::LOCATION
				END IF
				GOTO Reenter
			END IF


			CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Inventory GL Commission Account #\*
	!	.p
	!	The ^*Inventory GL Commission Account _#\* field
	!	enters a specific General Ledger account number which
	!	relates to a company location and/or a product type.
	!	.p
	!	Each company location and product type combination may, if
	!	desired, be associated with a specific General Ledger account number,
	!	or locations only or product types only may be associated with
	!	designated General Ledger account numbers.
	!	.p
	!	The General Ledger account number entered must be a valid account
	!	established in the Chart of accounts.  Pressing ^*<List Choices>\*,
	!	at this field, will cause valid choices to be displayed.
	!
	! Index:
	!	.x General Ledger>Commission Account Number
	!	.x Product Type account>General Ledger Account Number
	!
	!--

			PD_PRODACCT::INVACC = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "04;23", TEMP$, &
				PD_PRODACCT::INVACC, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "V0  ") = 1%)
				THEN
					PD_PRODACCT::INVACC = GL_CHART::ACCT
				END IF
				GOTO Reenter
			END IF

			CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Cost of Sale GL Account #\*
	!	.p
	!	The ^*Cost of Sale GL Account _#\* field
	!	enters a specific General Ledger account number which
	!	relates to a company location and/or a product type.
	!	.p
	!	Each company location and product type combination may, if
	!	desired, be associated with a specific General Ledger account number,
	!	or locations only or product types only may be associated with
	!	designated General Ledger account numbers.
	!	.p
	!	The General Ledger account number entered must be a valid account
	!	established in the Chart of accounts.  Pressing ^*<List Choices>\*,
	!	at this field, will cause valid choices to be displayed.
	!
	! Index:
	!	.x General Ledger>Cost of Sale Account Number
	!	.x Product Type account>General Ledger Account Number
	!
	!--

			PD_PRODACCT::COSACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "05;23", TEMP$, &
				PD_PRODACCT::COSACCT, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "V0  ") = 1%)
				THEN
					PD_PRODACCT::COSACCT = GL_CHART::ACCT
				END IF
				GOTO Reenter
			END IF

			CASE 5%
	!++
	! Abstract:FLD005
	!	^*(05) Product Discount GL Account #\*
	!	.p
	!	The ^*Product Discount GL Account _#\* field
	!	enters a specific General Ledger account number which
	!	relates to a company location and/or a product type.
	!	.p
	!	Each company location and product type combination may, if
	!	desired, be associated with a specific General Ledger account number,
	!	or locations only or product types only may be associated with
	!	designated General Ledger account numbers.
	!	.p
	!	The General Ledger account number entered must be a valid account
	!	established in the Chart of accounts.  Pressing ^*<List Choices>\*,
	!	at this field, will cause valid choices to be displayed.
	!
	! Index:
	!	.x General Ledger>Product Discount Account Number
	!	.x Product Type account>General Ledger Account Number
	!
	!--

			PD_PRODACCT::DISCACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "06;23", TEMP$, &
				PD_PRODACCT::DISCACCT, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "V0  ") = 1%)
				THEN
					PD_PRODACCT::DISCACCT = GL_CHART::ACCT
				END IF
				GOTO Reenter
			END IF
		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test the data
	!
20300	CASE OPT_TESTENTRY
		PD_MAIN_PRODACCT = 0%

		SELECT MLOOP

		CASE 1%
			!
			! Test PD_PRODACCT
			!
			PD_MAIN_PRODACCT = FUNC_TESTENTRY(SMG_WINDOW, &
				PD_PRODACCT::PRODTYPE, PD_PRODTYPE::DESCRIPTION, &
				"PD", MLOOP, "PROD", &
				"Product", PD_MAIN_PRODTYPE.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_PRODTYPE::DESCRIPTION , 2%, 32%, , SMG$M_BOLD)

		CASE 2%
			!
			! Test UTL_LOCATION
			!
			PD_MAIN_PRODACCT = FUNC_TESTENTRY(SMG_WINDOW, &
				PD_PRODACCT::LOCATION, UTL_LOCATION::LOCNAME, &
				"PD", MLOOP, "PROD", &
				"Location", UTL_MAIN_LOCATION.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_LOCATION::LOCNAME, 3%, 32%, , SMG$M_BOLD)

		CASE 3%
			!
			! Test INVACC
			!
			PD_MAIN_PRODACCT = FUNC_TESTENTRY(SMG_WINDOW, &
				PD_PRODACCT::INVACC, GL_CHART::DESCR, &
				"PD", MLOOP, "PROD", &
				"Account", GL_MAIN_CHART.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 4%, 42%, , SMG$M_BOLD)

		CASE 4%
			!
			! Test COSACCT
			!
			PD_MAIN_PRODACCT = FUNC_TESTENTRY(SMG_WINDOW, &
				PD_PRODACCT::COSACCT, GL_CHART::DESCR, &
				"PD", MLOOP, "PROD", &
				"Account", GL_MAIN_CHART.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 5%, 42%, , SMG$M_BOLD)

		CASE 5%
			!
			! Test DISCACCT
			!
			PD_MAIN_PRODACCT = FUNC_TESTENTRY(SMG_WINDOW, &
				PD_PRODACCT::DISCACCT, GL_CHART::DESCR, &
				"PD", MLOOP, "PROD", &
				"Account", GL_MAIN_CHART.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 6%, 42%, , SMG$M_BOLD)

		END SELECT

	!
	! Display the data
	!
	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(1%) AND 2%) = 0%
		THEN
			PD_PRODTYPE::DESCRIPTION = STRING$(LEN( &
				PD_PRODTYPE::DESCRIPTION), A"?"B) &
				IF MAIN_WINDOW(PD_MAIN_PRODTYPE.ID, "Q0" + &
					PD_PRODACCT::PRODTYPE) <> 1%

			IF INSTR(1%, PD_PRODACCT::PRODTYPE, "?") > 0%
			THEN
				PD_PRODTYPE::DESCRIPTION  = "Product Overlay Mask"
			END IF


			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_PRODTYPE::DESCRIPTION, 2%, 32%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(2%) AND 2%) = 0%
		THEN
			UTL_LOCATION::LOCNAME = STRING$(LEN( &
				UTL_LOCATION::LOCNAME), A"?"B) &
				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "Q0" + &
					PD_PRODACCT::LOCATION) <> 1%

			IF INSTR(1%, PD_PRODACCT::LOCATION, "?") > 0%
			THEN
				UTL_LOCATION::LOCNAME = "Location Overlay Mask"
				FLAG% = 1%
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_LOCATION::LOCNAME, 3%, 32%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(3%) AND 2%) = 0%
		THEN
			GL_CHART::DESCR = STRING$(LEN( &
				GL_CHART::DESCR), A"?"B) &
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, "Q0" + &
					PD_PRODACCT::INVACC) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 4%, 42%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(4%) AND 2%) = 0%
		THEN
			GL_CHART::DESCR = STRING$(LEN( &
				GL_CHART::DESCR), A"?"B) &
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, "Q0" + &
					PD_PRODACCT::COSACCT) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 5%, 42%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(5%) AND 2%) = 0%
		THEN
			GL_CHART::DESCR = STRING$(LEN( &
				GL_CHART::DESCR), A"?"B) &
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, "Q0" + &
					PD_PRODACCT::DISCACCT) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 6%, 42%, , SMG$M_BOLD)
		END IF

	!
	! Set PD_PRODACCT_OLD value
	!
20500	CASE OPT_SETOLD
		PD_PRODACCT_OLD = PD_PRODACCT

	!
	! Restore PD_PRODACCT_OLD value
	!
	CASE OPT_RESETOLD
		PD_PRODACCT = PD_PRODACCT_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PD_PRODACCT2 = PD_PRODACCT

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PD_PRODACCT = PD_PRODACCT2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "ProdType Location  Inventory          CostAcc                 DiscAcc"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "009,018,035,053"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = PD_PRODACCT::PRODTYPE + "     " + &
				PD_PRODACCT::LOCATION + "      " + &
				PD_PRODACCT::INVACC + " " + &
				PD_PRODACCT::COSACCT + "      " + &
				PD_PRODACCT::DISCACCT

		END SELECT

	!
	! Find the next record
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE PD_PRODACCT::LOCATION + PD_PRODACCT::PRODTYPE

		END SELECT

	END SELECT

28000	EXIT FUNCTION

29000	!******************************************************************
	! Trap errors
	!******************************************************************

	!
	! Resume to display untrapped error
	!
	ON ERROR GO BACK

32767	END FUNCTION
