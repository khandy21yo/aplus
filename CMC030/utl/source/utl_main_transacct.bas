1	%TITLE "Transaction Account"
	%SBTTL "UTL_MAIN_TRANSACCT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG UTL_MAIN_TRANSACCT(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	The ^*Transaction Account\* table defines the inventory transaction General
	!	Ledger accounts.
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UTL_MAIN_TRANSACCT/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP UTL_MAIN_TRANSACCT
	!	$ DELETE UTL_MAIN_TRANSACCT.OBJ;*
	!
	! Author:
	!
	!	07/05/90 - Lance Williams
	!
	! Modification history:
	!
	!	05/13/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	06/12/92 - Kevin Handy
	!		Clean up (check)
	!
	!	03/02/93 - Dan Perkins
	!		Changed "V0" to "VX" on chart of accounts to be
	!		able to list accounts starting at a particular
	!		account.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/28/97 - Kevin Handy
	!		Lose unecessary external definitions
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/06/2000 - Kevin Handy
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
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_TRANSACCT.HB"
	MAP (UTL_TRANSACCT)	UTL_TRANSACCT_CDD	UTL_TRANSACCT
	MAP (UTL_TRANSACCT_OLD) UTL_TRANSACCT_CDD	UTL_TRANSACCT_OLD, &
		UTL_TRANSACCT2

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_TRANSTYPE.HB"
	MAP (UTL_TRANSTYPE)	UTL_TRANSTYPE_CDD	UTL_TRANSTYPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD		GL_CHART

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODTYPE.HB"
	MAP (PD_PRODTYPE)	PD_PRODTYPE_CDD		PD_PRODTYPE

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_UTL_TRANSACCT) &
		UTL_TRANSACCT.CH%, &
		UTL_TRANSACCT.READONLY%

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
	! necesUTLry that have not already been opened.
	!
	CASE OPT_INIT

		!**************************************************************
		! Set up information
		!**************************************************************
		!
		! Define SMG_WINDOW
		!
		SMG_WINDOW::DESCR	= "Transaction Type GL Accounts"
		SMG_WINDOW::NHELP	= "UTL_MAIN_TRANSACCT"
		SMG_WINDOW::HSIZE	= 78%
		SMG_WINDOW::VSIZE	= 18%
		SMG_WINDOW::HPOS	= 2%
		SMG_WINDOW::VPOS	= 2%
		SMG_WINDOW::FLAGS	= 0%
		SMG_WINDOW::NITEMS	= 4%
		SMG_WINDOW::HVIEW	= 78%
		SMG_WINDOW::VVIEW	= 18%
		SMG_WINDOW::VHPOS	= 2%
		SMG_WINDOW::VVPOS	= 2%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Trans_type"
			SMG_WINDOW::KFIELD(0%, 0%)	= 1%
			SMG_WINDOW::KFIELD(0%, 1%)	= 1%
			SMG_WINDOW::KFIELD(0%, 2%)	= 2%

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, "QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF UTL_TRANSACCT.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF UTL_TRANSACCT.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_TRANSACCT.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			UTL_MAIN_TRANSACCT = ERR
			CONTINUE 770
		END WHEN

		UTL_TRANSACCT.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_TRANSACCT.OPN"
		USE
			UTL_MAIN_TRANSACCT = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		UTL_TRANSACCT.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(UTL_TRANSACCT.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = UTL_TRANSACCT.CH%
		WHEN ERROR IN
			RESET #UTL_TRANSACCT.CH%
			GET #UTL_TRANSACCT.CH%, REGARDLESS
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

		DATA	02,03, "(01) Trans. Type", &
			03,03, "(02) Product Type", &
			04,03, "(03) Location", &
			05,03, "(04) Acct. #", &
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
	!	^*(01) Transaction Type\*
	!	.p
	!	The ^*Transaction Type\* field enters a selected
	!	transaction type which has been established in the Transaction
	!	Type Table.
	!
	! Index:
	!
	!--
			UTL_TRANSACCT::TRANSTYPE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "02;23", TEMP$, &
				UTL_TRANSACCT::TRANSTYPE, MFLAG, "'E", &
				MVALUE)


			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UTL_MAIN_TRANSTYPE.ID, "V0") = 1%
				THEN
					UTL_TRANSACCT::TRANSTYPE = &
						UTL_TRANSTYPE::CODE
				END IF
				GOTO Reenter
			END IF


			CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Product Type\*
	!	.p
	!	The ^*Product Type\* field enters a selected
	!	product type which has been established in the Product
	!	Type Table.
	!
	! Index:
	!
	!--

			UTL_TRANSACCT::PRODTYPE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "03;23", TEMP$, &
				UTL_TRANSACCT::PRODTYPE, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(PD_MAIN_PRODTYPE.ID, "V0  ") = 1%)
				THEN
					UTL_TRANSACCT::PRODTYPE = &
						PD_PRODTYPE::CODE
				END IF
				GOTO Reenter
			END IF

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Location\*
	!	.p
	!	The ^*Location\* field enters a selected
	!	location number which has been established in the Utilities
	!	system.
	!
	! Index:
	!
	!--

			UTL_TRANSACCT::LOCATION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "04;23", TEMP$, &
				UTL_TRANSACCT::LOCATION, MFLAG, "'E", &
				MVALUE)


			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "V0") = 1%
				THEN
					UTL_TRANSACCT::LOCATION = &
						UTL_LOCATION::LOCATION
				END IF
				GOTO Reenter
			END IF


			CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Account Number\*
	!	.p
	!	The ^*Account Number\* field
	!	enters a specific General Ledger account number which
	!	relates to a company location and/or a product type.
	!	.p
	!	Each company location and product type combination may, if
	!	desired, be associated with a specific General Ledger account number,
	!	or locations only or product types only may be associated with
	!	designated General Ledger account numbers.
	!	.p
	!	The General Ledger account number entered must be a valid account
	!	established in the Chart of accounts. Pressing ^*<List Choices>\*,
	!	at this field, will cause valid choices to be displayed.
	!
	! Index:
	!	.x General Ledger>Account Number
	!	.x Product Type Account>General Ledger Account Number
	!
	!--

			UTL_TRANSACCT::ACCOUNT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "05;23", TEMP$, &
				UTL_TRANSACCT::ACCOUNT, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX  ") = 1%)
				THEN
					UTL_TRANSACCT::ACCOUNT = &
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
		UTL_MAIN_TRANSACCT = 0%

		SELECT MLOOP

		CASE 1%
			!
			! Test UTL_TRANSTYPE
			!
			UTL_MAIN_TRANSACCT = FUNC_TESTENTRY(SMG_WINDOW, &
				UTL_TRANSACCT::TRANSTYPE, &
				UTL_TRANSTYPE::DESCRIPTION, &
				"UTL", MLOOP, "PRG", &
				"Transaction", UTL_MAIN_TRANSTYPE.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_TRANSTYPE::DESCRIPTION , 2%, 32%, , &
				SMG$M_BOLD)

		CASE 2%
			!
			! Test PD_PRODTYPE
			!
			UTL_MAIN_TRANSACCT = FUNC_TESTENTRY(SMG_WINDOW, &
				UTL_TRANSACCT::PRODTYPE, &
				PD_PRODTYPE::DESCRIPTION, "UTL", MLOOP, "PRG", &
				"Product type", PD_MAIN_PRODTYPE.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_PRODTYPE::DESCRIPTION , 3%, 32%, , &
				SMG$M_BOLD)

		CASE 3%
			!
			! Test UTL_LOCATION
			!
			UTL_MAIN_TRANSACCT = FUNC_TESTENTRY(SMG_WINDOW, &
				UTL_TRANSACCT::LOCATION, &
				UTL_LOCATION::LOCNAME, "UTL", MLOOP, "PRG", &
				"Location", UTL_MAIN_LOCATION.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_LOCATION::LOCNAME, 4%, 32%, , SMG$M_BOLD)

		CASE 4%
			!
			! Test GL_CHART
			!
			UTL_MAIN_TRANSACCT = FUNC_TESTENTRY(SMG_WINDOW, &
				UTL_TRANSACCT::ACCOUNT, &
				GL_CHART::DESCR, "UTL", MLOOP, "PRG", &
				"Account", GL_MAIN_CHART.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 5%, 42%, , SMG$M_BOLD)

		END SELECT

	!
	! Display the data
	!
	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(1%) AND 2%) = 0%
		THEN
			UTL_TRANSTYPE::DESCRIPTION  = STRING$(LEN( &
				UTL_TRANSTYPE::DESCRIPTION), A"?"B) &
				IF  MAIN_WINDOW(UTL_MAIN_TRANSTYPE.ID, "Q0" + &
				UTL_TRANSACCT::TRANSTYPE) <> 1%
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_TRANSTYPE::DESCRIPTION , 2%, 32%, , &
				SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(2%) AND 2%) = 0%
		THEN
			PD_PRODTYPE::DESCRIPTION = STRING$(LEN( &
				PD_PRODTYPE::DESCRIPTION), A"?"B) &
				IF MAIN_WINDOW(PD_MAIN_PRODTYPE.ID, "Q0" + &
					UTL_TRANSACCT::PRODTYPE) <> 1%
			IF INSTR(1%, UTL_TRANSACCT::PRODTYPE, "?") > 0%
			THEN
				PD_PRODTYPE::DESCRIPTION = &
					"Product Type Overlay Mask"
			END IF
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_PRODTYPE::DESCRIPTION, 3%, 32%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(3%) AND 2%) = 0%
		THEN
			UTL_LOCATION::LOCNAME = STRING$(LEN( &
				UTL_LOCATION::LOCNAME), A"?"B) &
				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "Q0" + &
				UTL_TRANSACCT::LOCATION) <> 1%

			IF INSTR(1%, UTL_TRANSACCT::LOCATION, "?") > 0%
			THEN
				UTL_LOCATION::LOCNAME = "Location Overlay Mask"
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_LOCATION::LOCNAME, 4%, 32%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(4%) AND 2%) = 0%
		THEN
			GL_CHART::DESCR = STRING$(LEN( &
				GL_CHART::DESCR), A"?"B) &
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, "Q0" + &
				UTL_TRANSACCT::ACCOUNT) <> 1%
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 5%, 42%, , SMG$M_BOLD)
		END IF

	!
	! Set UTL_TRANSACCT_OLD value
	!
20500	CASE OPT_SETOLD
		UTL_TRANSACCT_OLD = UTL_TRANSACCT

	!
	! Restore UTL_TRANSACCT_OLD value
	!
	CASE OPT_RESETOLD
		UTL_TRANSACCT = UTL_TRANSACCT_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		UTL_TRANSACCT2 = UTL_TRANSACCT

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		UTL_TRANSACCT = UTL_TRANSACCT2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = " TType ProdType Loc  Account"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "007,016,021"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = UTL_TRANSACCT::TRANSTYPE + "   " + &
				UTL_TRANSACCT::PRODTYPE + "       " + &
				UTL_TRANSACCT::LOCATION + " " + &
				UTL_TRANSACCT::ACCOUNT
		END SELECT

	!
	! Find the next record
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE UTL_TRANSACCT::TRANSTYPE + &
				UTL_TRANSACCT::LOCATION

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
