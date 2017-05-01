1	%TITLE "Sales Commission Account"
	%SBTTL "SA_MAIN_COMMACCT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG SA_MAIN_COMMACCT(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	The ^*Sales Commission Account Table Report\*
	!	provides a list of Commission GL Accounts.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS SA_SOURCE:SA_MAIN_COMMACCT/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP SA_MAIN_COMMACCT
	!	$ DELETE SA_MAIN_COMMACCT.OBJ;*
	!
	! Author:
	!
	!	07/05/90 - Lance Williams
	!
	! Modification history:
	!
	!	08/27/91 - Dan Perkins
	!		Realigned View Screen.
	!
	!	04/13/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	04/28/92 - Kevin Handy
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
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/05/2000 - Kevin Handy
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
	%INCLUDE "FUNC_INCLUDE:SA_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[SA.OPEN]SA_COMMACCT.HB"
	MAP (SA_COMMACCT)	SA_COMMACCT_CDD		SA_COMMACCT
	MAP (SA_COMMACCT_OLD)	SA_COMMACCT_CDD		SA_COMMACCT_OLD, SA_COMMACCT2

	%INCLUDE "SOURCE:[SA.OPEN]SA_TYPE.HB"
	MAP (SA_TYPE)		SA_TYPE_CDD		SA_TYPE

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
	COM (CH_SA_COMMACCT) &
		SA_COMMACCT.CH%, &
		SA_COMMACCT.READONLY%

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
		SMG_WINDOW::DESCR	= "Sales Commission Account"
		SMG_WINDOW::CURREC	= -2%
		SMG_WINDOW::NHELP	= "SA_MAIN_COMMACCT"
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
		SMG_WINDOW::KNAME(0%) = "Type"
			SMG_WINDOW::KFIELD(0%, 0%)	= 1%
			SMG_WINDOW::KFIELD(0%, 1%)	= 1%

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, "QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF SA_COMMACCT.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF SA_COMMACCT.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[SA.OPEN]SA_COMMACCT.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			SA_MAIN_COMMACCT = ERR
			CONTINUE 770
		END WHEN

		SA_COMMACCT.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[SA.OPEN]SA_COMMACCT.OPN"
		USE
			SA_MAIN_COMMACCT = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		SA_COMMACCT.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(SA_COMMACCT.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = SA_COMMACCT.CH%
		WHEN ERROR IN
			RESET #SA_COMMACCT.CH%
			GET #SA_COMMACCT.CH%, REGARDLESS
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

		DATA	02,03, "(01) Salesman Type", &
			03,03, "(02) Location", &
			04,03, "(03) Comm. Exp Acct", &
			05,03, "(04) Comm. Pay Acct", &
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
	!	^*(01) Salesman Type\*
	!	.B
	!	.lm +5
	!	The ^*Salesman Type\* field enters a selected
	!	salesman type which has been established in the Salesman
	!	Type Table.
	!	.b
	!	The field will accommodate two characters.
	!	.b
	!	Valid Salesman types may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!
	!--
			SA_COMMACCT::SALTYPE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "02;23", TEMP$, &
				SA_COMMACCT::SALTYPE, MFLAG, "'E", &
				MVALUE)


			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(SA_MAIN_TYPE.ID, "V0") = 1%
				THEN
					SA_COMMACCT::SALTYPE = &
						SA_TYPE::TTYPE
				END IF
				GOTO Reenter
			END IF

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F17
			THEN
				V% = MAIN_WINDOW(SA_MAIN_TYPE.ID, "M")
				SA_COMMACCT::SALTYPE = SA_TYPE::TTYPE
				GOTO ReEnter
			END IF



		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Location\*
	!	.b
	!	.lm +5
	!	The ^*Location\* field enters a selected
	!	location number which has been established in the Utilities
	!	system.
	!	.b
	!	The field will accept up to four characters.
	!	Valid Location codes may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!
	!--

			SA_COMMACCT::LOCATION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "03;23", TEMP$, &
				SA_COMMACCT::LOCATION, MFLAG, "'E", &
				MVALUE)


			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "V0") = 1%
				THEN
					SA_COMMACCT::LOCATION = &
						UTL_LOCATION::LOCATION
				END IF
				GOTO Reenter
			END IF


			CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Commission Expense Account _#\*
	!	.b
	!	.lm +5
	!	The ^*Commission Expense Account _#\* field
	!	enters a specific General Ledger account number
	!	which relates to a company location and/or a product type.
	!	.b
	!	Each company location and product type combination may, if
	!	desired, be associated with a specific General Ledger account number,
	!	or locations only or product types only may be associated with
	!	designated General Ledger account numbers.
	!	.b
	!	The General Ledger account number entered must be a valid account
	!	established in the Chart of accounts.
	!	.b
	!	Valid account numbers may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x General Ledger>Commission Account Number
	!	.x Product Type account>General Ledger Account Number
	!
	!--

			SA_COMMACCT::EXPACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "04;23", TEMP$, &
				SA_COMMACCT::EXPACCT, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX  ") = 1%)
				THEN
					SA_COMMACCT::EXPACCT = &
						GL_CHART::ACCT
				END IF
				GOTO Reenter
			END IF

			CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Commision Payable Account #\*
	!	.b
	!	.lm +5
	!	The ^*Commision Payable Account _#\* field
	!	enters a specific General Ledger account number
	!	which relates to a company location and/or product type.
	!	.b
	!	Each company location and product type combination may, if
	!	desired, be associated with a specific General Ledger account number,
	!	or locations only or product types only may be associated with
	!	designated General Ledger account numbers.
	!	.b
	!	The General Ledger account number entered must be a valid account
	!	established in the Chart of accounts.
	!	.b
	!	Valid account codes may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x General Ledger>Commission Account Number
	!	.x Product Type account>General Ledger Account Number
	!
	!--

			SA_COMMACCT::PAYACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "05;23", TEMP$, &
				SA_COMMACCT::PAYACCT, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX  ") = 1%)
				THEN
					SA_COMMACCT::PAYACCT = &
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
		SA_MAIN_COMMACCT = 0%

		SELECT MLOOP

		CASE 1%
			!
			! Test SA_COMMACCT
			!
			SA_MAIN_COMMACCT = FUNC_TESTENTRY(SMG_WINDOW, &
				SA_COMMACCT::SALTYPE, &
				SA_TYPE::DESCR, &
				"SA", MLOOP, "PROG", &
				"Sale Type ", SA_MAIN_TYPE.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				SA_TYPE::DESCR, 2%, 32%, , SMG$M_BOLD)

		CASE 2%
			!
			! Test UTL_LOCATION
			!
			SA_MAIN_COMMACCT = FUNC_TESTENTRY(SMG_WINDOW, &
				SA_COMMACCT::LOCATION, &
				UTL_LOCATION::LOCNAME, &
				"SA", MLOOP, "PROG", &
				"Location ", UTL_MAIN_LOCATION.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_LOCATION::LOCNAME, 3%, 32%, , SMG$M_BOLD)

		CASE 3%
			!
			! Test EXPENSE account
			!
			SA_MAIN_COMMACCT = FUNC_TESTENTRY(SMG_WINDOW, &
				SA_COMMACCT::EXPACCT, &
				GL_CHART::DESCR, &
				"SA", MLOOP, "PROG", &
				"Account ", GL_MAIN_CHART.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 4%, 42%, , SMG$M_BOLD)

		CASE 4%
			!
			! Test PAY account
			!
			SA_MAIN_COMMACCT = FUNC_TESTENTRY(SMG_WINDOW, &
				SA_COMMACCT::PAYACCT, &
				GL_CHART::DESCR, &
				"SA", MLOOP, "PROG", &
				"Account ", GL_MAIN_CHART.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 5%, 42%, , SMG$M_BOLD)

		END SELECT

	!
	! Display the data
	!
	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(1%) AND 2%) = 0%
		THEN
			SA_TYPE::DESCR = STRING$(LEN( &
				SA_TYPE::DESCR), A"?"B) &
				IF  MAIN_WINDOW(SA_MAIN_TYPE.ID, "Q0" + &
				SA_COMMACCT::SALTYPE) <> 1%

			IF INSTR(1%, SA_COMMACCT::SALTYPE, "?") > 0%
			THEN
				SA_TYPE::DESCR = "Salesman Type Overlay Mask"
				FLAG% = 1%
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				SA_TYPE::DESCR, 2%, 32%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(2%) AND 2%) = 0%
		THEN
			UTL_LOCATION::LOCNAME = STRING$(LEN( &
				UTL_LOCATION::LOCNAME), A"?"B) &
				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "Q0" + &
				SA_COMMACCT::LOCATION) <> 1%

			IF INSTR(1%, SA_COMMACCT::LOCATION, "?") > 0%
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
					SA_COMMACCT::EXPACCT) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 4%, 42%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(4%) AND 2%) = 0%
		THEN
			GL_CHART::DESCR = STRING$(LEN( &
				GL_CHART::DESCR), A"?"B) &
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, "Q0" + &
					SA_COMMACCT::PAYACCT) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 5%, 42%, , SMG$M_BOLD)
		END IF

	!
	! Set SA_COMMACCT_OLD value
	!
20500	CASE OPT_SETOLD
		SA_COMMACCT_OLD = SA_COMMACCT

	!
	! Restore SA_COMMACCT_OLD value
	!
	CASE OPT_RESETOLD
		SA_COMMACCT = SA_COMMACCT_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		SA_COMMACCT2 = SA_COMMACCT

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		SA_COMMACCT = SA_COMMACCT2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Saletype  Location  ExpenseAcct         PayAcct"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "012,022,042"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = SA_COMMACCT::SALTYPE + "        " + &
				SA_COMMACCT::LOCATION + "      " + &
				SA_COMMACCT::EXPACCT + "  " + &
				SA_COMMACCT::PAYACCT

		END SELECT

	!
	! Find the next record
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE SA_COMMACCT::LOCATION + &
					SA_COMMACCT::SALTYPE

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
