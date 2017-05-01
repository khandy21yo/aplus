1	%TITLE "Sales Tax Maintenance"
	%SBTTL "OE_MAIN_SALESTAX"
	%IDENT "V3.6a Calico"

	FUNCTION LONG OE_MAIN_SALESTAX(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1987, 1988 BY
	!
	! Computer Management Center
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
	! Computer Management Center.
	!
	! CMC assumes no responsibility for the use or reliability of
	! its software on equipment which is not supported by CMC.
	!
	!++
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Sales Tax Maintenance\* option
	!	accesses the file where sales taxes are
	!	stored and either view, modify, or erase the accounts and their
	!	descriptions to the users specifications.
	!	.lm -5
	!
	! Index:
	!	.x Sales Tax Maintenance
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_MAIN_SALESTAX/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP OE_MAIN_SALESTAX
	!	$ DELETE OE_MAIN_SALESTAX.OBJ;*
	!
	!
	! Author:
	!
	!	07/05/90 - Lance Williams
	!
	! Modification history:
	!
	!	08/28/91 - Dan Perkins
	!		Realigned View Screen.
	!
	!	04/10/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	04/27/92 - Kevin Handy
	!		Clean up (check)
	!
	!	10/05/92 - Dan Perkins
	!		Only test accounts if tax amounts are not zero.
	!
	!	10/26/92 - Kevin Handy
	!		Clean up (Check)
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
	!	10/20/96 - Kevin Handy
	!		Clean up (Check)
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

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:OE_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[OE.OPEN]OE_SALESTAX.HB"
	MAP (OE_SALESTAX)	OE_SALESTAX_CDD		OE_SALESTAX
	MAP (OE_SALESTAX_OLD)	OE_SALESTAX_CDD		OE_SALESTAX_OLD, OE_SALESTAX2

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD		GL_CHART

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_OE_SALESTAX) &
		OE_SALESTAX.CH%, &
		OE_SALESTAX.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION MAIN_WINDOW
	EXTERNAL LONG   FUNCTION FUNC_TESTENTRY

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	!******************************************************************
	! Initialization
	!
	! This option is used to initialize the window structure,
	! set up the default values for add, and open all files
	! necessary that have not already been opened.
	!******************************************************************
	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Order Entry Salestax"
		SMG_WINDOW::NHELP = "OE_MAIN_SALESTAX"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 128%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 8%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "SALESTAX"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		!
		! Load in defaults for chart
		!
		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF OE_SALESTAX.CH% > 0%
		THEN
			!
			! If OE_SALESTAX is already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF OE_SALESTAX.READONLY%
			GOTO 790
		END IF

		!
		! Open OE_SALESTAX (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[OE.OPEN]OE_SALESTAX.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			OE_MAIN_SALESTAX  = ERR
			CONTINUE 770
		END WHEN

		OE_SALESTAX.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open OE_SALESTAX for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[OE.OPEN]OE_SALESTAX.OPN"
		USE
			OE_MAIN_SALESTAX = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		OE_SALESTAX.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open OE_SALESTAX, so reset channel
		!
		CALL ASSG_FREECHANNEL(OE_SALESTAX.CH%)

		GOTO ExitFunction

790		SMG_WINDOW::CHAN  = OE_SALESTAX.CH%
		WHEN ERROR IN
			RESET #OE_SALESTAX.CH%
			GET #OE_SALESTAX.CH%, REGARDLESS
		USE
			CONTINUE 32767 IF ERR = 11%
			EXIT HANDLER
		END WHEN

	%PAGE

20100	!******************************************************************
	! Display the background
	!
	! This option is used to display the background information
	! on the screen.  It must first clear any junk on the screen,
	! and then write the background onto it.
	!******************************************************************

	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	2,  1, "(01) Tax Code", &
			3,  1, "(02) Description", &
			4,  1, "(03) Tax Percent", &
			5,  1, "(04) Account #", &
			6,  1, "(05) City Tax %", &
			7,  1, "(06) City Account", &
			8,  1, "(07) County Tax %", &
			9,  1, "(08) County Account", &
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

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

20200	!******************************************************************
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user, display
	! data, set defaults, and return the data back according to
	! MFLAG.
	!******************************************************************
	CASE OPT_ENTRY
		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 ReEnter:	SCOPE::SCOPE_EXIT = 0%
		SELECT MLOOP

		CASE 1%
	!++
	! Abstract:FLD001
	!	.x Tax Code
	!	^*(01) Tax Code\*
	!	.b
	!	.lm +5
	!	The ^*Tax Code\* field enters a code which
	!	will identify the area (state) for which the sales tax is
	!	applicable.
	!	.b
	!	Example:  ^*ID\* - Idaho
	!	.b
	!	The field will accommodate two characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_SALESTAX::TAXCODE = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"2;22", TEMP$, &
				OE_SALESTAX::TAXCODE, MFLAG, "'E", MVALUE)

		CASE 2%
	!++
	! Abstract:FLD002
	!	.x Description
	!	^*(02) Description\*
	!	.b
	!	.lm +5
	!	The ^*Description\* field enters a brief description
	!	for the code entered in field (01).
	!	.b
	!	Example:  ID - ^*Idaho\*
	!	.b
	!	Twenty spaces are available for the description.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_SALESTAX::JURISDICTION = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"3;22", TEMP$, &
				OE_SALESTAX::JURISDICTION, MFLAG, "'E", MVALUE)

		CASE 3%
	!++
	! Abstract:FLD003
	!	.x State Tax
	!	^*(03) Tax Percent\*
	!	.b
	!	.lm +5
	!	The ^*Tax Percent\* field enters the tax percentage
	!	that is applicable for the tax code entered in field (01).
	!	.b
	!	Example:  If the sales tax percentage is to be 5%, the entry would be
	!	made as 5.00.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_SALESTAX::STATETAX = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"4;22", TEMP$, &
				OE_SALESTAX::STATETAX, MFLAG, "#,###,##.##", MVALUE)

		CASE 4%
	!++
	! Abstract:FLD004
	!	.x Account Number
	!	^*(04) Account Number\*
	!	.b
	!	.lm +5
	!	The ^*Account Number\* field enters the account
	!	number which has been defined for sales tax in the Chart of Accounts.
	!	.b
	!	This field cannot be set to null. A value must be typed
	!	when a record is being added.
	!	.b
	!	^*Note:\* After an account number has been added
	!	and transactions have been posted to the account,
	!	this field ^*cannot\* be changed.
	!	.b
	!	Eighteen spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_SALESTAX::STATEACC = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"5;22", TEMP$, &
				OE_SALESTAX::STATEACC, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX  ") = 1%)
				THEN
					OE_SALESTAX::STATEACC = &
						GL_CHART::ACCT
				END IF
				GOTO Reenter
			END IF

		CASE 5%
	!++
	! Abstract:FLD005
	!	.x City Tax
	!	^*(05) City Tax\*
	!	.B
	!	.LM +5
	!	The ^*City Tax\* field enters the applicable
	!	percentage of tax charged by the City.
	!	.b
	!	Example:  If the tax is to be 5%, the entry would be made
	!	as 5.00.  If there is no City Tax, this field would be left blank.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_SALESTAX::CITYTAX = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"6;22", TEMP$, &
				OE_SALESTAX::CITYTAX, MFLAG, "#,###,###.##", MVALUE)

		CASE 6%
	!++
	! Abstract:FLD006
	!	.x City Accounts
	!	^*(06) City Account\*
	!	.b
	!	.lm +5
	!	The ^*City Account Number\* field enters the General
	!	Ledger account which applies to city taxes.
	!	.b
	!	This field cannot be set to null. A value must be typed
	!	when a record is being added.
	!	.b
	!	Valid accounts may be viewed by pressing ^*List Choices\*.
	!	.b
	!	^*Note:\* After an account number has been added
	!	and transactions have been posted to the account, this field
	!	^*cannot\* be changed.
	!	.b
	!	Eighteen spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_SALESTAX::CITYACC = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"7;22", TEMP$, &
				OE_SALESTAX::CITYACC, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX  ") = 1%)
				THEN
					OE_SALESTAX::CITYACC = &
						GL_CHART::ACCT
				END IF
				GOTO Reenter
			END IF

		CASE 7%
	!++
	! Abstract:FLD007
	!	.x County Tax
	!	^*(07) County Tax\*
	!	.b
	!	.lm +5
	!	The ^*County Tax\* field enters the amount of tax
	!	that is applicable for the county.
	!	.b
	!	Example:  If the amount of county tax is 2%, the entry would be made
	!	as 2.00.  If there is no county Tax, this field would be left blank.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_SALESTAX::COUNTYTAX = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"8;22", TEMP$, &
				OE_SALESTAX::COUNTYTAX, MFLAG, "#,###,###.##", MVALUE)

		CASE 8%
	!++
	! Abstract:FLD008
	!	.x County Account
	!	^*(08) County Account\*
	!	.b
	!	.lm +5
	!	The ^*Account Number\* field enters the General Ledger
	!	account number which is defined for county taxes.
	!	.b
	!	This field cannot be set to null. A value must be typed
	!	when a record is being added.
	!	.b
	!	Valid accounts may be viewed by pressing ^*List Choices\*.
	!	.b
	!	^*Note:\*  After an account number has been added
	!	and transactions have been posted to the account, this
	!	field ^*cannot\* be changed.
	!	.b
	!	Eighteen spaces are available for the entry.
	!	.lm +5
	!
	! Index:
	!
	!--
			OE_SALESTAX::COUNTYACC = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"9;22", TEMP$, &
				OE_SALESTAX::COUNTYACC, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX  ") = 1%)
				THEN
					OE_SALESTAX::COUNTYACC = &
						GL_CHART::ACCT
				END IF
				GOTO Reenter
			END IF

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

20300	CASE OPT_TESTENTRY
		OE_MAIN_SALESTAX = 0%

		SELECT MLOOP

		CASE 4%
			GL_CHART::DESCR = ""

			IF OE_SALESTAX::STATEACC <> ""
			THEN
				!
				! Test GL_CHART
				!
				OE_MAIN_SALESTAX = FUNC_TESTENTRY(SMG_WINDOW, &
					OE_SALESTAX::STATEACC, &
					GL_CHART::DESCR, &
					"OE", MLOOP, "PROG", &
					"State Account", &
					GL_MAIN_CHART.ID)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 5%, 42%, , SMG$M_BOLD)

		CASE 6%
			GL_CHART::DESCR = ""

			IF OE_SALESTAX::CITYACC <> ""
			THEN
				!
				! Test GL_CHART
				!
				OE_MAIN_SALESTAX = FUNC_TESTENTRY(SMG_WINDOW, &
					OE_SALESTAX::CITYACC, &
					GL_CHART::DESCR, &
					"OE", MLOOP, "PROG", &
					"City Account", &
					GL_MAIN_CHART.ID)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 7%, 42%, , SMG$M_BOLD)

		CASE 8%
			GL_CHART::DESCR = ""

			IF OE_SALESTAX::COUNTYACC <> ""
			THEN
				!
				! Test GL_CHART
				!
				OE_MAIN_SALESTAX = FUNC_TESTENTRY(SMG_WINDOW, &
					OE_SALESTAX::COUNTYACC, &
					GL_CHART::DESCR, &
					"OE", MLOOP, "PROG", &
					"County Account", &
					GL_MAIN_CHART.ID)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 9%, 42%, , SMG$M_BOLD)

		END SELECT

	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(4%) AND 2%) = 0%
		THEN
			!
			! Display description for General Ledger Account Number
			!
			GL_CHART::DESCR = "" IF OE_SALESTAX::STATEACC = ""

			IF OE_SALESTAX::STATEACC <> ""
			THEN
				GL_CHART::DESCR = STRING$(LEN( &
					GL_CHART::DESCR), A"?"B) &
					IF MAIN_WINDOW(GL_MAIN_CHART.ID, "Q0" + &
					OE_SALESTAX::STATEACC) <> 1%
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 5%, 42%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(6%) AND 2%) = 0%
		THEN
			!
			! Display description for General Ledger Account Number
			!
			GL_CHART::DESCR = "" IF OE_SALESTAX::CITYACC = ""

			IF OE_SALESTAX::CITYACC <> ""
			THEN
				GL_CHART::DESCR = STRING$(LEN( &
					GL_CHART::DESCR), A"?"B) &
					IF MAIN_WINDOW(GL_MAIN_CHART.ID, "Q0" + &
					OE_SALESTAX::CITYACC) <> 1%
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 7%, 42%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(8%) AND 2%) = 0%
		THEN
			!
			! Display description for General Ledger Account Number
			!
			GL_CHART::DESCR = "" IF OE_SALESTAX::COUNTYACC = ""

			IF OE_SALESTAX::COUNTYACC <> ""
			THEN
				GL_CHART::DESCR = STRING$(LEN( &
					GL_CHART::DESCR), A"?"B) &
					IF MAIN_WINDOW(GL_MAIN_CHART.ID, "Q0" + &
					OE_SALESTAX::COUNTYACC) <> 1%
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 9%, 42%, , SMG$M_BOLD)
		END IF

	!
	! Set OE_SALESTAX_OLD value
	!
20500	CASE OPT_SETOLD
		OE_SALESTAX_OLD = OE_SALESTAX

	!
	! Restore OE_SALESTAX_OLD value
	!
	CASE OPT_RESETOLD
		OE_SALESTAX = OE_SALESTAX_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		OE_SALESTAX2 = OE_SALESTAX

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		OE_SALESTAX = OE_SALESTAX2

	!
	! View the OE_SALESTAX Record.
	!
	CASE OPT_VIEW

		SELECT MLOOP

		CASE 1%
			MVALUE = "TaxCode Description              StateTax   StateAcc" + &
				"                CityTax   CityAcc             CountyTax   CountyAcc"

		CASE 2%
			MVALUE = "008,030,043,063,077,096,109"

		CASE 3%
			MVALUE = OE_SALESTAX::TAXCODE + "    " + &
				OE_SALESTAX::JURISDICTION + "       " + &
				FORMAT$(OE_SALESTAX::STATETAX, "###.##") + "   " + &
				OE_SALESTAX::STATEACC + "       " + &
				FORMAT$(OE_SALESTAX::CITYTAX, "###.##") + "   " + &
				OE_SALESTAX::CITYACC + "     "   + &
				FORMAT$(OE_SALESTAX::COUNTYTAX, "###.##") + "   " + &
				OE_SALESTAX::COUNTYACC

		END SELECT
	!
	! Find the OE_SALESTAX Record.
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #OE_SALESTAX.CH%, &
				KEY #0% GE OE_SALESTAX::TAXCODE + "", &
				REGARDLESS

		END SELECT

	END SELECT

 ExitFunction:
	EXIT FUNCTION

29000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Resume to display untrapped error
	!
	ON ERROR GO BACK

	%PAGE

32767	END FUNCTION
