1	%TITLE "Promotional Sales Table Maintenance"
	%SBTTL "OE_MAIN_PROMO"
	%IDENT "V3.6a Calico"

	FUNCTION LONG OE_MAIN_PROMO(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1990 BY
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
	!	The ^*Promotional Sales Table Maintenance\* option
	!	accesses the file where promotional sales are
	!	stored and may be viewed, modified, or erased according to the
	!	user's specifications.
	!	.lm -5
	!
	! Index:
	!	.x Promotional Sales Table Maintenance
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_MAIN_PROMO/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP OE_MAIN_PROMO
	!	$ DELETE OE_MAIN_PROMO.OBJ;*
	!
	!
	! Author:
	!
	!	12/03/90 - Val James Allen
	!
	! Modification history:
	!
	!	08/27/91 - Dan Perkins
	!		Realigned View Screen.
	!
	!	04/10/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	04/27/92 - Kevin Handy
	!		Clean up (check)
	!
	!	04/05/93 - Kevin Handy
	!		Clean up (check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/20/96 - Kevin Handy
	!		Reformat source code
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

	%INCLUDE "SOURCE:[OE.OPEN]OE_PROMO.HB"
	MAP (OE_PROMO)		OE_PROMO_CDD		OE_PROMO
	MAP (OE_PROMO_OLD)	OE_PROMO_CDD		OE_PROMO_OLD, OE_PROMO2

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP(GL_CHART)		GL_CHART_CDD		GL_CHART

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_OE_PROMO) &
		OE_PROMO.CH%, &
		OE_PROMO.READONLY%

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
		SMG_WINDOW::DESCR = "Promotional Sales Table"
		SMG_WINDOW::NHELP = "OE_MAIN_PROMO"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 5%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Refpromo"
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
		IF OE_PROMO.CH% > 0%
		THEN
			!
			! If OE_PROMO is already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF OE_PROMO.READONLY%
			GOTO 790
		END IF

		!
		! Open OE_PROMO (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[OE.OPEN]OE_PROMO.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			OE_MAIN_PROMO  = ERR
			CONTINUE 770
		END WHEN

		OE_PROMO.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open OE_PROMO for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[OE.OPEN]OE_PROMO.OPN"
		USE
			OE_MAIN_PROMO = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		OE_PROMO.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open OE_PROMO, so reset channel
		!
		CALL ASSG_FREECHANNEL(OE_PROMO.CH%)

		GOTO ExitFunction

790		SMG_WINDOW::CHAN  = OE_PROMO.CH%
		WHEN ERROR IN
			RESET #OE_PROMO.CH%
			GET #OE_PROMO.CH%, REGARDLESS
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

		DATA	2,  1, "(01) Promo Number", &
			3,  1, "(02) From Date", &
			4,  1, "(03) To Date", &
			5,  1, "(04) GL Account #", &
			6,  1, "(05) Description", &
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
	!	.x Promotion Number
	!	^*(01) Promo Number\*
	!	.b
	!	.lm +5
	!	The ^*Promo Number\* field enters a promotion code
	!	which is assigned to each promotional process.
	!	.b
	!	Valid Promotion _#'s may be viewed by pressing ^*List Choices\*.
	!	.b
	!	Sixteen spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!
	!--

			OE_PROMO::REFPROMO = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"2;22", TEMP$, &
				OE_PROMO::REFPROMO, MFLAG, "'E", MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	.x From Date
	!	^*(02) From Date\*
	!	.b
	!	.lm +5
	!	The ^*From Date\* field enters the beginning date
	!	this promotion will be in effect.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!
	!--

			OE_PROMO::FROMDATE = ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"3;22", TEMP$, &
				OE_PROMO::FROMDATE, MFLAG, "'E", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	.x To Date
	!	^*(03) To Date\*
	!	.b
	!	.lm +5
	!	The ^*To Date\* field enters the ending date
	!	for this promotion.
	!	.lm -5
	!
	! Index:
	!
	!--

			OE_PROMO::TODATE = ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"4;22", TEMP$, &
				OE_PROMO::TODATE, MFLAG, "'E", MVALUE)


		CASE 4%

	!++
	! Abstract:FLD004
	!	.x G/L Account
	!	^*(04) G/L Account\*
	!	.b
	!	.lm +5
	!	The complete descriptive name of this field is General Ledger
	!	Chart of Account Number, though it is sometimes referred to as
	!	General Ledger Number, Account Number or Account.  This field
	!	cannot be left blank. A value must be typed when a record is
	!	being added.
	!	.b
	!	Eighteen spaces are available for entry of the GL Account number.
	!	.b
	!	Valid Accounts may be viewed by pressing ^*List Choices\*.
	!	.b
	!	^*Note:\* After an account number has been added
	!	and transactions have been posted to the account, this
	!	field ^*cannot\* be changed.
	!	.lm -5
	!
	! Index:
	!
	!--

			OE_PROMO::ACCOUNT = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"5;22", TEMP$, &
				OE_PROMO::ACCOUNT, MFLAG, "'E", MVALUE)


			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%)
				THEN
					OE_PROMO::ACCOUNT = &
						GL_CHART::ACCT
				END IF
				GOTO Reenter
			END IF

		CASE 5%

	!++
	! Abstract:FLD005
	!	.x Promo Description
	!	^*(05) Promo Description\*
	!	.b
	!	.lm +5
	!	The ^*Promo Description\* field enters a description
	!	or notes, which will be printed on the order or invoice form.
	!	.b
	!	Forty spaces are available for the description.
	!	.lm -5
	!
	! Index:
	!
	!--

			OE_PROMO::DESCRIPTION = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"6;22", TEMP$, &
				OE_PROMO::DESCRIPTION, MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

20300	CASE OPT_TESTENTRY
		OE_MAIN_PROMO = 0%

		SELECT MLOOP

		CASE 4%
			!
			! Test GL_CHART
			!
			OE_MAIN_PROMO = FUNC_TESTENTRY(SMG_WINDOW, &
				OE_PROMO::ACCOUNT, &
				GL_CHART::DESCR, &
				"OE", MLOOP, "PROG", &
				"Promo Account", &
				GL_MAIN_CHART.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 5%, 42%, , SMG$M_BOLD)

		END SELECT

	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(4%) AND 2%) = 0%
		THEN
			!
			! Display description for General Ledger Account Number
			!
			GL_CHART::DESCR = STRING$(LEN( &
				GL_CHART::DESCR), A"?"B) &
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, "Q0" + &
				OE_PROMO::ACCOUNT) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 5%, 42%, , SMG$M_BOLD)
		END IF

	!
	! Set OE_PROMO_OLD value
	!
20500	CASE OPT_SETOLD
		OE_PROMO_OLD = OE_PROMO

	!
	! Restore OE_PROMO_OLD value
	!
	CASE OPT_RESETOLD
		OE_PROMO = OE_PROMO_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		OE_PROMO2 = OE_PROMO

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		OE_PROMO = OE_PROMO2

	!
	! View the OE_PROMO Record.
	!
	CASE OPT_VIEW

		SELECT MLOOP

		CASE 1%
			MVALUE = "  Promotion Number From Date  To Date    Account Number"

		CASE 2%
			MVALUE = "019,030,041"

		CASE 3%
			MVALUE = OE_PROMO::REFPROMO + " " + &
				PRNT_DATE(OE_PROMO::FROMDATE, 8%) + " " + &
				PRNT_DATE(OE_PROMO::TODATE, 8%) + " " + &
				OE_PROMO::ACCOUNT

		END SELECT
	!
	! Find the OE_PROMO Record.
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #OE_PROMO.CH%, &
				KEY #0% GE OE_PROMO::REFPROMO + "", &
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
