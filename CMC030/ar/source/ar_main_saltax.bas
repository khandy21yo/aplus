1	%TITLE "Sales Tax Rate Table"
	%SBTTL "AR_MAIN_SALTAX"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AR_MAIN_SALTAX(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	The ^*Sales Tax Rate Table\* is used to define the sales tax
	!	rates for applicable sales tax jurisdictions.
	!	.lm -5
	!
	! Index:
	!	.x Sales Tax Rates>Maintain
	!	.x Maintenance>Sales Tax Rates
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
	!	$ BAS AR_SOURCE:AR_MAIN_SALTAX/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP AR_MAIN_SALTAX
	!	$ DELETE AR_MAIN_SALTAX.OBJ;*
	!
	! Author:
	!
	!	03/09/88 - Aaron Redd
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
	!	02/22/93 - Dan Perkins
	!		Changed "V0" to "VX" on chart of accounts to be
	!		able to list accounts starting at a particular
	!		account.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	07/03/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/21/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/02/2000 - Kevin Handy
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
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[AR.OPEN]AR_SALTAX.HB"
	MAP (AR_SALTAX)		AR_SALTAX_CDD		AR_SALTAX
	MAP (AR_SALTAX2) AR_SALTAX_CDD		AR_SALTAX_OLD, AR_SALTAX2

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD		GL_CHART

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_COUNTRY.HB"
	MAP (UTL_COUNTRY)	UTL_COUNTRY_CDD		UTL_COUNTRY

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_AR_SALTAX) &
		AR_SALTAX.CH%, &
		AR_SALTAX.READONLY%

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
		SMG_WINDOW::DESCR = "AR Sales Tax Table Maintenance"
		SMG_WINDOW::NHELP = "AR_MAIN_SALTAX"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 5%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Country, State, and County"
			SMG_WINDOW::KFIELD(0%, 0%) = 3%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
			SMG_WINDOW::KFIELD(0%, 2%) = 2%
			SMG_WINDOW::KFIELD(0%, 3%) = 3%

		!
		! Load in defaults
		!
		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF AR_SALTAX.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF AR_SALTAX.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_SALTAX.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			AR_MAIN_SALTAX = ERR
			CONTINUE 770
		END WHEN

		AR_SALTAX.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_SALTAX.OPN"
		USE
			AR_MAIN_SALTAX = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		AR_SALTAX.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
			CALL ASSG_FREECHANNEL(AR_SALTAX.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = AR_SALTAX.CH%

		WHEN ERROR IN
			RESET #AR_SALTAX.CH%
			GET #AR_SALTAX.CH%, REGARDLESS
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

		DATA	5, 1, "(01) Country", &
			7, 1, "(02) State", &
			9, 1, "(03) County", &
			11,1, "(04) %Sales Tax", &
			13,1, "(05) Account", &
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
	!	.ts 55
	!	^*(01) Country	2 Characters\*
	!	.b
	!	.lm +5
	!	The ^*Country\* field indicates the country to which
	!	a sales tax record has reference.
	!	.lm -5
	!
	! Index:
	!	.x Sales Tax>Country
	!	.x Country>Sales Tax
	!
	!--

			AR_SALTAX::COUNTRY = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "5;20", TEMP$, &
				AR_SALTAX::COUNTRY, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UTL_MAIN_COUNTRY.ID, "V0" + &
					AR_SALTAX::COUNTRY) = 1%
				THEN
					AR_SALTAX::COUNTRY = &
						UTL_COUNTRY::COUNTRY
				END IF
				GOTO E0Loop
			END IF

		CASE 2%

	!++
	! Abstract:FLD002
	!	.ts 55
	!	^*(02) State	2 Characters\*
	!	.b
	!	.lm +5
	!	The ^*State\* field enters a code
	!	representing the State jurisdiction for a specific
	!	sales tax record.
	!	.lm -5
	!
	! Index:
	!	.x Sales Tax>State
	!	.x State>Sales Tax
	!
	!--

			AR_SALTAX::STATE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "7;20", TEMP$, &
				AR_SALTAX::STATE, MFLAG, "'E", MVALUE)
		CASE 3%

	!++
	! Abstract:FLD003
	!	.ts 55
	!	^*(03) County	2 Characters\*
	!	.b
	!	.lm +5
	!	The ^*County\* field enters a code representing
	!	the County jurisdiction for a specific sales
	!	tax record.
	!	.lm -5
	!
	! Index:
	!	.x Sales Tax>County
	!	.x County>Sales Tax
	!
	!--

			AR_SALTAX::COUNTY = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "9;20", TEMP$, &
				AR_SALTAX::COUNTY, MFLAG, "'E", MVALUE)
		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) % Sales Tax\*
	!	.b
	!	.lm +5
	!	The ^*% Sales Tax\* field contains the percentage of Sales tax applicable
	!	in a specific jurisdiction.
	!	.lm -5
	!
	! Index:
	!	.x Sales Tax>Rate
	!	.x Sales Tax>Percentage
	!	.x Rates>Sales Tax
	!	.x Percentage>Sales Tax
	!
	!--

			AR_SALTAX::PERCENT = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "11;20", TEMP$, &
				AR_SALTAX::PERCENT, MFLAG, "####.####", MVALUE)
		CASE 5%

	!++
	! Abstract:FLD005
	!	^*(05) Account\*
	!	.b
	!	.lm +5
	!	The ^*Account\* field is provided to enter the General Ledger
	!	revenue account number to which sales taxes are to be credited.
	!	.b
	!	Pressing ^*List Choices\* will display a list of
	!	valid General Ledger account numbers.
	!	.lm -5
	!
	! Index:
	!	.x Sales Tax>Revenue Account
	!	.x Revenue Account>Sales Tax
	!
	!--

			AR_SALTAX::ACCOUNT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "13;20", TEMP$, &
				AR_SALTAX::ACCOUNT, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX  ") = 1%)
				THEN
					AR_SALTAX::ACCOUNT = &
						GL_CHART::ACCT
				END IF
				GOTO E0Loop
			END IF

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY

		AR_MAIN_SALTAX = 0%

		SELECT MLOOP

		CASE 5%
			!
			! Is the input defined?
			!
			AR_MAIN_SALTAX = FUNC_TESTENTRY(SMG_WINDOW, &
				AR_SALTAX::ACCOUNT, GL_CHART::DESCR, &
				"AR", MLOOP, "ACCT", &
				"Account number", GL_MAIN_CHART.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(GL_CHART::DESCR, 30%), &
				13%, 40%, , SMG$M_BOLD)

		END SELECT


	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(5%) AND 2%) = 0%
		THEN
			!
			! Is the input defined?
			!
			IF MAIN_WINDOW(GL_MAIN_CHART.ID, &
				"Q0" + AR_SALTAX::ACCOUNT) <> 1%
			THEN
				GL_CHART::DESCR = "????????????????????"
			END IF

			!
			! Print chart description
			!
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(GL_CHART::DESCR, 30%), &
				13%, 40%, , SMG$M_BOLD)

		END IF

	!
	! Set AR_SALTAX_OLD value
	!
20500	CASE OPT_SETOLD

		AR_SALTAX_OLD = AR_SALTAX

	!
	! Restore AR_SALTAX_OLD value
	!
	CASE OPT_RESETOLD

		AR_SALTAX = AR_SALTAX_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT

		AR_SALTAX2 = AR_SALTAX

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT

		AR_SALTAX = AR_SALTAX2

	!
	! View header
	!
	CASE OPT_VIEW

		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "   Ctry.   State   County   %Sales Tax   Account"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "010,018,027,040"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = "  " + AR_SALTAX::COUNTRY + "      " + &
				AR_SALTAX::STATE + "       " + &
				AR_SALTAX::COUNTY + "       " + &
				FORMAT$(AR_SALTAX::PERCENT, "##.###  ") + &
				"     " + AR_SALTAX::ACCOUNT

		END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP
		CASE 0%
			FIND #AR_SALTAX.CH%, &
				KEY #0% GE AR_SALTAX::COUNTRY + &
				AR_SALTAX::STATE + &
				AR_SALTAX::COUNTY, &
				REGARDLESS
		END SELECT
	END SELECT

 ExitFunction:
	EXIT FUNCTION

29000	!*****************************************************************
	! Trap Errors
	!*****************************************************************

	RESUME ExitFunction

32767	END FUNCTION
