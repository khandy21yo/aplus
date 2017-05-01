1	%TITLE "Maintain Service Charge Table"
	%SBTTL "AR_MAIN_SERCHG"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AR_MAIN_SERCHG(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	The ^*Maintain Service Charge Table\* is used to enter monthly service
	!	charge rates for various States in order to conform to the various usury
	!	laws.
	!	.lm -5
	!
	! Index:
	!	.x Service Charges>Maintain Rates
	!	.x Maintain>Service Charge Rates
	!	.x Service Charges>Table>Maintain
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
	!	$ BAS AR_SOURCE:AR_MAIN_SERCHG/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP AR_MAIN_SERCHG
	!	$ DELETE AR_MAIN_SERCHG.OBJ;*
	!
	!
	! Author:
	!
	!	02/12/88 - Aaron Redd
	!
	! Modification history:
	!
	!	05/16/88 - Aaron Redd
	!		Modified to allow R/O open of file if R/W open fails.
	!
	!	08/13/91 - Kevin Handy
	!		Modified to add SPERIOD field to maintenance.
	!
	!	04/15/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	04/22/92 - Kevin Handy
	!		Clean up (check)
	!
	!	08/19/92 - Dan Perkins
	!		Fixed bug in the OPT_VIEW section which caused
	!		the view not to work.  Fixed view screen.
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
	!	11/09/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[AR.OPEN]AR_SERCHG.HB"
	MAP (AR_SERCHG)		AR_SERCHG_CDD		AR_SERCHG
	MAP (AR_SERCHG2)	AR_SERCHG_CDD		AR_SERCHG_OLD, &
							AR_SERCHG2

	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.HB"
	MAP (AR_CONTROL)	AR_CONTROL_CDD		AR_CONTROL

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD		GL_CHART

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_COUNTRY.HB"
	MAP (UTL_COUNTRY)	UTL_COUNTRY_CDD		UTL_COUNTRY

	!
	! Common Areas
	!
	! These areas store information that is re-used between calls to
	! these functions.
	!
	COM (TT_GL_CHART) &
		ACCTITLE$ = 20%, &
		ACCTYPE$(6%) = 20%

	COM (CH_AR_SERCHG) &
		AR_SERCHG.CH%, &
		AR_SERCHG.READONLY%

	COM (CH_AR_CONTROL) &
		AR_CONTROL.CH%, &
		AR_CONTROL.READONLY%

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
		SMG_WINDOW::DESCR = "AR Service Charge Table Maintenance"
		SMG_WINDOW::NHELP = "AR_MAIN_SERCHG"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 8%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "LOG_CLASS"
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
		IF AR_SERCHG.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF AR_SERCHG.READONLY%
			GOTO 790
		END IF

740		IF AR_CONTROL.CH% = 0%
		THEN
			WHEN ERROR IN
				%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.OPN"
			USE
				FOR I% = 0% TO 4%
					AR_CONTROL::AGENAM(I%) = &
						"Period " + NUM1$(I% + 1%)
					AR_CONTROL::AGEPER(I%) = 30%
				NEXT I%
				CONTINUE 750
			END WHEN

			AR_CONTROL.READONLY% = -1%
		END IF

		WHEN ERROR IN
			GET #AR_CONTROL.CH%, RECORD 1%
		USE
			FOR I% = 0% TO 4%
				AR_CONTROL::AGENAM(I%) = &
					"Period " + NUM1$(I% + 1%)
				AR_CONTROL::AGEPER(I%) = 30%
			NEXT I%

			CONTINUE 750
		END WHEN

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_SERCHG.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			AR_MAIN_SERCHG = ERR
			CONTINUE 770
		END WHEN

		AR_SERCHG.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_SERCHG.OPN"
		USE
			AR_MAIN_SERCHG = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		AR_SERCHG.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
			CALL ASSG_FREECHANNEL(AR_SERCHG.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = AR_SERCHG.CH%
		WHEN ERROR IN
			RESET #AR_SERCHG.CH%
			GET #AR_SERCHG.CH%, REGARDLESS
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

		DATA	2,1, "(01) Country", &
			4,1, "(02) State", &
			6,1, "(03) Acct", &
			8,1, "(04) Rev Acct", &
			10,1, "(05) SC Percent", &
			12,1, "(06) Min SC", &
			14,1, "(07) Min Balance", &
			16,1, "(08) Start Period", &
			11,40, "Periods", &
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

		FOR I% = 0% TO 4%
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				NUM1$(I% + 1%) + " - " + &
				AR_CONTROL::AGENAM(I%), I% + 13%, 40%) &
				IF (SMG_WINDOW::HFLAG(I%) AND 2%) = 0%
		NEXT I%

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
	!	The ^*Country\* field is used to identify which country a record
	!	has reference to.
	!	.b
	!	This field requires an entry.
	!	.lm -5
	!
	! Index:
	!	.x Country>Service Charge Codes
	!	.x Service Charges>Country Codes
	!
	!--

			AR_SERCHG::COUNTRY = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "2;18", TEMP$, &
				AR_SERCHG::COUNTRY, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UTL_MAIN_COUNTRY.ID, "V0" + &
					AR_SERCHG::COUNTRY) = 1%
				THEN
					AR_SERCHG::COUNTRY = &
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
	!	The ^*State\* field is used to identify to which State a service
	!	charge record refers.
	!	.b
	!	This field requires an entry and should conform to the standard
	!	U.S. postal codes.
	!	.lm -5
	!
	! Index:
	!	.x State>Code>Service Charge
	!	.x Service Charges>State Code
	!
	!--

			AR_SERCHG::STATE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "4;18", TEMP$, &
				AR_SERCHG::STATE, MFLAG, "'E", MVALUE)
		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Account\*
	!	.b
	!	.lm +5
	!	The ^*Account\* field enters the Accounts Receivable
	!	account to which calculated service charges will be charged.
	!	If your system is a cash based system, this account number will be blank.
	!	.b
	!	Pressing ^*List Choices\* will provide a list of valid General Ledger
	!	account numbers.
	!	.lm -5
	!
	! Index:
	!	.x Account>Service Charge
	!	.x Service Charge>Account
	!
	!--

			AR_SERCHG::ACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "6;18", TEMP$, &
				AR_SERCHG::ACCT, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX  ") = 1%)
				THEN
					AR_SERCHG::ACCT = &
						GL_CHART::ACCT
				END IF
				GOTO E0Loop
			END IF

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Revenue Account\*
	!	.b
	!	.lm +5
	!	The ^*Revenue Account\* field enters the General
	!	Ledger account number to which service charges are to be credited.
	!	.b
	!	Pressing ^*List Choices\* will provide a list of valid General Ledger
	!	account numbers.
	!	.lm -5
	!
	! Index:
	!	.x Revenue Account>Service Charges
	!	.x Service Charges>Revenue Account
	!
	!--

			AR_SERCHG::SCREV = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "8;18", TEMP$, &
				AR_SERCHG::SCREV, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX  ") = 1%)
				THEN
					AR_SERCHG::SCREV = &
						GL_CHART::ACCT
				END IF
				GOTO E0Loop
			END IF

		CASE 5%

	!++
	! Abstract:FLD005
	!	.ts 55
	!	^*(05) Service Charge Percent	%%.%%\*
	!	.b
	!	.lm +5
	!	The ^*Service Charge Percent\* field allows for entry of the service charge
	!	rate.  The percentage entered should not exceed
	!	the maximum service charge percentage allowed by the usury
	!	code as defined for a specific State.
	!	.lm -5
	!
	! Index:
	!	.x Service Charges>Percent
	!	.x Percent>Service Charges
	!	.x Rate>Service Charges
	!	.x Service Charges>Rate
	!
	!--

			AR_SERCHG::SERCHG = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "10;18", TEMP$, &
				AR_SERCHG::SERCHG, MFLAG, "###.###", MVALUE)

		CASE 6%

	!++
	! Abstract:FLD006
	!	^*(06) Minimum Service Charge\*
	!	.b
	!	.lm +5
	!	The ^*Minimum Service Charge\* field determines the minimum
	!	amount of service charge per month which will be charged to any customer
	!	with any past due balance.
	!	.b
	!	Regardless of the service charge calculated, no service charge will
	!	be less than the value in this field.
	!	.b
	!	A common minimum monthly service charge is $0.50.
	!	.lm -5
	!
	! Index:
	!	.x Service Charges>Minimum
	!
	!--

			AR_SERCHG::MINIMUM = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "12;18", TEMP$, &
				AR_SERCHG::MINIMUM, MFLAG, "#######.##", &
				MVALUE)

		CASE 7%

	!++
	! Abstract:FLD007
	!	^*(07) Minimum Balance\*
	!	.b
	!	.lm +5
	!	The ^*Minimum Balance\* field determines a past due amount
	!	for which no service charge will be calculated as long as that amount
	!	does not exceed the value of this field.
	!	.b
	!	A common past due minimum balance is $5.00.
	!	.lm -5
	!
	! Index:
	!	.x Service Charge>Minimum Balance
	!
	!--

			AR_SERCHG::DOLLAR = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "14;18", TEMP$, &
				AR_SERCHG::DOLLAR, MFLAG, "#######.##", MVALUE)

		CASE 8%

	!++
	! Abstract:FLD008
	!	^*(08) Start Period\*
	!	.b
	!	.lm +5
	!	The ^*Start Period\* field is used to determine which
	!	aging period is considered past due.
	!	.b
	!	^*Note:\* A start period of 0 will cause the second period to
	!	be used. If you want to start with the first
	!	period, you must enter a 1 here.
	!	.lm -5
	!
	! Index:
	!	.x Service Charge>Minimum Balance
	!
	!--

			AR_SERCHG::SPERIOD = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "16;24", TEMP$, &
				AR_SERCHG::SPERIOD * 1.0, MFLAG, "####", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY

		AR_MAIN_SERCHG = 0%

		SELECT MLOOP

		CASE 1%
			IF (AR_SERCHG::COUNTRY = "") AND (AR_SERCHG::STATE = "")
			THEN
				AR_MAIN_SERCHG = 1%
			END IF

		CASE 3%
			!
			! Is the input defined?
			!
			AR_MAIN_SERCHG = FUNC_TESTENTRY( SMG_WINDOW, &
				AR_SERCHG::ACCT, GL_CHART::DESCR, &
				"AR", MLOOP, "ACCT", &
				"Account number", GL_MAIN_CHART.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(GL_CHART::DESCR, 30%), &
				6%, 40%, , SMG$M_BOLD)

		CASE 4%
			!
			! Is the input defined?
			!
			AR_MAIN_SERCHG = FUNC_TESTENTRY( SMG_WINDOW, &
				AR_SERCHG::SCREV, GL_CHART::DESCR, &
				"AR", MLOOP, "ACCT", &
				"Account number", GL_MAIN_CHART.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(GL_CHART::DESCR, 30%), &
				8%, 40%, , SMG$M_BOLD)

		CASE 8%
			IF (AR_SERCHG::SPERIOD < 0%) OR &
				(AR_SERCHG::SPERIOD > 5%)
			THEN
				AR_MAIN_SERCHG = 1%
			END IF

		END SELECT

	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(3%) AND 2%) = 0%
		THEN
			!
			! Is the input defined?
			!
			IF MAIN_WINDOW(GL_MAIN_CHART.ID, &
				"Q0" + AR_SERCHG::ACCT) <> 1%
			THEN
				GL_CHART::DESCR = "????????????????????"
			END IF

			!
			! Print chart description
			!
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(GL_CHART::DESCR, 30%), &
				6%, 40%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(4%) AND 2%) = 0%
		THEN
			!
			! Is the input defined?
			!
			IF MAIN_WINDOW(GL_MAIN_CHART.ID, &
				"Q0" + AR_SERCHG::SCREV) <> 1%
			THEN
				GL_CHART::DESCR = "????????????????????"
			END IF

			!
			! Print chart description
			!
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(GL_CHART::DESCR, 30%), &
				8%, 40%, , SMG$M_BOLD)
		END IF

	!
	! Set AR_SERCHG_OLD value
	!
20500	CASE OPT_SETOLD

		AR_SERCHG_OLD = AR_SERCHG

	!
	! Restore AR_SERCHG_OLD value
	!
	CASE OPT_RESETOLD

		AR_SERCHG = AR_SERCHG_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT

		AR_SERCHG2 = AR_SERCHG

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT

		AR_SERCHG = AR_SERCHG2

	!
	! View header
	!
	CASE OPT_VIEW

		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Ctry. State Account            " + &
				"SCRate      MinSC     MinBal Period"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "008,014,033,040,051,062"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE =  AR_SERCHG::COUNTRY + "    " + &
				AR_SERCHG::STATE + "    " + &
				AR_SERCHG::ACCT + " " + &
				FORMAT$(AR_SERCHG::SERCHG, "##.##% ") + &
				FORMAT$(AR_SERCHG::MINIMUM, "#######.## ") + &
				FORMAT$(AR_SERCHG::DOLLAR, "#######.##   ") + &
				FORMAT$(AR_SERCHG::SPERIOD, "####")

		END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP
		CASE 0%
			FIND #AR_SERCHG.CH%, &
				KEY #0% GE AR_SERCHG::COUNTRY + &
				AR_SERCHG::STATE + &
				AR_SERCHG::ACCT, &
				REGARDLESS
		END SELECT
	END SELECT

 ExitFunction:
	EXIT FUNCTION

29000	!***************************************************************
	! Trap Errors
	!***************************************************************

	RESUME ExitFunction

32767	END FUNCTION
