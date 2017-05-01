1	%TITLE "Account Control Maintenance"
	%SBTTL "CK_MAIN_CONTROLACC"
	%IDENT "V3.6a Calico"

	FUNCTION LONG CK_MAIN_CONTROLACC(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1988 BY
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
	!	The ^*Account Control Maintenance\* program maintains the Account control file
	!	for the Check reconciliation.
	!	.lm -5
	!
	! Index:
	!	.x Account Control Maintenance
	!	.x Control Maintenance>Account
	!	.x Maintenance>Control>Account
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS CK_SOURCE:CK_MAIN_CONTROLACC/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP CK_MAIN_CONTROLACC
	!	$ DELETE CK_MAIN_CONTROLACC.OBJ;*
	!
	! Author:
	!
	!	03/24/88 - Lance Williams
	!
	! Modification history:
	!
	!	04/28/88 - Kevin Handy
	!		Modified to allow R/O open of file if R/W fails.
	!
	!	05/07/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	06/16/92 - Kevin Handy
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
	!	10/17/96 - Kevin Handy
	!		Reformat source code
	!
	!	05/01/97 - Kevin Handy
	!		Clean up source code.
	!		Use integer for #key
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/10/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	10/20/2000 - Keivn Handy
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

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD		GL_CHART

	%INCLUDE "SOURCE:[CK.OPEN]CK_CONTROLACC.HB"
	MAP (CK_CONTROLACC)	CK_CONTROLACC_CDD	CK_CONTROLACC
	MAP (CK_CONTROLACC_OLD)	CK_CONTROLACC_CDD	CK_CONTROLACC_OLD, CK_CONTROLACC2

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_CK_CONTROLACC) &
		CK_CONTROLACC.CH%, &
		CK_CONTROLACC.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION MAIN_WINDOW
	EXTERNAL LONG    FUNCTION FUNC_TESTENTRY

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION
	!********************************************************************
	! Initialization
	!
	! This option is used to initialize the window structure,
	! set up the default values for add, and open all files
	! necessary that have not already been opened.
	!********************************************************************
	CASE OPT_INIT

		!******************************************************************
		! Set up information
		!******************************************************************

		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Bank Account Control File"
		SMG_WINDOW::NHELP = "CK_MAIN_CONTROLACC"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 5%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Account"
		SMG_WINDOW::KFIELD(0%, 0%) = 1%
		SMG_WINDOW::KFIELD(0%, 1%) = 1%

		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF CK_CONTROLACC.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF CK_CONTROLACC.READONLY%
			GOTO 790
		END IF

		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[CK.OPEN]CK_CONTROLACC.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			CK_MAIN_CONTROLACC = ERR
			CONTINUE 770
		END WHEN

		CK_CONTROLACC.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[CK.OPEN]CK_CONTROLACC.OPN"
		USE
			CK_MAIN_CONTROLACC = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		CK_CONTROLACC.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(CK_CONTROLACC.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = CK_CONTROLACC.CH%
		WHEN ERROR IN
			RESET #CK_CONTROLACC.CH%
			GET #CK_CONTROLACC.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN


	!**********************************************************************
	! Display the background
	!
	! This option is used to display the background information on the
	! screen.  It must first clear any junk on the screen, and then
	! write the background onto it.
	!**********************************************************************
20100	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	05,10, "(01) GL Account #", &
			06,10, "(02) Bank Code", &
			07,10, "(03) Start Check #", &
			08,10, "(04) End Check #", &
			09,10, "(05) Bank Account #", &
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

	!**********************************************************************
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user, display data,
	! set defaults, and return the data back according to MFLAG.
	!**********************************************************************
20200	CASE OPT_ENTRY
		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")
 E0Loop:
		SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) GL Account _#\*
	!	.b
	!	.lm +5
	!	The ^*GL Account _#\* field enters
	!	the number of a "cash account" which is defined in the
	!	General Ledger Chart of Accounts.
	!	.b
	!	Pressing ^*<List Choices>\*, while the cursor is located
	!	at this field, will cause a list of valid GL Account _#'s
	!	to be displayed.
	!	.lm -5
	!
	! Index:
	!	.x GL Account #>Bank Account Control File
	!	.x Bank Account Control File>GL Account #
	!
	!--

			CK_CONTROLACC::ACCOUNT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "05;30", TEMP$, &
				CK_CONTROLACC::ACCOUNT, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%)
				THEN
					CK_CONTROLACC::ACCOUNT = &
						GL_CHART::ACCT
				END IF
				GOTO E0Loop
			END IF

			IF MAIN_WINDOW(GL_MAIN_CHART.ID, &
				"Q0" + CK_CONTROLACC::ACCOUNT) <> 1%
			THEN
				GL_CHART::DESCR = "??????????????????????????"
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(GL_CHART::DESCR, 28%), &
				5%, 50%, , SMG$M_BOLD)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Bank Code\*
	!	.b
	!	.lm +5
	!	The ^*Bank Code\* field enters a user
	!	defined code which represents a bank account.
	!	.b
	!	The field will accommodate six (06) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Bank Code>Bank Account Control File
	!	.x Bank Account Control File>Bank Code
	!
	!--

			CK_CONTROLACC::BANK_ACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "06;30", TEMP$, &
				CK_CONTROLACC::BANK_ACCT, MFLAG, "'E", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Start Check _#\*
	!	.b
	!	.lm +5
	!	The ^*Start Check _#\* field enters the number
	!	of the first check in the series of checks to be used for a
	!	related bank account.
	!	.b
	!	The field will accommodate six (6) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Start Check _#>Bank Account Control File
	!	.x Bank Account Control File>Start Check _#
	!
	!--

			CK_CONTROLACC::STARTCK = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "07;30", TEMP$, &
				CK_CONTROLACC::STARTCK, MFLAG OR 2%, "'E", &
				MVALUE)

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) End Check _#\*
	!	.b
	!	.lm +5
	!	The ^*End Check _#\* field enters
	!	the last check number in the series of checks to be used
	!	for a related bank account.
	!	.b
	!	The field will accommodate an entry of six (6) alphanumeric
	!	characters.
	!	.lm -5
	!
	! Index:
	!	.x End Check _#>Bank Account Control File
	!	.x Bank Account Control File>End Check _#
	!
	!--

			CK_CONTROLACC::ENDCK = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "08;30", TEMP$, &
				CK_CONTROLACC::ENDCK, MFLAG OR 2%, "'E", &
				MVALUE)
		CASE 5%

	!++
	! Abstract:FLD005
	!	^*(05) Bank Account _#\*
	!	.b
	!	.lm +5
	!	The ^*Bank Account _#\* field enters the
	!	bank account number assigned by the bank for a checking
	!	account.
	!	.b
	!	The field will accommodate twenty (20) alphanumeric
	!	characters.
	!	.lm -5
	!
	! Index:
	!	.x Bank Account _#>Bank Account Control File
	!	.x Bank Account Control File>Bank Account _#
	!
	!--

			CK_CONTROLACC::BANK_NUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "09;30", TEMP$, &
				CK_CONTROLACC::BANK_NUM, MFLAG, "'E", &
				MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		CK_MAIN_CONTROLACC = 0%

		SELECT MLOOP

		CASE 1%
			IF CK_CONTROLACC::ACCOUNT = ""
			THEN
				CK_MAIN_CONTROLACC = 1%
				GOTO TLoop
			ELSE
				IF (MVALUE = "ADD")
				THEN
					WHEN ERROR IN
						GET #SMG_WINDOW::CHAN, &
							KEY #0% EQ CK_CONTROLACC::ACCOUNT + "", &
							REGARDLESS
					USE
						CONTINUE 32767 IF ERR = 155%
						EXIT HANDLER
					END WHEN

					CK_MAIN_CONTROLACC = 2%
					CALL ENTR_3MESSAGE(SCOPE, "Record Already Exists", 0%)
					GOTO TLoop
				END IF
			END IF

			!
			! Is the input defined?
			!
			CK_MAIN_CONTROLACC = FUNC_TESTENTRY(SMG_WINDOW, &
				CK_CONTROLACC::ACCOUNT, &
				GL_CHART::DESCR, &
				"CK", MLOOP, "PRG", &
				"Account", GL_MAIN_CHART.ID)

 Tloop:
		END SELECT

	!
	! Set CK_CONTROLACC_OLD value
	!
20500	CASE OPT_SETOLD
		CK_CONTROLACC_OLD = CK_CONTROLACC

	!
	! Restore CK_CONTROLACC_OLD value
	!
	CASE OPT_RESETOLD
		CK_CONTROLACC = CK_CONTROLACC_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		CK_CONTROLACC2 = CK_CONTROLACC

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		CK_CONTROLACC = CK_CONTROLACC2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
		MVALUE = "  Account             BankCode     StartCheck  Endcheck" + &
			"  BankNumber"
		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "021,035,047,057"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = CK_CONTROLACC::ACCOUNT + "  " + &
				CK_CONTROLACC::BANK_ACCT + "       " + &
				CK_CONTROLACC::STARTCK + "      " + &
				CK_CONTROLACC::ENDCK + "    " + &
				CK_CONTROLACC::BANK_NUM
		END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP
		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE CK_CONTROLACC::ACCOUNT + "", &
				REGARDLESS
		END SELECT

	END SELECT

27000	EXIT FUNCTION

29000	!
	! Trap errors
	!
	ON ERROR GO BACK

32767	END FUNCTION
