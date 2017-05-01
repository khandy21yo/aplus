1	%TITLE "General Ledger Budget Maintenance"
	%SBTTL "GL_MAIN_BUDGET"
	%IDENT "V3.6a Calico"

	FUNCTION LONG GL_MAIN_BUDGET(CDD_WINDOW_CDD SMG_WINDOW, &
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
	! ID:1010
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Budget Maintenance\* routine sets
	!	up and maintains budgets. The ^*cOpy\* function is included in the
	!	Budget COMMAND Menu. (Refer to the related Help messages for
	!	information on the cOpy command.) After a file for a specific
	!	budget year has been created by using the cOpy command, budget
	!	data may be added to each record in the file. A new budget
	!	record may be added to the file by using the ^*Add\* command.
	!	.lm -5
	!
	! Index:
	!	.x Budget>Maintenance
	!	.x Maintenance>Budget
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_MAIN_BUDGET/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN GL_MAIN_BUDGET
	!	$ DELETE GL_MAIN_BUDGET.OBJ;*
	!
	! Author:
	!
	!	04/15/87 - Kevin Handy
	!
	! Modification history:
	!
	!	05/10/88 - Lance Williams
	!		Modified to allow R/O open of file if R/W fails.
	!
	!	06/23/88 - Aaron Redd
	!		Split into two modules (_MAST_ and _MAIN_) in order
	!		to meet standardization requirements.
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
	!	10/18/96 - Kevin Handy
	!		Reformat source code
	!
	!	05/01/97 - Kevin Handy
	!		Reformat source code
	!		Use integer for #key
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE's
	!
	!	03/09/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	06/09/99 - Kevin Handy
	!		Lose line 27580 (Dead Code)
	!
	!	09/15/2000 - Kevin Handy
	!		Use LIB$DELETE_FILE instead of KILL
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
	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION FUNC_TESTENTRY
	EXTERNAL LONG	FUNCTION GL_MAIN_CHART
	EXTERNAL LONG	FUNCTION MAIN_WINDOW

	%PAGE

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[GL.OPEN]GL_BUD_YYYY.HB"
	MAP	(GL_BUD_YYYY)	GL_BUD_YYYY_CDD	GL_BUD_YYYY
	MAP	(GL_BUD_YYYY2)	GL_BUD_YYYY_CDD	GL_BUD_YYYY_OLD, GL_BUD_YYYY2

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP	(GL_CHART)	GL_CHART_CDD	GL_CHART

	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	MAP	(GL_PERIOD)	GL_PERIOD_CDD	GL_PERIOD

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_GL_BUD_YYYY) &
		GL_BUD_YYYY.CH%, &
		GL_BUD_YYYY.READONLY%
	COM (TT_GL_BUD_YYYY) &
		GL_BUDGET.YEAR$ = 4%, &
		GL_BUD_YYYY.DEV$ = 36%

	COM (CH_GL_PERIOD) &
		GL_PERIOD.CH%
	COM (CH_GL_CHART) &
		GL_CHART.CH%, &
		GL_CHART.READONLY%

	!
	! Declare some variables
	!
	DECLARE LONG XPOS, YPOS, TEMPW

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 29000

	SELECT MOPTION

	!***********************************************************
	! Initialization
	!
	! This option is used to initialize the window structure,
	! set up the default values for add, and open all files
	! necessary that have not already been opened.
	!***********************************************************
	CASE OPT_INIT

		!
		! Define window
		!
		SMG_WINDOW::DESCR = "GL Budget " + GL_BUDGET.YEAR$ + &
			" Maintenance"
		SMG_WINDOW::NHELP = "GL_MAIN_BUDGET"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 40%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Account"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		!
		! Load in defaults
		!
		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF GL_BUD_YYYY.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF GL_BUD_YYYY.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[GL.OPEN]GL_BUD_YYYY.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			GL_MAIN_BUDGET = ERR
			CONTINUE 770
		END WHEN

		GL_BUD_YYYY.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[GL.OPEN]GL_BUD_YYYY.OPN"
		USE
			GL_MAIN_BUDGET = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		GL_BUD_YYYY.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(GL_BUD_YYYY.CH%)
		GOTO ExitFunction

790		SMG_WINDOW::CHAN  = GL_BUD_YYYY.CH%
		WHEN ERROR IN
			RESET #GL_BUD_YYYY.CH%
			GET #GL_BUD_YYYY.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

		!
		! Get control file record
		!
		GOSUB 28000

20100	!***********************************************************
	! Display the background
	!
	! This option is used to display the background information
	! on the screen.  It must first clear any junk on the screen,
	! and then write the background onto it.
	!***********************************************************
	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	2,  2, "(01) Account", &
			2, 36, "Descr:", &
			4, 27, "Budget Dollars", &
			4, 46, "Budget Units", &
			4, 63, "Budget Hours", &
			0,  0, ""

		RESTORE

		READ XPOS, YPOS, XSTR$
		I% = 0%
		WHILE (XPOS <> 0%)
			I% = I% + 1%
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				XSTR$, XPOS, YPOS) &
				IF (SMG_WINDOW::HFLAG(I%) AND 2%) = 0%
			READ XPOS, YPOS, XSTR$
		NEXT

		FOR I% = 1% TO 13%
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				TRM$(GL_PERIOD::PERIOD(I%)), &
				I% + 5%, 2%)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				FORMAT$(I% + 1%, "(<0>#)"), &
				I% + 5%, 24%)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				FORMAT$(I% + 14%, "(<0>#)"), &
				I% + 5%, 41%)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				FORMAT$(I% + 27%, "(<0>#)"), &
				I% + 5%, 58%)
		NEXT I%

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

20150	!***********************************************************
	! Add to the option menu
	!***********************************************************
	CASE OPT_OPTLIST
		MVALUE = MVALUE + " cOpy Move"

	!***********************************************************
	! Window option
	!***********************************************************
	CASE OPT_MOREMENU

		SELECT MVALUE
		!
		! Copy
		!
		CASE "cOpy"
			GOSUB CopyOption

			!
			! Repaint screen
			!
			GL_MAIN_BUDGET = 8%

		!
		! Move
		!
		CASE "Move"
			GOSUB MoveOption

			!
			! Repaint screen
			!
			GL_MAIN_BUDGET = 8%

		END SELECT

20200	!***********************************************************
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user,
	! display data, set defaults, and return the data back
	! according to MFLAG.
	!***********************************************************
	CASE OPT_ENTRY
		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View "

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 E0Loop:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Account\*
	!	.b
	!	.lm +5
	!	This field refers to the account number in the
	!	General Ledger Chart of Accounts. If an account number
	!	which does not exist in the Chart of Accounts is entered
	!	in this field, the system will prompt the user to confirm
	!	the entry unless ^*ALLOW\* no invalid accounts has been set.
	!	If ^*ALLOW\* no invalid accounts has been set the system will
	!	prompt the user to enter a valid account number. If the account
	!	number exists in the Chart of Accounts, the ^*Descr\* will display the
	!	name related to the account number.
	!	.b
	!	Eighteen spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!
	!--

			GL_BUD_YYYY::ACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "2;18", TEMP$, &
				GL_BUD_YYYY::ACCT, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX  ") = 1%)
				THEN
					GL_BUD_YYYY::ACCT = &
						GL_CHART::ACCT
				END IF
				GOTO E0Loop
			END IF

			IF MAIN_WINDOW(GL_MAIN_CHART.ID, &
				"Q0" + GL_BUD_YYYY::ACCT) <> 1%
			THEN
				GL_CHART::DESCR = &
					STRING$(LEN(GL_CHART::DESCR), 63%)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(GL_CHART::DESCR, 34%), &
				2%, 43%, 0%, SMG$M_BOLD)

		CASE 2% TO 14%
			SCOPE::PRG_ITEM = "FLD002DOLL"
	!++
	! Abstract:FLD002DOLL
	!	^*(02)-(14) Budget Dollars\*
	!	.b
	!	.lm +5
	!	^*Budget Dollars\* refers to the amount of funds available for, required for,
	!	or assigned to a particular period.
	!	.lm -5
	!
	! Index:
	!
	!--
			GL_BUD_YYYY::DOLLAR(MLOOP - 1%) = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, NUM1$(MLOOP + 4%) + ";29", &
				TEMP$, GL_BUD_YYYY::DOLLAR(MLOOP - 1%), &
				MFLAG, "###,###,###", MVALUE)

		CASE 15% TO 27%
			SCOPE::PRG_ITEM = "FLD015UNIT"
	!++
	! Abstract:FLD015UNIT
	!	^*(15)-(27) Budget Units\*
	!	.b
	!	.lm +5
	!	^*Budget Units\* refers to the number of units available for, required for,
	!	or assigned to a particular period.
	!	.lm -5
	!
	! Index:
	!
	!--
			GL_BUD_YYYY::UNIT(MLOOP - 14%) = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, NUM1$(MLOOP - 9%) + ";46", &
				TEMP$, GL_BUD_YYYY::UNIT(MLOOP - 14%), &
				MFLAG, "###,###,###", MVALUE)

		CASE 28% TO 40%
			SCOPE::PRG_ITEM = "FLD028HOUR"
	!++
	! Abstract:FLD028HOUR
	!	^*(28)-(40) Budget Hours\*
	!	.b
	!	.lm +5
	!	^*Budget Hours\* refers to the number of hours available for, required for,
	!	or assigned to a particular period.
	!	.lm -5
	!
	! Index:
	!
	!--
			GL_BUD_YYYY::HOUR(MLOOP - 27%) = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, NUM1$(MLOOP - 22%) + ";63", &
				TEMP$, GL_BUD_YYYY::HOUR(MLOOP - 27%), &
				MFLAG, "###,###,###", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

20300	!***********************************************************
	! Test values
	!***********************************************************
	CASE OPT_TESTENTRY
		GL_MAIN_BUDGET = 0%

		SELECT MLOOP

		CASE 1%
			!
			! Don't allow duplicates
			!
			IF (MVALUE = "ADD")
			THEN
				GOSUB GetBudget

				IF GL_BUDGET_FOUND%
				THEN
					GL_MAIN_BUDGET = 2%
					CALL ENTR_3MESSAGE(SCOPE, "Record Already Exists", 1%)
					GOTO ExitFunction
				END IF
			END IF

			!
			! Don't allow blank account numbers
			!
			IF GL_BUD_YYYY::ACCT = ""
			THEN
				GL_MAIN_BUDGET = 1%
				CALL ENTR_3MESSAGE(SCOPE, "Illegal account number", 1%)
				GOTO ExitFunction
			END IF

			!
			! Make sure account number is defined
			!
			GL_MAIN_BUDGET = FUNC_TESTENTRY( SMG_WINDOW, &
				GL_BUD_YYYY::ACCT, GL_CHART::DESCR, &
				"GL", MLOOP, "ACCT", &
				"Account number", GL_MAIN_CHART.ID)

		END SELECT

20500	!***********************************************************
	! Set GL_BUD_YYYY_OLD value
	!***********************************************************
	CASE OPT_SETOLD
		GL_BUD_YYYY_OLD = GL_BUD_YYYY

	!***********************************************************
	! Restore GL_BUD_YYYY_OLD value
	!***********************************************************
	CASE OPT_RESETOLD
		GL_BUD_YYYY = GL_BUD_YYYY_OLD

	!***********************************************************
	! Set default value
	!***********************************************************
	CASE OPT_SETDEFAULT
		GL_BUD_YYYY2 = GL_BUD_YYYY

	!***********************************************************
	! Restore default value
	!***********************************************************
	CASE OPT_RESETDEFAULT
		GL_BUD_YYYY = GL_BUD_YYYY2

	!***********************************************************
	! View header
	!***********************************************************
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Account            Description"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "021"

		!
		! Convert current record into text
		!
		CASE 3%
			IF MAIN_WINDOW(GL_MAIN_CHART.ID, &
				"Q0" + GL_BUD_YYYY::ACCT) <> 1%
			THEN
				GL_CHART::DESCR = STRING$(LEN( &
					GL_CHART::DESCR), 63%)
			END IF

			MVALUE = GL_BUD_YYYY::ACCT + " " + &
				GL_CHART::DESCR

		END SELECT

	!***********************************************************
	! Find
	!***********************************************************
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			!
			! FIND according to the primary key
			!	(Account number)
			!
			FIND #GL_BUD_YYYY.CH%, &
				KEY #0% GE GL_BUD_YYYY::ACCT + "", &
				REGARDLESS

		END SELECT

	END SELECT

 ExitFunction:
	EXIT FUNCTION

	%PAGE

 CopyOption:
27000	!*******************************************************************
	! Subroutine to copy Chart of accounts to budget file
	!*******************************************************************
	INP$ = ENTR_3YESNO(SCOPE, SMG_WINDOW::WNUMBER, "", &
		"Confirm Copying of chart of accounts then press <Do>", &
		"N", 0%, "!", "")

	GOTO ComeBack1 IF INP$ <> "Y"

	TEMPW = SMG_WINDOW::WNUMBER

	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		18%, &
		78%, &
		SMG_WINDOW::WNUMBER &
	)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_WINDOW::WNUMBER, &
		SCOPE::SMG_PBID, &
		2%, &
		2% &
	)

	TEMP = 0.0
	CALL ENTR_3MESSAGE(SCOPE, "Creating new GL_BUD_YYYY file", 1%)

	CLOSE GL_BUD_YYYY.CH%

	CALL ASSG_FREECHANNEL(GL_BUD_YYYY.CH%)

 !	KILL GL_BUD_YYYY.DEV$ + "GL_BUD_" + GL_BUDGET.YEAR$ + ".MAS"

	SMG_STATUS% = LIB$DELETE_FILE(GL_BUD_YYYY.DEV$ + "GL_BUD_" + &
		GL_BUDGET.YEAR$ + ".MAS;*")

27010	!
	! Create new budget file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_BUD_YYYY.CRE"
	USE
		CALL ENTR_3MESSAGE(SCOPE, "Unable to open budget file", 0%)

		SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_WINDOW::WNUMBER)
		SMG_WINDOW::WNUMBER = TEMPW

		CONTINUE 32767
	END WHEN

	GL_MAIN_BUDGET = GL_MAIN_CHART(SMG_WINDOW, MOPTION, &
		MLOOP, MFLAG, MVALUE)

27050	WHEN ERROR IN
		RESET #GL_CHART.CH%
		GET #GL_CHART.CH%, REGARDLESS
	USE
		CALL ENTR_3MESSAGE(SCOPE, "Chart file is empty", 0%)

		SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_WINDOW::WNUMBER)
		SMG_WINDOW::WNUMBER = TEMPW

		CONTINUE 32767
	END WHEN

	CALL ENTR_3MESSAGE(SCOPE, "Adding accounts to GL_BUD_YYYY file", 1%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
		"Records processed", 10%, 1%)

27070	!
	! Add record to the file
	!
	GL_BUD_YYYY::ACCT		= GL_CHART::ACCT
	GL_BUD_YYYY::DOLLAR(LOOP%)	= 0.0 FOR LOOP% = 0% TO 13%
	GL_BUD_YYYY::UNIT(LOOP%)	= 0.0 FOR LOOP% = 0% TO 13%
	GL_BUD_YYYY::HOUR(LOOP%)	= 0.0 FOR LOOP% = 0% TO 13%

	WHEN ERROR IN
		PUT #GL_BUD_YYYY.CH%
	USE
		CALL ENTR_3MESSAGE(SCOPE, "Unable to add to budget file", 0%)

		SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_WINDOW::WNUMBER)
		SMG_WINDOW::WNUMBER = TEMPW

		CONTINUE 32767
	END WHEN
	TEMP = TEMP + 1.0

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
		FORMAT$(TEMP, "#######"), 10%, 19%)
	SMG_STATUS% = SMG$FLUSH_BUFFER(SCOPE::SMG_PBID)

27100	WHEN ERROR IN
		GET #GL_CHART.CH%, REGARDLESS
	USE
		CONTINUE 27200
	END WHEN

	GOTO 27070

27200	RESET #GL_BUD_YYYY.CH%
	GET #GL_BUD_YYYY.CH%, REGARDLESS

	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_WINDOW::WNUMBER)
	SMG_WINDOW::WNUMBER = TEMPW

 ComeBack1:
	RETURN

	%PAGE

 MoveOption:
27500	!*******************************************************************
	! Subroutine to move balances from real accounts
	!*******************************************************************
	BEGBAL% = 0%

	IF GL_PERIOD::FPFY <> GL_PERIOD::LASTPERCLO
	THEN
		IF GL_BUDGET.YEAR$ = GL_PERIOD::YEAR
		THEN
			BEGBAL% = GL_PERIOD::LASTPERCLO
		ELSE
			CALL ENTR_3MESSAGE(SCOPE, &
				"Year in GL doesn't match Budget file", 0%)
			GOTO ComeBack2
		END IF
	END IF

	INP$ = ENTR_3YESNO(SCOPE, SMG_WINDOW::WNUMBER, "", &
		"Confirm moving balances from chart to budget then press <Do>", &
		"N", 0%, "!", "")

	GOTO ComeBack2 IF INP$ <> "Y"

	TEMPW = SMG_WINDOW::WNUMBER

	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		18%, &
		78%, &
		SMG_WINDOW::WNUMBER &
	)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_WINDOW::WNUMBER, &
		SCOPE::SMG_PBID, &
		2%, &
		2% &
	)

	TEMP = 0.0
	CALL ENTR_3MESSAGE(SCOPE, &
		"Moving balance from real accounts to budget file", 1%)

	GL_MAIN_BUDGET = GL_MAIN_CHART(SMG_WINDOW, MOPTION, &
		MLOOP, MFLAG, MVALUE)

27550	WHEN ERROR IN
		RESET #GL_CHART.CH%
		GET #GL_CHART.CH%, REGARDLESS
	USE
		CALL ENTR_3MESSAGE(SCOPE, "Chart file is empty", 0%)

		SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_WINDOW::WNUMBER)
		SMG_WINDOW::WNUMBER = TEMPW

		CONTINUE 32767
	END WHEN

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
		"Records processed", 10%, 1%)

27570	GOTO 27600 IF INSTR(1%, GL_CHART::ACCTYPE, "R,E")

	!
	! Find budget record
	!
	WHEN ERROR IN
		GET #GL_BUD_YYYY.CH%, KEY #0% EQ GL_CHART::ACCT + ""
	USE
		CALL ENTR_3MESSAGE(SCOPE, "Unable to update budget file", 0%)

		SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_WINDOW::WNUMBER)
		SMG_WINDOW::WNUMBER = TEMPW

		CONTINUE 32767
	END WHEN

	GL_BUD_YYYY::DOLLAR(0%)	= GL_CHART::DOLLAR(BEGBAL%)
	GL_BUD_YYYY::UNIT(0%)	= GL_CHART::UNIT(BEGBAL%)
	GL_BUD_YYYY::HOUR(0%)	= GL_CHART::HOUR(BEGBAL%)

	WHEN ERROR IN
		UPDATE #GL_BUD_YYYY.CH%
	USE
		CALL ENTR_3MESSAGE(SCOPE, "Unable to update budget file", 0%)

		SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_WINDOW::WNUMBER)
		SMG_WINDOW::WNUMBER = TEMPW

		CONTINUE 32767
	END WHEN

	GOTO 27600

 !27580
	!
	! Add record to the file
	!
 !	GL_BUD_YYYY::ACCT		= GL_CHART::ACCT
 !	GL_BUD_YYYY::DOLLAR(LOOP%)	= 0.0 FOR LOOP% = 0% TO 13%
 !	GL_BUD_YYYY::UNIT(LOOP%)	= 0.0 FOR LOOP% = 0% TO 13%
 !	GL_BUD_YYYY::HOUR(LOOP%)	= 0.0 FOR LOOP% = 0% TO 13%
 !
 !	GL_BUD_YYYY::DOLLAR(0%)	= GL_CHART::DOLLAR(BEGBAL%)
 !	GL_BUD_YYYY::UNIT(0%)	= GL_CHART::UNIT(BEGBAL%)
 !	GL_BUD_YYYY::HOUR(0%)	= GL_CHART::HOUR(BEGBAL%)
 !
 !	PUT #GL_BUD_YYYY.CH%

27600	TEMP = TEMP + 1.0

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
		FORMAT$(TEMP, "#######"), 10%, 19%)
	SMG_STATUS% = SMG$FLUSH_BUFFER(SCOPE::SMG_PBID)

	WHEN ERROR IN
		GET #GL_CHART.CH%, REGARDLESS
	USE
		CONTINUE 27700
	END WHEN
	GOTO 27570

27700	RESET #GL_BUD_YYYY.CH%
	GET #GL_BUD_YYYY.CH%, REGARDLESS

	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_WINDOW::WNUMBER)
	SMG_WINDOW::WNUMBER = TEMPW

 ComeBack2:
	RETURN

	%PAGE

28000	!***********************************************************
	! Subroutine to get record from Control file
	!***********************************************************

	!
	! Open GL Control file
	!
	GOTO 28010 IF GL_PERIOD.CH% > 0%

	CALL ASSG_CHANNEL(GL_PERIOD.CH%, STAT%)
	IF STAT%
	THEN
		GL_MAIN_BUDGET = 1%
		GOTO ComeBack3
	END IF

	!
	! Open up period file, and grab record
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.OPN"
	USE
		CALL ENTR_3MESSAGE(SCOPE, &
			"Unable to find the GL control file", 0%)
		CONTINUE 28010
	END WHEN

28010	WHEN ERROR IN
		GET #GL_PERIOD.CH%, RECORD 1%, REGARDLESS
	USE
		CALL ENTR_3MESSAGE(SCOPE, &
			"Unable to find the GL control record", 0%)
		CONTINUE 32767
	END WHEN

	CLOSE GL_PERIOD.CH%

 ComeBack3:
	RETURN

	%PAGE

 GetBudget:
	!***********************************************************
	! Subroutine to determine if the record already exists
	!***********************************************************
28500	GL_BUDGET_FOUND% = 0%
	WHEN ERROR IN
		GET #GL_BUD_YYYY.CH%, KEY #0% EQ GL_BUD_YYYY::ACCT + ""
	USE
		CONTINUE 28510
	END WHEN

	GL_BUDGET_FOUND% = -1%

28510	RETURN

	%PAGE

29000	!***********************************************************
	! Trap errors
	!***********************************************************

	ON ERROR GO BACK

32767	!***********************************************************
	! End of GL_MAIN_BUDGET function
	!***********************************************************
	END FUNCTION
