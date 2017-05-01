1	%TITLE "GL Account Object Maintenance"
	%SBTTL "GL_MAIN_USERLIST"
	%IDENT "V3.6a Calico"

	FUNCTION LONG GL_MAIN_USERLIST(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1987, 1988 BY
	!
	! Software Solutions, Inc.
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
	! Software Solutions, Inc.
	!
	! Software Solutions, Inc. assumes no responsibility for the use
	! or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	! ID:1040
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Maintain/Copy Account Object\* routine is used to
	!	enter, maintain and copy the mask and account object files.
	!	.b
	!	This process is used to create a list of account objects. An
	!	account object is the core of an account where the department,
	!	location, division has been stripped from the account number.
	!	.B
	!	Example: The account number 5021-0010 represents salaries
	!	(5021) in location (0010). In this case the account object
	!	would be 5021-.
	!	.b
	!	After the account objects have been created, an account
	!	mask can be added. This account mask is used to represent
	!	the other parts of an account number, i.e. the location, division,
	!	etc. The mask would look like ????-0010. The account object
	!	would be substituted where the question marks are to create a
	!	new account number of 5021-0010.
	!	.b
	!	This process may be used to create a new group of accounts as new
	!	divisions, departments, or locations are added to the Chart of
	!	Accounts.
	!
	! Index:
	!	.x Mask>Maintenance
	!	.x Maintain>Mask
	!	.x Add>Mask
	!	.x Change>Mask
	!	.x Erase>Mask
	!	.x Mask>Add
	!	.x Mask>Change
	!	.x Mask>Erase
	!	.x Object>Maintenance
	!	.x Maintain>Object
	!	.x Add>Object
	!	.x Change>Object
	!	.x Erase>Object
	!	.x Object>Add
	!	.x Object>Change
	!	.x Object>Erase
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_MAIN_USERLIST/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP GL_MAIN_USERLIST
	!	$ DELETE GL_MAIN_USERLIST.OBJ;*
	!
	! Author:
	!
	!	08/19/97 - Kevin Handy
	!
	! Modification history:
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excess %PAGE's
	!
	!	10/30/2000 - Kevin Handy
	!		Use A"x"B
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION FUNC_TESTENTRY

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[GL.OPEN]GL_USERLIST.HB"
	MAP	(GL_USERLIST)	GL_USERLIST_CDD	GL_USERLIST
	MAP	(GL_USERLIST2)	GL_USERLIST_CDD	GL_USERLIST_OLD, GL_USERLIST2

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP	(GL_CHART)	GL_CHART_CDD	GL_CHART

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_GL_USERLIST) &
		GL_USERLIST.CH%, &
		GL_USERLIST.READONLY%

	!
	! Declare some variables
	!
	DECLARE LONG XPOS, YPOS

	%PAGE

	!
	! Set up error trapping
	!
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
		SMG_WINDOW::DESCR = "Valid User/Account List"
		SMG_WINDOW::NHELP = "GL_MAIN_USERLIST"
		SMG_WINDOW::CHAN  = GL_USERLIST.CH%
		SMG_WINDOW::HSIZE = 70%
		SMG_WINDOW::VSIZE = 14%
		SMG_WINDOW::HVIEW = 70%
		SMG_WINDOW::VVIEW = 14%
		SMG_WINDOW::HPOS  = 5%
		SMG_WINDOW::VPOS  = 4%
		SMG_WINDOW::VHPOS = 5%
		SMG_WINDOW::VVPOS = 4%
		SMG_WINDOW::NITEMS= 3%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 2%
		SMG_WINDOW::KNAME(0%) = "User"
			SMG_WINDOW::KFIELD(0%, 0%) = 2%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
			SMG_WINDOW::KFIELD(0%, 2%) = 2%
		SMG_WINDOW::KNAME(1%) = "Account"
			SMG_WINDOW::KFIELD(1%, 0%) = 2%
			SMG_WINDOW::KFIELD(1%, 1%) = 2%
			SMG_WINDOW::KFIELD(1%, 2%) = 1%

		!
		! Load in defaults
		!
		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF GL_USERLIST.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF GL_USERLIST.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[GL.OPEN]GL_USERLIST.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			GL_MAIN_USERLIST = ERR
			CONTINUE 770
		END WHEN

		GL_USERLIST.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[GL.OPEN]GL_USERLIST.OPN"
		USE
			GL_MAIN_USERLIST = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		GL_USERLIST.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(GL_USERLIST.CH%)

		GOTO ExitFunction

790		SMG_WINDOW::CHAN  = GL_USERLIST.CH%
		WHEN ERROR IN
			RESET #GL_USERLIST.CH%
			GET #GL_USERLIST.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

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

		DATA	3, 10, "(01) User", &
			4, 10, "(02) Account", &
			5, 10, "(03) Flag", &
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

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

20200	!******************************************************************
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user, display
	! data, set defaults, and return the data back according to
	! MFLAG.
	!******************************************************************
	CASE OPT_ENTRY
		TEMP$ = TRM$(SCOPE::PRG_ITEM)

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 E0Loop:
		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	.x Account Object
	!	^*(01) Account Object\*
	!	.b
	!	.lm +5
	!	The ^*Account Object\* is a unique key used to define an
	!	account object.
	!	.b
	!	This field cannot be left blank.  A value must be typed
	!	when a record is being added.
	!	.b
	!	Eighteen spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!	.x Object>Account Object
	!
	!--

			GL_USERLIST::USER = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"3;28", TEMP$, GL_USERLIST::USER, &
				MFLAG, "'E", MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	.x Object>Description
	!	^*(02) Description\*
	!	.b
	!	.lm +5
	!	This field is for the ^*Description\* of an account object.
	!	.b
	!	The field will accommodate 40 characters.
	!	.lm -5
	!
	! Index:
	!
	!--

			GL_USERLIST::ACCOUNT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"4;28", TEMP$, GL_USERLIST::ACCOUNT, &
				MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%)
				THEN
					GL_USERLIST::ACCOUNT = &
						GL_CHART::ACCT
				END IF
				GOTO E0Loop
			END IF

		CASE 3%

	!++
	! Abstract:FLD003
	!	.x Object>Flag
	!	^*(03) Flag\*
	!
	! Index:
	!
	!--

			GL_USERLIST::FLAG = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"5;28", TEMP$, GL_USERLIST::FLAG, &
				MFLAG, "'", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP$

20300	!******************************************************************
	! Test values
	!******************************************************************
	CASE OPT_TESTENTRY
		GL_MAIN_USERLIST = 0%

		SELECT MLOOP

		CASE 2%
			GL_MAIN_USERLIST = FUNC_TESTENTRY(SMG_WINDOW, &
				GL_USERLIST::ACCOUNT, GL_CHART::DESCR, &
				"GL", MLOOP, "ACCT", &
				"Account number", GL_MAIN_CHART.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(GL_CHART::DESCR, 30%), &
				4%, 45%, , SMG$M_BOLD)
		END SELECT

	CASE OPT_DISPLAY
		IF MAIN_WINDOW(GL_MAIN_CHART.ID, &
			"Q0" + GL_USERLIST::ACCOUNT) <> 1%
		THEN
			GL_CHART::DESCR = STRING$(LEN(GL_CHART::DESCR), A"?"B)
		END IF

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			LEFT(GL_CHART::DESCR, 30%), &
			4%, 45%, , SMG$M_BOLD)

20500	!******************************************************************
	! Set GL_USERLIST_OLD value
	!******************************************************************
	CASE OPT_SETOLD
		GL_USERLIST_OLD = GL_USERLIST

	!******************************************************************
	! Restore GL_USERLIST_OLD value
	!******************************************************************
	CASE OPT_RESETOLD
		GL_USERLIST = GL_USERLIST_OLD

	!******************************************************************
	! Set default value
	!******************************************************************
	CASE OPT_SETDEFAULT
		GL_USERLIST2 = GL_USERLIST

	!******************************************************************
	! Restore default value
	!******************************************************************
	CASE OPT_RESETDEFAULT
		GL_USERLIST = GL_USERLIST2

	!***********************************************************
	! View header
	!***********************************************************
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  User             Account            Flag"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "019,038"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = GL_USERLIST::USER + " " + &
				GL_USERLIST::ACCOUNT + " " + &
				GL_USERLIST::FLAG

		END SELECT

	!******************************************************************
	! Find
	!******************************************************************
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			!
			! FIND according to Primary Key
			!	(Object Mask)
			!
			FIND #GL_USERLIST.CH%, &
				KEY #0% GE GL_USERLIST::USER + &
				GL_USERLIST::ACCOUNT, &
				REGARDLESS
		CASE 1%
			!
			! FIND according to Primary Key
			!	(Object Mask)
			!
			FIND #GL_USERLIST.CH%, &
				KEY #0% GE GL_USERLIST::ACCOUNT + &
				GL_USERLIST::USER, &
				REGARDLESS
		END SELECT

	END SELECT

 ExitFunction:
	EXIT FUNCTION

	%PAGE

29000	!******************************************************************
	! Trap errors
	!******************************************************************

	ON ERROR GO BACK

32767	!******************************************************************
	! End of GL_MAIN_USERLIST function
	!******************************************************************
	END FUNCTION
