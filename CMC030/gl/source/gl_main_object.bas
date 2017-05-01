1	%TITLE "GL Account Object Maintenance"
	%SBTTL "GL_MAIN_OBJECT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG GL_MAIN_OBJECT(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

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
	!	$ BAS GL_SOURCE:GL_MAIN_OBJECT/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP GL_MAIN_OBJECT
	!	$ DELETE GL_MAIN_OBJECT.OBJ;*
	!
	! Author:
	!
	!	06/24/88 - Aaron Redd
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/18/96 - Kevin Handy
	!		Reformat source code
	!
	!	05/27/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/28/97 - Kevin Handy
	!		Lose unecessary external definitions
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
	!	11/10/2000 - Kevin Handy
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

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[GL.OPEN]GL_OBJECT.HB"
	MAP	(GL_OBJECT)	GL_OBJECT_CDD	GL_OBJECT
	MAP	(GL_OBJECT)	GL_OBJECT_CDD	GL_OBJECT_OLD,	GL_OBJECT2

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_GL_OBJECT) &
		GL_OBJECT.CH%, &
		GL_OBJECT.READONLY%
	COM (TT_GL_OBJECT) &
		ACCTITLE$ = 20%, &
		ACCTYPE$(6%) = 20%, &
		SUMTITLE$ = 20%, &
		SUMMARY$(4%) = 20%

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
		SMG_WINDOW::DESCR = "Account Object Maintenance"
		SMG_WINDOW::NHELP = "GL_MAIN_OBJECT"
		SMG_WINDOW::CHAN  = GL_OBJECT.CH%
		SMG_WINDOW::HSIZE = 60%
		SMG_WINDOW::VSIZE = 14%
		SMG_WINDOW::HVIEW = 60%
		SMG_WINDOW::VVIEW = 14%
		SMG_WINDOW::HPOS  = 10%
		SMG_WINDOW::VPOS  = 4%
		SMG_WINDOW::VHPOS = 10%
		SMG_WINDOW::VVPOS = 4%
		SMG_WINDOW::NITEMS= 7%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Object"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		!
		! Load in defaults
		!
		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

		!
		! Define account type
		!
		ACCTITLE$ = "Type   Description"
		ACCTYPE$(0%) = "6"
		ACCTYPE$(1%) = "A    Assets"
		ACCTYPE$(2%) = "L    Liability"
		ACCTYPE$(3%) = "O    Owners Equity"
		ACCTYPE$(4%) = "S    Income Summary"
		ACCTYPE$(5%) = "R    Revenue"
		ACCTYPE$(6%) = "E    Expense"

		!
		! Define summary flag
		!
		SUMTITLE$ = "Code   Description"
		SUMMARY$(0%) = "4"
		SUMMARY$(1%) = "1    Full detail"
		SUMMARY$(2%) = "2    By date"
		SUMMARY$(3%) = "3    One line"
		SUMMARY$(4%) = "4    By batch"

700		!
		! Declare channels
		!
		IF GL_OBJECT.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF GL_OBJECT.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[GL.OPEN]GL_OBJECT.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			GL_MAIN_OBJECT = ERR
			CONTINUE 770
		END WHEN

		GL_OBJECT.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[GL.OPEN]GL_OBJECT.OPN"
		USE
			GL_MAIN_OBJECT = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		GL_OBJECT.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(GL_OBJECT.CH%)

		GOTO ExitFunction

790		SMG_WINDOW::CHAN  = GL_OBJECT.CH%
		WHEN ERROR IN
			RESET #GL_OBJECT.CH%
			GET #GL_OBJECT.CH%, REGARDLESS
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

		DATA	3, 10, "(01) Account Object", &
			4, 10, "(02) Description Object", &
			5, 10, "(03) Type", &
			6, 10, "(04) Summary Flag", &
			9, 10, "(05) Cash Flow Object", &
			10, 10, "(06) Work Capital Object", &
			11, 10, "(07) Financial Type Object", &
			8, 10, "** Financial Report Codes **", &
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

			GL_OBJECT::OBJ_MASK = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"3;38", TEMP$, GL_OBJECT::OBJ_MASK, &
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

			GL_OBJECT::DESCR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"4;38", TEMP$, GL_OBJECT::DESCR, &
				MFLAG, "'E", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	.x Object>Account Type
	!	^*(03) Type\*
	!	.b
	!	.lm +5
	!	This field is used to code which ^*Type\* of account is being added.
	!	Valid codes are defined as follows:
	!	.TABLE 3,25
	!	.te
	!	^*A\* - Asset (Real account)
	!	.TE
	!	^*L\* - Liability (Real account)
	!	.TE
	!	^*O\* - Owners Equity (Real account)
	!	.TE
	!	^*S\* - Income Summary (Real account)
	!	.TE
	!	^*R\* - Revenue (Nominal account)
	!	.TE
	!	^*E\* - Expense (Nominal account)
	!	.END TABLE
	!	Pressing ^*List Choices\* will display all valid ^*Types\*.
	!	.lm -5
	!
	! Index:
	!	.x Account Type>Object
	!
	!--

			GL_OBJECT::ACCT_TYPE = ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"5;38", TEMP$, GL_OBJECT::ACCT_TYPE, &
				MFLAG, "'", MVALUE, ACCTYPE$(), &
				ACCTITLE$, "005")

		CASE 4%

	!++
	! Abstract:FLD004
	!	.x Summary Flag
	!	^*(04) Summary Flag\*
	!	.b
	!	.lm +5
	!	This field is used to control the level of detail to
	!	be printed in the General Ledger report. Valid flags are
	!	^*1-4\* and are defined as follows:
	!	.TABLE 3,25
	!	.te
	!	^*1\* - Complete detail
	!	.TE
	!	^*2\* - Summarize by date
	!	.TE
	!	^*3\* - Summarize, print only one line showing net change
	!	.TE
	!	^*4\* - Summarize by batch number
	!	.END TABLE
	!	Pressing ^*List Choices\* will display all valid flags.
	!	.lm -5
	!
	! Index:
	!	.x Object>Summary Flag
	!
	!--

			GL_OBJECT::SUMM_FLAG = ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"6;38", TEMP$, GL_OBJECT::SUMM_FLAG, &
				MFLAG, "'", MVALUE, SUMMARY$(), &
				SUMTITLE$, "005")

		CASE 5%

	!++
	! Abstract:FLD005
	!	.x Object>Cash Flow Code
	!	^*(05) Cash Flow Object\*
	!	.b
	!	.lm +5
	!	This field is used to define how each account will be presented
	!	in the ^*Cash Flow\* statement.
	!	.b
	!	Four spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!	.x Cash Flow Code>Object
	!
	!--

			GL_OBJECT::CASH_FLOW = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "9;38", TEMP$, &
				GL_OBJECT::CASH_FLOW, MFLAG, "'E", MVALUE)

		CASE 6%

	!++
	! Abstract:FLD006
	!	.x Object>Working Capital Code
	!	^*(06) Work Capital Object\*
	!	.b
	!	.lm +5
	!	The ^*Work Capital Object\* field is used to define how each
	!	account will be presented in the statement of working capital.
	!	.b
	!	Four spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!	.x Working Capital Code>Object
	!
	!--

			GL_OBJECT::WORK_CAPT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "10;38", TEMP$, &
				GL_OBJECT::WORK_CAPT, MFLAG, "'E", MVALUE)

		CASE 7%

	!++
	! Abstract:FLD007
	!	.x Object>Financial Type
	!	^*(07) Financial Type Object\*
	!	.b
	!	.lm +5
	!	The ^*Financial Type Object\* field is used to define how each
	!	account will be presented in the financial statements.
	!	.b
	!	Ten spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!	.x Financial Type/Object
	!
	!--

			GL_OBJECT::FIN_TYPE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "11;38", TEMP$, &
				GL_OBJECT::FIN_TYPE, MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP$

20300	!******************************************************************
	! Test values
	!******************************************************************
	CASE OPT_TESTENTRY
		GL_MAIN_OBJECT = 0%

		SELECT MLOOP

		CASE 1%
			IF GL_OBJECT::OBJ_MASK = ""
			THEN
				GL_MAIN_OBJECT = 1%
			ELSE
				IF (MVALUE = "ADD")
				THEN
					WHEN ERROR IN
						GET #GL_OBJECT.CH%, &
							KEY #0% EQ GL_OBJECT::OBJ_MASK + "", &
							REGARDLESS
					USE
						CONTINUE 32767 IF ERR = 155%
						EXIT HANDLER
					END WHEN

					GL_MAIN_OBJECT = 2%
					CALL ENTR_3MESSAGE(SCOPE, &
						"Record Already Exists", 1%)
				END IF
			END IF
		END SELECT

20500	!******************************************************************
	! Set GL_OBJECT_OLD value
	!******************************************************************
	CASE OPT_SETOLD
		GL_OBJECT_OLD = GL_OBJECT

	!******************************************************************
	! Restore GL_OBJECT_OLD value
	!******************************************************************
	CASE OPT_RESETOLD
		GL_OBJECT = GL_OBJECT_OLD

	!******************************************************************
	! Set default value
	!******************************************************************
	CASE OPT_SETDEFAULT
		GL_OBJECT2 = GL_OBJECT

	!******************************************************************
	! Restore default value
	!******************************************************************
	CASE OPT_RESETDEFAULT
		GL_OBJECT = GL_OBJECT2

	!***********************************************************
	! View header
	!***********************************************************
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Object             Description"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "021"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = GL_OBJECT::OBJ_MASK + " " + &
				GL_OBJECT::DESCR

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
			FIND #GL_OBJECT.CH%, &
				KEY #0% GE GL_OBJECT::OBJ_MASK + "", &
				REGARDLESS
		END SELECT

	END SELECT

 ExitFunction:
	EXIT FUNCTION

29000	!******************************************************************
	! Trap errors
	!******************************************************************

	ON ERROR GO BACK

32767	!******************************************************************
	! End of GL_MAIN_OBJECT function
	!******************************************************************
	END FUNCTION
