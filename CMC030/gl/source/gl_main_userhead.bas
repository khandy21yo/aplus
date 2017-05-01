1	%TITLE "GL User Defined Journal Header Maintenance"
	%SBTTL "GL_MAIN_USERHEAD"
	%IDENT "V3.6a Calico"

	FUNCTION LONG GL_MAIN_USERHEAD(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1998 BY
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
	! Software Solutions, Inc. assumes no responsibility for the
	! use or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Maintain User Defined Journal\* option
	!	maintains the ^*UJ\* journals.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_MAIN_USERHEAD/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP GL_MAIN_USERHEAD
	!	$ DELETE GL_MAIN_USERHEAD.OBJ;*
	!
	! Author:
	!
	!	11/18/1998 - Kevin Handy
	!
	! Modification history:
	!
	!	03/09/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	12/14/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	!
	! Maps
	!
	%INCLUDE "SOURCE:[GL.OPEN]GL_USERHEAD.HB"
	MAP (GL_USERHEAD)	GL_USERHEAD_CDD	GL_USERHEAD
	MAP (GL_USERHEAD_OLD)	GL_USERHEAD_CDD	GL_USERHEAD_OLD, GL_USERHEAD2

	%INCLUDE "SOURCE:[GL.OPEN]GL_USERDEF.HB"
	MAP	(GL_USERDEF)	GL_USERDEF_CDD	GL_USERDEF

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_GL_USERHEAD) &
		GL_USERHEAD.CH%, &
		GL_USERHEAD.READONLY%

	COM (TT_GL_USERJOUR) &
		BATCH_NO$ = 2%

	COM (CH_GL_USERDEF) &
		GL_USERDEF.CH%, &
		GL_USERDEF.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION FUNC_TESTENTRY

	!
	! Declare some variables
	!
	DECLARE RFA TEMP_RFA

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
		SMG_WINDOW::DESCR = "User Defined Journal " + BATCH_NO$ + &
			" Maintenance"
		SMG_WINDOW::NHELP = "GL_MAIN_USERHEAD"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 3%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Journal-number"
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
		IF GL_USERHEAD.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF GL_USERHEAD.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[GL.OPEN]GL_USERHEAD.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			GL_MAIN_USERHEAD = ERR
			CONTINUE 770
		END WHEN

		GL_USERHEAD.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[GL.OPEN]GL_USERHEAD.OPN"
		USE
			GL_MAIN_USERHEAD = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		GL_USERHEAD.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(GL_USERHEAD.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = GL_USERHEAD.CH%
		WHEN ERROR IN
			RESET #GL_USERHEAD.CH%
			GET #GL_USERHEAD.CH%, REGARDLESS
		USE
			CONTINUE 30000
		END WHEN

	!
	! Modify the menu
	!
	CASE OPT_OPTLIST
		MVALUE = MVALUE + " Line_items"

	!
	! Optional menu items
	!
5000	CASE OPT_MOREMENU
		SELECT SCOPE::PRG_ITEM

		!
		! Line option
		!
		CASE "Line_items"
	!++
	! Abstract:LINE_ITEMS
	!	^*Line__items\*
	!	.b
	!	.lm +5
	!	The ^*Line__items\* portion of this screen indicates how
	!	the transaction is to be allocated to various accounts and sub__codes.
	!	.b
	!	This portion of the screen will scroll, allowing as many as forty
	!	(40) line item distributions to be made.
	!	.lm -5
	!
	! Index:
	!
	!--
			!
			! Make sure there is a header
			!
			TEMP_RFA = GETRFA(GL_USERHEAD.CH%)

			GL_MAIN_USERHEAD = MAIN_JOURNAL(GL_MAIN_USERJOUR.ID, "")

		END SELECT

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

		DATA	1,  1, "(01) Code", &
			2,  1, "(02) Deposit", &
			3,  1, "(03) Date", &
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

		TEMP1% = SCOPE::SCOPE_EXIT

 E0Loop:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%
	!++
	! Abstract:FLD001
	!	.ts 55
	!	^*(01) Journal Code	4 Characters\*
	!	.b
	!	.lm +5
	!	The ^*Journal Code\* field maintains the
	!	reference code for a particular group of entries.
	!	.lm -5
	!
	! Index:
	!	.x Journal Code
	!
	!--

			GL_USERHEAD::JCODE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"1;16", TEMP$, &
				GL_USERHEAD::JCODE, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_USERDEF.ID, "VX  ") = 1%)
				THEN
					GL_USERHEAD::JCODE = &
						GL_USERDEF::JCODE
				END IF
				GOTO E0Loop
			END IF

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Deposit\*
	!	.b
	!	.lm +5
	!	The ^*Deposit\* field maintains the
	!	deposit number for the various bank accounts
	!	included in the journal
	!	.b
	!	The assumption is that all of the deposits will
	!	be deposited together.
	!	.lm -5
	!
	! Index:
	!
	!--
			GL_USERHEAD::DEPOSIT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"2;16", TEMP$, &
				GL_USERHEAD::DEPOSIT, MFLAG, &
				"'E", MVALUE)

		CASE 3%
	!++
	! Abstract:FLD003
	!	.ts 55
	!	^*(03) Date\*
	!	.b
	!	.lm +5
	!	The ^*Date\* field maintains the
	!	date that will be posted for all of the transactions.
	!	.b
	!	This journal is meant as a daily posting, so all the
	!	transactions are assumed to occur on the same day.
	!	.lm -5
	!
	! Index:
	!	.x Date
	!
	!--
			GL_USERHEAD::JDATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"3;16", TEMP$, &
				GL_USERHEAD::JDATE, MFLAG, "8", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		GL_MAIN_USERHEAD = 0%

		SELECT MLOOP

		CASE 1%
			IF GL_USERHEAD::JCODE = ""
			THEN
				GL_MAIN_USERHEAD = 1%
			END IF

			!
			! Is the input defined?
			!
			GL_MAIN_USERHEAD, ST% = FUNC_TESTENTRY(SMG_WINDOW, &
				GL_USERHEAD::JCODE, GL_USERDEF::DESCRIPTION, &
				"GL", MLOOP, "JCODE", &
				"JOURNAL CODE", GL_MAIN_USERDEF.ID)

		END SELECT

	!
	! Set GL_USERHEAD_OLD value
	!
20500	CASE OPT_SETOLD
		GL_USERHEAD_OLD = GL_USERHEAD

	!
	! Restore GL_USERHEAD_OLD value
	!
	CASE OPT_RESETOLD
		GL_USERHEAD = GL_USERHEAD_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		GL_USERHEAD2 = GL_USERHEAD

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		GL_USERHEAD = GL_USERHEAD2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = " Code  Depost   Date"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "007,014"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = GL_USERHEAD::JCODE + " " + &
				GL_USERHEAD::DEPOSIT + " " + &
				PRNT_DATE(GL_USERHEAD::JDATE, 8%)

		END SELECT
	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #GL_USERHEAD.CH%, &
				KEY #0% GE GL_USERHEAD::JCODE + "", &
				REGARDLESS
		END SELECT

	!
	! Handle finishing various options specially
	!
	CASE OPT_AFTEROPT

		SELECT SCOPE::PRG_ITEM

		!
		! Add records
		!
		CASE "Add"
			!
			! Add line items also
			!
			GL_MAIN_USERHEAD = MAIN_JOURNAL(GL_MAIN_USERJOUR.ID, "A")

		!
		! Change records
		!
		CASE "Change"
			!
			! Change line items to match new header
			! if the key was changed.
			!
			! The original record must be the one in the
			! MAP for this to be able to work.  The new
			! key is passed through the QUERY$ variable.
			!
			IF GL_USERHEAD_OLD::JCODE <> GL_USERHEAD::JCODE
			THEN
				TEMP$ = GL_USERHEAD::JCODE + ""
				GL_USERHEAD = GL_USERHEAD_OLD
				GL_MAIN_USERHEAD = MAIN_JOURNAL(GL_MAIN_USERJOUR.ID, &
					"C" + TEMP$)
			END IF

		!
		! Erase record
		!
		CASE "Erase"
			!
			! Erase any line items under the header
			!
			GL_MAIN_USERHEAD = MAIN_JOURNAL(GL_MAIN_USERJOUR.ID, "E")

		END SELECT

	END SELECT

	EXIT FUNCTION

29000	!***************************************************************
	! Trap errors
	!***************************************************************

	ON ERROR GO BACK

29900	!
	! Handle no header for Line function here
	!
	CALL ENTR_3MESSAGE(SCOPE, &
		"Sorry, but there is no current header item", 0%)
	GOTO 30000

30000	END FUNCTION
