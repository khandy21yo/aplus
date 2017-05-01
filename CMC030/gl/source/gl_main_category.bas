1	%TITLE "CHART OF ACCOUNTS CATEGORY MAINTENANCE"
	%SBTTL "GL_MAIN_CATEGORY"
	%IDENT "V3.6a Calico"

	FUNCTION LONG GL_MAIN_CATEGORY(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1989 BY
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
	! ID:1000
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	By using the ^*Sub Ledger Category Maintenance\*, the categories may be
	!	described and maintained. This is accomplished through the ^*Description\* and
	!	the ^*Title description.\*
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_MAIN_CATEGORY/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN GL_MAIN_CATEGORY
	!	$ DELETE GL_MAIN_CATEGORY.OBJ;*
	!
	! Author:
	!
	!	02/16/89 - B. Craig Larsen
	!
	! Modification history:
	!
	!	03/26/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/04/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/01/97 - Kevin Handy
	!		Reformat source code
	!		Use integer for #key
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
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
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[GL.OPEN]GL_CATEGORY.HB"
	MAP	(GL_CATEGORY)		GL_CATEGORY_CDD	GL_CATEGORY
	MAP	(GL_CATEGORY_OLD)	GL_CATEGORY_CDD	GL_CATEGORY_OLD, &
		GL_CATEGORY2

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_GL_CATEGORY) &
		GL_CATEGORY.CH%, &
		GL_CATEGORY.READONLY%

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

		!**************************************************************
		! Set up information
		!**************************************************************

		!
		! Define SMG_WINDOW
		!
		SMG_WINDOW::DESCR = "Chart of Accounts Category Description"
		SMG_WINDOW::NHELP = "GL_MAIN_CATEGORY"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 3%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Category"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%

		!
		! Category
		!

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF GL_CATEGORY.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if it was
			! that way from the last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF GL_CATEGORY.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[GL.OPEN]GL_CATEGORY.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			GL_MAIN_CATEGORY = ERR
			CONTINUE 770
		END WHEN

		GL_CATEGORY.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[GL.OPEN]GL_CATEGORY.OPN"
		USE
			GL_MAIN_CATEGORY = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		GL_CATEGORY.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(GL_CATEGORY.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = GL_CATEGORY.CH%
		WHEN ERROR IN
			RESET #GL_CATEGORY.CH%
			GET #GL_CATEGORY.CH%, REGARDLESS
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


		DATA	05,05, "(01) Category", &
			07,05, "(02) Description", &
			08,05, "(03) Title Description", &
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

 ReEnter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

			GL_CATEGORY::CATEGORY = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"5;28",TEMP$, GL_CATEGORY::CATEGORY, &
				MFLAG, "'E", MVALUE)
	!++
	! Abstract:FLD001
	!	^*(01) Category\*
	!	.b
	!	.lm +5
	!	^*Category\* refers to the type of category in which the account is classified.
	!	.b
	!	Example: If the account relates to Job Costing, an account category would
	!	be revenue, labor, materials, or overhead.  If the user has a need to print
	!	General Ledgers or Trial Balances for selected categories of accounts, those
	!	categories can be defined by utilizing this field.
	!	.b
	!	Four spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!
	!--

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Description\*
	!	.b
	!	.lm +5
	!	The ^*Description\* field is to contain a brief description of
	!	the account number entered in field (01).
	!	.b
	!	Forty spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!
	!--

			GL_CATEGORY::DESCR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"7;28",	TEMP$, GL_CATEGORY::DESCR, &
				MFLAG, "'E", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Title Description\*
	!	.b
	!	.lm +5
	!	^*Title Description\* refers to the shortened description of the account
	!	for the purpose of using it in titles.
	!	.b
	!	The field will accept 10 characters.
	!	.lm -5
	!
	! Index:
	!
	!--


			GL_CATEGORY::TITLEDESC = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"8;28",	TEMP$, GL_CATEGORY::TITLEDESC, &
				MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		GL_MAIN_CATEGORY = 0%

		SELECT MLOOP

		CASE 1%
			IF (MVALUE = "ADD")
			THEN
				WHEN ERROR IN
					GET #SMG_WINDOW::CHAN, KEY #0% EQ &
						GL_CATEGORY::CATEGORY + "", &
						REGARDLESS
				USE
					CONTINUE 32767 IF ERR = 155%
					EXIT HANDLER
				END WHEN

				GL_MAIN_CATEGORY = 2%
				CALL ENTR_3MESSAGE(SCOPE, &
					"Record Already Exists", 1%)
			END IF

		END SELECT

	CASE OPT_DISPLAY

	!
	! Set GL_CATEGORY_OLD value
	!
20500	CASE OPT_SETOLD
		GL_CATEGORY_OLD = GL_CATEGORY

	!
	! Restore GL_CATEGORY_OLD value
	!
	CASE OPT_RESETOLD
		GL_CATEGORY = GL_CATEGORY_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		GL_CATEGORY2 = GL_CATEGORY

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		GL_CATEGORY = GL_CATEGORY2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%

			MVALUE ="  Category   Description"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "011"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = "  " + &
				GL_CATEGORY::CATEGORY+ "     " + &
				GL_CATEGORY::DESCR

		END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP
		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE GL_CATEGORY::CATEGORY + "", &
				REGARDLESS
		END SELECT

	END SELECT

27000	EXIT FUNCTION

29000	!
	! Trap errors
	!
	ON ERROR GO BACK

32767	END FUNCTION
