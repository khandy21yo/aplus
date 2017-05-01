1	%TITLE "USER DEFINED REPORT TABLE"
	%SBTTL "GL_MAIN_VENCOL"
	%IDENT "V3.6a Calico"

	FUNCTION LONG GL_MAIN_VENCOL(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1988, 1989 BY
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
	! ID:1045
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*User Defined Report Table\* program creates
	!	a user defined report containing specified accounts with a defined key
	!	for fast and easy access.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_MAIN_VENCOL/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP GL_MAIN_VENCOL
	!	$ DELETE GL_MAIN_VENCOL.OBJ;*
	!
	! Author:
	!
	!	06/21/89 - Lance Williams
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
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%PAGE

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[GL.OPEN]GL_VENCOL.HB"
	MAP	(GL_VENCOL)	GL_VENCOL_CDD	GL_VENCOL
	MAP	(GL_VENCOL_OLD)	GL_VENCOL_CDD	GL_VENCOL_OLD, GL_VENCOL2


	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_GL_VENCOL) &
		GL_VENCOL.CH%, &
		GL_VENCOL.READONLY%

	!
	! Declare constants and/or variables
	!
	DECLARE	LONG	CONSTANT	MAX_COLUMNS = 6%
	DECLARE	LONG			XLONG, YLONG
	%PAGE

	!
	! Set up list of choices for field 8 in the initial screen
	!
	ECTITLE$ = "Code Description"
	EC$(0%) = "3"
	EC$(1%) = "D    Detail"
	EC$(2%) = "X    XREF"
	EC$(3%) = "S    Store"

	ON ERROR GOTO 29000

	%PAGE

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
		SMG_WINDOW::DESCR = "User Defined Report Table"
		SMG_WINDOW::NHELP = "GL_MAIN_VENCOL"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 10%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Record Key"
		SMG_WINDOW::KFIELD(0%, 0%) = 1%
		SMG_WINDOW::KFIELD(0%, 1%) = 2%

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF GL_VENCOL.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF GL_VENCOL.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[GL.OPEN]GL_VENCOL.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			GL_MAIN_VENCOL = ERR
			CONTINUE 770
		END WHEN

		GL_VENCOL.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[GL.OPEN]GL_VENCOL.OPN"
		USE
			GL_MAIN_VENCOL = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		GL_VENCOL.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(GL_VENCOL.CH%)

		GOTO ExitFunction

790		SMG_WINDOW::CHAN  = GL_VENCOL.CH%
		WHEN ERROR IN
			RESET #GL_VENCOL.CH%
			GET #GL_VENCOL.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

20100	!
	! Display the background
	!
	! This option is used to display the background information
	! on the screen.  It must first clear any junk on the screen,
	! and then write the background onto it.
	!
	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	2, 1, "(01) Key", &
			6, 1, "(02) Column 1", &
			8, 1, "(03) Column 2", &
			10, 1, "(04) Column 3", &
			12, 1, "(05) Column 4", &
			14, 1, "(06) Column 5", &
			16, 1, "(07) Column 6", &
			2, 45, "(08) Det/Summ", &
			3, 45, "(09) Title", &
			5, 16, "Title A/B     Account Wildcard", &
			0, 0, ""

		RESTORE
		READ XLONG, YLONG, ATEXT$

		WHILE (XLONG <> 0%)
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				ATEXT$, XLONG, YLONG)
			READ XLONG, YLONG, ATEXT$
		NEXT

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

20200	!
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user,
	! display data, set defaults, and return the data back
	! according to MFLAG.
	!
	CASE OPT_ENTRY
		TEMP$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 Reenter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Key\*
	!	.b
	!	.lm +5
	!	^*Key\* defines or labels the report so it can be found for later use and
	!	recognition.
	!	.lm -5
	!
	! Index:
	!
	!--
			GL_VENCOL::RECKEY = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "2;16", TEMP$, &
				GL_VENCOL::RECKEY, MFLAG, "'E", MVALUE)

		CASE 2% TO 7%
			SCOPE::PRG_ITEM = "FLD002COL"
	!++
	! Abstract:FLD002COL
	!	^*(02) Column 1 - (07) Column 6\*
	!	.b
	!	.lm +5
	!	Each ^*Column\* field enters a two line column heading
	!	and General Ledger account selection.  The right-most character position
	!	aligns in its respective column directly above the right-most position
	!	of the amounts displayed.
	!	.b
	!	The account selection can be made by entering a specific General Ledger
	!	account, a series of accounts separated by commas, or by utilizing wildcard
	!	techniques.
	!	.lm -5
	!
	! Index:
	!
	!--

			GL_VENCOL::COL_TITLEA(MLOOP - 1%) = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				NUM1$(MLOOP * 2% + 2%) + ";16", TEMP$, &
				GL_VENCOL::COL_TITLEA(MLOOP - 1%), &
				MFLAG, "'E", MVALUE)

			GL_VENCOL::COL_TITLEB(MLOOP - 1%) = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				NUM1$(MLOOP * 2% + 3%) + ";16", TEMP$, &
				GL_VENCOL::COL_TITLEB(MLOOP - 1%), &
				MFLAG, "'E", MVALUE)

			GL_VENCOL::COL_ACCOUNT(MLOOP - 1%) = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				NUM1$(MLOOP * 2% + 2%) + ";31", TEMP$, &
				GL_VENCOL::COL_ACCOUNT(MLOOP - 1%), &
				MFLAG, "'E", MVALUE)

		CASE 8%

	!++
	! Abstract:FLD008
	!	^*(08) Detail/Summary\*
	!	.b
	!	.lm +5
	!	The Detail/Summary field in the ^*User Defined Summary/Detail Report\*
	!	setting screen controls the level of detail which will
	!	be displayed:
	!	.b
	!	Valid Settings are:
	!	.table 3,25
	!	.te
	!	^*D\* - Detail
	!	.te
	!	^*S\* - Summary
	!	.end table
	!	The detail report lists, for every account selection, each specific
	!	transaction in the General Ledger including specific account numbers,
	!	cross-reference, cross-reference description, and date of the
	!	transaction. There is a page break after the information is printed
	!	for a specific location.
	!	.b
	!	The Summary report lists a line item for each location and a total of all
	!	period transactions for each defined selection.
	!	.lm -5
	!
	! Index:
	!
	!--
			GL_VENCOL::COL_FLAG = ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, "2;60", &
				TEMP$, GL_VENCOL::COL_FLAG, MFLAG, &
				"'", MVALUE, EC$(), ECTITLE$, "007")

		CASE 9%

	!++
	! Abstract:FLD009
	!	^*(09) Title\*
	!	.b
	!	.lm +5
	!	The ^*Title\* field allows for entry of a title for a specific report.
	!	.lm -5
	!
	! Index:
	!
	!--
			GL_VENCOL::COL_TITLE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "3;60", TEMP$, &
				GL_VENCOL::COL_TITLE, MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP$

20500	!******************************************************************
	! Set GL_VENCOL_OLD value
	!******************************************************************
	CASE OPT_SETOLD
		GL_VENCOL_OLD = GL_VENCOL


	!******************************************************************
	! Restore GL_VENCOL_OLD value
	!******************************************************************
	CASE OPT_RESETOLD
		GL_VENCOL = GL_VENCOL_OLD


	!******************************************************************
	! Set default value
	!******************************************************************
	CASE OPT_SETDEFAULT
		GL_VENCOL2 = GL_VENCOL


	!******************************************************************
	! Restore default value
	!******************************************************************
	CASE OPT_RESETDEFAULT
		GL_VENCOL= GL_VENCOL2


	!***********************************************************
	! View header
	!***********************************************************
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = " RecordKey   Column A      Column B" + &
				"     Account   Flag     Title "

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "012,025,038,049,057"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = GL_VENCOL::RECKEY + "   " + &
				GL_VENCOL::COL_TITLEA(MLOOP - 1%) + "  " + &
				GL_VENCOL::COL_TITLEB(MLOOP - 1%) + " " + &
				GL_VENCOL::COL_ACCOUNT(MLOOP - 1%) + "  " + &
				GL_VENCOL::COL_FLAG + "   " + &
				GL_VENCOL::COL_TITLE

		END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		FIND #GL_VENCOL.CH%, &
			KEY #0% GE GL_VENCOL::RECKEY + "", &
			REGARDLESS

	END SELECT

 ExitFunction:
	EXIT FUNCTION

	%PAGE

29000	!***************************************************************
	! Trap errors
	!***************************************************************

	ON ERROR GO BACK

32767	!***********************************************************
	! End of GL_MAIN_VENCOL function
	!***********************************************************
	END FUNCTION
