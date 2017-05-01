1	%TITLE "Object Control File"
	%SBTTL "AD_MAIN_CONTROLOBJ"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AD_MAIN_CONTROLOBJ(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1987 BY
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
	! Computer Management Center, Inc.
	!
	! CMC assumes no responsibility for the use or reliability of
	! its software on equipment which is not supported by CMC.
	!
	!++
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	In the ^*Object\* controlling file, one record for each
	!	object from the Object Description File is stored.  This
	!	record contains information concerning the last update
	!	and the last depreciated period for a particular Object
	!	and processing status flag.  Each object may have a different
	!	accounting era.
	!	.lm -5
	!
	! Index:
	!	.x Object
	!	.x Accounting Era
	!	.x Last Updated Period
	!	.x Last Depreciated Period
	!	.x Period>Last Updated
	!	.x Period>Last Depreciated
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_MAIN_CONTROLOBJ/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN AD_MAIN_CONTROLOBJ
	!	$ DELETE AD_MAIN_CONTROLOBJ.OBJ;*
	!
	! Author:
	!
	!	12/09/87 - Frank F. Starman
	!
	! Modification history:
	!
	!	03/18/92 - Dan Perkins
	!		Remove variable FILENAME$.
	!
	!	04/20/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	04/21/92 - Kevin Handy
	!		Clean up (check)
	!
	!	03/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/05/95 - Kevin Handy
	!		(V3.6)
	!		Update source to V3.6 standards.
	!
	!	04/12/95 - Kevin Handy
	!		Change scope.exit to scope::scope_exit
	!
	!	10/02/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/19/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/09/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	11/08/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--

	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:AD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[AD.OPEN]AD_CONTROLOBJ.HB"
	MAP (AD_CONTROLOBJ)	AD_CONTROLOBJ_CDD	AD_CONTROLOBJ
	MAP (AD_CONTROLOBJ_OLD) AD_CONTROLOBJ_CDD AD_CONTROLOBJ_OLD, &
		AD_CONTROLOBJ2

	%INCLUDE "SOURCE:[AD.OPEN]AD_OBJECT.HB"
	MAP (AD_OBJECT)		AD_OBJECT_CDD		AD_OBJECT

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_ERA.HB"
	MAP (UTL_ERA)		UTL_ERA_CDD		UTL_ERA

	MAP (CH_AD_CONTROLOBJ) AD_CONTROLOBJ.CH%

	COM (TT_AD_CONTROLOBJ) &
		CLOSETITLE$ = 20%, &
		CLOSETYPE$(6%) = 20%

	!
	! External functions
	!
	EXTERNAL LONG FUNCTION	FUNC_TESTENTRY

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	!
	! Initilization
	!
	CASE OPT_INIT

		!*************************************************************
		! Set up information
		!*************************************************************

		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Object control file"
		SMG_WINDOW::NHELP = "AD_MAIN_CONTROLOBJ"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 11%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Object"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%

		CLOSETITLE$ = "Type   Description"
		CLOSETYPE$(0%) = "6"
		CLOSETYPE$(1%) = "0    No status"
		CLOSETYPE$(2%) = "1    Updating"
		CLOSETYPE$(3%) = "2    Resetting"
		CLOSETYPE$(4%) = "3    Depreciating"
		CLOSETYPE$(5%) = "4    Posting to GL"
		CLOSETYPE$(6%) = "5    Posting units"

		CALL READ_DEFAULTS(SMG_WINDOW)

700		IF (AD_CONTROLOBJ.CH% <= 0%)
		THEN
			!
			! Open main file (existing) for modification
			!
			WHEN ERROR IN
				%INCLUDE "SOURCE:[AD.OPEN]AD_CONTROLOBJ.CRE"
			USE
				CALL ENTR_3MESSAGE(SCOPE, &
					"Unable to open AD_CONTROLOBJ file " + NUM1$(ERR), 0%)
				AD_MAIN_CONTROLOBJ = 1%
				CONTINUE 27000
			END WHEN
		END IF

710		SMG_WINDOW::CHAN  = AD_CONTROLOBJ.CH%

		WHEN ERROR IN
			RESET #AD_CONTROLOBJ.CH%
			GET #AD_CONTROLOBJ.CH%, REGARDLESS

		USE
			CONTINUE 27000 IF ERR = 11%
			EXIT HANDLER
		END WHEN

	!
	! Display window background
	!
	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)


		DATA	05,05, "(01) Object ", &
			06,05, "(02) Era Code", &
			07,05, "(03) Last Period Updated", &
			08,05, "(04) Last Period Depreciated", &
			09,05, "(05) Status Flag", &
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
20200	CASE OPT_ENTRY
		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")
 ReEnter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Object\*
	!	.b
	!	.lm +5
	!	The ^*Object\* field enters each applicable
	!	object code from the Object Description file.
	!	.b
	!	Pressing ^*<List Choices>\* while the cursor is located at
	!	this field will provide a list of valid object codes.
	!	.lm -5
	!
	! Index:
	!	.x Object
	!
	!--

			AD_CONTROLOBJ::DEP_OBJECT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"05;36", TEMP$, &
				AD_CONTROLOBJ::DEP_OBJECT, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(AD_MAIN_OBJECT.ID, "V0") = 1%
				THEN
				AD_CONTROLOBJ::DEP_OBJECT = &
						AD_OBJECT::DEP_OBJECT
				END IF
				GOTO Reenter
			END IF

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Era Code\*
	!	.b
	!	.lm +5
	!	The ^*Era Code\* field selects an
	!	accounting Era from the Profile File for the particular
	!	Object.
	!	.b
	!	This field may contain up to two (2) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Era Code
	!
	!--

			AD_CONTROLOBJ::ERA = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"06;36", TEMP$, &
				AD_CONTROLOBJ::ERA, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UTL_MAIN_ERA.ID, "V0  ") = 1%
				THEN
					AD_CONTROLOBJ::ERA = &
						UTL_ERA::ERA
				END IF
				GOTO Reenter
			END IF

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Last Period Updated\*
	!	.b
	!	.lm +5
	!	The ^*Last Period Updated\* field will automatically
	!	be updated when the Asset Ledger is posted to the Asset
	!	Balances and History File.
	!	.b
	!	The format for entry is YYYYPP.
	!	.lm -5
	!
	! Index:
	!
	!--

			AD_CONTROLOBJ::LASTPER = &
				ENTR_PERIOD(SMG_WINDOW::WNUMBER, &
				"07;36", TEMP$, AD_CONTROLOBJ::LASTPER, &
				MFLAG, AD_CONTROLOBJ::ERA, MVALUE)

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Last Period Depreciated\*
	!	.b
	!	.lm +5
	!	The ^*Last Period Depreciated\* field will be automatically
	!	updated when the depreciation calculation for a particular object
	!	is executed.
	!	.b
	!	The format for entry is YYYYPP.
	!	.lm -5
	!
	! Index:
	!	.x Last Period Depreciated
	!
	!--

			AD_CONTROLOBJ::LASTDEP = &
				ENTR_PERIOD(SMG_WINDOW::WNUMBER, &
				"08;36", TEMP$, AD_CONTROLOBJ::LASTDEP, &
				MFLAG, AD_CONTROLOBJ::ERA, MVALUE)


		CASE 5%

	!++
	! Abstract:FLD005
	!	^*(05) Status Flag\*
	!	.b
	!	.lm +5
	!	The ^*Status Flag\* indicates one of six possible processes
	!	for a particular object.
	!	.b
	!	.lm 10
	!	.list 0,"*"
	!	.le
	!	^*0\* = No status (Not running or process interrupted)
	!	.le
	!	^*1\* = Updating (Update the Asset Ledger to the Asset
	!	Balances and History File)
	!	.le
	!	^*2\* = Resetting (Resetting the Asset Balances and History
	!	File)
	!	.le
	!	^*3\* = Depreciating (Calculating depreciation for a period)
	!	.le
	!	^*4\* = Posting to GL (Post Asset Ledger to the General Ledger)
	!	.le
	!	^*5\* = Posting Units (Post Units Depreciation Journal to the
	!	Units Register File)
	!	.els
	!
	! Index:
	!	.x Status Flag
	!
	!--

			AD_CONTROLOBJ::STATUS_FLAG = &
				EDIT$(ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"09;36", TEMP$, AD_CONTROLOBJ::STATUS_FLAG, &
				MFLAG, "'", MVALUE, CLOSETYPE$(), &
				CLOSETITLE$, "005"), -1%)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		AD_MAIN_CONTROLOBJ = 0%

		SELECT MLOOP

		CASE 1%
			!
			! Is input defined?
			!
			AD_MAIN_CONTROLOBJ = FUNC_TESTENTRY(SMG_WINDOW, &
				AD_CONTROLOBJ::DEP_OBJECT, &
				AD_OBJECT::DESCRIPTION, &
				"AD", MLOOP, "OBJ", &
				"Object", AD_MAIN_OBJECT.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AD_OBJECT::DESCRIPTION, 5%, 43%, , SMG$M_BOLD)

			IF (MVALUE = "ADD")
			THEN
				WHEN ERROR IN
					GET #SMG_WINDOW::CHAN, &
						KEY #0% EQ AD_CONTROLOBJ::DEP_OBJECT + "", &
						REGARDLESS
				USE
					CONTINUE 32767 IF ERR = 155%
					EXIT HANDLER
				END WHEN

				AD_MAIN_CONTROLOBJ = 2%
				CALL ENTR_3MESSAGE(SCOPE, &
					"Record Already Exists", 0%)
			END IF

		CASE 2%
			!
			! Is the input defined?
			!
			AD_MAIN_CONTROLOBJ = FUNC_TESTENTRY(SMG_WINDOW, &
				AD_CONTROLOBJ::ERA, &
				UTL_ERA::DESCRIPTION, &
				"AD", MLOOP, "DES", &
				"Era", UTL_MAIN_ERA.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_ERA::DESCRIPTION, 6%, 43%, , SMG$M_BOLD)

		END SELECT

	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(1%) AND 2%) = 0%
		THEN
			DESCRIPTION$ = STRING$(20%, 63%)
			DESCRIPTION$ = AD_OBJECT::DESCRIPTION &
				IF MAIN_WINDOW(AD_MAIN_OBJECT.ID, &
				"Q0" + AD_CONTROLOBJ::DEP_OBJECT) = 1%

			!
			! Display name (ALSO IN TESTENTRY)
			!
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				DESCRIPTION$, 5%, 43%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(2%) AND 2%) = 0%
		THEN
			DESCRIPTION$ = STRING$(20%, 63%)
			DESCRIPTION$ = UTL_ERA::DESCRIPTION &
				IF MAIN_WINDOW(UTL_MAIN_ERA.ID, &
				"Q0" + AD_CONTROLOBJ::ERA) = 1%
			!
			! Display name (ALSO IN TESTENTRY)
			!
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				DESCRIPTION$, 6%, 43%, , SMG$M_BOLD)
		END IF

	!
	! Set AD_CONTROLOBJ_OLD value
	!
20500	CASE OPT_SETOLD
		AD_CONTROLOBJ_OLD = AD_CONTROLOBJ

	!
	! Restore AD_CONTROLOBJ_OLD value
	!
	CASE OPT_RESETOLD
		AD_CONTROLOBJ = AD_CONTROLOBJ_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		AD_CONTROLOBJ2 = AD_CONTROLOBJ

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		AD_CONTROLOBJ = AD_CONTROLOBJ2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Object Year PerUpd PerDep Status"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "009,014,021,028"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = AD_CONTROLOBJ::DEP_OBJECT + "      " + &
				AD_CONTROLOBJ::ERA + "   " + &
				AD_CONTROLOBJ::LASTPER + " " + &
				AD_CONTROLOBJ::LASTDEP + " " + &
				AD_CONTROLOBJ::STATUS_FLAG

			END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP
		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE AD_CONTROLOBJ::DEP_OBJECT + "", &
				REGARDLESS

		END SELECT

	END SELECT

27000	EXIT FUNCTION

29000	!
	! Trap errors
	!
	ON ERROR GO BACK

32767	END FUNCTION

