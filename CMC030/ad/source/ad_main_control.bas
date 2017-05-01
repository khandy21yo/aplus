1	%TITLE "Object for General Ledger"
	%SBTTL "AD_MAIN_CONTROL"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AD_MAIN_CONTROL(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
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
	!	The ^*Controlling File\* screen is used to select one
	!	of the ^*Objects\* from the Object description file, to
	!	be posted to the General Ledger File.
	!	.lm -5
	!
	! Index:
	!	.x Controlling File
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_MAIN_CONTROL/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN AD_MAIN_CONTROL
	!	$ DELETE AD_MAIN_CONTROL.OBJ;*
	!
	! Author:
	!
	!	12/09/87 - Frank F. Starman
	!
	! Modification history:
	!
	!	04/20/92 - Dan Perkins
	!		Use FUN_TESTENTRY to test input.
	!
	!	04/21/92 - Kevin Handy
	!		Clean up (check)
	!
	!	03/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/05/95 - Kevin Handy
	!		(V3.6)
	!		Update for V3.6 source standards.
	!
	!	04/12/95 - Kevin Handy
	!		Change scope.exit to scope::scope_exit
	!
	!	10/02/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
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
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[AD.OPEN]AD_CONTROL.HB"
	MAP (AD_CONTROL)	AD_CONTROL_CDD		AD_CONTROL
	MAP (AD_CONTROL_OLD)	AD_CONTROL_CDD		AD_CONTROL_OLD, AD_CONTROL2

	MAP (CH_AD_CONTROL) AD_CONTROL.CH%

	%INCLUDE "SOURCE:[AD.OPEN]AD_OBJECT.HB"
	MAP (AD_OBJECT)		AD_OBJECT_CDD		AD_OBJECT

	%INCLUDE "SOURCE:[AD.OPEN]AD_CONTROLOBJ.HB"
	MAP (AD_CONTROLOBJ)	AD_CONTROLOBJ_CDD	AD_CONTROLOBJ

	COM (TT_AD_CONTROL) &
		CLOSETITLE$ = 20%, &
		CLOSETYPE$(2%) = 20%

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION FUNC_TESTENTRY

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	!
	! Initilization
	!
	CASE OPT_INIT

		!******************************************************************
		! Set up information
		!******************************************************************

		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Object to GL control file"
		SMG_WINDOW::NHELP = "AD_MAIN_CONTROL"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::FLAGS = 128% !Relative file
		SMG_WINDOW::NITEMS= 2%

		SMG_WINDOW::NKEYS = 0%

20010		GOTO 20020 IF AD_CONTROL.CH% > 0%

		CALL READ_DEFAULTS(SMG_WINDOW)

		!
		! Open main file (existing) for modification
		!
		%INCLUDE "SOURCE:[AD.OPEN]AD_CONTROL.CRE"

		SMG_WINDOW::CHAN  = AD_CONTROL.CH%

20020		GOSUB 28000

	!
	! Select function
	!
	CASE OPT_OPTLIST

		MVALUE = "Change Blank Help eXit Object "

	!
	! Handle additional options
	!
	CASE OPT_MOREMENU
		SELECT EDIT$(MVALUE, -1%)

		!
		! Units transaction entry
		!
		CASE "OBJECT"
	!++
	! Abstract:OBJECT
	!	^*Object\*
	!	.lm +5
	!	.b
	!	In the ^*Object\* controlling file, one record for each
	!	object from the Object Description File is stored.  This
	!	record contains information concerning the last update
	!	and the last depreciated period for a particular Object
	!	and also a processing status flag.  Each object may have
	!	a different accounting era.
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
	!--
			AD_MAIN_CONTROL = MAIN_WINDOW(AD_MAIN_CONTROLOBJ.ID, "")
		END SELECT

	!
	! Display window background
	!
	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)


		DATA	04,  09, "(01) Object to GL", &
			06,  09, "(02) Last Period Posted", &
			0,   0, ""

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
	!	^*(01) Object to General Ledger\*
	!	.b
	!	.lm +5
	!	The ^*Object to General Ledger\* field enters the Object
	!	Code that is to be posted to the General Ledger.
	!	.b
	!	Pressing ^*<List Choices>\* will provide a list of
	!	valid codes.
	!	.lm -5
	!
	! Index:
	!	.x Object to General Ledger
	!
	!--


			AD_CONTROL::DEP_OBJECT = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"04;36", TEMP$, &
				AD_CONTROL::DEP_OBJECT, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(AD_MAIN_OBJECT.ID, "V0") = 1%
				THEN
				AD_CONTROL::DEP_OBJECT = &
						AD_OBJECT::DEP_OBJECT
				END IF
				GOTO Reenter
			END IF

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Last Period Posted\*
	!	.b
	!	.lm +5
	!	The ^*Last Period Posted\* field contains the period in which
	!	the Object was last posted to the General Ledger.
	!	.b
	!	The format for entry is YYYYPP.
	!	.lm -5
	!
	! Index:
	!	.x Last Period Posted
	!
	!--


			ERA$ = ""
			IF MAIN_WINDOW(AD_MAIN_CONTROLOBJ.ID, &
				"Q0" + AD_CONTROL::DEP_OBJECT) = 1%
			THEN
				ERA$ = AD_CONTROLOBJ::ERA
			END IF

			AD_CONTROL::LASTPER = ENTR_PERIOD(SMG_WINDOW::WNUMBER, &
				"06;36", TEMP$,AD_CONTROL::LASTPER, &
				MFLAG, ERA$, MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		AD_MAIN_CONTROL = 0%

		SELECT MLOOP

		CASE 1%
			IF AD_CONTROL::DEP_OBJECT <> ""
			THEN
				!
				! Is the input defined?
				!
				AD_MAIN_CONTROL = FUNC_TESTENTRY(SMG_WINDOW, &
					AD_CONTROL::DEP_OBJECT, &
					AD_OBJECT::DESCRIPTION, &
					"AD", MLOOP, "OBJ", &
					"Object", AD_MAIN_OBJECT.ID)
			ELSE
				AD_OBJECT::DESCRIPTION = &
					STRING$(LEN(AD_OBJECT::DESCRIPTION), &
					ASCII(" "))
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AD_OBJECT::DESCRIPTION, 4%, 39%, , SMG$M_BOLD)

		END SELECT

	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(1%) AND 2%) = 0%
		THEN
			DESCRIPTION$ = STRING$(20%, 63%)
			DESCRIPTION$ = AD_OBJECT::DESCRIPTION &
				IF MAIN_WINDOW(AD_MAIN_OBJECT.ID, &
				"Q0" + AD_CONTROL::DEP_OBJECT) = 1%
			!
			! Display name (ALSO IN TESTENTRY)
			!
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				DESCRIPTION$, 4%, 39%, , SMG$M_BOLD)
		END IF

	!
	! Set AD_CONTROL_OLD value
	!
20500	CASE OPT_SETOLD
		AD_CONTROL_OLD = AD_CONTROL

	!
	! Restore AD_CONTROL_OLD value
	!
	CASE OPT_RESETOLD
		AD_CONTROL = AD_CONTROL_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		AD_CONTROL2 = AD_CONTROL

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		AD_CONTROL = AD_CONTROL2

	END SELECT

27000	EXIT FUNCTION

28000	!
	! Get control record
	!
	WHEN ERROR IN
		GET #AD_CONTROL.CH%, RECORD 1%, REGARDLESS
	USE
		CONTINUE 28030
	END WHEN

	GOTO 28040

28030	!
	! Load in defaults for control file
	!
	AD_CONTROL::DEP_OBJECT = " "
	AD_CONTROL::LASTPER    = " "
	!AD_CONTROL::STATUS_FLAG = '0'

	WHEN ERROR IN
		PUT #AD_CONTROL.CH%
	USE
		CALL ENTR_3MESSAGE(SCOPE, "Unable to add a record", 0%)
		CONTINUE 32767
	END WHEN

28040	RETURN

29000	!
	! Trap errors
	!
	ON ERROR GO BACK

32767	END FUNCTION
