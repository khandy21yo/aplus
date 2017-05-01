1	%TITLE " Make Class Maintenance"
	%SBTTL "MO_MAIN_MAKECLASS"
	%IDENT "V3.6a Calico"

	FUNCTION LONG MO_MAIN_MAKECLASS(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1991, BY
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
	!	The ^*Make Classification\* option accesses
	!	the entry screen where pertinent information concerning the Make
	!	Classifications is to be entered and maintained.
	!	.b
	!	The Make Classification screen contains the following fields:
	!	.lm +10
	!	.b
	!	.LIST 0,"*"
	!	.LE
	!	(01) Class
	!	.LE
	!	(02) Description
	!	.els
	!	.lm -10
	!
	! Index:
	!	.x Maintain>Make>Classification
	!	.x Make>Classification>Maintenance
	!	.x Maintain>Classification>Make
	!	.x Classification>Make>Maintenance
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS MO_SOURCE:MO_MAIN_MAKECLASS/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN MO_MAIN_MAKECLASS
	!	$ DELETE MO_MAIN_MAKECLASS.OBJ;*
	!
	! Author:
	!
	!	02/27/91 - Craig Tanner
	!
	! Modification history:
	!
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	06/26/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/10/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	11/29/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[MO.OPEN]MO_MAKECLASS.HB"
	MAP (MO_MAKECLASS)	MO_MAKECLASS_CDD	MO_MAKECLASS
	MAP (MO_MAKECLASS_OLD)	MO_MAKECLASS_CDD	MO_MAKECLASS_OLD, MO_MAKECLASS2

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_MO_MAKECLASS) &
		MO_MAKECLASS.CH%, &
		MO_MAKECLASS.READONLY%

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	!
	! Initilization
	!
	! This option is used to initialize the window structure,
	! set up the default values for add, and open all files
	! necessary that have not already been opened.
	!
	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Make Class Maintenance"
		SMG_WINDOW::NHELP = "MO_MAIN_MAKECLASS"
		SMG_WINDOW::CHAN  = MO_MAKECLASS.CH%
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 2%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Class"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		IF INSTR(1%, " QV", MVALUE) <= 1%
		THEN
			!
			! Load in defaults
			!
			CALL READ_DEFAULTS(SMG_WINDOW)
		END IF

700		!
		! Declare channels
		!
		IF MO_MAKECLASS.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF MO_MAKECLASS.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[MO.OPEN]MO_MAKECLASS.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			MO_MAIN_MAKECLASS = ERR
			CONTINUE 770
		END WHEN

		MO_MAKECLASS.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[MO.OPEN]MO_MAKECLASS.OPN"
		USE
			MO_MAIN_MAKECLASS = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		MO_MAKECLASS.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(MO_MAKECLASS.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = MO_MAKECLASS.CH%
		WHEN ERROR IN
			RESET #MO_MAKECLASS.CH%
			GET #MO_MAKECLASS.CH%, REGARDLESS
		USE
			CONTINUE 32767 IF ERR = 11%
			EXIT HANDLER
		END WHEN


	!
	! Display the background
	!
	! This option is used to display the background information
	! on the screen.  It must first clear any junk on the screen,
	! and then write the background onto it.
	!

20100	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)
		DATA	06, 05, "(01) Class", &
			07, 05, "(02) Description", &
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
	! This option is used to enter the data from the user, display
	! data, set defaults, and return the data back according to
	! MFLAG.
	!
20200	CASE OPT_ENTRY
		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 Reenter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Class\*
	!	.b
	!	.lm +5
	!	The ^*Class\* field enters a code which
	!	identifies a particular "Class."
	!	.b
	!	The field will accommodate two (02) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Class>Maintenance
	!	.x Maintenance>Class
	!
	!--

			MO_MAKECLASS::CLASS = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "6;25", TEMP$, &
				MO_MAKECLASS::CLASS, MFLAG, "'E", &
				MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Description\*
	!	.b
	!	.lm +5
	!	The ^*Description\* field enters a description
	!	to identify the class code entered in field (01).
	!	.b
	!	The field will accommodate forty (40) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Class>Description
	!	.x Description>Class
	!
	!--

			MO_MAKECLASS::DESCR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "7;25", TEMP$, &
				MO_MAKECLASS::DESCR, MFLAG, "'E", &
				MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		MO_MAIN_MAKECLASS = 0%

		SELECT MLOOP

		CASE 1%
			IF MO_MAKECLASS::CLASS = ""
			THEN
				MO_MAIN_MAKECLASS = 1%
			ELSE
				SELECT MVALUE

				CASE "ADD"
					WHEN ERROR IN
						GET #MO_MAKECLASS.CH%, &
							KEY #0% EQ MO_MAKECLASS::CLASS + "", &
							REGARDLESS
					USE
						CONTINUE ExitProgram IF ERR = 155%
						EXIT HANDLER
					END WHEN

					MO_MAIN_MAKECLASS = 2%
					CALL ENTR_3MESSAGE(SCOPE, "Record Already Exists", 1%)

				END SELECT
			END IF

		END SELECT

	!
	! Test option
	!
	CASE OPT_TESTOPT
		MO_MAIN_MAKECLASS = 0%

	!
	! Set MO_MAKECLASS_OLD value
	!
20500	CASE OPT_SETOLD
		MO_MAKECLASS_OLD = MO_MAKECLASS

	!
	! Restore MO_MAKECLASS_OLD value
	!
	CASE OPT_RESETOLD
		MO_MAKECLASS = MO_MAKECLASS_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		MO_MAKECLASS2 = MO_MAKECLASS

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		MO_MAKECLASS = MO_MAKECLASS2

	!
	! View header
	!
	CASE OPT_VIEW

		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Class   Description                 "

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "009"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = MO_MAKECLASS::CLASS + "    " + &
				MO_MAKECLASS::DESCR

		END SELECT
	!
	! Find
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #MO_MAKECLASS.CH%, &
				KEY #0% GE MO_MAKECLASS::CLASS + "", REGARDLESS

		END SELECT

	END SELECT

 ExitProgram:
	EXIT FUNCTION

29000	!
	! Trap Errors
	!
	ON ERROR GO BACK

32767	END FUNCTION
