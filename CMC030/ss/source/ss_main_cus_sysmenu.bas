1	%TITLE "Maintain List of Systems Sold to Customer"
	%SBTTL "SS_MAIN_CUS_SYSMENU"
	%IDENT "V3.6a Calico"

	FUNCTION LONG SS_MAIN_CUS_SYSMENU(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

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
	! Abstract:HELP
	!	.p
	!	This function contains the routines necessary
	!	to maintain the SS Customer System List, a file which
	!	contains a list of all the systems a customer has purchased.
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS SS_SOURCE:SS_MAIN_CUS_SYSMENU/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN SS_MAIN_CUS_SYSMENU
	!	$ DELETE SS_MAIN_CUS_SYSMENU.OBJ;*
	!
	! Author:
	!
	!	03/20/88 - Robert Peterson
	!
	! Modification history:
	!
	!	05/25/88 - Aaron Redd
	!		Modified to allow R/O open of file if R/W open fails.
	!
	!	08/02/89 - Aaron Redd
	!		Rewrote as a simple MAIN function instead of a journal.
	!
	!	11/30/92 - Dan Perkins
	!		Added CASE 2 to OPT_SUBWIND so that VIEW would
	!		work properly.
	!
	!	12/01/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/28/97 - Kevin Handy
	!		Lose unecessary external definitions
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/13/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	!
	! Included files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:SS_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	!
	! CDD inclusions and related memory MAPs
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[SS.OPEN]SS_CUS_SYSMENU.HB"
	MAP (SS_CUS_SYSMENU)	SS_CUS_SYSMENU_CDD	SS_CUS_SYSMENU
	MAP (SS_CUS_SYSMENU_OLD) SS_CUS_SYSMENU_CDD	SS_CUS_SYSMENU_OLD, &
							SS_CUS_SYSMENU2

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_SYSTEM.HB"
	MAP (UTL_SYSTEM)	UTL_SYSTEM_CDD	UTL_SYSTEM

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_SS_CUS_SYSMENU) &
		SS_CUS_SYSMENU.CH%, &
		SS_CUS_SYSMENU.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION	MAIN_WINDOW

	!
	! Declare some variables
	!
	DECLARE	STRING	FILE_LIST(100%)
	DECLARE	STRING	MENU_LIST(20%)
	DECLARE	STRING	MENU_SYS_DIR, MENU_SYS_NAME

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 29000

	!
	! Set some initial variable values
	!
	MENU_SYS_DIR = "CMC:"
	MENU_SYS_NAME = "*.MNU"
	J% = 0%

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
		SMG_WINDOW::DESCR = "List of Customer Systems"
		SMG_WINDOW::NHELP = "SS_MAIN_CUS_SYSMENU"
		SMG_WINDOW::HSIZE = 70%
		SMG_WINDOW::VSIZE = 7%
		SMG_WINDOW::HPOS  = 3%
		SMG_WINDOW::VPOS  = 11%
		SMG_WINDOW::HVIEW = 70%
		SMG_WINDOW::VVIEW = 7%
		SMG_WINDOW::VHPOS = 3%
		SMG_WINDOW::VVPOS = 11%
		SMG_WINDOW::NITEMS= 4%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 2%
		SMG_WINDOW::KNAME(0%) = "Tape_Number"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
		SMG_WINDOW::KNAME(1%) = "System"
			SMG_WINDOW::KFIELD(1%, 0%) = 2%
			SMG_WINDOW::KFIELD(1%, 1%) = 3%
			SMG_WINDOW::KFIELD(1%, 2%) = 2%

		!
		! Load in defaults for system menu
		!
		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF SS_CUS_SYSMENU.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF SS_CUS_SYSMENU.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[SS.OPEN]SS_CUS_SYSMENU.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			SS_MAIN_CUS_SYSMENU = ERR
			CONTINUE 770
		END WHEN

		SS_CUS_SYSMENU.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[SS.OPEN]SS_CUS_SYSMENU.OPN"
		USE
			SS_MAIN_CUS_SYSMENU = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		SS_CUS_SYSMENU.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(SS_CUS_SYSMENU.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = SS_CUS_SYSMENU.CH%
		WHEN ERROR IN
			RESET #SS_CUS_SYSMENU.CH%
			GET #SS_CUS_SYSMENU.CH%, REGARDLESS
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

		DATA	2,  5, "(01) Tape Number", &
			3,  5, "(02) Install Date", &
			4,  5, "(03) System", &
			5,  5, "(04) Menu Number", &
			0,  0, ""

		RESTORE

		READ XPOS%, YPOS%, XSTR$
		I% = 0%

		WHILE XPOS% <> 0%
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

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 Reenter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%
			TAPE% = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"2;25", TEMP$, &
				VAL(EDIT$(SS_CUS_SYSMENU::TAPE, -1%)), &
				MFLAG, "##", MVALUE)

			SS_CUS_SYSMENU::TAPE = FORMAT$(TAPE%, "<0>#")

		CASE 2%
			SS_CUS_SYSMENU::INSDAT = ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"3;25", TEMP$, SS_CUS_SYSMENU::INSDAT, &
				MFLAG, "'E", MVALUE)

		CASE 3%
			SS_CUS_SYSMENU::SYSTEM = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"4;25", TEMP$, SS_CUS_SYSMENU::SYSTEM, &
				MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(UTL_MAIN_SYSTEM.ID, "V0  ") = 1%)
				THEN
					SS_CUS_SYSMENU::SYSTEM = &
						UTL_SYSTEM::SYSTEM
				END IF
				GOTO Reenter
			END IF

		CASE 4%
			CALL FIND_FILE(MENU_SYS_DIR +SS_CUS_SYSMENU::SYSTEM + &
					"_" + MENU_SYS_NAME, FILE_LIST(), 16%, "", "")

			FOR I% = 1% to VAL%(FILE_LIST(0%))
				IF (MID(FILE_LIST(I%), 4%, 6%) <> &
					MID(FILE_LIST(I% - 1%), 4%, 6%))
				THEN
					J% = J% + 1%
					MENU_LIST(J%) = MID(FILE_LIST(I%), 4%, 6%)
				END IF
			NEXT I%

			MENU_LIST(0%) = NUM1$(J%)

			SS_CUS_SYSMENU::MENNUM = ENTR_3STRINGLIST(SCOPE, SMG_WINDOW::WNUMBER, &
				"5;25", TEMP$, SS_CUS_SYSMENU::MENNUM, MFLAG, &
				"'E", MVALUE, MENU_LIST(), "Menu Number", "")

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		SS_MAIN_CUS_SYSMENU = 0%

		SELECT MLOOP

		CASE 2%
			SS_MAIN_CUS_SYSMENU = 1% &
				IF SS_CUS_SYSMENU::INSDAT = ""
		END SELECT

	!
	! Set SS_CUS_SYSMENU_OLD value
	!
20500	CASE OPT_SETOLD
		SS_CUS_SYSMENU_OLD = SS_CUS_SYSMENU

	!
	! Restore SS_CUS_SYSMENU_OLD value
	!
	CASE OPT_RESETOLD
		SS_CUS_SYSMENU = SS_CUS_SYSMENU_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		SS_CUS_SYSMENU2 = SS_CUS_SYSMENU

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		SS_CUS_SYSMENU = SS_CUS_SYSMENU2

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!
		SS_CUS_SYSMENU::CUSNUM = MVALUE

	!
	! Find
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #SS_CUS_SYSMENU.CH%, &
				KEY #0% GE SS_CUS_SYSMENU::CUSNUM + &
				SS_CUS_SYSMENU::TAPE, REGARDLESS

		CASE 1%
			FIND #SS_CUS_SYSMENU.CH%, &
				KEY #1% GE SS_CUS_SYSMENU::SYSTEM + &
				SS_CUS_SYSMENU::INSDAT + &
				SS_CUS_SYSMENU::CUSNUM, REGARDLESS

		END SELECT

	!
	! View header
	!
	CASE OPT_VIEW

		SELECT MLOOP
			!
			! Title (One line only)
			!
			CASE 1%
				MVALUE = "  Tape InstallDate System MenuNumber"

			!
			! Positions of lines
			!
			CASE 2%
				MVALUE = "007,019,026"

			!
			! Convert current record into text
			!
			CASE 3%
				MVALUE = SS_CUS_SYSMENU::TAPE + "   " + &
					PRNT_DATE(SS_CUS_SYSMENU::INSDAT, 0%) + "    "  + &
					SS_CUS_SYSMENU::SYSTEM + "     " + &
					SS_CUS_SYSMENU::MENNUM

		END SELECT

	!
	! Handle array of records
	!
	CASE OPT_SUBWIND

		SELECT MLOOP

		!
		! Find first record (if there is any)
		!
		CASE 1%
			!
			! Set init value
			!
			SMG_WINDOW::CURREC = -1%

27110			!
			! Search for first record
			!
			WHEN ERROR IN
				FIND #SMG_WINDOW::CHAN, KEY #0% EQ MVALUE, REGARDLESS

				!
				! Get a record
				!
				GET #SMG_WINDOW::CHAN
				SMG_WINDOW::CURREC = 0%
			USE
				CONTINUE 28000 IF ERR = 155%
				EXIT HANDLER
			END WHEN

		!
		! Find starting record (if there is any)
		!
		CASE 2%
			!
			! Set init value
			!
			SMG_WINDOW::CURREC = -1%

27115			!
			! Search for starting record
			!
			SELECT MFLAG

			CASE 0%
				WHEN ERROR IN
					FIND #SS_CUS_SYSMENU.CH%, &
						KEY #0% GE MVALUE + &
						SS_CUS_SYSMENU::TAPE, REGARDLESS
				USE
					CONTINUE 28000 IF ERR = 155%
					EXIT HANDLER
				END WHEN

			CASE 1%
				WHEN ERROR IN
					FIND #SS_CUS_SYSMENU.CH%, &
						KEY #1% GE SS_CUS_SYSMENU::SYSTEM + &
						SS_CUS_SYSMENU::INSDAT + &
						MVALUE, REGARDLESS
				USE
					CONTINUE 28000 IF ERR = 155%
					EXIT HANDLER
				END WHEN
			END SELECT

			!
			! Get a record
			!
			SMG_WINDOW::CURREC = 0%

		!
		! Check if still right key
		!
		CASE 3%
			SMG_WINDOW::CURREC = -1%

			IF SS_CUS_SYSMENU::CUSNUM = MVALUE
			THEN
				SMG_WINDOW::CURREC = 0%
			END IF

		!
		! Change key
		!
		CASE 6%
			SS_CUS_SYSMENU::CUSNUM = MVALUE

		END SELECT


	END SELECT

28000	EXIT FUNCTION

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
