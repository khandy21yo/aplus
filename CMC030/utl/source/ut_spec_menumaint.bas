1	%TITLE "Maintain the Menu"
	%SBTTL "UT_SPEC_MENUMAINT"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1987, 1988 BY
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
	!	.b
	!	.lm +5
	!	This program allows installation of a new system or to remove an
	!	existing one from the Menu. All changes occur in the
	!	Menu residing on the user default directory.
	!	.b
	!	After all changes are made, the user needs to confirm them before they will
	!	be imported to the Menu.
	!	.b
	!	^*Note:\*  Once a system is placed in the Menu there is no
	!	need to re-install it again.
	!	.lm -5
	!
	! Index:
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UT_SPEC_MENUMAINT/LINE
	!	$ LINK/EXECUTABLE=UTL_EXE:*.EXE UT_SPEC_MENUMAINT, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE UT_SPEC_MENUMAINT.OBJ;*
	!
	! AUTHOR:
	!
	!	11/11/86 - Robert Peterson
	!
	! MODIFICATION HISTORY:
	!
	!	01/31/88 - Robert Peterson
	!		Change install of a system so that if that system
	!		already exists then the report defaults will be
	!		copied into the new report structure.
	!
	!	08/01/88 - Kevin Handy and Aaron Redd
	!		Fixed bug where attempt to kill temp file
	!		at line 15450 caused a Fatal System I/O Failure.
	!
	!	02/10/89 - Kevin Handy
	!		Modified for change in ENTR_NOLSTRING
	!
	!	09/23/89 - Kevin Handy
	!		Modified to use ENTR_ENTER instead of
	!		ENTR_NOLSTRING.
	!
	!	08/10/90 - Frank F. Starman
	!		Do not install report settings records on the user
	!		account as well as do not remove them.
	!
	!	09/25/90 - Kevin Handy
	!		Modified so that it doesn't copy UTL_REPORT.IDX
	!		around on users account, since it is no longer
	!		necessary (and takes a long time).
	!
	!	10/05/90 - Frank F. Starman
	!		Make major changes. Work only with sequential files.
	!		Simplify remove process.
	!
	!	10/09/90 - Frank F. Starman
	!		Add Help option.
	!
	!	08/14/91 - Kevin Handy
	!		Remove A+.
	!
	!	09/04/91 - Kevin Handy
	!		Modified to edit menu file names "username.MNU"
	!		if it exists, instead of "MENU.MNU"
	!		Also modified to use default menu name
	!		as "username.MNU" if we are going to create a
	!		brand new menu.
	!
	!	06/18/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!		Add second paramater to FIND_FILEEXISTS.
	!		Fix last parameter to entr_3choice
	!
	!	09/17/96 - Kevin Handy
	!		Reformat source code
	!
	!	05/12/97 - Kevin Handy
	!		Lose UTL_WORK.CH% and WORK.CH% variables
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/13/99 - Kevin Handy
	!		Use BASIC$STARLET for LIB$
	!
	!	06/15/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!++
	! Abstract:COMMAND
	!	^*SYSTEM/INSTALL\*
	!	.b
	!	.lm +5
	!	^*System/Install\* enables the user to maintain the menu system.
	!	.b
	!	^*Format: SYSTEM/INSTALL\*
	!	.b
	!	^*Example:\*
	!	.table 3,25
	!	.te
	!	Menu Command Level> /SYSTEM/INSTALL
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x SYSTEM/INSTALL
	!
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	%INCLUDE "SOURCE:[SMG.OPEN]SMG_SCROLL.HB"
	DECLARE  SMG_SCROLL_CDD BRIEF_SCROLL
	DECLARE  SMG_SCROLL_CDD SYSTEMS_SCROLL


	!
	! External functions
	!
	EXTERNAL LONG   READ_3BROADCAST		! (Actually a function)
	EXTERNAL INTEGER FUNCTION DSPL_SCROLL
	EXTERNAL STRING FUNCTION READ_USERNAME

	!
	! Declare vars
	!
	DECLARE LONG SMG_BRIEF

	DECLARE INTEGER CONSTANT NUM_LINES = 50	! Size of the array

	!
	! Dimension statements
	!
	DIM BRIEF_LINE$(NUM_LINES)
	DIM SYSTEM_MENU$(200%)

	!
	! Initialize all the standard stuff through an external call
	!
	CALL READ_INITIALIZE

	%PAGE

	SYSTEMS_SCROLL::TOP_ARRAY	= 1%
	SYSTEMS_SCROLL::BOT_ARRAY	= 0%
	SYSTEMS_SCROLL::SCROLL_TOP	= 1%
	SYSTEMS_SCROLL::SCROLL_BOT	= 6%
	SYSTEMS_SCROLL::BEG_ELEMENT	= 1%
	SYSTEMS_SCROLL::END_ELEMENT	= 0%
	SYSTEMS_SCROLL::TOP_LINE	= 1
	SYSTEMS_SCROLL::CUR_LINE	= 1
	SYSTEMS_SCROLL::CUR_W_ROW	= 1%
	SYSTEMS_SCROLL::CUR_W_COL	= 1%
	SYSTEMS_SCROLL::FIND_LINE	= 1%
	SYSTEMS_SCROLL::SMG_FLAG	= 0%
	SYSTEMS_SCROLL::PROMPT		= "->"
	SYSTEMS_SCROLL::VIDEO_COMP	= 0%
	SYSTEMS_SCROLL::CHARSET	= 0%
	SYSTEMS_SCROLL::DRAW_COLS	= ""

	!
	! Assign channels
	!
	CALL ASSG_CHANNEL(IDX.CH%, STAT%)
	CALL ASSG_CHANNEL(MENU.CH%, STAT%)

	!
	! Look up device
	!
	CALL  READ_DEVICE("UTL_MENU", UTL_MENU.DEV$, STAT%)

	!
	! Other assignments
	!
	SYSTEM_MENU_NAME$ = "CMC:*.MNU"
	SYSTEM_MENU_PREFIX$ = "CMC:"

	!
	! Let's decide right now what name to apply to the menu.
	! Use the name "Username.MNU" if the file exists, otherwise
	! use "MENU.MNU"
	!
	USERMENU$ = UTL_MENU.DEV$ + READ_USERNAME + ".MNU"
	SOURCENAME$ = UTL_MENU.DEV$ + "MENU.MNU"

	IF FIND_FILEEXISTS(USERMENU$, 0%) <> 0%
	THEN
		SOURCENAME$ = USERMENU$
	END IF

40	!
	! Initilize last option pointer
	!
	OPT% = 0%
	OPT$ = "Install Remove Help eXit"

	%PAGE

	!
	! Initilization section - Prepare to do anything
	!
	ON ERROR GOTO 19000

	BRIEF_LINE$(I%) = "" FOR I% = 0% TO NUM_LINES

	!
	! Create the data display for system menu
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(12%, 70%, SMG_BRIEF)
	SMG_STATUS% = SMG$LABEL_BORDER(SMG_BRIEF, &
		" S Y S T E M S ", SMG$K_TOP,, SMG$M_BOLD)

	!
	! Paste the data display
	!
	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_BRIEF, SCOPE::SMG_PBID, 7%, 5%)
	SMG_STATUS% = SMG$SET_CURSOR_ABS(SMG_BRIEF, 1%, 1%)

	!
	! Define windows
	!
	BRIEF_SCROLL::WINDOW		= SMG_BRIEF
	BRIEF_SCROLL::TOP_ARRAY		= 1%
	BRIEF_SCROLL::BOT_ARRAY		= 0%
	BRIEF_SCROLL::SCROLL_TOP	= 1%
	BRIEF_SCROLL::SCROLL_BOT	= 12%
	BRIEF_SCROLL::BEG_ELEMENT	= 1%
	BRIEF_SCROLL::END_ELEMENT	= 0%
	BRIEF_SCROLL::TOP_LINE		= 1%
	BRIEF_SCROLL::CUR_LINE		= 1%
	BRIEF_SCROLL::CUR_W_ROW		= 1%
	BRIEF_SCROLL::CUR_W_COL		= 1%
	BRIEF_SCROLL::FIND_LINE		= 1%
	BRIEF_SCROLL::SMG_FLAG		= 2%
	BRIEF_SCROLL::PROMPT		= "->"
	BRIEF_SCROLL::VIDEO_COMP	= 0%
	BRIEF_SCROLL::CHARSET		= 0%
	BRIEF_SCROLL::DRAW_COLS		= "047"

500	!
	! Get source file
	!
	WHEN ERROR IN
		OPEN SOURCENAME$ AS FILE IDX.CH%, &
			ORGANIZATION SEQUENTIAL
	USE
		CONTINUE InitArray
	END WHEN

	BRIEF_LOOP% = 1%

	BRIEF_SCROLL::END_ELEMENT	= 0%
	BRIEF_SCROLL::BOT_ARRAY		= 0%
	BRIEF_SCROLL::TOP_LINE		= 1%
	BRIEF_SCROLL::FIND_LINE		= 1%

2000	!=================================================================
	! Read in first line
	!=================================================================

	WHEN ERROR IN
		LINPUT #IDX.CH%, INP$
	USE
		CONTINUE 2100 IF ERR = 11%
		FILENAME$ = SOURCENAME$
		CONTINUE HelpError
	END WHEN

	BRIEF_LOOP% = BRIEF_LOOP% + 1%
	BRIEF_LINE$(BRIEF_LOOP%) = LEFT(INP$, 45%) + RIGHT(INP$, 52%)

	GOTO 2000

2100	CLOSE IDX.CH%

	BRIEF_SCROLL::BOT_ARRAY, BRIEF_SCROLL::END_ELEMENT = BRIEF_LOOP%

 InitArray:
	!
	! Print the array
	!
	TEMP = DSPL_SCROLL(BRIEF_SCROLL, BRIEF_LINE$(), 0%, "PAINT")

 Menu:
3000	!==================================================================
	! Enter desired option
	!==================================================================

	SCOPE::PRG_ITEM = ""
	OPTION$ = ENTR_3OPTION(SCOPE, "COMMAND", OPT$, OPT%, OPTFLAG%)

	SYS_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)

	SELECT SCOPE::SCOPE_EXIT
	CASE SMG$K_TRM_CTRLC		! ^C
		GOTO InitArray

	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitCompile

	CASE SMG$K_TRM_UP, SMG$K_TRM_DOWN, SMG$K_TRM_PREV_SCREEN, &
			SMG$K_TRM_NEXT_SCREEN, SMG$K_TRM_F18, &
			SMG$K_TRM_F19

		TEMP = DSPL_SCROLL(BRIEF_SCROLL, &
			BRIEF_LINE$(), &
			SCOPE::SCOPE_EXIT, "")

		GOTO Menu

	!
	!
	!
	CASE SMG$K_TRM_INSERT_HERE

		GOSUB InstallMenu
		GOTO Menu

	!
	!
	!
	CASE SMG$K_TRM_REMOVE

		GOSUB Remove
		GOTO Menu

	!
	! Good keys
	!
	CASE 0%, 10%, 12%, 13%, 87%, 73%, 65%, &
		69%, 70%, 87%, SMG$K_TRM_DO

	!
	! Bad Keys
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO Menu

	END SELECT

 SelectOption:
	!
	! Decide what to do with the option
	!
	SELECT OPTION$

	CASE "I"	! Install new menu options
		GOSUB InstallMenu
		GOTO Menu

	CASE "R"	! Remove
		GOSUB Remove
		GOTO Menu

	CASE "H"	! Help
		CALL HELP_34MESSAGE(SCOPE, "", "H", SCOPE::PRG_PROGRAM, &
			"", "HELP")
		GOTO Menu

	CASE "X"	! Exit
		GOTO ExitCompile

	END SELECT

	OPTFLAG% = 0%
	GOTO Menu

	%PAGE

 InstallMenu:
6000	!================================================================
	! Install new menu select into main menu
	!================================================================

	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)

	SYSTEM_MENU$(LOOP%) = "" FOR LOOP% = 1% TO 200%

	!*********************************************************************
	! Look up system menus
	!*********************************************************************
	CALL FIND_FILE(SYSTEM_MENU_NAME$, SYSTEM_MENU$(), 16%, "", "")

	X% = ENTR_3CHOICE(SCOPE, "", "", SYSTEM_MENU$(), "", &
		2% + 8% + 128%, " SYSTEM LIST ", "", 0%)

	IF X% <= 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"No system selected - install aborted", 0%)
		GOTO Done
	END IF

	NEW_SYSTEM_MENU$ = SYSTEM_MENU$(X%)

6010	WHEN ERROR IN
		OPEN SYSTEM_MENU_PREFIX$ + NEW_SYSTEM_MENU$ + ".MNU" &
			FOR INPUT AS FILE #MENU.CH%, ACCESS READ
	USE
		CONTINUE Done IF ERR = 5%
		FILENAME$ = NEW_SYSTEM_MENU$
		CONTINUE HelpError
	END WHEN

6020	!
	! Load text into the array
	!
	WHEN ERROR IN
		LINPUT #MENU.CH%, INP$
	USE
		CONTINUE Done IF ERR = 11%
		FILENAME$ = "MENU"
		CONTINUE HelpError
	END WHEN

	GOTO 6020 IF LEFT(INP$, 1%) = "!"
	INP$ = EDIT$(INP$, 16%)
	DEL% = INSTR(1%, INP$, "H<HELP<")

	IF DEL% = 0%
	THEN
		DEL% = INSTR(1%, INP$, "P>PROG>")
		GOTO 6020 IF DEL% = 0%

		SP% = INSTR(1%, INP$, " ")
		PROMPT$ = LEFT(INP$, SP% - 1%)
		NEW_MENU_LINE$ = PROMPT$ + SPACE$(7% - LEN(PROMPT$)) + &
			MID(INP$, SP% + 1%, DEL% - SP% - 1%)
		NEW_MENU_LINE$ = LEFT(NEW_MENU_LINE$ + SPACE$(45%), 45%) + &
			RIGHT(INP$, DEL% + 6%)

	ELSE
		SP% = INSTR(1%, INP$, " ")
		PROMPT$ = LEFT(INP$, SP% - 1%)
		NEW_MENU_LINE$ = PROMPT$ + SPACE$(7% - LEN(PROMPT$)) + &
			MID(INP$, SP% + 1%, DEL% - SP% - 1%)
		NEW_MENU_LINE$ = LEFT(NEW_MENU_LINE$ + SPACE$(45%), 45%) + &
			"<*CMC:" + NEW_SYSTEM_MENU$ + ".MNU"
	END IF

	CLOSE MENU.CH%

	FOR LOOP% = 2% TO BRIEF_SCROLL::BOT_ARRAY

		SP% = INSTR(1%, BRIEF_LINE$(LOOP%), " ")
		IF PROMPT$ = LEFT(BRIEF_LINE$(LOOP%), SP% - 1%)
		THEN
			CALL HELP_34MESSAGE(SCOPE, "system already installed", &
				"W", SCOPE::PRG_PROGRAM, "", "SYSINS")

			GOTO Done
	!++
	! Warning:SYSINS
	!	^*System Already Installed\*
	!	.b
	!	.lm +5
	!	^*Explanation:\*
	!	.b
	!	A system with the same prompt has already been installed into
	!	the CMC Menu.
	!	.b
	!	^*User Action:\*
	!	.b
	!	Make sure the right system has been selected for installation. Try again
	!	after removing the existing system from the menu.
	!	.lm -5
	!
	! Index:
	!
	!--

		END IF

	NEXT LOOP%

	TEMP$ = "Position arrow on line above where system is to " + &
		"be installed - Then press <DO> "

	YPOS% = LEN(TEMP$)

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, TEMP$, 1%, 1%)

 InstallPosition:
	!*********************************************************************
	! Deterine position to install the new menu
	!*********************************************************************

	JUNK$ = " "
	SELECT ENTR_3ENTER(SCOPE, SCOPE::SMG_OPTION, 1%, YPOS%, &
		JUNK$, -1%, 4096%)

	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO Done

	CASE SMG$K_TRM_UP, SMG$K_TRM_DOWN, SMG$K_TRM_PREV_SCREEN, &
		SMG$K_TRM_NEXT_SCREEN, SMG$K_TRM_F18, &
		SMG$K_TRM_F19

		TEMP = DSPL_SCROLL(BRIEF_SCROLL, &
			BRIEF_LINE$(), &
			SCOPE::SCOPE_EXIT, "")

		GOTO InstallPosition

	!
	! Good keys
	!
	CASE 0%, 10%, 12%, 13%, 87%, 73%, 65%, &
		69%, 70%, 87%, SMG$K_TRM_DO

	!
	! Bad Keys
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO InstallPosition

	END SELECT

	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)

	INSTALL% = BRIEF_SCROLL::CUR_LINE + 1%

	! Now insert the menu item in the brief menu
	!
	BRIEF_LINE$(LOOP% + 1%) = BRIEF_LINE$(LOOP%) &
		FOR LOOP% = BRIEF_SCROLL::BOT_ARRAY TO INSTALL% STEP -1%

	BRIEF_LINE$(INSTALL%) = NEW_MENU_LINE$ &

	BRIEF_SCROLL::BOT_ARRAY, BRIEF_SCROLL::END_ELEMENT = &
		BRIEF_SCROLL::BOT_ARRAY + 1%

	BRIEF_SCROLL::FIND_LINE = 1%

	!
	! Redisplay windows
	!
	SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(BRIEF_SCROLL::WINDOW)

	TEMP = DSPL_SCROLL(BRIEF_SCROLL, BRIEF_LINE$(), 0%, "PAINT")

	SMG_STATUS% = SMG$END_DISPLAY_UPDATE(BRIEF_SCROLL::WINDOW)

 Done:
	RETURN

	%PAGE

 Remove:
6100	!================================================================
	! Remove a system from the menu
	!================================================================
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)

	REMOVE% = BRIEF_SCROLL::CUR_LINE

	IF REMOVE% <= 1%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "A system must be selected. . .", 0%)
		GOTO 6195
	END IF

 RemoveConfirm:
	TEMP$ = "Line will be removed. Confirm then press <Do> "

	SCOPE::PRG_ITEM = "REMLINE"
	!++
	! Abstract:REMLINE
	!--

	YESNO$ = EDIT$(ENTR_3YESNO(SCOPE, SCOPE::SMG_OPTION, "", &
			TEMP$, "N", 0%, "", ""), -1%)

	SELECT SCOPE::SCOPE_EXIT
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ

		GOTO 6195

	!
	! Good keys
	!
	CASE 0%, 10%, 12%, 13%, 87%, 73%, 65%, &
		69%, 70%, 87%, SMG$K_TRM_DO

	!
	! Bad Keys
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO RemoveConfirm

	END SELECT

	GOTO 6195 IF YESNO$ <> "Y"

	!
	! Remove the options from the menu
	!
	SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(BRIEF_SCROLL::WINDOW)

	BRIEF_LINE$(I%) = BRIEF_LINE$(I% + 1%) &
		FOR I% = REMOVE% TO BRIEF_SCROLL::BOT_ARRAY

	BRIEF_SCROLL::BOT_ARRAY, BRIEF_SCROLL::END_ELEMENT = &
		BRIEF_SCROLL::BOT_ARRAY - 1%


	IF BRIEF_SCROLL::BOT_ARRAY = 0%
	THEN
		BRIEF_SCROLL::BOT_ARRAY, BRIEF_SCROLL::END_ELEMENT, LOOP% = 1%
	END IF

	BRIEF_SCROLL::FIND_LINE, BRIEF_SCROLL::CUR_LINE = 1%

	!
	! Redisplay after remove has been completed
	!
	TEMP = DSPL_SCROLL(BRIEF_SCROLL, BRIEF_LINE$(), 0%, "PAINT")
	SMG_STATUS% = SMG$END_DISPLAY_UPDATE(BRIEF_SCROLL::WINDOW)

6195	RETURN

	%PAGE

 ExitCompile:
15000	!*******************************************************************
	! Compile menu
	!*******************************************************************

	SCOPE::PRG_ITEM = "CONFCHA"
	!++
	! Abstract:CONFCHA
	!	^*Confirm Changes\*
	!	.b
	!	.lm +5
	!	Enter ^*Y\* to install a new CMC Menu or enter ^*N\* to keep
	!	the original one.
	!	.lm -5
	!
	! Index:
	!
	!--

	TEMP$ = "Confirm storing changes then press <Do> "

	YESNO$ = EDIT$(ENTR_3YESNO(SCOPE, SMG_SCREEN_DATA%, "", &
		TEMP$, "N", 0%, "", ""), -1%)

	SELECT SCOPE::SCOPE_EXIT
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO Menu

	!
	! Good keys
	!
	CASE 0%, 10%, 12%, 13%, 87%, 73%, 65%, &
		69%, 70%, 87%, SMG$K_TRM_DO

	!
	! Bad Keys
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO ExitCompile

	END SELECT

	GOTO ExitProgram IF YESNO$ <> "Y"

	!
	! Get destination file
	!
15200	OPEN SOURCENAME$ FOR OUTPUT AS FILE IDX.CH%, &
		ORGANIZATION SEQUENTIAL, &
		ACCESS MODIFY, &
		ALLOW NONE

15400	!
	! Read in first line
	!
	FOR I% = 2% TO BRIEF_SCROLL::BOT_ARRAY
		INP$ = BRIEF_LINE$(I%)
		IF MID(INP$, 46%, 1%) = "<"
		THEN
			PRINT #IDX.CH%, LEFT(INP$, 45%) + &
				"H<HELP" + RIGHT(INP$, 46%)
		ELSE
			PRINT #IDX.CH%, LEFT(INP$, 45%) + &
				"P>PROG" + RIGHT(INP$, 46%)
		END IF
	NEXT I%

	CLOSE IDX.CH%

15450	!
	! Kill MENU + JOBNUM + .TMP file
	!
	!PURGE UTL_MENU.DEV$ + "MENU" + JOBNUM$ + ".TMP" FOR LOOP% = 1% TO 10%

 ExitProgram:
15500	!*******************************************************************
	! Exit program
	!*******************************************************************

	SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 0%)
	SYS_STATUS% = LIB$SET_SYMBOL("CMC$COMMAND", "")

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitProgram

19000	!======================================================================
	! Trap errors
	!======================================================================

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
