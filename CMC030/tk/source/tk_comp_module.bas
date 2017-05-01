1	%TITLE "Compile Executable Modules"
	%SBTTL "TK_COMP_MODULE"
	%IDENT "V3.6a Calico"

	!
	!	COPYRIGHT (C) 1987 BY
	!	Computer Management Center, Idaho Falls, Idaho.
	!
	! This software is furnished under a license and may be used and
	! copied only in accordance with terms of such license and with
	! the inclusion of the above copyright notice.  This software or
	! any other copies therof may not be provided or otherwise made
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
	!
	! Abstract:HELP
	!	.p
	!	This program is used to compile all of the source code
	!	into executable code.
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_COMP_MODULE/LINE
	!	$ LINK/EXE=TK_EXE: TK_COMP_MODULE, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE TK_COMP_MODULE.OBJ;*
	!
	! Author:
	!
	!	02/25/88 - Frank F. Starman
	!
	! Modification history:
	!
	!	02/08/89 - Kevin Handy
	!		Modified to handle C code.
	!
	!	10/19/89 - Kevin Handy
	!		Modified so that defaults are not zeroed every
	!		loop through the input.
	!
	!	05/14/90 - Kevin Handy
	!		Modified to set the protection on the executable
	!		created to "W:RE".
	!
	!	06/09/90 - Kevin Handy
	!		Modified to set "/NOLIST" option in the compile
	!		statements for C source code.
	!
	!	11/13/90 - Kevin Handy
	!		Added "ACCESS READ, ALLOW MODIFY" to open statement
	!		to try to bypass file locked error while source
	!		is being printed.
	!
	!	06/23/91 - Frank F. Starman
	!		Ask for version.
	!		Compile with debugger if DEB is found in the
	!		name of the com file.
	!
	!	06/26/91 - Frank F. Starman
	!		Default version 030.
	!
	!	02/21/92 - Kevin Handy
	!		Modified to use READ_INITIALIZE instead of
	!		KEYBOARD.OPN
	!
	!	04/15/93 - Kevin Handy
	!		Modified to use SUBR_SUBMIT instead of
	!		LIB$SPAWN to submit to que.  This should
	!		be faster and cleaner. (Didn't work?)
	!
	!	03/15/94 - Kevin Handy
	!		Disabled check for "OPTION SIZE". Let CHECK
	!		look for those type of problems.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	12/06/95 - Kevin Handy
	!		Modified default version number from 030 to 036.
	!		Reformatted source closer to 80 columns.
	!
	!	12/12/95 - Kevin Handy
	!		Lose bunches of commented out code.
	!
	!	09/13/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/28/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	10/07/97 - Kevin Handy
	!		Modified to handle 'GCC' (GNU-C)
	!		Some reformatting.
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/18/98 - Kevin Handy
	!		Shift the filename display down a few lines
	!
	!	04/08/99 - Kevin Handy
	!		Use BASIC$STARLET for LIB$ routines
	!
	!	04/12/99 - Kevin Handy
	!		Fix parameters to SET_BROADCAST_TRAPPING
	!
	!	10/24/99 - Kevin Handy
	!		Modify so that it outputs "/OPT=LEVEL=1" on the
	!		the Alpha processor due to a buggy optimizer.
	!
	!	05/27/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	08/07/2000 - Kevin Handy
	!		Clean up code, lose ON ERROR GOTO
	!
	!	12/10/2003 - Kevin Handy
	!		Exit program if Ctrl/Z typed in "version" or
	!		"comand file" entry fields instead of trying to
	!		execute command file.
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "$SYIDEF" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	!
	! External functions
	!
	EXTERNAL LONG		READ_3BROADCAST

	!
	! Dimension variables
	!
	DIM	FILE_NAME$(2000%), &
		FILE_NAMEC$(2000%), &
		DIR_NAME$(1000%), &
		COMP_TEXT$(100%)

	%PAGE

	CALL READ_INITIALIZE

	SCOPE::PRG_COMPANY	= "Compile executable modules"

	SCOPE::PRG_IDENT	= "PROG"
	SCOPE::PRG_PROGRAM	= "TK_COMP_MODULE"
	SCOPE::PRG_ITEM		= ""

	%PAGE

	!
	! Handle output file
	!
	COM_FILE.CH% = 5%
	READ_FILE.CH% = 6%

	!
	! Determine what hardware we are running on (Assume VAX)
	!
	FLAG% = LIB$GETSYI(SYI$_ARCH_NAME BY REF, RESULT% BY REF, HARDWARE$)
	HARDWARE$ = "VAX" IF HARDWARE$ = ""

	!
	! List of tasks
	!
	TASKTITLE$ = "Task  Description"
	TASK$(0%) = "2"
	TASK$(1%) = "L    Link"
	TASK$(2%) = "R    Replace"

	!
	! Create the data display
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(20%, 80%, SMG_SCREEN_DATA%,,,)
	CALL LIB$SIGNAL(SMG_STATUS%) IF (SMG_STATUS% AND 1%) = 0%

	!
	! Paste the data display
	!
	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, 1%, 1%)
	CALL LIB$SIGNAL(SMG_STATUS%) IF (SMG_STATUS% AND 1%) = 0%

	!
	! PRINT BANNER
	!
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"COMPILE AND LINK SOURCE PROGRAMS", 2%, 20%)
	SMG_STATUS% = SMG$DRAW_LINE(SMG_SCREEN_DATA%, 4%, 1%, 4%, 80%)

	!
	! Ask for version
	!
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Version ", 5%, 1%)

	VERSION$ = "036"
	VFLAG% = 1%

 AskForVersion:
	SCOPE::PRG_ITEM = "VERSION"
	VERSION$ = ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
		"5;40", "Version <" + VERSION$ + ">", VERSION$, &
		16%, "'E", VERSION$)

	!
	! Check for special keys typed
	!
	SELECT SCOPE::SCOPE_EXIT

	!
	! Control/C
	!
	CASE 3%, SMG$K_TRM_UP
		GOTO AskForVersion	! (Ignored)

	!
	! Downarrow
	!
	CASE SMG$K_TRM_DOWN
		GOTO AskForFile

	!
	! Exit key typed
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram1

	!
	! Good Keys
	!
	CASE 0%, 10%, 12%, &
		SMG$K_TRM_DO, SMG$K_TRM_CR, SMG$K_TRM_SELECT

	!
	! Bad Keys
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO AskForVersion

	END SELECT

	VFLAG% = 16%

	IF MID(VERSION$, 2%, 1%) = "4"
	THEN
		CMC_VER$ = "CMC040"
		SOURCE_VER$ = "SOURCE4:"
	ELSE
		CMC_VER$ = "CMC"
		SOURCE_VER$ = "SOURCE:"
	END IF

	SOURCE_DIRECTORY$ = SOURCE_VER$ + "[*.-]*.DIR"

	!
	! Ask for name of command file
	!
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Name of command file to build ", 6%, 1%)

	COM_FILE$ = LEFT("TEMP.COM" + SPACE$(20%), 20%)

 AskForFile:
	SCOPE::PRG_ITEM = "CMDFILE"
	COM_FILE$ = EDIT$(ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
		"6;40", "Command file <TEMP.COM>", COM_FILE$, &
		16%, "'E", COM_FILE$), -1%)

	!
	! Check for special keys typed
	!
	SELECT SCOPE::SCOPE_EXIT

	!
	! Control/C
	!
	CASE 3%
		GOTO AskForFile	! (Ignored)

	!
	! Uparrow
	!
	CASE SMG$K_TRM_UP
		GOTO AskForVersion

	!
	! Exit key typed
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram1

	!
	! Good Keys
	!
	CASE 0%, 10%, 12%, SMG$K_TRM_DOWN, &
		SMG$K_TRM_DO, SMG$K_TRM_CR, SMG$K_TRM_SELECT

	!
	! Bad Keys
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO AskForFile

	END SELECT

	IF INSTR(1%, COM_FILE$, "DEB")
	THEN
		COMP_DEBUG$,LINK_DEBUG$ = "/DEBUG"
	ELSE
		COMP_DEBUG$ = ""
		LINK_DEBUG$ = "/NOTRACEBACK"
	END IF

50	OPEN COM_FILE$ FOR OUTPUT AS FILE COM_FILE.CH%, &
		DEFAULTNAME "TEMP.COM", &
		RECORDSIZE 132%

	IF MID(VERSION$, 2%, 1%) = "4"
	THEN
		PRINT #COM_FILE.CH%, "$ @CMC040:LOGICALS"
	ELSE
		PRINT #COM_FILE.CH%, "$ @CMC:LOGICALS"
	END IF

	PRINT #COM_FILE.CH%, "$ SET NOON"

	!
	! Look up all directories for the source code
	!
	CALL ENTR_3MESSAGE(SCOPE, &
		"Looking up source directories.", 1% + 16%)

	CALL FIND_FILE(SOURCE_DIRECTORY$, DIR_NAME$(), 16%, "", "")
	DIR_LOOP% = VAL%(DIR_NAME$(0%)) + 1%

	DIR_NAME$(0%) = NUM1$(DIR_LOOP%)

	DIR_NAME$(DIR_LOOP%) = "*"

	CALL ENTR_3MESSAGE(SCOPE, "", 1%)

	!
	! If source directories do not exist then exit
	!
	IF DIR_LOOP% = 1%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Source directories do not exist.  Aborting. . .", 0%)
		GOTO ExitProgram
	END IF

	!
	! Initialize variables
	!
	LINKING$, DIRECT$, MODULE$, COMPILE_OPT$, EXE_PURGE$ = ""

100	!
	! Paint screen
	!
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Directory to compile " + SPACE$(50%), 7%, 1%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Link or Replace in library" + SPACE$(50%), 8%, 1%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Module(s) (Prefix only)" + SPACE$(50%), 9%, 1%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Set .LIS and .MAP option" + SPACE$(50%), 10%, 1%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		SPACE$(50%), 11%, 1%)

 EntrDir:
	!
	! Ask for directory to compile and link
	!
	SCOPE::PRG_ITEM = "DIRECT"

	DIRECT$ = EDIT$(ENTR_3STRINGLIST(SCOPE, SMG_SCREEN_DATA%, &
		"7;40", "Directory", &
		LEFT(DIRECT$ + SPACE$(10%), 10%), &
		16%, "'LLLLLLLLL", "", DIR_NAME$(), &
		"Source Directories", ""), -1%)

	!
	! Check for special keys typed
	!
	SELECT SCOPE::SCOPE_EXIT

	!
	! Control/C
	!
	CASE 3%
		GOTO EntrDir	! (Ignored)

	!
	! Uparrow
	!
	CASE SMG$K_TRM_UP
		GOTO EntrDir

	!
	! Downarrow
	!
	CASE SMG$K_TRM_DOWN
		GOTO EntrTask

	!
	! Exit key typed
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

	!
	! Good Keys
	!
	CASE 0%, 10%, 12%, &
		SMG$K_TRM_DO, SMG$K_TRM_CR, SMG$K_TRM_SELECT

	!
	! Bad Keys
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO EntrDir

	END SELECT


 EntrTask:
	!
	! Ask for directory to compile and link
	!
	SCOPE::PRG_ITEM = "TASK"

	LINKING$ = EDIT$(ENTR_3STRINGLIST(SCOPE, SMG_SCREEN_DATA%, &
		"8;40", "Link or Replace", &
		LEFT(LINKING$ + SPACE$(1%), 1%), &
		16%, "'", "L", TASK$(), &
		"Task Description", "005"), -1%)

	!
	! Check for special keys typed
	!
	SELECT SCOPE::SCOPE_EXIT

	!
	! Control/C
	!
	CASE 3%
		GOTO EntrTask	! (Ignored)

	!
	! Uparrow
	!
	CASE SMG$K_TRM_UP
		GOTO EntrDir

	!
	! Downarrow
	!
	CASE SMG$K_TRM_DOWN
		GOTO SelectLink

	!
	! Exit key typed
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

	!
	! Good Keys
	!
	CASE 0%, 10%, 12%, &
		SMG$K_TRM_DO, SMG$K_TRM_CR, SMG$K_TRM_SELECT

	!
	! Bad Keys
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO EntrTask

	END SELECT

 SelectLink:
	SELECT LINKING$

	CASE "R"

 EntrSub:
		!
		! Ask for modules
		!
		SCOPE::PRG_ITEM = "MODULE"

		MODULE$ = EDIT$(ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
			"9;40", "Module <Wildcard>", &
			LEFT(MODULE$ + SPACE$(30%), 30%), &
			16%, "'E", ""), -1%)

		!
		! Check for special keys typed
		!
		SELECT SCOPE::SCOPE_EXIT

		!
		! Control/C
		!
		CASE 3%
			GOTO EntrSub	! (Ignored)

		!
		! Uparrow
		!
		CASE SMG$K_TRM_UP
			GOTO EntrTask

		!
		! Downarrow
		!
		CASE SMG$K_TRM_DOWN
			GOTO EntrSubOption

		!
		! Exit key typed
		!
		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO ExitProgram

		!
		! Good Keys
		!
		CASE 0%, 10%, 12%, &
			SMG$K_TRM_DO, SMG$K_TRM_CR, SMG$K_TRM_SELECT

		!
		! Bad Keys
		!
		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO EntrSub

		END SELECT

		GOTO EntrSub IF EDIT$(MODULE$, -1%) = ""

 EntrSubOption:
		!
		! Ask for compile options
		!
		SCOPE::PRG_ITEM = "COMPILE_OPT"

		COMPILE_OPT$ = EDIT$(ENTR_3YESNO(SCOPE, SMG_SCREEN_DATA%, &
			"10;40", "Set Options", "N", &
			16%, "'", "N"), -1%)

		!
		! Check for special keys typed
		!
		SELECT SCOPE::SCOPE_EXIT

		!
		! Control/C
		!
		CASE 3%
			GOTO EntrSubOption	! (Ignored)

		!
		! Uparrow
		!
		CASE SMG$K_TRM_UP
			GOTO EntrSub

		!
		! Downarrow
		!
		CASE SMG$K_TRM_DOWN
			GOTO EntrSubOption

		!
		! Exit key typed
		!
		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO ExitProgram

		!
		! Good Keys
		!
		CASE 0%, 10%, 12%, &
			SMG$K_TRM_DO, SMG$K_TRM_CR, SMG$K_TRM_SELECT

		!
		! Bad Keys
		!
		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO EntrSubOption

		END SELECT

		IF COMPILE_OPT$ = "N"
		THEN
			NOLIST$ = "/NOLIST"
			NOMAP$ = "/NOMAP"
		ELSE
			NOLIST$, NOMAP$ = ""
		END IF

	CASE ELSE

 EntrExe:
		!
		! Ask for modules
		!
		SCOPE::PRG_ITEM = "MODULE"

		MODULE$ = EDIT$(ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
			"9;40", "Module <Wildcard>", &
			LEFT(MODULE$ + SPACE$(30%), 30%), &
			16%, "'E", ""), -1%)

		!
		! Check for special keys typed
		!
		SELECT SCOPE::SCOPE_EXIT

		!
		! Control/C
		!
		CASE 3%
			GOTO EntrExe	! (Ignored)

		!
		! Uparrow
		!
		CASE SMG$K_TRM_UP
			GOTO EntrTask

		!
		! Downarrow
		!
		CASE SMG$K_TRM_DOWN
			GOTO EntrExeOption

		!
		! Exit key typed
		!
		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO ExitProgram

		!
		! Good Keys
		!
		CASE 0%, 10%, 12%, &
			SMG$K_TRM_DO, SMG$K_TRM_CR, SMG$K_TRM_SELECT

		!
		! Bad Keys
		!
		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO EntrExe

		END SELECT

		GOTO EntrExe IF EDIT$(MODULE$, -1%) = ""

 EntrExeOption:
		!
		! Ask for compile options
		!
		SCOPE::PRG_ITEM = "COMPILE_OPT"

		COMPILE_OPT$ = EDIT$(ENTR_3YESNO(SCOPE, SMG_SCREEN_DATA%, &
			"10;40", "Set Options", "N", &
			16%, "'", "N"), -1%)

		!
		! Check for special keys typed
		!
		SELECT SCOPE::SCOPE_EXIT

		!
		! Control/C
		!
		CASE 3%
			GOTO EntrExeOption	! (Ignored)

		!
		! Uparrow
		!
		CASE SMG$K_TRM_UP
			GOTO EntrExe

		!
		! Downarrow
		!
		CASE SMG$K_TRM_DOWN
			GOTO EntrExePurge

		!
		! Exit key typed
		!
		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO ExitProgram

		!
		! Good Keys
		!
		CASE 0%, 10%, 12%, &
			SMG$K_TRM_DO, SMG$K_TRM_CR, SMG$K_TRM_SELECT

		!
		! Bad Keys
		!
		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO EntrExeOption

		END SELECT

		IF COMPILE_OPT$ = "N"
		THEN
			NOLIST$ = "/NOLIST"
			NOMAP$ = "/NOMAP"
		ELSE
			NOLIST$, NOMAP$ = ""
		END IF

 EntrExePurge:
		!
		! Ask if need to purge
		!
		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Purge .EXE files", 11%, 1%)

		SCOPE::PRG_ITEM = "EXE_PURGE"

		EXE_PURGE$ = EDIT$(ENTR_3YESNO(SCOPE, SMG_SCREEN_DATA%, &
			"11;40", "Purge", "Y", &
			16%, "'", "Y"), -1%)

		!
		! Check for special keys typed
		!
		SELECT SCOPE::SCOPE_EXIT

		!
		! Control/C
		!
		CASE 3%
			GOTO EntrExePurge	! (Ignored)

		!
		! Uparrow
		!
		CASE SMG$K_TRM_UP
			GOTO EntrExeOption

		!
		! Downarrow
		!
		CASE SMG$K_TRM_DOWN
			GOTO EntrExePurge

		!
		! Exit key typed
		!
		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO ExitProgram

		!
		! Good Keys
		!
		CASE 0%, 10%, 12%, &
			SMG$K_TRM_DO, SMG$K_TRM_CR, SMG$K_TRM_SELECT

		!
		! Bad Keys
		!
		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO EntrExePurge

		END SELECT

	END SELECT

200	!
	! Look up one file
	!
	CALL ENTR_3MESSAGE(SCOPE, "Looking up modules", 1%)

	FOR J% = 1% TO DIR_LOOP% - 1%

		GOTO NextJ IF COMP_STRING(DIR_NAME$(J%), DIRECT$) = 0%

		!
		! Look up under one directory for all possible source files
		!
		PREFIX$ = SOURCE_VER$ + "[" + DIR_NAME$(J%) + ".SOURCE]"
		CALL FIND_FILE(PREFIX$ + TRM$(MODULE$) + ".BAS", &
			FILE_NAME$(), 16%, "", "")
		CALL FIND_FILE(PREFIX$ + TRM$(MODULE$) + ".C", &
			FILE_NAMEC$(), 16%, "", "")

		!
		! Merge the two lists together, accepting the C file when
		! there is duplication.
		!
		LOOP%, LOOPBAS% = VAL%(FILE_NAME$(0%))
		LOOPC% = VAL%(FILE_NAMEC$(0%))

		FILE_NAME$(I%) = FILE_NAME$(I%) + ".BAS" &
			FOR I% = 1% TO LOOPBAS%

		FOR I% = 1% TO LOOPC%

			FOR K% = 1% TO LOOPBAS%

				IF LEFT(FILE_NAME$(K%), &
					INSTR(1%, FILE_NAME$(K%), ".")) = &
					FILE_NAMEC$(I%) + "."
				THEN
					FILE_NAME$(K%) = FILE_NAMEC$(I%) + ".C"
					GOTO NLoopX
				END IF

			NEXT K%

			LOOP% = LOOP% + 1%
			FILE_NAME$(LOOP%) = FILE_NAMEC$(I%) + ".C"
 NLoopX:
		NEXT I%

		!
		! Go through all of the files picked up, and
		! try to compile them.
		!
		FOR I% = 1% TO LOOP%
205			FILE_NAME$ = PREFIX$ + FILE_NAME$(I%)

			WHEN ERROR IN
				OPEN PREFIX$ + FILE_NAME$(I%) &
					FOR INPUT AS FILE READ_FILE.CH%, &
					ACCESS READ, &
					ALLOW MODIFY
			USE
				SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
					LEFT("Error reading file" + &
					FORMAT$(ERR, "#### ") + ERT$(ERR) + &
					SPACE$(78%), 78%), &
					16%, 1%)
				CONTINUE 210

			END WHEN

			FUNCSUB% = 0%
			GOSUB ReadFile

			CLOSE READ_FILE.CH%
210		NEXT I%

 NextJ:
	NEXT J%

	GOTO 100

 ExitProgram:

	PRINT #COM_FILE.CH%,'$WRITE SYS$OUTPUT ""'
	CLOSE COM_FILE.CH%

	!
	! Ask if the user would like to submit the com file to be processed
	!
	PROCESS_COM$ = EDIT$(ENTR_3YESNO(SCOPE, SMG_SCREEN_DATA%, &
		"", "Submit the command file for processing", "Y", &
		16%, "'", "N"), -1%)

	!
	! Check for special keys typed
	!
	SELECT SCOPE::SCOPE_EXIT

	!
	! Control/C
	!
	CASE 3%
		GOTO ExitProgram	! (Ignored)

	!
	! Exit key typed
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram1

	!
	! Good Keys
	!
	CASE 0%, 10%, 12%, &
		SMG$K_TRM_DO, SMG$K_TRM_CR, SMG$K_TRM_SELECT

	!
	! Bad Keys
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO ExitProgram

	END SELECT


	IF PROCESS_COM$ = "Y"
	THEN
		!
		! Submit command file
		!
		CALL ENTR_3MESSAGE(SCOPE, "Submitting command file", 1%)

		SMG_STATUS% = SMG$DISABLE_BROADCAST_TRAPPING(SCOPE::SMG_PBID)

		SMG_STATUS% = LIB$SPAWN("SUBMIT " + COM_FILE$ + &
			"/NOPRINT/NOTIFY")

		IF SMG_STATUS% <> 1% AND SMG_STATUS% <> 0%
		THEN
			CALL ENTR_3MESSAGE(SCOPE, "Error " + &
				NUM1$(SMG_STATUS%) + " has occured", 0%)
		END IF

		SLEEP 1%

		SMG_STATUS% = SMG$SET_BROADCAST_TRAPPING(SCOPE::SMG_PBID, &
			LOC(READ_3BROADCAST), LOC(SCOPE))

		CALL ENTR_3MESSAGE(SCOPE, "", 1%)
	END IF

 ExitProgram1:
	!
	! Re-establish cursor
	!
	SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 0%)

	SMG_STATUS% = SMG$DELETE_PASTEBOARD(SCOPE::SMG_PBID)

	GOTO 32767

	!=====================================================================
 ReadFile:

10000	WHEN ERROR IN
		LINPUT #READ_FILE.CH%, TEXT$
	USE
		CONTINUE RetRead IF ERR = 11%
		CONTINUE HelpError
	END WHEN

	TEXT$ = EDIT$(TEXT$, 8% + 32% + 128%)

	GOTO ReadFile IF (LEFT(TEXT$, 9%) <> "! COMPILE") AND &
		(LEFT(TEXT$, 9%) <> "* COMPILE")

	COMP_TEXT% = 0%

 CompRead:
	WHEN ERROR IN
		LINPUT #READ_FILE.CH%, TEXT$
	USE
		CONTINUE RetRead IF ERR = 11%
		CONTINUE HelpError
	END WHEN

	TEXT$ = EDIT$(TEXT$, 8% + 128%)

	GOTO CompRead IF (TEXT$ = "!") OR (TEXT$ = "*")

	IF (LEFT(TEXT$, 2%) = ("!" + '9'C)) OR (LEFT(TEXT$, 2%) = ("*" + '9'C))
	THEN
		TEXT$ = RIGHT(TEXT$, 3%)

		IF INSTR(1%, TEXT$, " BAS ") OR &
			LEFT(TEXT$, 4%) = "$ CC" OR &
			LEFT(TEXT$, 5%) = "$ GCC"
		THEN
			TEXT$ = TEXT$ + NOLIST$ + COMP_DEBUG$
		END IF

		IF INSTR(1%, TEXT$, "$ BAS ") AND &
			INSTR(1%, TEXT$, "OPT") = 0% AND &
			HARDWARE$ = "Alpha"
		THEN
			TEXT$ = TEXT$ + "/OPT=LEVEL=4"
		END IF

		IF INSTR(1%, TEXT$, "CMCFUN/LIB")
		THEN
			TEXT$ = TEXT$ + NOMAP$
		END IF

		IF INSTR(1%, TEXT$, "CMCLINK/OPTION")
		THEN
			TEXT$ = TEXT$ + NOMAP$ + LINK_DEBUG$
		END IF

		COMP_TEXT% = COMP_TEXT% + 1%
		COMP_TEXT$(COMP_TEXT%) = TEXT$

		GOTO CompRead
	END IF

10050	!
	! See if this is one we really want
	!
	IF COMP_TEXT% = 0%
	THEN
		PRINT #COM_FILE.CH%, &
			'$WRITE SYS$OUTPUT "Unable to find COMPILE command!"'
		GOTO RetRead
	END IF

	FUNCSUB% = -1%
	FUNCSUB% = 0% IF INSTR(1%, COMP_TEXT$(K%), "CMCLINK/OPTION") &
		FOR K% = 1% TO COMP_TEXT%

	GOTO RetRead IF (FUNCSUB% = 0%) AND (LINKING$ <> "L")
	GOTO RetRead IF (FUNCSUB% <> 0%) AND (LINKING$ = "L")

	!
	! Write out the compile commands
	!
	SEQ% = SEQ% + 1%

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		LEFT(FORMAT$(SEQ%, "#### ") + FILE_NAME$ + &
		SPACE$(60%), 60%), &
		15%, 1%)

	PRINT #COM_FILE.CH%,'$WRITE SYS$OUTPUT ""'

	IF LINKING$<>"L"
	THEN
		PRINT #COM_FILE.CH%,'$WRITE SYS$OUTPUT "Replacing ' + &
			EDIT$(FILE_NAME$(I%), 4%) + '"'
	ELSE
		PRINT #COM_FILE.CH%,'$WRITE SYS$OUTPUT "Compiling ' + &
			EDIT$(FILE_NAME$(I%), 4%) + '"'
	END IF

	PRINT #COM_FILE.CH%, COMP_TEXT$(K%) FOR K% = 1% TO COMP_TEXT%

	IF LINKING$ = "L" AND EXE_PURGE$ = "Y"
	THEN
		PRINT #COM_FILE.CH%, &
			"$ PURGE " + CMC_VER$ + &
			"$ROOT:[" + DIR_NAME$(J%) + "]" + &
			LEFT(FILE_NAME$(I%), INSTR(1%, FILE_NAME$(I%), ".")) + &
			"EXE"
	END IF

	IF LINKING$ = "L"
	THEN
		PRINT #COM_FILE.CH%, &
			"$ SET PROT=W:RE " + CMC_VER$ + &
			"$ROOT:[" + DIR_NAME$(J%) + "]" + &
			LEFT(FILE_NAME$(I%), INSTR(1%, FILE_NAME$(I%), ".")) + &
			"EXE"
	END IF

	PRINT #COM_FILE.CH%, "$!"

 RetRead:
	RETURN

 HelpError:
32700	!
	! Untrapped error
	!
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		LEFT(FORMAT$(ERR, "#### ") + &
		FORMAT$(ERL, "#### ") + &
		ERT$(ERR) + SPACE$(78%), 78%), &
		17%, 1%)

32767	END
