1	%TITLE "Create Batch File to Run TK_SPEC_INSRTLIB"
	%SBTTL "TK_SPEC_CREATEBAT"
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
	! Abstract:
	!
	!
	! Index:
	!
	! Option:
	!
	!
	! Input:
	!
	!
	! Output:
	!
	!
	! Example:
	!
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_SPEC_CREATEBAT/LINE
	!	$ LINK/EXE=TK_EXE: TK_SPEC_CREATEBAT, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE TK_SPEC_CREATEBAT.OBJ;*
	!
	! Author:
	!
	!	01/22/90 - Shad Rydalch
	!
	! Modification history:
	!
	!	05/29/90 - Lance Williams
	!		Modified it to ask the user which program type to
	!		extract from and where to find it.
	!
	!	02/21/92 - Kevin Handy
	!		Modified to use READ_INITIALIZE instead of
	!		KEYBOARD.OPN.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/16/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/28/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/08/99 - Kevin Handy
	!		Use BASIC$STARLET for LIB$ routines
	!
	!	04/12/99 - Kevin Handy
	!		Fix parameters to SET_BROADCAST_TRAPPING
	!
	!	10/17/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	ON ERROR GOTO 19000

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	!
	! External functions
	!
	EXTERNAL LONG		READ_3BROADCAST

	DECLARE LONG SYS_STATUS

	!
	! Dimension variables
	!
	DIM	FILE_NAME$(2000%), &
		FILE_NAMEC$(2000%), &
		DIR_NAME$(1000%), &
		COMP_TEXT$(100%)


	CALL READ_INITIALIZE

	SCOPE::PRG_COMPANY = "Extract help messages from modules"

	SCOPE::PRG_IDENT = "PROG"
	SCOPE::PRG_PROGRAM = "TK_SPEC_CREATEBAT"
	SCOPE::PRG_ITEM = ""

	%PAGE

	!
	! Handle output file
	!
	COM_FILE.CH% = 5%
	READ_FILE.CH% = 6%

	!
	! Set variables
	!
	SOURCE_DIRECTORY$ = "SOURCE:[*.-]*.DIR"

	!
	! Create the data display
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(20%, 80%, SMG_SCREEN_DATA%,,,)
	CALL LIB$SIGNAL(SMG_STATUS%) IF (SMG_STATUS% AND 1%) = 0%

	!
	! Paste the data display
	!
	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%, SCOPE::SMG_PBID, 1%, 1%)
	CALL LIB$SIGNAL(SMG_STATUS%) IF (SMG_STATUS% AND 1%) = 0%

	!
	! PRINT BANNER
	!
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "EXTRACT HELP MESSAGES FROM MODULES", 2%, 20%)
	SMG_STATUS% = SMG$DRAW_LINE(SMG_SCREEN_DATA%, 4%, 1%, 4%, 80%)

 ComFile:
	!
	! Ask for name of command file
	!
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Name of command file to build ", 5%, 1%)

	COM_FILE$ = LEFT("TEMP.COM" + SPACE$(20%), 20%)

	SCOPE::PRG_ITEM = "CMDFILE"

	COM_FILE$ = EDIT$(ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
		"5;40", "Command file <TEMP.COM>", COM_FILE$, &
		16%, "'E", COM_FILE$), -1%)

50	WHEN ERROR IN
		OPEN COM_FILE$ FOR OUTPUT AS FILE COM_FILE.CH%, &
			DEFAULTNAME "TEMP.COM", &
			RECORDSIZE 132%
	USE
		FILENAME$ = COM_FILE$
		CONTINUE HelpError
	END WHEN

	PRINT #COM_FILE.CH%, "$ @cmc:logicals"
	PRINT #COM_FILE.CH%, "$ set noon"

	!
	! Look up all directories for the source code
	!
	CALL ENTR_3MESSAGE(SCOPE, "Looking up source directories.", 1% + 16%)

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
	LINKING$, DIRECT$, MODULE$, NAME$, COMPILE_OPT$, EXE_PURGE$ = ""

100	!
	! Paint screen
	!
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"System name " + SPACE$(50%), 7%, 1%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Module(s) (Prefix only)" + SPACE$(50%), 8%, 1%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		SPACE$(50%), 10%, 1%)

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
		GOTO EntrMod

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

 EntrMod:
	!
	! Ask for modules
	!
	SCOPE::PRG_ITEM = "MODULE"

	MODULE$ = EDIT$(ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
		"8;40", "Module <Wildcard>", &
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
		GOTO EntrMod	! (Ignored)

	!
	! Uparrow
	!
	CASE SMG$K_TRM_UP
		GOTO EntrDir

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
		GOTO EntrMod

	END SELECT

	GOTO EntrMod IF EDIT$(MODULE$, -1%) = ""

200	!
	! Look up one file
	!
	CALL ENTR_3MESSAGE(SCOPE, "Looking up modules " + TRM$(MODULE$), 1%)

	PRINT #COM_FILE.CH%, '$write sys$output ""'
	PRINT #COM_FILE.CH%, "$run tk_exe:tk_spec_createrepf"
	PRINT #COM_FILE.CH%, DIRECT$
	PRINT #COM_FILE.CH%, TRM$(MODULE$)


	GOTO 100

 ExitProgram:

	PRINT #COM_FILE.CH%,'$write sys$output ""'
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
			CALL ENTR_3MESSAGE(SCOPE, &
				"Error " + NUM1$(SMG_STATUS%) + " has occured", 0%)
		END IF

		SLEEP 1%

		SMG_STATUS% = SMG$SET_BROADCAST_TRAPPING(SCOPE::SMG_PBID, &
			LOC(READ_3BROADCAST) BY VALUE, LOC(SCOPE) BY VALUE)

		CALL ENTR_3MESSAGE(SCOPE, "", 1%)
	END IF

 ExitProgram1:
	!
	! Re-establish cursor
	!
	SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 0%)

	SMG_STATUS% = SMG$DELETE_PASTEBOARD(SCOPE::SMG_PBID)

	GOTO 32767

HelpError:
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"%" + ERN$ + "-" + &
		FORMAT$(ERL, "#### ") + " " + &
		FILENAME$ + " " + &
		LEFT(FORMAT$(ERR, "#### ") + &
		ERT$(ERR) + SPACE$(78%), 78%), &
		17%, 1%)

	GOTO 32767

19000	!======================================================================
	! Error Trapping
	!======================================================================

	!
	! Untrapped error
	!
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		LEFT(FORMAT$(ERR, "#### ") + &
		FORMAT$(ERL, "#### ") + &
		ERT$(ERR) + SPACE$(78%), 78%), &
		17%, 1%)

	RESUME 32767

32767	END
