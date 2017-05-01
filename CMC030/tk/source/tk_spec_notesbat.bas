1	%TITLE "Create a List of Notes from a Program"
	%SBTTL "TK_SPEC_NOTESBAT"
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
	!	This program allows the user to have the comments
	!	of program extracted so that they might be examined
	!	to see wether or not the comments clearly explane
	!	the program.
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_SPEC_NOTESBAT/LINE
	!	$ LINK/EXE=TK_EXE: TK_SPEC_NOTESBAT, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE TK_SPEC_NOTESBAT.OBJ;*
	!
	! Author:
	!
	!	06/01/90 - Lance Williams
	!
	! Modification history:
	!
	!	06/18/90 - J. Shad Rydalch
	!		Modified so that output could be sent to the screen.
	!		Change point that program loops back to. From
	!		"Output	File" to "System Name"
	!
	!	02/21/92 - Kevin Handy
	!		Modified to use READ_INITIALIZE instead of
	!		KEYBOARD.OPN
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/16/96 - Kevin Handy
	!		Reformst source code.
	!
	!	08/28/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/09/99 - Kevin Handy
	!		Use BASIC$STARLET for LIB$
	!
	!	04/12/99 - Kevin Handy
	!		Fix parameters to SET_BROADCAST_TRAPPING
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
	DIM	DIR_NAME$(1000%)

	%PAGE

	CALL READ_INITIALIZE

	SCOPE::PRG_COMPANY = "EXTRACT NOTES FROM A SOURCE CODE"

	SCOPE::PRG_IDENT = "H"
	SCOPE::PRG_PROGRAM = "TK_SPEC_NOTESBAT"
	SCOPE::PRG_ITEM = "HELP"

	!
	! Handle output file
	!
	COM_FILE.CH% = 5%

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
	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, 1%, 1%)
	CALL LIB$SIGNAL(SMG_STATUS%) IF (SMG_STATUS% AND 1%) = 0%

	!
	! PRINT BANNER
	!
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Extract Notes", 2%, 20%)
	SMG_STATUS% = SMG$DRAW_LINE(SMG_SCREEN_DATA%, 4%, 1%, 4%, 80%)

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

 ComFile:
	!
	! Ask for name of command file
	!
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Name of command file to build ", 5%, 1%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Output File", 6%, 1%)

	SCOPE::PRG_ITEM  = "CMDFILE"

	COM_FILE$ = "TEMP.COM"
	COM_FILE$ = LEFT(COM_FILE$ + SPACE$(30%), 30%)

	COM_FILE$ = EDIT$(ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
		"5;40", "Command file", COM_FILE$, &
		16%, "'E", COM_FILE$), -1%)

50	OPEN COM_FILE$ FOR OUTPUT AS FILE COM_FILE.CH%, &
		DEFAULTNAME "TEMP.COM", &
		RECORDSIZE 132%

	PRINT #COM_FILE.CH%, "$ @cmc:logicals"
	PRINT #COM_FILE.CH%, "$ set noon"

 Entrout:
	!
	! Ask for a place to put the output
	!
	SCOPE::PRG_ITEM = "OUTPUT"

	NAME$ = "SYS$OUTPUT"
	NAME$ = LEFT(NAME$ + SPACE$(30%), 30%)

	NAME$ = EDIT$(ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
		"6;40", "Ouput file", NAME$, 16%, "'E", ""), -1%)

100	! PAINT OTHER FIELDS
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"System name " + SPACE$(42%), 7%, 1%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Module(s) (Prefix only)" + SPACE$(70%), 8%, 1%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Indexing" + SPACE$(41%), 9%, 1%)

 EntrDir:
	!
	! Ask for directory to compile and link
	!
	SCOPE::PRG_ITEM = "DIRECT"

	DIRECT$ = LEFT(DIRECT$ + SPACE$(2%), 2%)

	DIRECT$ = EDIT$(ENTR_3STRINGLIST(SCOPE, SMG_SCREEN_DATA%, &
		"7;40", "System Name <Wildcard>", DIRECT$, &
		16%, "'L", "", DIR_NAME$(), &
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

	MODULE$ = LEFT(MODULE$ + SPACE$(30%), 30%)

	MODULE$ = EDIT$(ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
		"8;40", "Module <Wildcard>", MODULE$, &
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
	! Downarrow
	!
	CASE SMG$K_TRM_DOWN
		GOTO EntrInd

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

 EntrInd:
	!
	! Ask for modules
	!
	SCOPE::PRG_ITEM = "INDEX"

	IND$ = "N"

	IND$ = EDIT$(ENTR_3YESNO(SCOPE, SMG_SCREEN_DATA%, &
		"9;40", "Indexing", "N", 16%, "'", "N"), -1%)

	!
	! Check for special keys typed
	!
	SELECT SCOPE::SCOPE_EXIT
	!
	! Control/C
	!
	CASE 3%
		GOTO EntrInd	! (Ignored)

	!
	! Uparrow
	!
	CASE SMG$K_TRM_UP
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
		GOTO EntrInd

	END SELECT

200	!
	! Write instructions to command file
	!
	CALL ENTR_3MESSAGE(SCOPE, "Looking up modules " + TRM$(MODULE$), 1%)

	PRINT #COM_FILE.CH%, '$write sys$output ""'
	PRINT #COM_FILE.CH%, "$run tk_exe:tk_spec_extnotes"
	PRINT #COM_FILE.CH%, DIRECT$
	PRINT #COM_FILE.CH%, TRM$(MODULE$)

	IF IND$ = "Y"
	THEN
		PRINT #COM_FILE.CH%, "$runoff/Intermediate PROG.TMP"
		PRINT #COM_FILE.CH%, "$runoff/Index PROG.BRN"
		PRINT #COM_FILE.CH%, "$runoff/bold=2/output=" + &
			TRM$(NAME$) + " " + "PROG.TMP"
		PRINT #COM_FILE.CH%, "$Delete PROG.BRN;*"
		PRINT #COM_FILE.CH%, "$Delete PROG.RNX;*"
	ELSE
		PRINT #COM_FILE.CH%, "$runoff/bold=2/output=" + &
			TRM$(NAME$) + " " + "PROG.TMP"
	END IF

	PRINT #COM_FILE.CH%, "$Delete PROG.TMP;*"

	GOTO 100

 ExitProgram:

	PRINT #COM_FILE.CH%, '$write sys$output ""'
	CLOSE COM_FILE.CH%

	SELECT NAME$
	CASE "SYS$OUTPUT"
		! notify user to wait
		CALL ENTR_3MESSAGE(SCOPE, "Creating list of notes.", 1% + 16%)

		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 0%)
		SMG_STATUS% = SMG$DELETE_PASTEBOARD(SCOPE::SMG_PBID)
		SMG_STATUS% = LIB$DO_COMMAND("@" + COM_FILE$)
	CASE ELSE

 AskForSub:
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
			GOTO AskForSub	! (Ignored)

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
			GOTO AskForSub

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
				CALL ENTR_3MESSAGE(SCOPE, "Error " + NUM1$(SMG_STATUS%) + " has occured", 0%)
			END IF

			SLEEP 1%

			SMG_STATUS% = SMG$SET_BROADCAST_TRAPPING(SCOPE::SMG_PBID, &
				LOC(READ_3BROADCAST) BY VALUE, LOC(SCOPE) BY VALUE)

			CALL ENTR_3MESSAGE(SCOPE, "", 1%)
		END IF

 ExitProgram1:
		! Re-establish cursor
		!
		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 0%)

		SMG_STATUS% = SMG$DELETE_PASTEBOARD(SCOPE::SMG_PBID)

	END SELECT

	!
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
