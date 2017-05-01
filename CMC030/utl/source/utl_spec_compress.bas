1	%TITLE "Compress Data Files"
	%SBTTL "UTL_SPEC_COMPRESS"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1989 BY
	!
	! Computer Management Center
	! Idaho Falls, Idaho.
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
	! Abstract:HELP
	!	.p
	!	This program creates a .COM file that, when run,
	!	will compress data files into a smaller area of memory.
	!	This seeming impossible task is accomplished by using
	!	the DCL command CONVERT to get rid of deleted records.
	!
	! Index:
	!	.x Compress Files
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
	!	$ BAS UTL_SOURCE:UTL_SPEC_COMPRESS/LINE
	!	$ LINK/EXE=UTL_EXE: UTL_SPEC_COMPRESS, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE UTL_SPEC_COMPRESS.OBJ;*
	!
	! Author:
	!
	!	06/08/89 - Aaron Redd
	!
	! Modification history:
	!
	!	06/27/89 - Kevin Handy
	!		Modified to use READ_INITIALIZE
	!
	!	04/12/90 - Kevin Handy
	!		Modified to truncate the file (releasing unused
	!		blocks) after the convert is done.
	!
	!	05/21/90 - Frank F. Starman
	!		Added COMMAND help message.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/30/96 - Kevin Handy
	!		Reformat source code
	!
	!	05/19/97 - Kevin Handy
	!		Reformat source code
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/12/99 - Kevin Handy
	!		Use BASIC$STARLET for LIB$ routines
	!		Fix parameters to SET_BROADCAST_TRAPPING
	!
	!	11/21/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--

	!++
	! Abstract:COMMAND
	!	^*COMPRESS\*
	!	.p
	!	^*Compress\* creates a .COM file that when run
	!	will compress data files into a smaller area of memory.
	!	This seeming impossible task is accomplished by using
	!	the DCL command CONVERT to get rid of deleted records.
	!	.p
	!	^*Format: COMPRESS\*
	!	.P
	!	^*Example:\*
	!	.literal
	!	Menu Command Level> /COMPRESS
	!	.end literal
	!
	! Index:
	!	.x COMPRESS
	!	.x Compress Files
	!
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

	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	!
	! External functions
	!
	EXTERNAL LONG			READ_3BROADCAST

	!
	! Declare variables and/or constants
	!
	DECLARE  LONG	SYS_STATUS

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

	%PAGE

	!******************************************************************
	! Initialize some rather important variables
	!******************************************************************
	CALL READ_INITIALIZE

	!
	! Set help information
	!
	SCOPE::PRG_ITEM = "HELP"

	!
	! Handle output file channel
	!
	COM_FILE.CH% = 5%

	%PAGE

	!******************************************************************
	! Set up the screen
	!******************************************************************
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
	! Print Banner
	!
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Compress Data Files", 2%, 20%)
	SMG_STATUS% = SMG$DRAW_LINE(SMG_SCREEN_DATA%, 4%, 1%, 4%, 80%)

	%PAGE

	!******************************************************************
	! Ask for Command file name
	!******************************************************************

	!
	! Ask for name of command file
	!
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Name of Command File to Build ", 5%, 1%)

	SCOPE::PRG_ITEM = "FLDCMD"
	!++
	! Abstract:FLDCMD
	!
	!	^*Name of Command File to Build\*
	!	.p
	!	Enter the name of the command file, which will be
	!	created and submitted.The default is TEMP.COM.
	!
	! Index:
	!	.x Command File
	!--

	COM_FILE$ = LEFT("TEMP.COM" + SPACE$(20%), 20%)
	COM_FILE$ = EDIT$(ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, "5;40", &
		"Command file <TEMP.COM>", COM_FILE$, &
		16%, "'E", COM_FILE$), -1%)

	!
	! Open the .COM file
	!
300	WHEN ERROR IN
		OPEN COM_FILE$ FOR OUTPUT AS FILE COM_FILE.CH%, &
			DEFAULTNAME "TEMP.COM", &
			RECORDSIZE 132%
	USE
		FILENAME$ = "TEMP.COM"
		CONTINUE HelpError
	END WHEN

	PRINT #COM_FILE.CH%, "$ SET NOON"

	%PAGE

 AskFileInfo:
	!******************************************************************
	! Ask for Directory and/or File to compress
	!******************************************************************
	!
	! Initialize variables
	!
	DIRECT$ = "                              "
	FILENAM$ = "                                        "

	!
	! Paint screen
	!
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Directory to compress " + &
		SPACE$(50%), 7%, 1%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"File(s) to compress" + &
		SPACE$(50%), 8%, 1%)

	!SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, SPACE$(60%), 12%, 1%)

 EntrDir:
	!
	! Ask for directory to compress
	!
	SCOPE::PRG_ITEM = "FLDDIR"

	!++
	! Abstract:FLDDIR
	!	^*Directory\*
	!	.p
	!	The ^*Directory\* field enters
	!	a directory name of the files to be compressed.
	!
	! Index:
	!	.x Directory
	!
	!--

	DIRECT$ = EDIT$(ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
		"7;40", "Directory", &
		LEFT(DIRECT$ + SPACE$(30%), 30%), &
		16%, "'E", ""), 38%)

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
	! Uparrow or Downarrow
	!
	CASE SMG$K_TRM_UP, SMG$K_TRM_DOWN
		GOTO EntrFil

	!
	! Exit key typed
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

	!
	! Good Keys
	!
	CASE 0%, 10%, 12%, SMG$K_TRM_DO, SMG$K_TRM_CR, SMG$K_TRM_SELECT

	!
	! Bad Keys
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO EntrDir

	END SELECT

 EntrFil:
	!
	! Ask for filename(s) to compress
	!
	SCOPE::PRG_ITEM = "FLDFIL"

	!++
	! Abstract:FLDFIL
	!	^*File Name\*
	!	.p
	!	The ^*File Name\* field enters
	!	names of the files to be compressed.
	!	.b
	!	Using wildcharacters is allowed.
	!
	! Index:
	!	.x File Name
	!
	!--

	FILENAM$ = EDIT$(ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
		"8;40", "Filename(s)", &
		LEFT(FILENAM$ + SPACE$(40%), 40%), &
		16%, "'E", ""), -1%)

	!
	! Check for special keys typed
	!
	SELECT SCOPE::SCOPE_EXIT
	!
	! Control/C
	!
	CASE 3%
		GOTO EntrFil	! (Ignored)

	!
	! Uparrow or Downarrow
	!
	CASE SMG$K_TRM_UP, SMG$K_TRM_DOWN
		GOTO EntrDir

	!
	! Exit key typed
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

	!
	! Good Keys
	!
	CASE 0%, 10%, 12%, SMG$K_TRM_DO, SMG$K_TRM_CR, SMG$K_TRM_SELECT

	!
	! Bad Keys
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO EntrFil

	END SELECT

	!
	! Make sure user actually entered something for a filename
	!
	GOTO EntrFil IF (FILENAM$ = "                                        ")

	!
	! Set the directory to null if nothing was entered
	!
	DIRECT$ = "" IF (DIRECT$ = "                              ")

	!
	! Add a wildcard file extension if the user didn't give one
	!
	PERIOD% = INSTR(1%, FILENAM$, ".")
	FILENAM$ = FILENAM$ + ".*" IF PERIOD% = 0%

	%PAGE

	!******************************************************************
	! Look up the actual files and put the info in the .COM file
	!******************************************************************
	!
	! Notify the user that we are "working hard"
	!
	CALL ENTR_3MESSAGE(SCOPE, "Looking up files", 1%)

	!
	! Initialize variables
	!
	X$ = "*.*"
	JUNK% = 0%
	FILE_NUM% = 0%

 LookUpFile:
	!
	! Look up one file
	!
	SYS_STATUS = LIB$FIND_FILE(DIRECT$ + FILENAM$, TEMP_NAM$, JUNK%, X$)

	X$ = ""

	IF (SYS_STATUS AND 1%) = 0%
	THEN
		IF (FILE_NUM% = 0%)
		THEN
			CALL ENTR_3MESSAGE(SCOPE, "Cannot find files!", 0%)

		ELSE
			!
			! Purge the directory after we get done
			!
			PRINT #COM_FILE.CH%, "$!"
			PRINT #COM_FILE.CH%, "$ PURGE " + DIRECT$
			PRINT #COM_FILE.CH%, "$!"
			PRINT #COM_FILE.CH%, "$ WRITE SYS$OUTPUT " + &
				'"Done compressing ' + DIRECT$ + FILENAM$ + '"'
		END IF

		GOTO AskFileInfo
	END IF

	!
	! Remove the Semicolon and Version number from the file specification
	!
	SEMICOLON% = INSTR(1%, TEMP_NAM$, ";")
	TEMP_NAM$ = LEFT(TEMP_NAM$, SEMICOLON% - 1%)

	FILE_NUM% = FILE_NUM% + 1%

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		LEFT(FORMAT$(FILE_NUM%, "#### ") + TEMP_NAM$ + &
		SPACE$(60%), 60%), 12%, 1%)

	PRINT #COM_FILE.CH%, "$!"
	PRINT #COM_FILE.CH%, "$ ANALYZE/RMS/FDL/OUTPUT=TEMP.FDL " + TEMP_NAM$
	PRINT #COM_FILE.CH%, &
		"$ EDIT/FDL/ANALYZE=TEMP.FDL/NOINTERACTIVE TEMP.FDL"
	PRINT #COM_FILE.CH%, "$ CONVERT/FILL/NOSORT/FDL=TEMP.FDL " + &
		TEMP_NAM$ + " " + TEMP_NAM$
	PRINT #COM_FILE.CH%, "$ DELETE TEMP.FDL;*"

	!
	! Erase the message at the bottom
	!
	CALL ENTR_3MESSAGE(SCOPE, "", 1%)

	GOTO LookUpFile

 ExitProgram:
	!******************************************************************
	! Exit the program and possibly submit the .COM file
	!******************************************************************
	!
	! Finish up the Command file and close it
	!
	PRINT #COM_FILE.CH%, "$!"
	PRINT #COM_FILE.CH%, '$ WRITE SYS$OUTPUT ""'

	SYS_STATUS = LIB$FIND_FILE_END(JUNK%)

	CLOSE COM_FILE.CH%

	!
	! Ask if the user would like to submit the com file to be processed
	!
	PROCESS_COM$ = EDIT$(ENTR_3YESNO(SCOPE, SMG_SCREEN_DATA%, "", &
		"Submit the command file for processing", "Y", &
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
	CASE 0%, 10%, 12%, SMG$K_TRM_DO, SMG$K_TRM_CR, SMG$K_TRM_SELECT

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
		SMG_STATUS% = LIB$SPAWN("SUBMIT/NOPRINT/NOTIFY " + COM_FILE$)

		!
		! Check for errors
		!
		IF (SMG_STATUS% <> 1%) AND (SMG_STATUS% <> 0%)
		THEN
			CALL ENTR_3MESSAGE(SCOPE, &
				"Error " + NUM1$(SMG_STATUS%) + &
				" has occured", 0%)
		END IF

		SMG_STATUS% = SMG$SET_BROADCAST_TRAPPING(SCOPE::SMG_PBID, &
			LOC(READ_3BROADCAST) BY VALUE, LOC(SCOPE) BY VALUE)
		SLEEP 1%

		CALL ENTR_3MESSAGE(SCOPE, "", 1%)

	END IF

	%PAGE

 ExitProgram1:
	!******************************************************************
	! If the user came straight here, then either he/she pressed <EXIT>
	! in ExitProgram, or he/she doesn't want to submit the .COM file
	!******************************************************************
	!
	! Re-establish cursor
	!
	SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 0%)
	SMG_STATUS% = SMG$DELETE_PASTEBOARD(SCOPE::SMG_PBID)

	!
	! Exit to menu
	!
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	GOTO ExitProgram

19000	!******************************************************************
	! Error Trapping
	!******************************************************************
	!
	! Handle untrapped errors
	!
	FILENAME$ = ""
	RESUME HelpError

32767	!******************************************************************
	! End of Special Routine UTL_SPEC_COMPRESS
	!******************************************************************
	END
