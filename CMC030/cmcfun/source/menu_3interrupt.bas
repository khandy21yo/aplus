1	%TITLE "Interrupt Command Level"
	%SBTTL "MENU_3INTERRUPT"
	%IDENT "V3.6a Calico"

	SUB MENU_3INTERRUPT(SCOPE_STRUCT SCOPE)

	!
	! COPYRIGHT (C) 1986 BY
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
	!
	! Abstract:HELP
	!	.p
	!	The ^*Interrupt Command Level\* is accessable from any process
	!	by pressing the interrupt key. After option has been selected,
	!	a subprocess is created and the current process waits for
	!	the subprocess termination.
	!	.p
	!	The following is a list of all Interrupt commands:
	!	.b
	!	.list 0,"*"
	!	.le
	!	^*DCL\*
	!	.le
	!	^*Document\*
	!	.le
	!	^*Menu\*
	!	.els
	!
	! Index:
	!	.x Interrupt
	!
	! Parameters:
	!
	!	The call of this program installs the menu.
	!
	!	The return of this subroutine is the interrupt menu.
	!
	! Example:
	!
	!	CALL MENU_3INTERRUPT
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:MENU_3INTERRUPT/NOLINE
	!	$ LIB FUNC_LIB:CMC_3VECTOR/REP MENU_3INTERRUPT
	!	$ DELETE MENU_3INTERRUPT.OBJ;*
	!
	! Author:
	!
	!	10/07/87 - Kevin Handy
	!
	!	12/01/89 - Kevin Handy
	!		Taken from MENU_INTERRUPT.
	!
	! Modification history:
	!
	!	10/13/87 - Kevin Handy
	!		Added DCL option.
	!
	!	10/14/88 - Frank Starman
	!		Change REF: to SIC:
	!
	!	02/10/89 - Kevin Handy
	!		Modified for changes in ENTR_NOLSTRING
	!
	!	09/22/89 - Kevin Handy
	!		Modified to use ENTR_ENTER instead of
	!		ENTR_NOLSTRING.
	!
	!	10/18/89 - Kevin Handy
	!		Fixed arrow key bug.
	!
	!	03/26/90 - Frank F. Starman
	!		Change key for document screen
	!
	!	04/25/90 - Frank F. Starman
	!		Return directly back to the interrupted process
	!		after a request is done.
	!
	!	05/31/90 - Frank F. Starman
	!		Add option for FLD screen. Used in some MAST and
	!		JOUR programs.
	!
	!	08/12/91 - Kevin Handy
	!		Reformatted stupid looking if-then-else statements.
	!
	!	08/14/91 - Kevin Handy
	!		Removed A+.
	!
	!	08/26/91 - Frank F. Starman
	!		Switch dcL and Clippboard option.
	!
	!	02/14/92 - Frank F. Starman
	!		Change option menu.
	!
	!	03/26/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/27/95 - Kevin Handy
	!		Take out clipboard option. Nobody uses it, and
	!		several people have gotten into at and couldn't
	!		figure out how to get out.
	!
	!	10/27/95 - Kevin Handy
	!		Put "Menu" first.
	!
	!	10/27/95 - Kevin Handy
	!		Reformat source.
	!
	!	09/04/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/08/99 - Kevin Handy
	!		Use BASIC$STARLET for LIB$ routines
	!
	!	06/10/99 - Kevin Handy
	!		Modify 'M' to not try to use TEMP$ to set up
	!		default value, since it was never initialized
	!
	!	09/14/2000 - Kevin Handy
	!		Use LIB$DELETE_FILE instead of KILL
	!--
	%PAGE

	OPTION SIZE    = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"
	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "SMG$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	RECORD SCREENCAPTURE_STRUCT
		LONG LINES
		STRING SCREEN(24%) = 132%
	END RECORD

	DECLARE SCREENCAPTURE_STRUCT SCREENCAPTURE

	!
	! External functions
	!
	EXTERNAL LONG FUNCTION DSPL_SCREENCAPTURE(STRING, LONG)

	!
	! Declare variables
	!
	DECLARE LONG INT_OPTION, OLD_OPTION, OLD_MESSAGE

	%PAGE

10	!
	! Run entry
	!
	INT_OPTION = SCOPE::SMG_OPTION
	OLD_IDENT$ = SCOPE::PRG_IDENT + ""
	OLD_PROGRAM$ = SCOPE::PRG_PROGRAM + ""
	OLD_ITEM$ = SCOPE::PRG_ITEM + ""

	GOTO RestoreVar IF TRM$(SCOPE::PRG_PROGRAM) = "MENU_3INTERRUPT"

	SCOPE::PRG_IDENT = "H"
	SCOPE::PRG_PROGRAM = "MENU_3INTERRUPT"
	SCOPE::PRG_ITEM = ""

100	!*******************************************************************
	! Initilizations
	!*******************************************************************

	!
	! Create the data display
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(2%, 132%, &
		SCOPE::SMG_OPTION, SMG$M_BORDER)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SCOPE::SMG_OPTION, &
		SCOPE::SMG_PBID, 21%, 1%)

	OPLIST$ = "Menu dcL Document Help eXit"
	OPT% = 0%

 SelectOption:
	OPT$ = ENTR_3OPTION(SCOPE, "Interrupt Command Level", OPLIST$, OPT%, 0%)

	!
	! Check for special keys typed
	!
	SELECT SCOPE::SCOPE_EXIT

	!
	! Exit key typed
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ

		GOTO ExitFunction

	!
	! Good keys
	!
	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
		! Good key

	!
	! Bad keys
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO SelectOption

	END SELECT

600	!***************************************************************
	! Handle selected item
	!***************************************************************

	!
	! NOTE: The commands are hard coded to the item number, so
	! remember to change this select statement if you re-arrange
	! the menu.
	!
	SELECT OPT$

	!++
	! Abstract:MENU
	!	^*Menu Command Level\*
	!	.p
	!	^*Menu Command Level\* allows to enter a macro command
	!	for the  spawn process. Return without any macro command
	!	will spawn directly into CMC Menu.
	!
	! Index:
	!	.x Menu>Command
	!	.x Command>Menu
	!
	!--
	CASE "M"
		SMG_STATUS%    = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
		SMG_STATUS%    = SMG$PUT_CHARS(SCOPE::SMG_OPTION, &
			"Menu Command Level: ", 1%, 1%, 1%)

		INP$ = SPACE$(55%)

 Macro:
		SELECT ENTR_3ENTER(SCOPE, SCOPE::SMG_OPTION, &
			1%, 22%, INP$, -1%, 16% + 4096%)

		! ^C, ^Z, Exit
		CASE SMG$K_TRM_CTRLC, &
			SMG$K_TRM_F8, SMG$K_TRM_F10, &
			SMG$K_TRM_CTRLZ

			SMG_STATUS% = SMG$POP_VIRTUAL_DISPLAY( &
				SCOPE::SMG_OPTION, SCOPE::SMG_PBID)
			! Restore OPTION  virtual display
			SCOPE::SMG_OPTION = OLD_OPTION
			! Restore MESSAGE virtual display
			SCOPE::SMG_MESSAGE = OLD_MESSAGE

			GOTO ExitFunction

		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO, &
			SMG$K_TRM_F7 ! Good keys

		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO Macro
		END SELECT

		INP$ = EDIT$(INP$, 4% + 8% + 128%)

		SYS_STATUS% = LIB$SET_SYMBOL("CMC$COMMAND", INP$)
		SMG_STATUS% = SMG$UNPASTE_VIRTUAL_DISPLAY( &
			SCOPE::SMG_OPTION, SCOPE::SMG_PBID)
		CALL SUBR_3SPAWN(SCOPE, "MENU")
		SYS_STATUS% = LIB$SET_SYMBOL("CMC$COMMAND", "")

	!
	! Exit
	!
	CASE "X"

	!
	! Help
	!
	CASE "H"

		CALL HELP_34MESSAGE(SCOPE, "", "H", &
			SCOPE::PRG_PROGRAM, "", "HELP")
		GOTO SelectOption

	!
	! DCL
	!
	CASE "L"

	!++
	! Abstract:DCL
	!	^*DCL\*
	!	.p
	!	^*DCL\* spawns process into VMS command level.
	!	Prompt ^*$Subprocess\* indicates
	!	spawning process. After typing LOGOUT subprocess returns
	!	back to the current process.
	!
	! Index:
	!	.x DCL
	!
	!--

		SMG_STATUS% = SMG$UNPASTE_VIRTUAL_DISPLAY( &
			SCOPE::SMG_OPTION, SCOPE::SMG_PBID)
		CALL SUBR_3SPAWN(SCOPE, "")

	!
	! Clipboard
	!
 !	CASE "C"
	!++
	! Abstract:CLIPBOARD
	!	^*Clipboard\*
	!	.p
	!	^*Clipboard\* allows for selection of  any part of current screen and to paste
	!	it into a text file.
	!
	! Index:
	!	.x Clipboard>Command
	!	.x Command>Clipboard
	!
	!--
 !
 !		NAME$ = READ_SYSLOG("CMC$CLIPBOARD")
 !
 !		IF EDIT$(NAME$, -1%)    = "CMC$CLIPBOARD"
 !		THEN
 !			CALL ENTR_3MESSAGE(SCOPE, &
 !				"CMC$CLIPBOARD: is unassigned ???", &
 !				0%)
 !		ELSE
 !			SMG_STATUS% = SMG$UNPASTE_VIRTUAL_DISPLAY( &
 !				SCOPE::SMG_OPTION, SCOPE::SMG_PBID)
 !
 !			CALL SUBR_3CLIPBOARDCUT(SCOPE, NAME$)
 !		END IF

	!
	! Document the current screen
	!
	CASE "D"

	!++
	! Abstract:DOCUMENT
	!	^*Document\*
	!	.p
	!	^*Document\* provides documentation of the current screen.
	!	After assigning a document key the screen will be placed
	!	into a text library on SIC: directory.
	!	.note
	!	The current process is temporarily interrupted.
	!	.end note
	!
	! Index:
	!	.x Document>Command
	!	.x Command>Document
	!
	!--
		!
		! Save the OPTION  virtual display
		!
		OLD_OPTION = SCOPE::SMG_OPTION
		SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(2%, 132%, &
			SCOPE::SMG_OPTION)
		SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SCOPE::SMG_OPTION, &
			SCOPE::SMG_PBID, 21%, 1%)

		!
		! Save the MESSAGE virtual display
		!
		OLD_MESSAGE = SCOPE::SMG_MESSAGE
		SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(2%, 132%, &
			SCOPE::SMG_MESSAGE)
		SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SCOPE::SMG_MESSAGE, &
			SCOPE::SMG_PBID, 23%, 1%)

		!
		! Ask for a name to store it under
		!
 Docu:		IF INSTR(1%, OLD_PROGRAM$, "_")
		THEN
			SOURCE$ = EDIT$(OLD_PROGRAM$, 2% + 4% + 32%)
		ELSE
			SOURCE$ = EDIT$(OLD_ITEM$, 2% + 4% + 32%)
		END IF

		SELECT EDIT$(OLD_ITEM$, -1%)
		CASE "VIEWLIST"
			TEMP$ = SOURCE$ + "$SCREENVIEWLIST"
		CASE "VIEWBY"
			TEMP$ = SOURCE$ + "$SCREENVIEWBYE"
		CASE ELSE
			IF LEFT(OLD_ITEM$, 3%) = "FLD"
			THEN
				TEMP$ = SOURCE$ + "$SCREENFLD"
			ELSE
				TEMP$ = SOURCE$ + "$SCREEN"
			END IF
		END SELECT

		SMG_STATUS%    = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
		SMG_STATUS%    = SMG$PUT_CHARS(SCOPE::SMG_OPTION, &
			"Screen Description:  ", 1%, 1%, 1%)

		INP$ = TEMP$ + SPACE$(55% - LEN(TEMP$))

		SELECT ENTR_3ENTER(SCOPE, SCOPE::SMG_OPTION, &
			1%, 22%, INP$, -1%, 16% + 4096%)

		! ^C, ^Z, Exit
		CASE SMG$K_TRM_CTRLC, &
			SMG$K_TRM_F8, SMG$K_TRM_F10, &
			SMG$K_TRM_CTRLZ

			SMG_STATUS%    = SMG$POP_VIRTUAL_DISPLAY( &
				SCOPE::SMG_OPTION, SCOPE::SMG_PBID)
			! Restore OPTION  virtual display
			SCOPE::SMG_OPTION = OLD_OPTION
			! Restore MESSAGE virtual display
			SCOPE::SMG_MESSAGE = OLD_MESSAGE

			GOTO ExitFunction

		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO, &
			SMG$K_TRM_F7 ! Good keys

		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO Docu
		END SELECT

		INP$ = EDIT$(INP$, 4% + 8% + 128%)

		GOTO Docu IF INP$ = ""

		!
		! Modify name if necessary
		!
		IF INSTR(1%, INP$, "$")
		THEN
			LIB_KEY$ = INP$
		ELSE
			LIB_KEY$ = SOURCE$ + "$" + INP$
		END IF

		SMG_STATUS% = SMG$POP_VIRTUAL_DISPLAY(SCOPE::SMG_OPTION, &
			SCOPE::SMG_PBID)

		!
		! Restore OPTION virtual display
		!
		SCOPE::SMG_OPTION = OLD_OPTION

		!
		! Restore MESSAGE virtual display
		!
		SCOPE::SMG_MESSAGE = OLD_MESSAGE

		SMG_STATUS% = SMG$UNPASTE_VIRTUAL_DISPLAY( &
			SCOPE::SMG_OPTION, SCOPE::SMG_PBID)

		!
		! Put the pasteboard out to the temp file
		!
		SCREENCAPTURE::LINES = 0%
		SCREENCAPTURE::SCREEN(X%) = "" FOR X% = 0% TO 24%

		P1% = LOC(DSPL_SCREENCAPTURE)
		P2% = LOC(SCREENCAPTURE)

		SMG_STATUS% = SMG$PUT_PASTEBOARD( &
			SCOPE::SMG_PBID BY REF, &
 !			LOC(DSPL_SCREENCAPTURE) BY VALUE, &
 !			LOC(SCREENCAPTURE) BY VALUE, &
			P1% BY VALUE, P2% BY VALUE, &
			0% BY REF)

		!
		! Insert text file into a library
		!
		UNDER% = INSTR(1%, SOURCE$ + "_", "_")
		LIB_NAME$ = "SIC:WINDOWS_" + LEFT(TEMP$, UNDER% - 1%)

		!
		! Create text file to insert into window library
		!
		INTERUPT_FILE$ = "WIND_" + READ_SYSJOB + ".TMP"

		SMG_STATUS% = LIB$GET_LUN(INTERUPT.CH%)

		OPEN INTERUPT_FILE$ FOR OUTPUT AS FILE INTERUPT.CH%, &
			RECORDSIZE 255%

		PRINT #INTERUPT.CH%, &
			TRM$(SCREENCAPTURE::SCREEN(I%)) &
			FOR I% = 0% TO 23%

		CLOSE #INTERUPT.CH%
		SMG_STATUS% = LIB$FREE_LUN(INTERUPT.CH%)

		!
		! Insert file into the library
		!
		ST% = LIBR_3INSERT(LIB_NAME$, INTERUPT_FILE$, LIB_KEY$)

		!
		! Distroy text file
		!
		SMG_STATUS% = LIB$DELETE_FILE(INTERUPT_FILE$ + ";*")

		!SMG_STATUS%    = SMG$PASTE_VIRTUAL_DISPLAY( &
		!	INTERRUPT_DISPLAY%, SCOPE::SMG_PBID, '7'L, '15'L)

	!
	! Bad keys
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO SelectOption

	END SELECT

	%PAGE

 ExitFunction:
10000	!********************************************************************
	! END PROGRAM
	!********************************************************************

	SMG_STATUS% = SMG$POP_VIRTUAL_DISPLAY(SCOPE::SMG_OPTION, &
		SCOPE::SMG_PBID)

 RestoreVar:
	SCOPE::SMG_OPTION = INT_OPTION
	SCOPE::PRG_IDENT = OLD_IDENT$
	SCOPE::PRG_PROGRAM = OLD_PROGRAM$
	SCOPE::PRG_ITEM = OLD_ITEM$

32767	END SUB
