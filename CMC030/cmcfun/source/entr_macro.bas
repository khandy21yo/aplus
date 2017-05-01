1	%TITLE "Menu (Macro) Command Level"
	%SBTTL "ENTR_MACRO"
	%IDENT "V3.6a Calico"

	FUNCTION LONG ENTR_MACRO(SCOPE_STRUCT SCOPE)

	!
	! COPYRIGHT (C) 1989 BY
	! Computer Management Center
	!
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
	!	.b
	!	.lm +5
	!	The ^*Menu Command Level (MCL)\* from a process allows
	!	to enter CMC or user defined macro commands.
	!	.b
	!	The following is list of valid CMC commands:
	!	.table 3,25
	!	.te
	!	^*AMORTIZATION\*
	!	.te
	!	^*BATCH\*,/MONITOR
	!	.te
	!	^*BYE\*
	!	.te
	!	^*CALENDAR\*
	!	.te
	!	^*COMPRESS\*
	!	.te
	!	^*COUNTRY\*,/LIST
	!	.te
	!	^*DEVICE\*,/LIST
	!	.te
	!	^*END\*
	!	.te
	!	^*EXIT\*
	!	.te
	!	^*HELP\*
	!	.te
	!	^*LOGOUT\*,/FULL
	!	.te
	!	^*MACRO\*,/DELETE,/LIST
	!	.te
	!	^*PROFILE\*,/PERIOD,/STRUCTURE
	!	.te
	!	^*QUIT\*
	!	.te
	!	^*REPORT\*,/DESTINATION,/PRINT,/SETTINGS
	!	.te
	!	^*STRING\*,/LIST,/PRINT
	!	.te
	!	^*SYSTEM\*,/INSTALL,/LIST
	!	.te
	!	^*TIMEOUT\*
	!	.end table
	!	.lm -5
	!
	! Index:
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:ENTR_MACRO/NOLINE
	!	$ LIB FUNC_LIB:CMC_3VECTOR/REP ENTR_MACRO
	!	$ DELETE ENTR_MACRO.OBJ;*
	!
	! AUTHOR:
	!
	!	04/06/90 - Frank F. Starman
	!
	! MODIFICATION HISTORY:
	!
	!	08/03/90 - Frank F. Starman
	!		Erase screen after entering a macro command.
	!
	!	03/25/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/03/96 - Kevin Handy
	!		Clean up (check)
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--
	%PAGE

	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	DECLARE LONG   SAVE_TIMEOUT
	DECLARE STRING SAVE_IDENT
	DECLARE STRING SAVE_PROGRAM
	DECLARE STRING SAVE_ITEM
	DECLARE STRING MACRO

	!
	! Save the prior help key
	!
	SAVE_IDENT = SCOPE::PRG_IDENT + ""
	SAVE_PROGRAM = SCOPE::PRG_PROGRAM + ""
	SAVE_ITEM = SCOPE::PRG_ITEM + ""
	SAVE_TIMEOUT = SCOPE::SCOPE_TIMEOUT

	GOTO RestoreScope IF TRM$(SCOPE::PRG_PROGRAM) = "ENTR_MACRO"

	SCOPE::PRG_PROGRAM = "ENTR_MACRO"
	SCOPE::PRG_ITEM = "HELP"
	SCOPE::PRG_IDENT = "H"

	!
	! Assume a good macro command
	!

	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(2%, 132%, MACRO%, &
		SMG$M_BORDER,,)
	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(MACRO%, &
		SCOPE::SMG_PBID, 21%, 1%)
	SMG_STATUS% = SMG$PUT_CHARS(MACRO%, &
		"Menu Command Level> ", 1%, 1%)

	MACRO = SPACE$(50%)
	SCOPE::SCOPE_TIMEOUT = 0%

 EnterMacro:
	EXIT_STATUS  = ENTR_3ENTER(SCOPE, MACRO%, &
		1%, 21%, MACRO, -1%, 8% + 16% + 1024% + 4096%)

	SELECT EXIT_STATUS
	!
	! Help keys
	!
	CASE SMG$K_TRM_F15

		CALL HELP_34MESSAGE(SCOPE, "", "H", &
			SCOPE::PRG_PROGRAM, "", "HELP")
		GOTO EnterMacro

	!
	! Good key
	!
	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

		IF MACRO <> ""
		THEN
			EXIT_STATUS = SMG$K_TRM_F10
		ELSE
			EXIT_STATUS = 0%
			GOTO ExitFunction
		END IF

	!
	! Repaint screen
	!
	CASE SMG$K_TRM_F11, SMG$K_TRM_CTRLW
		GOTO ExitFunction

	!
	! Go to MCL
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ

		EXIT_STATUS = SMG$K_TRM_F10

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO EnterMacro
	END SELECT

	MACRO = EDIT$(MACRO, 8% + 16% + 128%)

	!
	! Set symbol, that menu will understand it
	!
	CALL LIB$SET_SYMBOL("CMC$COMMAND", MACRO)

	!
	! delete all virtual arrays
	!
	SMG_STATUS% = SMG$POP_VIRTUAL_DISPLAY(SCOPE::SMG_OPTION, &
		SCOPE::SMG_PBID) &
		IF SCOPE::MACROFLAG = 0%
	!
	! Assign macro flag to allow quick exit from process to the menu
	!
	SCOPE::MACROFLAG = 3%

 ExitFunction:

	SMG_STATUS% = SMG$POP_VIRTUAL_DISPLAY(MACRO%, SCOPE::SMG_PBID)

 RestoreScope:
	!
	! Restore back the original help key
	!
	SCOPE::SCOPE_TIMEOUT = SAVE_TIMEOUT
	SCOPE::PRG_IDENT = SAVE_IDENT
	SCOPE::PRG_PROGRAM = SAVE_PROGRAM
	SCOPE::PRG_ITEM = SAVE_ITEM

	ENTR_MACRO = EXIT_STATUS

32767	END FUNCTION
