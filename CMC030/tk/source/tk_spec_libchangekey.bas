1	%TITLE "Maintain Help Libraries"
	%SBTTL "TK_SPEC_LIBCHANGEKEY"
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
	! Abstract:HELP
	!	.p
	!
	! Index:
	!	.x Help
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_SPEC_LIBCHANGEKEY/LINE
	!	$ LINK/EXE=TK_EXE:*.EXE TK_SPEC_LIBCHANGEKEY, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE TK_SPEC_LIBCHANGEKEY.OBJ;*
	!
	! Author:
	!
	!	05/29/90 - Lance Williams
	!
	! Modification history:
	!
	!	02/21/92 - Kevin Handy
	!		Modified to use READ_INITIALIZE only, instead of
	!		READ_INITIALIZE and KEYBOARD.OPN
	!
	!	06/14/93 - Kevin Handy
	!		Clean up (Check)
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
	!	03/04/99 - Kevin Handy
	!		Change variable 'PROGRAM$' to 'FILENAME$'
	!
	!	09/19/2000 - Kevin Handy
	!		Use LIB$DELETE_FILE instead of KILL
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	DIM LIB_INDEX$(100%), RFA LIB_RFA(100%)

	!
	! Include files
	!
	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! External functions
	!
	EXTERNAL LONG		READ_3BROADCAST
	EXTERNAL LONG		FUNCTION LIBR_EXTRACT
	EXTERNAL LONG		FUNCTION LIBR_3INSERT
	EXTERNAL LONG		FUNCTION HELP_INSERTSOURCE

	DECLARE LONG SYS_STATUS

	%PAGE

 !	ON ERROR GOTO 19000

	CALL READ_INITIALIZE

	!
	! Handle input/output file
	!
	TEMPFILE$ = READ_SYSJOB + ".TMP"

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
		"REPLACES OLD HELP MESSAGES WITH NEW HELP MESSAGES", 2%, 20%)
	SMG_STATUS% = SMG$DRAW_LINE(SMG_SCREEN_DATA%, 4%, 1%, 4%, 80%)

100	!
	! Get info from the user
	!
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Library: " + SPACE$(50%), 6%, 1%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Old   Key: " + SPACE$(50%), 7%, 1%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"New   Key:" + SPACE$(50%), 8%, 1%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		SPACE$(50%), 10%, 1%)

 EntrLib:

	OLDLIB1$ = ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
		"6;40", "Library ", OLDLIB1$ + SPACE$(2% - LEN(OLDLIB1$)), &
		16%, "'E", "")

	!
	! Check for special keys typed
	!
	SELECT SCOPE::SCOPE_EXIT
	!
	! Control/C
	!
	CASE 3%
		GOTO EntrLib	! (Ignored)

	!
	! Uparrow
	!
	CASE SMG$K_TRM_UP
		GOTO EntrLib

	!
	! Downarrow
	!
	CASE SMG$K_TRM_DOWN
		GOTO EntrOld

	!
	! Exit key typed
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO EndProgram

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
		GOTO EntrLib

	END SELECT

 EntrOld:
	OLDKEY$ = ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
		"7;40", "Old Key ", OLDKEY$ + &
		SPACE$(39 - LEN(OLDKEY$)), 16%, "'E", "")

	!
	! Check for special keys typed
	!
	SELECT SCOPE::SCOPE_EXIT
	!
	! Control/C
	!
	CASE 3%
		GOTO EntrOld	! (Ignored)

	!
	! Uparrow
	!
	CASE SMG$K_TRM_UP
		GOTO EntrLib

	!
	! Downarrow
	!
	CASE SMG$K_TRM_DOWN
		GOTO EntrNew

	!
	! Exit key typed
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO EndProgram

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
		GOTO EntrOld

	END SELECT

	OLDKEY$ = TRM$(OLDKEY$)
	X% = LEN(OLDKEY$)
	Y% = INSTR(1%, OLDKEY$, "$")
	Z% = INSTR(Y% + 1%, OLDKEY$, "$")
	NEWKEY$ = MID$(OLDKEY$, Y% + 1%, Z% - Y% - 1%)

 EntrNew:

	NEWKEY$ = ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
		"8;40", "New Key ", NEWKEY$ + &
		SPACE$(39% - LEN(NEWKEY$)), 16%, "'E", "")


	!
	! Check for special keys typed
	!
	SELECT SCOPE::SCOPE_EXIT
	!
	! Control/C
	!
	CASE 3%
		GOTO EntrNew	! (Ignored)

	!
	! Uparrow
	!
	CASE SMG$K_TRM_UP
		GOTO EntrOld

	!
	! Downarrow
	!
	CASE SMG$K_TRM_DOWN
		GOTO EntrNew

	!
	! Exit key typed
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO EndProgram

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
		GOTO EntrNew

	END SELECT

	GOTO EntrNew IF EDIT$(NEWKEY$, -1%) = ""

	NEWLIB$ = "REF:HELP_" + OLDLIB1$
	OLDLIB$ = "REF:" + OLDLIB1$

	CALL LIBR_INDEX(OLDLIB$, OLDKEY$, LIB_INDEX$(), LIB_RFA())

	LOOP% = VAL%(LIB_INDEX$(0%))

	CALL ENTR_3MESSAGE(SCOPE, "Found " + NUM1$(LOOP%) + " keys", 1%)

200	FOR I% = 1% TO LOOP%

		ST% = LIBR_EXTRACT(OLDLIB$, TEMPFILE$, LIB_INDEX$(I%))

		IF (ST% AND 1%) = 0%
		THEN
			CALL ENTR_3MESSAGE(SCOPE, "Error in extract ", 1%)
			GOTO 100
		END IF

		X% = INSTR(1%, LIB_INDEX$(I%), "$")
		X% = INSTR(X% + 1%, LIB_INDEX$(I%), "$")
		SUFF$ = RIGHT$(LIB_INDEX$(I%), X%)
		NEW.KEY$ = "H$" + TRM$(NEWKEY$) + SUFF$

		ST% = LIBR_3INSERT(NEWLIB$, TEMPFILE$, NEW.KEY$)

		IF (ST% AND 1%) = 0%
		THEN
			CALL ENTR_3MESSAGE(SCOPE, "Error in insert ", 1%)
			GOTO 100
		END IF

		CALL ENTR_3MESSAGE(SCOPE, NEW.KEY$ + &
			" has been added in the library.", 1%)

		!
		! Insert text in source code
		!
		ST% = HELP_INSERTSOURCE(NEWLIB$, NEW.KEY$)

		CALL ENTR_3MESSAGE(SCOPE, NEW.KEY$ + &
			" has been added in the source code.", 1%)

300 !		KILL TEMPFILE$

		SMG_STATUS% = LIB$DELETE_FILE(TEMPFILE$ + ";*")

310	NEXT I%

	GOTO 100

 ! HelpError:
	!*******************************************************************
	! Help Message for an Error
	!*******************************************************************
 !	PRINT ERN$ + " " + NUM1$(ERL) + " " + ERT$(ERR), &
 !		"ERR", FILENAME$, "ERROR" + NUM1$(ERR)
 !
 !	GOTO EndProgram

19000	!*******************************************************************
	! Trap errors
	!*******************************************************************
 !	FILENAME$ = ""
 !	RESUME HelpError

 EndProgram:
	!
	! Re-establish cursor
	!
	SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 0%)

	SMG_STATUS% = SMG$DELETE_PASTEBOARD(SCOPE::SMG_PBID)

32767	END
