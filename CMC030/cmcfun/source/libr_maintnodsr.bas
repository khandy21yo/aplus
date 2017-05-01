1	%TITLE "Library Maintenance without DSR"
	%SBTTL "LIBR_MAINTNODSR"
	%IDENT "V3.6a Calico"

	FUNCTION LONG LIBR_MAINTNODSR(LIB_FILE$, KEY_NAME$, PTITLE$, PFLAG%)

	!
	! COPYRIGHT (C) 1987 BY
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
	!
	! Abstract:HELP
	!	.p
	!	This function displays text from a library about
	!	a specific key, and allows maintenance of that
	!	text. It will not format it using DSR.
	!
	! Index:
	!
	! Parameters:
	!
	!	LIB_FILE$
	!		Passed name of library file to use
	!
	!	KEY_NAME$
	!		Full passed key name for text
	!
	!	PTITLE$
	!		Passed title to show on top of screen
	!
	!	PFLAG%
	!		Flag for special reasons.
	!		Not currently used.
	!
	!	Returned value
	!		A status in same format as DEC's functions.
	!		(Currently set to 1 only).
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:LIBR_MAINTNODSR/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP LIBR_MAINTNODSR
	!	$ DELETE LIBR_MAINTNODSR.OBJ;*
	!
	! Author:
	!
	!	07/08/87 - Kevin Handy
	!
	! Modification history:
	!
	!	02/10/89 - Kevin Handy
	!		Modified for changes in ENTR_NOLSTRING
	!
	!	09/22/89 - Kevin Handy
	!		Modified to use ENTR_ENTER instead of
	!		ENTR_NOLSTRING.
	!
	!	06/27/90 - Frank F. Starman
	!		Correct variables for help key message.
	!
	!	03/26/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/03/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/08/99 - Kevin Handy
	!		Use BASIC$STARLET fro LIB$ routines
	!
	!	04/08/99 - Kevin Handy
	!		Pass LOC(SCOPE) instead of SCOPE BY REF
	!
	!	06/20/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	09/14/2000 - Kevin Handy
	!		Use LIB$DELETE_FILE instead of KILL
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	%INCLUDE "SOURCE:[SMG.OPEN]SMG_SCROLL.HB"
	DECLARE	SMG_SCROLL_CDD	SMG_SCROLL

	!
	! External functions
	!
	EXTERNAL INTEGER	FUNCTION DSPL_SCROLL
	EXTERNAL LONG		READ_3BROADCAST
	EXTERNAL LONG		FUNCTION LIBR_DELETE
	EXTERNAL LONG		FUNCTION LIBR_NODIGSR
	EXTERNAL LONG		FUNCTION LIBR_EDIT
	EXTERNAL LONG		FUNCTION LIBR_EXTRACT

	!
	! Declarations
	!
	DECLARE LONG SVD
	DECLARE INTEGER CONSTANT NUM_LINES = 600	! Size of the array

	!
	! Dimension statements
	!
	DIM LINE_NUM$(NUM_LINES)

	%PAGE

100	!
	! Assume successful status
	!
	LIBR_MAINTNODSR = 1%

	!
	! Think up temp file name
	!
	TEMP_FILE$ = "TEMP" + READ_SYSJOB + ".TMP"

	!
	! Create virtual display
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(18%, 78%, SVD)

	!
	! Label the borders of system and user
	!
	SMG_STATUS% = SMG$LABEL_BORDER(SVD, PTITLE$,,, SMG$M_BOLD)

	SMG_SCROLL::WINDOW	= SVD
	SMG_SCROLL::SCROLL_TOP	= 1%
	SMG_SCROLL::SCROLL_BOT	= 18%

	SMG_SCROLL::TOP_ARRAY	= 1%
	SMG_SCROLL::BOT_ARRAY	= NUM_LINES
	SMG_SCROLL::TOP_LINE	= 1%
	SMG_SCROLL::BEG_ELEMENT	= 1%
	SMG_SCROLL::END_ELEMENT	= NUM_LINES
	SMG_SCROLL::CUR_LINE	= 1%
	SMG_SCROLL::CUR_W_COL	= 1%
	SMG_SCROLL::CUR_W_ROW	= 1%
	SMG_SCROLL::FIND_LINE	= 1%

	SMG_SCROLL::SMG_FLAG	= 0%
	SMG_SCROLL::PROMPT	= ""
	SMG_SCROLL::DRAW_COLS	= ""

	%PAGE

	!
	! Set up for help
	!
	OLD_IDENT$ = SCOPE::PRG_IDENT
	OLD_PROGRAM$ = SCOPE::PRG_PROGRAM
	OLD_ITEM$ = SCOPE::PRG_ITEM

	SCOPE::PRG_IDENT	= "H"
	SCOPE::PRG_PROGRAM	= "LIBR_MAINTNODSR"
	SCOPE::PRG_ITEM		= "HELP"

	!
	! Option list
	!
	OPT% = 0%

500	!
	! Load array and print it to the virtual display
	!
	GOSUB LoadAll

	V% = DSPL_SCROLL(SMG_SCROLL, LINE_NUM$(), 0%, "PAINT")

	!
	! Paste virtual displays to pasteboard
	!
	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SVD, SCOPE::SMG_PBID, 2%, 2%)
	GOTO Menu

 InitArray:
1000	!
	! Print the array
	!
	SMG_SCROLL::TOP_LINE = SMG_SCROLL::BEG_ELEMENT
	V% = DSPL_SCROLL(SMG_SCROLL, LINE_NUM$(), 0%, "PAINT")

	%PAGE

	!*******************************************************************
	! Main menu
	!*******************************************************************

 Menu:
3000	!
	! Enter desired option
	!
	OPLIST$ = "Edit Delete exTract Help Print eXit"
	OPT$ = ENTR_3OPTION(SCOPE, "COMMAND", OPLIST$, OPT%, OPTFLAG%)

	SELECT SCOPE::SCOPE_EXIT
	!
	! ^C
	!
	CASE SMG$K_TRM_CTRLC
		GOTO InitArray

	!
	! Exit key
	!
	CASE SMG$K_TRM_F8, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

	!
	! Find key
	!
	CASE SMG$K_TRM_FIND	! Find
		OPT$ = "F"

	!
	! Movement
	!
	CASE SMG$K_TRM_UP,		! Uparrow		&
		SMG$K_TRM_DOWN,		! Downarrow		&
		SMG$K_TRM_LEFT,		! Left arrow		&
		SMG$K_TRM_RIGHT,	! Right arrow		&
		SMG$K_TRM_PREV_SCREEN,	! Previous screen	&
		SMG$K_TRM_NEXT_SCREEN,	! Next screen		&
		SMG$K_TRM_F18,		! Top			&
		SMG$K_TRM_F19		! Bottom

		V% = DSPL_SCROLL(SMG_SCROLL, LINE_NUM$(), &
			SCOPE::SCOPE_EXIT, "")

		GOTO Menu

	!
	! Good keys
	!
	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO, SMG$K_TRM_F7

	!
	! Bad keys
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO Menu
	END SELECT

	!
	! Decide what to do with the option
	!
	SELECT OPT$
	!
	! Delete text
	!
	CASE "D"
 MagicDelete:
		!
		! Confirm deletion
		!
		OPT$ = ENTR_3YESNO(SCOPE, SCOPE::SMG_OPTION, "", &
			"Really delete text", "N", 0%, "", "")

		SELECT SCOPE::SCOPE_EXIT

		!
		! Exit keys
		!
		CASE SMG$K_TRM_F8, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO Menu

		!
		! Good keys
		!
		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

		!
		! Bad keys
		!
		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO Menu
		END SELECT

		!
		! Didn't answer "Y"
		!
		GOTO Menu IF OPT$ <> "Y"

		!
		! Actual delete command
		!
		ST% = LIBR_DELETE(LIB_FILE$, KEY_NAME$)

		IF (ST% AND 1%) = 0%
		THEN
			CALL ENTR_3MESSAGE(SCOPE, "Error in delete " + NUM1$(ST%), 0%)
		END IF

		!
		! Reload text
		!
		GOSUB LoadAll

		!
		! Re-display text
		!
		V% = DSPL_SCROLL(SMG_SCROLL, LINE_NUM$(), 0%, "PAINT")

	!
	! Edit text
	!
	CASE "E"
		!
		! Edit file
		!
		ST% = LIBR_EDIT(LIB_FILE$, KEY_NAME$)

		!
		! Refresh screen
		!
		ST% = SMG$REPAINT_SCREEN(SCOPE::SMG_PBID)

		!
		! Reload text
		!
		GOSUB LoadAll

		!
		! Re-display text
		!
		V% = DSPL_SCROLL(SMG_SCROLL, LINE_NUM$(), 0%, "PAINT")

	!
	! extract file
	!
	CASE "T"
 MagicExtract:
		FINAME$ = TRM$(ENTR_3STRING(SCOPE, SCOPE::SMG_OPTION, "", &
			"File to write into", SPACE$(32%), 0%, "", ""))

		SELECT SCOPE::SCOPE_EXIT

		!
		! Exit keys
		!
		CASE SMG$K_TRM_F8, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO Menu

		!
		! Good keys
		!
		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

		!
		! Bad keys
		!
		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO Menu

		END SELECT

		!
		! Null file name
		!
		GOTO Menu IF FINAME$ = ""

		!
		! Extract file
		!
		ST% = LIBR_EXTRACT(LIB_FILE$, FINAME$, KEY_NAME$)

		IF (ST% AND 1%) = 0%
		THEN
			CALL ENTR_3MESSAGE(SCOPE, "Error in extract " + NUM1$(ST%), 0%)
			GOTO Menu
		END IF

	!
	! Help
	!
	CASE "H"
		!CALL HELP_34MESSAGE(SCOPE, "", PRG_IDENT, PRG_PROGRAM, "",'HELP')
		PS% = INSTR(1%, KEY_NAME$ + "$", "$")
		PREFF$ = LEFT(KEY_NAME$, PS% - 1%)
		CALL HELP_34MESSAGE(SCOPE, "", "H", &
			LEFT(KEY_NAME$, 2%) + "_FORM", &
			"", PREFF$)

	!
	! Print the Help file
	!
	CASE "P"
 PrintFile:	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, "Output To: ", &
			1%, 1%, 1%)

		RINP$ = SPACE$(67%)

		SELECT ENTR_3ENTER(SCOPE, SCOPE::SMG_OPTION, 1%, 13%, &
			RINP$, -1%, 16% + 4096)

		!
		! ^C, ^Z, Exit
		!
		CASE SMG$K_TRM_CTRLC, &
			SMG$K_TRM_F8,	SMG$K_TRM_F10, &
			SMG$K_TRM_CTRLZ

			GOTO Menu

		!
		! Good keys
		!
		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO, &
			SMG$K_TRM_F7

		!
		! Bad keys
		!
		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO PrintFile
		END SELECT

		RINP$ = EDIT$(RINP$, 4% + 8% + 128% + 256%)

		!
		! Extract help from library
		!
		APP_FILE$ = ""
		ST% = LIBR_EXTRACT(LIB_FILE$, TEMP_FILE$, KEY_NAME$)
		IF (ST% AND 1%) = 1%
		THEN
			APP_FILE$ = TEMP_FILE$
		END IF

4900		XYZ$ = "TYPE/OUTPUT=" + RINP$ + " " + APP_FILE$

		SMG_STATUS% = SMG$DISABLE_BROADCAST_TRAPPING(SCOPE::SMG_PBID)
		SMG_STATUS% = LIB$SPAWN(XYZ$)
		SLEEP 1%
		SMG_STATUS% = SMG$SET_BROADCAST_TRAPPING(SCOPE::SMG_PBID, &
			LOC(READ_3BROADCAST), LOC(SCOPE))
		SMG_STATUS% = SMG$REPAINT_SCREEN(SCOPE::SMG_PBID)

		GOSUB 8000

	!
	! Exit
	!
	CASE "X"
		GOTO ExitProgram

	END SELECT

	OPTFLAG% = 0%
	GOTO Menu

 ExitProgram:
5000	!
	! Finish out
	!

 ExitProgram2:
	!
	! Delete all displays
	!
	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SVD)

	SCOPE::PRG_IDENT	= OLD_IDENT$
	SCOPE::PRG_PROGRAM	= OLD_PROGRAM$
	SCOPE::PRG_ITEM		= OLD_ITEM$

	EXIT FUNCTION

	%PAGE

 LoadAll:
6000	!

6020	LINE_NUM$(0%) = "0"

	ST% = LIBR_NODIGSR(LIB_FILE$, KEY_NAME$, LINE_NUM$())

	CURR_LINE% = VAL%(LINE_NUM$(0%))

6600	SMG_SCROLL::BOT_ARRAY, SMG_SCROLL::END_ELEMENT = CURR_LINE%
	SMG_SCROLL::BOT_ARRAY, SMG_SCROLL::END_ELEMENT = 1% &
		IF CURR_LINE% < 0%

	SMG_SCROLL::TOP_LINE = SMG_SCROLL::BEG_ELEMENT

	RETURN

	%PAGE

8000	!*******************************************************************
	! Try to kill all versions of the temp file
	!*******************************************************************

 !	WHEN ERROR IN
 !		KILL TEMP_FILE$
 !		KILL TEMP_FILE$
 !		KILL TEMP_FILE$
 !		KILL TEMP_FILE$
 !		KILL TEMP_FILE$
 !		KILL TEMP_FILE$
 !	USE
 !		CONTINUE 8020
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(TEMP_FILE$ + ";*")

8020	RETURN

32767	END FUNCTION
	!+-+-+
	!++
	! Abstract:DELETE
	!	^*Delete\*
	!	.p
	!	The ^*Delete\* is used to remove a form that is unecessary or
	!	unwanted.
	!
	! Index:
	!	.x Delete>Form
	!	.x Form>Delete
	!
	!--
