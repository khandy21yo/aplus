1	%TITLE "Window to Maintain Text Files"
	%SBTTL "TK_MAIN_TEXTFILEWINDOW"
	%IDENT "V3.6a Calico"

	FUNCTION STRING TK_MAIN_TEXTFILEWINDOW(STRING WPOSITION, STRING WSIZE, &
		STRING FILE_NAME, STRING PREFIX, STRING SUFFIX, &
		STRING TITLE, INTEGER XFLAG)

	!
	! COPYRIGHT (C) 1987 BY
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
	!	A window that allows a user to maintain a list
	!	of text files.  Maintenance options include
	!	edit, select, delete, help, and exit.
	!	.b
	!	.literal
	!		XFLAG		RESULT
	!		========================================================
	!		FSCAN FLAGS
	!		  0%		Full file specification(FSCN$_FILESPEC)
	!		  1%		Node name; icludes two colons and the
	!				access control string(if specified).
	!		  2%		Device name; includes colon.
	!		  4%		Root directory; includes colon.
	!		  8%		Directory name; includes brackets
	!				(or angle brackets).
	!		 16%		File name; includes quotation marks
	!				(if any).
	!		 32%		File type; includes period.
	!		 64%		Version number; includes semicolon
	!				(or period).
	!		NON FSCAN FLAGS
	!		256%		Internal title
	!		512%		Reverse window - border.
	!	.end literal
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_MAIN_TEXTFILEWINDOW/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP TK_MAIN_TEXTFILEWINDOW
	!	$ DELETE TK_MAIN_TEXTFILEWINDOW.OBJ;*
	!
	! AUTHOR:
	!
	!	05/01/87 - Robert Peterson
	!
	! MODIFICATION HISTORY:
	!
	!	02/10/89 - Kevin Handy
	!		Modified for changes in ENTR_NOLSTRING
	!
	!	09/23/89 - Kevin Handy
	!		modified to use ENTR_ENTER instead of
	!		ENTR_NOLSTRING.
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
	!	08/30/99 - Kevin Handy
	!		Somewhat better error trapping on EDT$EDIT
	!		calls (maybe now it won't crash to the $)
	!
	!	09/19/2000 - Kevin Handy]
	!		Use LIB$DELETE_FILE instead of KILL
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[SMG.OPEN]SMG_SCROLL.HB"
	DECLARE  SMG_SCROLL_CDD WINDOW_SCROLL

	EXTERNAL LONG    FUNCTION EDT$EDIT
	EXTERNAL INTEGER FUNCTION DSPL_SCROLL

	OLD_ITEM$ = SCOPE::PRG_ITEM		! Save the COMMON item

	SCOPE::PRG_ITEM = ""

	!
	! Declare vars
	!
	DECLARE LONG SMG_SCROLL, SYS_STATUS
	DECLARE LONG PROW, PCOL, WROWS, WCOLS

	DIM FILE$(1000%)

	CALL DSPL_SPLITCURSOR(WPOSITION, PROW,  PCOL)
	CALL DSPL_SPLITCURSOR(WSIZE,     WROWS, WCOLS)

1000	!***************************************************************
	! Select Text File
	!***************************************************************

	!
	! Create the data display
	!
	!
	! Create the window
	!
	IF (XFLAG AND 512%)
	THEN
		SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(WROWS, WCOLS, &
			SMG_SCROLL, SMG$M_BORDER, SMG$M_REVERSE)
	ELSE
		SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(WROWS, WCOLS, &
			SMG_SCROLL, SMG$M_BORDER)
	END IF

	!
	! Output the title and title line
	!
	IF (XFLAG AND 256%)
	THEN
		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCROLL, &
			" " + TITLE + " ", 1%, 1%)
		SMG_STATUS% = SMG$DRAW_LINE(SMG_SCROLL, 2%, 1%, 2%, WCOLS)
	ELSE
		SMG_STATUS% = SMG$LABEL_BORDER(SMG_SCROLL, " " + TITLE + " ", &
			SMG$K_TOP,, SMG$M_BOLD)
	END IF

	!
	! Define windows
	!
	WINDOW_SCROLL::WINDOW		= SMG_SCROLL
	WINDOW_SCROLL::TOP_ARRAY	= 1%
	WINDOW_SCROLL::BOT_ARRAY	= 1%
	WINDOW_SCROLL::SCROLL_TOP	= 1%
	WINDOW_SCROLL::SCROLL_TOP	= 3% IF (XFLAG AND 256%)
	WINDOW_SCROLL::SCROLL_BOT	= 12%
	WINDOW_SCROLL::TOP_LINE		= 1%
	WINDOW_SCROLL::BEG_ELEMENT	= 1%
	WINDOW_SCROLL::END_ELEMENT	= 1%
	WINDOW_SCROLL::CUR_LINE		= 1%
	WINDOW_SCROLL::CUR_W_ROW	= 1%
	WINDOW_SCROLL::CUR_W_COL	= 1%
	WINDOW_SCROLL::FIND_LINE	= 1%
	WINDOW_SCROLL::SMG_FLAG		= 0%
	WINDOW_SCROLL::PROMPT		= "->"
	WINDOW_SCROLL::VIDEO_COMP	= 0%
	WINDOW_SCROLL::CHARSET		= 0%
	WINDOW_SCROLL::DRAW_COLS	= ""

	!
	! Print the array
	!
	GOSUB LookUp

	TEMP = DSPL_SCROLL(WINDOW_SCROLL, FILE$(), 0%, "PAINT")
	TK_MAIN_TEXTFILEWINDOW = FILE$(WINDOW_SCROLL::CUR_LINE)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_SCROLL, SCOPE::SMG_PBID, &
		PROW, PCOL)

	OPT% = 0%

	OPLIST$ = "Edit Create Delete Help eXit" &

 Menu:
1050	SCOPE::PRG_ITEM= ""

	OPTION$ = ENTR_3OPTION(SCOPE, "COMMAND", OPLIST$, OPT%, FLAGW%)

	SELECT SCOPE::SCOPE_EXIT
	CASE SMG$K_TRM_UP, SMG$K_TRM_DOWN, &
		SMG$K_TRM_PREV_SCREEN, SMG$K_TRM_NEXT_SCREEN, &
		SMG$K_TRM_F18, SMG$K_TRM_F19

		V% = DSPL_SCROLL(WINDOW_SCROLL, FILE$(), SCOPE::SCOPE_EXIT, "")
		TK_MAIN_TEXTFILEWINDOW = FILE$(WINDOW_SCROLL::CUR_LINE)

		GOTO Menu

	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_SELECT

		GOTO ExitProgram

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 1050

	END SELECT

	SELECT OPTION$

	CASE "C"

1065		SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, &
			"New File Name: ", 1%, 1%, 1%)

		FILE_NAME$ = SPACE$(64%)
		SELECT ENTR_3ENTER(SCOPE, SCOPE::SMG_OPTION, &
			1%, 16%, FILE_NAME$, -1%, 16% + 4096%)

		! ^C, ^Z, PF2, Exit
		CASE SMG$K_TRM_CTRLC, SMG$K_TRM_PF2, &
			SMG$K_TRM_F10,	SMG$K_TRM_CTRLZ
			GOTO 1050

		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
			! Good keys

		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO 1065
		END SELECT

		FILE_NAME$ = EDIT$(FILE_NAME$, 4% + 8% + 128% + 256%)

		GOTO 1065 IF FILE_NAME$  = ""

		FOR LOOP% = 1% TO WINDOW_SCROLL::BOT_ARRAY
			IF FILE$(LOOP%) = FILE_NAME$
			THEN
				CALL ENTR_3MESSAGE(SCOPE, &
					"Duplicate file name . . .", 0%)
				GOTO 1065
			END IF
		NEXT LOOP%

		CALL ENTR_3MESSAGE(SCOPE, "Entering the editor 'EDT' " + &
			FILE_NAME$, 1%)

		SYS_STATUS = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 0%)

		WHEN ERROR IN
			SYS_STATUS = EDT$EDIT(PREFIX + &
				FILE_NAME$ + SUFFIX,,,,,,,)
		USE
			FILENAME$ = PREFIX + FILE_NAME$ + SUFFIX
			CONTINUE HelpError
		END WHEN

		SYS_STATUS = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)

		IF (SMG_STATUS% AND 1%) = 0%
		THEN
			CALL ENTR_3MESSAGE(SCOPE, &
				"ERROR in edit!!!! " + NUM1$(SMG_STATUS%), 0%)
			GOTO 1050
		END IF

		GOSUB LookUp

		WINDOW_SCROLL::CUR_LINE = 1%
		WINDOW_SCROLL::CUR_LINE = LOOP% &
			IF FILE$(LOOP%) = EDIT$(FILE_NAME$, -1%) &
			FOR LOOP% = 1% TO WINDOW_SCROLL::BOT_ARRAY

		WINDOW_SCROLL::FIND_LINE = WINDOW_SCROLL::CUR_LINE

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_SCROLL)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

		SMG_STATUS% = SMG$REPAINT_SCREEN(SCOPE::SMG_PBID)

		V% = DSPL_SCROLL(WINDOW_SCROLL, FILE$(), 0%, "PAINT")
		TK_MAIN_TEXTFILEWINDOW = FILE$(WINDOW_SCROLL::CUR_LINE)

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_SCROLL)

	!
	! Edit Text file
	!
	CASE "E"
		IF FILE$(WINDOW_SCROLL::CUR_LINE) = ""
		THEN
			CALL ENTR_3MESSAGE(SCOPE, &
				"File does not exits.  Use the Create option.", 0%)
			GOTO 1050
		END IF
		CALL ENTR_3MESSAGE(SCOPE, "Entering the editor 'EDT' " + &
			FILE$(WINDOW_SCROLL::CUR_LINE), 1%)

		SYS_STATUS = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 0%)

		WHEN ERROR IN
			SYS_STATUS = EDT$EDIT(PREFIX + &
				FILE$(WINDOW_SCROLL::CUR_LINE) + SUFFIX,,,,,,,)
		USE
			FILENAME$ = FILE_NAME$ + SUFFIX
			CONTINUE HelpError
		END WHEN

		SYS_STATUS = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)

		IF (SMG_STATUS% AND 1%) = 0%
		THEN
			CALL ENTR_3MESSAGE(SCOPE, &
				"ERROR in edit!!!! " + NUM1$(SMG_STATUS%), 0%)
			GOTO 1050
		END IF

		GOSUB LookUp

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_SCROLL)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

		SMG_STATUS% = SMG$REPAINT_SCREEN(SCOPE::SMG_PBID)

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_SCROLL)

	!
	! Delete a file
	!
	CASE "D"
1100		YESNO$ = ENTR_3YESNO(SCOPE, SMG_SCREEN_DATA%, &
			"", "Confirm deletion", "N", 0%, "", "")

		SELECT SCOPE::SCOPE_EXIT
		! ^C, ^Z, PF2, Exit
		CASE SMG$K_TRM_CTRLC, SMG$K_TRM_PF2, &
			SMG$K_TRM_F10, SMG$K_TRM_CTRLZ

			GOTO 1050

		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
			! Good keys

		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO 1100
		END SELECT

		IF LEFT(YESNO$, 1%) <> "Y"
		THEN
			GOTO 1050

		ELSE
			CALL ENTR_3MESSAGE(SCOPE, &
				"Deleting " + FILE$(WINDOW_SCROLL::CUR_LINE), &
				1%)
 !			WHEN ERROR IN
 !				KILL PREFIX + FILE$(WINDOW_SCROLL::CUR_LINE) + &
 !					SUFFIX WHILE (-1)
 !			USE
 !				CONTINUE 1110
 !			END WHEN

			SMG_STATUS% = LIB$DELETE_FILE(PREFIX + &
				FILE$(WINDOW_SCROLL::CUR_LINE) + SUFFIX + ";*")

		END IF

1110		GOSUB LookUp

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_SCROLL)

		SMG_STATUS% = SMG$REPAINT_SCREEN(SCOPE::SMG_PBID)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

		V% = DSPL_SCROLL(WINDOW_SCROLL, FILE$(), 0%, "PAINT")
		TK_MAIN_TEXTFILEWINDOW = FILE$(WINDOW_SCROLL::CUR_LINE)

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_SCROLL)

	!
	! Exit
	!
	CASE "X"
		GOTO ExitProgram

	!
	! Help
	!
	CASE "H"
		CALL HELP_3MESSAGE(SCOPE, "", "PROG", SCOPE::PRG_PROGRAM, &
			"HELP")
		GOTO 1050

	END SELECT

	GOTO 1050

 LookUp:
	!--------------------------------------------------------------
	! Look up file name
	!--------------------------------------------------------------
	CALL FIND_FILE(FILE_NAME, FILE$(), XFLAG, PREFIX, SUFFIX)

	WINDOW_SCROLL::BOT_ARRAY	= VAL%(FILE$(0%))
	WINDOW_SCROLL::BOT_ARRAY	= 1% IF WINDOW_SCROLL::BOT_ARRAY = 0%
	WINDOW_SCROLL::END_ELEMENT	= WINDOW_SCROLL::BOT_ARRAY

	FILE$(WINDOW_SCROLL::BOT_ARRAY + 1%) = ""

	RETURN

	%PAGE

 ExitProgram:
	!
	! Delete all displays
	!
	SMG_STATUS% = SMG$POP_VIRTUAL_DISPLAY(SMG_SCROLL, SCOPE::SMG_PBID)

	SCOPE::PRG_ITEM	= OLD_ITEM$		! Restore the COMMON item

	EXIT FUNCTION

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	GOTO ExitProgram

32767	END FUNCTION
