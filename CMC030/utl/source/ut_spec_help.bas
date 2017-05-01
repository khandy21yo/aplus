1	%TITLE "Maintain Help Libraries"
	%SBTTL "UT_SPEC_HELP"
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
	!	The ^*HELP\* option maintains the help libraries.
	!
	! Index:
	!	.x Help
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UT_SPEC_HELP/LINE
	!	$ LINK/EXE=UTL_EXE:*.EXE UT_SPEC_HELP, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE UT_SPEC_HELP.OBJ;*
	!
	! Author:
	!
	!	07/07/87 - Kevin Handy
	!
	! Modification history:
	!
	!	01/88 - Kevin Handy
	!		Added Next, and Display options.
	!
	!	04/06/88 - Kevin Handy
	!		Modified print option so it will not run text
	!		through runoff if display mode turned on.
	!
	!	02/10/89 - Kevin Handy
	!		Modified for changes in ENTR_NOLSTRING.
	!
	!	06/27/89 - Kevin Handy
	!		Modified to use READ_INITIALIZE.
	!
	!	09/23/89 - Kevin Handy
	!		Modified to use ENTR_ENTER instead of
	!		ENTR_NOLSTRING.
	!
	!	10/24/89 - Kevin Handy
	!		Modified so that this program can be spawned
	!		from HELP_MESSAGE to maintain system help
	!		messages, instead of putting all of that code
	!		into each program.
	!
	!	01/15/90 - Frank F. Starman
	!		Rename TK_MAST_HELP to UT_SPEC_HELP
	!
	!	05/20/90 - Frank F. Starman
	!		Erase display before exit program.
	!
	!	08/17/90 - Kevin Handy
	!		Remove connect option.
	!
	!	08/14/91 - Kevin Handy
	!		Deleted PRINT option, because it didn't even
	!		partially work anymore (frank?), and couldn't
	!		work with all of the odd commands that have been
	!		added that are not really RUNOFF commands.
	!
	!	08/20/91 - Kevin Handy
	!		Changed default key from "UNDEFINED" to "".
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!		Fix last param of entr_3choice
	!
	!	09/19/95 - Kevin Handy
	!		Lose extra include of CONSTANTS.INC.
	!		Reformat closer to 80 columns.
	!
	!	09/16/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/13/99 - Kevin Handy
	!		Use BASIC$STARLET for LIB$ SS$
	!
	!	07/27/2000 - Kevin Handy
	!		Lose ON ERROR trapping.
	!
	!	09/19/2000 - Kevin Handy
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

	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "$SSDEF" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	%INCLUDE "SOURCE:[SMG.OPEN]SMG_SCROLL.HB"
	DECLARE	SMG_SCROLL_CDD	SMG_SCROLL

	!
	! External functions
	!
	EXTERNAL INTEGER FUNCTION DSPL_SCROLL

	EXTERNAL LONG FUNCTION LIBR_DELETE
	EXTERNAL LONG FUNCTION LIBR_DIGSR
	EXTERNAL LONG FUNCTION LIBR_NODIGSR
	EXTERNAL LONG FUNCTION LIBR_EDIT
	EXTERNAL LONG FUNCTION LIBR_EXTRACT
	EXTERNAL LONG FUNCTION LIBR_3INSERT
	EXTERNAL LONG FUNCTION HELP_INSERTSOURCE

	!
	! Declarations
	!
	DECLARE LONG SVD
	DECLARE INTEGER CONSTANT NUM_LINES = 6000	! Size of the array
	DECLARE INTEGER CONSTANT LIB_INDEX = 2000	! Size of the array
	DECLARE INTEGER CONSTANT TEXT_FILE = 400	! Size of the array


	%PAGE

100	!
	! Initilize stuff
	!
	CALL READ_INITIALIZE

	!
	! Create virtual display
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(18%, 78%, SVD)

	!
	! Label the borders of system and user
	!
	SMG_STATUS% = SMG$LABEL_BORDER(SVD, " HELP ",,, SMG$M_BOLD)

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

	SMG_SCROLL::SMG_FLAG	= 1%
	SMG_SCROLL::PROMPT	= ""
	SMG_SCROLL::DRAW_COLS	= ""

	!
	! Dimension statements
	!
	DIM LINE_NUM$(NUM_LINES), TEXT_FILE$(TEXT_FILE), &
		LIB_INDEX$(LIB_INDEX), RFA LIB_RFA(LIB_INDEX)

	%PAGE

	!
	! Option list
	!
	OPT% = 0%
	DIGSR_FLAG% = 0%

	!
	! Which library is it in?
	!
	SYS_STATUS% = LIB$GET_SYMBOL("CMC$HELP_LIBRARY" BY DESC, &
		LFILE$ BY DESC,,)
	SYS_STATUS% = LIB$GET_SYMBOL("CMC$HELP_KEY" BY DESC, &
		KEY_NAME$ BY DESC,,)

	IF (LFILE$ = "") OR (KEY_NAME$ = "")
	THEN
		LFILE$ = "REF:HELP_DEFAULT"
		KEY_NAME$ = ""
		SPAWN_FLAG% = 0%
	ELSE
		SPAWN_FLAG% = -1%
	END IF

	DEV_NAME$ = "REF:"

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
	! Relabel the border
	!
	SMG_STATUS% = SMG$LABEL_BORDER(SVD, &
		" HELP - Lib: " + LFILE$ + "  Key: " + KEY_NAME$,,, SMG$M_BOLD)

	!
	! Enter desired option
	!
	OPTFLAG% = 0%
	SCOPE::PRG_ITEM= ""
	OPLIST$ = "Library-name Key-name Edit Delete exTract " + &
		"Next displaY chanGe-key Help Spell eXit"
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
	! Spell file
	!
	CASE "S"
		!
		! Call the spelling function
		!
		CALL HELP_SPELL(LFILE$, KEY_NAME$)

		!
		! Insert text in source code
		!
		ST% = HELP_INSERTSOURCE(LFILE$, KEY_NAME$)

		!
		! Reload text
		!
		GOSUB LoadAll

		!
		! Re-display text
		!
		V% = DSPL_SCROLL(SMG_SCROLL, LINE_NUM$(), 0%, "PAINT")

	!
	! Change library name
	!
	CASE "L"
 MagicLibrary:
		L1FILE$ = SPACE$(39%)
		LSET L1FILE$ = LFILE$

		LFILE$ = TRM$(ENTR_3STRING(SCOPE, SCOPE::SMG_OPTION, "", &
			"Library file name", L1FILE$, 0%, "", ""))

		SELECT SCOPE::SCOPE_EXIT
		!
		! ^C
		!
		CASE SMG$K_TRM_CTRLC
			GOTO Menu

		!
		! Exit key
		!
		CASE SMG$K_TRM_F8, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO Menu

		!
		! List Help Files
		!
		CASE SMG$K_TRM_F14
			TEXT_FILE$(LOOP%) = "" &
				FOR LOOP% = 1% TO TEXT_FILE

			CALL FIND_FILE(DEV_NAME$ + "*.TLB", TEXT_FILE$(), &
				16%, "", "")

			X% = ENTR_3CHOICE(SCOPE, "", "", TEXT_FILE$(), "", &
				8%, "Library", "", 0%)

			IF X% > 0%
			THEN
				LFILE$ = TEXT_FILE$(X%)
			ELSE
				GOTO MagicLibrary
			END IF

		!
		! Good keys
		!
		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO, SMG$K_TRM_F7

		!
		! Bad keys
		!
		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO MagicLibrary
		END SELECT

		!
		! Reload text
		!
		GOSUB LoadAll

		!
		! Re-display text
		!
		V% = DSPL_SCROLL(SMG_SCROLL, LINE_NUM$(), 0%, "PAINT")


	!
	! Change key name
	!
	CASE "K"
 MagicKey:
	!++
	! Abstract:KEY-NAME
	!	^* Key Name\*
	!
	! Index:
	!	.X Key name
	!
	!--
		L1FILE$ = SPACE$(39%)
		LSET L1FILE$ = KEY_NAME$

		KEY_NAME$ = TRM$(ENTR_3STRING(SCOPE, SCOPE::SMG_OPTION, "", &
			"Key name", L1FILE$, 0%, "", ""))

		SELECT SCOPE::SCOPE_EXIT
		!
		! ^C
		!
		CASE SMG$K_TRM_CTRLC
			GOTO Menu

		!
		! Exit key
		!
		CASE SMG$K_TRM_F8, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO Menu

		!
		! List Help Files
		!
		CASE SMG$K_TRM_F14
			LIB_INDEX$(LOOP%) = "" &
				FOR LOOP% = 1% TO LIB_INDEX

			CALL LIBR_INDEX(LFILE$, "*", LIB_INDEX$(), &
				LIB_RFA())

			X% = ENTR_3CHOICE(SCOPE, "", "", LIB_INDEX$(), "", &
				8%, "Key Name", "", 0%)

			IF X% > 0%
			THEN
				KEY_NAME$ = LIB_INDEX$(X%)
			ELSE
				GOTO MagicKey
			END IF

		!
		! Good keys
		!
		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO, SMG$K_TRM_F7

		!
		! Bad keys
		!
		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO MagicKey
		END SELECT

		!
		! Reload text
		!
		GOSUB LoadAll

		!
		! Re-display text
		!
		V% = DSPL_SCROLL(SMG_SCROLL, LINE_NUM$(), 0%, "PAINT")

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
		ST% = LIBR_DELETE(LFILE$, KEY_NAME$)

		IF (ST% AND 1%) = 0%
		THEN
			CALL ENTR_3MESSAGE(SCOPE, &
				"Error in delete " + NUM1$(ST%), 0%)
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
	!++
	! Abstract:EDIT
	!	^*Using the Editor\*
	!	.B
	!	^*Editor\*
	!	.b
	!	.lm +5
	!	The EDT editor is discussed briefly here.
	!	EDT has advanced screen
	!	functions plus line editing capabilities.
	!	EDT screen editing can only be used on supported DEC VT terminals.
	!	EDT line mode should be usable on any terminal.
	!	.b
	!	The following sections provide a brief description of how to use both the line
	!	editor and keypad editor. For more information, refer to
	!	VAX/VMS Volume 3 - Text Editors, Formatters. The EDT editor is the
	!	standard editor on VAX/VMS systems and is available at the DCL ($) prompt.
	!	.b
	!	EDT provides two basic methods of editing: line and keypad editing. The line
	!	editor requires that you specify both the editing command and the line(s) of
	!	text you want the command to affect; and would probably be the method of
	!	editing on a non-VTxxx type terminal. The keypad editor is used only on DEC
	!	VTxxx-compatible terminals. Changes are displayed as they take place by
	!	moving the cursor directly to the text to be changed and pressing one or two
	!	keys to perform commands.
	!	.B
	!	^*The Line Editor\*
	!	.b
	!	Line editing requires that you type a command and specify the line (or
	!	lines) to be changed. Most commands can be abbreviated. The text is stored
	!	in memory space called a buffer. The editor can handle many buffers, but
	!	novice users should not try to be too fancy with buffers.
	!	.b
	!	The following is a list of commonly used commands:
	!	.table 3,25
	!	.te
	!	^*CHANGE\* Change from line editing to keypad editing.
	!	.tE
	!	^*COPY\* Duplicate text in specified locations.
	!	.tE
	!	^*DELETE\* Delete specified range of lines.
	!	.te
	!	^*EXIT\* End editing session and store buffer.
	!	.te
	!	^*FIND\* Locate specified line.
	!	.te
	!	^*HELP\* Invoke editor help facility.
	!	.te
	!	^*INCLUDE\* Copies specified file into text buffer.
	!	.te
	!	^*INSERT\* Insert text in the buffer.
	!	.te
	!	^*MOVE\* Delete text from one location and insert it in another.
	!	.te
	!	^*QUIT\* End editing session by deleting buffer.
	!	.te
	!	^*RESEQUENCE\* Renumber buffer by increments of one.
	!	.e
	!	^*SUBSTITUTE\* Replace one character string with another.
	!	.tE
	!	^*TYPE\* Display specified range of lines.
	!	.tE
	!	^*WRITE\* Copy specified range from buffer to specified file.
	!	.End table
	!	.B 2
	!	^*Help\*
	!	.b
	!	The line editor has a ^*HELP\* command which provides information on
	!	a particular
	!	command. For example, to get help on the ^*TYPE\* command after you
	!	have invoked
	!	the editor, enter ^*HELP TYPE\*.
	!	.B 2
	!	^*Creating a File\*
	!	.b
	!	To create a new file, enter the ^*EDIT\* command, specifying a file's name
	!	as a parameter. For example, enter the following command at the system
	!	prompt to create a file SAMPLE.TXT:
	!	.B 2
	!	^*$ EDIT SAMPLE.TXT\*
	!	.B
	!	On the Computer the editor will go automatically into the keypad mode. This
	!	means the system will return a message `Input file does not exist', and `[EOB]'
	!	at the top of the screen . [This is only on a VT compatible terminal. If you
	!	do not have a VT compatible terminal, you will see the message `Input file does
	!	not exist' and then `C_*'.] To get to the line mode, press ^*CTRL-Z\*. You
	!	will now see an asterisk (_*). The message `Input file does not exist'
	!	means you do not have a file by that name in your directory. The asterisk
	!	(_*) is the prompt displayed by the line editor. To insert text into the
	!	file, type ^*INSERT\* followed by a ^*CR\*. Whatever you type after this
	!	command will be inserted as text until you type a ^*CTRL-Z\* to terminate
	!	insertion of text.
	!	.b
	!	Whatever you type is placed into temporary storage in a buffer until you file
	!	it by using the ^*EXIT\* command. The buffer will then
	!	be saved into whatever name you specified when first invoking the editor. If
	!	you do not wish to save your inputs, use the ^*QUIT\* command.
	!	.B 2
	!	^*Editing a File\*
	!	.b
	!	Edit an existing file the same way you would create one. Enter the
	!	^*EDIT\* command followed by the name of the file that you wish to edit.
	!	The latest version of the file will appear on the screen. As before, the
	!	editor will be in keypad mode and you must press ^*CTRL-Z\* to get to line
	!	mode. Again use the ^*EXIT\* command to store your changes, or
	!	the ^*QUIT\* command if you do not want to alter the file.
	!	.B 2
	!	^*Specifying Ranges\*
	!	.b
	!	Line numbers in increments of one are initially assigned to each line. These
	!	numbers can be used to specify the line or range of lines you wish a command to
	!	act upon. For example, to delete lines 3 through 6 in a file, you could type:
	!	^*DELETE 3 THRU 6\*. Following is a partial list of range specifications:
	!	.B 2
	!	.table 3,25
	!	.te
	!	^*.\* Current line.
	!	.tE
	!	^*n THRU n\* Line number n through line number n.
	!	.tE
	!	^*n:n\* Line number n through line number n.
	!	.tE
	!	^*BEGIN\* First line of the buffer.
	!	.tE
	!	^*END\* Empty line after last line of buffer.
	!	.tE
	!	^*BEFORE\* All the lines before the current line.
	!	.tE
	!	^*REST\* All lines including and after current line.
	!	.tE
	!	^*WHOLE\* All the lines in the buffer.
	!	.End table
	!	Using the ^*CR\* key allows you to move forward one line at
	!	a time.
	!	.B 2
	!	^*Inserting and Deleting Text\*
	!	.b
	!	To insert text use the ^*INSERT\* command. Text will be inserted
	!	immediately before the current line. If you specify a range, text will be
	!	inserted immediately before the first line of the specified range. Terminate
	!	the insertion of text with a ^*CTRL-Z\*.
	!	.b
	!	To delete text use the ^*DELETE\* command. If you do not specify a range,
	!	the current line is deleted.
	!	.B 2
	!	^*Substituting Text\*
	!	.b
	!	To replace one character string with another, use the ^*SUBSTITUTE\*
	!	command. To replace every occurrence of the string with another, use
	!	^*WHOLE\* as the parameter.
	!	.B 2
	!	^*Locating Text\*
	!	.b
	!	To display the entire buffer, use the ^*TYPE\* command
	!	specifying WHOLE as the parameter. To locate a string of text without
	!	displaying the entire buffer, use quotation marks around the text string.
	!	For example, ^*TYPE "sample"\* to locate the first occurrence
	!	of the word 'sample'.
	!	.B 2
	!	^*The Keypad Editor\*
	!	.b
	!	The keypad refers to the small group of keys to the right of the larger
	!	keyboard on your VTxxx-type terminal. Each key performs at least one editing
	!	function, sometimes two. To use the alternate function of the key, press the
	!	GOLD key or PF1 key before pressing the second key.
	!	.b
	!	To invoke the keypad editor, use the CHANGE command after you have invoked the
	!	editor. To display the diagram of the keypad and various key functions, press
	!	the HELP key or PF2 key. For information about a specific key, press the HELP
	!	key followed by the key in question.
	!	.B 2
	!	^*Creating A File\*
	!	.b
	!	To create a new file, enter the ^*EDIT\* command, specifying a file's name
	!	as a parameter.
	!	For example, enter the following command at the system
	!	prompt to create a file SAMPLE.TXT:
	!	^*$ EDIT SAMPLE.TXT\*
	!	.b
	!	On the Computer the editor will go automatically into the keypad mode. This
	!	means the system will return a message `Input file does not exist', and `[EOB]'
	!	at the top of the screen . [This is only on a VT compatible terminal. If you
	!	do not have a VT compatible terminal, you will see the message `Input file does
	!	not exist' and then `C_*'.] The message `Input file does not exist' means you do
	!	not have a file by that name in your directory. To insert text into the file
	!	just type the text. To save your file, type ^*CTRL-Z\* to invoke the line
	!	mode and type ^*EXIT <CR>\*. If you do not want to save your edits, type
	!	^*QUIT <CR>\*. To get back to keypad mode from the line mode, type
	!	^*CHANGE <CR>\* after the `_*' prompt.
	!	.B 2
	!	^*Editing A File\*
	!	.b
	!	To edit a file, issue the ^*EDIT\* command, specifying the file name as a
	!	parameter. The first 22 lines of your file will appear. You may now start
	!	editing your file.
	!	.B 2
	!	^*Manipulating The Cursor\*
	!	.b
	!	Use the arrow keys to move the cursor one character in the direction of the
	!	arrow. Use the WORD key on the VT keypad to move to
	!	the beginning of the next word. To move to the end of the line, use the EOL
	!	key. The BACKSPACE key on the main keyboard moves the cursor to the beginning
	!	of the line. The SECTION key moves the cursor 16-lines through the text and
	!	the PAGE key moves the cursor to the next (FF) character. (The cursor will
	!	move forward or backward depending upon whether you set the last direction of
	!	the cursor with the ADVANCE or BACKUP command.) The BOTTOM key moves the
	!	cursor to the line following the last character; the TOP key moves it to the
	!	first character.
	!	.B 2
	!	^*Deleting/Restoring Text\*
	!	.b
	!	Use DEL C to delete by character, DEL W to delete by word, DEL L to delete
	!	by line; restore by using UND C for characters, UND W for words, and UND L
	!	for lines.
	!	.B 2
	!	^*Locating Text\*
	!	.b
	!	Use the FIND command to search for a particular string between the current
	!	position of the cursor and the beginning or end of the buffer. To find the
	!	next occurrence of the same string, use the FINDNXT command. Use the ADVANCE
	!	or BACKUP command depending on the direction you wish to search.
	!	.B 2
	!	^*Moving Text\*
	!	.b
	!	To move text from one place to another, you "cut" it out of the file and
	!	"paste" it to a new area. To do this, move
	!	the cursor to the beginning of the
	!	line that you wish to move, and press the SELECT key. To mark the end of the
	!	selected range, move the cursor to the end of text to be moved and press the
	!	CUT key. The selected range of text will disappear from the screen and will be
	!	held in the PASTE buffer. To restore the text, move the cursor to the desired
	!	location and press the PASTE key. Text will remain in the buffer until another
	!	CUT operation is performed.
	!	.b 2
	!	^*RUNOFF COMMANDS\* (Till Frank Kills them again)
	!	.b
	!	Special character.
	!	These characters cause formatting to change inside of a line
	!	of test.
	!	.table 3,25
	!	.te
	!	(__)	Take next character as text
	!	.te
	!	(_*)	Bold one character
	!	.te
	!	(_^_*)	Turn on bolding
	!	.te
	!	(_\_*)	Turn off bolding
	!	.te
	!	(_&)	Underline one character
	!	.te
	!	(_^_&)	Turn on underline
	!	.te
	!	(_\_*)	Turn off underline
	!	.te
	!	(_~)	Italicize one character
	!	.te
	!	(_^_~)	Turn on Italics
	!	.te
	!	(_\_~)	Turn off italics
	!	.te
	!	(_#)	Quoted Space
	!	.end table
	!	Dot Commands.
	!	These commands must start on their own line.
	!	They affect formatting from that point to the end of the file.
	!	.table 3,25
	!	.te
	!	(.END FOOTNOTE) (.EFN)
	!	.te
	!	End a footnote started with (.FOOTNOTE)
	!	.te
	!	(.END LIST) (.ELS)
	!	.te
	!	End a list started with (.LIST)
	!	.te
	!	(.END LITERAL)
	!	.te
	!	End a literal started with (.LITERAL)
	!	.te
	!	(.END NOTE)
	!	.te
	!	End a note started with (.NOTE)
	!	.te
	!	(.END QUOTE)
	!	.te
	!	End a quote started with (.QUOTE).
	!	.te
	!	(.BLANK ^~nnn\~) (.B ^~nnn\~)
	!	.te
	!	Insert a blank line.
	!	Do not start a new paragraph.
	!	^~nnn\~ specifies number of blank lines.
	!	.te
	!	(.BREAK) (.BR)
	!	.te
	!	Break text.
	!	This causes any text to be output.
	!	Does not start a new paragraph.
	!	.te
	!	(.FILL) (.F)
	!	.te
	!	Turn on fill.
	!	This causes as much text to be used on each line output as possible,
	!	instead of using the line breaks entered in the source text.
	!	This is the default.
	!	.te
	!	(.FOOTNOTE) (.FN)
	!	.te
	!	Start a footnote.
	!	.te
	!	(.INDENT ^~nnn\~) (.I ^~nnn\~)
	!	.te
	!	Indent the following line by ^~nnn\~ spaces.
	!	Forces a BREAK.
	!	.te
	!	(.LEFT MARGIN ^~nnn\~) (.LM ^~nnn\~)
	!	.te
	!	Left Margin.
	!	moves the left margin depending on ^~nnn\~.
	!	If there is no sign on ^~nnn\~, it positions the margin to that position.
	!	If there if a '+' on ^~nnn\~ it moves the margin to the right that
	!	many spaces.
	!	If there if a '-' on ^~nnn\~ it moves the margin to the left that
	!	many spaces.
	!	.te
	!	(.LITERAL) (.LT)
	!	.te
	!	Starts a literal section.
	!	This section will ignore all formatting commands until it finds
	!	a matching (.END LITERAL).
	!	It will not fill, or justify within that segment.
	!	.te
	!	(.LIST ^~nnn\~,"^~z\~") (.LS ^~nnn\~,"^~z\~")
	!	.te
	!	Start a list.
	!	Will put ^~nnn\~ blank lines between each list item.
	!	List items are specified using the (.LIST ELEMENT) command.
	!	If ^~z\~ is given, will use that character as a bullet.
	!	.te
	!	(.LIST ELEMENT) (.LE)
	!	.te
	!	Starts an item in a list.
	!	.te
	!	(.NF)
	!	.te
	!	No Fill. This turns off (.FILL).
	!	.te
	!	(.NOTE ^~zzz\~) (.NT ^~zzz\~)
	!	.te
	!	Starts a note.
	!	A note consists of a segment of text, indented in both margins,
	!	with a title on top.
	!	The default title is "NOTE", but ^~zzz\~ can specify an alternate.
	!	.te
	!	(.PARAGRAPH) (.P)
	!	.te
	!	Start a paragraph.
	!	A paragraph is indented on the left margin.
	!	.te
	!	(.QUOTE) (.QT)
	!	.te
	!	Start a quote.
	!	This is section of text indented in on both margins.
	!	.te
	!	(.RIGHT MARGIN ^~nnn\~) (.RM ^~nnn\~)
	!	.te
	!	Right Margin.
	!	moves the left margin depending on ^~nnn\~.
	!	If there is no sign on ^~nnn\~, it positions the margin to that position.
	!	If there is a '+' on ^~nnn\~ it moves the margin to the right that
	!	many spaces.
	!	If there is a '-' on ^~nnn\~ it moves the margin to the left that
	!	many spaces.
	!	.end table
	!	.lm -5
	!
	! Index:
	!
	!--
	CASE "E"
		!
		! Edit file
		!
		ST% = LIBR_EDIT(LFILE$, KEY_NAME$)

		!
		! Refresh screen
		!
		ST% = SMG$REPAINT_SCREEN(SCOPE::SMG_PBID)

		!
		! Insert text in source code
		!
		ST% = HELP_INSERTSOURCE(LFILE$, KEY_NAME$)


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
		FILENAME$ = TRM$(ENTR_3STRING(SCOPE, SCOPE::SMG_OPTION, "", &
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
		GOTO Menu IF FILENAME$ = ""

		!
		! Extract file
		!
		ST% = LIBR_EXTRACT(LFILE$, FILENAME$, KEY_NAME$)

		IF (ST% AND 1%) = 0%
		THEN
			CALL ENTR_3MESSAGE(SCOPE, &
				"Error in extract " + NUM1$(ST%), 0%)
			GOTO Menu
		END IF


	!
	! Change key
	!
	CASE "G"
		JUNK$ = SPACE$(39%)
		LSET JUNK$ = KEY_NAME$

		NEW_KEY$ = TRM$(ENTR_3STRING(SCOPE, SCOPE::SMG_OPTION, "", &
			"Assign a new key ", JUNK$, 0%, "", ""))

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

		GOTO Menu IF NEW_KEY$ = ""

		TEMPFILE$ = READ_SYSJOB + ".TMP"
		ST% = LIBR_EXTRACT(LFILE$, TEMPFILE$, KEY_NAME$)

		IF (ST% AND 1%) = 0%
		THEN
			CALL ENTR_3MESSAGE(SCOPE, &
				"Error in extract " + NUM1$(ST%), 0%)
			GOTO Menu
		END IF

		ST% = LIBR_3INSERT(LFILE$, TEMPFILE$, NEW_KEY$)

		IF (ST% AND 1%) = 0%
		THEN
			CALL ENTR_3MESSAGE(SCOPE, "Error in insert " + NUM1$(ST%), 0%)
			GOTO Menu
		END IF

		ST% = LIBR_DELETE(LFILE$, KEY_NAME$)

		IF (ST% AND 1%) = 0%
		THEN
			CALL ENTR_3MESSAGE(SCOPE, &
				"Error in delete " + NUM1$(ST%), 0%)
			GOTO Menu
		END IF

 !		KILL TEMPFILE$
		SMG_STATUS% = LIB$DELETE_FILE(TEMPFILE$ + ";*")

		KEY_NAME$ = NEW_KEY$
		!
		! Reload text
		!
		GOSUB LoadAll

		!
		! Re-display text
		!
		V% = DSPL_SCROLL(SMG_SCROLL, LINE_NUM$(), 0%, "PAINT")

	!
	! Next
	!
	CASE "N"
		LIB_INDEX$(LOOP%) = "" &
			FOR LOOP% = 1% TO LIB_INDEX

		CALL LIBR_INDEX(LFILE$, "*", LIB_INDEX$(), &
			LIB_RFA())

		TEMP% = 1%

		WHILE (TEMP% < VAL%(LIB_INDEX$(0%))) AND &
			(LIB_INDEX$(TEMP%) <= KEY_NAME$)

			TEMP% = TEMP% + 1%
		NEXT

		KEY_NAME$ = LIB_INDEX$(TEMP%)

		!
		! Reload text
		!
		GOSUB LoadAll

		!
		! Re-display text
		!
		V% = DSPL_SCROLL(SMG_SCROLL, LINE_NUM$(), 0%, "PAINT")

	!
	! Display switch
	!
	CASE "Y"
		DIGSR_FLAG% = (DIGSR_FLAG% == 0%)

		IF (DIGSR_FLAG%)
		THEN
			SMG_SCROLL::SMG_FLAG = 0%
		ELSE
			SMG_SCROLL::SMG_FLAG = 1%
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
	! Help
	!
	CASE "H"
		CALL HELP_3MESSAGE(SCOPE, "", SCOPE::PRG_IDENT, &
			SCOPE::PRG_PROGRAM, "HELP")

	!
	! Exit
	!
	CASE "X"
		GOTO ExitProgram

	END SELECT

	GOTO Menu

 ExitProgram:

	!
	! Erase displays
	!
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SCROLL::WINDOW)

	IF SPAWN_FLAG%
	THEN
		GOTO 32767
	ELSE
		CALL SUBR_3EXITPROGRAM(SCOPE, "", "")
	END IF

	%PAGE

 LoadAll:
6000	!

6020	LINE_NUM$(0%) = "0"

	IF DIGSR_FLAG% = 0%
	THEN
		ST% = LIBR_DIGSR(LFILE$, KEY_NAME$, LINE_NUM$())
	ELSE
		ST% = LIBR_NODIGSR(LFILE$, KEY_NAME$, LINE_NUM$())
	END IF

	LINE_NUM$(0%) = "0" IF LINE_NUM$(0%) = "-1"
	CURR_LINE% = VAL%(LINE_NUM$(0%))

6600	SMG_SCROLL::BOT_ARRAY, SMG_SCROLL::END_ELEMENT = CURR_LINE%
	SMG_SCROLL::TOP_LINE = SMG_SCROLL::BEG_ELEMENT

	RETURN

	%PAGE

 ! HelpError:
	!***************************************************************
	! Help Message for an error
	!***************************************************************
 !	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
 !		"E", ERN$, FILENAME$, NUM1$(ERR))
 !	GOTO ExitProgram

32767	END
