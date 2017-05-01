1	%TITLE "Help Function"
	%SBTTL "HELP_3MESSAGE"
	%IDENT "V3.6a Calico"

	SUB HELP_3MESSAGE(SCOPE_STRUCT SCOPE, &
		STRING MESSAGES, &
		STRING HELP_SEVERITY, &
		STRING HELP_PROGNAME, &
		STRING HELP_ITEM)

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
	!	This function displays help messages to the screen.
	!
	! Index:
	!
	! Parameters:
	!
	!	MESSAGES
	!		Passed variable that tells what type of error
	!		the user is receiving.
	!
	!	HELP_SEVERITY
	!		Passed variable showing that there is an error
	!		in the program.
	!
	!	HELP_PROGNAME
	!		The passed program name where the error is comming from.
	!
	!	HELP_ITEM
	!		Passed item tells the official name of the error.
	!
	!
	!	This function displays help messages to the screen,
	!	allowing the user to look deeper into the help files
	!	as necessary.
	!
	! Example:
	!
	!	CALL HELP_3MESSAGE("Unable to allocate a channel", &
	!		"ERR", "ASSG_BATCH", "ERROR-GETCHAN")
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:HELP_3MESSAGE/NOLINE/NOOPT
	!	$ LIB FUNC_LIB:CMC_3VECTOR/REP HELP_3MESSAGE
	!	$ DELETE HELP_3MESSAGE.OBJ;*
	!
	! Author:
	!
	!	07/28/86 - Kevin Handy
	!
	! Modification history:
	!
	!	11/05/87 - Robert Peterson
	!		Change the number of lines in the help message
	!		from 600 to 1200
	!
	!	02/10/89 - Kevin Handy
	!		Modified for changes in ENTR_NOLSTRING.
	!
	!	09/22/89 - Kevin Handy
	!		Modified to use ENTR_3ENTER instead of
	!		ENTR_NOLSTRING.
	!
	!	10/24/89 - Kevin Handy
	!		Modified to call out to edit help messages
	!		instead of doing the work internally.
	!
	!	01/01/90 - Frank F. Starman
	!		Do not draw window. Kick out user notes.
	!		Add INFORMATION and ERROR severity.
	!
	!	01/15/90 - Frank F. Starman
	!		Change TK_MAST_HELP to UT_SPEC_HELP
	!
	!	02/05/90 - Frank F. Starman
	!		Add WARNING, SUCCESS, and FATAL error severity.
	!		Search by new key.
	!
	!	03/20/90 - Frank F. Starman
	!		Don's search for the old keys anymore.(PROG,REPO..)
	!
	!	03/12/92 - Kevin Handy
	!		Clean up vars (checkvar)
	!
	!	03/26/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/12/95 - Kevin Handy
	!		Update to V3.6 coding standards.
	!
	!	09/03/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/15/99 - Kevin Handy
	!		Lose commented out code
	!
	!	04/08/99 - Kevin Handy
	!		Use BASIC$STARLET for LIB$ and SMG$ routines
	!
	!	06/30/99 - Kevin Handy
	!		Compile with /NOOPT to lose problems
	!--

	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"
	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	%INCLUDE "SOURCE:[SMG.OPEN]SMG_SCROLL.HB"
	DECLARE	SMG_SCROLL_CDD	SMG_SCROLL

	! External functions
	!
	EXTERNAL INTEGER FUNCTION DSPL_SCROLL
	EXTERNAL LONG	FUNCTION LIBR_DIGSR
	EXTERNAL LONG		READ_3BROADCAST

	!
	! Declarations
	!
	DECLARE LONG SVD, OLD_OPTION, OLD_MESSAGE
	DECLARE INTEGER CONSTANT NUM_LINES = 3000	! Size of the array

	!
	! Dimension statements
	!
	DIM LINE_NUM$(NUM_LINES)

	DEFAULT_LIB$ = "REF:HELP_DEFAULT"

	%PAGE

	!
	! Create virtual display
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(20%, 132%, SVD)

	OLD_OPTION   = SCOPE::SMG_OPTION	! Save the OPTION  virtual display
	OLD_MESSAGE  = SCOPE::SMG_MESSAGE	! Save the MESSAGE virtual display
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(2%, 132%, SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SCOPE::SMG_OPTION, &
		SCOPE::SMG_PBID, 21%, 1%)
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(2%, 132%, SCOPE::SMG_MESSAGE)
	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SCOPE::SMG_MESSAGE, &
		SCOPE::SMG_PBID, 23%, 1%)

	SMG_SCROLL::WINDOW	= SVD
	SMG_SCROLL::SCROLL_TOP	= 3%
	SMG_SCROLL::SCROLL_BOT	= 20%

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

	%PAGE

	! Make copies of the parameters, because if help is called using
	! map information, and help changes them, the calling variables
	! will (magically) change at the same time.
	!
	S_HELP_SEVERITY$ = EDIT$(HELP_SEVERITY, 2% + 4% + 32%)
	O_HELP_PROGRAM$, S_HELP_PROGRAM$ = EDIT$(HELP_PROGNAME, 2% + 4% + 32%)
	S_HELP_ITEM$ = EDIT$(HELP_ITEM, 2% + 4% + 32%)

	!
	! If message is not comming from a source code
	!

	SELECT S_HELP_SEVERITY$
	CASE "ERR", "E", "F", "I", "S", "W"
		SEVERITY$ = LEFT(S_HELP_SEVERITY$, 1%)
	CASE ELSE
		SEVERITY$ = "H"
	END SELECT

	TEXT$ = TRM$(MESSAGES)

	IF TEXT$ = ""
	THEN
		SELECT SEVERITY$
		CASE "E"
			TEXT$ = "error message"
		CASE "F"
			TEXT$ = "fatal, or severe error"
		CASE "H"
			TEXT$ = "help message"
		CASE "I"
			TEXT$ = "information message"
		CASE "S"
			TEXT$ = "successful processing"
		CASE "W"
			TEXT$ = "warning message"
		END SELECT
	END IF

	!
	! Check for help inside of help
	!
	OLD_HELP_SEVERITY$ = SCOPE::PRG_IDENT + ""	! Save the COMMON Ident
	OLD_PROGRAM$ = SCOPE::PRG_PROGRAM + ""	! Save the COMMON program name
	OLD_HELP_ITEM$ = SCOPE::PRG_ITEM + ""		! Save the COMMON item

	GOTO ExitProgram IF TRM$(SCOPE::PRG_PROGRAM) = "HELP_3MESSAGE"

	SCOPE::PRG_PROGRAM	= "HELP_3MESSAGE"
	SCOPE::PRG_ITEM		= "HELP"
	SCOPE::PRG_IDENT	= "H"

	UNDER% = INSTR(1%, S_HELP_PROGRAM$ + "_", "_")

	!
	! Which library is it in?
	!
	IF UNDER% = 1%
	THEN
		LIB_FILE$ = S_HELP_ITEM$
		UNDER% = INSTR(1%, S_HELP_ITEM$ + "_", "_")
		!LIB_FILE$ = "REF:HELP_" + LEFT(S_HELP_ITEM$, UNDER% - 1%)
		O_HELP_PROGRAM$ = LEFT(S_HELP_ITEM$, UNDER% - 1%)
	ELSE
		LIB_FILE$ = S_HELP_PROGRAM$
	END IF

	LIB_FILE$ = "REF:HELP_" + LEFT(LIB_FILE$, UNDER% - 1%)

	!
	! What is the key name
	!
	KEY1$ = LEFT(SEVERITY$ + "$" + &
		S_HELP_PROGRAM$ + "$" + &
		S_HELP_ITEM$, 39%)
	KEY2$ = LEFT(SEVERITY$ + "$$" + &
		S_HELP_ITEM$, 39%)
	KEY3$ = LEFT(SEVERITY$ + "$$" + &
		S_HELP_ITEM$, 39%)

500	!
	! Load array and print it to the virtual display
	!
	GOSUB LoadAll

	V% = DSPL_SCROLL(SMG_SCROLL, LINE_NUM$(), 0%, "PAINT")

	!
	! Paste virtual displays to pasteboard
	!
	IF SEVERITY$ = "H"
	THEN
		SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SVD, &
			SCOPE::SMG_PBID, 1%, 1%)
	END IF

 Menu:
3000	!
	! Enter desired option
	!
	CALL ENTR_3MESSAGE(SCOPE, &
		"%" + O_HELP_PROGRAM$ + "-" + SEVERITY$ + &
		"-" + S_HELP_ITEM$ + ", " + TEXT$, &
		4% + 8%)

	SELECT SCOPE::SCOPE_EXIT
	!
	! ^C
	!
	CASE SMG$K_TRM_CTRLC, SMG$K_TRM_F11
		!
		! Print the array
		!
		SMG_SCROLL::TOP_LINE = SMG_SCROLL::BEG_ELEMENT
		V% = DSPL_SCROLL(SMG_SCROLL, LINE_NUM$(), 0%, "PAINT")

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

	!
	! F16 - Detail
	!
	CASE SMG$K_TRM_F16
		SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SVD, &
			SCOPE::SMG_PBID, 1%, 1%)

	!
	! F17 - Magic key to edit APP file
	!
	CASE SMG$K_TRM_F17
		GOSUB MagicKey
		GOSUB LoadAll
		V% = DSPL_SCROLL(SMG_SCROLL, LINE_NUM$(), 0%, "PAINT")

	!
	! Other keys are Exit keys
	!
	CASE ELSE
		GOTO ExitProgram

	END SELECT

	GOTO Menu

 ExitProgram:
	!
	! Delete all displays
	!
	SMG_STATUS% = SMG$POP_VIRTUAL_DISPLAY(SCOPE::SMG_OPTION, SCOPE::SMG_PBID)

	SCOPE::SMG_OPTION	= OLD_OPTION	! Restore OPTION  virtual display
	SCOPE::SMG_MESSAGE	= OLD_MESSAGE	! Restore MESSAGE virtual display
	SCOPE::PRG_IDENT	= OLD_HELP_SEVERITY$	+ ""
	SCOPE::PRG_PROGRAM	= OLD_PROGRAM$	+ ""
	SCOPE::PRG_ITEM		= OLD_HELP_ITEM$	+ ""

	EXIT SUB

	%PAGE

 LoadAll:
	!*******************************************************************
	! Load in the help text, and process it
	!*******************************************************************
	!
	! Initialize the number of lines currently loaded
	!
	LINE_NUM$(0%) = "0"

	!
	! Try reading specific help file
	!
	ST% = LIBR_DIGSR(LIB_FILE$, KEY1$, LINE_NUM$())
	KEY1_NAME$ = KEY1$
	LIB1_NAME$ = LIB_FILE$

	IF (ST% AND 1%) = 0%
	THEN
		!
		! If text not found in main help file, check out
		! the default help file.
		!
		ST% = LIBR_DIGSR(LIB_FILE$, KEY2$, LINE_NUM$())

		IF (ST% AND 1%) <> 0%
		THEN
			KEY1_NAME$ = KEY2$
		ELSE

			!
			! Check only if KEY3 <> KEY2
			!
			ST% = LIBR_DIGSR(LIB_FILE$, KEY3$, LINE_NUM$()) &
					IF KEY3$<>KEY2$

			IF (ST% AND 1%) <> 0%
			THEN
				KEY1_NAME$ = KEY3$
			ELSE
				IF LIB_FILE$ <> DEFAULT_LIB$
				THEN
					LIB_FILE$ = DEFAULT_LIB$
					GOTO LoadAll
				ELSE
					ST% = LIBR_DIGSR(DEFAULT_LIB$, &
						"H$$NODETAIL", LINE_NUM$())
				END IF
			END IF

		END IF

	END IF

	CURR_LINE% = VAL%(LINE_NUM$(0%))

	SMG_SCROLL::BOT_ARRAY, SMG_SCROLL::END_ELEMENT = &
		CURR_LINE%
	SMG_SCROLL::TOP_LINE = SMG_SCROLL::BEG_ELEMENT

	RETURN

	%PAGE

7000	!*******************************************************************
	! Magic key functions
	!*******************************************************************
 MagicKey:

	CALL LIB$SET_SYMBOL("CMC$HELP_LIBRARY", LIB1_NAME$)
	CALL LIB$SET_SYMBOL("CMC$HELP_KEY", KEY1_NAME$)

	SMG_STATUS% = SMG$DISABLE_BROADCAST_TRAPPING(SCOPE::SMG_PBID)

	SMG_STATUS% = LIB$SPAWN("RUN CMC:UT_SPEC_HELP")

	SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, SMG$M_CURSOR_OFF)

	SMG_STATUS% = SMG$SET_BROADCAST_TRAPPING(SCOPE::SMG_PBID, &
		LOC(READ_3BROADCAST), LOC(SCOPE))
	SMG_STATUS% = SMG$REPAINT_SCREEN(SCOPE::SMG_PBID)

	CALL LIB$SET_SYMBOL("CMC$HELP_LIBRARY", "")
	CALL LIB$SET_SYMBOL("CMC$HELP_KEY", "")

	RETURN

	END SUB
