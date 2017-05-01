1	%TITLE "Help Function"
	%SBTTL "HELP_34MESSAGE"
	%IDENT "V3.6a Calico"

	SUB HELP_34MESSAGE(SCOPE_STRUCT SCOPE, &
		STRING MESSAGES, &
		STRING HELP_SEVERITY, &
		STRING HELP_PROGNAME, &
		STRING HELP_FILENAME, &
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
	!	This function displays messages to the screen.
	!	.p
	!	If severity of the message is ^*H\* (help message)
	!	then full message will be displayed. Otherwise just
	!	a brief highlighted two lines ares appears. By pressing
	!	^*Do\* key, full message shows up.
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
	!	CALL HELP_34MESSAGE("Unable to allocate a channel", &
	!		"ERR", "ASSG_BATCH", "ERROR-GETCHAN")
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:HELP_34MESSAGE/NOOPT
	!	$ LIB FUNC_LIB:CMC_3VECTOR/REP HELP_34MESSAGE
	!	$ DELETE HELP_34MESSAGE.OBJ;*
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
	!	02/13/90 - Frank F. Starman
	!		Add new argument HELP_FILENAME
	!
	!	02/20/90 - Frank F. Starman
	!		Don't search for the old keys anymore.(PROG,REPO..)
	!
	!	04/25/90 - Frank F. Starman
	!		For non help messages (severity<>"H") do not
	!		display a full message screen.
	!
	!	05/25/90 - Frank F. Starman
	!		Only display messages if severity="I"
	!
	!	06/18/90 - Frank F. Starman
	!		Use <Do> key to display full message and erase it.
	!
	!	08/15/90 - Kevin Handy
	!		Attempt to speed up after hearing too many complaints
	!		about the sloooooooooooooow error messages.
	!
	!	09/07/90 - Frank F. Starman
	!		Use <Help> key instead <Do> key to display full message.
	!		Display E$$CMCERR message if there is no error
	!		message.
	!		Fixed problem with F17 key after speeding up some
	!		messages.
	!
	!	12/07/90 - Frank F. Starman
	!		Don't display HELP_FILENAME if it is a system name.
	!
	!	08/20/91 - Kevin Handy
	!		Adjusting the number of lines allowed in yet another
	!		one of Franks help functions.
	!
	!	03/14/92 - Kevin Handy
	!		Cleaned up vars (checkvar)
	!
	!	08/16/93 - Frank F. Starman
	!		Ring bell if warning.
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
	!	03/16/99 - Kevin Handy
	!		Lose useless 'ON ERROR GOTO 0'
	!
	!	04/08/99 - Kevin Handy
	!		Use BASIC$STARLET for LIB$ and SMG$ routines
	!
	!	04/12/99 - Kevin Handy
	!		Fix parameters to SET_BROADCAST_TRAPPING
	!
	!	06/29/99 - Kevin Handy
	!		Compile '/NOOPT' because the optimizer really
	!		messes this program up on Alpha.
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
	%INCLUDE "FUNC_INCLUDE:LIBRARY.COM"
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

	OLD_HELP_SEVERITY$ = SCOPE::PRG_IDENT + ""  !Save the COMMON Ident
	OLD_PROGRAM$ = SCOPE::PRG_PROGRAM + ""	!Save the COMMON program name
	OLD_HELP_ITEM$ = SCOPE::PRG_ITEM + ""	!Save the COMMON item

	! Make copies of the parameters, because if help is called using
	! map information, and help changes them, the calling variables
	! will (magically) change at the same time.
	!
	S_HELP_SEVERITY$ = EDIT$(HELP_SEVERITY, 2% + 4% + 32%)
	O_HELP_PROGRAM$, S_HELP_PROGRAM$ = EDIT$(HELP_PROGNAME, 2% + 4% + 32%)
	S_HELP_FILENAME$ = EDIT$(HELP_FILENAME, 2% + 4% + 32%)
	S_HELP_ITEM$ = EDIT$(HELP_ITEM, 2% + 4% + 32%)

	O_HELP_PROGRAM$ = S_HELP_ITEM$ IF O_HELP_PROGRAM$ = ""

	!
	! Don't display S_HELP_FILENAME$ if assumed to be a system name
	!
	IF LEN(S_HELP_FILENAME$) < 3%
	THEN
		TEXT$ = TRM$(MESSAGES)
	ELSE
		TEXT$ = TRM$(S_HELP_FILENAME$) + " " + TRM$(MESSAGES)
	END IF

	SEVERITY$ = LEFT(S_HELP_SEVERITY$, 1%)

	SELECT SEVERITY$
	CASE "E", "F"
		HIT% = 1%
	CASE "I", "S", "W"
		HIT% = 0%
	CASE ELSE
		SEVERITY$ = "H"
		HIT% = 1%
	END SELECT

	IF SEVERITY$ = "I"
	THEN
		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, &
			SMG$M_CURSOR_OFF)
		!
		! Just display (don't stop)
		CALL ENTR_3MESSAGE(SCOPE,TEXT$, 1% + 16%)
		GOTO RestoreScope
	END IF

	SCOPE::PRG_PROGRAM = "HELP_34MESSAGE"
	SCOPE::PRG_ITEM = "HELP"
	SCOPE::PRG_IDENT = "H"

	OLD_OPTION  = SCOPE::SMG_OPTION		! Save the OPTION  virtual display
	OLD_MESSAGE = SCOPE::SMG_MESSAGE	! Save the MESSAGE virtual display

	!
	! Create virtual display
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(20%, 132%, SVD)

	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(1%, 132%, SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(2%, 132%, &
		SCOPE::SMG_MESSAGE, SMG$M_BORDER)

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

	UNDER% = INSTR(1%, S_HELP_PROGRAM$ + "_", "_")

	!
	! Which library is it in?
	!
	IF UNDER% = 1%
	THEN
		LIB_FILE$ = S_HELP_ITEM$
		UNDER% = INSTR(1%, S_HELP_ITEM$ + "_", "_")
		O_HELP_PROGRAM$ = LEFT(S_HELP_ITEM$, UNDER% - 1%)
	ELSE
		LIB_FILE$ = S_HELP_PROGRAM$
	END IF

	LIB_FILE$, ORIG_LIB$ = "REF:HELP_" + LEFT(LIB_FILE$, UNDER% - 1%)

	!
	! What is the key name
	!
	KEY1$ = LEFT(SEVERITY$ + "$" + &
		S_HELP_PROGRAM$ + "$" + &
		S_HELP_FILENAME$ + S_HELP_ITEM$, 39%)
	KEY2$ = LEFT(SEVERITY$ + "$" + &
		S_HELP_FILENAME$ + "$" + &
		S_HELP_ITEM$, 39%)
	KEY3$ = LEFT(SEVERITY$ + "$$" + &
		S_HELP_ITEM$, 39%)

	!
	! Load array and print it to the virtual display
	!
 !	GOSUB LoadAll

	!
	! Paste virtual displays to pasteboard
	!
	IF SEVERITY$ = "H" OR SEVERITY$ = "E" OR SEVERITY$ = "F"
	THEN
		GOSUB LoadAll
		SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SVD, SCOPE::SMG_PBID, &
			1%, 1%)
		SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SCOPE::SMG_OPTION, &
			SCOPE::SMG_PBID, 21%, 1%)
	END IF

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SCOPE::SMG_MESSAGE, &
		SCOPE::SMG_PBID, 23%, 1%)

 Menu:
	!
	! Display desired message
	!
	IF HIT% = 0%
	THEN
		IF SEVERITY$ = "W"
		THEN
			SMG_STATUS% = SMG$RING_BELL(SCOPE::SMG_KBID)
			SMG_STATUS% = SMG$RING_BELL(SCOPE::SMG_KBID)
		END IF

		CALL ENTR_3MESSAGE(SCOPE, TEXT$, 4% + 8%)
	ELSE
		CALL ENTR_3MESSAGE(SCOPE, &
			"%" + O_HELP_PROGRAM$ + "-" + SEVERITY$ + &
			"-" + S_HELP_ITEM$ + ", " + TEXT$, 4% + 8%)
	END IF

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
	! F17 - Magic key to edit APP file
	!
	CASE SMG$K_TRM_F17

		CALL ENTR_3MESSAGE(SCOPE, "", 1% + 16%)

		!
		! Figure out the help key
		!
		GOSUB LoadAll IF HIT% = 0%

		ST% = LBR$INI_CONTROL(LR.INDEX%, LBR$C_READ)
		ST% = LBR$OPEN(LR.INDEX%, LIB1_NAME$, , ".TLB")
		IF (ST% AND 1%) = 0%
		THEN
			LIB1_NAME$ = DEFAULT_LIB$
		END IF
		ST% = LBR$CLOSE(LR.INDEX%)

		CALL LIB$SET_SYMBOL("CMC$HELP_LIBRARY", LIB1_NAME$)
		CALL LIB$SET_SYMBOL("CMC$HELP_KEY", KEY1_NAME$)

		SMG_STATUS% = SMG$DISABLE_BROADCAST_TRAPPING(SCOPE::SMG_PBID)

		SMG_STATUS% = LIB$SPAWN("RUN CMC:UT_SPEC_HELP")

		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, &
			SMG$M_CURSOR_OFF)

		SMG_STATUS% = SMG$SET_BROADCAST_TRAPPING(SCOPE::SMG_PBID, &
			LOC(READ_3BROADCAST), LOC(SCOPE))
		SMG_STATUS% = SMG$REPAINT_SCREEN(SCOPE::SMG_PBID)

		CALL LIB$SET_SYMBOL("CMC$HELP_LIBRARY", "")
		CALL LIB$SET_SYMBOL("CMC$HELP_KEY", "")

		GOSUB LoadAll

	CASE SMG$K_TRM_F15

		IF HIT% = 0%
		THEN
			!
			! Display full message
			!
			GOSUB LoadAll
			SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SVD, &
				SCOPE::SMG_PBID, 1%, 1%)
			SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY( &
				SCOPE::SMG_OPTION, SCOPE::SMG_PBID, &
				21%, 1%)
			HIT% = 1%
		END IF

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
	SMG_STATUS% = SMG$POP_VIRTUAL_DISPLAY(SVD, SCOPE::SMG_PBID)
	SMG_STATUS% = SMG$POP_VIRTUAL_DISPLAY(SCOPE::SMG_OPTION, &
		SCOPE::SMG_PBID)
	SMG_STATUS% = SMG$POP_VIRTUAL_DISPLAY(SCOPE::SMG_MESSAGE, &
		SCOPE::SMG_PBID)

	SCOPE::SMG_OPTION	= OLD_OPTION	! Restore OPTION  display
	SCOPE::SMG_MESSAGE	= OLD_MESSAGE	! Restore MESSAGE display

 RestoreScope:
	SCOPE::PRG_IDENT	= OLD_HELP_SEVERITY$
	SCOPE::PRG_PROGRAM	= OLD_PROGRAM$
	SCOPE::PRG_ITEM		= OLD_HELP_ITEM$

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
				IF KEY3$ <> KEY2$

			IF (ST% AND 1%) <> 0%
			THEN
				KEY1_NAME$ = KEY3$
			ELSE
				IF LIB_FILE$ <> DEFAULT_LIB$
				THEN
					LIB_FILE$ = DEFAULT_LIB$
					GOTO LoadAll
				ELSE
					IF SEVERITY$ = "E"
					THEN
						ST% = LIBR_DIGSR(DEFAULT_LIB$, &
							"E$$CMCERR", LINE_NUM$())
					ELSE
						ST% = LIBR_DIGSR(DEFAULT_LIB$, &
							"H$$NODETAIL", LINE_NUM$())
					END IF
					LIB1_NAME$ = ORIG_LIB$
				END IF
			END IF

		END IF

	END IF

	CURR_LINE% = VAL%(LINE_NUM$(0%))

	SMG_SCROLL::BOT_ARRAY, SMG_SCROLL::END_ELEMENT = &
		CURR_LINE%
	SMG_SCROLL::TOP_LINE = SMG_SCROLL::BEG_ELEMENT

	IF TEXT$ = " "
	THEN
		TEXT$ = EDIT$(LINE_NUM$(1%), 1% + 4% + 8% + 16% + 128%)
		TEXT$ = LEFT(TEXT$, LEN(TEXT$) - 3%)
	END IF

	V% = DSPL_SCROLL(SMG_SCROLL, LINE_NUM$(), 0%, "PAINT")

	RETURN

	END SUB
