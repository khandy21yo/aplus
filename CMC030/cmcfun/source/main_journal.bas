1	%TITLE "Line Window Maintenance"
	%SBTTL "MAIN_JOURNAL"
	%IDENT "V3.6a Calico"

	FUNCTION LONG MAIN_JOURNAL(WINDOW%, QUERY$)

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
	!	Maintains a journal file with multiple lines for
	!	one header.
	!
	!	This function is used to reduce the amount of code
	!	that is duplicated between programs.
	!
	!	This maintainence module calls the users screen through
	!	a user written function called MAINT_GROUP, using
	!	parameters defined in <MAIN_WINDOW.COM>.
	!
	! Parameters:
	!
	!	WINDOW%
	!		The passed number used to reference the window.
	!
	!	QUERY$
	!		Used to call up specific options.
	!
	!		The first letter that is passed through the function
	!		specifies what they really want to do.
	!		.table
	!			'A' - Add lines under header.
	!			'C' - Change lines to match header.
	!			'E' - Erase lines under header.
	!			'Q' - Query.
	!		.endtable
	!
	!		The second letter specifies which key they want to
	!		do it on.
	!		.table
	!			'0' - Key number 0
	!			'1' - Key number 1
	!		.endtable
	!
	!		The rest of the letters contain the text of the key
	!		to search for.
	!
	! Structure MAIN_WINDOW fields used:
	!
	!	IDENT
	!		Identifying number for this window assigned by
	!		the program.
	!
	!	DESCR
	!		Title placed on the border of the window.
	!
	!	NHELP
	!		Name to use for help in the PROGRAM field.
	!
	!	CHAN
	!		Channel to access file being edited on.  This
	!		function can only handle one file at a time.
	!
	!	HSIZ
	!		Width of the window to be created (NOT including
	!		the border).  Will change the terminal to 132 column
	!		mode if necessary.
	!
	!	VSIZ
	!		Height of the window (NOT including the border).
	!
	!	HPOS
	!		Horozontal position (width) to place the window.
	!		This does not include the border.
	!
	!	VPOS
	!		Vertical position (heignt) to place the window.
	!		This does not include the border.
	!
	!	NITEMS
	!		Number of fields in the window.
	!
	!	WNUMBER
	!		The window number of the window for this screen
	!		as returned by SMG$CREATE_VIRTUAL_DISPLAY.
	!
	!	LINREC
	!		The number of lines on the screen that each
	!		journal record requires to display.  The default
	!		is one (1).
	!
	!	TOPLIN
	!		The top line of the window allocated for records.
	!		(The start of the scrolling region).
	!
	!	BOTLIN
	!		The bottom line of the window allocated for records.
	!		(The end of the scrolling region).
	!
	!	CURREC
	!		The current record (actually an array pointer,
	!		not an RFA pointer).
	!
	!	CURLIN
	!		The first line on the window for the current
	!		record (pointed to by CURREC).
	!
	!	TOPREC
	!		The record displayed at the top of the screen.
	!
	!	MAXREC
	!		The maximum number of lines allowed per header.
	!		(Currently not working as a control).
	!
	!	TOTREC
	!		The total number of records entered under this
	!		header.
	!
	!	HFLAG(1..NITEMS)
	!		Hard/soft flags for the fields.
	!
	!	FLAGS
	!		Flag word controlling how certain things get done.
	!		.table
	!		  1 - Enable window option to appear on screen.
	!		  2 - Read only
	!		  4 - Edit existing fields only.  Add, key change
	!		not allowed.
	!		.endtable
	!
	!	Returns a status code.
	!	.table
	!		1 - Succusful
	!		2 - Error (Didn't press select?)
	!		4 - Severe error (Didn't find key?)
	!	.endtable
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:MAIN_JOURNAL
	!	$ LIB FUNC_LIB:CMCFUN/REP MAIN_JOURNAL
	!	$ DELETE MAIN_JOURNAL.OBJ;*
	!
	! Author:
	!
	!	08/05/87 - Kevin Handy
	!
	! Modification history:
	!
	!	08/14/87 - Kevin Handy
	!		Fix problem with initializing when it has to
	!		scroll the lines.
	!
	!	08/31/86 - Kevin Handy
	!		Fix problem with basic fouling up the error
	!		trapping inside of the help of an error.
	!
	!	12/11/87 - Kevin Handy
	!		Fix problem where aborting a change does not
	!		repaint the record with it's original value.
	!
	!	12/20/87 - Kevin Handy
	!		Added "Please wait" to initialization.
	!
	!	03/11/87 - Kevin Handy
	!		Modified initilize to repaint on exit key.
	!
	!	04/26/88 - Kevin Handy
	!		Fixed bug which could cause massive crash due
	!		to an error within an error.
	!
	!	05/05/88 - Kevin Handy
	!		Fixed bug in QueryErase where it did not erase
	!		down farther levels if they existed.
	!
	!	07/25/88 - Kevin Handy
	!		Modified QueryChange: so that it called
	!		OPT_AFTEROPT.
	!
	!	08/03/88 - Frank Starman
	!		Modified initilize to allow Uparrow and Downarrow key
	!
	!	07/05/89 - Kevin Handy
	!		Fixed program so errors returned from OPT_INIT
	!		are checked for.
	!
	!	08/14/92 - Kevin Handy
	!		Attempt to fix where someone changed all '#'s
	!		in program to tabs (who knows why)
	!
	!	09/30/92 - Kevin Handy
	!		Modified to allow Prev/Next keys to work in the
	!		change option.  Stays in same field, but goes to
	!		next record.
	!
	!	11/24/92 - Kevin Handy
	!		Fixed bud founed in Alpha-AXP conversion where
	!		ENTR_3NUMBER was called with 0% instead of 0.0
	!
	!	05/26/93 - Kevin Handy
	!		Removed tab character in "confirm erase" message
	!		so it could display correctly.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/03/96 - Kevin Handy
	!		Reformat source code.
	!
	!	12/19/97 - Kevin Handy
	!		Create constants in WINDOW.INC for OPT_ARRAY
	!		sub-options instead of hard coded magoc numbers.
	!
	!	12/19/97 - Kevin Handy
	!		Added a REFRESH result to OPT_MOREMENU.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/25/98 - Kevin Handy
	!		Add another digit to the find option.
	!
	!	06/08/99 - Kevin Handy
	!		Lose PaintLine subroutine, which is never called.
	!
	!	06/29/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!		Lose a lot of useless error traps.
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"
	DECLARE CDD_WINDOW_CDD SMG_WINDOW

	!
	! External declarations
	!
	EXTERNAL INTEGER FUNCTION MAINT_GROUP

	EXTERNAL INTEGER FUNCTION MAIN_DEFAULT
	EXTERNAL INTEGER FUNCTION MAIN_JOURADD
	EXTERNAL INTEGER FUNCTION MAIN_JOURCHANGE
	EXTERNAL INTEGER FUNCTION MAIN_JOURBLANK

	!
	! Declare functions
	!
	DECLARE RFA MAINT_FILE.RFA, NULL.RFA

	!
	! Dimension statements
	!
	DIM INIT%(64%)

	%PAGE

	!
	! Initilization section	Prepare to do anything
	!
	ON ERROR GOTO 19000

	KEEP_IDENT$ = SCOPE::PRG_IDENT + ""
	KEEP_PRG$ = SCOPE::PRG_PROGRAM + ""
	KEEP_ITEM$ = SCOPE::PRG_ITEM + ""

	!
	! Display please-wait message
	!
	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_MESSAGE, "Please wait.", &
		1%, 1%, 1%, SMG$M_BLINK + SMG$M_BOLD)

	!
	! Call up initilization
	!
	SMG_WINDOW::IDENT = WINDOW%
	SMG_WINDOW::LINREC = 1%
	SMG_WINDOW::TOPLIN = 1%
	SMG_WINDOW::CURREC = 1%
	SMG_WINDOW::TOPREC = 1%
	SMG_WINDOW::BOTLIN = 0%

	V% = MAINT_GROUP(SMG_WINDOW, OPT_INIT, 0%, 0%, "")

	SCOPE::PRG_PROGRAM = SMG_WINDOW::NHELP

	IF V% <> 0%
	THEN
		V1% = SMG$END_PASTEBOARD_UPDATE(SCOPE::SMG_PBID)

		CALL HELP_3MESSAGE(SCOPE, FILENAME$ + " " + NUM1$(ERR) + " " + ERT$(V%), &
			"ERR", ERN$, "ERROR" + NUM1$(V%))
		GOTO QueryExit
	END IF

	SMG_WINDOW::BOTLIN = SMG_WINDOW::VSIZE IF SMG_WINDOW::BOTLIN = 0%

	SMG_WINDOW::LPAGE(0%) = SMG_WINDOW::NITEMS

	!
	! Calculate the number of records that can fit on the screen
	!
	SCRREC% = (SMG_WINDOW::BOTLIN - SMG_WINDOW::TOPLIN + 1%) / &
		SMG_WINDOW::LINREC

	!
	! Initialize array of pointers (Maintained by user program)
	!
	V% = MAINT_GROUP(SMG_WINDOW, OPT_ARRAY, OPT_ARRAY_LOAD, 0%, "")

	!
	! Handle QUERY$ specially
	!
	IF QUERY$ <> ""
	THEN
		GOSUB QueryTest

		!
		! Remove please wait message
		!
		SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

		GOTO ExitProgram
	END IF

	!
	! Have to use a subroutine to create the window because call-in
	! functions may also need to do it.
	!
	GOSUB MakeWindow

	!
	! Initilize menu last option pointer
	!
	OPT% = 0%

	!
	! Calculate valid options
	!
	IF (SMG_WINDOW::FLAGS AND 2%)
	THEN
		!
		! Read/only
		!
		OPLIST$ = "Find Next Restore Help eXit"
	ELSE
		!
		! Edit existing fields only
		!
		IF (SMG_WINDOW::FLAGS AND 4%)
		THEN
			OPLIST$ = "Change Blank Initialize " + &
				"Find Next Restore Help eXit"
		ELSE
			!
			! Full featured
			!
			OPLIST$ = "Add Erase Change Blank Initialize " + &
				"Default Find Next Restore Help eXit"
		END IF
	END IF

	OPLIST$ = OPLIST$ + " Window" IF (SMG_WINDOW::FLAGS AND 1%)

	!
	! Let the user program change the options if necessary
	!
	V% = MAINT_GROUP(SMG_WINDOW, OPT_OPTLIST, 0%, 0%, OPLIST$)

	!
	! Remove please wait message
	!
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

	GOTO Menu

	%PAGE

1000	!*******************************************************************
	! Program restart point
	!*******************************************************************

 MenuCurrent:
1020	!
	! Display current record
	!
	GOSUB PaintScreen

 Menu:
1030	!
	! Enter desired option
	!
	TIMEOUT% = 0%

	SCOPE::PRG_ITEM = ""

	!
	! Input the option
	!
	OPT$ = ENTR_3OPTION(SCOPE, "COMMAND", OPLIST$, OPT%, 0%)

	SELECT SCOPE::SCOPE_EXIT
	!
	! Exit keys
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ

		SCOPE::PRG_ITEM = "EXIT"
		GOTO ExitProgram

	!
	! Select key
	!
	CASE SMG$K_TRM_SELECT

		GOTO ExitProgram

	!
	! Uparrow
	!
	CASE SMG$K_TRM_UP
		!
		! Move arrow up one line
		!
		SMG_WINDOW::CURREC = SMG_WINDOW::CURREC - 1% &
			IF SMG_WINDOW::CURREC > 1%

		GOTO MenuCurrent

	!
	! Downarrow
	!
	CASE SMG$K_TRM_DOWN
		!
		! Move arrow down one line
		!
		SMG_WINDOW::CURREC = SMG_WINDOW::CURREC + 1% &
			IF SMG_WINDOW::CURREC < SMG_WINDOW::TOTREC

		GOTO MenuCurrent

	!
	! Prev Screen
	!
	CASE SMG$K_TRM_PREV_SCREEN
		!
		! Move arrow up one line
		!
		SMG_WINDOW::CURREC = SMG_WINDOW::CURREC - SCRREC% + 1%
		SMG_WINDOW::CURREC = 1% IF SMG_WINDOW::CURREC < 1%

		GOTO MenuCurrent

	!
	! Next Screen
	!
	CASE SMG$K_TRM_NEXT_SCREEN
		!
		! Move arrow up one line
		!
		SMG_WINDOW::CURREC = SMG_WINDOW::CURREC + SCRREC% - 1%
		SMG_WINDOW::CURREC = SMG_WINDOW::TOTREC &
			IF SMG_WINDOW::CURREC > SMG_WINDOW::TOTREC

		GOTO MenuCurrent

	!
	! Top of screen
	!
	CASE SMG$K_TRM_F18
		!
		! Move arrow up one line
		!
		SMG_WINDOW::CURREC = 1%

		GOTO MenuCurrent

	!
	! Bottom of screen
	!
	CASE SMG$K_TRM_F19
		!
		! Move arrow up one line
		!
		SMG_WINDOW::CURREC = SMG_WINDOW::TOTREC
		SMG_WINDOW::CURREC = 1% IF SMG_WINDOW::CURREC < 1%

		GOTO MenuCurrent

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
		GOTO 1030
	END SELECT

	!
	! Decide what to do with the option
	!
	! This selection uses PRG_ITEM which contains the whole name
	! of the option, in case the user decides to disable one of the
	! standard ones and add one of his own with the same letter.
	!
	! NOTE: Case of letters is important here
	!
	SELECT TRM$(SCOPE::PRG_ITEM)
	!
	! Window
	!
	! This option allows the user to pull up another screen
	! over this one, or allow another menu.
	!
	CASE "Window"
		V% = MAINT_GROUP(SMG_WINDOW, OPT_WINDOW, 0%, 0%, "")

	!
	! Add
	!
	! This option allows records to be added to the file.
	!
	CASE "Add"
		GOSUB Addr

	!
	! Find
	!
	! This option allows an existing record to be found
	!
	CASE "Find"
		GOSUB Findr

	!
	! Change
	!
	! This option allows an existing record to be changed.
	!
	CASE "Change"
		GOSUB Changer

	!
	! Next
	!
	! This option will pull up the next record in the file
	!
	CASE "Next"
1120		!
		! Move arrow down one line
		!
		SMG_WINDOW::CURREC = SMG_WINDOW::CURREC + 1% &
			IF SMG_WINDOW::CURREC < SMG_WINDOW::TOTREC

	!
	! Erase
	!
	! This option will erase the current record
	!
	CASE "Erase"
		GOSUB Eraser

	!
	! Blank
	!
	! This option is a varient of the change that just blanks
	! (zeroes) fields, instead of allowing them to be changed
	! to anything.
	!
	CASE "Blank"
		GOSUB Changer

	!
	! Restore
	!
	! This option will move to the first record in the file
	!
	CASE "Restore"
1130		!
		! Move arrow to first line
		!
		SMG_WINDOW::CURREC = 1% &

	!
	! Initialize
	!
	! This option allows all records from the current record
	! to the end of the file to be changed.  Something like
	! a loop of (change, next).
	!
	CASE "Initialize"
		GOSUB Initr

	!
	! Default
	!
	! This option allows default values to be set for the
	! add option, so common values can be set up to come
	! up automatically.
	!
	CASE "Default"
		GOSUB Default

	!
	! Help
	!
	! This option calls out a help message describing the
	! program.
	!
	CASE "Help"
		CALL HELP_34MESSAGE(SCOPE, "", "H", SCOPE::PRG_PROGRAM, "", "HELP")
		GOTO Menu

	!
	! eXit
	!
	! This option exits out of the program.
	!
	CASE "eXit"
		GOTO ExitProgram

	!
	! All other options must be ones defined by the user.
	! These are passed on to the users MAINT_GROUP routine
	! for handling these options.
	!
	CASE ELSE
		!
		! Pass the full name of the option through
		!
		TEMP$ = TRM$(SCOPE::PRG_ITEM)

		!
		! Load in the record
		!
		V% = MAINT_GROUP(SMG_WINDOW, OPT_ARRAY, &
			OPT_ARRAY_GET, SMG_WINDOW::CURREC, "")

		!
		! Call the users routine
		!
		V% = MAINT_GROUP(SMG_WINDOW, &
			OPT_MOREMENU, 0%, 0%, TEMP$)

		!
		! Allow the user routine to force the program
		! to completely exit out.
		!
		SELECT TEMP$
		!
		! Exit the program
		!
		CASE "EXIT"
			GOTO ExitProgram

		!
		! Repaint the screen
		!
		CASE "REFRESH"
			REPAINT_FLAG% = -1%

		END SELECT

	END SELECT

	GOTO MenuCurrent

	%PAGE

 Addr:
2000	!*******************************************************************
	! Add new record(s)
	!
	! This option allows records to be added to the file.
	!
	!*******************************************************************

	!
	! Make sure help stuff is set up
	!
	SCOPE::PRG_ITEM = "Add"

	!
	! Set defaults and clear screen
	!
	OLDLIN% = SMG_WINDOW::CURREC

	SMG_WINDOW::CURREC = SMG_WINDOW::TOTREC + 1%

	GOSUB PaintScreen

	V% = MAINT_GROUP(SMG_WINDOW, OPT_RESETDEFAULT, 0%, 0%, "")

2110	SELECT MAIN_JOURADD(SMG_WINDOW)

	CASE 0%
2120		!
		! Add record to file
		!
		IF MAINT_GROUP(SMG_WINDOW, OPT_TESTOPT, 0%, 0%, "ADD") = 0%
		THEN
			WHEN ERROR IN
				PUT #SMG_WINDOW::CHAN
			USE
				CONTINUE 2200 IF ERR = 134%
				FILENAME$ = ""
				CONTINUE HelpError
			END WHEN

			SMG_WINDOW::TOTREC = SMG_WINDOW::TOTREC + 1%
			V% = MAINT_GROUP(SMG_WINDOW, OPT_ARRAY, OPT_ARRAY_SET, &
				SMG_WINDOW::TOTREC, "")
			V% = MAINT_GROUP(SMG_WINDOW, OPT_AFTEROPT, &
				0%, 0%, "Add")
			GOTO 2000
		END IF

	CASE 2%
		!
		! Bad input, Repaint Screen
		!
		GOSUB PaintScreen

	CASE 4%
		!
		! Some kind of abort
		!
		IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F8) OR &
			(SCOPE::SCOPE_EXIT = SMG$K_TRM_PF3)
		THEN
			!
			! PF1 or Cancel typed
			!
			GOTO 2000
		END IF

	END SELECT

	SMG_WINDOW::CURREC = OLDLIN%

	REPAINT_FLAG% = -1%
	GOSUB PaintScreen
	REPAINT_FLAG% = 0%

	RETURN

2200	!
	! Handle duplicate key detected
	!
	CALL ENTR_3MESSAGE(SCOPE, "Duplicate key detected! Change key or <Cancel>!", 0%)

	GOTO 2110

	%PAGE

 Eraser:
3000	!*******************************************************************
	! Erase a record
	!
	! This option will erase the current record
	!
	!*******************************************************************

	!
	! Is there a record to be deleted?
	!
	IF (SMG_WINDOW::CURREC > SMG_WINDOW::TOTREC)
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "There is no current record!", 0%)
		RETURN
	END IF

	!
	! Make sure they want to delete that record
	!
	INP$ = ENTR_3YESNO(SCOPE,  SMG_WINDOW::WNUMBER, "", &
		"Confirm deletion of line then press <DO>", "N", 0%, &
		"!", "")

	SELECT SCOPE::SCOPE_EXIT
	!
	! Exit keys
	!
	CASE 3%, SMG$K_TRM_F8, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ

		CALL ENTR_3MESSAGE(SCOPE, "Erase aborted", 1%)
		RETURN

	!
	! Good keys
	!
	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
		! Good key

	!
	! Bad Keys
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 3000
	END SELECT

	RETURN IF INP$ <> "Y"
	RETURN IF &
		MAINT_GROUP(SMG_WINDOW, OPT_TESTOPT, 0%, 0%, "ERASE") <> 0%

3010	!
	! Load in the record
	!
	V% = MAINT_GROUP(SMG_WINDOW, OPT_ARRAY, OPT_ARRAY_GETLOCK, &
		SMG_WINDOW::CURREC, "")

	!
	! Delete the record
	!
	WHEN ERROR IN
		DELETE #SMG_WINDOW::CHAN
	USE
		CALL ENTR_3MESSAGE(SCOPE, &
			"Record is locked! Unable to erase!", 0%)
		CONTINUE 3025
	END WHEN

	V% = MAINT_GROUP(SMG_WINDOW, OPT_AFTEROPT, 0%, 0%, "Erase")

	!
	! Remove record from array
	!
	V% = MAINT_GROUP(SMG_WINDOW, OPT_ARRAY, OPT_ARRAY_REMOVE, &
		SMG_WINDOW::CURREC, "")
	SMG_WINDOW::TOTREC = SMG_WINDOW::TOTREC - 1%

	!
	! Decide on the current record
	!
	SMG_WINDOW::CURREC = SMG_WINDOW::TOTREC &
		IF (SMG_WINDOW::CURREC > SMG_WINDOW::TOTREC) AND &
		(SMG_WINDOW::TOTREC > 0%)

	!
	! Repaint the screen
	!
	REPAINT_FLAG% = -1%
	GOSUB PaintScreen
	REPAINT_FLAG% = 0%

3025	!
	! Normal exit from erase
	!
	RETURN

	%PAGE

3110	!
	! Return back from error trap
	!
	RETURN



	%PAGE

 Changer:
4000	!*******************************************************************
	! Change/Blank record
	!
	! This option allows an existing record to be changed or blanked.
	!
	!*******************************************************************

	FIELDNO% = 0%

	IF SMG_WINDOW::CURREC > SMG_WINDOW::TOTREC
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "No current record!", 1%)
		GOTO 4015
	END IF

4005	V% = MAINT_GROUP(SMG_WINDOW, OPT_ARRAY, OPT_ARRAY_GETLOCK, &
		SMG_WINDOW::CURREC, "")

	WHEN ERROR IN
		MAINT_FILE.RFA = GETRFA(SMG_WINDOW::CHAN)
	USE
		CALL ENTR_3MESSAGE(SCOPE, "Unable to lock current record!", 0%)
		CONTINUE 4015
	END WHEN

	V% = MAINT_GROUP(SMG_WINDOW, OPT_SETOLD, 0%, 0%, "")

	SELECT OPT$

	CASE "C"
		STAT% = MAIN_JOURCHANGE(SMG_WINDOW, FIELDNO%)

	CASE "B"
		STAT% = MAIN_JOURBLANK(SMG_WINDOW)
	END SELECT

	SELECT STAT%

4010	!
	! Good change
	!
	CASE 0%
		IF MAINT_GROUP(SMG_WINDOW, OPT_TESTOPT, 0%, 0%, "ADD") = 0%
		THEN
			WHEN ERROR IN
				UPDATE #SMG_WINDOW::CHAN
			USE
				CONTINUE 4020 IF ERR = 130%
				FILENAME$ = ""
				CONTINUE HelpError
			END WHEN

			V% = MAINT_GROUP(SMG_WINDOW, OPT_ARRAY, &
				OPT_ARRAY_SET, SMG_WINDOW::CURREC, &
				TRM$(SCOPE::PRG_ITEM))
			V% = MAINT_GROUP(SMG_WINDOW, OPT_AFTEROPT, &
				0%, 0%, TRM$(SCOPE::PRG_ITEM))
		END IF

	!
	! Bad change, recall original
	!
	CASE ELSE
		UNLOCK #SMG_WINDOW::CHAN
		CALL ENTR_3MESSAGE(SCOPE, &
			TRM$(SCOPE::PRG_ITEM) + " aborted", 1%)
		REPAINT_FLAG% = -1%
		GOSUB PaintScreen
		REPAINT_FLAG% = 0%
		GOTO 4015

	END SELECT

	SELECT SCOPE::SCOPE_EXIT
	!
	! Uparrow
	!
	CASE -SMG$K_TRM_UP, SMG$K_TRM_PREV_SCREEN
		!
		! Move arrow up one line
		!
		SMG_WINDOW::CURREC = SMG_WINDOW::CURREC - 1% &
			IF SMG_WINDOW::CURREC > 1%
		GOSUB PaintScreen

		GOTO 4005

	!
	! Downarrow
	!
	CASE -SMG$K_TRM_DOWN, SMG$K_TRM_NEXT_SCREEN
		!
		! Move arrow down one line
		!
		SMG_WINDOW::CURREC = SMG_WINDOW::CURREC + 1% &
			IF SMG_WINDOW::CURREC < SMG_WINDOW::TOTREC
		GOSUB PaintScreen

		GOTO 4005

	END SELECT

4015	!
	! Normal exit from Change/Blank
	!
	RETURN

4020	!****
	! Enter here if key not changeable, and try deleting/reinserting
	! the record to bypass the key not changable error.
	!****

	IF (SMG_WINDOW::FLAGS AND 4%)
	THEN
		!
		! Do not allow change in edit/only version
		!
		CALL ENTR_3MESSAGE(SCOPE, "Key is not changable!", 0%)
		RETURN
	END IF

	FIND	#SMG_WINDOW::CHAN, RFA MAINT_FILE.RFA
	DELETE	#SMG_WINDOW::CHAN

4030	!
	! Add the new changed record to the file
	!
	WHEN ERROR IN
		PUT #SMG_WINDOW::CHAN
	USE
		CONTINUE 4040 IF ERR = 134%
		FILENAME$ = ""
		CONTINUE HelpError
	END WHEN

	V% = MAINT_GROUP(SMG_WINDOW, OPT_ARRAY, &
		OPT_ARRAY_SET, SMG_WINDOW::CURREC, TRM$(SCOPE::PRG_ITEM))
	V% = MAINT_GROUP(SMG_WINDOW, OPT_AFTEROPT, 0%, 0%, TRM$(SCOPE::PRG_ITEM))

	RETURN

4040	!****
	! Add the original record back into the file because a
	! duplicate key was found when attempting to change the
	! key the hard way.
	!****

	V% = MAINT_GROUP(SMG_WINDOW, OPT_RESETOLD, 0%, 0%, "")	! Restore the original buffer

	WHEN ERROR IN
		PUT #SMG_WINDOW::CHAN
	USE
		CONTINUE 4060 IF ERR = 134%
		FILENAME$ = ""
		CONTINUE HelpError
	END WHEN

	V% = MAINT_GROUP(SMG_WINDOW, OPT_ARRAY, &
		OPT_ARRAY_SET, SMG_WINDOW::CURREC, TRM$(SCOPE::PRG_ITEM))

4050	!
	! If item already exists, display it
	!
	UNLOCK	#SMG_WINDOW::CHAN

	CALL ENTR_3MESSAGE(SCOPE, "Record already exists with this key!", 0%)
	RETURN

4060	!****
	! Original record has duplicate key detected.  This occurs when
	! the original key will not go back into the file after it has
	! been deleted.
	!****

	V% = MAINT_GROUP(SMG_WINDOW, OPT_RESETOLD, 0%, 0%, "")	! Restore the original buffer

	GOSUB PaintScreen

	CALL HELP_3MESSAGE(SCOPE, &
		"Record ERASED because of key change  Win:" + &
		NUM1$(WINDOW%), &
		"ERR", ERN$, "CHANGE_ERASE")

	RETURN

	%PAGE

 Findr:
5000	!*******************************************************************
	! Find a record
	!*******************************************************************

	!
	! No find if no records
	!
	IF SMG_WINDOW::TOTREC = 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "No line items", 1%)
		RETURN
	END IF

	!
	! Ask record number to jump to
	!
	TEMP% = ENTR_3NUMBER(SCOPE, SCOPE::SMG_OPTION, "", &
		"Find line (1 to " + &
		NUM1$(SMG_WINDOW::TOTREC) + ")", 0.0, 0%, "####", "")

	RETURN IF TEMP% = 0%

	IF (TEMP% < 1%) OR (TEMP% > SMG_WINDOW::TOTREC)
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "Invalid line to find!", 0%)
		RETURN
	END IF

	!
	! Jump to line
	!
	SMG_WINDOW::CURREC = TEMP%

	!
	! Exit from find
	!
	RETURN

	%PAGE

 Initr:
8000	!*******************************************************************
	! Initilize a field
	!
	! This option allows all records from the current record
	! to the end of the file to be changed.  Something like
	! a loop of (change, next).
	!
	!*******************************************************************

	INIT.FLAG% = 0%
	INIT%(I%) = 0% FOR I% = 1% TO SMG_WINDOW::NITEMS

8010	!
	! Determine what to initialize
	!
	TEMP$ = ""
	TEMP$ = TEMP$ + NUM1$(LOOP%) + " " IF INIT%(LOOP%) &
		FOR LOOP% = 1% TO SMG_WINDOW::NITEMS
	CALL ENTR_3MESSAGE(SCOPE, TEMP$, 1%)

	!
	! Enter fields to initilize
	!
 InitItem:
	FLAG% = 4%
	FLAG% = 12% IF TIMEOUT%

	LOOP% = ENTR_3NUMBER(SCOPE, SCOPE::SMG_OPTION, "", &
		"Field to Initialize", 0.0, FLAG%, "##", "")

	SELECT SCOPE::SCOPE_EXIT
	!
	! Exit keys
	!
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_F8, SMG$K_TRM_CTRLZ, &
		SMG$K_TRM_TIMEOUT

		CALL ENTR_3MESSAGE(SCOPE, "Initialize aborted", 1%)
		RETURN

	END SELECT

	IF LOOP% < 0% OR LOOP% > SMG_WINDOW::NITEMS
	THEN
		GOTO InitItem
	END IF

	SELECT SCOPE::SCOPE_EXIT
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
		GOTO 8010
	END SELECT

8020	IF LOOP% <> 0%
	THEN
		INIT.FLAG%, INIT%(LOOP%) = -1%
		GOTO 8010
	END IF

	RETURN IF INIT.FLAG% = 0%

	OLDLIN% = SMG_WINDOW::CURREC

	!
	! Loop throu all lines, from the current line to the end
	! of the list of lines.
	!
	FOR RLOOP% = SMG_WINDOW::CURREC TO SMG_WINDOW::TOTREC

8050	!
	! Perform the initialization on each record
	!
		!
		! Grab the right record
		!
		SMG_WINDOW::CURREC = RLOOP%
		GOSUB PaintScreen	! Print current record
		SMG_STATUS% = MAINT_GROUP(SMG_WINDOW, OPT_ARRAY, &
			OPT_ARRAY_GETLOCK, RLOOP%, "")

		!
		! Scan through fields initializing all selected
		!
		FOR LOOP% = 1% TO SMG_WINDOW::NITEMS
8060			GOTO 8065 UNLESS INIT%(LOOP%)

			TIMEOUT% = -1%

 EnterOne:
			!
			!	Enter one field
			!
			FLAG% = 0%
			FLAG% = 8% IF TIMEOUT%
			CALL MAINT_GROUP(SMG_WINDOW, OPT_ENTRY, LOOP%, FLAG%, "")

			SELECT SCOPE::SCOPE_EXIT

			!
			! Exit keys
			!
			CASE 3%, SMG$K_TRM_F8, SMG$K_TRM_F10, &
				SMG$K_TRM_CTRLZ, SMG$K_TRM_TIMEOUT

				UNLOCK	#SMG_WINDOW::CHAN
				CALL ENTR_3MESSAGE(SCOPE, "Initialize aborted", 1%)
				SMG_WINDOW::CURREC = OLDLIN%

				!
				! Repaint on exit key
				!
				REPAINT_FLAG% = -1%
				GOSUB PaintScreen
				REPAINT_FLAG% = 0%
				RETURN

			END SELECT

			!
			! Test special cases for each field if nessary
			!
			GOTO EnterOne IF MAINT_GROUP(SMG_WINDOW, OPT_TESTENTRY, LOOP%, 0%, "NOTADD")

			SELECT SCOPE::SCOPE_EXIT
			!
			! Premature exit
			!
			CASE SMG$K_TRM_PF2

				IF MAINT_GROUP(SMG_WINDOW, OPT_TESTOPT, 0%, 0%, "INITIALIZE") = 0%
				THEN
					UPDATE	#SMG_WINDOW::CHAN
					V% = MAINT_GROUP(SMG_WINDOW, OPT_ARRAY, &
						OPT_ARRAY_SET, SMG_WINDOW::CURREC, TRM$(SCOPE::PRG_ITEM))
					V% = MAINT_GROUP(SMG_WINDOW, OPT_AFTEROPT, 0%, 0%, TRM$(SCOPE::PRG_ITEM))
				END IF
				SMG_WINDOW::CURREC = OLDLIN%
				RETURN

			!
			! Uparrow
			!
			CASE SMG$K_TRM_UP
				LOOP% = LOOP% - 1%
				UNTIL INIT%(LOOP%)
					GOTO 8060 IF LOOP% <= 1%
					LOOP% = LOOP% - 1%
				NEXT
				GOTO 8060

			!
			! Downarrow
			!
			CASE SMG$K_TRM_DOWN
				CUR.LOOP% = LOOP%
				LOOP% = LOOP% + 1%
				UNTIL INIT%(LOOP%)
					IF LOOP% >= SMG_WINDOW::NITEMS
					THEN
						LOOP% = CUR.LOOP%
						GOTO 8060
					END IF
					LOOP% = LOOP% + 1%
				NEXT
				GOTO 8060

			!
			! Good keys
			!
			CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
				! Good key

			!
			! Bad Keys
			!
			CASE ELSE
				CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
				GOTO 8060

			END SELECT


8065		NEXT LOOP%

		IF MAINT_GROUP(SMG_WINDOW, OPT_TESTOPT, 0%, 0%, "INITIALIZE") = 0%
		THEN
			V% = MAINT_GROUP(SMG_WINDOW, OPT_ARRAY, &
				OPT_ARRAY_SET, SMG_WINDOW::CURREC, &
				TRM$(SCOPE::PRG_ITEM))

			WHEN ERROR IN
				UPDATE #SMG_WINDOW::CHAN
			USE
				CALL ENTR_3MESSAGE(SCOPE, &
					"Key field not changeable!", 0%)
				CONTINUE 8050 IF ERR = 130%
				FILENAME$ = ""
				CONTINUE HelpError
			END WHEN
		END IF

	NEXT RLOOP%

8070	UNLOCK	#SMG_WINDOW::CHAN

	CALL ENTR_3MESSAGE(SCOPE, "Initialize has finished.", 0%)

	SMG_WINDOW::CURREC = OLDLIN%
	RETURN

	%PAGE

 Default:
9000	!*******************************************************************
	! Set defaults
	!*******************************************************************

	!
	! Put defaults into main area
	!
	OLDLIN% = SMG_WINDOW::CURREC

	SMG_WINDOW::CURREC = SMG_WINDOW::TOTREC + 1%

	GOSUB PaintScreen

	V% = MAINT_GROUP(SMG_WINDOW, OPT_RESETDEFAULT, 0%, 0%, "")

	SELECT MAIN_DEFAULT(SMG_WINDOW)

	!
	! Change is to be made
	!
	CASE 0%
		CALL ENTR_3MESSAGE(SCOPE, "Defaults changed!", 1%)

	!
	! Bad change
	!
	CASE ELSE
		CALL ENTR_3MESSAGE(SCOPE, "Defaults not changed!", 1%)
	END SELECT

	SMG_WINDOW::CURREC = OLDLIN%

	REPAINT_FLAG% = -1%
	GOSUB PaintScreen
	REPAINT_FLAG% = 0%

	RETURN

	%PAGE

 PaintScreen:
	!*******************************************************************
	! Paint all information onto the screen for the current record.
	!*******************************************************************

	TEMP% = SCOPE::SCOPE_EXIT

	!
	! Keep track of what lines have already been displayed
	!
	XX.TOP% = -99999%
	XX.BOT% = 99999%

	!
	! Scroll the screen up/down depending on the current top line
	! and the current selected line
	!
	SELECT SMG_WINDOW::CURREC
	!
	! Handle it when it is way above the top.
	!	If more than one window back
	!
	CASE < SMG_WINDOW::TOPREC - SCRREC%

		!
		! Make current line the top line
		!
		SMG_WINDOW::TOPREC = SMG_WINDOW::CURREC

	!
	! If it needs to scroll down, then do it.
	!	If within one window back
	!
	CASE SMG_WINDOW::TOPREC - SCRREC% &
			TO &
		SMG_WINDOW::TOPREC - 1%

		!
		! Scroll the display down the correct number of lines
		!
		TEMP% = (SMG_WINDOW::TOPREC - SMG_WINDOW::CURREC)
		ST% = SMG$SCROLL_DISPLAY_AREA(SMG_WINDOW::WNUMBER, &
			SMG_WINDOW::TOPLIN, 1%, &
			SMG_WINDOW::BOTLIN - SMG_WINDOW::TOPLIN + 1%, &
				SMG_WINDOW::HSIZE, &
			SMG$M_DOWN, &
			1%) FOR J% = 1% TO TEMP% * SMG_WINDOW::LINREC
		XX.BOT% = SMG_WINDOW::TOPREC UNLESS REPAINT_FLAG%
		SMG_WINDOW::TOPREC = SMG_WINDOW::TOPREC - TEMP%

	!
	! If it is already on the screen, handle that.
	!	On the current window
	!
	CASE SMG_WINDOW::TOPREC &
			TO &
		SMG_WINDOW::TOPREC + SCRREC% - 1%

		IF REPAINT_FLAG% = 0%
		THEN
			XX.TOP% = 99999%
			XX.BOT% = -99999%
		END IF

	!
	! If it needs to scroll up, then do it.
	!	If within one window ahead
	!
	CASE SMG_WINDOW::TOPREC + SCRREC% &
			TO &
		SMG_WINDOW::TOPREC + SCRREC% * 2%

		!
		! Scroll the display up the correct number of lines
		!
		TEMP% = (SMG_WINDOW::CURREC - SMG_WINDOW::TOPREC - &
			SCRREC% + 1%)
		ST% = SMG$SCROLL_DISPLAY_AREA(SMG_WINDOW::WNUMBER, &
			SMG_WINDOW::TOPLIN, 1%, &
			SMG_WINDOW::BOTLIN - SMG_WINDOW::TOPLIN + 1%, &
				SMG_WINDOW::HSIZE, &
			SMG$M_UP, &
			1%) FOR J% = 1% TO TEMP% * SMG_WINDOW::LINREC
		XX.TOP% = SMG_WINDOW::TOPREC + SCRREC% - 1% &
			UNLESS REPAINT_FLAG%
		SMG_WINDOW::TOPREC = SMG_WINDOW::TOPREC + TEMP%

	!
	! Further ahead than one window
	!
	CASE ELSE

		!
		! Put line on bottom of screen
		!
		!SMG_WINDOW::CURREC = SMG_WINDOW::TOPREC + SCRREC% - 1%

		SMG_WINDOW::TOPREC = SMG_WINDOW::CURREC - SCRREC% + 1%
		SMG_WINDOW::TOPREC = 1% IF SMG_WINDOW::TOPREC < 1%

	END SELECT

	SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

	!
	! Erase display area
	!
	IF REPAINT_FLAG%
	THEN
		!
		! Erase whole screen
		!
		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER, &
			SMG_WINDOW::TOPLIN, 1%, &
			SMG_WINDOW::BOTLIN, SMG_WINDOW::HSIZE)
	ELSE
		!
		! Erase arrows
		!
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"  ", I%, 1%) &
			FOR I% = SMG_WINDOW::TOPLIN TO SMG_WINDOW::BOTLIN &
				STEP SMG_WINDOW::LINREC
	END IF

	!
	! Go through all records
	!
	FOR J% = SMG_WINDOW::TOPREC TO SMG_WINDOW::TOPREC + SCRREC% - 1%
		!
		! Don't repaint if it is already on the screen
		!
		IF (J% > XX.TOP%) AND (J% < XX.BOT%)
		THEN
			!
			! Calculate position to place line on screen
			!
			SMG_WINDOW::CURLIN = SMG_WINDOW::TOPLIN + &
				(J% - SMG_WINDOW::TOPREC) * SMG_WINDOW::LINREC
			!
			! Go through all fields, if this record exists
			!
			IF (J% <= SMG_WINDOW::TOTREC)
			THEN
				!
				! Load in record
				!
				V% = MAINT_GROUP(SMG_WINDOW, OPT_ARRAY, &
					OPT_ARRAY_GET, J%, "")
				!
				! Display record
				!
				FOR I% = 1% TO SMG_WINDOW::NITEMS
					V% = MAINT_GROUP(SMG_WINDOW, OPT_ENTRY, I%, 1%, "")
				NEXT I%
				V% = MAINT_GROUP(SMG_WINDOW, OPT_ARRAY, &
					OPT_ARRAY_PRINT, 0%, "")
			END IF
		END IF
	NEXT J%

	!
	! Place cursor at current position
	!
	SMG_WINDOW::CURLIN = SMG_WINDOW::TOPLIN + &
		(SMG_WINDOW::CURREC - SMG_WINDOW::TOPREC) * SMG_WINDOW::LINREC

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
		"->", SMG_WINDOW::CURLIN, 1%)

	!
	! Handle anything extra
	!
	V% = MAINT_GROUP(SMG_WINDOW, OPT_DISPLAY, 0%, 0%, "") &

	SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)
	SCOPE::SCOPE_EXIT = TEMP%

	RETURN

	%PAGE

 ! PaintLine:
	!*******************************************************************
	! Repaint all information onto the screen for the current line,
	! even if it doesn't exist.  Record must be in memory.
	!*******************************************************************

 !	TEMP% = SCOPE::SCOPE_EXIT
 !
 !	SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)
 !
	!
	! Display record
	!
 !	FOR I% = 1% TO SMG_WINDOW::NITEMS
 !		V% = MAINT_GROUP(SMG_WINDOW, OPT_ENTRY, I%, 1%, "")
 !	NEXT I%

	!
	! Handle anything extra
	!
 !	V% = MAINT_GROUP(SMG_WINDOW, OPT_ARRAY, OPT_ARRAY_PRINT, 0%, "")
 !	V% = MAINT_GROUP(SMG_WINDOW, OPT_DISPLAY, 0%, 0%, "") &
 !
 !	SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)
 !	SCOPE::SCOPE_EXIT = TEMP%
 !
 !	RETURN

	%PAGE

 QueryTest:
11000	!*******************************************************************
	! This section is used to query the file for the existance
	! of a record.
	!*******************************************************************

	!
	! The first letter that is passed through the function
	! Specifies what they really want to do.
	!
	! The second letter specifies which key they want to do it on.
	!
	SELECT LEFT(QUERY$, 1%)

	!
	! Add lines to current header
	!
	CASE "A"
		GOSUB MakeWindow
		SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)
		GOSUB Addr

	!
	! Change all lines (must have old record in map) to new
	! header (must have new key in QUERY$ or in another easily
	! accessable position).
	!
	CASE "C"
		GOSUB QueryChange

	!
	! Erase all lines under this header
	!
	CASE "E"
		GOSUB QueryErase

	!
	! Query file for existance of record
	!
	CASE "Q"
		KEY_NUMBER% = VAL%(MID(QUERY$, 2%, 1%))

11010		WHEN ERROR IN
			GET #SMG_WINDOW::CHAN, &
				KEY #KEY_NUMBER% EQ RIGHT(QUERY$, 3%), &
				REGARDLESS
		USE
			MAIN_JOURNAL = ERR * 100%
			CONTINUE 11900
		END WHEN

		MAIN_JOURNAL = 1%

	END SELECT

11900	RETURN

	%PAGE

 QueryErase:
12000	!*******************************************************************
	! Erase all record matching the current header
	!*******************************************************************

	!
	! For all records under this header, which have already been
	! loaded in.  Not fixing the array because we will just immediately
	! jump out of the program.
	!
	FOR LOOP% = SMG_WINDOW::TOTREC TO 1% STEP -1%

		!
		! Grab the record
		!
		SMG_STATUS% = MAINT_GROUP(SMG_WINDOW, OPT_ARRAY, &
			OPT_ARRAY_GETLOCK, LOOP%, "")

		!
		! And delete it
		!
		DELETE #SMG_WINDOW::CHAN

		V% = MAINT_GROUP(SMG_WINDOW, OPT_AFTEROPT, 0%, 0%, "Erase")

	NEXT LOOP%

	RETURN

	%PAGE

 QueryChange:
12500	!*******************************************************************
	! Change all records matching the original header to the new
	! header.
	!*******************************************************************

	!
	! For all records under this header, which have already been
	! loaded in.  Not fixing the array because we will just immediately
	! jump out of the program.
	!
	FOR LOOP% = 1% TO SMG_WINDOW::TOTREC

		!
		! Grab the record
		!
		SMG_STATUS% = MAINT_GROUP(SMG_WINDOW, OPT_ARRAY, &
			OPT_ARRAY_GETLOCK, LOOP%, "")
		V% = MAINT_GROUP(SMG_WINDOW, OPT_SETOLD, 0%, 0%, "")

		!
		! Change the key
		!
		SMG_STATUS% = MAINT_GROUP(SMG_WINDOW, OPT_ARRAY, &
			OPT_ARRAY_SETKEY, LOOP%, QUERY$)

		!
		! And update it
		!
		DELETE #SMG_WINDOW::CHAN
		PUT #SMG_WINDOW::CHAN

		V% = MAINT_GROUP(SMG_WINDOW, OPT_AFTEROPT, 0%, 0%, "Change")

	NEXT LOOP%

	RETURN

	%PAGE

 MakeWindow:
	!*******************************************************************
	! Create display for this file
	!*******************************************************************

	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		SMG_WINDOW::VSIZE, &
		SMG_WINDOW::HSIZE, &
		SMG_WINDOW::WNUMBER, &
		SMG$M_BORDER &
	)

	!
	! Set up title string
	!
	TOP_TITLE$ = " " + EDIT$(SMG_WINDOW::DESCR, 4% + 8% + 128%) + " "

	SMG_STATUS% = SMG$LABEL_BORDER(SMG_WINDOW::WNUMBER, LEFT(TOP_TITLE$, &
		SMG_WINDOW::HSIZE))

	!
	! Display background
	!
	V% = MAINT_GROUP(SMG_WINDOW, OPT_BACKGROUND, 0%, 0%, "")

	!
	! Paint the screen
	!
	REPAINT_FLAG% = -1%
	GOSUB PaintScreen
	REPAINT_FLAG% = 0%

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_WINDOW::WNUMBER, &
		SCOPE::SMG_PBID, &
		SMG_WINDOW::VPOS, &
		SMG_WINDOW::HPOS &
	)

	RETURN

	%PAGE

 ExitProgram:
15000	!*******************************************************************
	! Exit program
	!*******************************************************************

	!
	! Do before THE exit
	!
	SMG_STATUS% = MAINT_GROUP(SMG_WINDOW, OPT_EXIT, 0%, 0%, "")

	!
	! Erase displays
	!
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

	!
	! Remove created display if not in query mode
	!
	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_WINDOW::WNUMBER)

 QueryExit:
	SCOPE::PRG_IDENT	= KEEP_IDENT$ + ""
	SCOPE::PRG_PROGRAM	= KEEP_PRG$
	SCOPE::PRG_ITEM		= KEEP_ITEM$ + ""

	EXIT FUNCTION

	%PAGE

19000	!*******************************************************************
	! Error trapping
	!*******************************************************************

	!
	! Error LINE(ERL) cases
	!
	IF (ERN$ <> "MAIN_JOURNAL") AND (ERR = 155%)
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "Record not found!", 1%)
		RESUME 3110
	END IF

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME 19990

 HelpError:
19990	!
	! This moved from inside of error trapping to here so that
	! errors occurinbg within help could be trapped.  Basic will
	! not allow any errors to be trapped inside of another
	! error.
	!
	CALL HELP_3MESSAGE(SCOPE, &
		FILENAME$ + " " + NUM1$(ERL) + " " + ERT$(ERR), &
		"ERR", ERN$, "ERROR" + NUM1$(ERR))

	GOTO ExitProgram

32767	END FUNCTION
