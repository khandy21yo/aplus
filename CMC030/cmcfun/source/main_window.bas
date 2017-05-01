1	%TITLE "Window Maintenance"
	%SBTTL "MAIN_WINDOW"
	%IDENT "V3.6a Calico"

	FUNCTION LONG MAIN_WINDOW(WINDOW%, QUERY$)

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
	!	Maintains a file that does not allow duplicate keys.
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
	!	QUERY$ is used to call up specific options.
	!		The first letter that is passed through the function
	!		specifies what they really want to do.
	!		.table
	!			'Q' - Query
	!			'V' - View with select
	!			'M' - Maintain one record
	!			'C' - Change record
	!			'E' - Delete record
	!			'S' - Find the record and goto menu item
	!			'A' - Add record
	!		.endtable
	!
	!		The second letter specifies which key they want to
	!		do it on.
	!		.list
	!			'0' - Key number 0
	!			'1' - Key number 1
	!		.endlist
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
	!	VHPOS
	!		Horozontal position (width) to place the view window.
	!		This does not include the border.
	!
	!	VVPOS
	!		Vertical position (heignt) to place the view window.
	!		This does not include the border.
	!
	!	NITEMS
	!		Number of fields in the window.
	!
	!	NKEYS
	!		Number of keys to handle for this file.  This
	!		field is associated with KNAME and KFIELD.
	!
	!	KNAME(1..NKEYS)
	!		Name of the keys.
	!
	!	KFIELD(1..NKEYS, 0)
	!		The number of fields that make up this key.
	!
	!	KFIELD(1..NKEYS, 1..KFEILD(1..NKEYS, 0))
	!		The fields that make up this key.
	!
	!	HVIEW
	!		The width (horozontal) of the view.
	!
	!	VVIEW
	!		The length (vertical) if the view.
	!
	!	RELREC
	!		For a relative file, the record number of the record
	!		to be maintained.
	!
	!	WNUMBER
	!		The window number of the window for this screen
	!		as returned by SMG$CREATE_VIRTUAL_DISPLAY.
	!
	!	LWINDOW
	!		The number of the page window, it it exist.
	!
	!	LWIDTH
	!		The width of tha page window.
	!
	!	LHEIGHT
	!		The height of the page window.
	!
	!	LVPOS
	!		The vertical position of the page window.
	!
	!	LHPOS
	!		The horozontal position of the page window.
	!
	!	LLAST
	!		The last page used by the program.
	!
	!	LPAGE(0..7)
	!		The last fields on each page.
	!
	!	LTITLE(0..7)
	!		The title for each page.
	!
	!	LCURR
	!		The current page on the screen.
	!
	!	HFLAG(1..NITEMS)
	!		Hard/soft flags for the fields.
	!
	!	FLAGS
	!	.table
	!		1 - Enable window option to appear on screen.
	!		2 - Read only
	!		4 - Edit existing fields only.  Add, key change
	!			not allowed.
	!		128 - Relative file only, maintain one record only.
	!	.endtable
	!
	!	Returns a status code.
	!	.table
	!		1 - Succusful (List choices pressed, etc.)
	!		2 - Error (Didn't press select?)
	!		4 - Severe error (Didn't find key?)
	!	.endtable
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:MAIN_WINDOW/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP MAIN_WINDOW
	!	$ DELETE MAIN_WINDOW.OBJ;*
	!
	! Author:
	!
	!	01/12/86 - Kevin Handy and B. Craig Larsen
	!
	! Modification history:
	!
	!	07/14/87 - Kevin Handy
	!		Re-organization, and changes.
	!
	!	08/31/87 - Kevin Handy
	!		Modified error trapping slightly, so that errors
	!		in the help call could be trapped.
	!
	!	09/10/87 - Kevin Handy
	!		Modified for a user defined title for each page.
	!
	!	09/14/87 - Kevin Handy
	!		Fixed problem with calling OPT_MOREMENU without
	!		a current record.
	!
	!	10/09/87 - Kevin Handy
	!		Fixed problen in Query View, where the help text
	!		was of form "FLD002" instead of "View".
	!		Added "M" option to Query.
	!
	!	10/09/87 - Kevin Handy
	!		Modified the view option to give "ViewList" help
	!		while fully in the view option.
	!
	!	04/22/88 - Kevin Handy
	!		Fixed bug which caused massive crash when
	!		an error on an empty file caused a massive
	!		error message and program crash to occur.
	!
	!	04/27/88 - Kevin Handy
	!		Very slight modifications in error recovery
	!		after OPT_INIT to fix problems there.
	!
	!	05/04/88 - Kevin Handy
	!		Modified the view so that it would start and
	!		end through values returned through the
	!		OPT_VIEW.
	!
	!	05/10/88 - Kevin Handy
	!		Fixed bug where help was called while in a
	!		pasteboard update mode.
	!
	!	05/18/88 - Frank Starman
	!		Add a new option OPT_SUBWIND for subwindow
	!		(journal as a window) business
	!
	!	11/15/88 - Frank Starman
	!		Add a new query option "E" and "C"
	!
	!	03/01/90 - Kevin Handy
	!		Modified to delete sub-page display if it is
	!		active when the routine is exited.
	!
	!	04/05/90 - Frank Starman
	!		Add a new option CTRL-M (F9) for exit using
	!		MCL commnads.
	!
	!	07/06/90 - Frank Starman
	!		Add a new query option "A"
	!
	!	07/18/90 - Frank Starman
	!		Simplify the page function.
	!
	!	08/28/90 - Frank Starman
	!		Pass flag 16% in ENTR_3OPTION function in some cases.
	!		Disable message if option has been aborted.
	!
	!	09/04/90 - Frank Starman
	!		Replace in some cases ENTR_3MESSAGE function with
	!		HELP_34MESSAGE.
	!
	!	01/07/91 - Frank Starman
	!		Call OPT_SUBWINDOW if calling the view thru query.
	!
	!	03/13/91 - Frank Starman
	!		Set conditions for HFLAG.
	!
	!	05/22/91 - J. Shad Rydalch
	!		Fixed problem when LOOP% > 64 got subscript out
	!		of range. Split IF statment into two parts.
	!		INITR sub routine.
	!
	!	06/25/91 - Frank F. Starman
	!		Set flag 1% for OPT_RESETDEFAULT
	!
	!	08/09/91 - Craig P. Tanner
	!		Pass QUERY$ through MVALUE in OPT_INIT so that
	!		program will know if it is being called by
	!		_MAST_ or MAIN_WINDOW.
	!
	!	08/20/91 - Frank F. Starman
	!		Pass RIGHT(QUERY$, 3%) as a MVALUE to subwindow.
	!
	!	09/13/91 - Frank F. Starman
	!		Match arguments in MAIN_WINDOWADD function.
	!
	!	09/30/91 - Kevin Handy
	!		Modified so that using negitive key numbers will
	!		not crash find.
	!
	!	10/08/91 - Kevin Handy
	!		Modified so that negitive numbers do not crash
	!		the view option.
	!
	!	02/10/92 - Frank F. Starman
	!		Allow F17 key as a special key for a function selection.
	!
	!	02/21/92 - Kevin Handy
	!		Modified to show window number as part of error message.
	!
	!	03/05/92 - Kevin Handy
	!		Fixed bug that "NoFind" routine would just fall
	!		through into the next subroutine, where it didn't
	!		belong.
	!
	!	08/14/92 - Kevin Handy
	!		Attempt to fix bug where someone changed all '#'s
	!		in program to tab characters. (Who knows why)
	!
	!	11/09/92 - Frank Starman
	!		Fixed bug in subwindow view.
	!
	!	11/24/92 - Kevin Handy
	!		Fixed bug found in Alpha-AXP conversion where
	!		ENTR_3NUMBER was called with 0% instead of 0.0
	!
	!	02/12/93 - Kevin Handy
	!		Modified to trap error in view when record
	!		is not found, and do something better than
	!		comming up with empty option list.
	!
	!	03/26/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	03/30/93 - Frank Starman
	!		Shorted Initialize option to Init.
	!
	!	05/14/93 - Kevin Handy
	!		Modified QueryChange to call OPT_AFTEROPT in case there
	!		are sub-lines when this is used as a sort of journal
	!		maintenance.
	!
	!	06/09/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	11/03/93 - Frank F. Starman
	!		Do not call opt_subwind with -1%, after Kevin made mdification
	!		to QueryChange. Othewise it would double erase.
	!
	!	04/05/94 - Kevin Handy
	!		Modified to set ATEMP$ to "Next" when in next
	!		option, so it won't ask for "view starting at"
	!		when can't find next record.
	!
	!	04/13/94 - Kevin Handy
	!		Fixed bug where "erase" changed prg_item to "DELCNF"
	!		and never changed it back to "Erase".
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	08/15/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!
	!	09/04/96 - Kevin Handy
	!		Clean up (Check)
	!		Lose line defining "PROMPT$".
	!
	!	08/28/97 - Kevin Handy
	!		Lose unecessary definitions
	!
	!	12/04/1997 - Kevin Handy
	!		Lose extra step (Resume BadInView,
	!		goto 6020) to just (resume 6020)
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/29/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!		Lose OPT_LOG calls, which nothing implements.
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
	DECLARE		CDD_WINDOW_CDD		SMG_WINDOW

	%INCLUDE "SOURCE:[SMG.OPEN]SMG_SCROLL.HB"
	DECLARE		SMG_SCROLL_CDD		SMG_SCROLL

	!
	! External declarations
	!
	EXTERNAL INTEGER FUNCTION DSPL_SCROLLCIR

	EXTERNAL INTEGER FUNCTION MAINT_GROUP
	EXTERNAL INTEGER FUNCTION MAIN_WINDOWADD
	EXTERNAL INTEGER FUNCTION MAIN_WINDOWCHANGE
	EXTERNAL INTEGER FUNCTION MAIN_WINDOWBLANK
	EXTERNAL INTEGER FUNCTION MAIN_DEFAULT

	!
	! Declare functions
	!
	DECLARE LONG SMG_VIEW, SMG_BLANK, SMG_BLANK1, &
		VIEW_WIDTH, ELEN, &
		C_WIDTH, NEWWIDTH

	DECLARE RFA INIT_RFA, MAINT_FILE_RFA, NULL_RFA

	%PAGE

	!
	! Initilization section	Prepare to do anything
	!
	ON ERROR GOTO 19000

	KEEP_IDENT$ = SCOPE::PRG_IDENT + ""
	KEEP_PRG$ = SCOPE::PRG_PROGRAM + ""
	KEEP_ITEM$ = SCOPE::PRG_ITEM + ""

	!
	! Assume things will work out OK
	!
	MAIN_WINDOW = 1%

	!
	! Call up initilization
	!
	SMG_WINDOW::IDENT = WINDOW%
	V% = MAINT_GROUP(SMG_WINDOW, OPT_INIT, 0%, 0%, LEFT(QUERY$, 1%))
	SCOPE::PRG_PROGRAM = SMG_WINDOW::NHELP

	IF V% <> 0%
	THEN
		V1% = SMG$END_PASTEBOARD_UPDATE(SCOPE::SMG_PBID)

		CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(V%), "E", &
			ERN$, "", NUM1$(V%)) IF (QUERY$ = "")

		GOTO QueryExit
	END IF

	DIM INIT%(SMG_WINDOW::NITEMS)

	!
	! Set up title string
	!
	TOP_TITLE$ = " " + EDIT$(SMG_WINDOW::DESCR, 4% + 8% + 128%) + " "
	TEMP1$ = EDIT$(SCOPE::PRG_COMPANY, 4% + 8% + 128%)

	TOP_TITLE$ = TOP_TITLE$ + "for " + TEMP1$ + " " &
		IF TEMP1$ <> "" AND SMG_WINDOW::CURREC <> -2%

	SMG_WINDOW::LPAGE(0%) = SMG_WINDOW::NITEMS &
		IF SMG_WINDOW::LPAGE(0%) = 0%

	SMG_WINDOW::LTITLE(0%) = "Main-screen" &
		IF EDIT$(SMG_WINDOW::LTITLE(0%), 4% + 128%) = ""

	FOR LOOP% = 1% TO SMG_WINDOW::LLAST

		SMG_WINDOW::LTITLE(LOOP%) = "Page-" + NUM1$(LOOP%) &
			IF EDIT$(SMG_WINDOW::LTITLE(LOOP%), 4% + 128%) = ""

	NEXT LOOP%

	!
	! Handle QUERY$ specially
	!
300	SELECT LEFT(QUERY$, 1%)

	CASE "M"
		!
		! Query type "M", go into maintainence with nearest record
		! to specified one
		!
		KEY_NUMBER% = VAL%(MID(QUERY$, 2%, 1%))

		WHEN ERROR IN
			IF EDIT$(RIGHT(QUERY$, 3%), -1%) = ""
			THEN
				RESET #SMG_WINDOW::CHAN
				GET #SMG_WINDOW::CHAN, REGARDLESS
			ELSE
				GET #SMG_WINDOW::CHAN, &
					KEY #KEY_NUMBER% GE RIGHT(QUERY$, 3%), &
					REGARDLESS
			END IF
		USE
			CONTINUE CreateDisplay
		END WHEN

	CASE "S"
		!
		! Go directly to Sub window(subwindow must be in menu list)
		!
		KEY_NUMBER% = VAL%(MID(QUERY$, 2%, 1%))
		X_INDEX% = INSTR(1%, QUERY$, "|")
		X_TEST$ = MID(QUERY$, 3%, X_INDEX% - 3%)
		X_TEST$ = "" IF X_TEST$ = "|"

		IF EDIT$(X_TEST$, -1%) = ""
		THEN
			RESET #SMG_WINDOW::CHAN
			GET #SMG_WINDOW::CHAN, REGARDLESS
		ELSE
			GET #SMG_WINDOW::CHAN, KEY #KEY_NUMBER% GE X_TEST$, &
				REGARDLESS
		END IF

		MENU_X$ = TRM$(RIGHT(QUERY$, X_INDEX% + 1%))

	CASE "A"
		!
		! Add line records for header
		!

	CASE ELSE
		IF QUERY$ <> ""
		THEN
			GOSUB QueryTest
			GOTO QueryExit
		END IF

	END SELECT

 CreateDisplay:
	!
	! Create display for this file
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		SMG_WINDOW::VSIZE, &
		SMG_WINDOW::HSIZE, &
		SMG_WINDOW::WNUMBER, &
		SMG$M_BORDER &
	)

	SMG_STATUS% = SMG$LABEL_BORDER(SMG_WINDOW::WNUMBER, LEFT(TOP_TITLE$, &
		SMG_WINDOW::HSIZE))

	!
	! Display background
	!
	V% = MAINT_GROUP(SMG_WINDOW, OPT_BACKGROUND, 0%, 0%, "")

	NEWWIDTH = SMG_WINDOW::HSIZE + 2%
	NEWWIDTH = 80% IF NEWWIDTH < 80%
	NEWWIDTH = 132% IF NEWWIDTH > 132%

	SMG_STATUS% = SMG$CHANGE_PBD_CHARACTERISTICS(SCOPE::SMG_PBID, , C_WIDTH)

	IF NEWWIDTH <> C_WIDTH
	THEN
		!
		! Change the width
		!
		SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(20%, 132%, SMG_BLANK1)

		SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_BLANK1, &
			SCOPE::SMG_PBID, 1%, 1%)

		SMG_STATUS% = SMG$CHANGE_PBD_CHARACTERISTICS(SCOPE::SMG_PBID, &
			NEWWIDTH)
	END IF

500	!
	! Get the RFA (May cause error)
	!
	IF (SMG_WINDOW::FLAGS AND 128%)
	THEN
		SMG_WINDOW::RELREC = 1% IF SMG_WINDOW::RELREC = 0%
		WHEN ERROR IN
			GET #SMG_WINDOW::CHAN, RECORD SMG_WINDOW::RELREC
		USE
			CONTINUE 510 IF (ERR = 131%) OR (ERR = 132%) OR &
				(ERR = 173%)
			FILENAME$ = ""
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Try to find the first record (Maintained by user program)
	!
	V% = MAINT_GROUP(SMG_WINDOW, OPT_SUBWIND, 1%, 0%, RIGHT(QUERY$, 3%))

	IF SMG_WINDOW::CURREC = -1%
	THEN
		SMG_WINDOW::CURREC = 0%
		GOTO 510
	END IF

	WHEN ERROR IN
		MAINT_FILE_RFA = GETRFA(SMG_WINDOW::CHAN)
	USE
		CONTINUE 510
	END WHEN

	CALL MAIN_PAINTSCREEN(SMG_WINDOW, " ", 0%)

510	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_WINDOW::WNUMBER, &
		SCOPE::SMG_PBID, &
		SMG_WINDOW::VPOS, &
		SMG_WINDOW::HPOS &
	)

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
		IF (SMG_WINDOW::FLAGS AND 128%)
		THEN
			OPLIST$ = "Help eXit"
		ELSE
			OPLIST$ = "Find Next Restore View Help eXit"
		END IF
	ELSE
		!
		! Edit existing fields only
		!
		IF (SMG_WINDOW::FLAGS AND 4%)
		THEN
			IF (SMG_WINDOW::FLAGS AND 128%)
			THEN
				OPLIST$ = "Change Blank Help eXit"
			ELSE
				OPLIST$ = "Change Blank Init " + &
					"Find Next Restore View Help eXit"
			END IF
		ELSE
			!
			! Full featured
			!
			IF (SMG_WINDOW::FLAGS AND 128%)
			THEN
				OPLIST$ = "Add Erase Change Blank Default " + &
					"Help eXit"
			ELSE
				OPLIST$ = "Add Erase Change Blank Init " + &
					"Default Find Next Restore View Help eXit"
			END IF
		END IF
	END IF

	OPLIST$ = OPLIST$ + " Page" IF (SMG_WINDOW::LLAST <> 0%)
	OPLIST$ = OPLIST$ + " Window" IF (SMG_WINDOW::FLAGS AND 1%)

	!
	! Let the user program change the options if necessary
	!
	V% = MAINT_GROUP(SMG_WINDOW, OPT_OPTLIST, 0%, 0%, OPLIST$)

	IF MENU_X$ <> ""
	THEN
		SCOPE::PRG_ITEM = MENU_X$
		MENU_X$ = ""
		GOTO Menu_X
	END IF

	IF LEFT(QUERY$, 1%) = "A"
	THEN
		!
		! Add lines to the header
		!
		GOSUB Addr
		GOTO ExitProgram
	END IF

	GOTO Menu

	%PAGE

1000	!*******************************************************************
	! Program restart point
	!*******************************************************************

 MenuCurrent:
1020	!
	! Display current record
	!
	WHEN ERROR IN
		GET #SMG_WINDOW::CHAN, RFA MAINT_FILE_RFA, REGARDLESS
	USE
		IF (ERR = 131%) OR (ERR = 132%) OR (ERR = 173%)
		THEN
			CONTINUE ErrorTrapA
		END IF

		CONTINUE HelpError
	END WHEN

	CALL MAIN_PAINTSCREEN(SMG_WINDOW, " ", 0%)

 Menu:
1030	!
	! Enter desired option
	!
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
	! Prev Page
	!
	CASE SMG$K_TRM_PREV_SCREEN

		IF SMG_WINDOW::LLAST <> 0%
		THEN
			CALL MAIN_PAINTSCREEN(SMG_WINDOW, "P", 0%)
		ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		END IF

		GOTO Menu

	!
	! Next Page
	!
	CASE SMG$K_TRM_NEXT_SCREEN

		IF SMG_WINDOW::LLAST <> 0%
		THEN
			CALL MAIN_PAINTSCREEN(SMG_WINDOW, "N", 0%)
		ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		END IF

		GOTO Menu

	!
	! Good keys
	!
	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO, SMG$K_TRM_SELECT
		! Good key

	CASE  SMG$K_TRM_F17
		V% = MAINT_GROUP(SMG_WINDOW, OPT_WINDOW, 0%, 0%, "MAGIC")

		GOTO Menu

	!
	! Bad keys
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 1030

	END SELECT

 Menu_X:
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
1120		ATEMP$ = SCOPE::PRG_ITEM
		GOSUB 3100	! Print current record
		GOTO Menu

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
1130		WHEN ERROR IN
			RESET #SMG_WINDOW::CHAN, KEY #0%
			FIND #SMG_WINDOW::CHAN, REGARDLESS
		USE
			CONTINUE Menu IF ERR = 11%
			FILENAME$ = ""
			CONTINUE HelpError
		END WHEN

		!
		! Try to find the first record (Maintained by user program)
		!
		V% = MAINT_GROUP(SMG_WINDOW, OPT_SUBWIND, 1%, 0%, &
			RIGHT(QUERY$, 3%))

		IF SMG_WINDOW::CURREC = 0%
		THEN
			MAINT_FILE_RFA = GETRFA(SMG_WINDOW::CHAN)
			GOTO MenuCurrent
		ELSE
			SMG_WINDOW::CURREC = 0%
			GOTO Menu
		END IF

	!
	!
	! This option allows records to be viewed
	!
	CASE "View"
		GOSUB Viewr
		GOTO MenuCurrent

	!
	! Initialize
	!
	! This option allows all records from the current record
	! to the end of the file to be changed.  Something like
	! a loop of (change, next).
	!
	CASE "Init"
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
		CALL HELP_34MESSAGE(SCOPE, "", "H", &
			SMG_WINDOW::NHELP, "", "HELP")

		GOTO Menu

	!
	! eXit
	!
	! This option exits out of the program.
	!
	CASE "eXit"
		GOTO ExitProgram

	!
	! Page
	!
	! This option allows the user to select a page
	! on a multiple page maintenance routine.
	!
	CASE "Page"
		!
		! Switch to the page
		!
		CALL MAIN_PAINTSCREEN(SMG_WINDOW, "N", 0%)
		GOTO Menu

1150	!
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
		! Call the users routine
		!
		V% = MAINT_GROUP(SMG_WINDOW, OPT_MOREMENU, &
			0%, 0%, TEMP$)

		GOTO ExitProgram IF TEMP$ = "EXIT"

		!
		! Allow the user routine to force the program
		! to completely exit out.
		!
		SELECT V%

		!
		! Good return make sure of the current record
		!
		CASE 8%

			WHEN ERROR IN
				MAINT_FILE_RFA = GETRFA(SMG_WINDOW::CHAN)
			USE
				CONTINUE ErrorTrapA
			END WHEN
		!
		! Bad return repaint
		!
		CASE 2%

		!
		! Exit the program
		!
		CASE 4%
			GOTO ExitProgram

		!
		! Go for the first record
		!
		CASE 16%
			SUBWIND% = -1%
			GOSUB 3110
			SUBWIND% = 0%
			GOTO Menu

		END SELECT

	END SELECT

	GOTO MenuCurrent

	%PAGE

 Addr:
2000	!*******************************************************************
	! Add new record
	!
	! This option allows records to be added to the file.
	!
	!*******************************************************************

	!
	! Set defaults and clear screen
	!
	V% = MAINT_GROUP(SMG_WINDOW, OPT_RESETDEFAULT, 0%, 1%, "")

2110	SELECT MAIN_WINDOWADD(SMG_WINDOW, QUERY$)

	CASE 0%
2120		!
		! Add record to file
		!
		IF MAINT_GROUP(SMG_WINDOW, OPT_TESTOPT, 0%, 0%, "ADD") = 0%
		THEN
			V% = MAINT_GROUP(SMG_WINDOW, OPT_SETOLD, 0%, 0%, "")

			WHEN ERROR IN
				IF (SMG_WINDOW::FLAGS AND 128%)
				THEN
					PUT #SMG_WINDOW::CHAN, &
						RECORD SMG_WINDOW::RELREC
				ELSE
					PUT #SMG_WINDOW::CHAN
				END IF
			USE
				CONTINUE 2200 IF ERR = 134%
				FILENAME$ = ""
				CONTINUE HelpError
			END WHEN

			MAINT_FILE_RFA = GETRFA(SMG_WINDOW::CHAN)
 !			V% = MAINT_GROUP(SMG_WINDOW, OPT_LOG, 0%, 0%, "")
			V% = MAINT_GROUP(SMG_WINDOW, OPT_AFTEROPT, 0%, 0%, &
				"Add")

			!
			! Try to add next line record for the header
			!
			GOTO 2000 IF LEFT(QUERY$, 1%) = "A" AND V% = 0%
		END IF

	CASE 2%
		!
		! Bad input, Repaint Screen
		!
		MAINT_FILE_RFA = GETRFA(SMG_WINDOW::CHAN)

	CASE 4%
		!
		! Some type of an abort
		!
		IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_PF3) OR &
			(SCOPE::SCOPE_EXIT = SMG$K_TRM_F8)
		THEN
			!
			! Cancel, PF3 typed.  These abort the add
			! and request that the add be restarted.
			!
			GOTO 2000
		END IF

	END SELECT

	!
	! Force back to main screen
	!
	CALL MAIN_PAINTSCREEN(SMG_WINDOW, "S", 0%)

	RETURN

2200	!
	! Handle duplicate key detected
	!
	CALL HELP_34MESSAGE(SCOPE, "duplicate key detected.", "W", &
		SMG_WINDOW::NHELP, "", "134")

	GOTO 2110

	%PAGE

 Eraser:
3000	!*******************************************************************
	! Erase a record
	!
	! This option will erase the current record
	!
	!*******************************************************************

	WHEN ERROR IN
		FIND #SMG_WINDOW::CHAN, RFA MAINT_FILE_RFA
	USE
		CONTINUE 3025 IF ERR = 173%
		CONTINUE 3035 IF ERR = 154%
		FILENAME$ = ""
		CONTINUE HelpError
	END WHEN

	SCOPE::PRG_ITEM = "DELCNF"
	!
	! Make sure they want to delete that record
	!
	INP$ = ENTR_3YESNO(SCOPE, SMG_WINDOW::WNUMBER, "", &
		"Confirm deletion - then press <DO>", "N", 0%, "!", "")

	SELECT SCOPE::SCOPE_EXIT

	!
	! Exit keys
	!
	CASE 3%, SMG$K_TRM_F8, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ

		CALL HELP_34MESSAGE(SCOPE, "erase aborted", "W", &
			"MAIN_WINDOW", "", "NOERASE")
	!++
	! Warning:NOERASE
	!	^*Erase Aborted\*
	!	.b
	!	.lm +5
	!	^*Explanation\*
	!	.b
	!	A record has not been erased from the file.
	!	.b
	!	^*User Action\*
	!	.b
	!	Type ^*Y\* to confirm a record deletion if attempt to delete
	!	the record.
	!	.lm -5
	!
	! Index:
	!	.x Erase Aborted
	!
	!--

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

	SCOPE::PRG_ITEM = "Erase"

	RETURN IF INP$ <> "Y"

	RETURN IF &
		MAINT_GROUP(SMG_WINDOW, OPT_TESTOPT, 0%, 0%, "ERASE") <> 0%

3010	!
	! Try to delete the record
	!
	DELETE	#SMG_WINDOW::CHAN
 !	V% = MAINT_GROUP(SMG_WINDOW, OPT_LOG, 0%, 0%, "Erase")
	V% = MAINT_GROUP(SMG_WINDOW, OPT_AFTEROPT, 0%, 0%, "Erase")

	!
	! everything seems to be OK, go for the next record
	!
	GOTO 3100

3025	!
	! empty files
	!
	CALL HELP_34MESSAGE(SCOPE, "no current record", "W", &
		SMG_WINDOW::NHELP, "", "131")

	RETURN

3035	!
	! record is lock
	!
	CALL HELP_34MESSAGE(SCOPE, "record is locked", "W", &
		SMG_WINDOW::NHELP, "", "154")

	RETURN

	%PAGE

3100	!*******************************************************************
	! Get the next record and print it
	!*******************************************************************

	!
	! First try, get the next record
	!
	WHEN ERROR IN
		GET #SMG_WINDOW::CHAN, REGARDLESS
	USE
		IF ERR = 11%
		THEN
			CONTINUE 3110
		END IF
		FILENAME$ = ""
		CONTINUE HelpError
	END WHEN

	V% = MAINT_GROUP(SMG_WINDOW, OPT_SUBWIND, 3%, 0%, RIGHT(QUERY$, 3%))

	IF SMG_WINDOW::CURREC = -1%
	THEN
		SMG_WINDOW::CURREC = 0%
		GOTO 3110
	END IF

	MAINT_FILE_RFA = GETRFA(SMG_WINDOW::CHAN)
	CALL MAIN_PAINTSCREEN(SMG_WINDOW, " ", 0%)

	RETURN

 NoFind:
	CALL HELP_34MESSAGE(SCOPE, "record not found !", "W", &
		SMG_WINDOW::NHELP, "", "155")

	GOTO MenuCurrent

3110	!
	! Second try, get the first record.
	!
	! Restore to first record when get on line 3100 failes
	! with end of file.
	!
	IF SCOPE::PRG_ITEM = "Next"
	THEN
		!
		! Next gets stuck at end of file
		!
		CALL HELP_34MESSAGE(SCOPE, "end of the file reached", "W", &
			SMG_WINDOW::NHELP, "", "11")

		WHEN ERROR IN
			GET #SMG_WINDOW::CHAN, RFA MAINT_FILE_RFA, REGARDLESS
		USE
			CONTINUE 3120 IF ERR = 11% OR ERR = 173%
			FILENAME$ = ""
			CONTINUE HelpError
		END WHEN

		RETURN
	END IF

	WHEN ERROR IN
		RESET #SMG_WINDOW::CHAN
		GET #SMG_WINDOW::CHAN, REGARDLESS
	USE
		CONTINUE 3120 IF ERR = 11% OR ERR = 173%
		FILENAME$ = ""
		CONTINUE HelpError
	END WHEN

	!
	! Initialize  (Maintained by user program)
	!
	V% = MAINT_GROUP(SMG_WINDOW, OPT_SUBWIND, 1%, 0%, RIGHT(QUERY$, 3%))

	IF SMG_WINDOW::CURREC = -1%
	THEN
		SMG_WINDOW::CURREC = 0%
		GOTO 3120
	END IF

	MAINT_FILE_RFA = GETRFA(SMG_WINDOW::CHAN)

	CALL HELP_34MESSAGE(SCOPE, "end of the file reached", "W", &
		SMG_WINDOW::NHELP, "", "11") IF SUBWIND% = 0%

	CALL MAIN_PAINTSCREEN(SMG_WINDOW, " ", 0%)

	RETURN

3120	!
	! Third try, give up.
	!
	! There is nothing here.  Blank the screen.  Restore to here when
	! reset on line 3110 failes.
	!
	V% = MAINT_GROUP(SMG_WINDOW, OPT_BACKGROUND, 0%, 0%, "")

	!CALL HELP_34MESSAGE(SCOPE, "no current record", "W", &
	!	SMG_WINDOW::NHELP, "", "131")

	RETURN

	%PAGE

 Changer:
4000	!*******************************************************************
	! Change/Blank record
	!
	! This option allows an existing record to be changed or blanked.
	!
	!*******************************************************************

	WHEN ERROR IN
		GET #SMG_WINDOW::CHAN, RFA MAINT_FILE_RFA	! Lock the record
	USE
		CONTINUE 4015 IF ERR = 173%
		CONTINUE 4017 IF ERR = 154%
		FILENAME$ = ""
		CONTINUE HelpError
	END WHEN

	V% = MAINT_GROUP(SMG_WINDOW, OPT_SETOLD, 0%, 0%, "")

	SELECT OPT$

	CASE "C"
		STAT% = MAIN_WINDOWCHANGE(SMG_WINDOW)

	CASE "B"
		STAT% = MAIN_WINDOWBLANK(SMG_WINDOW)

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

 !			V% = MAINT_GROUP(SMG_WINDOW, OPT_LOG, 0%, 0%, "")
			V% = MAINT_GROUP(SMG_WINDOW, OPT_AFTEROPT, &
				0%, 0%, TRM$(SCOPE::PRG_ITEM))
		END IF

	!
	! Bad change, recall original
	!
	CASE ELSE
		UNLOCK #SMG_WINDOW::CHAN
		CALL HELP_34MESSAGE(SCOPE, TRM$(SCOPE::PRG_ITEM) + " aborted", &
			"W", "MAIN_WINDOW", "", "NOCHANGE")
	!++
	! Warning:NOCHANGE
	!	^*No Change Done\*
	!	.b
	!	.lm +5
	!	^*Explanation\*
	!	.b
	!	No change has been done on the record.
	!	.b
	!	^*User Action\*
	!	.b
	!	Hit Return key instead of Exit key to save all changes after
	!	they have been done.
	!
	! Index:
	!	.x Change
	!
	!--

	END SELECT

	RETURN

4015	!
	! exit from Change/Blank
	!
	!
	! empty files
	!
	CALL HELP_34MESSAGE(SCOPE, "no current record", "W", &
		SMG_WINDOW::NHELP, "", "131")

	RETURN

4017	!
	! record is lock
	!
	CALL HELP_34MESSAGE(SCOPE, "record is locked", "W", &
		SMG_WINDOW::NHELP, "", "154")

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
		CALL HELP_34MESSAGE(SCOPE, "key is not changable", "W", &
			SMG_WINDOW::NHELP, "", "130")
		RETURN
	END IF

	FIND	#SMG_WINDOW::CHAN, RFA MAINT_FILE_RFA
	DELETE	#SMG_WINDOW::CHAN

4030	!
	! Add the new changed record to the file
	!
	WHEN ERROR IN
		PUT #SMG_WINDOW::CHAN
		MAINT_FILE_RFA = GETRFA(SMG_WINDOW::CHAN)
	USE
		CONTINUE 4040 IF ERR = 134%
		FILENAME$ = ""
		CONTINUE HelpError
	END WHEN

 !	V% = MAINT_GROUP(SMG_WINDOW, OPT_LOG, 0%, 0%, "")
	V% = MAINT_GROUP(SMG_WINDOW, OPT_AFTEROPT, 0%, 0%, &
		TRM$(SCOPE::PRG_ITEM))

	RETURN

4040	!****
	! Add the original record back into the file because a
	! duplicate key was found when attempting to change the
	! key the hard way.
	!****

	V% = MAINT_GROUP(SMG_WINDOW, OPT_RESETOLD, 0%, 0%, "")	! Restore the original buffer

	WHEN ERROR IN
		PUT #SMG_WINDOW::CHAN
		MAINT_FILE_RFA = GETRFA(SMG_WINDOW::CHAN)
	USE
		CONTINUE 4060 IF ERR = 134%
		FILENAME$ = ""
		CONTINUE HelpError
	END WHEN

4050	!
	! If item already exists, display it
	!
	UNLOCK	#SMG_WINDOW::CHAN

	CALL HELP_34MESSAGE(SCOPE, "record already exists with this key", "W", &
		SMG_WINDOW::NHELP, "", "134")

	RETURN

4060	!****
	! Original record has duplicate key detected.  This occurs when
	! the original key will not go back into the file after it has
	! been deleted.
	!****

	V% = MAINT_GROUP(SMG_WINDOW, OPT_RESETOLD, 0%, 0%, "")	! Restore the original buffer

	CALL MAIN_PAINTSCREEN(SMG_WINDOW, " ", 0%)

	RESET	#SMG_WINDOW::CHAN, KEY #0%
	FIND	#SMG_WINDOW::CHAN, REGARDLESS

	MAINT_FILE_RFA = GETRFA(SMG_WINDOW::CHAN)

	RETURN

	%PAGE

 Findr:
5000	!*******************************************************************
	! Find a record
	!*******************************************************************

	ATEMP$ = SCOPE::PRG_ITEM

	IF SMG_WINDOW::NKEYS = 1%
	THEN
		S_ITEM% = 0%
		GOTO 5020
	END IF

	!
	! Ask for item to search for
	!
	S_LIST$ = TRM$(SMG_WINDOW::KNAME(0%))

	S_LIST$ = S_LIST$ + " " + TRM$(SMG_WINDOW::KNAME(I%)) &
		FOR I% = 1% TO SMG_WINDOW::NKEYS - 1%

	SORT_BY% = 0%
	SCOPE::PRG_ITEM = "SEARCHBY"
	SORT_BY$ = ENTR_3OPTION(SCOPE, "Search by", S_LIST$ + " eXit", SORT_BY%, 16%)

	SELECT SCOPE::SCOPE_EXIT

	!
	! Exit keys
	!
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_F8, SMG$K_TRM_CTRLZ

		!CALL ENTR_3MESSAGE(SCOPE, "Find aborted", 1%)
		RETURN

	!
	! Good keys
	!
	CASE 0%, 10%, 12%, 13%, 296%
		! Good key

	!
	! Bad keys
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 5000

	END SELECT

	RETURN IF SORT_BY$ = "X"

	!
	! Figure out which one they chose
	!
	S_ITEM$ = SEG$(S_LIST$, SORT_BY%, INSTR(SORT_BY%, S_LIST$ + " ", " "))

	S_ITEM% = 0%

	S_ITEM% = I% IF S_ITEM$ = TRM$(SMG_WINDOW::KNAME(I%)) &
		FOR I% = 0% TO SMG_WINDOW::NKEYS - 1%

5020	!
	! Ask for field
	!
	SCOPE::PRG_ITEM = ATEMP$

	FOR Q% = 1% TO SMG_WINDOW::KFIELD(S_ITEM%, 0%)

5030		LOOP% = SMG_WINDOW::KFIELD(S_ITEM%, Q%)

		IF LOOP% <= 0%
		THEN
			CALL MAINT_GROUP(SMG_WINDOW, OPT_ENTRY, LOOP%, 0%, "")
		ELSE
			CALL MAINT_GROUP(SMG_WINDOW, OPT_ENTRY, LOOP%, 0%, "") &
				IF (SMG_WINDOW::HFLAG(LOOP%) AND 2%) = 0%
		END IF

		SELECT SCOPE::SCOPE_EXIT

		!
		! Exit keys
		!
		CASE 3%, SMG$K_TRM_F8, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ

			V% = MAINT_GROUP(SMG_WINDOW, OPT_BACKGROUND, 0%, 0%, "")
			!CALL ENTR_3MESSAGE(SCOPE, "Find aborted", 1%)

			RETURN

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
			GOTO 5030

		END SELECT

	NEXT Q%

5040	!
	! Search for the item
	!
	V% = MAINT_GROUP(SMG_WINDOW, OPT_FIND, S_ITEM%, 0%, "")

	!
	! Get the current record and print it
	!
	SMG_WINDOW::CURREC = -1%
	WHEN ERROR IN
		GET #SMG_WINDOW::CHAN, REGARDLESS
	USE
		CONTINUE 5050 IF ERR = 155% OR ERR = 11%
		FILENAME$ = ""
		CONTINUE HelpError
	END WHEN

	SMG_WINDOW::CURREC = 0%

	V% = MAINT_GROUP(SMG_WINDOW, OPT_SUBWIND, 3%, 0%, RIGHT(QUERY$, 3%))

	MAINT_FILE_RFA = GETRFA(SMG_WINDOW::CHAN) IF SMG_WINDOW::CURREC = 0%

5050	IF SMG_WINDOW::CURREC = -1%
	THEN
		SMG_WINDOW::CURREC = 0%
		CALL HELP_34MESSAGE(SCOPE, "record not found!", "W", &
			SMG_WINDOW::NHELP, "", "155")
	END IF

	!
	! Exit from find
	!
	RETURN

	%PAGE

 Viewr:
6000	!*******************************************************************
	! VIEW ROUTINE
	!*******************************************************************

	ATEMP$ = SCOPE::PRG_ITEM

	!
	! Don't ask for key number if there is only one key
	!
	IF SMG_WINDOW::NKEYS = 1%
	THEN
		S_ITEM% = 0%
		GOTO 6020
	END IF

	!
	! Ask for item to search for
	!
	S_LIST$ = TRM$(SMG_WINDOW::KNAME(0%))

	S_LIST$ = S_LIST$ + " " + TRM$(SMG_WINDOW::KNAME(I%)) &
		FOR I% = 1% TO SMG_WINDOW::NKEYS - 1%

	SORT_BY% = 0%

	SCOPE::PRG_ITEM = "VIEWBY"
	SORT_BY$ = ENTR_3OPTION(SCOPE, "View " + EDIT$(SMG_WINDOW::NHELP, 4% + 128%) + " by", &
		S_LIST$ + " eXit", SORT_BY%, 16%)

	SELECT SCOPE::SCOPE_EXIT

	!
	! Exit keys
	!
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_F8, SMG$K_TRM_CTRLZ
		!CALL ENTR_3MESSAGE(SCOPE, "View aborted", 1%)
		RETURN

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
		GOTO 6000

	END SELECT

	RETURN IF SORT_BY$ = "X"

	!
	! Figure out which one they chose
	!
	S_ITEM$ = SEG$(S_LIST$, SORT_BY%, INSTR(SORT_BY%, S_LIST$ + " ", " "))

	S_ITEM% = 0%

	S_ITEM% = I% IF S_ITEM$ = TRM$(SMG_WINDOW::KNAME(I%)) &
		FOR I% = 0% TO SMG_WINDOW::NKEYS - 1%

6020	!
	! Ask for field
	!
	SCOPE::PRG_ITEM = ATEMP$

	V% = MAINT_GROUP(SMG_WINDOW, OPT_SUBWIND, 1%, 0%, RIGHT(QUERY$, 3%)) &
		IF LEFT(QUERY$, 2%) = "VX"

	FOR Q% = 1% TO SMG_WINDOW::KFIELD(S_ITEM%, 0%)

6030		LOOP% = SMG_WINDOW::KFIELD(S_ITEM%, Q%)

		IF LOOP% >= 0%
		THEN
			CALL MAINT_GROUP(SMG_WINDOW, OPT_ENTRY, LOOP%, 0%, "") &
				IF (SMG_WINDOW::HFLAG(LOOP%) AND 2%) = 0%
		ELSE
			CALL MAINT_GROUP(SMG_WINDOW, OPT_ENTRY, LOOP%, 0%, "")
		END IF

		SELECT SCOPE::SCOPE_EXIT

		!
		! Exit keys
		!
		CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_F8, SMG$K_TRM_CTRLZ

			V% = MAINT_GROUP(SMG_WINDOW, OPT_BACKGROUND, 0%, 0%, "")
			!CALL ENTR_3MESSAGE(SCOPE, "View aborted", 1%)
			RETURN

		!
		! Good keys
		!
		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO, &
			SMG$K_TRM_F7	! Good key

		!
		! Bad keys
		!
		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO 6030

		END SELECT

	NEXT Q%

	!
	! Search for the item
	!
	IN_VIEW% = -1%
	V% = MAINT_GROUP(SMG_WINDOW, OPT_FIND, S_ITEM%, 0%, "")
	IN_VIEW% = 0%

	%PAGE

	!*******************************************************************
	! Another entry point into the view used by the QUERY$ stuff to
	! bypass all of the question that a regular view would ask.
	!*******************************************************************
 ViewBypass:
6040	SCOPE::PRG_ITEM = "ViewList"

	!
	! Allocate memory for view
	!
	VIEW_A_SIZ% = 500%

	DIM VIEW_A$(VIEW_A_SIZ%)
	DIM RFA VIEW_A_RFA(VIEW_A_SIZ%)

	!
	! Initialize the strings
	!
	V% = MAINT_GROUP(SMG_WINDOW, OPT_VIEW, 1%, 0%, NAME.STRING$)
	V% = MAINT_GROUP(SMG_WINDOW, OPT_VIEW, 2%, 0%, SMG_SCROLL::DRAW_COLS)

	!
	! Figure width of view
	!
	VIEW_WIDTH = SMG_WINDOW::HVIEW + 2%
	VIEW_WIDTH = 80% IF VIEW_WIDTH < 80%
	VIEW_WIDTH = 132% IF VIEW_WIDTH > 80%

	!
	! Create a full window, and paste it on
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(SMG_WINDOW::VVIEW, &
		SMG_WINDOW::HVIEW, SMG_VIEW, SMG$M_BORDER)

	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(20%, 132%, SMG_BLANK)
	SMG_STATUS% = SMG$LABEL_BORDER(SMG_VIEW, " View " + TOP_TITLE$)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_VIEW, NAME.STRING$, 1%, 1%)
	SMG_STATUS% = SMG$DRAW_LINE(SMG_VIEW, 2%, 1%, 2%, SMG_WINDOW::HVIEW)
	SMG_STATUS% = SMG$SET_CURSOR_ABS(SMG_VIEW, 3%, 1%)

	!
	! Create new scrolling description for the VIEW
	!
	SMG_SCROLL::WINDOW	= SMG_VIEW
	SMG_SCROLL::SCROLL_TOP	= 3%
	SMG_SCROLL::SCROLL_BOT	= SMG_WINDOW::VVIEW

	SMG_SCROLL::TOP_ARRAY	= 1%
	SMG_SCROLL::BOT_ARRAY	= VIEW_A_SIZ%
	SMG_SCROLL::CUR_LINE	= 1%
	SMG_SCROLL::CUR_W_COL	= 1%

	SMG_SCROLL::FIND_LINE	= 1%
	SMG_SCROLL::CUR_LINE	= 1%
	SMG_SCROLL::TOP_LINE	= 1%
	SMG_SCROLL::CUR_W_ROW	= 1%
	SMG_SCROLL::BEG_ELEMENT	= 1%

	SMG_SCROLL::END_ELEMENT	= 0%

	SMG_SCROLL::SMG_FLAG	= 6%
	SMG_SCROLL::PROMPT	= "->"
	SMG_SCROLL::NUM_COLM	= 1%

	PASTE_FLAG% = -1%

6050	!
	! Initialize the arrays
	!
	ELEN = 18%
	GOSUB LoadView

6100	V% = DSPL_SCROLLCIR(SMG_SCROLL, VIEW_A$(), 0%, "PAINT")

	IF PASTE_FLAG%
	THEN
		SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
		SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

		IF VIEW_WIDTH <> NEWWIDTH
		THEN
			!
			! Paste on a blank display to hide the width change
			!
			SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_BLANK, &
				SCOPE::SMG_PBID, 1%, 1%)

			SMG_STATUS% = SMG$CHANGE_PBD_CHARACTERISTICS(SCOPE::SMG_PBID, &
				VIEW_WIDTH)
		END IF

		!
		! Paste on display
		!
		SMG_WINDOW::VVPOS = 2% IF SMG_WINDOW::VVPOS = 0%
		SMG_WINDOW::VHPOS = 2% IF SMG_WINDOW::VHPOS = 0%

		SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_VIEW, SCOPE::SMG_PBID, &
			SMG_WINDOW::VVPOS, SMG_WINDOW::VHPOS)

		PASTE_FLAG% = 0%
	END IF

 V_Menu:
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	CALL ENTR_3MESSAGE(SCOPE, "<SELECT>,<Action Keys> or", 8%)

	SELECT SCOPE::SCOPE_EXIT

	!
	! Exit keys
	!
	CASE SMG$K_TRM_CTRLC, SMG$K_TRM_F8, SMG$K_TRM_F10, &
		SMG$K_TRM_CTRLZ, SMG$K_TRM_PF2

		GOTO ExitView	! Exit View

	!
	! Select key
	!
	CASE SMG$K_TRM_SELECT ! Select

		MAINT_FILE_RFA = VIEW_A_RFA(SMG_SCROLL::CUR_LINE)

	!
	! Movement up
	!
	CASE SMG$K_TRM_UP,		! Uparrow		&
		SMG$K_TRM_F18,		! Top			&
		SMG$K_TRM_PREV_SCREEN	! Previous screen

		SMG_SCROLL::SMG_FLAG = SMG_SCROLL::SMG_FLAG AND NOT (4%)

		ELEN = DSPL_SCROLLCIR(SMG_SCROLL, VIEW_A$(), &
			SCOPE::SCOPE_EXIT, "")

		GOTO V_Menu

	!
	! Movement down
	!
	CASE SMG$K_TRM_DOWN,		! Downarrow		&
		SMG$K_TRM_F19,		! Bottom		&
		SMG$K_TRM_NEXT_SCREEN	! Next screen

		SMG_SCROLL::SMG_FLAG = SMG_SCROLL::SMG_FLAG OR 6%

 ViewLoop:
		ELEN = DSPL_SCROLLCIR(SMG_SCROLL, VIEW_A$(), &
			SCOPE::SCOPE_EXIT, "")

		IF ELEN <> 0%
		THEN
			GOSUB LoadView
			GOTO ViewLoop
		END IF

		GOTO V_Menu

	!
	! Good keys
	!
	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO, SMG$K_TRM_F7 ! Good keys

	!
	! Bad keys
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO V_Menu

	END SELECT

 ExitView:
6200	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

	!
	! Deallocate memory for view
	!
	VIEW_A_SIZ% = 0%

	DIM VIEW_A$(VIEW_A_SIZ%)
	DIM RFA VIEW_A_RFA(VIEW_A_SIZ%)

	!
	! Display current record (unless called for a query only)
	!
	IF (QUERY$ = "") OR (SCOPE::SCOPE_EXIT = SMG$K_TRM_SELECT)
	THEN
		WHEN ERROR IN
			GET #SMG_WINDOW::CHAN, RFA MAINT_FILE_RFA, REGARDLESS
		USE
			CONTINUE ErrorTrapB
		END WHEN

		CALL MAIN_PAINTSCREEN(SMG_WINDOW, " ", 0%) &
			IF QUERY$ = ""
	END IF

6250	SMG_STATUS% = SMG$POP_VIRTUAL_DISPLAY(SMG_VIEW, SCOPE::SMG_PBID)

	IF VIEW_WIDTH <> NEWWIDTH
	THEN
		SMG_STATUS% = SMG$CHANGE_PBD_CHARACTERISTICS(SCOPE::SMG_PBID, &
			NEWWIDTH)

		SMG_STATUS% = SMG$POP_VIRTUAL_DISPLAY(SMG_BLANK, &
			SCOPE::SMG_PBID)
	END IF

	RETURN

	%PAGE

 LoadView:
	!*******************************************************************
	! Subroutine used in the VIEW
	!*******************************************************************
	I% = SMG_SCROLL::END_ELEMENT

	SMG_WINDOW::CURREC = 0%

	V% = MAINT_GROUP(SMG_WINDOW, OPT_SUBWIND, 2%, S_ITEM%, &
		RIGHT(QUERY$, 3%)) &
		IF SMG_SCROLL::END_ELEMENT = 0%

	SELECT SMG_WINDOW::CURREC

	CASE -1%
		SMG_WINDOW::CURREC = 0%
		GOTO 6550

	END SELECT

6500	WHEN ERROR IN
		GET #SMG_WINDOW::CHAN, REGARDLESS
	USE
		CONTINUE 6550 IF ERR = 11%
		FILENAME$ = ""
		CONTINUE HelpError
	END WHEN

	!
	! We pass QUERY$ through in case the user program needs it to
	! test against.
	!
	TEXT$ = QUERY$

	V% = MAINT_GROUP(SMG_WINDOW, OPT_SUBWIND, 3%, 0%, RIGHT(QUERY$, 3%))

	IF SMG_WINDOW::CURREC = -1%
	THEN
		SMG_WINDOW::CURREC = 0%
		GOTO 6550
	END IF

	V% = MAINT_GROUP(SMG_WINDOW, OPT_VIEW, 3%, 0%, TEXT$)

	SELECT V%

	!
	! Skip this record
	!
	CASE 1%
		GOTO 6500

	!
	! End this view
	!
	CASE 2%
		GOTO 6550

	END SELECT

	IF I% < SMG_SCROLL::BOT_ARRAY
	THEN
		I% = I% + 1%

		VIEW_A$(I%) = TEXT$ + ""
		VIEW_A_RFA(I%) = GETRFA(SMG_WINDOW::CHAN)

		GOTO 6500 IF I% < SMG_SCROLL::END_ELEMENT + ELEN

		SMG_SCROLL::END_ELEMENT = I%

		RETURN
	END IF

6550	SMG_SCROLL::SMG_FLAG    = SMG_SCROLL::SMG_FLAG AND NOT (4%)
	SMG_SCROLL::END_ELEMENT = I%

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

	WHEN ERROR IN
		INIT_RFA = GETRFA(SMG_WINDOW::CHAN)
	USE
		CONTINUE ErrorTrapA
	END WHEN

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
		LOOP% = ENTR_3NUMBER(SCOPE, SCOPE::SMG_OPTION, "", &
			"Field to Initialize", 0.0, 4%, "##", "")

		SELECT SCOPE::SCOPE_EXIT

		!
		! Exit keys
		!
		CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_F8, SMG$K_TRM_CTRLZ

			RETURN

		END SELECT

		!
		! Split this IF statment into two parts in order to test
		! LOOP% if to big caused problem with HFLAG array.
		!
		GOTO 8010 IF LOOP% < 0% OR LOOP% > SMG_WINDOW::NITEMS
		GOTO 8010 IF (SMG_WINDOW::HFLAG(LOOP%) AND 2%)

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

8020		IF LOOP% <> 0%
		THEN
			INIT.FLAG%, INIT%(LOOP%) = -1%
			GOTO 8010
		END IF

		RETURN IF INIT.FLAG% = 0%
		FIND #SMG_WINDOW::CHAN, RFA INIT_RFA	! Lock record

8050	!
	! Perform the initialization on each record
	!
		WHEN ERROR IN
			GET #SMG_WINDOW::CHAN	! Get the next record
		USE
			CONTINUE 8070 IF ERR = 11%
			CONTINUE NextLock IF ERR = 154%
			FILENAME$ = ""
			CONTINUE HelpError
		END WHEN

		V% = MAINT_GROUP(SMG_WINDOW, OPT_SETOLD, 0%, 0%, "")

		V% = MAINT_GROUP(SMG_WINDOW, OPT_SUBWIND, 3%, 0%, RIGHT(QUERY$, 3%))

		IF SMG_WINDOW::CURREC = -1%
		THEN
			SMG_WINDOW::CURREC = 0%
			GOTO 8070
		END IF

		CALL MAIN_PAINTSCREEN(SMG_WINDOW, " ", 0%)

		!
		! Scan through files initializing all
		!
		FOR LOOP% = 1% TO SMG_WINDOW::NITEMS

8060			GOTO 8065 UNLESS INIT%(LOOP%)

 EnterOne:
			!
			!	Enter one field
			!
			CALL MAINT_GROUP(SMG_WINDOW, OPT_ENTRY, LOOP%, 8%, "")

			SELECT SCOPE::SCOPE_EXIT

			!
			! Exit keys
			!
			CASE 3%, SMG$K_TRM_F8, SMG$K_TRM_F10, &
				SMG$K_TRM_CTRLZ, SMG$K_TRM_TIMEOUT

				UNLOCK	#SMG_WINDOW::CHAN

				CALL HELP_34MESSAGE(SCOPE, "initialize aborted", &
					"W", "MAIN_WINDOW", "", "ABORTINI")

	!++
	! Warning:ABORTINI
	!	^*Initialize Aborted\*
	!	.p
	!	^*Explanation\*
	!	.p
	!	Initialize has been aborted before finding the last record
	!	in a file.
	!	.p
	!	^*User Action\*
	!	.p
	!	None.
	!
	! Index:
	!	.x Initialize Aborted
	!--
				RETURN

			END SELECT

			!
			! Test special cases for each field if nessary
			!
			GOTO EnterOne &
				IF MAINT_GROUP(SMG_WINDOW, OPT_TESTENTRY, &
				LOOP%, 0%, "NOTADD")

			SELECT SCOPE::SCOPE_EXIT

			!
			! Premature exit
			!
			CASE SMG$K_TRM_PF2

				IF MAINT_GROUP(SMG_WINDOW, OPT_TESTOPT, &
					0%, 0%, "INITIALIZE") = 0%
				THEN
					UPDATE	#SMG_WINDOW::CHAN
 !					V% = MAINT_GROUP(SMG_WINDOW, OPT_LOG, 0%, 0%, TRM$(SCOPE::PRG_ITEM))
					V% = MAINT_GROUP(SMG_WINDOW, OPT_AFTEROPT, 0%, 0%, TRM$(SCOPE::PRG_ITEM))
				END IF

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
				GOTO 8060

			END SELECT

8065		NEXT LOOP%

		IF MAINT_GROUP(SMG_WINDOW, OPT_TESTOPT, 0%, 0%, &
			"INITIALIZE") = 0%
		THEN
			WHEN ERROR IN
				UPDATE	#SMG_WINDOW::CHAN
			USE
				CONTINUE KeyNoChange IF ERR = 130%
				FILENAME$ = ""
				CONTINUE HelpError
			END WHEN

 !			V% = MAINT_GROUP(SMG_WINDOW, OPT_LOG, 0%, 0%, &
 !				TRM$(SCOPE::PRG_ITEM))
			V% = MAINT_GROUP(SMG_WINDOW, OPT_AFTEROPT, 0%, 0%, &
				TRM$(SCOPE::PRG_ITEM))
		END IF

	GOTO 8050

 KeyNoChange:
	CALL HELP_34MESSAGE(SCOPE, "cannot initialize the key field", "W", &
		SMG_WINDOW::NHELP, "", "130")

	GOTO 8050

8070	UNLOCK	#SMG_WINDOW::CHAN

	!CALL ENTR_3MESSAGE(SCOPE, "Initialize has finished.", 0%)

	RETURN

 NextLock:
	CALL HELP_34MESSAGE(SCOPE, "next record is locked", "W", &
		SMG_WINDOW::NHELP, "", "154")

	RETURN

	%PAGE

 Default:
9000	!*******************************************************************
	! Set defaults
	!*******************************************************************

	!
	! Handle defaults
	!
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

	RETURN

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
	! Change all lines (must have old record in map) to new
	! header (must have new key in QUERY$ or in another easily
	! accessable position).
	!
	CASE "C"
		GOSUB QueryChange

	!
	! Erase all record matching this header
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
			MAIN_WINDOW = ERR * 100%
			CONTINUE 11900
		END WHEN

		MAIN_WINDOW = 1%

11015	CASE "R"
		SMG_STATUS% = MAINT_GROUP(SMG_WINDOW, OPT_SUBWIND, 1%, 0%, RIGHT(QUERY$, 3%))

		!
		! Find all records under this header
		!
		WHILE SMG_WINDOW::CURREC = 0%

			V% = MAINT_GROUP(SMG_WINDOW, OPT_SETOLD, 0%, 0%, "")
			V% = MAINT_GROUP(SMG_WINDOW, OPT_AFTEROPT, 0%, 0%, "Change")

			WHEN ERROR IN
				GET #SMG_WINDOW::CHAN
			USE
				CONTINUE 11900
			END WHEN

			V% = MAINT_GROUP(SMG_WINDOW, OPT_SUBWIND, 3%, 0%, RIGHT(QUERY$, 3%))
		NEXT

		SMG_WINDOW::CURREC = 0%

	!
	! The View option will allow a view to pop up
	! on the screen, where the user can select a record.
	!
11020	CASE "V"
		!
		! Het help to "View"
		!
		SCOPE::PRG_ITEM = "View"
		SMG_STATUS% = SMG$CHANGE_PBD_CHARACTERISTICS(SCOPE::SMG_PBID, , NEWWIDTH)

		IF MID(QUERY$, 2%, 1%) = "X"
		THEN
			GOSUB Viewr
		ELSE
			KEY_NUMBER% = VAL%(MID(QUERY$, 2%, 1%))

			!
			! Get starting record
			!
			WHEN ERROR IN
				IF LEN(QUERY$) < 3%
				THEN
					RESET #SMG_WINDOW::CHAN, &
						KEY #KEY_NUMBER%
				ELSE
					FIND #SMG_WINDOW::CHAN, &
						KEY #KEY_NUMBER% GE RIGHT(QUERY$, 3%), &
						REGARDLESS
				END IF
			USE
				MAIN_WINDOW = ERR * 100%
				CONTINUE 11900
			END WHEN

			!
			! Go into view
			!
			S_ITEM% = KEY_NUMBER%
			GOSUB ViewBypass
		END IF

		!
		! Succussful if the user typed select (1),
		! error if they didn't (2)
		!
		IF SCOPE::SCOPE_EXIT = SMG$K_TRM_SELECT
		THEN
			MAIN_WINDOW = 1%
		ELSE
			MAIN_WINDOW = 2%
		END IF

	END SELECT

11900	RETURN

 QueryErase:
12000	!*******************************************************************
	! Erase all record matching the current header
	!*******************************************************************

	V% = MAINT_GROUP(SMG_WINDOW, OPT_SETOLD, 0%, 0%, "")

	!
	! Find all records under this header
	!
	SMG_STATUS% = MAINT_GROUP(SMG_WINDOW, OPT_SUBWIND, 1%, 0%, &
		RIGHT(QUERY$, 3%))

	WHILE SMG_WINDOW::CURREC = 0%
		!
		! And delete it
		!
		DELETE #SMG_WINDOW::CHAN
		V% = MAINT_GROUP(SMG_WINDOW, OPT_AFTEROPT, 0%, 0%, "Erase")
		V% = MAINT_GROUP(SMG_WINDOW, OPT_SUBWIND, 1%, 0%, "Erase")
	NEXT

	SMG_WINDOW::CURREC = 0%

	RETURN

	%PAGE

 QueryChange:
12500	!*******************************************************************
	! Change all records matching the original header to the new
	! header.
	!*******************************************************************

	!
	! Display please-wait message
	!
	CALL ENTR_3MESSAGE(SCOPE, "", 1% + 16%)

	SMG_STATUS% = MAINT_GROUP(SMG_WINDOW, OPT_SUBWIND, 1%, 0%, &
		RIGHT(QUERY$, 3%))

	!
	! Find all records under this header
	!
	WHILE SMG_WINDOW::CURREC = 0%

		V% = MAINT_GROUP(SMG_WINDOW, OPT_SETOLD, 0%, 0%, "")

		!
		! Change the key
		!
		SMG_STATUS% = MAINT_GROUP(SMG_WINDOW, OPT_SUBWIND, 6%, 0%, &
			RIGHT(QUERY$, 3%))

		!
		! And update it
		!
		DELETE #SMG_WINDOW::CHAN
		PUT #SMG_WINDOW::CHAN
		SMG_STATUS% = MAINT_GROUP(SMG_WINDOW, OPT_SUBWIND, &
			1%, 0%, RIGHT(QUERY$, 3%))
		V% = MAINT_GROUP(SMG_WINDOW, OPT_AFTEROPT, 0%, 0%, "Change")
	NEXT

	SMG_WINDOW::CURREC = 0%

	!
	! Remove please wait message
	!
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

	RETURN

	%PAGE

 ExitProgram:
15000	!*******************************************************************
	! Exit program
	!*******************************************************************

	!
	! Erase displays
	!
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)

	!
	! Remove created display if not in query mode
	!
	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_WINDOW::WNUMBER)

	IF SMG_WINDOW::LCURR <> 0%
	THEN
		!
		! Change current page number
		!
		SMG_WINDOW::LCURR = 0%

		!
		! Delete the page
		!
		SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_WINDOW::LWINDOW)
	END IF

	IF NEWWIDTH <> C_WIDTH
	THEN
		!
		! Change the width
		!
		SMG_STATUS% = SMG$CHANGE_PBD_CHARACTERISTICS(SCOPE::SMG_PBID, &
			C_WIDTH)

		SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_BLANK1)
	END IF

 QueryExit:
	SCOPE::PRG_IDENT = KEEP_IDENT$
	SCOPE::PRG_PROGRAM = KEEP_PRG$
	SCOPE::PRG_ITEM = KEEP_ITEM$

	EXIT FUNCTION

	%PAGE

19000	!*******************************************************************
	! Error trapping
	!*******************************************************************

	IF IN_VIEW%
	THEN
		RESUME 6020
	END IF

	!===================================================================
	! Error LINE(ERL) cases
	!===================================================================

	RESUME NoFind IF (ERN$ <> "MAIN_WINDOW") AND (ERR = 155%)

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME 19990

 HelpError:
19990	!
	! This moved from inside error to outside so that errors occuring
	! at lower levels could be trapped.  Basic will not allow any
	! error to occur inside of an error no matter if it is in a
	! different module.
	!
	V1% = SMG$END_PASTEBOARD_UPDATE(SCOPE::SMG_PBID)

	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR) + &
		" (*" + NUM1$(WINDOW%) + ")", "E", ERN$, FILENAME$, NUM1$(ERR))

	GOTO ExitProgram

 ErrorTrapA:
	V1% = SMG$END_PASTEBOARD_UPDATE(SCOPE::SMG_PBID)

	V% = MAINT_GROUP(SMG_WINDOW, OPT_BACKGROUND, 0%, 0%, "")
	MAINT_FILE_RFA = NULL_RFA
	GOTO Menu

 ErrorTrapB:
	V1% = SMG$END_PASTEBOARD_UPDATE(SCOPE::SMG_PBID)
	V% = MAINT_GROUP(SMG_WINDOW, OPT_BACKGROUND, 0%, 0%, "")
	MAINT_FILE_RFA = NULL_RFA
	GOTO 6250

32767	END FUNCTION
	!+-+-+
	!++
	! Warning:DUPLKEY
	!	^*Duplicate Key\*
	!	.b
	!	.lm +5
	!	Record already exists in the file with the same key.
	!	.b
	!	^*User Action:\*
	!	.b
	!	Change key or abort adding (changing).
	!	.lm -5
	!
	! Index:
	!
	!--
