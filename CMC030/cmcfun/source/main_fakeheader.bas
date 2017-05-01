1	%TITLE "Window Maintenance"
	%SBTTL "MAIN_FAKEHEADER"
	%IDENT "V3.6a Calico"

	FUNCTION LONG MAIN_FAKEHEADER(WINDOW%, QUERY$)

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
	!	This maintenance module calls the users screen through
	!	a user written function called MAINT_GROUP, using
	!	parameters defined in <MAIN_FAKEHEADER.COM>.
	!
	! Index:
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
	!			'Q' - Query
	!			'V' - View with select
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
	! Structure MAIN_FAKEHEADER fields used:
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
	!	HFLAG(1..NITEMS)
	!		Hard/soft flags for the fields.
	!
	!	FLAGS
	!	.table
	!		  1 - Enable window option to appear on screen.
	!		  2 - Read only
	!		  4 - Edit existing fields only.  Add, key change
	!		      not allowed.
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
	!	$ BAS FUNC_SOURCE:MAIN_FAKEHEADER/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP MAIN_FAKEHEADER
	!	$ DELETE MAIN_FAKEHEADER.OBJ;*
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
	!		Modified so that errors in the error help
	!		coule be trapped.
	!
	!	09/10/87 - Kevin Handy
	!		Fixed problem in view where down-arrow would
	!		keep going down forever.
	!
	!	10/12/87 - Kevin Handy
	!		Modified to use "ViewList" in view for help.
	!
	!	03/26/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	11/01/95 - Kevin Handy
	!		Clean up source code.
	!		Lose extra return followed by a return.
	!
	!	09/03/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/08/99 - Kevin Handy
	!		Lose 2200, which is unreachable
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

	%INCLUDE "SOURCE:[SMG.OPEN]SMG_SCROLL.HB"
	DECLARE	SMG_SCROLL_CDD	SMG_SCROLL

	!
	! External declarations
	!
	EXTERNAL LONG    FUNCTION DSPL_SCROLLCIR
	EXTERNAL INTEGER FUNCTION MAINT_GROUP
	EXTERNAL INTEGER FUNCTION MAIN_JOURADD
	EXTERNAL INTEGER FUNCTION MAIN_JOURCHANGE
	EXTERNAL INTEGER FUNCTION MAIN_JOURBLANK

	!
	! Declare functions
	!
	DECLARE LONG SMG_VIEW, SMG_BLANK, SMG_BLANK1, &
		VIEW_WIDTH, ELEN, &
		C_WIDTH, NEWWIDTH

	%PAGE

	!
	! Initilization section	Prepare to do anything
	!
	ON ERROR GOTO 19000

	KEEP_IDENT$ = SCOPE::PRG_IDENT + ""
	KEEP_PRG$ = SCOPE::PRG_PROGRAM + ""
	KEEP_ITEM$ = SCOPE::PRG_ITEM + ""

	!
	! Assume thins will work out OK
	!
	MAIN_FAKEHEADER = 1%

	!
	! Call up initilization
	!
	SMG_WINDOW::IDENT = WINDOW%
	V% = MAINT_GROUP(SMG_WINDOW, OPT_INIT, 0%, 0%, "")
	SCOPE::PRG_PROGRAM = SMG_WINDOW::NHELP

	!
	! Set up title string
	!
	TOP_TITLE$ = " " + EDIT$(SMG_WINDOW::DESCR, 4% + 8% + 128%) + " "
	TEMP1$ = EDIT$(SCOPE::PRG_COMPANY, 4% + 8% + 128%)
	TOP_TITLE$ = TOP_TITLE$ + "for " + TEMP1$ + " " IF TEMP1$ <> ""

	!
	! Handle QUERY$ specially
	!
	IF QUERY$ <> ""
	THEN
		GOSUB QueryTest
		GOTO QueryExit
	END IF

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
	! Save current record
	!
	V% = MAINT_GROUP(SMG_WINDOW, OPT_SETOLD, 0%, 0%, "")

	GOSUB PaintScreen

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
		OPLIST$ = "Find Next Restore View Help eXit"
	ELSE
		!
		! Edit existing fields only
		!
		IF (SMG_WINDOW::FLAGS AND 4%)
		THEN
			OPLIST$ = "Change Blank " + &
				"Find Next Restore View Help eXit"
		ELSE
			!
			! Full featured
			!
			OPLIST$ = "Add Erase Change Blank " + &
				"Default Find Next Restore View Help eXit"
		END IF
	END IF

	OPLIST$ = OPLIST$ + " Window" IF (SMG_WINDOW::FLAGS AND 1%)

	!
	! Let the user program change the options if necessary
	!
	V% = MAINT_GROUP(SMG_WINDOW, OPT_OPTLIST, 0%, 0%, OPLIST$)

	GOTO Menu

	%PAGE

1000	!*******************************************************************
	! Program restart point
	!*******************************************************************

 MenuCurrent:
1020	!
	! Display current record
	!
	V% = MAINT_GROUP(SMG_WINDOW, OPT_RESETOLD, 0%, 0%, "")
	GOSUB PaintScreen

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
	! Select key
	!
	CASE SMG$K_TRM_SELECT

		GOTO ExitProgram

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
1120		GOSUB 3100	! Print current record
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
1130		V% = MAINT_GROUP(SMG_WINDOW, OPT_ARRAY, 10%, 0%, "")
		V% = MAINT_GROUP(SMG_WINDOW, OPT_SETOLD, 0%, 0%, "")
		GOTO MenuCurrent

	!
	!
	! This option allows records to be viewed
	!
	CASE "View"
		GOSUB Viewr
		GOTO MenuCurrent

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
		CALL HELP_3MESSAGE(SCOPE, "", "PROG", SCOPE::PRG_PROGRAM, "HELP")
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
	V% = MAINT_GROUP(SMG_WINDOW, OPT_RESETDEFAULT, 0%, 0%, "")

2110	SELECT MAIN_JOURADD(SMG_WINDOW)

	CASE 0%
2120		!
		! Add record to file
		!
		IF MAINT_GROUP(SMG_WINDOW, OPT_TESTOPT, 0%, 0%, "ADD") = 0%
		THEN
			V% = MAINT_GROUP(SMG_WINDOW, OPT_SETOLD, 0%, 0%, "")
			V% = MAINT_GROUP(SMG_WINDOW, OPT_AFTEROPT, 0%, 0%, "Add")
		END IF

	CASE 2%
		!
		! Bad input, Repaint Screen
		!
		V% = MAINT_GROUP(SMG_WINDOW, OPT_SETOLD, 0%, 0%, "")

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

	RETURN

 !2200
	!
	! Handle duplicate key detected
	!
 !CALL ENTR_3MESSAGE(SCOPE, "Duplicate key detected Change key or <Cancel>", 0%)
 !
 !	GOTO 2110

	%PAGE

 Eraser:
3000	!*******************************************************************
	! Erase a record
	!
	! This option will erase the current record
	!
	!*******************************************************************

	!
	! Make sure they want to delete that record
	!
	INP$ = ENTR_3YESNO(SCOPE,  SMG_WINDOW::WNUMBER, "", &
		"Confirm deletion - then press <DO>", "N", 0%, "!", "")

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
	! Delete the record
	!
	V% = MAINT_GROUP(SMG_WINDOW, OPT_AFTEROPT, 0%, 0%, "Erase")

	GOSUB 3100

3025	!
	! Normal exit from erase
	!
	RETURN

	%PAGE

3100	!*******************************************************************
	! Get the next record and print it
	!*******************************************************************

	!
	! First try, get the next record
	!
	V% = MAINT_GROUP(SMG_WINDOW, OPT_ARRAY, 11%, 0%, "")
	V% = MAINT_GROUP(SMG_WINDOW, OPT_SETOLD, 0%, 0%, "")
	GOSUB PaintScreen

3110	RETURN

	%PAGE

 Changer:
4000	!*******************************************************************
	! Change/Blank record
	!
	! This option allows an existing record to be changed or blanked.
	!
	!*******************************************************************

	V% = MAINT_GROUP(SMG_WINDOW, OPT_SETOLD, 0%, 0%, SCOPE::PRG_ITEM)

	SELECT OPT$

	CASE "C"
		STAT% = MAIN_JOURCHANGE(SMG_WINDOW, 0%)

	CASE "B"
		STAT% = MAIN_JOURBLANK(SMG_WINDOW)

	END SELECT

	SELECT STAT%

4010	!
	! Good change
	!
	CASE 0%
		V% = MAINT_GROUP(SMG_WINDOW, OPT_AFTEROPT, 0%, 0%, &
			TRM$(SCOPE::PRG_ITEM))
		V% = MAINT_GROUP(SMG_WINDOW, OPT_SETOLD, 0%, 0%, &
			SCOPE::PRG_ITEM)

	!
	! Bad change, recall original
	!
	CASE ELSE
		CALL ENTR_3MESSAGE(SCOPE, &
			TRM$(SCOPE::PRG_ITEM) + " aborted", 1%)

	END SELECT

4015	!
	! Normal exit from Change/Blank
	!
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
	SCOPE::PRG_ITEM = "VIEWBY"
	SORT_BY$ = ENTR_3OPTION(SCOPE, "Search by", S_LIST$ + " eXit", SORT_BY%, 8%)

	SELECT SCOPE::SCOPE_EXIT
	!
	! Exit keys
	!
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_F8, SMG$K_TRM_CTRLZ

		CALL ENTR_3MESSAGE(SCOPE, "Find aborted", 1%)
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

		CALL MAINT_GROUP(SMG_WINDOW, OPT_ENTRY, LOOP%, 0%, "")

		SELECT SCOPE::SCOPE_EXIT
		!
		! Exit keys
		!
		CASE 3%, SMG$K_TRM_F8, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ

			V% = MAINT_GROUP(SMG_WINDOW, OPT_BACKGROUND, 0%, 0%, "")
			CALL ENTR_3MESSAGE(SCOPE, "Find aborted", 1%)
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
	V% = MAINT_GROUP(SMG_WINDOW, OPT_SETOLD, 0%, 0%, "")

5050	!
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
		S_LIST$ + " eXit", SORT_BY%, 8%)

	SELECT SCOPE::SCOPE_EXIT
	!
	! Exit keys
	!
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_F8, SMG$K_TRM_CTRLZ
		CALL ENTR_3MESSAGE(SCOPE, "View aborted", 1%)
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

	FOR Q% = 1% TO SMG_WINDOW::KFIELD(S_ITEM%, 0%)

6030		LOOP% = SMG_WINDOW::KFIELD(S_ITEM%, Q%)

		CALL MAINT_GROUP(SMG_WINDOW, OPT_ENTRY, LOOP%, 0%, "")

		SELECT SCOPE::SCOPE_EXIT
		!
		! Exit keys
		!
		CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_F8, SMG$K_TRM_CTRLZ
			V% = MAINT_GROUP(SMG_WINDOW, OPT_BACKGROUND, 0%, 0%, "")
			CALL ENTR_3MESSAGE(SCOPE, "View aborted", 1%)
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
	V% = MAINT_GROUP(SMG_WINDOW, OPT_FIND, S_ITEM%, 0%, "")

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
		SMG_WINDOW::HVIEW, SMG_VIEW)
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
	SMG_SCROLL::CUR_LINE, SMG_SCROLL::CUR_W_COL	= 1%
	SMG_SCROLL::FIND_LINE, SMG_SCROLL::CUR_LINE, SMG_SCROLL::TOP_LINE, &
		SMG_SCROLL::CUR_W_ROW, SMG_SCROLL::BEG_ELEMENT	= 1%
	SMG_SCROLL::END_ELEMENT	= 0%

	SMG_SCROLL::SMG_FLAG	= 6%
	PROMPT$, SMG_SCROLL::PROMPT	= "->"
	SMG_SCROLL::NUM_COLM	= 1%

	PASTE_FLAG% = -1%

	TEMP% = 0%

6050	!
	! Initialize the arrays
	!
	ELEN = 18%
	QQ_FLAG% = 0%
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
		SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_VIEW, SCOPE::SMG_PBID, &
			SMG_WINDOW::VPOS, SMG_WINDOW::HPOS)
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

		IF (ELEN <> 0%)
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

	!
	! Display current record (unless called for a query only)
	!
	IF (QUERY$ = "") OR (SCOPE::SCOPE_EXIT = SMG$K_TRM_SELECT)
	THEN
		V% = MAINT_GROUP(SMG_WINDOW, OPT_RESETOLD, 0%, 0%, "")
		GOSUB PaintScreen IF QUERY$ = ""
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

	IF QQ_FLAG%
	THEN
		SMG_SCROLL::SMG_FLAG    = SMG_SCROLL::SMG_FLAG AND NOT (4%)
		GOTO 6550
	END IF

	I% = SMG_SCROLL::END_ELEMENT

6500	V% = MAINT_GROUP(SMG_WINDOW, OPT_VIEW, 3%, 0%, TEXT$)

	IF I% < SMG_SCROLL::BOT_ARRAY
	THEN
		I% = I% + 1%

		VIEW_A$(I%) = TEXT$ + ""

		GOTO 6550 IF MAINT_GROUP(SMG_WINDOW, OPT_ARRAY, 11%, 0%, "")

		GOTO 6500 IF I% < SMG_SCROLL::END_ELEMENT + ELEN

		SMG_SCROLL::END_ELEMENT = I%

		RETURN
	END IF

6550	QQ_FLAG% = -1%
	SMG_SCROLL::END_ELEMENT = I%

	RETURN

	%PAGE

 Default:
9000	!*******************************************************************
	! Set defaults
	!*******************************************************************

	!
	! Put defaults into main area
	!
	V% = MAINT_GROUP(SMG_WINDOW, OPT_RESETDEFAULT, 0%, 0%, "")

	SELECT MAIN_JOURCHANGE(SMG_WINDOW, 0%)

	!
	! Change is to be made
	!
	CASE 0%
		V% = MAINT_GROUP(SMG_WINDOW, OPT_SETDEFAULT, 0%, 0%, "")
		CALL ENTR_3MESSAGE(SCOPE, "Defaults changed!", 1%)
		RETURN

	!
	! Bad change
	!
	CASE ELSE
		CALL ENTR_3MESSAGE(SCOPE, "Defaults not changed!", 1%)
		RETURN

	END SELECT

	%PAGE

 PaintScreen:
	!*******************************************************************
	! Paint all information onto the screen for the current record.
	!*******************************************************************

	TEMP% = SCOPE::SCOPE_EXIT
	SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

	!
	! Go through all fields
	!
	FOR I% = 1% TO SMG_WINDOW::NITEMS
		V% = MAINT_GROUP(SMG_WINDOW, OPT_ENTRY, I%, 1%, "")
	NEXT I%

	!
	! Handle anything extra
	!
	V% = MAINT_GROUP(SMG_WINDOW, OPT_DISPLAY, 0%, 0%, "") &

	SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)
	SCOPE::SCOPE_EXIT = TEMP%
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
	! The View option will allow a view to pop up
	! on the screen, where the user can select a record.
	!
11020	CASE "V"
		SMG_STATUS% = SMG$CHANGE_PBD_CHARACTERISTICS(SCOPE::SMG_PBID, , NEWWIDTH)

		GOSUB Viewr

		!
		! Succussful if the user typed select (1),
		! error if they didn't (2)
		!
		IF SCOPE::SCOPE_EXIT = SMG$K_TRM_SELECT
		THEN
			MAIN_FAKEHEADER = 1%
		ELSE
			MAIN_FAKEHEADER = 2%
		END IF
	END SELECT

11900	RETURN

	%PAGE

 ExitProgram:
15000	!*******************************************************************
	! Exit program
	!*******************************************************************

	!
	! Erase displays
	!
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

	!
	! Remove created display if not in query mode
	!
	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_WINDOW::WNUMBER)

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

	IF  (ERN$ <> "MAIN_FAKEHEADER") AND (ERR = 155%)
	THEN
		RESUME 6550 IF SCOPE::PRG_ITEM = "View"
		CALL ENTR_3MESSAGE(SCOPE, "Record not found! " + NUM1$(ERR), 1%)
		RESUME 3110
	END IF

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

 HelpError:
19990	!
	! This section moved out of the error trapping so that errors
	! inside of the help call could be trapped.
	!
	CALL HELP_3MESSAGE(SCOPE, ERN$ + " " + NUM1$(ERL) + " " + ERT$(ERR), &
		"ERR", ERN$, "ERROR" + NUM1$(ERR))

	GOTO ExitProgram

32767	END FUNCTION
