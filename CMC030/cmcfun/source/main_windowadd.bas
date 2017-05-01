1	%TITLE "Simple Add Function"
	%SBTTL "MAIN_WINDOWADD"
	%IDENT "V3.6a Calico"

	FUNCTION INTEGER MAIN_WINDOWADD(CDD_WINDOW_CDD SMG_WINDOW, STRING QUERY)

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
	!	Processes the entry of a record into a file as in the
	!	Maintainence Programs.
	!
	!	Special function keys available are:
	!	.b
	!	.list 0,"*"
	!	.le
	!		PF2 - Exit add, and add record.
	!	.le
	!		PF3, Cancel - Restart add, Don't add record.
	!	.le
	!		Exit - Don't add record, go back to menu.
	!	.le
	!		Uparrow - Go back up one field.
	!	.le
	!		Downarrow - Go down one field.
	!	.le
	!		Return, do - Go down one field.
	!	.end LIST
	!
	! Index:
	!	.x Maintenance>Add
	!	.x Add>Maintenance
	!	.x Add>Window
	!	.x Window>Add
	!
	! Parameters:
	!
	!	CDD_WINDOW_CDD SMG_WINDOW
	!		The passed CDD record name describing the fields
	!		being added.
	!
	!
	!	Returns integer:
	!	.table
	!		0% for normal exit - add the record to the file.
	!
	!		2% bad input - repaint the screen.
	!
	!		4% bad input - recall record and repaint the screen.
	!	.endtable
	!
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:MAIN_WINDOWADD/NOLINE
	!	$ LIB FUNC_LIB:CMCFUN/REP MAIN_WINDOWADD
	!	$ DELETE MAIN_WINDOWADD.OBJ;*
	!
	! Author:
	!
	!	01/28/87 - Kevin Handy
	!
	! Modification history:
	!
	!	04/23/87 - Kevin Handy
	!		Fixed so that arrow keys work better
	!
	!	08/03/87 - Kevin Handy
	!		Modified handling of PF2, PF3, and Cancel
	!
	!	07/07/88 - Kevin Handy
	!		Modified so up-arrow will move up even if
	!		the field has junk in it that would fail.
	!
	!	10/19/90 - Frank F. Starman
	!		Replace ENTR_3MESSAGE with HELP_34MESSAGE function.
	!		Use EXIT_STATUS% variable. Have just one exit from
	!		the function.
	!
	!	03/13/91 - Frank F. Starman
	!		Set conditions for HFLAG.
	!
	!	09/13/91 - Frank F. Starman
	!		Add QUERY argument.
	!
	!	02/24/92 - Kevin Handy
	!		Cleaned up (check)
	!
	!	04/21/92 - Frank F. Starman
	!		Call OPT_AFTEROPT if add is aborted
	!
	!	01/08/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/31/93 - Frank F. Starman
	!		Set MVALUE to ADD if loop on OPT_ENTRY
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	04/30/96 - Kevin Handy
	!		Reformat source code.
	!
	!	04/30/96 - Kevin Handy
	!		Modified to handle case 255 (Special skip)
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standard
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

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	!
	! External declarations
	!
	EXTERNAL INTEGER FUNCTION MAINT_GROUP

	!
	! Assume no problems
	!
	EXIT_STATUS% = 0%

	%PAGE

	!
	! Set defaults and clear screen
	!
	CALL MAIN_PAINTSCREEN(SMG_WINDOW, " ", 0%)

	LOOP% = 1%

 AddField:
	GOTO ExitFunction IF LOOP% > SMG_WINDOW::NITEMS

	IF LOOP% <= 64%
	THEN
		GOTO TestEntry IF (SMG_WINDOW::HFLAG(LOOP%) AND 1%)
	END IF

 EnterField:
	!
	!	ENTER ONE FIELD
	!
	FLAG% = 16384%
	CALL MAIN_PAINTSCREEN(SMG_WINDOW, "C", LOOP%)
	V% = MAINT_GROUP(SMG_WINDOW, OPT_ENTRY, LOOP%, FLAG%, "ADD")

	SELECT SCOPE::SCOPE_EXIT
	!
	! Control C, Exit
	!
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, &
		SMG$K_TRM_F8, SMG$K_TRM_PF3

		CALL HELP_34MESSAGE(SCOPE, "add aborted", &
			"W", "MAIN_WINDOWADD", "", "NOADD") &
			IF QUERY <> "A"

	!++
	! Warning:NOADD
	!	^*Record not Added\*
	!	.b
	!	.lm +5
	!	^*Explanation\*
	!	.b
	!	No record has been added into the file.
	!	.b
	!	^*User Action\*
	!	.b
	!	Press the Return key instead of the Exit key on each field to be bypassed.
	!	.lm -5
	!
	! Index:
	!	.x Add
	!
	!--

		V% = MAINT_GROUP(SMG_WINDOW, OPT_AFTEROPT, 1%, 0%, "Erase") &
			IF SMG_WINDOW::LLAST <> 0% AND &
			LOOP% > SMG_WINDOW::LPAGE(0%)

		EXIT_STATUS% = 4%
		GOTO ExitFunction

	!
	! Uparrow
	!
	CASE SMG$K_TRM_UP
		!
		! Always back up one
		!
		LOOP% = LOOP% - 1% IF LOOP% > 1%

		!
		! Keep backing up past defaulted (hard)
		!
		LOOP% = LOOP% - 1% &
			WHILE (LOOP% > 1%) AND &
			(SMG_WINDOW::HFLAG(LOOP%) AND 1%)

		GOTO AddField

	!
	! good keys, continue
	!
	CASE SMG$K_TRM_PF2, SMG$K_TRM_DOWN, &
		0%, 10%, 12%, 13%, SMG$K_TRM_DO, 255%

	!
	! Bad keys
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO EnterField

	END SELECT

	SCOPE1.EXIT% = SCOPE::SCOPE_EXIT

 TestEntry:
	!
	! Test special cases for each field if nessary
	!
	SELECT MAINT_GROUP(SMG_WINDOW, OPT_TESTENTRY, LOOP%, 0%, "ADD")

	!
	! A 1 means that it is correctable with retry
	!
	CASE 1%
		GOTO EnterField

	!
	! A 2 means that the add MUST be aborted.
	! (Used mostly for exit keys typed in the
	! input, during tests, etc.
	!
	CASE 2%
		EXIT_STATUS% = 2%
		GOTO ExitFunction
	END SELECT

	SELECT SCOPE1.EXIT%

	!
	! Premature exit
	!
	CASE SMG$K_TRM_PF2
		!
		! Check out rest of fields
		!
		FOR I% = LOOP% + 1% TO SMG_WINDOW::NITEMS
			!
			! Test special cases for each field
			! if nessary
			!
			SELECT MAINT_GROUP(SMG_WINDOW, &
				OPT_TESTENTRY, LOOP%, 0%, "ADD")

			!
			! A 1 means that it is
			! correctable with retry
			!
			CASE 1%
				CALL ENTR_3MESSAGE(SCOPE, &
					"Exit not allowed because of field " + &
					NUM1$(I%), 0%)
				GOTO EnterField

			!
			! A 2 means that the add MUST be aborted.
			! (Used mostly for exit keys typed in the
			! input, during tests, etc.
			!
			CASE 2%
				EXIT_STATUS% = 2%
				GOTO ExitFunction
			END SELECT

		NEXT I%

		GOTO ExitFunction

	!
	! Downarrow
	!
	CASE SMG$K_TRM_DOWN
		!
		! Always advance by one
		!
		LOOP% = LOOP% + 1% &
			IF LOOP% < SMG_WINDOW::NITEMS

		!
		! Keep advancing past defaulted (hard)
		!
		LOOP% = LOOP% + 1% &
			WHILE (LOOP% < SMG_WINDOW::NITEMS) AND &
			(SMG_WINDOW::HFLAG(LOOP%) AND 1%)

		!
		! Back up past defaulted (hard) so won't
		! zip out.
		!
		LOOP% = LOOP% - 1% &
			WHILE (LOOP% > 1%) AND &
			(SMG_WINDOW::HFLAG(LOOP%) AND 1%)

		GOTO AddField

	!
	! Good keys
	!
	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO, 255%

	!
	! Bad keys
	!
	CASE ELSE
		SCOPE::SCOPE_EXIT = SCOPE1.EXIT%
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO EnterField
	END SELECT

 NextField:
	LOOP% = LOOP% + 1%
	GOTO AddField

 ExitFunction:
	!
	! Exit from the function
	!
	MAIN_WINDOWADD = EXIT_STATUS%

	END FUNCTION
