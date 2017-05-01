1	%TITLE "Simple Add Function"
	%SBTTL "MAIN_JOURADD"
	%IDENT "V3.6a Calico"

	FUNCTION INTEGER MAIN_JOURADD(CDD_WINDOW_CDD SMG_WINDOW)

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
	!	Maintenance Programs.
	!	Special function keys available are:
	!	.b
	!	.list 0,"*"
	!	.le
	!	PF2 - Exit add, and add record.
	!	.le
	!	PF3, Cancel - Restart add, Don't add record.
	!	.le
	!	Exit - Don't add record, go back to menu.
	!	.le
	!	Uparrow - Go back up one field.
	!	.le
	!	Downarrow - Go down one field.
	!	.le
	!	Return, do - Go down one field.
	!	.end list
	!
	! Index:
	!	.x Maintainence>Journal>Add
	!	.x Add>Journal
	!	.x Journal>Add
	!
	! Parameters:
	!
	!	CDD_WINDOW_CDD SMG_WINDOW
	!		The passed CDD record name describing the fields being added.
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
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:MAIN_JOURADD/NOLINE
	!	$ LIB FUNC_LIB:CMCFUN/REP MAIN_JOURADD
	!	$ DELETE MAIN_JOURADD.OBJ;*
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
	!		Modified to allow up-arrow to work even if
	!		the field contains garbage.
	!
	!	09/30/92 - Kevin Handy
	!		Modified so that it will go through the
	!		OP_TESTENTRY even if there is a hard default
	!		on the field.  This is so that fields that
	!		are forced on in the test will be set.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/17/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
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

	%PAGE

 Addr:
2000	!*******************************************************************
	! ADD NEW RECORD
	!*******************************************************************

	!
	! Set defaults and clear screen
	!
	GOSUB PaintScreen	! Print current record

	LOOP% = 1%

2010	GOTO 2120 IF LOOP% > SMG_WINDOW::NITEMS

	IF (SMG_WINDOW::HFLAG(LOOP%) AND 1%)
	THEN
		!
		! Test special cases for each field if nessary
		!
		SELECT MAINT_GROUP(SMG_WINDOW, OPT_TESTENTRY, LOOP%, 0%, "ADD")

		!
		! A 1 means that it is correctable with retry
		!
		CASE 1%
			GOTO 2020

		!
		! A 2 means that the add MUST be aborted.
		! (Used mostly for exit keys typed in the
		! input, during tests, etc.
		!
		CASE 2%
			MAIN_JOURADD = 2%
			EXIT FUNCTION
		END SELECT

		GOTO 2110
	END IF

2020	!
	!	ENTER ONE FIELD
	!
	FLAG% = 0% + 16384%
	V% = MAINT_GROUP(SMG_WINDOW, OPT_ENTRY, LOOP%, FLAG%, "")

	SELECT SCOPE::SCOPE_EXIT
	!
	! Control C, Exit
	!
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, &
		SMG$K_TRM_F8, SMG$K_TRM_PF3

		CALL ENTR_3MESSAGE(SCOPE, "Add aborted", 1%)
		GOTO MenuCurrent

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

		GOTO 2010

	END SELECT

	SCOPE1.EXIT% = SCOPE::SCOPE_EXIT
	!
	! Test special cases for each field if nessary
	!
	SELECT MAINT_GROUP(SMG_WINDOW, OPT_TESTENTRY, LOOP%, 0%, "ADD")

	!
	! A 1 means that it is correctable with retry
	!
	CASE 1%
		GOTO 2020

	!
	! A 2 means that the add MUST be aborted.
	! (Used mostly for exit keys typed in the
	! input, during tests, etc.
	!
	CASE 2%
		MAIN_JOURADD = 2%
		EXIT FUNCTION
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
				GOTO 2020

			!
			! A 2 means that the add MUST be aborted.
			! (Used mostly for exit keys typed in the
			! input, during tests, etc.
			!
			CASE 2%
				MAIN_JOURADD = 2%
				EXIT FUNCTION
			END SELECT

		NEXT I%

		GOTO 2120

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

		GOTO 2010

	!
	! Good keys
	!
	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

	!
	! Bad keys
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 2020
	END SELECT

2110	LOOP% = LOOP% + 1%
	GOTO 2010

2120	!
	! Normal Exit
	!
	MAIN_JOURADD = 0%

	EXIT FUNCTION

	!
	! Exit with a problem.  Requires recall of record and repaint
	!
 MenuCurrent:
	MAIN_JOURADD = 4%
	EXIT FUNCTION

 PaintScreen:
	!*******************************************************************
	! Paint current record on the screen.
	!*******************************************************************

	SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

	!
	! Paint the fields
	!
	FOR I% = 1% TO SMG_WINDOW::NITEMS
		V% = MAINT_GROUP(SMG_WINDOW, OPT_ENTRY, I%, 1%, "")
	NEXT I%

	V% = MAINT_GROUP(SMG_WINDOW, OPT_DISPLAY, 0%, 0%, "")

	SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)
	RETURN

	END FUNCTION
