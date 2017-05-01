1	%TITLE "Change Function"
	%SBTTL "MAIN_JOURCHANGE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG MAIN_JOURCHANGE(CDD_WINDOW_CDD SMG_WINDOW, LONG FIELDNO)

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
	!	This function handles changing information on the
	!	maintainence programs.
	!
	! Index:
	!	.x Maintainence
	!	.x Change
	!
	! Parameters:
	!
	!	SMG_WINDOW
	!		This passed variable is used to define the current window.
	!
	!
	!	Returned value
	!		The output of this function is a long value that is used to
	!		change information in the maintainence programs.
	!
	! Example:
	!
	!	STAT% = MAIN_JOURCHANGE(SMG_WINDOW)
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:MAIN_JOURCHANGE/NOLINE
	!	$ LIB FUNC_LIB:CMCFUN/REP MAIN_JOURCHANGE
	!	$ DELETE MAIN_JOURCHANGE.OBJ;*
	!
	! Author:
	!
	!	01/28/87 - Kevin Handy
	!
	! Modification history:
	!
	!	09/30/92 - Kevin Handy
	!		Modified to allow prev/next keys shift to
	!		same field on next record.
	!
	!	11/24/92 - Kevin Handy
	!		Fixed bug found in Alpha AXP conversion where
	!		ENTR_3NUMBER was called with 0% instead of 0.0
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

	!*******************************************************************
	! Do we enter knowing which field to start in already?
	!*******************************************************************

	IF FIELDNO <> 0%
	THEN
		LOOP% = FIELDNO
		FIELDNO = 0%
		GOSUB PaintScreen
		TIMEOUT% = -1%
		GOTO Changee
	END IF

 Changer:
4000	!*******************************************************************
	! CHANGE RECORD
	!*******************************************************************

	GOSUB PaintScreen
	TIMEOUT% = -1%

 ChangeLines:
 EnterItem:
	!
	!	Enter the item number(keypunch mode)
	!
	FLAG% = 4%
	FLAG% = 12% IF TIMEOUT%

	! (Hack to fix name)

	LOOP% = ENTR_3NUMBER(SCOPE, SCOPE::SMG_OPTION, &
		"", "Field to " + TRM$(SCOPE::PRG_ITEM), 0.0, FLAG%, "##", "")

	SELECT SCOPE::SCOPE_EXIT

	!
	! Abort key
	!
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, &
		SMG$K_TRM_PF2, SMG$K_TRM_F8, SMG$K_TRM_TIMEOUT

		GOTO Changef

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
		GOTO EnterItem
	END SELECT

	IF LOOP% = 0%
	THEN
		MAIN_JOURCHANGE = 0%
		EXIT FUNCTION
	END IF

	GOTO EnterItem IF LOOP% < 1% OR LOOP% > SMG_WINDOW::NITEMS

 Changee:
 EnterOne:
	!
	!	ENTER ONE FIELD
	!
	FLAG% = 0%
	FLAG% = 8% IF TIMEOUT%
	CALL MAINT_GROUP(SMG_WINDOW, OPT_ENTRY, LOOP%, FLAG%, "")

	SELECT SCOPE::SCOPE_EXIT
	!
	! Exit keys
	!
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, &
		SMG$K_TRM_F8, SMG$K_TRM_TIMEOUT

		CALL ENTR_3MESSAGE(SCOPE, "Change aborted", 1%)
		GOTO MenuCurrent

	END SELECT

	!
	! Test special cases for each field if nessary
	!
	SELECT MAINT_GROUP(SMG_WINDOW, OPT_TESTENTRY, LOOP%, 0%, "CHANGE")
	CASE 1%
		GOTO Changee

	CASE 2%
		MAIN_JOURCHANGE = 2%
		EXIT FUNCTION
	END SELECT

	SELECT SCOPE::SCOPE_EXIT
	!
	! Uparrow
	!
	CASE SMG$K_TRM_UP
		LOOP% = LOOP% - 1%
		LOOP% = 1% IF LOOP% <= 1%
		GOTO Changee

	!
	! Downarrow
	!
	CASE SMG$K_TRM_DOWN
		LOOP% = LOOP% + 1%
		LOOP% = SMG_WINDOW::NITEMS IF LOOP% > SMG_WINDOW::NITEMS
		GOTO Changee

	!
	! Gold/Uparrow, Dold/Downarrow
	!
	CASE -SMG$K_TRM_UP, -SMG$K_TRM_DOWN, &
		SMG$K_TRM_PREV_SCREEN, SMG$K_TRM_NEXT_SCREEN

		FIELDNO = LOOP%
		MAIN_JOURCHANGE = 0%
		EXIT FUNCTION

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
		GOTO Changee
	END SELECT

	GOTO ChangeLines

 Changef:
 MenuCurrent:
	MAIN_JOURCHANGE = 4%
	EXIT FUNCTION

	%PAGE

 PaintScreen:
	!*******************************************************************
	! Paint the current record on the screen
	!*******************************************************************

	SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

	!
	! Display fields
	!
	FOR I% = 1% TO SMG_WINDOW::NITEMS
		V% = MAINT_GROUP(SMG_WINDOW, OPT_ENTRY, I%, 1%, "")
	NEXT I%

	V% = MAINT_GROUP(SMG_WINDOW, OPT_DISPLAY, 0%, 0%, "")

	SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)
	RETURN

	END FUNCTION
