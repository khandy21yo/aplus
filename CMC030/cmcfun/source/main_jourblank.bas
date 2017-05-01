1	%TITLE "Blank Function"
	%SBTTL "MAIN_JOURBLANK"
	%IDENT "V3.6a Calico"

	FUNCTION INTEGER MAIN_JOURBLANK(CDD_WINDOW_CDD SMG_WINDOW)

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
	!	This function is used to handle blanking fields in
	!	maintenance programs.
	!
	! Index:
	!	.x Maintenance
	!	.x Blank
	!
	! Parameters:
	!
	!	SMG_WINDOW
	!		This is the definition of the current window.
	!
	!
	!	Returned value
	!		It returns a value that is used by the maintenance
	!		programs to blank fields.
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:MAIN_JOURBLANK/NOLINE
	!	$ LIB FUNC_LIB:CMCFUN/REP MAIN_JOURBLANK
	!	$ DELETE MAIN_JOURBLANK.OBJ;*
	!
	! Author:
	!
	!	01/28/87 - Kevin Handy
	!
	! Modification history:
	!
	!	11/24/92 - Kevin Handy
	!		Fixed buf found in Alpha AXP conversion where
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
	LOOP% = ENTR_3NUMBER(SCOPE, SCOPE::SMG_OPTION, &
		"", "Field to " + TRM$(SCOPE::PRG_ITEM), 0.0, FLAG%, "##", "")

	SELECT SCOPE::SCOPE_EXIT

	!
	! Exit key typed
	!
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_F8, SMG$K_TRM_CTRLZ, &
		SMG$K_TRM_TIMEOUT

		GOTO Changef

	!
	! Good key
	!
	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
		! Good key

	!
	! Bad key
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO EnterItem
	END SELECT

	IF LOOP% = 0%
	THEN
		MAIN_JOURBLANK = 0%
		EXIT FUNCTION
	END IF

	GOTO EnterItem IF LOOP% < 1% OR LOOP% > SMG_WINDOW::NITEMS

 Changee:
 EnterOne:
	!
	!	Enter one field
	!
	CALL MAINT_GROUP(SMG_WINDOW, OPT_ENTRY, LOOP%, 33%, "")

	!
	! Test special cases for each field if nessary
	!
	SELECT MAINT_GROUP(SMG_WINDOW, OPT_TESTENTRY, LOOP%, 0%, "BLANK")
	CASE 0%

	CASE ELSE
		CALL ENTR_3MESSAGE(SCOPE, "Field cannot be blanked!", 0%)
		GOTO Changef
	END SELECT

	GOTO ChangeLines

 Changef:
 MenuCurrent:
	MAIN_JOURBLANK = 4%
	EXIT FUNCTION

	%PAGE

 PaintScreen:
	!*******************************************************************
	! Paint the current record on the screen
	!*******************************************************************

	SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

	!
	! Display the fields
	!
	FOR I% = 1% TO SMG_WINDOW::NITEMS
		V% = MAINT_GROUP(SMG_WINDOW, OPT_ENTRY, I%, 1%, "")
	NEXT I%

	V% = MAINT_GROUP(SMG_WINDOW, OPT_DISPLAY, 0%, 0%, "")

	SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)
	RETURN

	END FUNCTION
