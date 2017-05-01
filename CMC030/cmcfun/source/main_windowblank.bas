1	%TITLE "Blank Function"
	%SBTTL "MAIN_WINDOWBLANK"
	%IDENT "V3.6a Calico"

	FUNCTION INTEGER MAIN_WINDOWBLANK(CDD_WINDOW_CDD SMG_WINDOW)

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
	!	maintainence programs.
	!
	! Index:
	!	.X Maintainence
	!	.X Blank
	!
	! Parameters:
	!
	!	SMG_WINDOW
	!		This is the definition of the current window.
	!
	!	This function returns an integer value that is used to blank
	!	fields in maintainence programs.
	!
	! Example:
	!
	!	STAT% = MAIN_WINDOWBLANK(SMG_WINDOW)
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:MAIN_WINDOWBLANK/NOLINE
	!	$ LIB FUNC_LIB:CMCFUN/REP MAIN_WINDOWBLANK
	!	$ DELETE MAIN_WINDOWBLANK.OBJ;*
	!
	! Author:
	!
	!	01/28/87 - Kevin Handy
	!
	! Modification history:
	!
	!	08/28/90 - Frank F. Starman
	!		Replace ENTR_3MESSAGE function with HELP_34message.
	!
	!	03/13/91 - Frank F. Starman
	!		Set conditions for HFLAG
	!
	!	05/22/91 - J. Shad Rydalch
	!		Fixed problem when LOOP% > 64 got subscript out
	!		of range. Split IF statment into two parts.
	!
	!	11/24/92 - Kevin Handy
	!		Fixed problem where ENTR_3NUMBER is being called
	!		with 0% instead of 0.0 as default value (as shown
	!		by attempted Alpha AXP conversion)
	!
	!	03/26/93 - Kevin Handy
	!		Clean up (Check)
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
	!
	!	10/25/98 - Kevin Handy
	!		Use SMG_WINDOW::IDENT instead of SMG_SCREEN_DATA%
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

	CALL MAIN_PAINTSCREEN(SMG_WINDOW, " ", 0%)
	TIMEOUT% = -1%

 ChangeLines:
	!
	!	Enter the item number(keypunch mode)
	!
	FLAG% = 4%
	FLAG% = 12% IF TIMEOUT%
	LOOP% = ENTR_3NUMBER(SCOPE, SMG_WINDOW::IDENT, &
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
		GOTO ChangeLines
	END SELECT

	IF LOOP% = 0%
	THEN
		MAIN_WINDOWBLANK = 0%
		EXIT FUNCTION
	END IF


	!
	! Split this IF statment into two parts in order to test
	! LOOP% if to big caused problem with HFLAG array.
	!
	GOTO ChangeLines IF LOOP% < 1% OR LOOP% > SMG_WINDOW::NITEMS
	GOTO ChangeLines IF (SMG_WINDOW::HFLAG(LOOP%) AND 2%)

	!
	!	Enter one field
	!
	CALL MAIN_PAINTSCREEN(SMG_WINDOW, "C", LOOP%)
	CALL MAINT_GROUP(SMG_WINDOW, OPT_ENTRY, LOOP%, 33%, "")

	!
	! Test special cases for each field if nessary
	!
	SELECT MAINT_GROUP(SMG_WINDOW, OPT_TESTENTRY, LOOP%, 0%, "BLANK")
	CASE 0%

	CASE ELSE
		CALL HELP_34MESSAGE(SCOPE, "field cannot be blanked", &
			"W", "MAIN_WINDOWBLANK", "", "NOBLANK")
	!++
	! Warning:NOBLANK
	!--
			GOTO Changef
	END SELECT

	GOTO ChangeLines

 Changef:
 MenuCurrent:
	MAIN_WINDOWBLANK = 4%

	END FUNCTION
