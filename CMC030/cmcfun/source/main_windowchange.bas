1	%TITLE "Change Function"
	%SBTTL "MAIN_WINDOWCHANGE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG MAIN_WINDOWCHANGE(CDD_WINDOW_CDD SMG_WINDOW)

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
	!	.X Maintainence
	!	.X Change
	!
	! Parameters:
	!
	!	SMG_WINDOW
	!		This is used to define the current window.
	!
	!
	!	Returned value
	!		The output of this function is a long value that is used to
	!		change information in the maintainence programs.
	!
	! Example:
	!
	!	STAT% = MAIN_WINDOWCHANGE(SMG_WINDOW)
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:MAIN_WINDOWCHANGE/NOLINE
	!	$ LIB FUNC_LIB:CMCFUN/REP MAIN_WINDOWCHANGE
	!	$ DELETE MAIN_WINDOWCHANGE.OBJ;*
	!
	! Author:
	!
	!	01/28/87 - Kevin Handy
	!
	! Modification history:
	!
	!	09/16/87 - Kevin Handy
	!		Fixed problem with SCOPE.EXIT% passing through
	!		OPT_TESTENTRY, and getting lost.
	!
	!	08/28/90 - Frank F. Starman
	!		Replace ENTR_3MESSAGE function with HELP_34MESSAGE.
	!
	!	03/13/91 - Frank F. Starman
	!		Set conditions for HFLAG.
	!
	!	05/22/91 - J. Shad Rydalch
	!		Fixed problem when LOOP% > 64 got subscript out
	!		of range. Split IF statment into two parts.
	!
	!	11/24/92 - Kevin Handy
	!		Fixed bug shown up by Alpha AXP where ENTR_3NUMBER
	!		is called with default value 0% instead of 0.0
	!
	!	08/31/93 - Frank Starman
	!		Set MVALUE to CHANCE if loop on OPT_ENTRY.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	04/23/96 - Kevin Handy
	!		Reformat source code
	!
	!	04/23/96 - Kevin Handy
	!		Added return type 255 (delete with high bit set)
	!		to mean that you can't edit this field. This will
	!		keep an infinite loop from occurring.
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

	CALL MAIN_PAINTSCREEN(SMG_WINDOW, " ", 0%)
	TIMEOUT% = -1%

 ChangeLines:
 EnterItem:
	!
	! Enter the item number(keypunch mode)
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
		GOTO EnterItem

	END SELECT

	IF LOOP% = 0%
	THEN
		MAIN_WINDOWCHANGE = 0%
		EXIT FUNCTION
	END IF

	!
	! Split this IF statment into two parts in order to test
	! LOOP% if to big caused problem with HFLAG array.
	!
	GOTO EnterItem IF LOOP% < 1% OR LOOP% > SMG_WINDOW::NITEMS
	GOTO EnterItem IF (SMG_WINDOW::HFLAG(LOOP%) AND 2%)

 Changee:
 EnterOne:
	!
	! ENTER ONE FIELD
	!
	FLAG% = 0%
	FLAG% = 8% IF TIMEOUT%
	CALL MAIN_PAINTSCREEN(SMG_WINDOW, "C", LOOP%)
	CALL MAINT_GROUP(SMG_WINDOW, OPT_ENTRY, LOOP%, FLAG%, "CHANGE")

	SELECT SCOPE::SCOPE_EXIT
	!
	! Exit keys
	!
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, &
		SMG$K_TRM_F8, SMG$K_TRM_TIMEOUT

		GOTO MenuCurrent

	!
	! You can't change this item
	!
	CASE 255%
		GOTO EnterItem

	END SELECT

	SCOPE.EXIT1% = SCOPE::SCOPE_EXIT

	!
	! Test special cases for each field if nessary
	!
	SELECT MAINT_GROUP(SMG_WINDOW, OPT_TESTENTRY, LOOP%, 0%, "CHANGE")
	CASE 1%
		GOTO Changee

	CASE 2%
		MAIN_WINDOWCHANGE = 2%
		EXIT FUNCTION
	END SELECT

	SELECT SCOPE.EXIT1%
	!
	! Uparrow
	!
	CASE SMG$K_TRM_UP
		LOOP% = LOOP% - 1%
		WHILE (SMG_WINDOW::HFLAG(LOOP%) AND 2%)
			LOOP% = LOOP% - 1%
		NEXT
		LOOP% = 1% IF LOOP% <= 1%
		GOTO Changee

	!
	! Downarrow
	!
	CASE SMG$K_TRM_DOWN
		LOOP% = LOOP% + 1%
		WHILE (SMG_WINDOW::HFLAG(LOOP%) AND 2%)
			LOOP% = LOOP% + 1%
		NEXT
		LOOP% = SMG_WINDOW::NITEMS IF LOOP% > SMG_WINDOW::NITEMS
		GOTO Changee

	!
	! Good keys
	!
	CASE 0%, 10%, 12%, 13%, 296%
		! Good key

	!
	! Bad keys
	!
	CASE ELSE
		SCOPE::SCOPE_EXIT = SCOPE.EXIT1%
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO Changee
	END SELECT

	GOTO ChangeLines

 MenuCurrent:
	MAIN_WINDOWCHANGE = 4%

	END FUNCTION
	!+-+-+
	!++
	! Warning:NOCHANGE
	!	^*No Change Done\*
	!	.b
	!	.lm +5
	!	^*Explanation\*
	!	.b
	!	No change has been done in the record.
	!	.b
	!	^*User Action\*
	!	.b
	!	Press the Return key instead of the Exit key to save all changes after
	!	they have been done.
	!	.lm -5
	!
	! Index:
	!	.x Change
	!
	!--
