1	%TITLE "Change Function"
	%SBTTL "MAIN_DEFAULT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG MAIN_DEFAULT(CDD_WINDOW_CDD SMG_WINDOW)

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
	!	This function handles defaulting information on the
	!	maintenance programs.
	!
	! Index:
	!	.x Maintainence
	!	.x Change
	!
	! Parameters:
	!
	!	SMG_WINDOW
	!		The definition of the current window.
	!
	!	The options for the default are:
	!	.table
	!			"H" - Hard default
	!
	!			"S" - Soft default
	!
	!			"B" - Blank a default
	!
	!			"T" - Store
	!
	!			"R" - Recall
	!
	!			"F" - Field
	!
	!			"A" - Allow
	!
	!			"X" - Exit
	!	.endtable
	!
	!	Returned value
	!		It sends out defaulting information to the
	!		maintainence programs.
	!
	! Example:
	!
	!	SELECT MAIN_DEFAULT(SMG_WINDOW)
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:MAIN_DEFAULT/NOLINE
	!	$ LIB FUNC_LIB:CMCFUN/REP MAIN_DEFAULT
	!	$ DELETE MAIN_DEFAULT.OBJ;*
	!
	! Author:
	!
	!	01/28/87 - Kevin Handy
	!
	! Modification history:
	!
	!	08/28/90 - Frank F. Starman
	!		Replace ENTR_MESSAGE function with HELP_34MESSAGE.
	!
	!	01/20/91 - Frank F. Starman
	!		Add the Field option.
	!
	!	03/11/91 - Frank F. Starman
	!		Add the Allow option.
	!
	!	11/24/92 - Kevin Handy
	!		Fixed bug that showed up in Alpha AXP conversion
	!		where ENTR_3NUMBER is called with 0% instead
	!		of 0.0
	!
	!	03/26/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/16/96 - Kevin Handy
	!		Reformat source code.
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

	!
	! Save an original variable
	!
	ORIG_ITEM$ = SCOPE::PRG_ITEM

	%PAGE

4000	!*******************************************************************
	! CHANGE RECORD
	!*******************************************************************
	SCOPE::PRG_ITEM = "Default"

	V% = MAINT_GROUP(SMG_WINDOW, OPT_RESETDEFAULT, 0%, 0%, "")

	DIM TEMP_HFLAG%(SMG_WINDOW::NITEMS)

	TEMP_COUNT% = 64%
	TEMP_COUNT% = SMG_WINDOW::NITEMS IF SMG_WINDOW::NITEMS < TEMP_COUNT%

	TEMP_HFLAG%(I%) = SMG_WINDOW::HFLAG(I%) FOR I% = 1% TO TEMP_COUNT%

	SMG_WINDOW::HFLAG(I%) = 0% FOR I% = 1% TO TEMP_COUNT%
	CALL MAIN_PAINTSCREEN(SMG_WINDOW, " ", 1%)

4100	!
	! Set up initial option display
	!
	SCOPE::PRG_ITEM = "DEFAULT_"
	OPT$ = ENTR_3OPTION(SCOPE, "Default", &
		"Hard Soft Blank sTore Recall Field eXit", OPT%, 8%)

	SELECT SCOPE::SCOPE_EXIT

	!
	! Good exit key typed
	!
	CASE SMG$K_TRM_CTRLZ, SMG$K_TRM_F10
		GOTO GoodExit

	!
	! Bad Exit keys
	!
	CASE 3%, SMG$K_TRM_PF2, SMG$K_TRM_F8

		GOTO BadExit

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
		GOTO 4100

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
		GOTO 4100

	!
	! Good keys
	!
	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

	!
	! Bad keys
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 4100
	END SELECT

	!
	! Now, handle the options
	!
	SELECT OPT$

	!
	! Remove Field
	!
	CASE "F"
		HARD.FLAG% = 3%
		GOTO ChangeLines

	!
	! Allow undefined input (default is Y)
	!
	CASE "A"
		GOTO ChangeLines

	!
	! Hard default
	!
	CASE "H"
		HARD.FLAG% = 1%
		GOTO ChangeLines

	!
	! Soft default
	!
	CASE "S"
		HARD.FLAG% = 0%
		GOTO ChangeLines

	!
	! Blank a default
	!
	CASE "B"
		GOTO BlankLines

	!
	! sTore
	!
	CASE "T"
		SMG_WINDOW::HFLAG(I%) = TEMP_HFLAG%(I%) &
			FOR I% = 1% TO TEMP_COUNT%
		V% = MAINT_GROUP(SMG_WINDOW, OPT_SETDEFAULT, 0%, 0%, "")

		CALL WRIT_DEFAULT(SMG_WINDOW)

		CALL HELP_34MESSAGE(SCOPE, "default has been stored", &
			"S", "MAIN_DEFAULT", "", "DEFST")

	!++
	! Success:DEFST
	!--
		GOTO 4000

	!
	! Recall
	!
	CASE "R"
		CALL READ_DEFAULTS(SMG_WINDOW)
		CALL HELP_34MESSAGE(SCOPE, "default recall completed", &
			"S", "MAIN_DEFAULT", "", "DEFREC")

	!++
	! Success:DEFREC
	!--
		GOTO 4000

	!
	! eXit
	!
	CASE "X"
		GOTO GoodExit
	END SELECT

	GOTO 4100

	%PAGE

 BlankLines:
	!
	!	Enter the item number(keypunch mode)
	!
	LOOP% = ENTR_3NUMBER(SCOPE, SCOPE::SMG_OPTION, &
		"", "Field to " + TRM$(SCOPE::PRG_ITEM), 0.0, 4%, "##", "")

	SELECT SCOPE::SCOPE_EXIT

	!
	! Abort key
	!
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, &
		SMG$K_TRM_PF2, SMG$K_TRM_F8

		GOTO BadExit

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
		GOTO BlankLines
	END SELECT

	GOTO 4100 IF LOOP% = 0%

	GOTO BlankLines IF (LOOP% < 1%) OR (LOOP% > SMG_WINDOW::NITEMS)

	TEMP_HFLAG%(LOOP%) = 0%
	CALL MAIN_PAINTSCREEN(SMG_WINDOW, "C", LOOP%)
	CALL MAINT_GROUP(SMG_WINDOW, OPT_ENTRY, LOOP%, 33%, "")

	GOTO BlankLines

	%PAGE

 ChangeLines:
	!
	!	Enter the item number(keypunch mode)
	!
	LOOP% = ENTR_3NUMBER(SCOPE, SCOPE::SMG_OPTION, &
		"", "Field to " + TRM$(SCOPE::PRG_ITEM), 0.0, 4%, "##", "")

	SELECT SCOPE::SCOPE_EXIT

	!
	! Abort key
	!
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, &
		SMG$K_TRM_PF2, SMG$K_TRM_F8

		GOTO 4100

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
		GOTO ChangeLines
	END SELECT

	GOTO 4100 IF LOOP% = 0%

	GOTO ChangeLines IF LOOP% < 1% OR LOOP% > SMG_WINDOW::NITEMS

 Changee:
	!
	!	ENTER ONE FIELD
	!
	CALL MAIN_PAINTSCREEN(SMG_WINDOW, "C", LOOP%)

	IF OPT$ = "A"
	THEN
		INP$ = ENTR_3YESNO(SCOPE, SCOPE::SMG_OPTION, &
			"", "Allow Undefined Input", "Y", 0%, "", "")
	ELSE
		TEMP_HFLAG%(LOOP%) = HARD.FLAG% + (TEMP_HFLAG%(I%) AND 4%)
		CALL MAINT_GROUP(SMG_WINDOW, OPT_ENTRY, LOOP%, 0%, "")
	END IF

	SELECT SCOPE::SCOPE_EXIT
	!
	! Exit keys
	!
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, &
		SMG$K_TRM_F8, SMG$K_TRM_TIMEOUT

		GOTO 4100
	END SELECT

	IF OPT$ = "A"
	THEN
		TEMP_HFLAG%(LOOP%) = TEMP_HFLAG%(LOOP%) OR  4%  IF INP$ = "Y"
		TEMP_HFLAG%(LOOP%) = TEMP_HFLAG%(LOOP%) AND 3%  IF INP$ = "N"
		GOTO ChangeLines
	END IF

	!
	! Test special cases for each field if nessary
	!
	SELECT MAINT_GROUP(SMG_WINDOW, OPT_TESTENTRY, LOOP%, 0%, "Default")
	CASE 1%
		GOTO Changee

	CASE 2%
		MAIN_DEFAULT = 2%
		EXIT FUNCTION
	END SELECT

	SELECT SCOPE::SCOPE_EXIT
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

 GoodExit:
	!
	! Load in the new defaults
	!
	SMG_WINDOW::HFLAG(I%) = TEMP_HFLAG%(I%) FOR I% = 1% TO TEMP_COUNT%

	V% = MAINT_GROUP(SMG_WINDOW, OPT_SETDEFAULT, 0%, 0%, "")
	V% = MAINT_GROUP(SMG_WINDOW, OPT_BACKGROUND, SMG_WINDOW::LCURR, 0%, "")

	MAIN_DEFAULT = 0%
	SCOPE::PRG_ITEM = ORIG_ITEM$

	EXIT FUNCTION


 BadExit:
	MAIN_DEFAULT = 4%
	SCOPE::PRG_ITEM = ORIG_ITEM$

	END FUNCTION
