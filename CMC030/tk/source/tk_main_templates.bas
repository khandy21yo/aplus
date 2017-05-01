1	%TITLE "Maintains Document Templates"
	%SBTTL "TK_MAIN_TEMPLATES"
	%IDENT "V3.6a Calico"

	SUB TK_MAIN_TEMPLATES

	!
	! COPYRIGHT (C) 1987 BY
	!
	! Computer Management Center
	! Idaho Falls, Idaho.
	!
	! This software is furnished under a license and may be used and
	! copied only in accordance with terms of such license and with
	! the inclusion of the above copyright notice.  This software or
	! any other copies therof may not be provided or otherwise made
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
	! Abstract:HELP
	!	.p
	!	Maintains the document templates.
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_MAIN_TEMPLATES/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP TK_MAIN_TEMPLATES
	!	$ DELETE TK_MAIN_TEMPLATES.OBJ;*
	!
	! Author:
	!
	!	06/19/87 - B. Craig Larsen
	!
	! Modification history:
	!
	!	06/11/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/13/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/04/2000 - Kevin Handy
	!		Lose useless error trap
	!--
	%PAGE

	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[SMG.OPEN]SMG_SCROLL.HB"
	DECLARE  SMG_SCROLL_CDD WINDOW_SCROLL

	EXTERNAL STRING  FUNCTION TK_MAIN_TEXTFILEWINDOW

	%PAGE

	OLD_IDENT$ = SCOPE::PRG_IDENT		! Save the COMMON Ident
	OLD_PROGRAM$ = SCOPE::PRG_PROGRAM	! Save the COMMON Program
	OLD_ITEM$ = SCOPE::PRG_ITEM		! Save the COMMON item

	SCOPE::PRG_IDENT	= "FUNC"
	SCOPE::PRG_PROGRAM	= "TK_MAIN_TEMPLATES"
	SCOPE::PRG_ITEM		= ""

	PROTO_FILE_NAME$ = "CMC:TK_*_TYPE.TEMPLATE"
	PROTO_PREFIX$ = "CMC:TK_"
	PROTO_SUFFIX$ = ".TEMPLATE"

100	!
	! Create the data display
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(12%, 76%, SMG_SCREEN_DATA%, &
		SMG$M_BORDER)

	SMG_STATUS% = SMG$LABEL_BORDER(SMG_SCREEN_DATA%, &
		" Maintain Templates ", SMG$K_TOP,, SMG$M_BOLD)

	TYPE.LINE$ = "   Task Templates            Function Templates" + &
		"         Element Templates   "
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, TYPE.LINE$, 1%, 1%)

	!
	! Paste the data display
	!
	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, 7%, 3%)

	EC_LOOP% = 1%
	GOSUB TurnOn

 Menu:
1000	!******************************************************************
	! Maintenance window
	!******************************************************************

	SCOPE::PRG_ITEM = ""

	OPTLIST$ = "Maintain_types Task_edit Function_edit Element_edit " + &
			"Help eXit"
	OPT$ = ENTR_3OPTION(SCOPE, "COMMAND", OPTLIST$, 0%, 0%)

	SELECT SCOPE::SCOPE_EXIT
	CASE SMG$K_TRM_DOWN

		GOSUB TurnOff

		EC_LOOP% = EC_LOOP% + 1%
		EC_LOOP% = 1% IF EC_LOOP% > 3%

		GOSUB TurnOn
		GOTO Menu

	CASE SMG$K_TRM_UP

		GOSUB TurnOff

		EC_LOOP% = EC_LOOP% - 1%
		EC_LOOP% = 3% IF EC_LOOP% < 1%

		GOSUB TurnOn
		GOTO Menu

	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ

		OPT$ = "X"

	CASE SMG$K_TRM_SELECT

		SELECT EC_LOOP%
		CASE 1%
			OPT$ = "T"
		CASE 2%
			OPT$ = "F"
		CASE 3%
			OPT$ = "E"
		END SELECT

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO Menu
	END SELECT

1500	SELECT OPT$
	!
	! Add a new field
	!
	CASE "M"
		TTYPE$ = TK_MAIN_TEXTFILEWINDOW("4;26", "13;30", &
			PROTO_FILE_NAME$, PROTO_PREFIX$, &
			PROTO_SUFFIX$, "Maintain Templates", 16%)

		OPT$ = LEFT(TTYPE$, 1%)
		GOTO 1500 IF OPT$ = "E" OR OPT$ = "F" OR OPT$ = "T"

	CASE "F"

		GOSUB TurnOff
		EC_LOOP% = 2%
		GOSUB TurnOn
		CALL TK_SUBR_DOCUTPLATE

	CASE "T"

		GOSUB TurnOff
		EC_LOOP% = 1%
		GOSUB TurnOn
		CALL TK_MAIN_TASKTPLATE

	CASE "E"

		GOSUB TurnOff
		EC_LOOP% = 3%
		GOSUB TurnOn
		CALL TK_MAIN_ELEMTPLATE

	!
	! Exit
	!
	CASE "X"

		GOTO ExitProgram

	!
	! Help
	!
	CASE "H"
		CALL HELP_3MESSAGE(SCOPE, "", "PROG", SCOPE::PRG_PROGRAM, &
			"HELP")

	END SELECT

	GOTO Menu

 ExitProgram:
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$POP_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%, SCOPE::SMG_PBID)

	SCOPE::PRG_IDENT	= OLD_IDENT$
	SCOPE::PRG_PROGRAM	= OLD_PROGRAM$
	SCOPE::PRG_ITEM		= OLD_ITEM$

	EXIT SUB

 TurnOn:
11000	!***************************************************************
	! Turn on the reverse
	!***************************************************************
	SELECT EC_LOOP%
	CASE 1%
		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			MID$(TYPE.LINE$, 4%, 14%),, 4%,, &
			SMG$M_REVERSE)

	CASE 2%
		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			MID$(TYPE.LINE$, 30%, 18%),, 30%,, &
			SMG$M_REVERSE)

	CASE 3%
		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			MID$(TYPE.LINE$, 57%, 17%),, 57%,, &
			SMG$M_REVERSE)

	END SELECT

	RETURN

 TurnOff:
12000	!***************************************************************
	! Turn off the reverse
	!***************************************************************
	SELECT EC_LOOP%
	CASE 1%
		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			MID$(TYPE.LINE$, 4%, 14%),, 4%)

	CASE 2%
		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			MID$(TYPE.LINE$, 30%, 18%),, 30%)

	CASE 3%
		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			MID$(TYPE.LINE$, 57%, 17%),, 57%)

	END SELECT

	RETURN

32767	END SUB
