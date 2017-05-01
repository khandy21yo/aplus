1	%TITLE "TK_MAIN_ELEMTPLATE Sub Process to Maintain a Element Template"
	%SBTTL "TK_MAIN_ELEMTPLATE"
	%IDENT "V3.6a Calico"

	SUB TK_MAIN_ELEMTPLATE

	!
	! COPYRIGHT (C) 1987 BY
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
	!	A process to aid in the maintenance of element templates.
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_MAIN_ELEMTPLATE
	!	$ LIB FUNC_LIB:CMCFUN/REP TK_MAIN_ELEMTPLATE
	!	$ DEL TK_MAIN_ELEMTPLATE.OBJ;*
	!
	! AUTHOR:
	!
	!	06/11/87 - B. Craig Larsen
	!
	! MODIFICATION HISTORY:
	!
	!	08/05/91 - Kevin Handy
	!		Added ACCESS READ to opens.
	!
	!	06/11/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/16/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/28/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/22/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[SMG.OPEN]SMG_SCROLL.HB"
	DECLARE  SMG_SCROLL_CDD WINDOW_SCROLL

	EXTERNAL LONG    FUNCTION EDT$EDIT
	EXTERNAL LONG    FUNCTION DSPL_SCROLL

	%PAGE

	ON ERROR GOTO 19000

	OLD_IDENT$ = SCOPE::PRG_IDENT		! Save the COMMON Ident
	OLD_PROGRAM$ = SCOPE::PRG_PROGRAM	! Save the COMMON Program
	OLD_ITEM$ = SCOPE::PRG_ITEM		! Save the COMMON item

	SCOPE::PRG_IDENT	= "FUNC"
	SCOPE::PRG_PROGRAM	= "FUNC_TK_MAIN_ELEMTPLATE"
	SCOPE::PRG_ITEM		= ""

50	!
	! Declare vars
	!
	DECLARE LONG SMG_SCROLL

	DECLARE INTEGER CONSTANT ELEM_ITEMS = 500
	DECLARE INTEGER CONSTANT FILE_ITEMS = 100

	FILE_NAME$ = "CMC:TK_ELEMENT_TYPE.TEMPLATE"
	PREFIX$ = "CMC:HLP_ELEMENT_TYPE_"
	SUFFIX$ = ".TEMPLATE"

	SCROLL_BOT% = 10%

	DIM	NAME$(ELEM_ITEMS),	TTEST$(ELEM_ITEMS), &
		TTYPE$(ELEM_ITEMS),	DESC$(ELEM_ITEMS), &
		SIZE$(ELEM_ITEMS),	LINES$(ELEM_ITEMS)

	!
	! Help file classes
	!
	HELP_CLASS$(1%) = "Application"
	HELP_CLASS$(2%) = "User"
	HELP_CLASS$(3%) = "Training"
	HELP_CLASS$(4%) = "Getting started"

	HELP_CLASS_CODE$(1%) = "HELP"
	HELP_CLASS_CODE$(2%) = "USER"
	HELP_CLASS_CODE$(3%) = "TRAIN"
	HELP_CLASS_CODE$(4%) = "GET_START"

	NEW.CH% = 4%
	OLD.CH% = 5%
	EDIT.CH% = 6%

300	!
	! Create the data display
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(SCROLL_BOT%, 76%, &
		SMG_SCROLL, SMG$M_BORDER)

	SMG_STATUS% = SMG$LABEL_BORDER(SMG_SCROLL, " Element Template ", &
		SMG$K_TOP,, SMG$M_BOLD)

	TITLE$ = "  (01)              (02)              (03)            (04) (05)              "
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCROLL, TITLE$, 1%, 1%,, &
		SMG$M_REVERSE)

	TITLE$ = "  Name              Description       Type            Size Test              "
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCROLL, TITLE$, 2%, 1%,, &
		SMG$M_REVERSE)

	!
	! Paste the data display
	!
	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_SCROLL, &
		SCOPE::SMG_PBID, 9%, 3%)

500	GOSUB ReadInArray

	!
	! Define windows
	!
	WINDOW_SCROLL::WINDOW		= SMG_SCROLL
	WINDOW_SCROLL::TOP_ARRAY	= 1%
	WINDOW_SCROLL::BOT_ARRAY	= ARRAY_LOOP% + 1%
	WINDOW_SCROLL::BOT_ARRAY	= 1% IF ARRAY_LOOP% = 0%
	WINDOW_SCROLL::SCROLL_TOP	= 3%
	WINDOW_SCROLL::SCROLL_BOT	= SCROLL_BOT%
	WINDOW_SCROLL::TOP_LINE		= 1%
	WINDOW_SCROLL::BEG_ELEMENT	= 1%
	WINDOW_SCROLL::END_ELEMENT	= ARRAY_LOOP% + 1%
	WINDOW_SCROLL::END_ELEMENT	= 1% IF ARRAY_LOOP% = 0%
	WINDOW_SCROLL::CUR_LINE		= 1%
	WINDOW_SCROLL::CUR_W_ROW	= 1%
	WINDOW_SCROLL::CUR_W_COL	= 1%
	WINDOW_SCROLL::FIND_LINE	= 1%
	WINDOW_SCROLL::SMG_FLAG		= 2%
	WINDOW_SCROLL::PROMPT		= "->"
	WINDOW_SCROLL::VIDEO_COMP	= 0%
	WINDOW_SCROLL::CHARSET		= 0%
	WINDOW_SCROLL::DRAW_COLS	= "020,038,054,059"

	!
	! Print the array
	!
	TEMP% = DSPL_SCROLL(WINDOW_SCROLL, LINES$(), 0%, "PAINT")
	ARRAY_LOOP% = WINDOW_SCROLL::CUR_LINE

 Menu:
1000	!******************************************************************
	! Maintenance window
	!******************************************************************

	SCOPE::PRG_ITEM	= ""

	OPTLIST$ = "Add Change Delete Edit Help eXit"
	OPT$ = ENTR_3OPTION(SCOPE, "COMMAND", OPTLIST$, OPT%, FLAGW%)

	SELECT SCOPE::SCOPE_EXIT
	CASE SMG$K_TRM_UP, SMG$K_TRM_DOWN, &
		SMG$K_TRM_PREV_SCREEN, SMG$K_TRM_NEXT_SCREEN, &
		SMG$K_TRM_F18, SMG$K_TRM_F19

		V% = DSPL_SCROLL(WINDOW_SCROLL, LINES$(), &
			SCOPE::SCOPE_EXIT, "PAINT")
		ARRAY_LOOP% = WINDOW_SCROLL::CUR_LINE

		GOTO Menu

	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ

		OPT$ = "X"

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO Menu
	END SELECT

	SELECT OPT$
	!
	! Add a new field
	!
	CASE "A"
		FUN_NAME$ = "Add "
 NextEntry:
		FOR LOOP% = WINDOW_SCROLL::BOT_ARRAY TO ARRAY_LOOP% STEP -1%
			NAME$(LOOP% + 1%) = NAME$(LOOP%)
			DESC$(LOOP% + 1%) = DESC$(LOOP%)
			TTYPE$(LOOP% + 1%) = TTYPE$(LOOP%)
			SIZE$(LOOP% + 1%) = SIZE$(LOOP%)
			TTEST$(LOOP% + 1%) = TTEST$(LOOP%)
			LINES$(LOOP% + 1%) = LINES$(LOOP%)
		NEXT LOOP%

		WINDOW_SCROLL::BOT_ARRAY, WINDOW_SCROLL::END_ELEMENT = &
			WINDOW_SCROLL::BOT_ARRAY + 1%

		LINES$(ARRAY_LOOP%) = ""
		NAME$(ARRAY_LOOP%) = ""
		DESC$(ARRAY_LOOP%) = ""
		TTYPE$(ARRAY_LOOP%) = ""
		SIZE$(ARRAY_LOOP%) = ""
		TTEST$(ARRAY_LOOP%) = ""

		X% = DSPL_SCROLL(WINDOW_SCROLL, LINES$(), 0%, "PAINT")
		ARRAY_LOOP% = WINDOW_SCROLL::CUR_LINE

		FOR ENTERCHANGE_LOOP% = 1% TO 5%
 EnterLoop:
1100		!******************************************
		! Enter data to screen
		!******************************************
		GOSUB EnterChange

		SELECT SCOPE::SCOPE_EXIT
		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ

			FOR LOOP% = ARRAY_LOOP% TO WINDOW_SCROLL::BOT_ARRAY
				NAME$(LOOP%) = NAME$(LOOP% + 1%)
				DESC$(LOOP%) = DESC$(LOOP% + 1%)
				TTYPE$(LOOP%) = TTYPE$(LOOP% + 1%)
				SIZE$(LOOP%) = SIZE$(LOOP% + 1%)
				TTEST$(LOOP%) = TTEST$(LOOP% + 1%)
				LINES$(LOOP%) = LINES$(LOOP% + 1%)
			NEXT LOOP%

			LINES$(WINDOW_SCROLL::BOT_ARRAY) = ""

			X% = DSPL_SCROLL(WINDOW_SCROLL, LINES$(), 0%, &
				"PAINT")
			ARRAY_LOOP% = WINDOW_SCROLL::CUR_LINE

			WINDOW_SCROLL::BOT_ARRAY, &
				WINDOW_SCROLL::END_ELEMENT = &
					WINDOW_SCROLL::BOT_ARRAY - 1%
			CALL ENTR_3MESSAGE(SCOPE, "Add aborted", 1%)
			GOTO Menu

		CASE SMG$K_TRM_DOWN

			ENTERCHANGE_LOOP% = ENTERCHANGE_LOOP% + 1%
			ENTERCHANGE_LOOP% = 5% IF ENTERCHANGE_LOOP% > 5%
			GOTO EnterLoop

		CASE SMG$K_TRM_UP

			ENTERCHANGE_LOOP% = ENTERCHANGE_LOOP% - 1%
			ENTERCHANGE_LOOP% = 1% IF ENTERCHANGE_LOOP% < 1%
			GOTO EnterLoop

		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO EnterLoop
		END SELECT

		NEXT ENTERCHANGE_LOOP%

		GOSUB SetUpArray

		WINDOW_SCROLL::FIND_LINE = WINDOW_SCROLL::CUR_LINE + 1%

		ARRAY_LOOP% = DSPL_SCROLL(WINDOW_SCROLL, LINES$(), 0%, &
			"FIND")
		ARRAY_LOOP% = WINDOW_SCROLL::CUR_LINE

		GOTO NextEntry

	CASE "C"	! Change a field
		FUN_NAME$ = "Change "
		ENTERCHANGE_LOOP% = 0%
		GOSUB SaveFields

 ChangeLoop:
1200		!****************************************************
		! Change data to screen
		!****************************************************
		ITM$ = EDIT$(ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
			"", "Item to change", "  ", 4%, &
			"", ""), -1%)
		ITM$ = "0" + ITM$ IF LEN(ITM$) < 2% AND ITM$ <> ""

		SELECT SCOPE::SCOPE_EXIT
		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ

			GOSUB RestoreFields
			X% = DSPL_SCROLL(WINDOW_SCROLL, &
				LINES$(), 0%, "PAINT")
			ARRAY_LOOP% = WINDOW_SCROLL::CUR_LINE
			CALL ENTR_3MESSAGE(SCOPE, "Change aborted", 1%)
			GOTO Menu

		CASE SMG$K_TRM_DOWN

			ENTERCHANGE_LOOP% = ENTERCHANGE_LOOP% + 1%
			IF ENTERCHANGE_LOOP% > 5%
			THEN
				IF ARRAY_LOOP% < WINDOW_SCROLL::END_ELEMENT - 1%
				THEN
					ENTERCHANGE_LOOP% = 1%
					ARRAY_LOOP% = DSPL_SCROLL( &
						WINDOW_SCROLL, &
						LINES$(), SCOPE::SCOPE_EXIT, &
						"")

					ARRAY_LOOP% = &
						WINDOW_SCROLL::CUR_LINE
					GOSUB SaveFields
				ELSE
					ENTERCHANGE_LOOP% = 5%
				END IF
			END IF
			ITM$ = NUM1$(ENTERCHANGE_LOOP%)

		CASE SMG$K_TRM_UP
			ENTERCHANGE_LOOP% = ENTERCHANGE_LOOP% - 1%
			IF ENTERCHANGE_LOOP% < 1%
			THEN
				IF ARRAY_LOOP% > WINDOW_SCROLL::BEG_ELEMENT
				THEN
					ENTERCHANGE_LOOP% = 5%
					ARRAY_LOOP% = DSPL_SCROLL( &
						WINDOW_SCROLL, &
						LINES$(), SCOPE::SCOPE_EXIT, &
						"")

					ARRAY_LOOP% = &
						WINDOW_SCROLL::CUR_LINE
					GOSUB SaveFields
				ELSE
					ENTERCHANGE_LOOP% = 1%
				END IF
			END IF
			ITM$ = NUM1$(ENTERCHANGE_LOOP%)

		!
		! Good keys
		!
		CASE 0%, 10%, 12%, 13%, 87%, 73%, 65%, &
			69%, 70%, 87%, SMG$K_TRM_DO

		!
		! Bad Keys
		!
		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO ChangeLoop
		END SELECT

		GOTO Menu IF ITM$ = ""

 ChangeLoop1:
		WHEN ERROR IN
			ENTERCHANGE_LOOP% = VAL%(ITM$)
		USE
			CONTINUE 1200
		END WHEN

1210		GOTO ChangeLoop IF ENTERCHANGE_LOOP% < 0% OR &
			ENTERCHANGE_LOOP% > 5%

		SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, SPACE$(80%), 1%, 1%)

		GOSUB EnterChange

		SELECT SCOPE::SCOPE_EXIT
		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ

			GOSUB RestoreFields
			X% = DSPL_SCROLL(WINDOW_SCROLL, &
				LINES$(), 0%, "PAINT")
			ARRAY_LOOP% = WINDOW_SCROLL::CUR_LINE
			CALL ENTR_3MESSAGE(SCOPE, "Change aborted", 1%)
			GOTO Menu

		CASE SMG$K_TRM_DOWN

			ENTERCHANGE_LOOP% = ENTERCHANGE_LOOP% + 1%
			IF ENTERCHANGE_LOOP% > 5%
			THEN
				IF ARRAY_LOOP% < WINDOW_SCROLL::END_ELEMENT - 1%
				THEN
					ENTERCHANGE_LOOP% = 1%
					ARRAY_LOOP% = DSPL_SCROLL( &
						WINDOW_SCROLL, &
						LINES$(), SCOPE::SCOPE_EXIT, &
						"")

					ARRAY_LOOP% = &
						WINDOW_SCROLL::CUR_LINE
					GOSUB SaveFields
				ELSE
					ENTERCHANGE_LOOP% = 5%
				END IF
			END IF
			ITM$ = NUM1$(ENTERCHANGE_LOOP%)
			GOTO ChangeLoop1

		CASE SMG$K_TRM_UP

			ENTERCHANGE_LOOP% = ENTERCHANGE_LOOP% - 1%
			IF ENTERCHANGE_LOOP% < 1%
			THEN
				IF ARRAY_LOOP% > WINDOW_SCROLL::BEG_ELEMENT
				THEN
					ENTERCHANGE_LOOP% = 5%
					ARRAY_LOOP% = DSPL_SCROLL( &
						WINDOW_SCROLL, &
						LINES$(), SCOPE::SCOPE_EXIT, &
						"")

					ARRAY_LOOP% = &
						WINDOW_SCROLL::CUR_LINE
					GOSUB SaveFields
				ELSE
					ENTERCHANGE_LOOP% = 1%
				END IF
			END IF
			ITM$ = NUM1$(ENTERCHANGE_LOOP%)
			GOTO ChangeLoop1

		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO ChangeLoop
		END SELECT

		GOSUB SetUpArray

		GOTO ChangeLoop

	!
	! Delete a line in DDL
	!
	CASE "D"
 EraseLoop:	INP$ = ENTR_3YESNO(SCOPE, SMG_SCREEN_DATA%, &
			"", "Confirm deletion ", "", 0%, &
			"'E", "")

		SELECT SCOPE::SCOPE_EXIT
		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ

			GOTO Menu

		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO EraseLoop
		END SELECT

		GOTO Menu IF INP$ <> "Y" OR &
			ARRAY_LOOP% = WINDOW_SCROLL::END_ELEMENT

		FOR LOOP% = ARRAY_LOOP% TO WINDOW_SCROLL::BOT_ARRAY
			NAME$(LOOP%) = NAME$(LOOP% + 1%)
			DESC$(LOOP%) = DESC$(LOOP% + 1%)
			TTYPE$(LOOP%) = TTYPE$(LOOP% + 1%)
			SIZE$(LOOP%) = SIZE$(LOOP% + 1%)
			TTEST$(LOOP%) = TTEST$(LOOP% + 1%)
			LINES$(LOOP%) = LINES$(LOOP% + 1%)
		NEXT LOOP%

		WINDOW_SCROLL::BOT_ARRAY, WINDOW_SCROLL::END_ELEMENT = &
			WINDOW_SCROLL::BOT_ARRAY - 1%

		X% = DSPL_SCROLL(WINDOW_SCROLL, LINES$(), 0%, "PAINT")
		ARRAY_LOOP% = WINDOW_SCROLL::CUR_LINE

		SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

	!
	! Exit
	!
	CASE "X"

 FileLoop:	INP$ = ENTR_3YESNO(SCOPE, SMG_SCREEN_DATA%, "", &
			"Save the file - " + &
			FILE_NAME$ + " ? ", "", 0%, "'E", "")

		SELECT SCOPE::SCOPE_EXIT
		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ

			GOTO Menu

		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

		CASE ELSE

			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO FileLoop
		END SELECT

		GOSUB WriteOutArray IF INP$ = "Y"

		SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

		GOTO ExitProgram

	!
	! Edit the file
	!
	CASE "E"
		GOTO Menu IF ARRAY_LOOP% > WINDOW_SCROLL::END_ELEMENT - 1%

		X% = ENTR_3CHOICE(SCOPE, "6;31", "", HELP_CLASS$(), &
			"", 8%, "Help Types", "", 2%)

		GOTO Menu IF X% < 1% OR X% > 4%

		EDIT_FILE$ = PREFIX$ + HELP_CLASS_CODE$(X%) + &
			"_" + EDIT$(NAME$(ARRAY_LOOP%), 8% + 32% + 128%) + &
			SUFFIX$

8100		WHEN ERROR IN
			OPEN EDIT_FILE$ FOR INPUT AS FILE #EDIT.CH%, &
				ACCESS READ, ALLOW MODIFY
		USE
			CLOSE EDIT.CH%
			SELECT ERR
			CASE 2%	! Illegal file
				CALL ENTR_3MESSAGE(SCOPE, "Illegal file name!!!", 1%)
				CONTINUE Menu

			CASE 5%	! Cant find file
				CONTINUE 8110
			END SELECT

			EXIT HANDLER
		END WHEN

		CLOSE EDIT.CH%

8110		CALL ENTR_3MESSAGE(SCOPE, "Entering the editor 'EDT' " + &
			EDIT_FILE$, 1%)
		SLEEP 1%
		SELECT SCOPE::SCOPE_EXIT
		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ

			CALL ENTR_3MESSAGE(SCOPE, "Edit aborted", 1%)
			GOTO Menu

		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

		CASE ELSE

			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO 8110
		END SELECT

		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 0%)

		SMG_STATUS% = EDT$EDIT(EDIT_FILE$,,,,,,,)
		IF (SMG_STATUS% AND 1%) = 0%
		THEN
			CALL ENTR_3MESSAGE(SCOPE, "ERROR in edit!!!! " + &
				NUM1$(SMG_STATUS%), 0%)
			SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)
			GOTO Menu
		END IF

		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)

		SMG_STATUS% = SMG$BEGIN_PASTEBOARD_UPDATE(SCOPE::SMG_PBID)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

		SMG_STATUS% = SMG$REPAINT_SCREEN(SCOPE::SMG_PBID)

		SMG_STATUS% = SMG$END_PASTEBOARD_UPDATE(SCOPE::SMG_PBID)

	!
	! Help
	!
	CASE "H"
		CALL HELP_3MESSAGE(SCOPE, "", "PROG", &
			SCOPE::PRG_PROGRAM, "HELP")

	END SELECT

	GOTO Menu

 ExitProgram:
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$POP_VIRTUAL_DISPLAY(SMG_SCROLL, SCOPE::SMG_PBID)

	SCOPE::PRG_ITEM		= OLD_ITEM$
	SCOPE::PRG_PROGRAM	= OLD_PROGRAM$
	SCOPE::PRG_IDENT	= OLD_IDENT$

	EXIT SUB

 ReadInArray:
9000	!
	! Read in the possible Task Types
	!
	WHEN ERROR IN
		OPEN FILE_NAME$ FOR INPUT AS FILE #OLD.CH%, &
			ACCESS READ, ALLOW MODIFY
	USE
		CONTINUE 9500
	END WHEN

	ARRAY_LOOP% = 0%

9100	!
	! Read data element file
	!
	WHEN ERROR IN
		LINPUT #OLD.CH%, INP$
	USE
		CONTINUE 9500
	END WHEN

	INP$ = EDIT$(INP$, 4% + 8% + 16% + 128%)

	GOTO 9100 IF EDIT$(INP$, -1%) = ""

	IF RIGHT(INP$, LEN(INP$)) <> "."
	THEN
		WORK_LINE$ = WORK_LINE$ + INP$
		GOTO 9100
	END IF

	WORK_LINE$ = WORK_LINE$ + LEFT(INP$, LEN(INP$) - 1%)

	GOSUB ElementParse

	GOSUB SetUpArray

	GOTO 9100

9500	CLOSE OLD.CH%

	RETURN

 WriteOutArray:
10000	OPEN FILE_NAME$ FOR OUTPUT AS FILE #NEW.CH%

	PRINT #NEW.CH%, &
		EDIT$(NAME$(I%),  8% + 128%) + "<DESC>" + &
		EDIT$(DESC$(I%),  8% + 128%) + "<TYPE>" + &
		EDIT$(TTYPE$(I%), 8% + 128%) + "<SIZE>" + &
		EDIT$(SIZE$(I%),  8% + 128%) + "<TEST>" + &
		EDIT$(TTEST$(I%), 8% + 128%) + "." &
			FOR I% = WINDOW_SCROLL::BEG_ELEMENT TO &
			(WINDOW_SCROLL::END_ELEMENT - 1%)

	CLOSE NEW.CH%

	RETURN

 EnterChange:
11000	!***************************************************************
	! Enter and change data
	!***************************************************************
	TEMP_PRG_ITEM$ = SCOPE::PRG_ITEM

	SCOPE::PRG_ITEM = "FLD" + FORMAT$(ENTERCHANGE_LOOP%, "<0>##")

	SELECT ENTERCHANGE_LOOP%
	CASE 1%
		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCROLL, &
			MID$(LINES$(ARRAY_LOOP%), 1%, 17%),, &
			3%,, SMG$M_REVERSE)

		NAME$(ARRAY_LOOP%) = ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
			"", FUN_NAME$ + &
			"name", SPACE$(40%), 32%, "'E", &
			NAME$(ARRAY_LOOP%))

	CASE 2%
		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCROLL, &
			MID$(LINES$(ARRAY_LOOP%), 19%, 17%),, &
			21%,, SMG$M_REVERSE)

		DESC$(ARRAY_LOOP%) = ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
			"", FUN_NAME$ + &
			"description", SPACE$(50%), 32%, "'E", &
			DESC$(ARRAY_LOOP%))

	CASE 3%
		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCROLL, &
			MID$(LINES$(ARRAY_LOOP%), 37%, 15%),, &
			39%,, SMG$M_REVERSE)

		TTYPE$(ARRAY_LOOP%) = ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
			"", FUN_NAME$ + &
			"type", SPACE$(40%), 32%, "'E", &
			TTYPE$(ARRAY_LOOP%))

	CASE 4%
		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCROLL, &
			MID$(LINES$(ARRAY_LOOP%), 53%, 4%),, &
			55%,, SMG$M_REVERSE)

		SIZE$(ARRAY_LOOP%) = ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
			"", FUN_NAME$ + &
			"size", SPACE$(4%), 32%, "'E", &
			SIZE$(ARRAY_LOOP%))

	CASE 5%
		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCROLL, &
			MID$(LINES$(ARRAY_LOOP%), 58%, 17%),, &
			60%,, SMG$M_REVERSE)

		TTEST$(ARRAY_LOOP%) = ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
			"", FUN_NAME$ + &
			"test", SPACE$(50%), 32%, "'E", &
			TTEST$(ARRAY_LOOP%))
	END SELECT

	SELECT SCOPE::SCOPE_EXIT
	CASE SMG$K_TRM_F10,	SMG$K_TRM_CTRLZ

		GOTO EnterChange1
	END SELECT

 EnterChange1:
	GOSUB SetUpArray

	X% = DSPL_SCROLL(WINDOW_SCROLL, LINES$(), 0%, "PAINT")
	ARRAY_LOOP% = WINDOW_SCROLL::CUR_LINE

	SCOPE::PRG_ITEM = TEMP_PRG_ITEM$

	RETURN

	%PAGE

 SetUpArray:
	!
	! Set arrays into line to print
	LINES$(ARRAY_LOOP%) = &
		LEFT(NAME$(ARRAY_LOOP%)   + &
		STRING$(17% - LEN(NAME$(ARRAY_LOOP%)), 32%), 17%) + &
		" " + LEFT(DESC$(ARRAY_LOOP%)  + &
		STRING$(17% - LEN(DESC$(ARRAY_LOOP%)), 32%), 17%) + &
		" " + LEFT(TTYPE$(ARRAY_LOOP%)   + &
		STRING$(15% - LEN(TTYPE$(ARRAY_LOOP%)), 32%), 15%) + &
		" " + LEFT(SIZE$(ARRAY_LOOP%) + &
		STRING$(4% - LEN(SIZE$(ARRAY_LOOP%)), 32%), 4%) + &
		" " + LEFT(TTEST$(ARRAY_LOOP%)    + &
		STRING$(17% - LEN(TTEST$(ARRAY_LOOP%)), 32%), 17%)

	RETURN

 SaveFields:
	T.LINES$ = LINES$(ARRAY_LOOP%)
	T.NAME$ = NAME$(ARRAY_LOOP%)
	T.DESC$ = DESC$(ARRAY_LOOP%)
	T.TTYPE$ = TTYPE$(ARRAY_LOOP%)
	T.TTEST$ = TTEST$(ARRAY_LOOP%)
	T.SIZE$ = SIZE$(ARRAY_LOOP%)

	RETURN

 RestoreFields:
	LINES$(ARRAY_LOOP%) = T.LINES$
	NAME$(ARRAY_LOOP%) = T.NAME$
	DESC$(ARRAY_LOOP%) = T.DESC$
	TTYPE$(ARRAY_LOOP%) = T.TTYPE$
	TTEST$(ARRAY_LOOP%) = T.TTEST$
	SIZE$(ARRAY_LOOP%) = T.SIZE$

	RETURN

	%Page

 ElementParse:
	!
	! Parse the element description file
	!
	TEMP% = INSTR(1%, WORK_LINE$, "<")

	TEMP% = LEN(WORK_LINE$) + 1% IF TEMP% = 0%

	ARRAY_LOOP% = ARRAY_LOOP% + 1%

	NAME$(ARRAY_LOOP%) = EDIT$(LEFT(WORK_LINE$, TEMP% - 1%), -1%)

	TEMP$ = "<DESC>"
	TEMP% = INSTR(1%, WORK_LINE$, TEMP$)
	IF TEMP%
	THEN
		TEMP1% = INSTR(TEMP% + 1%, WORK_LINE$, "<")
		TEMP1% = LEN(WORK_LINE$) + 1% IF TEMP1% = 0%
		DESC$(ARRAY_LOOP%) = TRM$(MID(WORK_LINE$, TEMP% + LEN(TEMP$), &
			TEMP1% - (TEMP% + LEN(TEMP$))))
	END IF

	TEMP$ = "<SIZE>"
	TEMP% = INSTR(1%, WORK_LINE$, TEMP$)
	IF TEMP%
	THEN
		TEMP1% = INSTR(TEMP% + 1%, WORK_LINE$, "<")
		TEMP1% = LEN(WORK_LINE$) + 1% IF TEMP1% = 0%
		SIZE$(ARRAY_LOOP%) = TRM$(MID(WORK_LINE$, TEMP% + LEN(TEMP$), &
			TEMP1% - (TEMP% + LEN(TEMP$))))
	END IF

	TEMP$ = "<TEST>"
	TEMP% = INSTR(1%, WORK_LINE$, TEMP$)
	IF TEMP%
	THEN
		TEMP1% = INSTR(TEMP% + 1%, WORK_LINE$, "<")
		TEMP1% = LEN(WORK_LINE$) + 1% IF TEMP1% = 0%
		TTEST$(ARRAY_LOOP%) = TRM$(MID(WORK_LINE$, TEMP% + LEN(TEMP$), &
			TEMP1% - (TEMP% + LEN(TEMP$))))
	END IF

	TEMP$ = "<TYPE>"
	TEMP% = INSTR(1%, WORK_LINE$, TEMP$)
	IF TEMP%
	THEN
		TEMP1% = INSTR(TEMP% + 1%, WORK_LINE$, "<")
		TEMP1% = LEN(WORK_LINE$) + 1% IF TEMP1% = 0%
		TTYPE$(ARRAY_LOOP%) = TRM$(MID(WORK_LINE$, TEMP% + LEN(TEMP$), &
			TEMP1% - (TEMP% + LEN(TEMP$))))
	END IF

	WORK_LINE$ = ""

	RETURN

	%Page

19000	!======================================================================
	! Error Trapping
	!======================================================================

	CALL ENTR_3MESSAGE(SCOPE, &
		"TK_MAIN_ELEMTPLATE-ERR:  " + NUM1$(ERR) + &
		" at line " + NUM1$(ERL), 0%)

	ON ERROR GOTO 0

32767	END SUB
