1	%TITLE "Sub Process to Create a Ddl File"
	%SBTTL "TK_MAIN_DDL"
	%IDENT "V3.6a Calico"

	SUB TK_MAIN_DDL(STRING PAR_DIR, &
		STRING PAR_DATABASE, &
		STRING PAR_RECSTRUCT, &
		STRING PAR_RECDESC)

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
	!	A process to aid in the creation of a ddl file.
	!
	! Index:
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_MAIN_DDL/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP TK_MAIN_DDL
	!	$ DELETE TK_MAIN_DDL.OBJ;*
	!
	! AUTHOR:
	!
	!	05/01/87 - Robert Peterson
	!
	! MODIFICATION HISTORY:
	!
	!	05/31/90 - Kevin Handy
	!		Modified to allow only upper case input in several
	!		of the fields.
	!
	!	08/05/91 - Kevin Handy
	!		Modified to use ACCESS READ on opens so they don't
	!		mark files as having been modified.
	!
	!	09/17/91 - Kevin Handy
	!		Modified so that it will not change descriptions
	!		when you change the ELEMENT if there is already a
	!		description there.  Makes it easier to add ELEMENTS
	!		to existing files.
	!
	!	06/11/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/29/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/28/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/08/99 - Kevin Handy
	!		Use BASIC$STARLET for LIB$ routines
	!
	!	04/12/99 - Kevin Handy
	!		Don't disable broadast trapping
	!
	!	11/22/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "$LIBDEF" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	%INCLUDE "FUNC_INCLUDE:TK_WINDOW.INC"

	%INCLUDE "SOURCE:[SMG.OPEN]SMG_SCROLL.HB"
	DECLARE  SMG_SCROLL_CDD LOC_WINDOW

	%INCLUDE "SOURCE:[SMG.OPEN]SMG_DDL.HB"
	DECLARE  SMG_DDL_CDD LOC_DDL

	%INCLUDE "SOURCE:[TK.OPEN]TK_ELEMENT.HB"
	MAP (TK_ELEMENT)	TK_ELEMENT_CDD	TK_ELEMENT

	EXTERNAL STRING FUNCTION TK_MAIN_TEXTFILEWINDOW
	EXTERNAL LONG	FUNCTION DSPL_SCROLL
	EXTERNAL LONG	FUNCTION MAIN_WINDOW
	EXTERNAL LONG   FUNCTION LIBR_MAINT

	TEMP_IDENT$ = SCOPE::PRG_IDENT
	TEMP_PROGRAM$ = SCOPE::PRG_PROGRAM
	TEMP_ITEM$ = SCOPE::PRG_ITEM

	SCOPE::PRG_IDENT = "FUNC"
	SCOPE::PRG_PROGRAM = "TK_MAIN_DDL"
	SCOPE::PRG_ITEM = ""

	!
	! Declare vars
	!
	DECLARE LONG SMG_SCROLL
	DECLARE LONG TEXT_FILE.CH

	DIM	LOC_LINES$(200%), &
		ATTRIBUTE_TYPE$(100%), &
		DATA_TYPE$(100%), &
		DATA_FIELD_SIZE$(100%)

	ATTRIBUTE_TYPE_FILE$ = "CMC:TK_ATTRIBUTE_TYPE.TEMPLATE"
	DATA_TYPE_FILE$ = "CMC:TK_DATA_TYPE.TEMPLATE"
	PROTO_PAR_RECSTRUCT$ = "CMC:TK_*.TEMPLATE"
	PROTO_PREFIX$ = "CMC:TK_"
	PROTO_SUFFIX$ = ".TEMPLATE"

100	!
	! Get the channels from VMS
	!
	SMG_STATUS% = LIB$GET_LUN(TEXT_FILE.CH)
	GOTO ErrorGetCh IF SMG_STATUS% = LIB$_INSLUN
	GOTO 200

 ErrorGetCh:
	CALL ENTR_3MESSAGE(SCOPE, &
		"No free channels from VMS:  " + NUM1$(SMG_STATUS%), 0%)
	GOTO ExitProgram

200	GOSUB LoadTemplateArrays

500	!
	! Get the ddl file
	!
	CALL TK_SUBR_DDLEXTRACT(LOC_DDL, &
		PAR_DATABASE, &
		PAR_RECSTRUCT, &
		FDE_STATUS%)

	LOC_DEGREE% = LOC_DDL::FIELD_NUM

	LOC_DEGREE% = 0% IF LOC_DEGREE% < 0%

600	!
	! Paint DDL window
	!
	CLOSE #TEXT_FILE.CH

	GOSUB SetUpArray FOR ARRAY_LOOP% = 1% TO LOC_DEGREE%

	!
	! Create the data display
	!
	!
	! Create the window
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(10%, 76%, SMG_SCROLL, &
		SMG$M_BORDER)

	SMG_STATUS% = SMG$LABEL_BORDER(	SMG_SCROLL, &
		"Record Structure Window", SMG$K_TOP,, SMG$M_BOLD)

	TITLE$ = "   (01)        (02)       (03)      (04)    (05) " + &
			" (06)  "
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCROLL, TITLE$, 1%, 3%)

	TITLE$ = "Field name Data element Attribute Data type Size " + &
			"Comment"

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCROLL, TITLE$, 2%, 3%)

	SMG_STATUS% = SMG$DRAW_LINE(SMG_SCROLL, 3%, 1%, 3%, 74%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_SCROLL, SCOPE::SMG_PBID, &
		9%, 3%)

	!
	! Define windows
	!
	LOC_WINDOW::WINDOW	= SMG_SCROLL
	LOC_WINDOW::TOP_ARRAY	= 1%
	LOC_WINDOW::BOT_ARRAY	= LOC_DEGREE% + 1%
	LOC_WINDOW::SCROLL_TOP	= 4%
	LOC_WINDOW::SCROLL_BOT	= 10%
	LOC_WINDOW::TOP_LINE	= 1%
	LOC_WINDOW::BEG_ELEMENT	= 1%
	LOC_WINDOW::END_ELEMENT	= LOC_DEGREE% + 1%
	LOC_WINDOW::CUR_LINE	= 1%
	LOC_WINDOW::CUR_W_ROW	= 1%
	LOC_WINDOW::CUR_W_COL	= 1%
	LOC_WINDOW::FIND_LINE	= 1%
	LOC_WINDOW::SMG_FLAG	= 2%
	LOC_WINDOW::PROMPT	= "->"
	LOC_WINDOW::VIDEO_COMP	= 0%
	LOC_WINDOW::CHARSET	= 0%
	LOC_WINDOW::DRAW_COLS	= "013,026,036,046,051"

	!
	! Print the array
	!
	TEMP% = DSPL_SCROLL(LOC_WINDOW, LOC_LINES$(), 0%, "PAINT")

1000	!******************************************************************
	! DDL maintenance window
	!******************************************************************
	SCOPE::PRG_ITEM = ""

	OPT$ = ENTR_3OPTION(SCOPE, "COMMAND", &
		"Add Change Erase Proto_tables Field_definition Help eXit", &
		OPT%, FLAGW%)

	SELECT SCOPE::SCOPE_EXIT
	CASE SMG$K_TRM_UP,		SMG$K_TRM_DOWN, &
		SMG$K_TRM_PREV_SCREEN,	SMG$K_TRM_NEXT_SCREEN, &
		SMG$K_TRM_F18,		SMG$K_TRM_F19

		V% = DSPL_SCROLL(LOC_WINDOW, LOC_LINES$(), &
			SCOPE::SCOPE_EXIT, "PAINT")

		GOTO 1000

	CASE SMG$K_TRM_F10,		SMG$K_TRM_CTRLZ

		GOTO ExitCompile

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 1000

	END SELECT

	ARRAY_LOOP% = LOC_WINDOW::CUR_LINE

	SELECT OPT$
	!
	! Add a new field to DDL
	!
	CASE "A"
		FUN_NAME$ = "Add"
 NextEntry:
		LOC_LINES$(LOOP% + 1%) = LOC_LINES$(LOOP%) &
			FOR LOOP% = LOC_WINDOW::BOT_ARRAY TO ARRAY_LOOP% &
				STEP -1%

		LOC_WINDOW::BOT_ARRAY, LOC_WINDOW::END_ELEMENT = &
			LOC_WINDOW::BOT_ARRAY + 1%

		LOC_LINES$(ARRAY_LOOP%) = ""

		X% = DSPL_SCROLL(LOC_WINDOW, LOC_LINES$(), 0%, "PAINT")

		FIELD_NAME$ = ""
		FIELD_ATTRIBUTE$ = ""
		FIELD_TYPE$ = ""
		FIELD_ELEMENT$ = ""
		FIELD_SIZE$ = ""
		FIELD_DESC$ = ""

		FOR ENTERCHANGE_LOOP% = 1% TO 6%
 EnterLoop:
1100			!******************************************
			! Enter data to screen
			!******************************************
			GOSUB EnterChange

			SELECT SCOPE::SCOPE_EXIT
			CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ

				LOC_LINES$(LOOP%) = LOC_LINES$(LOOP% + 1%) &
					FOR LOOP% = ARRAY_LOOP% TO LOC_WINDOW::BOT_ARRAY

				LOC_LINES$(LOC_WINDOW::BOT_ARRAY) = ""

				X% = DSPL_SCROLL(LOC_WINDOW, &
					LOC_LINES$(), 0%, "PAINT")

				LOC_WINDOW::BOT_ARRAY, LOC_WINDOW::END_ELEMENT = &
					LOC_WINDOW::BOT_ARRAY - 1%
				CALL ENTR_3MESSAGE(SCOPE, "Add aborted", 1%)
				GOTO 1000

			CASE SMG$K_TRM_DOWN

				ENTERCHANGE_LOOP% = ENTERCHANGE_LOOP% + 1%
				ENTERCHANGE_LOOP% = 6% IF ENTERCHANGE_LOOP% > 6%
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

		FOR LOOP% = LOC_WINDOW::BOT_ARRAY TO ARRAY_LOOP% STEP -1%
			LOC_DDL::FIELD_NAME(LOOP% + 1%) = LOC_DDL::FIELD_NAME(LOOP%)
			LOC_DDL::FIELD_ATTRIBUTE(LOOP% + 1%) = LOC_DDL::FIELD_ATTRIBUTE(LOOP%)
			LOC_DDL::FIELD_TYPE(LOOP% + 1%) = LOC_DDL::FIELD_TYPE(LOOP%)
			LOC_DDL::FIELD_ELEMENT(LOOP% + 1%) = LOC_DDL::FIELD_ELEMENT(LOOP%)
			LOC_DDL::FIELD_SIZE(LOOP% + 1%) = LOC_DDL::FIELD_SIZE(LOOP%)
			LOC_DDL::FIELD_DESC(LOOP% + 1%) = LOC_DDL::FIELD_DESC(LOOP%)
		NEXT LOOP%

		LOC_DDL::FIELD_NAME(ARRAY_LOOP%)	= FIELD_NAME$
		LOC_DDL::FIELD_ATTRIBUTE(ARRAY_LOOP%)	= FIELD_ATTRIBUTE$
		LOC_DDL::FIELD_TYPE(ARRAY_LOOP%)	= FIELD_TYPE$
		LOC_DDL::FIELD_ELEMENT(ARRAY_LOOP%)	= FIELD_ELEMENT$
		LOC_DDL::FIELD_SIZE(ARRAY_LOOP%)	= FIELD_SIZE$
		LOC_DDL::FIELD_DESC(ARRAY_LOOP%)	= FIELD_DESC$

		GOSUB SetUpArray

		LOC_WINDOW::FIND_LINE = LOC_WINDOW::CUR_LINE + 1%

		ARRAY_LOOP% = DSPL_SCROLL(LOC_WINDOW, LOC_LINES$(), 0%, &
			"FIND")

		ARRAY_LOOP% = LOC_WINDOW::CUR_LINE

		GOTO NextEntry

	CASE "C"	! Change a field

		FUN_NAME$ = "Change "

		IF ARRAY_LOOP% = LOC_WINDOW::BOT_ARRAY
		THEN
			CALL ENTR_3MESSAGE(SCOPE, "Line must be added before a " + &
				"change can be made", 0%)
			GOTO 1000
		END IF

		FIELD_NAME$ = LOC_DDL::FIELD_NAME(ARRAY_LOOP%)
		FIELD_ATTRIBUTE$ = LOC_DDL::FIELD_ATTRIBUTE(ARRAY_LOOP%)
		FIELD_TYPE$ = LOC_DDL::FIELD_TYPE(ARRAY_LOOP%)
		FIELD_ELEMENT$ = LOC_DDL::FIELD_ELEMENT(ARRAY_LOOP%)
		FIELD_SIZE$ = LOC_DDL::FIELD_SIZE(ARRAY_LOOP%)
		FIELD_DESC$ = LOC_DDL::FIELD_DESC(ARRAY_LOOP%)
		ENTERCHANGE_LOOP% = 0%

 ChangeLoop:
1200		!****************************************************
		! Change data to screen
		!****************************************************
		ITM$ = EDIT$(ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
			"", "Item to change", "  ", 4%, "", ""), -1%)
		ITM$ = "0" + ITM$ IF LEN(ITM$) < 2% AND ITM$ <> ""

		SELECT SCOPE::SCOPE_EXIT
		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ

			GOSUB SetUpArray
			X% = DSPL_SCROLL(LOC_WINDOW, LOC_LINES$(), 0%, "PAINT")
			CALL ENTR_3MESSAGE(SCOPE, "Change aborted", 1%)
			GOTO 1000

		CASE SMG$K_TRM_DOWN

			ENTERCHANGE_LOOP% = ENTERCHANGE_LOOP% + 1%
			ENTERCHANGE_LOOP% = 6% IF ENTERCHANGE_LOOP% > 6%
			ITM$ = NUM1$(ENTERCHANGE_LOOP%)

		CASE SMG$K_TRM_UP

			ENTERCHANGE_LOOP% = 7% IF ENTERCHANGE_LOOP% = 0%
			ENTERCHANGE_LOOP% = ENTERCHANGE_LOOP% - 1%
			ENTERCHANGE_LOOP% = 1% IF ENTERCHANGE_LOOP% < 1%
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

		IF ITM$ = ""
		THEN
			LOC_DDL::FIELD_NAME(ARRAY_LOOP%) = FIELD_NAME$
			LOC_DDL::FIELD_ATTRIBUTE(ARRAY_LOOP%)= FIELD_ATTRIBUTE$
			LOC_DDL::FIELD_TYPE(ARRAY_LOOP%) = FIELD_TYPE$
			LOC_DDL::FIELD_ELEMENT(ARRAY_LOOP%) = FIELD_ELEMENT$
			LOC_DDL::FIELD_SIZE(ARRAY_LOOP%) = FIELD_SIZE$
			LOC_DDL::FIELD_DESC(ARRAY_LOOP%) = FIELD_DESC$

			GOSUB SetUpArray

			GOTO 1000
		END IF

 ChangeLoop1:
		WHEN ERROR IN
			ENTERCHANGE_LOOP% = VAL%(ITM$)
		USE
			CONTINUE 1200
		END WHEN

1210		GOTO ChangeLoop IF ENTERCHANGE_LOOP% < 0% OR &
			ENTERCHANGE_LOOP% > 6%

		GOSUB EnterChange

		SELECT SCOPE::SCOPE_EXIT
		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ

			GOSUB SetUpArray
			X% = DSPL_SCROLL(LOC_WINDOW, LOC_LINES$(), 0%, "PAINT")
			CALL ENTR_3MESSAGE(SCOPE, "Change aborted", 1%)
			GOTO 1000

		CASE SMG$K_TRM_DOWN

			ENTERCHANGE_LOOP% = ENTERCHANGE_LOOP% + 1%
			ENTERCHANGE_LOOP% = 6% IF ENTERCHANGE_LOOP% > 6%
			ITM$ = NUM1$(ENTERCHANGE_LOOP%)
			GOTO ChangeLoop1

		CASE SMG$K_TRM_UP

			ENTERCHNAGE_LOOP% = 7% IF ENTERCHANGE_LOOP% = 0%
			ENTERCHANGE_LOOP% = ENTERCHANGE_LOOP% - 1%
			ENTERCHANGE_LOOP% = 1% IF ENTERCHANGE_LOOP% < 1%
			ITM$ = NUM1$(ENTERCHANGE_LOOP%)
			GOTO ChangeLoop1

		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

		CASE ELSE

			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO ChangeLoop

		END SELECT

		GOTO ChangeLoop

	!
	! Erase a line in DDL
	!
	CASE "E"
 EraseLoop:
		IF ARRAY_LOOP% = LOC_WINDOW::BOT_ARRAY
		THEN
			CALL ENTR_3MESSAGE(SCOPE, "Line cannot be erased",0%)
			GOTO 1000
		END IF

		INP$ = ENTR_3YESNO(SCOPE, SMG_SCREEN_DATA%, &
			"", "Confirm deletion then press <DO> ", &
			"", 0%, "'E", "")

		SELECT SCOPE::SCOPE_EXIT
		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ

			GOTO 1000

		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

		CASE ELSE

			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO EraseLoop
		END SELECT

		GOTO 1000 IF INP$ <> "Y"

		FOR LOOP% = ARRAY_LOOP%  TO LOC_WINDOW::BOT_ARRAY
			LOC_DDL::FIELD_NAME(LOOP%)	= &
				LOC_DDL::FIELD_NAME(LOOP% + 1%)
			LOC_DDL::FIELD_ATTRIBUTE(LOOP%)	= &
				LOC_DDL::FIELD_ATTRIBUTE(LOOP% + 1%)
			LOC_DDL::FIELD_TYPE(LOOP%)	= &
				LOC_DDL::FIELD_TYPE(LOOP% + 1%)
			LOC_DDL::FIELD_ELEMENT(LOOP%)	= &
				LOC_DDL::FIELD_ELEMENT(LOOP% + 1%)
			LOC_DDL::FIELD_SIZE(LOOP%)	= &
				LOC_DDL::FIELD_SIZE(LOOP% + 1%)
			LOC_DDL::FIELD_DESC(LOOP%)	= &
				LOC_DDL::FIELD_DESC(LOOP% + 1%)
			LOC_LINES$(LOOP%) = LOC_LINES$(LOOP% + 1%)
		NEXT LOOP%

		LOC_WINDOW::BOT_ARRAY, LOC_WINDOW::END_ELEMENT = &
			LOC_WINDOW::BOT_ARRAY - 1%

		X% = DSPL_SCROLL(LOC_WINDOW, LOC_LINES$(), 0%, "PAINT")

	CASE "P"	! Maintain Prototyping file template Tables

		ZX$ = TK_MAIN_TEXTFILEWINDOW("10;4", "10;30", &
			PROTO_PAR_RECSTRUCT$, PROTO_PREFIX$, &
			PROTO_SUFFIX$, "Prototyping Tables", 16%)

		GOSUB LoadTemplateArrays

	CASE "F"	! Document fields
		!*****************************************
		! Look up field names
		!*****************************************
		LOC_DDL::FIELD_NAME(0%) = &
			NUM1$(LOC_WINDOW::BOT_ARRAY - 1%)

 FieldNames:
		X% = ENTR_3CHOICE(SCOPE, "", "", LOC_DDL::FIELD_NAME(), &
			"", 8% + 64%, "Field Name", "", 0%)

		SELECT SCOPE::SCOPE_EXIT
		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ

			GOTO 1000

		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

			IF X% > 0%
			THEN
				SMG_STATUS% = LIBR_MAINT( &
					"REF:HELP_" + LEFT(PAR_RECSTRUCT, 2%), &
					TRM$(LOC_DDL::FIELD_NAME(X%)), &
					"Field Documentation", 0%)

 !				SMG_STATUS% = SMG$DISABLE_BROADCAST_TRAPPING( &
 !					SCOPE::SMG_PBID)
			END IF

		CASE ELSE

			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		END SELECT

		GOTO FieldNames

	!
	! Exit
	!
	CASE "X"

		GOTO ExitCompile

	!
	! Help
	!
	CASE "H"

		CALL HELP_3MESSAGE(SCOPE, "", "PROG", SCOPE::PRG_PROGRAM, "HELP")

	END SELECT

	GOTO 1000

 ExitCompile:
	SCOPE::PRG_ITEM = "COMPILE"

	TEMP$ = "Confirm storing changes then press <Do> "

	YESNO$ = ENTR_3YESNO(SCOPE, SMG_SCREEN_DATA%, "", TEMP$, "N", 0%, "", "")

	SELECT SCOPE::SCOPE_EXIT
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ

		GOTO 1000

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
		GOTO ExitCompile

	END SELECT

	GOTO ExitProgram IF YESNO$ <> "Y"

	LOC_DDL::FIELD_NUM	= LOC_WINDOW::BOT_ARRAY - 1%
	LOC_DDL::DESCR		= PAR_RECDESC

	CALL TK_SUBR_DDLCOMPILE(LOC_DDL, PAR_DATABASE, PAR_RECSTRUCT)

 ExitProgram:

	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SCROLL)

	CLOSE #TEXT_FILE.CH

	SMG_STATUS% = LIB$FREE_LUN(TEXT_FILE.CH)

	CALL ENTR_3MESSAGE(SCOPE, "Error " + NUM1$(SMG_STATUS%) + " has occured",0%) &
		IF (SMG_STATUS% AND 1%) = 0%

	SCOPE::PRG_ITEM = TEMP_ITEM$
	SCOPE::PRG_PROGRAM = TEMP_PROGRAM$
	SCOPE::PRG_IDENT = TEMP_IDENT$

	EXIT SUB

 EnterChange:
11000	!***************************************************************
	! Enter and change data
	!***************************************************************

	TEMP_ITEM$ = SCOPE::PRG_ITEM

	SCOPE::PRG_ITEM = "FLD" + FORMAT$(ENTERCHANGE_LOOP%, "<0>##")

	SELECT ENTERCHANGE_LOOP%
	CASE 1%

		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCROLL, &
			MID$(LOC_LINES$(ARRAY_LOOP%), 1%, 10%),, &
			3%,, SMG$M_REVERSE)

		FIELD_NAME$ = ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
			"", FUN_NAME$ + " name ", &
			SPACE$(20%), 16% + 32%, "'E", FIELD_NAME$)

	CASE 2%

		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCROLL, &
			MID$(LOC_LINES$(ARRAY_LOOP%), 12%, 12%),, &
			14%,, SMG$M_REVERSE)

		FIELD_ELEMENT$ = ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
			"", FUN_NAME$ + " element ", &
			SPACE$(20%), 16% + 32%, "'E", FIELD_ELEMENT$)

	CASE 3%

		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCROLL, &
			MID$(LOC_LINES$(ARRAY_LOOP%), 25%, 9%),, &
			27%,, SMG$M_REVERSE)

		FIELD_ATTRIBUTE$ = ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
			"", FUN_NAME$ + " Attribute ", &
			SPACE$(20%), 16% + 32%, "'E", FIELD_ATTRIBUTE$)

	CASE 4%

		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCROLL, &
			MID$(LOC_LINES$(ARRAY_LOOP%), 35%, 9%),, &
			37%,, SMG$M_REVERSE)

		FIELD_TYPE$ = ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
			"", FUN_NAME$ + " type ", &
			SPACE$(20%), 16% + 32%, "'E", FIELD_TYPE$)

	CASE 5%

		IF FIELD_TYPE$ = "TEXT"
		THEN
			SMG_STATUS% = SMG$PUT_CHARS(SMG_SCROLL, &
				MID$(LOC_LINES$(ARRAY_LOOP%), 45%, 4%),, &
				47%,, SMG$M_REVERSE)

			FIELD_SIZE$ = ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
				"", FUN_NAME$ + " size ", &
				SPACE$(4%), 32% + 2%, "'E", FIELD_SIZE$)
		END IF

	CASE 6%

		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCROLL, &
			MID$(LOC_LINES$(ARRAY_LOOP%), 50%, 23%),, &
			52%,, SMG$M_REVERSE)

		FIELD_DESC$ = ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
			"", FUN_NAME$ + " description ", &
			SPACE$(40%), 32%, "'E", FIELD_DESC$)
	END SELECT

	SELECT SCOPE::SCOPE_EXIT
	CASE SMG$K_TRM_F10,	SMG$K_TRM_CTRLZ, &
		SMG$K_TRM_DOWN,	SMG$K_TRM_UP

		GOTO EnterChange2

	CASE SMG$K_TRM_F14,	SMG$K_TRM_F17

		GOTO EnterChange1

	END SELECT

	SELECT ENTERCHANGE_LOOP%
	CASE 1%

		FOR LOOP% = 1% TO LOC_WINDOW::BOT_ARRAY
			IF LOOP% <> LOC_WINDOW::CUR_LINE
			THEN
				IF EDIT$(FIELD_NAME$, -1%) = &
					EDIT$(LOC_DDL::FIELD_NAME(LOOP%), -1%)
				THEN
					CALL ENTR_3MESSAGE(SCOPE, "Duplicate field name.",0%)
					GOTO EnterChange
				END IF
			END IF
		NEXT LOOP%

	CASE 2%

		SCOPE::PRG_IDENT = "PROG"
		IF FIELD_ELEMENT$ <> "" AND &
			MAIN_WINDOW(TK_MAIN_ELEMENT.ID, &
			"Q0" + FIELD_ELEMENT$) = 1%
		THEN
			FIELD_TYPE$ = TK_ELEMENT::ETYPE &
				IF TRM$(TK_ELEMENT::ETYPE) <> ""
			FIELD_SIZE$ = NUM1$(TK_ELEMENT::ESIZE)
			FIELD_DESC$ = TK_ELEMENT::DESCR &
				IF TRM$(TK_ELEMENT::DESCR) <> "" AND &
				FIELD_DESC$ = ""
		END IF
		SCOPE::PRG_IDENT = "FUNC"

	CASE 3%

		IF FIELD_ATTRIBUTE$ <> ""
		THEN
			TEMP% = 0%
			FOR LOOP% = 1% TO ATTRIBUTE_TYPE%
				TEMP% = -1% IF INSTR(1%, FIELD_ATTRIBUTE$, &
					ATTRIBUTE_TYPE$(LOOP%))
			NEXT LOOP%
			SCOPE::SCOPE_EXIT = SMG$K_TRM_F14 IF TEMP% = 0%
		END IF

	CASE 4%

		TEMP% = 0%
		FOR LOOP% = 1% TO DATA_TYPE%
			IF DATA_TYPE$(LOOP%) = FIELD_TYPE$
			THEN
				TEMP% = -1%
				FIELD_SIZE$ = DATA_FIELD_SIZE$(LOOP%) &
					IF FIELD_TYPE$ <> "TEXT"
			END IF
		NEXT LOOP%
		SCOPE::SCOPE_EXIT = SMG$K_TRM_F14 IF TEMP% = 0%

	END SELECT

 EnterChange1:
	SELECT SCOPE::SCOPE_EXIT
	CASE SMG$K_TRM_F14

		SCOPE::SCOPE_EXIT = 0%

		SELECT ENTERCHANGE_LOOP%
		CASE 2%

			SCOPE::PRG_IDENT = "PROG"
			FIELD_ELEMENT$ = TK_ELEMENT::ELEMENT &
				IF MAIN_WINDOW(TK_MAIN_ELEMENT.ID, "VX   ") = 1%
			SCOPE::PRG_IDENT = "FUNC"
			GOTO EnterChange

		CASE 3%

			!*****************************************
			! Look up attributes
			!*****************************************
			X% = ENTR_3CHOICE(SCOPE, "", "", ATTRIBUTE_TYPE$(), &
				FIELD_ATTRIBUTE$, 8% + 64%, &
				"ATTRIBUTES", "", 0%)

			GOTO EnterChange IF X% <= 0%

			FIELD_ATTRIBUTE$ = ATTRIBUTE_TYPE$(X%)
			GOTO EnterChange

		CASE 4%

			!*****************************************
			! Look up data types
			!*****************************************
			X% = ENTR_3CHOICE(SCOPE, "", "", DATA_TYPE$(), &
				FIELD_TYPE$, 8% + 64%, "DATA TYPES", "", 0%)

			GOTO EnterChange IF X% <= 0%

			FIELD_TYPE$ = DATA_TYPE$(X%)
			GOTO EnterChange

		END SELECT

	CASE SMG$K_TRM_F17

		IF ENTERCHANGE_LOOP% = 2%
		THEN
			SCOPE::PRG_IDENT = "PROG"
			V% = MAIN_WINDOW(TK_MAIN_ELEMENT.ID, "M   ")
			SCOPE::PRG_IDENT = "FUNC"
			GOTO EnterChange
		END IF

	END SELECT

 EnterChange2:
	LOC_LINES$(ARRAY_LOOP%) = &
		LEFT(FIELD_NAME$ + STRING$(10% - &
		LEN(FIELD_NAME$), 32%), 10%) + &
		" " + &
		LEFT(FIELD_ELEMENT$ + STRING$(12% - &
		LEN(FIELD_ELEMENT$), 32%), 12%) + &
		" " + &
		LEFT(FIELD_ATTRIBUTE$ + STRING$(9% - &
		LEN(FIELD_ATTRIBUTE$), 32%), 9%) + &
		" " + &
		LEFT(FIELD_TYPE$ + STRING$(9% - &
		LEN(FIELD_TYPE$), 32%), 9%) + &
		" " + &
		FORMAT$(VAL%(FIELD_SIZE$), "#### ") + &
		LEFT(FIELD_DESC$ + STRING$(23% - &
		LEN(FIELD_DESC$), 32%), 23%)

	X% = DSPL_SCROLL(LOC_WINDOW, LOC_LINES$(), 0%, "PAINT")

	SCOPE::PRG_ITEM = TEMP_ITEM$

	RETURN

	%PAGE

 SetUpArray:
	!
	! Set arrays into line to print
	LOC_LINES$(ARRAY_LOOP%) = &
		LEFT(LOC_DDL::FIELD_NAME(ARRAY_LOOP%) + STRING$(10% - &
		LEN(LOC_DDL::FIELD_NAME(ARRAY_LOOP%)), 32%), 10%) + &
		" " + &
		LEFT(LOC_DDL::FIELD_ELEMENT(ARRAY_LOOP%) + STRING$(12% - &
		LEN(LOC_DDL::FIELD_ELEMENT(ARRAY_LOOP%)), 32%), 12%) + &
		" " + &
		LEFT(LOC_DDL::FIELD_ATTRIBUTE(ARRAY_LOOP%) + STRING$(9% - &
		LEN(LOC_DDL::FIELD_ATTRIBUTE(ARRAY_LOOP%)), 32%), 9%) + &
		" " + &
		LEFT(LOC_DDL::FIELD_TYPE(ARRAY_LOOP%) + STRING$(9% - &
		LEN(LOC_DDL::FIELD_TYPE(ARRAY_LOOP%)), 32%), 9%) + &
		" " + &
		FORMAT$(VAL%(LOC_DDL::FIELD_SIZE(ARRAY_LOOP%)), "#### ") + &
		LEFT(LOC_DDL::FIELD_DESC(ARRAY_LOOP%) + STRING$(23% - &
		LEN(LOC_DDL::FIELD_DESC(ARRAY_LOOP%)), 32%), 23%)

	RETURN

	%Page

 LoadTemplateArrays:
	CALL ENTR_3MESSAGE(SCOPE, "Loading Prototyping file templates", 1%)

12000	!
	! Open attribute type template file
	!
	CLOSE TEXT_FILE.CH

	WHEN ERROR IN
		OPEN ATTRIBUTE_TYPE_FILE$ FOR INPUT AS FILE TEXT_FILE.CH, &
			ACCESS READ, ALLOW MODIFY, &
			RECORDSIZE 132%
	USE
		CONTINUE 12020
	END WHEN

	ATTRIBUTE_TYPE% = 0%

12010	!
	! Read attribute type file
	!
	WHEN ERROR IN
		LINPUT #TEXT_FILE.CH, INP$
	USE
		CONTINUE 12020
	END WHEN

	INP$ = EDIT$(INP$, -1%)

	IF INP$ <> ""
	THEN
		ATTRIBUTE_TYPE% = ATTRIBUTE_TYPE% + 1%
		ATTRIBUTE_TYPE$(ATTRIBUTE_TYPE%) = INP$
	END IF

	GOTO 12010

12020	!
	! Store number of elements in array
	!
	ATTRIBUTE_TYPE$(0%) = NUM1$(ATTRIBUTE_TYPE%)

	!
	! Open data type template file
	!
	CLOSE TEXT_FILE.CH

	WHEN ERROR IN
		OPEN DATA_TYPE_FILE$ FOR INPUT AS FILE TEXT_FILE.CH, &
			ACCESS READ, ALLOW MODIFY, &
			RECORDSIZE 132%
	USE
		CONTINUE 12040
	END WHEN

	DATA_TYPE% = 0%

12030	!
	! Read data type file
	!
	WHEN ERROR IN
		LINPUT #TEXT_FILE.CH, INP$
	USE
		CONTINUE 12040
	END WHEN

	INP$ = EDIT$(INP$, 4% + 8% + 16% + 32% + 128%)

	IF INP$ <> ""
	THEN
		TEMP% = INSTR(1%, INP$, "SIZE>")
		IF TEMP%
		THEN
			DATA_TYPE% = DATA_TYPE% + 1%
			DATA_FIELD_SIZE$(DATA_TYPE%) = RIGHT(INP$, TEMP% + 5%)
			DATA_TYPE$(DATA_TYPE%) = TRM$(LEFT(INP$, TEMP% - 1%))
		END IF
	END IF

	GOTO 12030

12040	!
	! Store number of elements in the array
	!
	DATA_TYPE$(0%) = NUM1$(DATA_TYPE%)

	!
	! Close out and return
	!
	CLOSE TEXT_FILE.CH

	CALL ENTR_3MESSAGE(SCOPE, "", 1%)

	RETURN

32767	END SUB
