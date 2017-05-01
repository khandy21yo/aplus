1	%TITLE "Sub Process to Create File Opens"
	%SBTTL "TK_SUBR_CREATEOPEN"
	%IDENT "V3.6a Calico"

	SUB TK_SUBR_CREATEOPEN( &
		STRING	SYSTEM_NAME, &
		STRING	FILE_NAME, &
		STRING	LOCATION, &
		STRING	NEW_FILE_NAME, &
		LONG	FLAG, &
		LONG	SUB_STATUS)

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
	!
	! ABSTRACT:HELP
	!	.p
	!	A process to aid in the creation of a ddl file.
	!		FLAG		MEANING
	!		  0%		Normal
	!		  1%		Rename opens and ddls to the
	!				NEW_FILE_NAME.
	!		  2%		Call this program again if this is set.
	!				(This is used to clear the variables
	!				during the open erase.  The structures
	!				doesn't seem to get cleared unless it is
	!				manually.)
	!
	! Index:
	!
	! Option:
	!
	!
	! COMPILE:
	!
	!	$ BAS TK_SOURCE:TK_SUBR_CREATEOPEN
	!	$ LIB FUNC_LIB:CMCFUN/REP TK_SUBR_CREATEOPEN
	!	$ DELETE TK_SUBR_CREATEOPEN.OBJ;*
	!
	! AUTHOR:
	!
	!	05/01/87 - Robert Peterson
	!
	! MODIFICATION HISTORY:
	!
	!	06/27/88 - Kevin Handy
	!		Added .PUR create type.
	!
	!	07/18/88 - Kevin Handy
	!		Changed PUR to OLD.
	!		Modified so open allocates channel number,
	!		reads device, creates .NAME$ variable.
	!
	!	01/24/89 - Frank Starman
	!		Added .PST create type.
	!
	!	04/03/89 - Robert Peterson
	!		Reformat screens.
	!
	!	11/16/90 - Kevin Handy
	!		Modified to ignore .H files.
	!
	!	08/05/91 - Kevin Handy
	!		Added ACCESS READ to open file.
	!
	!	05/22/92 - Frank F. Starman
	!		Read file attributes from .CRE file
	!		if there is any.
	!
	!	06/12/92 - Kevin Handy
	!		Clean up (check)
	!
	!	06/15/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	12/15/95 - Kevin Handy
	!		Change RIGHT(NUM1$()) to FORMAT$()
	!		Reformat source closer to 80 columns.
	!
	!	05/28/96 - Kevin Handy
	!		Reformat Source code.
	!		Modified to llow .HB files as header files.
	!
	!	08/28/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/11/98 - Kevin Handy
	!		Drop 'DDL's from allowed open types.
	!
	!	04/08/99 - Kevin Handy
	!		Fix parameter passing problems with OPENS_SCROLL.
	!		Use BASIC$STARLET for LIB$ and SMG$ references
	!
	!	05/14/99 - Kevin Handy
	!		Modified to apply a default "INDEXED/FIXED"
	!		organization to files, so I don't have to keep
	!		changing them.
	!
	!	09/11/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!		Reformat source code.
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

50	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "$LIBDEF" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "STR$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "$SSDEF" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	%INCLUDE "FUNC_INCLUDE:TK_WINDOW.INC"

	%INCLUDE "SOURCE:[SMG.OPEN]SMG_SCROLL.HB"
	DECLARE  SMG_SCROLL_CDD KEYS_SCROLL
	DECLARE  SMG_SCROLL_CDD OPENS_SCROLL

	%INCLUDE "SOURCE:[SMG.OPEN]SMG_DDL.HB"
	DECLARE  SMG_DDL_CDD DDL

	%INCLUDE "SOURCE:[SMG.OPEN]SMG_OPN.HB"
	DECLARE  SMG_OPN_CDD OPN

	EXTERNAL STRING  FUNCTION TK_MAIN_TEXTFILEWINDOW
	EXTERNAL INTEGER FUNCTION DSPL_SCROLL
	EXTERNAL LONG    FUNCTION MAIN_JOURNAL

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (FIELD_NAME) &
		STRING SUBSTRUCT = 50%, &
		STRING FIELD_NAME(300%) = 64%

	SUB_STATUS = SS$_NORMAL

	SCOPE::PRG_IDENT	= "FUNC"
	SCOPE::PRG_PROGRAM	= "TK_SUBR_CREATEOPEN"
	SCOPE::PRG_ITEM	= ""

	!
	! Declare vars
	!
	DECLARE LONG SMG_SCROLL, SMG_SCROLL1, SMG_SCROLL2, SYS_STATUS
	DECLARE LONG TEXT_FILE.CH

	KEY_WIN_ELEM% = 300%

	DIM FIELD_LINES$(KEY_WIN_ELEM%), &
		WINDOW_LINES$(KEY_WIN_ELEM%), &
		WINDOW_KEYS$(KEY_WIN_ELEM%), &
		WINDOW_FIELDS$(KEY_WIN_ELEM%), &
		WINDOW_DUP$(KEY_WIN_ELEM%), &
		WINDOW_CHG$(KEY_WIN_ELEM%), &
		FILE_TYPE$(50%), &
		FILE_TYPE_DEF$(100%), &
		FILE_TYPE_DEF_DESC$(100%), &
		FILE_EXTENSION$(100%), &
		FILE_EXTENSION_DESC$(100%), &
		ORGNIZATION$(6%), &
		STRCTURE$(4%)

	FLAG = FLAG AND NOT 2%	! Clear always the recall bit
	F_NAME$ = FILE_NAME

	!
	! Set sub structure name equal to file name
	! The sub structure variable is passed to the
	! foreign key maintenance process to become part
	! of the primary key in that file structure.
	!
	SUBSTRUCT = FILE_NAME

	FILE_EXTENSION$ = "CMC:TK_FILE_EXTENSION.TEMPLATE"
	FILE_TYPE$ = "CMC:TK_FILE_TYPE.TEMPLATE"
	PROTO_FILE_NAME$ = "CMC:TK_*.TEMPLATE"
	PROTO_PREFIX$ = "CMC:TK_"
	PROTO_SUFFIX$ = ".TEMPLATE"

	ORGNIZATION$(1%) = "VIRTUAL"
	ORGNIZATION$(2%) = "UNDEFINED"
	ORGNIZATION$(3%) = "INDEXED"
	ORGNIZATION$(4%) = "SEQUENTIAL"
	ORGNIZATION$(5%) = "RELATIVE"
	ORGNIZATION$(6%) = ""
	ORGNIZATION% = 5%

	STRCTURE$(1%) = "STREAM"
	STRCTURE$(2%) = "VARIABLE"
	STRCTURE$(3%) = "FIXED"
	STRCTURE$(4%) = ""
	STRCTURE% = 3%

100	!*******************************************************************
	! Get channels from VMS
	!*******************************************************************

	SMG_STATUS% = LIB$GET_LUN(TEXT_FILE.CH)
	GOTO ErrorGetCh IF SMG_STATUS% = LIB$_INSLUN
	GOTO 200

 ErrorGetCh:
	CALL ENTR_3MESSAGE(SCOPE, "No free channels from VMS:  " + NUM1$(SMG_STATUS%), 0%)
	GOTO ExitProgram

200	GOSUB LoadTemplateArrays

	!
	! Read in file layout
	!
	CALL TK_SUBR_DDLEXTRACT(DDL, SYSTEM_NAME, F_NAME$, FDE_STATUS%)
	IF (FDE_STATUS% AND 1%) = 0%
	THEN
		SUB_STATUS = FDE_STATUS%
		GOTO ExitProgram
	END IF

400	!
	! Get all open include file names
	!
	CALL FIND_FILE("SOURCE:[" + TRM$(LOCATION) + ".OPEN]" + &
		TRM$(F_NAME$) + ".*", &
		FILE_TYPE$(), 0% + 1% + 2% + 4% + 8% + 16% + 32%, "", "")

	OPN::EXTENSION = ""
	OPN::FILE_NAME = ""
	OPN::ORGNIZATION = "INDEXED"
	OPN::STRCTURE = "FIXED"

	OPENS1% = VAL%(FILE_TYPE$(0%))
	OPENS% = 0%
	TEMPL.FILE% = 1%

	FOR LOOP% = 1% TO OPENS1%
		TEMP1%,TEMP% = INSTR(1%, FILE_TYPE$(LOOP%), ".")

		WHILE TEMP%
			TEMP% = INSTR(TEMP% + 1%, FILE_TYPE$(LOOP%), ".")
			TEMP1% = TEMP% IF TEMP% <> 0%
		NEXT

		FILE_TYPE$(LOOP%) = RIGHT(FILE_TYPE$(LOOP%), TEMP1% + 1%)

		TEMPL.FILE% = LOOP% IF FILE_TYPE$(LOOP%)="CRE"

		IF (FILE_TYPE$(LOOP%) <> "H") AND &
			(FILE_TYPE$(LOOP%) <> "HB") AND &
			(FILE_TYPE$(LOOP%) <> "DDL")
		THEN
			OPENS% = OPENS% + 1%
			FILE_TYPE$(OPENS%) = FILE_TYPE$(LOOP%)
		END IF

	NEXT LOOP%

	FILE_TYPE$(LOOP%) = "" FOR LOOP% = OPENS% + 1% TO OPENS1%

	GOTO 500 IF OPENS% = 0%

	!
	! Extract all information possible from open file
	!
	CALL TK_SUBR_EXTRACTOPEN(OPN, F_NAME$, &
		FILE_TYPE$(TEMPL.FILE%), "SOURCE:[" + LOCATION + ".OPEN]")

500	FC% = DDL::FIELD_NUM

	OPENS_SCROLL::BOT_ARRAY		= OPENS% + 1%
	OPENS_SCROLL::BOT_ARRAY		= 1% IF OPENS% = 0%

	GOSUB InitKeys

	KEYS_SCROLL::BOT_ARRAY		= OPN::KEYS_NUM + 1%
	KEYS_SCROLL::BOT_ARRAY		= 1% IF OPN::KEYS_NUM = 0%

	IF FLAG = 1%
	THEN
		F_NAME$ = NEW_FILE_NAME

		OPN::FILE_NAME = TRM$(F_NAME$) + '.DEV$ + "' + &
			TRM$(F_NAME$) + "." + TRM$(OPN::EXTENSION) + '"'
		OPN::FILE_NAME = TRM$(F_NAME$) + '.DEV$ + "' + &
			TRM$(F_NAME$) + '_" + YYYY_PP$ + ".' + &
			TRM$(OPN::EXTENSION) + '"' &
				IF TRM$(OPN::EXTENSION) = "LED"
		OPN::FILE_NAME = TRM$(F_NAME$) + '.DEV$ + "' + &
			TRM$(F_NAME$) + '_"' + ' + BATCH_NO$ + ".' + &
			TRM$(OPN::EXTENSION) + '"' &
				IF TRM$(OPN::EXTENSION) = "JRL"

		GOSUB 13000
		GOTO ExitProgram
	END IF

	FIELD_LINES$(ARRAY_LOOP%) = &
		LEFT(DDL::FIELD_NAME(ARRAY_LOOP%) + STRING$(10% - &
		LEN(DDL::FIELD_NAME(ARRAY_LOOP%)), 32%), 10%) + &
		" " + &
		LEFT(DDL::FIELD_TYPE(ARRAY_LOOP%) + STRING$(9% - &
		LEN(DDL::FIELD_TYPE(ARRAY_LOOP%)), 32%), 9%) + &
		" " + &
		LEFT(STRING$(4% - LEN(TRM$(DDL::FIELD_SIZE(ARRAY_LOOP%))), 32%) + &
		TRM$(DDL::FIELD_SIZE(ARRAY_LOOP%)), 4%) + &
		" " + &
		LEFT(DDL::FIELD_DESC(ARRAY_LOOP%) + STRING$(23% - &
		LEN(DDL::FIELD_DESC(ARRAY_LOOP%)), 32%), 23%) &
			FOR ARRAY_LOOP% = 1% TO FC%

	!
	! Set the name of the fields in a common that can be passed
	! to the foreign key definition process
	!
	FIELD_NAME(ARRAY_LOOP%) = TRM$(DDL::FIELD_NAME(ARRAY_LOOP%)) &
		FOR ARRAY_LOOP% = 1% TO FC%

	FIELD_NAME(ARRAY_LOOP% + 1%) = ""

	!
	! Set the number lines in the array in the zero element
	! in field lines.  This is used in list choices to
	! determine the length of the array
	!
	FIELD_NAME(0%) = NUM1$(FC%)

	!
	! Define windows
	!

	!
	! Create the attribute display
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(4%, 60%, SMG_SCROLL2, &
		SMG$M_BORDER)

	SMG_STATUS% = SMG$LABEL_BORDER(SMG_SCROLL2, "ATTRIBUTES", SMG$K_TOP,, &
		SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCROLL2, "(01) EXTENSION", 1%, 1%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCROLL2, "(02) FILE OPEN", 2%, 1%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCROLL2, "(03) ORGANIZATION", 3%, 1%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCROLL2, "(04) STRUCTURE", 4%, 1%)

	!
	! Print the array and windows
	!
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCROLL2, &
		EDIT$(OPN::EXTENSION, 8% + 128%), 1%, 20%,, SMG$M_BOLD)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCROLL2, &
		EDIT$(OPN::FILE_NAME, 8% + 128%), 2%, 20%,, SMG$M_BOLD)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCROLL2, &
		EDIT$(OPN::ORGNIZATION, 8% + 128%), 3%, 20%,, SMG$M_BOLD)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCROLL2, &
		EDIT$(OPN::STRCTURE, 8% + 128%), 4%, 20%,, SMG$M_BOLD)

	!
	! Paste attributes
	!
	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_SCROLL2, SCOPE::SMG_PBID, &
		9%, 3%)

1000	!******************************************************************
	! Open maintenance window
	!******************************************************************

	SCOPE::PRG_ITEM = ""

	OPTLIST$ = "Add Change File_opens Keys_primary_alternate fOreign_keys Proto_tables Erase Help eXit"
	OPT$ = ENTR_3OPTION(SCOPE, "COMMAND", OPTLIST$, OPT%, FLAGW%)

	SELECT SCOPE::SCOPE_EXIT
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitCreate

	CASE SMG$K_TRM_DOWN
		ATT_LOOP% = ATT_LOOP% + 1%
		ATT_LOOP% = 4% IF ATT_LOOP% > 4%
		ITM$ = NUM1$(ATT_LOOP%)

	CASE SMG$K_TRM_UP
		ATT_LOOP% = 5% IF ATT_LOOP% = 0%
		ATT_LOOP% = ATT_LOOP% - 1%
		ATT_LOOP% = 1% IF ATT_LOOP% < 1%
		ITM$ = NUM1$(ATT_LOOP%)

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 1000

	END SELECT

	SELECT OPT$
	!******************************************************************
	! attributes maintenance
	!******************************************************************
	CASE "C", "A"
		GOSUB 2000

	!********************************************************************************
	! Maintain file opens
	!********************************************************************************
	CASE "F"
		!
		! Create the opens display
		!
		SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(4%, 16%, SMG_SCROLL1, &
			SMG$M_BORDER)

		SMG_STATUS% = SMG$LABEL_BORDER(SMG_SCROLL1, "FILE OPENS", SMG$K_TOP,, &
			SMG$M_BOLD)

		OPENS_SCROLL::WINDOW		= SMG_SCROLL1
		OPENS_SCROLL::TOP_ARRAY		= 1%

		OPENS_SCROLL::SCROLL_TOP	= 1%
		OPENS_SCROLL::SCROLL_BOT	= 4%
		OPENS_SCROLL::TOP_LINE		= 1%
		OPENS_SCROLL::BEG_ELEMENT	= 1%
		OPENS_SCROLL::END_ELEMENT	= OPENS% + 1%
		OPENS_SCROLL::CUR_LINE		= 1%
		OPENS_SCROLL::CUR_W_ROW		= 1%
		OPENS_SCROLL::CUR_W_COL		= 1%
		OPENS_SCROLL::FIND_LINE		= 1%
		OPENS_SCROLL::SMG_FLAG		= 0%
		OPENS_SCROLL::PROMPT		= "->"
		OPENS_SCROLL::VIDEO_COMP	= 0%
		OPENS_SCROLL::CHARSET		= 0%
		OPENS_SCROLL::DRAW_COLS		= ""

		!
		! Opens
		!
		SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_SCROLL1, SCOPE::SMG_PBID, &
			10%, 4%)

		!
		! Display arrays on screen
		!
		TEMP = DSPL_SCROLL(OPENS_SCROLL, FILE_TYPE$(), 0%, "PAINT")

		GOSUB 3000

		SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SCROLL1)

	! *******************************************************************
	! Maintain open keys
	! *******************************************************************
	CASE "K"
		!
		! Create the key display
		!
		SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(9%, 76%, SMG_SCROLL, &
			SMG$M_BORDER)

		SMG_STATUS% = SMG$LABEL_BORDER(SMG_SCROLL, "PRIMARY AND ALTERNATE KEY WINDOW", SMG$K_TOP,, &
			SMG$M_BOLD)

		TITLE$ = "      (01)" + SPACE$(54%) + " (02) (03)"
		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCROLL, TITLE$, 1%, 3%)

		TITLE$ = "    Fields" + SPACE$(54%) + " DUP  CHG "
		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCROLL, TITLE$, 2%, 3%)

		SMG_STATUS% = SMG$DRAW_LINE(SMG_SCROLL, 3%, 1%, 3%, 76%)

		!
		! Keys
		!
		SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_SCROLL, SCOPE::SMG_PBID, &
			10%, 4%)

		KEYS_SCROLL::WINDOW		= SMG_SCROLL
		KEYS_SCROLL::TOP_ARRAY		= 1%

		KEYS_SCROLL::SCROLL_TOP		= 4%
		KEYS_SCROLL::SCROLL_BOT		= 9%
		KEYS_SCROLL::TOP_LINE		= 1%
		KEYS_SCROLL::BEG_ELEMENT	= 1%
		KEYS_SCROLL::END_ELEMENT	= OPN::KEYS_NUM + 1%
		KEYS_SCROLL::CUR_LINE		= 1%
		KEYS_SCROLL::CUR_W_ROW		= 1%
		KEYS_SCROLL::CUR_W_COL		= 1%
		KEYS_SCROLL::FIND_LINE		= 1%
		KEYS_SCROLL::SMG_FLAG		= 2%
		KEYS_SCROLL::PROMPT		= "->"
		KEYS_SCROLL::VIDEO_COMP		= 0%
		KEYS_SCROLL::CHARSET		= 0%
		KEYS_SCROLL::DRAW_COLS		= "006,067,072"

		!
		! Display arrays on screen
		!
		TEMP = DSPL_SCROLL(KEYS_SCROLL, WINDOW_LINES$(), 0%, "PAINT")

		GOSUB 4000

		SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SCROLL)

	! *******************************************************************
	! Maintain foreign keys
	! *******************************************************************
	CASE "O"
		!
		! Make sure there is a header
		!
		TK_SUBR_CREATEOPEN = MAIN_JOURNAL(TK_MAIN_FOREIGN.ID, "")

	! *******************************************************************
	! Maintain Prototyping file template files
	! *******************************************************************
	CASE "P"
		ZX$ = TK_MAIN_TEXTFILEWINDOW("10;4", "9;30", PROTO_FILE_NAME$, &
			PROTO_PREFIX$, PROTO_SUFFIX$, "Prototyping", 16%)

		GOSUB LoadTemplateArrays

	CASE "X"	! Exit
		GOTO ExitCreate

	CASE "E"	! Erase
		!
		! Delete the opens
		!
		SMG_STATUS% = LIB$DELETE_FILE( &
			"SOURCE:[" + &
			EDIT$(LOCATION, 2% + 4% + 32% + 256%) + ".OPEN]" + &
			EDIT$(FILE_NAME, 2% + 4% + 32% + 256%) + ".*;*")

		FLAG = FLAG OR 2%	! Set the recall bit

		GOTO ExitProgram

	CASE "H"	! Help
		CALL HELP_3MESSAGE(SCOPE, "", "PROG", SCOPE::PRG_PROGRAM, "HELP")

	END SELECT

	GOTO 1000

 ExitCreate:
	!*****************************************************************************
	! Exit out of subroutine
	!*****************************************************************************
	SCOPE::PRG_ITEM = "CREATE"

	TEMP$ = "Create open files "

	YESNO$ = EDIT$(ENTR_3YESNO(SCOPE, SMG_SCREEN_DATA%, &
		"", TEMP$, "N", 0%, "", ""), -1%)

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
		GOTO ExitCreate

	END SELECT

	GOSUB 13000 IF YESNO$ = "Y"

 ExitProgram:
	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SCROLL2)

	CLOSE TEXT_FILE.CH

	SMG_STATUS% = LIB$FREE_LUN(TEXT_FILE.CH)

	EXIT SUB


2000	!**************************************************************************
	! attribute maintenance
	!**************************************************************************
	SELECT OPT$
	!
	! Add attributes
	!
	CASE "A"
		FUN_NAME$ = "Add"
		ITM$ = "01"

	!
	! Change an item
	!
	CASE "C"
		FUN_NAME$ = "Change"

		ITM$ = EDIT$(ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, "", &
			"Item to " + FUN_NAME$, "  ", 4%, "", ""), -1%)
		ITM$ = "0" + ITM$ IF LEN(ITM$) < 2% AND ITM$ <> ""

		SELECT SCOPE::SCOPE_EXIT
		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO Ret2000

		CASE SMG$K_TRM_DOWN
			ATT_LOOP% = ATT_LOOP% + 1%
			ATT_LOOP% = 4% IF ATT_LOOP% > 4%
			ITM$ = NUM1$(ATT_LOOP%)

		CASE SMG$K_TRM_UP
			ATT_LOOP% = 5% &
				IF ATT_LOOP% = 0%
			ATT_LOOP% = ATT_LOOP% - 1%
			ATT_LOOP% = 1% &
				IF ATT_LOOP% < 1%
			ITM$ = NUM1$(ATT_LOOP%)

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
			GOTO Ret2000

		END SELECT

		GOTO Ret2000 IF ITM$ = ""

	END SELECT

 ChangeLoop1:
	WHEN ERROR IN
		ATT_LOOP% = VAL%(ITM$)
	USE
		CONTINUE 2000
	END WHEN

2010	GOTO Ret2000 IF ATT_LOOP% < 0% OR &
		ATT_LOOP% > 4%

	EXTENSION$ = OPN::EXTENSION
	FILE_NAME$ = OPN::FILE_NAME
	ORGNIZATION$ = OPN::ORGNIZATION
	STRCTURE$ = OPN::STRCTURE

 AttEnterChange:
	!***************************************************************
	! Enter and change attribute data
	!***************************************************************

	TEMP_PRG_ITEM$ = SCOPE::PRG_ITEM

	SCOPE::PRG_ITEM = "FLD" + FORMAT$(ATT_LOOP%, "<0>##")

	SELECT ATT_LOOP%
	CASE 1%
		EXTENSION$ = EDIT$(ENTR_3STRING(SCOPE, SMG_SCROLL2, "01;20", &
			FUN_NAME$ + " extension ", SPACE$(3%), 32%, &
			"'E", EDIT$(OPN::EXTENSION, 8% + 128%)), -1%)

		IF EXTENSION$ <> ""
		THEN
			TEMP% = 0%
			TEMP% = -1% &
				IF FILE_EXTENSION$(LOOP%) = EXTENSION$ &
				FOR LOOP% = 1% TO FILE_EXTENSION%
			SCOPE::SCOPE_EXIT = SMG$K_TRM_F14 IF TEMP% = 0%
		END IF

		IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
		THEN
			!*****************************************
			! Look up file extensions
			!*****************************************
			X% = ENTR_3CHOICE(SCOPE, "", "", &
				FILE_EXTENSION_DESC$(), &
				OPN::EXTENSION, 2% + 8% + 64%, &
				"  EXT DESCRIPTION", "006,", 2%)

			GOTO AttEnterChange IF X% <= 0%

			OPN::EXTENSION = FILE_EXTENSION$(X%)
			GOTO AttEnterChange
		END IF

	CASE 2%
		TEMP$ = TRM$(F_NAME$) + '.DEV$ + "' + &
			TRM$(F_NAME$) + "." + TRM$(OPN::EXTENSION) + '"'
		TEMP$ = TRM$(F_NAME$) + '.DEV$ + "' + &
			TRM$(F_NAME$) + '_" + YYYY_PP$ + ".' + &
			TRM$(OPN::EXTENSION) + '"' &
				IF TRM$(OPN::EXTENSION) = "LED"
		TEMP$ = TRM$(F_NAME$) + '.DEV$ + "' + &
			TRM$(F_NAME$) + '_" + BATCH_NO$ + ".' + &
			TRM$(OPN::EXTENSION) + '"' &
				IF TRM$(OPN::EXTENSION) = "JRL"

		OPN::FILE_NAME = TEMP$ &
			IF EDIT$(OPN::FILE_NAME, -1%) = ""

		FILE_NAME$ = EDIT$(ENTR_3STRING(SCOPE, SMG_SCROLL2, "02;20", &
			FUN_NAME$ + " file name ", SPACE$(55%), 32%, &
			"'E", EDIT$(OPN::FILE_NAME, 8% + 128%)), 32%)

		IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
		THEN
			!*****************************************
			! Look up file extensions
			!*****************************************
			TEMP$(0%) = "1"
			TEMP$(1%) = TEMP$

			X% = ENTR_3CHOICE(SCOPE, "", "", TEMP$(), &
				OPN::FILE_NAME, 8% + 64%, &
				"  DEFAULT FILE NAME", "", 0%)

			GOTO AttEnterChange IF X% <= 0%

			OPN::FILE_NAME = TEMP$(X%)

			GOTO AttEnterChange
		END IF

	CASE 3%
		ORGNIZATION$ = EDIT$(ENTR_3STRING(SCOPE, SMG_SCROLL2, "03;20", &
			FUN_NAME$ + " organization ", &
			SPACE$(30%), 32%, "'E", &
			EDIT$(OPN::ORGNIZATION, 8% + 128%)), -1%)

		IF ORGNIZATION$ <> ""
		THEN
			TEMP% = 0%
			TEMP% = -1% &
				IF ORGNIZATION$(LOOP%) = ORGNIZATION$ &
				FOR LOOP% = 1% TO ORGNIZATION%
			SCOPE::SCOPE_EXIT = SMG$K_TRM_F14 IF TEMP% = 0%
		END IF

		IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
		THEN
			!*****************************************
			! Look up organization
			!*****************************************
			X% = ENTR_3CHOICE(SCOPE, "", "", &
				ORGNIZATION$(), OPN::ORGNIZATION, &
				8% + 64%, "ORGANIZATION", "", 2%)

			GOTO AttEnterChange IF X% <= 0%

			OPN::ORGNIZATION = ORGNIZATION$(X%)
			GOTO AttEnterChange
		END IF

		IF ORGNIZATION$ = "RELATIVE"
		THEN
			FOR I% = 0% TO KEY_WIN_ELEM%
				WINDOW_LINES$(I%) = ""
				WINDOW_KEYS$(I%) = ""
				WINDOW_FIELDS$(I%) = ""
				WINDOW_DUP$(I%) = ""
				WINDOW_CHG$(I%) = ""
			NEXT I%

			OPN::KEYS(I%) = "" &
				FOR I% = 0% TO 31%

			OPN::KEYS_NUM	= 1%

			GOSUB InitKeys

			OPN::STRCTURE	= "FIXED"
			STRCTURE$ = ENTR_3STRING(SCOPE,  SMG_SCROLL2, "04;20", &
				"", OPN::STRCTURE, 1%, "'E", "")

			KEYS_SCROLL::FIND_LINE	= 1%
			V% = DSPL_SCROLL(KEYS_SCROLL, WINDOW_LINES$(), &
				0%, "PAINT")
		END IF

	CASE 4%
		STRCTURE$ = EDIT$(ENTR_3STRING(SCOPE, SMG_SCROLL2, "04;20", &
			FUN_NAME$ + " structure ", SPACE$(30%), 32%, &
			"'E", EDIT$(OPN::STRCTURE, 8% + 128%)), -1%)

		IF OPN::ORGNIZATION = "RELATIVE"
		THEN
			STRCTURE$(1%) = "FIXED"
			STRCTURE$(2%) = ""
			STRCTURE% = 1%
		ELSE
			STRCTURE$(1%) = "STREAM"
			STRCTURE$(2%) = "VARIABLE"
			STRCTURE$(3%) = "FIXED"
			STRCTURE$(4%) = ""
			STRCTURE% = 3%
		END IF

		IF STRCTURE$ <> ""
		THEN
			TEMP% = 0%
			TEMP% = -1% IF STRCTURE$(LOOP%) = STRCTURE$ &
				FOR LOOP% = 1% TO STRCTURE%
			SCOPE::SCOPE_EXIT = SMG$K_TRM_F14 IF TEMP% = 0%
		END IF

		IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
		THEN
			!*****************************************
			! Look up structure
			!*****************************************
			X% = ENTR_3CHOICE(SCOPE, "", "", &
				STRCTURE$(), OPN::STRCTURE, 8% + 64%, &
				"STRUCTURE", "", 2%)

			GOTO AttEnterChange IF X% <= 0%

			OPN::STRCTURE	= STRCTURE$(X%)
			GOTO AttEnterChange
		END IF
	END SELECT

	SELECT SCOPE::SCOPE_EXIT
	CASE SMG$K_TRM_F10,	SMG$K_TRM_CTRLZ
		GOTO AttEnterChange1
	END SELECT

	OPN::EXTENSION	= EXTENSION$
	OPN::FILE_NAME	= FILE_NAME$
	OPN::ORGNIZATION= ORGNIZATION$
	OPN::STRCTURE	= STRCTURE$

 AttEnterChange1:
	SCOPE::PRG_ITEM = TEMP_PRG_ITEM$

 ChangeLoop2:
	SELECT SCOPE::SCOPE_EXIT
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO Ret2000

	CASE SMG$K_TRM_DOWN
		ATT_LOOP% = ATT_LOOP% + 1%
		ATT_LOOP% = 4% IF ATT_LOOP% > 4%
		ITM$ = NUM1$(ATT_LOOP%)
		GOTO ChangeLoop1

	CASE SMG$K_TRM_UP
		ATTCHANGE_LOOP% = 7% IF ATT_LOOP% = 0%
		ATT_LOOP% = ATT_LOOP% - 1%
		ATT_LOOP% = 1% IF ATT_LOOP% < 1%
		ITM$ = NUM1$(ATT_LOOP%)
		GOTO ChangeLoop1

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

		IF FUN_NAME$ = "Add" AND ATT_LOOP% < 4%
		THEN
			SCOPE::SCOPE_EXIT = SMG$K_TRM_DOWN
			GOTO ChangeLoop2
		END IF

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO Ret2000

	END SELECT

 Ret2000:
	RETURN

	%Page

3000	!******************************************************************
	! File open maintenance
	!******************************************************************
	OPT2% = 0%
 OpenPosition:
	SCOPE::PRG_ITEM = ""

	OPTLIST1$ = "Add Erase Help eXit"
	OPT1$ = ENTR_3OPTION(SCOPE, "COMMAND", OPTLIST1$, OPT2%, FLAGW%)

	SELECT SCOPE::SCOPE_EXIT
	CASE SMG$K_TRM_UP, SMG$K_TRM_DOWN, &
		SMG$K_TRM_PREV_SCREEN, SMG$K_TRM_NEXT_SCREEN, &
		SMG$K_TRM_F18, SMG$K_TRM_F19

		V% = DSPL_SCROLL(OPENS_SCROLL, FILE_TYPE$(), &
			SCOPE::SCOPE_EXIT, "PAINT")
		GOTO OpenPosition

	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ

		GOTO Ret3000

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO OpenPosition

	END SELECT

	ARRAY_LOOP% = OPENS_SCROLL::CUR_LINE
	ARRAY_LOOP% = 1% IF ARRAY_LOOP% = 0%

	SELECT OPT1$
	!
	! Add a new key
	!
	CASE "A"
		FUN_NAME$ = "Add"

 OpenPosition1:	FILE_TYPE$(LOOP% + 1%) = FILE_TYPE$(LOOP%) &
			FOR LOOP% = OPENS_SCROLL::BOT_ARRAY TO &
			ARRAY_LOOP% STEP -1%

		FILE_TYPE$(ARRAY_LOOP%), FILE_TYPE$ = ""

		OPENS_SCROLL::BOT_ARRAY, OPENS_SCROLL::END_ELEMENT = &
			OPENS_SCROLL::BOT_ARRAY + 1%

		TEMP = DSPL_SCROLL(OPENS_SCROLL, FILE_TYPE$(), &
			SCOPE::SCOPE_EXIT, "PAINT")

 OpenPosition2:
		SMG_STATUS% = SMG$PUT_CHARS(OPENS_SCROLL::WINDOW, &
			FILE_TYPE$(ARRAY_LOOP%),, 3%,, SMG$M_REVERSE)

		FILE_TYPE$ = EDIT$(ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
			"", FUN_NAME$ + " file type ", SPACE$(3%), &
			32%, "'E", FILE_TYPE$(ARRAY_LOOP%)), -1%)

		SMG_STATUS% = SMG$PUT_CHARS(OPENS_SCROLL::WINDOW, &
			FILE_TYPE$(ARRAY_LOOP%),, 3%,,)

 OpenPosition3:
		SELECT SCOPE::SCOPE_EXIT
		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			FILE_TYPE$(LOOP%) = FILE_TYPE$(LOOP% + &
				1%) FOR LOOP% = ARRAY_LOOP% &
				TO OPENS_SCROLL::BOT_ARRAY

			TEMP = DSPL_SCROLL(OPENS_SCROLL, &
				FILE_TYPE$(), SCOPE::SCOPE_EXIT, &
				"PAINT")

			OPENS_SCROLL::BOT_ARRAY, &
				OPENS_SCROLL::END_ELEMENT = &
				OPENS_SCROLL::BOT_ARRAY - 1%

			GOTO OpenPosition

		CASE SMG$K_TRM_F14
			!*****************************************
			! Look up open method
			!*****************************************
			X% = ENTR_3CHOICE(SCOPE, "", "", &
				FILE_TYPE_DEF_DESC$(), &
				FILE_TYPE$, 2% + 8% + 64%, &
				"  TYP   OPEN METHOD", "007,", &
				2%)

			GOTO OpenPosition2 IF X% <= 0%

			FILE_TYPE$(ARRAY_LOOP%) = &
				FILE_TYPE_DEF$(X%)
			GOTO OpenPosition2

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
			GOTO OpenPosition2

		END SELECT

		IF FILE_TYPE$ <> ""
		THEN
			FOR LOOP% = 1% TO OPENS_SCROLL::BOT_ARRAY - 1%
				IF FILE_TYPE$(LOOP%) = FILE_TYPE$ AND &
					LOOP% <> ARRAY_LOOP%
				THEN
					CALL ENTR_3MESSAGE(SCOPE, "Duplicate file open", 0%)
					SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
					GOTO OpenPosition3
				END IF
			NEXT LOOP%

			TEMP% = 0%
			TEMP% = -1% IF FILE_TYPE_DEF$(LOOP%) = &
				FILE_TYPE$ FOR LOOP% = 1% TO FILE_TYPE%

			IF TEMP% = 0%
			THEN
				SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
				GOTO OpenPosition3
			ELSE
				FILE_TYPE$(ARRAY_LOOP%) = FILE_TYPE$

				TEMP = DSPL_SCROLL(OPENS_SCROLL, &
					FILE_TYPE$(), SCOPE::SCOPE_EXIT, &
					"PAINT")
				GOTO OpenPosition1
			END IF
		ELSE
			SCOPE::SCOPE_EXIT = SMG$K_TRM_F10
			GOTO OpenPosition3
		END IF

	!
	! Erase
	!
	CASE "E"
		IF OPENS_SCROLL::BOT_ARRAY > 1% AND &
			OPENS_SCROLL::BOT_ARRAY <> ARRAY_LOOP%
		THEN
			FILE_TYPE$(LOOP%) = FILE_TYPE$(LOOP% + 1%) &
				FOR LOOP% = ARRAY_LOOP% TO &
				OPENS_SCROLL::BOT_ARRAY

			TEMP = DSPL_SCROLL(OPENS_SCROLL, &
				FILE_TYPE$(), &
				SCOPE::SCOPE_EXIT, &
				"PAINT")

			OPENS_SCROLL::BOT_ARRAY, &
				OPENS_SCROLL::END_ELEMENT = &
					OPENS_SCROLL::BOT_ARRAY - 1%
		ELSE
			CALL ENTR_3MESSAGE(SCOPE, "Open cannot be erased ", 0%)
		END IF

	!
	! Help
	!
	CASE "H"
		CALL HELP_3MESSAGE(SCOPE, "", "PROG", SCOPE::PRG_PROGRAM, "HELP")

	!
	! eXit
	!
	CASE "X"
		GOTO Ret3000

	!
	! Bad Keys
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO OpenPosition
	END SELECT

	GOTO OpenPosition

 Ret3000:
	RETURN

	%Page

4000	!******************************************************************
	! key maintenance
	!******************************************************************

	IF OPN::ORGNIZATION = "RELATIVE"
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "A Relative file does not have keys!!!", 1%)
		GOTO Ret4000
	END IF

	OPT2% = 0%

 KeyMaintenance:
	SCOPE::PRG_ITEM = ""

	OPTLIST1$ = "Add Change Erase Help eXit"
	OPT1$ = ENTR_3OPTION(SCOPE, "COMMAND", OPTLIST1$, OPT2%, FLAGW%)

	SELECT SCOPE::SCOPE_EXIT
	CASE SMG$K_TRM_UP, SMG$K_TRM_DOWN, &
		SMG$K_TRM_PREV_SCREEN, SMG$K_TRM_NEXT_SCREEN, &
		SMG$K_TRM_F18, SMG$K_TRM_F19

		V% = DSPL_SCROLL(KEYS_SCROLL, WINDOW_LINES$(), &
			SCOPE::SCOPE_EXIT, "PAINT")
		GOTO KeyMaintenance

	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO Ret4000

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO KeyMaintenance

	END SELECT

	ARRAY_LOOP% = KEYS_SCROLL::CUR_LINE

	SELECT OPT1$
	!
	! Add a new key
	!
	CASE "A"
		FUN_NAME$ = "Add"
 KeyNextEntry:
		WINDOW_LINES$(LOOP% + 1%) = WINDOW_LINES$(LOOP%) &
			FOR LOOP% = KEYS_SCROLL::BOT_ARRAY TO ARRAY_LOOP% &
				STEP -1%

		KEYS_SCROLL::BOT_ARRAY, KEYS_SCROLL::END_ELEMENT = &
			KEYS_SCROLL::BOT_ARRAY + 1%

		WINDOW_LINES$(ARRAY_LOOP%) = SPACE$(76%)

		X% = DSPL_SCROLL(KEYS_SCROLL, &
			WINDOW_LINES$(), &
			0%, &
			"PAINT")

		WINDOW_KEYS$ = SPACE$(3%)
		WINDOW_FIELDS$ = ""
		WINDOW_DUP$ = ""
		WINDOW_CHG$ = ""

		FOR ENTERCHANGE_LOOP% = 1% TO 3%
 KeyEnterLoop:
4100			!******************************************
			! Enter data to screen
			!******************************************
			GOSUB KeyEnterChange

			SELECT SCOPE::SCOPE_EXIT
			CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
				WINDOW_LINES$(LOOP%) = WINDOW_LINES$(LOOP% + 1%) &
					FOR LOOP% = ARRAY_LOOP% TO KEYS_SCROLL::BOT_ARRAY

				WINDOW_LINES$(KEYS_SCROLL::BOT_ARRAY) = ""

				X% = DSPL_SCROLL(KEYS_SCROLL, &
					WINDOW_LINES$(), &
					0%, &
					"PAINT")

				KEYS_SCROLL::BOT_ARRAY, KEYS_SCROLL::END_ELEMENT = &
					KEYS_SCROLL::BOT_ARRAY - 1%
				GOTO KeyMaintenance

			CASE SMG$K_TRM_DOWN
				ENTERCHANGE_LOOP% = ENTERCHANGE_LOOP% + 1%
				ENTERCHANGE_LOOP% = 3% IF ENTERCHANGE_LOOP% > 3%
				GOTO KeyEnterLoop

			CASE SMG$K_TRM_UP
				ENTERCHANGE_LOOP% = ENTERCHANGE_LOOP% - 1%
				ENTERCHANGE_LOOP% = 1% IF ENTERCHANGE_LOOP% < 1%
				GOTO KeyEnterLoop

			CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

			CASE ELSE
				CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
				GOTO KeyEnterLoop

			END SELECT

		NEXT ENTERCHANGE_LOOP%

		FOR LOOP% = KEYS_SCROLL::BOT_ARRAY TO ARRAY_LOOP% STEP -1%
			WINDOW_FIELDS$(LOOP% + 1%)= WINDOW_FIELDS$(LOOP%)
			WINDOW_DUP$(LOOP% + 1%) = WINDOW_DUP$(LOOP%)
			WINDOW_CHG$(LOOP% + 1%) = WINDOW_CHG$(LOOP%)
		NEXT LOOP%

		WINDOW_FIELDS$(ARRAY_LOOP%) = WINDOW_FIELDS$
		WINDOW_DUP$(ARRAY_LOOP%) = WINDOW_DUP$
		WINDOW_CHG$(ARRAY_LOOP%) = WINDOW_CHG$

		!
		! Re-order the number sequence
		!
		FOR ARRAY_LOOP% = 1% TO KEYS_SCROLL::BOT_ARRAY
			IF EDIT$(WINDOW_FIELDS$(ARRAY_LOOP%), 2% + 4%) <> ""
			THEN
				WINDOW_KEYS$(ARRAY_LOOP%) = &
					FORMAT$(ARRAY_LOOP% - 1%, "<0>###")
				GOSUB KeySetUpArray
			ELSE
				GOTO KeyNext
			END IF
		NEXT ARRAY_LOOP%

 KeyNext:	KEYS_SCROLL::FIND_LINE = KEYS_SCROLL::CUR_LINE + 1%

		ARRAY_LOOP% = DSPL_SCROLL(KEYS_SCROLL, WINDOW_LINES$(), &
			0%, "FIND")

		ARRAY_LOOP% = KEYS_SCROLL::CUR_LINE

		GOTO KeyNextEntry

	CASE "C"	! Change a field
		IF ARRAY_LOOP% = KEYS_SCROLL::BOT_ARRAY
		THEN
			CALL ENTR_3MESSAGE(SCOPE, &
				"Line must be added before a change can be made", 0%)
			GOTO 4000
		END IF

		WINDOW_KEYS$ = WINDOW_KEYS$(ARRAY_LOOP%)
		WINDOW_FIELDS$ = WINDOW_FIELDS$(ARRAY_LOOP%)
		WINDOW_DUP$ = WINDOW_DUP$(ARRAY_LOOP%)
		WINDOW_CHG$ = WINDOW_CHG$(ARRAY_LOOP%)
		ENTERCHANGE_LOOP% = 0%

 KeyChangeLoop:
4200		!****************************************************
		! Change data to screen
		!****************************************************
		FUN_NAME$ = "Change"
		ITM$ = EDIT$(ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
			"", "Item to change", "  ", 4%, "", ""), -1%)
		ITM$ = "0" + ITM$ IF LEN(ITM$) < 2% AND ITM$ <> ""

		SELECT SCOPE::SCOPE_EXIT

		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			CALL ENTR_3MESSAGE(SCOPE, "Change aborted", 1%)
			GOTO KeyMaintenance

		CASE SMG$K_TRM_DOWN
			ENTERCHANGE_LOOP% = ENTERCHANGE_LOOP% + 1%
			ENTERCHANGE_LOOP% = 3% IF ENTERCHANGE_LOOP% > 3%
			ITM$ = NUM1$(ENTERCHANGE_LOOP%)

		CASE SMG$K_TRM_UP
			ENTERCHANGE_LOOP% = 4% IF ENTERCHANGE_LOOP% = 0%
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
			GOTO KeyChangeLoop

		END SELECT

		GOTO KeyMaintenance IF ITM$ = ""

 KeyChangeLoop1:
		WHEN ERROR IN
			ENTERCHANGE_LOOP% = VAL%(ITM$)
		USE
			CONTINUE 4200
		END WHEN

4210		GOTO KeyChangeLoop IF ENTERCHANGE_LOOP% < 0% OR &
			ENTERCHANGE_LOOP% > 3%

		GOSUB KeyEnterChange

		SELECT SCOPE::SCOPE_EXIT
		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOSUB KeySetUpArray
			X% = DSPL_SCROLL(KEYS_SCROLL, &
				WINDOW_LINES$(), &
				0%, &
				"PAINT")
			CALL ENTR_3MESSAGE(SCOPE, "Change aborted", 1%)
			GOTO KeyMaintenance

		CASE SMG$K_TRM_DOWN
			ENTERCHANGE_LOOP% = ENTERCHANGE_LOOP% + 1%
			ENTERCHANGE_LOOP% = 3% IF ENTERCHANGE_LOOP% > 3%
			ITM$ = NUM1$(ENTERCHANGE_LOOP%)
			GOTO KeyChangeLoop1

		CASE SMG$K_TRM_UP
			ENTERCHANGE_LOOP% = 4% IF ENTERCHANGE_LOOP% = 0%
			ENTERCHANGE_LOOP% = ENTERCHANGE_LOOP% - 1%
			ENTERCHANGE_LOOP% = 1% IF ENTERCHANGE_LOOP% < 1%
			ITM$ = NUM1$(ENTERCHANGE_LOOP%)
			GOTO KeyChangeLoop1

		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO KeyChangeLoop

		END SELECT

		WINDOW_FIELDS$(ARRAY_LOOP%) = WINDOW_FIELDS$
		WINDOW_DUP$(ARRAY_LOOP%) = WINDOW_DUP$
		WINDOW_CHG$(ARRAY_LOOP%) = WINDOW_CHG$
		WINDOW_CHG$(ARRAY_LOOP%) = "N" IF ARRAY_LOOP% = 1%

		GOSUB KeySetUpArray

		GOTO KeyChangeLoop

	!
	! Erase a line in DDL
	!
	CASE "E"
		IF ARRAY_LOOP% = KEYS_SCROLL::BOT_ARRAY
		THEN
			CALL ENTR_3MESSAGE(SCOPE, "Line cannot be erased", 0%)
			GOTO 4000
		END IF

 KeyEraseLoop:
		INP$ = EDIT$(ENTR_3YESNO(SCOPE, SMG_SCREEN_DATA%, "", &
			"Confirm deletion ", "", 0%, "'E", ""), -1%)

		SELECT SCOPE::SCOPE_EXIT
		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ

			GOTO KeyMaintenance

		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO KeyEraseLoop
		END SELECT

		GOTO KeyMaintenance IF INP$ <> "Y"

		FOR LOOP% = ARRAY_LOOP%  TO KEYS_SCROLL::BOT_ARRAY
			WINDOW_FIELDS$(LOOP%) = WINDOW_FIELDS$(LOOP% + 1%)
			WINDOW_DUP$(LOOP%) = WINDOW_DUP$(LOOP% + 1%)
			WINDOW_CHG$(LOOP%) = WINDOW_CHG$(LOOP% + 1%)
			WINDOW_CHG$(LOOP%) = "N" IF LOOP% = 1%
			WINDOW_LINES$(LOOP%) = WINDOW_LINES$(LOOP% + 1%)
		NEXT LOOP%

		!
		! Re-order the number sequence
		!
		FOR ARRAY_LOOP% = 1% TO KEYS_SCROLL::BOT_ARRAY
			IF EDIT$(WINDOW_FIELDS$(ARRAY_LOOP%), 2% + 4%) <> ""
			THEN
				WINDOW_KEYS$(ARRAY_LOOP%) = &
					FORMAT$(ARRAY_LOOP% - 1%, "<0>###")
				GOSUB KeySetUpArray
			ELSE
				GOTO KeyErase
			END IF
		NEXT ARRAY_LOOP%

 KeyErase:	KEYS_SCROLL::BOT_ARRAY, KEYS_SCROLL::END_ELEMENT = &
			KEYS_SCROLL::BOT_ARRAY - 1%

		X% = DSPL_SCROLL(KEYS_SCROLL, WINDOW_LINES$(), &
			0%, "PAINT")

	!
	! Help
	!
	CASE "H"
		CALL HELP_3MESSAGE(SCOPE, "", "PROG", SCOPE::PRG_PROGRAM, "HELP")

	!
	! Exit
	!
	CASE "X"
		GOTO Ret4000

	END SELECT

	GOTO KeyMaintenance

 Ret4000:
	RETURN

	%Page

11100	!*********************************************************************
	! Enter change key window
	!*********************************************************************
 KeyEnterChange:
	TEMP_PRG_ITEM$ = SCOPE::PRG_ITEM

	SCOPE::PRG_ITEM = "KEY" + FORMAT$(ENTERCHANGE_LOOP%, "<0>##")

	SELECT ENTERCHANGE_LOOP%
	CASE 1%
		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCROLL, &
			MID(WINDOW_LINES$(ARRAY_LOOP%), 5%, 60%),, &
			7%,, SMG$M_REVERSE)

		WINDOW_FIELDS$ = EDIT$(ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
			"", FUN_NAME$ + " name ", &
			SPACE$(60%), 32%, "'E", WINDOW_FIELDS$), -1%)

	CASE 2%
		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCROLL, &
			MID$(WINDOW_LINES$(ARRAY_LOOP%), 67%, 1%),, &
			69%,, SMG$M_REVERSE)

		WINDOW_DUP$ = "N"
		WINDOW_DUP$ = EDIT$(ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
			"", FUN_NAME$ + " duplicates (Y/N) ", &
			SPACE$(1%), 32%, "'E", WINDOW_DUP$), -1%)

	CASE 3%

		WINDOW_CHG$ = "N" IF WINDOW_CHG$ <> "Y"

		IF ARRAY_LOOP% = 1%
		THEN
			SCOPE::SCOPE_EXIT = 13%
			GOTO KeyEnterChange1
		END IF

		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCROLL, &
			MID$(WINDOW_LINES$(ARRAY_LOOP%), 72%, 1%),, &
			74%,, SMG$M_REVERSE)

		WINDOW_CHG$ = EDIT$(ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
			"", FUN_NAME$ + " changes (Y/N) ", &
			SPACE$(1%), 32%, "'E", WINDOW_CHG$), -1%)

	END SELECT

	SELECT SCOPE::SCOPE_EXIT
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, &
		SMG$K_TRM_DOWN, SMG$K_TRM_UP

		GOTO KeyEnterChange1

	CASE SMG$K_TRM_F14

		!
		! Look up fields
		!
		X% = ENTR_3CHOICE(SCOPE, "", "", FIELD_LINES$(), KEYS$, &
			2% + 8% + 64%, "  FIELD      DATA" + &
			" TYPE SIZE DESCRIPTION", "013,023,028", 0%)

		GOTO KeyEnterChange IF X% <= 0%

		TEMP$ = ""
		TEMP$ = ", " IF WINDOW_FIELDS$ <> ""
		WINDOW_FIELDS$ = TRM$(WINDOW_FIELDS$) + TEMP$ + &
			TRM$(DDL::FIELD_NAME(X%))
		GOTO KeyEnterChange
	END SELECT

	!
	! Tests for each field
	!
	SELECT ENTERCHANGE_LOOP%
	CASE 1%
		TEMP$ = WINDOW_FIELDS$
		WHILE TEMP$ <> ""
			TEMP% = 0%
			TEMP% = INSTR(1%, TEMP$, ",")
			TEMP% = LEN(TEMP$) + 1% IF TEMP% = 0%
			TEMP1$ = EDIT$(LEFT(TEMP$, TEMP% - 1%), -1%)
			TEMP$ = EDIT$(RIGHT(TEMP$, TEMP% + 1%), -1%)
			TEST% = 0%
			TEST% = -1% IF TEMP1$ = EDIT$( &
				DDL::FIELD_NAME(LOOP%), -1%) &
					FOR LOOP% = 1% TO FC%
			IF TEST% = 0%
			THEN
				CALL ENTR_3MESSAGE(SCOPE, 'Undefined field:  "' + &
					TEMP1$ + '"  - Please'+ &
					" enter again", 1%)
				GOTO KeyEnterChange
			END IF
		NEXT

	CASE 2%
		IF WINDOW_DUP$ <> "Y" AND &
			WINDOW_DUP$ <> "N" AND &
			WINDOW_DUP$ <> ""
		THEN
			CALL ENTR_3MESSAGE(SCOPE, &
				"Must be either 'Y' or 'N'", 0%)
			GOTO KeyEnterChange
		END IF

	CASE 3%
		IF WINDOW_CHG$ <> "Y" AND &
			WINDOW_CHG$ <> "N" AND &
			WINDOW_CHG$ <> ""
		THEN
			CALL ENTR_3MESSAGE(SCOPE, &
				"Must be either 'Y' or 'N'", 0%)
			GOTO KeyEnterChange
		END IF
	END SELECT

	GOTO KeyEnterChange IF WINDOW_FIELDS$ = ""

 KeyEnterChange1:
	WINDOW_LINES$(ARRAY_LOOP%) = &
		LEFT(WINDOW_KEYS$ + "   ", 3%) + " " + &
		LEFT(WINDOW_FIELDS$ + STRING$(60% - &
		LEN(WINDOW_FIELDS$), 32%), 60%) + "  " + &
		LEFT(WINDOW_DUP$ + " ", 1%) + "    " + &
		LEFT(WINDOW_CHG$ + " ", 1%)

	X% = DSPL_SCROLL(KEYS_SCROLL, WINDOW_LINES$(), 0%, "PAINT")

	SCOPE::PRG_ITEM = TEMP_PRG_ITEM$

	RETURN

	%PAGE

 KeySetUpArray:
	!
	! Set arrays into line to print
	WINDOW_LINES$(ARRAY_LOOP%) = &
		WINDOW_KEYS$(ARRAY_LOOP%) + " " + &
		LEFT(WINDOW_FIELDS$(ARRAY_LOOP%) + STRING$(60% - &
		LEN(WINDOW_FIELDS$(ARRAY_LOOP%)), 32%), 60%) + "  " + &
		LEFT(WINDOW_DUP$(ARRAY_LOOP%) + " ", 1%) + "    " + &
		LEFT(WINDOW_CHG$(ARRAY_LOOP%) + " ", 1%)

	RETURN

	%Page

 LoadTemplateArrays:
12000	!
	! Open file extension template file
	!
	CLOSE TEXT_FILE.CH

	WHEN ERROR IN
		OPEN FILE_EXTENSION$ FOR INPUT AS FILE TEXT_FILE.CH, &
			ACCESS READ, ALLOW MODIFY, &
			RECORDSIZE 132%
	USE
		CONTINUE 12020
	END WHEN

	FILE_EXTENSION% = 0%

12010	!
	! Read attribute type file
	!
	WHEN ERROR IN
		LINPUT #TEXT_FILE.CH, INP$
	USE
		CONTINUE 12020
	END WHEN

	WORK_LINE$ = EDIT$(INP$, 8% + 128%)

	TEMP% = INSTR(1%, WORK_LINE$, "<")
	TEMP% = LEN(WORK_LINE$) + 1% IF TEMP% = 0%
	FILE_EXTENSION% = FILE_EXTENSION% + 1%
	FILE_EXTENSION$(FILE_EXTENSION%) = LEFT(WORK_LINE$, TEMP% - 1%)

	TEMP$ = "<DESC>"
	TEMP% = INSTR(1%, WORK_LINE$, TEMP$)
	IF TEMP%
	THEN
		TEMP1% = INSTR(TEMP% + 1%, WORK_LINE$, "<")
		TEMP1% = LEN(WORK_LINE$) + 1% IF TEMP1% = 0%
		FILE_EXTENSION_DESC$(FILE_EXTENSION%) = &
			FILE_EXTENSION$(FILE_EXTENSION%) + " " + &
			TRM$(MID(WORK_LINE$, TEMP% + LEN(TEMP$), &
			TEMP1% - (TEMP% + LEN(TEMP$))))
	END IF

	GOTO 12010

12020	!
	! Open template file
	!
	CLOSE TEXT_FILE.CH

	WHEN ERROR IN
		OPEN FILE_TYPE$ FOR INPUT AS FILE TEXT_FILE.CH, &
			ACCESS READ, ALLOW MODIFY, &
			RECORDSIZE 132%
	USE
		CONTINUE 12040
	END WHEN

	FILE_TYPE% = 0%

12030	!
	! Read data type file
	!
	WHEN ERROR IN
		LINPUT #TEXT_FILE.CH, INP$
	USE
		CONTINUE 12040
	END WHEN

	WORK_LINE$ = EDIT$(INP$, 8% + 128%)

	TEMP% = INSTR(1%, WORK_LINE$, "<")
	TEMP% = LEN(WORK_LINE$) + 1% IF TEMP% = 0%
	FILE_TYPE% = FILE_TYPE% + 1%
	FILE_TYPE_DEF$(FILE_TYPE%) = LEFT(WORK_LINE$, TEMP% - 1%)

	TEMP$ = "<DESC>"
	TEMP% = INSTR(1%, WORK_LINE$, TEMP$)
	IF TEMP%
	THEN
		TEMP1% = INSTR(TEMP% + 1%, WORK_LINE$, "<")
		TEMP1% = LEN(WORK_LINE$) + 1% IF TEMP1% = 0%
		FILE_TYPE_DEF_DESC$(FILE_TYPE%) = &
			FILE_TYPE_DEF$(FILE_TYPE%) + "   " + &
			TRM$(MID(WORK_LINE$, TEMP% + LEN(TEMP$), &
			TEMP1% - (TEMP% + LEN(TEMP$))))
	END IF

	GOTO 12030

12040	!
	! End of template files
	!
	RETURN

	%Page

13000	!---------------------------------------------------------------------
	! Create open statements
	!---------------------------------------------------------------------
	CLOSE TEXT_FILE.CH

	FOR OPEN_LOOP% = 1% TO OPENS_SCROLL::BOT_ARRAY - 1%
		FILE_TYPE$ = FILE_TYPE$(OPEN_LOOP%)

		!
		! Don't create certian files
		!
		SELECT FILE_TYPE$
		CASE "DDL", "HB", "H"
			GOTO 13090
		END SELECT

		WHEN ERROR IN
			OPEN "SOURCE:[" + TRM$(LOCATION) + ".OPEN]" + &
				TRM$(F_NAME$) + "." + FILE_TYPE$ &
				FOR OUTPUT AS FILE TEXT_FILE.CH, &
				RECORDSIZE 132%
		USE
			CONTINUE 14000 IF ERR = 1%
			EXIT HANDLER
		END WHEN


		SELECT FILE_TYPE$

		CASE "ARC"
			OPEN_METHOD$ = "create, open read/write"
			TEMP_TYPE$ = "_ARC"

		CASE "CRE"
			OPEN_METHOD$ = "create, open read/write"
			TEMP_TYPE$ = ""

		CASE "MOD"
			OPEN_METHOD$ = "open read/write"
			TEMP_TYPE$ = ""

		CASE "OPN"
			OPEN_METHOD$ = "open read only"
			TEMP_TYPE$ = ""

		CASE "UPD"
			OPEN_METHOD$ = "open update only"
			TEMP_TYPE$ = ""

		CASE "TEN"
			OPEN_METHOD$ = "open temporary file"
			TEMP_TYPE$ = ""

		CASE "PUR"
			OPEN_METHOD$ = "create, open read/write purge"
			TEMP_TYPE$ = "_OLD"

		CASE "OLD"
			OPEN_METHOD$ = "open read only"
			TEMP_TYPE$ = "_OLD"

		CASE "NEW"
			OPEN_METHOD$ = "create _new, open read/write purge"
			TEMP_TYPE$ = "_NEW"

		CASE "NEU"
			OPEN_METHOD$ = "No create _new, open read/write purge"
			TEMP_TYPE$ = "_NEW"

		CASE "PST"
			OPEN_METHOD$ = "create, modify, allow read"
			TEMP_TYPE$ = ""

		CASE ELSE
			OPEN_METHOD$ = "unknown"
			TEMP_TYPE$ = ""

		END SELECT

13010		!
		! Comment at top of file
		!
		PRINT #TEXT_FILE.CH, '9'C + "!" + STRING$(70%, 61%)
		PRINT #TEXT_FILE.CH, '9'C + "! " + TRM$(F_NAME$) + &
			" file (" + OPEN_METHOD$ + ")"
		PRINT #TEXT_FILE.CH, '9'C + "!" + STRING$(70%, 61%)
		PRINT #TEXT_FILE.CH

		!
		! Assign channel
		!
		PRINT #TEXT_FILE.CH, &
			'9'C; "CALL ASSG_CHANNEL("; TRM$(F_NAME$); &
			".CH" + TEMP_TYPE$ + "%, STAT%)"

		!
		! Get device
		!
		IF FILE_TYPE$ <> "ARC"
		THEN
			PRINT #TEXT_FILE.CH, &
				'9'C; "CALL READ_DEVICE('"; &
				TRM$(F_NAME$); "',"; TRM$(F_NAME$); &
				".DEV$, STAT%)"
		ELSE
			F_NAME_ARC$ = TRM$(F_NAME$) + "_ARC"
			PRINT #TEXT_FILE.CH, &
				'9'C; "CALL READ_DEVICE('"; &
				TRM$(F_NAME_ARC$); "',"; TRM$(F_NAME$); ".DEV$, STAT%)"
		END IF

		!
		! Get protection code
		!
		SELECT FILE_TYPE$

		CASE "ARC", "CRE", "PUR", "PST"

			IF FILE_TYPE$ <> "ARC"
			THEN
				PRINT #TEXT_FILE.CH, &
					'9'C + "CALL READ_PROTECTION('"; &
					TRM$(F_NAME$); "',"; &
					TRM$(F_NAME$) + ".PRO$, STAT%)"
			ELSE
				PRINT #TEXT_FILE.CH, &
					'9'C + "CALL READ_PROTECTION('"; &
					TRM$(F_NAME_ARC$); "',"; &
					TRM$(F_NAME$) + ".PRO$, STAT%)"
			END IF

			PRINT #TEXT_FILE.CH, &
				'9'C + "CALL READ_CURPROTECTION(OLD_PROT$, STAT%)"
			PRINT #TEXT_FILE.CH, '9'C + "CALL WRIT_CURPROTECTION(" + &
				TRM$(F_NAME$) + ".PRO$, STAT%)"
		END SELECT

		PRINT #TEXT_FILE.CH

		!
		! Create name
		!
		SELECT FILE_TYPE$

		CASE "PUR", "OLD", "NEW", "ARC", "NEU"
			TEMP$ = EDIT$(OPN::FILE_NAME, 8% + 128%)
			TEMP$ = LEFT(TEMP$, LEN(TEMP$) - 1%) + &
				TEMP_TYPE$ + &
				RIGHT(TEMP$, LEN(TEMP$))
			PRINT #TEXT_FILE.CH, &
				'9'C; TRM$(F_NAME$); &
				".NAME"; TEMP_TYPE$; "$ = "; &
				TEMP$

		CASE ELSE
			PRINT #TEXT_FILE.CH, &
				'9'C; TRM$(F_NAME$); &
				".NAME"; TEMP_TYPE$; "$ = "; &
				EDIT$(OPN::FILE_NAME, 8% + 128%)
		END SELECT

		PRINT #TEXT_FILE.CH

		SELECT FILE_TYPE$
		CASE "CRE", "PST", "ARC", "NEU"
			PRINT #TEXT_FILE.CH, '9'C + "OPEN " + &
				TRM$(F_NAME$) + ".NAME" + TEMP_TYPE$ + "$" + &
				" AS FILE " + TRM$(F_NAME$) + &
				".CH" + TEMP_TYPE$ + "%, &"

		CASE "TEN"
			PRINT #TEXT_FILE.CH, '9'C + "OPEN " + &
				TRM$(F_NAME$) + ".NAME" + TEMP_TYPE$ + "$" + &
				" FOR OUTPUT AS FILE " + &
				TRM$(F_NAME$) + &
				".CH" + TEMP_TYPE$ + "%, &"

		CASE "PUR", "NEW"
			PRINT #TEXT_FILE.CH, '9'C + "OPEN " + &
				TRM$(F_NAME$) + ".NAME" + TEMP_TYPE$ + "$" + &
				" FOR OUTPUT AS FILE " + &
				TRM$(F_NAME$) + &
				".CH" + TEMP_TYPE$ + "%, &"

		CASE ELSE
			PRINT #TEXT_FILE.CH, '9'C + "OPEN " + &
				TRM$(F_NAME$) + ".NAME" + TEMP_TYPE$ + "$" + &
				" FOR INPUT AS FILE " + &
				TRM$(F_NAME$) + &
				".CH" + TEMP_TYPE$ + "%, &"
		END SELECT

		PRINT #TEXT_FILE.CH, '9'C + '9'C + &
			"ORGANIZATION " + EDIT$(OPN::ORGNIZATION, 8% + 128%) + &
			" " + EDIT$(OPN::STRCTURE, 8% + 128%) + ", &"
		PRINT #TEXT_FILE.CH, '9'C + '9'C + &
			"MAP " + TRM$(F_NAME$) + ", &"

		SELECT FILE_TYPE$

		CASE "ARC", "CRE", "PUR", "PST", "NEW", "NEU"

			PRINT #TEXT_FILE.CH, '9'C + '9'C + "BUFFER 32%, &"
		END SELECT

		!
		! Handle keys in an indexed file
		!
		IF EDIT$(OPN::ORGNIZATION, -1%) = "INDEXED"
		THEN
			!
			! Once for each key in the open
			!
			FOR LOOP% = 1% TO KEYS_SCROLL::BOT_ARRAY - 1%

				WORK$ = WINDOW_FIELDS$(LOOP%)

				IF WORK$ <> ""
				THEN
					!
					! Handle primary/alternate keys
					!
					IF LOOP% > 1%
					THEN
						PRINT #TEXT_FILE.CH, &
							'9'C + &
							'9'C + &
							"ALTERNATE KEY &"
					ELSE
						PRINT #TEXT_FILE.CH, &
							'9'C + &
							'9'C + &
							"PRIMARY KEY &"
					END IF

					TEST% = 0%
					TEMP$ = ""
					WHILE WORK$ <> ""
						TEMP% = INSTR(1%, WORK$, ",")
						IF TEST% = 0% AND TEMP%
						THEN
							TEST% = -1%
							PRINT #TEXT_FILE.CH, &
								'9'C + &
								'9'C + "( &"
						END IF
						TEMP%  = LEN(WORK$) + 1% &
							IF TEMP% = 0%
						TEMP1$ = EDIT$(LEFT(WORK$, &
							TEMP% - 1%), -1%)
						WORK$  = EDIT$(RIGHT(WORK$, &
							TEMP% + 1%), -1%)
						KEYS$ = TRM$(F_NAME$) + "::" + &
							TEMP1$
						IF TEST%
						THEN
							KEYS$ = KEYS$ + "," &
								UNLESS WORK$ = ""
							KEYS$ = KEYS$ + " &"
						ELSE
							KEYS$ = KEYS$ + "," &
								IF WINDOW_DUP$(LOOP%) <> "Y" AND WINDOW_CHG$(LOOP%) <> "Y"
							KEYS$ = KEYS$ + " &"
						END IF
						PRINT #TEXT_FILE.CH, &
							'9'C + '9'C + &
							'9'C + KEYS$
					NEXT

					KEYS$ = ""
					IF TEST%
					THEN
						KEYS$ = KEYS$ + ")" + '9'C
					ELSE
						KEYS$ = KEYS$ + '9'C
					END IF

					KEYS$ = KEYS$ + "DUPLICATES " &
						IF WINDOW_DUP$(LOOP%) = "Y"
					KEYS$ = KEYS$ + "CHANGES" &
						IF WINDOW_CHG$(LOOP%) = "Y" &
						AND LOOP% > 1%

					IF EDIT$(KEYS$, -1%) <> ""
					THEN
						KEYS$ = KEYS$ + ", &"
						PRINT #TEXT_FILE.CH,'9'C + &
							'9'C + KEYS$
					END IF
				END IF
			NEXT LOOP%
		END IF

		TEMP$ = ""
		FOR LOOP% = 1% TO FILE_TYPE%
			TEMP$ = RIGHT(FILE_TYPE_DEF_DESC$(LOOP%), 7%) &
				IF FILE_TYPE_DEF$(LOOP%) = FILE_TYPE$
		NEXT LOOP%

		PRINT #TEXT_FILE.CH, '9'C + '9'C + TEMP$

		SELECT FILE_TYPE$

		CASE "ARC", "CRE", "PUR", "PST"

			PRINT #TEXT_FILE.CH
			PRINT #TEXT_FILE.CH, '9'C + &
				"CALL WRIT_CURPROTECTION(OLD_PROT$, STAT%)"
		END SELECT

		PRINT #TEXT_FILE.CH

		CLOSE TEXT_FILE.CH

13090	NEXT OPEN_LOOP%

	RETURN

	%Page

14000	!=====================================================================
	! Create directory
	!=====================================================================
	CALL ENTR_3MESSAGE(SCOPE, &
		"Creating directory for " + TRM$(SYSTEM_NAME), 1%)

	SMG_STATUS% = LIB$SPAWN("CREATE/DIR SOURCE:[" + TRM$(LOCATION) + &
		".OPEN]")

	IF (SMG_STATUS% AND 1%) = 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "Error " + NUM1$(SMG_STATUS%) + &
			" has occured", 0%)
		GOTO ExitProgram
	END IF

	SLEEP 1%

	SMG_STATUS% = SMG$REPAINT_SCREEN(SCOPE::SMG_PBID)

	CALL ENTR_3MESSAGE(SCOPE, "", 1%)

	GOTO 13000

	%Page

 InitKeys:
	!
	! Initialize the window fields for the key window
	!
	OPN::KEYS(LOOP% + 1%) = OPN::KEYS(LOOP%) FOR LOOP% = OPN::KEYS_NUM TO &
		0% STEP -1%

	OPN::KEYS_NUM = OPN::KEYS_NUM + 1% IF EDIT$(OPN::KEYS(0%), 4%) <> ""

	FOR LOOP% = 1% TO OPN::KEYS_NUM
		V% = INSTR(1%, OPN::KEYS(LOOP%), "::")

		WHILE V% > 0%
			U% = STR$FIND_FIRST_IN_SET(RIGHT( &
				OPN::KEYS(LOOP%), V% + 2%), " ,)")
			X$ = MID(OPN::KEYS(LOOP%), V% + 2%, U% - 1%)
			TEST% =  0%
			TEST% = -1% IF X$ = EDIT$(DDL::FIELD_NAME(Z%), &
				-1%) FOR Z% = 1% TO FC%
			IF TEST% <> 0%
			THEN
				TEMP$ = ""
				TEMP$ = "," IF WINDOW_FIELDS$(LOOP%) <> ""
				WINDOW_FIELDS$(LOOP%) = WINDOW_FIELDS$(LOOP%) &
					+ TEMP$ + X$
			END IF

			V% = INSTR(V% + U% + 2%, OPN::KEYS(LOOP%), "::")
		NEXT

		GOTO InitKeys1 IF WINDOW_FIELDS$(LOOP%) = ""

		ARRAY_LOOP%  = LOOP%

		WINDOW_KEYS$(LOOP%) = FORMAT$(LOOP% - 1%, "<0>###")
		WINDOW_DUP$(LOOP%) = "N"
		WINDOW_DUP$(LOOP%) = "Y" &
			IF INSTR(1%, OPN::KEYS(LOOP%), "DUPLICATES")
		WINDOW_CHG$(LOOP%) = "N"
		WINDOW_CHG$(LOOP%) = "Y" &
			IF INSTR(1%, OPN::KEYS(LOOP%), "CHANGES")

		GOSUB KeySetUpArray
 InitKeys1:
	NEXT LOOP%

	RETURN

32767	END SUB
