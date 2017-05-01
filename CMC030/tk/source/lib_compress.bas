1	%TITLE "Document a Menu"
	%SBTTL "LIB_COMPRESS"
	%IDENT "V3.6a Calico"

	!
	!	COPYRIGHT (C) 1987 BY
	!	Computer Management Center, Idaho Falls, Idaho.
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
	! Abstract:HELP
	!	.p
	!	This program will read a menu source file, and from
	!	that, will create a documentation book.
	!
	! Index:
	!	Documentation
	!	Menu
	!
	! Option:
	!
	!
	! Input:
	!
	!	Name of file to document
	!
	!	Name of LaTeX file to create.
	!
	! Output:
	!
	!	LaTeX documentation file
	!
	! Example:
	!
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:LIB_COMPRESS
	!	$ LINK/EXE=TK_EXE: LIB_COMPRESS, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE LIB_COMPRESS.OBJ;*
	!
	! Author:
	!
	!	10/26/87 - Kevin Handy
	!
	! Modification history:
	!
	!	11/18/87 - Kevin Handy
	!		Added SMG stuff to make it look better.
	!
	!	06/27/89 - Kevin Handy
	!		Modified to use READ_INITIALIZE.
	!
	!	06/18/90 - Kevin Handy
	!		Increased number of items allowed in a library.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/29/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:LIBRARY.COM"

	!
	! External functions
	!
	EXTERNAL LONG          TK_DOCU_GETMODULES

	!
	! Declare variables
	!
	MAP (LBR_JUNKJUNK) TEXT$ = 132%, TEXT1$ = 256%		! Read buffer
	DECLARE RFA TXRFA, NULL.RFA, WIN_RFA, WIN1_RFA

	MAP (TK_DOCU) &
		IN_LEVEL%, &
		MODCOUNT%, &
		MODNAME$(2000%) = 50%, &
		RFA MODRFA(2000%)

	DIM MAIN_LOOP%(20%)
	DECLARE RFA LAST_RFA

	!
	! Create array to hold legal commands
	!
	DECLARE INTEGER CONSTANT MAX_COMMAND = 100%
	DIM COMMAND_LIST$(MAX_COMMAND),	COMMAND_LIST%(MAX_COMMAND)

	MAP (IOBUF) LONG IO_BUF(6%)
	MAP (IOBUF) WORD IO_BUF_W(12%)
	MAP (IOBUF1) NAME.BUFFER$ = 50%

	DECLARE LONG CONSTANT FSCN$_NAME = 6

	MAP (LBR_JUNK_JUNK) C_ARRAY%(19%)
	MAP (LBR_JUNK_JUNK) C_ARRAY$ = 88%

	%PAGE

1000	!*******************************************************************
	! Allocate channels
	!*******************************************************************

	SOURCE.CH% = 5%
	LATEX.CH% = 6%

	!*******************************************************************
	! Initilize for runoff conversion
	!*******************************************************************

	CALL READ_INITIALIZE

	SMG_STATUS% = SMG$BEGIN_PASTEBOARD_UPDATE(SCOPE::SMG_PBID)

	!
	! Create IO window
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(4%, 78%, TK_INPUT%, &
		SMG$M_BORDER)
	SMG_STATUS% = SMG$LABEL_BORDER(TK_INPUT%, "Library Compress")
	SMG_STATUS% = SMG$PUT_CHARS(TK_INPUT%, "Input file", 2%, 1%)
	SMG_STATUS% = SMG$PUT_CHARS(TK_INPUT%, "Output file", 3%, 1%)
	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(TK_INPUT%, SCOPE::SMG_PBID, 2%, 2%)

	!
	! Create entertainment window
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(13%, 78%, TK_MESSAGE%, &
		SMG$M_BORDER)
	SMG_STATUS% = SMG$LABEL_BORDER(TK_MESSAGE%, "Messages")
	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(TK_MESSAGE%, SCOPE::SMG_PBID, 7%, 2%)
	SMG_STATUS% = SMG$SET_CURSOR_ABS(TK_MESSAGE%, 1%, 1%)

	SMG_STATUS% = SMG$END_PASTEBOARD_UPDATE(SCOPE::SMG_PBID)

	%PAGE


1200	!*******************************************************************
	! Get menu source file
	!*******************************************************************

	LIB_NAME$ = ENTR_3STRING(SCOPE, TK_INPUT%, "2;13", &
		"Name of input file <.TLB> ", "REF:HELP_GL.TLB" + SPACE$(20%), &
		0%, "'E", "")

	SELECT SCOPE::SCOPE_EXIT
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE SMG$K_TRM_UP
		GOTO 1200

	CASE 10%, 12%, 13%, 0%, SMG$K_TRM_DOWN, SMG$K_TRM_DO

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 1200
	END SELECT

	!
	! Set up the control structure if necessary
	!
	ST% = LBR$INI_CONTROL(LIB_INPUT%, LBR$C_READ)

	IF (ST% AND 1%) = 0%
	THEN
		PRINT "Unable to initilize library"; ST%
		STOP
	END IF

	!
	! Open the library function
	!
	ST% = LBR$OPEN(LIB_INPUT%, LIB_NAME$, , "REF:.TLB")

	IF (ST% AND 1%) = 0%
	THEN
		PRINT "Error in LIB open "; ST%
		STOP
	END IF


1400	!*******************************************************************
	! Get output file name
	!*******************************************************************

	OUT_NAME$ = ENTR_3STRING(SCOPE, TK_INPUT%, "3;13", &
		"Name of output file <.TLB> ", LIB_NAME$, &
		0%, "'E", "")

	SELECT SCOPE::SCOPE_EXIT
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE SMG$K_TRM_UP
		GOTO 1200

	CASE SMG$K_TRM_DOWN
		GOTO 1400

	CASE 10%, 12%, 13%, 0%, SMG$K_TRM_DO

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 1400
	END SELECT

	ST% = LBR$INI_CONTROL(LIB_OUTPUT%, LBR$C_CREATE, LBR$C_TYP_TXT)

	PRINT "CREATE INI_CONTROL "; ST%

	IF (ST% AND 1%) = 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "Unable to initilize library", 0%)
		STOP
	END IF

	!
	! Open the library file
	!
	C_ARRAY%( 0%) = LBR$C_TYP_TXT		! Library type
	C_ARRAY%( 1%) = 39%			! Key length
	C_ARRAY%( 2%) = 11%			! Initial allocation
	C_ARRAY%( 3%) = 1%			! Number of keys
	C_ARRAY%( 4%) = 0%			! Additional chars in header
	C_ARRAY%( 5%) = 200%			! Preallocated indexes
	C_ARRAY%( 6%) = 0%			! History records
	C_ARRAY%( 7%) = 3%			! Format of library
	C_ARRAY%( 8%) = 0%			! Index casing

	ST% = LBR$OPEN(LIB_OUTPUT%, OUT_NAME$, C_ARRAY$ BY REF, ".TLB")

	PRINT "CREATE OPEN "; ST%

	IF (ST% AND 1%) = 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "Unable to initilize library", 0%)
		STOP
	END IF


2000	!*******************************************************************
	! Scan through file, handling one line at a time
	!*******************************************************************

	GOSUB 4000

2100	!*******************************************************************
	! Finish up file
	!*******************************************************************

	ST% = LBR$CLOSE(LIB_INPUT%)
	ST% = LBR$CLOSE(LIB_OUTPUT%)

 ExitProgram:
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE

4000	!*******************************************************************
	! Read in one help file, and convert to TeX format
	!*******************************************************************

	ST% = LBR$GET_INDEX(LIB_INPUT%, 1%, &
		TK_DOCU_GETMODULES, "*")

	LAST_TEXT$ = "XX"
	LAST_PRIKEY$ = ""

	FOR MAIN_LOOP% = 1% TO MODCOUNT%

		TEMP% = INSTR(1%, MODNAME$(MAIN_LOOP%), "$")
		TEMP% = INSTR(TEMP% + 1%, MODNAME$(MAIN_LOOP%), "$")
		TEMP$ = LEFT(MODNAME$(MAIN_LOOP%), TEMP%)

		IF TEMP$ <> LAST_PRIKEY$
		THEN
			LAST_TEXT$ = "XX"
			LAST_PRIKEY$ = TEMP$
		END IF

4010		!
		! Point to text
		!
		ST% = LBR$FIND(LIB_INPUT%, MODRFA(MAIN_LOOP%))

		IF (ST% AND 1%) = 0%
		THEN
			PRINT "Error in LIB find"; ST%; &
				MODNAME$(MAIN_LOOP%)
			GOTO LoopNext
		END IF

		!
		! Copy over text
		!
		FULL_TEXT$ = ""
 Loop:
		TEXT$ = ""
		ST% = LBR$GET_RECORD(LIB_INPUT%, TEXT$)

		IF (ST% AND 1%) = 1%
		THEN
			FULL_TEXT$ = FULL_TEXT$ + TRM$(TEXT$) + '10'C
			GOTO Loop
		END IF

		IF LAST_TEXT$ == FULL_TEXT$
		THEN
			ST% = LBR$INSERT_KEY(LIB_OUTPUT%, &
				MODNAME$(MAIN_LOOP%), LAST_RFA)
		IF (ST% AND 1%) = 0%
		THEN
			CALL ENTR_3MESSAGE(SCOPE, &
				"Unable to insert key library", 0%)
			STOP
		END IF

			ST% = SMG$PUT_LINE(TK_MESSAGE%, &
				MODNAME$(MAIN_LOOP%) + "  Duplicated")
		ELSE
			LAST_TEXT$ = FULL_TEXT$

			TEMP% = INSTR(1%, FULL_TEXT$, '10'C)
			WHILE TEMP%
				TEMP$ = LEFT(FULL_TEXT$, TEMP% - 1%)
				FULL_TEXT$ = RIGHT(FULL_TEXT$, TEMP% + 1%)
				ST% = LBR$PUT_RECORD(LIB_OUTPUT%, TEMP$, LAST_RFA)
				IF (ST% AND 1%) = 0%
				THEN
					CALL ENTR_3MESSAGE(SCOPE, &
						"Unable to add line library", 0%)
					STOP
				END IF

				TEMP% = INSTR(1%, FULL_TEXT$, '10'C)
			NEXT

			ST% = LBR$PUT_END(LIB_OUTPUT%)

			ST% = LBR$INSERT_KEY(LIB_OUTPUT%, &
				MODNAME$(MAIN_LOOP%), LAST_RFA)
	IF (ST% AND 1%) = 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "Unable to insert key library", 0%)
		STOP
	END IF

			ST% = SMG$PUT_LINE(TK_MESSAGE%, &
				MODNAME$(MAIN_LOOP%) + "  New")
		END IF

	!
	! Finish up loop (Faked Next)
	!
 LoopNext:
	NEXT MAIN_LOOP%

	RETURN

20000	END

21000	FUNCTION LONG TK_DOCU_GETMODULES(MODKEY$, RFA MODRFA)

	!
	! This function graps the names passed to it from the
	! LIB$SEARCH call
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	MAP (TK_DOCU) &
		IN_LEVEL%, &
		MODCOUNT%, &
		MODNAME$(2000%) = 50%, &
		RFA MODRFA(2000%)

	!
	! Add to list
	!
	MODCOUNT% = MODCOUNT% + 1%
	MODNAME$(MODCOUNT%) = MODKEY$
	MODRFA(MODCOUNT%) = MODRFA


 ExitFunction:
	TK_DOCU_GETMODULES = 1%

	END FUNCTION
