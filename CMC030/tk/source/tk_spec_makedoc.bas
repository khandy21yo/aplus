1	%TITLE "Document a Menu"
	%SBTTL "TK_SPEC_MAKEDOC"
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
	! Abstract:HELP
	!	.p
	!	This program create a command file to process
	!	the documentation for a system.
	!
	! Index:
	!
	! Option:
	!
	! Output:
	!
	!	.COM file.
	!
	! Example:
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_SPEC_MAKEDOC/NOOPT
	!	$ LINK/EXE=TK_EXE: TK_SPEC_MAKEDOC, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE TK_SPEC_MAKEDOC.OBJ;*
	!
	! Author:
	!
	!	04/06/88 - Kevin Handy
	!
	! Modification History:
	!
	!	07/12/88 - Kevin Handy
	!		Modified to clear the screen before spawning,
	!		and to display a message saying "please wait".
	!
	!	12/12/88 - Frank F. Starman
	!		Add SIC codes
	!
	!	06/07/89 - Aaron Redd
	!		Modified to allow for index debugging information
	!		in the margins of the documentation.
	!
	!	06/27/89 - Kevin Handy
	!		Modified to use READ_INITIALIZE.
	!
	!	05/02/90 - Kevin Handy
	!		Modified to use IdxTeX instead of home-written
	!		routines. This should create much nicer looking
	!		indexes.
	!
	!	05/17/90 - Kevin Handy
	!		Changes to support differenced in headers and
	!		footers.
	!
	!	06/22/90 - Kevin Handy
	!		Modified to stack paper in normal order, instead
	!		of the default reverse order.
	!
	!	07/10/90 - Frank F. Starman
	!		Ask for time when to submit.
	!
	!	08/08/90 - Frank F. Starman
	!		Transfer SIC code so that can be printed in
	!		the documentation.
	!
	!	06/14/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!		Fix last patameter to entr_3choice.
	!
	!	10/29/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/30/99 - Kevin Handy
	!		Compile /NOOPT to lose problems on Alpha
	!--
	%PAGE

	!
	! Set up compiling options
	!
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
	EXTERNAL LONG            TK_DOCU_GETMODULES

	!
	! Declare variables
	!
	MAP (LBR_JUNKJUNK) TEXT$ = 132%, TEXT1$ = 256%		! Read buffer
	DECLARE RFA TXRFA, NULL.RFA, WIN_RFA, WIN1_RFA

	MAP (TK_DOCU) &
		IN_LEVEL%, &
		MODCOUNT%, &
		MODNAME$(1000%) = 50%, &
		RFA MODRFA(1000%)

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

	!
	! Dir stuff
	!
	DECLARE INTEGER CONSTANT TEXT_FILE = 400	! Size of the array
	DIM TEXT_FILE$(TEXT_FILE)

	DIM VTEXT$(20%)

	%PAGE

	!*******************************************************************
	! Init usefull information
	!*******************************************************************

	SCOPE::PRG_ITEM = "HELP"

	VTEXT$(0%) = "2"
	VTEXT$(1%) = "DVIJEP  HP Lazerjet II"
	VTEXT$(2%) = "DVIL75  DEC LA75"

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
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(18%, 78%, TK_INPUT%, &
		SMG$M_BORDER)
	SMG_STATUS% = SMG$LABEL_BORDER(TK_INPUT%, "Create Documentation")
	SMG_STATUS% = SMG$PUT_CHARS(TK_INPUT%, "SIC", 2%, 1%)
	SMG_STATUS% = SMG$PUT_CHARS(TK_INPUT%, "System", 3%, 1%)
	SMG_STATUS% = SMG$PUT_CHARS(TK_INPUT%, "Only Create .TeX file?", 4%, 1%)
	SMG_STATUS% = SMG$PUT_CHARS(TK_INPUT%, " Create Printer file?", 5%, 1%)
	SMG_STATUS% = SMG$PUT_CHARS(TK_INPUT%, "  Printer device", 6%, 1%)
	SMG_STATUS% = SMG$PUT_CHARS(TK_INPUT%, "Only User's Part?", 7%, 1%)
	SMG_STATUS% = SMG$PUT_CHARS(TK_INPUT%, "Index debugging on?", 8%, 1%)
	SMG_STATUS% = SMG$PUT_CHARS(TK_INPUT%, "Submit to batch?", 10%, 1%)
	SMG_STATUS% = SMG$PUT_CHARS(TK_INPUT%, " Spool output", 11%, 1%)
	SMG_STATUS% = SMG$PUT_CHARS(TK_INPUT%, " Start Time", 12%, 1%)
	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(TK_INPUT%, SCOPE::SMG_PBID, 2%, 2%)

	SMG_STATUS% = SMG$END_PASTEBOARD_UPDATE(SCOPE::SMG_PBID)

	%PAGE


	SIC$ = "0     "
	SOURCE_NAME$ = SPACE$(40%)
	JUST_TEX$ = "N"
	MAKE_PRINTER$ = "Y"
	PRINTER_DRIVER$ = "DVIJEP"
	USERS$ = "N"
	INDEX_DBG$ = "N"
	DO_SUBMIT$ = "Y"
	DO_SPOOL$ = "Y"

1200	!*******************************************************************
	! Get system to document
	!*******************************************************************

	SCOPE::PRG_ITEM = "FLD001"
	SIC$ = ENTR_3STRING(SCOPE, TK_INPUT%, "2;25", "SIC ", SIC$, 0%, "'E", "")

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

1220	SIC$ = "0" IF SIC$ = ""
	SCOPE::PRG_ITEM = "FLD002"
	SOURCE_NAME$ = ENTR_3STRING(SCOPE, TK_INPUT%, "3;25", &
		"System to document (No extension or account) ", SOURCE_NAME$, &
		0%, "'E", "")

	SELECT SCOPE::SCOPE_EXIT
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE SMG$K_TRM_UP
		GOTO 1200

	CASE 10%, 12%, 13%, 0%, SMG$K_TRM_DOWN, SMG$K_TRM_DO

	!
	! List Help Files
	!
	CASE SMG$K_TRM_F14
		TEXT_FILE$(LOOP%) = "" &
			FOR LOOP% = 1% TO TEXT_FILE

		CALL FIND_FILE("CMC:*.MNU", TEXT_FILE$(), &
			16%, "CMC:", ".MNU")

		X% = ENTR_3CHOICE(SCOPE, "", "", TEXT_FILE$(), "", &
			8%, "Library", "", 0%)

		IF X% > 0%
		THEN
			LSET SOURCE_NAME$ = TEXT_FILE$(X%)
		END IF
		GOTO 1220

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 1220
	END SELECT


1250	!*******************************************************************
	! Just tex file?
	!*******************************************************************

	GOTO 1220 IF SOURCE_NAME$ = ""

	SCOPE::PRG_ITEM = "FLD003"
	JUST_TEX$ = ENTR_3YESNO(SCOPE, TK_INPUT%, "4;25", &
		"Just create .TeX file", JUST_TEX$, &
		0%, "'E", "")

	SELECT SCOPE::SCOPE_EXIT
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE SMG$K_TRM_UP
		GOTO 1200

	CASE SMG$K_TRM_DOWN
		GOTO 1300

	CASE 10%, 12%, 13%, 0%, SMG$K_TRM_DO

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 1250
	END SELECT

1300	!*******************************************************************
	! Create printer file?
	!*******************************************************************

	SCOPE::PRG_ITEM = "FLD004"
	IF (JUST_TEX$ = "Y")
	THEN
		MAKE_PRINTER$ = "N"
		XFLAG% = 1%
	ELSE
		XFLAG% = 0%
	END IF

	MAKE_PRINTER$ = ENTR_3YESNO(SCOPE, TK_INPUT%, "5;25", &
		"Create printer file", MAKE_PRINTER$, &
		XFLAG%, "'E", "")

	SELECT SCOPE::SCOPE_EXIT
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE SMG$K_TRM_UP
		GOTO 1250

	CASE SMG$K_TRM_DOWN
		GOTO 1350

	CASE 10%, 12%, 13%, 0%, SMG$K_TRM_DO

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 1300
	END SELECT


1350	!*******************************************************************
	! Get printer driver
	!*******************************************************************

	SCOPE::PRG_ITEM = "FLD005"
	IF (MAKE_PRINTER$ = "N")
	THEN
		LSET PRINTER_DRIVER$ = ""
		XFLAG% = 1%
	ELSE
		XFLAG% = 0%
	END IF

	PRINTER_DRIVER$ = ENTR_3STRING(SCOPE, TK_INPUT%, "6;25", &
		"Create printer file", PRINTER_DRIVER$, &
		XFLAG%, "'E", "")

	SELECT SCOPE::SCOPE_EXIT
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE SMG$K_TRM_UP
		GOTO 1300

	CASE SMG$K_TRM_DOWN
		GOTO 1400

	CASE SMG$K_TRM_F14
		X% = ENTR_3CHOICE(SCOPE, "", "", VTEXT$(), PRINTER_DRIVER$, &
			138%, "Printer  Description", "009", 0%)

		PRINTER_DRIVER$ = LEFT(VTEXT$(X%), 6%) IF X% > 0%

		GOTO 1350

	CASE 10%, 12%, 13%, 0%, SMG$K_TRM_DO

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 1350
	END SELECT

1400	!*******************************************************************
	! Print only user's guide
	!*******************************************************************

	SCOPE::PRG_ITEM = "FLD006"

	USERS$ = ENTR_3YESNO(SCOPE, TK_INPUT%, "7;25", &
		"Only user's part", USERS$, &
		0%, "'E", "")

	SELECT SCOPE::SCOPE_EXIT
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE SMG$K_TRM_UP
		GOTO 1350

	CASE SMG$K_TRM_DOWN
		GOTO 1420

	CASE 10%, 12%, 13%, 0%, SMG$K_TRM_DO

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 1400
	END SELECT

1420	!*******************************************************************
	! Decide if user wants index debugging
	!*******************************************************************

	SCOPE::PRG_ITEM = "FLD007"
	INDEX_DBG$ = ENTR_3YESNO(SCOPE, TK_INPUT%, "8;25", &
		"Index Debugging", INDEX_DBG$, &
		0%, "'E", "")

	SELECT SCOPE::SCOPE_EXIT
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE SMG$K_TRM_UP
		GOTO 1400

	CASE SMG$K_TRM_DOWN
		GOTO 1450

	CASE 10%, 12%, 13%, 0%, SMG$K_TRM_DO

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 1420
	END SELECT


1450	!*******************************************************************
	! Get output file name
	!*******************************************************************

	SCOPE::PRG_ITEM = "FLD008"
	DO_SUBMIT$ = ENTR_3YESNO(SCOPE, TK_INPUT%, "10;25", &
		"Submit to batch", DO_SUBMIT$, &
		0%, "'E", "")

	SELECT SCOPE::SCOPE_EXIT
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE SMG$K_TRM_UP
		GOTO 1420

	CASE SMG$K_TRM_DOWN
		GOTO 1500

	CASE 10%, 12%, 13%, 0%, SMG$K_TRM_DO

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 1450
	END SELECT


1500	!*******************************************************************
	! Get output file name
	!*******************************************************************

	SCOPE::PRG_ITEM = "FLD009"
	IF (DO_SUBMIT$ = "N")
	THEN
		DO_SPOOL$ = "N"
		XFLAG% = 1%
	ELSE
		XFLAG% = 0%
	END IF

	DO_SPOOL$ = ENTR_3YESNO(SCOPE, TK_INPUT%, "11;25", &
		"Spool log file", DO_SPOOL$, &
		XFLAG%, "'E", "")

	SELECT SCOPE::SCOPE_EXIT
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE SMG$K_TRM_UP
		GOTO 1450

	CASE SMG$K_TRM_DOWN
		GOTO 1500

	CASE 10%, 12%, 13%, 0%, SMG$K_TRM_DO

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 1500
	END SELECT

1600	!*******************************************************************
	! Ask for time
	!*******************************************************************

	SCOPE::PRG_ITEM = "FLD010"

	IF (DO_SUBMIT$ = "N")
	THEN
		TIME_SPOOL$ = ""
		XFLAG% = 1%
	ELSE
		TIME_SPOOL$ = TIME_NOW
		XFLAG% = 0%
	END IF


	TIME_SPOOL$ = ENTR_3TIME(SCOPE, TK_INPUT%, "12;25", &
		"Time", TIME_SPOOL$, XFLAG%, "'E", "")

	SELECT SCOPE::SCOPE_EXIT
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE SMG$K_TRM_UP
		GOTO 1500

	CASE SMG$K_TRM_DOWN
		GOTO 1600

	CASE 10%, 12%, 13%, 0%, SMG$K_TRM_DO

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 1600
	END SELECT

4000	!*******************************************************************
	! Create the .COM file
	!*******************************************************************

	SCOPE::PRG_ITEM = "HELP"

	COM_NAME$ = TRM$(SOURCE_NAME$) + ".COM"

	OPEN COM_NAME$ FOR OUTPUT AS FILE LATEX.CH%

	!
	! Create .TeX file from menu
	!
	PRINT #LATEX.CH%, "$ SET VERIFY"
	PRINT #LATEX.CH%, "$ ASSIGN $DISK3:[CMC030.SIC." + TRM$(SIC$) + "] SIC:"
	PRINT #LATEX.CH%, "$!"
	PRINT #LATEX.CH%, "$!**********************************************************************"
	PRINT #LATEX.CH%, "$!"
	PRINT #LATEX.CH%, "$ RUN CMC$ROOT:[TK]TK_SPEC_DOCUAPPLICATION"
	PRINT #LATEX.CH%, TRM$(SOURCE_NAME$)
	PRINT #LATEX.CH%, TRM$(INDEX_DBG$)
	PRINT #LATEX.CH%, TRM$(SIC$)
	PRINT #LATEX.CH%, TRM$(USERS$)

	IF JUST_TEX$ = "N"
	THEN
		!
		! Run TeX file through LaTeX twice to create DVI file
		!
		PRINT #LATEX.CH%, "$!"
		PRINT #LATEX.CH%, "$!**********************************************************************"
		PRINT #LATEX.CH%, "$!"
		PRINT #LATEX.CH%, "$ LATEX "; TRM$(SOURCE_NAME$)
		PRINT #LATEX.CH%, "$ IDXTEX "; TRM$(SOURCE_NAME$); " /TOC=REPORT"
		PRINT #LATEX.CH%, "$ LATEX "; TRM$(SOURCE_NAME$)

		!
		! Run DVI file through selected driver
		!
		IF MAKE_PRINTER$ = "Y"
		THEN
			IF PRINTER_DRIVER$ = "DVIJEP"
			THEN
				! Reverse the normal order
				DVIREVERSE$ = " -b"
			ELSE
				DVIREVERSE$ = ""
			END IF

			PRINT #LATEX.CH%, "$!"
			PRINT #LATEX.CH%, "$!**********************************************************************"
			PRINT #LATEX.CH%, "$!"
			PRINT #LATEX.CH%, "$ "; &
				TRM$(PRINTER_DRIVER$); DVIREVERSE$; &
				" -x0.76in "; &
				TRM$(SOURCE_NAME$)
		END IF

	END IF

	PRINT #LATEX.CH%, "$ PURGE "; TRM$(SOURCE_NAME$); "*.*/LO"
	CLOSE #LATEX.CH%

	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(TK_INPUT%)

	TIME_SPOOL$ = PRNT_TIME(TIME_SPOOL$, 0%)
	!
	! Spawn the processing if requested
	!
	IF (DO_SUBMIT$ = "Y")
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "Spawning.", 1% + 16%)

		IF (DO_SPOOL$ = "Y")
		THEN
			CALL SUBR_3SPAWN(SCOPE, "$ SUBMIT/NOTIFY/PRINT/AFTER=" + &
				TIME_SPOOL$ + " " + TRM$(SOURCE_NAME$))
		ELSE
			CALL SUBR_3SPAWN(SCOPE, "$ SUBMIT/NOTIFY/NOPRINT/AFTER=" + &
				TIME_SPOOL$ + " " + TRM$(SOURCE_NAME$))
		END IF
	END IF

 ExitProgram:
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

32767	END
