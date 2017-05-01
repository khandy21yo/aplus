1	%TITLE "Output Raw Text to a Report File"
	%SBTTL "OUTP_RAWPRINT"
	%IDENT "V3.6a Calico"

	SUB OUTP_RAWPRINT(STRING COLUMNS, UTL_REPORTX_CDD UTL_REPORTX, &
		STRING HDR(), STRING TXT, INTEGER LINEOFF)

	!
	! COPYRIGHT (C) 1986 BY
	! Software Solutions, Inc.
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
	! Software Solutions, Inc.
	!
	! Software Solutions, Inc. assumes no responsibility for the use
	! or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	!
	! Abstract:HELP
	!	.p
	!	This subroutine is used to output raw text to a report
	!	file without adding any formatting of it's own.
	!
	! Index:
	!	.x Print Line
	!
	! Parameters:
	!
	!	COLUMNS$
	!		Passed specification for not only where the
	!		columns are, but also labels for
	!		these columns.  This string is used for shipping
	!		the report to Graphics, a Spreadsheet, or a
	!		Database.  Each column of data should have a
	!		section in this string, in this form:
	!
	!		... <DataType><Label>:<EndingPos>, ...
	!
	!		<DataType> is a single character describing
	!			the type of data passed in that column:
	!			($ = String, D = Date, P = Phone number,
	!			T = Time, and V = Number value).
	!		<Label> is a label for the data passed in that
	!			column.
	!		<EndingPos> is the horizontal position along
	!			the line where this column ends.
	!
	!	UTL_REPORTX
	!		This passed structure contains the definitions of
	!		various items for the report being printed.  These
	!		items are modified according to actions taken; line
	!		number, page number incremented, etc.
	!
	!	HDR()
	!		(Not Used)
	!		Headers to be printed on the top of every page.
	!
	!	TXT
	!		Passed text to print.
	!
	!	LINEOFF
	!		Variable used to control paging.  A positive value
	!		causes paging to occur that many lines earlier,
	!		and a negitive value causes it to occur that many
	!		lines later.  A special case of LINEOFF >= 1000
	!		exists that forces a new page, but the line is not
	!		counted/printed.
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:OUTP_RAWPRINT/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP OUTP_RAWPRINT
	!	$ DELETE OUTP_RAWPRINT.OBJ;*
	!
	! Author:
	!
	!	03/23/87 - Kevin Handy
	!
	! Modification history:
	!
	!	09/01/87 - Kevin Handy
	!		Modified to allow user to type exit, help, interrupt,
	!		etc. during report output (even in Printer Port mode)
	!
	!	10/19/87 - Kevin Handy
	!		Minor fix for interupt menu.
	!
	!	10/19/87 - Kevin Handy
	!		Modified not to form feed at top of form, but to
	!		form feed at bottom.
	!
	!	10/20/87 - Kevin Handy
	!		Modified for new print types 6, 7, and 8.
	!		(Clipboard, Word processor, file cabinet)
	!
	!	03/23/88 - Kevin Handy
	!		Fixed bug whiched caused problem when user
	!		typed an undefined key at screen pause prompt.
	!
	!	07/19/88 - Aaron Redd
	!		Modified for new destinations 10 and 11.
	!		(SuperComp 20/20 and D.I.F.)
	!
	!	06/27/89 - Kevin Handy
	!		Modified to read report name out of UTL_PROFILE
	!		instead of using UTL_SET.
	!
	!	11/09/89 - Kevin Handy
	!		Modified to uset ENTR_4SPECIALKEYS instead of
	!		doing all the checks on RRR_FLAG% manually.
	!
	!	11/13/89 - Kevin Handy
	!		Modified to handle PlanPerfect files.
	!
	!	04/13/90 - Kevin Handy
	!		Modified output to spreadsheets to re-format
	!		any numbers that have commas, or minus on the end,
	!		so that the spreadsheets won't think that they are
	!		strings.
	!
	!	08/07/90 - Frank F. Starman
	!		Change text for messages.
	!
	!	02/27/92 - Kevin Handy
	!		Modifications to make form-feed through printer
	!		ports work better.
	!
	!	03/14/95 - Kevin Handy
	!		(V3.6)
	!		Modified to handle reportx::offset variable.
	!
	!	03/21/95 - Kevin Handy
	!		Fix bug found with headers.
	!
	!	03/24/95 - Kevin Handy
	!		Another bug in headers.
	!
	!	04/10/95 - Kevin Handy
	!		Updated to V3.6 Calico Coding Standards.
	!		Clean up (Check)
	!
	!	04/19/95 - Kevin Handy
	!		Lose bad external def's that caused errors.
	!		Fix 1st parameter to entr_4specialkeys.
	!
	!	11/01/95 - Kevin Handy
	!		Modified calls to DSPL_SCROLLCIR to use
	!		SMG$??? instead of confusing references to "FIND".
	!
	!	11/20/95 - Kevin Handy
	!		Remove several lines of commented out code.
	!
	!	03/25/96 - Kevin Handy
	!		Reformat source code.
	!
	!	03/25/96 - Kevin Handy
	!		Changed LINEOFF from WORD to INTEGER, since that
	!		is what normally gets passed through.
	!
	!	09/16/97 - Kevin Handy
	!		Lose commented out code.
	!		Lose code for clipboard, which isn't usable.
	!		Lose code for File Cabinet, which Didn't work.
	!
	!	10/13/97 - Kevin Handy
	!		Make sure text length is less than 511 characters.
	!
	!	11/11/97 - Kevin Handy
	!		Use constants instead of hard coded numbers in
	!		comparison to PRINTTO
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/08/98 - Kevin Handy
	!		Lose the map for UTL_PROFILE, which was not used.
	!
	!	04/08/99 - Kevin Handy
	!		Use BASIC$STARLET for STR$ routines
	!
	!	04/21/99 - Kevin Handy
	!		Fix unsolicited input
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "STR$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	!
	! Maps
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"

	!
	! Helper function for the offset business
	!
	DEF FNOFFSET$(SOURCE$, OFFSET%)

		SELECT OFFSET%

		CASE > 0%
			FNOFFSET$ = SPACE$(OFFSET%) + SOURCE$

		CASE < 0%
			FNOFFSET$ = RIGHT(SOURCE$, -OFFSET% + 1%)

		CASE ELSE
			FNOFFSET$ = SOURCE$

		END SELECT

	END DEF

	!
	! Create a structure to contain all of the display stuff for
	! scrolling up and down.
	!
	DECLARE INTEGER CONSTANT MAX_SMG_TEXT = 200

	%INCLUDE "SOURCE:[SMG.OPEN]SMG_SCROLL.HB"

	MAP (DP_ARENA_KEVIN) &
		SMG_SCROLL_CDD SMG_SCROLL, &
		STRING SMG_TEXT(MAX_SMG_TEXT) = 132%

	MAP (DP_OUTP_XUNSOL) RRR_FLAG%

	COM (TITLE_LINE) STRING TITLE1 = 80%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION DSPL_SCROLLCIR

	EXTERNAL LONG   OUTP_XUNSOL	! (It's really an AST routine)

	%PAGE

	!
	! Save the key for help
	!
	OLD_HELP_SEVERITY$ = SCOPE::PRG_IDENT
	OLD_PROGRAM$ = SCOPE::PRG_PROGRAM
	OLD_HELP_ITEM$ = SCOPE::PRG_ITEM

	SCOPE::PRG_PROGRAM = "OUTP_RAWPRINT"
	SCOPE::PRG_ITEM = "HELP"
	SCOPE::PRG_IDENT = "H"

	!
	! Exit due to report request
	!
	GOTO ExitSub IF (UTL_REPORTX::STAT <> 0%)

	IF TRM$(HDR(1%)) <> ""
	THEN
		TITLE1 = TRM$(HDR(1%))
	END IF

	!
	! Special LINEOFF (-12345) for OUTP_FINISH when in display mode.
	! This is used so that when you hit the end, you
	! can still arrow back up and down to your hearts content.
	! Q.FLAG% is used to change the flag used by DSPL_SCROLLCIR.
	!
	IF (LINEOFF = -12345%) AND (UTL_REPORTX::PRINTTO = OUTP_TODISPLAY)
	THEN
		Q.FLAG% = 2%
		GOSUB 2000
		GOTO ExitSub
	END IF

	Q.FLAG% = 6%

	!
	! Output goes to printer if to printer port
	!
	IF (UTL_REPORTX::PRINTTO = OUTP_TOLOCAL) AND &
		(UTL_REPORTX::PAGENO >= UTL_REPORTX::STARTP)
	THEN
		CALL WRIT_STRING(UTL_REPORTX::TOLOCAL, TOLOCAL$)
		PRINT #UTL_REPORTX::CHAN, TOLOCAL$;
	END IF

	!
	! Force the title if this is the first time through,
	! or it is time to do a page.
	!
	GOSUB 2000 IF &
		(UTL_REPORTX::PAGENO = 0%) AND (UTL_REPORTX::PRINTTO < 10%) OR &
		((UTL_REPORTX::LINENO + LINEOFF) >= (UTL_REPORTX::PAGELEN - 7%) AND &
			(UTL_REPORTX::PRINTTO <> OUTP_TODISPLAY)) AND &
			(UTL_REPORTX::PRINTTO < 10%) OR &
		((UTL_REPORTX::LINENO) >= (UTL_REPORTX::PAGELEN) AND &
			(UTL_REPORTX::PRINTTO = OUTP_TODISPLAY))

	!
	! Count the line
	!
	UTL_REPORTX::LINENO = UTL_REPORTX::LINENO + 1% IF LINEOFF < 1000% &
		AND UTL_REPORTX::AUTOSCROLL = 0%

	!
	! Will we be printing? (Check for start and end page, as
	! well as report request to exit)
	!
	GOTO 1500 UNLESS &
		((UTL_REPORTX::PAGENO >= UTL_REPORTX::STARTP) AND &
		((UTL_REPORTX::PAGENO <= UTL_REPORTX::ENDP) OR &
			(UTL_REPORTX::ENDP = 0%)))

	GOTO 1500 IF (UTL_REPORTX::STAT <> 0%)

	!
	! If the lineoff<1000 then line will print otherwise line
	! will not print
	!
	IF LINEOFF < 1000%
	THEN
		!
		! Handle the various types of output
		!
		SELECT UTL_REPORTX::PRINTTO

		!
		! Display
		!
		CASE OUTP_TODISPLAY
			SMG_SCROLL::END_ELEMENT = SMG_SCROLL::END_ELEMENT + 1%
			SMG_SCROLL::END_ELEMENT = SMG_SCROLL::TOP_ARRAY &
				IF SMG_SCROLL::END_ELEMENT > SMG_SCROLL::BOT_ARRAY
			SMG_SCROLL::BEG_ELEMENT = SMG_SCROLL::END_ELEMENT + 1% &
				IF SMG_SCROLL::BEG_ELEMENT = SMG_SCROLL::END_ELEMENT
			SMG_SCROLL::BEG_ELEMENT = SMG_SCROLL::TOP_ARRAY &
				IF SMG_SCROLL::BEG_ELEMENT > SMG_SCROLL::BOT_ARRAY

			SMG_TEXT(SMG_SCROLL::END_ELEMENT) = &
				FNOFFSET$(TXT, UTL_REPORTX::OFFSET)
			SMG_SCROLL::FIND_LINE = SMG_SCROLL::END_ELEMENT

			JUNK% = DSPL_SCROLLCIR(SMG_SCROLL, SMG_TEXT(), &
				SMG$K_TRM_FIND, "")

		!
		! SuperComp 20/20, PlanPerfect
		!
		CASE OUTP_TO2020, OUTP_TOPL
			COLUMN$ = COLUMNS + ","
			STARTPOS% = 0%
			PRINT.FLAG2% = 0%
			TEMP$ = ""

			WHILE COLUMN$ <> ""
				COMMAPOS% = INSTR(1%, COLUMN$, ",")
				THIS.COL$ = LEFT(COLUMN$, COMMAPOS% - 1%)
				COLUMN$ = RIGHT(COLUMN$, COMMAPOS% + 1%)

				COLONPOS% = INSTR(1%, THIS.COL$, ":")
				ENDPOS% = VAL%(RIGHT(THIS.COL$, COLONPOS% + 1%))
				DATA$ = TRM$(SEG$(TXT, STARTPOS%, ENDPOS%))

				IF EDIT$(LEFT(THIS.COL$, 1%), -1%) = "V"
				THEN
					!
					! Clean up data a little
					!
					DATA$ = EDIT$(DATA$, 8% + 128%)

					DATA$ = "-" + SEG$(DATA$, 2%, LEN(DATA$) - 1%) &
						IF RIGHT(DATA$, LEN(DATA$)) = ")" AND &
						LEFT(DATA$, 1%) = "("
					DATA$ = "-" + LEFT(DATA$, LEN(DATA$) - 1%) &
						IF RIGHT(DATA$, LEN(DATA$)) = "-"

					I% = INSTR(1%, DATA$, ",")
					WHILE I%
						DATA$ = LEFT(DATA$, I% - 1%) + &
							RIGHT(DATA$, I% + 1%)
						I% = INSTR(1%, DATA$, ",")
					NEXT

					I% = INSTR(1%, DATA$, "$")
					IF I%
					THEN
						DATA$ = LEFT(DATA$, I% - 1%) + &
							RIGHT(DATA$, I% + 1%)
					END IF
				END IF

				IF UTL_REPORTX::PRINTTO = OUTP_TOPL
				THEN
					TEMP$ = TEMP$ + "	" + DATA$
					PRINT.FLAG2% = -1% IF DATA$ <> ""
				ELSE
					SELECT EDIT$(LEFT(THIS.COL$, 1%), -1%)
					CASE "V"
						TEMP$ = TEMP$ + &
							DATA$ + " #"
						PRINT.FLAG2% = -1% &
							IF STR$FIND_FIRST_IN_SET(DATA$, "123456789")
					CASE ELSE
						TEMP$ = TEMP$ + '"' + DATA$ + '" #'
						PRINT.FLAG2% = -1% IF DATA$ <> ""
					END SELECT
				END IF

				STARTPOS% = ENDPOS% + 1%
			NEXT

			SELECT UTL_REPORTX::PRINTTO
			CASE OUTP_TO2020
				TEMP$ = TEMP$ + "#"
			CASE OUTP_TOPL
				TEMP$ = RIGHT(TEMP$, 2%)
			END SELECT

			PRINT #UTL_REPORTX::CHAN, TEMP$ IF PRINT.FLAG2%

			CALL ENTR_3MESSAGE(SCOPE, TITLE1, 1% + 16%) &
				IF UTL_REPORTX::LINENO = 1%

		!
		! Normal output
		!
		CASE ELSE
			PRINT #UTL_REPORTX::CHAN, TXT

		END SELECT
	END IF

1500	!
	! Output goes to the screen if in printer port mode
	!
	IF UTL_REPORTX::PRINTTO = OUTP_TOLOCAL
	THEN
		CALL WRIT_STRING(UTL_REPORTX::TOSCREEN, TOSCREEN$)
		PRINT #UTL_REPORTX::CHAN, TOSCREEN$;
	END IF

	!
	! Flag to end program if we reach the end page number
	!
	UTL_REPORTX::STAT = -50% &
		IF (UTL_REPORTX::PAGENO > UTL_REPORTX::ENDP) AND &
		(UTL_REPORTX::ENDP <> 0%)

	!
	! Handle any special junk in RRR_FLAG%
	!
	SELECT RRR_FLAG%

	!
	! Nothing
	!
	CASE 0%
		! Nothing

	!
	! Exit keys
	!
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8

		UTL_REPORTX::STAT = RRR_FLAG%

	!
	! Else
	!
	CASE ELSE
		SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

		SMG_STATUS% = ENTR_4SPECIALKEYS(SCOPE, &
			SCOPE::SMG_OPTION BY VALUE, 256% BY VALUE, &
			RRR_FLAG% BY VALUE)

		SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
			LOC(OUTP_XUNSOL) BY VALUE, &
			LOC(SCOPE::SMG_KBID) BY VALUE)

	END SELECT

	RRR_FLAG% = 0%

 ExitSub:
	!
	! Restore the key for help
	!
	SCOPE::PRG_IDENT = OLD_HELP_SEVERITY$
	SCOPE::PRG_PROGRAM = OLD_PROGRAM$
	SCOPE::PRG_ITEM = OLD_HELP_ITEM$

	EXIT SUB

	%PAGE

2000	!*******************************************************************
	! This subroutine does the actual paging
	!*******************************************************************

	!
	! Count one page
	!
	UTL_REPORTX::PAGENO = UTL_REPORTX::PAGENO + 1%

	!
	! Are we skipping this page? Check first for page range, but
	! force first title when printing to a clipboard, wp document,
	! or file cabinet.
	!
	PRINT_FLAG1% = ((UTL_REPORTX::PAGENO >= UTL_REPORTX::STARTP) AND &
		((UTL_REPORTX::PAGENO <= UTL_REPORTX::ENDP) OR &
		(UTL_REPORTX::ENDP = 0%)))

	!
	! On first page, enable unsolicited input trapping
	!
	IF UTL_REPORTX::PAGENO = 1%
	THEN
		SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
			LOC(OUTP_XUNSOL) BY VALUE, &
			LOC(SCOPE::SMG_KBID) BY VALUE)
		RRR_FLAG% = 0%
	END IF

	!
	! While in display, title goes on line two and three
	!
	SELECT UTL_REPORTX::PRINTTO

	CASE OUTP_TODISPLAY
		!
		! Pause
		!
		SCOPE::SCOPE_EXIT = 0%

		IF (UTL_REPORTX::PAGENO <> 1%) AND (UTL_REPORTX::AUTOSCROLL = 0%)
		THEN
 PauseLoop:
			SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

			CALL ENTR_3MESSAGE(SCOPE, TITLE1, 0%)

			SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
				LOC(OUTP_XUNSOL) BY VALUE, &
				LOC(SCOPE::SMG_KBID) BY VALUE)

			SELECT SCOPE::SCOPE_EXIT

			!
			! An exit key was typed
			!
			CASE 3%, SMG$K_TRM_CTRLZ, SMG$K_TRM_F10, SMG$K_TRM_F8

				UTL_REPORTX::STAT = SCOPE::SCOPE_EXIT
				GOTO Ret2000

			!
			! Cursor positioning (up) key was typed
			!
			CASE SMG$K_TRM_UP, &
				SMG$K_TRM_F18, &
				SMG$K_TRM_PREV_SCREEN

				SMG_SCROLL::SMG_FLAG = 2%

				TEMP% = DSPL_SCROLLCIR(SMG_SCROLL, &
					SMG_TEXT(), SCOPE::SCOPE_EXIT, "")

				GOTO PauseLoop

			!
			! Cursor positioning key (down) was typed
			!
			CASE SMG$K_TRM_DOWN, &
				SMG$K_TRM_F19, &
				SMG$K_TRM_NEXT_SCREEN

				SMG_SCROLL::SMG_FLAG = Q.FLAG%

				TEMP% = DSPL_SCROLLCIR(SMG_SCROLL, &
					SMG_TEXT(), SCOPE::SCOPE_EXIT, "")

				GOTO PauseLoop IF TEMP% <= 0%

				UTL_REPORTX::LINENO = &
					UTL_REPORTX::LINENO - TEMP%

			!
			! Return, etc. act as next screen
			!
			CASE 10%, 12%, 13%, SMG$K_TRM_F7, SMG$K_TRM_DO

				SMG_SCROLL::SMG_FLAG = Q.FLAG%

				TEMP% = DSPL_SCROLLCIR(SMG_SCROLL, &
					SMG_TEXT(), &
					SMG$K_TRM_NEXT_SCREEN, &
					"")

				GOTO PauseLoop IF TEMP% <= 0%

				UTL_REPORTX::LINENO = &
					UTL_REPORTX::LINENO - TEMP%

			!
			! Case else
			!
			CASE ELSE
				SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

				CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)

				SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
					LOC(OUTP_XUNSOL) BY VALUE, &
					LOC(SCOPE::SMG_KBID) BY VALUE)
				GOTO PauseLoop

			END SELECT

		END IF

		!
		! Search for header
		!
		V% = 0% FOR JUNK% = 1% UNTIL HDR(JUNK%) = ""
		V% = 0% FOR JUNK1% = JUNK% + 1% UNTIL HDR(JUNK1%) = ""

		!
		! Clear the screen and print the title if necessary
		!
		IF (UTL_REPORTX::PAGENO = 1%) OR (LINEOFF > 1000%)
		THEN
			!
			! Display the header
			!
			FOR I% = JUNK% + 1% TO JUNK1% - 1%

				JUNK$ = HDR(I%)
				JUNK$ = "" IF JUNK$ = "."
				JUNK$ = FNOFFSET$(JUNK$, UTL_REPORTX::OFFSET)
				SMG_STATUS% = SMG$PUT_CHARS(UTL_REPORTX::WINDOW, &
					JUNK$ + SPACE$(UTL_REPORTX::REPWIDTH - LEN(JUNK$)), &
					(I% - JUNK%), 1%, 1%, SMG$M_REVERSE)
			NEXT I%

			!
			! Set up scrolling region
			!
			SMG_STATUS% = SMG$SET_DISPLAY_SCROLL_REGION(UTL_REPORTX::WINDOW, &
				JUNK1% - JUNK%, 20%)

			!
			! Create SMG_SCROLL array
			!
			SMG_SCROLL::WINDOW = UTL_REPORTX::WINDOW
			SMG_SCROLL::SCROLL_TOP = JUNK1% - JUNK%
			SMG_SCROLL::SCROLL_BOT = 20%
			SMG_SCROLL::TOP_ARRAY = 1%
			SMG_SCROLL::BOT_ARRAY = MAX_SMG_TEXT
			SMG_SCROLL::CUR_LINE, SMG_SCROLL::CUR_W_COL = 1%
			SMG_SCROLL::FIND_LINE, SMG_SCROLL::TOP_LINE, &
				SMG_SCROLL::CUR_W_ROW, SMG_SCROLL::BEG_ELEMENT = 1%
			SMG_SCROLL::END_ELEMENT = 1%
			SMG_TEXT(1%) = ""

			SMG_SCROLL::SMG_FLAG = 6%
			SMG_SCROLL::PROMPT = ""
			SMG_SCROLL::NUM_COLM = 1%

			!
			! Calculate number of lines available on the screen
			!
			UTL_REPORTX::LINENO = JUNK1% - JUNK%
		END IF


	CASE ELSE
		!
		! Skip to the top of the next page
		!
		IF UTL_REPORTX::PAGENO = 1%
		THEN
			UTL_REPORTX::SDATE = DATE$(0%)
			UTL_REPORTX::STIME = TIME$(0%)
		END IF

		!
		! Handle title while skipping pages (Use special loop
		! because it looks better than 1000 if statements in
		! the printing section that follows).
		!
		! Also, handle types 7, and 8 (Word Processing,
		! File cabinet), after the first header, so that the user
		! can actuall use start/end page.
		!
		IF (PRINT_FLAG1% = 0%) OR &
			((UTL_REPORTX::PAGENO <> 1%) AND &
			(UTL_REPORTX::PRINTTO = OUTP_TOWP))
		THEN
			!
			! Print page anyway if need a title for wp.
			!
			GOTO 2050 &
				IF (UTL_REPORTX::PAGENO = 1%) AND &
				(UTL_REPORTX::PRINTTO = OUTP_TOWP)

			!
			! Special GOSUB 3000's in title
			!
			UTL_REPORTX::LINENO = 5%

			GOTO 2100
		END IF

2050		!
		! Output the title
		!
		UTL_REPORTX::LINENO = 4%! Minimum number of lines in header
					! Modified to adjust for excesses in
					! paging section, and room for blank
					! lines at bottom of page.

	END SELECT

 Ret2000:
2100	RETURN

	END SUB
	!+-+-+
	!++
	! Success:DISPLAY
	!	^*Display Report\*
	!	.b
	!	.lm +5
	!	Scrolling to the next screen, previous screen,
	!	line by line or exit.
	!	.lm -5
	!
	! Index:
	!
	!--
