1	%TITLE "Handles Output to Forms"
	%SBTTL "OUTP_LINENOTITLE"
	%IDENT "V3.6a Calico"

	SUB OUTP_LINENOTITLE(STRING COLUMNS, UTL_REPORTX_CDD UTL_REPORTX, &
		STRING TXT, INTEGER FFLAG)

	!
	! COPYRIGHT (C) 1986 BY
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
	!	This subroutine handles paging for the programs in the
	!	REPORT system.
	!
	! Parameters:
	!
	!
	!	UTL_REPORTX
	!		The passed definition for the current report.
	!
	!	TXT
	!		The end element used in the text.
	!
	!	FFLAG
	!		1 - Write a form feed after the text
	!
	!
	!	This subroutine handles the paging process for the programs
	!	currently in the REPORT system.
	!
	! Example:
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:OUTP_LINENOTITLE/NOLINE
	!	$ LIB FUNC_LIB:CMCFUN/REP OUTP_LINENOTITLE
	!	$ DELETE OUTP_LINENOTITLE.OBJ;*
	!
	! Author:
	!
	!	04/22/87 - Kevin Handy
	!
	! Modification history:
	!
	!	09/01/87 - Kevin Handy
	!		Modified to allow user to type exit, help, interrupt,
	!		etc. during report output (even in Printer Port mode)
	!
	!	10/19/87 - Kevin Handy
	!		Fixed problem where function had a form feed hard
	!		coded into it.
	!
	!	04/06/89 - Kevin Handy
	!		Modified handling escape sequences
	!
	!	11/09/89 - Kevin Handy
	!		Modified to uset ENTR_4SPECIALKEYS instead of
	!		doing all the checks on RRR_FLAG% manually.
	!
	!	04/13/90 - Kevin Handy
	!		Added COLUMNS field in call so that we can send
	!		forms to spreadsheets if we really want to go
	!		through the extra effort.
	!
	!	02/25/92 - Kevin Handy
	!		Modifications to lose extra form-feed at end of
	!		a document.
	!
	!	03/26/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	03/14/95 - Kevin Handy
	!		(V3.6)
	!		Modified to handle reportx::offset variable.
	!
	!	04/10/95 - Kevin Handy
	!		Update to V3.6 coding standards.
	!		Clean up (Check)
	!
	!	04/19/95 - Kevin Handy
	!		Lose bad external def's that caused errors.
	!		Fix 1st parameter to entr_4specialkeys.
	!
	!	11/01/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!
	!	11/01/95 - Kevin Handy
	!		Modify calls to DSPL_SCROLLCIR to use SMG$K_TRM_FIND
	!		instead of a confusing collection of "FIND".
	!
	!	03/25/96 - Kevin Handy
	!		Reformat source code.
	!
	!	03/25/96 - Kevin Handy
	!		Change LINEOFF from WORD to INTEGER, since that is
	!		what normaly gets passed.
	!
	!	09/16/97 - Kevin Handy
	!		Generic reformatting.
	!
	!	11/11/97 - Kevin Handy
	!		Use constants instead of hard coded numbers in
	!		comparisons to PRINTTO
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/08/99 - Kevin Handy
	!		Use BASIC$STARLET for STR$ routines
	!
	!	04/20/99 - Kevin Handy
	!		Fix unsolicited input
	!
	!	09/23/2005 - Kevin Handy
	!		Add autoscroll key (Control/A)
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "STR$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

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

	!
	! External functions
	!
	EXTERNAL INTEGER FUNCTION DSPL_SCROLLCIR
	EXTERNAL LONG   OUTP_XUNSOL

	%PAGE

	!
	!Save the key for help
	!
	OLD_HELP_SEVERITY$ = SCOPE::PRG_IDENT
	OLD_PROGRAM$ = SCOPE::PRG_PROGRAM
	OLD_HELP_ITEM$ = SCOPE::PRG_ITEM

	SCOPE::PRG_PROGRAM = "OUTP_LINENOTITLE"
	SCOPE::PRG_ITEM = "HELP"
	SCOPE::PRG_IDENT = "H"

	!
	! Exit due to report request
	!
	GOTO ExitSub IF (UTL_REPORTX::STAT <> 0%)

	!
	! Special LINEOFF (-12345) for OUTP_FINISH when in display mode.
	! This is used so that when you hit the end of the report, you
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
	! Output a to-printer sequence if to printer port
	!
	IF (UTL_REPORTX::PRINTTO = OUTP_TOLOCAL)
	THEN
		CALL WRIT_STRING(UTL_REPORTX::TOLOCAL, TOLOCAL$)
		PRINT #UTL_REPORTX::CHAN, TOLOCAL$;
	END IF

	!
	! Count the line
	!
	UTL_REPORTX::LINENO = UTL_REPORTX::LINENO + 1%

	!
	! Force the "title" If this is the first time through,
	! or if it is time to do a page.
	!
	GOSUB 2000 IF (UTL_REPORTX::PAGENO = 0%) &
		OR (UTL_REPORTX::LINENO = UTL_REPORTX::PAGELEN)

	!
	! Handle the various types of output
	!
	SELECT UTL_REPORTX::PRINTTO

	!
	! Display
	! Display ignores form feed requests (FFLAG and 1)
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
		PRINT_FLAG2% = 0%
		TEMP$ = ""

		WHILE COLUMN$ <> ""

			COMMAPOS% = INSTR(1%, COLUMN$, ",")
			THIS.COL$ = LEFT(COLUMN$, COMMAPOS% - 1%)
			COLUMN$ = RIGHT(COLUMN$, COMMAPOS% + 1%)

			COLONPOS% = INSTR(1%, THIS.COL$, ":")
			ENDPOS% = VAL%(RIGHT(THIS.COL$, COLONPOS% + 1%))
			DATA$ = EDIT$(SEG$(TXT, STARTPOS%, ENDPOS%), 8% + 128%)

			IF EDIT$(LEFT(THIS.COL$, 1%), -1%) = "V"
			THEN
				!
				! Clean up data a little
				!
				DATA$ = EDIT$(DATA$, 8% + 128%)

				DATA$ = LEFT(DATA$, LEN(DATA$) - 1%) &
					IF RIGHT(DATA$, LEN(DATA$)) = "%"

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
				PRINT_FLAG2% = -1% IF DATA$ <> ""
			ELSE
				SELECT EDIT$(LEFT(THIS.COL$, 1%), -1%)

				CASE "V"
					TEMP$ = TEMP$ + DATA$ + " #"

					PRINT_FLAG2% = -1% &
						IF STR$FIND_FIRST_IN_SET(DATA$, &
						"123456789")

				CASE ELSE
					TEMP$ = TEMP$ + '"' + DATA$ + '" #'
					PRINT_FLAG2% = -1% IF DATA$ <> ""

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

		PRINT #UTL_REPORTX::CHAN, TEMP$ IF PRINT_FLAG2%

		CALL ENTR_3MESSAGE(SCOPE, "", 1% + 16%) &
			IF UTL_REPORTX::LINENO = 1%

	!
	! Normal output
	! Put out a form feed if flag bit 1 is set.
	!
	CASE ELSE
		PRINT #UTL_REPORTX::CHAN, &
			TRM$(FNOFFSET$(TXT, UTL_REPORTX::OFFSET))

		IF (FFLAG AND 1%)
		THEN
			CALL OUTP_FORMFF(UTL_REPORTX)
		END IF

	END SELECT

1500	!
	! Output a to screen command if in printer port mode
	!
	IF UTL_REPORTX::PRINTTO = OUTP_TOLOCAL
	THEN
		CALL WRIT_STRING(UTL_REPORTX::TOSCREEN, TOSCREEN$)
		PRINT #UTL_REPORTX::CHAN, TOSCREEN$;
	END IF

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
	! Autoscroll Toggle Key Typed
	!
	CASE 1%
		IF UTL_REPORTX::AUTOSCROLL = 0%
		THEN
			UTL_REPORTX::AUTOSCROLL = -1%
		ELSE
			UTL_REPORTX::AUTOSCROLL = 0%
		END IF

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
	!Restore the key for help
	!
	SCOPE::PRG_IDENT = OLD_HELP_SEVERITY$
	SCOPE::PRG_PROGRAM = OLD_PROGRAM$
	SCOPE::PRG_ITEM = OLD_HELP_ITEM$

	EXIT SUB

	%PAGE

2000	!*******************************************************************
	! This subroutine handles initilization at the start of the report
	! and pausing every few lines on a scope.  The PAGENO variable
	! is used to decide if this is the first time in, or not.
	!*******************************************************************

	!
	! Count one page
	!
	UTL_REPORTX::PAGENO = UTL_REPORTX::PAGENO + 1%

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
			SMG_STATUS% = &
				SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

			CALL ENTR_3MESSAGE(SCOPE, &
				"Printing, press <EXIT> to exit.", 0%)

			SMG_STATUS% = &
				SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
				LOC(OUTP_XUNSOL) BY VALUE, &
				LOC(SCOPE::SMG_KBID) BY VALUE)

			SELECT SCOPE::SCOPE_EXIT

			!
			! Autoscroll Toggle Key Typed
			!
			CASE 1%
				UTL_REPORTX::AUTOSCROLL = -1%

			!
			! An exit key was typed
			!
			CASE 3%, SMG$K_TRM_CTRLZ, SMG$K_TRM_F10, &
				SMG$K_TRM_F8

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
			CASE 10%, 12%, 13%, SMG$K_TRM_F7

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
		! Clear the screen
		!
		IF (UTL_REPORTX::PAGENO = 1%) OR (LINEOFF > 1000%)
		THEN
			!
			! Set up scrolling region
			!
			SMG_STATUS% = SMG$SET_DISPLAY_SCROLL_REGION(UTL_REPORTX::WINDOW, &
				1%, 20%)

			!
			! Create SMG_SCROLL array
			!
			SMG_SCROLL::WINDOW = UTL_REPORTX::WINDOW
			SMG_SCROLL::SCROLL_TOP = 1%
			SMG_SCROLL::SCROLL_BOT = 20%
			SMG_SCROLL::TOP_ARRAY = 1%
			SMG_SCROLL::BOT_ARRAY = MAX_SMG_TEXT
			SMG_SCROLL::CUR_LINE, SMG_SCROLL::CUR_W_COL = 1%

			SMG_SCROLL::FIND_LINE, SMG_SCROLL::TOP_LINE, &
				SMG_SCROLL::CUR_W_ROW, &
				SMG_SCROLL::BEG_ELEMENT = 1%

			SMG_SCROLL::END_ELEMENT = 1%
			SMG_TEXT(1%) = ""

			SMG_SCROLL::SMG_FLAG = 6%
			SMG_SCROLL::PROMPT = ""
			SMG_SCROLL::NUM_COLM = 1%

			!
			! Calculate number of lines available on the screen
			!
			UTL_REPORTX::LINENO = 1%
		END IF

	!
	! S2020, DIF
	! Again, do nothing;  titles are not allowed
	!
	!CASE 10%, 11%

	CASE ELSE
		IF UTL_REPORTX::PAGENO = 1%
		THEN
			!
			! NOTE:  If you take out this ENTR_MESSAGE call, leave in the
			! print of '[5i' business.  It is necessary for proper
			! operation.
			!
			IF UTL_REPORTX::PRINTTO = OUTP_TOLOCAL
			THEN
				CALL WRIT_STRING(UTL_REPORTX::TOSCREEN, TOSCREEN$)
				PRINT #UTL_REPORTX::CHAN, TOSCREEN$;
			END IF

			CALL ENTR_3MESSAGE(SCOPE, "Printing. . .", 1% + 16%)

			IF UTL_REPORTX::PRINTTO = OUTP_TOLOCAL
			THEN
				CALL WRIT_STRING(UTL_REPORTX::TOLOCAL, TOLOCAL$)
				PRINT #UTL_REPORTX::CHAN, TOLOCAL$;
			END IF
		END IF

	END SELECT

 Ret2000:
	RETURN

	END SUB
