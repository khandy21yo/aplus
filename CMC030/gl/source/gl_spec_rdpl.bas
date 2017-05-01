1	%TITLE "Read Plan Perfect File into Budget File"
	%SBTTL "GL_SPEC_RDPL"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1991 BY
	!
	! Computer Management Center, Inc.
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
	! Abstract:HELP
	!	.B
	!	.LM +5
	!	Often an electronic spreadsheet is used in budget preparation to enter
	!	detailed data. The spreadsheet may be transferred to a Plan Perfect File after
	!	being prepared on an on-line spreadsheet or on a personal computer.
	!	After the budget on the spreadsheet is accepted, the spreadsheet file can be
	!	copied by the system into the system budget file.
	!	.B
	!	Enter the budget data in the spreadsheet to
	!	reflect the change or sum of the budgeted transactions for each
	!	period and budgeted account. Account numbers to be budgeted
	!	are to be entered in a designated column with period budgets
	!	entered in the related rows.
	!	.note
	!	The Plan Perfect files ^*must\* have an extension of ^*.PLN\* for
	!	the read Plan Perfect process to be able to find them.
	!	.end note
	!	.NOTE Passwords
	!	You must remove any password on the spreadsheet in order for this
	!	program to be able to read it.  Using a password in PlanPerfect
	!	causes it to scramble the data in the file, making it unreadable
	!	to anything other than PlanPerfect.  If you need passwords, remove
	!	it temporarily, run this program, then put it back on.
	!	.end note
	!
	! Index:
	!	.x Budget>Copy Plan Perfect File
	!	.x Spreadsheet>Copy Plan Perfect File to Budget
	!	.x Copy>Plan Perfect File to Budget
	!	.x Plan Perfect File to Budget>Copy
	!	.x Copy Plan Perfect File>Budget
	!	.x Copy Plan Perfect File to Budget>Spreadsheet
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_SPEC_RDPL/LINE
	!	$ LINK/EXECUTABLE=GL_EXE: GL_SPEC_RDPL, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_SPEC_RDPL.OBJ;*
	!
	! Author:
	!
	!	04/08/91 - Kevin Handy
	!		Taken from GL_SPEC_RDDIF and extensively modified
	!		to directly read planperfect files instead of
	!		DIF files.
	!
	! Modification history:
	!
	!	04/09/91 - Kevin Handy
	!		Used RESET instead of Close/Reopen to get
	!		back to the front of the file.
	!
	!	05/11/91 - Frank F. Starmanm
	!		Fix bug that month budget will be imported to the
	!		right month.
	!
	!	03/18/92 - Kevin Handy
	!		Modified to trap subscript out of range error
	!		while loading rows.
	!
	!	03/30/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/09/93 - Kevin Handy
	!		Increased size of dimensions for files.
	!
	!	02/28/95 - Kevin Handy
	!		Changed titles to read "PLN" instead of "DIF".
	!
	!	04/14/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!		Fix last parameter to ENTR_3CHOICE.
	!
	!	11/20/95 - Kevin Handy
	!		Removed /NOWARN on compile statement.
	!
	!	12/15/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!		Change RIGHT(NUM1$()) to FORMAT$().
	!
	!	05/15/97 - Kevin Handy
	!		Reformat source code
	!
	!	08/20/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/21/99 - Kevin Handy
	!		Fix unsolicited input
	!
	!	06/02/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	10/30/2000 - Kevin Handy
	!		Use A"x"B
	!
	!	06/17/2002 - Kevin Handy
	!		Round numbers coming in to two digits
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[GL.OPEN]GL_BUD_YYYY.HB"
	MAP (GL_BUD_YYYY)		GL_BUD_YYYY_CDD	GL_BUD_YYYY

	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	MAP (GL_PERIOD)		GL_PERIOD_CDD	GL_PERIOD

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD	GL_CHART

	%INCLUDE "SOURCE:[SMG.OPEN]SMG_SCROLL.HB"
	DECLARE SMG_SCROLL_CDD	SMG_SCROLL
	DECLARE SMG_SCROLL_CDD	COL_SCROLL

	MAP (GL_RDDIF) GL_RDDIF_BUFFER$ = 128%
	MAP (DP_OUTP_XUNSOL) RRR_FLAG%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION DSPL_SCROLL
	EXTERNAL LONG	OUTP_XUNSOL ! (It's really an AST routine)

	!
	! Declare  Vars
	!
	DECLARE LONG SMG_DIF, SMG_COL

	RRR_FLAG% = 0%

	!
	! Dimension
	!
	DIM GL_BUD_YYYY_FILE$(100%), GL_RDDIF_FILE$(1000%)
	DIM D_I_F$(100%), COL$(40%)
	DIM COL%(40%), CHART_FORMAT%(18%), AMOUNT(50%)

	%PAGE

	!
	! Initialization section - Prepare to do anything
	!
	ON ERROR GOTO 19000

	!
	! Initialize all the standard stuff through an external call
	!
	CALL READ_INITIALIZE

	!
	! Set up for help
	!
	SCOPE::PRG_IDENT = "PROG"
	SCOPE::PRG_PROGRAM = READ_SYSPN
	SCOPE::PRG_ITEM = ""

	!
	! Declare channels
	!
	CALL ASSG_CHANNEL(GL_RDDIF.CH%, STAT%)

	!
	! Get info required for main file
	!
	CALL READ_DEVICE("GL_RDDIF", GL_RDDIF.DEV$, STAT%)
	CALL READ_DEVICE("GL_BUD_YYYY", GL_BUD_YYYY.DEV$, STAT%)

200	!
	! Query user for year of file
	!
	CALL FIND_FILE(GL_BUD_YYYY.DEV$ + "GL_BUD_*.MAS", &
		GL_BUD_YYYY_FILE$(), 16%, "", "")

	GL_BUD_YYYY_FILE% = VAL%(GL_BUD_YYYY_FILE$(0%))

	IF GL_BUD_YYYY_FILE%
	THEN
		GL_BUD_YYYY_FILE$(LOOP%) = &
			MID(GL_BUD_YYYY_FILE$(LOOP%), 8%, 4%) &
			FOR LOOP% = 1% TO GL_BUD_YYYY_FILE%

		TEMP$ = "GL Budget Files"

		X% = ENTR_3CHOICE(SCOPE, "", "", GL_BUD_YYYY_FILE$(), "", &
			0%, TEMP$, "", 0%)

		SELECT SCOPE::SCOPE_EXIT
		CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
			GOTO ExitProgram

		END SELECT

		IF X% > 0%
		THEN
			GL_BUDGET.YEAR$ = EDIT$(GL_BUD_YYYY_FILE$(X%), -1%)
			GOTO 300
		END IF
	END IF

	!
	! Ask for year
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		20%, &
		80%, &
		SMG_SCREEN_DATA% &
	)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, &
		1%, &
		1% &
	)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Budget Year:", 11%, 30%)

220	SCOPE::PRG_ITEM = "YEAR"
	GL_BUDGET.YEAR$ = LEFT(DATE_TODAY, 4%)

	SELECT ENTR_3ENTER(SCOPE, SMG_SCREEN_DATA%, 11%, 43%, &
		GL_BUDGET.YEAR$, -1%, 0%)

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		GOTO ExitProgram

	END SELECT

	GL_BUDGET.YEAR$ = EDIT$(GL_BUDGET.YEAR$, -1%)

	GL_BUDGET.YEAR$ = LEFT(DATE_TODAY, 2%) + GL_BUDGET.YEAR$ &
		IF LEN(GL_BUDGET.YEAR$) = 2%

	IF LEN(GL_BUDGET.YEAR$) <> 4%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Please enter the budget year in YYYY format", 0%)
		GOTO 220
	END IF

300	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%)

	!
	! Open up period file, and grab record
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.OPN"

		GET #GL_PERIOD.CH%, RECORD 1%, REGARDLESS
		CLOSE GL_PERIOD.CH%
	USE
		FILENAME$ = "GL_PERIOD"
		CONTINUE HelpError
	END WHEN

310	!
	! Open up chart file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.OPN"

		RESET #GL_CHART.CH%
		GET #GL_CHART.CH%, REGARDLESS
		UNLOCK #GL_CHART.CH%
	USE
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

	CHANGE GL_CHART::ACCT TO CHART_FORMAT%

320	!
	! Open up Budget file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_BUD_YYYY.CRE"
	USE
		FILENAME$ = "GL_BUD_" + GL_BUDGET.YEAR$
		CONTINUE HelpError
	END WHEN

400	!
	! Set up screens
	!
	COL$(1%) = "Account #"
	COL$(LOOP% + 1%) = LEFT(GL_PERIOD::PERIOD(LOOP%), 20%) + " Dollar" &
		FOR LOOP% = 1% TO GL_PERIOD::FPFY
	COL$(LOOP% + 1% + GL_PERIOD::FPFY) = &
		LEFT(GL_PERIOD::PERIOD(LOOP%), 20%) + " Unit" &
		FOR LOOP% = 1% TO GL_PERIOD::FPFY
	COL$(LOOP% + 1% + (GL_PERIOD::FPFY * 2%)) = &
		LEFT(GL_PERIOD::PERIOD(LOOP%), 20%) + " Hour" &
		FOR LOOP% = 1% TO GL_PERIOD::FPFY

500	!
	! Query user for dif file
	!
	CALL FIND_FILE(GL_RDDIF.DEV$ + "*.PLN", GL_RDDIF_FILE$(), 16%, "", "")

	GL_RDDIF_FILE% = VAL%(GL_RDDIF_FILE$(0%))

	IF GL_RDDIF_FILE%
	THEN
		FILE_EXT$ = ".PLN"
		TEMP$ = "Budget PLN Files"
		GOTO 580
	END IF

	CALL ENTR_3MESSAGE(SCOPE, "No plan perfect files found", 0%)
	GOTO ExitProgram

580	X% = ENTR_3CHOICE(SCOPE, "", "", GL_RDDIF_FILE$(), "", &
		0%, TEMP$, "", 0%)

	SELECT SCOPE::SCOPE_EXIT

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		GOTO ExitProgram

	END SELECT

	IF X% <= 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "Select a Plan Perfect file", 0%)
		GOTO 580
	END IF

590	OPEN GL_RDDIF.DEV$ + GL_RDDIF_FILE$(X%) + FILE_EXT$ &
		FOR INPUT AS FILE GL_RDDIF.CH%, &
		ORGANIZATION SEQUENTIAL FIXED, &
		RECORDTYPE NONE, &
		MAP GL_RDDIF, &
		ACCESS READ, &
		ALLOW READ

	BUFFERED$ = ""

600	!***************************************************************
	! Read dif file for columns
	!***************************************************************
	!
	! Create the data display
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		18%,		! 20 Rows &
		38%,		! 40 Columns &
		SMG_DIF,	! Identifier &
		SMG$M_BORDER	! Put a border around it &
	)

	CALL LIB$SIGNAL(SMG_STATUS%) IF (SMG_STATUS% AND 1%) = 0%

	SMG_STATUS% = SMG$LABEL_BORDER(SMG_DIF, &
		"Read PLN file")

	!
	! Paste the data display
	!
	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_DIF,	! Data pasteboard &
		SCOPE::SMG_PBID,	! Pasetboard &
		2%,		! Row to start in &
		2%,		! Column to start in &
				! Don't need top-disp &
	)

	CALL LIB$SIGNAL(SMG_STATUS%) IF (SMG_STATUS% AND 1%) = 0%

	SMG_STATUS% = SMG$SET_CURSOR_ABS(SMG_DIF, 1%, 1%)

	!
	! Define scrolling region
	!
	SMG_SCROLL::WINDOW	= SMG_DIF
	SMG_SCROLL::TOP_ARRAY	= 1%
	SMG_SCROLL::BOT_ARRAY	= 0%
	SMG_SCROLL::SCROLL_TOP	= 1%
	SMG_SCROLL::SCROLL_BOT	= 18%
	SMG_SCROLL::BEG_ELEMENT	= 1%
	SMG_SCROLL::END_ELEMENT	= 0%
	SMG_SCROLL::TOP_LINE	= 1%
	SMG_SCROLL::CUR_LINE	= 1%
	SMG_SCROLL::CUR_W_ROW	= 1%
	SMG_SCROLL::CUR_W_COL	= 1%
	SMG_SCROLL::FIND_LINE	= 1%
	SMG_SCROLL::SMG_FLAG	= 0%
	SMG_SCROLL::PROMPT	= "->"
	SMG_SCROLL::VIDEO_COMP	= 0%
	SMG_SCROLL::CHARSET	= 0%
	SMG_SCROLL::DRAW_COLS	= ""

	!
	! Create the column display
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		18%,		! 20 Rows &
		38%,		! 40 Columns &
		SMG_COL,	! Identifier &
		SMG$M_BORDER	! Put a border around it &
	)

	CALL LIB$SIGNAL(SMG_STATUS%) IF (SMG_STATUS% AND 1%) = 0%

	SMG_STATUS% = SMG$LABEL_BORDER(SMG_COL, &
		"Set Columns")

	!
	! Paste the data display
	!
	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_COL,	! Data pasteboard &
		SCOPE::SMG_PBID,	! Pasetboard &
		2%,		! Row to start in &
		42%,		! Column to start in &
				! Don't need top-disp &
	)

	CALL LIB$SIGNAL(SMG_STATUS%) IF (SMG_STATUS% AND 1%) = 0%

	SMG_STATUS% = SMG$SET_CURSOR_ABS(SMG_COL, 1%, 1%)

	!
	! Define scrolling region
	!
	COL_SCROLL::WINDOW	= SMG_COL
	COL_SCROLL::TOP_ARRAY	= 1%
	COL_SCROLL::BOT_ARRAY	= GL_PERIOD::FPFY * 3% + 1%
	COL_SCROLL::SCROLL_TOP	= 1%
	COL_SCROLL::SCROLL_BOT	= 18%
	COL_SCROLL::BEG_ELEMENT	= 1%
	COL_SCROLL::END_ELEMENT	= GL_PERIOD::FPFY * 3% + 1%
	COL_SCROLL::TOP_LINE	= 1%
	COL_SCROLL::CUR_LINE	= 1%
	COL_SCROLL::CUR_W_ROW	= 1%
	COL_SCROLL::CUR_W_COL	= 1%
	COL_SCROLL::FIND_LINE	= 1%
	COL_SCROLL::SMG_FLAG	= 0%
	COL_SCROLL::PROMPT	= "->"
	COL_SCROLL::VIDEO_COMP	= 0%
	COL_SCROLL::CHARSET	= 0%
	COL_SCROLL::DRAW_COLS	= ""

	TEMP% = DSPL_SCROLL(COL_SCROLL, COL$(), 0%, "PAINT")

650	!*******************************************************************
	! Skip Junk at fromt of PlanPerfect File
	!*******************************************************************

	!
	! Read Plan-Perfect Identification
	!
	JUNK$ = FNBUFFER$(16%)

	IF (MID(JUNK$, 2%, 3%) <> "WPC") OR &
		(ASCII(MID(JUNK$, 9%, 1%)) <> 9%)
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"This is not a Plan Perfect File!", 0%)
		GOTO ExitProgram
	END IF

	!
	! Loop through file, looking for Start of Cells marker.
	!
	CODE% = 0%
	WHILE (CODE% <> 25%)
		JUNK$ = FNBUFFER$(4%)
		CODE% = ASCII(MID(JUNK$, 1%, 1%)) + &
			ASCII(MID(JUNK$, 2%, 1%)) * 256%
		CLEN% = ASCII(MID(JUNK$, 3%, 1%)) + &
			ASCII(MID(JUNK$, 4%, 1%)) * 256%
		JUNK$ = FNBUFFER$(CLEN%)
	NEXT

	!
	! Prime the pump
	!
	Gosub ReadCell

700	!********************************************************************
	! Read cells into working array
	!********************************************************************

	IF (COL% = 65536%)
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "End of Spreadsheet Found!", 0%)
		GOTO ExitProgram
	END IF

	!
	! Pull in entire row
	!
	THISROW% = ROW%

710	WHILE (THISROW% = ROW%)
		WHEN ERROR IN
			D_I_F$(COL%) = SVALUE$
		USE
			CONTINUE 720 IF ERR = 55%
			FILENAME$ = ""
			CONTINUE HelpError
		END WHEN

		DIF% = COL% IF COL% > DIF%
		Gosub ReadCell
	NEXT

720	!
	! Now play with it
	!
	SMG_SCROLL::BOT_ARRAY	= DIF%
	SMG_SCROLL::BOT_ARRAY	= 1% IF DIF% = 0%
	SMG_SCROLL::END_ELEMENT	= DIF%
	SMG_SCROLL::END_ELEMENT	= 1% IF DIF% = 0%

 DifInit:
	TEMP% = DSPL_SCROLL(SMG_SCROLL, D_I_F$(), 0%, "PAINT")

	!
	! Initialize last option pointer
	!
	OPT% = 0%

	OPT$ = "Next_pln_row Reset_pln_row Set_columns eXit"

 DifMenu:
	!==================================================================
	! Enter desired option
	!==================================================================
	SCOPE::PRG_ITEM = ""

	OPTION$ = ENTR_3OPTION(SCOPE, "COMMAND", OPT$, OPT%, OPTFLAG%)

	SYS_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)

	SELECT SCOPE::SCOPE_EXIT
	CASE SMG$K_TRM_CTRLC		! ^C
		GOTO DifInit

	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

	CASE SMG$K_TRM_UP, SMG$K_TRM_DOWN, SMG$K_TRM_PREV_SCREEN, &
		SMG$K_TRM_NEXT_SCREEN, SMG$K_TRM_F18, SMG$K_TRM_F19

		TEMP% = DSPL_SCROLL(smg_SCROLL, &
			D_I_F$(), &
			SCOPE::SCOPE_EXIT, &
			"")

		GOTO DifMenu

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
		GOTO DifMenu

	END SELECT

 SelectOption:
	!
	! Decide what to do with the option
	!
	SELECT OPTION$
	CASE "S"	! Set columns
		COL_LOOP% = 1%
		GOTO 1000

	CASE "N"	! Next dif row
		DIF% = 0%
		GOTO 700

	CASE "R"	! Reset dif row
		RESET #GL_RDDIF.CH%

		BUFFERED$ = ""

		!
		! Read Plan-Perfect Identification
		!
		JUNK$ = FNBUFFER$(16%)

		IF (MID(JUNK$, 2%, 3%) <> "WPC") OR &
			(ASCII(MID(JUNK$, 9%, 1%)) <> 9%)
		THEN
			CALL ENTR_3MESSAGE(SCOPE, &
				"This is not a Plan Perfect File!", 0%)
			GOTO ExitProgram
		END IF

		!
		! Loop through file, looking for Start of Cells marker.
		!
		CODE% = 0%
		WHILE (CODE% <> 25%)
			JUNK$ = FNBUFFER$(4%)
			CODE% = ASCII(MID(JUNK$, 1%, 1%)) + &
				ASCII(MID(JUNK$, 2%, 1%)) * 256%
			CLEN% = ASCII(MID(JUNK$, 3%, 1%)) + &
				ASCII(MID(JUNK$, 4%, 1%)) * 256%
			JUNK$ = FNBUFFER$(CLEN%)
		NEXT

		!
		! Prime the pump
		!
		GOSUB ReadCell

		GOTO 700

	CASE "X"	! Exit
		GOTO ExitProgram

	END SELECT

	OPTFLAG% = 0%
	GOTO DifMenu

	%PAGE

1000	!*******************************************************************
	! Set columns
	!*******************************************************************
	TEMP$ = "Select column in 'Read pln window' " + &
		"- Then press <Do> or <Exit> for next step "

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, TEMP$, 1%, 1%)

	JUNK$ = " "
	SELECT ENTR_3ENTER(SCOPE, SCOPE::SMG_OPTION, 1%, &
		LEN(TEMP$), JUNK$, -1%, 4096%)

	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO 2000

	CASE SMG$K_TRM_UP, SMG$K_TRM_DOWN, SMG$K_TRM_PREV_SCREEN, &
		SMG$K_TRM_NEXT_SCREEN, SMG$K_TRM_F18, SMG$K_TRM_F19

		TEMP% = DSPL_SCROLL(SMG_SCROLL, &
			D_I_F$(), &
			SCOPE::SCOPE_EXIT, &
			"")

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
		GOTO 1000

	END SELECT

	WORK% = SMG_SCROLL::CUR_LINE

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, SPACE$(80%), 1%, 1%)

1010	!****************************************************************
	! Select the budget item
	!****************************************************************
	TEMP$ = "Select budget item in 'Set Columns window' " + &
		"- Then press <Do> "

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, TEMP$, 1%, 1%)

	JUNK$ = " "
	SELECT ENTR_3ENTER(SCOPE, SCOPE::SMG_OPTION, 1%, LEN(TEMP$), &
		JUNK$, -1%, 4096%)

	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO 2000

	CASE SMG$K_TRM_UP, SMG$K_TRM_DOWN, SMG$K_TRM_PREV_SCREEN, &
		SMG$K_TRM_NEXT_SCREEN, SMG$K_TRM_F18, SMG$K_TRM_F19

		TEMP% = DSPL_SCROLL(COL_SCROLL, &
			COL$(), &
			SCOPE::SCOPE_EXIT, &
			"")

		GOTO 1010

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
		GOTO 1010

	END SELECT

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, SPACE$(80%), 1%, 1%)

	COL_LOOP% = COL_SCROLL::CUR_LINE

	ACC_COL% = WORK% IF COL_LOOP% = 1%

	COL%(COL_LOOP%) = WORK%

	COL$(COL_LOOP%) = LEFT(COL$(COL_LOOP%) + SPACE$(28%), 28%) + &
		" COL=" + FORMAT$(WORK%, "<0>##")

	TEMP% = DSPL_SCROLL(COL_SCROLL, COL$(), 0%, "PAINT")

	COL_LOOP% = COL_LOOP% + 1%

	IF COL_LOOP% <= GL_PERIOD::FPFY * 3% + 1%
	THEN
		COL_SCROLL::FIND_LINE = COL_LOOP%
		TEMP% = DSPL_SCROLL(COL_SCROLL, COL$(), 0%, "FIND")
		IF WORK% + 1% <= SMG_SCROLL::END_ELEMENT
		THEN
			SMG_SCROLL::FIND_LINE = WORK% + 1%
			TEMP% = DSPL_SCROLL(SMG_SCROLL, D_I_F$(), 0%, "FIND")
		END IF
	END IF

	GOTO 1000

2000	!
	! Set colunms
	!
	COL$ = "!"

	COL$ = COL$ + FORMAT$(COL%(LOOP%), "<0>#") + "!" IF COL%(LOOP%) &
		FOR LOOP% = 1% TO 40%

	!***************************************************************
	! Confirm that everything is ok
	!***************************************************************
	SCOPE::PRG_ITEM = "CONFIRM"

	TEMP$ = "Confirm processing file - Then press <Do> "

2100	YESNO$ = EDIT$(ENTR_3YESNO(SCOPE, SCOPE::SMG_OPTION, &
		"", TEMP$, "N", 0%, "", ""), -1%)

	SELECT SCOPE::SCOPE_EXIT

	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

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
		GOTO 2100

	END SELECT

	IF YESNO$ <> "Y"
	THEN
		GOTO ExitProgram
	END IF

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, SPACE$(80%), 1%, 1%)

17000	!-----------------------------------READ A DIF FILE

	RESET #GL_RDDIF.CH%

	BUFFERED$ = ""

	!
	! Read Plan-Perfect Identification
	!
	JUNK$ = FNBUFFER$(16%)

	IF (MID(JUNK$, 2%, 3%) <> "WPC") OR &
		(ASCII(MID(JUNK$, 9%, 1%)) <> 9%)
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"This is not a Plan Perfect File!", 0%)
		GOTO ExitProgram
	END IF

	!
	! Loop through file, looking for Start of Cells marker.
	!
	CODE% = 0%
	WHILE (CODE% <> 25%)
		JUNK$ = FNBUFFER$(4%)
		CODE% = ASCII(MID(JUNK$, 1%, 1%)) + &
			ASCII(MID(JUNK$, 2%, 1%)) * 256%
		CLEN% = ASCII(MID(JUNK$, 3%, 1%)) + &
			ASCII(MID(JUNK$, 4%, 1%)) * 256%
		JUNK$ = FNBUFFER$(CLEN%)
	NEXT

	!
	! Prime the pump
	!
	GOSUB ReadCell

	!
	! Set up to trap interrupt
	!
	SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
		LOC(OUTP_XUNSOL) BY VALUE, &
		LOC(SCOPE::SMG_KBID) BY VALUE)

	RRR_FLAG% = 0%

17030	!
	! Handle any special junk in RRR_FLAG%
	!
	SELECT RRR_FLAG%

	!
	! Repaint screen
	!
	CASE SMG$K_TRM_F11, SMG$K_TRM_CTRLW
		SMG_STATUS% = SMG$REPAINT_SCREEN(SCOPE::SMG_PBID)
		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)

	!
	! Help
	!
	CASE SMG$K_TRM_HELP
		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)
		CALL HELP_34MESSAGE(SCOPE, "", SCOPE::PRG_IDENT, &
			SCOPE::PRG_PROGRAM, "", SCOPE::PRG_ITEM)
		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)

	!
	! Interupt
	!
	CASE SMG$K_TRM_F6, SMG$K_TRM_F20
		SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

		CALL MENU_3INTERRUPT(SCOPE)

		SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
			LOC(OUTP_XUNSOL) BY VALUE, &
			LOC(SCOPE::SMG_KBID) BY VALUE)

	!
	! Exit
	!
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		GOTO ExitProgram

	END SELECT

	RRR_FLAG% = 0%

	!
	! Read one line for the dif file
	!
	IF (COL% = 65536%)
	THEN
		GOTO ExitProgram
	END IF

	!
	! Pull in entire row
	!
	THISROW% = ROW%

	WHILE (THISROW% = ROW%)
		!
		! Handle account number
		!
		IF (COL% = ACC_COL%) AND (SVALUE$ <> "")
		THEN
			TEMP_ACCT_NUM$ = SVALUE$
			GOSUB LookUpAcct
		END IF

		GOSUB SetColumns

		!
		! Preload next cell
		!
		Gosub ReadCell
	NEXT

	GOSUB PutBudget

17390	GOTO 17030

	!******************************************************************
	! End of the program
	!******************************************************************
 ExitProgram:
	!
	! Disable unsolicited input
	!
	SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

 GetBudgetRecord:
18000	WHEN ERROR IN
		GET #GL_BUD_YYYY.CH%, KEY #0% EQ ACCOUNT_NUM$
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE 18010
	END WHEN

	RETURN

18010	GL_BUD_YYYY::DOLLAR(LOOP%)	= 0.0 FOR LOOP% = 0% TO 13%
	GL_BUD_YYYY::UNIT(LOOP%)	= 0.0 FOR LOOP% = 0% TO 13%
	GL_BUD_YYYY::HOUR(LOOP%)	= 0.0 FOR LOOP% = 0% TO 13%

	GL_BUD_YYYY::ACCT		= EDIT$(ACCOUNT_NUM$, 8% + 128%)

	PUT #GL_BUD_YYYY.CH%

	GET #GL_BUD_YYYY.CH%, KEY #0% EQ ACCOUNT_NUM$

	RETURN

 LookUpAcct:
18100	!**************************************************************
	! Look up the account number
	!**************************************************************
	TEMP_ACCT_NUM$ = XLATE(TEMP_ACCT_NUM$, STRING$(48%, 0%) + &
		"0123456789") + STRING$(8%, A"0"B)
	ACCOUNT_NUM$ = ""

	FOR LOOP% = 1% TO 18%
		ACCOUNT_NUM$ = ACCOUNT_NUM$ + CHR$(CHART_FORMAT%(LOOP%)) &
			IF CHART_FORMAT%(LOOP%) < 48% OR &
			CHART_FORMAT%(LOOP%) > 57%
		ACCOUNT_NUM$ = ACCOUNT_NUM$ + LEFT(TEMP_ACCT_NUM$, 1%) &
			IF CHART_FORMAT%(LOOP%) > 47% AND &
			CHART_FORMAT%(LOOP%) < 58%
		TEMP_ACCT_NUM$ = RIGHT(TEMP_ACCT_NUM$, 2%) &
			IF CHART_FORMAT%(LOOP%) > 47% AND &
			CHART_FORMAT%(LOOP%) < 58%
	NEXT LOOP%

	WHEN ERROR IN
		FIND #GL_CHART.CH%, KEY #0% EQ ACCOUNT_NUM$, REGARDLESS
	USE
		CONTINUE 18190
	END WHEN

	CALL ENTR_3MESSAGE(SCOPE, ACCOUNT_NUM$, 1%)
	ACCOUNT_TEST% = -1%

18190	RETURN

	%Page

 PutBudget:
18200	!********************************************************************
	! Put Budget Data
	!********************************************************************
	IF ACCOUNT_TEST%
	THEN
		GOSUB GetBudgetRecord

		GL_BUD_YYYY::DOLLAR(I%) = INT(AMOUNT(COL%(I% + 1%))) &
			IF COL%(I% + 1%) &
			FOR I% = 1% TO GL_PERIOD::FPFY

		GL_BUD_YYYY::UNIT(I%) = &
			INT(AMOUNT(COL%(I% + GL_PERIOD::FPFY + 1%))) &
			IF COL%(I% + GL_PERIOD::FPFY + 1%) &
			FOR I% = 1% TO GL_PERIOD::FPFY

		GL_BUD_YYYY::HOUR(I%) = &
			INT(AMOUNT(COL%(I% + 2% * GL_PERIOD::FPFY + 1%))) &
			IF COL%(I% + 2% * GL_PERIOD::FPFY + 1%) &
			FOR I% = 1% TO GL_PERIOD::FPFY

		WHEN ERROR IN
			UPDATE #GL_BUD_YYYY.CH%
		USE
			FILENAME$ = "GL_BUD_" + GL_BUDGET.YEAR$
			CONTINUE HelpError
		END WHEN
	END IF

18290	AMOUNT(I%) = 0.0 FOR I% = 0% TO 50%
	ACCOUNT_TEST% = 0%

	RETURN

	%Page

 SetColumns:
18400	!****************************************************************
	! Set budget values into the columns
	!***************************************************************
	TEMP% = 1%
	WHEN ERROR IN
		A = FUNC_ROUND(VAL(SVALUE$), 2%)
	USE
		CONTINUE 18490 IF ERR = 52%
		FILENAME$ = ""
		CONTINUE HelpError
	END WHEN

 SetColumns1:
	PP% = (INSTR(TEMP%, COL$, "!" + FORMAT$(COL%, "<0>#") + "!") + 2%) / &
		3% - 1%
	IF PP% > 0%
	THEN
		AMOUNT(COL%) = A
		TEMP% = (PP% + 1%) * 3% + 1%
		GOTO SetColumns1
	END IF

18490	RETURN

	%Page

18500	!*******************************************************************
	! Buffering function
	!*******************************************************************

	DEF *FNBUFFER$(X%)

		WHILE LEN(BUFFERED$) < X%
			GET #GL_RDDIF.CH%
			BUFFERED$ = BUFFERED$ + GL_RDDIF_BUFFER$
		NEXT

		FNBUFFER$ = LEFT(BUFFERED$, X%)
		BUFFERED$ = RIGHT(BUFFERED$, X% + 1%)

	FNEND

	%PAGE

18600	!*******************************************************************
	! Read in next cell
	!*******************************************************************
 ReadCell:

	JUNK$ = FNBUFFER$(20%)

	ROW% = ASCII(MID(JUNK$, 1%, 1%)) + &
		ASCII(MID(JUNK$, 2%, 1%)) * 256% + 1%
	COL% = ASCII(MID(JUNK$, 3%, 1%)) + &
		ASCII(MID(JUNK$, 4%, 1%)) * 256% + 1%
	FORMAT% = ASCII(MID(JUNK$, 13%, 1%)) AND 7%
	FLEN% = ASCII(MID(JUNK$, 19%, 1%)) + &
		ASCII(MID(JUNK$, 20%, 1%)) * 256%

	SELECT FORMAT%

	!
	! Empty Cell (or nothing usefull in it)
	!
	CASE 0%, 4%, 5%, 6%
		SVALUE$ = ""

	!
	! Floating Point Number
	!
	CASE 1%
		SVALUE = 0.0
		EXP% = ASCII(MID(JUNK$, 5%, 1%))
		SVALUE = SVALUE + 1.0 * ASCII(MID(JUNK$, I% + 5%, 1%)) / &
			(256.0 ^ I%) &
			FOR I% = 1% TO 7%
		SVALUE = SVALUE * 16.0 ^ ((EXP% AND 127%) - 64%)
		SVALUE = -SVALUE IF EXP% AND 128%
		SVALUE$ = NUM1$(SVALUE)

	!
	! Short Text
	!
	CASE 2%
		TLEN% = ASCII(MID(JUNK$, 5%, 1%))
		SVALUE1$ = MID(JUNK$, 6%, TLEN%)
		SVALUE$ = ""
		FOR I% = 1% TO LEN(SVALUE1$)
			I1% = ASCII(MID(SVALUE1$, I%, 1%))
			SVALUE$ = SVALUE$ + CHR$(I1%) &
				IF (I1% >= 32%) AND (I1% <= 126%)
		NEXT I%

	!
	! Long Text
	!
	CASE 3%
		TLEN% = ASCII(MID(JUNK$, 5%, 1%)) + &
			ASCII(MID(JUNK$, 6%, 1%)) * 256%
		SVALUE1$ = FNBUFFER$(TLEN%)
		SVALUE$ = ""
		FOR I% = 1% TO LEN(SVALUE1$)
			I1% = ASCII(MID(SVALUE1$, I%, 1%))
			SVALUE$ = SVALUE$ + CHR$(I1%) &
				IF (I1% >= 32%) AND (I1% <= 126%)
		NEXT I%
	END SELECT

	!
	! Strip of any formulas that might exist
	!
	IF FLEN% <> 0%
	THEN
		JUNK$ = FNBUFFER$(FLEN%)
	END IF

	RETURN

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitProgram

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
