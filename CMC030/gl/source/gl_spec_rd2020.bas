1	%TITLE "Read DIF File and Store Data in Budget File"
	%SBTTL "GL_SPEC_RD2020"
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
	!	.b
	!	.lm +5
	!	A common practice used in budget preparation is to enter
	!	detail data in an electronic spreadsheet. The spreadsheet may
	!	be prepared on an on-line spreadsheet or on a personal computer
	!	and transferred to a Dif File. Once the final budget as prepared
	!	in the spreadsheet is accepted, the spreadsheet file can be copied
	!	by the system into the system budget file.
	!	.b
	!	First, the budget data must be entered in the spreadsheet to
	!	reflect the change (or sum of the budgeted transactions) for each
	!	period for each budgeted account. Account numbers to be budgeted
	!	are to be entered in a designated column with period budgets
	!	entered in the related rows.
	!	.b
	!	If the spreadsheet is prepared with the use of an on-line 20/20
	!	system, the following steps are to be taken in order to copy the
	!	spreadsheet into the Budget file:
	!	.b
	!	.ls 0
	!	.le
	!	Access the 20/20 system.
	!	.le
	!	Retrieve the Budget Spreadsheet by accessing the Store->Read
	!	20/20 menu commands.
	!	.le
	!	Access the Store->Export->Data->Delimited 20/20 menu commands.
	!	.le
	!	Enter ^*<FILENAME>.2020\*.
	!	.le
	!	Access the Export->Range 20/20 menu commands.
	!	.le
	!	Designate the range of the Spreadsheet.
	!	.le
	!	Exit 20/20.
	!	.le
	!	Access the GL->SPEC->COPY menu path.
	!	.le
	!	^*<Select>\* the appropriate Budget Year.
	!	.le
	!	^*<Select>\* the 20/20 Spreadsheet File.
	!	.le
	!	Access ^*Next__dif__row\* Command menu function.
	!	.le
	!	Set Columns.
	!	.le
	!	Press ^*<Enter>\*.
	!	.le
	!	Confirm (Yes or No).
	!	.le
	!	Press ^*<Do>\* to copy.
	!	.els
	!	.b
	!	The following are instructions for copying a spreadsheet from a
	!	personal computer:
	!	.b
	!	After the spreadsheet is prepared, the file must be translated
	!	into a DIF file and then moved to the target machine. The DIF file
	!	is then processed using the ^*Copy Dif File into Budget File\*
	!	program. The ^&translation\& and ^&move\& operations are explained
	!	in the following paragraphs. The ^&copy\& operation is explained
	!	in the various sections of the ^*Copy Dif File into Budget File\*
	!	program.
	!	.b
	!	^*FLOPPY DRIVE SYSTEM\* (For a one hard disk system, skip
	!	paragraphs I and II.
	!	.b 1
	!	I.##^&Translating\& Spreadsheet (.WKS) files to .DIF format
	!	.b 1
	!	.ls 0
	!	.le
	!	Load spreadsheet on off-line system
	!	.le
	!	Select the Translate Option
	!	.le
	!	Select "WKS-> DIF" Option
	!	.le
	!	Insert data disk in drive A
	!	.le
	!	Select A as the source disk
	!	.le
	!	Use the arrow keys to point to a file and press the
	!	space bar to toggle the mark
	!	.le
	!	Commence processing the file ???
	!	.le
	!	Select B as the output disk
	!	.le
	!	Commence the file translation process
	!	.le
	!	When the process is completed, exit the Translate
	!	Utility
	!	.le
	!	Exit the spreadsheet system
	!	.els
	!	.note
	!	Output into the DIF file in row order, if your version requests
	!	which order it should output in.
	!	.end note
	!	.lm -3
	!	II. ^&Move\& the Spreadsheet DIF File to the target computer.
	!	III. ^&Copy\& the Spreadsheet File to the Budget File.
	!	.ls 0
	!	.le
	!	Access the GL->SPEC->COPY menu path.
	!	.le
	!	^*<Select>\* the appropriate Budget Year.
	!	.le
	!	^*<Select>\* the Spreadsheet File.
	!	.le
	!	Access ^*Next__dif__row\* Command menu function.
	!	.le
	!	Set Columns.
	!	.le
	!	Press ^*<Enter>\*.
	!	.le
	!	Confirm (Yes or No).
	!	.le
	!	Press ^*<Do>\* to copy.
	!	.els
	!	.note
	!	The 2020 files ^*must\* have an extension of ^*.2020\* for the read 2020
	!	process to be able to find them, and they must have been saved in text
	!	format.
	!	.end note
	!
	! Index:
	!	.x Budget>Copy 2020 File
	!	.x Spreadsheet>Copy 2020 File to Budget
	!	.x Copy>2020 File to Budget
	!	.x 2020 File to Budget>Copy
	!	.x Copy 2020 File>Budget
	!	.x Copy 2020 File to Budget>Spreadsheet
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_SPEC_RD2020/LINE
	!	$ LINK/EXECUTABLE=GL_EXE: GL_SPEC_RD2020, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_SPEC_RD2020.OBJ;*
	!
	! Author:
	!
	!	04/09/91 - Kevin Handy
	!		Split from GL_SPEC_RDDIF so that we weren't trying
	!		to do every possible spreadsheet type in one program.
	!		It was getting too confusing, and documenting it
	!		was nearly impossible.
	!
	! Modification history:
	!
	!	05/11/91 - Frank F. Starmanm
	!		Fix bug that month budget will be imported to the
	!		right month.
	!
	!	05/30/91 - Kevin Handy
	!		Changed "Close/open" sequence to a "Reset".
	!
	!	05/30/91 - Kevin Handy
	!		Added "Access read, allow read" to open of
	!		dif file.
	!
	!	08/14/91 - Kevin Handy
	!		Removed A+.
	!
	!	03/30/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/14/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!		Fix last parameter in ENTR_3CHOICE.
	!
	!	12/15/95 - Kevin Handy
	!		Reformat closer to 80 columns.
	!		Change RIGHT(NUM1$()) to FORMAT$().
	!
	!	05/15/97 - Kevin Handy
	!		Reformat closer to 80 columns.
	!
	!	08/25/97 - Kevin Handy
	!		Use 'val%' instead of 'val'
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

	MAP (DP_OUTP_XUNSOL) RRR_FLAG%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION DSPL_SCROLL
	EXTERNAL LONG	OUTP_XUNSOL ! (It's really an AST routine)

	%PAGE

	!
	! Declare  Vars
	!
	DECLARE LONG SMG_DIF, SMG_COL

	RRR_FLAG% = 0%

	!
	! Dimension
	!
	DIM GL_BUD_YYYY_FILE$(25%), GL_RDDIF_FILE$(100%)
	DIM D_I_F$(300%), COL$(40%)
	DIM COL%(40%), CHART_FORMAT%(18%), AMOUNT(50%)

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
	CALL FIND_FILE( GL_BUD_YYYY.DEV$ + "GL_BUD_*.MAS", &
		GL_BUD_YYYY_FILE$(), 16%, "", "")

	GL_BUD_YYYY_FILE% = VAL%(GL_BUD_YYYY_FILE$(0%))

	IF GL_BUD_YYYY_FILE%
	THEN
		GL_BUD_YYYY_FILE$(LOOP%) = &
			MID(GL_BUD_YYYY_FILE$(LOOP%),8%,4%) &
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

500	CALL FIND_FILE(GL_RDDIF.DEV$ + "*.2020", GL_RDDIF_FILE$(), &
		16%, "", "")

	GL_RDDIF_FILE% = VAL%(GL_RDDIF_FILE$(0%))

	IF GL_RDDIF_FILE% = 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "DIF files do not exist", 0%)
		GOTO ExitProgram
	END IF

	FILE_EXT$ = ".2020"
	TEMP$ = "Budget 2020 Files"

580	X% = ENTR_3CHOICE(SCOPE, "", "", GL_RDDIF_FILE$(), "", &
		0%, TEMP$, "", 0%)

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		GOTO ExitProgram

	END SELECT

	IF X% <= 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "Select a DIF files ", 0%)
		GOTO 580
	END IF

590	OPEN GL_RDDIF.DEV$ + GL_RDDIF_FILE$(X%) + FILE_EXT$ FOR INPUT &
		AS FILE GL_RDDIF.CH%, &
		ACCESS READ, ALLOW READ

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
		"Read DIF file")

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

700	!********************************************************************
	! Read a row
	!********************************************************************
	LINPUT #GL_RDDIF.CH%, INP$

	!
	! S 2020 default export file layout
	!

 S2020:	! Finished with this line
	IF INP$ <> "#"
	THEN
		I% = INSTR(1%, INP$, " #")
		!
		! If no '#' found then get next record
		!
		IF I% = 0%
		THEN
			GOTO 700
		END IF

		INP1$ = LEFT(INP$, I% - 1%)
		INP$ = RIGHT(INP$, I% + 2%)

		IF INSTR(1%, "^'", LEFT(INP1$, 1%))
		THEN
			INP1$ = RIGHT(INP1$, 2%)
			TEST$ = '"'
		ELSE
			IF INSTR(1%, "0123456789", LEFT(INP1$, 1%))
			THEN
				TEST$ = "V"
			ELSE
				TEST$ = '"'
			END IF
		END IF

		DIF% = DIF% + 1%

		D_I_F$(DIF%) = FORMAT$(DIF%, "<0>##") + " " + INP1$

		GOTO S2020
	END IF

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

	OPT$ = "Next_dif_row Set_columns eXit"

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
		INP1$ = ""
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
	TEMP$ = "Select column in 'Read dif window' " + &
		"- Then press <Do> or <Exit> for next step "

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, TEMP$, 1%, 1%)

	JUNK$ = " "
	SELECT ENTR_3ENTER(SCOPE, SCOPE::SMG_OPTION, 1%, LEN(TEMP$), &
		JUNK$, -1%, 4096%)

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
	TEMP$ = "Select budget item in 'Set Columns window' - Then press <Do> "

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, TEMP$, 1%, 1%)

	JUNK$ = " "
	SELECT ENTR_3ENTER(SCOPE, SCOPE::SMG_OPTION, 1%, &
		LEN(TEMP$), JUNK$, -1%, 4096%)

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

2100	YESNO$ = EDIT$(ENTR_3YESNO(SCOPE, SCOPE::SMG_OPTION, "", &
		TEMP$, "N", 0%, "", ""), -1%)

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
	WHEN ERROR IN
		LINPUT #GL_RDDIF.CH%, INP$
	USE
		CONTINUE ExitProgram
	END WHEN

	!
	! If S2020 structure
	!
 S2020SpreadSheet:
	IF INP$ <> "#"
	THEN
		I% = INSTR(1%, INP$, " #")
		!
		! If no '#' found then get next record
		!
		IF I% = 0%
		THEN
			GOTO 17390
		END IF

		INP1$ = LEFT(INP$, I% - 1%)
		INP$ = RIGHT(INP$, I% + 2%)

		IF INSTR(1%, "^'", LEFT(INP1$, 1%))
		THEN
			INP1$ = RIGHT(INP1$, 2%)
			TEST$ = '"'
		ELSE
			IF INSTR(1%, "-0123456789", LEFT(INP1$, 1%))
			THEN
				TEST$ = "V"
			ELSE
				TEST$ = '"'
			END IF
		END IF

		A = 0.0
		COL% = COL% + 1%

		IF TEST$ = "V"
		THEN
			A = VAL(INP1$)
		END IF

		IF COL% = ACC_COL% AND INP1$ <> ""
		THEN
			TEMP_ACCT_NUM$ = INP1$

			GOSUB LookUpAcct
		END IF

		GOSUB SetColumns

		GOTO S2020SpreadSheet

	END IF

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

		GL_BUD_YYYY::DOLLAR(I%) = &
			INT(AMOUNT(COL%(I% + 1%))) IF COL%(I% + 1%) &
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
	INP1$ = ""
	COL% = 0%
	ACCOUNT_TEST% = 0%

	RETURN

	%Page

 SetColumns:
	!****************************************************************
	! Set budget values into the columns
	!***************************************************************
	TEMP% = 1%

 SetColumns1:
	PP% = (INSTR(TEMP%, COL$, "!" + FORMAT$(COL%, "<0>#") &
		+ "!") + 2%) / 3% - 1%
	IF PP% > 0%
	THEN
		AMOUNT(COL%) = A
		TEMP% = (PP% + 1%) * 3% + 1%
		GOTO SetColumns1
	END IF

	RETURN

	%Page

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
