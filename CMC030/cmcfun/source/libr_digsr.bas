1	%TITLE "Digital Standard Runoff for Text Array"
	%SBTTL "LIBR_DIGSR"
	%IDENT "V3.6a Calico"

	FUNCTION LONG LIBR_DIGSR(LIB_NAME$, KEY_NAME$, CODE$())

	!
	! COPYRIGHT (C) 1987, 1989 BY
	!
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
	!	This subroutine takes the filename and given array and
	!	opens the file and reads the DSR commands embedded in
	!	the text array and interprets them setting them up in
	!	the array for use by SMG routines(PUT_VIRTUAL_DISPLAY_ENCODED).
	!
	! Parameters:
	!
	!	LIB_NAME$
	!		The passed name of the library to pull the text from.
	!
	!	KEY_NAME$
	!		The passed name of the key to use to select the
	!		right text.
	!
	!	CODE$(0%)
	!		Contains the number of lines already in
	!		use in the CODE$() array.
	!
	!	CODE$()
	!		The text returned back in PRNT_ENCODED format.
	!		CODE$(0%) is modified to point to the last line.
	!
	!	Returns a status code.
	!
	! Example:
	!
	!	TEXT$(0%) = "0"
	!	ST% = LIBR_DIGSR("HELP_GL", "ADDRESS", TEXT$())
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:LIBR_DIGSR/LINE
	!	$ LIB FUNC_LIB:CMC_3VECTOR/REP LIBR_DIGSR
	!	$ DELETE LIBR_DIGSR.OBJ;*
	!
	! Author:
	!
	!	02/20/87 - B. Craig Larsen
	!
	! Modification history:
	!
	!	07/08/87 - Kevin Handy
	!		Modified to pull text out of a library.
	!
	!	09/30/87 - Kevin Handy
	!		Added code for LIST ELEMENT stuff.
	!
	!	10/01/87 - Kevin Handy
	!		Fixed bug where too many parameters on a line
	!		caused the program to crash.
	!
	!	12/22/87 - Kevin Handy
	!		Expanded the .LIST enviornment
	!
	!	06/17/88 - Kevin Handy
	!		Several mods for .RM, .FILL, .JUSTIFY
	!
	!	06/30/88 - Kevin Handy
	!		Added italics flags (underlines on VTxx term)
	!		using ^~ and \~ for flags.
	!
	!	07/19/88 - Kevin Handy
	!		Removed defaulting to REF:
	!
	!	07/26/88 - Kevin Handy
	!		Fix .RM problem where an offset position entered
	!		would offset against the left margin instead of
	!		the right.
	!
	!	06/16/89 - Kevin Handy
	!		Modifications to bring it closer to fitting
	!		in the sharable library.
	!
	!	10/17/89 - Kevin Handy
	!		Extended maximum length of an input line.
	!
	!	10/12/90 - Kevin Handy
	!		Modified the .NOTE item so that it will center
	!		the title better.
	!
	!	08/12/91 - Kevin Handy
	!		Added FOOTNOTE and END FOOTNOTE commands.
	!
	!	08/14/91 - Kevin Handy
	!		Added QUOTE and END QUOTE commands.
	!
	!	03/14/92 - Kevin Handy
	!		Clean up vars (checkvar)
	!
	!	03/23/92 - Kevin Handy
	!		Add in tab stops and tabs (.TS), (\009).
	!
	!	03/26/92 - Kevin Handy
	!		Added Table option (.TABLE, .ENDTABLE, .TE).
	!
	!	03/27/92 - Kevin Handy
	!		Added (.TT) "Table Title" option. Looks like
	!		(.TE) here, but is handled slightly different
	!		in LaTeX output.
	!
	!	09/15/92 - Kevin Handy
	!		Modified to print out an error message when bad
	!		numbers are entered in the table option, due to the
	!		massive number of problems caused by this.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	11/20/95 - Kevin Handy
	!		Lose /NOWARN on compile statement.
	!
	!	10/17/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	01/08/99 - Kevin Handy
	!		Fix bug with '.en' incorrectly modifying the
	!		left margin.
	!
	!	04/08/99 - Kevin Handy
	!		Use BASIC$STARLET for STR$ routines
	!
	!	12/28/2000 - Kevin Handy
	!		Use some WHEN ERROR IN
	!--

	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:SMG_EXTERNALS.COM"
	%INCLUDE "FUNC_INCLUDE:LIBRARY.COM"
	%INCLUDE "STR$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	!
	! Declarations
	!
	DECLARE LONG SUB_MATCH, POSITION
	DECLARE RFA TXRFA
	DECLARE INTEGER CONSTANT MAX_LIST = 20%

	DIM LIST_CHAR$(MAX_LIST), LIST_NUM%(MAX_LIST), LIST.SKIP%(MAX_LIST)
	DIM ARY_ATTR%(200%), ARY_ATTP%(200%)

	DIM TS%(32%), OLDTS%(32%)	! Tab Stops

	RECORD FD_RECORD
		VARIANT
		CASE
			STRING FD = 5
		CASE
			WORD CBEG
			WORD CLEN
			BYTE CATR
		END VARIANT
	END RECORD

	DECLARE FD_RECORD FD

	RECORD FDL_RECORD
		VARIANT
		CASE
			STRING FDL = 2
		CASE
			WORD FDLEN
		END VARIANT
	END RECORD

	DECLARE FDL_RECORD FDL

	%PAGE

	!
	! Assume success
	!
	LIBR_DIGSR = 1%

	LEFT_MAR% = 5%		! Left margin
	RIGHT_MAR% = 73%	! Right Margin
	INDENT% = 0%		! Indent
	LIST_LIST% = 0%		! In a list?
	TABLE_TABLE% = 0%	! In a table?
	NOTE_NOTE% = 0%		! In a note?
	NOTE_QUOTE% = 0%	! In a note?
	NOTE_FOOTNOTE% = 0%	! In a note?
	FILL_FLAG% = -1%	! Fill/justify flag
	CUR_ATTR% = 0%		! Current attributes

	ARY_ATTR%(0%) = 0%	! Lowest attribute if none
	ARY_ATTP%(0%) = 0%	! And starts at the first character

	!
	! Initilize tab stops
	!
	TS%(I%) = 8% * I% FOR I% = 1% TO 31%
	TS%(32%) = 32767%

	!
	! Initilization
	!
	ON ERROR GOTO 19000

	CURR_LINE% = VAL%(CODE$(0%))
	CURR_LINE% = 0% IF CURR_LINE% < 1%
	FDL::FDLEN, FD::CBEG, FD::CLEN, FD::CATR = 0%

50	!
	! Set up the control structure if necessary
	!
	ST% = LBR$INI_CONTROL(LR.INDEX%, LBR$C_READ)

	IF (ST% AND 1%) = 0%
	THEN
		LIBR_DIGSR = ST%
		EXIT FUNCTION
	END IF

	!
	! Open APP_TEXT library file
	!
	ST% = LBR$OPEN(LR.INDEX%, LIB_NAME$, , ".TLB")

	IF (ST% AND 1%) = 0%
	THEN
		LIBR_DIGSR = ST%
		GOTO ExitProgram
	END IF

	ST% = LBR$LOOKUP_KEY(LR.INDEX%, KEY_NAME$, TXRFA)

	IF (ST% AND 1%) = 0%
	THEN
		LIBR_DIGSR = ST%
		GOTO ExitProgram
	END IF

100	!*******************************************************************
	! Read text in from input file
	!*******************************************************************

	TEXT$ = SPACE$(150%)
	ST% = LBR$GET_RECORD(LR.INDEX%, TEXT$)
	IF (ST% AND 1%) = 0%
	THEN
		GOSUB BreakText
		GOTO ExitProgram
	END IF

	INP$ = TRM$(TEXT$)

	!
	! Handle this line in whatever mode is necessary
	!
	IF LITERAL%
	THEN
		GOSUB LiteralLine
	ELSE
		IF LEFT(INP$, 1%) = "."
		THEN
			GOSUB CommandLine
		ELSE
			GOSUB TextLine
			GOSUB BreakText IF FILL_FLAG% = 0%
		END IF
	END IF

	GOTO 100

	!*******************************************************************
	! Exit the program
	!*******************************************************************

 ExitProgram:
	IF LIST_LIST% <> 0%
	THEN
		GOSUB BreakText
		THIS_TEXT$ = "** Missing .END LIST"
		GOSUB BuildText
		GOSUB BreakText
	END IF

	IF TABLE_TABLE% <> 0%
	THEN
		GOSUB BreakText
		THIS_TEXT$ = "** Missing .END TABLE"
		GOSUB BuildText
		GOSUB BreakText
	END IF

	IF NOTE_NOTE% <> 0%
	THEN
		GOSUB BreakText
		THIS_TEXT$ = "** Missing .END NOTE"
		GOSUB BuildText
		GOSUB BreakText
	END IF

	IF NOTE_QUOTE% <> 0%
	THEN
		GOSUB BreakText
		THIS_TEXT$ = "** Missing .END QUOTE"
		GOSUB BuildText
		GOSUB BreakText
	END IF

	IF NOTE_FOOTNOTE% <> 0%
	THEN
		GOSUB BreakText
		THIS_TEXT$ = "** Missing .END FOOTNOTE"
		GOSUB BuildText
		GOSUB BreakText
	END IF

	CODE$(0%) = NUM1$(CURR_LINE%)
	CODE$(CURR_LINE% + 1%) = ""

	!
	! TEXT file(close)
	!
	ST% = LBR$CLOSE(LR.INDEX%)

	EXIT FUNCTION

 TextLine:
	!**********************************************************************
	! Check a text line for any current DSR flags/commands
	!**********************************************************************

	FDL::FDLEN = 0%

	!
	! Check for Margins and indent
	!
	POSITION = 0%

	!
	! Append a space to front of line if appending to existing text
	!
	IF TOTAL_TEXT$ <> ""
	THEN
		THIS_TEXT$ = " "
		GOSUB BuildText
	END IF

 !	INP$ = EDIT$(INP$, 8% + 16% + 128%)
	INP$ = EDIT$(INP$, 128%)

 Text1:
	!
	! Search for junk in input string
	!
	SMG_STATUS% = STR$FIND_FIRST_SUBSTRING(INP$, POSITION, SUB_MATCH, &
		"*", "&", "^*", "\*", "^&", "\&", "_", "#", &
		"^~", "\~", "	") &
		UNLESS LITERAL%

	!
	! If none found, handle properly
	!
	IF POSITION = 0%
	THEN
		THIS_TEXT$ = INP$
		GOSUB BuildText
		GOTO EndText
	END IF

	!
	! Handle starting part
	!
	THIS_TEXT$ = LEFT(INP$, POSITION - 1%)
	GOSUB BuildText
	INP$ = RIGHT(INP$, POSITION + 1%)

	SELECT SUB_MATCH
	!
	! Bold char (*)
	!
	CASE 1%
		ONCE_ATTR% = ONCE_ATTR% OR SMG$M_BOLD

	!
	! Underline char (&)
	!
	CASE 2%
		ONCE_ATTR% = ONCE_ATTR% OR SMG$M_UNDERLINE

	!
	! Start bold (^*)
	!
	CASE 3%
		CUR_ATTR% = CUR_ATTR% OR SMG$M_BOLD
		INP$ = RIGHT(INP$, 2%)

	!
	! Stop  Bold (\*)
	!
	CASE 4%
		CUR_ATTR% = CUR_ATTR% AND (NOT SMG$M_BOLD)
		INP$ = RIGHT(INP$, 2%)

	!
	! Start Underline (^&)
	! Start italicized (^~)
	!
	CASE 5%, 9%
		CUR_ATTR% = CUR_ATTR% OR SMG$M_UNDERLINE
		INP$ = RIGHT(INP$, 2%)

	!
	! Stop  Underline (\&)
	! End italicized (\~)
	!
	CASE 6%, 10%
		CUR_ATTR% = CUR_ATTR% AND (NOT SMG$M_UNDERLINE)
		INP$ = RIGHT(INP$, 2%)

	!
	! Accept char (_)
	!
	CASE 7%
		THIS_TEXT$ = LEFT(INP$, 1%)
		GOSUB BuildText
		INP$ = RIGHT(INP$, 2%)

	!
	! Space char (#)
	!
	CASE 8%
		THIS_TEXT$ = " "
		GOSUB BuildText

	!
	! Tab Character
	!
	CASE 11%
		TEMP% = LEN(TOTAL_TEXT$) + INDENT% + LEFT_MAR%
		TEMP1% = 0%
		TEMP1% = TS%(I%) &
			IF TS%(I%) > TEMP% &
			FOR I% = 32% TO 1% STEP -1%
		TEMP1% = 0% IF TEMP1% > 80%

		THIS_TEXT$ = SPACE$(TEMP1% - TEMP%)
		GOSUB BuildText

	END SELECT

	GOTO Text1

 EndText:
	RETURN

	%PAGE

 CommandLine:
	!**********************************************************************
	! Identify the Command line and execute the line
	!**********************************************************************

	DSRCOM$ = EDIT$(RIGHT(INP$, 2%), &
		4% + 8% + 16% + 32% + 128% + 256%)

	DSR$(I%)= "" FOR I% = 0% TO 10%
	I% = 0%
	TEMP% = 1%
	DSRCOM1$ = ""

 GetCommand:
	IZ% = INSTR(1%, DSRCOM$, " ")
	IZ1% = INSTR(1%, DSRCOM$, ",")
	IZ% = IZ1% IF (IZ% = 0%) OR ((IZ1% > 0%) AND (IZ1% < IZ%))

	IF IZ%
	THEN
		DSR$(I%)= LEFT (DSRCOM$, IZ% - 1%)
		DSRCOM$ = RIGHT(DSRCOM$, IZ% + 1%)
		DSRCOM1$ = DSRCOM$ IF I% = 0%
		I% = I% + 1%
		GOTO GetCommand IF I% < 10%
	END IF

	DSR$(I%) = DSRCOM$ + ""

	SELECT DSR$(0%)
	!
	! .end
	!
	CASE "END"
		SELECT DSR$(1%)

		!
		! .end list
		!
		CASE "LIST"
			GOSUB BreakText
			LEFT_MAR% = LEFT_MAR% - 4%
			GOSUB BlankLine
			LIST_LIST% = LIST_LIST% - 1%

		!
		! .end note
		!
		CASE "NOTE"
			GOSUB BreakText
 !				LEFT_MAR% = LEFT_MAR% - 8%
 !				RIGHT_MAR% = RIGHT_MAR% + 8%
			GOSUB BlankLine
			NOTE_NOTE% = NOTE_NOTE% - 1%

		!
		! .end quote
		!
		CASE "QUOTE"
			GOSUB BreakText
			LEFT_MAR% = LEFT_MAR% - 8%
			RIGHT_MAR% = RIGHT_MAR% + 8%
			GOSUB BlankLine
			NOTE_QUOTE% = NOTE_QUOTE% - 1%

		!
		! .end footnote
		!
		CASE "FOOTNOTE"
			GOSUB BreakText
			LEFT_MAR% = LEFT_MAR% - 8%
			RIGHT_MAR% = RIGHT_MAR% + 8%
			GOSUB BlankLine
			NOTE_FOOTNOTE% = NOTE_FOOTNOTE% - 1%

		CASE "TABLE"
			GOSUB BreakText
			GOSUB BlankLine
			TABLE_TABLE% = TABLE_TABLE% - 1%
			TS%(I%) = OLDTS%(J%) FOR J% = 0% TO 32%
			LEFT_MAR% = LEFT_MAR% - 5%
			RIGHT_MAR% = RIGHT_MAR% + 5%

		CASE "OPTION"
			GOSUB BreakText
			GOSUB BlankLine
			OPTION.TABLE% = OPTION.TABLE% - 1%
			LEFT_MAR% = LEFT_MAR% - 5%
			TS%(I%) = OLDTS%(I%) FOR I% = 0% TO 32%

		CASE "EXAMPLE"
			GOSUB BreakText
			GOSUB BlankLine
			EXAMPLE_TABLE% = EXAMPLE_TABLE% - 1%
			LEFT_MAR% = LEFT_MAR% - 5%
			TS%(I%) = OLDTS%(I%) FOR I% = 0% TO 32%

		END SELECT

	!
	! .b
	! .blank
	!
	CASE "B", "BLANK"

6000		GOSUB BreakText
		WHEN ERROR IN
			BLANKL% = VAL%(DSR$(1%))
		USE
			IF ERR = 52%	! Illegal number
			THEN
				BLANKL% = 1%
				CONTINUE 6010
			END IF
			EXIT HANDLER
		END WHEN

		BLANKL% = 1% IF BLANKL% < 1%

6010		GOSUB BlankLine &
			FOR I% = 1% TO BLANKL%

	!
	! .br
	! .break
	!
	CASE "BR", "BREAK"
		GOSUB BreakText

	!
	! .i
	! .indent
	!
	CASE "I",	"INDENT"
		GOSUB BreakText
6020		WHEN ERROR IN
			INDENT% = VAL%(DSR$(1%))
		USE
			IF ERR = 52%	! Illegal number
			THEN
				INDENT% = 0%
				CONTINUE CommandLoop
			END IF
			EXIT HANDLER
		END WHEN

	!
	! .lm
	!
	CASE "LM"
		GOSUB BreakText
6030		SELECT LEFT(DSR$(1%), 1%)
		CASE "+", "-"
			LEFT_MAR% = LEFT_MAR% + VAL%(DSR$(1%))
		CASE ELSE
			LEFT_MAR% = VAL%(DSR$(1%))
		END SELECT

	!
	! .left
	!
	CASE "LEFT"
6040		SELECT DSR$(1%)
		!
		! .left margin
		!
		CASE "MARGIN"
			GOSUB BreakText

			SELECT LEFT(DSR$(2%), 1%)

			CASE "+", "-"
				LEFT_MAR% = &
					LEFT_MAR% + &
					VAL%(DSR$(2%))
			CASE ELSE
				LEFT_MAR% = &
					VAL%(DSR$(2%))
			END SELECT
		END SELECT

	!
	! .lt
	! .literal
	!
	CASE "LT", "LITERAL"
		GOSUB BreakText
		LITERAL% =  -1%

	!
	! .list
	!
	CASE "LIST"
		SELECT DSR$(1%)
		!
		! .list element
		!
		CASE "ELEMENT"
			GOSUB BreakText
			GOSUB BlankLine &
				FOR TEMP% = 1% TO LIST.SKIP%(LIST_LIST%)

			!
			! Place a star (or number) on the line
			!
			KEEP_ATTR% = CUR_ATTR%
			CUR_ATTR% = SMG$M_BOLD
			IF LIST_CHAR$(LIST_LIST%) = ""
			THEN
				THIS_TEXT$ = NUM1$(LIST_NUM%(LIST_LIST%))
				LIST_NUM%(LIST_LIST%) = &
					LIST_NUM%(LIST_LIST%) + 1%
			ELSE
				THIS_TEXT$ = LIST_CHAR$(LIST_LIST%)
			END IF
			INDENT% = -(LEN(THIS_TEXT$) + 1%)
			GOSUB BuildText
			CUR_ATTR% = KEEP_ATTR%

		!
		! .list *
		!
		CASE ELSE
			GOSUB BreakText
			GOSUB BeginList

		END SELECT

	!
	! .els
	!
	CASE "ELS"
		GOSUB BreakText
		LEFT_MAR% = LEFT_MAR% - 4%
		GOSUB BlankLine
		LIST_LIST% = LIST_LIST% - 1%

	!
	! .ls
	!
	CASE "LS"
		GOSUB BreakText
		GOSUB BeginList

	!
	! .le
	!
	CASE "LE"
		GOSUB BreakText
		GOSUB BlankLine &
			FOR TEMP% = 1% TO LIST.SKIP%(LIST_LIST%)

		!
		! Place a star (or number) on the line
		!
		KEEP_ATTR% = CUR_ATTR%
		CUR_ATTR% = SMG$M_BOLD
		IF LIST_CHAR$(LIST_LIST%) = ""
		THEN
			THIS_TEXT$ = NUM1$(LIST_NUM%(LIST_LIST%))
			LIST_NUM%(LIST_LIST%) = &
				LIST_NUM%(LIST_LIST%) + 1%
		ELSE
			THIS_TEXT$ = LIST_CHAR$(LIST_LIST%)
		END IF
		INDENT% = -(LEN(THIS_TEXT$) + 1%)
		GOSUB BuildText
		CUR_ATTR% = KEEP_ATTR%

	!
	! .nt
	! .note
	!
	CASE "NOTE", "NT"
		GOSUB BreakText
 !		LEFT_MAR% = LEFT_MAR% + 8%
 !		RIGHT_MAR% = RIGHT_MAR% - 8%
		GOSUB BlankLine
		DSRCOM1$ = "Note" IF DSRCOM1$ = ""
 !		TOTAL_TEXT$ = &
 !			SPACE$((RIGHT_MAR% - LEFT_MAR%) / 2% - &
 !			LEN(DSRCOM1$) / 2%) + DSRCOM1$
		THIS_TEXT$ = DSRCOM1$ + ": "
		GOSUB BuildText

 !		GOSUB BreakText
 !		GOSUB BlankLine
		NOTE_NOTE% = NOTE_NOTE% + 1%

	!
	! .qt
	! .quote
	!
	CASE "QUOTE", "QT"
		GOSUB BreakText
		LEFT_MAR% = LEFT_MAR% + 8%
		RIGHT_MAR% = RIGHT_MAR% - 8%
		TOTAL_TEXT$ = ""
		GOSUB BlankLine
		NOTE_QUOTE% = NOTE_QUOTE% + 1%

	!
	! .en
	!
	CASE "EN"
		GOSUB BreakText
 !		LEFT_MAR% = LEFT_MAR% - 8%
 !		RIGHT_MAR% = RIGHT_MAR% - 8%
		GOSUB BlankLine
		NOTE_NOTE% = NOTE_NOTE% - 1%

	!
	! .eqt
	!
	CASE "EQT"
		GOSUB BreakText
		LEFT_MAR% = LEFT_MAR% - 8%
		RIGHT_MAR% = RIGHT_MAR% - 8%
		GOSUB BlankLine
		NOTE_QUOTE% = NOTE_QUOTE% - 1%

	!
	! .fn
	! .footnote
	!
	CASE "FOOTNOTE", "FN"
		GOSUB BreakText
		LEFT_MAR% = LEFT_MAR% + 8%
		RIGHT_MAR% = RIGHT_MAR% - 8%
		GOSUB BlankLine
		DSRCOM1$ = "FOOTNOTE" IF DSRCOM1$ = ""
		TOTAL_TEXT$ = &
			SPACE$((RIGHT_MAR% - LEFT_MAR%) / 2% - &
			LEN(DSRCOM1$) / 2%) + DSRCOM1$
		GOSUB BreakText
		GOSUB BlankLine
		NOTE_FOOTNOTE% = NOTE_FOOTNOTE% + 1%

	!
	! .efn
	!
	CASE "EFN"
		GOSUB BreakText
		LEFT_MAR% = LEFT_MAR% - 8%
		RIGHT_MAR% = RIGHT_MAR% - 8%
		GOSUB BlankLine
		NOTE_FOOTNOTE% = NOTE_FOOTNOTE% - 1%

	!
	! .p
	! .paragraph
	!
	CASE "P", "PARAGRAPH"
		GOSUB BreakText
		GOSUB BlankLine
		IF DSR$(1%) <> ""
		THEN
			INDENT% = VAL%(DSR$(1%))
		ELSE
			INDENT% = 5%
		END IF

	!
	! .right
	!
	CASE "RIGHT"

		SELECT DSR$(1%)
		!
		! .right margin
		!
		CASE "MARGIN"
			GOSUB BreakText
			SELECT LEFT(DSR$(2%), 1%)

			CASE "+", "-"
				RIGHT_MAR% = &
					RIGHT_MAR% + &
					VAL%(DSR$(2%))

			CASE ELSE
				RIGHT_MAR% = VAL%(DSR$(2%))
			END SELECT
		END SELECT

	!
	! .rm
	!
	CASE "RM"
		GOSUB BreakText

		SELECT LEFT(DSR$(1%), 1%)

		CASE "+", "-"
			RIGHT_MAR% = RIGHT_MAR% + VAL%(DSR$(1%))

		CASE ELSE
			RIGHT_MAR% = VAL%(DSR$(1%))

		END SELECT

	!
	! .fill, .justify (assumed to be the same thing)
	!
	CASE "FILL", "F", "JUSTIFY", "J"
		GOSUB BreakText
		FILL_FLAG% = -1%

	!
	! No fill, no justify
	!
	CASE "NF", "NJ"
		GOSUB BreakText
		FILL_FLAG% = 0%

	!
	! Tab Stops
	!
6090	CASE "TS", "TABSTOP"
		FOR TEMP% = 1% TO I%
			TS%(TEMP%) = VAL%(DSR$(TEMP%))
		NEXT TEMP%
		TS%(I% + 1%) = 32767%

	!
	! Tables
	!
	CASE "TABLE"
		GOSUB BreakText
		GOSUB BlankLine
		TABLE_TABLE% = TABLE_TABLE% + 1%
		OLDTS%(J%) = TS%(J%) FOR J% = 0% TO 32%
		LEFT_MAR% = LEFT_MAR% + 5%
		RIGHT_MAR% = RIGHT_MAR% - 5%

		FOR TEMP% = 1% TO I%
			TS%(TEMP%) = VAL%(DSR$(TEMP%)) + LEFT_MAR%
		NEXT TEMP%
		TS%(I% + 1%) = 32767%

	CASE "ENDTABLE"
		GOSUB BreakText
		GOSUB BlankLine
		TABLE_TABLE% = TABLE_TABLE% - 1%
		TS%(J%) = OLDTS%(J%) FOR J% = 0% TO 32%
		LEFT_MAR% = LEFT_MAR% - 5%
		RIGHT_MAR% = RIGHT_MAR% + 5%

	CASE "TE", "TT"
		GOSUB BreakText

	!
	! Options
	!
	CASE "OPTION"
		GOSUB BreakText
		GOSUB BlankLine
		TOTAL_TEXT$ = "Option:"
		GOSUB BreakText
		OPTION.TABLE% = OPTION.TABLE% + 1%
		LEFT_MAR% = LEFT_MAR% + 5%
		OLDTS%(I%) = TS%(I%) FOR I% = 0% TO 32%
		TS%(I%) = 8% * I% FOR I% = 1% TO 31%
		TS%(32%) = 32767%

	CASE "ENDOPTION"
		GOSUB BreakText
		GOSUB BlankLine
		OPTION.TABLE% = OPTION.TABLE% - 1%
		LEFT_MAR% = LEFT_MAR% - 5%
		TS%(I%) = OLDTS%(I%) FOR I% = 0% TO 32%

	CASE "OE"
		GOSUB BreakText

	!
	! Examples
	!
	CASE "EXAMPLE"
		GOSUB BreakText
		GOSUB BlankLine
		TOTAL_TEXT$ = "Example:"
		GOSUB BreakText
		EXAMPLE_TABLE% = EXAMPLE_TABLE% + 1%
		LEFT_MAR% = LEFT_MAR% + 5%
		OLDTS%(I%)=TS%(I%) FOR I% = 0% TO 32%
		TS%(I%) = 8% * I% FOR I% = 1% TO 31%
		TS%(32%) = 32767%

	CASE "ENDEXAMPLE"
		GOSUB BreakText
		GOSUB BlankLine
		OPTION.TABLE% = OPTION.TABLE% - 1%
		LEFT_MAR% = LEFT_MAR% - 5%
		TS%(I%) = OLDTS%(I%) FOR I% = 0% TO 32%

	CASE "EE"
		GOSUB BreakText

	CASE "FIELD"
		LEFT_MAR% = LEFT_MAR% + 5%
		TS%(1%) = 55%
		TS%(2%) = 32767%
		INDENT% = -5%
	END SELECT

 CommandLoop:

	RETURN

 BeginList:
8000	LIST_LIST% = LIST_LIST% + 1%
	LIST_CHAR$(LIST_LIST%) = ""
	LIST_NUM%(LIST_LIST%) = 1%
	LIST.SKIP%(LIST_LIST%) = 1%
	LEFT_MAR% = LEFT_MAR% + 4%

	DSR$(1%) = EDIT$(DSR$(1%), 8% + 128%)
	DSR$(2%) = EDIT$(DSR$(2%), 8% + 128%)

	IF (LEFT(DSR$(1%), 1%) <> '"') AND (DSR$(1%) <> "")
	THEN
		TEMP% = 2%
		WHEN ERROR IN
			LIST.SKIP%(LIST_LIST%) = VAL%(DSR$(1%))
		USE
			CONTINUE 8090 IF ERR = 52%
			EXIT HANDLER
		END WHEN
	ELSE
		TEMP% = 1%
	END IF

	IF LEFT(DSR$(TEMP%), 1%) = '"'
	THEN
		TEMP1% = INSTR(2%, DSR$(TEMP%), '"')
		LIST_CHAR$(LIST_LIST%) = SEG$(DSR$(TEMP%), 2%, TEMP1% - 1%)
	END IF

8090	RETURN

	%PAGE

8400	!*******************************************************************
	! Handle literal text
	!*******************************************************************

 LiteralLine:

	TEMP$ = EDIT$(INP$, -1%)

	!
	! Handle end of literal text
	!
	IF (TEMP$ = ".EL") OR (TEMP$ = ".ENDLITERAL")
	THEN
		LITERAL% = 0%
		GOTO 8490
	END IF

	!
	! Insert literal text (we go directly into the output array
	! because it is the easiest way to go)
	!
	FDL::FDLEN = 2%
	CURR_LINE% = CURR_LINE% + 1%
	CODE$(CURR_LINE%) = SPACE$(LEFT_MAR%) + INP$ + FDL::FDL

8490	RETURN

	%PAGE

9000	!*******************************************************************
	! Add text to current build buffer
	!
	! Adds THIS_TEXT$ to the output text, while keeping the attributes
	! set at this point.
	!*******************************************************************

 BuildText:
	!
	! Skip blank text
	!
	GOTO 9090 IF THIS_TEXT$ == ""

	!
	! Handle one time only attributes
	!
	IF (ONCE_ATTR% <> 0%)
	THEN
		IF (ONCE_ATTR% OR CUR_ATTR%) <> ARY_ATTR%(ARY_ATTR%)
		THEN
			ARY_ATTR% = ARY_ATTR% + 1% &
				IF (ARY_ATTP%(ARY_ATTR%) <> LEN(TOTAL_TEXT$) + 1%) OR &
				(ARY_ATTR% = 0%)
			ARY_ATTR%(ARY_ATTR%) = CUR_ATTR% OR ONCE_ATTR%
			ARY_ATTP%(ARY_ATTR%) = LEN(TOTAL_TEXT$) + 1%
		END IF
		TOTAL_TEXT$ = TOTAL_TEXT$ + LEFT(THIS_TEXT$, 1%)
		THIS_TEXT$ = RIGHT(THIS_TEXT$, 2%)
		ONCE_ATTR% = 0%
	END IF

	!
	! Handle Any new attributes
	!
	IF (CUR_ATTR% <> ARY_ATTR%(ARY_ATTR%)) OR (ARY_ATTR% = 0%)
	THEN
		ARY_ATTR% = ARY_ATTR% + 1% &
			IF (ARY_ATTP%(ARY_ATTR%) <> LEN(TOTAL_TEXT$) + 1%) OR &
			(ARY_ATTR% = 0%)
		ARY_ATTR%(ARY_ATTR%) = CUR_ATTR%
		ARY_ATTP%(ARY_ATTR%) = LEN(TOTAL_TEXT$) + 1%
	END IF

	TOTAL_TEXT$ = TOTAL_TEXT$ + THIS_TEXT$

9090	RETURN

	%PAGE

9100	!*******************************************************************
	! Break text - write out all of the line into the output
	! area.
	!
	! If input is blank, there is no output.
	!*******************************************************************

 BreakText:

	!
	! Init necessary information
	!
	TOTAL_TEXT$ = TRM$(TOTAL_TEXT$)
	TEMP_USED% = 1%

	!
	! Force ending to attribute list
	!
	ARY_ATTR% = ARY_ATTR% + 1%
	ARY_ATTR%(ARY_ATTR%) = 0%
	ARY_ATTP%(ARY_ATTR%) = LEN(TOTAL_TEXT$) + 1%

	WHILE (TOTAL_TEXT$ <> "")

		!
		! Maximum width
		!
		TEMP_LENGTH% = RIGHT_MAR% - LEFT_MAR% - INDENT%
		TEMP_LENGTH% = 40% IF TEMP_LENGTH% < 5%

		!
		! Search for previous space (if possible)
		!
		IF LEN(TOTAL_TEXT$) < TEMP_LENGTH%
		THEN
			PART_TEXT$ = TOTAL_TEXT$
			TOTAL_TEXT$ = ""
			TEMP_XSKIP% = LEN(TOTAL_TEXT$)
		ELSE
			TEMP_I% = TEMP_LENGTH% + 1%
			TEMP_I% = TEMP_I% - 1% &
				UNTIL (TEMP_I% = 0%) OR (MID(TOTAL_TEXT$, TEMP_I%, 1%) = " ")

			IF TEMP_I% = 0%
			THEN
				PART_TEXT$ = LEFT(TOTAL_TEXT$, TEMP_LENGTH%)
				TOTAL_TEXT$ = RIGHT(TOTAL_TEXT$, TEMP_LENGTH% + 1%)
				TEMP_XSKIP% = TEMP_LENGTH%
			ELSE
				PART_TEXT$ = LEFT(TOTAL_TEXT$, TEMP_I% - 1%)
				TOTAL_TEXT$ = RIGHT(TOTAL_TEXT$, TEMP_I% + 1%)
				TEMP_XSKIP% = TEMP_I%
			END IF
		END IF

		!
		! Handle text.  First create the attributes for this line
		!
		TEMP_ATTR$ = ""
		FOR TEMP_I% = 1% TO ARY_ATTR%

			!
			! If attribute is in range, add it to the list
			!
			TEMP_BEG% = ARY_ATTP%(TEMP_I% - 1%) - TEMP_USED% + 1%
			TEMP_BEG% = 1% IF TEMP_BEG% < 1%
			TEMP_END% = ARY_ATTP%(TEMP_I%) - TEMP_USED%
			TEMP_END% = LEN(PART_TEXT$) IF TEMP_END% > LEN(PART_TEXT$)
			IF TEMP_END% >= TEMP_BEG%
			THEN
				FD::CBEG = TEMP_BEG% + LEFT_MAR% + INDENT%
				FD::CLEN = TEMP_END% - TEMP_BEG% + 1%
				FD::CATR = ARY_ATTR%(TEMP_I% - 1%)
				TEMP_ATTR$ = TEMP_ATTR$ + FD::FD
			END IF

		NEXT TEMP_I%

		FDL::FDLEN = 2% + LEN(TEMP_ATTR$)
		CURR_LINE% = CURR_LINE% + 1%
		CODE$(CURR_LINE%) = SPACE$(LEFT_MAR% + INDENT%) + &
			PART_TEXT$ + TEMP_ATTR$ + FDL::FDL
		INDENT% = 0%
		TEMP_USED% = TEMP_USED% + TEMP_XSKIP%

	NEXT

	ARY_ATTR% = 0%

	RETURN

9200	!*******************************************************************
	! Blank line
	!
	! Insert a blank line into the output
	!*******************************************************************

 BlankLine:
	!
	! Handle text
	!
	FDL::FDLEN = 2%
	CURR_LINE% = CURR_LINE% + 1%
	CODE$(CURR_LINE%) = "" + FDL::FDL

	RETURN

19000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	SELECT ERL
	CASE 6030%, 6040%	! Left Margin
		IF ERR = 52%	! Illegal number
		THEN
			LEFT_MAR% = 0%
			GOSUB BreakText
			TOTAL_TEXT$ = " %% Illegal Number %%"
			GOSUB BreakText
			RESUME CommandLoop
		END IF

	CASE 6090%		! Tab Stops
		IF ERR = 52%	! Illegal number
		THEN
			TS%(I%) = 8% * I% FOR I% = 1% TO 31%
			TS%(32%) = 32767%
			GOSUB BreakText
			TOTAL_TEXT$ = " %% Illegal Number %%"
			GOSUB BreakText
			RESUME CommandLoop
		END IF

	END SELECT

	ON ERROR GOTO 0

	END FUNCTION
