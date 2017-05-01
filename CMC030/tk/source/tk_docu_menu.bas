1	%TITLE "Document a Menu"
	%SBTTL "TK_DOCU_MENU"
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
	!	This program reads a menu source file, and from
	!	that creates a documentation book.
	!
	! Index:
	!	.x Documentation
	!	.x Menu
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
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_DOCU_MENU
	!	$ LINK/EXE=TK_EXE: TK_DOCU_MENU, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE TK_DOCU_MENU.OBJ;*
	!
	! Author:
	!
	!	10/26/87 - Kevin Handy
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	11/20/95 - Kevin Handy
	!		Lose /NOWARN on compile.
	!		Lose unnecessary external definitions.
	!
	!	10/29/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/08/99 - Kevin Handy
	!		Use BASIC$STARLET for STR$ routines
	!
	!	06/09/99 - Kevin Handy
	!		Lose line 7000 (Dead code)
	!
	!	11/27/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:LIBRARY.COM"

	%INCLUDE "STR$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	!
	! Declare variables
	!
	MAP (LBR_JUNKJUNK) TEXT$ = 132%		! Read buffer
	DECLARE RFA TXRFA

	!
	! Create array to hold legal commands
	!
	DECLARE INTEGER CONSTANT MAX_COMMAND = 100%
	DIM COMMAND_LIST$(MAX_COMMAND),	COMMAND_LIST%(MAX_COMMAND)

	%PAGE

1000	!*******************************************************************
	! Allocate channels
	!*******************************************************************

	SOURCE.CH% = 5%
	LATEX.CH% = 6%

	!*******************************************************************
	! Initilize for runoff conversion
	!*******************************************************************

	!
	! Initialize command list
	!
	GOSUB LoadCommands

	%PAGE


1200	!*******************************************************************
	! Get menu source file
	!*******************************************************************

	LINPUT "Name of menu file <.MNU> "; SOURCE.NAME$

	OPEN SOURCE.NAME$ FOR INPUT AS FILE SOURCE.CH%, &
		ACCESS READ, &
		ALLOW MODIFY, &
		DEFAULTNAME ".MNU"

	FIRST_UNDERSCORE% = INSTR(1%, SOURCE.NAME$, "_")
	IF FIRST_UNDERSCORE% = 0%
	THEN
		MENU_SYSTEM$ = "UTL"
	ELSE
		MENU_SYSTEM$ = LEFT(SOURCE.NAME$, FIRST_UNDERSCORE% - 1%)
	END IF

	LINPUT "Title for cover "; LATEX.TITLE$

1400	!*******************************************************************
	! Get output file name
	!*******************************************************************

	LINPUT "Name of output file <.TEX> "; LATEX.NAME$

	OPEN LATEX.NAME$ FOR OUTPUT AS FILE LATEX.CH%, &
		DEFAULTNAME ".TEX", &
		RECORDSIZE 255%



1500	!
	! Put a header on the output file
	!
	PRINT #LATEX.CH%, "\documentstyle{book}"
	PRINT #LATEX.CH%, "\begin{document}"
	PRINT #LATEX.CH%, "\title{" + FNTEX$(LATEX.TITLE$) + "}"
	PRINT #LATEX.CH%, "\author{Created by TK\_DOCU\_MENU}"
	PRINT #LATEX.CH%, "\date{" + PRNT_FANCYDATE(DATE_TODAY) + "}"
	PRINT #LATEX.CH%, "\maketitle"
	PRINT #LATEX.CH%, "\pagenumbering{roman}"
	PRINT #LATEX.CH%, "\tableofcontents"
	PRINT #LATEX.CH%, "\setcounter {tocdepth}{6}"

2000	!*******************************************************************
	! Scan through file, handling one line at a time
	!*******************************************************************

	FLAG_CASE% = 0%		! 0 = Mixed case, 1 = Upper Case,
				! 2 = Lower Case

	!
	! Read in one line
	!
	WHEN ERROR IN
		LINPUT #SOURCE.CH%, INLINE$
	USE
		CONTINUE 2100 IF ERR = 11%
		EXIT HANDLER
	END WHEN

	GOTO 2000 IF INLINE$ = ""
	PRINT INLINE$

	GOSUB ParseLine

	GOTO 2000

2100	!*******************************************************************
	! Finish up file
	!*******************************************************************

	PRINT #LATEX.CH%, "\end{document}"

	CLOSE LATEX.CH%
	CLOSE SOURCE.CH%

	GOTO 32767

2200	!*******************************************************************
	! Parse one line
	!*******************************************************************
 ParseLine:

	!
	! Pull off dots
	!
	DOT_COUNTER% = 0%
	WHILE MID(INLINE$, DOT_COUNTER% + 1%, 1%) = "."
		DOT_COUNTER% = DOT_COUNTER% + 1%
	NEXT
	INLINE$ = RIGHT(INLINE$, DOT_COUNTER% + 1%)

	!
	! Pull apart line
	!
	FIRST_SPACE% = INSTR(1%, INLINE$, " ")

	FIRST_ARROW% = INSTR(FIRST_SPACE% + 1%, INLINE$, ">")
	FIRST_ARROW% = INSTR(FIRST_SPACE% + 1%, INLINE$, "<") &
		IF FIRST_ARROW% = 0%

	IF FIRST_ARROW% = 0%
	THEN
		PRINT "Unable to parse input line from menu '"; INLINE$; "'"
		GOTO 2000
	END IF

	SECOND_ARROW% = INSTR(FIRST_ARROW% + 1%, INLINE$, ">")
	SECOND_ARROW% = INSTR(FIRST_ARROW% + 1%, INLINE$, "<") &
		IF SECOND_ARROW% = 0%

	IF SECOND_ARROW% = 0%
	THEN
		PRINT "Unable to parse input line from menu '"; INLINE$; "'"
		GOTO 2000
	END IF

	!
	! Seperate parts
	!
	MENU_OPTION$ = LEFT(INLINE$, FIRST_SPACE% - 1%)
	MENU_DESCR$ = SEG$(INLINE$, FIRST_SPACE% + 1%, FIRST_ARROW% - 2%)
	MENU_FILE$ = RIGHT(INLINE$, SECOND_ARROW% + 1%)
	IF MID(INLINE$, FIRST_ARROW%, 1%) = ">"
	THEN
		MENU_TYPE$ = "P"
	ELSE
		MENU_TYPE$ = "H"
	END IF

	!
	! Yank off system name if possible
	!
	MENU_SYSTEM$ = MENU_OPTION$ IF DOT_COUNTER% = 0%

	!
	! Dump out proper title for this dot level
	!
	SELECT DOT_COUNTER%

	!
	! Start of the menu
	!
	CASE 0%
		PRINT #LATEX.CH%, "\cleardoublepage"
		PRINT #LATEX.CH%, "\pagenumbering{arabic}"
		PRINT #LATEX.CH%, "\vspace*{50pt}"
		PRINT #LATEX.CH%, "{\parindent 0pt \raggedright \Huge \bf"
		PRINT #LATEX.CH%, FNTEX$(MENU_OPTION$ + " - " + &
			MENU_DESCR$)
		PRINT #LATEX.CH%, "\par \nobreak \vskip 40pt}"
		PRINT #LATEX.CH%, "\addcontentsline{toc}{chapter}{" + &
			FNTEX$(MENU_OPTION$ + " - " + &
			MENU_DESCR$) + "}"

	!
	! Start of a chapter
	!
	CASE 1%
		PRINT #LATEX.CH%, "\chapter {" + &
			FNTEX$(MENU_OPTION$ + " - " + &
			MENU_DESCR$) + "}"

	!
	! Start of a section
	!
	CASE 2%
		PRINT #LATEX.CH%, "\section {" + &
			FNTEX$(MENU_OPTION$ + " - " + &
			MENU_DESCR$) + "}"

	!
	! Start of a subsection
	!
	CASE 3%
		PRINT #LATEX.CH%, "\subsection {" + &
			FNTEX$(MENU_OPTION$ + " - " + &
			MENU_DESCR$) + "}"

	!
	! Start of a subsubsection
	!
	CASE 4%
		PRINT #LATEX.CH%, "\subsubsection {" + &
			FNTEX$(MENU_OPTION$ + " - " + &
			MENU_DESCR$) + "}"

	!
	! Start of a paragraph
	!
	CASE 5%
		PRINT #LATEX.CH%, "\paragraph {" + &
			FNTEX$(MENU_OPTION$ + " - " + &
			MENU_DESCR$) + "}"

	!
	! Start of a subparagraph
	!
	CASE ELSE
		PRINT #LATEX.CH%, "\subparagraph {" + &
			FNTEX$(MENU_OPTION$ + " - " + &
			MENU_DESCR$) + "}"

	END SELECT

	GOSUB Helpfile

	RETURN

	%PAGE

2400	!*******************************************************************
	! Handle help file for this line
	!*******************************************************************
 HelpFile:

	CALL HELP_MENUHELPKEY(MENU_FILE$, &
		MENU_TYPE$, &
		MENU_SYSTEM$, &
		TLB_IDENT$, &
		TLB_PROGRAM$, &
		TLB_ITEM$, &
		TLB_DEVICE$, &
		1%)

	IF MENU_SYSTEM$ = ""
	THEN
		LIB_NAME$ = "HELP_DEFAULT"
	ELSE
		LIB_NAME$ = "HELP_" + MENU_SYSTEM$
	END IF

	KEY_NAME$ = TRM$(TLB_IDENT$) + "$" + &
		TRM$(TLB_PROGRAM$) + "$" + TRM$(TLB_ITEM$)

	!
	! Set up the control structure if necessary
	!
	ST% = LBR$INI_CONTROL(LR.INDEX%, LBR$C_READ)

	IF (ST% AND 1%) = 0%
	THEN
		PRINT "Unable to initilize library"
		RETURN
	END IF

	!
	! Open the library function
	!
	ST% = LBR$OPEN(LR.INDEX%, LIB_NAME$, , "REF:.TLB")

	IF (ST% AND 1%) = 0%
	THEN
		RETURN
	END IF

	!
	! Search for key in file
	!
	ST% = LBR$LOOKUP_KEY(LR.INDEX%, KEY_NAME$, TXRFA)

	IF (ST% AND 1%) = 0%
	THEN
		GOTO CloseLibrary
	END IF

	!
	! Copy over text
	!
 Loop:
	TEXT$ = ""
	ST% = LBR$GET_RECORD(LR.INDEX%, TEXT$)

	IF (ST% AND 1%) = 1%
	THEN
		INLINE$ = TRM$(TEXT$)
		GOSUB 6000
		GOTO Loop
	END IF

	!
	! Close library file
	!
 CloseLibrary:
	ST% = LBR$CLOSE(LR.INDEX%)

	RETURN

	%PAGE

6000	!*******************************************************************
	! Read in a line and convert
	!*******************************************************************

	!
	! Handle end of a footnote
	!
	IF FLAG_FOOTNOTE%
	THEN
		IF LEFT(INLINE$, 1%) = "!"
		THEN
			PRINT #LATEX.CH%, "}"
			FLAG_FOOTNOTE% = 0%
			RETURN
		END IF
	END IF


6010	!
	! Handle dot commands on line
	!
	IF LEFT(INLINE$, 1%) = "."
	THEN
		!
		! Pull off one dor command
		!
		GOSUB ProcessDot
		!
		! Skip line if it there is nothing else on it
		!
		RETURN IF INLINE$ = ""
		!
		! Try for more dot commands
		!
		GOTO 6010
	END IF

	!
	! Everything else must be regular text
	!
	GOSUB ProcessText

	IF FLAG_FILL%
	THEN
		PRINT #LATEX.CH%, ""
	ELSE
		PRINT #LATEX.CH%
	END IF

	RETURN

	%PAGE

 !7000
	!*******************************************************************
	! Finish up output
	!*******************************************************************
 !
 !	PRINT #LATEX.CH%, "\end{document}"
 !
 !	GOTO 32767

	%PAGE

8000	!*******************************************************************
	! Process dot commands
	!*******************************************************************
 ProcessDot:

	!
	! Hang onto just one command
	!
	TEMP% = INSTR(1%, INLINE$, ";")
	IF TEMP%
	THEN
		DOT_COMMAND$ = EDIT$(LEFT(INLINE$, TEMP% - 1%), 32% + 128%)
		INLINE$ = RIGHT(TEMP$, TEMP% + 1%)
	ELSE
		DOT_COMMAND$ = EDIT$(INLINE$, 32% + 128%)
		INLINE$ = ""
	END IF

	!
	! Scan command for command characters
	!
	FOR DOT_COMMAND% = 1% TO COMMAND_LIST%

		GOTO 8100 IF LEFT(DOT_COMMAND$, &
			LEN(COMMAND_LIST$(DOT_COMMAND%))) = &
			COMMAND_LIST$(DOT_COMMAND%)

	NEXT DOT_COMMAND%

	PRINT "*** Undefined dot command: "; DOT_COMMAND$
	PRINT #LATEX.CH%, "% " + DOT_COMMAND$

	RETURN

8100	!
	! We have a command, process it
	!
	DOT_WIDTH% = LEN(COMMAND_LIST$(DOT_COMMAND%)) + 1%
	SELECT COMMAND_LIST%(DOT_COMMAND%)

	!
	! Break
	!
	CASE 1%
		PRINT #LATEX.CH%, ""

	!
	! Comment
	!
	CASE 2%
		PRINT #LATEX.CH%, "%"; &
			RIGHT(DOT_COMMAND$, DOT_WIDTH%); INLINE$
		INLINE$ = ""

	!
	! Upper case
	!
	CASE 3%
		FLAG_CASE% = 1%

	!
	! Lower case
	!
	CASE 4%
		FLAG_CASE% = 2%

	!
	! Page
	!
	CASE 5%
		PRINT #LATEX.CH%, "\newpage"

	!
	! Skip
	!
	CASE 6%
		TEMP% = VAL%(RIGHT(DOT_COMMAND$, DOT_WIDTH%))
		IF TEMP% = 1% OR TEMP% = 0%
		THEN
			PRINT #LATEX.CH%, ""
		ELSE
			PRINT #LATEX.CH%, "\vspace{"; &
				LEFT(NUM1$(TEMP% / 6.0), 6%); "in}"
		END IF

	!
	! Figure
	!
	CASE 7%
		TEMP% = VAL%(RIGHT(DOT_COMMAND$, DOT_WIDTH%))
		PRINT #LATEX.CH%, "\begin{figure}\vspace{" + &
			LEFT(NUM1$(TEMP% / 6.0), 6%) + &
			"in}\end{figure}"

	!
	! Indent
	!
	CASE 8%
		TEMP% = VAL%(RIGHT(DOT_COMMAND$, DOT_WIDTH%))
		PRINT #LATEX.CH%, ""
		PRINT #LATEX.CH%, "\hspace{" + &
			LEFT(NUM1$(TEMP% / 10.0), 6%) + "in}"

	!
	! Center
	!
	CASE 9%
		PRINT #LATEX.CH%, "\begin{center}"
		GOSUB ProcessText
		PRINT #LATEX.CH%
		PRINT #LATEX.CH%, "\end{center}"
		INLINE$ = ""

	!
	! Footnote
	!
	CASE 10%
		PRINT #LATEX.CH%, "\footnote{";
		FLAG_FOOTNOTE% = -1%

	!
	! Begin note
	!
	CASE 11%
		PRINT #LATEX.CH%, "\begin{quotation}"
		PRINT #LATEX.CH%, "\begin{center}NOTE\end{center}"
		INLINE$ = RIGHT(DOT_COMMAND$, DOT_WIDTH%) + INLINE$

	!
	! End note
	!
	CASE 12%
		PRINT #LATEX.CH%, "\end{quotation}"

	!
	! Begin List
	!
	CASE 13%
		TEMP$ = RIGHT(DOT_COMMAND$, DOT_WIDTH%)
		IF LEFT(TEMP$, 1%) = '"'
		THEN
			PRINT #LATEX.CH%, "\begin{itemize}"
			FLAG_LIST$ = FLAG_LIST$ + "*"
		ELSE
			PRINT #LATEX.CH%, "\begin{enumerate}"
			FLAG_LIST$ = FLAG_LIST$ + "N"
		END IF

	!
	! List element
	!
	CASE 14%
		PRINT #LATEX.CH%, "\item ";

	!
	! End list
	!
	CASE 15%
		TEMP$ = RIGHT(FLAG_LIST$, LEN(FLAG_LIST$))
		FLAG_LIST$ = LEFT(FLAG_LIST$, LEN(FLAG_LIST$) - 1%)

		IF TEMP$ = "*"
		THEN
			PRINT #LATEX.CH%, "\end{itemize}"
		ELSE
			PRINT #LATEX.CH%, "\end{enumerate}"
		END IF

	!
	! Fill
	!
	CASE 16%
		FLAG_FILL% = 0%

	!
	! Nofill
	!
	CASE 17%
		FLAG_FILL% = 1%

	!
	! Index
	!
	CASE 19%
		PRINT #LATEX.CH%, "\index{";

		DOT_TEMP$ = INLINE$
		INLINE$ = RIGHT(DOT_COMMAND$, DOT_WIDTH%)
		IF INLINE$ = ""
		THEN
			INLINE$ = DOT_TEMP$
			DOT_TEMP$ = ""
		END IF

		GOSUB ProcessText

		PRINT #LATEX.CH%, "}"

		INLINE$ = DOT_TEMP$

	!
	! Test page
	!
	CASE 20%
		PRINT #LATEX.CH%, "\pagebreak[1]"

	!
	! Paragraph
	!
	CASE 26%
		PRINT #LATEX.CH%
		PRINT #LATEX.CH%

	END SELECT

8900	RETURN

	%PAGE

9000	!*******************************************************************
	! Process text
	!*******************************************************************
 ProcessText:

	INLINE% = 1%			! Pointer into line
	FLAG_CASE_TEMP% = 0%		! Temporary case flag
	FLAG_BOLD_TEMP% = 0%		! Temporary Bold flag
	OUT_LINE$ = ""			! Output text

9010	ONE_CHAR$ = MID(INLINE$, INLINE%, 1%)

	SELECT ONE_CHAR$

	!
	! Underline - Quote character
	!
	CASE "_"

		INLINE% = INLINE% + 1%
		OUT_LINE$ = OUT_LINE$ + FNFIX_CHAR$(MID(INLINE$, INLINE%, 1%))

	!
	! Quoted space
	!
	CASE "#"
		OUT_LINE$ = OUT_LINE$ + "\ "

	!
	! Star - Bold character
	!
	CASE "*"
		FLAG_BOLD_TEMP% = -1%

	!
	! Uparrow
	!
	CASE "^"
		TWO_CHAR$ = MID(INLINE$, INLINE% + 1%, 1%)

		SELECT TWO_CHAR$

		!
		! Another uparrow - uppercase all
		!
		CASE "^"
			INLINE% = INLINE% + 1%
			FLAG_CASE% = 1%

		!
		! A astrix - Bold all
		!
		CASE "*"
			INLINE% = INLINE% + 1%
			IF MID(INLINE$, INLINE% + 1%, 1%) = " "
			THEN
				OUT_LINE$ = OUT_LINE$ + " \bf "
				INLINE% = INLINE% + 1%
			ELSE
				OUT_LINE$ = OUT_LINE$ + "\bf "
			END IF

		!
		! A ampersand - Underline all
		!
		CASE "&"
			INLINE% = INLINE% + 1%
			OUT_LINE$ = OUT_LINE$ + "\underline{"

		!
		! Otherwise, uppercase next character
		!
		CASE ELSE
			FLAG_CASE_TEMP% = 1%

		END SELECT

	!
	! Back-slash
	!
	CASE "\"
		TWO_CHAR$ = MID(INLINE$, INLINE% + 1%, 1%)

		SELECT TWO_CHAR$

		!
		! Another back-slash - Lowercase all
		!
		CASE "\"
			INLINE% = INLINE% + 1%
			FLAG_CASE% = 2%

		!
		! A astrix - unBold all
		!
		CASE "*"
			INLINE% = INLINE% + 1%
			IF MID(INLINE$, INLINE% + 1%, 1%) = " "
			THEN
				OUT_LINE$ = OUT_LINE$ + " \rm "
				INLINE% = INLINE% + 1%
			ELSE
				OUT_LINE$ = OUT_LINE$ + "\rm "
			END IF

		!
		! A ampersand - Stop Underline all
		!
		CASE "&"
			INLINE% = INLINE% + 1%
			OUT_LINE$ = OUT_LINE$ + "}"

		!
		! Otherwise, Lowercase next character
		!
		CASE ELSE
			FLAG_CASE_TEMP% = 2%

		END SELECT

	!
	! All other characters must be letters
	!
	CASE ELSE
		!
		! Dump out character
		!
		OUT_LINE$ = OUT_LINE$ + FNFIX_CHAR$(ONE_CHAR$)

	END SELECT

	INLINE% = INLINE% + 1%

	GOTO 9010 IF INLINE% <= LEN(INLINE$)

	PRINT #LATEX.CH%, OUT_LINE$;

	RETURN

	%PAGE

15000	!*******************************************************************
	! Data statement defining commands that are understood
	!*******************************************************************

 LoadCommands:

	!
	! Load in data statement
	!
	COMMAND_LIST% = 0%

	READ NVALUE%, NVALUE$

	WHILE (NVALUE% <> 0%)

		COMMAND_LIST% = COMMAND_LIST% + 1%
		COMMAND_LIST$(COMMAND_LIST%) = NVALUE$
		COMMAND_LIST%(COMMAND_LIST%) = NVALUE%

		READ NVALUE%, NVALUE$

	NEXT

	RETURN

	!
	! First element identifies the operation, second is the text
	! for the command.
	!
	DATA	1,	".BREAK ", &
		1,	".BR ", &
		2,	".COMMENT ", &
		3,	".UC ", &
		3,	".UPPER CASE ", &
		3,	".UPPERCASE ", &
		4,	".LC ", &
		4,	".LOWER CASE ", &
		4,	".LOWERCASE ", &
		5,	".PAGE ", &
		5,	".PG ", &
		6,	".SKIP ", &
		6,	".S ", &
		6,	".BLANK ", &
		6,	".B ", &
		7,	".FIGURE ", &
		7,	".FG ", &
		8,	".INDENT ", &
		8,	".I ", &
		9,	".CENTER ", &
		9,	".CENTRE ", &
		9,	".C ", &
		10,	".FOOTNOTE ", &
		10,	".FN ", &
		11,	".NOTE ", &
		11,	".NT ", &
		12,	".END NOTE ", &
		12,	".ENDNOTE ", &
		12,	".EN ", &
		14,	".LIST ELEMENT ", &
		14,	".LISTELEMENT ", &
		14,	".LE ", &
		13,	".LIST ", &
		13,	".LS ", &
		15,	".END LIST ", &
		15,	".ENDLIST ", &
		15,	".ELS ", &
		16,	".FILL ", &
		16,	".F ", &
		17,	".NO FILL ", &
		17,	".NOFILL ", &
		17,	".NF", &
		19,	".INDEX ", &
		19,	".X ", &
		20,	".TEST PAGE ", &
		20,	".TESTPAGE ", &
		20,	".TP ", &
		26,	".P ", &
		26,	".PARAGRAPH ", &
		0,	""

	%PAGE

18000	!*******************************************************************
	! Function to convert text into a TeXable format
	!*******************************************************************

	DEF FNTEX$(INLINE$)

		TEMP_IN$ = INLINE$
		TEMP_IN% = 0%
 TexLoop:
		TEMP_OLD% = TEMP_IN% + 1%
		TEMP_IN% = STR$FIND_FIRST_IN_SET(RIGHT(TEMP_IN$, TEMP_OLD%), &
			"#$%&_{}<>~^")

		IF TEMP_IN%
		THEN
			TEMP_IN% = TEMP_IN% + TEMP_OLD% - 1%

			SELECT MID(TEMP_IN$, TEMP_IN%, 1%)

			CASE "~", "^", "\"
				TEMP_IN$ = LEFT(TEMP_IN$, TEMP_IN% - 1%) + &
					"\verb+" + &
					MID(TEMP_IN$, TEMP_IN%, 1%) + "+" + &
					RIGHT(TEMP_IN$, TEMP_IN% + 1%)
				TEMP_IN% = TEMP_IN% + 7%

			CASE "<", ">"
				TEMP_IN$ = LEFT(TEMP_IN$, TEMP_IN% - 1%) + &
					"$" + &
					MID(TEMP_IN$, TEMP_IN%, 1%) + "$" + &
					RIGHT(TEMP_IN$, TEMP_IN% + 1%)
				TEMP_IN% = TEMP_IN% + 2%


			CASE ELSE
				TEMP_IN$ = LEFT(TEMP_IN$, TEMP_IN% - 1%) + &
					"\" + &
					RIGHT(TEMP_IN$, TEMP_IN%)
				TEMP_IN% = TEMP_IN% + 1%
			END SELECT

			GOTO TexLoop
		END IF

		FNTEX$ = TEMP_IN$
	FNEND

	!*******************************************************************
	! Function to do case conversions
	!*******************************************************************

	DEF FNSET_CASE$(A1$, A%)

		A$ = A1$

		SELECT A%

		!
		! Upper case
		!
		CASE 1%
			A$ = EDIT$(A$, 32%)

		!
		! Lower case
		!
		CASE 2%
			A$ = CHR$(ASCII(A$) + 32%) &
				IF (A$ >= "A") AND (A$ <= "Z")

		END SELECT

		FNSET_CASE$ = A$

	FNEND

	%PAGE

	!*******************************************************************
	! Function to process on one character
	!*******************************************************************

	DEF* FNFIX_CHAR$(A1$)

		!
		! Put character into proper case
		!
		A$ = FNSET_CASE$(FNSET_CASE$(A1$, FLAG_CASE%), &
			FLAG_CASE_TEMP%)

		!
		! Fix character if it is one of the special ones
		!
		SELECT A$

		CASE "$", "&", "%", "#", "_", "{", "}"
			A$ = "\" + A$

		CASE "~", "^", "\", ">", "<"
			A$ = "\verb'" + A$ + "'"

		END SELECT

		!
		! Handle bold one character
		!
		IF FLAG_BOLD_TEMP%
		THEN
			A$ = "{\bf " + A$ + "}"
			FLAG_BOLD_TEMP% = 0%
		END IF

		!
		! Send on character
		!
		FNFIX_CHAR$ = A$

		!
		! Reset temp case flag
		!
		FLAG_CASE_TEMP% = 0%

	FNEND

32767	END
