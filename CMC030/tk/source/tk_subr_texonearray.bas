1	%TITLE "Document One Help Message"
	%SBTTL "TK_SUBR_TEXONEARRAY"
	%IDENT "V3.6a Calico"

	SUB TK_SUBR_TEXONEARRAY( &
		TEX_ARRAY%,		! Input Array &
		TEX_ARRAY$(),		! Input Array &
		! &
		LATEX.CH%,		! Where to write out documentation &
		! &
		COMMAND_LIST%,		! Number of known commands &
		COMMAND_LIST$(),	! Known command names &
		COMMAND_LIST%(),	! Parsed command names &
		! &
		GLOSSARY%,		! Number if items in glossarry &
		GLOSSARY$()		! Glossary items &
	)

	!
	!	COPYRIGHT (C) 1988 BY
	!	Computer Management Center, Inc.
	!	Idaho Falls, Idaho.
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
	!	This subroutine will pull in one help message, and process
	!	it in it's entirety.
	!	Note: This function is recursive to process additional
	!	information in each help message that is specified.
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
	!	LATEX.CH%
	!		Where to write out documentation
	!
	!	COMMAND_LIST%
	!		Number of known commands
	!
	!	COMMAND_LIST$()
	!		Known command names
	!
	!	COMMAND_LIST%()
	!		Parsed command names
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
	!	$ BAS TK_SOURCE:TK_SUBR_TEXONEARRAY
	!	$ LIB FUNC_LIB:CMCFUN/REP TK_SUBR_TEXONEARRAY
	!	$ DELETE TK_SUBR_TEXONEARRAY.OBJ;*
	!
	! Author:
	!
	!	10/26/87 - Kevin Handy
	!
	!	10/15/90 - Kevin Handy
	!		Stolen from TK_SUBR_TEXONEMESSAGE.
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	11/20/95 - Kevin Handy
	!		Lose /NOWARN on compile.
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE
	!
	!	04/13/99 - Kevin Handy
	!		Use BASIC$STARLET for STR$
	!
	!	06/09/99 - Kevin Handy
	!		Remove FNLEVEL$ (Dead Code)
	!		Remove FNKEY$ (Dead Code)
	!		Remove ExtraLine (Dead Code)
	!		Remove lines 7000 (Dead Code)
	!
	!	12/22/2000 - Kevin Handy
	!		Lose useless error trap
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:LIBRARY.COM"
	%INCLUDE "STR$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	!
	! External functions
	!
	EXTERNAL LONG			TK_DOCU_GETMODULES
	EXTERNAL STRING	FUNCTION	TK_FUNC_TEXSTRING

	%PAGE

4000	!*******************************************************************
	! Read in one help file, and convert to TeX format
	!*******************************************************************

	!
	! Let's make sure quotes point in the right direction
	!
	QUOTE_FLAG% = 0%
	ARRAY_ITEM% = 0%

	!
	! loop through all modules found.  For/Next statements have to be
	! faked because basic doesn't like arrays as the loop variable.
	!
 LoopMain:
		!
		! Copy over text
		!
 Loop:
		IF (ARRAY_ITEM% <= TEX_ARRAY%)
		THEN
			INLINE$ = TRM$(TEX_ARRAY$(ARRAY_ITEM%))
			ARRAY_ITEM% = ARRAY_ITEM% + 1%

			GOSUB 6000
			GOTO Loop
		END IF

		!
		! Finish up loop
		!
 LoopNext:

 LoopMainEnd:

	!
	! Close library file
	!
 CloseLibrary:

	!
	! Close this library in preperation for next search
	!
	GOTO ExitProgram

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
	IF LITERAL%
	THEN
		PRINT #LATEX.CH%, INLINE$
	ELSE
		GOSUB ProcessText

		PRINT #LATEX.CH%, OUT_LINE$
	END IF

	RETURN

	%PAGE

8000	!*******************************************************************
	! Process dot commands
	!*******************************************************************
 ProcessDot:

	!
	! Hang onto just one command
	!
	DOT_COMMAND$ = EDIT$(INLINE$, 32% + 128%)
	DOT1_COMMAND$ = EDIT$(INLINE$, 128%)
	INLINE$ = ""

	!
	! Scan command for command characters
	!
	FOR DOT_COMMAND% = 1% TO COMMAND_LIST%

		GOTO 8100 IF LEFT(DOT_COMMAND$, &
			LEN(COMMAND_LIST$(DOT_COMMAND%))) = &
			COMMAND_LIST$(DOT_COMMAND%)

	NEXT DOT_COMMAND%

	PRINT "  *** " + TRM$(MODNAME$(MAIN_LOOP%)) + &
		" undef dot cmd: " + DOT_COMMAND$
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
				RIGHT(DOT1_COMMAND$, DOT_WIDTH%); INLINE$
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
			PRINT #LATEX.CH%, "\begin{figure}[htp]\vspace{" + &
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
			PRINT #LATEX.CH%, OUT_LINE$
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
			TEMP$ = RIGHT(DOT1_COMMAND$, DOT_WIDTH%)
			TEMP$ = "NOTE" IF TEMP$ = ""
			PRINT #LATEX.CH%, "\begin{note}["; TEMP$; "]"

		!
		! End note
		!
		CASE 12%
			PRINT #LATEX.CH%, "\end{note}"

		!
		! Begin List
		!
		CASE 13%
			TEMP$ = RIGHT(DOT1_COMMAND$, DOT_WIDTH%)
			IF LEFT(EDIT$(TEMP$, -1%), 1%) = "0"
			THEN
				SP_TEMP$ = "\setlength{\itemsep}{-.5\parsep plus 1pt}"
			ELSE
				SP_TEMP$ = ""
			END IF

			IF INSTR(1%, TEMP$, '"') <> 0%
			THEN
				PRINT #LATEX.CH%, "{"; SP_TEMP$; "\begin{itemize}"
				FLAG_LIST$ = FLAG_LIST$ + "*"
				ZZ_ONE% = INSTR(1%, TEMP$, '"')
				ZZ_TWO% = INSTR(ZZ_ONE% + 1%, TEMP$, '"')
				FLAG_LIST$(LEN(FLAG_LIST$)) = &
					SEG$(TEMP$, ZZ_ONE% + 1%, ZZ_TWO% - 1%)
			ELSE
				PRINT #LATEX.CH%, "{"; SP_TEMP$; "\begin{enumerate}"
				FLAG_LIST$ = FLAG_LIST$ + "N"
				FLAG_LIST$(LEN(FLAG_LIST$)) = ""
			END IF

		!
		! List element
		!
		CASE 14%
			IF LEN(FLAG_LIST$(LEN(FLAG_LIST$))) > 1%
			THEN
				PRINT #LATEX.CH%, "\item[" + &
					FLAG_LIST$(LEN(FLAG_LIST$)) + &
					"] ";
			ELSE
				PRINT #LATEX.CH%, "\item ";
			END IF
		!
		! End list
		!
		CASE 15%
			TEMP$ = RIGHT(FLAG_LIST$, LEN(FLAG_LIST$))
			FLAG_LIST$ = LEFT(FLAG_LIST$, LEN(FLAG_LIST$) - 1%)

			IF TEMP$ = "*"
			THEN
				PRINT #LATEX.CH%, "\end{itemize}}"
			ELSE
				PRINT #LATEX.CH%, "\end{enumerate}}"
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
			TEMP$,TEMP3$ = RIGHT(DOT1_COMMAND$, DOT_WIDTH%)
			TEMP2$ = ""
			PRINT #LATEX.CH%, "\index{";
			WHILE TEMP$ <> ""
				I% = INSTR(1%, TEMP$, ">")
				IF I%
				THEN
					TEMP1$ = LEFT(TEMP$, I% - 1%)
					TEMP$ = RIGHT(TEMP$, I% + 1%)
				ELSE
					TEMP1$ = TEMP$
					TEMP$ = ""
				END IF

				PRINT #LATEX.CH%, TEMP2$; TK_FUNC_TEXSTRING(TEMP1$);
				TEMP2$ = ">"
			NEXT
			PRINT #LATEX.CH%, "}"

			!
			! If index debugging is on, put TEMP$ in the margin
			!
			IF IDX.DBG% = 1%
			THEN
				PRINT #LATEX.CH%, "{\indexfont ["; &
					TK_FUNC_TEXSTRING(TEMP3$); "]} "
			END IF

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


		!
		! Literal
		!
		CASE 48%
			LITERAL% = -1%
			PRINT #LATEX.CH%, "\begin{verbatim}"

		!
		! End literal
		!
		CASE 49%
			LITERAL% = 0%
			PRINT #LATEX.CH%, "\end{verbatim}"

		!
		! Left margin (Nothing for now)
		!
		CASE 55%
			!
			! No code at the moment
			!

		!
		! Glossary entry
		!
		CASE 56%
			TEMP$ = EDIT$(RIGHT(DOT1_COMMAND$, DOT_WIDTH%), &
				4% + 8% + 16% + 32% + 128%)

			FOR I% = 1% TO GLOSSARY%

				!
				! Already in there
				!
				IF GLOSSARY$(I%) = TEMP$
				THEN
					GOTO 8900
				END IF

				!
				! Not there yet
				!
				IF GLOSSARY$(I%) > TEMP$
				THEN
					GLOSSARY$(J% + 1%) = GLOSSARY$(J%) &
						FOR J% = GLOSSARY% TO I% STEP -1%
					GLOSSARY$(I%) = TEMP$
					GLOSSARY% = GLOSSARY% + 1%
					GOTO 8900
				END IF
			NEXT I%

			!
			! Must be past the end of the current list.
			! Add to end of list.
			!
			GLOSSARY% = GLOSSARY% + 1%
			GLOSSARY$(GLOSSARY%) = TEMP$

	END SELECT

8900	RETURN

	%PAGE

9000	!*******************************************************************
	! Process text
	!*******************************************************************
 ProcessText:

	FLAG_BOLD_TEMP% = 0%		! Temporary Bold flag
	OUT_LINE$ = ""			! Output text
	INLINE1$ = INLINE$ + ""

9010	I% = STR$FIND_FIRST_IN_SET(INLINE1$, "_#*^\")

	IF I% = 0%
	THEN
		OUT_LINE$ = OUT_LINE$ + FNFIX_CHAR$(INLINE1$)
		INLINE1$ = ""
		ONE_CHAR$ = ""
	ELSE
		OUT_LINE$ = OUT_LINE$ + FNFIX_CHAR$(LEFT(INLINE1$, I% - 1%))
		ONE_CHAR$ = MID(INLINE1$, I%, 1%)
		INLINE1$ = RIGHT(INLINE1$, I% + 1%)
	END IF

	SELECT ONE_CHAR$

	!
	! Underline - Quote character
	!
	CASE "_"
		OUT_LINE$ = OUT_LINE$ + FNFIX_CHAR$(LEFT(INLINE1$, 1%))
		INLINE1$ = RIGHT(INLINE1$, 2%)

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
		TWO_CHAR$ = LEFT(INLINE1$, 1%)
		INLINE1$ = RIGHT(INLINE1$, 2%)

		SELECT TWO_CHAR$

		!
		! A astrix - Bold all
		!
		CASE "*"
			IF LEFT(INLINE1$, 1%) = " "
			THEN
				OUT_LINE$ = OUT_LINE$ + " \bf "
			ELSE
				OUT_LINE$ = OUT_LINE$ + "\bf "
			END IF

		!
		! A tilde - italicize all
		!
		CASE "~"
			IF LEFT(INLINE1$, 1%) = " "
			THEN
				OUT_LINE$ = OUT_LINE$ + " \it "
			ELSE
				OUT_LINE$ = OUT_LINE$ + "\it "
			END IF

		!
		! A ampersand - Underline all
		!
		CASE "&"
			OUT_LINE$ = OUT_LINE$ + "\underline{"

		END SELECT

	!
	! Back-slash
	!
	CASE "\"
		TWO_CHAR$ = LEFT(INLINE1$, 1%)
		INLINE1$ = RIGHT(INLINE1$, 2%)

		SELECT TWO_CHAR$

		!
		! A astrix - unBold all
		!
		CASE "*"
			IF LEFT(INLINE1$, 1%) = " "
			THEN
				OUT_LINE$ = OUT_LINE$ + " \rm "
			ELSE
				OUT_LINE$ = OUT_LINE$ + "\rm "
			END IF

		!
		! A tilde - out of italics
		!
		CASE "~"
			IF LEFT(INLINE1$, 1%) = " "
			THEN
				OUT_LINE$ = OUT_LINE$ + " \rm "
			ELSE
				OUT_LINE$ = OUT_LINE$ + "\rm "
			END IF

		!
		! A ampersand - Stop Underline all
		!
		CASE "&"
			OUT_LINE$ = OUT_LINE$ + "}"

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

	GOTO 9010 IF INLINE1$ <> ""

	RETURN

	%PAGE

18000	!*******************************************************************
	! Function to process on one character
	!*******************************************************************

	DEF* FNFIX_CHAR$(A1$)

		!
		! Put character into proper case
		!
		O1$ = ""
		A2$ = A1$

		WHILE (A2$ <> "")

			IF (FLAG_BOLD_TEMP%)
			THEN
				A$ = LEFT(A2$, 1%)
				A2$ = RIGHT(A2$, 2%)
			ELSE
				FIXFIND% = STR$FIND_FIRST_IN_SET(A2$, &
					"$&%#_{}~^\><?|" + '"')

				IF (FIXFIND%)
				THEN
					O1$ = O1$ + LEFT(A2$, FIXFIND% - 1%)
					A$ = MID(A2$, FIXFIND%, 1%)
					A2$ = RIGHT(A2$, FIXFIND% + 1%)
				ELSE
					O1$ = O1$ + A2$
					A$ = ""
					A2$ = ""
				END IF
			END IF

			!
			! Fix character if it is one of the special ones
			!
			SELECT A$

				CASE "$", "&", "%", "#", "_", "{", "}"
					O$ = "\" + A$

				CASE "~", "^", "\", ">", "<"
					O$ = "{\tt\char" + NUM1$(ASCII(A$)) + "}"

				CASE "?"	! Lig problem (?')
					O$ = "{" + A$ + "}"

				CASE "|"
					O$ = "{\vrule}"

				CASE '"'
					!
					! Alternate the quote marker between `` and ''.
					!
					IF QUOTE_FLAG%
					THEN
						O$ = "''"
						QUOTE_FLAG% = 0%
					ELSE
						O$ = "``"
						QUOTE_FLAG% = -1%
					END IF

				CASE ELSE
					O$ = A$
			END SELECT

			!
			! Handle bold one character
			!
			IF FLAG_BOLD_TEMP%
			THEN
				O$ = "{\bf " + O$ + "}"
				FLAG_BOLD_TEMP% = 0%
			END IF

			O1$ = O1$ + O$

		NEXT

		!
		! Send on character
		!
		FNFIX_CHAR$ = O1$

	FNEND

	%PAGE

20000	!
	! Exit
	!
 ExitProgram:
	END SUB
