1	%TITLE "Document One Help Message"
	%SBTTL "TK_SUBR_TEXONEMESSAGE"
	%IDENT "V3.6a Calico"

	SUB TK_SUBR_TEXONEMESSAGE( &
		IN_LEVEL%,		! Which level are we at &
		TITLE_TYPE$,		! What type of title for this section &
		TITLE_NAME$,		! Title for this section &
		! &
		KEY_NAME$,		! Name of message to print (wildcard) &
		KEY_NAME_BASE$,		! Used for missing fields in KEY_NAME$ &
		DOCU_LIB$,		! Document Library &
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
	!	IN_LEVEL%
	!		Which level are we at
	!
	!	TITLE_TYPE$
	!		What type of title for this section
	!		.table
	!			list - simple list
	!
	!			list-header - List with title on each item
	!
	!			section - Begin a new section
	!
	!			section-header - Begin a new section.
	!				create a title from source if
	!				possible.
	!		.endtable
	!
	!	TITLE_NAME$
	!		Title for this section
	!
	!	KEY_NAME$
	!		Name of message to print (can be wildcard)
	!
	!	KEY_NAME_BASE$
	!		Used for missing fields in KEY_NAME$
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
	!	$ BAS TK_SOURCE:TK_SUBR_TEXONEMESSAGE
	!	$ LIB FUNC_LIB:CMCFUN/REP TK_SUBR_TEXONEMESSAGE
	!	$ DELETE TK_SUBR_TEXONEMESSAGE.OBJ;*
	!
	! Author:
	!
	!	10/26/87 - Kevin Handy
	!
	! Modification history:
	!
	!	06/24/88 - Kevin Handy
	!		Modified so that LBR$GET_INDEX does not give
	!		all of the junk that starts to match. Was
	!		assuming a "*" at the end of the string.
	!
	!	06/29/88 - Kevin Handy
	!		Fixed ligature problem with ?' becomming
	!		an upside-down question-mark.
	!
	!	06/30/88 - Kevin Handy
	!		Added italics flag (^~, \~).
	!
	!	07/01/88 - Kevin Handy
	!		Modified to force direction of quote symbol
	!		at the start of each help message.
	!
	!	07/11/88 - Kevin Handy
	!		Modified to fix bug with only pulling up correct
	!		help text. (re 06/24/88 change)  Only pulled up
	!		text when full name was called out in TeX command.
	!
	!	07/12/88 - Kevin Handy
	!		Modified to list out help messages in the main
	!		help library for the system being processed
	!		that were not included in the documentation.
	!
	!	07/13/88 - Kevin Handy
	!		Added the ability to output error message at the
	!		end of the documentation.  Requires creating a
	!		help message of the-form "ERR$gl_system$HELP",
	!		(replace "gl_system" with the name of the menu
	!		being printed).
	!
	!	07/14/88 - Kevin Handy
	!		Added more space between each paragraph in the
	!		documentation.  Modified list environment.
	!
	!	07/25/88 - Kevin Handy
	!		Split program into smaller chunks so I could
	!		use those chunks in other programs as necessary.
	!
	!	05/02/89 - Kevin Handy
	!		Modified ".list" enviornment so that if there
	!		is an item in the quotes more than one character
	!		long, it will be used for the blob on the items.
	!
	!	06/06/89 - Aaron Redd
	!		Modified to put index debugging information in
	!		the right margin (tiny print, italics).
	!
	!	02/01/90 - Kevin Handy
	!		Added code to automatically generate glossary.
	!
	!	04/30/90 - Kevin Handy
	!		Modified the index command so that it would
	!		output the ">" without putting "$>$" so that
	!		IDXTEX could handle it properly.
	!
	!	05/18/90 - Frank F. Starman
	!		Added new argument DOCU_LIB$.
	!
	!	06/13/90 - Kevin Handy
	!		Changed index proofing mode since there is no
	!		longer any margin to write the index items
	!		into any more.
	!
	!	11/07/90 - Kevin Handy
	!		Modified to leave only those items in the toc
	!		that are specified at the menu level, since Frank
	!		changed it to include lots else which makes it
	!		impossible to use the toc easily.
	!
	!	08/12/91 - Kevin Handy
	!		Added FOOTNOTE and END FOOTNOTE commands.
	!
	!	08/13/91 - Kevin Handy
	!		Modified to output "\newline" for a ".br"
	!		instead of a blank line, which caused a new
	!		paragraph with an indent.
	!
	!	08/14/91 - Kevin Handy
	!		Added QUOTE and END QUOTE commands.
	!
	!	08/15/91 - Kevin Handy
	!		Changes to use "\par" and such instead of
	!		blank lines to try for a better appearence.
	!
	!	08/15/91 - Kevin Handy
	!		The "\par" did not work out very well, so changed
	!		it to "\newline\hspace*{parindent}".
	!
	!	08/15/91 - Kevin Handy
	!		The \newline stuff worked a little better, but was
	!		getting too complex, so I creaded macros \dopar
	!		and \dounpar (in CMCEXTRA.STY) to make my life
	!		easier.
	!
	!	03/27/92 - Kevin Handy
	!		Added ".TABLE" option, and simplistic handling
	!		of tabs, which I will amend later.
	!
	!	03/27/92 - Kevin Handy
	!		Fixed bug in ".BLANK xx" command that caused
	!		the paragraph not to be terminated.
	!		(Needed a \newline command).
	!
	!	04/01/92 - Kevin Handy
	!		Fixed bug where would add space at end of line
	!		if bold-off is at end of line.
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
	!	04/13/99 - Kevin Handy
	!		Use BASIC$STARLET for STR$
	!
	!	06/09/99 - Kevin Handy
	!		Lose line 7000 (Dead Code)
	!
	!	12/22/2000 - Kevin Handy
	!		Lose unecessary error trap
	!		Lose commented out lines
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
	EXTERNAL LONG	FUNCTION	COMP_STRING
	EXTERNAL LONG			TK_DOCU_GETMODULES
	EXTERNAL STRING	FUNCTION	TK_FUNC_TEXSTRING

	!
	! Declare variables
	!
	MAP (LBR_JUNKJUNK) TEXT$ = 132%		! Read buffer

	MAP (TK_DOCU) &
		MODCOUNT1%, &
		MODNAME1$(200%) = 50%, &
		LIBNAME1$(200%) = 50%, &
		CURLIB$ = 50%, &
		RFA MODRFA1(200%)

	DIM MODNAME$(200%), &
		LIBNAME$(200%), &
		FLAG_LIST$(20%), &
		RFA MODRFA(200%)

	MAP (TK_DOCUALL) &
		MODCOUNTALL%, &
		MODNAMEALL$(1500%) = 50%, &
		MODFLAGALL%(1500%), &
		IDX.DBG%

	%PAGE

4000	!*******************************************************************
	! Read in one help file, and convert to TeX format
	!*******************************************************************
	PRINT #LATEX.CH%, "%BEGIN "; KEY_NAME$; IN_LEVEL%
	! PRINT "%BEGIN "; KEY_NAME$; IN_LEVEL%

	!
	! Let's make sure quotes point in the right direction
	!
	QUOTE_FLAG% = 0%

	!
	! Search for key in file
	!
	MODCOUNT1% = 0%

	KEY_NAME_LIST$ = EDIT$(KEY_NAME$, 2% + 32%)
	KEY_NAME_LIST1$ = ""

	WHILE KEY_NAME_LIST$ <> ""

		!
		! Pull one item off of the search list
		!
		TEMP% = INSTR(1%, KEY_NAME_LIST$, ",")

		IF TEMP%
		THEN
			KEY_NAME1$ = LEFT(KEY_NAME_LIST$, TEMP% - 1%)
			KEY_NAME_LIST$ = RIGHT(KEY_NAME_LIST$, &
				TEMP% + 1%)
		ELSE
			KEY_NAME1$ = KEY_NAME_LIST$
			KEY_NAME_LIST$ = ""
		END IF

		!
		! Do a little bit more processing on the key
		!
		KEY_NAME1$ = FNKEY$(KEY_NAME1$, KEY_NAME_BASE$)
		KEY_NAME_LIST1$ = KEY_NAME_LIST1$ + "," + KEY_NAME1$

		!
		! Now, figure out the name of the library that this
		! message should be in.
		!
		IF TRM$(DOCU_LIB$) <> ""
		THEN
			LIB_NAME$ = DOCU_LIB$
		ELSE
			TEMP% = INSTR(1%, KEY_NAME1$, "$")
			TEMP1% = INSTR(TEMP% + 1%, KEY_NAME1$ + "$", "$")
			TEMP$ = SEG$(KEY_NAME1$, TEMP% + 1%, TEMP1% - 1%)

			UNDER% = INSTR(1%, TEMP$ + "_", "_")
			!
			! Which library is it in?
			!
			IF UNDER% = 1%
			THEN
				TEMP$ = RIGHT(KEY_NAME1$, TEMP1% + 1%)
				UNDER% = INSTR(1%, TEMP$ + "_", "_")
			END IF

			LIB_NAME$ = "REF:HELP_" + LEFT(TEMP$, UNDER% - 1%)

			!LIB_NAME$ = "REF:HELP_UTL" IF LEFT(TEMP$, 2%)="UT"

		END IF

		!
		! Set up the control structure if necessary
		!
		ST% = LBR$INI_CONTROL(LR_INDEX%, LBR$C_READ)

		IF (ST% AND 1%) = 0%
		THEN
			PRINT "  *** Unable to initilize library"; &
				ST%; KEY_NAME1$
			GOTO ExitProgram
		END IF

		!
		! Open the library function
		!
		ST% = LBR$OPEN(LR_INDEX%, LIB_NAME$, , "REF:.TLB")

		IF (ST% AND 1%) = 0%
		THEN
			LIB_NAME$ = "REF:HELP_DEFAULT"
			ST% = LBR$CLOSE(LR_INDEX%)
			ST% = LBR$INI_CONTROL(LR_INDEX%, LBR$C_READ)
			ST% = LBR$OPEN(LR_INDEX%, LIB_NAME$, , "REF:.TLB")

			IF (ST% AND 1%) = 0%
			THEN
				PRINT "  *** Error in LIB open "; ST%; KEY_NAME1$
				GOTO ExitProgram
			END IF
		END IF

		CURLIB$ = LIB_NAME$

		ST% = LBR$GET_INDEX(LR_INDEX%, 1%, &
			TK_DOCU_GETMODULES, KEY_NAME1$)

 !		IF (ST% AND 1%) = 0%
 !		THEN
 !			PRINT "Error in LIB get index "; ST%; KEY_NAME1$
 !		END IF

		!
		! Close this library in preperation for next search
		!
		ST% = LBR$CLOSE(LR_INDEX%)

	NEXT

	!
	! Due to the fact that the lib search may return more than we
	! desire or want, we now have to go through the list removing junk.
	!
	! While doing this, also move into local array.
	!
	TEMP_TOTAL% = 0%
	KEY_NAME_LIST1$ = RIGHT(KEY_NAME_LIST1$, 2%)

	FOR LOOP% = 1% TO MODCOUNT1%
		!
		! Keep this one?
		!
		IF COMP_STRING(MODNAME1$(LOOP%), KEY_NAME_LIST1$)
		THEN
			TEMP_TOTAL% = TEMP_TOTAL% + 1%
			MODNAME$(TEMP_TOTAL%) = MODNAME1$(LOOP%)
			MODRFA(TEMP_TOTAL%) = MODRFA1(LOOP%)
			LIBNAME$(TEMP_TOTAL%) = LIBNAME1$(LOOP%)
		END IF

	NEXT LOOP%

	MODCOUNT% = TEMP_TOTAL%

	!
	! Null out current open lib name
	!
	LR_INDEX% = 0%
	THIS_LIB$ = ""

	!
	! There must be at least one item to print
	!
	IF MODCOUNT% = 0%
	THEN
		PRINT "  *** No fields found " + TRM$(KEY_NAME1$)
		GOTO CloseLibrary
	END IF

	!
	! Print header if declared
	!
	SELECT TITLE_TYPE$

	CASE "section"
		PRINT #LATEX.CH%, FNLEVEL$(IN_LEVEL%, TITLE_NAME$, 0%)
		PRINT "  *** More than one item in a section" IF MODCOUNT% > 1%

	CASE "section-header"
		PRINT "  *** More than one item in a section" IF MODCOUNT% > 1%

	CASE "glossary"
		PRINT #LATEX.CH%, "\item ";

	CASE "list"
		PRINT #LATEX.CH%, "\begin{itemize}"

	END SELECT

	!
	! loop through all modules found.  For/Next statements have to be
	! faked because basic doesn't like arrays as the loop variable.
	!
 LoopMain:
	FOR MAIN_LOOP% = 1% TO MODCOUNT%

		TEST_TITLE% = 0%

		IF (THIS_LIB$ <> LIBNAME$(MAIN_LOOP%))
		THEN
			!
			! Close this library in preperation for next search
			!
			ST% = LBR$CLOSE(LR_INDEX%) &
				IF (THIS_LIB$ <> "")

			THIS_LIB$ = LIBNAME$(MAIN_LOOP%)

			!
			! Set up the control structure if necessary
			!
			ST% = LBR$INI_CONTROL(LR_INDEX%, LBR$C_READ)

			IF (ST% AND 1%) = 0%
			THEN
				PRINT "Unable to initilize library"; ST%
				GOTO ExitProgram
			END IF

			!
			! Open the library function
			!
			ST% = LBR$OPEN(LR_INDEX%, LIBNAME$(MAIN_LOOP%), , "REF:.TLB")

			IF (ST% AND 1%) = 0%
			THEN
				PRINT "Error in LIB open "; ST%
				GOTO ExitProgram
			END IF

		END IF

		!
		! Point to text
		!
		ST% = LBR$FIND(LR_INDEX%, MODRFA(MAIN_LOOP%))

		IF (ST% AND 1%) = 0%
		THEN
 !			PRINT "Error in LIB find"; ST%; &
 !				MODNAME$(MAIN_LOOP%)
			GOTO LoopNext
		END IF

		!
		! Handle titles on each segment
		!
		SELECT TITLE_TYPE$

		CASE "list"
			PRINT #LATEX.CH%, "\item"

		END SELECT

		!
		! Copy over text
		!
 Loop:
		TEXT$ = ""
		ST% = LBR$GET_RECORD(LR_INDEX%, TEXT$)

		IF (ST% AND 1%) = 1%
		THEN
			INLINE$ = TRM$(TEXT$)

			!
			! Print header if declared
			!
			IF TEST_TITLE% = 0%
			THEN
				SELECT TITLE_TYPE$

				CASE "list-header", "section-header"

					IF LEFT(INLINE$, 2%) = "^*"
					THEN
						INLINE$ = RIGHT(INLINE$, 3%)
						INLINE$ = LEFT(INLINE$, LEN(INLINE$) - 2%) &
						IF RIGHT(INLINE$, LEN(INLINE$) - 1%) = "\*"
						GOSUB ProcessText
						PRINT #LATEX.CH%, &
							FNLEVEL$(-IN_LEVEL%, OUT_LINE$, 0%)
						TEST_TITLE% = -1%
						GOTO Loop
					ELSE
						IF LEFT(INLINE$, 1%) <> "."
						THEN
							PRINT #LATEX.CH%, &
								FNLEVEL$(IN_LEVEL%, TITLE_NAME$, 0%)
							TEST_TITLE% = -1%
						END IF
					END IF
				END SELECT
			END IF

			GOSUB 6000
			GOTO Loop
		END IF

		!
		! Finish up loop
		!
 LoopNext:

	NEXT MAIN_LOOP%

 LoopMainEnd:

	!
	! Finish up this section
	!
	SELECT TITLE_TYPE$

	CASE "list"
		PRINT #LATEX.CH%, "\end{itemize}"

	END SELECT

	!
	! Close library file
	!
 CloseLibrary:

	!
	! Close this library in preperation for next search
	!
	ST% = LBR$CLOSE(LR_INDEX%) &
		IF (THIS_LIB$ <> "")

	GOTO ExitProgram

	%PAGE

6000	!*******************************************************************
	! Read in a line and convert
	!*******************************************************************

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
			PRINT #LATEX.CH%, "\dounpar ";

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
				PRINT #LATEX.CH%, "\dopar ";
			ELSE
				PRINT #LATEX.CH%, "\newline\vspace{"; &
					LEFT(NUM1$(TEMP% / 6.0), 6%); "in}\dopar"
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
			PRINT #LATEX.CH%, "\\"
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
				PRINT #LATEX.CH%, "{"; "\begin{itemize}"; SP_TEMP$
				FLAG_LIST$ = FLAG_LIST$ + "*"
				ZZ_ONE% = INSTR(1%, TEMP$, '"')
				ZZ_TWO% = INSTR(ZZ_ONE% + 1%, TEMP$, '"')
				FLAG_LIST$(LEN(FLAG_LIST$)) = &
					SEG$(TEMP$, ZZ_ONE% + 1%, ZZ_TWO% - 1%)
			ELSE
				PRINT #LATEX.CH%, "{"; "\begin{enumerate}"; SP_TEMP$
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
		! End Footnote
		!
		CASE 18%
			PRINT #LATEX.CH%, "}"

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
		! Quote
		!
		CASE 21%
			PRINT #LATEX.CH%, "\begin{quote}"

		!
		! End Quote
		!
		CASE 22%
			PRINT #LATEX.CH%, "\end{quote}"


		!
		! Paragraph
		!
		CASE 26%
			PRINT #LATEX.CH%, "\dopar ";
 !			PRINT #LATEX.CH%


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
		! Document fields (Assume same file as current)
		!
		CASE 50%
			GOSUB ExtraLine		! Handle extended lines

			KEY_NAME$ = RIGHT(DOT_COMMAND$, DOT_WIDTH%)

			CALL TK_SUBR_TEXONEMESSAGE( &
				IN_LEVEL% + 1%, "list", "Entry Fields", &
				KEY_NAME$, KEY_NAME_BASE$, "", &
				LATEX.CH%, &
				COMMAND_LIST%, COMMAND_LIST$(), COMMAND_LIST%(), &
				GLOSSARY%, GLOSSARY$())

			INLINE$ = ""

		!
		! Options (Assume same file as current)
		!
		CASE 51%
			GOSUB ExtraLine		! Handle extended lines

			KEY_NAME$ = RIGHT(DOT1_COMMAND$, DOT_WIDTH%)

			CALL TK_SUBR_TEXONEMESSAGE( &
				IN_LEVEL% + 1%, "list-header", "Additional Function", &
				KEY_NAME$, KEY_NAME_BASE$, "", &
				LATEX.CH%, &
				COMMAND_LIST%, COMMAND_LIST$(), COMMAND_LIST%(), &
				GLOSSARY%, GLOSSARY$())

			INLINE$ = ""

		!
		! Screens
		!
		CASE 52%
			GOSUB ExtraLine		! Handle extended lines

			WIN_NAME$ = RIGHT(DOT1_COMMAND$, DOT_WIDTH%)

			CALL TK_SUBR_TEXSCREEN(WIN_NAME$, KEY_NAME_BASE$, LATEX.CH%, 3%)

			INLINE$ = ""

		!
		! Reports
		!
		CASE 53%
			GOSUB ExtraLine		! Handle extended lines

			WIN_NAME$ = RIGHT(DOT1_COMMAND$, DOT_WIDTH%)

			CALL TK_SUBR_TEXSCREEN(WIN_NAME$, KEY_NAME_BASE$, LATEX.CH%, 5%)

			INLINE$ = ""

		!
		! Special text to pass straight through to LaTeX
		!
		CASE 54%
			GOSUB ExtraLine		! Handle extended lines

			TEMP$ = RIGHT(DOT1_COMMAND$, DOT_WIDTH%)
			PRINT #LATEX.CH%, TEMP$

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

		CASE 57%
			TEMP$ = RIGHT(DOT1_COMMAND$, DOT_WIDTH%)
			IF TEMP$ = ""
			THEN
				I% = 2%
			ELSE
				I% = 1%
				J% = 0%
				WHILE INSTR(J% + 1%, DOT1_COMMAND$, ",")
					I% = I% + 1%
					J% = INSTR(J% + 1%, DOT1_COMMAND$, ",")
				NEXT
			END IF

			TEMP$ = "\newline\vspace*{0.05in}\newline\hspace*{0.5in}\begin{tabular}{"
			TEMP$ = TEMP$ + "l" FOR J% = 1% TO I%
			TEMP$ = TEMP$ + "}"
			PRINT #LATEX.CH%, TEMP$
			IN_TABLE% = -1%

		CASE 58%
			SELECT IN_TABLE%
			CASE -2%	! Just printed something
				PRINT #LATEX.CH%, " \\"

			CASE -3%	! Just printed title
				PRINT #LATEX.CH%, " \\ \hline"

			END SELECT

			IN_TABLE% = -2%

		CASE 59%
			PRINT #LATEX.CH%, " \\"
			PRINT #LATEX.CH%, "\end{tabular}\newline\vspace{0.1in}\newline"
			IN_TABLE% = 0%

		CASE 60%
			IN_TABLE% = -3%

		!
		! .Option
		!
		CASE 61%
			PRINT #LATEX.CH%, "\newline\vspace*{0.1in}\newline Option:"
			TEMP$ = "\newline\vspace*{0.05in}\newline\hspace*{0.5in}\begin{tabular}{|"
			TEMP$ = TEMP$ + "l|" FOR J% = 1% TO 2%
			TEMP$ = TEMP$ + "}\hline"
			PRINT #LATEX.CH%, TEMP$
			IN_TABLE% = -1%

		CASE 62%
			SELECT IN_TABLE%
			CASE -2%	! Just printed something
				PRINT #LATEX.CH%, " \\ \hline"

			CASE -3%	! Just printed title
				PRINT #LATEX.CH%, " \\ \hline \hline"

			END SELECT

			IN_TABLE% = -2%

		CASE 63%
			PRINT #LATEX.CH%, " \\ \hline"
			PRINT #LATEX.CH%, "\end{tabular}\newline\vspace{0.1in}\newline"
			IN_TABLE% = 0%


		!
		! .Example
		!
		CASE 64%
			PRINT #LATEX.CH%, "\newline\vspace*{0.1in}\newline Example:"
			TEMP$ = "\newline\vspace*{0.05in}\newline\hspace*{0.5in}\begin{tabular}{|"
			TEMP$ = TEMP$ + "l|" FOR J% = 1% TO 2%
			TEMP$ = TEMP$ + "}\hline"
			PRINT #LATEX.CH%, TEMP$
			IN_TABLE% = -1%

		CASE 65%
			SELECT IN_TABLE%
			CASE -2%	! Just printed something
				PRINT #LATEX.CH%, " \\ \hline"

			CASE -3%	! Just printed title
				PRINT #LATEX.CH%, " \\ \hline \hline"

			END SELECT

			IN_TABLE% = -2%

		CASE 66%
			PRINT #LATEX.CH%, " \\ \hline"
			PRINT #LATEX.CH%, "\end{tabular}\newline\vspace{0.1in}\newline"
			IN_TABLE% = 0%


	END SELECT

8900	RETURN

	%PAGE

 ExtraLine:
	!*******************************************************************
	! Handle extended lines for special TeX commands
	!*******************************************************************

	IF RIGHT(DOT1_COMMAND$, LEN(DOT1_COMMAND$)) = "-"
	THEN
		TEXT$ = ""
		ST% = LBR$GET_RECORD(LR_INDEX%, TEXT$)
		IF (ST% AND 1%) = 1%
		THEN
			TEMP$ = TRM$(TEXT$)
			TEMP$ = RIGHT(TEMP$, 3%) IF LEFT(TEMP$, 2%) = ".!"
			DOT1_COMMAND$ = LEFT(DOT1_COMMAND$, &
				LEN(DOT1_COMMAND$) - 1%) + TEMP$
			GOTO ExtraLine
		END IF
	END IF

	RETURN

	%PAGE

9000	!*******************************************************************
	! Process text
	!
	! Scan for any tab characters and handle properly
	!*******************************************************************
 ProcessText:
	OUT_LINE$ = ""			! Output text
	FLAG_BOLD_TEMP% = 0%		! Temporary Bold flag

	!
	! Are there any tabs in input line?
	!
	IF INSTR(1%, INLINE$, '9'C) = 0%
	THEN
		INLINE1$ = INLINE$ + ""
		GOSUB ProcessText1
		RETURN
	END IF

	INLINE2$ = INLINE$

 ProcessTabs:
	TABCH% = INSTR(1%, INLINE2$, '9'C)

	IF TABCH%
	THEN
		INLINE1$ = LEFT(INLINE2$, TABCH% - 1%)
		INLINE2$ = RIGHT(INLINE2$, TABCH% + 1%)

		IF (IN_TABLE%)
		THEN
			GOSUB ProcessText1
			OUT_LINE$ = OUT_LINE$ + " & "
		ELSE
			GOSUB ProcessText1
			OUT_LINE$ = OUT_LINE$ + "\/\hfill "
		END IF

		GOTO ProcessTabs
	ELSE
		INLINE1$ = INLINE2$
		GOSUB ProcessText1
	END IF

	RETURN

	!*******************************************************************
	! Handle actual text part of line
	!*******************************************************************

 ProcessText1:

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
			IF LEFT(INLINE1$, 1%) == " "
			THEN
				OUT_LINE$ = OUT_LINE$ + " \bf "
			ELSE
				OUT_LINE$ = OUT_LINE$ + "\bf "
			END IF

		!
		! A tilde - italicize all
		!
		CASE "~"
			IF LEFT(INLINE1$, 1%) == " "
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
			IF LEFT(INLINE1$, 1%) == " "
			THEN
				OUT_LINE$ = OUT_LINE$ + " \rm "
			ELSE
				OUT_LINE$ = OUT_LINE$ + "\rm "
			END IF

		!
		! A tilde - out of italics
		!
		CASE "~"
			IF LEFT(INLINE1$, 1%) == " "
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

	!*******************************************************************
	! Print level titles
	!*******************************************************************

	DEF FNLEVEL$(DOT_COUNTER%, TITLE$, STAR_FLAG%)

	IF STAR_FLAG%
	THEN
		STAR_FLAG$ = "*"
	ELSE
		STAR_FLAG$ = ""
	END IF

	IF DOT_COUNTER% < 0%
	THEN
		TITLE1$ = TITLE$
	ELSE
		TITLE1$ = TK_FUNC_TEXSTRING(TITLE$)
	END IF

	SELECT ABS(DOT_COUNTER%)

		!
		! Start of a chapter
		!
		CASE 0%, 1%
			FNLEVEL$ = "\chapter{" + TITLE1$ + "}"

		!
		! Start of a section
		!
		CASE 2%
			FNLEVEL$ = "\pagebreak[3]\section" + STAR_FLAG$ + &
			"{" + TITLE1$ + "}"

		!
		! Start of a subsection
		!
		CASE 3%
			FNLEVEL$ = "\pagebreak[2]\subsection" + STAR_FLAG$ + &
			"{" + TITLE1$ + "}"

		!
		! Start of a subsubsection
		!
		CASE 4%
			FNLEVEL$ = "\pagebreak[1]\subsubsection" + STAR_FLAG$ + &
				"{" + TITLE1$ + "}"

		!
		! Start of a paragraph
		!
		CASE 5%
			FNLEVEL$ = "\paragraph {" + TITLE1$ + "}"

		!
		! Start of a subparagraph
		!
		CASE ELSE
			FNLEVEL$ = "\subparagraph {" + TITLE1$ + "}"

	END SELECT

	FNEND

	%PAGE

	!*******************************************************************
	! Calculate key name
	!*******************************************************************

	DEF FNKEY$(A$, B$)

		IF INSTR(1%, A$, "$")
		THEN
			FNKEY$ = A$
		ELSE
			TEMP% = INSTR(1%, B$, "$")
			TEMP% = INSTR(TEMP% + 1%, B$, "$")
			FNKEY$ = LEFT(B$, TEMP%) + A$
		END IF
	FNEND

	%PAGE

20000	!
	! Exit
	!
 ExitProgram:
	END SUB

21000	FUNCTION LONG TK_DOCU_GETMODULES(MODKEY$, RFA MODRFA)

	!
	! This function graps the names passed to it from the
	! LIB$SEARCH call
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	MAP (TK_DOCU) &
		MODCOUNT%, &
		MODNAME$(200%) = 50%, &
		LIBNAME$(200%) = 50%, &
		CURLIB$ = 50%, &
		RFA MODRFA(200%)

	MAP (TK_DOCUALL) &
		MODCOUNTALL%, &
		MODNAMEALL$(1500%) = 50%, &
		MODFLAGALL%(1500%)

	!
	! Don't add name if we have amassed too many items already.
	!
	GOTO ExitFunction &
		IF MODCOUNT% = 200%

	!
	! Flag the item as having been used.
	! This is used as a rough check to see if everything in the
	! main library gets printed.
	!
	I% = 1%
	WHILE (I% <= MODCOUNTALL%) ! AND (MODKEY$ >= MODNAMEALL$(I%))

		MODFLAGALL%(I%) = -1% IF (MODKEY$ = MODNAMEALL$(I%))
		I% = I% + 1%

	NEXT

	!
	! Don't add name if already have a key to that item.
	!
	GOTO ExitFunction &
		IF MODRFA(LOOP%) = MODRFA &
		FOR LOOP% = 1% TO MODCOUNT%

	!
	! Add to list
	!
	MODCOUNT% = MODCOUNT% + 1%
	MODNAME$(MODCOUNT%) = MODKEY$
	MODRFA(MODCOUNT%) = MODRFA
	LIBNAME$(MODCOUNT%) = CURLIB$

 ExitFunction:
	TK_DOCU_GETMODULES = 1%

	END FUNCTION
