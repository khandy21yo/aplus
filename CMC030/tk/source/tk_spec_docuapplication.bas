1	%TITLE "Document a System Menu"
	%SBTTL "TK_SPEC_DOCUAPPLICATION"
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
	!	This program will read a menu source file, and from
	!	that, will create a documentation book.
	!
	! Index:
	!
	! Option:
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
	!	$ BAS TK_SOURCE:TK_SPEC_DOCUAPPLICATION
	!	$ LINK/EXE=TK_EXE: TK_SPEC_DOCUAPPLICATION, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE TK_SPEC_DOCUAPPLICATION.OBJ;*
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
	!	03/25/88 - Kevin Handy
	!		Removes SMG stuff so it could work in
	!		a command file (re 11/18/87).
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
	!	07/22/88 - Kevin Handy
	!		Split program into smaller chunks so I could
	!		use those chunks in other programs as necessary.
	!
	!	06/06/89 - Aaron Redd
	!		Modified so that the program would ask if the
	!		user wanted index debugging information printed.
	!
	!	02/01/90 - Kevin Handy
	!		Added automatic glossary creation.
	!
	!	02/02/90 - Kevin Handy
	!		Created new style file (CMCBOOK.STY) from old
	!		one supplied with LaTex (BOOK.STY) which underlines
	!		the titles.
	!
	!	02/04/90 - Frank F. Starman
	!		Add line for cmc version number
	!
	!	02/05/90 - Kevin Handy
	!		Put default glossary words into a text file instead
	!		of a DATA statement.
	!
	!	04/22/90 - Frank F. Starman
	!		Print CMC A+ Messages chapter.
	!
	!	05/02/90 - Kevin Handy
	!		Removed \makeglossary command so that we don't
	!		create a useless file.
	!
	!	05/20/90 - Frank F. Starman
	!		Reorganize LOG file and break document into two parts.
	!
	!	06/13/90 - Kevin Handy
	!		Changed format of page headers/footers.
	!		Changed handling of index proofing mode.
	!
	!	06/22/90 - Kevin Handy
	!		Changed format of footer on chapter pages.
	!
	!	08/08/90 - Frank F. Starman
	!		Ask for the SIC code and define it for LATEX.
	!		Also define menu name for LATEX.
	!
	!	08/21/90 - Frank F. Starman
	!		Add Data File chapter in CMC A+ document.
	!
	!	09/05/90 - Frank F. Starman
	!		Add new three sections in User's Guide part
	!		(Getting Started, Routines, Tips)
	!
	!	08/05/91 - Kevin Handy
	!		Added ACCESS READ to open statements.
	!
	!	08/12/91 - Kevin Handy
	!		Added FOOTNOTE and END FOOTNOTE  option.
	!
	!	08/14/91 - Kevin Handy
	!		Removed A+.
	!
	!	08/14/91 - Kevin Handy
	!		Added QUOTE and END QUOTE commands.
	!
	!	03/27/92 - Kevin Handy
	!		Added "TABLE" option.
	!
	!	03/27/92 - Kevin Handy
	!		Added "OPTION" and "EXAMPLE" options.
	!
	!	03/27/92 - Kevin Handy
	!		Changed "parindent" and "listparindent" to
	!		zero to generate blocked paragraphs instead
	!		of indented paragraphs.
	!
	!	07/27/92 - Kevin Handy
	!		Removed "fullpage" style since I have included
	!		it in the "cmcextra" style.
	!
	!	03/30/93 - Kevin Handy
	!		Removed several error messages that print out since
	!		Frank's changes force tons of error messages to
	!		print, which mean nothing.
	!
	!	06/14/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	11/20/95 - Kevin Handy
	!		Lose /NOWARN on compile.
	!
	!	10/29/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/28/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/09/99 - Kevin Handy
	!		Remove FNLABEL$ (Dead code)
	!
	!	08/10/99 - Kevin Handy
	!		Clean up (Check)
	!
	!	10/17/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	05/16/2003 - Kevin Handy
	!		Fixes to run LaTeX under Linux
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:LIBRARY.COM"

	!
	! External functions
	!
	EXTERNAL LONG TK_DOCU_GETMODULES_ALL

	!
	! Declare variables
	!
	MAP (LBR_JUNKJUNK) TEXT$ = 132%, TEXT1$ = 256%		! Read buffer
	DECLARE RFA TXRFA, NULL.RFA, WIN_RFA, WIN1_RFA

	MAP (TK_DOCUALL) &
		MODCOUNTALL%, &
		MODNAMEALL$(1500%) = 50%, &
		MODFLAGALL%(1500%), &
		IDX.DBG%

	!
	! Create array to hold legal commands
	!
	DECLARE INTEGER CONSTANT MAX_COMMAND = 100%
	DIM COMMAND_LIST$(MAX_COMMAND),	COMMAND_LIST%(MAX_COMMAND)

	MAP (IOBUF) LONG IO_BUF(6%)
	MAP (IOBUF) WORD IO_BUF_W(12%)
	MAP (IOBUF1) NAME.BUFFER$ = 50%

	DIM GLOSSARY$(500%)
	DIM HELP_LIB$(50%)

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION SYS$FILESCAN
	DECLARE LONG CONSTANT FSCN$_NAME = 6

	%PAGE

1000	!*******************************************************************
	! Allocate channels
	!*******************************************************************

	GLOSSARY.CH% = 4%
	SOURCE.CH% = 5%
	LATEX.CH% = 6%

	!
	! Set up file name for menu look up
	!
	MENU_FILE_NAME$ = "CMC:*.MNU"
	MENU_PREFIX$ = "CMC:"
	MENU_SUFFIX$ = ""
	TLB_DEVICE$ = "REF:"

	!*******************************************************************
	! Initilize for runoff conversion
	!*******************************************************************

	!
	! Initialize command list
	!
	COMMAND_LIST% = 0%

	READ NVALUE%, NVALUE$

	WHILE (NVALUE% <> 0%)

		COMMAND_LIST% = COMMAND_LIST% + 1%
		COMMAND_LIST$(COMMAND_LIST%) = NVALUE$
		COMMAND_LIST%(COMMAND_LIST%) = NVALUE%

		READ NVALUE%, NVALUE$

	NEXT

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
		17,	".NF ", &
		18,	".END FOOTNOTE ", &
		18,	".EFN ", &
		19,	".INDEX ", &
		19,	".X ", &
		20,	".TEST PAGE ", &
		20,	".TESTPAGE ", &
		20,	".TP ", &
		21,	".QUOTE", &
		21,	".QT", &
		22,	".END QUOTE", &
		22,	".EQT", &
		26,	".P ", &
		26,	".PARAGRAPH ", &
		48,	".LITERAL ", &
		48,	".LT ", &
		49,	".END LITERAL ", &
		49,	".ENDLITERAL ", &
		49,	".EL ", &
		50,	".!.FIELD ", &
		50,	".!.FIELDS ", &
		51,	".!.OPTION ", &
		51,	".!.OPTIONS ", &
		52,	".!.SCREEN ", &
		52,	".!.SCREENS ", &
		53,	".!.REPORT ", &
		53,	".!.REPORTS ", &
		54,	".!!", &
		55,	".LM ", &
		55,	".LEFT MARGIN ", &
		55,	".LEFTMARGIN ", &
		56,	".!.GLOSSARY", &
		56,	".ENTRY", &
		56,	".Y", &
		57,	".TABLE ", &
		58,	".TE ", &
		59,	".END TABLE ", &
		59,	".ENDTABLE ", &
		60,	".TT ", &
		61,	".OPTION ", &
		62,	".OE ", &
		63,	".END OPTION ", &
		63,	".ENDTOPION ", &
		64,	".EXAMPLE ", &
		65,	".EE ", &
		66,	".END EXAMPLE ", &
		66,	".ENDEXAMPLE ", &
		0,	""

	!
	! Load list of initial glossary words
	!
	GLOSSARY% = 0%

1100	WHEN ERROR IN
		OPEN "REF:GLOSSARY.TEMPLATE" FOR INPUT AS FILE GLOSSARY.CH%, &
			ACCESS READ, ALLOW MODIFY
	USE
		CONTINUE 1190
	END WHEN

1110	WHEN ERROR IN
		LINPUT #GLOSSARY.CH%, GLOSSARY$
	USE
		CONTINUE 1190
	END WHEN

	GLOSSARY$ = EDIT$(GLOSSARY$, -1%)

	IF GLOSSARY$ <> ""
	THEN
		GLOSSARY% = GLOSSARY% + 1%
		GLOSSARY$(GLOSSARY%) = GLOSSARY$
	END IF

	GOTO 1110

	%PAGE

1190	!*******************************************************************
	! Get menu source file
	!*******************************************************************
	FILE_NAME$ = ""

1200	LINPUT "Name of menu file (No account or extension) "; SOURCE.NAME$

	SOURCE.NAME$ = EDIT$(SOURCE.NAME$, 2% + 32% + 256%)

	OPEN SOURCE.NAME$ FOR INPUT AS FILE SOURCE.CH%, &
		ACCESS READ, &
		ALLOW MODIFY, &
		DEFAULTNAME "CMC:.MNU"

1300	!*******************************************************************
	! Create dummy index file, so that we will not need three passes
	!*******************************************************************

	INDEX.NAME$ = SOURCE.NAME$

	OPEN INDEX.NAME$ FOR OUTPUT AS FILE LATEX.CH%, &
		DEFAULTNAME ".IND", &
		RECORDSIZE 255%, &
		ALLOW READ

	PRINT #LATEX.CH%, "\makeatletter"
	PRINT #LATEX.CH%, "\def\indexhead#1#2#3{\par\if@nobreak \everypar{}\else"
	PRINT #LATEX.CH%, "  \addpenalty{\@secpenalty}\addvspace{#1}\fi"
	PRINT #LATEX.CH%, "  \begingroup \large\bf \hfil #3 \hfil\par \endgroup"
	PRINT #LATEX.CH%, "  \@xsect{#2}}"
	PRINT #LATEX.CH%, "\makeatother"
	PRINT #LATEX.CH%, "\def\indexindent{\par\hangindent 50pt\hspace*{40pt}}"
	PRINT #LATEX.CH%, "\begin{theindex} \raggedright"
	PRINT #LATEX.CH%, "\addcontentsline{toc}{chapter}{Index}\typeout{Index.}"
	PRINT #LATEX.CH%, "\item Index not yet generated"
	PRINT #LATEX.CH%, "\end{theindex}"

	CLOSE #LATEX.CH%

1400	!*******************************************************************
	! Get output file name
	!*******************************************************************

 !	LINPUT "Name of output file <.TEX> "; LATEX.NAME$
	LATEX.NAME$ = SOURCE.NAME$

	OPEN LATEX.NAME$ FOR OUTPUT AS FILE LATEX.CH%, &
		DEFAULTNAME ".TEX", &
		RECORDSIZE 255%, &
		ALLOW READ

	!*******************************************************************
	! Does the user want index debugging?
	!*******************************************************************
	IDX.DBG% = 0%

	LINPUT "Index debugging on <Y/N> "; DBG.VRBL$

	IDX.DBG% = 1% IF LEFT(EDIT$(DBG.VRBL$, -1%), 1%) = "Y"

	!
	! What is SIC code ?
	!
	LINPUT "SIC code: "; SIC$

	!
	! Only user's manual ?
	!
	LINPUT "Only user manual: "; USERS$

1500	!
	! Get base name of input file
	!
	NAME.BUFFER$ = SOURCE.NAME$

	!
	! Strip off all but the program name
	!
	IO_BUF_W(1%) = FSCN$_NAME
	IO_BUF_W(0%) = 0%
	IO_BUF(1%) = 0%
	IO_BUF(2%) = 0%
	IO_BUF(3%) = 0%
	SYS_STATUS% = SYS$FILESCAN( &
		NAME.BUFFER$ BY DESC, &
		IO_BUF() BY REF, 0%)
	TEMP_LONG% = IO_BUF(1%)
	TEMP1_LONG% = LOC(NAME.BUFFER$)
	TEMP_LONG% = TEMP_LONG% - TEMP1_LONG% + 1%
	SOURCE.BASE$ = MID(NAME.BUFFER$, &
		TEMP_LONG%, IO_BUF_W(0%))

	!
	! replace underscores
	!
	LATEX.SOURCE.NAME$ = EDIT$(SOURCE.NAME$, -1%)
	UNDER% = INSTR(1%, LATEX.SOURCE.NAME$, "_")

	WHILE UNDER%
		LATEX.SOURCE.NAME$ = &
			LEFT(LATEX.SOURCE.NAME$, UNDER% - 1%) + "-" + &
			RIGHT(LATEX.SOURCE.NAME$, UNDER% + 1%)
		UNDER% = INSTR(1%, LATEX.SOURCE.NAME$, "_")
	NEXT
	!
	! Put a header on the output file
	!
	PRINT #LATEX.CH%, "\documentstyle[cmcextra,fancyheadings]{book}"
	PRINT #LATEX.CH%, "\makeindex"

	!
	! Use FANCYHEADINGS to format header/footer
	!
	PRINT #LATEX.CH%, "\pagestyle{fancyplain}"
	PRINT #LATEX.CH%, "\setlength{\headrulewidth}{1.0pt}"
	PRINT #LATEX.CH%, "\setlength{\footrulewidth}{1.0pt}"
	PRINT #LATEX.CH%, "\setlength{\plainfootrulewidth}{1.0pt}"
	PRINT #LATEX.CH%, "\setlength{\parindent}{0.0pt}"
	PRINT #LATEX.CH%, "\setlength{\listparindent}{0.0pt}"
 !	PRINT #LATEX.CH%, "\addtolength{\headwidth}{\marginparsep}"
 !	PRINT #LATEX.CH%, "\addtolength{\headwidth}{\marginparwidth}"
	PRINT #LATEX.CH%, "\renewcommand{\chaptermark}[1]{\markboth" + &
		"{Chapter \thechapter. \  #1}" + &
		"{Chapter \thechapter. \  #1}}"
	PRINT #LATEX.CH%, "\renewcommand{\sectionmark}[1]{\markright{\thesection\ #1}}"
	PRINT #LATEX.CH%, "\lhead[\fancyplain{}{\rm\thepage}]{\fancyplain{}{\sc\rightmark}}"
	PRINT #LATEX.CH%, "\rhead[\fancyplain{}{\sc\rightmark}]{\fancyplain{}{\rm\thepage}}"
	PRINT #LATEX.CH%, "\lfoot[\fancyplain{\rm\thepage}{\rm\thepage}]{\fancyplain{\sc\leftmark}{\sc\leftmark}}"
	PRINT #LATEX.CH%, "\rfoot[\fancyplain{\sc\leftmark}{\sc\leftmark}]{\fancyplain{\rm\thepage}{\rm\thepage}}"
	PRINT #LATEX.CH%, "\cfoot[\fancyplain{}]{}"
	PRINT #LATEX.CH%, "\addtolength{\footskip}{10pt}"
	PRINT #LATEX.CH%, "\newfont{\indexfont}{ccr7}"

	PRINT #LATEX.CH%, "\begin{document}"
	PRINT #LATEX.CH%, "\setcounter{tocdepth}{6}"	! Number everything
	PRINT #LATEX.CH%, "\setcounter{secnumdepth}{6}"
	PRINT #LATEX.CH%, "\sloppy"			! Less extended lines
	PRINT #LATEX.CH%, "\def\menuname{" + LATEX.SOURCE.NAME$ + "}"
	PRINT #LATEX.CH%, "\def\cmcversion{3.5}"
	PRINT #LATEX.CH%, "\def\sic{" + SIC$ + "}"
	PRINT #LATEX.CH%, "\input{cmc/ref/"; SOURCE.BASE$; ".FRONT}"
	PRINT #LATEX.CH%, "\setlength{\parskip}{6pt plus 1pt}"
							! spc btwen pgfs
	PRINT #LATEX.CH%, "\setlength{\parsep}{6pt plus 1pt}"
							! spc btwen pgfs

1600	!*******************************************************************
	! Get list of all modules in the main help file for this system
	!*******************************************************************

	!
	! Get default help library name from name of system
	!
	I% = INSTR(1%, SOURCE.NAME$, "_")
	IF I%
	THEN
		SYSTEM.NAME$ = LEFT(SOURCE.NAME$, I% - 1%)
		MAIN_HELP$ = "HELP_" + SYSTEM.NAME$
	ELSE
		SYSTEM.NAME$ = SOURCE.NAME$
		MAIN_HELP$ = "HELP_DEFAULT"
	END IF

	IF (I% = 0%) OR (INSTR(1%, SOURCE.NAME$, "SYSTEM") = 0%)
	THEN
		GOTO 2000
	END IF

	!
	! Set up the control structure if necessary
	!
	ST% = LBR$INI_CONTROL(LR.INDEX_ALL%, LBR$C_READ)

	IF (ST% AND 1%) = 0%
	THEN
		PRINT "  *** Unable to initilize main library"; ST%
	END IF

	!
	! Open the library function
	!
	ST% = LBR$OPEN(LR.INDEX_ALL%, MAIN_HELP$, , "REF:.TLB")

	IF (ST% AND 1%) = 0%
	THEN
		PRINT "Error in LIB open "; ST%; MAIN_HELP$
		GOTO 2000
	END IF

	!
	! Search for key in file
	!
	MODCOUNTALL% = 0%

	ST% = LBR$GET_INDEX(LR.INDEX_ALL%, 1%, &
		TK_DOCU_GETMODULES_ALL, "*")

	IF (ST% AND 1%) = 0%
	THEN
		PRINT "  *** Error in LIB get index "; ST%; KEY_NAME1$
	END IF

	ST% = LBR$CLOSE(LR.INDEX%)
	CHAPTER% = 0%

2000	!*******************************************************************
	! Scan through file, handling one line at a time
	!*******************************************************************

	QUOTE_FLAG% = 0%	! Which way does the current quote marker point

	!
	! Read in one line
	!
	WHEN ERROR IN
		LINPUT #SOURCE.CH%, INLINE$
	USE
		CONTINUE 3000 IF ERR = 11%
		!IF MESSAGE% = 0% AND ERR = 11%
		!THEN
		!	INLINE$ = ".MES Messages H<HELP<CMC_MESSAGES"
		!	MESSAGE% = -1%
		!	RESUME ReadLine
		!END IF

		EXIT HANDLER
	END WHEN

	GOTO 2000 IF INLINE$ = "" OR LEFT(EDIT$(INLINE$, -1%), 1%) = "!"

 ReadLine:

2200	!*******************************************************************
	! Parse one line
	!*******************************************************************
 ParseLine:

	FULL.LINE$ = INLINE$
	!
	! Pull off dots
	!
	DOT_COUNTER% = 0%
	WHILE MID(INLINE$, DOT_COUNTER% + 1%, 1%) = "."
		DOT_COUNTER% = DOT_COUNTER% + 1%
	NEXT
	INLINE$ = RIGHT(INLINE$, DOT_COUNTER% + 1%)

	GOTO INSERTINDEX IF DOT_COUNTER% AND USERS$ = "Y"
	!
	! Pull apart line
	!
	FIRST_SPACE% = INSTR(1%, INLINE$, " ")

	FIRST_ARROW% = INSTR(FIRST_SPACE% + 1%, INLINE$, ">")
	FIRST_ARROW% = INSTR(FIRST_SPACE% + 1%, INLINE$, "<") &
		IF FIRST_ARROW% = 0%

	IF FIRST_ARROW% = 0%
	THEN
		PRINT "  *** Unable to parse input line from menu '"; INLINE$; "'"
		GOTO 2000
	END IF

	SECOND_ARROW% = INSTR(FIRST_ARROW% + 1%, INLINE$, ">")
	SECOND_ARROW% = INSTR(FIRST_ARROW% + 1%, INLINE$, "<") &
		IF SECOND_ARROW% = 0%

	IF SECOND_ARROW% = 0%
	THEN
		PRINT "  *** Unable to parse input line from menu '"; INLINE$; "'"
		GOTO 2000
	END IF

	!
	! Seperate parts
	!
	MENU_OPTION$ = LEFT(INLINE$, FIRST_SPACE% - 1%)
	MENU_DESCR$ = SEG$(INLINE$, FIRST_SPACE% + 1%, FIRST_ARROW% - 2%)
	MENU_IDENT$ = SEG$(INLINE$, FIRST_ARROW% + 1%, SECOND_ARROW% - 1%)
	MENU_FILE$ = RIGHT(INLINE$, SECOND_ARROW% + 1%)

	IF MID(INLINE$, FIRST_ARROW%, 1%) = ">"
	THEN
		MENU_TYPE$ = "P"
	ELSE
		MENU_TYPE$ = "H"
	END IF

	!
	! get system name if possible
	!
	MENU_SYSTEM$ = TRM$(MENU_OPTION$) IF DOT_COUNTER% = 0%

	!
	! Handle help file for this line
	!
	CALL HELP_MENUHELPKEY(MENU_FILE$, &
		MENU_TYPE$, &
		MENU_SYSTEM$, &
		TLB_IDENT$, &
		TLB_PROGRAM$, &
		TLB_ITEM$, &
		TLB_DEVICE$, &
		1%)

	KEY_NAME_BASE$, KEY_NAME$ = "H$" + TRM$(TLB_PROGRAM$) + "$" + &
				TRM$(TLB_ITEM$)


	DOT_COUNTER% = 1% IF DOT_COUNTER% < 1%

	IF DOT_COUNTER% = 1%
	THEN
		CHAPTER% = CHAPTER% + 1%

		SELECT CHAPTER%

		CASE 1%
			PRINT #LATEX.CH%, "\part{User's Guide}"
			PRINT
			PRINT "--- User's guide part ---"
			PRINT
		CASE 2%
			PRINT #LATEX.CH%, "\part{Reference Manual}"
			PRINT
			PRINT "--- Reference manual part ---"
			PRINT
			PRINT "--- Chapter 2 ---"
		CASE ELSE
			PRINT "--- Chapter " + NUM1$(CHAPTER%) + " ---"
		END SELECT

	END IF

	PRINT #LATEX.CH%, "\vfill\pagebreak[4]"
	PRINT #LATEX.CH%, "%"; FULL.LINE$

	PRINT "-" + EDIT$(INLINE$, 16%)

	CALL TK_SUBR_TEXONEMESSAGE( &
		DOT_COUNTER%, "section-header", &
		MENU_OPTION$ + " - " + MENU_DESCR$, &
		KEY_NAME$, KEY_NAME_BASE$, "", &
		LATEX.CH%, &
		COMMAND_LIST%, COMMAND_LIST$(), COMMAND_LIST%(), &
		GLOSSARY%, GLOSSARY$())


	SELECT CHAPTER%

	CASE 1%
		IF SOURCE.BASE$ = "CMCAPLUS"
		THEN
			PRINT
			PRINT "--- Elementary VMS System Management chapter ---"
			PRINT #LATEX.CH%, "\input{cmc/ref/easy_manage.tex}"
		END IF

		!
		! Getting Started
		!
		KEY_NAME_BASE$, KEY_NAME$ = "H$" + SYSTEM.NAME$ + "$GETSTART"

		PRINT #LATEX.CH%, "\vfill\pagebreak[4]"
		PRINT #LATEX.CH%, "%"; KEY_NAME$

		PRINT "-" + KEY_NAME$

		CALL TK_SUBR_TEXONEMESSAGE( &
			DOT_COUNTER% + 1%, "section", "Getting Started", &
			KEY_NAME$, KEY_NAME_BASE$, "", LATEX.CH%, &
			COMMAND_LIST%, COMMAND_LIST$(), COMMAND_LIST%(), &
			GLOSSARY%, GLOSSARY$())

		!
		! Routine
		!
		KEY_NAME_BASE$, KEY_NAME$ = "H$" + SYSTEM.NAME$ + "$ROUTINES"

		PRINT #LATEX.CH%, "\vfill\pagebreak[4]"
		PRINT #LATEX.CH%, "%"; KEY_NAME$

		PRINT "-" + KEY_NAME$

		CALL TK_SUBR_TEXONEMESSAGE( &
			DOT_COUNTER% + 1%, "section", "Periodic Routines", &
			KEY_NAME$, KEY_NAME_BASE$, "", LATEX.CH%, &
			COMMAND_LIST%, COMMAND_LIST$(), COMMAND_LIST%(), &
			GLOSSARY%, GLOSSARY$())

		!
		! Tips and Tricks
		!
		KEY_NAME_BASE$, KEY_NAME$ = "H$" + SYSTEM.NAME$ + "$TIPS"

		PRINT #LATEX.CH%, "\vfill\pagebreak[4]"
		PRINT #LATEX.CH%, "%"; KEY_NAME$

		PRINT "-" + KEY_NAME$

		CALL TK_SUBR_TEXONEMESSAGE( &
			DOT_COUNTER% + 1%, "section", "Tips", &
			KEY_NAME$, KEY_NAME_BASE$, "", LATEX.CH%, &
			COMMAND_LIST%, COMMAND_LIST$(), COMMAND_LIST%(), &
			GLOSSARY%, GLOSSARY$())
	END SELECT

	!
	! Read in next line
	!
	GOTO 2000

	%PAGE

3000	!*******************************************************************
	! Finish up file
	!*******************************************************************

	!
	! Data file list chapter for CMC document
	!
	IF SOURCE.BASE$ = "CMCAPLUS"
	THEN
		PRINT
		PRINT "--- CMC Data file chapter ---"
		!
		! Print file names
		!
		PRINT #LATEX.CH%, "\input{cmc/ref/datafiles.tex}"

		!
		! Find all help text libraries
		!
		CALL FIND_FILE("REF:HELP_*.TLB", HELP_LIB$(), 16%, "", "")
		HELP_LIB% = VAL%(HELP_LIB$(0%))

		!
		! Look only for FILE suffix
		!
		FOR HL% = 1% TO HELP_LIB%
			PRINT HELP_LIB$(HL%)
			CALL TK_SUBR_TEXONEMESSAGE(2%, "list", "Data Files", &
				"*$FILE", "", "REF:" + HELP_LIB$(HL%), LATEX.CH%, &
				COMMAND_LIST%, COMMAND_LIST$(), COMMAND_LIST%(), &
				GLOSSARY%, GLOSSARY$())
		NEXT HL%

	END IF

	PRINT
	PRINT "--- System messages chapter ---"
	!
	! Print system messages
	!
	PRINT #LATEX.CH%, "\input{cmc/ref/MESSAGES.TEX}"

	UNDER% = INSTR(1%, SOURCE.BASE$ + "_", "_")
	SYS.LIB$ = LEFT(SOURCE.BASE$, UNDER% - 1%)

	CALL TK_SUBR_TEXONEMESSAGE( &
		1%, "list", "Messages", &
		"E$*,F$*,I$*,S$*,W$*", KEY_NAME_BASE$, "REF:HELP_" + SYS.LIB$, &
		LATEX.CH%, &
		COMMAND_LIST%, COMMAND_LIST$(), COMMAND_LIST%(), &
		GLOSSARY%, GLOSSARY$())

	PRINT
	PRINT "--- Appendexis ---"
	!
	! Look for appendexis
	!
	PRINT #LATEX.CH%, "\input{cmc/ref/" + SOURCE.BASE$; ".BACK}"

	!
	! Print out the glossary
	!
	PRINT #LATEX.CH%, "\renewcommand{\chaptermark}[1]{\markboth" + &
		"{Glossary \thechapter. \  #1}" + &
		"{Glossary \thechapter. \  #1}}"
	PRINT #LATEX.CH%, "\chapter*{Glossary}"
	PRINT #LATEX.CH%, "\addcontentsline{toc}{chapter}{Glossary}"
	PRINT #LATEX.CH%, "\markboth{GLOSSARY}{GLOSSARY}"
	PRINT #LATEX.CH%, "\begin{description}"
	GLOSSARY1% = 0%

	PRINT
	PRINT "--- Glossary terms ---"
	FOR I% = 1% TO GLOSSARY%
		!
		! Print glossary
		!
		CALL TK_SUBR_TEXONEMESSAGE( &
			2%, "glossary", "Glossary", &
			GLOSSARY$(I%), "GLOSSARY", "REF:GLOSSARY", &
			LATEX.CH%, &
			COMMAND_LIST%, COMMAND_LIST$(), COMMAND_LIST%(), &
			GLOSSARY1%, GLOSSARY1$())
	NEXT I%

	PRINT #LATEX.CH%, "\end{description}"
	PRINT #LATEX.CH%, "\cleardoublepage"

 InsertIndex:
	!
	! Finish up the document by printing the index, and closing
	! the document.
	!
	PRINT #LATEX.CH%, "\insertindex"
	PRINT #LATEX.CH%, "\end{document}"

	ST% = LBR$CLOSE(WIN.INDEX%)
	CLOSE LATEX.CH%
	CLOSE SOURCE.CH%

	GOTO ExitProgram IF USERS$ = "Y"
	!
	! Print out to the log any possibly missed data in the
	! main library.
	!
 !	PRINT
 !	PRINT "  *** Not used text for this document ***"
 !	FOR I% = 1% TO MODCOUNTALL%
 !		PRINT MODNAMEALL$(I%) IF MODFLAGALL%(I%) = 0%
 !	NEXT I%
 !	PRINT

 ExitProgram:
	GOTO 20000

	!*******************************************************************
	! Print level titles
	!*******************************************************************

 !	DEF FNLEVEL$(DOT_COUNTER%, TITLE$)
 !
 !	SELECT DOT_COUNTER%
 !
	!
	! Start of a chapter
	!
 !	CASE 0%, 1%
 !		FNLEVEL$ = "\chapter {" + TK_FUNC_TEXSTRING(TITLE$) + "}"
 !
	!
	! Start of a section
	!
 !	CASE 2%
 !		FNLEVEL$ = "\pagebreak[3]\section {" + &
 !			TK_FUNC_TEXSTRING(TITLE$) + "}"
 !
	!
	! Start of a subsection
	!
 !	CASE 3%
 !		FNLEVEL$ = "\pagebreak[2]\subsection {" + &
 !			TK_FUNC_TEXSTRING(TITLE$) + "}"

	!
	! Start of a subsubsection
	!
 !	CASE 4%
 !		FNLEVEL$ = "\pagebreak[1]\subsubsection {" + &
 !			TK_FUNC_TEXSTRING(TITLE$) + "}"
 !
	!
	! Start of a paragraph
	!
 !	CASE 5%
 !		FNLEVEL$ = "\paragraph {" + TK_FUNC_TEXSTRING(TITLE$) + "}"
 !
	!
	! Start of a subparagraph
	!
 !	CASE ELSE
 !		FNLEVEL$ = "\subparagraph {" + TK_FUNC_TEXSTRING(TITLE$) + "}"
 !
 !	END SELECT
 !
 !	FNEND

20000	END

21100	FUNCTION LONG TK_DOCU_GETMODULES_ALL(MODKEY$, RFA MODRFA)

	!
	! This function graps the names passed to it from the
	! LIB$SEARCH call
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	MAP (TK_DOCUALL) &
		MODCOUNTALL%, &
		MODNAMEALL$(1500%) = 50%, &
		MODFLAGALL%(1500%)

	!
	! Don't add name if already have a key to that item,
	! or we have amassed too many items already.
	!
	GOTO ExitFunction &
		IF MODCOUNTALL% >= 1500%

	!
	! Add to list
	!
	MODCOUNTALL% = MODCOUNTALL% + 1%
	MODNAMEALL$(MODCOUNTALL%) = MODKEY$
	MODFLAGALL%(MODCOUNTALL%) = 0%


 ExitFunction:
	TK_DOCU_GETMODULES_ALL = 1%

	END FUNCTION
