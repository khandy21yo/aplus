1	%TITLE "TK_DOCU_MODULE - Document a program module"
	%SBTTL "DOCUMENTATION"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1987 BY
	! Computer Management Center, Inc.
	! Idaho Falls, Idaho  83401.
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
	! Abstract:
	!	^*TK_DOCU_MODULE - Document a program module\*
	!	.p
	!	This program creates documentation (TeX format) from
	!	source code (.BAS).  It does this by reading the
	!	file containing the source code and parsing out the
	!	desired information.
	!	.p
	!	An example of a command file to run this program is as
	!	follows:
	!	.b
	!	.literal
	!	$ RUN TK_EXE:TK_DOCU_MODULE
	!	TEMP.TEX
	!	U
	!	Functions
	!	FUNC_SOURCE:*.BAS
	!	$ LATEX TEMP
	!	$ idxtex temp /toc=report
	!	$ LATEX TEMP
	!	$ DVIJEP -B TEMP
	!	.end literal
	!
	! Index:
	!	.x Document>Programs
	!	.x Programs>Document
	!	.x TeX>Programs
	!	.x LaTeX>Programs
	!	.x Programs>TeX
	!	.x Programs>LaTeX
	!
	! Input:
	!
	! Output:
	!
	!	Creates a TeX format file.
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_DOCU_MODULE
	!	$ LINK/EXE=TK_EXE: TK_DOCU_MODULE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE TK_DOCU_MODULE.OBJ;*
	!
	! Author:
	!
	!	10/21/87 - Kevin Handy
	!
	! Modification history:
	!
	!	10/15/90 - Kevin Handy
	!		Modifications to change format of output.
	!
	!	10/15/90 - Kevin Handy
	!		Modified to use RUNOFF input.
	!
	!	10/16/90 - Kevin Handy
	!		Added Index.
	!
	!	08/27/91 - Kevin Handy
	!		Modifications to handle C.
	!
	!	09/09/91 - Kevin Handy
	!		Modifications to generation of titles.
	!
	!	09/09/91 - Kevin Handy
	!		More modifications to handle C better.
	!
	!	07/27/92 - Kevin Handy
	!		Removed "fullpage" style since I have included
	!		it in the "cmcextra" style.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/29/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/08/99 - Kevin Handy
	!		Use BASIC$STARLET for LIB$ and STR$ routines
	!
	!	04/13/99 - Kevin Handy
	!		Change a 'CONTEXT' to 'CONTEXT%'
	!
	!	12/13/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	05/16/2003 - Kevin Handy
	!		Fixed to run LaTeX under Linux
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "STR$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	EXTERNAL LONG FUNCTION SYS$FILESCAN
	EXTERNAL STRING FUNCTION MAKETEX

	DECLARE LONG CONSTANT FSCN$_NAME = 6

	DECLARE LONG SYS_STATUS

	MAP (Z) NAME_BUFFER$ = 255%

	!
	! Arrays for delayed items
	!
	DIM HOLD_FUN$(100%)
	DIM HOLD_SUB$(100%)
	DIM HOLD_CDD$(100%)

	DIM HOLD_ABSTRACT$(500%)
	DIM HOLD_INPUT$(500%)
	DIM HOLD_OUTPUT$(500%)
	DIM HOLD_PARAM$(500%)
	DIM HOLD_EXAMPLE$(500%)
	DIM HOLD_ENVIOR$(500%)
	DIM HOLD_COMPILE$(500%)
	DIM HOLD_AUTHOR$(500%)
	DIM HOLD_MODIF$(500%)
	DIM HOLD_VARIABLE$(1000%)
	DIM HOLD_DEF$(500%)
	DIM HOLD_INDEX$(500%)

	DECLARE INTEGER CONSTANT MAX_COMMAND = 100
	DIM COMMAND_LIST$(MAX_COMMAND),	COMMAND_LIST%(MAX_COMMAND)

	!
	! Create a couple of buffers
	!
	MAP (IOBUF) LONG IO_BUF(6%)
	MAP (IOBUF) WORD IO_BUF_W(12%)
	MAP (IOBUF1) NAME_BUFFER1$ = 50%

	%PAGE

	!*******************************************************************
	! Allocate specific channels
	!*******************************************************************

	LATEX.CH% = 5%
	READ_FILE.CH% = 6%
	FUN_FILE.CH% = 7%
	ABSTRACT.CH% = 8%

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
		19,	".INDEX ", &
		19,	".X ", &
		20,	".TEST PAGE ", &
		20,	".TESTPAGE ", &
		20,	".TP ", &
		26,	".P ", &
		26,	".PARAGRAPH ", &
		48,	".LITERAL ", &
		48,	".LT ", &
		49,	".END LITERAL ", &
		49,	".ENDLITERAL ", &
		49,	".EL ", &
		54,	".!!", &
		55,	".LM ", &
		55,	".LEFT MARGIN ", &
		55,	".LEFTMARGIN ", &
		56,	".!.GLOSSARY", &
		56,	".ENTRY", &
		56,	".Y", &
		0,	""

100	!*******************************************************************
	! Get name for output file
	!*******************************************************************

	LINPUT "Base name of TeX file to build <TEMP.TEX> "; COM_FILE$

110	!*******************************************************************
	! Ask for long or short form
	!*******************************************************************

	LINPUT "User (U) or Full (F) documentation <U> "; HOWMUCH$

	HOWMUCH$ = LEFT(EDIT$(HOWMUCH$, -1%), 1%)

	!*******************************************************************
	! Ask for title of manual
	!*******************************************************************

	LINPUT "Title for book "; USER_TITLE$

	IF USER_TITLE$ = ""
	THEN
		IF HOWMUCH$ = "F"
		THEN
			USER_TITLE$ = "Module Technical Documentation"
		ELSE
			USER_TITLE$ = "Module User Documentation"
		END IF
	END IF

	USER_TITLE$ = MAKETEX(USER_TITLE$)

120	!*******************************************************************
	! Create dummy index file, so that we will not need three passes
	!*******************************************************************

	INDEX.NAME$ = COM_FILE$
	I% = INSTR(1%, INDEX.NAME$, ".")
	INDEX.NAME$ = LEFT(INDEX.NAME$, I% - 1%) IF I%

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

130	!*******************************************************************
	! Initilize the start of the output file
	!*******************************************************************

	OPEN COM_FILE$ FOR OUTPUT AS FILE LATEX.CH%, &
		DEFAULTNAME "TEMP.TEX", &
		RECORDSIZE 132%, &
		ACCESS WRITE, &
		ALLOW READ

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
	PRINT #LATEX.CH%, "\def\usertitle{";USER_TITLE$;"}"
	PRINT #LATEX.CH%, "\def\menuname{?}"
	PRINT #LATEX.CH%, "\def\cmcversion{3.5}"
	PRINT #LATEX.CH%, "\def\sic{?}"
	PRINT #LATEX.CH%, "\input{!/cmc/ref/MODULE.FRONT}"
	PRINT #LATEX.CH%, "\setlength{\parskip}{6pt plus 1pt}"
							! spc btwen pgfs
	PRINT #LATEX.CH%, "\setlength{\parsep}{6pt plus 1pt}"
							! spc btwen pgfs


140	!*******************************************************************
	! Open up abstract file
	!*******************************************************************

	OPEN "REF:MODULE.ABSTRACT" FOR INPUT AS FILE ABSTRACT.CH%, &
		ACCESS READ, &
		ALLOW MODIFY

200	!*******************************************************************
	! Ask for wildcard file name
	!*******************************************************************

	WHEN ERROR IN
		LINPUT "Wildcard name to build for <.BAS> (Return to exit)"; &
			FILE_WILDCARD$
	USE
		FILE_WILDCARD$ = ""
		CONTINUE 210
	END WHEN


210	IF FILE_WILDCARD$ = ""
	THEN
		PRINT #LATEX.CH%, "\input{cmc/ref/" + SOURCE.BASE$; ".BACK}"
		PRINT #LATEX.CH%, "\insertindex"
		PRINT #LATEX.CH%, "\end{document}"
		GOTO ExitProgram
	END IF

	CONTEXT% = 0%

	JUNK$ = "*.*"

300	!*******************************************************************
	! Look up and process one file
	!*******************************************************************

	SYS_STATUS = LIB$FIND_FILE(FILE_WILDCARD$, NAME_BUFFER$, &
		CONTEXT%, JUNK$)

	JUNK$ = ""

	!
	! Finish up when no more
	!
	IF (SYS_STATUS AND 1%) = 0%
	THEN
		GOTO 200
	END IF

	!
	! Do one file
	!
	FILE_NAME$ = TRM$(NAME_BUFFER$)

	!
	! See if it is a valid program type
	!
	I% = 0%
	I% = INSTR(I% + 1%, FILE_NAME$, ".") &
		WHILE INSTR(I% + 1%, FILE_NAME$, ".")
	FTYPE$ = RIGHT(FILE_NAME$, I% + 1%)
	I% = INSTR(1%, FTYPE$, ";")
	FTYPE$ = LEFT(FTYPE$, I% - 1%) IF I%

	SELECT FTYPE$

	CASE "BAS"
		!
		! Do this file
		!
		COMCHR$ = "!"
		PRINT "Starting "; FILE_NAME$
		GOSUB ProcessOneFile

	CASE "C"
		!
		! Do this file
		!
		COMCHR$ = "*"
		PRINT "Starting "; FILE_NAME$
		GOSUB ProcessOneFile
	END SELECT

	!
	! Set up for next file
	!
	GOTO 300

	%PAGE

4000	!*******************************************************************
	! Process one file
	!*******************************************************************

 ProcessOneFile:

	OPEN FILE_NAME$ FOR INPUT AS FILE READ_FILE.CH%, &
		ACCESS READ, &
		ALLOW MODIFY

4010	!
	! Strip off all but the program name
	!
	NAME_BUFFER1$ = FILE_NAME$
	IO_BUF_W(1%) = FSCN$_NAME

	IO_BUF_W(0%) = 0%
	IO_BUF(1%) = 0%
	IO_BUF(2%) = 0%
	IO_BUF(3%) = 0%
	SYS_STATUS% = SYS$FILESCAN(NAME_BUFFER1$ BY DESC, &
		IO_BUF() BY REF, 0%)
	TEMP_LONG% = IO_BUF(1%)
	TEMP1_LONG% = LOC(NAME_BUFFER1$)
	TEMP_LONG% = TEMP_LONG% - TEMP1_LONG% + 1%
	SECTION_NAME$ = MID(NAME_BUFFER1$, TEMP_LONG%, IO_BUF_W(0%))

	!
	! Init counts
	!
	HOLD_FUN% = 0%
	HOLD_SUB% = 0%
	HOLD_CDD% = 0%

	HOLD_ABSTRACT% = 0%
	HOLD_INPUT% = 0%
	HOLD_OUTPUT% = 0%
	HOLD_PARAM% = 0%
	HOLD_EXAMPLE% = 0%
	HOLD_ENVIOR% = 0%
	HOLD_COMPILE% = 0%
	HOLD_AUTHOR% = 0%
	HOLD_MODIF% = 0%
	HOLD_VARIABLE% = 0%
	HOLD_DEF% = 0%
	HOLD_INDEX% = 0%

	EOF_FLAG% = 0%
	COM_FLAG% = -1%

	MASTER_TITLE$ = ""

4100	!
	! Search for something intresting in file
	!
	GOSUB ReadOneLine
	GOTO 4500 IF EOF_FLAG%

	INLINE$ = RIGHT(INLINE$, 2%) IF LEFT(INLINE$, 1%) = "1"

4110	TEMP$ = EDIT$(INLINE$, -1%)

	GOTO 4100 IF INLINE$ = ""

	GOTO 4150 IF (COM_FLAG% <> 0%) OR (LEFT(TEMP$, 1%) <> COMCHR$)

	COMTXT$ = RIGHT(TEMP$, 2%)

	!
	! Strip of any colons.
	!
	IF RIGHT(COMTXT$, LEN(COMTXT$)) = ":"
	THEN
		COMTXT$ = LEFT(COMTXT$, LEN(COMTXT$) - 1%)
	END IF

	!
	! Make Singular.
	!
	IF RIGHT(COMTXT$, LEN(COMTXT$)) = "S"
	THEN
		COMTXT$ = LEFT(COMTXT$, LEN(COMTXT$) - 1%)
	END IF

	SELECT COMTXT$

	CASE "ABSTRACT", "ABSTRACT:HELP"

		IF (HOLD_ABSTRACT% = 0%)
		THEN
			CALL LOADBLOCKCOMMENT(READ_FILE.CH%, &
				HOLD_ABSTRACT$(), HOLD_ABSTRACT%, INLINE$, ftype$)
			GOTO 4110
		END IF

	CASE "INDEX"

		IF (HOLD_INDEX% = 0%)
		THEN
			CALL LOADBLOCKCOMMENT(READ_FILE.CH%, &
				HOLD_INDEX$(), HOLD_INDEX%, INLINE$, ftype$)
			GOTO 4110
		END IF

	CASE "INPUT"

		CALL LOADBLOCKCOMMENT(READ_FILE.CH%, &
			HOLD_INPUT$(), HOLD_INPUT%, INLINE$, ftype$)
		GOTO 4110

	CASE "OUTPUT"

		CALL LOADBLOCKCOMMENT(READ_FILE.CH%, &
			HOLD_OUTPUT$(), HOLD_OUTPUT%, INLINE$, ftype$)
		GOTO 4110

	CASE "PARAMETER"

		CALL LOADBLOCKCOMMENT(READ_FILE.CH%, &
			HOLD_PARAM$(), HOLD_PARAM%, INLINE$, ftype$)
		GOTO 4110

	CASE "EXAMPLE"

		CALL LOADBLOCKCOMMENT(READ_FILE.CH%, &
			HOLD_EXAMPLE$(), HOLD_EXAMPLE%, INLINE$, ftype$)
		GOTO 4110

	CASE "ENVIRONMENT"
		CALL LOADBLOCKCOMMENT(READ_FILE.CH%, &
			HOLD_ENVIOR$(), HOLD_ENVIOR%, INLINE$, ftype$)
		GOTO 4110

	CASE "COMPILE"

		CALL LOADBLOCKCOMMENT(READ_FILE.CH%, &
			HOLD_COMPILE$(), HOLD_COMPILE%, INLINE$, ftype$)
		GOTO 4110

	CASE "AUTHOR"

		CALL LOADBLOCKCOMMENT(READ_FILE.CH%, &
			HOLD_AUTHOR$(), HOLD_AUTHOR%, INLINE$, ftype$)
		GOTO 4110

	CASE "MODIFICATIONHISTORY"

		CALL LOADBLOCKCOMMENT(READ_FILE.CH%, &
			HOLD_MODIF$(), HOLD_MODIF%, INLINE$, ftype$)
		GOTO 4110

	CASE "VARIABLE"

		CALL LOADBLOCKCOMMENT(READ_FILE.CH%, &
			HOLD_VARIABLE$(), HOLD_VARIABLE%, INLINE$, ftype$)
		GOTO 4110

	CASE "--"

		COM_FLAG% = -1%
		GOTO 4100

	END SELECT

4150	IF INSTR(1%, LEFT(TEMP$, 12%), "%TITLE")
	THEN
		I% = INSTR(1%, TEMP$, '"')
		I1% = INSTR(I% + 1%, TEMP$, '"')
		MASTER_TITLE$ = SEG$(TEMP$, I% + 1%, I1% - 1%)
 !		PRINT #LATEX.CH%, "\markright{" + &
 !			MAKETEX(MASTER_TITLE$) + "}"
		GOTO 4100
	END IF

	IF TEMP$ = COMCHR$ + "++"
	THEN
		COM_FLAG% = 0%
		GOTO 4100
	END IF

	IF TEMP$ = COMCHR$
	THEN
		GOTO 4100
	END IF

	STRIP$ = EDIT$(INLINE$, 8% + 16% + 32%)

	!
	! Handle definition of function/sub
	!
	SELECT FTYPE$

	CASE "BAS"
		IF ((LEFT(STRIP$, 4%) = "SUB ") OR (LEFT(STRIP$, 9%) = "FUNCTION ")) AND &
			(HOLD_DEF% = 0%)
		THEN
			HOLD_DEF% = 1%
			HOLD_DEF$(1%) = INLINE$

			WHILE RIGHT(HOLD_DEF$(HOLD_DEF%), LEN(HOLD_DEF$(HOLD_DEF%))) = "&"

				GOSUB ReadOneLine
				INLINE$ = RIGHT(INLINE$, 2%) IF LEFT(INLINE$, 1%) = '9'C
				HOLD_DEF% = HOLD_DEF% + 1%
				HOLD_DEF$(HOLD_DEF%) = INLINE$

			NEXT

			GOTO 4100
		END IF

	CASE "C"
		IF ((LEFT(STRIP$, 4%) = "int ") OR (LEFT(STRIP$, 5%) = "void ")) AND &
			(HOLD_DEF% = 0%)
		THEN
			HOLD_DEF% = 1%
			HOLD_DEF$(1%) = INLINE$

			WHILE RIGHT(HOLD_DEF$(HOLD_DEF%), LEN(HOLD_DEF$(HOLD_DEF%))) <> ")"

				GOSUB ReadOneLine
				INLINE$ = RIGHT(INLINE$, 2%) IF LEFT(INLINE$, 1%) = '9'C
				HOLD_DEF% = HOLD_DEF% + 1%
				HOLD_DEF$(HOLD_DEF%) = INLINE$

			NEXT

			GOTO 4100
		END IF


	END SELECT

	GOSUB StripDownLine		! Strip off junk from line

	IF LEFT(STRIP$, 8%) = "EXTERNAL "
	THEN
		GROUP% = 5%
		GROUP$ = "External Functions"
		GOSUB AnExternalFun
		GOTO 4100
	END IF

	IF LEFT(STRIP$, 5%) = "CALL "
	THEN
		GROUP% = 5%
		GROUP$ = "External Subroutines"
		GOSUB AnExternalSub
		GOTO 4100
	END IF

	IF LEFT(STRIP$, 19%) = "%INCLUDE %FROM %CDD"
	THEN
		GOSUB AnCdd
		GOTO 4100
	END IF

	GOTO 4100

4500	!*******************************************************************
	! End of this file
	!*******************************************************************

	I% = INSTR(4%, SECTION_NAME$, "_")
	I% = LEN(SECTION_NAME$) + 1% IF I% = 0%
	TEST$ = LEFT(SECTION_NAME$, I% - 1%)
	IF TEST$ <> CURRENT_CHAPTER$
	THEN
		CURRENT_CHAPTER$ = TEST$
		GOSUB LoadAbstract
	END IF

	PRINT #LATEX.CH%, "\clearpage"
	PRINT #LATEX.CH%, "\section {"; MAKETEX(SECTION_NAME$); "}"

	IF (HOLD_ABSTRACT% = 0%) AND (MASTER_TITLE$ <> "")
	THEN
		HOLD_ABSTRACT% = 1%
		HOLD_ABSTRACT$(1%) = MASTER_TITLE$ + "..."
	END IF

	!
	! Dump out index entries
	!
	IF HOLD_INDEX%
	THEN
		FOR I% = 1% TO HOLD_INDEX%
			SELECT EDIT$(LEFT(HOLD_INDEX$(I%), 2%), -1%)

			CASE ".X"
				TEMP$ = &
					MAKETEX(EDIT$(RIGHT(HOLD_INDEX$(I%), &
					3%), 8% + 128%))
				WHILE INSTR(1%, TEMP$, "$>$")
					J% = INSTR(1%, TEMP$, "$>$")
					TEMP$ = LEFT(TEMP$, J% - 1%) + ">" + &
						RIGHT(TEMP$, J% + 3%)
				NEXT
				PRINT #LATEX.CH%, "\index{"; &
					TEMP$; &
					"}";
			CASE ".Y"
				GLOSSARY% = GLOSSARY% + 1%
				GLOSSARY$(GLOSSARY%) = &
					EDIT$(RIGHT(HOLD_INDEX$(I%), 3%), &
					8% + 128%)

			CASE ""

			CASE ELSE
				PRINT "  *** Invalid index line: '"; HOLD_INDEX$(I%); "'."

			END SELECT
		NEXT I%
	END IF

4510	IF HOLD_ABSTRACT%
	THEN
		CALL TK_SUBR_TEXONEARRAY( &
			HOLD_ABSTRACT%,		! Input Array &
			HOLD_ABSTRACT$(),	! Input Array &
			LATEX.CH%,		! Where to write out documentation &
			COMMAND_LIST%,		! Number of known commands &
			COMMAND_LIST$(),	! Known command names &
			COMMAND_LIST%(),	! Parsed command names &
			GLOSSARY%,		! Number if items in glossarry &
			GLOSSARY$()		! Glossary items &
		)
	END IF

4520	IF HOLD_DEF%
	THEN
		CALL WRITETEXTV(LATEX.CH%, &
			HOLD_DEF$(), HOLD_DEF%, "Definition")
	END IF

	IF HOLD_INPUT%
	THEN
		CALL WRITETEXTL(LATEX.CH%, &
			HOLD_INPUT$(), HOLD_INPUT%, "Inputs")
	END IF

	IF HOLD_OUTPUT%
	THEN
		CALL WRITETEXTL(LATEX.CH%, &
			HOLD_OUTPUT$(), HOLD_OUTPUT%, "Outputs")
	END IF

	IF HOLD_PARAM%
	THEN
		CALL WRITETEXTL(LATEX.CH%, &
			HOLD_PARAM$(), HOLD_PARAM%, "Parameters")
	END IF

	IF HOLD_EXAMPLE%
	THEN
		CALL WRITETEXTV(LATEX.CH%, &
			HOLD_EXAMPLE$(), HOLD_EXAMPLE%, "Example")
	END IF

	IF (HOLD_ENVIOR% <> 0%) AND (HOWMUCH$ = "F")
	THEN
		CALL WRITETEXTL(LATEX.CH%, &
			HOLD_ENVIOR$(), HOLD_ENVIOR%, "Environment")
	END IF

	IF (HOLD_COMPILE% <> 0%) AND (HOWMUCH$ = "F")
	THEN
		CALL WRITETEXTV(LATEX.CH%, &
			HOLD_COMPILE$(), HOLD_COMPILE%, "Compile")
	END IF

	IF (HOLD_AUTHOR% <> 0%) AND (HOWMUCH$ = "F")
	THEN
		CALL WRITETEXTL(LATEX.CH%, &
			HOLD_AUTHOR$(), HOLD_AUTHOR%, "Author")
	END IF

	IF (HOLD_MODIF% <> 0%) AND (HOWMUCH$ = "F")
	THEN
		CALL WRITETEXTL(LATEX.CH%, &
			HOLD_MODIF$(), HOLD_MODIF%, "Modification History")
	END IF

	IF (HOLD_VARIABLE% <> 0%) AND (HOWMUCH$ = "F")
	THEN
		CALL WRITETEXTL(LATEX.CH%, &
			HOLD_VARIABLE$(), HOLD_VARIABLE%, "Variables")
	END IF

	!
	! Dump CDD records out
	!
	IF (HOLD_CDD% <> 0%) AND (HOWMUCH$ = "F")
	THEN
		PRINT #LATEX.CH%, "\subsection*{CDD Records}"
		PRINT #LATEX.CH%, "\begin{itemize}"
		FOR TEMP% = 1% TO HOLD_CDD%
			FUN_NAME$ = HOLD_CDD$(TEMP%)
			PRINT #LATEX.CH%, "\item "; MAKETEX(FUN_NAME$)
		NEXT TEMP%
		PRINT #LATEX.CH%, "\end{itemize}"
	END IF

	!
	! Dump functions out
	!
	IF (HOLD_FUN% <> 0%) AND (HOWMUCH$ = "F")
	THEN
		PRINT #LATEX.CH%, "\subsection*{External Functions}"
		PRINT #LATEX.CH%, "\begin{itemize}"
		FOR TEMP% = 1% TO HOLD_FUN%
			FUN_NAME$ = HOLD_FUN$(TEMP%)
			GOSUB FunDocu
		NEXT TEMP%
		PRINT #LATEX.CH%, "\end{itemize}"
	END IF

	!
	! Dump subroutines out
	!
	IF (HOLD_SUB% <> 0%) AND (HOWMUCH$ = "F")
	THEN
		PRINT #LATEX.CH%, "\subsection*{External Subroutines}"
		PRINT #LATEX.CH%, "\begin{itemize}"
		FOR TEMP% = 1% TO HOLD_SUB%
			FUN_NAME$ = HOLD_SUB$(TEMP%)
			GOSUB FunDocu
		NEXT TEMP%
		PRINT #LATEX.CH%, "\end{itemize}"
	END IF

	PRINT #LATEX.CH%, "\newpage"

	RETURN

	%PAGE

	!*******************************************************************
	! Handle external functions
	!*******************************************************************

 AnExternalFun:

	IF HOWMUCH$ <> "F"
	THEN
		GOSUB SkipOverStuff
		GOTO AnExternalFunIgnore
	END IF

	TEMP% = INSTR(1%, STRIP$, "FUNCTION")
	RETURN IF TEMP% = 0%

	TEMP1$ = EDIT$(RIGHT(STRIP$, TEMP% + 8%), 2% + 32%)

 AnExternalFunLoop:
	TEMP% = STR$FIND_FIRST_IN_SET(TEMP1$, ",(")

	IF TEMP%
	THEN
		TEMP$ = LEFT(TEMP1$, TEMP% - 1%)
		TEMP1$ = RIGHT(TEMP1$, TEMP%)
		IF LEFT(TEMP1$, 1%) = "("
		THEN
			TEMP% = INSTR(2%, TEMP1$, ")")
			TEMP% = LEN(TEMP1$) IF TEMP% = 0%
			TEMP1$ = RIGHT(TEMP1$, TEMP% + 1%)
		END IF

		GOSUB AnExternalFun1

		TEMP1$ = RIGHT(TEMP1$, 2%) IF LEFT(TEMP1$, 1%) = ","
		GOTO AnExternalFunLoop IF TEMP1$ <> ""
	ELSE
		TEMP$ = TEMP1$
		TEMP1$ = ""
		GOSUB AnExternalFun1
	END IF

 AnExternalFunIgnore:
	RETURN

 AnExternalFun1:
	RETURN IF TEMP$ = HOLD_FUN$(TEMP%) FOR TEMP% = 1% TO HOLD_FUN%

	HOLD_FUN% = HOLD_FUN% + 1%
	HOLD_FUN$(HOLD_FUN%) = TEMP$

	RETURN

	!*******************************************************************
	! Handle external SUBROUTINES
	!*******************************************************************

 AnExternalSub:

	IF HOWMUCH$ <> "F"
	THEN
		GOSUB SkipOverStuff
		GOTO AnExternalSubIgnore
	END IF

	TEMP$ = EDIT$(RIGHT(STRIP$, 5%), 2% + 32%)
	TEMP% = INSTR(1%, TEMP$, "(")
	TEMP$ = LEFT(TEMP$, TEMP% - 1%) IF TEMP%

	RETURN IF TEMP$ = HOLD_SUB$(TEMP%) FOR TEMP% = 1% TO HOLD_SUB%

	HOLD_SUB% = HOLD_SUB% + 1%
	HOLD_SUB$(HOLD_SUB%) = TEMP$

 AnExternalSubIgnore:
	RETURN

	!*******************************************************************
	! Handle CDD's
	!*******************************************************************

 AnCdd:

	IF HOWMUCH$ <> "F"
	THEN
		GOSUB SkipOverStuff
		GOTO AnCddIgnore
	END IF

	TEMP$ = INLINE$
	TEMP% = INSTR(1%, TEMP$, '"')
	TEMP$ = RIGHT(TEMP$, TEMP% + 1%) IF TEMP%
	TEMP% = INSTR(1%, TEMP$, '"')
	TEMP$ = LEFT(TEMP$, TEMP% - 1%) IF TEMP%

	TEMP$ = EDIT$(TEMP$, 2% + 32%)

	RETURN IF TEMP$ = HOLD_CDD$(TEMP%) FOR TEMP% = 1% TO HOLD_CDD%

	HOLD_CDD% = HOLD_CDD% + 1%
	HOLD_CDD$(HOLD_CDD%) = TEMP$

 AnCddIgnore:
	RETURN

	%PAGE

10000	!*******************************************************************
	! Read one line from source file
	!*******************************************************************

 ReadOneLine:
	WHEN ERROR IN
		LINPUT #READ_FILE.CH%, INLINE$
	USE
		EOF_FLAG% = -1%
		CONTINUE 10020
	END WHEN

10020	RETURN

	%PAGE

10100	!*******************************************************************
	! Strip all unecessiencal information off of a line
	!*******************************************************************

 StripDownLine:

	!
	! Remove all excess spaces, for everything to upper case
	!
	STRIP$ = EDIT$(INLINE$, 8% + 16% + 32% + 128% + 256%)
	TEMP% = 0%

10110	!
	! Remove all strings and comments
	!
	TEMPOLD% = TEMP% + 1%
	TEMP% = STR$FIND_FIRST_IN_SET(RIGHT(STRIP$, TEMPOLD%), "'!" + '"')

	IF TEMP%
	THEN
		TEMP% = TEMP% + TEMPOLD% - 1%
		TEMP1$ = MID(STRIP$, TEMP%, 1%)
		TEMP1% = INSTR(TEMP% + 1%, STRIP$, TEMP1$)
		TEMP1% = LEN(STRIP$) If TEMP1% = 0%
		STRIP$ = LEFT(STRIP$, TEMP% - 1%) + " " + RIGHT(STRIP$, TEMP1% + 1%)

		GOTO 10110
	END IF

	!
	! Done
	!
	RETURN

	%PAGE

	!*******************************************************************
	! Skip over unused information
	!*******************************************************************

 SkipOverStuff:

	GOSUB ReadOneLine

	IF FTYPE$ = "BAS"
	THEN
		GOTO SkipExit IF LEFT(INLINE$, 2%) <> '9'C + "!"
		GOTO SkipExit IF LEFT(INLINE$, 3%) == '9'C + "! "
		GOTO SkipExit IF TRM$(LEFT(INLINE$, 3%)) <> '9'C + "!"
	ELSE
		GOTO SkipExit IF LEFT(INLINE$, 2%) <> " *"
		GOTO SkipExit IF LEFT(INLINE$, 3%) == " * "
		GOTO SkipExit IF TRM$(LEFT(INLINE$, 3%)) <> " *"
	END IF

	GOTO SkipOverStuff

 SkipExit:
	RETURN

	%PAGE

12000	!*******************************************************************
	! Document one function/subroutine
	!*******************************************************************
 FunDocu:

	PRINT #LATEX.CH%, "\item "; MAKETEX(FUN_NAME$);

12010	!
	! Try to open source of function on SMG:
	!
	WHEN ERROR IN
		OPEN "FUNC_SOURCE:" + FUN_NAME$ + ".BAS" FOR INPUT AS FILE FUN_FILE.CH%, &
			ACCESS READ, &
			ALLOW MODIFY
	USE
		CONTINUE 12015
	END WHEN

	GOTO 12050

12015	!
	! Try to open source of function on SMG:
	!
	WHEN ERROR IN
		OPEN "FUNC_SOURCE:" + FUN_NAME$ + ".BAS" FOR INPUT AS FILE FUN_FILE.CH%, &
			ACCESS READ, &
			ALLOW MODIFY
	USE
		CONTINUE 12020
	END WHEN

	GOTO 12050

12020	!
	! Try the account pointed to by the part of the name before the
	! underscore
	!
	LU% = INSTR(1%, FUN_NAME$, "_")
	GOTO 12090 IF LU% <= 1%

	WHEN ERROR IN
		OPEN "SOURCE:[" + LEFT(FUN_NAME$, LU% - 1%) + ".SOURCE]" + FUN_NAME$ + &
			".BAS" AS FILE FUN_FILE.CH%, &
			ACCESS READ, &
			ALLOW MODIFY
	USE
		CONTINUE 12090
	END WHEN

	GOTO 12050

12050	!
	! Read in first few lines and search for a %TITLE command
	!
	FOR ZZ% = 1% TO 5%
		WHEN ERROR IN
			LINPUT #FUN_FILE.CH%, INLINE$
		USE
			CONTINUE 12080
		END WHEN

		LU% = INSTR(1%, INLINE$, "%TITLE")

		IF LU%
		THEN
			INLINE$ = EDIT$(RIGHT(INLINE$, LU% + 6%), 8% + 128%)
			INLINE$ = RIGHT(INLINE$, 2%) &
				IF LEFT(INLINE$, 1%) = '"'
			INLINE$ = LEFT(INLINE$, LEN(INLINE$) - 1%) &
				IF RIGHT(INLINE$, LEN(INLINE$)) = '"'
			INLINE$ = RIGHT(INLINE$, LEN(FUN_NAME$) + 1%) &
				IF LEFT(INLINE$, LEN(FUN_NAME$)) = FUN_NAME$
			INLINE$ = EDIT$(INLINE$, 8% + 128%)
			INLINE$ = RIGHT(INLINE$, 2%) &
				IF LEFT(INLINE$, 1%) = "-"
			PRINT #LATEX.CH%, "\\"
			PRINT #LATEX.CH%, MAKETEX(INLINE$);
			GOTO 12080
		END IF
	NEXT ZZ%

12080	CLOSE #FUN_FILE.CH%

12090	PRINT #LATEX.CH%
	RETURN

	%PAGE

 LoadAbstract:
13000	!*******************************************************************
	! Read abstract for this group
	!*******************************************************************

	RESET #ABSTRACT.CH%

	ABS_FLAG% = 0%
	CHP_FLAG% = 0%

13010	WHEN ERROR IN
		LINPUT #ABSTRACT.CH%, INAB$
	USE
		CONTINUE 13090
	END WHEN

	IF LEFT(INAB$, 1%) = "~"
	THEN
		IF INAB$ = "~" + CURRENT_CHAPTER$
		THEN
			CHP_FLAG% = -1%
			ABS_FLAG% = -1%
			LINPUT #ABSTRACT.CH%, MASTER_TITLE$

			PRINT #LATEX.CH%, "\chapter {"; &
				MAKETEX(CURRENT_CHAPTER$); &
				" -- "; &
				MAKETEX(MASTER_TITLE$); &
				"}"
		ELSE
			ABS_FLAG% = 0%
		END IF
	ELSE
		PRINT #LATEX.CH%, INAB$ IF ABS_FLAG%
	END IF

	GOTO 13010

13090	IF CHP_FLAG% = 0%
	THEN
		PRINT #LATEX.CH%, "\chapter {"; &
			MAKETEX(CURRENT_CHAPTER$); &
			" -- "; &
			"Undefined"; &
			"}"
	END IF

	RETURN

	%PAGE

	!*******************************************************************
	! Exit from the program
	!*******************************************************************

 ExitProgram:
	SYS_STATUS = LIB$FIND_FILE_END(CONTEXT%)

	CLOSE LATEX.CH%

19999	END

20000	SUB LOADBLOCKCOMMENT(CHAN%, ARY$(), ARY%, NEXTLINE$, ftype$)

	!*******************************************************************
	! This subroutine is used to read in a block of comment text
	! into an array for processing elsewhere.
	!*******************************************************************

	ON ERROR GOTO 20900

	!
	! Initilize variables
	!
 !	ARY% = 0%		% Nothing loaded into array yet

 MainLoop:

	!
	! Read in one line
	!
	LINPUT #CHAN%, NEXTLINE$

	!
	! Exit out of loop if out of this comment
	!
	IF FTYPE$ = "BAS"
	THEN
		GOTO MainExit IF LEFT(NEXTLINE$, 2%) <> '9'C + "!"
		GOTO MainExit IF LEFT(NEXTLINE$, 3%) == '9'C + "! "
		GOTO MainExit IF TRM$(LEFT(NEXTLINE$, 3%)) <> '9'C + "!"
	ELSE
		GOTO MainExit IF LEFT(NEXTLINE$, 2%) <> " *"
		GOTO MainExit IF LEFT(NEXTLINE$, 3%) == " * "
		GOTO MainExit IF TRM$(LEFT(NEXTLINE$, 3%)) <> " *"
	END IF

	!
	! Add info to array
	!
	NEXTLINE$ = RIGHT(NEXTLINE$, 4%)

	IF (ARY% <> 0%) OR (NEXTLINE$ <> "")
	THEN
		ARY% = ARY% + 1%
		ARY$(ARY%) = NEXTLINE$
	END IF

	!
	! Cycle back around
	!
	GOTO MainLoop

 MainExit:
	!
	! Strip nulls off of end of text
	!
	WHILE (ARY% > 0%) AND (ARY$(ARY%) = "")

		ARY% = ARY% - 1%

	NEXT

	EXIT SUB

20900	!
	! Trap errors
	!
	NEXTLINE$ = '255'C
	RESUME MainExit IF ERR = 11%
	ON ERROR GOTO 0

	END SUB

22000	SUB WRITETEXTV(CHAN%, ARY$(), ARY%, SECTION$)

	!*******************************************************************
	! This subroutine is used to write in a block of comment text
	! from an array.
	!
	! Verbatim format.
	!*******************************************************************

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	GOTO ExitText IF ARY% = 0%

	PRINT #CHAN%, "\subsection*{"; SECTION$; "}"
	PRINT #CHAN%, "\begin{verbatim}"

	LSTFLAG% = 0%

 Text1:

	FOR LOOP% = 1% TO ARY%

		INLINE$ = ARY$(LOOP%)

		!
		! Make tabs indent better in TeX (Ignores chr[9]).
		!
		I% = INSTR(1%, INLINE$, '9'C)
		WHILE I%
			INLINE$ = LEFT(INLINE$, I% - 1%) + "    " + &
				RIGHT(INLINE$, I% + 1%)
			I% = INSTR(1%, INLINE$, '9'C)
		NEXT

		PRINT #CHAN%, "   "; INLINE$

	NEXT LOOP%

 ExitText:

	PRINT #CHAN%, "\end{verbatim}"

	END SUB

23000	SUB WRITETEXTL(CHAN%, ARY$(), ARY%, SECTION$)

	!*******************************************************************
	! This subroutine is used to write in a block of comment text
	! from an array.
	!
	! List of text items format.
	!*******************************************************************

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "STR$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	EXTERNAL STRING FUNCTION MAKETEX

	DIM LIST%(100%), LISTFLAG%(100%)


	GOTO ExitText IF ARY% = 0%

	PRINT #CHAN%, "\subsection*{"; SECTION$; "}"
	PRINT #CHAN%, "\begin{itemize}"

	ITEM_FLAG% = 0%
	FIRST_FLAG% = 0%
	INLIST% = 0%

 Text1:

	FOR LOOP% = 1% TO ARY%

		INLINE$ = ARY$(LOOP%)
		INLINE1$ = EDIT$(INLINE$, -1%)

		!
		! Handle special flags
		!
		SELECT INLINE1$

		CASE ".LIST"
			!
			! List enviornment. Each item is bulleted.
			!
			INLIST% = INLIST% + 1%
			PRINT #CHAN%, "\begin{itemize}"
			LIST%(INLIST%) = 1%
			LISTFLAG%(INLIST%) = -1%

		CASE ".ENDLIST"
			!
			! End the list enviornment
			!
			PRINT "***Error: .LIST ended with .ENDTABLE" &
				IF LIST%(INLIST%) <> 1%
			INLIST% = INLIST% - 1%
			PRINT #CHAN%, "\end{itemize}"

		CASE ".TABLE"
			INLIST% = INLIST% + 1%
			PRINT #CHAN%, "\begin{description}"
			LIST%(INLIST%) = 2%
			LISTFLAG%(INLIST%) = -2%

		CASE ".ENDTABLE"
			PRINT "***Error: .TABLE ended with .ENDLIST" &
				IF LIST%(INLIST%) <> 2%
			INLIST% = INLIST% - 1%
			PRINT #CHAN%, "\end{description}"

		CASE ELSE
			IF (LISTFLAG%(INLIST%) = -2%) AND (INLINE$ <> "")
			THEN
				INLINE$ = EDIT$(INLINE$, 8%)
				I% = STR$FIND_FIRST_IN_SET(INLINE$, '9'C + " ")
				I% = LEN(INLINE$) + 1% IF I% = 0%
				PRINT #CHAN%, "\item["; &
					MAKETEX(LEFT(INLINE$, I% - 1%)); "]"
				INLINE$ = RIGHT(INLINE$, I% + 1%)
				LSTFLAG%(INLIST%) = 0%
			END IF

			IF LISTFLAG%(INLIST%) = -1%
			THEN
				IF LIST%(INLIST%) = 2%
				THEN
					LISTFLAG%(INLIST%) = -2%
				ELSE
					PRINT #CHAN%, "\item"
					LISTFLAG%(INLIST%) = 0%
				END IF
			END IF

			IF INLIST% = 0%
			THEN
				IF (INLINE$ = "")
				THEN
					ITEM_FLAG% = 0%
					FIRST_FLAG% = 0%
				ELSE
					PRINT #CHAN%, "\item" IF ITEM_FLAG% = 0%
					ITEM_FLAG% = -1%
					PRINT #CHAN% IF FIRST_FLAG% = 1%
					PRINT #CHAN%, MAKETEX(INLINE$)
					FIRST_FLAG% = FIRST_FLAG% + 1%
				END IF
			ELSE
				PRINT #CHAN%, MAKETEX(INLINE$)
			END IF
		END SELECT

 ExitLoop:
	NEXT LOOP%

 ExitText:
	IF INLIST%
	THEN
		PRINT "***ERROR: Missing .ENDLIST or .ENDTABLE!"
	END IF

	PRINT #CHAN%, "\end{itemize}"

	END SUB

29000	FUNCTION STRING MAKETEX(INLINE$)

	!*******************************************************************
	! Function to convert text into a TeXable format
	!*******************************************************************

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "STR$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

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

		CASE "_"
			TEMP_IN$ = LEFT(TEMP_IN$, TEMP_IN% - 1%) + &
				"\_\," + &
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

	MAKETEX = TEMP_IN$

	END FUNCTION
