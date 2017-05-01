1	%TITLE "Document a Menu"
	%SBTTL "TK_DOCU_MENU_RNO"
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
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_DOCU_MENU_RNO
	!	$ LINK/EXE=TK_EXE: TK_DOCU_MENU_RNO, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE TK_DOCU_MENU_RNO.OBJ;*
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
	!	12/21/2000 - Kevin Handy
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
	! External functions
	!
	EXTERNAL LONG FUNCTION LIBR_EXTRACTFILE

	!
	! Declare variables
	!
	MAP (LBR_JUNKJUNK) TEXT$ = 132%		! Read buffer
	DECLARE RFA TXRFA

	%PAGE

1000	!*******************************************************************
	! Allocate channels
	!*******************************************************************

	SOURCE.CH% = 5%
	LATEX.CH% = 6%

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

1400	!*******************************************************************
	! Get output file name
	!*******************************************************************

	LINPUT "Name of output file <.RNO> "; LATEX.NAME$

	OPEN LATEX.NAME$ FOR OUTPUT AS FILE LATEX.CH%, &
		DEFAULTNAME ".RNO", &
		RECORDSIZE 255%

1500	!
	! Put a header on the output file
	!
	PRINT #LATEX.CH%, ".FL BOLD"
	PRINT #LATEX.CH%, ".FL UNDERLINE"
	PRINT #LATEX.CH%, ".FL INDEX"

2000	!*******************************************************************
	! Scan through file, handling one line at a time
	!*******************************************************************

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

	PRINT #LATEX.CH%, ""

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
		PRINT #LATEX.CH%, ".B 10"
		PRINT #LATEX.CH%, ".C"
		PRINT #LATEX.CH%, FNRNO$(MENU_OPTION$)
		PRINT #LATEX.CH%, ".C"
		PRINT #LATEX.CH%, FNRNO$(MENU_DESCR$)

	!
	! Start of a chapter
	!
	CASE 1%
		PRINT #LATEX.CH%, ".CH " + &
			FNRNO$(MENU_OPTION$ + " - " + &
			MENU_DESCR$)

	!
	! Start of a section
	!
	CASE 2%
		PRINT #LATEX.CH%, ".HL 1 " + &
			FNRNO$(MENU_OPTION$ + " - " + &
			MENU_DESCR$)

	!
	! Start of a subsection
	!
	CASE 3%
		PRINT #LATEX.CH%, ".HL 2 " + &
			FNRNO$(MENU_OPTION$ + " - " + &
			MENU_DESCR$)

	!
	! Start of a subsubsection
	!
	CASE 4%
		PRINT #LATEX.CH%, ".HL 3 " + &
			FNRNO$(MENU_OPTION$ + " - " + &
			MENU_DESCR$)

	!
	! Start of a paragraph
	!
	CASE 5%
		PRINT #LATEX.CH%, ".HL 4 " + &
			FNRNO$(MENU_OPTION$ + " - " + &
			MENU_DESCR$)

	!
	! Start of a subparagraph
	!
	CASE ELSE
		PRINT #LATEX.CH%, ".HL 5 " + &
			FNRNO$(MENU_OPTION$ + " - " + &
			MENU_DESCR$)

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

	ST% = LIBR_EXTRACTFILE(LIB_NAME$, LATEX.CH%, KEY_NAME$, 1%)

	RETURN

	%PAGE

 !7000
	!*******************************************************************
	! Finish up output
	!*******************************************************************

 !	PRINT #LATEX.CH%, ""
 !
 !	GOTO 32767

	%PAGE

18000	!*******************************************************************
	! Function to convert text into a TeXable format
	!*******************************************************************

	DEF FNRNO$(INLINE$)

		TEMP_IN$ = INLINE$
		TEMP_IN% = 0%
 TexLoop:
		TEMP_OLD% = TEMP_IN% + 1%
		TEMP_IN% = STR$FIND_FIRST_IN_SET(RIGHT(TEMP_IN$, TEMP_OLD%), &
			"#&_*~^")

		IF TEMP_IN%
		THEN
			TEMP_IN% = TEMP_IN% + TEMP_OLD% - 1%

			TEMP_IN$ = LEFT(TEMP_IN$, TEMP_IN% - 1%) + &
				"_" + &
				RIGHT(TEMP_IN$, TEMP_IN%)
			TEMP_IN% = TEMP_IN% + 1%

			GOTO TexLoop
		END IF

		FNRNO$ = TEMP_IN$
	FNEND

32767	END
