1	%TITLE "Output a Screen to File in TeX Format"
	%SBTTL "TK_SUBR_TEXSCREEN"
	%IDENT "V3.6a Calico"

	SUB TK_SUBR_TEXSCREEN(WIN_NAME$, KEY_NAME_BASE$, LATEX.CH%, FLAGS%)

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
	!	This subroutine will pull a file in from a TeX file
	!	and produce the code necessary to display it.
	!
	! Index:
	!	.x Documentation
	!	.x Screen>TeX
	!	.x Report>TeX
	!	.x TeX>Screen
	!	.x TeX>Report
	!
	! Option:
	!
	!
	! Input:
	!
	!	WIN_NAME$
	!		String containing the window name and any of the
	!		following:
	!		.table
	!			\caption{xxx} - Put a caption on top of
	!			the screen.
	!
	!			\keyuse{xxx} - Key use of a report.
	!
	!			\sugrou{xxx} - Suggested routing.
	!
	!			\frepre{xxx} - Frequency of preperation.
	!
	!			\altrep{xxx} - Alternate reports.
	!		.endtable
	!
	!	KEY_NAME_BASE$
	!		Base name used in filling out unknown information
	!		in the WIN_NAME$ variable.
	!
	!	LATEX.CH%
	!		Channel to write output text to.
	!
	!	FLAGS%
	!		Various flags for modifying output.
	!		.table
	!			1% - Make into a figure.
	!
	!			2% - Make a screen.
	!
	!			4% - Make a report.
	!
	!			8% - Put "\feed" after report
	!		.endtable
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
	!	$ BAS TK_SOURCE:TK_SUBR_TEXSCREEN
	!	$ LIB/REP FUNC_LIB:CMCFUN TK_SUBR_TEXSCREEN
	!	$ DELETE TK_SUBR_TEXSCREEN.OBJ;*
	!
	! Author:
	!
	!	07/22/88 - Kevin Handy
	!
	! Modification history:
	!
	!	10/14/88 - Frank Starman
	!		Change REF: to SIC:.
	!
	!	05/17/90 - Frank Starman
	!		Print text for each figure from library.
	!
	!	03/30/93 - Kevin Handy
	!		Disables error messages since Frank's changes forces
	!		hundreds to print out, which mean nothing.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE
	!
	!	06/21/99 - Kevin Handy
	!		Lose code for PRINT_LEVEL$, which was never set.
	!
	!	12/22/2000 - Kevin Handy
	!		Lose usless 'on error goto 0' trap.
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
	EXTERNAL STRING FUNCTION TK_FUNC_TEXSTRING

	!
	! Declare variables
	!
	MAP (LBR_JUNKJUNK) TEXT$ = 132%, TEXT1$ = 256%		! Read buffer
	DECLARE RFA TXRFA, NULL.RFA, WIN_RFA, WIN1_RFA

	%PAGE

	!*******************************************************************
	! Dump out one file in verbatim mode
	!*******************************************************************

 DoVerbatim:
	!
	! If a caption command is given, then use it, otherwise
	! fake up something.
	!
	I56% = INSTR(1%, EDIT$(WIN_NAME$, 32%), "\CAPTION{")
	IF I56% <> 0%
	THEN
		!
		! Pull off caption name. Strip off braces if there are any.
		!
		I57% = INSTR(I56%, WIN_NAME$, "}")
		I57% = LEN(WIN_NAME$) + 1% IF I57% = 0%
		CAPTION_NAME1$ = SEG$(WIN_NAME$, I56% + 9%, I57% - 1%)

		!
		! Shorten the window number by that much.
		!
		WIN_NAME$ = LEFT(WIN_NAME$, I56% - 1%) + &
			RIGHT(WIN_NAME$, I57% + 1%)
	ELSE
		!
		! Fake up a caption to show.
		! Currently using window number.
		!
		CAPTION_NAME1$ = FNKEY$(WIN_NAME$, KEY_NAME_BASE$)
	END IF

	!
	! If a keyuse command is given, then use it, otherwise
	! fake up something.
	!
	I56% = INSTR(1%, EDIT$(WIN_NAME$, 32%), "\KEYUSE{")
	IF I56% <> 0%
	THEN
		!
		! Pull off caption name. Strip off braces if there are any.
		!
		I57% = INSTR(I56%, WIN_NAME$, "}")
		I57% = LEN(WIN_NAME$) + 1% IF I57% = 0%
		KEYUSE_NAME1$ = SEG$(WIN_NAME$, I56% + 8%, I57% - 1%)

		!
		! Shorten the window number by that much.
		!
		WIN_NAME$ = LEFT(WIN_NAME$, I56% - 1%) + &
			RIGHT(WIN_NAME$, I57% + 1%)
	ELSE
		!
		! Fake up a caption to show.
		! Currently using window number.
		!
		KEYUSE_NAME1$ = ""
	END IF

	!
	! If a sugrou command is given, then use it, otherwise
	! fake up something.
	!
	I56% = INSTR(1%, EDIT$(WIN_NAME$, 32%), "\SUGROU{")
	IF I56% <> 0%
	THEN
		!
		! Pull off caption name. Strip off braces if there are any.
		!
		I57% = INSTR(I56%, WIN_NAME$, "}")
		I57% = LEN(WIN_NAME$) + 1% IF I57% = 0%
		SUGROU_NAME1$ = SEG$(WIN_NAME$, I56% + 8%, I57% - 1%)

		!
		! Shorten the window number by that much.
		!
		WIN_NAME$ = LEFT(WIN_NAME$, I56% - 1%) + &
			RIGHT(WIN_NAME$, I57% + 1%)
	ELSE
		!
		! Fake up a caption to show.
		! Currently using window number.
		!
		SUBROU_NAME1$ = ""
	END IF

	!
	! If a frepre command is given, then use it, otherwise
	! fake up something.
	!
	I56% = INSTR(1%, EDIT$(WIN_NAME$, 32%), "\FREPRE{")
	IF I56% <> 0%
	THEN
		!
		! Pull off caption name. Strip off braces if there are any.
		!
		I57% = INSTR(I56%, WIN_NAME$, "}")
		I57% = LEN(WIN_NAME$) + 1% IF I57% = 0%
		FREPRE_NAME1$ = SEG$(WIN_NAME$, I56% + 8%, I57% - 1%)

		!
		! Shorten the window number by that much.
		!
		WIN_NAME$ = LEFT(WIN_NAME$, I56% - 1%) + &
			RIGHT(WIN_NAME$, I57% + 1%)
	ELSE
		!
		! Fake up a caption to show.
		! Currently using window number.
		!
		FREPRE_NAME1$ = ""
	END IF

	!
	! If a altrep command is given, then use it, otherwise
	! fake up something.
	!
	I56% = INSTR(1%, EDIT$(WIN_NAME$, 32%), "\ALTREP{")
	IF I56% <> 0%
	THEN
		!
		! Pull off caption name. Strip off braces if there are any.
		!
		I57% = INSTR(I56%, WIN_NAME$, "}")
		I57% = LEN(WIN_NAME$) + 1% IF I57% = 0%
		ALTREP_NAME1$ = SEG$(WIN_NAME$, I56% + 8%, I57% - 1%)

		!
		! Shorten the window number by that much.
		!
		WIN_NAME$ = LEFT(WIN_NAME$, I56% - 1%) + &
			RIGHT(WIN_NAME$, I57% + 1%)
	ELSE
		!
		! Fake up a caption to show.
		! Currently using window number.
		!
		ALTREP_NAME1$ = ""
	END IF

	!
	! Fancy up key somewhat
	!
	WIN_NAME1$ = FNKEY$(WIN_NAME$, KEY_NAME_BASE$)
 ! PRINT #LATEX.CH%, "%DOVE "; WIN_NAME1$

	!
	! Search for key in file
	!
	TEMP% = INSTR(1%, WIN_NAME1$, "$")
	TEMP1% = INSTR(1%, LEFT(WIN_NAME1$, TEMP% - 1%) + "_", "_")

	THIS_DOVE$ = "WINDOWS_" + LEFT(WIN_NAME1$, TEMP1% - 1%)

	!
	! Set up the control structure if necessary
	!
	ST% = LBR$INI_CONTROL(WIN.INDEX%, LBR$C_READ)

	IF (ST% AND 1%) = 0%
	THEN
		PRINT "  *** Unable to initilize window library"; ST%; WIN_NAME1$
		GOTO 32767
	END IF

	!
	! Open the library function
	!
	ST% = LBR$OPEN(WIN.INDEX%, THIS_DOVE$, , "SIC:.TLB")

	IF (ST% AND 1%) = 0%
	THEN
		PRINT "  *** Unable to open LIB "; WIN_NAME1$; ST%
		GOTO 32767
	END IF

	!
	! Search for key in file
	!
	ST% = LBR$LOOKUP_KEY(WIN.INDEX%, WIN_NAME1$, WIN_RFA)
	WIN1_RFA = WIN_RFA

	IF (ST% AND 1%) = 0%
	THEN
 !		DOL% = INSTR(1%, WIN_NAME1$, "$")
 !		IF RIGHT(WIN_NAME1$, DOL% + 1%) = "REPORT"
 !		THEN
 !			PRINT "  *** No report figure found " + TRM$(WIN_NAME1$)
 !		ELSE
 !			PRINT "  *** No screen figure found " + TRM$(WIN_NAME1$)
 !		END IF

		GOTO DVCloseLibrary
	END IF

	!
	! Find width of longest line to determine what size of
	! type to use. (We want as big of type as possible, up
	! to normal type size, for readibility)
	!
	DVWIDTH% = 0%
 DVPLoop:
	TEXT1$ = ""
	ST% = LBR$GET_RECORD(WIN.INDEX%, TEXT1$)

	IF (ST% AND 1%) = 1%
	THEN
		INLINE1$ = TRM$(TEXT1$)
		DVWIDTH% = LEN(INLINE1$) IF LEN(INLINE1$) > DVWIDTH%
		GOTO DVPLoop
	END IF

	!
	! Error if report has nothing.
	!
	IF (DVWIDTH% = 0%)
	THEN
		PRINT "  *** Empty report in "; &
			TRM$(KEY_NAME$(IN_LEVEL%)); " for "; &
			TRM$(WIN_NAME1$)
		GOTO DVCloseLibrary
	END IF

	SELECT DVWIDTH%

 !		CASE 0% TO 49%
 !			DVWIDTH$ = "\normalsize"
 !			DVCHAR = 14%

		CASE 0% TO 85%
			DVWIDTH$ = "\scriptsize"
			DVCHAR = 19.4
			DVWIDTH% = 80% IF DVWIDTH% < 80%

		CASE ELSE
			DVWIDTH$ = "\tiny"
			DVCHAR = 27.2

	END SELECT

	!
	! Relocate text (Assume it will work)
	!
	ST% = LBR$FIND(WIN.INDEX%, WIN1_RFA)

	!
	! Print header if declared
	!
 !	IF (PRINT_LEVEL$ <> "")
 !	THEN
 !		PRINT #LATEX.CH%, PRINT_LEVEL$
 !		PRINT_LEVEL$ = ""
 !	END IF

	IF (FLAGS% AND 2%)
	THEN
		ISA_SCREEN% = -1%
	ELSE
		IF (FLAGS% AND 4%)
		THEN
			ISA_SCREEN% = -2%
		ELSE
			ISA_SCREEN% = 0%
		END IF
	END IF

	!
	! Print text for figure
	!
	PRINT #LATEX.CH%, "Figure \ref{fig:" + WIN_NAME1$ + "}" + &
		" shows the " + TK_FUNC_TEXSTRING(CAPTION_NAME1$) + "."

	!
	! Copy over text
	!
	SELECT ISA_SCREEN%

	CASE -1%
		XWIDTH = (1.0 + DVWIDTH%) / DVCHAR
		IF (FLAGS% AND 1%)
		THEN
			PRINT #LATEX.CH%, "{\begin{figure}[tbp]"
			PRINT #LATEX.CH%, "\caption{"; TK_FUNC_TEXSTRING(CAPTION_NAME1$); "}"
		ELSE
			PRINT #LATEX.CH%, "\begin{center}"
			PRINT #LATEX.CH%, TK_FUNC_TEXSTRING(CAPTION_NAME1$)
			PRINT #LATEX.CH%, "\end{center}"
		END IF
		PRINT #LATEX.CH%, "\begin{center}\begin{boxedminipage}[t]{"; &
			NUM1$(XWIDTH); "in}"
		PRINT #LATEX.CH%, DVWIDTH$; "\begin{verbatim}"

	CASE -2%
		!
		! This is a report. Force the character size.
		!
		DVWIDTH$ = "\tiny"
		DVCHAR = 27.45

		XWIDTH = (2.0 + DVWIDTH%) / DVCHAR

		IF (FLAGS% AND 1%)
		THEN
			PRINT #LATEX.CH%, "{\begin{figure}[tbp]"
			PRINT #LATEX.CH%, "\caption{"; TK_FUNC_TEXSTRING(CAPTION_NAME1$); "}"
		ELSE
			PRINT #LATEX.CH%, "\begin{center}"
			PRINT #LATEX.CH%, TK_FUNC_TEXSTRING(CAPTION_NAME1$)
			PRINT #LATEX.CH%, "\end{center}"
		END IF

		PRINT #LATEX.CH%, "\begin{center}\begin{boxedminipage}[t]{"; &
			NUM1$(XWIDTH); "in}"
		PRINT #LATEX.CH%, "{"; DVWIDTH$; "\begin{verbatim}"

	CASE ELSE
		!
		! Simple verbatim text
		!
		PRINT #LATEX.CH%, "{"; DVWIDTH$; "\begin{verbatim}"

	END SELECT

 DVLoop:
	TEXT1$ = ""
	ST% = LBR$GET_RECORD(WIN.INDEX%, TEXT1$)

	IF (ST% AND 1%) = 1%
	THEN
		INLINE1$ = TRM$(TEXT1$)
		PRINT #LATEX.CH%, INLINE1$ UNLESS ASCII(INLINE1$) = 27%
		GOTO DVLoop
	END IF

	SELECT ISA_SCREEN%

	CASE -1%
		PRINT #LATEX.CH%, ""
		PRINT #LATEX.CH%, "\end{verbatim}\end{boxedminipage}"
		PRINT #LATEX.CH%, "\end{center}"
		PRINT #LATEX.CH%, "\label{fig:"; WIN_NAME1$; "}"

		IF (FLAGS% AND 1%)
		THEN
			PRINT #LATEX.CH%, "\end{figure}}"
		END IF

	CASE -2%
		PRINT #LATEX.CH%, ""
		PRINT #LATEX.CH%, ""
		PRINT #LATEX.CH%, "\end{verbatim}}\end{boxedminipage}\end{center}"

		!
		! Write out key use of report
		!
		IF KEYUSE_NAME1$ <> ""
		THEN
			PRINT #LATEX.CH%, ""
			PRINT #LATEX.CH%, "{\bf Key Use of Report:}"
			PRINT #LATEX.CH%, "\begin{quote}"
			PRINT #LATEX.CH%, KEYUSE_NAME1$
			PRINT #LATEX.CH%, "\end{quote}"
		END IF

		!
		! Write out any suggusted routings
		!
		IF SUGROU_NAME1$ <> ""
		THEN
			PRINT #LATEX.CH%, ""
			PRINT #LATEX.CH%, "{\bf Suggested Routing:}"
			PRINT #LATEX.CH%, "\begin{quote}"
			PRINT #LATEX.CH%, SUGROU_NAME1$
			PRINT #LATEX.CH%, "\end{quote}"
		END IF

		!
		! Write out any frequency of preperation
		!
		IF FREPRE_NAME1$ <> ""
		THEN
			PRINT #LATEX.CH%, ""
			PRINT #LATEX.CH%, "{\bf Frequency of Preparation:}"
			PRINT #LATEX.CH%, "\begin{quote}"
			PRINT #LATEX.CH%, FREPRE_NAME1$
			PRINT #LATEX.CH%, "\end{quote}"
		END IF

		!
		! Write out any alternate routings
		!
		IF ALTREP_NAME1$ <> ""
		THEN
			PRINT #LATEX.CH%, ""
			PRINT #LATEX.CH%, "{\bf Alternative Reports:}"
			PRINT #LATEX.CH%, "\begin{quote}"
			PRINT #LATEX.CH%, ALTREP_NAME1$
			PRINT #LATEX.CH%, "\end{quote}"
		END IF

		!
		! Slap a label onto this screen
		!
		PRINT #LATEX.CH%, "\label{fig:"; WIN_NAME1$; "}"

		IF (FLAGS% AND 1%)
		THEN
			PRINT #LATEX.CH%, "\end{figure}}"
		END IF

	CASE ELSE
			PRINT #LATEX.CH%, "\end{verbatim}}"

	END SELECT

	IF (FLAGS% AND 8%)
	THEN
		PRINT #LATEX.CH%, "\clearpage"
	END IF

	!
	! Close library file
	!
 DVCloseLibrary:
	!
	! Close original
	!
	ST% = LBR$CLOSE(WIN.INDEX%)

	GOTO 32767

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

32767	END SUB
