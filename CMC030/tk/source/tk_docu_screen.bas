1	%TITLE "Document a Menu"
	%SBTTL "TK_DOCU_SCREEN"
	%IDENT "V3.6a Calico"

	!
	!	COPYRIGHT (C) 1987, 1988 BY
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
	!	$ BAS TK_SOURCE:TK_DOCU_SCREEN/NOWAR
	!	$ LINK/EXE=TK_EXE: TK_DOCU_SCREEN, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE TK_DOCU_SCREEN.OBJ;*
	!
	! Author:
	!
	!	10/26/87 - Kevin Handy
	!
	!	04/01/88 - Kevin Handy
	!		Taken from TK_DOCU_APPLICATION
	!
	! Modification history:
	!
	!	11/18/87 - Kevin Handy
	!		Added SMG stuff to make it look better.
	!
	!	03/25/88 - Kevin Handy
	!		Removes SMG stuff so it could work in
	!		a command file.
	!
	!	09/20/91 - Kevin Handy
	!		Added warning message about defining "SIC"
	!		because it was a real pain when I didn't
	!		know about it.
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
	!	12/11/2000 - Kevin Handy
	!		Use WHEN ERROR IN
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
	EXTERNAL LONG    TK_DOCU_GETMODULES

	!
	! Declare variables
	!
	MAP (LBR_JUNKJUNK) TEXT$ = 132%, TEXT1$ = 256%		! Read buffer
	DECLARE RFA WIN_RFA

	MAP (TK_DOCU) &
		IN_LEVEL%, &
		MODCOUNT%(20%), &
		MODNAME$(20%, 100%) = 50%, &
		RFA MODRFA(20%, 100%)

	DIM KEY_NAME$(40%), LR.INDEX%(40%), &
		MAIN_LOOP%(40%)

	!
	! Create array to hold legal commands
	!
	DECLARE INTEGER CONSTANT MAX_COMMAND = 100%
	DIM COMMAND_LIST$(MAX_COMMAND),	COMMAND_LIST%(MAX_COMMAND)

	MAP (IOBUF) LONG IO_BUF(6%)
	MAP (IOBUF) WORD IO_BUF_W(12%)
	MAP (IOBUF1) NAME.BUFFER$ = 50%

	EXTERNAL LONG    FUNCTION SYS$FILESCAN
	DECLARE LONG CONSTANT FSCN$_NAME = 6

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

	!*******************************************************************
	! Get menu source file
	!*******************************************************************
	PRINT
	PRINT "Make Sure that you have SIC defined, otherwise no"
	PRINT "screens will be found!"
	PRINT

1200	LINPUT "Name of menu file (No account or externsion) "; SOURCE.NAME$

	OPEN SOURCE.NAME$ FOR INPUT AS FILE SOURCE.CH%, &
		ACCESS READ, &
		ALLOW MODIFY, &
		DEFAULTNAME "CMC:*.MNU"

1400	!*******************************************************************
	! Get output file name
	!*******************************************************************

 !	LINPUT "Name of output file <.TEX> "; LATEX.NAME$
	LATEX.NAME$ = SOURCE.NAME$ + "_SCREEN"

	OPEN LATEX.NAME$ FOR OUTPUT AS FILE LATEX.CH%, &
		DEFAULTNAME ".TEX", &
		RECORDSIZE 255%, &
		ALLOW READ


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
	! Put a header on the output file
	!
	PRINT #LATEX.CH%, "\documentstyle[cmcextra]{book}"
	PRINT #LATEX.CH%, "\textwidth 5in"		! Make pages wider
	PRINT #LATEX.CH%, "\evensidemargin 1in"		! Repair margin
	PRINT #LATEX.CH%, "\begin{document}"
	PRINT #LATEX.CH%, "\sloppy"			! Less extended lines

2000	!*******************************************************************
	! Scan through file, handling one line at a time
	!*******************************************************************

	FLAG_CASE% = 0%		! 0 = Mixed case, 1 = Upper Case,
				! 2 = Lower Case
	QUOTE_FLAG% = 0%	! Which way does the current quote marker point

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
	PRINT #LATEX.CH%, "%"; INLINE$

	GOSUB ParseLine

	GOTO 2000

2100	!*******************************************************************
	! Finish up file
	!*******************************************************************

	PRINT #LATEX.CH%, "\end{document}"

	ST% = LBR$CLOSE(WIN.INDEX%)
	CLOSE LATEX.CH%
	CLOSE SOURCE.CH%

 ExitProgram:
	GOTO 20000

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

	TEMP% = INSTR(1%, TLB_PROGRAM$, "_")
	IF TEMP%
	THEN
		LIB_NAME$ = "HELP_" + LEFT(TLB_PROGRAM$, TEMP% - 1%)
	ELSE
		IF MENU_SYSTEM$ = ""
		THEN
			LIB_NAME$ = "HELP_DEFAULT"
		ELSE
			LIB_NAME$ = "HELP_" + MENU_SYSTEM$
		END IF
	END IF

	KEY_NAME_BASE$, KEY_NAME$ = TRM$(TLB_IDENT$) + "$" + &
		TRM$(TLB_PROGRAM$) + "$" + TRM$(TLB_ITEM$)

	GOSUB 4000

	RETURN

	%PAGE

4000	!*******************************************************************
	! Read in one help file, and convert to TeX format
	!*******************************************************************
 ! PRINT #LATEX.CH%, "%BEGIN "; KEY_NAME$

	!
	! Store info so lower levels can access it
	!
	IN_LEVEL% = IN_LEVEL% + 1%
	KEY_NAME$(IN_LEVEL%) = KEY_NAME$

	!
	! Set up the control structure if necessary
	!
	ST% = LBR$INI_CONTROL(LR.INDEX%(IN_LEVEL%), LBR$C_READ)

	IF (ST% AND 1%) = 0%
	THEN
 !		PRINT "Unable to initilize library"; ST%
		IN_LEVEL% = IN_LEVEL% - 1%
		RETURN
	END IF

	!
	! Open the library function
	!
	ST% = LBR$OPEN(LR.INDEX%(IN_LEVEL%), LIB_NAME$, , "REF:.TLB")

	IF (ST% AND 1%) = 0%
	THEN
 !		PRINT "Error in LIB open "; ST%
		IN_LEVEL% = IN_LEVEL% - 1%
		RETURN
	END IF

	!
	! Search for key in file
	!
	MODCOUNT%(IN_LEVEL%) = 0%

	KEY_NAME_LIST$ = EDIT$(KEY_NAME$, 2% + 32%)

	WHILE KEY_NAME_LIST$ <> ""

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

		KEY_NAME1$ = FNKEY$(KEY_NAME1$, KEY_NAME_BASE$)

		ST% = LBR$GET_INDEX(LR.INDEX%(IN_LEVEL%), 1%, &
			TK_DOCU_GETMODULES, KEY_NAME1$)

	NEXT

	!
	! There must be at least one item to print
	!
	IF MODCOUNT%(IN_LEVEL%) = 0%
	THEN
		PRINT "** No fields found " + TRM$(KEY_NAME$)
		GOTO CloseLibrary
	END IF

	!
	! loop through all modules found.  For/Next statements have to be
	! faked because basic doesn't like arrays as the loop variable.
	!
	MAIN_LOOP%(IN_LEVEL%) = 0%
 LoopMain:
	MAIN_LOOP%(IN_LEVEL%) = MAIN_LOOP%(IN_LEVEL%) + 1%
	GOTO LoopMainEnd IF MAIN_LOOP%(IN_LEVEL%) > MODCOUNT%(IN_LEVEL%)

		!
		! Point to text
		!
		ST% = LBR$FIND(LR.INDEX%(IN_LEVEL%), &
			MODRFA(IN_LEVEL%, MAIN_LOOP%(IN_LEVEL%)))

		IF (ST% AND 1%) = 0%
		THEN
			GOTO LoopNext
		END IF

		!
		! Copy over text
		!
 Loop:
		TEXT$ = ""
		ST% = LBR$GET_RECORD(LR.INDEX%(IN_LEVEL%), TEXT$)

		IF (ST% AND 1%) = 1%
		THEN
			INLINE$ = TRM$(TEXT$)
			GOSUB 6000
			GOTO Loop
		END IF

	!
	! Finish up loop (Faked Next)
	!
 LoopNext:
	GOTO LoopMain

 LoopMainEnd:

	!
	! Close library file
	!
 CloseLibrary:
	ST% = LBR$CLOSE(LR.INDEX%(IN_LEVEL%))

	PRINT #LATEX.CH%, "%END "; KEY_NAME$(IN_LEVEL%)
	IN_LEVEL% = IN_LEVEL% - 1%

	RETURN

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

	RETURN

	%PAGE

7000	!*******************************************************************
	! Finish up output
	!*******************************************************************

	PRINT #LATEX.CH%, "\end{document}"

	GOTO 20000

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
		DOT1_COMMAND$ = EDIT$(LEFT(INLINE$, TEMP% - 1%), 128%)
		INLINE$ = RIGHT(TEMP$, TEMP% + 1%)
	ELSE
		DOT_COMMAND$ = EDIT$(INLINE$, 32% + 128%)
		DOT1_COMMAND$ = EDIT$(INLINE$, 128%)
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

	RETURN

8100	!
	! We have a command, process it
	!
	DOT_WIDTH% = LEN(COMMAND_LIST$(DOT_COMMAND%)) + 1%
	SELECT COMMAND_LIST%(DOT_COMMAND%)

	!
	! Document fields (Assume same file as current)
	!
	CASE 50%
		KEY_NAME$ = RIGHT(DOT_COMMAND$, DOT_WIDTH%)

		GOSUB 4000

		INLINE$ = ""

	!
	! Options (Assume same file as current)
	!
	CASE 51%
		KEY_NAME$ = RIGHT(DOT1_COMMAND$, DOT_WIDTH%)

		GOSUB 4000

		INLINE$ = ""

	!
	! Screens
	!
	CASE 52%
		WIN_NAME$ = RIGHT(DOT1_COMMAND$, DOT_WIDTH%)

		CALL TK_SUBR_TEXSCREEN(WIN_NAME$, KEY_NAME_BASE$, &
			LATEX.CH%, 2% + 8%)

		INLINE$ = ""

	!
	! Reports
	!
	CASE 53%
		WIN_NAME$ = RIGHT(DOT1_COMMAND$, DOT_WIDTH%)

		CALL TK_SUBR_TEXSCREEN(WIN_NAME$, KEY_NAME_BASE$, &
			LATEX.CH%, 4% + 8%)

		INLINE$ = ""

	END SELECT

8900	RETURN

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
	DATA	50,	".!.FIELD ", &
		50,	".!.FIELDS ", &
		51,	".!.OPTION ", &
		51,	".!.OPTIONS ", &
		52,	".!.SCREEN ", &
		52,	".!.SCREENS ", &
		53,	".!.REPORT ", &
		53,	".!.REPORTS ", &
		0,	""

	%PAGE

18000	!*******************************************************************
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

20000	END

21000	FUNCTION LONG TK_DOCU_GETMODULES(MODKEY$, RFA MODRFA)

	!
	! This function graps the names passed to it from the
	! LIB$SEARCH call
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	MAP (TK_DOCU) &
		IN_LEVEL%, &
		MODCOUNT%(20%), &
		MODNAME$(20%, 100%) = 50%, &
		RFA MODRFA(20%, 100%)

	!
	! Don't add name if already have a key to that item,
	! or we have amassed too many items already.
	!
	GOTO ExitFunction &
		IF MODCOUNT%(IN_LEVEL%) = 100%

	GOTO ExitFunction &
		IF MODRFA(IN_LEVEL%, LOOP%) = MODRFA &
		FOR LOOP% = 1% TO MODCOUNT%(IN_LEVEL%)

	!
	! Add to list
	!
	MODCOUNT%(IN_LEVEL%) = MODCOUNT%(IN_LEVEL%) + 1%
	MODNAME$(IN_LEVEL%, MODCOUNT%(IN_LEVEL%)) = MODKEY$
	MODRFA(IN_LEVEL%, MODCOUNT%(IN_LEVEL%)) = MODRFA


 ExitFunction:
	TK_DOCU_GETMODULES = 1%

	END FUNCTION
