1	%TITLE "Sub Process to Create Module Documenation File"
	%SBTTL "TK_SUBR_MODULEDOCU"
	%IDENT "V3.6a Calico"

	SUB TK_SUBR_MODULEDOCU(LIB_NAME$, KEY_NAME$, SOURCE_NAME$, KEY_DESCR$)

	!
	! COPYRIGHT (C) 1987 BY
	! Computer Management Center
	! Idaho Falls, Idaho.
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
	!	This procedure is used to generate generic
	!	documentation for a function.
	!
	! Index:
	!
	! Option:
	!
	! Input:
	!
	!	LIB_NAME$ - Name of the library to put documentation
	!	in.
	!
	!	KEY_NAME$ - Name of key to associate with the
	!	text in the library.
	!
	!	SOURCE_NAME$ - Name of the source file to read while
	!	trying to create documentation.
	!
	!	KEY_DESCR$ - Short (one line) of description to be
	!	placed at the top of the documentation created.
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_SUBR_MODULEDOCU/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP TK_SUBR_MODULEDOCU
	!	$ DELETE TK_SUBR_MODULEDOCU.OBJ;*
	!
	! Author:
	!
	!	05/01/87 - Robert Peterson
	!
	! Modification history:
	!
	!	07/08/87 - Kevin Handy
	!		Modified to use library files, to read source for
	!		documentation and generally to be more generic.
	!
	!	08/05/91 - Kevin Handy
	!		Added ACCESS READ to open statements.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/30/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/13/99 - Kevin Handy
	!		Use BASIC$STARLET for LIB$
	!
	!	09/19/2000 - Kevin Handy
	!		Use LIB$DELETE_FILE instead of KILL
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	ON ERROR GOTO 19000

	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "$LIBDEF" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION LIBR_3INSERT

100	!
	! Think up temp file name
	!
	TEMP.FILE$ = "TEMP" + READ_SYSJOB + ".TMP"

	!
	! Get the channels from VMS
	!
	SMG_STATUS% = LIB$GET_LUN(WORK.CH%)
	IF SMG_STATUS% = LIB$_INSLUN
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"No free channels from VMS:  " + NUM1$(SMG_STATUS%), 0%)
		GOTO ExitProgram
	END IF

	SMG_STATUS% = LIB$GET_LUN(SOURCE.CH%)
	IF SMG_STATUS% = LIB$_INSLUN
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"No free channels from VMS:  " + NUM1$(SMG_STATUS%), 0%)
		GOTO ExitProgram
	END IF

200	CALL ENTR_3MESSAGE(SCOPE, "Creating " + SOURCE_NAME$ + &
		" documentation", 1%)

1000	!
	! Open the document for output
	!
	WHEN ERROR IN
		OPEN TEMP.FILE$ FOR OUTPUT AS FILE #WORK.CH%
	USE
		CALL ENTR_3MESSAGE(SCOPE, &
			"Unable to open documentation file", 0%)
		CONTINUE ExitProgram
	END WHEN


1100	!
	! Put title in text file
	!
	PRINT #WORK.CH%, ".LEFT MARGIN 5"
	PRINT #WORK.CH%, ".FL SPACE"
	PRINT #WORK.CH%, ".FL BOLD"
	PRINT #WORK.CH%, "^*" + FN_RNO$(KEY_DESCR$) + "\*"
	PRINT #WORK.CH%, ".B"
	PRINT #WORK.CH%, ".LM +5"

	!
	! Try to load in any text from source code
	!
	GOSUB ReadSource

	CLOSE #WORK.CH%

	!
	! Insert text into library
	!
	ST% = LIBR_3INSERT(LIB_NAME$, TEMP.FILE$, KEY_NAME$)

 ExitProgram:
1200	!
	! Finish up and exit
	!
	ST% = LIB$FREE_LUN(WORK.CH%)
	ST% = LIB$FREE_LUN(SOURCE.CH%)

	CALL ENTR_3MESSAGE(SCOPE, &
		"Documentation for " + SOURCE_NAME$ + " has been created", 1%)

1210 !	KILL TEMP.FILE$
 !	KILL TEMP.FILE$
 !	KILL TEMP.FILE$
 !	KILL TEMP.FILE$

	SMG_STATUS% = LIB$DELETE_FILE(TEMP.FILE$ + ";*")

1220	EXIT SUB

	%PAGE

5000	!*******************************************************************
	! Read source code to create some documentation
	!*******************************************************************
 ReadSource:

	WHEN ERROR IN
		OPEN SOURCE_NAME$ FOR INPUT AS FILE SOURCE.CH%, &
			ACCESS READ, ALLOW MODIFY
	USE
		CONTINUE 5500
	END WHEN

5100	!
	! Search first part of file for a definition statement,
	! ie. SUB FUNCTION, etc.
	!
	WHEN ERROR IN
		LINPUT #SOURCE.CH%, TEXT$
	USE
		CONTINUE 5500
	END WHEN

	TEXT$ = EDIT$(TEXT$, 8% + 128%)

	GOTO 5100 IF LEFT(TEXT$, 1%) = "%"

	IF (LEFT(TEXT$, 3%) = "SUB") OR (LEFT(TEXT$, 8%) = "FUNCTION")
	THEN
		PRINT #WORK.CH%, ".LM -5"
		PRINT #WORK.CH%, ".B"
		PRINT #WORK.CH%, "Definition:"
		PRINT #WORK.CH%, ".B"
		PRINT #WORK.CH%, ".LM +5"
 DefLoop:
		PRINT #WORK.CH%, FN_RNO$(TEXT$)

		!
		! Handle continuation statements
		!
		WHILE RIGHT(TEXT$, LEN(TEXT$)) = "&"
			LINPUT #SOURCE.CH%, TEXT$
			PRINT #WORK.CH%, ".BREAK"
			PRINT #WORK.CH%, FN_RNO$(TEXT$)
		NEXT
	END IF

	GOTO 5100 IF LEFT(TEXT$, 1%) <> "!"

5200	!
	! Search first part of file for a comment statement,
	! ie. SUB FUNCTION, etc.
	!
	WHEN ERROR IN
		LINPUT #SOURCE.CH%, TEXT$
	USE
		CONTINUE 5500
	END WHEN

	TEXT$ = EDIT$(TEXT$, 8% + 128%)

	GOTO 5500 IF TEXT$ = "%PAGE"

	GOTO 5200 UNLESS TEXT$ = "!++"

5300	!
	! Handle description of function, until the end of description
	! "--" if found, or the end of the source code.
	!
	WHEN ERROR IN
		LINPUT #SOURCE.CH%, TEXT$
	USE
		CONTINUE 5500
	END WHEN

	TEXT$ = EDIT$(TEXT$, 8% + 128%)

	GOTO 5500 IF TEXT$ = "!--"

	TEXT$ = RIGHT(TEXT$, 2%)

	!
	! Handle blank lines
	!
	IF TEXT$ = ""
	THEN
		PRINT #WORK.CH%, ".B"
		GOTO 5300
	END IF

	!
	! Handle titles
	!
	IF LEFT(TEXT$, 1%) == " "
	THEN
		PRINT #WORK.CH%, ".LM -5"
		PRINT #WORK.CH%, FN_RNO$(RIGHT(TEXT$, 2%))
		PRINT #WORK.CH%, ".LM +5"
		GOTO 5300
	END IF

	!
	! Handle the rest of the stuff
	!
	PRINT #WORK.CH%, ".BREAK" IF LEFT(TEXT$, 2%) = ('9'C + '9'C)
	PRINT #WORK.CH%, FN_RNO$(EDIT$(TEXT$, 8%))

	GOTO 5300

5500	!
	! Exit from create
	!
	CLOSE SOURCE.CH%

	RETURN

	%PAGE

9000	!*******************************************************************
	! Runoff text conversion function.
	!
	! Converts text into a format so that runoff will display
	! it as is, and not convert all of hte stars, underlines,
	! and whatnot.
	!*******************************************************************

	DEF FN_RNO$(A$)

		X$ = FN_FIX$(A$, "_", "__")
		X$ = FN_FIX$(X$, "*", "_*")
		X$ = FN_FIX$(X$, "^", "_^")
		X$ = FN_FIX$(X$, "&", "_&")
		X$ = FN_FIX$(X$, "$", "_$")
		X$ = FN_FIX$(X$, "%", "_%")

		FN_RNO$ = X$
	FNEND

	%PAGE

	!*******************************************************************
	! FN_FIX function is used by FN_RNO
	!*******************************************************************

	DEF FN_FIX$(A$, B$, C$)

		X$ = A$

		ILL% = 0%

 FnfixLoop:	ILL% = INSTR(ILL% + 1%, X$, B$)

		IF ILL%
		THEN
			X$ = LEFT(X$, ILL% - 1%) + C$ + &
				RIGHT(X$, ILL% + LEN(B$))
			ILL% = ILL% + LEN(C$) - 1%
			GOTO FnfixLoop
		END IF

		FN_FIX$ = X$
	FNEND

	%PAGE

19000	!*******************************************************************
	! Error Trapping
	!*******************************************************************

	ON ERROR GOTO 0

32767	END SUB
