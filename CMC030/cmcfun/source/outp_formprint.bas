1	%TITLE "Print Out a Section of the Form"
	%SBTTL "OUTP_FORMPRINT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG OUTP_FORMPRINT(UTL_REPORTX_CDD UTL_REPORTX, &
		LONG PRINT_GROUP, &
		STRING FORM_TEXT, &
		LONG FORM_GROUPS, &
		FORM_GROUP_CDD FORM_GROUP(), &
		LONG FLAG)

	!
	!	COPYRIGHT (C) 1987 BY
	!	Computer Management Center, Idaho Falls, Idaho.
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
	!	This function is used to print out the form groups
	!	with the formatting embeded in them.
	!
	! Parameters:
	!
	!	UTL_REPORX
	!		Structure used for outputting a report.
	!
	!	PRINT_GROUP
	!		Passed pointer to the proper group to print.
	!
	!	FORM_TEXT$
	!		The actual form to print.
	!
	!	FORM_GROUPS
	!		The passed number of groups on the form.
	!
	!	FORM_GROUP()
	!		The definition of the groups.
	!
	!	FLAG
	!	.list
	!		Bit encoded flag word
	!		1 - Printing a verify check
	!	.endlist
	!
	!	Returs number of lines printed.
	!
	! Example:
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:OUTP_FORMPRINT/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP OUTP_FORMPRINT
	!	$ DELETE OUTP_FORMPRINT.OBJ;*
	!
	! Author:
	!
	!	10/06/87 - Kevin Handy
	!
	! Modification history:
	!
	!	04/18/88 - Kevin Handy
	!		Modified to handle long segments of text that
	!		must wrap around through any number of lines.
	!		(Format ~nnn).
	!
	!	08/03/88 - Kevin Handy
	!		Added FORMAT() and EDIT() functions to string
	!		expression parsing.
	!
	!	01/22/90 - Kevin Handy
	!		Fixed bug in missing include for SCOPE which
	!		could cause the program to crash horribly.
	!
	!	01/23/90 - Kevin Handy
	!		Fixed bug that caused format like "<0>###.##" to
	!		fail badly.
	!
	!	01/31/92 - Kevin Handy
	!		Allow dots (.) in variable names since Frank has been
	!		using them for years even though they didn't work.
	!
	!	02/01/92 - Kevin Handy
	!		Modified to use STR$FIND_FIRST_NOT_IN_SET to look
	!		for variable names instead of slow for loop.
	!
	!	02/24/92 - Kevin Handy
	!		Modified error trap for "EQ" test from err 51 to
	!		err 52.  This should work correctly.
	!
	!	03/19/92 - Kevin Handy
	!		Added "DATE6" function.
	!
	!	05/21/92 - Kevin Handy
	!		Modified to leave without printing anything if the
	!		PRINT_GROUP is zero.
	!
	!	06/25/92 - Kevin Handy
	!		Added "Right" and "Left" string options.
	!
	!	10/14/92 - Kevin Handy
	!		Added "Incdate" string operation.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	04/25/95 - Kevin Handy
	!		Include FUNCTION.HB inside of local functions.
	!
	!	05/19/95 - Kevin Handy
	!		Added ZIPBAR() function.
	!
	!	03/25/96 - Kevin Handy
	!		Reformat source code.
	!
	!	09/26/96 - Kevin Handy
	!		Clean up.
	!
	!	09/24/97 - Kevin Handy
	!		Added CODE39() function
	!
	!	10/13/97 - Kevin Handy
	!		Modifications to be able to handle alternate
	!		printer types in the Code39 function.
	!
	!	10/16/97 - Kevin Handy
	!		Fix problems with CODE39 function, need to make
	!		sure PRINTX has been set up.
	!
	!	11/11/97 - Kevin Handy
	!		Lose commented out code
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/08/99 - Kevin Handy
	!		Use BASIC$STARLET for STR$ routines
	!
	!	06/19/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	09/25/2000 - Kevin Handy
	!		Add a "KINGBJ" function to allow pulling in last
	!		"customer number" supplied to parameters.
	!		It is currently a very simple test and may need
	!		changing (checks only first character).
	!
	!	10/30/2000 - Kevin Handy
	!		Use A"x"B
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include necessary files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"
	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:PRINT35.INC"
	MAP (PRINTX) PRINTX_CDD PRINTX

	!
	! Define variables
	!
	EXTERNAL STRING FUNCTION FORM_PARSES
	EXTERNAL REAL   FUNCTION FORM_PARSEN

	!
	! Define map variables
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"

	%INCLUDE "SOURCE:[UTL.OPEN]FORM_GROUP.HB"

	MAP (OUTP_FORMPRINT_PARSE) &
		XFLAG%, &
		PRINTTYPE$ = 8%, &
		PRINTTO%


	!
	! Dimension Statements
	!
	DIM OUT_EXTRA$(100%)

	%PAGE

	ON ERROR GOTO 19000

	!
	! Leave if nothing to print
	!
	IF PRINT_GROUP = 0%
	THEN
		OUTP_FORMPRINT = 0%
		EXIT FUNCTION
	END IF

	PRINTTO% = UTL_REPORTX::PRINTTO

	!
	! Pass flag to all parts of the parser that need it
	!
	XFLAG% = FLAG
	PRINTTYPE$ = UTL_REPORTX::PRINTTYPE

	!
	! Strip out the group for this part
	!
	THIS_PART$ = SEG$(FORM_TEXT, FORM_GROUP(PRINT_GROUP)::POINTER, &
		FORM_GROUP(PRINT_GROUP + 1%)::POINTER - 1%)

	OUT_PART$ = ""
	OUT_EXTRA% = 0%
	LINE_COUNTER% = 0%

	!
	! Search for a part that will need to be processed
	!
 Outerloop:
	I% = INSTR(1%, THIS_PART$, "<")

	IF I% = 0%
	THEN
		OUT_PART$ = OUT_PART$ + THIS_PART$
		GOTO EndOuterLoop
	END IF

	OUT_PART$ = OUT_PART$ + LEFT(THIS_PART$, I% - 1%)

	THIS_PART$ = RIGHT(THIS_PART$, I%)

	!
	! Check for a <* ... *> type
	!
	IF LEFT(THIS_PART$, 2%) = "<*"
	THEN
		I% = INSTR(1%, THIS_PART$, ">")

		COMMAND$ = SEG$(THIS_PART$, 3%, I% - 1%)
		COMMAND$ = LEFT(COMMAND$, LEN(COMMAND$) - 1%) &
			IF RIGHT(COMMAND$, LEN(COMMAND$)) = "*"
		THIS_PART$ = RIGHT(THIS_PART$, I% + 1%)

		GOSUB HandleStars
		GOTO OuterLoop
	END IF

	!
	! Hope for a format statement
	!
	IF LEFT(THIS_PART$, 2%) = '<"'
	THEN
		I% = INSTR(3%, THIS_PART$, '"')

		FMT$ = SEG$(THIS_PART$, 3%, I% - 1%)

		I% = I% + 1% &
			UNTIL (INSTR(1%, ",>", MID(THIS_PART$, I%, 1%)) <> 0%) OR &
				(I% > LEN(THIS_PART$))

		IF MID(THIS_PART$, I%, 1%) = ","
		THEN
			J% = INSTR(I%, THIS_PART$, ">")
			J% = LEN(THIS_PART$) + 1% IF J% = 0%

			VAR$ = EDIT$(SEG$(THIS_PART$, I% + 1%, J% - 1%), &
				2% + 32% + 256%)
		ELSE
			VAR$ = FMT$
			FMT$ = ""
			J% = I%
		END IF

		THIS_PART$ = RIGHT(THIS_PART$, J% + 1%)

		GOSUB HandleFormat
		GOTO OuterLoop
	END IF

	!
	! Complain about undefined information at this point
	!
	CALL ENTR_3MESSAGE(SCOPE, &
		"Undefined <...> command in form '" + THIS_PART$ + "'", 0%)

	THIS_PART$ = RIGHT(THIS_PART$, 2%)
	OUT_PART$ = OUT_PART$ + "<"
	GOTO OuterLoop

 EndOuterLoop:
	!
	! Force a final return.
	!
	IF RIGHT(OUT_PART$, LEN(OUT_PART$)) = '10'C
	THEN
		OUT_THIS$ = OUT_PART$
	ELSE
		OUT_THIS$ = OUT_PART$ + '10'C
	END IF

	GOSUB DumpPart

	!
	! Return the total number of lines that has been printed.
	!
	OUTP_FORMPRINT = LINE_COUNTER%

	EXIT FUNCTION

	%PAGE

	!*******************************************************************
	! Handle the star commands
	!*******************************************************************
 HandleStars:

	!-------------------------------------------------------------------
	! Escape sequence for printer
	!-------------------------------------------------------------------

	!
	! Assign work group
	!
	TEST% = INSTR(1%, COMMAND$, " ")
	GOTO HandleStarsEnd IF TEST% = 0%
	WORK_GROUP$ = TRM$(LEFT(COMMAND$, TEST%))

	COMMAND$ = RIGHT(COMMAND$, TEST% + 1%)

	!
	! What is the test
	!
	TEST% = INSTR(1%, COMMAND$, " ")
	GOTO HandleStarsEnd IF TEST% = 0%
	TEST$ = TRM$(LEFT(COMMAND$, TEST%))

	! What value is to be tested
	!
	COMMAND$ = EDIT$(RIGHT(COMMAND$, TEST% + 1%), -1%)

	!
	! Look up group to find sequence in
	!
	GOTO HandleStarsProcess IF (WORK_GROUP$ = PRINTX::GROUPX(I%)) &
		FOR I% = 1% TO PRINTX::GROUPS

	GOTO HandleStarsEnd

 HandleStarsProcess:
	!
	! We got a match
	!
	SEQ$ = ""

	FOR J% = PRINTX::GROUPP(I%) TO (PRINTX::GROUPP(I% + 1%) AND 2047%) - 1%

		GOTO HandleStarsProcess2 IF PRINTX::ITEM(J%) = ""

1000		WHEN ERROR IN
			PRINT_ITEM = VAL(EDIT$(PRINTX::ITEM(J%), -1%))
			TEST_VALUE = VAL(COMMAND$)
		USE
			CONTINUE 1010 IF ERR = 52%
			EXIT HANDLER
		END WHEN

		!
		! Handle test value and print item as a value
		!
		SELECT TEST$

		CASE "GE"
			IF PRINT_ITEM >= TEST_VALUE AND SEQ$ = ""
			THEN
				SEQ$ = TRM$(PRINTX::SEQU(J%))
			END IF

		CASE "LE"
			IF PRINT_ITEM <= TEST_VALUE AND SEQ$ = ""
			THEN
				SEQ$ = TRM$(PRINTX::SEQU(J%))
			END IF

		CASE ELSE
			IF PRINT_ITEM = TEST_VALUE AND SEQ$ = ""
			THEN
				SEQ$ = TRM$(PRINTX::SEQU(J%))
			END IF

		END SELECT

		GOTO HandleStarsProcess2

1010		!
		! Test for string values
		!
		SELECT TEST$

		CASE "GE"
			IF EDIT$(PRINTX::ITEM(J%), -1%) >= COMMAND$ &
				AND SEQ$ = ""
			THEN
				SEQ$ = TRM$(PRINTX::SEQU(J%))
			END IF

		CASE "LE"
			IF EDIT$(PRINTX::ITEM(J%), -1%) <= COMMAND$ &
				AND SEQ$ = ""
			THEN
				SEQ$ = TRM$(PRINTX::SEQU(J%))
			END IF

		CASE ELSE
			IF EDIT$(PRINTX::ITEM(J%), -1%) = COMMAND$ &
				AND SEQ$ = ""
			THEN
				SEQ$ = TRM$(PRINTX::SEQU(J%))
			END IF

		END SELECT

 HandleStarsProcess2:

	NEXT J%

	CALL WRIT_STRING(SEQ$, RESULT$)

	OUT_PART$ = OUT_PART$ + RESULT$

 HandleStarsEnd:
	RETURN

	%PAGE

	!*******************************************************************
	! Handle the format commands, special function.
	!*******************************************************************

	!
	! This function is used to convert the numeric expressions into
	! printable format.
	!
	DEF FNDIGIT$(SOURCE_STRING$, SEARCH_STRING$, REPLACE_STRING$)

		QTEMP% = INSTR(1%, SOURCE_STRING$, SEARCH_STRING$)

		WHILE QTEMP%
			SOURCE_STRING$ = LEFT(SOURCE_STRING$, QTEMP% - 1%) + &
				REPLACE_STRING$ + &
				RIGHT(SOURCE_STRING$, &
					QTEMP% + LEN(SEARCH_STRING$))

			QTEMP% = INSTR(QTEMP% + 1%, SOURCE_STRING$, &
				SEARCH_STRING$)
		NEXT

		FNDIGIT$ = SOURCE_STRING$
	FNEND

	%PAGE

	!*******************************************************************
	! Handle the format commands
	!*******************************************************************

 HandleFormat:
	!
	! Handle lines of text that may wrap to the next line
	!
	IF LEFT(FMT$, 1%) = "~"
	THEN
		!
		! Strip all new-lines from current output line.
		!
		JUNK% = 0%
		JUNK% = INSTR(JUNK% + 1%, OUT_PART$, '10'C) &
			WHILE INSTR(JUNK% + 1%, OUT_PART$, '10'C)

		IF JUNK%
		THEN
			OUT_THIS$ = OUT_THIS$ + LEFT(OUT_PART$, JUNK%)
			OUT_PART$ = RIGHT(OUT_PART$, JUNK% + 1%)
			GOSUB DumpPart
		END IF

		!
		! Calculate information on how to display
		!
		JLIN% = 0%				! Output line number
		JLEN% = VAL%(RIGHT(FMT$, 2%))		! Length available to
							! display line in.
		JPOS% = LEN(OUT_PART$)			! Character position
							! to start at.
		!
		! Get the text to display
		!
		JUNK% = 1%
		JUNK$ = FORM_PARSES(VAR$, JUNK%)

		!
		! This code will remove all <cr><lf> from text
		!
		JUNK$ = XLATE(JUNK$, STRING$(13%, 0%) + " " + STRING$(18%, 0%) + &
			' !"#$%&' + "'()*+,-./0123456789:;<=>?@" + &
			"ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`" + &
			"abcdefghijklmnopqrstuvwxyz{|}~" + '127'C)

		!
		! Process the text
		!
		WHILE JUNK$ <> ""

			!
			! Quick calculation of line length
			!
			JUNK% = LEN(JUNK$)
			JUNK1% = INSTR(1%, JUNK$, '13'C)
			JUNK% = JUNK1% - 1% &
				IF (JUNK1% < JLEN%) AND (JUNK1% > 0%)

			!
			! If not good enough, work at it
			!
			IF JUNK% > JLEN%
			THEN
				JUNK% = JLEN%
				JUNK% = JUNK% - 1% UNTIL &
					(MID(JUNK$, JUNK%, 1%) = " ") OR &
					(JUNK% = 0%)
				JUNK% = JLEN% &
					IF (JUNK% = 0%)
			END IF

			!
			! Now use that line of text
			!
			IF JLIN% = 0%
			THEN
				OUT_PART$ = OUT_PART$ + LEFT(JUNK$, JUNK%) + &
					SPACE$(JLEN% - JUNK%)
			ELSE
				IF JLIN% > OUT_EXTRA%
				THEN
					OUT_EXTRA$(JLIN%) = ""
					OUT_EXTRA% = JLIN%
				END IF
				OUT_EXTRA$(JLIN%) = OUT_EXTRA$(JLIN%) + &
					SPACE$(JPOS% - LEN(OUT_EXTRA$(JLIN%))) + &
					LEFT(JUNK$, JUNK%)
			END IF

			JUNK$ = RIGHT(JUNK$, JUNK% + 1%)
			JUNK$ = RIGHT(JUNK$, 2%) &
				IF (LEFT(JUNK$, 1%) = '13'C) OR &
				(LEFT(JUNK$, 1%) = " ")
			JLIN% = JLIN% + 1%
		NEXT

		GOTO ExitHandleFormat

	END IF


	!
	! Handle as either a string or a floating point format
	!
	IF INSTR(1%, "'!/", LEFT(FMT$, 1%))
	THEN
		!
		! Handle as a string.  (NOTE: The function FORM_PARSES_VAR
		! does all of the work in handling the preview form)
		!
		JUNK% = 1%
		OUT_PART$ = OUT_PART$ + &
			FORMAT$(FORM_PARSES(VAR$, JUNK%), FMT$)

	ELSE
		!
		! handle as a real (NOTE: We need to handle the preview form
		! for this specially here.  The parseing functions would
		! not do a good job of it.)
		!
		IF (FLAG AND 1%) = 0%
		THEN
			!
			! Use actual value
			!
			JUNK% = 1%
			OUT_PART$ = OUT_PART$ + &
				FORMAT$(FORM_PARSEN(VAR$, JUNK%), FMT$)
		ELSE
			!
			! Fake up something using the format
			!
			JUNK$ = FNDIGIT$(FMT$, "#", "9")
			JUNK$ = FNDIGIT$(JUNK$, "<0>", "9")
			JUNK$ = FNDIGIT$(JUNK$, "<%>", "9")
			JUNK$ = FNDIGIT$(JUNK$, "$$", "$9")

			OUT_PART$ = OUT_PART$ + JUNK$
		END IF
	END IF

 ExitHandleFormat:
	RETURN

	%PAGE

	!*******************************************************************
	! Print out the information currently ready to print.
	!*******************************************************************
 DumpPart:

	!
	! Dump out whatever exists in OUT_THIS$
	!
	WHILE INSTR(1%, OUT_THIS$, '10'C)

		I% = INSTR(1%, OUT_THIS$, '10'C)

		CALL OUTP_LINENOTITLE(LYT_LINE$, UTL_REPORTX, &
			LEFT(OUT_THIS$, I% - 1%), 0%)

		LINE_COUNTER% = LINE_COUNTER% + 1%

		OUT_THIS$ = RIGHT(OUT_THIS$, I% + 1%)

		!
		! Dump out any extra lines connected to first line
		!
		FOR LOOP% = 1% TO OUT_EXTRA%

			CALL OUTP_LINENOTITLE(LYT_LINE$, UTL_REPORTX, &
				OUT_EXTRA$(LOOP%), 0%)

			LINE_COUNTER% = LINE_COUNTER% + 1%

		NEXT LOOP%

		OUT_EXTRA% = 0%
	NEXT

	RETURN

	%PAGE

19000	!***************************************************************
	! E R R O R   T R A P P I N G   C O D E
	!***************************************************************

	ON ERROR GO BACK

	END FUNCTION


30000	FUNCTION REAL FORM_PARSEN(XPARSE$, RIBIT%)
 !*************************************************************************
 ! Form Number parser.  This parser was initily designed for GL_GENFIN, to
 ! replace the hack and burn parser that was originally there.
 !
 ! To calculate a value using the parser, use something similiar to
 ! the following:
 !
 !		JUNK% = 1%
 !		STRING$ = "(1+2)*(3+4)"
 !		RETURN_VALUE = FORM_PARSEN(STRING$, JUNK%)
 !
 ! Where STRING$ is the string to be evaluated, and JUNK% points to
 ! the first character to be parsed.
 !
 ! PARSER returns the calculated value as its returned value, and it
 ! alse returns the character position following the expression in JUNK%
 ! so you can see where it finished.
 !
 !*************************************************************************
 !
 ! Parsing grammer for FORM parser
 !
 !	<expr>	-->	<expr> + <term>			; addition
 !			| <expr> - <term>		; subtraction
 !			| <term>
 !
 !	<term>	-->	<term> * <logical>		; multiplication
 !			| <term> / <logical>		; division
 !			| <logical>
 !
 !	<logical> -->	<logical> AND <factor>		; AND
 !			| <logical> OR <factor>		; OR
 !			| <logical> <> <factor>		; not equal
 !			| <logical> >= <factor>		; more or equal
 !			| <logical> <= <factor>		; less or equal
 !			| <logical> = <factor>		; equal
 !			| <logical> > <factor>		; greater than
 !			| <logical> < <factor>		; less than
 !			| <factor>
 !
 !	<factor> -->	ABS(<expr>)			; Absolute value
 !			| NOT <factor>			; NOT
 !			| (<expr>)			; paren
 !			| -<factor>			; unary minus
 !			| <var>
 !			| number
 !
 !	<var>	-->	letter(letter|digit|":"|"_")*
 !
 ! Logical operations return one (1) for true, and zero (0) for false.
 !
 ! NOTE: Very little syntax checking is done.  This stuff attempts
 ! to get a value, and expects it to work.  Having a check printer
 ! crash is unplesant enough without the program bitching about it
 ! all the time.
 !
 !*************************************************************************

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! External functions
	!
	EXTERNAL REAL FUNCTION FORM_PARSEN_EXPR

	RIBIT% = RIBIT% + 1% WHILE MID(XPARSE$, RIBIT%, 1%) == " "
	FORM_PARSEN = FORM_PARSEN_EXPR(XPARSE$, RIBIT%)
	RIBIT% = RIBIT% + 1% WHILE MID(XPARSE$, RIBIT%, 1%) == " "

	END FUNCTION

30100	FUNCTION REAL FORM_PARSEN_EXPR(XPARSE$, RIBIT%)
 !*************************************************************************
 ! FORM_PARSEN_EXPR is the expression parser of the genfin parser.  It handles
 ! the <expr> parts of the evaluation
 !*************************************************************************

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! External functions
	!
	EXTERNAL REAL FUNCTION FORM_PARSEN_TERM

	RIBIT% = RIBIT% + 1% WHILE MID(XPARSE$, RIBIT%, 1%) == " "
	TEMP = FORM_PARSEN_TERM(XPARSE$, RIBIT%)

30110	RIBIT% = RIBIT% + 1% WHILE MID(XPARSE$, RIBIT%, 1%) == " "

	!
	! Addition
	!
	IF MID(XPARSE$, RIBIT%, 1%) = "+"
	THEN
		RIBIT% = RIBIT% + 1%
		TEMP1 = FORM_PARSEN_TERM(XPARSE$, RIBIT%)
		TEMP = TEMP + TEMP1
		GOTO 30110
	END IF

	!
	! Subtraction
	!
	IF MID(XPARSE$, RIBIT%, 1%) = "-"
	THEN
		RIBIT% = RIBIT% + 1%
		TEMP1 = FORM_PARSEN_TERM(XPARSE$, RIBIT%)
		TEMP = TEMP - TEMP1
		GOTO 30110
	END IF

	FORM_PARSEN_EXPR = TEMP

	END FUNCTION

30200	FUNCTION REAL FORM_PARSEN_TERM(XPARSE$, RIBIT%)
 !*************************************************************************
 ! FORM_PARSEN_TERM is the expression parser of the genfin parser.  It handles
 ! the <term> parts of the evaluation
 !*************************************************************************

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! External functions
	!
	EXTERNAL REAL FUNCTION FORM_PARSEN_LOGICAL

	RIBIT% = RIBIT% + 1% WHILE MID(XPARSE$, RIBIT%, 1%) == " "
	TEMP = FORM_PARSEN_LOGICAL(XPARSE$, RIBIT%)

30210	RIBIT% = RIBIT% + 1% WHILE MID(XPARSE$, RIBIT%, 1%) == " "

	!
	! Multiplication
	!
	IF MID(XPARSE$, RIBIT%, 1%) = "*"
	THEN
		RIBIT% = RIBIT% + 1%
		TEMP1 = FORM_PARSEN_LOGICAL(XPARSE$, RIBIT%)
		TEMP = TEMP * TEMP1
		GOTO 30210
	END IF

	!
	! Division
	!
	IF MID(XPARSE$, RIBIT%, 1%) = "/"
	THEN
		RIBIT% = RIBIT% + 1%
		TEMP1 = FORM_PARSEN_LOGICAL(XPARSE$, RIBIT%)
		TEMP1 = 1. IF TEMP1 = 0.0
		TEMP = TEMP / TEMP1
		GOTO 30210
	END IF

	FORM_PARSEN_TERM = TEMP

	END FUNCTION

30300	FUNCTION REAL FORM_PARSEN_LOGICAL(XPARSE$, RIBIT%)
 !*************************************************************************
 ! FORM_PARSEN_LOGICAL is the expression parser of the genfin parser.  It handles
 ! the <logical> parts of the evaluation
 !*************************************************************************

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! External functions
	!
	EXTERNAL REAL FUNCTION FORM_PARSEN_FACTOR

	RIBIT% = RIBIT% + 1% WHILE MID(XPARSE$, RIBIT%, 1%) == " "

	TEMP = FORM_PARSEN_FACTOR(XPARSE$, RIBIT%)

30310	RIBIT% = RIBIT% + 1% WHILE MID(XPARSE$, RIBIT%, 1%) == " "

	!
	! Logical and
	!
	IF MID(XPARSE$, RIBIT%, 3%) = "AND"
	THEN
		RIBIT% = RIBIT% + 3%
		TEMP1 = FORM_PARSEN_FACTOR(XPARSE$, RIBIT%)
		TEMP = ((TEMP <> 0%) AND (TEMP1 <> 0%)) AND 1%
		GOTO 30310
	END IF

	!
	! Logical or
	!
	IF MID(XPARSE$, RIBIT%, 2%) = "OR"
	THEN
		RIBIT% = RIBIT% + 2%
		TEMP1 = FORM_PARSEN_FACTOR(XPARSE$, RIBIT%)
		TEMP = ((TEMP <> 0%) OR (TEMP1 <> 0%)) AND 1%
		GOTO 30310
	END IF

	!
	! not equal to
	!
	IF MID(XPARSE$, RIBIT%, 2%) = "<>"
	THEN
		RIBIT% = RIBIT% + 2%
		TEMP1 = FORM_PARSEN_FACTOR(XPARSE$, RIBIT%)
		TEMP = (TEMP <> TEMP1) AND 1%
		GOTO 30310
	END IF

	!
	! Less than or equal to
	!
	IF MID(XPARSE$, RIBIT%, 2%) = "<="
	THEN
		RIBIT% = RIBIT% + 2%
		TEMP1 = FORM_PARSEN_FACTOR(XPARSE$, RIBIT%)
		TEMP = (TEMP <= TEMP1) AND 1%
		GOTO 30310
	END IF

	!
	! Greater than or equal to
	!
	IF MID(XPARSE$, RIBIT%, 2%) = ">="
	THEN
		RIBIT% = RIBIT% + 2%
		TEMP1 = FORM_PARSEN_FACTOR(XPARSE$, RIBIT%)
		TEMP = (TEMP >= TEMP1) AND 1%
		GOTO 30310
	END IF

	!
	! Equal to
	!
	IF MID(XPARSE$, RIBIT%, 1%) = "="
	THEN
		RIBIT% = RIBIT% + 1%
		TEMP1 = FORM_PARSEN_FACTOR(XPARSE$, RIBIT%)
		TEMP = (TEMP = TEMP1) AND 1%
		GOTO 30310
	END IF

	!
	! Greater than
	!
	IF MID(XPARSE$, RIBIT%, 1%) = ">"
	THEN
		RIBIT% = RIBIT% + 1%
		TEMP1 = FORM_PARSEN_FACTOR(XPARSE$, RIBIT%)
		TEMP = (TEMP > TEMP1) AND 1%
		GOTO 30310
	END IF

	!
	! Less than
	!
	IF MID(XPARSE$, RIBIT%, 1%) = "<"
	THEN
		RIBIT% = RIBIT% + 1%
		TEMP1 = FORM_PARSEN_FACTOR(XPARSE$, RIBIT%)
		TEMP = (TEMP < TEMP1) AND 1%
		GOTO 30310
	END IF

	FORM_PARSEN_LOGICAL = TEMP

	END FUNCTION

30400	FUNCTION REAL FORM_PARSEN_FACTOR(XPARSE$, RIBIT%)
 !*************************************************************************
 ! FORM_PARSEN_FACTOR is the expression parser of the genfin parser.  It handles
 ! the <factor> parts of the evaluation
 !*************************************************************************

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! External functions
	!
	EXTERNAL REAL FUNCTION FORM_PARSEN_EXPR
	EXTERNAL REAL FUNCTION FORM_PARSEN_FACTOR
	EXTERNAL REAL FUNCTION FORM_PARSEN_NUMBER
	EXTERNAL REAL FUNCTION FORM_PARSEN_VAR

	RIBIT% = RIBIT% + 1% WHILE MID(XPARSE$, RIBIT%, 1%) == " "

	!
	! Handle parentheses
	!
	IF MID(XPARSE$, RIBIT%, 1%) = "("
	THEN
		RIBIT% = RIBIT% + 1%
		FORM_PARSEN_FACTOR = FORM_PARSEN_EXPR(XPARSE$, RIBIT%)
		RIBIT% = RIBIT% + 1% IF MID(XPARSE$, RIBIT%, 1%) = ")"
		EXIT FUNCTION
	END IF

	!
	! Absolute value function
	!
	IF MID(XPARSE$, RIBIT%, 4%) = "ABS("
	THEN
		RIBIT% = RIBIT% + 4%
		FORM_PARSEN_FACTOR = ABS(FORM_PARSEN_EXPR(XPARSE$, RIBIT%))
		RIBIT% = RIBIT% + 1% IF MID(XPARSE$, RIBIT%, 1%) = ")"
		EXIT FUNCTION
	END IF

	!
	! Logical (unary) NOT
	!
	IF MID(XPARSE$, RIBIT%, 3%) = "NOT"
	THEN
		RIBIT% = RIBIT% + 3%
		FORM_PARSEN_FACTOR = (FORM_PARSEN_FACTOR(XPARSE$, RIBIT%) = 0%) &
			AND 1%
		EXIT FUNCTION
	END IF

	!
	! Unary minus
	!
	IF MID(XPARSE$, RIBIT%, 1%) = "-"
	THEN
		RIBIT% = RIBIT% + 1%
		FORM_PARSEN_FACTOR = - FORM_PARSEN_FACTOR(XPARSE$, RIBIT%)
		EXIT FUNCTION
	END IF

	!
	! Numerical constant
	!
	IF INSTR(1%, "0123456789", MID(XPARSE$, RIBIT%, 1%))
	THEN
		FORM_PARSEN_FACTOR = FORM_PARSEN_NUMBER(XPARSE$, RIBIT%)
		EXIT FUNCTION
	END IF

	!
	! Variable
	!
	FORM_PARSEN_FACTOR = FORM_PARSEN_VAR(XPARSE$, RIBIT%)

	END FUNCTION

30500	FUNCTION REAL FORM_PARSEN_NUMBER(XPARSE$, RIBIT%)
 !*************************************************************************
 ! FORM_PARSEN_NUMBER is the expression parser of the genfin parser.  It handles
 ! the <expr> parts of the evaluation.
 !
 ! This function will pull a number off of the input string.
 !*************************************************************************

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "STR$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	RIBIT% = RIBIT% + 1% WHILE MID(XPARSE$, RIBIT%, 1%) == " "

	!
	! Search for the end of the number.  (NOTE: Negitive numbers
	! are handled through the unary minus operator, so it doesn't
	! need to be handled here.)
	!
	I% = STR$FIND_FIRST_NOT_IN_SET(RIGHT(XPARSE$, RIBIT%), &
		" 0123456789.")
	IF I%
	THEN
		I% = RIBIT% + I% - 1%
	ELSE
		I% = LEN(XPARSE$) + 1%
	END IF


	!
	! Calculate the value of the number
	!
	FORM_PARSEN_NUMBER = VAL(SEG$(XPARSE$, RIBIT%, I% - 1%))

	RIBIT% = I%

	END FUNCTION

30600	FUNCTION REAL FORM_PARSEN_VAR(XPARSE$, RIBIT%)
 !*************************************************************************
 ! FORM_PARSEN_VAR is the expression parser of the genfin parser.  It handles
 ! the <var> parts of the evaluation.
 !
 ! This module handles the variables available to genfin, except the
 ! odd (n) type variable which is handled in FORM_PARSEN_FACTOR.
 !*************************************************************************

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "STR$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	!
	! Skip spaces
	!
	RIBIT% = RIBIT% + 1% WHILE MID(XPARSE$, RIBIT%, 1%) == " "

	!
	! Pull off variable name
	!
	I% = STR$FIND_FIRST_NOT_IN_SET(RIGHT(XPARSE$, RIBIT%), &
		"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_:$.")
	IF I%
	THEN
		I% = RIBIT% + I% - 1%
	ELSE
		I% = LEN(XPARSE$) + 1%
	END IF


	!
	! Ask for numerical value of variable
	!
	CALL FORM_LOADVAR(SEG$(XPARSE$, RIBIT%, I% - 1%), TEMP, TEMP$)

	RIBIT% = I%
	FORM_PARSEN_VAR = TEMP

	END FUNCTION


31000	FUNCTION STRING FORM_PARSES(XPARSE$, RIBIT%)
 !*************************************************************************
 ! Form Number parser.  This parser was initily designed for GL_GENFIN, to
 ! replace the hack and burn parser that was originally there.
 !
 ! To calculate a value using the parser, use something similiar to
 ! the following:
 !
 !		JUNK% = 1%
 !		STRING$ = "(1+2)*(3+4)"
 !		RETURN_VALUE = FORM_PARSES(STRING$, JUNK%)
 !
 ! Where STRING$ is the string to be evaluated, and JUNK% points to
 ! the first character to be parsed.
 !
 ! PARSER returns the calculated value as its returned value, and it
 ! alse returns the character position following the expression in JUNK%
 ! so you can see where it finished.
 !
 !*************************************************************************
 !
 ! Parsing grammer for FORM parser
 !
 !	<expr>	-->	<expr> + <term>			; addition
 !			| <term>
 !
 !	<term>	-->	"text"				; Text constant
 !			| <var>
 !			| ( <expr> )
 !			| CKPR(<num-expr>)		; Check-protect value
 !			| FORMAT(<num-expr>,<expr>)	; Format number to string
 !			| EDIT(<expr>,<num-expr>)	; Editing function
 !			| LEFT(<expr>,<num-expr>)	; Left
 !			| RIGHT(<expr>,<num-expr>)	; Right
 !			| DATE6(<expr>)			; mm/dd/yy date
 !			| INCDATE(<expr>,<num-expr>)	; Increment date
 !			| ZIPBAR(<expr>)		; Zipcode Barcode
 !
 !	<var>	-->	letter(letter|digit|":"|"_")*
 !
 ! Logical operations return one (1) for true, and zero (0) for false.
 !
 ! NOTE: Very little syntax checking is done.  This stuff attempts
 ! to get a value, and expects it to work.  Having a check printer
 ! crash is unplesant enough without the program bitching about it
 ! all the time.
 !
 !*************************************************************************

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! External functions
	!
	EXTERNAL STRING FUNCTION FORM_PARSES_EXPR

	RIBIT% = RIBIT% + 1% WHILE MID(XPARSE$, RIBIT%, 1%) == " "
	FORM_PARSES = FORM_PARSES_EXPR(XPARSE$, RIBIT%)
	RIBIT% = RIBIT% + 1% WHILE MID(XPARSE$, RIBIT%, 1%) == " "

	END FUNCTION

31100	FUNCTION STRING FORM_PARSES_EXPR(XPARSE$, RIBIT%)
 !*************************************************************************
 ! FORM_PARSES_EXPR is the expression parser of the genfin parser.  It handles
 ! the <expr> parts of the evaluation
 !*************************************************************************

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! External functions
	!
	EXTERNAL STRING FUNCTION FORM_PARSES_TERM

	RIBIT% = RIBIT% + 1% WHILE MID(XPARSE$, RIBIT%, 1%) == " "
	TEMP$ = FORM_PARSES_TERM(XPARSE$, RIBIT%)

31110	RIBIT% = RIBIT% + 1% WHILE MID(XPARSE$, RIBIT%, 1%) == " "

	!
	! Addition
	!
	IF MID(XPARSE$, RIBIT%, 1%) = "+"
	THEN
		RIBIT% = RIBIT% + 1%
		TEMP1$ = FORM_PARSES_TERM(XPARSE$, RIBIT%)
		TEMP$ = TEMP$ + TEMP1$
		GOTO 31110
	END IF

	FORM_PARSES_EXPR = TEMP$

	END FUNCTION

31200	FUNCTION STRING FORM_PARSES_TERM(XPARSE$, RIBIT%)
 !*************************************************************************
 ! FORM_PARSES_EXPR is the expression parser of the genfin parser.  It handles
 ! the <expr> parts of the evaluation
 !*************************************************************************

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	%INCLUDE "FUNC_INCLUDE:PRINT35.INC"
	MAP (PRINTX) PRINTX_CDD PRINTX

	MAP (OUTP_FORMPRINT_PARSE) &
		XFLAG%, &
		PRINTTYPE$ = 8%, &
		PRINTTO%

	!
	! External functions
	!
	EXTERNAL STRING FUNCTION FORM_PARSES_EXPR
	EXTERNAL STRING FUNCTION FORM_PARSES_VAR
	EXTERNAL REAL   FUNCTION FORM_PARSEN
	EXTERNAL STRING FUNCTION FORM_PARSES
	EXTERNAL STRING FUNCTION OUTP_CODE39
	EXTERNAL LONG   FUNCTION FIND_3PRINTGROUPITEM

	RIBIT% = RIBIT% + 1% WHILE MID(XPARSE$, RIBIT%, 1%) == " "

	!
	! Handle parentheses
	!
	IF MID(XPARSE$, RIBIT%, 1%) = "("
	THEN
		RIBIT% = RIBIT% + 1%
		FORM_PARSES_TERM = FORM_PARSES_EXPR(XPARSE$, RIBIT%)
		RIBIT% = RIBIT% + 1% IF MID(XPARSE$, RIBIT%, 1%) = ")"
		EXIT FUNCTION
	END IF

	!
	! Handle quoted strings
	!
	IF MID(XPARSE$, RIBIT%, 1%) = '"'
	THEN
		I% = INSTR(RIBIT% + 1%, XPARSE$, '"')
		I% = LEN(XPARSE$) + 1% IF I% = 0%
		FORM_PARSES_TERM = SEG$(XPARSE$, RIBIT% + 1%, I% - 1%)
		RIBIT% = I% + 1%
		EXIT FUNCTION
	END IF

	!
	! Handle check-protect value
	!
	IF MID(XPARSE$, RIBIT%, 5%) = "CKPR("
	THEN
		RIBIT% = RIBIT% + 5%

		!
		! Get value (use form_parsen because it skips spaces
		! by itself on the front and end.)
		!
		TEMP = FORM_PARSEN(XPARSE$, RIBIT%)
		!
		! Check protect it
		!
		FORM_PARSES_TERM = PRNT_CHECKPROTECT(TEMP)
		!
		! Skip end paren if there (hopefully is)
		!
		RIBIT% = RIBIT% + 1% &
			IF MID(XPARSE$, RIBIT%, 1%) = ")"

		EXIT FUNCTION
	END IF

	!
	! Handle date6 value
	!
	IF MID(XPARSE$, RIBIT%, 6%) = "DATE6("
	THEN
		RIBIT% = RIBIT% + 6%

		!
		! Get value (use form_parsen because it skips spaces
		! by itself on the front and end.)
		!
		TEMP = FORM_PARSEN(XPARSE$, RIBIT%)
		!
		! Check protect it
		!
		FORM_PARSES_TERM = LEFT(TEMP$, 6%) + RIGHT(TEMP$, 9%)
		!
		! Skip end paren if there (hopefully is)
		!
		RIBIT% = RIBIT% + 1% &
			IF MID(XPARSE$, RIBIT%, 1%) = ")"

		EXIT FUNCTION
	END IF

	!
	! Handle format value
	!
	IF MID(XPARSE$, RIBIT%, 7%) = "FORMAT("
	THEN
		RIBIT% = RIBIT% + 7%

		!
		! Get value (use form_parsen because it skips spaces
		! by itself on the front and end.)
		!
		TEMP = FORM_PARSEN(XPARSE$, RIBIT%)
		!
		! Skip over comma if it is found (better be)
		!
		RIBIT% = RIBIT% + 1% IF MID(XPARSE$, RIBIT%, 1%) == ","
		!
		! Pull off string to use to format
		!
		TEMP$ = FORM_PARSES(XPARSE$, RIBIT%)
		!
		! Skip end paren if there (hopefully is)
		!
		RIBIT% = RIBIT% + 1% &
			IF MID(XPARSE$, RIBIT%, 1%) = ")"

		!
		! Return formatted expression
		!
		FORM_PARSES_TERM = FORMAT$(TEMP, TEMP$)

		EXIT FUNCTION
	END IF

	!
	! Handle edit value
	!
	IF MID(XPARSE$, RIBIT%, 5%) = "EDIT("
	THEN
		RIBIT% = RIBIT% + 5%

		!
		! Get value (use form_parsen because it skips spaces
		! by itself on the front and end.)
		!
		TEMP$ = FORM_PARSES(XPARSE$, RIBIT%)
		!
		! Skip over comma if it is found (better be)
		!
		RIBIT% = RIBIT% + 1% IF MID(XPARSE$, RIBIT%, 1%) == ","
		!
		! Pull off string to use to format
		!
		TEMP = FORM_PARSEN(XPARSE$, RIBIT%)
		!
		! Skip end paren if there (hopefully is)
		!
		RIBIT% = RIBIT% + 1% &
			IF MID(XPARSE$, RIBIT%, 1%) = ")"

		!
		! Return formatted expression
		!
		FORM_PARSES_TERM = EDIT$(TEMP$, TEMP)

		EXIT FUNCTION
	END IF

	!
	! Handle right value
	!
	IF MID(XPARSE$, RIBIT%, 6%) = "RIGHT("
	THEN
		RIBIT% = RIBIT% + 6%

		!
		! Get value (use form_parsen because it skips spaces
		! by itself on the front and end.)
		!
		TEMP$ = FORM_PARSES(XPARSE$, RIBIT%)
		!
		! Skip over comma if it is found (better be)
		!
		RIBIT% = RIBIT% + 1% IF MID(XPARSE$, RIBIT%, 1%) == ","
		!
		! Pull off string to use to format
		!
		TEMP = FORM_PARSEN(XPARSE$, RIBIT%)
		!
		! Skip end paren if there (hopefully is)
		!
		RIBIT% = RIBIT% + 1% &
			IF MID(XPARSE$, RIBIT%, 1%) = ")"

		!
		! Return formatted expression
		!
		FORM_PARSES_TERM = RIGHT(TEMP$, TEMP)

		EXIT FUNCTION
	END IF

	!
	! Handle Left value
	!
	IF MID(XPARSE$, RIBIT%, 5%) = "LEFT("
	THEN
		RIBIT% = RIBIT% + 5%

		!
		! Get value (use form_parsen because it skips spaces
		! by itself on the front and end.)
		!
		TEMP$ = FORM_PARSES(XPARSE$, RIBIT%)
		!
		! Skip over comma if it is found (better be)
		!
		RIBIT% = RIBIT% + 1% IF MID(XPARSE$, RIBIT%, 1%) == ","
		!
		! Pull off string to use to format
		!
		TEMP = FORM_PARSEN(XPARSE$, RIBIT%)
		!
		! Skip end paren if there (hopefully is)
		!
		RIBIT% = RIBIT% + 1% &
			IF MID(XPARSE$, RIBIT%, 1%) = ")"

		!
		! Return formatted expression
		!
		FORM_PARSES_TERM = LEFT(TEMP$, TEMP)

		EXIT FUNCTION
	END IF

	!
	! Handle incdate value
	!
	IF MID(XPARSE$, RIBIT%, 8%) = "INCDATE("
	THEN
		RIBIT% = RIBIT% + 8%

		!
		! Get value (use form_parsen because it skips spaces
		! by itself on the front and end.)
		!
		TEMP$ = FORM_PARSES(XPARSE$, RIBIT%)
		!
		! Skip over comma if it is found (better be)
		!
		RIBIT% = RIBIT% + 1% IF MID(XPARSE$, RIBIT%, 1%) == ","
		!
		! Pull off string to use to format
		!
		TEMP = FORM_PARSEN(XPARSE$, RIBIT%)
		!
		! Skip end paren if there (hopefully is)
		!
		RIBIT% = RIBIT% + 1% &
			IF MID(XPARSE$, RIBIT%, 1%) = ")"
		!
		! Return formatted expression
		!
		TEMP% = TEMP
		FORM_PARSES_TERM = DATE_INVDCODE(DATE_DAYCODE(TEMP$) + TEMP%)

		EXIT FUNCTION
	END IF

	!
	! Handle zipbar value
	!
	IF MID(XPARSE$, RIBIT%, 7%) = "ZIPBAR("
	THEN
		RIBIT% = RIBIT% + 7%

		!
		! Get value (use form_parsen because it skips spaces
		! by itself on the front and end.)
		!
		TEMP$ = FORM_PARSES(XPARSE$, RIBIT%)
		!
		! Check protect it
		!
		FORM_PARSES_TERM = ZIPC_BARCODE(TEMP$)
		!
		! Skip end paren if there (hopefully is)
		!
		RIBIT% = RIBIT% + 1% &
			IF MID(XPARSE$, RIBIT%, 1%) = ")"

		EXIT FUNCTION
	END IF

	!
	! Handle code39 value
	!
	IF MID(XPARSE$, RIBIT%, 7%) = "CODE39("
	THEN
		RIBIT% = RIBIT% + 7%

		!
		! Make sure we have the information we need
		!
		IF PRINTX::ITEMS == 0
		THEN
			CALL OUTP_INITIALIZE(PRINTTYPE$)
		END IF

		!
		! Get value (use form_parsen because it skips spaces
		! by itself on the front and end.)
		!
		TEMP$ = FORM_PARSES(XPARSE$, RIBIT%)

		!
		! Get type of printer
		!
		LOOP% = FIND_3PRINTGROUPITEM("BC", "*", PRINTX)
		IF LOOP% > 0%
		THEN
			INTRO% = VAL%(PRINTX::SEQU(LOOP%))
		ELSE
			INTRO% = 0%
		END IF

		!
		! Generate Barcode
		!
		FORM_PARSES_TERM = OUTP_CODE39(TEMP$, PRINTTO%, INTRO%)

		!
		! Skip end paren if there (hopefully is)
		!
		RIBIT% = RIBIT% + 1% &
			IF MID(XPARSE$, RIBIT%, 1%) = ")"

		EXIT FUNCTION
	END IF

	!
	! Handle KINGB "pick the last numeric value" function
	!
	IF MID(XPARSE$, RIBIT%, 7%) = "KINGBJ("
	THEN
		RIBIT% = RIBIT% + 7%

		!
		! Get first value (use form_parsen because it skips spaces
		! by itself on the front and end.)
		!
		TEMP$ = FORM_PARSES(XPARSE$, RIBIT%)
 ! print "Start: "; TEMP$

		!
		! Skip over comma if it is found (better be)
		!
		WHILE MID(XPARSE$, RIBIT%, 1%) == ","

			RIBIT% = RIBIT% + 1%

			!
			! Pull off string to use to format
			!
			TEMP2$ = FORM_PARSES(XPARSE$, RIBIT%)

			!
			! Keep string if it is numeric (customer #) value
			!
			IF INSTR(1%, "0123456789", LEFT(TEMP2$, 1%))
			THEN
				TEMP$ = TEMP2$
			END IF
 ! print "Param: "; TEMP2$; "=>"; TEMP$
		NEXT

		!
		! Skip end paren if there (hopefully is)
		!
		RIBIT% = RIBIT% + 1% &
			IF MID(XPARSE$, RIBIT%, 1%) = ")"

		!
		! Return string containing numeric value
		!
		FORM_PARSES_TERM = TEMP$

		EXIT FUNCTION
	END IF

	!
	! Must be a variable
	!
	FORM_PARSES_TERM = FORM_PARSES_VAR(XPARSE$, RIBIT%)

	END FUNCTION

31600	FUNCTION STRING FORM_PARSES_VAR(XPARSE$, RIBIT%)
 !*************************************************************************
 ! FORM_PARSES_VAR is the expression parser of the genfin parser.  It handles
 ! the <var> parts of the evaluation.
 !
 ! This module handles the variables available to genfin.
 !*************************************************************************

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "STR$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	!
	! Use flag as passed to OUTP_FORMPRINT
	!
	MAP (OUTP_FORMPRINT_PARSE) &
		XFLAG%, &
		PRINTTYPE$ = 8%, &
		PRINTTO%

	!
	! Skip spaces
	!
	RIBIT% = RIBIT% + 1% WHILE MID(XPARSE$, RIBIT%, 1%) == " "

	!
	! Pull off variable name
	!
	I% = STR$FIND_FIRST_NOT_IN_SET(RIGHT(XPARSE$, RIBIT%), &
		"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_:$.")
	IF I%
	THEN
		I% = RIBIT% + I% - 1%
	ELSE
		I% = LEN(XPARSE$) + 1%
	END IF

	!
	! Ask for numerical value of variable
	!
	CALL FORM_LOADVAR(SEG$(XPARSE$, RIBIT%, I% - 1%), TEMP, TEMP$)

	IF (XFLAG% AND 1%) = 0%
	THEN
		FORM_PARSES_VAR = TRM$(TEMP$)
	ELSE
		FORM_PARSES_VAR = STRING$(LEN(TEMP$), A"X"B)
	END IF

	RIBIT% = I%

	END FUNCTION
