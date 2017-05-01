1	%TITLE "Check Line Numbers"
	%SBTTL "TK_SPEC_CHECKLINE"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1986, 1988, 1991 BY
	!
	! Computer Management Center, Inc.
	! Idaho Falls, Idaho.
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
	! Abstract:HELP
	!	.p
	!	This program is used to clean out garbage in
	!	other source codes, and to look for problems
	!	that may not be readily apparent.
	!	It is an attempt to produce better code by losing
	!	unnecessary stuff from source files.
	!	.p
	!	It checks other programs to
	!	make sure all of it's line numbers are
	!	in order, that error trapping is for
	!	existing lines, and
	!	all of it's declared functions
	!	are being used.
	!	.note
	!	This is not a very intelligent program.  It looks
	!	at things in ^~very\~ simple terms.  It is only an indication
	!	of possible problems, and is not a final authority.
	!	Please do not trust what this program tells you implictly.
	!	.end note
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_SPEC_CHECKLINE/LINE
	!	$ LINK/EXE=TK_EXE: TK_SPEC_CHECKLINE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE TK_SPEC_CHECKLINE.OBJ;*
	!
	! Author:
	!
	!	09/29/88 - J. Shad Rydalch
	!
	!	07/11/91 - Jeff C. Beard
	!		Code for unused functions.
	!
	! Modification History:
	!
	!	07/11/91 - Craig Tanner
	!		Added check for undefined line number in
	!		error trapping.
	!
	!	07/12/91 - Kevin Handy
	!		Debug error line check.
	!		Remove SMG stuff.
	!
	!	07/14/91 - Kevin Handy
	!		Fix bug with word "case" in a comment.
	!		handle error trapping at a line other than 19000.
	!
	!	07/14/91 - Kevin Handy
	!		Modifications all over the place to fix minor bugs.
	!
	!	07/16/91 - Kevin Handy
	!		Unwound error trapping.
	!
	!	07/16/91 - Kevin Handy
	!		Unwound external definitions.
	!
	!	07/25/91 - Kevin Handy
	!		Modified open to use ACCESS READ, ALLOW MODIFY,
	!		so won't cause everything to show modification
	!		date.
	!
	!	09/17/91 - Kevin Handy
	!		Merged TK_SPEC_CHECKLINE and TK_SPEC_CHECKFUNC
	!		into one program.
	!
	!	09/20/91 - Kevin Handy
	!		Fixed several bugs involved with combination.
	!		Combine lines with "&", Filename printing
	!		problem, unable to match function names.
	!
	!	09/20/91 - Kevin Handy
	!		Modified so that it could handle several external
	!		function definitions on the same line seperated by
	!		commas.
	!
	!	09/20/91 - Kevin Handy
	!		Modified function stuff so that it is not case
	!		sensitive.
	!
	!	09/20/91 - Kevin Handy
	!		Added error trapping for line 100.
	!
	!	09/26/91 - Kevin Handy
	!		Added simple checks for useless/misused whitespace
	!
	!	09/30/91 - Kevin Handy
	!		Removed lines commented out message.
	!
	!	10/10/91 - Kevin Handy
	!		Added checks for <tab><space>, "else...", "then..."
	!
	!	10/11/91 - Kevin Handy
	!		Reduced number of spurious responses.
	!
	!	03/12/92 - Kevin Handy
	!		Added check for duplicate lines in error trapping.
	!
	!	09/22/92 - Kevin Handy
	!		Check for ".table 3.25"
	!
	!	03/18/93 - Kevin Handy
	!		Check for <space><comma>.
	!
	!	03/19/93 - Kevin Handy
	!		Allowed ", ," for null parameters.
	!
	!	03/19/93 - Kevin Handy
	!		Added check for continuation (&) not led by
	!		whitespace.
	!
	!	03/22/93 - Kevin Handy
	!		Check for "\&" at end of lines, which may occur in
	!		documentation (end bold), and don't mark a warning.
	!
	!	03/25/93 - Kevin Handy
	!		Modified to strip off quoted strings before processing
	!		lines, replacing them with an "X" as a position
	!		holder.
	!
	!	03/25/93 - Kevin Handy
	!		Modified to allow comma as only thing at front of
	!		a line.
	!
	!	04/05/93 - Kevin Handy
	!		Modified handling of <TAB><,> so that it doesn't
	!		get flagged on continuation lines.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	05/12/95 - Kevin Handy
	!		modified error message to print file it was
	!		working on when it crashed.
	!
	!	05/16/95 - Kevin Handy
	!		Modified to print an error message if unable to read
	!		source code at line 4100, and continue.
	!		Reformatting.
	!
	!	07/11/95 - Kevin Handy
	!		Handle "()" in parameter definitions better.
	!
	!	08/20/96 - Kevin Handy
	!		Remove lots of commented out code.
	!		Check for "THEN", "ELSE", "END IF" not on their
	!		own lines.
	!
	!	08/24/96 - Kevin Handy
	!		Allow for 'CASE ELSE' statements.
	!
	!	08/26/96 - Kevin Handy
	!		Scan for "CASE<tab>".
	!
	!	10/03/96 - Kevin Handy
	!		Scan for "<tab>=" (ignore "a::b =")
	!
	!	08/28/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Watch for %PAGE which are close together.
	!
	!	03/05/99 - Kevin Handy
	!		Flag "ON ERROR GOTO"
	!
	!	03/05/99 - Kevin Handy
	!		Use new 'WHEN ERROR'
	!
	!	03/15/99 - Kevin Handy
	!		Watch for bad comments
	!
	!	04/08/99 - Kevin Handy
	!		Watch for SCOPE without ever using SCOPE_STRUCT
	!
	!	04/08/99 - Kevin Handy
	!		Use BASIC$STARLET for LIB$ and STR$ routines
	!
	!	04/15/99 - Kevin Handy
	!		Ignore lines in case statements when error trapping
	!		is not enabled.
	!
	!	06/09/99 - Kevin Handy
	!		Lose HelpError (Dead code)
	!
	!	04/11/2000 - Kevin Handy
	!		Drop error message for "ON ERROR" when there also
	!		exists a "WHEN ERROR IN"
	!
	!	07/14/2000 - Kevin Handy
	!		Drop ">" on front of filename printout.
	!
	!	07/25/2000 - Kevin Handy
	!		Change LINE_NUM() to LINE_NUM%()
	!
	!	08/22/2000 - Kevin Handy
	!		Test for short files.
	!
	!	09/14/2000 - Kevin Handy
	!		Add flag to use LIB$DELETE_FILE instead of KILL
	!
	!	10/27/2000 - Kevin Handy
	!		Bitch about "ASCII('X')"
	!
	!	08/30/2001 - Kevin Handy
	!		Watch for the stupid "provides_the_means_to"
	!		and "or_displaying" in the help.
	!
	!	08/31/2001 - Kevin Handy
	!		Added a lot more cases of awful documentation.
	!
	!	09/04/2001 - Kevin Handy
	!		Add check to see if inside of a comment
	!
	!--

	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "STR$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	DECLARE LONG SYS_STATUS
	DIM LINE_NUM%(1000%)
	DIM STRING NAME_FUNC(200)

	MAP (Z) NAME.BUFFER$ = 255%

	%PAGE

	FINAM.CH% = 10%

100	PRINT

	!
	! Get wildcard directory
	!
	WHEN ERROR IN
		PRINT "Wildcard name to check (return to exit) <.BAS>";
		LINPUT FILE.NAME$
	USE
		CONTINUE 5000 IF ERR = 11%
		EXIT HANDLER
	END WHEN

	GOTO ExitProgram IF EDIT$(FILE.NAME$, -1%) = ""

	CONTEXT% = 0%

	JUNK$ = "*.BAS"

110	WHEN ERROR IN
		PRINT "Extreme Tests <N>";
		LINPUT EXTREME$
	USE
		CONTINUE 5000 IF ERR = 11%
		EXIT HANDLER
	END WHEN

	EXTREME$ = LEFT(EDIT$(EXTREME$, -1%), 1%)

200	!
	! Look up one file
	!
	SYS_STATUS = LIB$FIND_FILE(FILE.NAME$, NAME.BUFFER$, CONTEXT%, JUNK$)

	JUNK$ = ""

	GOTO 100 IF (SYS_STATUS AND 1%) = 0%

	xFILENAME$ = TRM$(NAME.BUFFER$)

	GOSUB 4000

	!
	! Set up for next file
	!
	GOTO 200

4000	!*******************************************************************
	! Scan one file
	!*******************************************************************

	LAST_X% = 0%
	LAST_X1% = 0%
	x% = 0%
	NAME.FLAG% = -1%
	NUM_LINE% = 0%
	SEL_ERR% = 0%
	LOOKFORSELECT% = 0%
	FOUNDSELECT% = 0%
	SPACEFRONT% = 0%
	BLANKLINE% = 0%
	SPACEEND% = 0%
	TABEND% = 0%
	SPACECOMMENT% = 0%
	COMMENTOUT% = 0%
	SPACETAB% = 0%
	SPACECOMMA% = 0%
	TABCOMMA% = 0%
	TABSPACE% = 0%
	THENXXX% = 0%
	ELSEXXX% = 0%
	IFTAB% = 0%
	ERRORLIST$ = ""
	ANDSIGN% = 0%
	BADCMC% = 0%
	BADCOMIT% = 0%
	TAILTHEN% = 0%
	TAILELSE% = 0%
	TAILENDIF% = 0%
	CASETAB% = 0%
	EQUALTAB% = 0%
	TABEQUAL% = 0%
	PAGELINE% = 0%
	ONERRORGOTO% = 0%
	WHENERROR% = 0%
	SCOPESTRUCT% = 0%
	COUNTLINE% = 0%
	KILLFLAG% = 0%
	ASCIIQUOTE% = 0%
	PROVIDESTHE% = 0%
	ORDISPLAY% = 0%
	INTHEREPORT% = 0%
	THECONTENTS% = 0%
	INTHEFORM% = 0%
	VALUEENTERED% = 0%
	SETTINGINTHE% = 0%
	INCOMMENT% = 0%

	WHEN ERROR IN
		OPEN xFILENAME$ FOR INPUT AS FILE FINAM.CH%, &
			ACCESS READ, ALLOW MODIFY
	USE
		PRINT xFILENAME$ + "  " + ERT$(ERR)
		CONTINUE 4900
	END WHEN

4100	WHEN ERROR IN
		LINPUT #FINAM.CH%, TEXT$
	USE
		IF ERR <> 11%
		THEN
			IF NAME.FLAG%
			THEN
				PRINT
				PRINT xFILENAME$
				NAME.FLAG% = 0%
			END IF
			PRINT ERT$(ERR) + " for line " + STR$(NUM%)
		END IF

		CONTINUE 4900
	END WHEN

	COUNTLINE% = COUNTLINE% + 1%

	!
	! Watch for %PAGE calls
	!
	IF EDIT$(TEXT$, -1%) = "%PAGE"
	THEN
		!
		! Use an arbitrary limit for lines between %PAGE's
		!
		IF PAGELINE% < 8%
		THEN
			GOSUB PrintName
			PRINT "  %PAGE's very close together"
		END IF
		PAGELINE% = 0%
	ELSE
		PAGELINE% = PAGELINE% + 1%
	END IF

	IF (INSTR(1%, " 0123456789", LEFT$(TEXT$, 1%)) >= 2%)
	THEN
		!
		!	FIND LAST DIGIT
		!
		I% = 1%
		I% = I% + 1% &
			WHILE INSTR(1%, " 0123456789", &
			MID(TEXT$, I% + 1%, 1%)) > 1%
		X% = VAL%(LEFT$(TEXT$, I%))
		TEXT$ = RIGHT(TEXT$, I% + 1%)

		IF X% < LAST_X%
		THEN
			GOSUB PrintName
			PRINT "  Line out of sequence ";X%
		END IF

		IF X% = LAST_X%
		THEN
			GOSUB PrintName
			PRINT "  Line number duplicated ";X%
		END IF

		!
		! Load all line numbers into array
		!
		NUM_LINE% = NUM_LINE% + 1%
		LINE_NUM%(NUM_LINE%) = X%
	END IF

	IF X% = 0%
	THEN
		GOSUB PrintName
		PRINT "  Missing first line number"
		x% = -1%
	END IF

	IF INSTR(1%, TEXT$, "!" + "++")
	THEN
		INCOMMENT% = 1%
	END IF

	IF INSTR(1%, TEXT$, "!" + "--")
	THEN
		INCOMMENT% = 0%
	END IF

	IF INCOMMENT% <> 0%
	THEN
		IF INSTR(1%, TEXT$, "provides the " + "means") OR &
			INSTR(1%, TEXT$, "provides a " + "means") OR &
			INSTR(1%, TEXT$, "provides for " + "entry")
		THEN
			PROVIDESTHE% = 1%
		END IF

		IF INSTR(1%, TEXT$, "or " + "display") OR &
			INSTR(1%, TEXT$, "or be " + "display")
		THEN
			ORDISPLAY% = 1%
		END IF

		IF INSTR(1%, TEXT$, "in the " + "report") OR &
			INSTR(1%, TEXT$, "of the " + "report")
		THEN
			INTHEREPORT% = 1%
		END IF

		IF INSTR(1%, TEXT$, "he contents " + "of")
		THEN
			THECONTENTS% = 1%
		END IF

		IF INSTR(1%, TEXT$, "in the " + "form") OR &
			INSTR(1%, TEXT$, "of the " + "form")
		THEN
			INTHEFORM% = 1%
		END IF

		IF INSTR(1%, TEXT$, "value entered " + "in")
		THEN
			VALUEENTERED% = 1%
		END IF

		IF INSTR(1%, TEXT$, "he setting " + "in the")
		THEN
			SETTINGINTHE% = 1%
		END IF
	END IF

	IF INSTR(1%, TEXT$, "  " + "- Kevin") AND EXTREME$ = "Y"
	THEN
		GOSUB PrintName
		PRINT "  Bad Modification Comment"
	END IF

	IF INSTR(1%, TEXT$, '9'C + "IF" + '9'C)
	THEN
		IFTAB% = -1%
	END IF

	!
	! Check for goofy if-then-else-endif sequences.
	!
	TEXT1$ = EDIT$(TEXT$, 8% + 16% + 32% + 128%)

	IF LEFT(TEXT1$, 5%) = "KILL "
	THEN
		KILLFLAG% = -1%
	END IF

	IF LEFT(TEXT1$, 5%) = "THEN"
	THEN
		IF RIGHT(TEXT1$, 6%) <> ""
		THEN
			THENXXX% = -1%
		END IF
	END IF

	IF LEFT(TEXT1$, 5%) = "ELSE"
	THEN
		IF RIGHT(TEXT1$, 6%) <> ""
		THEN
			ELSEXXX% = -1%
		END IF
	END IF

	!
	! Strip off quoted strings
	!
	L% = STR$FIND_FIRST_IN_SET(TEXT$, "'" + '"')

	WHILE L%
		X$ = MID(TEXT$, L%, 1%)
		L1% = INSTR(L% + 1%, TEXT$, X$)
		L1% = LEN(TEXT$) IF L1% = 0%
		TEXT$ = LEFT(TEXT$, L% - 1%) + "X" + RIGHT(TEXT$, L1% + 1%)

		L% = STR$FIND_FIRST_IN_SET(TEXT$, "'" + '"')
	NEXT

	!
	! Check for <Tab><,>
	!
	L% = INSTR(1%, TEXT$, "	" + ",")
	IF (L% <> 0%)
	THEN
		IF TRM$(LEFT(TEXT$, L% - 1%)) <> ""
		THEN
			TABCOMMA% = -1%
		END IF
	END IF

	!
	! Check for CMC spelled wrong
	!
	IF INSTR(1%, TEXT$, "Computer management Center")
	THEN
		BADCMC% = -1%
	END IF

	!
	! Check for commitment spelled wrong
	!
	IF INSTR(1%, TEXT$, "committment")
	THEN
		BADCOMIT% = -1%
	END IF

	I% = INSTR(1%, TEXT$, "=" + "	")
	IF I%
	THEN
		EQUALTAB% = -1%
	END IF

	I% = (INSTR(1%, TEXT$, "	" + "=") AND &
		(INSTR(1%, TEXT$, "::") = 0%))
	IF I%
	THEN
		TABEQUAL% = -1%
	END IF

	!
	! Pull in more if there is a continuation
	!
	WHILE RIGHT(TEXT$, LEN(TEXT$)) = "&"

		IF INSTR(1%, " 	\", MID(TEXT$, LEN(TEXT$) - 1%, 1%)) = 0%
		THEN
			ANDSIGN% = -1%
		END IF

		LINPUT #FINAM.CH%,TEXT1$

		!
		! Strip off quoted strings
		!
		L% = STR$FIND_FIRST_IN_SET(TEXT1$, "'" + '"')

		WHILE L%
			X$ = MID(TEXT1$, L%, 1%)
			L1% = INSTR(L% + 1%, TEXT1$, X$)
			L1% = LEN(TEXT1$) IF L1% = 0%
			TEXT1$ = LEFT(TEXT1$, L% - 1%) + "X" + &
				RIGHT(TEXT1$, L1% + 1%)

			L% = STR$FIND_FIRST_IN_SET(TEXT1$, "'" + '"')
		NEXT

		!
		! Check for <Tab><,>
		!
		L% = INSTR(1%, TEXT1$, "	" + ",")
		IF (L% <> 0%)
		THEN
			IF TRM$(LEFT(TEXT1$, L% - 1%)) <> ""
			THEN
				TABCOMMA% = -1%
			END IF
		END IF

		I% = INSTR(1%, TEXT1$, "=" + "	")
		IF I%
		THEN
			EQUALTAB% = -1%
		END IF

		I% = (INSTR(1%, TEXT$, "	" + "=") AND &
			(INSTR(1%, TEXT$, "::") = 0%))
		IF I%
		THEN
			TABEQUAL% = -1%
		END IF

		TEXT$ = TRM$(LEFT(TEXT$, LEN(TEXT$) - 1%)) + TEXT1$
 !		TEXT$ = TEXT$ + TEXT1$

	NEXT

	!
	! Check for various useless whitespace
	!
	IF (TEXT$ <> "") AND (EDIT$(TEXT$, -1%) = "")
	THEN
		BLANKLINE% = -1%
	ELSE
		IF INSTR(1%, TEXT$, " " + "	")
		THEN
			SPACETAB% = -1%
		END IF

		IF (INSTR(1%, TEXT$, " ,") <> 0%) AND &
			(INSTR(1%, TEXT$, ", ,") = 0%)
		THEN
			SPACECOMMA% = -1%
		END IF

		I% = INSTR(1%, TEXT$, "	" + " ")
		IF I%
		THEN
			I1% = INSTR(1%, TEXT$, "!")
			IF (I1% = 0%) OR (I1% > I%)
			THEN
				TABSPACE% = -1%
			END IF
		END IF

		I% = INSTR(1%, TEXT$, "CASE" + "	")
		IF I%
		THEN
			CASETAB% = -1%
		END IF

		IF ASCII(TEXT$) = 32%
		THEN
			IF LEFT(TEXT$, 2%) == " !"
			THEN
				COMMENTOUT% = -1%
			ELSE
				!
				! Don't display error if it is a label
				!
				SPACEFRONT% = -1% IF INSTR(1%, TEXT$, ":") = 0%
			END IF
		END IF

		I% = ASCII(RIGHT(TEXT$, LEN(TEXT$)))
		IF I% = 32%
		THEN
			SPACEEND% = -1%
		END IF

		IF I% = 9%
		THEN
			TABEND% = -1%
		END IF
	END IF

	!
	! Past line 19999, stop looking for error trapping
	!
	IF TEXT$ == '9'C + "END SELECT"
	THEN
		LOOKFORSLECT% = 0%
		FOUNDSELECT% = 0%
	END IF


	!
	! Lose any junk spaces
	!
	TEXT1$ = EDIT$(TEXT$, -1%)

	IF INSTR(1%, TEXT$, "!" + "  ") OR INSTR(1%, TEXT$, "! " + "	")
	THEN
		SPACECOMMENT% = -1%
	END IF

	!
	! Lose comments
	!
	I% = INSTR(1%, TEXT1$, "!")
	IF I%
	THEN
		IF INSTR(I%, TEXT1$, ".TABLE 3.25")
		THEN
			GOSUB PrintName
			PRINT "  Table 3.25"
		END IF
		TEXT1$ = LEFT(TEXT1$, I% - 1%)
	END IF

	!
	! Check for ASCII("x")
	!
	IF INSTR(1%, TEXT1$, 'ASCII("')
	THEN
		ASCIIQUOTE% = -1%
	END IF

	IF INSTR(1%, TEXT1$, "ASCII('")
	THEN
		ASCIIQUOTE% = -1%
	END IF

	!
	! Lose any text strings
	!
	I% = INSTR(1%, TEXT1$, '"')
	I1% = INSTR(I% + 1%, TEXT1$, '"')
	WHILE I%
		IF I1%
		THEN
			TEXT1$ = LEFT(TEXT1$, I% - 1%) + RIGHT(TEXT1$, I1% + 1%)
			I% = INSTR(I% + 1%, TEXT1$, '"')
			I1% = INSTR(I% + 1%, TEXT1$, '"')
		ELSE
			TEXT1$ = LEFT(TEXT1$, I% - 1%)
			I% = 0%
		END IF
	NEXT

	!
	! Check for "xxx THEN xxx"
	!
	IF (TEXT1$ <> "THEN") AND &
		(INSTR(1%, " " + TEXT1$ + " ", " THEN ") <> 0%) AND &
		(THENXXX% = 0%)
	THEN
		TAILTHEN% = -1%
	END IF

	!
	! Check for "xxx ELSE xxx"
	!
	IF ((TEXT1$ <> "ELSE") AND (TEXT1$ <> "CASE ELSE")) AND &
		(INSTR(1%, " " + TEXT1$ + " ", " ELSE ") <> 0%) AND &
		(ELSEXXX% = 0%)
	THEN
		TAILELSE% = -1%
	END IF

	!
	! Check for "xxx END IF xxx"
	!
	IF (TEXT1$ <> "END IF") AND &
		(INSTR(1%, " " + TEXT1$ + " ", " END IF ") <> 0%)
	THEN
		TAILENDIF% = -1%
	END IF

	!
	! Search for external definitions
	!
	IF INSTR(1%, TEXT1$, "EXTERNAL")
	THEN
		V% = INSTR(1%, TEXT1$, "FUNCTION")
		IF V%
		THEN
			TEMP$ = RIGHT$(TEXT1$, V% + 8%)
			GOSUB FunctionNames
			GOTO 4100
		END IF
	END IF

	!
	! Check for old "ON ERROR GOTO" error trapping
	!
	IF INSTR(1%, TEXT1$, "ONERRORGOTO")
	THEN
		ONERRORGOTO% = ONERRORGOTO% + 1% &
			IF INSTR(1%, TEXT1$, "ONERRORGOTO%") = 0%
	END IF

	IF INSTR(1%, TEXT1$, "WHENERRORIN")
	THEN
		WHENERROR% = WHENERROR% + 1%
	END IF

	!
	! Check for SCOPE and SCOPE_STRUCT
	!
	!	Leave these two items on the same line so that it
	!	doesn't show up as an error when they are seen.
	!
	IF INSTR(1%, TEXT1$, "SCOPE") AND (INSTR(1%, TEXT1$, "SCOPE_STRUCT") = 0%)
	THEN
		SCOPESTRUCT% = SCOPESTRUCT% OR 1%
	END IF

	IF INSTR(1%, TEXT1$, "SCOPE_STRUCT")
	THEN
		SCOPESTRUCT% = SCOPESTRUCT% OR 2%
	END IF

	!
	! See if external functions are used here
	!
	FOR SEARCH_NUM% = 0% TO LAST_X1% - 1%

		IF NAME_FUNC(SEARCH_NUM%) <> ""
		THEN
			!
			! MAKES IT BLANK IF IT'S NOT THE DECLARATION STATEMENT
			!
			IF INSTR(1%, TEXT1$, NAME_FUNC(SEARCH_NUM%))
			THEN
				NAME_FUNC(SEARCH_NUM%) = ""
			END IF
		END IF

	NEXT SEARCH_NUM%

	GOTO LookTrap IF LOOKFORSELECT%

	!
	! At line 19000, so start looking for error trapping
	!
	IF (INSTR(1%, TEXT1$, "SELECTERL") <> 0%)	! (X% = 19000%) OR
	THEN
		LOOKFORSELECT% = -1%
		FOUNDSELECT% = -1%
	END IF

	LAST_X% = X%

	GOTO 4100

 LookTrap:

4200	IF LOOKFORSELECT%
	THEN
		IF INSTR(1%, TEXT1$, "SELECTERL")
		THEN
			FOUNDSELECT% = -1%
			GOTO 4100
		END IF

		IF INSTR(1%, TEXT1$, "SELECT")
		THEN
			FOUNDSELECT% = NOT FOUNDSELECT%
		END IF
	END IF

	IF FOUNDSELECT%
	THEN
		PTR% = INSTR(1%, TEXT1$, "CASE")
		IF PTR%
		THEN
			PTR% = PTR% + 4%
			GOSUB PullLine
			WHILE NUM%
				GOSUB ExtraTrap
				IF MID(TEXT1$, PTR%, 1%) = ","
				THEN
					PTR% = PTR% + 1%
					GOSUB PullLine
				ELSE
					NUM% = 0%
				END IF
			NEXT
			GOTO 4100
		END IF

		PTR% = INSTR(1%, TEXT1$, "ERL=")
		IF PTR%
		THEN
			PTR% = PTR% + 4%
			GOSUB PullLine
			GOSUB ExtraTrap
			GOTO 4100
		END IF
	END IF

	GOTO 4100

4900	!*******************************************************************
	! Finish up
	!*******************************************************************

	!
	! Program too short
	!
	IF COUNTLINE% < 5%
	THEN
		GOSUB PrintName
		PRINT "  Source code seems to be too short"
	END IF

	!
	! Print out unused functions
	!
	FOR I% = 0% TO LAST_X1% - 1%
		IF NOT(NAME_FUNC(I%) = "")
		THEN
			GOSUB PrintName
			PRINT "  Function declared but not used: "; NAME_FUNC(I%)
		END IF
	NEXT I%

	IF SPACEFRONT%
	THEN
		GOSUB PrintName
		PRINT "  Spaces at front of lines instead of tabs"
	END IF

	IF BLANKLINE%
	THEN
		GOSUB PrintName
		PRINT "  Lines consisting of whitespace"
	END IF

	IF SPACEEND%
	THEN
		GOSUB PrintName
		PRINT "  Lines ending with spaces"
	END IF

	IF TABEND%
	THEN
		GOSUB PrintName
		PRINT "  Lines ending with tabs"
	END IF

	IF SPACECOMMENT%
	THEN
		GOSUB PrintName
		PRINT "  Spaces instead of tabs used to format comments"
	END IF

 !	IF COMMENTOUT%
 !	THEN
 !		GOSUB PrintName
 !		PRINT "  Lines commented out"
 !	END IF

	IF SPACETAB%
	THEN
		GOSUB PrintName
		PRINT "  <Space><tab> sequence seen"
	END IF

	IF SPACECOMMA%
	THEN
		GOSUB PrintName
		PRINT "  <Space><,> sequence seen"
	END IF

	IF TABCOMMA%
	THEN
		GOSUB PrintName
		PRINT "  <Tab><,> sequence seen"
	END IF

	IF CASETAB%
	THEN
		GOSUB PrintName
		PRINT "  CASE<tab> sequence seen"
	END IF

	IF EQUALTAB%
	THEN
		GOSUB PrintName
		PRINT "  =<tab> sequence seen"
	END IF

	IF TABEQUAL%
	THEN
		GOSUB PrintName
		PRINT "  <tab>= sequence seen"
	END IF

	IF ANDSIGN%
	THEN
		GOSUB PrintName
		PRINT "  <&> not preceded by whitespace"
	END IF

	IF TABSPACE%
	THEN
		GOSUB PrintName
		PRINT "  <tab><Space> sequence seen"
	END IF

	IF THENXXX%
	THEN
		GOSUB PrintName
		PRINT "  'THEN' not alone on a line"
	END IF

	IF ELSEXXX%
	THEN
		GOSUB PrintName
		PRINT "  'ELSE' not alone on a line"
	END IF

	IF ASCIIQUOTE%
	THEN
		GOSUB PrintName
		PRINT "  ASCII('X') should be A'X'B"
	END IF

	IF IFTAB%
	THEN
		GOSUB PrintName
		PRINT "  IF<tab> Seen"
	END IF

	IF BADCMC%
	THEN
		GOSUB PrintName
		PRINT "  Computer >m<anagement Center"
	END IF

	IF BADCOMIT%
	THEN
		GOSUB PrintName
		PRINT "  Commitment spelled Committment"
	END IF

	IF TAILTHEN%
	THEN
		GOSUB PrintName
		PRINT "  'THEN' not on it's own line"
	END IF

	IF TAILELSE%
	THEN
		GOSUB PrintName
		PRINT "  'ELSE' not on it's own line"
	END IF

	IF TAILENDIF%
	THEN
		GOSUB PrintName
		PRINT "  'ENDIF' not on it's own line"
	END IF

	IF (ONERRORGOTO% <> 0%) AND (EXTREME$ = "Y") AND (WHENERROR% = 0%)
	THEN
		GOSUB PrintName
		PRINT "  Uses old 'ON ERROR" + " GOTO'"
	END IF

	IF SCOPESTRUCT% = 1%
	THEN
		GOSUB PrintName
		PRINT "  Uses SCOPE but never references SCOPE_STRUCT"
	END IF

	IF KILLFLAG% = 1%
	THEN
		GOSUB PrintName
		PRINT "  Uses KILL instead of LIB$DELETE_FILE"
	END IF

	IF PROVIDESTHE% <> 0%
	THEN
		GOSUB PrintName
		PRINT "  Has 'provides" + " the means to' in documentation"
	END IF

	IF ORDISPLAY% <> 0%
	THEN
		GOSUB PrintName
		PRINT "  Has 'or " + "display' in documentation"
	END IF

	IF INTHEREPORT% <> 0%
	THEN
		GOSUB PrintName
		PRINT "  Has 'in the " + "report' in documentation"
	END IF

	IF THECONTENTS% <> 0%
	THEN
		GOSUB PrintName
		PRINT "  Has 'the contents " + "of' in documentation"
	END IF

	IF INTHEFORM% <> 0%
	THEN
		GOSUB PrintName
		PRINT "  Has 'in the " + "form' in documentation"
	END IF

	IF VALUEENTERED% <> 0%
	THEN
		GOSUB PrintName
		PRINT "  Has 'value entered " + "in' in documentation"
	END IF

	IF SETTINGINTHE% <> 0%
	THEN
		GOSUB PrintName
		PRINT "  Has 'The setting " + "in the' in documentation"
	END IF

	RETURN

5000	!*******************************************************************
	! Exit Program
	!*******************************************************************
 ExitProgram:

	GOTO 32767

	!*******************************************************************
	! Check error trap lines
	!*******************************************************************
 ExtraTrap:

	!
	! If we aren't trapping errors, this doesn't mean anything
	!
	RETURN IF ONERRORGOTO% = 0%

	NUM_FOUND% = 0%

	FOR INDEX% = 1% TO NUM_LINE%
		IF NUM% = LINE_NUM%(INDEX%)
		THEN
			NUM_FOUND% = 1%
		END IF
	NEXT INDEX%

	IF NUM_FOUND% = 0%
	THEN
		GOSUB PrintName
		PRINT "  Error trapping for line " + STR$(NUM%) + &
			", which does not exist"
	END IF

	NUMX$ = "!" + NUM1$(NUM%) + "!"

	IF INSTR(1%, ERRORLIST$, NUMX$)
	THEN
		GOSUB PrintName
		PRINT "  Duplicate Error Trap for line " + STR$(NUM%)
	END IF

	ERRORLIST$ = ERRORLIST$ + NUMX$

	RETURN

	!*******************************************************************
	! Print out program name, unless it's already been printed.
	!*******************************************************************
 PrintName:
17000	IF NAME.FLAG%
	THEN
		PRINT
		PRINT xFILENAME$
		NAME.FLAG% = 0%
	END IF

	RETURN

	!*******************************************************************
	! Pull line number off of program
	!*******************************************************************
 PullLine:
17100	K% = PTR%
	PTR% = PTR% + 1% UNTIL &
		INSTR(1%, " 0123456789", MID(TEXT1$, PTR% + 1%, 1%)) <= 1% &
		OR PTR% >= LEN(TEXT1$)

	WHEN ERROR IN
		NUM% = VAL%(SEG$(TEXT1$, K%, PTR%))
	USE
		NUM% = 0%
	END WHEN

	PTR% = PTR% + 1%
	PTR% = PTR% + 1% UNTIL &
		INSTR(1%, " %", MID(TEXT1$, PTR%, 1%)) <= 1% &
		OR PTR% >= LEN(TEXT1$)

17190	RETURN


17200	!*******************************************************************
	! Rip function names off of source lines
	!*******************************************************************

 FunctionNames:
17210	!
	! Strip off parameter declarations
	!
	I1% = INSTR(1%, TEMP$, "()")
	IF (I1% <> 0%)
	THEN
		TEMP$ = LEFT(TEMP$, I1% - 1%) + RIGHT(TEMP$, I1% + 2%)
		GOTO 17210
	END IF

	I1% = INSTR(1%, TEMP$, "(")
	I2% = INSTR(I1%, TEMP$, ")")
	IF (I1% <> 0%) AND (I2% <> 0%)
	THEN
		TEMP$ = LEFT(TEMP$, I1% - 1%) + RIGHT(TEMP$, I2% + 1%)
		GOTO 17210
	END IF

	!
	! Pull off all function names, even if seperated by commas.
	!
	TEMP$ = TEMP$ + ","

	WHILE INSTR(1%, TEMP$, ",")

		I1% = INSTR(1%, TEMP$, ",")
		NAME_FUNC(LAST_X1%) = EDIT$(LEFT(TEMP$, I1% - 1%), -1%)
		LAST_X1% = LAST_X1% + 1%
		TEMP$ = RIGHT(TEMP$, I1% + 1%)

	NEXT

	!
	! We is done.
	!
	RETURN

32767	END
