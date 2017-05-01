1	%TITLE "Maintain Financial Statement Command Files"
	%SBTTL "GL_SPEC_FINCMD"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1987, 1988 BY
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
	!	.b
	!	.lm +5
	!	The ^*Command Files\* contain the instructions which cause the system
	!	to generate the financial statement reports. A Command File is created
	!	for each financial statement required by a user and identified in the
	!	Layout File.
	!	.b
	!	The following information lists the various commands available for
	!	the financial statement generator and a brief explanation of each.
	!	.b
	!	^*INPUT\* "message"; VARIABLE
	!	.B
	!	If any INPUTS are needed, they must be the first commands in the
	!	file. No restrictions are placed on the VARIABLE. It can be any
	!	length composed of any characters. Each occurrence of this variable
	!	within the file will be replaced by the string entered in the
	!	layout definition file. The first INPUT will take input #1,
	!	the second will take #2, and so on. Up to 10 inputs may be used.
	!	.b
	!	^*TITLE\*
	!	.B
	!	"text...";TAB(20);"text"
	!	.B
	!	"<DATE######_>"
	!	.B
	!	"<##########_>#text"
	!	.B
	!	END
	!	.B
	!	.lm -5
	!	The title for each page is entered following the TITLE command.
	!	Each line between the TITLE and the END commands is considered
	!	as a line of the title, including blank lines. The word DATE
	!	enclosed in <_> indicates a place for the date to be printed.
	!	A blank field enclosed in <_> indicates a field to be specified
	!	in the file following the PRINT TITLE command. Other hard coded
	!	values include:
	!	.b
	!	.lm +5
	!	<DATE_> - The date entered in the report-settings screen.
	!	.b
	!	<PERIOD_> - The name of the current period as defined in the
	!	period control file.
	!	.b
	!	<NPER_> - The number of periods (including the current one) that
	!	make up this period.
	!	.b
	!	<NPERSTR_> - The number of periods (including the current one) that
	!	make up this period in string format ("^~six\~" instead of "^~6\~").
	!	.b
	!	<COMPANY_> - The company name, as defined in the company
	!	profile.
	!	.b
	!	<REPORT_> - The title for the statement as defined in the
	!	FINCMD option.
	!	.b
	!	<TITLEA_> - Alternate title A as defined in the FINCMD option.
	!	.b
	!	<TITLEB_> - Alternate title B as defined in the FINCMD option.
	!	.b
	!	<TITLEC_> - Alternate title C as defined in the FINCMD option.
	!	.b
	!	<TITLED_> - Alternate title D as defined in the FINCMD option.
	!	.b
	!	<TIME_> - The time the report was started.
	!	.b
	!	<ADATE_> - The actual date the statement printing was
	!	started.
	!	.b
	!	<FROMDATE> - The starting date of the current period (MM/YY/DD).
	!	.b
	!	<TODATE> - The ending date of the current period (MM/YY/DD).
	!	.b
	!	.lm -5
	!	For example, the command 'PRINT TITLE, INSERT' would cause the
	!	following to be printed:
	!	.BREAK
	!	-----------------------------------
	!	.BREAK
	!	text. . . text
	!	.BREAK
	!	06/30/87
	!	.BREAK
	!	INSERT text
	!	.b 1
	!	.BREAK
	!	-----------------------------------
	!	.BREAK
	!	.B
	!	^*SET ZERO\*
	!	.B
	!	Overrides the "Print Zero (Y/N)" entered by the user at the
	!	report settings command with a "Y" from current point in the
	!	statement on.
	!	.B
	!	^*SET NOZERO\*
	!	.B
	!	Overrides the "Print Zero (Y/N)" entered by the user at the
	!	report settings command with a "N" from current point in the
	!	statement on.
	!	.B
	!	^*PRINT\*
	!	.B
	!	The PRINT command is used to print the statement. It can
	!	take the following forms:
	!	.LIST "-"
	!	.LIST ELEMENT
	!	^*PRINT\*
	!	.B
	!	A PRINT alone will print one blank line.
	!	.LIST ELEMENT
	!	^*PRINT TITLE\* {,text.text}
	!	.B
	!	Print the stored title (defined using the TITLE command)
	!	inserting the text (if any) into the insert fields (if any).
	!	.LIST ELEMENT
	!	^*PRINT PAGE\* x
	!	.B
	!	Advance to line x on the current page. Advance to the next
	!	page if x is 0 or not present.
	!	.LIST ELEMENT
	!	^*PRINT F\*x {, <args_> ...}
	!	.B
	!	Print the FORMAT number x. May be followed by arguments
	!	if the format includes variable fields. (See description
	!	of FORMAT.)
	!	.LIST ELEMENT
	!	^*PRINT F\*x <wildcard codes>
	!	.B
	!	Print the codes matching the specification. Note that a
	!	FORMAT must be used to specify what to print from the
	!	chart file.
	!	.b 1
	!	Example: PRINT F1 AC????????
	!	.b 1
	!	_?'s cause detail lines to be printed. _*'s cause summary
	!	of accounts.
	!	.LIST ELEMENT
	!	^*PRINT AND SUM INTO T\*x *Fx <wildcard codes> {, <args_>}
	!	.b
	!	Print (as above) except sum the numbers printed into
	!	totals record number x. The total of column 1 will
	!	be added to Tx(1), the total of column 2 will be added
	!	to Tx(2), etc.
	!	.b 1
	!	Example: PRINT AND SUM INTO T4 F1 AC????????
	!	.b 1
	!	_?'s cause detail lines to be printed. _*'s cause summary
	!	of accounts.
	!	.list element
	!	^*PRINT\* ...;
	!	.B
	!	Any print command may be followed by ';' to indicate no
	!	return after printing.
	!	.end list
	!	.B
	!	^*SUM INTO T\*x *Fx <wildcard codes> {,<args_>}
	!	.B
	!	Identical to PRINT AND SUM except will sum without printing.
	!	.B
	!	^*FORMAT\* x {text},{arguments}
	!	.b
	!	Specify a format to be used for printing.
	!	The format number *x must be a number between zero (0) and nine (9).
	!	The text can contain the following:
	!	.list "-"
	!	.list element
	!	^*"_\##########_\"\*
	!	.B
	!	to specify a string field. (slashes)
	!	.list element
	!	^*"_$_#_#_#,_#_#_#._#_#-"\*
	!	.B
	!	to specify a number field (_$'s, commas, and ._#_#
	!	are optional)
	!	.list element
	!	*"text*"
	!	.b
	!	Will be printed exactly as entered.
	!	.end list
	!	.b
	!	^*LAYOUT "^~xxx\~,^~xxx\~,..."\*
	!	.b
	!	Describe the layout of the following lines so that the statement
	!	may be output to a spreadsheet program.
	!	.p
	!	The ^~xxx\~ part of the layout contains four items for each field
	!	defined in the output: The field type ($=string, V=Number), the field
	!	name (Short description, letters only), a colon ('*:'),
	!	and the ending character position (the starting position is taken
	!	from the previous field).
	!	.b
	!	^*ADD T\*x*(y^*)=T\*m*(n^*)+T\*o*(p*)
	!	.b
	!	Perform arithmetic operations (+,-,_*,_/) and
	!	store the result in total Tx(y). Constants are allowed
	!	(T1(2)=T1(2)_*2) as are parentheses
	!	(T1(2)=T1(2)_*(4+T1(3)))
	!	.b
	!	^*ADD T\*x^*=T\*m^*+T\*o
	!	.b
	!	Perform arithmetic operations (+,-,_*,_/) and
	!	store the result in total Tx.
	!	This version is the same as the previous, except it
	!	does all elements of the total at once, instead of
	!	individual elements.
	!	Constants are allowed
	!	(T1=T1_*2) as are parentheses
	!	(T1=T1_*(4+T1))
	!	.B
	!	^*LOOP\* variable arg1,arg2,arg3
	!	.b
	!	Loops are constructed with the LOOP command. Each
	!	occurrence of the "variable" within the loop will be
	!	replaced by the specified arguments. The number of
	!	arguments determines the number of loops executed.
	!	.NOTE CAUTION
	!	Do not construct two loops with the same
	!	variable name in one command file.
	!	The parser will not know where to terminate
	!	the loop and unpredictable results will occur.
	!	.END NOTE
	!	.b 1
	!	EXAMPLE: LOOP A_$ CAT,DOG,COW
	!	.b 1
	!	^*LOOP VARIABLE\* variable arg1,arg2
	!	.b
	!	Creates another variable that changes with the loop
	!	variable according to the specified arguments.
	!	.B
	!	^*LOOP\* variable ^*END\*
	!	.b
	!	Indicates the end of a loop.
	!	.B
	!	^*GOTO\* argument
	!	.b
	!	Indicates a jump to the line containing the specified
	!	argument. GOTO's are usually used with conditional
	!	statements.
	!	.b
	!	Example:
	!	.b
	!	.literal
	!	GOTO CAT_$ IF A_$=CAT
	!	ADD T1(1)=T2(1)
	!	.end literal
	!	.BREAK
	!	CAT_$
	!	.B
	!	^*TESTPAGE\* argument
	!	.b
	!	Checks for the given number of lines left on the page, and causes a
	!	page break if necessary.
	!	.B
	!	^*Continuation Lines\*
	!	.b
	!	To continue a statement on a new line, place a "_/" at
	!	the end of the first line.
	!	.b 1
	!	Example: PRINT AND SUM_/
	!	.BREAK
	!	INTO T4 F1 AC????????
	!	.b 1
	!	_?'s cause detail lines to be printed. _*'s cause summary
	!	of accounts.
	!	.B 1
	!	<command> ^*IF\* <expr_>
	!	.b
	!	Conditionally execute the command if the expression
	!	is TRUE (nonzero).
	!	.B 1
	!	<command> ^*UNLESS\* <expr_>
	!	.b
	!	Conditionally execute the command if the expression
	!	is FALSE (zero).
	!	.b
	!	.p
	!	Arguments to the above commands can make use of the
	!	following functions. For functions returning information
	!	about a period, the <expr_> tells how many periods to
	!	look back, thus 0 is the current period, and 1 is for
	!	the previous period. For the previous year, use the
	!	function ^*FPFY\*, the period before that is ^*(FPFY + 1)\*.
	!	.list "-"
	!	.list element
	!	*1,*2,*3
	!	.b
	!	A single digit specifies a string field from CHART.
	!	(Only valid in the FORMAT command)
	!	.footnote
	!	There may be old command files using the following numbers
	!	(parenthesis required):
	!	(-2) = BDY(0), (-1) = BDP(0), (0) = ADY(0), (1) = ADY(1), ...,
	!	(13) = ADY(13).
	!	.efn
	!	.list "*"
	!	.le
	!	1#-#account number.
	!	.le
	!	2#-#account description.
	!	.le
	!	3#-#account type.
	!	.end list
	!	.list element
	!	*Tn
	!	.b
	!	Entire array of totals (created by SUM INTO,
	!	PRINT AND SUM INTO, etc). *n must be between zero (0)
	!	and twenty (20).
	!	.list element
	!	*Tn*(<expr_>*)
	!	.b
	!	One element of a total (created by SUM INTO,
	!	PRINT AND SUM INTO, etc). *n must be between zero (0)
	!	and twenty (20). ^*<expr_>\* must be between zero (0)
	!	and fifteen (15).
	!	.list element
	!	^*BDP(\*<expr_>*)
	!	.b
	!	Budget _$ period amount. <expr_> specifies which period
	!	to use.
	!	.list element
	!	^*BDY(\*<expr_>*)
	!	.b
	!	Budget _$ year amount. <expr_> specifies which period
	!	to use.
	!	.list element
	!	^*BDEOY(0)\*
	!	.b
	!	Budget Dollars for End of Year.
	!	.list element
	!	^*ADP(\*<expr_>*)
	!	.b
	!	Actual _$ period amount. <expr_> specifies which period
	!	to use.
	!	.list element
	!	^*ADY(\*<expr_>*)
	!	.b
	!	Actual _$ year amount. <expr_> specifies which period
	!	to use.
	!	.list element
	!	^*ADEOY(\*0*)
	!	.b
	!	Actual _$ end of last year.
	!	.list element
	!	^*BUP(\*<expr_>*)
	!	.b
	!	Budget unit period amount. <expr_> specifies which period
	!	to use.
	!	.list element
	!	^*BUY(\*<expr_>*)
	!	.b
	!	Budget unit year amount. <expr_> specifies which period
	!	to use.
	!	.list element
	!	^*AUP(\*<expr_>*)
	!	.b
	!	Actual unit period amount. <expr_> specifies which period
	!	to use.
	!	.list element
	!	^*AUY(\*<expr_>*)
	!	.b
	!	Actual unit year amount. <expr_> specifies which period
	!	to use.
	!	.list element
	!	^*AUEOY(\*0*)
	!	.b
	!	Actual unit end of last year.
	!	.list element
	!	^*BHP(\*<expr_>*)
	!	.b
	!	Budget hour period amount. <expr_> specifies which period
	!	to use.
	!	.list element
	!	^*BHY(\*<expr_>*)
	!	.b
	!	Budget hour year amount. <expr_> specifies which period
	!	to use.
	!	.list element
	!	^*AHP(\*<expr_>*)
	!	.b
	!	Actual hour period amount. <expr_> specifies which period
	!	to use.
	!	.list element
	!	^*AHY(\*<expr_>*)
	!	.b
	!	Actual hour year amount. <expr_> specifies which period
	!	to use.
	!	.list element
	!	^*AHEOY(\*0*)
	!	.b
	!	Actual hour end of last year.
	!	.list element
	!	^*FPFY\*
	!	.b
	!	Number of periods/year (as defined in the control file).
	!	.list element
	!	*X
	!	.b
	!	A variable argument to be specified in the PRINT command
	!	(only valid in the FORMAT command).
	!	.end list
	!	.b
	!	.p
	!	The following operations may be used on the above variables
	!	and functions to produce the desired results.
	!	.list "*"
	!	.le
	!	<expr_> *+ <expr_>
	!	.b
	!	Add two items together.
	!	.le
	!	<expr_> *- <expr_>
	!	.b
	!	Subtract two items.
	!	.le
	!	<expr_> *_* <expr_>
	!	.b
	!	Multiply two items together.
	!	.le
	!	<expr_> *_/ <expr_>
	!	.b
	!	Divide two items.
	!	.le
	!	*- <expr_>
	!	.b
	!	Negative value (unary minus).
	!	.le
	!	*(<expr_>*)
	!	.B
	!	Parentheses grouping. Used to force the order of
	!	evaluation. ie. (A+B)_*(C+D) will do the additions
	!	before the multiplications, but A+B_*C+D will do
	!	the multiply before the additions.
	!	.le
	!	<expr_> ^*AND\* <expr_>
	!	.b
	!	Logical AND operation (mostly used in IF statements).
	!	Returns TRUE (nonzero) if both items are TRUE (nonzero),
	!	otherwise returns FALSE (zero).
	!	.le
	!	<expr_> ^*OR\* <expr_>
	!	.b
	!	Logical OR operation (mostly used in IF statements).
	!	Returns TRUE (nonzero) if either item is TRUE (nonzero),
	!	otherwise returns FALSE (zero).
	!	.le
	!	^*NOT\* <expr_>
	!	.b
	!	Logical NOT operation (mostly used in IF statements).
	!	Returns TRUE (nonzero) if the item is FALSE (zero),
	!	otherwise returns FALSE (zero).
	!	.le
	!	<expr_> ^*<_>\* <expr_>
	!	.b
	!	Comparison operation (mostly used in IF statements).
	!	Returns TRUE (nonzero) if the two items are not the same,
	!	otherwise returns FALSE (zero).
	!	.le
	!	<expr_> ^*_>=\* <expr_>
	!	.b
	!	Comparison operation (mostly used in IF statements).
	!	Returns TRUE (nonzero) if the first item is larger or
	!	equal to the second item, otherwise returns FALSE (zero).
	!	.le
	!	<expr_> ^*_<=\* <expr_>
	!	.b
	!	Comparison operation (mostly used in IF statements).
	!	Returns TRUE (nonzero) if the first item is smaller or
	!	equal to the second item, otherwise returns FALSE (zero).
	!	.le
	!	<expr_> ^*=\* <expr_>
	!	.b
	!	Comparison operation (mostly used in IF statements).
	!	Returns TRUE (nonzero) if the two items are the same,
	!	otherwise returns FALSE (zero).
	!	.le
	!	<expr_> ^*_>\* <expr_>
	!	.b
	!	Comparison operation (mostly used in IF statements).
	!	Returns TRUE (nonzero) if the first item is larger
	!	than the second item, otherwise returns FALSE (zero).
	!	.le
	!	<expr_> ^*_<\* <expr_>
	!	.b
	!	Comparison operation (mostly used in IF statements).
	!	Returns TRUE (nonzero) if the first item is smaller
	!	than the second item, otherwise returns FALSE (zero).
	!	.end list
	!
	! Index:
	!	.x Financial Statement
	!	.x Financial Statement>Command File
	!	.x Financial Statement>Format
	!	.x Genfin>Financial Statement
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_SPEC_FINCMD/LINE
	!	$ LINK/EXECUTABLE=GL_EXE: GL_SPEC_FINCMD, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_SPEC_FINCMD.OBJ;*
	!
	! Author:
	!
	!	11/11/86 - Robert Peterson
	!
	! Modification history:
	!
	!	05/13/88 - Lance Williams
	!		Modified to allow R/O open of file if R/W fails.
	!
	!	03/29/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/18/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! External functions
	!
	EXTERNAL STRING  FUNCTION TK_MAIN_TEXTFILEWINDOW

	!
	! Initailize set and device files
	!
	CALL READ_INITIALIZE

60	!
	! Other assignments
	!
	CALL READ_DEVICE("GL_FINCMD", GL_FINCMD.DEV$, STAT%)
	FINCMD_NAME$ = GL_FINCMD.DEV$ + "*.FS"
	FINCMD_PREFIX$ = GL_FINCMD.DEV$
	FINCMD_SUFFIX$ = ".FS"

	!***************************************************
	! Maintain System file menu
	!***************************************************
	ZX$ = TK_MAIN_TEXTFILEWINDOW("4;10", "12;30", FINCMD_NAME$, &
		FINCMD_PREFIX$, FINCMD_SUFFIX$, &
		"FINANCIAL COMMAND FILE", 16%)

 ExitProgram:
	!*******************************************************************
	! Exit program
	!*******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE

32767	END
