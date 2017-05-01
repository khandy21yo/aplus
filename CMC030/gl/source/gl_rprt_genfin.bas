1	%TITLE "General Ledger Financial Report Writer"
	%SBTTL "GL_RPRT_GENFIN"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1987, 1988 BY
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
	! ID:FINSTA
	!
	! Abstract:HELP
	!	.B
	!	.LM +5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_RPRT_GENFIN/LINE
	!	$ LINK/EXECUTABLE=GL_EXE: GL_RPRT_GENFIN, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_RPRT_GENFIN.OBJ;*
	!
	! Author:
	!
	!	12/15/86 - Kevin Handy
	!
	! Modification history:
	!
	!	10/10/88 - Kevin Handy
	!		Modified to handle <TITLEA> to <TITLED>.
	!
	!	10/11/88 - Kevin Handy
	!		Added <PERIOD> variable.
	!
	!	12/09/88 - Kevin Handy
	!		Added year to <PERIOD> variable.  Added
	!		<FROMDATE> and <TODATE>.
	!
	!	12/22/88 - Kevin Handy
	!		Changed <FROMDATE> and <TODATE> to give the
	!		date in PRNT_FANCYDATE format instead of the
	!		PRNT_DATE format.
	!
	!	06/20/89 - Kevin Handy
	!		Added variable for the number of periods
	!		being reported <NPER>.
	!
	!	06/23/89 - Kevin Handy
	!		Fixed bug where an item that starts with a star
	!		or a question mark can get lost in the search
	!		for the first alowwable record in chart.
	!		(Using a find with a blank key doesn't work).
	!
	!	08/29/89 - Kevin Handy
	!		Added debug option to see if I could find out
	!		where a command file was hanging up.
	!
	!	09/18/89 - Kevin Handy
	!		Added <NPERSTR> variable, to give text version
	!		of <NPER>.
	!
	!	09/18/89 - Kevin Handy
	!		Modified <PERIOD> to have actual year (not financial
	!		year), and to show the last day of that period.
	!
	!	10/25/89 - Kevin Handy
	!		Modifications to clarify what is happening in the
	!		PRINT routines, and possibly speed this sucker
	!		up a little.  Split handling of '*' types and
	!		regular types.
	!
	!	10/25/89 - Kevin Handy
	!		Fixed minor bugs that may (at some point) have
	!		caused odd problems.
	!
	!	04/13/90 - Kevin Handy
	!		Added LAYOUT statement to allow exporting to
	!		spreadsheets.
	!
	!	04/23/90 - Kevin Handy
	!		Increased size of PARM_VALUE$ because it was
	!		causing a very hard to find error when the input
	!		exceeded 128.
	!
	!	05/10/90 - Kevin Handy
	!		Modified to use label PrintData instead of line
	!		number 2400, to increase readability of the program.
	!
	!	01/10/91 - Craig Tanner
	!		Where FILENAME$ = "GL_BUD_YYYY", changed to =
	!		"GL_BUD_" + GL_BUDGET_YEAR$
	!
	!	03/18/91 - Kevin Handy
	!		Added debug code to show which groups of accounts
	!		were not used printing specified financial statement.
	!
	!	03/19/91 - Kevin Handy
	!		Modified debug lines to remove formfeed after every
	!		debug line printed.
	!
	!	05/17/91 - Frank F. Starman
	!		Increase dimension for FINSTA_CMD$ from 500 to 1000.
	!
	!	05/17/91 - Kevin Handy
	!		Better error trapping for 1042 (debugging).
	!
	!	06/04/91 - Kevin Handy
	!		Minor improvements in parser for a tiny bit more
	!		speed.  Removed one level of call.  Changed
	!		float-to-integer compare to float-to-float copmpare.
	!
	!	08/14/91 - Kevin Handy
	!		Added BDEOY function.
	!
	!	08/14/91 - Frank F. Starman
	!		Temporary disable BDEOY because creates subscript
	!		out of range error.
	!
	!	08/15/91 - Kevin Handy
	!		Fixed BDEOY code.
	!
	!	08/16/91 - Kevin Handy
	!		Reversed order of NPER and NPERSTR constants
	!		so the replacestring function wouldn't get
	!		too confused.
	!
	!	09/16/91 - Kevin Handy
	!		Modified to ignore blank lines on input of
	!		command file.
	!
	!	03/19/93 - Kevin Handy
	!		Very minor speedup by handling several cases of
	!		"xxx = yyy \ xxx = zzz if ppp" as "if ppp then
	!		xxx=zzz else xxx = yyy endif", thus losing an
	!		average of 1/2 assignments per use.
	!
	!	03/23/93 - Kevin Handy
	!		Minor speed improvements.  May not be noticable,
	!		but may help.
	!
	!	03/24/93 - Kevin Handy
	!		More speedup stuff.  Changed mass if if-then
	!		statements in the parser to two select statements.
	!		One for 6 char, and one for 4 char.  Assuming
	!		select will be faster than several mid's.
	!
	!	03/24/93 - Kevin Handy
	!		Lost unnecessary test for whitespace in PARSE_NUMBER.
	!
	!	03/24/93 - Kevin Handy
	!		I know I'm going to regret it, but I added "Syntax
	!		Error" messages when it can't figure out things.
	!
	!	03/25/93 - Kevin Handy
	!		Added more tests for UTL_REPORTX::STAT to try to
	!		lose lockup when exit key is typed.
	!
	!	03/26/93 - Kevin Handy
	!		Additional minor speedups.  Modified to not create
	!		copy of string when there is not a ";" to strip off.
	!		modified "FORMAT" to use Z3$ directly instead of
	!		copying Z3$ into Z$ then using it.
	!
	!	04/01/93 - Kevin Handy
	!		Added "SET ZERO" and "SET NOZERO" commands to
	!		allow statement to force "PRINT ZERO (Y/N)"
	!		over report settings.
	!
	!	04/01/93 - Kevin Handy
	!		Also added "SET DEBUG" and "SET NODEBUG" commands,
	!		but operates globally (Shouldn't leave these in
	!		after done debugging).
	!
	!	05/12/93 - Kevin Handy
	!		Added handling of comment lines (lines starting
	!		with exclamation mark character), so they don't
	!		produce an error message.
	!
	!	06/14/93 - Kevin Handy
	!		Added a REGARDLESS to GL_CHART.
	!
	!	07/28/93 - Kevin Handy
	!		Modified to call OUTP_LINE with a 0 in paging
	!		flag instead of a 1 when printing out syntax
	!		error messages.
	!
	!	04/11/94 - Kevin Handy
	!		Removed superfluous IF statements inside of
	!		the same IF statement.
	!
	!	08/16/94 - Kevin Handy
	!		Ignore labels (must start with a '$') so we don't
	!		get a syntax error for it.
	!
	!	08/16/94 - Kevin Handy
	!		Added command "GOTO", which was documented but didn't
	!		really exist before.
	!
	!	08/24/94 - Kevin Handy
	!		Moved position of "debug(R)" so that it will not skip
	!		over it occasionaly, and it will display the line
	!		AFTER substitutions have been made (not before)
	!
	!	08/24/94 - Kevin Handy
	!		Made the page test routine into a seperate subroutine
	!		instead of al of the repeated code throughout the
	!		program.
	!
	!	08/24/94 - Kevin Handy
	!		Took over the variable PAGE_BREAK_TEST% from a strange
	!		test while in display, and created a commend "TESTPAGE"
	!		to check for room on the page for the next line.
	!
	!	03/10/95 - Kevin Handy
	!		Fixed budget problem using BDY() function, it was
	!		confused about R,E codes.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	11/20/95 - Kevin Handy
	!		Lose /NOWARN on compile statement.
	!
	!	12/15/95 - Kevin Handy
	!		Reformat source code closer to 80 columns.
	!		Change RIGHT(NUM1$()) to FORMAT$().
	!
	!	01/09/96 - Kevin Handy
	!		Modified error message on financial command (1050)
	!		to display name of file, instead of "GL_FINSTA_WRK".
	!
	!	03/28/97 - Kevin Handy
	!		Change FNF$ to FNFORMAT$.
	!
	!	08/04/97 - Kevin Handy
	!		Changes to handle more than 9 format statements.
	!		Change variable 'F0$()' to 'FMT$()' and 'F%' to 'FMT%'.
	!		Increase dimension of FMT$() from 9 to 19.
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	09/10/97 - Kevin Handy
	!		Lose some commented out code
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/08/98 - Kevin Handy
	!		Use 'report name' for the company name instead
	!		of the 'menu name'
	!
	!	04/08/99 - Kevin Handy
	!		Use BASIC$STARLET for LIB$ and STR$ routines
	!
	!	04/18/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!		Change ErrorHelp to HelpError
	!
	!	07/18/2000 - Kevin Handy
	!		Fix filename for error messages on line 310
	!		Add UTL_WORK.DEV$ to work file name to match
	!		how GL_RPRT_FINSTA creates it.
	!
	!	09/15/2000 - Kevin Handy
	!		Use LIB$DELETE_FILE instead of KILL
	!
	!	10/30/2000 - Kevin Handy
	!		Use A"x"B
	!
	!	02/08/2002 - Kevin Handy
	!		Be able to see '*'s after the first 10 characters
	!		of a balance code.  Also chop match string to 10
	!		more reliably.
	!
	!	06/19/2002 - Kevin Handy
	!		Much reformatting (for 80 columns), and changing of
	!		'.' to '_' in variable names, and added some more
	!		internal documentation.
	!
	!	06/24/2002 - Kevin Handy
	!		Added code to handle Quarters. ADQ() and <PERIOD>
	!
	!	06/24/2003 - Kevin Handy
	!		More fixes to ADQ() code.
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include statements
	!
	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "STR$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	!
	! Map statements
	!
	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	MAP (GL_PERIOD)		GL_PERIOD_CDD	GL_PERIOD

	%INCLUDE "SOURCE:[GL.OPEN]GL_BUD_YYYY.HB"
	MAP (GL_BUD_YYYY)	GL_BUD_YYYY_CDD	GL_BUD_YYYY

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD	GL_CHART

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_PROFILE.HB"
	MAP (UTL_PROFILE) UTL_PROFILE_CDD UTL_PROFILE

	!
	! This one is not external, but must be defined thus so that
	! basic can handle it.
	!
	EXTERNAL STRING	FUNCTION REPLACE_STRING
	EXTERNAL REAL	FUNCTION PARSE_EXPR
	EXTERNAL STRING FUNCTION STRING_PARSE

	!
	! Common area to pass variables to PARSE function
	!
	COMMON (PARSECOM) ACTDOL(20%), ACTUNIT(20%), ACTHOUR(20%), &
		BUDDOL(13%), BUDUNIT(13%), BUDHOUR(13%), &
		A(20%, 15%), &
		PARM_VALUE$ = 250%, PARM_POINTER%, &
		ACCTYPE$ = 1%, &
		TNLOOP%, &
		BDEOY(2%)

	!
	! Dimension statements
	!
	DIM T$(3%), A1(15%), T0$(15%), FMT$(20%), I9$(10%), L9$(4%, 4%)

	DIM FINSTA_CMD$(1000%)		! Holds work file array

	DIM PROGWIDE_VAR$(20%), PROGWIDE_VALUE$(20%)
	DIM LOCAL_VAR$(30%), LOCAL_VALUE$(30%)

	%PAGE

	ON ERROR GOTO 19000

	!
	! Allocate channels
	!
	CALL ASSG_CHANNEL(CMD.CH%, STAT%)
	CALL ASSG_CHANNEL(FINSTA.WRK%, STAT%)

	!
	! Get the name of the temp file by pulling out the
	! name of the job that started this process.
	!
	CALL READ_DEVICE("UTL_WORK", UTL_WORK.DEV$, STAT%)
	SYS_STATUS% = LIB$GET_SYMBOL("CMC$REPORT" BY DESC, &
		FINSTA_WORK_FILE$ BY DESC,,)

	IF (SYS_STATUS% AND 1%) = 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Unable to find GENFIN WORK file name!", 0%)
		GOTO ExitProgram
	END IF

	UND1% = INSTR(1%, FINSTA_WORK_FILE$, "_")

	FINSTA_WORK_FILE$ = UTL_WORK.DEV$ + "TEMP_" + &
		SEG$(FINSTA_WORK_FILE$, 5%, UND1% - 1%) + ".TMP"

 Init:	!
	! Initialize for report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	PAGE_BREAK_TEST% = 0%

	!++
	! Abstract:FLD01
	!
	! Datatype:TEXT
	! Size:1
	! Valid Input: Y,N
	!--
	ZERO$ = TRM$(UTL_REPORTX::OPTDEF(0%))

	!++
	! Abstract:FLD10
	!	^*(10) Debug\*
	!	.b
	!	.lm +5
	!	The ^*Debug\* option is used to help find problems while
	!	developing new command files. It outputs an extremely
	!	large amount of information that may be useful in solving
	!	problems.
	!	.b
	!	Under normal use, this field should be set to "*N" to
	!	disable the debugging.
	!	.lm -5
	!
	! Index:
	!
	!--
	DEBUG% = (LEFT(UTL_REPORTX::OPTDEF(9%), 1%) = "Y")

	CALL READ_DEVICE("GL_FINCMD", GL_FINCMD.DEV$, STAT%)

300	!
	! General Ledger Period File
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.OPN"
		GET #GL_PERIOD.CH%, RECORD 1%, REGARDLESS
		CLOSE #GL_PERIOD.CH%
	USE
		FILENAME$ = "GL_PERIOD"
		CONTINUE HelpError
	END WHEN

310	!
	! Read in financial statement work file
	!
	WHEN ERROR IN
		OPEN FINSTA_WORK_FILE$ FOR INPUT AS FILE FINSTA.WRK%
	USE
		FILENAME$ = FINSTA_WORK_FILE$
		CONTINUE HelpError
	END WHEN

	WHEN ERROR IN
		INPUT LINE #FINSTA.WRK%, CMD$
	USE
		FILENAME$ = FINSTA_WORK_FILE$
		CONTINUE HelpError
	END WHEN
	CMD$ = EDIT$(CMD$, 128% + 8% + 4%)

	WORK% = -1% IF LEFT(CMD$, 1%) = "Y"

	IF WORK%
	THEN
		GL_PERIOD::LASTPERCLO = GL_PERIOD::LASTPERCLO + 1%
		IF GL_PERIOD::LASTPERCLO > GL_PERIOD::FPFY
		THEN
			GL_PERIOD::LASTPERCLO = 1%
			GL_PERIOD::YEAR = FORMAT$(VAL%( &
				GL_PERIOD::YEAR) + 1%, "<0>###")
		END IF
	END IF

320	!
	! Open up current budget file
	!
	GL_BUDGET_YEAR$ = GL_PERIOD::YEAR

	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_BUD_YYYY.OPN"
	USE
		CONTINUE 330 IF ERR = 5%
		FILENAME$ = "GL_BUD_" + GL_BUDGET_YEAR$
		CONTINUE HelpError
	END WHEN

330	!
	! Open chart of accounts
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.OPN"
	USE
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

	!
	! Get period dates
	!
	CALL DATE_GLDATE(GL_PERIOD, &
		GL_PERIOD::YEAR + FORMAT$(GL_PERIOD::LASTPERCLO, "<0>#"), &
		PERIOD_FROM$, &
		PERIOD_TO$)

	!
	! Initialize Initialization arrays
	!
	PROGWIDE_VAR% = 15%
	PROGWIDE_VAR$(1%) = "<DATE>"
	PROGWIDE_VAR$(2%) = "<COMPANY>"
	PROGWIDE_VAR$(3%) = "<REPORT>"
	PROGWIDE_VAR$(4%) = "<TIME>"
	PROGWIDE_VAR$(5%) = "<ADATE>"
	PROGWIDE_VAR$(6%) = "<TITLEA>"
	PROGWIDE_VAR$(7%) = "<TITLEB>"
	PROGWIDE_VAR$(8%) = "<TITLEC>"
	PROGWIDE_VAR$(9%) = "<TITLED>"
	PROGWIDE_VAR$(10%) = "<PERIOD>"
	PROGWIDE_VAR$(11%) = "<FROMDATE>"
	PROGWIDE_VAR$(12%) = "<TODATE>"
	PROGWIDE_VAR$(13%) = "<NPERSTR>"
	PROGWIDE_VAR$(14%) = "<NPER>"
	PROGWIDE_VAR$(15%) = "<QUARTER>"

	PROGWIDE_VALUE$(1%) = TRM$(UTL_REPORTX::REPDATE)
	PROGWIDE_VALUE$(2%) = TRM$(UTL_PROFILE::REP_NAME)
	PROGWIDE_VALUE$(3%) = ""
	PROGWIDE_VALUE$(4%) = TIME$(0%)
	PROGWIDE_VALUE$(5%) = DATE$(0%)
	PROGWIDE_VALUE$(6%) = ""
	PROGWIDE_VALUE$(7%) = ""
	PROGWIDE_VALUE$(8%) = ""
	PROGWIDE_VALUE$(9%) = ""

	IF GL_PERIOD::LASTPERCLO < GL_PERIOD::NEWYEAR
	THEN
		THIS_YEAR$ = FORMAT$(VAL%(GL_PERIOD::YEAR) - 1%, "<0>###")
	ELSE
		THIS_YEAR$ = GL_PERIOD::YEAR
	END IF

	PROGWIDE_VALUE$(10%) = &
		TRM$(GL_PERIOD::PERIOD(GL_PERIOD::LASTPERCLO)) + &
		" " + RIGHT(GL_PERIOD::ENDDATE(GL_PERIOD::LASTPERCLO), 3%) + &
		", " + THIS_YEAR$
	PROGWIDE_VALUE$(11%) = PRNT_FANCYDATE(PERIOD_FROM$)
	PROGWIDE_VALUE$(12%) = PRNT_FANCYDATE(PERIOD_TO$)
	PROGWIDE_VALUE$(14%) = NUM1$(GL_PERIOD::LASTPERCLO)

	IF (GL_PERIOD::LASTPERCLO = 1%)
	THEN
		PROGWIDE_VALUE$(13%) = "One Period"
	ELSE
		PROGWIDE_VALUE$(13%) = &
			PRNT_NUMBERTEXT(GL_PERIOD::LASTPERCLO * 1%) + " Periods"
	END IF

	PROGWIDE_VALUE$(15%) = NUM1$(MOD(GL_PERIOD::LASTPERCLO - 1%, 4%) + 1%) + &
		" Quarter of " + THIS_YEAR$

	%PAGE

1000	!******************************************************************
	! Major loop for all financial statements to be printed
	!******************************************************************
	LOOP% = 1%
	I9% = 0%
	PROGWIDE_VALUE$(3%) = ""
	DEBUG_GROUP$ = ""

1020	WHEN ERROR IN
		INPUT LINE #FINSTA.WRK%, CMD$
	USE
		CONTINUE 10000 IF ERR = 11%
		FILENAME$ = "FINSTA_WORK_FILE"
		CONTINUE HelpError
	END WHEN

	CMD$ = EDIT$(CMD$, 128% + 8% + 4%)

	GOTO 1050 IF CMD$ = "<>" AND FIRST_TEST%
	FIRST_TEST% = -1% IF CMD$ = "<>"
	TEMP% = INSTR(1%, CMD$, ">") + 1%
	TEST$ = MID(CMD$, 2%, TEMP% - 3%)

	SELECT TEST$
	CASE "CMD"
		FINSTA_CMD$ = RIGHT(CMD$, TEMP%)

	CASE "TITLE"
		PROGWIDE_VALUE$(3%) = RIGHT(CMD$, TEMP%)

	CASE "TITLEA"
		PROGWIDE_VALUE$(6%) = RIGHT(CMD$, TEMP%)

	CASE "TITLEB"
		PROGWIDE_VALUE$(7%) = RIGHT(CMD$, TEMP%)

	CASE "TITLEC"
		PROGWIDE_VALUE$(8%) = RIGHT(CMD$, TEMP%)

	CASE "TITLED"
		PROGWIDE_VALUE$(9%) = RIGHT(CMD$, TEMP%)

	CASE "TYPE"
		IF TYP$ <> "C" AND TYP$ <> "W"
		THEN
			TYP$ = "O"
		ELSE
			TYP$ = RIGHT(CMD$, TEMP%)
		END IF

	CASE "INP"
		I9% = I9% + 1%
		I9$(I9%) = RIGHT(CMD$, TEMP%)
	END SELECT

	GOTO 1020

	%PAGE

1040	!
	! Return here to finish up current statement
	!
	CALL OUTP_LINENOTITLE(LYT_LINE$, UTL_REPORTX, STR_BUILD$, 1%)
	GOTO ExitProgram IF UTL_REPORTX::STAT
	UTL_REPORTX::LINENO = 0%
	STR_BUILD$ = ""

	FOR I% = 0% TO 20%
		A(I%, J%) = 0.0 FOR J% = 1% TO 15%
		ACTDOL(I%) = 0.0
		ACTUNIT(I%) = 0.0
		ACTHOUR(I%) = 0.0
	NEXT I%
	FOR I% = 0% TO 13%
		BUDDOL(I%) = 0.0
		BUDUNIT(I%) = 0.0
		BUDHOUR(I%) = 0.0
	NEXT I%
	BDEOY(I%) = 0.0 FOR I% = 0% TO 2%
	A1(I%) = 0.0 FOR I% = 1% TO 15%
	T0$(I%) = "" FOR I% = 1% TO 15%
	FMT$(I%) = "" FOR I% = 0% TO 20%
	L9$(I%, J%) = "" FOR J% = 1% TO 4% FOR I% = 1% TO 4%

1041	GOTO 1045 UNLESS DEBUG%
	!
	! Loop through chart and show what accounts were not used
	!
	CALL OUTP_LINENOTITLE("$DEBUG:132",UTL_REPORTX, &
		"Debug(X): Looking for unused accounts", 0%)

	SELECT TYP$

	CASE "C"
		RESET #GL_CHART.CH%, KEY #1%

	CASE "W"
		RESET #GL_CHART.CH%, KEY #2%

	CASE ELSE
		RESET #GL_CHART.CH%, KEY #3%

	END SELECT

1042	WHEN ERROR IN
		GET #GL_CHART.CH%, REGARDLESS
	USE
		CONTINUE 1045
	END WHEN

	SELECT TYP$
	CASE "C"
		TEST$ = GL_CHART::FLOW
	CASE "W"
		TEST$ = GL_CHART::WORK
	CASE ELSE
		TEST$ = GL_CHART::FINTYPE
	END SELECT

	IF INSTR(1%, DEBUG_GROUP$, TEST$) = 0%
	THEN
		TEXT$ = "Debug(X): " + GL_CHART::ACCT + " " + &
			GL_CHART::DESCR + " " + TEST$
		CALL OUTP_LINENOTITLE("$DEBUG:132", UTL_REPORTX, TEXT$, 0%)
	END IF

	GOTO 1042

1045	LOOP% = 1%
	FIRST_TEST% = 0%
	I9% = 0%
	DEBUG_GROUP$ = ""

	GOTO 1020

	%PAGE

1050	!******************************************************************
	! Loop for a single financial statement
	!******************************************************************

	CALL ENTR_3MESSAGE(SCOPE, "Processing " + PROGWIDE_VALUE$(3%), 1%)

	!
	! Open up control file, and read in the whole thing
	!
	WHEN ERROR IN
		OPEN GL_FINCMD.DEV$ + FINSTA_CMD$ FOR INPUT AS FILE CMD.CH%, &
			RECORDSIZE 132%, ACCESS READ
	USE
		FILENAME$ = FINSTA_CMD$
		CONTINUE HelpError
	END WHEN

	LOCAL_VAR% = 0%

	I9% = 0%

	CHECK_FOR_INPUTS% = 0%

	ZERO1$ = ""

1052	!*******************************************************************
	! (LOOP) Read in the text file
	!*******************************************************************

	WHEN ERROR IN
		LINPUT #CMD.CH%, Z$
	USE
		CONTINUE 1060 IF ERR = 11%
		FILENAME$ = "GL_FINCMD"
		CONTINUE HelpError
	END WHEN

	GOTO 1052 IF Z$ = ""
	GOTO 1052 IF LEFT(Z$, 1%) = "!"

	CALL OUTP_LINENOTITLE("$DEBUG:132", UTL_REPORTX, "Debug(L):" + Z$, 0%) &
		IF DEBUG%

	!
	! Check for input items
	!
	IF CHECK_FOR_INPUTS% = 0%
	THEN
		Z$ = EDIT$(Z$, 4%)
		IF LEFT(Z$, 4%) = "FORM" OR LEFT(Z$, 5%) = "TITLE"
		THEN
			CHECK_FOR_INPUTS% = -1%
			REPORT_PAGE% = 1%

			FINSTA_CMD% = 0%
			FINSTA_POINTER% = 0%

			GOTO 1054
		END IF

		!
		! Should require it to start "INPUT", but it doesn't
		!
		I% = INSTR(1%, Z$, '"')
		I1% = INSTR(I% + 1%, Z$, '"')
		V$ = EDIT$(RIGHT(Z$, I1% + 1%), -1%)
		V$ = RIGHT(V$, 2%) &
			IF LEFT(V$, 1%) = ";" OR LEFT(V$, 1%) = ","

		I9% = I9% + 1%
		LOCAL_VAR% = LOCAL_VAR% + 1%
		LOCAL_VAR$(LOCAL_VAR%) = V$
		LOCAL_VALUE$(LOCAL_VAR%) = I9$(I9%)

		GOTO 1052
	END IF

1054	!
	! Append next line to this one if continuation flag exists
	!
	IF RIGHT(Z$, LEN(Z$)) = "/"
	THEN
		LINPUT #CMD.CH%, Z1$
		CALL OUTP_LINENOTITLE("$DEBUG:132", &
			UTL_REPORTX, "Debug(L): " + Z1$, 0%) &
			IF DEBUG%
		Z$ = LEFT(Z$, LEN(Z$) - 1%) + EDIT$(Z1$, 8% + 128% + 256%)
		GOTO 1054
	END IF

	Z$ = EDIT$(Z$, 4% + 8% + 128% + 256%)

	!
	! Replace the <xxxx> vars defined in the program (hard coded)
	!
	Z$ = REPLACE_STRING(Z$, PROGWIDE_VAR%, &
		PROGWIDE_VAR$(), PROGWIDE_VALUE$())

	!
	! Replace the (INPUT "message"; VAR) DEFINITIONS
	!
	Z$ = REPLACE_STRING(Z$, LOCAL_VAR%, &
		LOCAL_VAR$(), LOCAL_VALUE$())

	FINSTA_CMD% = FINSTA_CMD% + 1%
	FINSTA_CMD$(FINSTA_CMD%)  = EDIT$(Z$, 8% + 16% + 128% + 256%)

	!
	! Loop back to get next line
	!
	GOTO 1052

	! (LOOP) ends

	%PAGE

1060	!*********************************************************************
	! (LOOP) Process the source file
	!*********************************************************************

	GOSUB TestPageBreak

	!
	! Pull up one line from the command file
	!
	FINSTA_POINTER% = FINSTA_POINTER% + 1%
	GOTO 1040 IF FINSTA_POINTER% > FINSTA_CMD%
	Z$ = FINSTA_CMD$(FINSTA_POINTER%)
	GOTO 1060 IF Z$ = ""

	!
	! Skip statement if it is (probibly) a label
	!
	GOTO 1060 IF LEFT(Z$, 1%) = "$"

1070	!
	! Handle as-of-yet unhandled replacements
	!
	GOSUB 18100

	CALL OUTP_LINENOTITLE("$DEBUG:132", UTL_REPORTX, "Debug(R) " + Z$, 0%) &
		IF DEBUG%

	!
	! Check for conditionals
	!
	I% = INSTR(1%, Z$, "UNLESS")
	IF I%
	THEN
		Z4$ = RIGHT(Z$, I% + 6%)
		Z$ = TRM$(LEFT(Z$, I% - 1%))
		TEMP% = 1%
		TEMP = PARSE_EXPR(Z4$, TEMP%)
		GOTO 1060 UNLESS TEMP <> 0.0
	END IF

	I% = INSTR(1%, Z$, " IF ") IF I% = 0%
	IF I% <> 0%
	THEN
		Z4$ = RIGHT(Z$, I% + 4%)
		Z$ = TRM$(LEFT(Z$, I% - 1%))
		TEMP% = 1%
		TEMP = PARSE_EXPR(Z4$, TEMP%)
		GOTO 1060 IF TEMP = 0.0
	END IF

	SEMICOLON% = (RIGHT(Z$, LEN(Z$)) = ";")
	Z$ = LEFT(Z$, LEN(Z$) - 1%) IF SEMICOLON%

	!
	! Check for comma
	!
	Z3$ = Z$
	I% = INSTR(1%, Z$, ",")
	IF I% = 0%
	THEN
		Z1$ = ""
		Z$ = Z$ + " "
	ELSE
		Z1$ = RIGHT(Z$, I% + 1%) + ","
		Z$ = LEFT(Z$, I% - 1%) + " "
	END IF

1100	!
	! Handle main option
	!
	I% = INSTR(1%, Z$, " ")
	C$ = LEFT(Z$, I% - 1%)
	S%, S0%, NOPRINT% = 0%

	SELECT C$

	CASE "TITLE"
		!*****************************************************
		! TITLE
		!*****************************************************
		T0% = 0%
		F7$ = ""

 Title:		!
		! Pull up one line from the command file
		!
		FINSTA_POINTER% = FINSTA_POINTER% + 1%
		GOTO 1040 IF FINSTA_POINTER% > FINSTA_CMD%
		Z$ = FINSTA_CMD$(FINSTA_POINTER%)
		CALL OUTP_LINENOTITLE("$DEBUG:132", UTL_REPORTX, &
			"Debug(T): " + Z$, 0%) &
			IF DEBUG%

		GOSUB 18100
		IF Z$ <> "END"
		THEN
			T0% = T0% + 1%
			T0$(T0%) = FNFORMAT$(Z$)
			GOTO Title
		END IF

	CASE "PRINT", "SUM"
		!*****************************************************
		! PRINT, SUM
		!*****************************************************

		!
		! SUM INTO
		!
		IF LEFT(Z$, 8%) = "SUM INTO"
		THEN
			S% = -1%
			Z$ = RIGHT(Z$, 10%)
			I% = INSTR(1%, Z$, " ")
			GOTO 1060 IF I% = 0%
			S0% = VAL%(MID(Z$, 2%, I% - 2%))
			Z$ = RIGHT(Z$, I% + 1%)
			NOPRINT% = -1%
			GOTO PrintFormat
		END IF

		!*****************************************************
		! PRINT
		!*****************************************************

		Z$ = RIGHT(Z$, I% + 1%)

		!
		! PRINT <cr>
		!
		IF Z$ = ""
		THEN
			IF SEMICOLON% <>-1%
			THEN
				CALL OUTP_LINENOTITLE(LYT_LINE$, UTL_REPORTX, STR_BUILD$, 0%)
				GOTO ExitProgram IF UTL_REPORTX::STAT
				STR_BUILD$ = ""

				GOSUB TestPageBreak
			END IF

			GOTO 1060
		END IF

		!
		! PRINT PAGE <cr>
		!
		IF LEFT(Z$, 4%) = "PAGE"
		THEN
			CALL OUTP_LINENOTITLE(LYT_LINE$, UTL_REPORTX, STR_BUILD$, 1%)
			GOTO ExitProgram IF UTL_REPORTX::STAT
			STR_BUILD$ = ""
			UTL_REPORTX::LINENO = 0%
			REPORT_PAGE% = REPORT_PAGE% + 1%
			GOTO 1060
		END IF

		!
		! PRINT TITLE <cr>
		!
		IF LEFT(Z$, 5%) = "TITLE"
		THEN
			F7$ = Z1$
			GOSUB PrintTitle
			GOTO 1060
		END IF

		!
		! PRINT TAB ...
		!
		IF LEFT(Z$, 1%) = '"' OR LEFT(Z$, 3%) = "TAB"
		THEN
			!
			! Print tab
			!
			Z$ = Z3$
			Z$ = RIGHT(Z$, INSTR(1%, Z$, " ") + 1%)
			STR_BUILD$ = STR_BUILD$ + FNFORMAT$(Z$)
			IF SEMICOLON% <> -1%
			THEN
				CALL OUTP_LINENOTITLE(LYT_LINE$, UTL_REPORTX, STR_BUILD$, 0%)
				GOTO ExitProgram IF UTL_REPORTX::STAT
				STR_BUILD$ = ""

				GOSUB TestPageBreak
			END IF
			GOTO 1060
		END IF

		!
		! PRINT AND SUM INTO
		!
		S% = 0%
		IF LEFT(Z$, 12%) = "AND SUM INTO"
		THEN
			!
			! Print and sum into
			!
			S% = -1%
			Z$ = RIGHT(Z$, 14%)
			I% = INSTR(1%, Z$, " ")
			S0% = VAL%(MID(Z$, 2%, I% - 2%))
			Z$ = RIGHT(Z$, I% + 1%)
		END IF

 PrintFormat:	GOTO 1060 IF LEFT(Z$, 1%) <> "F"
		!
		! Print format (Fxx)
		!
		I1% = 1%
		I1% = I1% + 1% &
			WHILE INSTR(1%, " 0123456789", &
			MID(Z$, I1% + 2%, 1%)) > 1%
		FMT% = VAL%(MID(Z$, 2%, I1%))
		Z$ = RIGHT(Z$, I1% + 3%)
		IF Z$ = ""
		THEN
			GOSUB PrintData
			GOTO 1060
		END IF

		W$ = EDIT$(Z$, 4% + 8% + 128%) + ""
		W$ = W$ + "," + LEFT(Z1$, LEN(Z1$) - 1%) IF Z1$ <> ""

		!
		! Handle list of codes
		!
		WHILE W$ <> ""
			!
			! Strip one code (W1$) from command string (W$)
			!
			I1% = INSTR(1%, W$, ",") - 1%
			I1% = LEN(W$) IF I1% = -1%
			W2$ = LEFT(W$, I1%)
			W$ = RIGHT(W$, I1% + 2%)

			IF TYP$ = "C" OR TYP$ = "W"
			THEN
				IND.FLAG$ = LEFT(W2$, 1%)
				W2$ = RIGHT(W2$, 2%)
			END IF

			!
			! Set a flag if uses a "*"
			!
			IF INSTR(1%, W2$, "*")
			THEN
				STAR_FLAG% = -1%
			ELSE
				STAR_FLAG% = 0%
			END IF

			W2$ = LEFT(W2$, 10%)
			TEMP$ = ""
			TEMP1$ = ""
			CHANGE W2$ TO W1%

			!
			! Change all '*' to '?' in W2$
			!
			W2$ = LEFT(W2$, I% - 1%) + "?" + &
				RIGHT(W2$, I% + 1%) IF MID(W2$, I%, 1%) = "*" &
				FOR I% = 1% TO LEN(W2$)

			!
			! Find length on non-wildcard part of the string
			!
			TEST% = INSTR(1%, W2$, "?") - 1%
			TEST% = LEN(W2$) IF TEST% < 0%
			W1$ = LEFT(W2$, TEST%)

			!
			! Pull in first record for this group
			!
			GOSUB GetChartRecord
			GOTO FinalNext IF CHART_GET_SUCCESS%

			!
			! Pull off the type of this record
			!
			SELECT TYP$
			CASE "C"
				TEST$ = GL_CHART::FLOW
			CASE "W"
				TEST$ = GL_CHART::WORK
			CASE ELSE
				TEST$ = GL_CHART::FINTYPE
			END SELECT

			GOTO NoStarCode IF STAR_FLAG% = 0%

			!****************************************
			! Handle Star'd types
			!****************************************

 StarCode:		!
			! Ignore this record if doesn't belong at all
			!
			IF COMP_STRING(TEST$, W2$) = 0%
			THEN
				GOTO GetNextStar
			END IF

			!
			! Add to debug list if necessary
			!
			IF DEBUG%
			THEN
				DEBUG_GROUP$ = DEBUG_GROUP$ + "," + TEST$
			END IF

			!
			! Look only at parts that are not
			! wildcarded
			!
			TEMP1$ = ""
			TEMP1$ = TEMP1$ + MID(TEST$, I%, 1%) &
				IF W1%(I%) <> 42% &
				FOR I% = 1% TO W1%(0%)

			!
			! If were working on a group already
			!
			IF TEMP$ <> ""
			THEN
				!
				! If it's the same group as before
				!
				IF TEMP$ = TEMP1$
				THEN
					!
					! If on same group, summarize
					! and keep going.
					!
					GOSUB 18250
					GOTO GetNextStar
				END IF

				!
				! Print this group
				!
				Z7$ = Z1$
				GOSUB PrintData
				Z1$ = Z7$
			END IF

			!
			! Must be starting a new group, initialize
			! all necessary parts.
			!

			!
			! Strip anything from first '-' in
			! the chart description
			!
			TEMP% = INSTR(1%, GL_CHART::DESCR, "-")
			GL_CHART::DESCR = LEFT(GL_CHART::DESCR, TEMP% - 1%) &
				IF TEMP% > 2%

			!
			! Initilize this group
			!
			T$(1%) = GL_CHART::ACCT
			T$(2%) = GL_CHART::DESCR
			ACCTYPE$, T$(3%) = GL_CHART::ACCTYPE
			TEMP$ = TEMP1$

			GOSUB 18200

 GetNextStar:		!
			! Grab next record from chart of accounts
			!
			GOSUB GetChartNext

			!
			! Keep going if not at the end
			!
			IF CHART_GET_NEXT% = 0%
			THEN
				!
				! Pull off the type of this record
				!
				SELECT TYP$
				CASE "C"
					TEST$ = GL_CHART::FLOW
				CASE "W"
					TEST$ = GL_CHART::WORK
				CASE ELSE
					TEST$ = GL_CHART::FINTYPE
				END SELECT

				GOTO StarCode IF LEFT(TEST$, TEST%) = LEFT(W2$, TEST%)
			END IF

			!
			! (Possibly) output last accout for '*'
			!
			IF TEMP$ <> ""
			THEN
				Z7$ = Z1$
				GOSUB PrintData
				Z1$ = Z7$
			END IF

			GOTO FinalNext

			!****************************************
			! Handle codes without '*'
			!****************************************

 NoStarCode:		!
			! Set up information for this record
			!
			IF COMP_STRING(TEST$, W2$)
			THEN
				!
				! Add to debug list if necessary
				!
				IF DEBUG%
				THEN
					DEBUG_GROUP$ = DEBUG_GROUP$ + "," + TEST$
				END IF

				!
				! Set the description
				!
				T$(1%) = GL_CHART::ACCT
				T$(2%) = GL_CHART::DESCR
				ACCTYPE$, T$(3%) = GL_CHART::ACCTYPE

				GOSUB 18200

				Z7$ = Z1$
				GOSUB PrintData
				Z1$ = Z7$
			END IF

 GetNextNostar:		!
			! Grab next record from chart of accounts
			!
			GOSUB GetChartNext

			!
			! Keep going if not at the end
			!
			IF CHART_GET_NEXT% = 0%
			THEN
				!
				! Pull off the type of this record
				!
				SELECT TYP$
				CASE "C"
					TEST$ = GL_CHART::FLOW
				CASE "W"
					TEST$ = GL_CHART::WORK
				CASE ELSE
					TEST$ = GL_CHART::FINTYPE
				END SELECT

				GOTO NoStarCode IF LEFT(TEST$, TEST%) = LEFT(W2$, TEST%)
			END IF

			!****************************************
			! End of loop
			!****************************************

 FinalNext:	NEXT


	CASE "FORMAT"
		!*****************************************************
		! FORMAT
		!*****************************************************

		I1% = 1%
		I1% = I1% + 1% &
			WHILE INSTR(1%, " 0123456789", &
			MID(Z3$, I% + I1% + 1%, 1%)) > 1%
		FMT% = VAL%(MID(Z3$, I% + 1%, I1%))
		Z$ = RIGHT(Z3$, I% + I1% + 2%)
		F1% = -1%
		FMT$(FMT%) = FNFORMAT$(Z$)
		F1% = 0%

	CASE "LAYOUT"
		!*****************************************************
		! LAYOUT FOR SPREADSHEETS
		!*****************************************************

		TEMP% = 1%
		LYT_LINE$ = STRING_PARSE(RIGHT(Z3$, 7%), TEMP%)

	CASE "ADD"
		!*****************************************************
		! ADD
		!*****************************************************

		!
		! Split equation into its lvalue and rvalue
		!
		Z$ = RIGHT(Z$, 5%)
		GOTO 1060 IF Z$ = ""
		I% = INSTR(1%, Z$, "=")
		GOTO 1060 IF I% = 0%
		Z2$ = RIGHT(Z$, I% + 1%)
		Z$ = LEFT(Z$, I% - 1%)
		I% = INSTR(1%, Z$, "(")

		!
		! If there is no parentheses in the lvalue, then
		! we are working with an entire array of totals,
		! ELSE we are only working with an individual
		! total.  In either case, a "T" is assumed.
		!
		IF I% = 0%
		THEN
			A1(L%) = 0. FOR L% = 1% TO 15%
			!
			! Get accumulator number to store totals in
			!
			I2% = VAL%(RIGHT(Z$, 2%))

			!
			! Loop through all possible elements in the array
			!
			FOR TNLOOP% = 0% TO 15%
				TEMP% = 1%
				A(I2%, TNLOOP%) = PARSE_EXPR(Z2$, TEMP%)
			NEXT TNLOOP%

		ELSE
			!
			! INDIVIDUAL TOTALS
			!
			I1% = INSTR(1%, Z$, ")")
			TEMP% = 1%
			TEMP1% = 1%
			A(VAL(MID(Z$, 2%, I% - 2%)), &
				PARSE_EXPR(SEG$(Z$, I% + 1%, I1% - 1%), TEMP1%)) = &
				PARSE_EXPR(Z2$, TEMP%)
		END IF

	CASE "GOTO"
		Z$ = EDIT$(RIGHT(Z3$, I% + 1%), 8% + 128%)
		TEMP% = 0%
		TEMP% = TEMP1% &
			IF FINSTA_CMD$(TEMP1%) = Z$ &
			FOR TEMP1% = 1% TO FINSTA_CMD%
		IF TEMP%
		THEN
			FINSTA_POINTER% = TEMP%
		END IF

	CASE "LOOP"
		!*****************************************************
		! LOOP
		!*****************************************************

		Z$ = Z3$
		Z$ = RIGHT(Z$, I% + 1%)
		I% = INSTR(1%, Z$, " ")
		R$ = LEFT(Z$, I% - 1%)

		IF RIGHT(Z$, I% + 1%) = "END"
		THEN

			!
			! Start a loop
			!
			FOR L1% = 0% UNTIL L9$(L9% - 1%, L1%) = ""
				I% = INSTR(1%, L9$(L9% - 1%, L1%), ",")
				I% = INSTR(1%, L9$(L9% - 1%, L1%), '255'C) &
					IF I% = 0%
				I% = INSTR(2%, L9$(L9% - 1%, L1%), '"') + 1% &
					IF LEFT(L9$(L9% - 1%, L1%), 1%) = '"'
				L9$(L9% - 1%, L1%) = RIGHT(L9$(L9% - 1%, L1%), I% + 1%)
				IF INSTR(1%, L9$(L9% - 1%, L1%), ",") = 0% AND &
					INSTR(1%, L9$(L9% - 1%, L1%), '255'C) = 0%
				THEN
					L9% = L9% - 1%
					L9$(L9%, L%) = "" FOR L% = 0% TO 4%
					GOTO 1060
				END IF

			NEXT L1%

			!
			! Pull up one line from the command file
			!
			FINSTA_POINTER% = 1%
			FINSTA_POINTER% = FINSTA_POINTER% + 1% &
				UNTIL (LEFT(FINSTA_CMD$(FINSTA_POINTER%), 5% + LEN(R$)) = &
					"LOOP " + R$) OR (FINSTA_POINTER% >= FINSTA_CMD%)

 LoopLoop:		!
			! Pull up one line from the command file
			!
			FINSTA_POINTER% = FINSTA_POINTER% + 1%
			GOTO 1040 IF FINSTA_POINTER% > FINSTA_CMD%
			K$ = FINSTA_CMD$(FINSTA_POINTER%)

			GOTO LoopLoop UNLESS LEFT(K$, 13%) <> "LOOP VARIABLE" AND LEFT(K$, 5% + &
				LEN(R$)) <> "LOOP " + R$
			Z$ = K$
			GOTO 1070
		END IF

		L9$(L9%, 0%) = RIGHT(Z$, I% + 1%) + '255'C + R$
		L9% = L9% + 1%
		I% = 0%

 LoopLoop1:	!
		! Pull up one line from the command file
		!
		FINSTA_POINTER% = FINSTA_POINTER% + 1%
		GOTO 1040 IF FINSTA_POINTER% > FINSTA_CMD%
		K$ = FINSTA_CMD$(FINSTA_POINTER%)

		IF LEFT(K$, 13%) = "LOOP VARIABLE"
		THEN
			Z$ = K$
			GOSUB 18100
			K$ = Z$
			I% = I% + 1%
			K$ = RIGHT(EDIT$(K$, 12%), 15%)
			I1% = INSTR(1%, K$, " ")
			L9$(L9% - 1%, I%) = RIGHT(K$, I1% + 1%) + '255'C + &
				LEFT(K$, I1% - 1%)
			GOTO LoopLoop1
		END IF

		Z$ = K$
		GOTO 1070

	!*******************************************************
	! Old "FORM NORMAL" and "FORM WIDE"
	!*******************************************************
	CASE "FORM"
		! Ignore

	!*******************************************************
	! END
	!*******************************************************

	CASE "END"
		GOTO 1040

	CASE "SET"
		SELECT RIGHT(Z$, I% + 1%)
		CASE "ZERO"
			ZERO1$ = "Y"
		CASE "NOZERO"
			ZERO1$ = "N"
		CASE "DEBUG"
			DEBUG% = -1%
		CASE "NODEBUG"
			DEBUG% = 0%
		END SELECT

	CASE "TESTPAGE"
		Z4$ = RIGHT(Z$, I% + 1%)
		TEMP% = 1%
		TEMP = PARSE_EXPR(Z4$, TEMP%)
		PAGE_BREAK_TEST% = TEMP

	CASE ELSE
		TEXT$ = "-- Syntax Error -- " + Z$
		CALL OUTP_LINENOTITLE("", UTL_REPORTX, TEXT$, 0%)

	END SELECT

	GOTO 1060

	%PAGE

 PrintData:
2400	!******************************************************************
	! PRINT DATA IN Z$, Z1$, ..., Zn$ USING FORMAT NUMBER FMT%
	!******************************************************************

	GOSUB TestPageBreak

	P0$ = ""
	P4% = 0%
	LOOP.COUNT% = 0%

	!
	! Handle ZERO$ and ZERO1$ possibilities
	! ZERO1$ has precidence over ZERO$.
	!
	SELECT ZERO1$
	CASE "N"
		ZERO% = 0%
	CASE "Y"
		ZERO% = -1%
	CASE ELSE
		IF ZERO$ = "N"
		THEN
			ZERO% = 0%
		ELSE
			ZERO% = -1%
		END IF
	END SELECT

	F$ = FMT$(FMT%)
	F1$ = ""
	GOTO 2490 IF F$ = ""

	IF LEFT(F$, 6%) = "WRKCAP"
	THEN
		WRKCAP.FLAG% = -1%
		F$ = RIGHT(F$, 7%)
	ELSE
		WRKCAP.FLAG% = 0%
	END IF

	!
	! Strip format statement into <format> and <Parameter> parts
	!
	PARM_POINTER% = 1%
	PARM_VALUE$ = Z1$

	I% = INSTR(1%, F$, '255'C)
	IF I% <> 0%
	THEN
		F1$ = EDIT$(RIGHT(F$, I% + 1%) + ",", 2%)
		F1$ = RIGHT(F1$, 2%) IF LEFT(F1$, 1%) = ","
		F$ = LEFT(F$, I% - 1%)
	END IF

	!
	! If this line is a total line then print regardless
	!
	ZERO% = -1% IF F1$ = "" OR INSTR(1%, F1$, "X")

	!
	! Parse all of the parameters in Z1$ into values
	!
	F1TEMP% = 1%

2420	!
	! Search for the first formatting character in F$
	!
	F1TEMP% = F1TEMP% + 1% IF MID(F1$, F1TEMP%, 1%) = ","

	L% = STR$FIND_FIRST_IN_SET(F$, "\$#")

	IF (L% = 0%)
	THEN
		P0$ = P0$ + F$
		F$ = ""
		GOTO 2490
	ELSE
		P0$ = P0$ + LEFT(F$, L% - 1%)
		F$ = RIGHT(F$, L%)
	END IF

	LOOP.COUNT% = LOOP.COUNT% + 1%

	!
	! Find end of format string.
	!
	IF LEFT(F$, 1%) = "\"
	THEN
		I% = INSTR(2%, F$, "\")
		IF MID(F1$, F1TEMP%, 1%) = "X"
		THEN
			I3% = INSTR(PARM_POINTER%, PARM_VALUE$, ",")
			I3% = LEN(PARM_VALUE$) + 1% IF I3% = 0%
			P0$ = P0$ + FORMAT$(MID(PARM_VALUE$, &
				PARM_POINTER%, I3% - PARM_POINTER%), &
				LEFT(F$, I%))
			PARM_POINTER% = I3% + 1%
		ELSE
			T% = PARSE_EXPR(F1$, F1TEMP%)
			P0$ = P0$ + FORMAT$(T$(T%), LEFT(F$, I%))
		END IF
		T% = INSTR(F1TEMP%, F1$, ",")
		F1TEMP% = T% + 1%
		F$ = RIGHT(F$, I% + 1%)
		GOTO 2420
	END IF

2450	!
	! Search for end of numeric print using format
	!
	L% = STR$FIND_FIRST_NOT_IN_SET(F$, "$#,.-^~")

	IF L% = 0%
	THEN
		L% = LEN(F$)
	ELSE
		L% = L% - 1%
	END IF

2460	!
	! Calculate value to print
	!
	P = PARSE_EXPR(F1$, F1TEMP%)

2470	RETURN IF P <= 0. AND WRKCAP.FLAG%
	WRKCAP.FLAG% = 0%
	UF% = INSTR(1%, LEFT(F$, L%), "~")
	P = INT(P + .51) IF INSTR(1%, LEFT(F$, L%), ".") = 0%
	ZERO% = -1% IF P <> 0.0

	!
	! Skip over lots of code is we aren't goint to print
	!
	IF NOPRINT% <> -1%
	THEN
		!
		! Format the number for output
		!
		P$ = FORMAT$(ABS(P), LEFT(F$, L%))
		LZ% = LEN(P$)
		P$ = EDIT$(P$, -1%)

		IF UF% = 0%
		THEN
			!
			! Negitive numbers are formatted (xxx)
			!
			IF P < 0.0
			THEN
				P$ = "(" + P$ + ")"
			ELSE
				P$ = P$ + " "
			END IF
		ELSE
			!
			! Strip off ~
			!
			ZZ% = INSTR(1%, P$, "~")
			P$ = LEFT(P$, ZZ% - 1%) + RIGHT(P$, ZZ% + 1%)

			!
			! Negitive numbers are formatted xxxU, positive are xxxF
			!
			IF P < 0.0
			THEN
				P$ = P$ + "U"
			ELSE
				P$ = P$ + "F"
			END IF
		END IF

		P1$ = SPACE$(LZ%)
		RSET P1$ = P$
		P0$ = P0$ + P1$
	END IF

	F$ = RIGHT(F$, L% + 1%)
	IF S% = -1%
	THEN
		P4% = P4% + 1%
		A(S0%, P4%) = A(S0%, P4%) + P
	END IF

2480	GOTO 2420

2490	!
	! Output line
	!
	RETURN IF NOPRINT% = -1% OR ZERO% = 0%
	STR_BUILD$ = STR_BUILD$ + P0$
	RETURN IF SEMICOLON% = -1%

	CALL OUTP_LINENOTITLE(LYT_LINE$, UTL_REPORTX, STR_BUILD$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT
	STR_BUILD$ = ""

	GOSUB TestPageBreak

	RETURN

	!*******************************************************************
	! Check for a page break
	!*******************************************************************

 TestPageBreak:
	IF (UTL_REPORTX::LINENO > &
		UTL_REPORTX::PAGELEN - PAGE_BREAK_TEST% - 7%) AND &
		(UTL_REPORTX::PRINTTO <> 1%)
	THEN
		CALL OUTP_LINENOTITLE(LYT_LINE$, UTL_REPORTX, STR_BUILD$, 1%) &
			IF UTL_REPORTX::PRINTTO <> 1%
		GOTO ExitProgram IF UTL_REPORTX::STAT

		UTL_REPORTX::LINENO = 0%
		Z1$ = F7$
		REPORT_PAGE% = REPORT_PAGE% + 1%
		GOSUB PrintTitle IF UTL_REPORTX::PRINTTO <> 1%
	END IF

	PAGE_BREAK_TEST% = 0%

	RETURN

	%PAGE

 ExitProgram:
10000	!============================================================
	!	END PROGRAM
	!============================================================

	CLOSE FINSTA.WRK%, GL_CHART.CH%, GL_BUD_YYYY.CH%

10010 !	WHEN ERROR IN
 !		KILL FINSTA_WORK_FILE$ WHILE (-1%)
 !	USE
 !		CONTINUE 10040
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(FINSTA_WORK_FILE$ + ";*")

10040	CALL OUTP_FINISHNOTITLE(UTL_REPORTX)

	!
	! Exit to next program or menu
	!
	IF TRM$(UTL_REPORTX::NEXTRUN) = ""
	THEN
		CALL SUBR_3EXITPROGRAM(SCOPE, "", "")
	ELSE
		CALL SUBR_3EXITPROGRAM(SCOPE, "RUN " + UTL_REPORTX::NEXTRUN, "")
	END IF

	%PAGE

18100	!================================================================
	! REPLACE VARIABLES
	!================================================================

	!
	! Replace loop variables
	!
18180	FOR L% = 0% TO L9% - 1%
		FOR L1% = 0% UNTIL L9$(L%, L1%) = ""
			I1% = INSTR(1%, L9$(L%, L1%), ",")
			I1% = INSTR(1%, L9$(L%, L1%), '255'C) IF I1% = 0%
			I1% = INSTR(2%, L9$(L%, L1%), '"') + 1% &
				IF LEFT(L9$(L%, L1%), 1%) = '"'
			L8$ = LEFT(L9$(L%, L1%), I1% - 1%)
			L7$ = RIGHT(L9$(L%, L1%), INSTR(1%, L9$(L%, L1%), &
				'255'C) + 1%)
			L8$ = MID(L8$, 2%, LEN(L8$) - 2%) IF LEFT(L8$, 1%) = '"'
			GOTO 18195 IF(LEFT(Z$, 4%) = "LOOP" AND INSTR(1%, Z$, &
				"END") <> 0%) OR L8$ = ""

18190			I6% = INSTR(1%, Z$, L7$)
			IF I6% <> 0%
			THEN
				Z$ = LEFT(Z$, I6% - 1%) + L8$ + &
					RIGHT(Z$, I6% + LEN(L7$))
				GOTO 18190
			END IF

18195		NEXT L1%
	NEXT L%
	RETURN

	%PAGE

18200	!==============================================================
	! TOTAL MONTHS
	!==============================================================

	FOR L% = 0% TO 20%
		ACTDOL(L%) = 0.0
		ACTUNIT(L%) = 0.0
		ACTHOUR(L%) = 0.0
	NEXT L%
	FOR L% = 0% TO 13%
		BUDDOL(L%) = 0.0
		BUDUNIT(L%) = 0.0
		BUDHOUR(L%) = 0.0
	NEXT L%
	BDEOY(L%) = 0.0 FOR L% = 0% TO 2%

18250	!****************************************************************
	! Get budget, play with numbers, etc.
	!****************************************************************

	WHEN ERROR IN
		GET #GL_BUD_YYYY.CH%, KEY #0% EQ GL_CHART::ACCT, REGARDLESS
	USE
		GL_BUD_YYYY::DOLLAR(LOOP%), GL_BUD_YYYY::UNIT(LOOP%), &
			GL_BUD_YYYY::HOUR(LOOP%) = 0.0 FOR LOOP% = 1% TO 13%
		CONTINUE 18280
	END WHEN

18280	!
	! If this is a working financial statement, then shift things
	! around to make them easier to handle. (Look like final)
	!
	IF WORK%
	THEN
		FOR I% = 20% TO 1% STEP -1%
			GL_CHART::DOLLAR(I%) = GL_CHART::DOLLAR(I% - 1%)
			GL_CHART::UNIT(I%) = GL_CHART::UNIT(I% - 1%)
			GL_CHART::HOUR(I%) = GL_CHART::HOUR(I% - 1%)
		NEXT I%

		GL_CHART::DOLLAR(0%) = GL_CHART::CURDOL
		GL_CHART::UNIT(0%) = GL_CHART::CURHOUR
		GL_CHART::HOUR(0%) = GL_CHART::CURUNIT
	END IF

	!
	! If this is a type "R" or "E" account, then we need to know the
	! YTD amount from last year as a beginning point of the budget
	!
	IF INSTR(1%, "RE", GL_CHART::ACCTYPE) <> 0%
	THEN
		BUDDOL(0%) = BUDDOL(0%) + &
			GL_CHART::DOLLAR(GL_PERIOD::LASTPERCLO + 1%)
		BDEOY(0%) = BDEOY(0%) + &
			GL_CHART::DOLLAR(GL_PERIOD::LASTPERCLO + 1%)
		BUDUNIT(0%), BUDHOUR(0%) = 0.0
	END IF

	!
	! If this is NOT a CashFlow or WorkCapitol statement,
	!
	IF INSTR(1%, "CW", TYP$) = 0%
	THEN
		FOR L% = 0% TO 20%
			ACTDOL(L%) = ACTDOL(L%) + GL_CHART::DOLLAR(L%)
			ACTUNIT(L%) = ACTUNIT(L%) + GL_CHART::UNIT(L%)
			ACTHOUR(L%) = ACTHOUR(L%) + GL_CHART::HOUR(L%)
		NEXT L%

		FOR L% = 1% TO GL_PERIOD::LASTPERCLO
			BUDDOL(L%) = BUDDOL(L%) + &
				GL_BUD_YYYY::DOLLAR(GL_PERIOD::LASTPERCLO - &
				L% + 1%)
			BUDUNIT(L%) = BUDUNIT(L%) + &
				GL_BUD_YYYY::UNIT(GL_PERIOD::LASTPERCLO - &
				L% + 1%)
			BUDHOUR(L%) = BUDHOUR(L%) + &
				GL_BUD_YYYY::HOUR(GL_PERIOD::LASTPERCLO - &
				L% + 1%)
		NEXT L%
		FOR L% = 1% TO GL_PERIOD::FPFY
			BDEOY(0%) = BDEOY(0%) + &
				GL_BUD_YYYY::DOLLAR(L%)
		NEXT L%

	ELSE
		FOR L% = 0% TO 19%
			DOLLAR = GL_CHART::DOLLAR(L%)
			DOLLAR = GL_CHART::DOLLAR(L%) - &
				GL_CHART::DOLLAR(L% + 1%) &
				IF IND.FLAG$ = "I" OR IND.FLAG$ = "D"
			DOLLAR = -(GL_CHART::DOLLAR(L%) - &
				GL_CHART::DOLLAR(L% + 1%)) &
				IF IND.FLAG$ = "I" AND &
				INSTR(1%, "LO", GL_CHART::ACCTYPE) OR &
				IND.FLAG$ = "D" AND &
				INSTR(1%, "LO", GL_CHART::ACCTYPE)

			IF IND.FLAG$ = "I"
			THEN
				ACTDOL(L%) = ACTDOL(L%) + DOLLAR IF DOLLAR > 0.0
			ELSE
				IF IND.FLAG$ = "D"
				THEN
					ACTDOL(L%) = ACTDOL(L%) - DOLLAR &
						IF DOLLAR < 0.0
				ELSE
					ACTDOL(L%) = ACTDOL(L%) + DOLLAR
				END IF
			END IF
		NEXT L%

		FOR L% = 1% TO 13%
			DOLLAR = GL_BUD_YYYY::DOLLAR(L%)
			IF IND.FLAG$ = "I"
			THEN
				ACTDOL(L%) = ACTDOL(L%) + DOLLAR IF DOLLAR > 0.0
			ELSE
				IF IND.FLAG$ = "D"
				THEN
					ACTDOL(L%) = ACTDOL(L%) - DOLLAR &
						IF DOLLAR < 0.0
				ELSE
					ACTDOL(L%) = ACTDOL(L%) + DOLLAR
				END IF
			END IF
		NEXT L%
	END IF

	RETURN

	%PAGE

 PrintTitle:
18500	!********************************************************************
	! Print the title
	!********************************************************************
	UTL_REPORTX::LINENO = 0%

	FOR L% = 1% TO T0%
		T0$(0%) = T0$(L%)

18510		I% = INSTR(1%, T0$(0%), "<")
		IF I% = 0%
		THEN
			CALL OUTP_LINENOTITLE("", UTL_REPORTX, &
				STR_BUILD$ + T0$(0%), 0%)
			GOTO ExitProgram IF UTL_REPORTX::STAT
			STR_BUILD$ = ""
			GOTO 18540
		END IF

18520		STR_BUILD$ = STR_BUILD$ + LEFT(T0$(0%), I% - 1%)
		T0$(0%) = RIGHT(T0$(0%), I%)
		I% = INSTR(1%, T0$(0%), ">")
		IF LEFT(T0$(0%), I%) = "<P>"
		THEN
			STR_BUILD$ = STR_BUILD$ + FORMAT$(REPORT_PAGE%, "###")
		ELSE
			IF LEFT(T0$(0%), 5%) = "<DATE"
			THEN
				STR_BUILD$ = STR_BUILD$ + &
					FORMAT$(UTL_REPORTX::REPDATE, &
					"\" + SPACE$(I% - 2%) + "\")
			ELSE
				I1% = INSTR(1%, Z1$, ",")
				STR_BUILD$ = STR_BUILD$ + &
					FORMAT$(LEFT(Z1$, I1% - 1%), &
					"\" + SPACE$(I% - 2%) + "\")
				Z1$ = RIGHT(Z1$, I1% + 1%)
			END IF
		END IF

18530		T0$(0%) = RIGHT(T0$(0%), I% + 1%)
		GOTO 18510

18540	NEXT L%

	RETURN

	%Page

 GetChartRecord:
18600	!*****************************************************************
	! Get chart record
	!*****************************************************************

	CHART_GET_SUCCESS% = -1%

	WHEN ERROR IN
		SELECT TYP$
		CASE "C"
			IF W1$ = ""
			THEN
				RESET #GL_CHART.CH%, KEY #1%
				GET #GL_CHART.CH%, REGARDLESS
			ELSE
				GET #GL_CHART.CH%, KEY #1% GE W1$, REGARDLESS
			END IF

		CASE "W"
			IF W1$ = ""
			THEN
				RESET #GL_CHART.CH%, KEY #2%
				GET #GL_CHART.CH%, REGARDLESS
			ELSE
				GET #GL_CHART.CH%, KEY #2% GE W1$, REGARDLESS
			END IF

		CASE ELSE
			IF W1$ = ""
			THEN
				RESET #GL_CHART.CH%, KEY #3%
				GET #GL_CHART.CH%, REGARDLESS
			ELSE
				GET #GL_CHART.CH%, KEY #3% GE W1$, REGARDLESS
			END IF

		END SELECT
	USE
		CONTINUE 18690
	END WHEN

	CHART_GET_SUCCESS% = 0%

18690	RETURN


 GetChartNext:
18700	!*****************************************************************
	! Get next chart record
	!*****************************************************************

	CHART_GET_NEXT% = -1%

	WHEN ERROR IN
		GET #GL_CHART.CH%, REGARDLESS
	USE
		CONTINUE 18790 IF ERR = 11%
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

	CHART_GET_NEXT% = 0%

18790	RETURN

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_3MESSAGE(SCOPE, FILENAME$ + " " + NUM1$(ERL) + " " + &
		ERT$(ERR), "ERR", ERN$, "ERROR" + NUM1$(ERR))
	GOTO ExitProgram

19000	!******************************************************************
	! ERRORS
	!******************************************************************

	FILENAME$ = ""
	RESUME HelpError

	%PAGE

30000	!----------------------PROGRAM FUNCTIONS------------------------

30400	!
	! Strip a format statement apart into the format part and the
	! variable definition parts.  The format part is put into
	! T0$(0%).  Returns <format> <255> <parameters>
	!
	DEF FNFORMAT$(Z$)
		T0$(0%) = ""

30410		GOTO 30450 IF Z$ = ""
		IF LEFT(Z$, 1%) = '"'
		THEN
			I% = INSTR(2%, Z$, '"')
			I% = LEN(Z$) + 1% IF I% = 0%
			T0$(0%) = T0$(0%) + MID(Z$, 2%, I% - 2%)
			Z$ = RIGHT(Z$, I% + 1%)
			GOTO 30410
		END IF

30420		IF LEFT(Z$, 1%) = ";"
		THEN
			Z$ = RIGHT(Z$, 2%)
			GOTO 30410
		END IF

30430		IF LEFT(Z$, 3%) = "TAB"
		THEN
			I% = INSTR(4%, Z$, ")")
			I% = INSTR(4%, Z$, ";") - 1% IF I% = 0%
			I% = LEN(Z$) IF I% <= 0%
			I2% = VAL%(MID(Z$, 5%, I% - 5%))
			T0$(0%) = T0$(0%) + SPACE$(I2% - LEN(T0$(0%)))
			Z$ = RIGHT(Z$, I% + 1%)
			GOTO 30410
		END IF

30440		T0$(0%) = T0$(0%) + '255'C + Z$ IF F1%

30450		FNFORMAT$ = T0$(0%)
	FNEND

30900	END


	%PAGE

31000	!******************************************************************
	! This function is used to replace all occurrences of one variable
	! with a given value.
	!******************************************************************

	FUNCTION STRING REPLACE_STRING(STRING STARTING, WORD ARRAY_COUNT, &
		STRING ARRAY_VAR(), STRING ARRAY_VALUE())

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Set up a var to work in so we don't mess up the original
	!
	Z$ = STARTING
	!
	! For all of the vars defined in the ARRAY, do the search/
	! replace sequence.
	!
	FOR L% = 1% TO ARRAY_COUNT

		Z1$ = ARRAY_VAR(L%)

		!
		! If it is enclosed in brackets, we must let them
		! specify an optional width in the variable found
		! in the string.
		!
		Z1% = 0%
		IF LEFT(Z1$, 1%) = "<"
		THEN
			Z1$ = LEFT(Z1$, LEN(Z1$) - 1%)
			Z1% = -1%
		END IF

		I6% = 0%

31010		I6% = INSTR(I6% + 1%, Z$, Z1$)
		IF I6% <> 0% AND Z1$ <> ""
		THEN
			IF Z1% = 0%
			THEN
				!
				! Simple replacement, no width
				! necessary.
				!
				I7% = I6% + LEN(Z1$)
				Z$ = LEFT(Z$, I6% - 1%) + &
					ARRAY_VALUE(L%) + &
					RIGHT(Z$, I7%)
			ELSE
				!
				! OH NO| It may have a width
				!
				I7% = INSTR(I6%, Z$, ">")
				I7% = I6% + LEN(Z1$) IF I7% = 0%
				Z2$ = SEG$(Z$, I6% + LEN(Z1$), I7% - 1%)
				IF Z2$ <> ""
				THEN
					!
					! Anything besides a number causes
					! a mis-match.
					!
					GOTO 31010 IF INSTR(1%, "0123456789", &
						MID(Z2$, I%, 1%)) = 0% &
						FOR I% = 1% TO LEN(Z2$)
					!
					! Create the append string
					!
					Z2% = VAL%(Z2$)
					AV$ = FORMAT$(ARRAY_VALUE(L%), &
						"'" + STRING$(Z2%, A"C"B))
				ELSE
					AV$ = ARRAY_VALUE(L%)
				END IF

				!
				! Replace string
				!
				Z$ = LEFT(Z$, I6% - 1%) + AV$ + &
					RIGHT(Z$, I7% + 1%)

			END IF

			!
			! Go back and try again
			!
			GOTO 31010
		END IF
	NEXT L%

	REPLACE_STRING = TRM$(Z$)

	END FUNCTION

	%PAGE

31200	FUNCTION STRING STRING_PARSE(XPARSE$, RIBIT%)

	!**********************************************************************
	! Parsing grammer for GENFIN parser
	!
	!	<expr>	-->	<expr> + <var>			; addition
	!			| <var>
	!
	!	<var>	-->	| "string"			; fill in value
	!			| 'string'			; fill in value
	!**********************************************************************

		OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

		!
		! External functions
		!
		EXTERNAL STRING FUNCTION STRING_EXPR

		RIBIT% = RIBIT% + 1% WHILE MID(XPARSE$, RIBIT%, 1%) == " "
		STRING_PARSE = STRING_EXPR(XPARSE$, RIBIT%)
		RIBIT% = RIBIT% + 1% WHILE MID(XPARSE$, RIBIT%, 1%) == " "

	END FUNCTION

31300	FUNCTION STRING STRING_EXPR(XPARSE$, RIBIT%)
	!**********************************************************************
	! PARSE_EXPR is the expression parser of the genfin parser.  It handles
	! the <expr> parts of the evaluation
	!**********************************************************************

		OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

		!
		! External functions
		!
		EXTERNAL STRING FUNCTION STRING_VAR

		RIBIT% = RIBIT% + 1% WHILE MID(XPARSE$, RIBIT%, 1%) == " "
		TEMP$ = STRING_VAR(XPARSE$, RIBIT%)

31310		RIBIT% = RIBIT% + 1% WHILE MID(XPARSE$, RIBIT%, 1%) == " "

		!
		! Addition
		!
		IF MID(XPARSE$, RIBIT%, 1%) = "+"
		THEN
			RIBIT% = RIBIT% + 1%
			TEMP1$ = STRING_VAR(XPARSE$, RIBIT%)
			TEMP$ = TEMP$ + TEMP1$
			GOTO 31310
		END IF

		STRING_EXPR = TEMP$

	END FUNCTION

31600	FUNCTION STRING STRING_VAR(XPARSE$, RIBIT%)
	!**********************************************************************
	! PARSE_VAR is the expression parser of the genfin parser.  It handles
	! the <var> parts of the evaluation.
	!
	! This module handles the variables available to genfin, except the
	! odd (n) type variable which is handled in PARSE_FACTOR.
	!**********************************************************************

		OPTION SIZE = (INTEGER LONG, REAL GFLOAT)


		IF RIBIT% > LEN(XPARSE$)
		THEN
			STRING_VAR = ""
			EXIT FUNCTION
		END IF

		!
		! Fill in parameter
		!
		IF MID(XPARSE$, RIBIT%, 1%) = '"'
		THEN
			I% = INSTR(RIBIT% + 1%, XPARSE$, '"')
			STRING_VAR = SEG$(XPARSE$, RIBIT% + 1%, I% - 1%)
			RIBIT% = I% + 1%
			EXIT FUNCTION
		END IF

		!
		! Fill in parameter
		!
		IF MID(XPARSE$, RIBIT%, 1%) = "'"
		THEN
			I% = INSTR(RIBIT% + 1%, XPARSE$, "'")
			STRING_VAR = SEG$(XPARSE$, RIBIT% + 1%, I% - 1%)
			RIBIT% = I% + 1%
			EXIT FUNCTION
		END IF

	END FUNCTION

	!**********************************************************************
	! Genfin parser.  This parser was Initialy designed for GL_GENFIN, to
	! replace the hack and burn parser that was originally there.
	!
	! To calculate a value using the parser, use something similiar to
	! the following:
	!
	!		JUNK% = 1%
	!		STRING$ = "(1+2)*(3+4)"
	!		RETURN_VALUE = PARSE_EXPR(STRING$, JUNK%)
	!
	! Where STRING$ is the string to be evaluated, and JUNK% points to
	! the first character to be parsed.
	!
	! PARSER returns the calculated value as its returned value, and it
	! alse returns the character position following the expression in JUNK%
	! so you can see where it finished.
	!
	!**********************************************************************
	!
	! Parsing grammer for GENFIN parser
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
	!	<factor> -->	(n)				; special genfin var
	!			| ABS(<expr>)			; Absolute value
	!			| NOT <factor>			; NOT
	!			| (<expr>)			; paren
	!			| -<factor>			; unary minus
	!			| <var>
	!			| number
	!
	!	<var>	-->	Tn(<expr>)			; normal totals
	!			| BDP(<expr>)			; budget $ period amount
	!			| BDY(<expr>)			; budget $ year amount
	!			| ADP(<expr>)			; actual $ period amount
	!			| ADY(<expr>)			; actual $ year amount
	!			| ADEOY(<expr>)			; actual $ at end of year
	!			| BUP(<expr>)			; budget unit period amount
	!			| BUY(<expr>)			; budget unit year amount
	!			| AUP(<expr>)			; actual unit period amount
	!			| AUY(<expr>)			; actual unit year amount
	!			| AUEOY(<expr>)			; actual unit at end of year
	!			| BHP(<expr>)			; budget hour period amount
	!			| BHY(<expr>)			; budget hour year amount
	!			| AHP(<expr>)			; actual hour period amount
	!			| AHY(<expr>)			; actual hour year amount
	!			| AHEOY(<expr>)			; actual hour at end of year
	!			| FPFY				; Number of periods/year
	!			| X				; fill in value
	!
	! Logical operations return one (1) for true, and zero (0) for false.
	!
	!*************************************************************************

32100	FUNCTION REAL PARSE_EXPR(XPARSE$, RIBIT%)
	!*************************************************************************
	! PARSE_EXPR is the expression parser of the genfin parser.  It handles
	! the <expr> parts of the evaluation
	!*************************************************************************

		OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

		!
		! External functions
		!
		EXTERNAL REAL FUNCTION PARSE_TERM

		RIBIT% = RIBIT% + 1% WHILE MID(XPARSE$, RIBIT%, 1%) == " "
		TEMP = PARSE_TERM(XPARSE$, RIBIT%)

32110		RIBIT% = RIBIT% + 1% WHILE MID(XPARSE$, RIBIT%, 1%) == " "

		!
		! Addition
		!
		IF MID(XPARSE$, RIBIT%, 1%) = "+"
		THEN
			RIBIT% = RIBIT% + 1%
			TEMP1 = PARSE_TERM(XPARSE$, RIBIT%)
			TEMP = TEMP + TEMP1
			GOTO 32110
		END IF

		!
		! Subtraction
		!
		IF MID(XPARSE$, RIBIT%, 1%) = "-"
		THEN
			RIBIT% = RIBIT% + 1%
			TEMP1 = PARSE_TERM(XPARSE$, RIBIT%)
			TEMP = TEMP - TEMP1
			GOTO 32110
		END IF

	RIBIT% = RIBIT% + 1% WHILE MID(XPARSE$, RIBIT%, 1%) == " "
	PARSE_EXPR = TEMP

	END FUNCTION

32200	FUNCTION REAL PARSE_TERM(XPARSE$, RIBIT%)
	!*************************************************************************
	! PARSE_TERM is the expression parser of the genfin parser.  It handles
	! the <term> parts of the evaluation
	!*************************************************************************

		OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

		!
		! External functions
		!
		EXTERNAL REAL FUNCTION PARSE_LOGICAL

		RIBIT% = RIBIT% + 1% WHILE MID(XPARSE$, RIBIT%, 1%) == " "
		TEMP = PARSE_LOGICAL(XPARSE$, RIBIT%)

32210		RIBIT% = RIBIT% + 1% WHILE MID(XPARSE$, RIBIT%, 1%) == " "

		!
		! Multiplication
		!
		IF MID(XPARSE$, RIBIT%, 1%) = "*"
		THEN
			RIBIT% = RIBIT% + 1%
			TEMP1 = PARSE_LOGICAL(XPARSE$, RIBIT%)
			TEMP = TEMP * TEMP1
			GOTO 32210
		END IF

		!
		! Division
		!
		IF MID(XPARSE$, RIBIT%, 1%) = "/"
		THEN
			RIBIT% = RIBIT% + 1%
			TEMP1 = PARSE_LOGICAL(XPARSE$, RIBIT%)
			IF TEMP1 = 0.0
			THEN
				TEMP = 0.0
			ELSE
				TEMP = TEMP / TEMP1
			END IF
			GOTO 32210
		END IF

	PARSE_TERM = TEMP

	END FUNCTION

32300	FUNCTION REAL PARSE_LOGICAL(XPARSE$, RIBIT%)
	!**********************************************************************
	! PARSE_LOGICAL is the expression parser of the genfin parser.  It
	! handles the <logical> parts of the evaluation
	!**********************************************************************

		OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

		!
		! External functions
		!
		EXTERNAL REAL FUNCTION PARSE_FACTOR

		RIBIT% = RIBIT% + 1% WHILE MID(XPARSE$, RIBIT%, 1%) == " "

		TEMP = PARSE_FACTOR(XPARSE$, RIBIT%)

32310		RIBIT% = RIBIT% + 1% WHILE MID(XPARSE$, RIBIT%, 1%) == " "

		!
		! Logical and
		!
		IF MID(XPARSE$, RIBIT%, 3%) = "AND"
		THEN
			RIBIT% = RIBIT% + 3%
			TEMP1 = PARSE_FACTOR(XPARSE$, RIBIT%)
			TEMP = ((TEMP <> 0.0) AND (TEMP1 <> 0.0)) AND 1%
			GOTO 32310
		END IF

		!
		! Logical or
		!
		IF MID(XPARSE$, RIBIT%, 2%) = "OR"
		THEN
			RIBIT% = RIBIT% + 2%
			TEMP1 = PARSE_FACTOR(XPARSE$, RIBIT%)
			TEMP = ((TEMP <> 0.0) OR (TEMP1 <> 0.0)) AND 1%
			GOTO 32310
		END IF

		!
		! not equal to
		!
		IF MID(XPARSE$, RIBIT%, 2%) = "<>"
		THEN
			RIBIT% = RIBIT% + 2%
			TEMP1 = PARSE_FACTOR(XPARSE$, RIBIT%)
			TEMP = (TEMP <> TEMP1) AND 1%
			GOTO 32310
		END IF

		!
		! Less than or equal to
		!
		IF MID(XPARSE$, RIBIT%, 2%) = "<="
		THEN
			RIBIT% = RIBIT% + 2%
			TEMP1 = PARSE_FACTOR(XPARSE$, RIBIT%)
			TEMP = (TEMP <= TEMP1) AND 1%
			GOTO 32310
		END IF

		!
		! Greater than or equal to
		!
		IF MID(XPARSE$, RIBIT%, 2%) = ">="
		THEN
			RIBIT% = RIBIT% + 2%
			TEMP1 = PARSE_FACTOR(XPARSE$, RIBIT%)
			TEMP = (TEMP >= TEMP1) AND 1%
			GOTO 32310
		END IF

		!
		! Equal to
		!
		IF MID(XPARSE$, RIBIT%, 1%) = "="
		THEN
			RIBIT% = RIBIT% + 1%
			TEMP1 = PARSE_FACTOR(XPARSE$, RIBIT%)
			TEMP = (TEMP = TEMP1) AND 1%
			GOTO 32310
		END IF

		!
		! Greater than
		!
		IF MID(XPARSE$, RIBIT%, 1%) = ">"
		THEN
			RIBIT% = RIBIT% + 1%
			TEMP1 = PARSE_FACTOR(XPARSE$, RIBIT%)
			TEMP = (TEMP > TEMP1) AND 1%
			GOTO 32310
		END IF

		!
		! Less than
		!
		IF MID(XPARSE$, RIBIT%, 1%) = "<"
		THEN
			RIBIT% = RIBIT% + 1%
			TEMP1 = PARSE_FACTOR(XPARSE$, RIBIT%)
			TEMP = (TEMP < TEMP1) AND 1%
			GOTO 32310
		END IF

	PARSE_LOGICAL = TEMP

	END FUNCTION

32400	FUNCTION REAL PARSE_FACTOR(XPARSE$, RIBIT%)
	!**********************************************************************
	! PARSE_FACTOR is the expression parser of the genfin parser.  It handles
	! the <factor> parts of the evaluation
	!**********************************************************************

		OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

		!
		! External functions
		!
		EXTERNAL REAL FUNCTION PARSE_EXPR
		EXTERNAL REAL FUNCTION PARSE_FACTOR
		EXTERNAL REAL FUNCTION PARSE_NUMBER
		EXTERNAL REAL FUNCTION PARSE_VAR

		!
		! Map statements
		!
		%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
		MAP (GL_PERIOD)	GL_PERIOD_CDD	GL_PERIOD

		!
		! Common areas
		!
		COMMON (PARSECOM) ACTDOL(20%), ACTUNIT(20%), ACTHOUR(20%), &
			BUDDOL(13%), BUDUNIT(13%), BUDHOUR(13%), &
			A(20%, 15%), &
			PARM_VALUE$ = 250%, PARM_POINTER%, &
			ACCTYPE$ = 1%, &
			TNLOOP%, &
			BDEOY(2%)

		RIBIT% = RIBIT% + 1% WHILE MID(XPARSE$, RIBIT%, 1%) == " "

		!
		! Handle parentheses
		!
		GOTO 32440 IF MID(XPARSE$, RIBIT%, 1%) <> "("

			RIBIT% = RIBIT% + 1%
			I% = INSTR(RIBIT%, XPARSE$, ")")

			!
			! Special hack to handle (nn) genfin type variables
			!
			IF ((I% - RIBIT%) < 3%) AND (I% <> 0%)
			THEN
				!
				! Make sure it is filled only with digits
				!
				GOTO 32420 UNLESS INSTR(1%, "-0123456789", &
					MID(XPARSE$, L%, 1%)) &
					FOR L% = RIBIT% TO I% - 1%
				!
				! Strip out digits, and get subscript value
				!
				TEMP% = VAL%(SEG$(XPARSE$, RIBIT%, I% - 1%))

				SELECT TEMP%
				!
				! Year to date budget
				!
				CASE -2%
					TEMP = 0.0

					IF INSTR(1%, "RE", ACCTYPE$) = 0%
					THEN
						TEMP = BUDDOL(0%)
					END IF

					TEMP = TEMP + BUDDOL(TEMP%) &
						FOR TEMP% = 1% TO GL_PERIOD::LASTPERCLO
					PARSE_FACTOR = TEMP
				!
				! Month budget
				!
				CASE -1%
					PARSE_FACTOR = BUDDOL(1%)

				!
				! First period in account year
				!
				CASE 1%
					IF INSTR(1%, "R!E", ACCTYPE$) = 0%
					THEN
						PARSE_FACTOR = ACTDOL(TEMP%)
					ELSE
						IF GL_PERIOD::LASTPERCLO = 1%
						THEN
							PARSE_FACTOR = 0.0
						ELSE
							PARSE_FACTOR = ACTDOL(TEMP%)
						END IF
					END IF

				!
				! First period last year
				!
				CASE 13%
					IF INSTR(1%, "R!E", ACCTYPE$) = 0%
					THEN
						PARSE_FACTOR = ACTDOL(TEMP%)
					ELSE
						IF GL_PERIOD::LASTPERCLO = 1%
						THEN
							PARSE_FACTOR = 0.0
						ELSE
							PARSE_FACTOR = ACTDOL(TEMP%)
						END IF
					END IF

				!
				! Actual yearly amount
				!
				CASE ELSE
					PARSE_FACTOR = ACTDOL(TEMP%)

				END SELECT
				RIBIT% = I% + 1%
				EXIT FUNCTION
			END IF

32420			!
			! If it gets to here, then it is ordanary paren.
			!
			TEMP = PARSE_EXPR(XPARSE$, RIBIT%)
			PARSE_FACTOR = TEMP
			RIBIT% = RIBIT% + 1% IF MID(XPARSE$, RIBIT%, 1%) = ")"
			EXIT FUNCTION

32440		!
		! Absolute value function
		!
		IF MID(XPARSE$, RIBIT%, 4%) = "ABS("
		THEN
			RIBIT% = RIBIT% + 4%
			PARSE_FACTOR = ABS(PARSE_EXPR(XPARSE$, RIBIT%))
			RIBIT% = RIBIT% + 1% IF MID(XPARSE$, RIBIT%, 1%) = ")"
			EXIT FUNCTION
		END IF

		!
		! Logical (unary) NOT
		!
		IF MID(XPARSE$, RIBIT%, 3%) = "NOT"
		THEN
			RIBIT% = RIBIT% + 3%
			PARSE_FACTOR = (PARSE_FACTOR(XPARSE$, RIBIT%) = 0%) &
				AND 1%
			EXIT FUNCTION
		END IF

		!
		! Unary minus
		!
		IF MID(XPARSE$, RIBIT%, 1%) = "-"
		THEN
			RIBIT% = RIBIT% + 1%
			PARSE_FACTOR = - PARSE_FACTOR(XPARSE$, RIBIT%)
			EXIT FUNCTION
		END IF

		!
		! Numerical constant
		!
		IF INSTR(1%, "0123456789.", MID(XPARSE$, RIBIT%, 1%))
		THEN
			PARSE_FACTOR = PARSE_NUMBER(XPARSE$, RIBIT%)
			EXIT FUNCTION
		END IF

		!
		! Variable
		!
		PARSE_FACTOR = PARSE_VAR(XPARSE$, RIBIT%)

	END FUNCTION

32500	FUNCTION REAL PARSE_NUMBER(XPARSE$, RIBIT%)
	!**********************************************************************
	! PARSE_NUMBER is the expression parser of the genfin parser.  It handles
	! the <expr> parts of the evaluation.
	!
	! This function will pull a number off of the input string.
	!**********************************************************************

		OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

		!
		! Search for the end of the number.  (NOTE: Negitive numbers
		! are handled through the unary minus operator, so it doesn't
		! need to be handled here.)
		!
		I% = RIBIT%
		I% = I% + 1% &
			WHILE INSTR(1%, " 0123456789.", &
				MID(XPARSE$, I%, 1%)) > 1%

		!
		! Calculate the value of the number
		!
		PARSE_NUMBER = VAL(SEG$(XPARSE$, RIBIT%, I% - 1%))

		RIBIT% = I%

	END FUNCTION

32600	FUNCTION REAL PARSE_VAR(XPARSE$, RIBIT%)
	!**********************************************************************
	! PARSE_VAR is the expression parser of the genfin parser.  It handles
	! the <var> parts of the evaluation.
	!
	! This module handles the variables available to genfin, except the
	! odd (n) type variable which is handled in PARSE_FACTOR.
	!
	!	ADEOY(expr) Actual $ end of year
	!	BDEOY(expr) Budget dollar period to date
	!	AUEOY(expr) Actual unit end of year
	!	AHEOY(expr) Actual hour end of year
	!	ADP(expr) Actual dollar Period to date
	!	ADY(expr) Actual dollar year to date
	!	BDP(expr) Budget dollar period to date
	!	BDY(expr) Budget dollar year to date
	!	AUP(expr) Actual unit Period to date
	!	AUY(expr) Actual unit year to date
	!	BUP(expr) Budget unit period to date
	!	BUY(expr) Budget unit year to date
	!	AHP(expr) Actual hour Period to date
	!	AHY(expr) Actual hour year to date
	!	BHP(expr) Budget hour period to date
	!	BHY(expr) Budget hour year to date
	!	FPFY
	!	Tn(expr)
	!	X
	!
	!**********************************************************************

		OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

		!
		! External functions
		!
		EXTERNAL REAL FUNCTION PARSE_EXPR

		!
		! Map statements
		!
		%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
		MAP (GL_PERIOD)	GL_PERIOD_CDD	GL_PERIOD

		%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
		DECLARE UTL_REPORTX_CDD UTL_REPORTX

		!
		! Common areas
		!
		COMMON (PARSECOM) ACTDOL(20%), ACTUNIT(20%), ACTHOUR(20%), &
			BUDDOL(13%), BUDUNIT(13%), BUDHOUR(13%), &
			A(20%, 15%), &
			PARM_VALUE$ = 250%, PARM_POINTER%, &
			ACCTYPE$ = 1%, &
			TNLOOP%, &
			BDEOY(2%)

		!
		! Functions to ease calculation of start/end of quarters
		!
		DEF FNFIRSTQTRPER%(QTR%)
			!
			! Calculate using 3 periods per qtr, add in offset
			! for number of periods already passed.
			!
			FNFIRSTQTRPER% = &
				QTR% * 3% + 1% + &
				MOD(GL_PERIOD::LASTPERCLO - 1%, 3%)
		FNEND

		!
		! Functions to ease calculation of start/end of quarters
		!
		DEF FNLASTQTRPER%(QTR%)
			!
			! Calculate using 3 periods per qtr, add in offset
			! for number of periods already passed.
			!
			TEMP% = &
				QTR% * 3% - 3% + 1% + &
				MOD(GL_PERIOD::LASTPERCLO - 1%, 3%)
			TEMP% = 0% IF TEMP% < 0%
			FNLASTQTRPER% = TEMP%
		FNEND

		!
		! Past end of string?
		!
		IF RIBIT% > LEN(XPARSE$)
		THEN
			PARSE_VAR = 0.0
		END IF

		SELECT MID(XPARSE$, RIBIT%, 6%)
		!
		! ADEOY(expr) Actual $ end of year
		!
		CASE "ADEOY("
			RIBIT% = RIBIT% + 6%
			TEMP1% = PARSE_EXPR(XPARSE$, RIBIT%)
			TEMP1% = GL_PERIOD::LASTPERCLO + &
				(TEMP1% * GL_PERIOD::FPFY)

			RIBIT% = RIBIT% + 1% &
				WHILE MID(XPARSE$, RIBIT%, 1%) == " "
			RIBIT% = RIBIT% + 1% &
				IF MID(XPARSE$, RIBIT%, 1%) = ")"
			PARSE_VAR = ACTDOL(TEMP1%)
			EXIT FUNCTION

		!
		! BDEOY(expr) Budget dollar period to date
		!
		CASE "BDEOY("
			RIBIT% = RIBIT% + 6%
			TEMP1% = PARSE_EXPR(XPARSE$, RIBIT%)
			RIBIT% = RIBIT% + 1% &
				WHILE MID(XPARSE$, RIBIT%, 1%) == " "
			RIBIT% = RIBIT% + 1% &
				IF MID(XPARSE$, RIBIT%, 1%) = ")"
			PARSE_VAR = BDEOY(TEMP1%)
			EXIT FUNCTION

		!
		! AUEOY(expr) Actual unit end of year
		!
		CASE "AUEOY("
			RIBIT% = RIBIT% + 6%
			TEMP1% = PARSE_EXPR(XPARSE$, RIBIT%)
			TEMP1% = GL_PERIOD::LASTPERCLO + (TEMP1% * GL_PERIOD::FPFY)
			RIBIT% = RIBIT% + 1% &
				WHILE MID(XPARSE$, RIBIT%, 1%) == " "
			RIBIT% = RIBIT% + 1% &
				IF MID(XPARSE$, RIBIT%, 1%) = ")"
			PARSE_VAR = ACTUNIT(TEMP1%)
			EXIT FUNCTION

		!
		! AHEOY(expr) Actual hour end of year
		!
		CASE "AHEOY("
			RIBIT% = RIBIT% + 6%
			TEMP1% = PARSE_EXPR(XPARSE$, RIBIT%)
			TEMP1% = GL_PERIOD::LASTPERCLO + &
				(TEMP1% * GL_PERIOD::FPFY)
			RIBIT% = RIBIT% + 1% &
				WHILE MID(XPARSE$, RIBIT%, 1%) == " "
			RIBIT% = RIBIT% + 1% &
				IF MID(XPARSE$, RIBIT%, 1%) = ")"
			PARSE_VAR = ACTHOUR(TEMP1%)
			EXIT FUNCTION

		END SELECT

		SELECT MID(XPARSE$, RIBIT%, 4%)
		!
		! ADP(expr) Actual dollar Period to date
		!
		CASE "ADP("
			RIBIT% = RIBIT% + 4%
			TEMP1% = PARSE_EXPR(XPARSE$, RIBIT%)
			RIBIT% = RIBIT% + 1% &
				WHILE MID(XPARSE$, RIBIT%, 1%) == " "
			RIBIT% = RIBIT% + 1% IF MID(XPARSE$, RIBIT%, 1%) = ")"
			IF INSTR(1%, "RE", ACCTYPE$) = 0%
			THEN
				PARSE_VAR = ACTDOL(TEMP1%) - ACTDOL(TEMP1% + 1%)
			ELSE
				IF GL_PERIOD::LASTPERCLO - (TEMP1% - ( &
					GL_PERIOD::FPFY * INT(TEMP1% / &
					GL_PERIOD::FPFY))) <> 1%
				THEN
					PARSE_VAR = ACTDOL(TEMP1%) - &
						ACTDOL(TEMP1% + 1%)
				ELSE
					PARSE_VAR = ACTDOL(TEMP1%)
				END IF
			END IF

			EXIT FUNCTION

		!
		! ADQ(expr) Actual dollar Quarter to date
		!
		CASE "ADQ("
			RIBIT% = RIBIT% + 4%
			TEMP1% = PARSE_EXPR(XPARSE$, RIBIT%)
			RIBIT% = RIBIT% + 1% &
				WHILE MID(XPARSE$, RIBIT%, 1%) == " "
			RIBIT% = RIBIT% + 1% IF MID(XPARSE$, RIBIT%, 1%) = ")"

			TEMPL% = FNLASTQTRPER%(TEMP1%)
			TEMPF% = FNFIRSTQTRPER%(TEMP1%)

			IF INSTR(1%, "RE", ACCTYPE$) = 0%
			THEN
				PARSE_VAR = ACTDOL(TEMPL%) - ACTDOL(TEMPF%)
			ELSE
 !				IF MOD(TEMPF%, 12%) <> &
 !					GL_PERIOD::LASTPERCLO - 1%
				IF GL_PERIOD::LASTPERCLO - (TEMP1% * 3% - ( &
					GL_PERIOD::FPFY * INT(TEMP1% * 3% / &
					GL_PERIOD::FPFY))) > 3%
				THEN
					PARSE_VAR = &
						ACTDOL(TEMPL%) - ACTDOL(TEMPF%)
				ELSE
					PARSE_VAR = &
						ACTDOL(TEMPL%)
				END IF
			END IF

			EXIT FUNCTION

		!
		! ADY(expr) Actual dollar year to date
		!
		CASE "ADY("
			RIBIT% = RIBIT% + 4%
			TEMP1% = PARSE_EXPR(XPARSE$, RIBIT%)
			RIBIT% = RIBIT% + 1% &
				WHILE MID(XPARSE$, RIBIT%, 1%) == " "
			RIBIT% = RIBIT% + 1% IF MID(XPARSE$, RIBIT%, 1%) = ")"
			PARSE_VAR = ACTDOL(TEMP1%)
			EXIT FUNCTION

		!
		! BDP(expr) Budget dollar period to date
		!
		CASE "BDP("
			RIBIT% = RIBIT% + 4%
			TEMP1% = PARSE_EXPR(XPARSE$, RIBIT%) + 1%
			RIBIT% = RIBIT% + 1% &
				WHILE MID(XPARSE$, RIBIT%, 1%) == " "
			RIBIT% = RIBIT% + 1% IF MID(XPARSE$, RIBIT%, 1%) = ")"
			PARSE_VAR = BUDDOL(TEMP1%)
			EXIT FUNCTION

		!
		! BDY(expr) Budget dollar year to date
		!
		CASE "BDY("
			RIBIT% = RIBIT% + 4%
			TEMP1% = PARSE_EXPR(XPARSE$, RIBIT%) + 1%
			RIBIT% = RIBIT% + 1% &
				WHILE MID(XPARSE$, RIBIT%, 1%) == " "
			RIBIT% = RIBIT% + 1% IF MID(XPARSE$, RIBIT%, 1%) = ")"
			TEMP = 0.0

			IF INSTR(1%, "RE", ACCTYPE$) = 0%
			THEN
				TEMP = BUDDOL(0%)
			END IF

			TEMP = TEMP + BUDDOL(TEMP%) &
				FOR TEMP% = 1% TO GL_PERIOD::LASTPERCLO
			PARSE_VAR = TEMP
			EXIT FUNCTION

		!
		! AUP(expr) Actual unit Period to date
		!
		CASE "AUP("
			RIBIT% = RIBIT% + 4%
			TEMP1% = PARSE_EXPR(XPARSE$, RIBIT%)
			RIBIT% = RIBIT% + 1% &
				WHILE MID(XPARSE$, RIBIT%, 1%) == " "
			RIBIT% = RIBIT% + 1% IF MID(XPARSE$, RIBIT%, 1%) = ")"
			IF INSTR(1%, "RE", ACCTYPE$) = 0%
			THEN
				PARSE_VAR = ACTUNIT(TEMP1%) - &
					ACTUNIT(TEMP1% + 1%)
			ELSE
				IF GL_PERIOD::LASTPERCLO - (TEMP1% - ( &
					GL_PERIOD::FPFY * INT(TEMP1% / &
					GL_PERIOD::FPFY))) <> 1%
				THEN
					PARSE_VAR = ACTUNIT(TEMP1%) - &
						ACTUNIT(TEMP1% + 1%)
				ELSE
					PARSE_VAR = ACTUNIT(TEMP1%)
				END IF
			END IF

			EXIT FUNCTION

		!
		! AUY(expr) Actual unit year to date
		!
		CASE "AUY("
			RIBIT% = RIBIT% + 4%
			TEMP1% = PARSE_EXPR(XPARSE$, RIBIT%)
			RIBIT% = RIBIT% + 1% &
				WHILE MID(XPARSE$, RIBIT%, 1%) == " "
			RIBIT% = RIBIT% + 1% IF MID(XPARSE$, RIBIT%, 1%) = ")"
			PARSE_VAR = ACTUNIT(TEMP1%)
			EXIT FUNCTION

		!
		! BUP(expr) Budget unit period to date
		!
		CASE "BUP("
			RIBIT% = RIBIT% + 4%
			TEMP1% = PARSE_EXPR(XPARSE$, RIBIT%) + 1%
			RIBIT% = RIBIT% + 1% &
				WHILE MID(XPARSE$, RIBIT%, 1%) == " "
			RIBIT% = RIBIT% + 1% IF MID(XPARSE$, RIBIT%, 1%) = ")"
			PARSE_VAR = BUDUNIT(TEMP1%)
			EXIT FUNCTION

		!
		! BUY(expr) Budget unit year to date
		!
		CASE "BUY("
			RIBIT% = RIBIT% + 4%
			TEMP1% = PARSE_EXPR(XPARSE$, RIBIT%) + 1%
			RIBIT% = RIBIT% + 1% &
				WHILE MID(XPARSE$, RIBIT%, 1%) == " "
			RIBIT% = RIBIT% + 1% IF MID(XPARSE$, RIBIT%, 1%) = ")"
			TEMP = 0.0

			IF INSTR(1%, "RE", ACCTYPE$) = 0%
			THEN
				TEMP = BUDUNIT(0%)
			END IF

			TEMP = TEMP + BUDUNIT(TEMP%) &
				FOR TEMP% = 1% TO GL_PERIOD::LASTPERCLO
			PARSE_VAR = TEMP
			EXIT FUNCTION

		!
		! AHP(expr) Actual hour Period to date
		!
		CASE "AHP("
			RIBIT% = RIBIT% + 4%
			TEMP1% = PARSE_EXPR(XPARSE$, RIBIT%)
			RIBIT% = RIBIT% + 1% &
				WHILE MID(XPARSE$, RIBIT%, 1%) == " "
			RIBIT% = RIBIT% + 1% IF MID(XPARSE$, RIBIT%, 1%) = ")"
			IF INSTR(1%, "RE", ACCTYPE$) = 0%
			THEN
				PARSE_VAR = ACTHOUR(TEMP1%) - &
					ACTHOUR(TEMP1% + 1%)
			ELSE
				IF GL_PERIOD::LASTPERCLO - (TEMP1% - ( &
					GL_PERIOD::FPFY * INT(TEMP1% / &
					GL_PERIOD::FPFY))) <> 1%
				THEN
					PARSE_VAR = ACTHOUR(TEMP1%) - &
						ACTHOUR(TEMP1% + 1%)
				ELSE
					PARSE_VAR = ACTHOUR(TEMP1%)
				END IF
			END IF

			EXIT FUNCTION

		!
		! AHY(expr) Actual hour year to date
		!
		CASE "AHY("
			RIBIT% = RIBIT% + 4%
			TEMP1% = PARSE_EXPR(XPARSE$, RIBIT%)
			RIBIT% = RIBIT% + 1% &
				WHILE MID(XPARSE$, RIBIT%, 1%) == " "
			RIBIT% = RIBIT% + 1% IF MID(XPARSE$, RIBIT%, 1%) = ")"
			PARSE_VAR = ACTHOUR(TEMP1%)
			EXIT FUNCTION

		!
		! BHP(expr) Budget hour period to date
		!
		CASE "BHP("
			RIBIT% = RIBIT% + 4%
			TEMP1% = PARSE_EXPR(XPARSE$, RIBIT%) + 1%
			RIBIT% = RIBIT% + 1% &
				WHILE MID(XPARSE$, RIBIT%, 1%) == " "
			RIBIT% = RIBIT% + 1% IF MID(XPARSE$, RIBIT%, 1%) = ")"
			PARSE_VAR = BUDHOUR(TEMP1%)
			EXIT FUNCTION

		!
		! BHY(expr) Budget hour year to date
		!
		CASE "BHY("
			RIBIT% = RIBIT% + 4%
			TEMP1% = PARSE_EXPR(XPARSE$, RIBIT%) + 1%
			RIBIT% = RIBIT% + 1% &
				WHILE MID(XPARSE$, RIBIT%, 1%) == " "
			RIBIT% = RIBIT% + 1% IF MID(XPARSE$, RIBIT%, 1%) = ")"
			TEMP = 0.0

			IF INSTR(1%, "RE", ACCTYPE$) = 0%
			THEN
				TEMP = BUDHOUR(0%)
			END IF

			TEMP = TEMP + BUDHOUR(TEMP%) &
				FOR TEMP% = 1% TO GL_PERIOD::LASTPERCLO
			PARSE_VAR = TEMP
			EXIT FUNCTION

		!
		! FPFY
		!
		CASE "FPFY"
			RIBIT% = RIBIT% + 4%
			PARSE_VAR = GL_PERIOD::FPFY
			EXIT FUNCTION

		END SELECT

		!
		! Fill in parameter
		!
		IF MID(XPARSE$, RIBIT%, 1%) = "X"
		THEN
			PARSE_VAR = PARSE_EXPR(PARM_VALUE$, PARM_POINTER%)
			PARM_POINTER% = INSTR(PARM_POINTER%, &
				PARM_VALUE$, ",") + 1%
			RIBIT% = RIBIT% + 1%
			EXIT FUNCTION
		END IF

		!
		! Tn(expr)
		!
		IF MID(XPARSE$, RIBIT%, 1%) = "T"
		THEN
			I% = RIBIT% + 1%
			I% = I% + 1% &
				UNTIL INSTR(1%, " 0123456789", &
				MID(XPARSE$, I%, 1%)) <= 1%
			TEMP% = VAL%(SEG$(XPARSE$, RIBIT% + 1%, I% - 1%))
			IF MID(XPARSE$, I%, 1%) = "("
			THEN
				RIBIT% = I% + 1%
				TEMP1% = PARSE_EXPR(XPARSE$, RIBIT%)
				RIBIT% = RIBIT% + 1% &
					WHILE MID(XPARSE$, RIBIT%, 1%) == " "
				RIBIT% = RIBIT% + 1% &
					IF MID(XPARSE$, RIBIT%, 1%) = ")"
				PARSE_VAR = A(TEMP%, TEMP1%)
			ELSE
				RIBIT% = I%
				PARSE_VAR = A(TEMP%, TNLOOP%)
			END IF

			EXIT FUNCTION
		END IF

		!
		! We didn't find anything.  This is a syntax error.
		!
		PARSE_VAR = 0.0

		TEXT$ = "-- Syntax Error -- " + XPARSE$
		CALL OUTP_LINENOTITLE("", UTL_REPORTX, TEXT$, 0%)

		TEXT$ = "   In CMD File     " + SPACE$(RIBIT% - 1%) + "^"
		CALL OUTP_LINENOTITLE("", UTL_REPORTX, TEXT$, 1%)

	END FUNCTION
