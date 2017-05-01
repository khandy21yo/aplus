1	%TITLE "One Dimensional Ceiling Table"
	%SBTTL "AD_RPRT_CEILINGONE"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1986 BY
	!
	! Computer Management Center
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
	! ID:AD026
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*One Dimensional Ceiling Tables\* option prints the One
	!	Dimensional Ceiling Table. Contained in this report are the following fields:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	Table
	!	.le
	!	Effective Date
	!	.le
	!	Numbered Periods
	!	.els
	!
	! Index:
	!	.x One Dim Ceiling Tables>Report
	!	.x Report>One Dim Ceiling Tables
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_RPRT_CEILINGONE/LINE
	!	$ LINK/EXE=AD_EXE: AD_RPRT_CEILINGONE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AD_RPRT_CEILINGONE.OBJ;*
	!
	! AUTHOR:
	!
	!	09/19/88 - Frank Starman
	!
	! MODIFICATION HISTORY:
	!
	!	03/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/10/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!
	!	01/29/96 - Kevin Handy
	!		Reformat source code.
	!		Change STRING$(...,ASCII(" ")) to SPACE$(...) in
	!		several places.
	!
	!	05/12/97 - Kevin Handy
	!		Reformat source code.
	!		Make MAX_YEARS an integer.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/15/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	10/27/2000 - Kevin Handy
	!		Use A"x"B
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	%INCLUDE "SOURCE:[AD.OPEN]AD_CEILINGONE.HB"
	MAP (AD_CEILINGONE) AD_CEILINGONE_CDD AD_CEILINGONE

	!
	! External functions
	!
	DECLARE LONG LINE_LOOP
	DECLARE STRING TEXT, FROM_ITEM, TO_ITEM, WLDCRD, TEST_TABLE, TEST_DATE
	DECLARE WORD CONSTANT PRINT_WIDTH = 132%, MAX_YEARS = 100%

	DIMENSION REAL CEILING(MAX_YEARS)
	DIMENSION LONG CEIL_YEAR(MAX_YEARS)

	%PAGE

	ON ERROR GOTO 19000

	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, PRINT_WIDTH)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Table\*
	!	.B
	!	.LM +5
	!	The ^*From Table\* field causes the
	!	report to begin with the selected table.
	!	.B
	!	A blank setting will cause the report to begin with the first
	!	table in the file.
	!	.LM -5
	!
	! Index:
	!	.x From Table>One Dimensional Ceiling Table
	!	.x one Dimensional Ceiling Table>From Table
	!
	!--

	TO_ITEM = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Table\*
	!	.B
	!	.LM +5
	!	The ^*To Table\* field causes the
	!	report to end with the selected table.
	!	.B
	!	A blank setting will cause the report to end with the last
	!	table in the file.
	!	.LM -5
	!
	! Index:
	!	.x One Dimensional Ceiling Table>To Table
	!	.x To Table>One Dimensional Ceiling Table
	!
	!--

	WLDCRD = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.B
	!	.LM +5
	!	The ^*Wildcard\* field prints only selected
	!	tables by entering a "wildcard" value in this field.
	!	.LM -5
	!
	! Index:
	!	.x Wildcard>One Dimensional Ceiling Table
	!	.x One Dimensional Ceiling Table>Wildcard
	!
	!--


300	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_CEILINGONE.OPN"
	USE
		FILENAME$ = "AD_CEILINGONE.OPN"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "ONE  DIMENSIONAL  CEILING  TABLE"
	TITLE$(2%) = "Asset Depreciation System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	!		1234567890123456789012345678901234567890
	TITLE$(4%) = "Table  EffDate   " + &
		"        01        02        03        04       05" + &
		"        06        07        08        09       10" + &
		"        11"

	TITLE$(5%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM = ""
		THEN
			RESET #AD_CEILINGONE.CH%
		ELSE
			FIND #AD_CEILINGONE.CH%, &
				KEY #0% GE FROM_ITEM, &
				REGARDLESS
		END IF
	USE
		CALL ENTR_3MESSAGE(SCOPE, &
			"Unable to find beginning record!", 0%)
		CONTINUE ExitProgram
	END WHEN

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #AD_CEILINGONE.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "AD_CEILINGONE"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO ExitTotal IF (AD_CEILINGONE::OPTTABLE > TO_ITEM) AND TO_ITEM <> ""

	GOTO GetNextRec &
		IF COMP_STRING(EDIT$(AD_CEILINGONE::OPTTABLE, -1%), &
		WLDCRD) = 0% AND WLDCRD <> ""

17300	!
	! Print out one line
	!
	IF AD_CEILINGONE::OPTTABLE + AD_CEILINGONE::EFFDATE <> &
		TEST_TABLE + TEST_DATE AND TEST_TABLE <> ""
	THEN
		GOSUB 18000
		TEXT = SPACE$(18%) + STRING$(132% - 18%, A"-"B)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT, 0%)
	END IF

	LINE_LOOP = VAL(AD_CEILINGONE::DEP_YEAR)
	CEIL_YEAR(LINE_LOOP) = -1%
	CEILING(LINE_LOOP) = AD_CEILINGONE::CEILING

17350	!
	! Try for next record
	!
	TEST_TABLE = AD_CEILINGONE::OPTTABLE
	TEST_DATE = AD_CEILINGONE::EFFDATE

	GOTO GetNextRec

 ExitTotal:
17400	!
	! Handle end of report
	!
	GOSUB 18000

 ExitProgram:
	CALL OUTP_FINISH(UTL_REPORTX)

	!
	! Exit to next program or menu
	!
	IF TRM$(UTL_REPORTX::NEXTRUN) = ""
	THEN
		CALL SUBR_3EXITPROGRAM(SCOPE, "", "")
	ELSE
		CALL SUBR_3EXITPROGRAM(SCOPE, "RUN " + UTL_REPORTX::NEXTRUN, "")
	END IF

18000	RETURN IF LINE_LOOP = 0%

18010	TEXT = TEST_TABLE + " " + &
		PRNT_DATE(TEST_DATE, 8%) + " "
	I = 1%
	LAST_PERC = 0.0
	FOR I = 1% TO LINE_LOOP
		IF CEIL_YEAR(I) = 0%
		THEN
			CEILING(I) = LAST_PERC
		END IF

		TEXT = TEXT + FORMAT$(CEILING(I), "##,###.## ")
		LAST_PERC = CEILING(I)
	NEXT I
	TEXT = TEXT + "( same each year thereafter)"
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), LEFT(TEXT, 126%), 0%)

	WHILE LEN(TEXT) > 126%
		TEXT = RIGHT(TEXT, 127%)
		TEXT = TEST_TABLE + SPACE$(18%) + TEXT
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), LEFT(TEXT, 126%), 0%)
	NEXT

	CEIL_YEAR(I) = 0% FOR I = 1% TO LINE_LOOP
	LINE_LOOP = 0%
	RETURN

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	UTL_REPORTX::STAT = -1%
	GOTO ExitProgram

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
