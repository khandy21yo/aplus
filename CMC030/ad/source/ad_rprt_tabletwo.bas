1	%TITLE "Two Dimensional Table"
	%SBTTL "AD_RPRT_TABLETWO"
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
	! ID:AD017
	!
	! Abstract:HELP
	!	.B
	!	.LM +5
	!	The ^*Two Dimensional Optional Tables\* option prints
	!	of the Two Dimensional Table. The following fields are contained in this
	!	table report:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	Table
	!	.le
	!	Effective Date
	!	.le
	!	Recovery Period
	!	.le
	!	Year
	!	.le
	!	Numbered Periods
	!	.els
	!
	! Index:
	!	.x Two Dimensional Optional Table>Report
	!	.x Report>Two Dimensional Optional Table
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_RPRT_TABLETWO/LINE
	!	$ LINK/EXE=AD_EXE: AD_RPRT_TABLETWO, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AD_RPRT_TABLETWO.OBJ;*
	!
	! AUTHOR:
	!
	!	12/06/87 - Frank F. Starman
	!
	! MODIFICATION HISTORY:
	!
	!		Changed I & J to I% & J% in FOR-NEXT loops.  Removed variable
	!		TOTAL.LINE which is not used.
	!
	!	03/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/28/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/20/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/23/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[AD.OPEN]AD_TABLETWO.HB"
	MAP (AD_TABLETWO) AD_TABLETWO_CDD AD_TABLETWO

	DECLARE STRING TEXT, FROM_ITEM, TO_ITEM, WLDCRD, TEST_TABLE, &
		TEST_DATE, TEST_YEARS

	DECLARE WORD CONSTANT PRINT_WIDTH = 132%, MAX_YEARS = 100%, &
		MAX_MONTH = 11%

	DIMENSION	REAL	LAST_PERC(MAX_MONTH)
	DIMENSION	REAL	TOTAL_PERC(MAX_MONTH)
	DIMENSION	REAL	PERCENTAGE(MAX_YEARS, MAX_MONTH)
	DIMENSION	WORD	TABLE_YEAR(MAX_YEARS)

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
	!	A blank setting causes the report to begin with the first
	!	table in the file.
	!	.LM -5
	!
	! Index:
	!	.x From Table>Two Dimension Tables
	!	.x Two Dimension Tables>From Table
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
	!	A blank setting causes the report to end with the last
	!	table in the file.
	!	.LM -5
	!
	! Index:
	!	.x To Table>Two Dimension Tables
	!	.x Two Dimension Tables>To Table
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
	!	.x Wildcard>Two Dimensional Table
	!	.x Two Dimensional Table>Wildcard
	!
	!--


300	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_TABLETWO.OPN"
	USE
		FILENAME$ = "AD_TABLETWO"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "TWO  DIMENSIONAL  OPTIONAL  TABLE"
	TITLE$(2%) = "Asset Depreciation System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	!		1234567890123456789012345678901234567890
	TITLE$(4%) = "Table  EffDate    RecPer Yr" + &
		"    01     02     03     04     05     06" + &
		"     07     08     09     10     11     12"

	TITLE$(5%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM = ""
		THEN
			RESET #AD_TABLETWO.CH%
		ELSE
			FIND #AD_TABLETWO.CH%, KEY #0% GE FROM_ITEM, REGARDLESS
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
		GET #AD_TABLETWO.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "AD_TABLETWO"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO ExitTotal IF (AD_TABLETWO::OPTTABLE > TO_ITEM) AND TO_ITEM <> ""

	GOTO GetNextRec &
		IF COMP_STRING(EDIT$(AD_TABLETWO::OPTTABLE, -1%), WLDCRD) = 0% &
		AND WLDCRD <> ""

17300	!
	! Print out one line
	!
	IF (AD_TABLETWO::OPTTABLE <> TEST_TABLE OR &
		AD_TABLETWO::EFFDATE <> TEST_DATE OR &
		AD_TABLETWO::YEARS <> TEST_YEARS) AND TEST_TABLE <> ""
	THEN
		GOSUB 18000
	END IF

	LINE_LOOP% = VAL(AD_TABLETWO::DEP_YEAR)
	TABLE_YEAR(LINE_LOOP%) = -1%

	FOR I% = 0% TO 11%
		PERCENTAGE(LINE_LOOP%, I%) = &
			FUNC_ROUND(AD_TABLETWO::PERCENTAGE(I%) * 0.01, 2%)
	NEXT I%

17350	!
	! Try for next record
	!
	TEST_TABLE = AD_TABLETWO::OPTTABLE
	TEST_DATE = AD_TABLETWO::EFFDATE
	TEST_YEARS = AD_TABLETWO::YEARS

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

18000	RETURN IF LINE_LOOP% = 0%

18010	TEXT = TEST_TABLE + " " + &
		PRNT_DATE(TEST_DATE, 8%) + " " + &
		TEST_YEARS + "   "

	I% = 1%
	LAST_PERC(J%) = 0. FOR J% = 0% TO 11%

	WHILE I% <= LINE_LOOP% OR I% <= VAL(TEST_YEARS)
		TEXT = TEXT + FORMAT$(I%, "<0># ")
		FOR J% = 0% TO 11%
			IF TABLE_YEAR(I%) = 0%
			THEN
				PERCENTAGE(I%, J%) = LAST_PERC(J%)
			END IF

			TEXT= TEXT + FORMAT$(PERCENTAGE(I%, J%), "##.##  ")
			LAST_PERC(J%) = PERCENTAGE(I%, J%)
			TOTAL_PERC(J%) = TOTAL_PERC(J%) + PERCENTAGE(I%, J%)
		NEXT J%

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT, 0%)
		TEXT = TEST_TABLE + SPACE$(19%)
		I% = I% + 1%
	NEXT

	TABLE_YEAR(J%) = 0% FOR J% = 1% TO I%
	LINE_LOOP% = 0%

	WARNING% = 0%
	TEXT = STRING$(26%, A"-"B)

	FOR J% = 0% TO 11%
		IF FUNC_ROUND(TOTAL_PERC(J%), 2%) = 100.00
		THEN
			TEXT = TEXT + "-------"
		ELSE
			TEXT = TEXT + FORMAT$(TOTAL_PERC(J%), "####.##")
			WARNING% = -1%
		END IF
	NEXT J%

	IF WARNING%
	THEN
		TEXT = TEXT + "_Check_Table_Above_"
	ELSE
		TEXT = TEXT + "-------------------"
	END IF

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT, -1%)
	TOTAL_PERC(J%) = 0% FOR J% = 0% TO 11%

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
