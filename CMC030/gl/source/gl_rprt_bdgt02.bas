1	%TITLE "Budget History Report"
	%SBTTL "GL_RPRT_BDGT02"
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
	! ID:BDGT02
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	Executing the ^*Print Budget History Report\* option prints the Chart of
	!	Accounts, including dollar, unit, and hour amounts budgeted for each account
	!	for each accounting period, as well as annual budget totals for each account.
	!	.b
	!	The following fields are included:
	!	.table 30
	!	.te
	!	Account
	!	.te
	!	Description
	!	.te
	!	Budget Dollars
	!	.te
	!	Budget Units
	!	.te
	!	Budget Hours
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Budget History
	!	.x Budget History>Print
	!	.x Print>Budget History
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_RPRT_BDGT02/LINE
	!	$ LINK/EXECUTABLE=GL_EXE: GL_RPRT_BDGT02, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_RPRT_BDGT02.OBJ;*
	!
	! Author:
	!
	!	11/26/86 - Kevin Handy
	!
	! Modification history:
	!
	!	01/10/91 - Craig Tanner
	!		Where FILENAME$ = "GL_BUD_YYYY", changed to =
	!		"GL_BUD_" + GL_BUDGET.YEAR$
	!
	!	03/29/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/04/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/23/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/20/97 - Kevin Handy
	!		Don't need to allocate channel for report
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/27/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!******************************************************************
	! External modules needed
	!******************************************************************

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!******************************************************************
	! Set up data storage areas (MAPs, DIMENSIONs, DECLAREs)
	!******************************************************************

	!
	! CDD inclusions
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD	UTL_REPORTX

	%INCLUDE "SOURCE:[GL.OPEN]GL_BUD_YYYY.HB"
	MAP	(GL_BUD_YYYY)	GL_BUD_YYYY_CDD	GL_BUD_YYYY

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP	(GL_CHART)	GL_CHART_CDD	GL_CHART

	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	MAP	(GL_PERIOD)	GL_PERIOD_CDD	GL_PERIOD

	!
	! Dimension statements
	!
	DIM	REAL	TOT_DOLLAR(13%), TOT_UNIT(13%), TOT_HOUR(13%)

	%PAGE

	!******************************************************************
	! Take care of anything else before starting the report
	!******************************************************************

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

 Initialization:
	!******************************************************************
	! Get ready to begin
	!******************************************************************

	!
	! Initialize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Account\*
	!	.b
	!	.lm +5
	!	The ^*From Account\* field causes
	!	printing to begin with a specified
	!	account number.
	!	.b
	!	If the report is to start with the first account number in the
	!	file, this field should be left blank.
	!	.lm -5
	!
	! Index:
	!
	! Required:
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Account\*
	!	.b
	!	.lm +5
	!	The ^*To Account\* field causes
	!	printing to end with a specified
	!	account number.
	!	.b
	!	If the report is to end with the last account in the file, this
	!	field should be left blank.
	!	.lm -5
	!
	! Index:
	!
	!--

	GL_BUDGET.YEAR$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) Year\*
	!	.b
	!	.lm +5
	!	The ^*Year\* setting causes the report to print
	!	a particular fiscal year.
	!	.b
	!	The format for entry is YYYY.
	!	.lm -5
	!
	! Index:
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* setting enables the user to print a
	!	report including selected accounts only, using the wildcard
	!	technique.
	!	.b
	!	Example: If the Chart of Accounts format were 99999-99 and
	!	the two rightmost numbers represented a department number and a
	!	report were to be printed to include department "02" only, the
	!	Wildcard setting of "?????-02" would cause the report to print
	!	all accounts with the suffix 02.
	!	.lm -5
	!
	! Index:
	!
	!--

	TOT_DOLLAR(I%), TOT_UNIT(I%), TOT_HOUR(I%) = 0.0 &
		FOR I% = 1% TO 13%

	%PAGE

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.OPN"
		GET #GL_PERIOD.CH%, RECORD 1%, REGARDLESS
		CLOSE GL_PERIOD.CH%
	USE
		FILENAME$ = "GL_PERIOD"
		CONTINUE HelpError
	END WHEN

	!
	! Open Chart of Accounts file
	!
310	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.OPN"
	USE
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

	!
	! Open Budget file
	!
320	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_BUD_YYYY.OPN"
	USE
		FILENAME$ = "GL_BUD_" + GL_BUDGET.YEAR$
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
	!
	! Titles
	!
	TITLE$(1%) = "Budget History"
	TITLE$(2%) = ""
	TITLE$(3%) = ""

	!
	! Headings
	!
	TITLE$(4%) = "Account            Description              " + &
		"               Period                  Dollars      " + &
		"    Units          Hours"
	TITLE$(5%) = ""

	!
	! Layouts for printed lines
	!
	LYT_LINE$ = "$Account:018,$Filler:058,$Period:075,VDollar:090," + &
		"VUnit:105,VHour:120"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #GL_BUD_YYYY.CH%
		ELSE
			FIND #GL_BUD_YYYY.CH%, KEY #0% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17100	!******************************************************************
	! Main report loop starts here
	!******************************************************************

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #GL_BUD_YYYY.CH%, REGARDLESS
	USE
		CONTINUE 17900 IF ERR = 11%
		FILENAME$ = "GL_BUD_" + GL_BUDGET.YEAR$
		CONTINUE HelpError
	END WHEN

	!
	! Check status
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Check current record
	!
	GOTO 17900 IF (GL_BUD_YYYY::ACCT > TO_ITEM$) AND (TO_ITEM$ <> "")

	GOTO GetNextRec &
		IF COMP_STRING(EDIT$(GL_BUD_YYYY::ACCT, -1%), WLDCRD$) = 0% &
		AND WLDCRD$ <> ""

17200	!
	! Get information out of chart of accounts
	!
	WHEN ERROR IN
		GET #GL_CHART.CH%, KEY #0% EQ GL_BUD_YYYY::ACCT, REGARDLESS
	USE
		GL_CHART::DESCR = STRING$(LEN(GL_CHART::DESCR), 63%)
		GL_CHART::DOLLAR(I%) = 0.0 &
			FOR I% = 0% TO 20%
		CONTINUE 17300
	END WHEN

17300	END_BAL_DOLLAR, END_BAL_UNIT, END_BAL_HOUR = 0.0
	END_BAL_DOLLAR = END_BAL_DOLLAR + GL_BUD_YYYY::DOLLAR(I%) &
		FOR I% = 1% TO GL_PERIOD::FPFY

	END_BAL_UNIT = END_BAL_UNIT + GL_BUD_YYYY::UNIT(I%) &
		FOR I% = 1% TO GL_PERIOD::FPFY

	END_BAL_HOUR = END_BAL_HOUR + GL_BUD_YYYY::HOUR(I%) &
		FOR I% = 1% TO GL_PERIOD::FPFY

	!
	! Print out one line
	!
	FOR I% = 1% TO GL_PERIOD::FPFY

		TEXT$ = SPACE$(58%)

		TEXT$ = GL_CHART::ACCT + " " + LEFT(GL_CHART::DESCR, 39%) &
			IF I% = 1%

		TEXT$ = TEXT$ + " " + &
			LEFT(GL_PERIOD::PERIOD(I%) + SPACE$(16%), 16%) + " " + &
			FORMAT$(GL_BUD_YYYY::DOLLAR(I%), "###,###,###.## ") + &
			FORMAT$(GL_BUD_YYYY::UNIT(I%),   "###,###,###.## ") + &
			FORMAT$(GL_BUD_YYYY::HOUR(I%),   "###,###,###.## ")

		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		TOT_DOLLAR(I%) = TOT_DOLLAR(I%) + GL_BUD_YYYY::DOLLAR(I%)
		TOT_UNIT(I%) = TOT_UNIT(I%) + GL_BUD_YYYY::UNIT(I%)
		TOT_HOUR(I%) = TOT_HOUR(I%) + GL_BUD_YYYY::HOUR(I%)

	NEXT I%

	TEXT$ = SPACE$(19%) + "TOTAL" + SPACE$(52%) + &
		FORMAT$(END_BAL_DOLLAR, "###,###,###.## ") + &
		FORMAT$(END_BAL_UNIT,   "###,###,###.## ") + &
		FORMAT$(END_BAL_HOUR,   "###,###,###.## ")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Try for next record
	!
	GOTO GetNextRec

	%PAGE

17900	!******************************************************************
	! Handle end of report
	!******************************************************************

 ExitTotal:
	!
	! Print out totals
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 2000%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	END_BAL_DOLLAR, END_BAL_UNIT, END_BAL_HOUR = 0.0
	END_BAL_DOLLAR = END_BAL_DOLLAR + TOT_DOLLAR(I%) &
		FOR I% = 1% TO GL_PERIOD::FPFY

	END_BAL_UNIT = END_BAL_UNIT + TOT_UNIT(I%) &
		FOR I% = 1% TO GL_PERIOD::FPFY

	END_BAL_HOUR = END_BAL_HOUR + TOT_HOUR(I%) &
		FOR I% = 1% TO GL_PERIOD::FPFY

	FOR I% = 1% TO GL_PERIOD::FPFY

		TEXT$ = "TOTALS" + SPACE$(52%) IF I% = 1%

		TEXT$ = TEXT$ + " " + &
			LEFT(GL_PERIOD::PERIOD(I%) + SPACE$(16%), 16%) + " " + &
			FORMAT$(TOT_DOLLAR(I%), "###,###,###.## ") + &
			FORMAT$(TOT_UNIT(I%),   "###,###,###.## ") + &
			FORMAT$(TOT_HOUR(I%),   "###,###,###.## ")
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		TEXT$ = SPACE$(58%)

	NEXT I%

	TEXT$ = SPACE$(19%) + "TOTAL" + SPACE$(52%) + &
		FORMAT$(END_BAL_DOLLAR, "###,###,###.## ") + &
		FORMAT$(END_BAL_UNIT,   "###,###,###.## ") + &
		FORMAT$(END_BAL_HOUR,   "###,###,###.## ")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

 ExitProgram:
	!
	! Finish up the report
	!
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
	! Handle untrapped errors
	!
	FILENAME$ = ""
	RESUME HelpError

32767	!******************************************************************
	! End of report GL_RPRT_BDGT02
	!******************************************************************
	END
