1	%TITLE "Budget Report by Period"
	%SBTTL "GL_RPRT_BDGT01"
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
	! ID:BDGT01
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	Accessing the ^*Print Budget Report by Period\* option produces a report
	!	which includes account number, account description, the amount budgeted for
	!	each account for a specified accounting period, and the amount budgeted
	!	year-to-date for each account.
	!	.b
	!	The following fields are included:
	!	.table 30
	!	.te
	!	Account
	!	.te
	!	Description
	!	.te
	!	Budget Period
	!	.te
	!	To Date
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Budget by Period
	!	.x Print>Budget by Period
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_RPRT_BDGT01/LINE
	!	$ LINK/EXECUTABLE=GL_EXE: GL_RPRT_BDGT01, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_RPRT_BDGT01.OBJ;*
	!
	! Author:
	!
	!	11/24/86 - Kevin Handy
	!
	! Modification history:
	!
	!	01/10/91 - Craig Tanner
	!		Where FILENAME$ = "GL_BUD_YYYY", changed to =
	!		"GL_BUD_" + GL_BUDGET.YEAR$
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	12/15/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!		Change RIGHT(NUM1$()) to FORMAT$().
	!
	!	05/15/97 - Kevin Handy
	!		Reformat source code
	!		Use integer for #key
	!		Reverse test or "R" and "E" in budget determination.
	!
	!	08/20/97 - Kevin Handy
	!		Don't need to allocate channel for report
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/27/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	06/02/2005 - Kevin Handy
	!		Lose goofy addition of GL YTD in the TO_DATE field.
	!		It made for strange (~doubled) amounts.
	!
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
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 80%)
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
	!	If the report is to start with the first account in the file, the field
	!	should be left blank.
	!	.lm -5
	!
	! Index:
	!
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

	GL_BUDGET.YEAR$ = LEFT(EDIT$(UTL_REPORTX::OPTDEF(2%), 132%), 4%)

	!++
	! Abstract:FLD03
	!	^*(03) Year/Period\*
	!	.b
	!	.lm +5
	!	The ^*Year/Period\* setting determines the
	!	specific budget period for which information will be printed,
	!	as well as the year-to-date amounts budgeted at the end of
	!	the period specified.
	!	.b
	!	The format for entry is YYYYPP.
	!	.lm -5
	!
	! Index:
	!
	!--

	BUDGET_PER$ = EDIT$(MID(UTL_REPORTX::OPTDEF(2%), 5%, 2%), 132%)
	BUDGET_PER% = VAL%(BUDGET_PER$)
	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field enables the user to print a
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
	! Get some data from the control record
	!
	LASTPERCLO% = GL_PERIOD::LASTPERCLO
	YEAR$ = GL_PERIOD::YEAR
	FPFY% = GL_PERIOD::FPFY

	TO_DATE_PERIOD% = -1%

	LOOP% = 0%

	WHILE LOOP% <= 20% AND TO_DATE_PERIOD% = -1%
		IF YEAR$ = GL_BUDGET.YEAR$ AND BUDGET_PER% = LASTPERCLO%
		THEN
			TO_DATE_PERIOD% = LOOP%
		END IF

		LASTPERCLO% = LASTPERCLO% - 1%

		IF LASTPERCLO% < 1%
		THEN
			LASTPERCLO% = FPFY%
			YEAR$ = FORMAT$(VAL%(YEAR$) - 1%, "<0>####")
		END IF

		LOOP% = LOOP% + 1%
	NEXT

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
	TITLE$(1%) = "Budget Report"
	TITLE$(2%) = "For the budget period " + BUDGET_PER$ + "_" + &
		GL_BUDGET.YEAR$
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "Account              Description               " + &
		"         Bud per " + BUDGET_PER$ + "       To-Date"
	TITLE$(5%) = ""

	!
	! Layouts for printed lines
	!
	LYT_LINE$ = "$Account:018,$Descr:050,VDollar:063,VAmount:078"

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
		CONTINUE ExitTotal IF ERR = 11%
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
	GOTO ExitTotal IF (GL_BUD_YYYY::ACCT > TO_ITEM$) AND (TO_ITEM$ <> "")

	GOTO GetNextRec &
		IF COMP_STRING(EDIT$(GL_BUD_YYYY::ACCT, -1%), WLDCRD$) = 0% &
		AND WLDCRD$ <> ""

	!
	! Get information out of Chart of Accounts
	!
	GOSUB GetAccount

	!
	! Print out one line
	!
	TO_DATE_AMOUNT = TO_DATE_AMOUNT + GL_BUD_YYYY::DOLLAR(LOOP%) &
		FOR LOOP% = 1% TO BUDGET_PER%

	TEXT$ = GL_BUD_YYYY::ACCT + "  " + &
		LEFT(GL_CHART::DESCR, 30%) + "  " + &
		FORMAT$(GL_BUD_YYYY::DOLLAR(BUDGET_PER%), "###,###,###") + &
		FORMAT$(TO_DATE_AMOUNT, " ##,###,###,###")

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	BDGT.PER.TOT = BDGT.PER.TOT + GL_BUD_YYYY::DOLLAR(BUDGET_PER%)
	TO_DATE_TOT  = TO_DATE_TOT + TO_DATE_AMOUNT

	!
	! Try for next record
	!
	GOTO GetNextRec

	%PAGE

17900	!******************************************************************
	! Handle totals and other items before EXITing
	!******************************************************************

 ExitTotal:
	!
	! Print out totals
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	TEXT$ = SPACE$(30%) + &
		"TOTALS" + &
		SPACE$(16%) + &
		FORMAT$(BDGT.PER.TOT, "###,###,### ") + &
		FORMAT$(TO_DATE_TOT, "##,###,###,###")

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

 GetAccount:
18000	!******************************************************************
	! Subroutine to get information for real accounts from GL_CHART
	!******************************************************************

	TO_DATE_AMOUNT = 0.0

	WHEN ERROR IN
		GET #GL_CHART.CH%, KEY #0% EQ GL_BUD_YYYY::ACCT, REGARDLESS
	USE
		GL_CHART::DESCR = STRING$(LEN(GL_CHART::DESCR), 63%)
		GL_CHART::DOLLAR(I%) = 0.0 &
			FOR I% = 0% TO 20%
		CONTINUE ComeBack
	END WHEN

	!
	! Get beginning balance for real accounts from chart of accounts
	!
 !	IF ((GL_CHART::ACCTYPE = "R") OR (GL_CHART::ACCTYPE = "E")) AND &
 !		TO_DATE_PERIOD% >= 0%
 !	THEN
 !		TO_DATE_AMOUNT = GL_CHART::DOLLAR(TO_DATE_PERIOD%)
 !	END IF

 ComeBack:
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
	! Handle untrapped errors
	!
	FILENAME$ = ""
	RESUME HelpError

	!******************************************************************
	! End of report GL_RPRT_BDGT01
	!******************************************************************
	END
