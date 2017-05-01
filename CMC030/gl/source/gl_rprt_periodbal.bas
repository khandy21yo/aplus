1	%TITLE "TRLB - Print the Trial Balance"
	%SBTTL "GL_RPRT_PERIODBAL"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1999 BY
	!
	! Software Solutions, Inc.
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
	! Software Solutions, Inc.
	!
	! Software Solutions, Inc. assumes no responsibility for the use
	! or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	! ID:GLTRLB
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Trial Balance\* report contains a list of the accounts
	!	in the General Ledger showing debit or credit balances and
	!	verification that debits equal credits. A Trial Balance report
	!	may be printed at anytime.
	!	.b
	!	The report includes columns for the following information:
	!	.table 30
	!	.te
	!	Account _#
	!	.te
	!	Account Description
	!	.te
	!	Beginning Balance
	!	.te
	!	Change
	!	.te
	!	Ending Balance
	!	.end table
	!	.LM -5
	!
	! Index:
	!	.x Trial Balance>Print
	!	.x Print>Trial Balance
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_RPRT_PERIODBAL/LINE
	!	$ LINK/EXECUTABLE=GL_EXE: GL_RPRT_PERIODBAL, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_RPRT_PERIODBAL.OBJ;*
	!
	! Author:
	!
	!	03/15/99 - Kevin Handy
	!		Based on GL_RPRT_PERIODBAL
	!
	! Modification history:
	!
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"
	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD	UTL_REPORTX

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP	(GL_CHART)	GL_CHART_CDD	GL_CHART

	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	MAP	(GL_PERIOD)	GL_PERIOD_CDD	GL_PERIOD

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP	(GL_YYYY_PP)	GL_YYYY_PP_CDD	GL_YYYY_PP

	!
	! Dimension arrays
	!
	DIM STRING UNDEFINED_ACCT(1000%)

	!
	! Declare some variables
	!
	DECLARE RFA GL_CHART_RFA

	%PAGE

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
	!	^*(01) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* setting causes
	!	printing to begin with a specified
	!	item.
	!	.b
	!	If the report is to start with the first item in the file, this
	!	field should be left blank.
	!	.lm -5
	!
	! Index:
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* setting causes
	!	printing to end with a specified
	!	item.
	!	.b
	!	If the report is to end with the last item in the file, this field
	!	should be left blank.
	!	.lm -5
	!
	! Index:
	!
	!--

	ZERO_BALANCE$ = LEFT(EDIT$(UTL_REPORTX::OPTDEF(2%), -1%), 1%)

	!++
	! Abstract:FLD03
	!	^*(03) Zero Balances?\*
	!	.b
	!	.lm +5
	!	The ^*Zero Balances\* setting allows the user to print the
	!	Trial Balance and include all line items which may or may not
	!	have a balance.
	!	.b
	!	A ^*Y (Yes)\* response will cause the report to print all line items,
	!	including zero balances, and an ^*N (No)\* response will cause the report
	!	to eliminate all line items with zero balances.
	!	.lm -5
	!
	! Index:
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* setting enables the user to print a
	!	report including selected accounts only, using the wildcarding
	!	technique.
	!	.b
	!	Example: If the Chart of Accounts format were 99999-99 and
	!	the two rightmost numbers represented a department number and a
	!	report were to be printed to include department "02" only, the
	!	Wildcard setting of ^*?????-02\* would cause the report to print
	!	all accounts with the suffix 02.
	!	.lm -5
	!
	! Index:
	!
	!--

	EXTRA_TITLE$ = EDIT$(UTL_REPORTX::OPTDEF(4%), 132%)

	!++
	! Abstract:FLD05
	!	^*(05) Supplemental Title\*
	!	.b
	!	.lm +5
	!	The ^*Supplemental Title\* field is used to add an additional
	!	title to the General Ledger report.
	!	.b
	!	Example: If only one department or location was
	!	being printed, that department or location name
	!	could be entered into the supplemental title field.
	!	.b
	!	This field may be left blank.
	!	.lm -5
	!
	! Index:
	!	.x Supplemental Title
	!	.x Report Settings>Supplemental Title
	!
	!--

	PERIOD$ = EDIT$(UTL_REPORTX::OPTDEF(5%), 132%)

	!++
	! Abstract:FLD06
	!	^*(06) GL Period\*
	!	.b
	!	.lm +5
	!	.lm -5
	!
	! Index:
	!	.x Period
	!
	!--

	GL_EMPTY_FLAG% = 0%

	%PAGE

300	YYYY_PP$ = LEFT(PERIOD$, 4%) + "_" + RIGHT(PERIOD$, 5%)

	!
	! Open GL Chart of Accounts file
	!
310	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.OPN"
	USE
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

	!
	! Open the current GL Period file
	!
320	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.OPN"
	USE
		FILENAME$ = "GL_" + YYYY_PP$
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
	!
	! Titles
	!
	IF EXTRA_TITLE$ <> ""
	THEN
		TITLE% = 1%
		TITLE$(1%) = EXTRA_TITLE$
	ELSE
		TITLE% = 0%
	END IF

	TITLE$(TITLE% + 1%) = "Period Balance"
	TITLE$(TITLE% + 2%) = "For the Period " + PERIOD$
	TITLE$(TITLE% + 3%) = ""

	!
	! Headers
	!
	TITLE$(TITLE% + 4%) = "Acct #              Description      " + &
		"                     Change"
	TITLE$(TITLE% + 5%) = ""

	!
	! Layouts for the lines that are printed
	!
	LYT_LINE1$ = "$ACCOUNT:020,$DESCRIP:050,BALANCE:067"

	%PAGE

	SUMMARY_FLAG% = 0%

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #GL_CHART.CH%
		ELSE
			FIND #GL_CHART.CH%, KEY #0% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

	!
	! Find First record in the general ledger
	!
17050	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #GL_YYYY_PP.CH%
		ELSE
			FIND #GL_YYYY_PP.CH%, KEY #0% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
		GL_EMPTY_FLAG% = -1%
	END WHEN

	WHEN ERROR IN
		GET #GL_YYYY_PP.CH%, REGARDLESS
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF
		GL_EMPTY_FLAG% = -1%
	END WHEN

17100	!******************************************************************
	! Main report loop starts here
	!******************************************************************

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #GL_CHART.CH%, REGARDLESS
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF
		CONTINUE 17900 IF ERR = 11%
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

	GL_RECORD_FOUND% = 0%

	!
	! Check to see if wild card and then check to see if match
	!
	IF WLDCRD$ <> ""
	THEN
		GOTO 17100 &
			IF COMP_STRING(EDIT$(GL_CHART::ACCT, -1%), WLDCRD$) = 0%
	END IF

	!
	! Check current record
	!
	GOTO ExitTotal &
		IF (EDIT$(GL_CHART::ACCT, -1%) > TO_ITEM$) AND TO_ITEM$ <> ""

17200	GOTO ExitProgram IF UTL_REPORTX::STAT

	IF GL_YYYY_PP::ACCT < GL_CHART::ACCT AND &
		GL_EMPTY_FLAG% = 0% AND WLDCRD$ = ""
	THEN
		UNDEFINED_ACCT% = -1%
		GL_CHART_RFA = GETRFA(GL_CHART.CH%)
		GL_CHART::ACCT		= GL_YYYY_PP::ACCT
		GL_CHART::DESCR		= STRING$(LEN(GL_CHART::DESCR), 63%)
		GL_CHART::DOLLAR(0%)	= 0.0
		UNDEFINED_LOOP% = UNDEFINED_LOOP% + 1%
		UNDEFINED_ACCT(UNDEFINED_LOOP%) = GL_CHART::ACCT
	END IF


	!
	! Calculate all of the GL_YYYY_PP amounts
	!
	GOSUB 18000

	GOSUB PrintTrlBal

	!
	! Try for next record
	!
	IF UNDEFINED_ACCT%
	THEN
		UNDEFINED_ACCT% = 0%
		WHEN ERROR IN
			GET #GL_CHART.CH%, RFA GL_CHART_RFA, REGARDLESS
		USE
			FILENAME$ = "GL_CHART"
			CONTINUE HelpError
		END WHEN
		GOTO 17200
	END IF

	GOTO 17100

	%PAGE

17900	!******************************************************************
	! Handle totals and other items before EXITing
	!******************************************************************

	IF GL_YYYY_PP::ACCT > GL_CHART::ACCT AND WLDCRD$ = ""
	THEN
		GL_CHART::ACCT		= GL_YYYY_PP::ACCT
		GL_CHART::DESCR		= STRING$(LEN(GL_CHART::DESCR), 63%)
		UNDEFINED_LOOP% = UNDEFINED_LOOP% + 1%
		UNDEFINED_ACCT(UNDEFINED_LOOP%) = GL_CHART::ACCT
		GOSUB 18000
		GOSUB PrintTrlBal

		GOTO ExitProgram IF UTL_REPORTX::STAT

		IF GL_YYYY_PP_EOF% = 0%
		THEN
			GOTO 17900
		END IF
	END IF

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

 ExitTotal:
	!
	! Print out totals
	!
	TEXT$ = "                              Balance             " + &
		FORMAT$(CHG_TOTAL, "###,###,###.##-  ")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	IF UNDEFINED_LOOP%
	THEN
		TEXT$ = "The following accounts are undefined"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 1%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		FOR LOOP% = 1% TO UNDEFINED_LOOP%

			TEXT$ = UNDEFINED_ACCT(LOOP%)

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 1%)
			GOTO ExitProgram IF UTL_REPORTX::STAT

		NEXT LOOP%
	END IF

 ExitProgram:
	!
	! Finish up report
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

18000	!******************************************************************
	! Total up all of the GL-YYYY-PP records for a given account
	!******************************************************************

	CHG_BAL = 0.0

	WHEN ERROR IN
		FIND #GL_YYYY_PP.CH%, KEY #0% GE GL_CHART::ACCT, REGARDLESS
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE 18090
	END WHEN

18010	!
	! Get next gl record
	!
	WHEN ERROR IN
		GET #GL_YYYY_PP.CH%, REGARDLESS
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		GL_EMPTY_FLAG%, GL_YYYY_PP_EOF% = -1% IF ERR = 11%
		CONTINUE 18090
	END WHEN

	GOTO 18090 IF GL_YYYY_PP::ACCT <> GL_CHART::ACCT

	GL_RECORD_FOUND% = -1%

	CHG_BAL = FUNC_ROUND(CHG_BAL + GL_YYYY_PP::AMOUNT, 2%)

	GOTO 18010

18090	RETURN

	%PAGE

 PrintTrlBal:
	!*************************************************************
	! Print trial balance
	!*************************************************************
	GOTO ComeBack &
		IF (FUNC_ROUND(BEG_BAL, 2%) = 0.0) AND &
		(ZERO_BALANCE$ = "N") AND &
		(GL_RECORD_FOUND% = 0%)

	!
	! Print out all the stuff.
	!
	TEXT$ = GL_CHART::ACCT + "  " + &
		LEFT(GL_CHART::DESCR, 30%) + &
		FORMAT$(CHG_BAL, "###,###,###.##-")

	CALL OUTP_LINE(LYT_LINE1$, UTL_REPORTX, TITLE$(), TEXT$, 3%)

	BEG_TOTAL = FUNC_ROUND(BEG_TOTAL + BEG_BAL, 2%)
	CHG_TOTAL = FUNC_ROUND(CHG_TOTAL + CHG_BAL, 2%)

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

32767	!******************************************************************
	! End of report GL_RPRT_PERIODBAL
	!******************************************************************
	END
