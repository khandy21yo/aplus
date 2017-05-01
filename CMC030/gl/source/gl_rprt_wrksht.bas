1	%TITLE "WKSH - Print the Worksheet"
	%SBTTL "GL_RPRT_WRKSHT"
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
	! ID:GLWKSH
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	A ^*Worksheet\* lists essentially the same information
	!	as a Trial Balance, except there are columns for debit
	!	and credit adjustments and for debit and credit adjusted
	!	balances.
	!	.b
	!	^*Worksheets\* are sometimes referred to as a "Working
	!	Trial Balance" and can be used as an aid in making
	!	adjusting journal entries. The following information is included in this
	!	report:
	!	.table 30
	!	Account Number
	!	.te
	!	Description
	!	.te
	!	Debit Amount
	!	.te
	!	Credit Amount
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Worksheet>Print
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_RPRT_WRKSHT/LINE
	!	$ LINK/EXECUTABLE=GL_EXE: GL_RPRT_WRKSHT, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_RPRT_WRKSHT.OBJ;*
	!
	! Author:
	!
	!	02/01/85 - Randall Beard
	!
	! Modification history:
	!
	!	05/24/86 - Kevin Handy
	!		Converted to VAX BASIC
	!
	!	03/30/88 - Robert Peterson
	!		Modified wild card option to ignore search for
	!		undefined accounts in the transaction folder.
	!
	!	09/11/90 - Kevin Handy
	!		Moved calculation for current period out of
	!		select statement to make it easier to read.
	!
	!	01/10/91 - Craig Tanner
	!		Where FILENAME$ = "GL_YYYY_PP" in error handler,
	!		changed to = "GL_" + YYYY_PP$.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	12/15/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!		Change RIGHT(NUM1$()) to FORMAT$()
	!
	!	09/04/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/27/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/20/97 - Kevin Handy
	!		Don't allocate channel for report
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/07/2000 - Kevin Handy
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
	! Set up data storage areas  (MAPs, DIMENSIONs, DECLAREs, etc.)
	!******************************************************************

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
	DIM	STRING	UNDEFINED_ACCT(300%)

	!
	! Declare some variables
	!
	DECLARE	RFA	GL_CHART_RFA

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
	!	The ^*From Account\* field causes the
	!	printing to begin with a specified
	!	account number.
	!	.b
	!	If the report is to begin with the first account in file, this field
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

	ZERO_BALANCE$ = LEFT(EDIT$(UTL_REPORTX::OPTDEF(2%), -1%), 1%)

	!++
	! Abstract:FLD03
	!	^*(03) Zero Balance?\*
	!	.b
	!	.lm +5
	!	A ^*Y (Yes)\* value in the ^*Zero Balance\* setting will cause
	!	the report to include all accounts in the file. A ^*N (No)\* value will
	!	cause all accounts which have a zero balance,
	!	no transaction activity, and a zero ending balance not to be
	!	printed.
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
	!	report were to be printed to include department ^*"02"\* only, the
	!	Wildcard setting of ^*"?????-02"\* would cause the report to print
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
	!	title to the General Ledger report. For example: if only
	!	one department or location was being printed, that
	!	department or location name could be entered into the
	!	supplemental title field.
	!	.b
	!	This field may be left blank.
	!	.lm -5
	!
	! Index:
	!	.x Supplemental Title
	!	.x Report Settings>Supplemental Title
	!
	!--


	LINES_1$ = "|------------|------------|------------|------------|"
	LINES_2$ = "|            |            |            |            |"
	USE_1_1$ = "###,###,###.##- "
	USE_1_2$ = "                "
	USE_2$ = SPACE$(79%) + LINES_1$
	GL_EMPTY_FLAG% = 0%

	%PAGE


300	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.OPN"

		GET #GL_PERIOD.CH%, RECORD 1%, REGARDLESS
		CLOSE GL_PERIOD.CH%
	USE
		FILENAME$ = "GL_PERIOD"
		CONTINUE HelpError
	END WHEN

	SELECT GL_PERIOD::CLOSEFLAG

	CASE "1"
		CALL HELP_3MESSAGE(SCOPE, "GL Close in process", &
			"ERR", "GL_CLOSE", "ERROR_CLOSE")
		UTL_REPORTX::STAT = -1%
		GOTO ExitProgram

	CASE "2"
		CALL HELP_3MESSAGE(SCOPE, "GL Reset in process", &
			"ERR", "GL_RESET", "ERROR_RESET")
		UTL_REPORTX::STAT = -1%
		GOTO ExitProgram

	END SELECT

	!
	! Calculate current period
	!
	CHART_LOOP% = 0%
	CUR_PERIOD% = GL_PERIOD::LASTPERCLO + 1%
	YEAR$ = GL_PERIOD::YEAR

	IF CUR_PERIOD% > GL_PERIOD::FPFY
	THEN
		CUR_PERIOD% = 1%
		YEAR$ = FORMAT$(VAL%(YEAR$) + 1%, "<0>###")
	END IF

	YYYY_PP$ = YEAR$ + "_" + FORMAT$(CUR_PERIOD%, "<0>#")

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
	! Open the current GL Year, Period file
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
	! Set up titles and whatnot
	!
	IF EXTRA_TITLE$ <> ""
	THEN
		TITLE% = 1%
		TITLE$(1%) = EXTRA_TITLE$
	ELSE
		TITLE% = 0%
	END IF

	TITLE$(TITLE% + 1%) = "Worksheet"
	TITLE$(TITLE% + 2%) = "For the Period Ended " + &
		TRM$(GL_PERIOD::PERIOD(CUR_PERIOD%)) + " of Year " + YEAR$
	TITLE$(TITLE% + 3%) = ""

	!
	! Headers
	!
	TITLE$(TITLE% + 4%) = SPACE$(79%) + LINES_1$
	TITLE$(TITLE% + 5%) = "Acct #              Description                " + &
		"     Debit amt      Credit amt  |   Debit    |   Credit" + &
		"   |   Debit    |   Credit   |"
	TITLE$(TITLE% + 6%) = SPACE$(79%) + LINES_1$
	TITLE$(TITLE% + 7%) = ""

	!
	! Layouts for printed lines
	!
	LYT_LINE$ = "$Account:018,$Descr:045,VCredit:063,VDebit:079"

	%PAGE

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

17050	!
	! Find First record in the general ledger
	!
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #GL_YYYY_PP.CH%
		ELSE
			FIND #GL_YYYY_PP.CH%, KEY #0% GE FROM_ITEM$, REGARDLESS
		END IF

		GET #GL_YYYY_PP.CH%, REGARDLESS
	USE
		GL_EMPTY_FLAG% = -1%
		CONTINUE 17100
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
		CONTINUE 17900 IF ERR = 11%
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO ExitTotal &
		IF (EDIT$(GL_CHART::ACCT, -1%) > TO_ITEM$) AND TO_ITEM$ <> ""

	!
	! Check to see if wildcard then check to see if match
	!
	GOTO 17100 &
		IF COMP_STRING(EDIT$(GL_CHART::ACCT, -1%), WLDCRD$) = 0% AND &
		WLDCRD$ <> ""

17130	GOTO ExitProgram IF UTL_REPORTX::STAT

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
	! Calculate beginning balance
	!
	BEG_BAL = FUNC_ROUND(GL_CHART::DOLLAR(CHART_LOOP%), 2%)

	IF CUR_PERIOD% = 1%
	THEN
		!
		! Fun and games on starting first period of a year.
		!
		BEG_BAL = 0.0 IF INSTR(1%, "RE", GL_CHART::ACCTYPE)
		IF GL_CHART::ACCTYPE = "S" AND SUMMARY_FLAG% = 0%
		THEN
			BEG_BAL = FUNC_ROUND(BEG_BAL + &
				GL_PERIOD::SUMMARYTOTAL, 2%)
			SUMMARY_FLAG% = -1%
		END IF
	END IF

	!
	! Calculate all of the GL_YYYY_PP amounts
	!
	GL_RECORD_FOUND% = 0%
	GOSUB 18000

	GOSUB PrintWrkSht

	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Try for next record
	!
	IF UNDEFINED_ACCT%
	THEN
		UNDEFINED_ACCT% = 0%
		GET #GL_CHART.CH%, RFA GL_CHART_RFA, REGARDLESS
		GOTO 17130
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
		BEG_BAL = 0.0
		GOSUB 18000
		GOSUB PrintWrkSht
		IF GL_YYYY_PP_EOF% = 0%
		THEN
			GOTO 17900
		END IF
	END IF

 ExitTotal:
	!
	! Print out the totals
	!
	TEXT$ = SPACE$(18%) + "  " + "TOTAL                      " + &
		FORMAT$(DEBIT_AMOUNT, USE_1_1$) + &
		FORMAT$(-CREDIT_AMOUNT,USE_1_1$)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$ + LINES_2$, 0%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), USE_2$, 0%)

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

18000	!******************************************************************
	! Subroutine to total up all of the GL-PP-YYYY records for
	! a given account.
	!******************************************************************

	CHG_BAL = 0.0

	WHEN ERROR IN
		FIND #GL_YYYY_PP.CH%, KEY #0% GE GL_CHART::ACCT, REGARDLESS
	USE
		GL_EMPTY_FLAG%, GL_YYYY_PP_EOF% = -1% IF ERR = 11%
		CONTINUE 18090
	END WHEN


18010	!
	! Get next gl record
	!
	WHEN ERROR IN
		GET #GL_YYYY_PP.CH%, REGARDLESS
	USE
		GL_EMPTY_FLAG%, GL_YYYY_PP_EOF% = -1% IF ERR = 11%
		CONTINUE 18090
	END WHEN

	GOTO 18090 IF GL_YYYY_PP::ACCT <> GL_CHART::ACCT

	GL_RECORD_FOUND% = -1%

	CHG_BAL = FUNC_ROUND(CHG_BAL + GL_YYYY_PP::AMOUNT, 2%)

	GOTO 18010

18090	RETURN

	%PAGE

 PrintWrkSht:
	!*************************************************************
	! Subroutine to print WorkSheet
	!*************************************************************

	END_BAL = FUNC_ROUND(BEG_BAL + CHG_BAL, 2%)

	GOTO ComeBack IF BEG_BAL = 0.0 AND ZERO_BALANCE$ = "N" AND &
			GL_RECORD_FOUND% = 0%

	!
	! Print out all the stuff.
	!
	TEXT$ = GL_CHART::ACCT + "  " + LEFT(GL_CHART::DESCR, 25%) + "  "

	IF END_BAL < 0%
	THEN
		TEXT$ = TEXT$ + FORMAT$(-END_BAL, USE_1_2$ + USE_1_1$)
		CREDIT_AMOUNT = FUNC_ROUND(CREDIT_AMOUNT + END_BAL, 2%)
	ELSE
		TEXT$ = TEXT$ + FORMAT$(END_BAL, USE_1_1$ + USE_1_2$)
		DEBIT_AMOUNT = FUNC_ROUND(DEBIT_AMOUNT + END_BAL, 2%)
	END IF

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$ + LINES_2$, 0%)
	GOTO ComeBack IF UTL_REPORTX::STAT

	!
	! Fill in next line
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), USE_2$, 0%)

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

32767	!******************************************************************
	! End of report GL_RPRT_WRKSHT
	!******************************************************************
	END
