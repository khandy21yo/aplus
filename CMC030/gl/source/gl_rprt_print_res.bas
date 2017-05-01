1	%TITLE "GENLED - Print the General Ledger"
	%SBTTL "GL_RPRT_PRINT_RES"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1997 BY
	!
	! Software Solutions, Inc
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
	! Software Solutions, Inc
	!
	! Software Solutions, Inc assumes no responsibility for the use
	! or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc
	!
	!++
	! ID:GLPRNR
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*General Ledger Report\* is a listing of transactions by
	!	account number in date order, with resulting balances.
	!	.b
	!	The report includes the following fields:
	!	.table 30
	!	.te
	!	Account _#
	!	.te
	!	Date
	!	.te
	!	Source
	!	.te
	!	Reference
	!	.te
	!	Description
	!	.te
	!	Sub-Account
	!	.te
	!	Cross Reference
	!	.te
	!	Check
	!	.te
	!	Current Transactions
	!	.te
	!	Year-to-Date Balance
	!	.end table
	!	.lm -5
	!	Note:  Check the ledger to make certain the beginning balances are correct.
	!	If the ledger is showing incorrect balances or more detail than desired,
	!	check to make sure the "Annual Ledger" flag is set correctly.
	!
	! Index:
	!	.X General Ledger>Print
	!	.X Print>General Ledger
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_RPRT_PRINT_RES/LINE
	!	$ LINK/EXECUTABLE=GL_EXE: GL_RPRT_PRINT_RES, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_RPRT_PRINT_RES.OBJ;*
	!
	! Author:
	!
	!	08/20/97 - Kevin Handy
	!		Based on GL_RPRT_PRINT, Modified to make a
	!		restricted detail version that looks into
	!		GL_USERLIST to see if they can see detail.
	!
	! Modification history:
	!
	!	08/25/97 - Kevin Handy
	!		Use 'val' instead of 'val%'
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/04/2000 - Kevin Handy
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
	! Set up data storage areas (MAPs, DIMENSIONs, DECLAREs, etc.)
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

	%INCLUDE "SOURCE:[GL.OPEN]GL_USERLIST.HB"
	MAP	(GL_USERLIST)	GL_USERLIST_CDD	GL_USERLIST

	!
	! Dimension arrays
	!
	DIM LONG GL_YYYY_PP.CH(13%), GL_YYYY_PP_EOF(13%), TRAN_COUNT(300%)
	DIM REAL SUMMARY_TOTAL(300%)
	DIM STRING TRANDAT(300%), SOURCE(300%), BATCH_ID(300%), &
		GLPERIOD(13%), TEST_ACCT(13%), UNDEFINED_ACCT(300%)

	!
	! Declare some variables
	!
	DECLARE	RFA GL_CHART_RFA

	%PAGE

	!******************************************************************
	! Take care of anything else before starting the report
	!******************************************************************

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

	%PAGE

 Initialization:
	!******************************************************************
	! Get ready to begin
	!******************************************************************

	GL_USERLIST::USER = READ_USERNAME
	USERNAME$ = GL_USERLIST::USER + ""

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
	!	If the report is to start with the first account in the file,
	!	this field should be left blank.
	!	.lm -5
	!
	! Index:
	!	.x From Account
	!	.x General Ledger Report>From Account
	!	.x Report Setting>From Account
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Account\*
	!	.lm +5
	!	.b
	!	The ^*To Account\* field causes the
	!	printing to end with a specified
	!	account number.
	!	.b
	!	If the report is to end with the last
	!	account in the file, this field should be left blank.
	!	.lm -5
	!
	! Index:
	!	.x To Account
	!	.x General Ledger Report>To Account
	!	.x Report Settings>To Account
	!
	!--

	ZERO_BALANCE$ = LEFT(EDIT$(UTL_REPORTX::OPTDEF(2%), -1%), 1%)

	!++
	! Abstract:FLD03
	!	.x Zero Balance
	!	^*(03) Zero Balance\*
	!	.b
	!	.lm +5
	!	A ^*(Y) Yes\* entered in this field causes
	!	the report to include all accounts in the Chart of Accounts.
	!	A ^*(N) No\* entered causes a print suppression
	!	of all accounts
	!	which have a zero beginning balance, no transaction activity,
	!	and a zero ending balance.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x General Ledger Report>Zero Balances
	!	.x Report Settings>Zero Balances
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)

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
	!	.x Wildcard
	!	.x General Ledger Report>Wildcard
	!	.x Report Settings>Wildcard
	!
	!--

	CNTRL_FLAG$ = LEFT(EDIT$(UTL_REPORTX::OPTDEF(4%), 132%), 1%)

	!++
	! Abstract:FLD05
	!	.x Control Flag
	!	^*(05) Control Flag\*
	!	.b
	!	.lm +5
	!	An ^*N (No)\* value in the ^*Control Flag\* field causes a
	!	complete detail printing of a General Ledger Report, i.e., every
	!	transaction for every account number will be listed.
	!	.b
	!	A ^*Y (Yes)\* value in this setting causes the system to refer
	!	to the Summary Flag in each Chart of Accounts record and print
	!	the level of detail as specified for each account according to
	!	the code.
	!	.b
	!	Valid Summary Flags are:
	!	.table 3,25
	!	.te
	!	Code	Description
	!	.b
	!	.te
	!	##1	Full detail
	!	.te
	!	##2	By date
	!	.te
	!	##3	By batch
	!	.end table
	!	.LM -5
	!
	! Index:
	!	.x General Ledger Report>Control Flag
	!	.x Report Settings>Control Flag
	!
	!--

	ANNUAL_GL$ = LEFT(EDIT$(UTL_REPORTX::OPTDEF(5%), 132%), 1%)

	!++
	! Abstract:FLD06
	!	.x Annual General Ledger Report
	!	^*(06) Annual General Ledger\*
	!	.b
	!	.lm +5
	!	A ^*N (No)\* value in the ^*Annual G/L\* field causes a
	!	normal General Ledger for a single period to be printed.
	!	.b
	!	A ^*Y (Yes)\* value in this field causes a General Ledger
	!	to be printed which will include transactions for an entire
	!	fiscal year.
	!	.lm -5
	!
	! Index:
	!	.x General Ledger Report>Annual
	!	.x Report Settings>Annual General Ledger
	!
	!--

	EXTRA_TITLE$ = EDIT$(UTL_REPORTX::OPTDEF(6%), 132%)

	!++
	! Abstract:FLD07
	!	^*(07) Supplemental Title\*
	!	.b
	!	.lm +5
	!	The ^*Supplemental Title\* field is used to add an additional
	!	title to the General Ledger report.  If only one department or
	!	location was being printed, that department or location name
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

	IF (LEFT(EDIT$(UTL_REPORTX::OPTDEF(7%), 132%), 1%) = "Y") AND &
		(ANNUAL_GL$ = "Y")
	THEN
		INTERMEDIATE% = -1%
	ELSE
		INTERMEDIATE% = 0%
	END IF

	!++
	! Abstract:FLD08
	!	^*(08) Intermediate Subtotals\*
	!	.b
	!	.lm +5
	!	The ^*Intermediate Subtotals\* field is used to print
	!	intermediate subtotals for an annual general ledger.
	!	This field has no meaning for a monthly general ledger.
	!	.lm -5
	!
	! Index:
	!	.x Intermediate Totals
	!	.x Report Settings>Intermediate Totals
	!
	!--

	%PAGE


300	!
	! Get period infotmation, check close status
	!
	WHEN ERROR IN
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
	! Set up periods
	!
	CHART_LOOP% = 0%
	CUR_PERIOD% = GL_PERIOD::LASTPERCLO + 1%
	YEAR$ = GL_PERIOD::YEAR

	IF CUR_PERIOD% > GL_PERIOD::FPFY
	THEN
		CUR_PERIOD% = 1%
		YEAR$ = FORMAT$(VAL%(YEAR$) + 1%, "<0>###")
	END IF

	IF ANNUAL_GL$ = "Y"
	THEN
		GLPERIOD(LOOP%) = YEAR$ + "_" + FORMAT$(LOOP%, "<0>#") &
			FOR LOOP% = 1% TO CUR_PERIOD%
		CHART_LOOP% = CUR_PERIOD% - 1%
	ELSE
		GLPERIOD(1%) = YEAR$ + "_" + FORMAT$(CUR_PERIOD%, "<0>#")
	END IF

305	!
	! Open User/Account validation list
	!
	%INCLUDE "SOURCE:[GL.OPEN]GL_USERLIST.OPN"

	!
	! Open chart file
	!
310	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.OPN"
	USE
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

	!
	! Open the current gl period file(s)
	!
	ATLEASTONE% = 0%

320	FOR LOOP% = 1% TO CHART_LOOP% + 1%
		YYYY_PP$ = GLPERIOD(LOOP%)

		GL_YYYY_PP.CH% = 0%
		GL_YYYY_PP.CH(LOOP%) = 0%

		WHEN ERROR IN
			%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.OPN"
		USE
			CONTINUE 330 IF ERR = 5%
			FILENAME$ = "GL_" + YYYY_PP$
			CONTINUE HelpError
		END WHEN

		GL_YYYY_PP.CH(LOOP%) = GL_YYYY_PP.CH%
		ATLEASTONE% = -1%

330	NEXT LOOP%

	IF (ATLEASTONE% = 0%)
	THEN
		FILENAME$ = "GL_" + GLPERIOD(1%)
		GOTO HelpError
	END IF

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

	TITLE$(TITLE% + 1%) = "General Ledger"
	TITLE$(TITLE% + 2%) = "For the Period Ending " + &
		TRM$(GL_PERIOD::PERIOD(CUR_PERIOD%)) + " in Year " + YEAR$
	TITLE$(TITLE% + 3%) = ""

	!
	! Headers
	!
	TITLE$(TITLE% + 4%) = "Account number       Date   SC   Reference    " + &
		"    Description           Subacc     Xref #     Check " + &
		"       Current         Balance"
	TITLE$(TITLE% + 5%) = ""

	!
	! Layout for printed lines
	!
	LYT_LINE$ = "$Account:018,DTranDate:027,$Source:032," + &
		"$RefNum:049,$Descr:071,$SubAcct:082,$XRefNum:093," + &
		"$CheckNum:100,VAmount:115"
	LYT_LINE1$ = "$Account:018,DTranDate:018,$Source:018," + &
		"$RefNum:018,$Descr:115,$SubAcct:115,$XRefNum:115," + &
		"$CheckNum:115,VAmount:115,VBalance:132"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	IF ANNUAL_GL$ = "Y"
	THEN
		GOSUB LoadSummary
	ELSE
		SUMMARYTOTAL = GL_PERIOD::SUMMARYTOTAL
	END IF

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

	GL_EMPTY_FLAG% = 0%

17050	!
	! Find the first record in the General Ledger Period file
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
	! Main loop starts here
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
	GOTO 17100 IF COMP_STRING(EDIT$(GL_CHART::ACCT, -1%), WLDCRD$) = 0% &
		AND WLDCRD$ <> ""

17200	GOTO ExitProgram IF UTL_REPORTX::STAT

	IF (GL_YYYY_PP::ACCT < GL_CHART::ACCT) AND &
		(GL_EMPTY_FLAG% = 0%) AND &
		(WLDCRD$ = "")
	THEN
		UNDEFINED_ACCT% = -1%
		GL_CHART_RFA = GETRFA(GL_CHART.CH%)
		GL_CHART::ACCT		= GL_YYYY_PP::ACCT
		GL_CHART::DESCR		= STRING$(LEN(GL_CHART::DESCR), 63%)
		GL_CHART::SUMMARY	= "4"
		GL_CHART::DOLLAR(CHART_LOOP%) = 0.0
		UNDEFINED_LOOP% = UNDEFINED_LOOP% + 1%
		UNDEFINED_ACCT(UNDEFINED_LOOP%) = GL_CHART::ACCT
	END IF

	!
	! Calculate beginning balance
	!
	BEG_BAL = FUNC_ROUND(GL_CHART::DOLLAR(CHART_LOOP%), 2%)

	IF (CUR_PERIOD% = 1%) OR (ANNUAL_GL$ = "Y")
	THEN
		!
		! Fun and games on starting first period of a year.
		!
		BEG_BAL = 0.0 &
			IF INSTR(1%, "RE", GL_CHART::ACCTYPE)
		IF GL_CHART::ACCTYPE = "S" AND SUMMARY_FLAG% = 0%
		THEN
			BEG_BAL = BEG_BAL + SUMMARYTOTAL
			SUMMARY_FLAG% = -1%
		END IF
	END IF

	!
	! Print Beginning balance and Calculate all of the GL_YYYY_PP amounts
	!
	GOSUB 18000
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOSUB PrintEndBal
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Try for next record
	!
	IF UNDEFINED_ACCT%
	THEN
		UNDEFINED_ACCT% = 0%
		GET #GL_CHART.CH%, RFA GL_CHART_RFA, REGARDLESS
		GOTO 17200
	END IF

	GOTO 17100

	%PAGE

17900	!******************************************************************
	! Handle totals and other items before EXITing
	!******************************************************************

	!
	! Handle the end of the report
	!
	FOR LOOP% = 1% TO CHART_LOOP% + 1%
		GL_YYYY_PP::ACCT = TEST_ACCT(LOOP%) &
			IF (GL_YYYY_PP::ACCT > GL_CHART::ACCT) AND &
			GL_YYYY_PP::ACCT < TEST_ACCT(LOOP%)
	NEXT LOOP%

	IF (GL_YYYY_PP::ACCT > GL_CHART::ACCT) AND (WLDCRD$ = "")
	THEN
		GL_CHART::ACCT		= GL_YYYY_PP::ACCT
		GL_CHART::DESCR		= STRING$(LEN(GL_CHART::DESCR), 63%)
		GL_CHART::SUMMARY	= "4"
		BEG_BAL = 0.0
		UNDEFINED_LOOP% = UNDEFINED_LOOP% + 1%
		UNDEFINED_ACCT(UNDEFINED_LOOP%) = GL_CHART::ACCT

		GOSUB 18000
		GOSUB PrintEndBal

		GOTO ExitProgram IF UTL_REPORTX::STAT

		TEST% = -1%

		TEST% = 0% IF GL_YYYY_PP_EOF(LOOP%) = 0% &
			FOR LOOP% = 1% TO CHART_LOOP% + 1%

		GOTO 17900 IF TEST% = 0%
	END IF

 ExitTotal:
	!
	! Print out totals at the end of the report
	!
	TEXT$ = SPACE$(66%) + &
		"TOTAL----------------------->                      " + &
		FORMAT$(TOTAL_AMOUNT, "##,###,###.##-") + &
		"*"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 1%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

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
	! Subroutine to total up all of the GL_YYYY_PP records
	! for a given account.
	!******************************************************************

	GL_RECORD_FOUND%, PRINT_BEGBAL%, TRAN_COUNT%, BATCH_COUNT% = 0%

	CHG_BAL, SUMMARY_TOTAL = 0.0

	GL_CHART::SUMMARY = "1" IF CNTRL_FLAG$ <> "Y" OR &
		INSTR(1%, "234", GL_CHART::SUMMARY) = 0%

	IF (GL_CHART::SUMMARY <> "4")
	THEN
		WHEN ERROR IN
			GET #GL_USERLIST.CH%, &
				KEY #0% EQ USERNAME$ + GL_CHART::ACCT, &
				REGARDLESS
		USE
			GL_CHART::SUMMARY = "4"
			CONTINUE 18005
		END WHEN
	END IF

18005	IF ZERO_BALANCE$ <> "N" OR BEG_BAL <> 0.0
	THEN
		PRINT_BEGBAL% = -1%
		GOSUB PrintBegBal
		GOTO ComeBack1 IF UTL_REPORTX::STAT
	END IF

	TEST_ACCT(LOOP%) = "" FOR LOOP% = 1% TO CHART_LOOP% + 1%

	FOR LOOP% = 1% TO CHART_LOOP% + 1%

		WHEN ERROR IN
			FIND #GL_YYYY_PP.CH(LOOP%), &
				KEY #0% GE GL_CHART::ACCT, &
				REGARDLESS
		USE
			GL_EMPTY_FLAG%, GL_YYYY_PP_EOF(LOOP%) = -1% IF ERR = 11%
			CONTINUE 18020
		END WHEN

18010		WHEN ERROR IN
			GET #GL_YYYY_PP.CH(LOOP%), REGARDLESS
		USE
			GL_EMPTY_FLAG%, GL_YYYY_PP_EOF(LOOP%) = -1% IF ERR = 11%
			CONTINUE 18020
		END WHEN

		TEST_ACCT(LOOP%) = GL_YYYY_PP::ACCT

		GOTO 18020 IF GL_YYYY_PP::ACCT <> GL_CHART::ACCT

		IF PRINT_BEGBAL% = 0%
		THEN
			PRINT_BEGBAL% = -1%
			GOSUB PrintBegBal
			GOTO ComeBack1 IF UTL_REPORTX::STAT
		END IF

		CHG_BAL = CHG_BAL + GL_YYYY_PP::AMOUNT

		SELECT GL_CHART::SUMMARY

		CASE "2"
			IF TRANDAT$ <> GL_YYYY_PP::TRANDAT AND &
				TRAN_COUNT% <> 0%
			THEN
				DESCR$ = "SUMMARY (" + NUM1$(TRAN_COUNT%) + ")"
				GOSUB PrintSummaryLine
				SUMMARY_TOTAL = 0.0
				TRAN_COUNT% = 0%
			END IF

			ACCT$ = GL_YYYY_PP::ACCT
			TRANDAT$ = GL_YYYY_PP::TRANDAT
			SOURCE$ = "SUM"
			REFNO$ = ""
			SUBACC$ = ""
			XREFNO$ = ""
			CKNO$ = ""
			TRAN_COUNT% = TRAN_COUNT% + 1%
			SUMMARY_TOTAL = FUNC_ROUND(SUMMARY_TOTAL + &
				GL_YYYY_PP::AMOUNT, 2%)

		CASE "3"
			GOTO BatchLoopEnd IF BATCH_ID(TEST%) = &
				GL_YYYY_PP::BTHNUM &
				FOR TEST% = 1% TO BATCH_COUNT%

			BATCH_COUNT%, TEST% = BATCH_COUNT% + 1%
			TRANDAT(TEST%) = GL_YYYY_PP::TRANDAT
			SOURCE(TEST%) = GL_YYYY_PP::SOURCE
			BATCH_ID(TEST%) = GL_YYYY_PP::BTHNUM
			TRAN_COUNT(TEST%) = 0%
			SUMMARY_TOTAL(TEST%) = 0.0

 BatchLoopEnd:		ACCT$ = GL_YYYY_PP::ACCT
			TRAN_COUNT(TEST%) = TRAN_COUNT(TEST%) + 1%
			SUMMARY_TOTAL(TEST%) = &
				FUNC_ROUND(SUMMARY_TOTAL(TEST%) + &
				GL_YYYY_PP::AMOUNT, 2%)

		CASE "4"
			ACCT$ = GL_YYYY_PP::ACCT
			TRANDAT$ = GL_YYYY_PP::TRANDAT
			SOURCE$ = "SUM"
			REFNO$ = ""
			SUBACC$ = ""
			XREFNO$ = ""
			CKNO$ = ""
			TRAN_COUNT% = TRAN_COUNT% + 1%
			SUMMARY_TOTAL = FUNC_ROUND(SUMMARY_TOTAL + &
				GL_YYYY_PP::AMOUNT, 2%)

		CASE ELSE

			!
			! Display record
			!
			TEXT$ = GL_YYYY_PP::ACCT + " " + &
				PRNT_DATE(GL_YYYY_PP::TRANDAT, 6%) + " " + &
				GL_YYYY_PP::SOURCE + " " + &
				GL_YYYY_PP::REFNO + " " + &
				LEFT(GL_YYYY_PP::DESCR, 21%) + " " + &
				GL_YYYY_PP::SUBACC + " " + &
				GL_YYYY_PP::XREFNO + " " + &
				GL_YYYY_PP::CKNO + " " + &
				FORMAT$(GL_YYYY_PP::AMOUNT, "##,###,###.##-")

			CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), &
				TEXT$, 1%)
			GOTO ComeBack1 IF UTL_REPORTX::STAT

		END SELECT

		GL_RECORD_FOUND% = -1%
		PFLAG% = -1%

		GOTO 18010

18020		!
		! Print intermediate subtotals
		!
		IF INTERMEDIATE%
		THEN
			GOSUB OutputSummary

			IF PFLAG%
			THEN
				TEXT$ = SPACE$(101%) + &
					FORMAT$(CHG_BAL, "##,###,###.##-  ") + &
					FORMAT$(BEG_BAL + CHG_BAL, "##,###,###.##-") + &
					"*"

				CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
					TEXT$, 0%)
			END IF
		END IF

		PFLAG% = 0%

	NEXT LOOP%

	GOSUB OutputSummary

	FOR LOOP% = 1% TO CHART_LOOP% + 1%
		IF TEST_ACCT(LOOP%) < GL_YYYY_PP::ACCT
		THEN
			TEST_ACCT(LOOP%) = GL_YYYY_PP::ACCT
		END IF
	NEXT LOOP%

 ComeBack1:
	RETURN

	%PAGE

18050	!*******************************************************************
	! Print out all of the summarizes information
	!*******************************************************************

 OutputSummary:

	SELECT GL_CHART::SUMMARY

	CASE "2", "4"
		IF TRAN_COUNT% <> 0%
		THEN
			DESCR$ = "SUMMARY (" + NUM1$(TRAN_COUNT%) + ")"
			GOSUB PrintSummaryLine
		END IF

	CASE "3"
		FOR ZLOOP% = 1% TO BATCH_COUNT%
			TRANDAT$ = TRANDAT(ZLOOP%)
			SOURCE$ = SOURCE(ZLOOP%)
			DESCR$ = "SUMMARY (" + NUM1$(TRAN_COUNT(ZLOOP%)) + ")"
			REFNO$ = BATCH_ID(ZLOOP%)
			SUBACC$ = ""
			XREFNO$ = ""
			CKNO$ = ""
			SUMMARY_TOTAL = SUMMARY_TOTAL(ZLOOP%)
			GOSUB PrintSummaryLine
		NEXT ZLOOP%

	END SELECT

	BATCH_COUNT% = 0%
	SUMMARY_TOTAL = 0.0
	TRAN_COUNT% = 0%

	RETURN

 PrintBegBal:
	!******************************************************************
	! Subroutine to display starting balance
	!******************************************************************
	TEXT$ = GL_CHART::ACCT + " " + &
		LEFT(GL_CHART::DESCR, 39%) + SPACE$(59%) + &
		FORMAT$(BEG_BAL, "##,###,###.##-")

	CALL OUTP_LINE(LYT_LINE1$, UTL_REPORTX, TITLE$(), TEXT$, 5%)

	RETURN

	%PAGE

 PrintEndBal:
	!****************************************************************
	! Subroutine to print totals
	!****************************************************************
	GOTO ComeBack2 &
		IF (ZERO_BALANCE$ = "N") AND &
		(GL_RECORD_FOUND% = 0%) AND &
		(PRINT_BEGBAL% = 0%)

	!
	! Fill in next line
	!
	TEXT$ = SPACE$(101%) + &
		FORMAT$(CHG_BAL, "##,###,###.##-  ") + &
		FORMAT$(BEG_BAL + CHG_BAL, "##,###,###.##-") + &
		"*"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ComeBack2 IF UTL_REPORTX::STAT

	TOTAL_AMOUNT = FUNC_ROUND(TOTAL_AMOUNT + BEG_BAL + CHG_BAL, 2%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

 ComeBack2:
	RETURN

	%PAGE

 LoadSummary:
	!*******************************************************************
	! Calculate the summary account, since the number in the GL
	! control file may not be correct after a close/reset
	!*******************************************************************

18100	SUMMARYTOTAL = 0.0

	WHEN ERROR IN
		RESET #GL_CHART.CH%
	USE
		CONTINUE 18190
	END WHEN

18110	WHEN ERROR IN
		GET #GL_CHART.CH%, REGARDLESS
	USE
		CONTINUE 18190
	END WHEN

	IF INSTR(1%, "RE", GL_CHART::ACCTYPE)
	THEN
		SUMMARYTOTAL = FUNC_ROUND(SUMMARYTOTAL + &
			GL_CHART::DOLLAR(CHART_LOOP%), 2%)
	END IF

	GOTO 18110

18190	RETURN

	%PAGE

 PrintSummaryLine:
	!*****************************************************************
	! Print summary line
	!*****************************************************************

	!
	! Display record
	!
	TEXT$ = LEFT(ACCT$ + SPACE$(18%), 18%) + " " + &
		PRNT_DATE(TRANDAT$, 6%) + " " + &
		LEFT(SOURCE$ + "    ", 4%) + " " + &
		LEFT(REFNO$ + SPACE$(16%), 16%) + " " + &
		LEFT(DESCR$ + SPACE$(21%), 21%) + " " + &
		LEFT(SUBACC$ + SPACE$(10%), 10%) + " " + &
		LEFT(XREFNO$ + SPACE$(10%), 10%) + " " + &
		LEFT(CKNO$ + SPACE$(6%), 6%) + " " + &
		FORMAT$(SUMMARY_TOTAL, "##,###,###.##-")

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 1%)

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
	! End of report GL_RPRT_PRINT_RES
	!******************************************************************
	END
