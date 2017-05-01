1	%TITLE "General Ledger by Source Code"
	%SBTTL "GL_RPRT_GLSRC"
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
	! ID:GLSRC
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Print GL by Source Code\* option prints transactions
	!	in a specific General Ledger file by specified source codes.
	!	.b
	!	The report can include all General Ledger accounts affected by a specified
	!	source code or selected accounts only can be printed. Wildcarding techniques
	!	may also be used for General Ledger account selection, as well as for the
	!	source code selection.
	!	.b
	!	This report includes the following fields:
	!	.table 30
	!	.te
	!	Account Number
	!	.te
	!	Date
	!	.te
	!	Source Code
	!	.te
	!	Reference
	!	.te
	!	Description
	!	.te
	!	Cross Reference Number
	!	.te
	!	Check Number
	!	.te
	!	Current Amount
	!	.te
	!	Balance
	!	.end table
	!	.lm -15
	!	.b
	!	.lm +10
	!	Since hand check transactions can be entered in a Purchases Journal and
	!	the credit transaction to the General Ledger "Cash" account contains a
	!	CD (Cash Disbursement) source code, a Source Code Audit report for a CD
	!	source code will be out of balance by the same amount that a Source Code
	!	Audit report for a PJ source code will be out of balance.
	!	.lm -10
	!
	! Index:
	!	.x Audit>Reports>Source Code
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_RPRT_GLSRC/LINE
	!	$ LINK/EXECUTABLE=GL_EXE: GL_RPRT_GLSRC, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_RPRT_GLSRC.OBJ;*
	!
	! Author:
	!
	!	09/09/88 - Kevin Handy
	!
	! Modification history:
	!
	!	09/12/88 - Kevin Handy
	!		Modified title.  Added grand total.
	!
	!	09/14/88 - Kevin Handy
	!		Fixed totaling problems.
	!
	!	11/11/88 - Kevin Handy
	!		Modified to pull up and print vendor
	!		name on AP items.
	!
	!	11/15/88 - Kevin Handy
	!		Removed mod re. 11/11/88.  Removed
	!		subaccount to make room for more description
	!		to print out.
	!
	!	01/10/91 - Craig Tanner
	!		Where FILENAME$ = "GL_YYYY_PP" in error handler,
	!		changed to = "GL_" + YYYY_PP$.
	!
	!	06/03/91 - Frank F. Starman
	!		Add last page with debit and credit totals
	!
	!	10/23/92 - Dan Perkins
	!		Added arguement to GL_OUTP_ACCTSUM because of a change
	!		to this function.
	!
	!	03/18/93 - Kevin Handy
	!		Added arguement to GL_OUTP_ACCTSUM for units.
	!
	!	12/07/93 - Kevin Handy
	!		Removed EDIT$(,-1) on check against TO_ITEM$ because
	!		it doesn't work like the users want, and is
	!		unecessary.
	!
	!	12/07/93 - Kevin Handy
	!		Modified to use more than just the first six
	!		characters of  from-to fields.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/20/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/15/97 - Kevin Handy
	!		Reformat source code
	!		Use integer for #key
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	05/22/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	11/06/2000 - Kevin Handy
	!		Set up for multiple periods, instead of
	!		only one period.
	!
	!	07/16/2003 - Kevin Handy
	!		Clean up code.
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

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION GL_OUTP_ACCTSUM

	!
	! Arrays
	!
	DIM GL_YYYY_PP_FILE$(1000%)

	%PAGE

	!******************************************************************
	! Set up data storage areas (MAPs, DIMENSIONs, DECLAREs)
	!******************************************************************

	!
	! CDD inclusions
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE UTL_REPORTX_CDD	UTL_REPORTX

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP	(GL_CHART)	GL_CHART_CDD	GL_CHART

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP	(GL_YYYY_PP)	GL_YYYY_PP_CDD	GL_YYYY_PP

	!
	! Declare some variables
	!
	DECLARE REAL	TOTAL_AMOUNT
	DECLARE STRING	CURRENT_YEAR, CURRENT_PERIOD


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

	FROM_ITEM$ = TRM$(UTL_REPORTX::OPTDEF(0%))

	!++
	! Abstract:FLD01
	!	^*(01) From Account\*
	!	.b
	!	.lm +5
	!	The ^*From Account\* field causes the report to begin printing
	!	with a specific General Ledger Account number.
	!	.b
	!	If the report is to start with the first account in the file, this
	!	field should be left blank.
	!	.lm -5
	!
	! Index:
	!
	!--

	TO_ITEM$ = TRM$(UTL_REPORTX::OPTDEF(1%))

	!++
	! Abstract:FLD02
	!	^*(02) To Account\*
	!	.b
	!	.lm +5
	!	The ^*To Account\* field causes the report to end printing
	!	with a specific General Ledger account number.
	!	.b
	!	If the report is to end with the last General Ledger account number
	!	in the file, this field should be left blank.
	!	.lm -5
	!
	! Index:
	!
	!--

	START_PERIOD$ = LEFT(UTL_REPORTX::OPTDEF(2%), 4%) + "_" + &
		RIGHT(UTL_REPORTX::OPTDEF(2%),5%)
	!++
	! Abstract:FLD03
	!	^*(03) Period\*
	!	.b
	!	.lm +5
	!	The ^*Period\* refers to the accounting time frame that will be considered
	!	when running the desired program.
	!	.b
	!	The format for entry is YYYYPP.
	!	.lm -5
	!
	! Index:
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard Account\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard Account\* field enters a value which
	!	will cause selected General Ledger accounts only to be printed.
	!	.b
	!	If this field is left blank, all General Ledger accounts which have
	!	transactions, unless restricted by the "From Account" and/or "To Account"
	!	report settings, will be included.
	!	.lm -5
	!
	! Index:
	!
	!--

	SRCWLD$ = EDIT$(UTL_REPORTX::OPTDEF(4%), 132%)

	!++
	! Abstract:FLD05
	!	^*(05) Wildcard Source\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard Source\* field in this report setting screen
	!	specifies the source code(s) to be selected. Wildcarding techniques
	!	may be used in the selection, or multiple source codes may be entered
	!	separated with a comma.
	!	.b
	!	Valid source codes are:
	!	.table 3,25
	!	.te
	!	^*AD\*	Asset Depreciation Ledger
	!	.te
	!	^*ADJ\*	Inventory Control Cycle Count Journal
	!	.te
	!	^*BA\*	Billing Agency Journal
	!	.te
	!	^*BC\*	Customer Billing Journal
	!	.te
	!	^*BI\*	Billing Insurance Journal
	!	.te
	!	^*BT\*	Billing Tuition Journal
	!	.te
	!	^*CD\*	Cash Disburments Journal
	!	.te
	!	^*CRJ\*	Cash Receipts Journal
	!	.te
	!	^*IJ\*	Inventory Transaction Journal
	!	.te
	!	^*LB\*	Legal Billing Billed Journal
	!	.te
	!	^*LBSJ\*	Legal Billing Sales Journal
	!	.te
	!	^*PJ\*	Purchases Journal
	!	.te
	!	^*PR\*	From Payroll System Journals
	!	.te
	!	^*RJ\*	Restaurant Management Journal
	!	.te
	!	^*SJ\*	Sales Journal
	!	.te
	!	^*TM\*	Tenant Management Billing Journal
	!	.end table
	!	.lm -5
	!
	! Index:
	!
	!--

	END_PERIOD$ = TRM$(LEFT(UTL_REPORTX::OPTDEF(7%), 4%) + "_" + &
		RIGHT(UTL_REPORTX::OPTDEF(7%), 5%))

	END_PERIOD$ = START_PERIOD$ &
		IF END_PERIOD$ = ""

	!++
	! Abstract:FLD02
	!	^*(02) Period\*
	!	.b
	!	.lm +5
	!	^*Period\* refers to the accounting period that will be considered when
	!	running the report.
	!	.b
	!	The format for entry is YYYYPP.
	!	.lm -5
	!
	! Index:
	!
	!--

	TOTAL_AMOUNT = 0.0
	CURRENT_YEAR = LEFT(YYYY_PP$, 4%)
	CURRENT_PERIOD = RIGHT(YYYY_PP$, 6%)

	%PAGE

	!
	! Open chart file
	!
310	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.OPN"
	USE
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

400	!*******************************************************************
	! Process GL Files
	!*******************************************************************

	CALL READ_DEVICE("GL_YYYY_PP", GL_YYYY_PP.DEV$, STAT%)

	CALL FIND_FILE( GL_YYYY_PP.DEV$ + "GL_*.LED", GL_YYYY_PP_FILE$(), &
		16%, "", "")

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram
	END SELECT

	GL_YYYY_PP_FILE% = VAL%(GL_YYYY_PP_FILE$(0%))

	IF GL_YYYY_PP_FILE% = 0%
	THEN
		GOTO ExitProgram
	END IF

	%PAGE

 ReportTitle:
	!
	! Titles
	!
	TITLE$(1%) = "GL Audit Report"
	TITLE$(2%) = "Period " + TRM$(CURRENT_PERIOD) + &
		" of Year " + CURRENT_YEAR
	TITLE$(3%) = "For source code(s): " + SRCWLD$
	TITLE$(4%) = "GL System"
	TITLE$(5%) = ""

	!
	! Headings
	!
	TITLE$(6%) = "Account number       Date   SC   Reference    " + &
		"    Description                      Xref #     Check " + &
		"       Current         Balance"
	TITLE$(7%) = "."

	!
	! Layouts for printed lines
	!
	LYT_LINE$ = "$Account:018,DTranDate:027,$Source:032," + &
		"$RefNum:049,$Descr:082,$XRefNum:093," + &
		"$CheckNum:100,VAmount:115"
	LYT_LINE1$ = "$Account:018,DTranDate:018,$Source:018," + &
		"$RefNum:018,$Descr:115,$XRefNum:115," + &
		"$CheckNum:115,VAmount:115,VBalance:132"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	FOR LOOP% = 1% TO GL_YYYY_PP_FILE%
		YYYY_PP$ = MID(GL_YYYY_PP_FILE$(LOOP%), 4%, 7%)

		IF YYYY_PP$ >= START_PERIOD$ AND &
			YYYY_PP$ <= END_PERIOD$
		THEN
			GOSUB 17050
			GOTO ExitTotal IF UTL_REPORTX::STAT

			GOSUB PrintTotal
			GOSUB PeriodTotal
			GOTO ExitTotal IF UTL_REPORTX::STAT
			CLOSE GL_YYYY_PP.CH%
		END IF
	NEXT LOOP%

	GOTO ExitTotal

17050	!*******************************************************************
	! Process one period
	!*******************************************************************

	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.OPN"
	USE
		FILENAME$ = "GL_" + YYYY_PP$
		CONTINUE HelpError
	END WHEN

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #GL_YYYY_PP.CH%
		ELSE
			FIND #GL_YYYY_PP.CH%, KEY #0% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
		FILENAME$ = "GL_" + YYYY_PP$
		CONTINUE HelpError
	END WHEN

	TEXT$ = "Starting Period " + YYYY_PP$
	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), "", 0%)
	GOTO 17190 IF UTL_REPORTX::STAT


	FLAG_PRINTED% = 0%
	THIS_ACCT$ = ""
	GRAND_TOTAL = 0.0

 GetNextRec:
17100	!******************************************************************
	! Main report loop starts here
	!******************************************************************

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #GL_YYYY_PP.CH%, REGARDLESS
	USE
		CONTINUE 17190 IF ERR = 11%
		FILENAME$ = "GL_" + YYYY_PP$
		CONTINUE HelpError
	END WHEN

	!
	! Check status
	!
	GOTO 17190 IF UTL_REPORTX::STAT

	!
	! Check current record
	!
	IF TO_ITEM$ <> ""
	THEN
		GOTO GetNextRec IF (GL_YYYY_PP::ACCT > TO_ITEM$)
	END IF

	IF WLDCRD$ <> ""
	THEN
		GOTO GetNextRec IF COMP_STRING(EDIT$(GL_YYYY_PP::ACCT, -1%), &
			WLDCRD$) = 0%
	END IF

	IF SRCWLD$ <> ""
	THEN
		GOTO GetNextRec IF COMP_STRING(EDIT$(GL_YYYY_PP::SOURCE, -1%), &
			SRCWLD$) = 0%
	END IF

	!
	! Print out one line of the report
	!
	GOSUB NewAccount IF GL_YYYY_PP::ACCT <> THIS_ACCT$

17110	!
	! Set up description(s)
	!
	DSC1$ = GL_YYYY_PP::DESCR

17120	TOTAL_AMOUNT = TOTAL_AMOUNT + GL_YYYY_PP::AMOUNT
	PERIOD_TOTAL = GRAND_TOTAL + GL_YYYY_PP::AMOUNT
	GRAND_TOTAL = GRAND_TOTAL + GL_YYYY_PP::AMOUNT

	TEXT$ = GL_YYYY_PP::ACCT + " " + &
		PRNT_DATE(GL_YYYY_PP::TRANDAT, 6%) + " " + &
		GL_YYYY_PP::SOURCE + " " + &
		GL_YYYY_PP::REFNO + " " + &
		LEFT(DSC1$, 30%) + "   " + &
		GL_YYYY_PP::XREFNO + " " + &
		GL_YYYY_PP::CKNO + " " + &
		FORMAT$(GL_YYYY_PP::AMOUNT, "##,###,###.##-")

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO 17190 IF UTL_REPORTX::STAT

	GOTO 17190 IF GL_OUTP_ACCTSUM (OPT_ADDREC, GL_YYYY_PP::ACCT, &
		0.0, GL_YYYY_PP::AMOUNT, 0.0, TITLE$(), UTL_REPORTX) <> &
		CMC$_NORMAL

	FLAG_PRINTED% = -1%

	!
	! Try for next record
	!
	GOTO GetNextRec

17190	RETURN

	%PAGE

17500	!*******************************************************************
	! New account, read and then display
	!*******************************************************************
 NewAccount:
	GOSUB PrintTotal

	GL_CHART::ACCT = GL_YYYY_PP::ACCT
	GL_CHART::DESCR = "??????????????????????????"

	WHEN ERROR IN
		GET #GL_CHART.CH%, KEY #0% EQ GL_YYYY_PP::ACCT, REGARDLESS
	USE
		CONTINUE 17510
	END WHEN

17510	TEXT$ = GL_CHART::ACCT + " " + &
		LEFT(GL_CHART::DESCR, 39%)

	CALL OUTP_LINE(LYT_LINE1$, UTL_REPORTX, TITLE$(), TEXT$, 5%)

	THIS_ACCT$ = GL_YYYY_PP::ACCT

	RETURN

 PrintTotal:
	!
	! Print totals of previous account if necessary
	!
	IF FLAG_PRINTED%
	THEN
		TEXT$ = SPACE$(66%) + &
			"TOTAL----------------------->                      " + &
			FORMAT$(TOTAL_AMOUNT, "##,###,###.##-") + &
			"*"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		TOTAL_AMOUNT = 0.0
		FLAG_PRINTED% = 0%
	END IF

	RETURN

 PeriodTotal:
	!
	! Print totals of previous account if necessary
	!
	TEXT$ = SPACE$(66%) + &
		"PERIOD " + YYYY_PP$ + &
		" TOTAL-------->                      " + &
		FORMAT$(PERIOD_AMOUNT, "##,###,###.##-") + &
		"*"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TOTAL_AMOUNT = 0.0
	FLAG_PRINTED% = 0%

	RETURN

	%PAGE

	!******************************************************************
	! Handle totals and other items before EXITing
	!******************************************************************

 ExitTotal:
	!
	! Print out totals
	!
	TEXT$ = SPACE$(60%) + &
		"GRAND TOTAL----------------------->                      " + &
		FORMAT$(GRAND_TOTAL, "##,###,###.##-") + &
		"*"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	%PAGE

	!
	! Print last page
	!
	V% = GL_OUTP_ACCTSUM(OPT_SUMMARY, "", 0.0, 0.0, 0.0, &
		TITLE$(), UTL_REPORTX)

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

	END
