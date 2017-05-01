1	%TITLE "Print General Ledger Sequential Entry Report"
	%SBTTL "GL_RPRT_AUDT00"
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
	! ID:GLAUDT
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This report lists all
	!	transactions in a specific General Ledger file in the order in
	!	which the entries were posted.
	!	.b
	!	The following information is included in the Sequential Entry Report:
	!	.table 30
	!	.te
	!	Batch Number
	!	.te
	!	Account Number
	!	.te
	!	Transaction Date
	!	.te
	!	Source
	!	.te
	!	Reference
	!	.te
	!	Check Number
	!	.te
	!	Description
	!	.te
	!	Cross Reference
	!	.te
	!	Subaccount
	!	.te
	!	Transaction Amount
	!	.te
	!	Running Total Amount
	!	.end table
	!	.LM -15
	!
	! Index:
	!	.x Print>GL Sequential Entry Report
	!	.x GL Sequential Entry>Print
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_RPRT_AUDT00/LINE
	!	$ LINK/EXECUTABLE=GL_EXE: GL_RPRT_AUDT00, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_RPRT_AUDT00.OBJ;*
	!
	! Author:
	!
	!	11/11/86 - B. Craig Larsen
	!
	! Modification history:
	!
	!	01/10/91 - Craig Tanner
	!		Where FILENAME$ = "GL_YYYY_PP" in error handler,
	!		changed to = "GL_" + YYYY_PP$.
	!
	!	04/14/92 - Kevin Handy
	!		Added wildcard for source (to figure out a problem
	!		at Crown).
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/04/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/20/97 - Kevin Handy
	!		Don't need to assign channel for report
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE's
	!
	!	06/22/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP	(GL_YYYY_PP)	GL_YYYY_PP_CDD	GL_YYYY_PP

	!
	! Declare some variables
	!
	DECLARE REAL	RUNNING_TOTAL
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

	FROM_ITEM$ = LEFT(EDIT$(UTL_REPORTX::OPTDEF(0%), 132%), 6%)

	!++
	! Abstract:FLD01
	!	^*(01) From Batch _#\*
	!	.b
	!	.lm +5
	!	The ^*From Batch _#\* field selects a
	!	batch number from which the report is to begin printing.
	!	.b
	!	If the report is to begin with the first batch _# in the file,
	!	this field should be left blank.
	!	.lm -5
	!
	! Index:
	!
	!--

	TO_ITEM$ = LEFT(EDIT$(UTL_REPORTX::OPTDEF(1%), 132%), 6%)

	!++
	! Abstract:FLD02
	!	^*(02) To Batch _#\*
	!	.b
	!	.lm +5
	!	The ^*To Batch _#\* field selects a batch
	!	number with which the report will end printing.
	!	.b
	!	If the report is to end with the last batch number in the file, this
	!	field should be left blank.
	!	.lm -5
	!
	! Index:
	!
	!--

	YYYY_PP$ = LEFT(UTL_REPORTX::OPTDEF(2%), 4%) + "_" + &
		RIGHT(UTL_REPORTX::OPTDEF(2%), 5%)
	!++
	! Abstract:FLD03
	!	^*(03) Period\*
	!	.b
	!	.lm +5
	!	The ^*Period\* refers to the accounting period that will be considered
	!	when running the report.
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
	!	^*(04) Account Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Account Wildcard\* setting prints a General
	!	Ledger Audit Report including selected accounts only, using
	!	wildcard techniques. An ^*_*\* or blank in this setting
	!	will cause an Audit Report to list transactions in ^&all\&
	!	batches.
	!	.b
	!	Note: In lieu of using wildcard techniques to print
	!	an Audit Report, consideration may be given to
	!	using the General Ledger Query option.\*
	!	.lm -5
	!
	! Index:
	!
	!--


	WLDSRC$ = EDIT$(UTL_REPORTX::OPTDEF(4%), 132%)

	!++
	! Abstract:FLD05
	!	^*(05) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Source Wildcard\* setting prints a General
	!	Ledger Audit Report including selected sources only, using
	!	wildcard techniques. An ^*_*\* or blank in this setting
	!	will cause an Audit Report to list transactions in ^&all\&
	!	batches.
	!	.b
	!	Note: In lieu of using wildcard techniques to print
	!	an Audit Report, consideration may be given to
	!	using the General Ledger Query option.\*
	!	.lm -5
	!
	! Index:
	!
	!--


	RUNNING_TOTAL = 0.0
	CURRENT_YEAR = LEFT(YYYY_PP$, 4%)
	CURRENT_PERIOD = RIGHT(YYYY_PP$, 6%)

300	WHEN ERROR IN
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
	TITLE$(1%) = "GL Audit Report"
	TITLE$(2%) = "Period " + CURRENT_PERIOD + " of Year " + CURRENT_YEAR
	TITLE$(3%) = ""

	!
	! Headings
	!
	TITLE$(4%) = "Batch  Acct #             TranDate SC   " + &
		"Reference        Check  Description          " + &
		"XRef # SubCD             Amount        Run Tot"
	TITLE$(5%) = ""

	!
	! Layouts for printed lines
	!
	LYT_LINE$ = "$BatchNum:006,$Account:025,DTranDate:034," + &
		"$Source:039,$RefNum:056,$CheckNum:063,$Descr:084," + &
		"$XRefNum:091,$SubAcct:102,VAmount:116,VTotal:131"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #GL_YYYY_PP.CH%, KEY #4%
		ELSE
			FIND #GL_YYYY_PP.CH%, KEY #4% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
		FILENAME$ = "GL_" + YYYY_PP$
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
		GET #GL_YYYY_PP.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "GL_" + YYYY_PP$
		CONTINUE HelpError
	END WHEN

	!
	! Check status
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Check current record
	!
	GOTO GetNextRec IF (EDIT$(GL_YYYY_PP::BTHNUM, -1%) > TO_ITEM$) &
		AND TO_ITEM$ <> ""

	IF WLDCRD$ <> ""
	THEN
		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(GL_YYYY_PP::ACCT, -1%), &
			WLDCRD$) = 0%
	END IF

	IF WLDSRC$ <> ""
	THEN
		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(GL_YYYY_PP::SOURCE, -1%), &
			WLDSRC$) = 0%
	END IF

	!
	! Print out one line of the report
	!
	RUNNING_TOTAL = RUNNING_TOTAL + GL_YYYY_PP::AMOUNT

	TEXT$ = GL_YYYY_PP::BTHNUM + " " + &
		GL_YYYY_PP::ACCT + " " + &
		PRNT_DATE(GL_YYYY_PP::TRANDAT, 0%) + " " + &
		GL_YYYY_PP::SOURCE + " " + &
		GL_YYYY_PP::REFNO + " " + &
		GL_YYYY_PP::CKNO + " " + &
		LEFT(GL_YYYY_PP::DESCR, 20%) + " " + &
		LEFT(GL_YYYY_PP::XREFNO, 6%) + " " + &
		GL_YYYY_PP::SUBACC + " " + &
		FORMAT$(GL_YYYY_PP::AMOUNT, "##,###,###.##") + &
		FORMAT$(RUNNING_TOTAL, " ###,###,###.##")

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT


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
	! End of report GL_RPRT_AUDT00
	!******************************************************************
	END
