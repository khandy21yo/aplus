1	%TITLE "Daily Audit Report"
	%SBTTL "GL_RPRT_AUDITDAILY"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1993 BY
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
	! ID:GL0005
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Daily Audit Report\* lists the transmittal information for
	!	each batch, which has been posted to a specified General
	!	Ledger file.
	!	.b
	!	The following columns are included:
	!	.table 30
	!	.te
	!	Account Number
	!	.te
	!	Description
	!	.te
	!	Debit Amount
	!	.te
	!	Credit Amount
	!	.end table
	!	.LM -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_RPRT_AUDITDAILY/LINE
	!	$ LINK/EXECUTABLE=GL_EXE: GL_RPRT_AUDITDAILY, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_RPRT_AUDITDAILY.OBJ;*
	!
	! Author:
	!
	!	02/16/93 - Dan Perkins
	!
	! Modification history:
	!
	!	02/26/93 - Kevin Handy
	!		Clean up (Check)
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
	!	06/13/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	12/18/2003 - Kevin Handy
	!		Increase number of digits available to be printed.
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
	EXTERNAL LONG	FUNCTION GL_EXAM_CHART

	!******************************************************************
	! Set up data storage areas (MAPs, DIMENSIONs, DECLAREs)
	!******************************************************************

	!
	! CDD inclusions
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP (GL_YYYY_PP)	GL_YYYY_PP_CDD		GL_YYYY_PP

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	DECLARE			GL_CHART_CDD		GL_CHART_EXAM

	!
	! Define record for the array(s)
	!
	RECORD TOTAL_RECORD
		STRING	FLAG = 1%
		STRING	ACCT = 18%
		REAL	D_QTY
		REAL	D_AMT
		REAL	M_QTY
		REAL	M_AMT
	END RECORD

	!
	! Dimension arrays
	!
	DIM	TOTAL_RECORD	DAILY(100%)

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

	REPORT_DATE$ = EDIT$(DATE_STOREDATE(UTL_REPORTX::OPTDEF(0%)), -1)

	!++
	! Abstract:FLD01
	!	^*(01) Report Date\*
	!	.b
	!	.lm +5
	!	The ^*Report Date\* allows printing
	!	including only those sub accounts with transactions on or before
	!	the Report Date.
	!	.b
	!	A blank field will cause the report to print all
	!	backorders in the register file.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!
	!--

	YYYY_PP$ = LEFT(UTL_REPORTX::OPTDEF(1%), 4%) + "_" + &
		RIGHT(UTL_REPORTX::OPTDEF(1%), 5%)

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

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) From SubAccount _#\*
	!	.b
	!	.lm +5
	!	The ^*From SubAccount _#\* setting selects
	!	a sub account number from which the report is to begin printing.
	!	.b
	!	If the report is to begin with the first sub account _# in the
	!	file, this field should be left blank.
	!	.lm -5
	!
	! Index:
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) To SubAccount _#\*
	!	.b
	!	.lm +5
	!	The ^*To SubAccount _#\* setting selects a
	!	sub account number with which the report is to end.
	!	.b
	!	If the report is to end with the last sub account _# in the
	!	file, this field should be left blank.
	!	.lm -5
	!
	! Index:
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) SubAccount Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*SubAccount Wildcard\* setting prints
	!	the report including selected sub accounts only, using
	!	wildcard techniques. A blank or an _* in this field will
	!	cause the report to list ^&all\& sub accounts.
	!	.lm -5
	!
	! Index:
	!
	!--

	!
	! Get the current period file
	!
300	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.OPN"
	USE
		FILENAME$ = "GL_" + YYYY_PP$
		CONTINUE HelpError
	END WHEN

	%PAGE

	CURRENT_YEAR$ = LEFT(YYYY_PP$, 4%)
	CURRENT_PERIOD$ = RIGHT(YYYY_PP$, 6%)
	TEST_SUB$ = SPACE$(LEN(GL_YYYY_PP::SUBACC) + 1%)
	ARRAY_TOTAL% = 0%
	LIN% = 0%

 ReportTitle:
	!
	! Titles
	!
	TITLE$(1%) = "DAILY AUDIT REPORT AS OF " + PRNT_DATE(REPORT_DATE$, 8%)

	TITLE$(2%) = "Period " + TRM$(CURRENT_PERIOD$) + " of Year " + &
		TRM$(CURRENT_YEAR$)

	TITLE$(3%) = "GL System"
	TITLE$(4%) = ""

	!
	! Headers
	!
	TITLE$(5%) = "Account #           Description              " + &
		"                       Today Qty       MTD Qty       " + &
		"Today Sale         MTD Sale"

	TITLE$(6%) = ""

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #GL_YYYY_PP.CH%, KEY #1%
		ELSE
			FIND #GL_YYYY_PP.CH%, &
				KEY #1% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "GL_" + YYYY_PP$
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	!******************************************************************
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
	GOTO ExitTotal &
		IF (GL_YYYY_PP::SUBACC > TO_ITEM$) AND TO_ITEM$ <> ""

	GOTO GetNextRec &
		IF COMP_ARRAY(EDIT$(GL_YYYY_PP::SUBACC, -1%), WLDCRD$) = 0% &
		AND WLDCRD$ <> ""

	GOTO GetNextRec &
		IF GL_YYYY_PP::TRANDAT > REPORT_DATE$

	!
	! Total up this Sub Account if they have changed
	!
	GOSUB SubAcct &
		IF GL_YYYY_PP::SUBACC <> TEST_SUB$

	TEST_SUB$ = GL_YYYY_PP::SUBACC

	!
	! Search SUBACCT balance list for currently existing account
	!
	IF GL_YYYY_PP::AMOUNT > 0.0
	THEN
		FLAG$ = "D"
	ELSE
		FLAG$ = "C"
	END IF

	GOTO GotAccount &
		IF DAILY(I%)::FLAG + DAILY(I%)::ACCT = &
			FLAG$ + GL_YYYY_PP::ACCT &
			FOR I% = 1% TO ARRAY_TOTAL%

	!
	! Item not found, create it
	!
	I%, ARRAY_TOTAL% = ARRAY_TOTAL% + 1%

	DAILY(I%)::FLAG  = FLAG$
	DAILY(I%)::ACCT  = GL_YYYY_PP::ACCT
	DAILY(I%)::D_QTY = 0.0
	DAILY(I%)::D_AMT = 0.0
	DAILY(I%)::M_QTY = 0.0
	DAILY(I%)::M_AMT = 0.0

 GotAccount:
	!
	! Add credit/debit amounts
	!
	IF GL_YYYY_PP::TRANDAT = REPORT_DATE$
	THEN
		DAILY(I%)::D_AMT = DAILY(I%)::D_AMT + GL_YYYY_PP::AMOUNT
		DAILY(I%)::D_QTY = DAILY(I%)::D_QTY + GL_YYYY_PP::UNITS
	END IF

	DAILY(I%)::M_AMT = DAILY(I%)::M_AMT + GL_YYYY_PP::AMOUNT
	DAILY(I%)::M_QTY = DAILY(I%)::M_QTY + GL_YYYY_PP::UNITS

	!
	! Try for next record
	!
	GOTO GetNextRec

	%PAGE

 ExitTotal:
	!
	! Total up last Sub Account
	!
	GOSUB SubAcct

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

 SubAcct:
	GOTO ExitSubAcct IF ARRAY_TOTAL% = 0%

	ST_D_AMT, ST_M_AMT = 0.0
	CR_D_AMT, DB_D_AMT = 0.0
	CR_M_AMT, DB_M_AMT = 0.0

	!
	! Sort the array, CREDITS first, then DEBITS
	!
	FOR I% = 1% TO ARRAY_TOTAL% - 1%

		INDEX% = I%
		DAILY(0%) = DAILY(I%)

		FOR J% = I% + 1% TO ARRAY_TOTAL%

			IF DAILY(J%)::FLAG + DAILY(J%)::ACCT < &
				DAILY(0%)::FLAG + DAILY(0%)::ACCT
			THEN
				DAILY(0%) = DAILY(J%)
				INDEX% = J%
			END IF

		NEXT J%

		DAILY(INDEX%) = DAILY(I%)
		DAILY(I%)     = DAILY(0%)

	NEXT I%

	!
	! Print title for batch
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
		"SUBACCOUNT NUMBER:  " + TEST_SUB$, LIN%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

	FLAG$ = "C"

	FOR I% = 1% TO ARRAY_TOTAL%

		IF DAILY(I%)::FLAG <> FLAG$
		THEN
			GOSUB SubTotal
			FLAG$ = "D"
		END IF

		!
		! Get account description
		!
		V% = GL_EXAM_CHART(DAILY(I%)::ACCT, GL_CHART_EXAM)

		TEXT$ = DAILY(I%)::ACCT + "     " + &
			GL_CHART_EXAM::DESCR + &
			FORMAT$(DAILY(I%)::D_QTY, "<%>###,###,###.#") + &
			FORMAT$(DAILY(I%)::M_QTY, "<%>###,###,###.#") + &
			FORMAT$(DAILY(I%)::D_AMT, "##,###,###,###.##") + &
			FORMAT$(DAILY(I%)::M_AMT, "##,###,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		ST_D_AMT = ST_D_AMT + DAILY(I%)::D_AMT
		ST_M_AMT = ST_M_AMT + DAILY(I%)::M_AMT

	NEXT I%

	FLAG$ = "D"
	GOSUB SubTotal

 ExitSubAcct:
	!
	! Initialize for new batch group
	!
	FOR I% = 1% TO ARRAY_TOTAL%

		DAILY(I%)::FLAG  = ""
		DAILY(I%)::ACCT  = ""
		DAILY(I%)::D_QTY = 0.0
		DAILY(I%)::D_AMT = 0.0
		DAILY(I%)::M_QTY = 0.0
		DAILY(I%)::M_AMT = 0.0

	NEXT I%

	ARRAY_TOTAL% = 0%

	RETURN

	%PAGE

 SubTotal:
	SELECT FLAG$

	CASE "C"
		TEXT$ = "CREDIT TOTAL: "
		CR_D_AMT = CR_D_AMT + ST_D_AMT
		CR_M_AMT = CR_M_AMT + ST_M_AMT

	CASE "D"
		TEXT$ = " DEBIT TOTAL: "
		DB_D_AMT = DB_D_AMT + ST_D_AMT
		DB_M_AMT = DB_M_AMT + ST_M_AMT

	END SELECT

	TEXT$ = SPACE$(77%) + TEXT$ + &
		FORMAT$(ST_D_AMT, "##,###,###,###.##") + &
		FORMAT$(ST_M_AMT, "##,###,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

	ST_D_AMT, ST_M_AMT = 0.0

	IF FLAG$ = "D"
	THEN
		TEXT$ = SPACE$(88%) + "+/-" + &
			FORMAT$(CR_D_AMT + DB_D_AMT, "##,###,###,###.##") + &
			FORMAT$(CR_M_AMT + DB_M_AMT, "##,###,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		CR_D_AMT, DB_D_AMT = 0.0
		CR_M_AMT, DB_M_AMT = 0.0

	END IF

	LIN% = 999%

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

32767	END
