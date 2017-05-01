1	%TITLE "Job Summary"
	%SBTTL "WP_RPRT_JOBSUMMARY"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1992 BY
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
	! ID:WP0057
	!
	! Abstract:HELP
	!	.lm +5
	!	.b
	!	The ^*Job Summary\* reports jobs which may be ready to be closed, includes
	!	the following information:
	!	.table 3,25
	!	.te
	!	Job Number              Job Description
	!	.te
	!	Type                    Class
	!	.te
	!	Job Date                Status
	!	.te
	!	Close Date              Location
	!	.te
	!	Operator
	!	.te
	!	Line                    Transaction Type
	!	.te
	!	Item Code               Item Code Description
	!	.te
	!	Ordered Quantity        Completed Quantity
	!	.te
	!	Cancelled Quantity      Cost
	!	.te
	!	Balance Quantity        Expected Start
	!	.te
	!	Expected Completion     Job Buyoff Total
	!	.te
	!	Material Line Number    Requisition Number
	!	.te
	!	RecLine                 Product Number
	!	.te
	!	Product Description     Requisition Quantity
	!	.te
	!	Issue Quantity          Cancelled Quantity
	!	.te
	!	Balance Quantity        Amount (Dollars)
	!	.te
	!	Total Material Issued
	!	.te
	!	(in dollars)
	!	.te
	!	WIP Group               GL Account #
	!	.te
	!	GL Description          Beginning Balances
	!	.te
	!	Changes	Total
	!	.te
	!	Actual Costs for:
	!	.te
	!	Burden	Labor
	!	.te
	!	Parts	Raw Material
	!	.te
	!	Total	Buyoff
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Job Summary>Report
	!	.x Report>Job Summary
	!	.x Closed Jobs>Summary Report
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_RPRT_JOBSUMMARY/LINE
	!	$ LINK/EXE=WP_EXE: WP_RPRT_JOBSUMMARY, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE WP_RPRT_JOBSUMMARY.OBJ;*
	!
	! Author:
	!
	!	11/04/92 - Dan Perkins
	!
	! Modification History:
	!
	!	11/17/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	12/02/92 - Dan Perkins
	!		Switched BalQty and Amount fields.  Added a
	!		warning note if out of balance.
	!
	!	12/03/92 - Dan Perkins
	!		Display BalQty to three decimal places.
	!
	!	12/10/92 - Dan Perkins
	!		Display QTY(9%) as the extended cost instead of
	!		multiplying by QTY(2%) to get the extended cost.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/13/96 - Kevin Handy
	!		Reformat source code.
	!		Add comments to source code.
	!
	!	07/28/98 - Kevin Handy
	!		Zero out BOFF insead of BYOFF (which wasn't
	!		used for anyhing)
	!
	!	07/31/98 - Kevin Handy
	!		Swipe a couple of spaces between fields to give
	!		the GL summary another digit for amounts.
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	05/09/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--

	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[SB.OPEN]SB_CONTROL.HB"
	MAP (SB_CONTROL)	SB_CONTROL_CDD		SB_CONTROL

	%INCLUDE "SOURCE:[JC.OPEN]JC_JOB.HB"
	MAP (SB_SUBACCOUNT)	JC_JOB_CDD		JC_JOB

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP (SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT

	%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.HB"
	DECLARE			WP_REGLINE_CDD		WP_REGLINE_READ

	%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.HB"
	DECLARE			WP_REQREGISTER_CDD	WP_REQREGISTER_READ

	%INCLUDE "SOURCE:[SB.OPEN]SB_BALANCE.HB"
	MAP (SB_BALANCE)	SB_BALANCE_CDD		SB_BALANCE

	%INCLUDE "SOURCE:[SB.OPEN]SB_ACCOUNT.HB"
	MAP (SB_ACCOUNT)	SB_ACCOUNT_CDD		SB_ACCOUNT

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	DECLARE			GL_CHART_CDD		GL_CHART_EXAM

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION WP_READ_REGLINE
	EXTERNAL LONG    FUNCTION WP_READ_REQREGISTER
	EXTERNAL LONG    FUNCTION PD_EXAM_PRODUCT
	EXTERNAL LONG    FUNCTION GL_EXAM_CHART

	%PAGE

	ON ERROR GOTO 19000

	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram &
		IF UTL_REPORTX::STAT

	SORT_BY$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	.x Sort By
	!	^*(01) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field causes the report to print in
	!	a selected order. The following values are valid:
	!	.table 3,25
	!	.te
	!	^*C\* - Class
	!	.te
	!	^*J\* - Job Number
	!	.te
	!	^*T\* - Job Type
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field causes the printing
	!	to begin with a selected item.
	!	.b
	!	A blank field causes the report to begin with the first item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field causes the report to end printing
	!	with a selected item.
	!	.b
	!	A blank field causes the report to end with the last item in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field prints a report including selected
	!	items only using the "wildcarding" technique.
	!	.b
	!	For more information on "Wildcarding", see Appendix A.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard
	!
	!--

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[SB.OPEN]SB_CONTROL.OPN"

		GET #SB_CONTROL.CH%, KEY #0% EQ "J", REGARDLESS

		CLOSE SB_CONTROL.CH%
	USE
		FILENAME$ = "SB_CONTROL"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.OPN"
	USE
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

320	WHEN ERROR IN
		%INCLUDE "SOURCE:[SB.OPEN]SB_BALANCE.OPN"
	USE
		FILENAME$ = "SB_BALANCE"
		CONTINUE HelpError
	END WHEN

330	WHEN ERROR IN
		%INCLUDE "SOURCE:[SB.OPEN]SB_ACCOUNT.OPN"
	USE
		FILENAME$ = "SB_ACCOUNT"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	SELECT SORT_BY$

	CASE "J"
		SORT_KEY% = 0%
		ADD_TITLE$ = "BY JOB NUMBER"

	CASE "T"
		SORT_KEY% = 1%
		ADD_TITLE$ = "BY JOB TYPE"

	CASE "C"
		SORT_KEY% = 2%
		ADD_TITLE$ = "BY JOB CLASS"
	END SELECT

	TITLE$(1%) = "JOB SUMMARY REPORT " + ADD_TITLE$
	TITLE$(2%) = "Work In Process System"
	TITLE$(3%) = ""

	TITLE$(4%) = "JobNumber  JobDescription               " + &
		"            Type Class JobDate    Status" + &
		" CloseDate  Location Operator   ReferenceNo"

	TITLE$(5%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	PAGE% = 0%

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			FIND #SB_SUBACCOUNT.CH%, &
				KEY #KEY_NUM% EQ "J", &
				REGARDLESS
		ELSE
			FIND #SB_SUBACCOUNT.CH%, &
				KEY #KEY_NUM% EQ "J" + FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	!
	! Get next record
	!
	QTY_FLAG% = 0%

	WHEN ERROR IN
		GET #SB_SUBACCOUNT.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

	GOTO ExitProgram &
		IF SB_SUBACCOUNT::SUBJECT <> "J"

	SELECT SORT_BY$

	CASE "J"
		GOTO ExitProgram &
			IF (SB_SUBACCOUNT::SUBACCOUNT > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$( &
			SB_SUBACCOUNT::SUBACCOUNT, -1%), WLDCRD$) = 0%

	CASE "T"
		GOTO ExitProgram &
			IF (SB_SUBACCOUNT::TTYPE > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$( &
			SB_SUBACCOUNT::TTYPE, -1%), WLDCRD$) = 0%

	CASE "C"
		GOTO ExitProgram &
			IF (SB_SUBACCOUNT::CLASS > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$( &
			SB_SUBACCOUNT::CLASS, -1%), WLDCRD$) = 0%

	END SELECT

	!
	! Build and print the Header out now
	!
	TEXT$ = JC_JOB::JOB + " " + &
		JC_JOB::DESCR + " " + &
		JC_JOB::TTYPE + "   " + &
		JC_JOB::CLASS + "  " + &
		PRNT_DATE(JC_JOB::BDATE, 8%) + " " + &
		JC_JOB::SSTATUS + "      " + &
		PRNT_DATE(JC_JOB::EDATE, 8%) + " " + &
		JC_JOB::LOCATION + "     " + &
		JC_JOB::OPERATOR + " " + &
		JC_JOB::REFNO

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, PAGE%)
	GOTO ExitProgram &
		IF UTL_REPORTX::STAT

	!*******************************************************************
	! Scan through WP_REGLINE
	!*******************************************************************

	REGLINE_TOTAL = 0.0
	LLINE$        = "    "
	TITLE_FLAG%   = -1%

 ReadRegline:
	GOTO OutaRegline &
		IF WP_READ_REGLINE(JC_JOB::JOB, &
		LLINE$, "GT", WP_REGLINE_READ, QTY()) <> CMC$_NORMAL

	EXTCOST = FUNC_ROUND(QTY(9%), 2%)

	QTY_FLAG% = -1% &
		IF QTY(0%) <> 0.0

	REGLINE_TOTAL = REGLINE_TOTAL + EXTCOST
	LLINE$        = WP_REGLINE_READ::LLINE

	IF TITLE_FLAG%
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

		TEXT$ = "Line TranType Itemcode       " + &
			"Description                " + &
			" OrdQty  CompQty   CanQty       " + &
			"BalQty        Cost  ExpectStart  ExpectComp"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 2%)

		TITLE_FLAG% = 0%

	END IF

	SELECT WP_REGLINE_READ::TTYPE

	CASE "L"
		TTYPE$ = "LABOR   "

	CASE "M"
		TTYPE$ = "MATERIAL"

	END SELECT

	TEXT$ = WP_REGLINE_READ::LLINE + " " + &
		TTYPE$ + " " + &
		WP_REGLINE_READ::ITEMCODE + " " + &
		LEFT(WP_REGLINE_READ::DESCR, 25%) + "  " + &
		FORMAT$(QTY(1%), "###,###") + "  " + &
		FORMAT$(QTY(2%), "###,###") + "  " + &
		FORMAT$(QTY(3%), "###,###") + "  " + &
		FORMAT$(QTY(0%), "<%>##,###.###") + "  " + &
		FORMAT$(EXTCOST, "###,###.##") + "  " + &
		PRNT_DATE(WP_REGLINE_READ::START_DATE, 8%) + "   " + &
		PRNT_DATE(WP_REGLINE_READ::COMP_DATE, 8%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram &
		IF UTL_REPORTX::STAT

	GOTO ReadRegline

 OutaRegline:
	IF TITLE_FLAG% = 0%
	THEN
		TEXT$ = "Job Buyoff Total" + SPACE$(78%) + &
			FORMAT$(REGLINE_TOTAL, "#,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	END IF

	!*******************************************************************
	! Scan through WP_REQREGISTER
	!*******************************************************************

	REQREGISTER_TOTAL = 0.0
	LLINE$ = ""
	REQNUMBER$ = "          "
	REQLINE$ = "    "
	TITLE_FLAG% = -1%

 ReadReqRegister:
	GOTO OutaReadReqRegister &
		IF WP_READ_REQREGISTER(JC_JOB::JOB, &
		LLINE$, REQNUMBER$ + REQLINE$, "GT", &
		WP_REQREGISTER_READ, QTY()) <> CMC$_NORMAL

	EXTCOST = FUNC_ROUND(QTY(9%), 2%)

	QTY_FLAG% = -1% &
		IF QTY(0%) <> 0.0

	REQREGISTER_TOTAL = REQREGISTER_TOTAL + EXTCOST
	LLINE$ = WP_REQREGISTER_READ::LLINE
	REQNUMBER$ = WP_REQREGISTER_READ::REQNUM
	REQLINE$ = WP_REQREGISTER_READ::REQLIN

	IF TITLE_FLAG%
	THEN
		TEXT$ = "Line     ReqNum      RecLine  Product         " + &
			"Description                     " + &
			" ReqQty   IssQty   CanQty       BalQty      Amount"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 2%)

		TITLE_FLAG% = 0%
	END IF

	!
	! Get product description
	!
	V% = PD_EXAM_PRODUCT(WP_REQREGISTER_READ::PRODUCT, PD_PRODUCT_EXAM)

	TEXT$ = WP_REQREGISTER_READ::LLINE + "     " + &
		CONV_STRING(WP_REQREGISTER_READ::REQNUM, CMC$_LEFT) + "  " + &
		WP_REQREGISTER_READ::REQLIN + "     " + &
		WP_REQREGISTER_READ::PRODUCT + "  " + &
		LEFT(PD_PRODUCT_EXAM::DESCRIPTION, 30%) + "  " + &
		FORMAT$(QTY(1%), "###,###") + "  " + &
		FORMAT$(QTY(2%), "###,###") + "  " + &
		FORMAT$(QTY(3%), "###,###") + "  " + &
		FORMAT$(QTY(0%), "<%>##,###.###") + "  " + &
		FORMAT$(EXTCOST, "<%>##,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram &
		IF UTL_REPORTX::STAT

	GOTO ReadReqRegister

 OutaReadReqRegister:
	IF TITLE_FLAG% = 0%
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

		TEXT$ = "Total Material Issued" + SPACE$(95%) + &
			FORMAT$(REQREGISTER_TOTAL, "#,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	END IF

	!*******************************************************************
	! Scan through SB_BALANCE
	!*******************************************************************

	!
	! Figure the account and the amount
	!
	BILL, BOFF, BURD, LABR, PMAT, RMAT, ACT_TOT = 0.0

	BAL_FLAG% = -1%

17100	WHEN ERROR IN
		FIND #SB_BALANCE.CH%, &
			KEY #1% EQ SB_CONTROL::PERIOD + "JC" + JC_JOB::JOB, &
			REGARDLESS
	USE
		CONTINUE GetNextRec IF ERR = 155%
		FILENAME$ = "SB_BALANCE"
		CONTINUE HelpError
	END WHEN

 GetBalRec:
17120	WHEN ERROR IN
		GET #SB_BALANCE.CH%, REGARDLESS
	USE
		CONTINUE PrintLast IF ERR = 11%
		FILENAME$ = "SB_BALANCE"
		CONTINUE HelpError
	END WHEN

	GOTO PrintLast &
		IF SB_BALANCE::PERIOD <> SB_CONTROL::PERIOD OR &
		SB_BALANCE::SYSTEM <> "JC" OR &
		SB_BALANCE::SUBACCOUNT <> JC_JOB::JOB

17200	WHEN ERROR IN
		FIND #SB_ACCOUNT.CH%, KEY #0% EQ "JC", REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "SB_ACCOUNT"
		CONTINUE HelpError
	END WHEN

 NextAcct:
17220	WHEN ERROR IN
		GET #SB_ACCOUNT.CH%, REGARDLESS
	USE
		CONTINUE PrintLast IF ERR = 11%
		FILENAME$ = "SB_ACCOUNT"
		CONTINUE HelpError
	END WHEN

	IF COMP_ARRAY(SB_BALANCE::ACCOUNT, SB_ACCOUNT::ACCOUNT) = 0%
	THEN
		GOTO NextAcct
	END IF

	GOTO PrintLast IF SB_ACCOUNT::SYSTEM <> SB_BALANCE::SYSTEM

	TOT_BAL = SB_BALANCE::AMOUNT + SB_BALANCE::BEG_AMOUNT

	SELECT SB_ACCOUNT::ACCTGROUP

	CASE "BILL"
		BILL = BILL + TOT_BAL

	CASE "BOFF"
		BOFF = BOFF + TOT_BAL

	CASE "BURD"
		BURD = BURD + TOT_BAL

	CASE "DLAB", "ILAB"
		LABR = LABR + TOT_BAL

	CASE "PMAT"
		PMAT = PMAT + TOT_BAL

	CASE "RMAT"
		RMAT = RMAT + TOT_BAL

	END SELECT

	!
	! Print the balance
	!
	IF BAL_FLAG%
	THEN
		TEXT$ = "Group  Account             Description  " + &
			"                                 BegBal " + &
			"    Changes       Total"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		BAL_FLAG% = 0%
	END IF

	!
	! Get GL_CHART description
	!
	V% = GL_EXAM_CHART(SB_BALANCE::ACCOUNT, GL_CHART_EXAM)

	TEXT$ = SB_ACCOUNT::ACCTGROUP + "   " + &
		SB_BALANCE::ACCOUNT + "  " + &
		GL_CHART_EXAM::DESCR + &
		FORMAT$(SB_BALANCE::BEG_AMOUNT, "#,###,###.##") + &
		FORMAT$(SB_BALANCE::AMOUNT, "#,###,###.##") + &
		FORMAT$(TOT_BAL, "#,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOTO GetBalRec

 PrintLast:
	TEXT$ = "Cost                 Burden        Labor        Parts" + &
		"       RawMat         Total       Buyoff"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "Actual           " + &
		FORMAT$(BURD, "###,###.##") + "   " + &
		FORMAT$(LABR, "###,###.##") + "   " + &
		FORMAT$(PMAT, "###,###.##") + "   " + &
		FORMAT$(RMAT, "###,###.##") + "   " + &
		FORMAT$(BURD + LABR + PMAT + RMAT, " ###,###.##") + "   " + &
		FORMAT$(BOFF, "###,###.##")

	IF QTY_FLAG%
	THEN
		TEXT$ = TEXT$ + SPACE$(15%) + "NOT READY TO CLOSE !!"
	END IF

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram &
		IF UTL_REPORTX::STAT

	PAGE% = 999%

	GOTO GetNextRec

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

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	UTL_REPORTX::STAT = -1%
	GOTO Exitprogram

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
