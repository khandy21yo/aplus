1	%TITLE "WIP Closing Journal Report"
	%SBTTL "WP_RPRT_CLOSEREPORT"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1996 BY
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
	! Software Solutions, Inc. assumes no responsibility for the
	! use or reliability of its software on equipment which is
	! not supported by Software Solutions, Inc.
	!
	!++
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This program prints the WIP Closing Journal
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_RPRT_CLOSEREPORT
	!	$ LINK/EXECUTABLE=WP_EXE:*.EXE WP_RPRT_CLOSEREPORT, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE WP_RPRT_CLOSEREPORT.OBJ;*
	!
	! Author:
	!
	!	08/21/96 - Kevin Handy
	!		Based upon WP_OUTP_CLOSEJOUR.
	!
	! Modification History:
	!
	!	08/25/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	06/16/98 - Kevin Handy
	!		Increase size of batch number from 2 to 8 characters
	!
	!	06/16/98 - Kevin Handy
	!		QTY(9) is now extended cost.
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	05/19/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!		Add several REGARDLESS clauses
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

	%INCLUDE "SOURCE:[WP.OPEN]WP_CLOSEJOUR.HB"
	MAP (WP_CLOSEJOUR)	WP_CLOSEJOUR_CDD	WP_CLOSEJOUR

	%INCLUDE "SOURCE:[WP.OPEN]WP_CLOSELINE.HB"
	MAP (WP_CLOSELINE)	WP_CLOSELINE_CDD	WP_CLOSELINE

	%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.HB"
	MAP (WP_REGLINE)	WP_REGLINE_CDD		WP_REGLINE
	DECLARE			WP_REGLINE_CDD		WP_REGLINE_READ

	%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.HB"
	MAP (WP_REQREGISTER)	WP_REQREGISTER_CDD	WP_REQREGISTER
	DECLARE			WP_REQREGISTER_CDD	WP_REQREGISTER_READ

	%INCLUDE "SOURCE:[JC.OPEN]JC_JOB.HB"
	MAP (SB_SUBACCOUNT)	JC_JOB_CDD		JC_JOB

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP (SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT

	%INCLUDE "SOURCE:[SB.OPEN]SB_BALANCE.HB"
	MAP (SB_BALANCE)	SB_BALANCE_CDD		SB_BALANCE

	%INCLUDE "SOURCE:[SB.OPEN]SB_ACCOUNT.HB"
	MAP (SB_ACCOUNT)	SB_ACCOUNT_CDD		SB_ACCOUNT

	%INCLUDE "SOURCE:[JC.OPEN]JC_TYPE.HB"
	MAP (JC_TYPE)		JC_TYPE_CDD		JC_TYPE

	%INCLUDE "SOURCE:[JC.OPEN]JC_CLASS.HB"
	MAP (JC_CLASS)		JC_CLASS_CDD		JC_CLASS

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	DECLARE			GL_CHART_CDD		GL_CHART_EXAM

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	DECLARE			UTL_LOCATION_CDD	UTL_LOCATION_EXAM

	%INCLUDE "SOURCE:[WP.OPEN]WP_CONTROL.HB"
	MAP (WP_CONTROL)	WP_CONTROL_CDD		WP_CONTROL

	%INCLUDE "SOURCE:[BM.OPEN]BM_RELATION.HB"
	MAP (BM_RELATION)	BM_RELATION_CDD		BM_RELATION

	%INCLUDE "SOURCE:[BM.OPEN]BM_CONTROL.HB"
	MAP (BM_CONTROL)	BM_CONTROL_CDD		BM_CONTROL

	%INCLUDE "SOURCE:[SB.OPEN]SB_CONTROL.HB"
	MAP (SB_CONTROL)	SB_CONTROL_CDD		SB_CONTROL

	%INCLUDE "SOURCE:[BM.OPEN]BM_PRODOPER.HB"
	DECLARE			BM_PRODOPER_CDD		BM_PRODOPER_READ

	%INCLUDE "SOURCE:[PR.OPEN]PR_OPER.HB"
	DECLARE			PR_OPER_CDD		PR_OPER_READ

	!
	! Common statements
	!
	COM (CH_WP_CLOSEJOUR)        WP_CLOSEJOUR.CH%
	COM (CH_JC_TYPE_READ)        JC_TYPE.CH%
	COM (CH_JC_CLASS_READ)       JC_CLASS.CH%

	COM (CH_WP_CLOSELINE) &
		WP_CLOSELINE.CH%, &
		WP_CLOSELINE.READONLY%

	COM (BATCH_WP_CLOSEJOUR) &
		BATCH_NO$ = 8%


	COM (CH_BM_CONTROL_READ)	BM_CONTROL.CH%
	COM (CH_BM_RELATION_READ)	BM_RELATION.CH%
	COM (CH_SB_ACCOUNT_READ)	SB_ACCOUNT.CH%
	COM (CH_SB_BALANCE_READ)	SB_BALANCE.CH%
	COM (CH_SB_CONTROL_READ) &
		SB_CONTROL.CH%, &
		SYSTEM$ = 2%, &
		PERIOD$ = 6%

	COM (CH_WP_SB_SUBACCOUNT)	SB_SUBACCOUNT.CH%
	COM (CH_WP_CONTROL)		WP_CONTROL.CH%
	COM (CH_WP_REGLINE_READ)	WP_REGLINE.CH%
	COM (CH_WP_REGREGISTER_READ)	WP_REGREGISTER.CH%
	COM (CH_WP_REQREGISTER_READ)	WP_REQREGISTER.CH%


	!
	! External functions
	!
	EXTERNAL LONG FUNCTION WP_READ_REGLINE
	EXTERNAL LONG FUNCTION WP_READ_REQREGISTER
	EXTERNAL LONG FUNCTION PD_EXAM_PRODUCT
	EXTERNAL LONG FUNCTION UTL_EXAM_LOCATION
	EXTERNAL LONG FUNCTION GL_EXAM_CHART
	EXTERNAL LONG FUNCTION PD_EXAM_PRODUCT
	EXTERNAL LONG FUNCTION BM_READ_PRODOPER
	EXTERNAL LONG FUNCTION PR_READ_OPERATION
	EXTERNAL REAL FUNCTION PC_READ_COST

	!
	! Some local definitions
	!
	RECORD COMPONENT_RECORD
		STRING NUMBER = 14%
		REAL   QUANTITY
	END RECORD

	RECORD BOM_RECORD
		STRING	PRODUCT = 14%
		RFA	RFA_LEVEL
		REAL	QTY
	END RECORD

	RECORD ISSUE_RECORD
		STRING	PRODUCT
		REAL	QTY
		REAL	LABOR
		REAL	HOURS
	END RECORD

	DIM COMPONENT_RECORD	COMPONENT(5000%)
	DIM BOM_RECORD		BOM(400%)
	DIM ISSUE_RECORD	ISSUE(1000%)

	%PAGE

	ON ERROR GOTO 19000

 Init:	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitFunction IF UTL_REPORTX::STAT

	!
	! Set up from user input
	!
	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Job\*
	!	.b
	!	.lm +5
	!	The ^*From Job\* field enters the period with which
	!	the report will begin printing.  A blank field causes the report to begin
	!	with the first period in the file.
	!	.lm -5
	!
	! Index:
	!
	!--
	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Job\*
	!	.b
	!	.lm +5
	!	The ^*To Job\* field enters the period with which
	!	the report will end. A blank field causes the report to end with
	!	the last period in the file.
	!	.lm -5
	!
	! Index:
	!
	!--
	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(5%), 132%)

	!++
	! Abstract:FLD06
	!	^*(06) Calc Var (Y,N)\*
	!	.lm +5
	!	.b
	!	The ^*Calc[ulate] Var[iances] (Y,N)\* field
	!	prints a journal report before and after
	!	variances have been calculated.
	!	.lm -5
	!
	! Index:
	!	.x Variances>Calculate
	!	.x Calculate>Variances
	!
	!--

	!***************************************************************
	! Open Report files
	!***************************************************************

	!
	! Open other necessary files
	!
600	WHEN ERROR IN
		%INCLUDE "SOURCE:[WP.OPEN]WP_CLOSEJOUR.OPN"
	USE
		FILENAME$ = "WP_CLOSEJOUR"
		CONTINUE HelpError
	END WHEN

610	WHEN ERROR IN
		%INCLUDE "SOURCE:[WP.OPEN]WP_CLOSELINE.OPN"
	USE
		CONTINUE 620 IF ERR = 5%
		FILENAME$ = "WP_CLOSELINE"
		CONTINUE HelpError
	END WHEN

620	WHEN ERROR IN
		%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.OPN"
	USE
		FILENAME$ = "JC_JOB"
		CONTINUE HelpError
	END WHEN

650	WHEN ERROR IN
		%INCLUDE "SOURCE:[JC.OPEN]JC_TYPE.OPN"
	USE
		CONTINUE 660 IF ERR = 5%
		FILENAME$ = "JC_TYPE"
		CONTINUE HelpError
	END WHEN

660	WHEN ERROR IN
		%INCLUDE "SOURCE:[JC.OPEN]JC_CLASS.OPN"
	USE
		CONTINUE 730 IF ERR = 5%
		FILENAME$ = "JC_CLASS"
		CONTINUE HelpError
	END WHEN

	!
	! Open relation file
	!
730	WHEN ERROR IN
		%INCLUDE "SOURCE:[BM.OPEN]BM_RELATION.OPN"
	USE
		FILENAME$ = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

	!
	! Open req register line file
	!
731	WHEN ERROR IN
		%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.OPN"
	USE
		FILENAME$ = "WP_REQREGISTER"
		CONTINUE HelpError
	END WHEN

	!
	! Open register line file
	!
732	WHEN ERROR IN
		%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.OPN"
	USE
		FILENAME$ = "WP_REQREGISTER"
		CONTINUE HelpError
	END WHEN

	!
	! Open WIP balance file
	!
733	WHEN ERROR IN
		%INCLUDE "SOURCE:[SB.OPEN]SB_BALANCE.OPN"
	USE
		FILENAME$ = "SB_BALANCE"
		CONTINUE HelpError
	END WHEN

	!
	! Open WIP control file
	!
734	WHEN ERROR IN
		%INCLUDE "SOURCE:[WP.OPEN]WP_CONTROL.OPN"
		GET #WP_CONTROL.CH%, RECORD 1%, REGARDLESS
		CLOSE #WP_CONTROL.CH%
	USE
		FILENAME$ = "WP_CONTROL"
		CONTINUE HelpError
	END WHEN

	!
	! Open BM control file
	!
735	WHEN ERROR IN
		%INCLUDE "SOURCE:[BM.OPEN]BM_CONTROL.OPN"
		GET #BM_CONTROL.CH%, RECORD 1%, REGARDLESS
		CLOSE #BM_CONTROL.CH%
	USE
		FILENAME$ = "BM_CONTROL"
		CONTINUE HelpError
	END WHEN

	!
	! Open SB control file
	!
736	WHEN ERROR IN
		%INCLUDE "SOURCE:[SB.OPEN]SB_CONTROL.OPN"
		GET #SB_CONTROL.CH%, KEY #0% EQ "JC", REGARDLESS
		SYSTEM$ = SB_CONTROL::SYSTEM
		PERIOD$ = SB_CONTROL::PERIOD
		CLOSE #SB_CONTROL.CH%
	USE
		FILENAME$ = "SB_CONTROL"
		CONTINUE HelpError
	END WHEN

	!
	! Open WIP account file
	!
738	WHEN ERROR IN
		%INCLUDE "SOURCE:[SB.OPEN]SB_ACCOUNT.OPN"
	USE
		FILENAME$ = "SB_ACCOUNT"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "CLOSING JOURNAL REPORT"
	TITLE$(2%) = "BATCH No. " + BATCH_NO$
	TITLE$(4%) = "Work In Process System"
	TITLE$(5%) = ""
	TITLE$(6%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	PAGE% = 0%

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #WP_CLOSEJOUR.CH%, KEY #0%
		ELSE
			FIND #WP_CLOSEJOUR.CH%, &
				KEY #0% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE ExitFunction IF ERR = 155%
		FILENAME$ = "WP_CLOSEJOUR"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	WHEN ERROR IN
		GET #WP_CLOSEJOUR.CH%, REGARDLESS
	USE
		CONTINUE ExitFunction IF ERR = 11%
		FILENAME$ = "WP_CLOSEJOUR"
		CONTINUE HelpError
	END WHEN

	GOTO ExitFunction IF (WP_CLOSEJOUR::JOB > TO_ITEM$) AND TO_ITEM$ <> ""

	!
	! Get other job information
	!
17100	WHEN ERROR IN
		GET #SB_SUBACCOUNT.CH%, &
			KEY #0% EQ "J" + WP_CLOSEJOUR::JOB, &
			REGARDLESS
	USE
		FILENAME$ = "JC_JOB"
		CONTINUE HelpError
	END WHEN

17110	JC_TYPE::DESCR = SPACE$(LEN(JC_TYPE::DESCR))

	WHEN ERROR IN
		GET #JC_TYPE.CH%, &
			KEY #0% EQ JC_JOB::TTYPE, &
			REGARDLESS
	USE
		CONTINUE 17120 IF ERR = 9% OR ERR = 155%
		FILENAME$ = "JC_TYPE"
		CONTINUE HelpError
	END WHEN

17120	JC_CLASS::DESCR = SPACE$(LEN(JC_CLASS::DESCR))

	WHEN ERROR IN
		GET #JC_CLASS.CH%, &
			KEY #0% EQ JC_JOB::CLASS, &
			REGARDLESS
	USE
		CONTINUE GetLocation IF ERR = 9% OR ERR = 155%
		FILENAME$ = "JC_CLASS"
		CONTINUE HelpError
	END WHEN

 GetLocation:
	!
	! Get Location information
	!
	V% = UTL_EXAM_LOCATION(JC_JOB::LOCATION, UTL_LOCATION_EXAM)

	TITLE$(3%) = "Job No. " + WP_CLOSEJOUR::JOB

	TEXT$ = SPACE$(20%) + "Job#:             " + WP_CLOSEJOUR::JOB

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, PAGE%)

	TEXT$ = SPACE$(20%) + "Job Description:  " + JC_JOB::DESCR

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = SPACE$(20%) + "Type:             " + JC_JOB::TTYPE + &
		"       " + JC_TYPE::DESCR

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = SPACE$(20%) + "Class:            " + JC_JOB::CLASS + &
		"     " + JC_CLASS::DESCR

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = SPACE$(20%) + "Location:         " + JC_JOB::LOCATION + &
		"     " + UTL_LOCATION_EXAM::LOCNAME

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = SPACE$(20%) + "Reference Number: " + JC_JOB::REFNO

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = SPACE$(20%) + "Begin Date:       " + &
		PRNT_DATE(JC_JOB::BDATE, 8%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = SPACE$(20%) + "Close Date:       " + &
		PRNT_DATE(WP_CLOSEJOUR::CLOSEDATE, 8%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = SPACE$(20%) + "Operator:         " + WP_CLOSEJOUR::OPERATOR

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	GOTO ExitFunction IF UTL_REPORTX::STAT

17200	!
	! Do the variance for one line
	!
	GOSUB CalcVariance

	!
	! Get the requisition info from the WP_REQREGISTER
	!
	ISSUETOTAL = 0.0
	LLINE$ = ""
	REQNUMBER$ = "          "
	REQLINE$ = "    "
	TITLE_FLAG% = -1%

 ReadReqRegister:
	GOTO OutaReadReqRegister IF WP_READ_REQREGISTER(WP_CLOSEJOUR::JOB, &
		LLINE$, REQNUMBER$ + REQLINE$, "GT", &
		WP_REQREGISTER_READ, QTY()) <> CMC$_NORMAL

	ISSUETOTAL = ISSUETOTAL + QTY(9%)
	LLINE$ = WP_REQREGISTER_READ::LLINE
	REQNUMBER$ = WP_REQREGISTER_READ::REQNUM
	REQLINE$ = WP_REQREGISTER_READ::REQLIN

	IF TITLE_FLAG%
	THEN
		TEXT$ = "Line     ReqNum      RecLine  Product         " + &
			"Description                     " + &
			" ReqQty   IssQty   CanQty      Amount   BalQty"

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
		FORMAT$(QTY(9%), "<%>##,###.##") + "  " + &
		FORMAT$(QTY(0%), "<%>##,###.###")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 2%)
	GOTO ExitFunction IF UTL_REPORTX::STAT

	GOTO ReadReqRegister

 OutaReadReqRegister:
	IF TITLE_FLAG% = 0%
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

		TEXT$ = "Total Material Issued" + SPACE$(82%) + &
			FORMAT$(ISSUETOTAL, "#,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	END IF

	!
	! Gonna do the CLOSE lines now
	!
17300	TOTAL, VARTOT, BURDTOT, LABETOT, LABRTOT, MATTOT = 0.0

	WHEN ERROR IN
		FIND #WP_CLOSELINE.CH%, &
			KEY #0% EQ WP_CLOSEJOUR::JOB, &
			REGARDLESS
	USE
		CONTINUE ExitCloseLine IF ERR = 9% OR ERR = 155%
		FILENAME$ = "WP_CLOSELINE"
		CONTINUE HelpError
	END WHEN

	TEXT$ = "Variances"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 5%)

 ReadCloseLine:
17320	WHEN ERROR IN
		GET #WP_CLOSELINE.CH%, REGARDLESS
	USE
		CONTINUE ExitCloseLine IF ERR = 11%
		FILENAME$ = "WP_CLOSELINE"
		CONTINUE HelpError
	END WHEN

	GOTO ExitCloseLine IF WP_CLOSELINE::JOB <> WP_CLOSEJOUR::JOB

	TOTAL = TOTAL + WP_CLOSELINE::VAMOUNT

	IF WP_CLOSELINE::LFLAG = "V"
	THEN
		SELECT WP_CLOSELINE::VCLASS

		CASE "BURD"
			BURDTOT = BURDTOT + WP_CLOSELINE::VAMOUNT

		CASE "LABE"
			LABETOT = LABETOT + WP_CLOSELINE::VAMOUNT

		CASE "LABR"
			LABRTOT = LABRTOT + WP_CLOSELINE::VAMOUNT

		CASE "MAT"
			MATTOT = MATTOT + WP_CLOSELINE::VAMOUNT

		END SELECT
	END IF

	!
	! Get GL_CHART description
	!
	V% = GL_EXAM_CHART(WP_CLOSELINE::VACCT, GL_CHART_EXAM)

	TEXT$ = WP_CLOSELINE::VCLASS + "  " + &
		WP_CLOSELINE::VACCT + "  " + &
		GL_CHART_EXAM::DESCR + "  " + &
		FORMAT$(WP_CLOSELINE::VAMOUNT, "###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitFunction IF UTL_REPORTX::STAT

	GOTO ReadCloseLine

 ExitCloseLine:
	TEXT$ = "Total" + SPACE$(LEN(WP_CLOSELINE::VCLASS + "  " + &
		WP_CLOSELINE::VACCT + "  " + &
		GL_CHART_EXAM::DESCR + "  ") - 5%) + &
		FORMAT$(TOTAL, "###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	GOTO ExitFunction IF UTL_REPORTX::STAT

	!
	! Print Out the Statistics
	!
	TEXT$ = "Cost                           Burden               " + &
		"Labor               Parts              RawMat             " + &
		"  Total"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3%)

	STD_TOTAL = WP_CLOSEJOUR::STDBURDEN + WP_CLOSEJOUR::STDLABOR + &
		WP_CLOSEJOUR::STDPARTS + WP_CLOSEJOUR::STDRAWMAT

	TEXT$ = "Standard         " + SPACE$(10%) + &
		FORMAT$(WP_CLOSEJOUR::STDBURDEN, "###,###.##") + SPACE$(10%) + &
		FORMAT$(WP_CLOSEJOUR::STDLABOR, "###,###.##") + SPACE$(10%) + &
		FORMAT$(WP_CLOSEJOUR::STDPARTS, "###,###.##") + SPACE$(10%) + &
		FORMAT$(WP_CLOSEJOUR::STDRAWMAT, "###,###.##") + SPACE$(10%) + &
		FORMAT$(STD_TOTAL, "###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	ACT_TOTAL = WP_CLOSEJOUR::ACTBURDEN + WP_CLOSEJOUR::ACTLABOR + &
		WP_CLOSEJOUR::ACTPARTS + WP_CLOSEJOUR::ACTRAWMAT

	TEXT$ = "Actual           " + SPACE$(10%) + &
		FORMAT$(WP_CLOSEJOUR::ACTBURDEN, "###,###.##") + SPACE$(10%) + &
		FORMAT$(WP_CLOSEJOUR::ACTLABOR, "###,###.##") + SPACE$(10%) + &
		FORMAT$(WP_CLOSEJOUR::ACTPARTS, "###,###.##") + SPACE$(10%) + &
		FORMAT$(WP_CLOSEJOUR::ACTRAWMAT, "###,###.##") + SPACE$(10%) + &
		FORMAT$(ACT_TOTAL, "###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	VARTOT = BURDTOT + LABETOT + LABRTOT + MATTOT

	TEXT$ = "Variance             " + &
		"Burd  " + FORMAT$(BURDTOT, "###,###.##") + "  " + &
		"LabEff  " + FORMAT$(LABETOT, "###,###.##") + SPACE$(20%) + &
		"Material  " + FORMAT$(MATTOT, "###,###.##") + "   " + &
		"Total  " + FORMAT$(VARTOT, "###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = SPACE$(39%) + "LabRat  " + FORMAT$(LABRTOT, "###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	GOTO ExitFunction IF UTL_REPORTX::STAT

	PAGE% = 999%

	GOTO GetNextRec

 ExitFunction:
	CALL OUTP_FINISH(UTL_REPORTX)

 Exit2:
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

	!*******************************************************************
	! Calculate/print variance for one line
	!*******************************************************************
 CalcVariance:

	!
	! Initialize standard and actual to zero
	!
	WP_STDBURDEN = 0.0
	WP_STDLABOR  = 0.0
	WP_STDPARTS  = 0.0
	WP_STDRAWMAT = 0.0

	WP_ACTBURDEN = 0.0
	WP_ACTLABOR  = 0.0
	WP_ACTPARTS  = 0.0
	WP_ACTRAWMAT = 0.0

	!!
	BUYOFFTOTAL = 0.0
	LLINE$ = "    "
	TITLE_FLAG% = -1%
	!!

	TESTLINE$ = SPACE$(LEN(WP_REGLINE_READ::LLINE))

 ReadRegLineX:
	GOTO StartClose IF WP_READ_REGLINE(WP_CLOSEJOUR::JOB, &
		TESTLINE$, "GT", WP_REGLINE_READ, QTY()) <> CMC$_NORMAL

	!!
	BUYOFFTOTAL = FUNC_ROUND(BUYOFFTOTAL + QTY(9%), 2%)
	LLINE$ = WP_REGLINE_READ::LLINE

	IF TITLE_FLAG%
	THEN
		TEXT$ = "Line  TranType  Itemcode        " + &
			"Description                " + &
			" OrdQty  CompQty   CanQty        Cost   BalQty  " + &
			"ExpectStart  ExpectComp"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 2%)

		TITLE_FLAG% = 0%

	END IF

	SELECT WP_REGLINE_READ::TTYPE

	CASE "L"
		TTYPE$ = "LABOR   "

	CASE "M"
		TTYPE$ = "MATERIAL"

	END SELECT

	TEXT$ = WP_REGLINE_READ::LLINE + "  " + &
		TTYPE$ + "  " + &
		WP_REGLINE_READ::ITEMCODE + "  " + &
		LEFT(WP_REGLINE_READ::DESCR, 25%) + "  " + &
		FORMAT$(QTY(1%), "###,###") + "  " + &
		FORMAT$(QTY(2%), "###,###") + "  " + &
		FORMAT$(QTY(3%), "###,###") + "  " + &
		FORMAT$(QTY(9%), "###,###.##") + "  " + &
		FORMAT$(QTY(0%), "<%>##,###") + "  " + &
		PRNT_DATE(WP_REGLINE_READ::START_DATE, 8%) + "   " + &
		PRNT_DATE(WP_REGLINE_READ::COMP_DATE, 8%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitFunction IF UTL_REPORTX::STAT
	!!

	IF FUNC_ROUND(QTY(0%), 2%) <> 0.0
	THEN
		MESSAGE$ = "Rem buyoff qty " + &
			NUM1$(FUNC_ROUND(QTY(0%), 2%)) + " at job line " + &
			WP_REGLINE_READ::LLINE

		EXIT_STATUS = CMC$_TERMINATED
		GOTO ExitVariance
	END IF


	TESTLINE$, LASTLINE$ = WP_REGLINE_READ::LLINE

	REQLINE$ = SPACE$(LEN(WP_REQREGISTER_READ::REQNUM + &
		WP_REQREGISTER_READ::REQLIN))

 ReadReqLine:
	GOTO ReadRegLineX IF LASTLINE$ <> TESTLINE$ OR &
		WP_READ_REQREGISTER (WP_CLOSEJOUR::JOB, LASTLINE$, REQLINE$, &
		"GT", WP_REQREGISTER_READ, QTY()) <> CMC$_NORMAL

	IF FUNC_ROUND(QTY(0%), 2%) <> 0.0
	THEN
		MESSAGE$ = "Rem issue qty " + &
			NUM1$(FUNC_ROUND(QTY(0%), 2%)) + " at req line " + &
			TRM$(WP_REQREGISTER_READ::REQNUM) + "," + &
			WP_REQREGISTER_READ::REQLIN

		EXIT_STATUS = CMC$_TERMINATED
		GOTO ExitVariance
	END IF

	LASTLINE$ = WP_REQREGISTER_READ::LLINE
	REQLINE$  = WP_REQREGISTER_READ::REQNUM + WP_REQREGISTER_READ::REQLIN

	GOTO ReadReqLine

 StartClose:

	IF TITLE_FLAG% = 0%
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

		TEXT$ = "Job Buyoff Total" + SPACE$(68%) + &
			FORMAT$(BUYOFFTOTAL, "#,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	END IF

	!*******************************************************************
	! Calculate ACTUAL totals, put them in WP_CLOSELINE
	! (Reads from SB_BALANCE register.
	!*******************************************************************

17705	TOTAL_LABOR = 0.0
	TOTAL_BURDEN = 0.0
	TOTAL_MAT = 0.0
	TOTAL_RMAT = 0.0
	TOTAL_LABOR_HOURS = 0.0

	WHEN ERROR IN
		FIND #SB_BALANCE.CH%, &
			KEY #1% EQ PERIOD$ + SYSTEM$ + WP_CLOSEJOUR::JOB, &
			REGARDLESS
	USE
		CONTINUE 17800 IF ERR = 155%
		FILENAME$ = "SB_BALANCE"
		CONTINUE HelpError
	END WHEN

 NextBalance:
17707	WHEN ERROR IN
		GET #SB_BALANCE.CH%, REGARDLESS
	USE
		CONTINUE 17800 IF ERR = 11%
		FILENAME$ = "SB_BALANCE"
		CONTINUE HelpError
	END WHEN

	GOTO 17800 &
		IF (SB_BALANCE::PERIOD <> PERIOD$) OR &
		(SB_BALANCE::SYSTEM <> SYSTEM$) OR &
		(SB_BALANCE::SUBACCOUNT <> WP_CLOSEJOUR::JOB)

	RESET #SB_ACCOUNT.CH%

17710	WHEN ERROR IN
		GET #SB_ACCOUNT.CH%, REGARDLESS
	USE
		CONTINUE NextBalance IF ERR = 11% OR ERR = 9%
		FILENAME$ = "SB_ACCOUNT"
		CONTINUE HelpError
	END WHEN

17720	IF COMP_STRING(SB_BALANCE::ACCOUNT, SB_ACCOUNT::ACCOUNT)
	THEN
		SELECT SB_ACCOUNT::ACCTGROUP

		CASE "BURD", "ILAB"
			TOTAL_BURDEN = FUNC_ROUND(TOTAL_BURDEN + &
				SB_BALANCE::AMOUNT + SB_BALANCE::BEG_AMOUNT, 2%)

		CASE "DLAB"
			TOTAL_LABOR = FUNC_ROUND(TOTAL_LABOR + &
				SB_BALANCE::AMOUNT + SB_BALANCE::BEG_AMOUNT, 2%)

			TOTAL_LABOR_HOURS = FUNC_ROUND(TOTAL_LABOR_HOURS + &
				SB_BALANCE::HOURS + SB_BALANCE::BEG_HOURS, 2%)

		CASE "PMAT"
			TOTAL_MAT = FUNC_ROUND(TOTAL_MAT + &
				SB_BALANCE::AMOUNT + SB_BALANCE::BEG_AMOUNT, 2%)

		CASE "RMAT"
			TOTAL_RMAT = FUNC_ROUND(TOTAL_RMAT + &
				SB_BALANCE::AMOUNT + SB_BALANCE::BEG_AMOUNT, 2%)

		END SELECT

		GOTO NextBalance
	END IF

	GOTO 17710

17800	!
	! Set up actuals in close journal
	!
	WP_ACTBURDEN = TOTAL_BURDEN
	WP_ACTLABOR  = TOTAL_LABOR
	WP_ACTPARTS  = TOTAL_MAT
	WP_ACTRAWMAT = TOTAL_RMAT

	ACT_TOTAL = WP_CLOSEJOUR::ACTBURDEN + WP_CLOSEJOUR::ACTLABOR + &
		WP_CLOSEJOUR::ACTPARTS + WP_CLOSEJOUR::ACTRAWMAT

	ISSUE% = 0%

	!*******************************************************************
	! Calculate STANDARD totals
	!*******************************************************************

	!
	! Scan through the WP_REGLINE journal, finding out which
	! products have been ordered.
	!
18000	WHEN ERROR IN
		FIND #WP_REGLINE.CH%, &
			KEY #0% EQ WP_CLOSEJOUR::JOB, &
			REGARDLESS
	USE
		CONTINUE CalcVar IF ERR = 155% OR ERR = 9%
		FILENAME$ = "WP_REGLINE"
		CONTINUE HelpError
	END WHEN

	STD_TITLE_FLAG% = 0%

 NextJobLine:
18003	WHEN ERROR IN
		GET #WP_REGLINE.CH%, REGARDLESS
	USE
		CONTINUE CalcVar IF ERR = 11%
		FILENAME$ = "WP_REGLINE"
		CONTINUE HelpError
	END WHEN

	GOTO CalcVar IF WP_REGLINE::JOB <> WP_CLOSEJOUR::JOB

	GOTO NextJobLine IF WP_REGLINE::REC_TYPE <> "02"

	TOTAL_MAT = 0.0
	TOTAL_RMAT = 0.0
	TOTAL_LABOR = 0.0

	IF WP_REGLINE::TTYPE = "L"
	THEN
		TOTAL_LABOR = FUNC_ROUND(WP_REGLINE::COST * &
			WP_REGLINE::QTY, 2%)

		!
		! DONE WITH THE LINE
		!
		GOTO DisplayTotals
	END IF

	!
	! Create an initial component in case bill of materials
	! doesn't pull up anything.
	!
	COMPONENT% = 1%
	COMPONENT(COMPONENT%)::NUMBER	= WP_REGLINE::ITEMCODE
	COMPONENT(COMPONENT%)::QUANTITY = WP_REGLINE::QTY

	!
	! Check the BM_RELATION file to determine what
	! components are used in this product.
	!
18007	WHEN ERROR IN
		FIND #BM_RELATION.CH%, &
			KEY #0% EQ WP_REGLINE::ITEMCODE, &
			REGARDLESS
	USE
		CONTINUE 18033 IF ERR = 155%
		FILENAME$ = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

18010	WHEN ERROR IN
		GET #BM_RELATION.CH%, REGARDLESS
	USE
		CONTINUE 18033 IF ERR = 11%
		FILENAME$ = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

	BOM(1%)::PRODUCT = BM_RELATION::PRODUCT

	COMPONENT% = 0%
	BOM(0%)::QTY = WP_REGLINE::QTY
	BOM% = 1%

	!
	! GoDownTree
	!
18015	BOM(BOM%)::PRODUCT	= BM_RELATION::PRODUCT
	BOM(BOM%)::QTY		= BOM(BOM% - 1%)::QTY * BM_RELATION::QUANTITY
	BOM(BOM%)::RFA_LEVEL	= GETRFA(BM_RELATION.CH%)
	SCRAP%			= BM_RELATION::SCRAP

18020	WHEN ERROR IN
		GET #BM_RELATION.CH%, &
			KEY #0% EQ BM_RELATION::COMPONENT, &
			REGARDLESS
	USE
		CONTINUE 18500 IF ERR = 155%
		FILENAME$ = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

	GOTO CheckForLabor IF SCRAP% = 0%

	!
	! try to figure out, if there is really a shrinkage
	!
	COMP_ISS_QTY = 0.0

18021	WHEN ERROR IN
		FIND #WP_REQREGISTER.CH%, &
			KEY #2% EQ BM_RELATION::COMPONENT + &
			JC_JOB::LOCATION + WP_CLOSEJOUR::JOB, &
			REGARDLESS
	USE
		CONTINUE CheckForLabor IF ERR = 155% OR ERR = 9%
		FILENAME$ = "WP_REQREGISTER"
		CONTINUE HelpError
	END WHEN

 NextCompRec:
18022	WHEN ERROR IN
		GET #WP_REQREGISTER.CH%, REGARDLESS
	USE
		CONTINUE CheckForLabor IF ERR = 11%
		FILENAME$ = "WP_REQREGISTER"
		CONTINUE HelpError
	END WHEN

	IF (BM_RELATION::COMPONENT = WP_REQREGISTER::PRODUCT) AND &
		(JC_JOB::LOCATION = WP_REQREGISTER::LOCATION) AND &
		(WP_CLOSEJOUR::JOB = WP_REQREGISTER::JOB)
	THEN
		IF WP_REQREGISTER::RECTYP = "02"
		THEN
			COMP_ISS_QTY = COMP_ISS_QTY + WP_REQREGISTER::QTY
		END IF

		GOTO NextCompRec
	END IF

 CheckForLabor:
	!
	! test if the higher level had been issued
	!
	FOR L% = 1% TO ISSUE%

		IF BM_RELATION::PRODUCT = ISSUE(L%)::PRODUCT
		THEN
			IF ISSUE(L%)::QTY >= BOM(BOM%)::QTY
			THEN
				ISSUE(L%)::QTY = ISSUE(L%)::QTY - &
					BOM(BOM%)::QTY

				TOTAL_LABOR = TOTAL_LABOR - &
					FUNC_ROUND(BOM(BOM%)::QTY * &
					ISSUE(L%)::LABOR, 2%)

				!
				! add product to the array
				!
				ISSUE_QTY = BOM(BOM%)::QTY
				GOSUB 18550
				GOTO 18030
			ELSE
				IF ISSUE(L%)::QTY > 0.0
				THEN
					TOTAL_LABOR = TOTAL_LABOR - &
						ISSUE(L%)::QTY * &
						ISSUE(L%)::LABOR

					BOM(BOM%)::QTY = &
						BOM(BOM%)::QTY - ISSUE(L%)::QTY

					!
					! add product to the array
					!
					ISSUE_QTY = ISSUE(L%)::QTY
					GOSUB 18550
				END IF

				GOTO ContDown
			END IF
		END IF

	NEXT L%

	!
	! try to figure out, how much had been issued for a product
	!
18023	WHEN ERROR IN
		FIND #WP_REQREGISTER.CH%, &
			KEY #2% EQ BM_RELATION::PRODUCT + &
			JC_JOB::LOCATION + WP_CLOSEJOUR::JOB, &
			REGARDLESS
	USE
		CONTINUE ContDown IF ERR = 155% OR ERR = 9%
		FILENAME$ = "WP_REQREGISTER"
		CONTINUE HelpError
	END WHEN

	ISSUE% = ISSUE% + 1%

	ISSUE(ISSUE%)::PRODUCT	= BM_RELATION::PRODUCT
	ISSUE(ISSUE%)::QTY	= 0.0
	ISSUE(ISSUE%)::LABOR	= 0.0
	ISSUE(ISSUE%)::HOURS	= 0.0

 NextProdRec:
18024	WHEN ERROR IN
		GET #WP_REQREGISTER.CH%, REGARDLESS
	USE
		CONTINUE 18026 IF ERR = 11%
		FILENAME$ = "WP_REQREGISTER"
		CONTINUE HelpError
	END WHEN

	IF (BM_RELATION::PRODUCT = WP_REQREGISTER::PRODUCT) AND &
		(JC_JOB::LOCATION = WP_REQREGISTER::LOCATION) AND &
		(WP_CLOSEJOUR::JOB = WP_REQREGISTER::JOB)
	THEN
		IF WP_REQREGISTER::RECTYP = "02"
		THEN
			ISSUE(ISSUE%)::QTY = ISSUE(ISSUE%)::QTY + &
				WP_REQREGISTER::QTY
		END IF

		GOTO NextProdRec
	END IF

	!
	! Force shrinkage
	!
18026	IF (SCRAP% <> 0%) AND (COMP_ISS_QTY = 0.0) AND &
		(ISSUE(ISSUE%)::QTY < BOM(BOM%)::QTY)
	THEN
		ISSUE(ISSUE%)::QTY = BOM(BOM%)::QTY
	END IF

	OPERATION$ = "        "

	!
	! try to figure out how much labor is needed for a product
	!
 ReadProdOper:
	IF BM_READ_PRODOPER(BM_RELATION::PRODUCT, &
		OPERATION$, "GT", WP_REGLINE::COMP_DATE, &
		BM_PRODOPER_READ) = CMC$_NORMAL
	THEN
		OPERATION$ = BM_PRODOPER_READ::OPERATION

		IF PR_READ_OPERATION(BM_PRODOPER_READ::OPERATION, &
			"", PR_OPER_READ) = CMC$_NORMAL
		THEN
			HOUR_RATE = PR_OPER_READ::HOUR_RATE
		ELSE
			HOUR_RATE = 0.0
		END IF

		!
		! dollar labor per unit
		!
		ISSUE(ISSUE%)::LABOR = FUNC_ROUND(ISSUE(ISSUE%)::LABOR + &
			HOUR_RATE * BM_PRODOPER_READ::HOURS, 2%)

		!
		! hours per unit
		!
		ISSUE(ISSUE%)::HOURS = ISSUE(ISSUE%)::HOURS + &
			BM_PRODOPER_READ::HOURS

		GOTO ReadProdOper
	END IF

	GOTO CheckForLabor

	!*******************************************************************
	! Go down another level
	!*******************************************************************
 ContDown:
	BOM% = BOM% + 1%
	GOTO 18015

	!*******************************************************************
	! Go Up One Level
	!*******************************************************************
18027	GOTO 18033 IF BOM% - 1% = 0%

	BOM% = BOM% - 1%

18030	WHEN ERROR IN
		GET #BM_RELATION.CH%, RFA BOM(BOM%)::RFA_LEVEL, REGARDLESS

		GET #BM_RELATION.CH%, REGARDLESS
	USE
		CONTINUE 18027 IF ERR = 155% OR ERR = 11%
		FILENAME$ = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

	IF BM_RELATION::PRODUCT <> BOM(BOM%)::PRODUCT
	THEN
		GOTO 18027
	ELSE
		GOTO 18015
	END IF

	!*******************************************************************
	!
	!*******************************************************************

18033	FOR I% = 1% TO COMPONENT%

		V% = PD_EXAM_PRODUCT(COMPONENT(I%)::NUMBER, PD_PRODUCT_EXAM)

		PD_PRODUCT_EXAM::PRODUCT_FACTOR = 1.0 &
			IF PD_PRODUCT_EXAM::PRODUCT_FACTOR = 0.0

		COST = PC_READ_COST(COMPONENT(I%)::NUMBER, &
			JC_JOB::LOCATION, WP_REGLINE::COMP_DATE, "")

		COST = COST / PD_PRODUCT_EXAM::PRODUCT_FACTOR

		IF COMP_STRING(PD_PRODUCT_EXAM::PROD_TYPE, BM_CONTROL::RMAT)
		THEN
			TOTAL_RMAT = TOTAL_RMAT + &
				FUNC_ROUND(COMPONENT(I%)::QUANTITY * COST, 2%)
		ELSE
			TOTAL_MAT = TOTAL_MAT + &
				FUNC_ROUND(COMPONENT(I%)::QUANTITY * COST, 2%)
		END IF

	NEXT I%

	!
	! total labor for parent
	!
	OPERATION$ = "        "
	PR_OPER_READ::PIECE_RATE = 0.0

 ReadOperation:
	IF BM_READ_PRODOPER(WP_REGLINE::ITEMCODE, &
		OPERATION$, "GT", WP_REGLINE::COMP_DATE, &
		BM_PRODOPER_READ) = CMC$_NORMAL
	THEN
		OPERATION$ = BM_PRODOPER_READ::OPERATION

		IF PR_READ_OPERATION(BM_PRODOPER_READ::OPERATION, &
			"", PR_OPER_READ) = CMC$_NORMAL
		THEN
			HOUR_RATE = PR_OPER_READ::HOUR_RATE
		ELSE
			HOUR_RATE = 0.0
		END IF

		TOTAL_LABOR = TOTAL_LABOR + &
			FUNC_ROUND(HOUR_RATE * BM_PRODOPER_READ::HOURS, 2%) * &
			WP_REGLINE::QTY

		GOTO ReadOperation
	END IF

	GOTO DisplayTotals

18500	!*******************************************************************
	! Array of terminals
	! Try to add this Bill of Material Item to the component list,
	! if it has the correct product type.
	!*******************************************************************

	FOR I% = 1% TO COMPONENT%

		IF BM_RELATION::COMPONENT = COMPONENT(I%)::NUMBER
		THEN
			COMPONENT(I%)::QUANTITY = COMPONENT(I%)::QUANTITY + &
				FUNC_ROUND(BOM(BOM%)::QTY, 3%)

			GOTO EndTerm
		END IF

	NEXT I%

	V% = PD_EXAM_PRODUCT(BM_RELATION::COMPONENT, PD_PRODUCT_EXAM)

18510	IF COMP_STRING(PD_PRODUCT_EXAM::PROD_TYPE, BM_CONTROL::PRODTYPE)
	THEN
		COMPONENT% = COMPONENT% + 1%

		COMPONENT(COMPONENT%)::NUMBER = BM_RELATION::COMPONENT
		COMPONENT(COMPONENT%)::QUANTITY = &
			FUNC_ROUND(BOM(BOM%)::QTY, 3%)
	END IF

 EndTerm:
	GOTO 18030

18550	!*******************************************************************
	! Array of terminals from higher level
	!*******************************************************************
	FOR I% = 1% TO COMPONENT%

		IF BM_RELATION::PRODUCT = COMPONENT(I%)::NUMBER
		THEN
			COMPONENT(I%)::QUANTITY = COMPONENT(I%)::QUANTITY + &
				FUNC_ROUND(ISSUE_QTY, 3%)

			GOTO Ret18550
		END IF

	NEXT I%

	COMPONENT% = COMPONENT% + 1%

	COMPONENT(COMPONENT%)::NUMBER = BM_RELATION::PRODUCT

	COMPONENT(COMPONENT%)::QUANTITY = FUNC_ROUND(ISSUE_QTY, 3%)

 Ret18550:
	RETURN

	!*******************************************************************
	! Done with one issue.
	!*******************************************************************
 DisplayTotals:
18600	TOTAL_COST = FUNC_ROUND(PC_READ_COST(WP_REGLINE::ITEMCODE, &
		JC_JOB::LOCATION, WP_REGLINE::COMP_DATE, "") * &
		WP_REGLINE::QTY, 2%)

	!
	! Apply a dollar rule. No labor if it comes to less than one
	! dollar.
	!
	IF ABS(TOTAL_LABOR) < 1.0
	THEN
		TOTAL_LABOR = 0.0
		TOTAL_BURDEN = 0.0
		TOTAL_MAT = FUNC_ROUND(TOTAL_COST - TOTAL_RMAT, 2%)
	ELSE
		TOTAL_BURDEN = FUNC_ROUND(TOTAL_COST - TOTAL_LABOR - &
			TOTAL_MAT - TOTAL_RMAT, 2%)
	END IF

	!
	! Set up standards in WP_CLOSEJOUR
	!
	WP_STDBURDEN = FUNC_ROUND(WP_STDBURDEN + TOTAL_BURDEN, 2%)
	WP_STDLABOR  = FUNC_ROUND(WP_STDLABOR + TOTAL_LABOR, 2%)
	WP_STDPARTS  = FUNC_ROUND(WP_STDPARTS + TOTAL_MAT, 2%)
	WP_STDRAWMAT = FUNC_ROUND(WP_STDRAWMAT + TOTAL_RMAT, 2%)

	STD_TOTAL = FUNC_ROUND(WP_STDBURDEN + WP_STDLABOR + &
		WP_STDPARTS + WP_STDRAWMAT, 2%)

	IF STD_TITLE_FLAG% = 0%
	THEN
		TEXT$ = "Line  TranType  Itemcode        " + &
			"Description                " + &
			"  StdBurd    StdLab    StdMat   StdRmat     Total"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3%)
		STD_TITLE_FLAG% = -1%
	END IF

	TOTAL = FUNC_ROUND(TOTAL_BURDEN + TOTAL_LABOR + TOTAL_MAT + &
		TOTAL_RMAT, 2%)

	TEXT$ = WP_REGLINE::LLINE + "  " + &
		TTYPE$ + "  " + &
		WP_REGLINE::ITEMCODE + "  " + &
		LEFT(WP_REGLINE::DESCR, 25%) + "  " + &
		FORMAT$(TOTAL_BURDEN, "######.## ") + &
		FORMAT$(TOTAL_LABOR, "######.## ") + &
		FORMAT$(TOTAL_MAT, "######.## ") + &
		FORMAT$(TOTAL_RMAT, "######.## ") + &
		FORMAT$(TOTAL, "######.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitFunction IF UTL_REPORTX::STAT

	GOTO NextJobLine

 CalcVar:

	IF STD_TITLE_FLAG% = -1%
	THEN
		STD_TOTAL = FUNC_ROUND(WP_STDBURDEN + &
			WP_STDLABOR + &
			WP_STDPARTS + WP_STDRAWMAT, 2%)
		TEXT$ = "                                " + &
			"TOTAL                      " + &
			FORMAT$(WP_STDBURDEN, "######.## ") + &
			FORMAT$(WP_STDLABOR, "######.## ") + &
			FORMAT$(WP_STDPARTS, "######.## ") + &
			FORMAT$(WP_STDRAWMAT, "######.## ") + &
			FORMAT$(STD_TOTAL, "######.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -2%)
		GOTO ExitFunction IF UTL_REPORTX::STAT
	END IF

	RETURN

 ExitVariance:
	TEXT$ = MESSAGE$
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	RETURN

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	UTL_REPORTX::STAT = -1%
	GOTO ExitFunction

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Untrapped error
	!
	RESUME HelpError

32767	END
