1	%TITLE "Requisition Issuing Journal Report"
	%SBTTL "WP_RPRT_ISSJOUR"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1991 BY
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
	! ID:WP0018
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Requisition Issuing Journal\* Report contains the following information:
	!	.table 3,25
	!	.te
	!	Requisition Number	Job Number
	!	.te
	!	Job line	Job Description
	!	.te
	!	Job Type	Job Class
	!	.te
	!	Operator	Requisition Line
	!	.te
	!	Line Type	Product Code
	!	.te
	!	Cost	Issue Quantity
	!	.te
	!	Extended Cost	Cancelled Quantity
	!	.te
	!	Issue Date for Line	WIP Account
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Requisition Issuing Journal
	!	.x Requisition Issuing Journal>Report
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_RPRT_ISSJOUR/LINE
	!	$ LINK/EXE=WP_EXE: WP_RPRT_ISSJOUR, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE WP_RPRT_ISSJOUR.OBJ;*
	!
	! AUTHOR:
	!
	!	07/23/91 - Craig Tanner
	!
	! MODIFICATION HISTORY:
	!
	!	09/27/91 - Deborah K. Fries
	!		Used functions to read files
	!		Improved error trapping
	!		Cleaned source code.
	!
	!	07/21/92 - Dan Perkins
	!		Use CONV_STRING to left justify REQ Number.
	!		Clean program code.
	!
	!	09/01/92 - Kevin Handy
	!		Clean up (check)
	!
	!	10/01/92 - Dan Perkins
	!		Changed JC_JOB map statement to keep program from
	!		crashing.
	!
	!	10/09/92 - Dan Perkins
	!		Display Product Descripton and not WIP account.
	!
	!	10/23/92 - Dan Perkins
	!		Added arguement to GL_OUTP_ACCTSUM because of a change
	!		in that function.
	!
	!	10/26/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	03/18/93 - Kevin Handy
	!		Added parameter to GL_OUTP_ACCTSUM for units.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/18/96 - Kevin Handy
	!		Reformat source code
	!
	!	07/23/98 - Kevin Handy
	!		Added ability so have it print out req qty
	!		instead of cost.
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	02/10/2000 - Kevin Handy
	!		Fix padding of FROM_ITEM$ so that it didn't use
	!		the length of TO_ITEM$
	!
	!	05/04/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	11/06/2000 - Kevin Handy
	!		Use A"x"B
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include codes.inc
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Include cdd
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[WP.OPEN]WP_ISSJOUR.HB"
	MAP (WP_ISSJOUR)	WP_ISSJOUR_CDD		WP_ISSJOUR

	%INCLUDE "SOURCE:[WP.OPEN]WP_ISSLINE.HB"
	MAP (WP_ISSLINE)	WP_ISSLINE_CDD		WP_ISSLINE

	%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.HB"
	DECLARE			WP_REQREGISTER_CDD	WP_REQREGISTER_READ

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	%INCLUDE "SOURCE:[PD.OPEN]PD_ACCOUNT.HB"
	DECLARE			PD_ACCOUNT_CDD		PD_ACCOUNT_READ

	%INCLUDE "SOURCE:[JC.OPEN]JC_JOB.HB"
	MAP (SB_SUBACCOUNT)	JC_JOB_CDD		JC_JOB_EXAM

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	DECLARE			GL_CHART_CDD		GL_CHART_EXAM

	!
	! Declare external functions
	!
	EXTERNAL LONG    FUNCTION GL_OUTP_ACCTSUM
	EXTERNAL LONG    FUNCTION PD_READ_ACCOUNT
	EXTERNAL LONG    FUNCTION WP_READ_REQREGISTER
	EXTERNAL LONG    FUNCTION PD_EXAM_PRODUCT
	EXTERNAL LONG    FUNCTION SB_EXAM_SUBACCOUNT

	%PAGE

	ON ERROR GOTO 19000

	!
 Init:	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Item\*
	!	.B
	!	.LM +5
	!	The ^*From Item\* field
	!	enters the Job Number with which the report will begin printing.
	!	.B
	!	A blank field will cause the report to begin with the first record in the file.
	!	.LM -5
	!
	! Index:
	!	.x From Item>Issue Journal Print
	!	.x Issue Journal>Print>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field
	!	specifies the Job Number with which the report will end printing.
	!	.b
	!	A blank field will cause the report to end with the last record in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Issue Journal Print
	!	.x Issue Journal>Print>To Item
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field
	!	selects a designated job(s) to be printed by entering a wildcard value
	!	using the wildcarding technique.
	!	.b
	!	For more information relative to the wildcarding technique, refer to Appendix
	!	B.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Issue Journal Print
	!	.x Issue Joural>Print>Wildcard
	!
	!--

	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)

	!++
	! Abstract:FLD04
	!	^* (04) Batch Number\*
	!	.b
	!	.lm +5
	!	The ^*Batch Number\* field enters an Issue Journal batch
	!	to be printed.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Batch Number>Print Issue Journal
	!	.x Print Issue Journal>Batch Number
	!	.x Issue Journal>Batch Number
	!
	!--

	SHOWCOST$ = LEFT$(UTL_REPORTX::OPTDEF(5%), 1%)

	!++
	! Abstract:FLD06
	!	^* (06) Show Cost/Req\*
	!	.b
	!	.lm +5
	!	Specifies if you want to see the ^*Cost\* or
	!	he ^*Requisition Quanity\* on the report.
	!	.lm -5
	!
	! Index:
	!	.x Show Cost>Print Issue Journal
	!	.x Print Issue Journal>Show Cost
	!
	!--

	!
	! Pad From Item and To Item with zeros if necessary
	!
	FROM_ITEM$ = STRING$(10% - LEN(FROM_ITEM$), A"0"B) + FROM_ITEM$ &
		IF FROM_ITEM$ <> ""

	TO_ITEM$ = STRING$(10% - LEN(TO_ITEM$), A"0"B) + TO_ITEM$ &
		IF TO_ITEM$ <> ""

300	!
	! Open Order Journal file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[WP.OPEN]WP_ISSJOUR.OPN"
	USE
		FILENAME$ = "WP_ISSJOUR"
		CONTINUE HelpError
	END WHEN

310	!
	! Open Order Line file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[WP.OPEN]WP_ISSLINE.OPN"
	USE
		CONTINUE ReportTitle IF ERR = 5%
		FILENAME$ = "WP_ISSLINE"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "ISSUE JOURNAL REPORT"
	TITLE$(2%) = "BATCH No. " + BATCH_NO$
	TITLE$(3%) = "Work In Process System"
	TITLE$(4%) = ""

	!
	! Heading
	!
	TITLE$(5%) = "ReqNumber  JobNumber  JobLn JobDescription               " + &
		"            Type Class Operator"

	SELECT SHOWCOST$
	CASE "R"
		TITLE$(6%) = "                              ReqLin Lntyp Product" + &
			"        Description             ReqQty  IssueQty"        + &
			"   ExtCost CancelQty IssueDate"
	CASE ELSE
		TITLE$(6%) = "                              ReqLin Lntyp Product" + &
			"        Description               Cost  IssueQty"        + &
			"   ExtCost CancelQty IssueDate"
	END SELECT

	TITLE$(7%) = "."

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	!
	! If from item blank then reset item file
	! else try to find the first record
	!
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #WP_ISSJOUR.CH%, KEY #0%
		ELSE
			FIND #WP_ISSJOUR.CH%, KEY #0% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "WP_ISSJOUR"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitTotal IF UTL_REPORTX::STAT

	!
	! Get next Order Journal record
	!
	WHEN ERROR IN
		GET #WP_ISSJOUR.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "WP_ISSJOUR"
		CONTINUE HelpError
	END WHEN

	GOTO ExitTotal IF (WP_ISSJOUR::JOB > TO_ITEM$) AND TO_ITEM$ <> ""

	GOTO GetNextRec &
		IF COMP_ARRAY(EDIT$(WP_ISSJOUR::JOB, -1%), WLDCRD$) = 0% &
		AND WLDCRD$ <> ""

	V% = SB_EXAM_SUBACCOUNT("J", WP_ISSJOUR::JOB, JC_JOB_EXAM)

	!
	! Print out one line
	!
	TEXT$ = CONV_STRING(WP_ISSJOUR::REQNUM, CMC$_LEFT) + " " + &
		WP_ISSJOUR::JOB + " " + &
		WP_ISSJOUR::LLINE + "  " + &
		JC_JOB_EXAM::DESCR + " " + &
		JC_JOB_EXAM::TTYPE + "   " + &
		JC_JOB_EXAM::CLASS + "  " + &
		WP_ISSJOUR::OPERATOR

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitTotal IF UTL_REPORTX::STAT

	!
	! Assign Variables for later totals
	!
	JOB_TOTAL = 0.0

	!
	! Check current Order Line record
	!
17310	WHEN ERROR IN
		FIND #WP_ISSLINE.CH%, &
			KEY #0% EQ WP_ISSJOUR::REQNUM + &
			WP_ISSJOUR::JOB + WP_ISSJOUR::LLINE, &
			REGARDLESS
	USE
		CONTINUE Totaline IF ERR = 155% OR ERR = 9% OR ERR = 11%
		FILENAME$ = "WP_ISSLINE"
		CONTINUE HelpError
	END WHEN

 IssLine:
	WHEN ERROR IN
		GET #WP_ISSLINE.CH%, REGARDLESS
	USE
		CONTINUE Totaline IF ERR = 155% OR ERR = 9% OR ERR = 11%
		FILENAME$ = "WP_ISSLINE"
		CONTINUE HelpError
	END WHEN

	GOTO Totaline IF WP_ISSLINE::REQNUM + WP_ISSLINE::JOB + &
		WP_ISSLINE::LLINE <> &
		WP_ISSJOUR::REQNUM + WP_ISSJOUR::JOB + WP_ISSJOUR::LLINE

	IF WP_READ_REQREGISTER(WP_ISSJOUR::JOB, WP_ISSLINE::LLINE, &
		WP_ISSLINE::REQNUM + WP_ISSLINE::REQLINE, &
		"EQ", WP_REQREGISTER_READ, QTY()) = CMC$_NORMAL
	THEN
		REQREM = QTY(1%) - QTY(2%)
	ELSE
		WP_REQREGISTER_READ::LOCATION = JC_JOB_EXAM::LOCATION
		REQREM = 0.0
	END IF

	EXT_PRICE = FUNC_ROUND(WP_ISSLINE::QTYISSUE * WP_ISSLINE::COST, 2%)

	JOB_TOTAL = JOB_TOTAL + EXT_PRICE

	!
	! Get product type and account
	!
	V% = PD_EXAM_PRODUCT(WP_ISSLINE::PRODUCT, PD_PRODUCT_EXAM)

	V% = PD_READ_ACCOUNT(WP_REQREGISTER_READ::LOCATION, &
		PD_PRODUCT_EXAM::PROD_TYPE, PD_ACCOUNT_READ)

	!
	! Print out one line
	!
	TEXT$ = CONV_STRING(WP_ISSJOUR::REQNUM, CMC$_LEFT) + " " + &
		WP_ISSJOUR::JOB + " " + &
		WP_ISSJOUR::LLINE + "    " + &
		WP_ISSLINE::REQLINE + "   " + &
		WP_REQREGISTER_READ::RECTYP + "      " + &
		WP_ISSLINE::PRODUCT + " " + &
		LEFT(PD_PRODUCT_EXAM::DESCRIPTION, 20%) + " "

	SELECT SHOWCOST$
	CASE "R"
		TEXT$ = TEXT$ + &
			FORMAT$(REQREM, "######.##") + " "
	CASE ELSE
		TEXT$ = TEXT$ + &
			FORMAT$(WP_ISSLINE::COST, "######.##") + " "
	END SELECT

	TEXT$ = TEXT$ + &
		FORMAT$(WP_ISSLINE::QTYISSUE, "######.##")  + " " + &
		FORMAT$(EXT_PRICE, "######.##") + " " + &
		FORMAT$(WP_ISSLINE::QTYCANCEL, "######.##") + " " + &
		PRNT_DATE(WP_ISSLINE::ISSDATE, 8%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitTotal IF UTL_REPORTX::STAT

	!
	! Credit inventory accounts
	!
	GOTO ExitProgram &
		IF GL_OUTP_ACCTSUM (OPT_ADDREC, PD_ACCOUNT_READ::INVACCT, &
		0.0, -EXT_PRICE, 0.0, TITLE$(), UTL_REPORTX) <> CMC$_NORMAL

	!
	! Debit the WIP account
	!
	GOTO ExitProgram &
		IF GL_OUTP_ACCTSUM (OPT_ADDREC, PD_ACCOUNT_READ::WIPACCT, &
		0.0, EXT_PRICE, 0.0, TITLE$(), UTL_REPORTX) <> CMC$_NORMAL

	GOTO IssLine

 Totaline:
	!
	! Print out totals line for end of one order
	!
	TEXT$ = CONV_STRING(WP_ISSJOUR::REQNUM, CMC$_LEFT) + " " + &
		WP_ISSJOUR::JOB + " " + &
		WP_ISSJOUR::LLINE + "  " + &
		"Total:" + SPACE$(65%) + &
		FORMAT$(JOB_TOTAL, "######.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

	GOTO GetNextRec

 ExitTotal:
	V% = GL_OUTP_ACCTSUM(OPT_SUMMARY, "", 0.0, 0.0, 0.0, &
		TITLE$(), UTL_REPORTX)

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
	!***************************************************************
	! Help Message for an error
	!***************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	UTL_REPORTX::STAT = -1%
	GOTO ExitProgram

19000	!***************************************************************
	! ERROR TRAPPING
	!***************************************************************

	!
	! Resume to display untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
