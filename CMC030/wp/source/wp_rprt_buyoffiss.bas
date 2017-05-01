1	%TITLE "Print Buy off/Issue Order Journal"
	%SBTTL "WP_RPRT_BUYOFFISS"
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
	! ID:WP0115
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Print Buy off/Issue Journal\* option
	!	prints a journal
	!	report which contains the following information:
	!	.table 3,25
	!	.te
	!	Job Number	Job Type
	!	.te
	!	Job Description	Job Class
	!	.te
	!	Location	General Ledger Acct _#
	!	.te
	!	Line Number	Line Type
	!	.te
	!	Item Code	Item Cost
	!	.te
	!	Completed Quantity	Extended Cost
	!	.te
	!	Cancelled Quantity	Buy off Date for Line
	!	.te
	!	Serial _#(Equipment)	General Ledger Acct _#
	!	.te
	!	Indicator of invalid Account Number (_*)
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Buy off
	!	.x Report>Issue
	!	.x Buy off>Report
	!	.x Issue>Report
	!	.x Print>Buy off
	!	.x Print>Issue
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_RPRT_BUYOFFISS/LINE
	!	$ LINK/EXE=WP_EXE: WP_RPRT_BUYOFFISS, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE WP_RPRT_BUYOFFISS.OBJ;*
	!
	! Author:
	!
	!	06/04/91 - Val James "Maxed Out" Allen
	!
	! Modification History:
	!
	!	07/22/91 - Craig Tanner
	!		Fixed the format report to comply with
	!		standards, and added the GL Account summary page.
	!
	!	09/25/91 - Dan Perkins
	!		Included use of functions in code.
	!		Cleaned up program code, checked error trapping.
	!
	!	10/23/92 - Dan Perkins
	!		Added arguement to GL_OUTP_ACCTSUM because of a change
	!		in that function.
	!
	!	03/18/93 - Kevin Handy
	!		Added parameter to GL_OUTP_ACCTSUM for units.
	!
	!	06/18/93 - Dan Perkins
	!		Added issue total.  Cleaned code.
	!
	!	06/23/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/31/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	05/24/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[WP.OPEN]WP_BUYOFFLINE.HB"
	MAP (WP_BUYOFFLINE)	WP_BUYOFFLINE_CDD	WP_BUYOFFLINE

	%INCLUDE "SOURCE:[WP.OPEN]WP_BUYOFF.HB"
	MAP (WP_BUYOFF)		WP_BUYOFF_CDD		WP_BUYOFF

	%INCLUDE "SOURCE:[JC.OPEN]JC_JOB.HB"
	DECLARE			JC_JOB_CDD		JC_JOB_EXAM

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	%INCLUDE "SOURCE:[PD.OPEN]PD_ACCOUNT.HB"
	DECLARE			PD_ACCOUNT_CDD		PD_ACCOUNT_READ

	%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.HB"
	DECLARE			WP_REGLINE_CDD		WP_REGLINE_READ

	%INCLUDE "SOURCE:[WP.OPEN]WP_ISSLINE.HB"
	MAP (WP_ISSLINE)	WP_ISSLINE_CDD	WP_ISSLINE

	!
	! Declare external functions
	!
	EXTERNAL LONG    FUNCTION GL_OUTP_ACCTSUM
	EXTERNAL LONG    FUNCTION SB_EXAM_SUBACCOUNT
	EXTERNAL LONG    FUNCTION PD_EXAM_PRODUCT
	EXTERNAL LONG    FUNCTION WP_READ_REGLINE
	EXTERNAL LONG    FUNCTION PD_READ_ACCOUNT

	%PAGE

	ON ERROR GOTO 19000

	!
 Init:	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT


	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^* (01) Batch Number\*
	!	.b
	!	.lm +5
	!	The ^*Batch Number\* field enters the number of a
	!	Buy off Journal batch to be printed.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Batch Number>Print Journal
	!	.x Print Journal>Batch Number
	!
	!--


	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	FROM_ITEM$ = STRING$(10% - LEN(FROM_ITEM$), A"0"B) + &
		FROM_ITEM$ IF FROM_ITEM$ <> ""

	!++
	! Abstract:FLD02
	!	^*(02) From Job\*
	!	.b
	!	.lm +5
	!	The ^*From Job\* field
	!	enters the Job Number with which to begin.
	!	.b
	!	A blank fields begin with the first Job in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Job
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	TO_ITEM$ = STRING$(10% - LEN(TO_ITEM$), A"0"B) + &
		TO_ITEM$ IF TO_ITEM$ <> ""

	!++
	! Abstract:FLD03
	!	^*(03) To Job\*
	!	.b
	!	.lm +5
	!	The ^*To Job\* field
	!	enters the Job Number with which to end.
	!	.b
	!	A blank field ends with the last
	!	Job in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Job
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field
	!	selects a designated item or items to be printed by entering a
	!	"wildcard" value using the wildcarding technique.
	!	.b
	!	For additional information relative to wildcarding technique, refer to
	!	Appendix B.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard
	!
	!--


300	!
	! Open Order Journal file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[WP.OPEN]WP_BUYOFF.OPN"
	USE
		FILENAME$ = "WP_BUYOFF"
		CONTINUE HelpError
	END WHEN

310	!
	! Open Order Line file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[WP.OPEN]WP_BUYOFFLINE.OPN"
	USE
		FILENAME$ = "WP_BUYOFFLINE"
		CONTINUE HelpError
	END WHEN

320	!
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
	TITLE$(1%) = "BUY OFF JOURNAL REPORT"
	TITLE$(2%) = "BATCH No. " + BATCH_NO$
	TITLE$(3%) = "Work In Process System"
	TITLE$(4%) = ""

	!
	! Heading
	!
	TITLE$(5%) = "JobNumber  JobDescription               " + &
		"            Type Class Location  WIPAcctNumber"

	TITLE$(6%) = "            Line  Ltype Itemcode        UnitCost" + &
		"   CompQty Extension CancelQty BuyoffDate Inv/CosAcctNum" + &
		"     FinishedGoodsID"

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
			RESET #WP_BUYOFF.CH%, KEY #0%
		ELSE
			FIND #WP_BUYOFF.CH%, KEY #0% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "WP_BUYOFF"
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
		GET #WP_BUYOFF.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "WP_BUYOFF"
		CONTINUE HelpError
	END WHEN

	GOTO ExitTotal IF (WP_BUYOFF::JOB > TO_ITEM$) AND TO_ITEM$ <> ""

	GOTO GetNextRec IF COMP_ARRAY(EDIT$(WP_BUYOFF::JOB, -1%), WLDCRD$) = 0% &
		AND WLDCRD$ <> ""

	V% = SB_EXAM_SUBACCOUNT("J", WP_BUYOFF::JOB, JC_JOB_EXAM)

	!
	! Print out one line
	!
	TEXT$ = WP_BUYOFF::JOB + " " + &
		JC_JOB_EXAM::DESCR + " " + &
		JC_JOB_EXAM::TTYPE + "   " + &
		JC_JOB_EXAM::CLASS + "  " + &
		JC_JOB_EXAM::LOCATION + "      " + &
		WP_BUYOFF::ACCT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitTotal IF UTL_REPORTX::STAT

	!
	! Assign Variables for later totals
	!
	JOB_TOTAL, ISSUE_TOTAL = 0.0

	!
	! Check current Order Line record
	!
17310	WHEN ERROR IN
		FIND #WP_BUYOFFLINE.CH%, KEY #0% EQ WP_BUYOFF::JOB, REGARDLESS
	USE
		CONTINUE LineTotal IF ERR = 155%
		FILENAME$ = "WP_BUYOFFLINE"
		CONTINUE HelpError
	END WHEN

 BuyoffLine:
17320	WHEN ERROR IN
		GET #WP_BUYOFFLINE.CH%, REGARDLESS
	USE
		CONTINUE LineTotal IF ERR = 11%
		FILENAME$ = "WP_BUYOFFLINE"
		CONTINUE HelpError
	END WHEN

	GOTO LineTotal IF WP_BUYOFFLINE::JOB <> WP_BUYOFF::JOB

	V% = WP_READ_REGLINE(WP_BUYOFF::JOB, WP_BUYOFFLINE::LLINE, "EQ", &
		WP_REGLINE_READ, QTY())

	EXT_PRICE = FUNC_ROUND(WP_BUYOFFLINE::COMPQTY * &
		WP_BUYOFFLINE::COST, 2%)

	JOB_TOTAL = JOB_TOTAL + EXT_PRICE

	!
	! Print out one line
	!
	TEXT$ = WP_BUYOFF::JOB + "  " + &
		WP_BUYOFFLINE::LLINE + "  " + &
		WP_REGLINE_READ::TTYPE + "     " + &
		WP_REGLINE_READ::ITEMCODE + " " + &
		FORMAT$(WP_BUYOFFLINE::COST, "######.##") + " " + &
		FORMAT$(WP_BUYOFFLINE::COMPQTY, "######.##") + " " + &
		FORMAT$(EXT_PRICE, "######.##") + " " + &
		FORMAT$(WP_BUYOFFLINE::CANCELQTY, "######.##") + " " + &
		PRNT_DATE(WP_BUYOFFLINE::BDATE, 8%) + " " + &
		WP_BUYOFFLINE::ACCT + " " + &
		WP_BUYOFFLINE::FG_ID_NO

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitTotal IF UTL_REPORTX::STAT

	!
	! Debit the register line accounts
	!
	GOTO ExitProgram IF GL_OUTP_ACCTSUM (OPT_ADDREC, WP_BUYOFFLINE::ACCT, &
		0.0, EXT_PRICE, 0.0, TITLE$(), UTL_REPORTX) <> CMC$_NORMAL

17350	WHEN ERROR IN
		FIND #WP_ISSLINE.CH%, &
			KEY #0% EQ "          " + WP_BUYOFFLINE::JOB + &
			WP_BUYOFFLINE::LLINE, &
			REGARDLESS
	USE
		CONTINUE BuyOffLine IF ERR = 155% OR ERR = 11% OR  ERR = 9%
		FILENAME$ = "WP_ISSLINE"
		CONTINUE HelpError
	END WHEN

	TEXT$ = ".......................   " + &
		"ReqLine  Lt      Product#       Description           " + &
		"UnitCost    IssQty Extension    CanQty IssueDate"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

 IssLine:
	WHEN ERROR IN
		GET #WP_ISSLINE.CH%, REGARDLESS
	USE
		CONTINUE BuyOffLine IF ERR = 155% OR ERR = 11% OR  ERR = 9%
		FILENAME$ = "WP_ISSLINE"
		CONTINUE HelpError
	END WHEN

	GOTO BuyOffLine IF WP_ISSLINE::REQNUM + WP_ISSLINE::JOB + &
		WP_ISSLINE::LLINE <> &
		"          " + WP_BUYOFFLINE::JOB + WP_BUYOFFLINE::LLINE

	EXT_PRICE = FUNC_ROUND(WP_ISSLINE::QTYISSUE * WP_ISSLINE::COST, 2%)

	ISSUE_TOTAL = ISSUE_TOTAL + EXT_PRICE

	!
	! Get product type and account
	!
	V% = PD_EXAM_PRODUCT(WP_ISSLINE::PRODUCT, PD_PRODUCT_EXAM)

	V% = PD_READ_ACCOUNT(JC_JOB_EXAM::LOCATION, &
		PD_PRODUCT_EXAM::PROD_TYPE, PD_ACCOUNT_READ)

	!
	! Print out one line
	!
	TEXT$ = WP_BUYOFF::JOB + " " + &
		WP_BUYOFFLINE::LLINE + SPACE$(14%)  + &
		WP_ISSLINE::REQLINE + "   M      " + &
		WP_ISSLINE::PRODUCT + " " + &
		LEFT(PD_PRODUCT_EXAM::DESCRIPTION, 20%) + " " + &
		FORMAT$(WP_ISSLINE::COST, "######.##") + " " + &
		FORMAT$(WP_ISSLINE::QTYISSUE, "######.##") + " " + &
		FORMAT$(EXT_PRICE, "######.##") + " " + &
		FORMAT$(WP_ISSLINE::QTYCANCEL, "######.##") + " " + &
		PRNT_DATE(WP_ISSLINE::ISSDATE, 8%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitTotal IF UTL_REPORTX::STAT

	!
	! Credit inventory accounts
	!
	GOTO ExitProgram IF GL_OUTP_ACCTSUM (OPT_ADDREC, PD_ACCOUNT_READ::INVACCT, &
		0.0, -EXT_PRICE, 0.0, TITLE$(), UTL_REPORTX) <> CMC$_NORMAL

	!
	! Debit the WIP account
	!
	GOTO ExitProgram IF GL_OUTP_ACCTSUM (OPT_ADDREC, PD_ACCOUNT_READ::WIPACCT, &
		0.0, EXT_PRICE, 0.0, TITLE$(), UTL_REPORTX) <> CMC$_NORMAL

	GOTO IssLine

 LineTotal:
	!
	! Print out totals line for end of one order
	!
	TEXT$ = WP_BUYOFF::JOB + SPACE$(10%) + &
		"Total:" + SPACE$(33%) + &
		FORMAT$(JOB_TOTAL, "######.##") + SPACE$(31%) + &
		FORMAT$(ISSUE_TOTAL, "######.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

	GOTO ExitProgram IF GL_OUTP_ACCTSUM (OPT_ADDREC, WP_BUYOFF::ACCT, &
		0.0, -JOB_TOTAL, 0.0, TITLE$(), UTL_REPORTX) <> CMC$_NORMAL

	GOTO GetNextRec

 ExitTotal:
	V% = GL_OUTP_ACCTSUM(OPT_SUMMARY, "", 0.0, 0.0, 0.0, TITLE$(), UTL_REPORTX)

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
