1	%TITLE "Print Requisition Journal"
	%SBTTL "WP_RPRT_REQJOUR"
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
	! ID:WP0025
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Print Requisition Journal\* option prints the
	!	Materials Requisition Journal Report.  The report contains the following
	!	information:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	Job Number
	!	.le
	!	Job Type
	!	.le
	!	Job Description
	!	.le
	!	Job Class
	!	.le
	!	Location
	!	.le
	!	Line Number
	!	.le
	!	Line Type
	!	.le
	!	Item Code
	!	.le
	!	Item Cost
	!	.le
	!	Completed Quantity
	!	.le
	!	Extended Cost
	!	.le
	!	Cancelled Quantity
	!	.le
	!	Buyoff Date for Line
	!	.els
	!	.lm -15
	!
	! Index:
	!	.x Report>Materials Requisition Journal
	!	.x Requisition Journal>Report
	!	.x Materials Requisition Journal>Report
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_RPRT_REQJOUR/LINE
	!	$ LINK/EXE=WP_EXE: WP_RPRT_REQJOUR, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE WP_RPRT_REQJOUR.OBJ;*
	!
	! Author:
	!
	!	07/24/91 - Jeff Beard
	!
	! Modification History:
	!
	!	10/02/91 - Dan Perkins
	!		Cleaned up program code.
	!		Checked error trapping.
	!
	!	07/20/92 - Dan Perkins
	!		Added BOMUOM to report.
	!
	!	09/01/92 - Kevin Handy
	!		Clean up (check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/18/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	07/20/2000 - Kevin Handy
	!		Use WHEN ERROR IN
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

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	%INCLUDE "SOURCE:[JC.OPEN]JC_JOB.HB"
	MAP (SB_SUBACCOUNT)	JC_JOB_CDD		JC_JOB_EXAM

	%INCLUDE "SOURCE:[WP.OPEN]WP_REQJOUR.HB"
	MAP (WP_REQJOUR)	WP_REQJOUR_CDD		WP_REQJOUR

	%INCLUDE "SOURCE:[WP.OPEN]WP_REQLINE.HB"
	MAP (WP_REQLINE)	WP_REQLINE_CDD		WP_REQLINE

	!
	! Declare external functions
	!
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
	!	.b
	!	.lm +5
	!	The ^*From Item\* field enters a job number with which the
	!	the Requisition Journal will begin printing.
	!	.b
	!	A blank field will cause the report to begin with the first job in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field enters a job number at which the
	!	printing will end.
	!	.b
	!	A blank field will cause the report to end with the last job in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects designated jobs to be
	!	printed by entering a "wildcard", using the wildcarding technique.
	!	.b
	!	For additional information relative to wildcarding technique, see
	!	Appendix B.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard
	!
	!--

	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)

	!++
	! Abstract:FLD04
	!	^* (04) Batch Number\*
	!	.b
	!	.lm +5
	!	The ^*Batch Number\* field enters a batch to be printed.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Batch Number>Print Materials Requisitions Journal
	!	.x Print>Materials Requisition Journal>Batch Number
	!
	!--

300	!
	! Open Order Journal file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[WP.OPEN]WP_REQJOUR.OPN"
	USE
		FILENAME$ = "WP_REQJOUR"
		CONTINUE HelpError
	END WHEN

310	!
	! Open Order Line file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[WP.OPEN]WP_REQLINE.OPN"
	USE
		CONTINUE ReportTitle IF ERR = 5%
		FILENAME$ = "WP_REQLINE"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "REQUISITION JOURNAL REPORT"
	TITLE$(2%) = "BATCH No. " + BATCH_NO$
	TITLE$(3%) = "Work In Process System"
	TITLE$(4%) = ""

	!
	! Heading
	!
	TITLE$(5%) = "JobNumber  JobDescription              " + &
		"  Type Class Status Line  ReqDate      " + &
		" Operator     Notes"

	TITLE$(6%) = "            Operation ReqNum     ProductNum" + &
		"     Description                                " + &
		"     Qty UnitMeasure"

	TITLE$(7%) = "."

	!
	! Routine to load left justified zeros into FROM_ITEM$
	! and TO_ITEM$ if any order numbers are entered as ranges
	!
	!FROM_ITEM$ = STRING$(10% - LEN(FROM_ITEM$), ASCII("0")) + &
	!	FROM_ITEM$ IF FROM_ITEM$ <> ""

	!TO_ITEM$ = STRING$(10% - LEN(TO_ITEM$), ASCII("0")) + &
	!	TO_ITEM$ IF TO_ITEM$ <> ""

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
			RESET #WP_REQJOUR.CH%, KEY #0%
		ELSE
			FIND #WP_REQJOUR.CH%, KEY #0% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR= 155%
		FILENAME$ = "WP_REQJOUR"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	GOTO ExitProgram IF UTL_REPORTX::STAT

	WHEN ERROR IN
		GET #WP_REQJOUR.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "WP_REQJOUR"
		CONTINUE HelpError
	END WHEN

	GOTO ExitProgram IF (WP_REQJOUR::JOB > TO_ITEM$) AND TO_ITEM$ <> ""

	GOTO GetNextRec &
		IF COMP_ARRAY(EDIT$(WP_REQJOUR::JOB, -1%), WLDCRD$) = 0% &
		AND WLDCRD$ <> ""

	!
	! Get Job Information
	!
	V% = SB_EXAM_SUBACCOUNT("J", WP_REQJOUR::JOB, JC_JOB_EXAM)

	!
	! Print out one line
	!
	TEXT$ = WP_REQJOUR::JOB + " " + &
		LEFT$(JC_JOB_EXAM::DESCR, 29%) + " " + &
		JC_JOB_EXAM::TTYPE + "   " + &
		JC_JOB_EXAM::CLASS + "  " + &
		JC_JOB_EXAM::SSTATUS + "      " + &
		WP_REQJOUR::LLINE + "  " + &
		PRNT_DATE(WP_REQJOUR::REQDATE, 8%) + "    " + &
		WP_REQJOUR::OPERATOR + "   " + &
		LEFT$(TRM$(WP_REQJOUR::NOTES(0%)) + " " + &
			WP_REQJOUR::NOTES(1%), 37%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Check current Order Line record
	!
17310	WHEN ERROR IN
		FIND #WP_REQLINE.CH%, &
			KEY #0% GE WP_REQJOUR::JOB + WP_REQJOUR::LLINE, &
			REGARDLESS
	USE
		CONTINUE NewRec IF ERR = 155% OR ERR = 9% OR ERR = 11%
		FILENAME$ = "WP_REQLINE"
		CONTINUE HelpError
	END WHEN

 GetReqLine:
	WHEN ERROR IN
		GET #WP_REQLINE.CH%, REGARDLESS
	USE
		CONTINUE NewRec IF ERR = 155% OR ERR = 9% OR ERR = 11%
		FILENAME$ = "WP_REQLINE"
		CONTINUE HelpError
	END WHEN

	GOTO NewRec IF WP_REQLINE::JOB + WP_REQLINE::LLINE <> &
		WP_REQJOUR::JOB + WP_REQJOUR::LLINE

	V% = PD_EXAM_PRODUCT(WP_REQLINE::PRODUCT, PD_PRODUCT_EXAM)

	!
	! Print out one line
	!
	TEXT$ = WP_REQJOUR::JOB + "  " + &
		WP_REQLINE::OPERATION + "  " + &
		CONV_STRING(WP_REQLINE::REQNUM, CMC$_LEFT) + " " + &
		WP_REQLINE::PRODUCT + " " + &
		PD_PRODUCT_EXAM::DESCRIPTION + "  " + &
		FORMAT$(WP_REQLINE::QTY, "#,###,###") + " " + &
		PD_PRODUCT_EXAM::BOMUOM

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOTO GetReqLine

 NewRec:
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
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
