1	%TITLE "Print Job Journal"
	%SBTTL "WP_RPRT_JOBJOUR"
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
	! ID:WP005
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Print Job Journal\* option
	!	prints a report which contains the following information:
	!	.table 3,25
	!	.te
	!	Job Number	Job Type
	!	.te
	!	Job Description	Job Notes
	!	.te
	!	Job Class	Location
	!	.te
	!	Operator	Reference No.
	!	.te
	!	Line Number	Line Type
	!	.te
	!	Item Code	Item Cost
	!	.te
	!	Quantity	Extended Cost
	!	.te
	!	Line Description	Start Date
	!	.te
	!	Expected Completion Date
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Job Journal
	!	.x Job Order Journal>Report
	!	.x Job Order Journal>Print
	!	.x Print>Job Order Journal
	!	.x Work Order Journal>Print
	!	.x Print>Work Order Journal
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_RPRT_JOBJOUR/LINE
	!	$ LINK/EXE=WP_EXE: WP_RPRT_JOBJOUR, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE WP_RPRT_JOBJOUR.OBJ;*
	!
	! AUTHOR:
	!
	!	06/04/91 - Val James Allen
	!
	! MODIFICATION HISTORY:
	!
	!	09/11/91 - Frank F. Starman
	!		Add error trapping for WP_REGLINE file.
	!
	!	09/27/91 - Dan Perkins
	!		Cleaned up program code.
	!		Checked error trapping.
	!
	!	06/08/93 - Frank F. Starman
	!		Added req lines
	!
	!	06/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	06/06/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	05/10/2000 - Kevin Handy
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
	DECLARE		UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.HB"
	DECLARE			WP_REGLINE_CDD	WP_REGLINE_READ

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	%INCLUDE "SOURCE:[WP.OPEN]WP_ORDERLINE.HB"
	MAP (WP_ORDERLINE)	WP_ORDERLINE_CDD	WP_ORDERLINE

	%INCLUDE "SOURCE:[WP.OPEN]WP_JOB.HB"
	MAP (WP_JOB) WP_JOB_CDD WP_JOB

	%INCLUDE "SOURCE:[WP.OPEN]WP_REQLINE.HB"
	MAP (WP_REQLINE)	WP_REQLINE_CDD		WP_REQLINE

	!
	! Declare external functions
	!
	EXTERNAL LONG	FUNCTION WP_READ_REGLINE
	EXTERNAL LONG    FUNCTION PD_EXAM_PRODUCT

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
	!	The ^*Batch Number\* field enters a job journal batch to
	!	be printed.
	!	.b
	!	An valid batch number is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Batch Number>Print Journal
	!	.x Print Journal>Batch Number
	!
	!--


	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(1%), -1%)

	!++
	! Abstract:FLD02
	!	.x Sort by
	!	^*(02) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by (J,C,T)\* field
	!	determines the order in which the Order Journal will print.
	!	.b
	!	Valid settings are:
	!	.table 3,25
	!	.te
	!	^*J\* - Job Number
	!	.te
	!	^*C\* - Job Class
	!	.te
	!	^*T\* - Job Type
	!	.end table
	!	A setting is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field
	!	enters the item with which the report will begin printing.  The entry
	!	in this field must be in agreement with the entry in field (02) Sort by.
	!	.b
	!	A blank field will cause the report to begin with the first item in the file,
	!	as designated by the (02) Sort by field.
	!	.lm -5
	!
	! Index:
	!	.x From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)

	!++
	! Abstract:FLD04
	!	^*(04) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field
	!	enters the item number with which the report is to end printing.
	!	The entry must be in agreement with the entry in the (02) Sort by field.
	!	.b
	!	A blank field will cause the report to continue until the last item in the file
	!	has been printed.
	!	.lm -5
	!
	! Index:
	!	.x To Item
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field
	!	selects designated item(s) to be printed by entering a "wildcard"
	!	with the wildcarding technique.
	!	.b
	!	For more information about the wildcarding technique, see Appendix B.
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
		%INCLUDE "SOURCE:[WP.OPEN]WP_JOB.OPN"
	USE
		FILENAME$ = "WP_JOB"
		CONTINUE HelpError
	END WHEN

310	!
	! Open Order Line file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[WP.OPEN]WP_ORDERLINE.OPN"
	USE
		FILENAME$ = "WP_ORDERLINE"
		CONTINUE HelpError
	END WHEN

320	!
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
	TITLE$(1%) = "WORK ORDER JOURNAL REPORT"
	TITLE$(2%) = "BATCH No. " + BATCH_NO$
	TITLE$(3%) = "Work In Process System"
	TITLE$(4%) = ""

	!
	! Heading
	!
	!	      1234567890123456789012345678901234567890
	TITLE$(5%) = "JobNumber  JobDescription/Notes         " + &
		"            Type Class CreateDate Locati" + &
		"on Operator   ReferenceNo"
	TITLE$(6%) = "            Line Ltype Itemcode       LineDe" + &
		"scription                           Quan" + &
		"tity  ItemCost Extension  StartDate   Co" + &
		"mpDate"
	TITLE$(7%) = "                  Operation ReqNum    ReqL ProductNum" + &
		"     Description                                " + &
		"     Qty UnitMeasure"
	TITLE$(8%) = "."

	!
	! Select which method to sort by
	!
	SELECT SORTBY$

	CASE "T"
		K_NUM% = 1%
		TITLE$(1%) = " WORK ORDER JOURNAL REPORT BY JOB TYPE"

	CASE "C"
		K_NUM% = 2%
		TITLE$(1%) = " WORK ORDER JOURNAL REPORT BY JOB CLASS"

	CASE "J"
		K_NUM% = 0%
		TITLE$(1%) = " WORK ORDER JOURNAL REPORT BY JOB NUMBER"

	!
	! Routine to load left justified zeros into FROM_ITEM$
	! and TO_ITEM$ if any order numbers are entered as ranges
	!
	FROM_ITEM$ = STRING$(10% - LEN(FROM_ITEM$), A"0"B) + FROM_ITEM$ &
		IF FROM_ITEM$ <> ""

	TO_ITEM$ = STRING$(10% - LEN(TO_ITEM$), A"0"B) + TO_ITEM$ &
		IF TO_ITEM$ <> ""

	END SELECT

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #WP_JOB.CH%, KEY #K_NUM%
		ELSE
			FIND #WP_JOB.CH%, KEY #K_NUM% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "WP_JOB"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	GOTO ExitTotal IF UTL_REPORTX::STAT

	WHEN ERROR IN
		GET #WP_JOB.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "WP_JOB"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record if should be printed
	!
	SELECT SORTBY$

	CASE "C"
		GOTO ExitTotal IF (WP_JOB::CLASS > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_ARRAY(EDIT$(WP_JOB::CLASS, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "J"
		GOTO ExitTotal IF (WP_JOB::JOB > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_ARRAY(EDIT$(WP_JOB::JOB, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "T"
		GOTO ExitTotal IF (WP_JOB::TTYPE > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_ARRAY(EDIT$(WP_JOB::TTYPE, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	END SELECT

	!
	! Set up for line number
	!
	LASTLINE$ = "0000"

 GetRegline:
	GOTO PrintJob IF WP_READ_REGLINE(WP_JOB::JOB, LASTLINE$, "GT", &
		WP_REGLINE_READ, QTY()) <> CMC$_NORMAL

	LASTLINE$ = WP_REGLINE_READ::LLINE

	GOTO GetRegLine

 PrintJob:
	!
	! Print out one line
	!
	TEXT$ = WP_JOB::JOB + " " + &
		WP_JOB::DESCR + " " + &
		WP_JOB::TTYPE + "   " + &
		WP_JOB::CLASS + "  " + &
		PRNT_DATE(WP_JOB::BDATE, 8%) + " " + &
		WP_JOB::LOCATION + "     " + &
		WP_JOB::OPERATOR + " " + &
		WP_JOB::REFNO

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitTotal IF UTL_REPORTX::STAT

	IF WP_JOB::NOTES(0%) <> "" AND WP_JOB::NOTES(1%) <> ""
	THEN
		IF WP_JOB::NOTES(0%) = ""
		THEN
			WP_JOB::NOTES(0%) = WP_JOB::NOTES(1%)
			WP_JOB::NOTES(1%) = ""
		END IF

		TEXT$ = WP_JOB::JOB + " " + &
			WP_JOB::NOTES(0%) + " " + &
			WP_JOB::NOTES(1%)

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitTotal IF UTL_REPORTX::STAT
	END IF

	JOB_TOTAL = 0.0

17310	WHEN ERROR IN
		FIND #WP_ORDERLINE.CH%, KEY #0% EQ WP_JOB::JOB, REGARDLESS
	USE
		CONTINUE PrintTotals IF ERR = 155%
		FILENAME$ = "WP_ORDERLINE"
		CONTINUE HelpError
	END WHEN

 GetOrderLine:
17320	WHEN ERROR IN
		GET #WP_ORDERLINE.CH%, REGARDLESS
	USE
		CONTINUE PrintTotals IF ERR = 11%
		FILENAME$ = "WP_ORDERLINE"
		CONTINUE HelpError
	END WHEN

	GOTO PrintTotals IF WP_ORDERLINE::JOB <> WP_JOB::JOB

	EXT_PRICE = FUNC_ROUND(WP_ORDERLINE::QTY * WP_ORDERLINE::COST, 2%)

	JOB_TOTAL = JOB_TOTAL + EXT_PRICE

	V% = FUNC_INCREMENT(LASTLINE$)

	!
	! Print out one line
	!
	TEXT$ = WP_JOB::JOB + "  " + &
		LASTLINE$ + " " + &
		WP_ORDERLINE::TTYPE + "     " + &
		WP_ORDERLINE::ITEMCODE + " " + &
		WP_ORDERLINE::DESCR + " " + &
		FORMAT$(WP_ORDERLINE::QTY, "######.##") + " " + &
		FORMAT$(WP_ORDERLINE::COST, "######.##") + " " + &
		FORMAT$(EXT_PRICE, "######.##") + "  " + &
		PRNT_DATE(WP_ORDERLINE::START_DATE, 8%) + "  " + &
		PRNT_DATE(WP_ORDERLINE::COMP_DATE, 8%)

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitTotal IF UTL_REPORTX::STAT

	LASTREQLINE$ = "0000"

17330	WHEN ERROR IN
		FIND #WP_REQLINE.CH%, &
			KEY #0% GE WP_JOB::JOB + WP_ORDERLINE::LLINE, &
			REGARDLESS
	USE
		CONTINUE GetOrderLine IF ERR = 155% OR ERR = 9% OR ERR = 11%
		FILENAME$ = "WP_REQLINE"
		CONTINUE HelpError
	END WHEN

 GetReqLine:
	WHEN ERROR IN
		GET #WP_REQLINE.CH%, REGARDLESS
	USE
		CONTINUE GetOrderLine IF ERR = 155% OR ERR = 9% OR ERR = 11%
		FILENAME$ = "WP_REQLINE"
		CONTINUE HelpError
	END WHEN

	GOTO GetOrderLine IF WP_REQLINE::JOB <> WP_JOB::JOB OR &
		WP_REQLINE::LLINE <> WP_ORDERLINE::LLINE

	V% = PD_EXAM_PRODUCT(WP_REQLINE::PRODUCT, PD_PRODUCT_EXAM)

	IF WP_REQLINE::REQNUM = "" AND (TRM$(WP_REQLINE::REQLINE) = "" OR &
		WP_REQLINE::REQLINE = "0000")
	THEN
		V% = FUNC_INCREMENT(LASTREQLINE$)
	ELSE
		LASTREQLINE$ = WP_REQLINE::REQLINE
	END IF

	!
	! Print out one line
	!
	TEXT$ = WP_JOB::JOB + "  " + &
		LASTLINE$ + "  " + &
		WP_REQLINE::OPERATION + "  " + &
		CONV_STRING(WP_REQLINE::REQNUM, CMC$_LEFT) + " " + &
		LASTREQLINE$ + " " + &
		WP_REQLINE::PRODUCT + " " + &
		PD_PRODUCT_EXAM::DESCRIPTION + "  " + &
		FORMAT$(WP_REQLINE::QTY, "#,###,###") + " " + &
		PD_PRODUCT_EXAM::BOMUOM

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOTO GetReqLine

 PrintTotals:
	!
	! Print out totals line for end of one order
	!
	TEXT$ = WP_JOB::JOB + SPACE$(72%) + "Job Total: " + &
		SPACE$(5%) + FORMAT$(JOB_TOTAL, "#######.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

	GOTO GetNextRec

 ExitTotal:

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
