1	%TITLE "Requistion Summary Report"
	%SBTTL "WP_RPRT_REQSUMMARY_COST"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 2003 BY
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
	! use or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	! ID:WP0065
	!
	! Abstract:HELP
	!	.lm +5
	!	.b
	!	The ^*Requisition Summary Report\* contains the following information:
	!	.table 3,25
	!	.te
	!	Job Number	Job Description
	!	.te
	!	Type	Class
	!	.te
	!	Order Date	Location
	!	.te
	!	Operator	Reference No
	!	.te
	!	Status Flag	Closed Date
	!	.te
	!	Line	Transaction Type
	!	.te
	!	Item code	Description
	!	.te
	!	Quantity Ordered	Quantity Completed
	!	.te
	!	Quantity Cancelled	Balance
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Requisition Summary
	!	.x Requisition Summary>Report
	!	.x Print>Requisition Summary
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_RPRT_REQSUMMARY_COST/LINE
	!	$ LINK/EXE=WP_EXE: WP_RPRT_REQSUMMARY_COST, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE WP_RPRT_REQSUMMARY_COST.OBJ;*
	!
	! Author:
	!
	!	03/27/2003 - Kevin Handy
	!
	! Modification History:
	!
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include codes
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Include cdd
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.HB"
	DECLARE			WP_REGLINE_CDD		WP_REGLINE_READ

	%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.HB"
	DECLARE			WP_REQREGISTER_CDD	WP_REQREGISTER_READ

	%INCLUDE "SOURCE:[JC.OPEN]JC_JOB.HB"
	MAP (SB_SUBACCOUNT)	JC_JOB_CDD		JC_JOB

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP (SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	!
	! Declare external functions
	!
	EXTERNAL LONG  FUNCTION WP_READ_REGLINE
	EXTERNAL LONG  FUNCTION WP_READ_REQREGISTER
	EXTERNAL LONG  FUNCTION PD_EXAM_PRODUCT
	EXTERNAL REAL  FUNCTION PC_READ_COST
	EXTERNAL STRING FUNCTION DATE_TODAY

	%PAGE

	ON ERROR GOTO 19000

	!
 Init:	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)

	!++
	! Abstract:FLD01
	!	^*(01) Sort by (J,T,C)\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field determines the order
	!	in which the report will print.
	!	.b
	!	Valid settings are:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	J - Job Number
	!	.le
	!	T - Job Type
	!	.le
	!	C - Class
	!	.els
	!	.lm -5
	!	.b
	!	A setting is required in this field. No other settings are valid.
	!	.lm -5
	!
	! Index:
	!	.x Sort>Job Status Summary Report
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field enters the item from which be report
	!	is to begin.  The "item" must be in agreement with the setting in field (01)
	!	Sort by.
	!	.b
	!	A blank field will cause the report to begin with the first item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Job Status Summary Report
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field specifies the item to which the
	!	report will print.  The "item" must be in agreement with the setting in
	!	field (01) Sort by.
	!	.b
	!	A blank field will cause the report to end with the last item in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Job Status Summary Report
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects designated items to be
	!	printed by using Wildcarding techniques.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Job Status Summary Report
	!
	!--

300	!
	! Open  Subaccount Register file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.OPN"
	USE
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	! Select which method to sort by
	!
	SELECT SORTBY$

	CASE "T"
		K_NUM% = 1%
		TITLE$(1%) = " REGISTER SUMMARY BY JOB TYPE"

	CASE "C"
		K_NUM% = 2%
		TITLE$(1%) = " REGISTER SUMMARY BY JOB CLASS"

	CASE "J"
		K_NUM% = 0%
		TITLE$(1%) = " REGISTER SUMMARY BY JOB NUMBER"

	END SELECT

	TITLE$(2%) = " Work In Process System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "JobNumber  JobDescription               " + &
		"            Type Class   JobDate  Status" + &
		" CloseDate  Location Operator   Referenc" + &
		"eNo"

	TITLE$(5%) = "              Line   TType ItemCode      D" + &
		"escription                    Q" + &
		"tyOrdered  QtyComplete  QtyCancel   Balance"

	TITLE$(6%) = "                ReqNum      Reqlin Produc" + &
		"t        Description        " + &
		"      QtyOrdered QtyComplete QtyCancel" + &
		"    Balance    Extension"

	TITLE$(7%) =  "."

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	!
	! If from item is blank then reset Register file
	! else try to find the first record
	!
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			FIND #SB_SUBACCOUNT.CH%, &
				KEY #K_NUM% GE "J", &
				REGARDLESS
		ELSE
			FIND #SB_SUBACCOUNT.CH%, &
				KEY #K_NUM% GE "J" + FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE ExitTotal IF ERR = 155%
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	GOTO ExitProgram IF UTL_REPORTX::STAT

	WHEN ERROR IN
		GET #SB_SUBACCOUNT.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

	GOTO ExitTotal IF JC_JOB::SUBJECT <> "J"

	!
	! Check current record if should be printed
	!
	SELECT SORTBY$

	CASE "C"
		GOTO ExitTotal IF (JC_JOB::CLASS > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_ARRAY &
			(EDIT$(JC_JOB::CLASS, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "J"
		GOTO ExitTotal IF (JC_JOB::JOB > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_ARRAY &
			(EDIT$(JC_JOB::JOB, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "T"
		GOTO ExitTotal IF (JC_JOB::TTYPE > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_ARRAY &
			(EDIT$(JC_JOB::TTYPE, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""
	END SELECT

 PrintItNow:
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

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	NEXT_LINE$ = "   "

 ReadRegLine:
	GOTO NewOrder IF WP_READ_REGLINE(JC_JOB::JOB, NEXT_LINE$, "GT", &
		WP_REGLINE_READ, QTY()) <> CMC$_NORMAL

	NEXT_LINE$ = WP_REGLINE_READ::LLINE

	TEXT$ = JC_JOB::JOB + "    " + &
		WP_REGLINE_READ::LLINE + "   " + &
		WP_REGLINE_READ::TTYPE + "     " + &
		WP_REGLINE_READ::ITEMCODE + &
		LEFT(WP_REGLINE_READ::DESCR, 30%) + " " + &
		FORMAT$(QTY(1%), "#,###,###") + "    " + &
		FORMAT$(QTY(2%), "#,###,###") + "  " + &
		FORMAT$(QTY(3%), "#,###,###") + " " + &
		FORMAT$(QTY(0%), "#,###,###")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	REQNUMBER$ = "          "
	REQLINE$ = "    "

 NextRequistion:
	GOTO ReadRegline IF WP_READ_REQREGISTER(JC_JOB::JOB, &
		WP_REGLINE_READ::LLINE, &
		REQNUMBER$ + REQLINE$, "GT", &
		WP_REQREGISTER_READ, QTY()) <> CMC$_NORMAL

	GOTO ReadRegline &
		IF WP_REQREGISTER_READ::LLINE <> WP_REGLINE_READ::LLINE

	REQNUMBER$ = WP_REQREGISTER_READ::REQNUM
	REQLINE$   = WP_REQREGISTER_READ::REQLIN

	V% = PD_EXAM_PRODUCT(WP_REQREGISTER_READ::PRODUCT, PD_PRODUCT_EXAM)

	COST = PC_READ_COST(WP_REQREGISTER_READ::PRODUCT, &
		WP_REQREGISTER_READ::LOCATION, DATE_TODAY, "")
	EXTENSION = FUNC_ROUND(QTY(0%) * COST, 2%)

	SUBTOTAL_EXTENSION = SUBTOTAL_EXTENSION + EXTENSION
	TOTAL_EXTENSION = TOTAL_EXTENSION + EXTENSION

	IF (QTY(0%) <> 0.0)
	THEN
		TEXT$ = WP_REQREGISTER_READ::JOB + "      " + &
			CONV_STRING( &
				WP_REQREGISTER_READ::REQNUM, CMC$_LEFT)	+ "  " + &
			WP_REQREGISTER_READ::REQLIN + "   " + &
			WP_REQREGISTER_READ::PRODUCT + " " + &
			LEFT(PD_PRODUCT_EXAM::DESCRIPTION, 20%) + "   " + &
			PD_PRODUCT_EXAM::PROD_TYPE + "  " + &
			FORMAT$(QTY(1%), "###,###") + "     " + &
			FORMAT$(QTY(2%), "###,###") + "   " + &
			FORMAT$(QTY(3%), "###,###") + "    " + &
			FORMAT$(QTY(0%), "###,###") + "    " + &
			FORMAT$(EXTENSION, "###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	GOTO NextRequistion

 NewOrder:
	TEXT$ = SPACE$(28%) + &
		"SUB TOTAL" + &
		SPACE$(84%) + &
		FORMAT$(SUBTOTAL_EXTENSION, "###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -3%)

	SUBTOTAL_EXTENSION = 0.0

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

	GOTO GetNextRec

 ExitTotal:
	IF SUBTOTAL_EXTENSION <> 0.0
	THEN
		TEXT$ = SPACE$(28%) + &
			"SUB TOTAL" + &
			SPACE$(84%) + &
			FORMAT$(SUBTOTAL_EXTENSION, "###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -3%)

		SUBTOTAL_EXTENSION = 0.0
	END IF

	TEXT$ = SPACE$(28%) + &
		"GRAND TOTAL" + &
		SPACE$(82%) + &
		FORMAT$(TOTAL_EXTENSION, "###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -3%)

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
	!+-+-+
	!++
	! Abstract:FLD05
	!	^*(05) Batch Number\*
	!	.b
	!	.lm +5
	!	The ^*Batch Number\* field selects the batch number which
	!	will be printed on the summary report.
	!	.lm -5
	!
	! Index:
	!	.x Batch Number>Job Status Summary Report
	!
	!--
