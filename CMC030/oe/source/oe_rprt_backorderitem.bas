1	%TITLE "Ordered Items by Product Report"
	%SBTTL "OE_RPRT_BACKORDERITEM"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 2002 BY
	!
	! Software Solutions, Inc.
	! Idaho Falls, Idaho.
	!
	! This software is furnished under a license and may be
	! copied only in accordance with terms of such license and with
	! the inclusion of the above copyright notice.  This software or
	! any other copies thereof may not be provided or otherwise made
	! available to any other persON_  No title to and ownership of
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
	! ID:OE024
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Ordered Items by Product\* Report contains columns for the following
	!	information:
	!	.table 3,25
	!	.te
	!	Product Number
	!	.te
	!	Product Description
	!	.te
	!	Product Type
	!	.te
	!	Product Category
	!	.te
	!	Customer Name
	!	.te
	!	Customer PO Number
	!	.te
	!	Order Number
	!	.te
	!	Quantity Ordered
	!	.te
	!	Order Date
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Ordered Items by Product
	!	.x Ordered Items by Product>Report
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_RPRT_BACKORDERITEM/LINE
	!	$ LINK/EXE=OE_EXE: OE_RPRT_BACKORDERITEM, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE OE_RPRT_BACKORDERITEM.OBJ;*
	!
	! Author:
	!
	!	07/29/2002 - Kevin Handy
	!		Based on OE_RPRT_ORDERITEM
	!
	! Modification History:
	!
	!	11/24/2003 - Kevin Handy
	!		Don't let THIS_BO go negitive.
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include scope.com
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Include cdd
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.HB"
	MAP (OE_REGLINE)	OE_REGLINE_CDD		OE_REGLINE
	DECLARE OE_REGLINE_CDD THIS_REGLINE

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.HB"
	DECLARE			OE_REGHEADER_CDD	OE_REGHEADER_READ

	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.HB"
	MAP (PO_REG_LINE)	PO_REG_LINE_CDD		PO_REG_LINE

	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_SUB_LINE.HB"
	MAP (PO_REG_SUB_LINE)	PO_REG_SUB_LINE_CDD	PO_REG_SUB_LINE

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	DECLARE			AP_VENDOR_CDD		AP_VENDOR_EXAM

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	DECLARE			AR_35CUSTOM_CDD		AR_35CUSTOM_EXAM

	!
	! Declare external functions
	!
	EXTERNAL LONG    FUNCTION OE_READ_REGHEADER
	EXTERNAL LONG    FUNCTION AR_EXAM_CUSTOM
	EXTERNAL REAL    FUNCTION PC_READ_COST
	EXTERNAL LONG    FUNCTION IC_READ_35BALANCE_ALL
	EXTERNAL LONG    FUNCTION AP_EXAM_VENDOR

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
	!	.x Sort by>Order Items by Product Report
	!	^*(01) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field determines if the
	!	report is to be printed in product number order, product
	!	type, product category, product description order.
	!	.b
	!	Valid codes are:
	!	.table 3,25
	!	.te
	!	^*P\* - Product Number
	!	.te
	!	^*T\* - Product Type
	!	.te
	!	^*C\* - Product Category
	!	.te
	!	^*D\* - Product Description
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Order Items by Product Report>Sort by
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field enters an item with
	!	which the report is to begin printing.  The value entered
	!	must be in agreement with field (01) Sort by.
	!	.b
	!	A blank field will cause the report to begin with the first
	!	item in the file.
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
	!	The ^*To Item\* field specifies the item
	!	with which the report is to end printing.  The value
	!	entered must be in agreement with field (01)
	!	Sort by.
	!	.b
	!	A blank field will cause the report to end with the last
	!	item in the file.
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
	!	The ^*Wildcard\* field selects
	!	designated items to be printed by entering a "wildcard"
	!	using the Wildcarding
	!	Technique.
	!	.b
	!	For information on "Wildcarding" techniques, refer to Appendix B.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard
	!
	!--

	WILDCUSNUM$ = TRM$(UTL_REPORTX::OPTDEF(4%))

	!++
	! Abstract:FLD05
	!	^*(05) Wildcard Customer\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard Customer\* field selects
	!	designated items to be printed by entering a "wildcard"
	!	using the Wildcarding
	!	Technique.
	!	.b
	!	For information on "Wildcarding" techniques, refer to Appendix B.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard
	!
	!--

	FROM_DATE$ = EDIT$(DATE_STOREDATE(UTL_REPORTX::OPTDEF(5%)), -1%)

	!++
	! Abstract:FLD06
	!	.x From Date>Items Ordered Summary Report
	!	^*(06) From Date\*
	!	.b
	!	.lm +5
	!	The ^*From Date\* field enters the date from which the report
	!	is to begin printing.
	!	.b
	!	A blank field will cause the report to begin with the earliest
	!	dated item in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	TO_DATE$ = EDIT$(DATE_STOREDATE(UTL_REPORTX::OPTDEF(6%)), -1%)

	!++
	! Abstract:FLD07
	!	.x To Date>Items Ordered Summary Report
	!	^*(07) To Date\*
	!	.b
	!	.lm +5
	!	The ^*To Date\* field specifies the date with which the
	!	report will end printing.
	!	.b
	!	A blank field will cause the report to end with the most recent date
	!	in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	DETAIL_RPT$ = EDIT$(UTL_REPORTX::OPTDEF(7%), -1%)

	!++
	! Abstract:FLD08
	!	.x Detail Report>Items Ordered Summary Report
	!	^*(08) Detail Report\*
	!	.b
	!	.lm +5
	!	The ^*Detail Report\* field prints the report
	!	in detail or to print an abbreviated report.
	!	.table 3,25
	!	.te
	!	^*Y\* - Yes detail
	!	.te
	!	^*N\* - No detail
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

310	!
	! Open Order Line file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.OPN"
	USE
		FILENAME$ = "OE_REGLINE"
		CONTINUE HelpError
	END WHEN

315	!
	! Open Product file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

600	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.OPN"
	USE
		CONTINUE 610 IF ERR = 5%
		FILENAME$ = "PO_REG_LINE"
		CONTINUE HelpError
	END WHEN

610	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_REG_SUB_LINE.OPN"
	USE
		CONTINUE 620 IF ERR = 5%
		FILENAME$ = "PO_REG_SUB_LINE"
		CONTINUE HelpError
	END WHEN

620	!

 ReportTitle:
	!
	! Title
	!
	SELECT SORTBY$

	CASE "P"
		K_NUM% = 0%
		TITLE$(1%) = "ORDER ITEMS BY PRODUCT NUMBER"

	CASE "T"
		K_NUM% = 1%
		TITLE$(1%) = "ORDER ITEMS BY PRODUCT TYPE"

	CASE "C"
		K_NUM% = 2%
		TITLE$(1%) = "ORDER ITEMS BY PRODUCT CATEGORY"

	CASE "D"
		K_NUM% = 3%
		TITLE$(1%) = "ORDER ITEMS BY PRODUCT DESCRIPTION"

	CASE "S"
		K_NUM% = 4%
		TITLE$(1%) = "ORDER ITEMS BY PRODUCT SECONDARY CODE"

	END SELECT

	!
	! Heading
	!
	TITLE$(2%) = "Order Entry System"
	TITLE$(3%) = "From " + PRNT_DATE(FROM_DATE$, 8%) + &
		" To " + PRNT_DATE(TO_DATE$, 8%)
	TITLE$(3%) = "Before " + PRNT_DATE(TO_DATE$, 8%) &
		IF FROM_DATE$ = ""
	TITLE$(3%) = "After " + PRNT_DATE(FROM_DATE$, 8%) &
		IF TO_DATE$ = ""
	TITLE$(3%) = "For All Dates" &
		IF FROM_DATE$ + TO_DATE$ = ""
	LN% = 4%
	IF (WILDCUSNUM$ <> "")
	THEN
		TITLE$(LN%) = "Customer " + WILDCUSNUM$
		LN% = LN% + 1%
	END IF

	TITLE$(LN%) = ""

	TITLE$(LN% + 1%) = "Product        Description          " + &
		"    Order# OrdDate  CusName" + &
		"               QtyBO   Onhand  Usable OnOrder    Ext Cost   Bo Cost"

	TITLE$(LN% + 2%) = "."

	!
	! Check to see if the Order Date
	! is in desired range
	!
	FROM_DATE$ = "01010001" &
		IF FROM_DATE$ = ""
	TO_DATE$   = "31129999" &
		IF TO_DATE$   = ""

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PD_PRODUCT.CH%, KEY #K_NUM%
		ELSE
			FIND #PD_PRODUCT.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
	!
	! Main loop starts here
	!
	GOTO ExitProgram &
		IF UTL_REPORTX::STAT

	IF PRINT_FLAG%
	THEN
		!
		! See if we can get a balance for the product
		!
		V% = IC_READ_35BALANCE_ALL(PD_PRODUCT::PRODUCT_NUM, &
			OE_REGHEADER_READ::LOCATION, BALANCE(,))

		ON_HAND = BALANCE(1%, 1%) + BALANCE(1%, 2%) + BALANCE(1%, 3%)
		ON_ORDER = BALANCE(3%, 1%) + BALANCE(3%, 2%) + BALANCE(3%, 3%)
		ALLOC = BALANCE(2%, 1%) + BALANCE(2%, 2%) + BALANCE(2%, 3%)

		GOSUB POData

		NETQTY = MAX(0, MIN(ON_HAND, TOTAL))

		UNIT_COST = PC_READ_COST( &
			THIS_REGLINE::PRODUCT, &
			OE_REGHEADER_READ::LOCATION, &
			DATE_TODAY, "")

		NETCOST = FUNC_ROUND(NETQTY * UNIT_COST, 2%)
		BOCOST = FUNC_ROUND(TOTAL * UNIT_COST, 2%)

		TEXT$ = PD_PRODUCT::PRODUCT_NUM + " " + &
				LEFT$(PD_PRODUCT::DESCRIPTION, 20%) + " " + &
			SPACE$(8%) + &
			"Product Total:" + &
			SPACE$(19%) + &
			FORMAT$(TOTAL, "####### ") + &
			FORMAT$(ON_HAND, "####### ") + &
			FORMAT$(NETQTY, "####### ") + &
			FORMAT$(PO_BAL_QTY, "#######") + &
			FORMAT$(NETCOST, "#,###,###.##") + &
			FORMAT$(BOCOST, "#,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

		GRAND_TOTAL = GRAND_TOTAL + TOTAL
		GRAND_ONHAND = GRAND_ONHAND + ON_HAND
		GRAND_NETQTY = GRAND_NETQTY + NETQTY
		GRAND_NETCOST = GRAND_NETCOST + NETCOST
		GRAND_BOCOST = GRAND_BOCOST + BOCOST
		GRAND_PO_BAL_QTY = GRAND_PO_BAL_QTY + PO_BAL_QTY

	END IF

	!
	! Get next Product record
	!
17020	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

	!
	! Initialize variables
	!
	EXT_LINE_TOTAL = EXT_LINE_TOTAL + EXT_LINE
	LINE_COST_TOTAL = LINE_COST_TOTAL + LINE_COST
	PRINT_FLAG% = 0%
	TOTAL       = 0.0
	EXT_LINE    = 0.0
	LINE_COST   = 0.0

	!
	! Check current record, see if should be printed
	!
	SELECT SORTBY$
	CASE "P"
		GOTO ExitTotal &
			IF (PD_PRODUCT::PRODUCT_NUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		IF WLDCRD$ <> ""
		THEN
			GOTO GetNextRec &
				IF COMP_STRING(EDIT$(PD_PRODUCT::PRODUCT_NUM, &
				-1%), WLDCRD$) = 0%
		END IF

	CASE "T"
		GOTO ExitTotal &
			IF (PD_PRODUCT::PROD_TYPE > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		IF WLDCRD$ <> ""
		THEN
			GOTO GetNextRec &
				IF COMP_STRING(EDIT$(PD_PRODUCT::PROD_TYPE, &
				-1%), WLDCRD$) = 0%
		END IF

	CASE "C"
		GOTO ExitTotal &
			IF (PD_PRODUCT::CATEGORY > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		IF WLDCRD$ <> ""
		THEN
			GOTO GetNextRec &
				IF COMP_STRING(EDIT$(PD_PRODUCT::CATEGORY, &
				-1%), WLDCRD$) = 0%
		END IF

	CASE "D"
		GOTO ExitTotal &
			IF (PD_PRODUCT::DESCRIPTION > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		IF WLDCRD$ <> ""
		THEN
			GOTO GetNextRec &
				IF COMP_STRING(EDIT$(PD_PRODUCT::DESCRIPTION, &
				-1%), WLDCRD$) = 0%
		END IF

	CASE "S"
		GOTO ExitTotal &
			IF (PD_PRODUCT::SECONDARY_CODE > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		IF WLDCRD$ <> ""
		THEN
			GOTO GetNextRec &
				IF COMP_STRING(EDIT$( &
				PD_PRODUCT::SECONDARY_CODE, -1%), WLDCRD$) = 0%
		END IF

	END SELECT

17100	WHEN ERROR IN
		FIND #OE_REGLINE.CH%, &
			KEY #1% GE PD_PRODUCT::PRODUCT_NUM, &
			REGARDLESS
	USE
		CONTINUE GetNextRec IF ERR = 155%
		FILENAME$ = "OE_REGLINE"
		CONTINUE HelpError
	END WHEN

	THIS_REGLINE = OE_REGLINE
	THIS_BO = 0.0

 GetRegLine:
17120	WHEN ERROR IN
		GET #OE_REGLINE.CH%, REGARDLESS
	USE
		CONTINUE GetNextRec IF ERR = 11%
		FILENAME$ = "OE_REGLINE"
		CONTINUE HelpError
	END WHEN

	GOTO ExitProgram &
		IF UTL_REPORTX::STAT

	IF OE_REGLINE::PRODUCT <> PD_PRODUCT::PRODUCT_NUM
	THEN
		GOSUB DumpLine
		GOTO GetNextRec
	END IF

	IF OE_REGLINE::ORDNUM <> THIS_REGLINE::ORDNUM
	THEN
		GOSUB DumpLine
		THIS_REGLINE = OE_REGLINE
		THIS_BO = 0.0
	END IF

	!
	! Has anything been backordered?
	!
	SELECT OE_REGLINE::TRANTYPE
	CASE "01"
		THIS_BO = THIS_BO + OE_REGLINE::QTY

	CASE "02"
		THIS_BO = THIS_BO - OE_REGLINE::QTY

	CASE "03"
		THIS_BO = THIS_BO - OE_REGLINE::QTY

	END SELECT

	GOTO GetRegline

	!*******************************************************************
	! Write out one line
	!*******************************************************************

 DumpLine:
	V% = OE_READ_REGHEADER(THIS_REGLINE::ORDNUM, OE_REGHEADER_READ)

	IF WILDCUSNUM$ <> ""
	THEN
		IF COMP_STRING(OE_REGHEADER_READ::CUSNUM, WILDCUSNUM$) = 0%
		THEN
			GOTO DumpLineExit
		END IF
	END IF


	THIS_BO = MAX(0, THIS_BO)

	IF OE_REGHEADER_READ::ORDDATE >= FROM_DATE$ AND &
		OE_REGHEADER_READ::ORDDATE <= TO_DATE$ AND &
		FUNC_ROUND(THIS_BO, 0%) <> 0.0
	THEN
		PRICE = FUNC_ROUND(THIS_BO * THIS_REGLINE::PRICE, 2%)

		UNIT_COST = PC_READ_COST( &
			THIS_REGLINE::PRODUCT, &
			OE_REGHEADER_READ::LOCATION, &
			DATE_TODAY, "")
		COST = FUNC_ROUND(THIS_BO * UNIT_COST, 2%)

		EXT_LINE    = EXT_LINE + PRICE
		LINE_COST   = LINE_COST + COST
		TOTAL       = TOTAL + THIS_BO

		SELECT DETAIL_RPT$

		CASE "Y"
			!
			! Check current Custom record
			!
			V% = AR_EXAM_CUSTOM(OE_REGHEADER_READ::CUSNUM, &
				AR_35CUSTOM_EXAM)

			TEXT$ = PD_PRODUCT::PRODUCT_NUM + " " + &
				LEFT$(PD_PRODUCT::DESCRIPTION, 20%) + " " + &
				THIS_REGLINE::ORDNUM + " " + &
				PRNT_DATE(OE_REGHEADER_READ::ORDDATE, 6%) + " " + &
				LEFT$(AR_35CUSTOM_EXAM::CUSNAM, 20%) + " " + &
				FORMAT$(THIS_BO, "####### ")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
			GOTO ExitProgram &
				IF UTL_REPORTX::STAT

		END SELECT

		PRINT_FLAG% = -1%
	END IF

 DumpLineExit:
	RETURN

 ExitTotal:
	TEXT$ = SPACE$(14%) + &
		SPACE$(8%) + &
		"  Grand Total:" + &
		SPACE$(41%) + &
		FORMAT$(GRAND_TOTAL, "####### ") + &
		FORMAT$(GRAND_ONHAND, "####### ") + &
		FORMAT$(GRAND_NETQTY, "####### ") + &
		FORMAT$(GRAND_PO_BAL_QTY, "#######") + &
		FORMAT$(GRAND_NETCOST, "#,###,###.##") + &
		FORMAT$(GRAND_BOCOST, "#,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -2%)

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

	!*******************************************************************
	! Generqte PO data
	!*******************************************************************

 POData:
17500	PO_ORD_QTY, PO_REC_QTY, PO_CAN_QTY, PO_BAL_QTY = 0.0

	WHEN ERROR IN
		FIND #PO_REG_LINE.CH%, &
			KEY #4% EQ PD_PRODUCT::PRODUCT_NUM, &
			REGARDLESS
	USE
		CONTINUE FindWPRegline IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PO_REG_LINE"
		CONTINUE HelpError
	END WHEN

 POGetNextRec:
17520	WHEN ERROR IN
		GET #PO_REG_LINE.CH%, REGARDLESS
	USE
		CONTINUE FindWPRegline IF ERR = 11%
		FILENAME$ = "PO_REG_LINE"
		CONTINUE HelpError
	END WHEN

	GOTO FindWPRegline &
		IF PO_REG_LINE::PRODUCT <> PD_PRODUCT::PRODUCT_NUM

17600	WHEN ERROR IN
		FIND #PO_REG_SUB_LINE.CH%, &
			KEY #0% EQ PO_REG_LINE::PO + PO_REG_LINE::PO_LINE, &
			REGARDLESS
	USE
		CONTINUE POGetNextRec IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PO_REG_SUB_LINE"
		CONTINUE HelpError
	END WHEN

	ORD_QTY, REC_QTY, CAN_QTY = 0.0
	REC_DATE$ = "        "

 POGetSRec:
17620	WHEN ERROR IN
		GET #PO_REG_SUB_LINE.CH%, REGARDLESS
	USE
		CONTINUE POPrintPOInfo IF ERR = 11%
		FILENAME$ = "PO_REG_SUB_LINE"
		CONTINUE HelpError
	END WHEN

	GOTO POPrintPOInfo &
		IF PO_REG_SUB_LINE::PO <> PO_REG_LINE::PO OR &
		PO_REG_SUB_LINE::PO_LINE <> PO_REG_LINE::PO_LINE

	SELECT PO_REG_SUB_LINE::PO_ACTION

	CASE "01"
		ORD_QTY = ORD_QTY + PO_REG_SUB_LINE::QTY
		PRICE = PO_REG_SUB_LINE::PRICE

	CASE "02"
		REC_QTY = REC_QTY + PO_REG_SUB_LINE::QTY

		REC_DATE$ = PO_REG_SUB_LINE::ACTION_DATE &
			IF PO_REG_SUB_LINE::ACTION_DATE > REC_DATE$

	CASE "03"
		CAN_QTY = CAN_QTY + PO_REG_SUB_LINE::QTY

	CASE "09"
		PRICE = PO_REG_SUB_LINE::PRICE

	END SELECT

	GOTO POGetSRec

 POPrintPOInfo:
	BALANCE = ORD_QTY - REC_QTY - CAN_QTY
	BALANCE = 0.0 &
		IF BALANCE < 0.0

	!
	! Get Vendor information
	!
	V% = AP_EXAM_VENDOR(PO_REG_LINE::VENDOR, AP_VENDOR_EXAM)

	!
	! Print the stuff out
	!
	IF (BALANCE <> 0.0)
	THEN
		TEXT$ = PD_PRODUCT::PRODUCT_NUM + " " + &
			LEFT$(PD_PRODUCT::DESCRIPTION, 20%) + " " + &
			CONV_STRING(PO_REG_LINE::PO, CMC$_LEFT) + " " + &
			PRNT_DATE(PO_REG_LINE::ORDDATE, 6%) + " " + &
			LEFT(AP_VENDOR_EXAM::VENNAM, 30%) + " " + &
			FORMAT$(BALANCE, "              #######")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitTotal IF UTL_REPORTX::STAT
	END IF

	PO_ORD_QTY = PO_ORD_QTY + ORD_QTY
	PO_REC_QTY = PO_REC_QTY + REC_QTY
	PO_CAN_QTY = PO_CAN_QTY + CAN_QTY
	PO_BAL_QTY = PO_BAL_QTY + BALANCE

	IF REC_QTY <> 0.0
	THEN
		W_QTY = W_QTY + REC_QTY
		W_PRICE = W_PRICE + REC_QTY * PRICE
	ELSE
		W_QTY = W_QTY + ORD_QTY
		W_PRICE = W_PRICE + ORD_QTY * PRICE
	END IF

	ORD_QTY, REC_QTY, CAN_QTY = 0.0

	GOTO POGetNextRec

 FindWPRegline:
	RETURN

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
