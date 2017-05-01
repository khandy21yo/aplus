1	%TITLE "Shipped Items by Product Report"
	%SBTTL "OE_RPRT_SHIPPROD"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1991 BY
	!
	! Computer Management Center, Inc.
	! Idaho Falls, Idaho.
	!
	! This software is furnished under a license and may be
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
	! ID:OE029
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Shipped Items by Product\* Report contains columns for the following
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
	!	Quantity Shipped
	!	.te
	!	Price
	!	.te
	!	Cost
	!	.te
	!	Gross Margin
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Shipped Items by Product
	!	.x Shipped Items by Product>Report
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_RPRT_SHIPPROD/LINE
	!	$ LINK/EXE=OE_EXE: OE_RPRT_SHIPPROD, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE OE_RPRT_SHIPPROD.OBJ;*
	!
	! Author:
	!
	!	11/12/91 - Frank F. Starman
	!
	! Modification History:
	!
	!	11/25/91 - Dan Perkins
	!		Changed date from OE_REGHEADER to OE_REGLINE
	!		in order to read the ship date.  Changed
	!		title from Order Items to Shipped Items.
	!
	!	04/09/92 - Dan Perkins
	!		Use function CONV_STRING to manipulate ORDER NUMBER.
	!
	!	04/27/92 - Kevin Handy
	!		Clean up (check)
	!
	!	04/08/93 - Kevin Handy
	!		Clean up (check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/06/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/25/97 - Kevin Handy
	!		Lose unecessary function definitions
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	08/16/99 - Kevin Handy
	!		Clean up source code
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

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.HB"
	DECLARE			OE_REGHEADER_CDD	OE_REGHEADER_READ

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	DECLARE			AR_35CUSTOM_CDD		AR_35CUSTOM_EXAM

	!
	! Declare external functions
	!
	EXTERNAL LONG	FUNCTION OE_READ_REGHEADER
	EXTERNAL LONG	FUNCTION AR_EXAM_CUSTOM

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
	!	.lm +5
	!	.b
	!	The ^*Sort by\* field determines if the
	!	report is to be printed in product number order, product
	!	description order, category, or type.
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
	!	The ^*From Item\* field enters the item with which
	!	the report is to begin printing.  The value entered must
	!	be in agreement with field (01) Sort by.
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
	!	The ^*To Item\* field enters an item with which the
	!	report will end printing.  The value entered must be in
	!	agreement with field (01) Sort by.
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
	!	Technique.  The value entered must be in agreement with
	!	field (01) Sort by.
	!	.b
	!	For information on "Wildcarding" techniques refer to Appendix B.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard
	!
	!--

	FROM_DATE$ = EDIT$(DATE_STOREDATE(UTL_REPORTX::OPTDEF(5%)), -1%)

	!++
	! Abstract:FLD06
	!	.x From Date>Shipped Items Report
	!	^*(06) From Date\*
	!	.b
	!	.lm +5
	!	The ^*From Date\* field enters the date from which the
	!	report is to begin printing.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
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
	!	.x To Date>Shipped Items Report
	!	^*(07) To Date\*
	!	.b
	!	.lm +5
	!	The ^*To Date\* field enters the date with which the report
	!	is to end printing.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.b
	!	A blank field will cause the report to end with the most recent date in
	!	the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	DETAIL_RPT$ = EDIT$(UTL_REPORTX::OPTDEF(7%), -1%)

	!++
	! Abstract:FLD08
	!	.x Detail Report>Shipped Items Report
	!	^*(08) Detail Report\*
	!	.b
	!	.lm +5
	!	The ^*Detail Report\* field chooses whether the report
	!	will print in detail or be abbreviated.
	!	.b
	!	Valid entries are:
	!	.table 3,25
	!	.te
	!	^*Y\* - Yes, print in detail
	!	.te
	!	^*N\* - No, abbreviated
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

	MARGIN_FLAG$ = EDIT$(UTL_REPORTX::OPTDEF(8%), -1%)

	!++
	! Abstract:FLD09
	!	^*(09) Show Totals\*
	!	.b
	!	.lm +5
	!	The Show Totals field indicates if totals are to be
	!	shown on the report.
	!	.b
	!	Valid entries are:
	!	.table 3,25
	!	.te
	!	^*Y\* - Yes
	!	.te
	!	^*N\* - No
	!	.lm -5
	!	.end table
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
	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"

 ReportTitle:
	!
	! Title
	!
	SELECT SORTBY$
	CASE "P"
		K_NUM% = 0%
		TITLE$(1%) = "SHIPPED ITEMS BY PRODUCT NUMBER"

	CASE "T"
		K_NUM% = 1%
		TITLE$(1%) = "SHIPPED ITEMS BY PRODUCT TYPE"

	CASE "C"
		K_NUM% = 2%
		TITLE$(1%) = "SHIPPED ITEMS BY PRODUCT CATEGORY"

	CASE "D"
		K_NUM% = 3%
		TITLE$(1%) = "SHIPPED ITEMS BY PRODUCT DESCRIPTION"
	END SELECT

	TITLE$(2%) = "Order Entry System"
	TITLE$(3%) = "From " + PRNT_DATE(FROM_DATE$, 8%) + &
		" To " + PRNT_DATE(TO_DATE$, 8%)
	TITLE$(3%) = "Before " + PRNT_DATE(TO_DATE$, 8%) IF FROM_DATE$ = ""
	TITLE$(3%) = "After " + PRNT_DATE(FROM_DATE$, 8%) IF TO_DATE$ = ""
	TITLE$(3%) = "For All Dates" IF FROM_DATE$ + TO_DATE$ = ""
	TITLE$(4%) = ""

	!
	! Heading
	!
	!	'     1234567890123456789012345678901234567890

	SELECT DETAIL_RPT$ + MARGIN_FLAG$

	CASE "NN"
		TITLE$(5%) = "Product        Description          " + &
			"                    PT PCat  QtyShp     ExtPrice"

	CASE "NY"
		TITLE$(5%) = "Product        Description          " + &
			"                    PT PCat  QtyShp     ExtPrice" + &
			"      TotCost       Margin Perc"

	CASE "YN"
		TITLE$(5%) = "Product        Description          " + &
			"PT PCat  QtyShp ShpDate  CusName" + &
			"              Order#         ExtPrice"

	CASE "YY"
		TITLE$(5%) = "Product        Description          " + &
			"PT PCat  QtyShp ShpDate  CusName" + &
			"              Order#         ExtPrice" + &
			"      TotCost       Margin"
	END SELECT

	TITLE$(6%) = "."

	!
	! Check to see if the Ship Date
	! is in desired range
	!
	FROM_DATE$ = "01010001" IF FROM_DATE$ = ""
	TO_DATE$   = "31129999" IF TO_DATE$   = ""

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	!
	! If from category blank then reset ORDERITEM file
	! else try to find the first record
	!
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
		IF ERR = 154%
		THEN
			!
			! Wait for 5 seconds if record is lock
			!
			SLEEP 5%
			RETRY
		END IF
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	IF PRINT_FLAG%
	THEN
		SELECT DETAIL_RPT$ + MARGIN_FLAG$

		CASE "NN"
			TEXT$ = PD_PRODUCT::PRODUCT_NUM	+ " " + &
				PD_PRODUCT::DESCRIPTION + " " + &
				PD_PRODUCT::PROD_TYPE + " " + &
				PD_PRODUCT::CATEGORY + " " + &
				FORMAT$(TOTAL, "#######") + " " + &
				FORMAT$(EXT_LINE, "#,###,###.##")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		CASE "NY"
			PERC= 0.0
			PERC = (EXT_LINE - LINE_COST) / EXT_LINE * 100.0 &
				IF EXT_LINE <> 0.0

			TEXT$ = PD_PRODUCT::PRODUCT_NUM + " " + &
				PD_PRODUCT::DESCRIPTION + " " + &
				PD_PRODUCT::PROD_TYPE + " " + &
				PD_PRODUCT::CATEGORY + " " + &
				FORMAT$(TOTAL, "####### ") + &
				FORMAT$(EXT_LINE, "#,###,###.## ") + &
				FORMAT$(LINE_COST, "#,###,###.## ") + &
				FORMAT$(EXT_LINE - LINE_COST, "#,###,###.## ") + &
				FORMAT$(PERC, "###%")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		CASE "YN"
			TEXT$ = PD_PRODUCT::PRODUCT_NUM + &
				SPACE$(16%) + &
				"Product Total:" + &
				FORMAT$(TOTAL, "#######") + &
				SPACE$(42%) + &
				FORMAT$(EXT_LINE, "#,###,###.##")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -2%)

		CASE ELSE
			TEXT$ = PD_PRODUCT::PRODUCT_NUM + &
				SPACE$(16%) + &
				"Product Total:" + &
				FORMAT$(TOTAL, "#######") + &
				SPACE$(42%) + &
				FORMAT$(EXT_LINE, "#,###,###.##") + " " + &
				FORMAT$(LINE_COST, "#,###,###.##") + " " + &
				FORMAT$(EXT_LINE - LINE_COST, "#,###,###.##")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
		END SELECT

		EXT_LINE_TOTAL = EXT_LINE_TOTAL + EXT_LINE
		LINE_COST_TOTAL = LINE_COST_TOTAL + LINE_COST

	END IF

	!
	! Get next Product record
	!
17020	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, REGARDLESS
	USE
		IF ERR = 154%
		THEN
			!
			! Wait for 5 seconds if record is lock
			!
			SLEEP 5%
			RETRY
		END IF
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

	!
	! Initialize variables
	!
	PRINT_FLAG% = 0%
	TOTAL = 0.0
	EXT_LINE = 0.0
	LINE_COST = 0.0

	!
	! Check current record, see if should be printed
	!
	SELECT SORTBY$

	CASE "P"
		GOTO ExitTotal IF (PD_PRODUCT::PRODUCT_NUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(PD_PRODUCT::PRODUCT_NUM, -1%), &
			WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "T"
		GOTO ExitTotal IF (PD_PRODUCT::PROD_TYPE > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(PD_PRODUCT::PROD_TYPE, -1%), &
			WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "C"
		GOTO ExitTotal IF (PD_PRODUCT::CATEGORY > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(PD_PRODUCT::CATEGORY, -1%), &
			WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "D"
		GOTO ExitTotal IF (PD_PRODUCT::DESCRIPTION > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(PD_PRODUCT::DESCRIPTION, -1%), &
			WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	END SELECT

17100	WHEN ERROR IN
		FIND #OE_REGLINE.CH%, &
			KEY #1% GE PD_PRODUCT::PRODUCT_NUM, &
			REGARDLESS
	USE
		IF ERR = 154%
		THEN
			!
			! Wait for 5 seconds if record is lock
			!
			SLEEP 5%
			RETRY
		END IF
		CONTINUE GetNextRec IF ERR = 155%
		FILENAME$ = "OE_REGLINE"
		CONTINUE HelpError
	END WHEN

 GetRegLine:
17120	WHEN ERROR IN
		GET #OE_REGLINE.CH%, REGARDLESS
	USE
		IF ERR = 154%
		THEN
			!
			! Wait for 5 seconds if record is lock
			!
			SLEEP 5%
			RETRY
		END IF
		CONTINUE GetNextRec IF ERR = 11%
		FILENAME$ = "OE_REGLINE"
		CONTINUE HelpError
	END WHEN

	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOTO GetNextRec IF OE_REGLINE::PRODUCT <> PD_PRODUCT::PRODUCT_NUM

	!
	! Has anything been ordered ?
	!
	GOTO GetRegLine IF OE_REGLINE::TRANTYPE <> "02"

	IF OE_REGLINE::TDATE >= FROM_DATE$ AND OE_REGLINE::TDATE <= TO_DATE$
	THEN
		V% = OE_READ_REGHEADER(OE_REGLINE::ORDNUM, OE_REGHEADER_READ)

		PRICE = FUNC_ROUND(OE_REGLINE::QTY * OE_REGLINE::PRICE, 2%)
		COST = FUNC_ROUND(OE_REGLINE::QTY * OE_REGLINE::COST, 2%)

		EXT_LINE    = EXT_LINE + PRICE
		LINE_COST   = LINE_COST + COST
		TOTAL       = TOTAL + OE_REGLINE::QTY

		SELECT DETAIL_RPT$ + MARGIN_FLAG$

		CASE "YY"
			!
			! Check current Custom record
			!
			V% = AR_EXAM_CUSTOM(OE_REGHEADER_READ::CUSNUM, &
				AR_35CUSTOM_EXAM)

			TEXT$ = PD_PRODUCT::PRODUCT_NUM + " " + &
				LEFT$(PD_PRODUCT::DESCRIPTION, 20%) + " " + &
				PD_PRODUCT::PROD_TYPE + " " + &
				PD_PRODUCT::CATEGORY + " " + &
				FORMAT$(OE_REGLINE::QTY, "#######") + " " + &
				PRNT_DATE(OE_REGLINE::TDATE, 6%) + " " + &
				LEFT$(AR_35CUSTOM_EXAM::CUSNAM, 20%) + " " + &
				CONV_STRING(OE_REGLINE::ORDNUM, CMC$_LEFT) + " " + &
				FORMAT$(PRICE, "#,###,###.##") + " " + &
				FORMAT$(COST, "#,###,###.##") + " " + &
				FORMAT$(PRICE - COST, "#,###,###.##")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
			GOTO ExitProgram IF UTL_REPORTX::STAT

		CASE "YN"
			!
			! Check current Custom record
			!
			V% = AR_EXAM_CUSTOM(OE_REGHEADER_READ::CUSNUM, &
				AR_35CUSTOM_EXAM)

			TEXT$ = PD_PRODUCT::PRODUCT_NUM + " " + &
				LEFT$(PD_PRODUCT::DESCRIPTION, 20%) + " " + &
				PD_PRODUCT::PROD_TYPE + " " + &
				PD_PRODUCT::CATEGORY + " " + &
				FORMAT$(OE_REGLINE::QTY, "#######") + " " + &
				PRNT_DATE(OE_REGLINE::TDATE, 6%) + " " + &
				LEFT$(AR_35CUSTOM_EXAM::CUSNAM, 20%) + " " + &
				CONV_STRING(OE_REGLINE::ORDNUM, CMC$_LEFT) + " " + &
				FORMAT$(PRICE, "#,###,###.##")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
			GOTO ExitProgram IF UTL_REPORTX::STAT

		END SELECT

		PRINT_FLAG% = -1%
	END IF

	GOTO GetRegline

 ExitTotal:
	SELECT DETAIL_RPT$ + MARGIN_FLAG$

	CASE "YN"
		TEXT$ = SPACE$(32%) + &
			"Grand Total:" + &
			SPACE$(49%) + &
			FORMAT$(EXT_LINE_TOTAL, "#,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)

	CASE "YY"
		TEXT$ = SPACE$(32%) + &
			"Grand Total:" + &
			SPACE$(49%) + &
			FORMAT$(EXT_LINE_TOTAL, "#,###,###.##") + " " + &
			FORMAT$(LINE_COST_TOTAL, "#,###,###.##") + " " + &
			FORMAT$(EXT_LINE_TOTAL - LINE_COST_TOTAL, "#,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)

	CASE "NN"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
		TEXT$ = SPACE$(32%) + &
			"Grand Total:" + &
			SPACE$(28%) + &
			FORMAT$(EXT_LINE_TOTAL, "#,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)

	CASE "NY"
		PERC = 0.0
		PERC = (EXT_LINE_TOTAL - LINE_COST_TOTAL) / EXT_LINE_TOTAL * &
			100.0 &
			IF EXT_LINE_TOTAL <> 0.0
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -2%)
		TEXT$ = SPACE$(32%) + &
			"Grand Total:" + &
			SPACE$(28%) + &
			FORMAT$(EXT_LINE_TOTAL, "#,###,###.##") + " " + &
			FORMAT$(LINE_COST_TOTAL, "#,###,###.##") + " " + &
			FORMAT$(EXT_LINE_TOTAL - LINE_COST_TOTAL, "#,###,###.##") + " " + &
			FORMAT$(PERC, "###%")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)

	END SELECT

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

	SELECT ERR
	CASE 154%
		!
		! Wait for 5 seconds if record is lock
		!
		SLEEP 5%
		RESUME
	END SELECT

	!
	! Resume to display untrapped error
	!
	RESUME HelpError

32767	END
