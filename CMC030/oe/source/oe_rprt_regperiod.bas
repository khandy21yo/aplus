1	%TITLE "Order Status by Period"
	%SBTTL "OE_RPRT_REGPERIOD"
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
	! ID:OE028
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Order Status by Period\* Report contains the following fields of information:
	!	.table 3,25
	!	.te
	!	Document Number	Sales Type
	!	.te
	!	Sales Category	Customer Number
	!	.te
	!	Customer Name	Customer's State
	!	.te
	!	Order Date	Product
	!	.te
	!	Product Description	Quantity Ordered
	!	.te
	!	Quantity Shipped	Quantity Canceled
	!	.te
	!	Quantity Backordered	Unit Cost
	!	.te
	!	Unit Price	Promo
	!	.te
	!	Misc Charges	Order Discount %
	!	.te
	!	Order Total	#
	!	.end table
	!	.lm -5
	!
	! Index:
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_RPRT_REGPERIOD/LINE
	!	$ LINK/EXE=OE_EXE: OE_RPRT_REGPERIOD, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE OE_RPRT_REGPERIOD.OBJ;*
	!
	! AUTHOR:
	!
	!	08/17/92 - Dan Perkins
	!
	! MODIFICATION HISTORY:
	!
	!	09/21/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	01/29/93 - Dan Perkins
	!		Added option to not print zero balances if desired.
	!
	!	06/17/94 - Kevin Handy
	!		Added code for ::MISCH2.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/06/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/12/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include SCOPE.COM
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Include cdd
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.HB"
	MAP (OE_REGHEADER)	OE_REGHEADER_CDD	OE_REGHEADER

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.HB"
	DECLARE			OE_REGLINE_CDD		OE_REGLINE_READ

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	DECLARE			AR_35CUSTOM_CDD		AR_CUSTOM_EXAM

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	!
	! Declare external functions
	!
	EXTERNAL STRING	FUNCTION CONV_STRING
	EXTERNAL LONG	FUNCTION OE_READ_REGLINE
	EXTERNAL LONG	FUNCTION PD_EXAM_PRODUCT
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
	!	^*(01) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field
	!	determines the order in which the
	!	report will print.
	!	.b
	!	Valid settings are:
	!	.table 3,25
	!	.te
	!	^*D\* - Document Number
	!	.te
	!	^*T\* - Sale Type
	!	.te
	!	^*C\* - Sale Category
	!	.te
	!	^*N\* - Customer Number
	!	.end table
	!	A setting is required in this field.
	!	. lm -5
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
	!	The ^*From Item\* field enters the item from which the
	!	report will begin printing.  The value entered must
	!	be in agreement with field (01) Sort by.
	!	.b
	!	A blank field will cause the report to begin with the first item in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field specifies the item at which the
	!	report will end printing.  The value entered must be in
	!	agreement with field (01) Sort by.
	!	.b
	!	A blank field will cause the report to end with the last item in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects designated items to be
	!	printed by entering a wildcard. The wildcard must be in agreement with the
	!	contents of field (01) Sort by.
	!	.b
	!	See Appendix B for more information regarding wildcarding techniques.
	!	.lm -5
	!
	! Index:
	!
	!--

	FROM_DATE$ = DATE_STOREDATE(TRM$(UTL_REPORTX::OPTDEF(5%)))

	!++
	! Abstract:FLD06
	!	^*(06) From Date\*
	!	.b
	!	.lm +5
	!	The ^*From Date\* field refers to the starting date for which report
	!	data will be printed.
	!	.lm -5
	!
	! Index:
	!
	!--

	TO_DATE$ = DATE_STOREDATE(TRM$(UTL_REPORTX::OPTDEF(6%)))

	!++
	! Abstract:FLD07
	!	^*(07) To Date\*
	!	.b
	!	.lm +5
	!	The ^*To Date\* field refers to the ending date for which report
	!	data will be printed.
	!	.lm -5
	!
	! Index:
	!
	!--

	ZERO_BAL$ = EDIT$(UTL_REPORTX::OPTDEF(7%), -1%)

	!++
	! Abstract:FLD08
	!	^*(08) Print Zero Balances\*
	!	.b
	!	.lm +5
	!	The ^*Print Zero Balances\* field allows the user to decide
	!	whether to print orders with zero balances or not.
	!	.b
	!	Valid settings are:
	!	.table 3,25
	!	.te
	!	^*Y\* - Yes
	!	.te
	!	^*N\* - No
	!	.end table
	!	A setting is required in this field.
	!	. lm -5
	!
	! Index:
	!
	!--

300	!
	! Open Order Register file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.OPN"
	USE
		FILENAME$ = "OE_REGHEADER"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	! Select which method to sort by
	!
	SELECT SORTBY$

	CASE "D"
		K_NUM% = 0%
		TITLE$(1%) = "REGISTER PERIOD REPORT BY DOCUMENT NUMBER"

		FROM_ITEM$ = SPACE$(LEN(OE_REGHEADER::ORDNUM) - &
			LEN(FROM_ITEM$)) + FROM_ITEM$
		TO_ITEM$ = SPACE$(LEN(OE_REGHEADER::ORDNUM) - &
			LEN(TO_ITEM$)) + TO_ITEM$

	CASE "T"
		K_NUM% = 1%
		TITLE$(1%) = "REGISTER PERIOD REPORT BY SALE TYPE"

	CASE "C"
		K_NUM% = 2%
		TITLE$(1%) = "REGISTER PERIOD REPORT BY SALE CATEGORY"

	CASE "N"
		K_NUM% = 3%
		TITLE$(1%) = "REGISTER PERIOD REPORT BY CUSTOMER NUMBER"

	END SELECT


	TITLE$(2%) = "From " + PRNT_DATE(FROM_DATE$, 8%) + &
		" To " + PRNT_DATE(TO_DATE$, 8%)
	TITLE$(2%) = "Before " + PRNT_DATE(TO_DATE$, 8%) IF FROM_DATE$ = ""
	TITLE$(2%) = "After " + PRNT_DATE(FROM_DATE$, 8%) IF TO_DATE$ = ""
	TITLE$(2%) = "For All Dates" IF FROM_DATE$ + TO_DATE$ = ""

	TITLE$(3%) = " Order Entry System"
	TITLE$(4%) = ""

	!
	! Heading
	!
	TITLE$(5%) = "Document#   SalTyp  SalCat  CustNumber  CustName     " + &
                     "                   State  OrdDate"

	TITLE$(6%) = "               Line  Product         Descr           " + &
		"                 QtyOrd       QtyShip     QtyCancel  " + &
		"     QtyBack       Balance"

	TITLE$(7%) = "                            UnitCost     UnitPrice  " + &
		"  Discount       Promo        Misc  NetUnitPrice    " + &
		"   ExtCost      ExtPrice"

	TITLE$(8%) = "."

	!
	! Adjust Dates
	!
	FROM_DATE$ = "01010001" IF FROM_DATE$ = ""
	TO_DATE$   = "31129999" IF TO_DATE$   = ""

	REPORTTOTAL = 0.0

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #OE_REGHEADER.CH%, KEY #K_NUM%
		ELSE
			FIND #OE_REGHEADER.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "OE_REGHEADER"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next Order Register record
	!
	WHEN ERROR IN
		GET #OE_REGHEADER.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "OE_REGHEADER"
		CONTINUE HelpError
	END WHEN

	!
	! See if we are within the date reange
	!
	GOTO  GetNextRec IF OE_REGHEADER::ORDDATE < FROM_DATE$
	GOTO  GetNextRec IF OE_REGHEADER::ORDDATE > TO_DATE$

	!
	! Check current record if should be printed
	!
	SELECT SORTBY$
	CASE "D"
		GOTO ExitTotal IF (OE_REGHEADER::ORDNUM > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$(OE_REGHEADER::ORDNUM, &
			-1%), WLDCRD$) = 0% AND WLDCRD$ <> ""

	CASE "C"
		GOTO ExitTotal IF (OE_REGHEADER::ORDCAT > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$(OE_REGHEADER::ORDCAT, &
			-1%), WLDCRD$) = 0% AND WLDCRD$ <> ""

	CASE "N"
		GOTO ExitTotal IF (OE_REGHEADER::CUSNUM > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$(OE_REGHEADER::CUSNUM, &
			-1%), WLDCRD$) = 0% AND WLDCRD$ <> ""

	CASE "T"
		GOTO ExitTotal IF (OE_REGHEADER::ORDTYPE > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$(OE_REGHEADER::ORDTYPE, &
			-1%), WLDCRD$) = 0% AND WLDCRD$ <> ""

	END SELECT

	!
	! Check current Custom record
	!
	V% = AR_EXAM_CUSTOM(OE_REGHEADER::CUSNUM, AR_CUSTOM_EXAM)

	!
	! Check current Register Line record
	!
	LINE$ = "    "
	ORDERTOTAL = 0.0
	HEADER_PRINTED% = 0%

 GetLine:
	GOTO PrintTotal IF OE_READ_REGLINE(OE_REGHEADER::ORDNUM, LINE$, "GT", &
		OE_REGLINE_READ, QTY()) <> CMC$_NORMAL

	LINE$ = OE_REGLINE_READ::LIN

	GOTO GetLine IF ZERO_BAL$ = "N" AND QTY(0%) = 0.0

	IF HEADER_PRINTED% = 0%
	THEN
		!
		! Print out one line
		!
		TEXT$ = CONV_STRING(OE_REGHEADER::ORDNUM, CMC$_LEFT) + "  " + &
			OE_REGHEADER::ORDTYPE + "      " + &
			OE_REGHEADER::ORDCAT + "    " + &
			OE_REGHEADER::CUSNUM + "  " + &
			LEFT$(AR_CUSTOM_EXAM::CUSNAM, 30%) + "  " + &
			AR_CUSTOM_EXAM::STATE + "     " + &
			PRNT_DATE(OE_REGHEADER::ORDDATE, 8%)

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		HEADER_PRINTED% = -1%
	END IF

	!
	! Get the Product Description
	!
	IF PD_EXAM_PRODUCT(OE_REGLINE_READ::PRODUCT, PD_PRODUCT_EXAM) <> &
		CMC$_NORMAL
	THEN
		PD_PRODUCT_EXAM::DESCRIPTION = ""
	END IF

	!
	! Print out one line
	!
	TEXT$ = CONV_STRING(OE_REGHEADER::ORDNUM, CMC$_LEFT) + "     " + &
		OE_REGLINE_READ::LIN + "  " + &
		OE_REGLINE_READ::PRODUCT + "  " + &
		LEFT$(PD_PRODUCT_EXAM::DESCRIPTION, 25%) + "  " + &
		FORMAT$(QTY(1%), "###,###,###,") + "  " + &
		FORMAT$(QTY(2%), "###,###,###,") + "  " + &
		FORMAT$(QTY(3%), "###,###,###,") + "  " + &
		FORMAT$(QTY(7%), "###,###,###,") + "  " + &
		FORMAT$(QTY(0%), "###,###,###,")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	REAL_PRICE = FUNC_ROUND((1 - OE_REGLINE_READ::DISCOUNT / 100.0) * &
		(OE_REGLINE_READ::PRICE - OE_REGLINE_READ::PROMO), 3%)

	EXT_COST  = FUNC_ROUND(OE_REGLINE_READ::COST * QTY(1%), 2%)

	ORDERLINE = FUNC_ROUND((REAL_PRICE + &
		OE_REGLINE_READ::MISCH + &
		OE_REGLINE_READ::MISCH2) * &
		QTY(1%), 2%)

	ORDERTOTAL = ORDERTOTAL + ORDERLINE

	!HEADER_TAX = FUNC_ROUND((HEADER_TOTAL - HEADER_DISC) * &
	!	OE_ORDERJOUR::SALESTAX / 100.0, 2%)

	!ORDER_TAX = FUNC_ROUND((ORDER_TOTAL - HEADER_DISC) * &
	!	OE_ORDERJOUR::SALESTAX / 100.0, 2%)

	TEXT$ = CONV_STRING(OE_REGHEADER::ORDNUM, CMC$_LEFT) + "     " + &
		OE_REGLINE_READ::LIN + "     " + &
		FORMAT$(OE_REGLINE_READ::COST, "#,###,###.##") + "  " + &
		FORMAT$(OE_REGLINE_READ::PRICE, "#,###,###.##") + "  " + &
		FORMAT$(OE_REGLINE_READ::DISCOUNT, "###,###.##") + "  " + &
		FORMAT$(OE_REGLINE_READ::PROMO, "###,###.##") + "  " + &
		FORMAT$(OE_REGLINE_READ::MISCH + &
			OE_REGLINE_READ::MISCH2, "###,###.##") + "  " + &
		FORMAT$(REAL_PRICE, "#,###,###.##") + "  " + &
		FORMAT$(EXT_COST, "#,###,###.##") + "  " + &
		FORMAT$(ORDERLINE, "#,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOTO GetLine

 PrintTotal:
	GOTO GetNextRec IF ORDERTOTAL = 0.0

	ORDERTOTAL = FUNC_ROUND(ORDERTOTAL * &
		(1.0 - OE_REGHEADER::DISC / 100.0), 2%)

	TEXT$ = CONV_STRING(OE_REGHEADER::ORDNUM, CMC$_LEFT) + "     " + &
		"Order Discount%: " + "     " + &
		FORMAT$(OE_REGHEADER::DISC, "##.##%") + SPACE$(55%) + &
		"Order Total: " + "     " + &
		FORMAT$(ORDERTOTAL, "#,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

	REPORTTOTAL = REPORTTOTAL + ORDERTOTAL

	GOTO GetNextRec

 ExitTotal:
	TEXT$ = "               Report Total: " + SPACE$(87%) + &
		FORMAT$(REPORTTOTAL, "#,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)

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
