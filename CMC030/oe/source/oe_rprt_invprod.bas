1	%TITLE "Invoiced Product Report"
	%SBTTL "OE_RPRT_INVPROD"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1992 BY
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
	! ID:OE026
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Invoiced Product\* Report contains columns for the following
	!	information:
	!	.table 3,25
	!	.te
	!	Invoice Number
	!	.te
	!	Product Number
	!	.te
	!	Product Description
	!	.te
	!	Invoice Date
	!	.te
	!	Invoice Quantity
	!	.te
	!	Price
	!	.te
	!	Promo
	!	.te
	!	Discount _%
	!	.te
	!	Inventory Dollar Amount
	!	.te
	!	Total Discount Price
	!	.te
	!	Amount Invoiced
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Invoiced Product Report
	!	.x Invoiced Product Report>Report
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_RPRT_INVPROD/LINE
	!	$ LINK/EXE=OE_EXE: OE_RPRT_INVPROD, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE OE_RPRT_INVPROD.OBJ;*
	!
	! Author:
	!
	!	04/07/92 - Dan Perkins
	!		Modified from OE_RPRT_SHIPPROD.
	!
	! Modification History:
	!
	!	04/27/92 - Kevin Handy
	!		Clean up (check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/06/96 - Kevin Handy
	!		Reformat source code.
	!
	!	06/24/97 - Kevin Handy
	!		Use cost instead of price for inventory amount
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/11/2000 - Kevin Handy
	!		Use WHEN ERROR IN
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

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.HB"
	MAP (OE_REGLINE)	OE_REGLINE_CDD		OE_REGLINE

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.HB"
	DECLARE			OE_REGHEADER_CDD	OE_REGHEADER_READ

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	DECLARE			AR_35CUSTOM_CDD		AR_35CUSTOM_EXAM

	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.HB"
	MAP (AR_OPEN)		AR_OPEN_CDD		AR_OPEN

	%INCLUDE "SOURCE:[AR.OPEN]AR_CLOSED.HB"
	MAP (AR_CLOSED)		AR_CLOSED_CDD		AR_CLOSED

	!
	! Declare external functions
	!
	EXTERNAL LONG    FUNCTION OE_READ_REGHEADER
	EXTERNAL LONG    FUNCTION AR_EXAM_CUSTOM
	EXTERNAL LONG    FUNCTION PD_EXAM_PRODUCT

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
	!	^*(01) From Invoice\*
	!	.B
	!	.LM +5
	!	The ^*From Invoice\* field enters an invoice _# with
	!	which the report will begin printing.
	!	.b
	!	A blank field will cause the report to begin with the first
	!	invoice _# in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Invoice\*
	!	.lm +5
	!	.b
	!	The ^*To Invoice\* field enters an invoice _#
	!	with which the report will end printing.
	!	.b
	!	A blank field will cause the report to end with the last
	!	invoice _# in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Customer Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Customer Wildcard\* field selects
	!	designated customers to be printed by entering a "wildcard"
	!	using the Wildcarding Technique.
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
	!	.x From Date
	!	^*(06) From Date\*
	!	.b
	!	.lm +5
	!	The ^*From Date\* field enters the date with which
	!	the report will begin printing.
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
	!	.x To Date
	!	^*(07) To Date\*
	!	.b
	!	.lm +5
	!	The ^*To Date\* field enters the date with which the
	!	report will end printing.
	!	.b
	!	A blank field will cause the report to end with the most recent
	!	date in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	YYYYPP$ = EDIT$(UTL_REPORTX::OPTDEF(7%), 132%)

	!++
	! Abstract:FLD08
	!	.x Period
	!	^*(08) Period\*
	!	.b
	!	.lm +5
	!	The ^*Period\* field selects a specific
	!	accounting period for which the report is to be printed.
	!	The period entered must be in agreement with the dates entered in
	!	fields (06) and (07).
	!	.b
	!	A blank field will cause the report to include all accounting
	!	periods.
	!	.lm -5
	!
	! Index:
	!
	!--

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.OPN"
	USE
		FILENAME$ = "OE_REGLINE"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.OPN"
	USE
		CONTINUE 320 IF ERR = 5%
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

320	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CLOSED.OPN"
	USE
		CONTINUE ReportTitle IF ERR = 5%
		FILENAME$ = "AR_CLOSED"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	IF YYYYPP$ <> ""
	THEN
		ADD_TITLE$ = " FOR " + YYYYPP$
	ELSE
		ADD_TITLE$ = ""
	END IF

	TITLE$(1%) = "INVOICED PRODUCTS REPORT" + ADD_TITLE$
	TITLE$(2%) = "Order Entry System"
	TITLE$(3%) = "From " + PRNT_DATE(FROM_DATE$, 8%) + " To " + &
		PRNT_DATE(TO_DATE$, 8%)
	TITLE$(3%) = "Before " + PRNT_DATE(TO_DATE$, 8%) IF FROM_DATE$ = ""
	TITLE$(3%) = "After " + PRNT_DATE(FROM_DATE$, 8%) IF TO_DATE$ = ""
	TITLE$(3%) = "For All Dates" IF FROM_DATE$ + TO_DATE$ = ""
	TITLE$(4%) = ""

	!
	! Heading
	!
	TITLE$(5%) = "Invoice  Product        Description             " + &
		"InvDate  Doc#          InvQty        Price "      + &
		" Promo  Disc%      ExtPrice"

	TITLE$(6%) = "                   CusNum     Customer          " + &
		"              InventoryCost                "      + &
		"                   ExtTotal    InvoiceAmt"

	TITLE$(7%) = "."

	!
	! Set some date ranges if FROM_DATE$
	! and TO_DATE$ are blank
	!
	FROM_DATE$ = "01010001" IF FROM_DATE$ = ""
	TO_DATE$   = "31129999" IF TO_DATE$   = ""

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	!
	! Initialize variables
	!
	PRINT_FLAG%         = 0%
	TOT_EXTPRICE        = 0.0
	TOT_INVENTORYPRICE  = 0.0
	RPRT_EXTPRICE       = 0.0
	RPRT_INVENTORYPRICE = 0.0
	RPRT_ARSALES        = 0.0

	!
	! Find the first record which is not blank
	!
	FROM_ITEM$ = CHR$(33%) IF FROM_ITEM$ = ""

	WHEN ERROR IN
		FIND #OE_REGLINE.CH%, KEY #3% GE FROM_ITEM$, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "OE_REGLINE"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next Product record
	!
17020	WHEN ERROR IN
		GET #OE_REGLINE.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "OE_REGLINE"
		CONTINUE HelpError
	END WHEN

	GOTO ExitTotal IF (OE_REGLINE::REFNUM > TO_ITEM$) AND TO_ITEM$ <> ""

	!
	! Has anything been ordered ?
	!
	GOTO GetNextRec IF OE_REGLINE::TRANTYPE <> "02"

	GOTO GetNextRec IF OE_REGLINE::TDATE < FROM_DATE$
	GOTO GetNextRec IF OE_REGLINE::TDATE > TO_DATE$
	GOTO GetNextRec IF OE_REGLINE::PERIOD <> YYYYPP$ AND YYYYPP$ <> ""

	GOSUB PrintCustomer IF PRINT_FLAG% AND OE_REGLINE::REFNUM <> OLD_INV$

	!
	! Get Header Info
	!
	V% = OE_READ_REGHEADER(OE_REGLINE::ORDNUM, OE_REGHEADER_READ)

	GOTO GetNextRec IF COMP_STRING(EDIT$(OE_REGHEADER_READ::CUSNUM, -1%), &
		WLDCRD$) = 0% AND WLDCRD$ <> ""

	!
	! Get Product Info
	!
	V% = PD_EXAM_PRODUCT(OE_REGLINE::PRODUCT, PD_PRODUCT_EXAM)

	EXT_PRICE = FUNC_ROUND(OE_REGLINE::QTY * &
		(OE_REGLINE::PRICE - OE_REGLINE::PROMO) * &
		(1.0 - OE_REGLINE::DISCOUNT), 2%)

 !	INVENTORY_PRICE = FUNC_ROUND(OE_REGLINE::QTY * OE_REGLINE::PRICE, 2%)
	INVENTORY_PRICE = FUNC_ROUND(OE_REGLINE::QTY * OE_REGLINE::COST, 2%)

	TEXT$ = OE_REGLINE::REFNUM + " " + &
		OE_REGLINE::PRODUCT + " " + &
		LEFT$(PD_PRODUCT_EXAM::DESCRIPTION, 23%) + " " + &
		PRNT_DATE(OE_REGLINE::TDATE, 6%) + " " + &
		CONV_STRING(OE_REGLINE::ORDNUM, CMC$_LEFT) + " " + &
		FORMAT$(OE_REGLINE::QTY, "#,###,###") + " " + &
		FORMAT$(OE_REGLINE::PRICE, "#,###,###.##") + " " + &
		FORMAT$(OE_REGLINE::PROMO, "###.##") + " " + &
		FORMAT$(OE_REGLINE::DISCOUNT, "###.##") + " " + &
		FORMAT$(EXT_PRICE, "##,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	PRINT_FLAG% = -1%
	OLD_INV$    = OE_REGLINE::REFNUM

	TOT_EXTPRICE       = TOT_EXTPRICE + EXT_PRICE
	TOT_INVENTORYPRICE = TOT_INVENTORYPRICE + INVENTORY_PRICE

 !	EXT_PRICE       = 0.0
 !	INVENTORY_PRICE = 0.0

	GOTO GetNextRec

 ExitTotal:
	GOSUB PrintCustomer IF PRINT_FLAG%

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

	TEXT$ = SPACE$(24%) + &
		"G R A N D  T O T A L S" + &
		SPACE$(16%) + &
		FORMAT$(RPRT_INVENTORYPRICE, "##,###,###.##") + " " + &
		SPACE$(29%) + &
		FORMAT$(RPRT_EXTPRICE, "##,###,###.##") + " " + &
		FORMAT$(RPRT_ARSALES, "##,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

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

 PrintCustomer:
	AR_SALES  = 0.0

18000	WHEN ERROR IN
		FIND #AR_OPEN.CH%, &
			KEY #0% EQ OE_REGHEADER_READ::CUSNUM + OLD_INV$ + "01", &
			REGARDLESS
	USE
		CONTINUE 18100 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

18010	WHEN ERROR IN
		GET #AR_OPEN.CH%, REGARDLESS
	USE
		CONTINUE L18200 IF ERR = 11%
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

	GOTO L18200 IF AR_OPEN::CUSNUM <> OE_REGHEADER_READ::CUSNUM OR &
		AR_OPEN::INVNUM <> OLD_INV$ OR &
		AR_OPEN::TRATYP <> "01"

	AR_SALES = AR_SALES + AR_OPEN::SALAMT

	GOTO 18010

18100	WHEN ERROR IN
		FIND #AR_CLOSED.CH%, KEY #0% EQ OE_REGHEADER_READ::CUSNUM + &
			OLD_INV$ + "01", REGARDLESS
	USE
		CONTINUE L18200 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "AR_CLOSED"
		CONTINUE HelpError
	END WHEN

18110	WHEN ERROR IN
		GET #AR_CLOSED.CH%, REGARDLESS
	USE
		CONTINUE L18200 IF ERR = 11%
		FILENAME$ = "AR_CLOSED"
		CONTINUE HelpError
	END WHEN

	GOTO L18200 &
		IF AR_CLOSED::CUSNUM <> OE_REGHEADER_READ::CUSNUM OR &
			AR_CLOSED::INVNUM <> OLD_INV$ OR &
			AR_CLOSED::TRATYP <> "01"

	AR_SALES = AR_SALES + AR_CLOSED::SALAMT

	GOTO 18110

 L18200:
	MISC.STUFF = AR_SALES - TOT_EXTPRICE

	!
	! Get Customer Info
	!
	V% = AR_EXAM_CUSTOM(OE_REGHEADER_READ::CUSNUM, AR_35CUSTOM_EXAM)

	TEXT$ = OLD_INV$ + &
		SPACE$(11%) + &
		OE_REGHEADER_READ::CUSNUM + " " + &
		LEFT(AR_35CUSTOM_EXAM::CUSNAM, 25%) + &
		SPACE$(7%) + &
		FORMAT$(TOT_INVENTORYPRICE, "##,###,###.##") + " " + &
		SPACE$(29%) + &
		FORMAT$(TOT_EXTPRICE, "##,###,###.##") + " " + &
		FORMAT$(AR_SALES, "##,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

	RPRT_EXTPRICE       = RPRT_EXTPRICE + TOT_EXTPRICE
	RPRT_INVENTORYPRICE = RPRT_INVENTORYPRICE + TOT_INVENTORYPRICE
	RPRT_ARSALES        = RPRT_ARSALES + AR_SALES

	TOT_EXTPRICE       = 0.0
	TOT_INVENTORYPRICE = 0.0
	PRINT_FLAG%        = 0%

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
