1	%TITLE "Order Journal Report"
	%SBTTL "MO_RPRT_ORDERJOUR"
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
	! ID:MO0005
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Order Journal Entry\* Report contains
	!	the following information:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	Order Number
	!	.le
	!	Order Type
	!	.le
	!	Order Date
	!	.le
	!	Customer Number
	!	.le
	!	Customer Name
	!	.le
	!	Customer PO Number
	!	.le
	!	Discount
	!	.le
	!	Make
	!	.le
	!	Make Year
	!	.le
	!	Make Type
	!	.le
	!	Make Size
	!	.le
	!	Model Code
	!	.le
	!	Quantity Ordered
	!	.le
	!	Extended Price
	!	.le
	!	Cost
	!	.le
	!	Product
	!	.els
	!	.lm -10
	!	Option Group
	!	.lm +10
	!	.list 0,"*"
	!	.le
	!	Option
	!	.le
	!	Description
	!	.le
	!	Quantity Ordered
	!	.le
	!	Price
	!	.le
	!	Cost
	!	.le
	!	Product
	!	.els
	!	.lm -10
	!
	! Index:
	!	.x Report>Order Journal
	!	.x Order Journal>Report
	!
	! Compile:
	!
	!	$ BAS MO_SOURCE:MO_RPRT_ORDERJOUR/LINE
	!	$ LINK/EXE=MO_EXE: MO_RPRT_ORDERJOUR, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE MO_RPRT_ORDERJOUR.OBJ;*
	!
	! AUTHOR:
	!
	!	03/14/91 - Craig Tanner
	!
	! MODIFICATION HISTORY:
	!
	!	09/12/91 - Deborah K. Fries
	!		Enlarged extend price field and fix location on
	!		screen
	!
	!	10/04/91 - Deborah K. Fries
	!		Used function to read file
	!		Cleaned source code
	!		Improved error trapping
	!
	!	06/16/92 - Dan Perkins
	!		Added OE_ORDERLINE to print out.
	!		Cleaned source code.
	!
	!	07/01/92 - Kevin Handy
	!		Clean up (check)
	!
	!	08/20/92 - Kevin Handy
	!		Clean up (check)
	!
	!	10/13/92 - Dan Perkins
	!		Added COST, GROSS MARGIN, and PERCENT MARGIN
	!		fields to report.
	!
	!	10/26/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	11/09/92 - Dan Perkins
	!		Added unit costs to report.
	!
	!	06/17/94 - Kevin Handy
	!		Added in MISCH2 rate.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/02/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/12/2000 - Kevin Handy
	!		Use WHEN ERROR IN
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

	%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERJOUR.HB"
	MAP (OE_ORDERJOUR)	OE_ORDERJOUR_CDD	OE_ORDERJOUR

	%INCLUDE "SOURCE:[MO.OPEN]MO_ORDERLINE.HB"
	MAP (MO_ORDERLINE)	MO_ORDERLINE_CDD	MO_ORDERLINE

	%INCLUDE "SOURCE:[MO.OPEN]MO_ORDERLINEOPT.HB"
	MAP (MO_ORDERLINEOPT)	MO_ORDERLINEOPT_CDD	MO_ORDERLINEOPT

	%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERLINE.HB"
	MAP (OE_ORDERLINE)	OE_ORDERLINE_CDD	OE_ORDERLINE

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	DECLARE			AR_35CUSTOM_CDD		AR_35CUSTOM_EXAM

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	!
	! Declare external functions
	!
	EXTERNAL LONG    FUNCTION AR_EXAM_CUSTOM
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
	!	.x Batch Number>Print Journal
	!	^*(01) Batch Number\*
	!	.b
	!	.lm +5
	!	The ^*Batch Number\* field enters a
	!	particular batch to be printed.
	!	.b
	!	Only one batch at a time may be printed.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
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
	!	The ^*Sort by\* field determines the order
	!	in which the report will print.
	!	.b
	!	Valid settings are:
	!	.table 3,25
	!	.te
	!	^*C\* - Customer Number
	!	.te
	!	^*D\* - Document Number
	!	.Te
	!	^*T\* - Sale Type
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
	!	The ^*From Item\* field enters the
	!	item with which the report will begin printing.
	!	The value entered must be in agreement with
	!	field (02), Sort by.
	!	.b
	!	A blank field will cause the report to begin with the first
	!	item in the file.
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
	!	The ^*To Item\* field allows printing to
	!	end with a specified item.  The value entered must be in
	!	agreement with field (02), Sort by.
	!	.b
	!	A blank field will cause the report to end with the last
	!	item in the file.
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
	!	The ^*Wildcard\* field selects
	!	designated items to be printed by entering a "wildcard"
	!	for Wildcarding
	!	Technique.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard
	!
	!--

	REG_NO$ = EDIT$(UTL_REPORTX::OPTDEF(5%), -1%)

	!++
	! Abstract:FLD06
	!	^* (06) Register Number\*
	!	.b
	!	.lm +5
	!	The ^*Register Number\* field enters a
	!	particular batch to be posted.
	!	.b
	!	Only one batch at a time may be posted.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Batch Number>Order Journal
	!
	! Required:
	!--

	BATCH_NO$ = REG_NO$ + "_" + BATCH_NO$

300	!
	! Open Order Journal file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERJOUR.OPN"
	USE
		FILENAME$ = "OE_ORDERJOUR"
		CONTINUE HelpError
	END WHEN

310	!
	! Open Order Line file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[MO.OPEN]MO_ORDERLINE.OPN"
	USE
		CONTINUE 320 IF ERR = 5%
		FILENAME$ = "MO_ORDERLINE"
		CONTINUE HelpError
	END WHEN

320	!
	! Open Order Line Option file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[MO.OPEN]MO_ORDERLINEOPT.OPN"
	USE
		CONTINUE 330 IF ERR = 5%
		FILENAME$ = "MO_ORDERLINEOPT"
		CONTINUE HelpError
	END WHEN

330	!
	! Open OE Order Line file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERLINE.OPN"
	USE
		CONTINUE ReportTitle IF ERR = 5%
		FILENAME$ = "OE_ORDERLINE"
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
		TITLE$(1%) = "MANUFACTURING  ORDER JOURNAL BY SALE TYPE"

	CASE "C"
		K_NUM% = 2%
		TITLE$(1%) = "MANUFACTURING ORDER JOURNAL BY CUSTOMER NUMBER"

	CASE "D"
		K_NUM% = 0%
		TITLE$(1%) = "MANUFACTURING ORDER JOURNAL BY DOCUMENT NUMBER"
		FROM_ITEM$ = SPACE$(LEN(OE_ORDERJOUR::ORDNUM) - &
			LEN(FROM_ITEM$)) + FROM_ITEM$
		TO_ITEM$ = SPACE$(LEN(OE_ORDERJOUR::ORDNUM) - &
			LEN(TO_ITEM$)) + TO_ITEM$

	END SELECT

	TITLE$(2%) = "BATCH No. " + BATCH_NO$
	TITLE$(3%) = " Manufacturing Order System"
	TITLE$(4%) = ""

	!
	! Heading
	!
	TITLE$(5%) = "Doc #       SaleType     OrdDate     CusNumber  " + &
		"CusName                                     CustPo#" + &
		"      Disc"

	TITLE$(6%) = "              ReqDate     Make       Year Mtype " + &
		"Msize MdlCode Product            QtyOrd      UnitCost " + &
		"    UnitPrice       TotPrice"

	TITLE$(7%) = "                OptGroup  Option  Description "

	TITLE$(8%) = "."

	%PAGE

	PRINT_LINE% = 0%

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
			RESET #OE_ORDERJOUR.CH%, KEY #K_NUM%
		ELSE
			FIND #OE_ORDERJOUR.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE ExitTotal IF ERR = 155%
		FILENAME$ = "OE_ORDERJOUR"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	GOTO ExitTotal IF UTL_REPORTX::STAT

	WHEN ERROR IN
		GET #OE_ORDERJOUR.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "OE_ORDERJOUR"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record if should be printed
	!
	SELECT SORTBY$

	CASE "C"
		GOTO ExitTotal IF (OE_ORDERJOUR::CUSNUM > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(OE_ORDERJOUR::CUSNUM, -1%), &
			WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

		GOSUB SortTotal IF (OE_ORDERJOUR::CUSNUM <> &
			LAST_CUSNUM$) AND PRINT_LINE%

		LAST_CUSNUM$ = OE_ORDERJOUR::CUSNUM

		ST$ = "Cust Tot  "

	CASE "D"
		GOTO ExitTotal IF (OE_ORDERJOUR::ORDNUM > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(OE_ORDERJOUR::ORDNUM, -1%), &
			WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "T"
		GOTO ExitTotal IF (OE_ORDERJOUR::ORDTYPE > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(OE_ORDERJOUR::ORDTYPE, -1%), &
			WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

		GOSUB SortTotal IF (OE_ORDERJOUR::ORDTYPE <> &
			LAST_ORDTYPE$) AND PRINT_LINE%

		LAST_ORDTYPE$ = OE_ORDERJOUR::ORDTYPE

		ST$ = "SalTyp Tot"

	END SELECT

	!
	! Check current Custom record
	!
	V% = AR_EXAM_CUSTOM(OE_ORDERJOUR::CUSNUM, AR_35CUSTOM_EXAM)

	!
	! Print out one line
	!
	TEXT$ = CONV_STRING(OE_ORDERJOUR::ORDNUM, CMC$_LEFT) + "  " + &
		OE_ORDERJOUR::ORDTYPE + "           " + &
		PRNT_DATE(OE_ORDERJOUR::ORDDATE, 8%) + "  " + &
		OE_ORDERJOUR::CUSNUM + " " + &
		LEFT$(AR_35CUSTOM_EXAM::CUSNAM, 40%) + "    " + &
		OE_ORDERJOUR::CUSTPO + " " + &
		FORMAT$(OE_ORDERJOUR::DISC, "##.##") + "%"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitTotal IF UTL_REPORTX::STAT

	PRINT_LINE% = -1%

	!
	! Check current Order Line record
	!
17100	WHEN ERROR IN
		FIND #MO_ORDERLINE.CH%, &
			KEY #0% GE OE_ORDERJOUR::ORDNUM, &
			REGARDLESS
	USE
		CONTINUE 17300 IF ERR = 155% OR ERR = 9% OR ERR = 11%
		FILENAME$ = "MO_ORDERLINE"
		CONTINUE HelpError
	END WHEN

 OrderLine:
	WHEN ERROR IN
		GET #MO_ORDERLINE.CH%, REGARDLESS
	USE
		CONTINUE 17300 IF ERR = 155% OR ERR = 9% OR ERR = 11%
		FILENAME$ = "MO_ORDERLINE"
		CONTINUE HelpError
	END WHEN

	GOTO 17300 IF MO_ORDERLINE::ORDNUM <> OE_ORDERJOUR::ORDNUM

	!
	! Find extended price/cost and add to ordertotal/sorttotal/grand total
	!
	EXT_PRICE = FUNC_ROUND(MO_ORDERLINE::ORDQTY * &
		MO_ORDERLINE::PRICE, 2%)

	EXT_PRICE_OT = EXT_PRICE_OT + EXT_PRICE

	EXT_COST = FUNC_ROUND(MO_ORDERLINE::ORDQTY * &
		MO_ORDERLINE::COST, 2%)

	EXT_COST_OT = EXT_COST_OT + EXT_COST

	!
	! Print out one line
	!
	TEXT$ = CONV_STRING(OE_ORDERJOUR::ORDNUM, CMC$_LEFT) + "    " + &
		PRNT_DATE(MO_ORDERLINE::REQDATE, 8%) + "  " + &
		MO_ORDERLINE::MAKE + " " + &
		MO_ORDERLINE::YEAR + " " + &
		MO_ORDERLINE::MTYPE + "    " + &
		MO_ORDERLINE::MSIZE + "  " + &
		MO_ORDERLINE::MODELCODE + "    " + &
		MO_ORDERLINE::PRODUCT + "  "   + &
		FORMAT$(MO_ORDERLINE::ORDQTY, "#,###,###") + "  " + &
		FORMAT$(MO_ORDERLINE::COST, "#,###,###.##") + "  " + &
		FORMAT$(MO_ORDERLINE::PRICE, "#,###,###.##") + "  " + &
		FORMAT$(EXT_PRICE, "##,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitTotal IF UTL_REPORTX::STAT

	!
	! Check current Order Line record
	!
	OPTION_FLAG% = 0%

17200	WHEN ERROR IN
		FIND #MO_ORDERLINEOPT.CH%, KEY #0% EQ MO_ORDERLINE::ORDNUM + &
			MO_ORDERLINE::LIN + MO_ORDERLINE::MAKE + &
			MO_ORDERLINE::MODELCODE, REGARDLESS
	USE
		CONTINUE EndOrderLine IF ERR = 155% OR ERR = 9% OR ERR = 11%
		FILENAME$ = "MO_ORDERLINEOPT"
		CONTINUE HelpError
	END WHEN

	OPTION_FLAG% = -1%

 OrderLineOpt:
	WHEN ERROR IN
		GET #MO_ORDERLINEOPT.CH%, REGARDLESS
	USE
		CONTINUE EndOrderLine IF ERR = 155% OR ERR = 9% OR ERR = 11%
		FILENAME$ = "MO_ORDERLINEOPT"
		CONTINUE HelpError
	END WHEN

	GOTO EndOrderLine IF MO_ORDERLINEOPT::ORDNUM + &
		MO_ORDERLINEOPT::LIN + &
		MO_ORDERLINEOPT::MAKE + &
		MO_ORDERLINEOPT::MODELCODE <> &
		MO_ORDERLINE::ORDNUM + &
		MO_ORDERLINE::LIN + &
		MO_ORDERLINE::MAKE + &
		MO_ORDERLINE::MODELCODE

	!
	! Find extended price/cost and add to subtotal/grand total
	!
	EXT_PRICE = FUNC_ROUND(MO_ORDERLINEOPT::ORDQTY * &
		MO_ORDERLINEOPT::PRICE, 2%)

	EXT_PRICE_OT = EXT_PRICE_OT + EXT_PRICE
	MAKE_TOTAL   = MAKE_TOTAL + EXT_PRICE

	EXT_COST = FUNC_ROUND(MO_ORDERLINEOPT::ORDQTY * &
		MO_ORDERLINEOPT::COST, 2%)

	EXT_COST_OT = EXT_COST_OT + EXT_COST

	!
	! Print out one line
	!
	TEXT$ = CONV_STRING(OE_ORDERJOUR::ORDNUM, CMC$_LEFT) + SPACE$(6%) + &
		MO_ORDERLINEOPT::OPTGROUP + SPACE$(8%) + &
		MO_ORDERLINEOPT::OPTN + "    "     + &
		LEFT(MO_ORDERLINEOPT::OPTDESCR, 26%) + "  " + &
		MO_ORDERLINEOPT::PRODUCT + "  "       + &
		FORMAT$(MO_ORDERLINEOPT::ORDQTY, "#,###,###") + "  " + &
		FORMAT$(MO_ORDERLINEOPT::COST, "#,###,###.##") + "  " + &
		FORMAT$(MO_ORDERLINEOPT::PRICE, "#,###,###.##") + "  " + &
		FORMAT$(EXT_PRICE, "##,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitTotal IF UTL_REPORTX::STAT

	GOTO OrderLineOpt

	!
	! Check current OE Order Line record
	!
17300	TITLE_FLAG%  = -1%
	OPTION_FLAG% = 0%

	WHEN ERROR IN
		FIND #OE_ORDERLINE.CH%, &
			KEY #0% GE OE_ORDERJOUR::ORDNUM, &
			REGARDLESS
	USE
		CONTINUE EndOEOrderLine IF ERR = 155% OR ERR = 9% OR ERR = 11%
		FILENAME$ = "OE_ORDERLINE"
		CONTINUE HelpError
	END WHEN

 OEOrderLine:
	WHEN ERROR IN
		GET #OE_ORDERLINE.CH%, REGARDLESS
	USE
		CONTINUE EndOEOrderLine IF ERR = 155% OR ERR = 9% OR ERR = 11%
		FILENAME$ = "OE_ORDERLINE"
		CONTINUE HelpError
	END WHEN

	GOTO EndOEOrderLine IF OE_ORDERLINE::ORDNUM <> OE_ORDERJOUR::ORDNUM

	OPTION_FLAG% = OPTION_FLAG% + 1%

	IF TITLE_FLAG%
	THEN
		TEXT$ = CONV_STRING(OE_ORDERJOUR::ORDNUM, CMC$_LEFT) + &
			SPACE$(42%) + "***** INVENTORY ITEMS *****"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		TEXT$ = CONV_STRING(OE_ORDERJOUR::ORDNUM, CMC$_LEFT) + &
			"    ReqDate     Product        Description  " + &
			"           QtyOrd  UnitPrice   " + &
			"PromoOff   Disc%    MisChrg       LineTotal"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		TITLE_FLAG% = 0%
	END IF

	!
	! Find extended price/cost and add to ordertotal/sorttotal/grand total
	!
	EXT_PRICE = FUNC_ROUND(OE_ORDERLINE::ORDQTY * &
		((OE_ORDERLINE::PRICE - OE_ORDERLINE::PROMO) * &
		(1 - OE_ORDERLINE::DISCOUNT / 100.0) + &
		OE_ORDERLINE::MISCH + OE_ORDERLINE::MISCH2), 2%)

	EXT_PRICE_OT = EXT_PRICE_OT + EXT_PRICE
	INV_TOTAL    = INV_TOTAL + EXT_PRICE

	EXT_COST = FUNC_ROUND(OE_ORDERLINE::ORDQTY * OE_ORDERLINE::COST, 2%)

	EXT_COST_OT = EXT_COST_OT + EXT_COST

	!
	! Get the product description
	!
	V% = PD_EXAM_PRODUCT(OE_ORDERLINE::PRODUCT, PD_PRODUCT_EXAM)

	!
	! Print out one line
	!
	TEXT$ = CONV_STRING(OE_ORDERJOUR::ORDNUM, CMC$_LEFT) + "    " + &
		PRNT_DATE(OE_ORDERLINE::REQDATE, 8%) + "  " + &
		OE_ORDERLINE::PRODUCT + " " + &
		LEFT$(PD_PRODUCT_EXAM::DESCRIPTION, 20%) + " " + &
		FORMAT$(OE_ORDERLINE::ORDQTY, "#,###,###") + " " + &
		FORMAT$(OE_ORDERLINE::PRICE, "###,###.##") + " " + &
		FORMAT$(OE_ORDERLINE::PROMO, "###,###.##") + "  " + &
		FORMAT$(OE_ORDERLINE::DISCOUNT, "##.##") + "% "   + &
		FORMAT$(OE_ORDERLINE::MISCH + &
			OE_ORDERLINE::MISCH2, "###,###.##") + "    " + &
		FORMAT$(EXT_PRICE, "#,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitTotal IF UTL_REPORTX::STAT

	GOTO OEOrderLine

 EndOrderLine:
	GROSS_MARGIN = EXT_PRICE_OT - EXT_COST_OT

	IF EXT_PRICE_OT = 0.0
	THEN
		GT_PERCENT = 0.0
	ELSE
		GT_PERCENT = FUNC_ROUND((GROSS_MARGIN / EXT_PRICE_OT) * &
			100.0, 2% )
	END IF

	TEXT$ = CONV_STRING(OE_ORDERJOUR::ORDNUM, CMC$_LEFT) + &
		"............................" + &
		"  Price: " + &
		FORMAT$(EXT_PRICE_OT, "##,###,###.##") + &
		"   Cost: " + &
		FORMAT$(EXT_COST_OT, "##,###,###.##") + &
		"  Margin: " + &
		FORMAT$(GROSS_MARGIN, "###,###.##") + &
		"  Percent: " + &
		FORMAT$(GT_PERCENT, "###.##%")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO OrderLine

 EndOEOrderLine:
	!
	! Print out totals line for end of inventory
	!
	IF OPTION_FLAG% > 1%
	THEN
		TEXT$ = CONV_STRING(OE_ORDERJOUR::ORDNUM, CMC$_LEFT) + &
			SPACE$(86%) + &
			"Inventory Total: " + &
			SPACE$(2%) + &
			FORMAT$(INV_TOTAL, "##,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	INV_TOTAL = 0.0

 OrdTotal:
	!
	! Assign Variables for later totals
	!
	ORDDISC_OT = FUNC_ROUND((OE_ORDERJOUR::DISC / 100.0) * &
		EXT_PRICE_OT, 2%)

	TAX_OT = FUNC_ROUND((OE_ORDERJOUR::SALESTAX / 100) * &
		(EXT_PRICE_OT - ORDDISC_OT), 2%)

	TAX_OT = 0.0 IF TAX_OT < 0.0

	TOTAL = OE_ORDERJOUR::FREIGHT - ORDDISC_OT + &
		OE_ORDERJOUR::MISC + TAX_OT + EXT_PRICE_OT

	!
	! Print out order totals
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
		CONV_STRING(OE_ORDERJOUR::ORDNUM, CMC$_LEFT), -1%)

	TEXT$ = CONV_STRING(OE_ORDERJOUR::ORDNUM, CMC$_LEFT) + &
		"  Freight: " + &
		FORMAT$(OE_ORDERJOUR::FREIGHT, "###,###.##") + &
		"  Disc: "    + &
		FORMAT$(ORDDISC_OT, "###,###.##") + &
		"  MisChrg: " + &
		FORMAT$(OE_ORDERJOUR::MISC, "###,###.##") + &
		"  Tax: "     + &
		FORMAT$(TAX_OT, "###,###.##") + &
		"  SubTot: "  + &
		FORMAT$(EXT_PRICE_OT, "##,###,###.##") + &
		"     "       + &
		FORMAT$(TOTAL, "##,###,###.##*")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	!
	! Set values to zero
	!
	EXT_PRICE_ST = EXT_PRICE_ST + EXT_PRICE_OT
	FREIGHT_ST   = FREIGHT_ST + OE_ORDERJOUR::FREIGHT
	ORDDISC_ST   = ORDDISC_ST + ORDDISC_OT
	MISC_ST      = MISC_ST + OE_ORDERJOUR::MISC
	TAX_ST       = TAX_ST + TAX_OT

	EXT_PRICE_GT = EXT_PRICE_GT + EXT_PRICE_OT
	FREIGHT_GT = FREIGHT_GT + OE_ORDERJOUR::FREIGHT
	ORDDISC_GT = ORDDISC_GT + ORDDISC_OT
	MISC_GT = MISC_GT + OE_ORDERJOUR::MISC
	TAX_GT = TAX_GT + TAX_OT

	EXT_PRICE_OT = 0.0
	EXT_COST_OT = 0.0
	ORDDISC_OT = 0.0
	TAX_OT = 0.0

	GOTO GetNextRec

 ExitTotal:
	!
	! Will not print out total or grand total line unless at least one item
	! has been printed
	!
	IF PRINT_LINE%
	THEN
		IF SORTBY$ = "T" OR SORTBY$ = "C"
		THEN
			GOSUB SortTotal
		END IF

		!
		! Handle end of report
		!
		TOTAL_GT = FREIGHT_GT - ORDDISC_GT + MISC_GT + TAX_GT + &
			EXT_PRICE_GT

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

		TEXT$ = "Grand Tot "  + &
			"  Freight: " + &
				FORMAT$(FREIGHT_GT, "###,###.##") + &
			"  Disc: "    + &
				FORMAT$(ORDDISC_GT, "###,###.##") + &
			"  MisChrg: " + &
				FORMAT$(MISC_GT, "###,###.##") + &
			"  Tax: "     + &
				FORMAT$(TAX_GT, "###,###.##") + &
			"  SubTot: "  + &
				FORMAT$(EXT_PRICE_GT, "##,###,###.##") + &
			"     "       + &
				FORMAT$(TOTAL_GT, "##,###,###.##*")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	END IF

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

 SortTotal:
	!
	! Print out subtotal line if sorting by order type or customer
	! number
	!
	TOTAL = FREIGHT_ST - ORDDISC_ST + MISC_ST + TAX_ST + EXT_PRICE_ST

	TEXT$ = ST$  + &
		"  Freight: " + &
			FORMAT$(FREIGHT_ST, "###,###.##") + &
		"  Disc: "    + &
			FORMAT$(ORDDISC_ST, "###,###.##") + &
		"  MisChrg: " + &
			FORMAT$(MISC_ST, "###,###.##") + &
		"  Tax: "     + &
			FORMAT$(TAX_ST, "###,###.##") + &
		"  SubTot: "  + &
			FORMAT$(EXT_PRICE_ST, "##,###,###.##") + &
		"     "       + &
			FORMAT$(TOTAL, "##,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

	!
	! Set values to zero
	!
	EXT_PRICE_ST = 0.0
	FREIGHT_ST   = 0.0
	ORDDISC_ST   = 0.0
	MISC_ST      = 0.0
	TAX_ST       = 0.0

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
