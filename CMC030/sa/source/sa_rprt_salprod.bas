1	%TITLE "Sales Account Product "
	%SBTTL "SA_RPRT_SALPROD"
	%IDENT "V3.5"

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
	! ID:0017
	!
	! Abstract:HELP
	!	.p
	!	The ^*Sales Account Product\* Report contains
	!	the following information:
	!	.b
	!	.lm +10
	!	.list 0,"*"
	!	.le
	!	Product Number
	!	.le
	!	Product Description
	!	.le
	!	Product Type
	!	.le
	!	Product Category
	!	.le
	!	(Period-to-date and Year-to-date)
	!	.le
	!	Quantity Sold
	!	.le
	!	Cost of Sale
	!	.le
	!	Sales Total
	!	.le
	!	Percent of all product sales
	!	.le
	!	Gross Margin (dollars)
	!	.le
	!	Percent of all Gross Margin dollars
	!	.le
	!	Margin Percentage
	!	.els
	!	.lm -10
	!
	! Index:
	!	.x Report>Product Sales
	!	.x Product Sales>Report
	!
	! Compile:
	!
	!	$ BAS SA_SOURCE:SA_RPRT_SALPROD/LINE
	!	$ LINK/EXE=SA_EXE: SA_RPRT_SALPROD, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE SA_RPRT_SALPROD.OBJ;*
	!
	! Author:
	!
	!	01/18/91 - Val James Allen
	!
	! Modification History:
	!
	!	12/06/91 - Dan Perkins
	!		Added detail option, specifically for King B,
	!		to enable user to see details of each transaction.
	!		Also added the period to the report title.
	!
	!	02/04/92 - Kevin Handy
	!		Cleaned out junk (check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/13/96 - Kevin Handy
	!		Reformat source code
	!
	!	06/03/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/15/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.HB"
	MAP (IC_TRANSACTION)	IC_TRANSACTION_CDD	IC_TRANSACTION

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.HB"
	DECLARE			OE_REGHEADER_CDD	OE_REGHEADER_READ

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	DECLARE			AR_35CUSTOM_CDD		AR_35CUSTOM_EXAM

	!
	! Declare external functions
	!
	EXTERNAL LONG    FUNCTION IC_READ_HISTORY
	EXTERNAL LONG    FUNCTION OE_READ_REGHEADER
	EXTERNAL LONG    FUNCTION AR_EXAM_CUSTOM

	%PAGE

	TEMPCAT$ = "????"
	TEMPTYPE$ = "??"

	ON ERROR GOTO 19000

	!
 Init:	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	YYYYPP$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) Period\*
	!	.p
	!	The ^*Period\* field enters the
	!	period to print.
	!
	! Index:
	!	.x Period
	!
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(1%), -1%)

	!++
	! Abstract:FLD02
	!	^*(02) Sort by (P,C,T,D,S)\*
	!	.p
	!	The ^*Sort by\* field determines the order
	!	to print in.
	!	.p
	!	Valid settings are:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	P - Product Number
	!	.le
	!	C - Product Category
	!	.le
	!	T - Product Type
	!	.le
	!	D - Product Description
	!	.le
	!	S - Secondary Code
	!	.els
	!	.lm -10
	!	.p
	!	A setting is required in this field.  No other settings are
	!	valid.
	!
	! Index:
	!	.x Sort by
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03)From Item\*
	!	.p
	!	The ^*From Item\* field enters the
	!	item to begin the report with.
	!	.p
	!	A blank field causes the report to begin with the first
	!	item in the file.
	!
	! Index:
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)

	!++
	! Abstract:FLD04
	!	^*(04) To Item\*
	!	.p
	!	The ^*To Item\* field specifies the item
	!	to end with.
	!	.p
	!	A blank field causes the report to end with the last
	!	item in the file.
	!
	! Index:
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Wildcard\*
	!	.p
	!	The ^*Wildcard\* field selects
	!	designated programs to be printed by entering a "wildcard"
	!	for Wildcarding Technique.
	!
	! Index:
	!
	!--

	DETAIL$ = EDIT$(UTL_REPORTX::OPTDEF(5%), -1%)

	!++
	! Abstract:FLD06
	!	^*(06) Detail\*
	!	.p
	!	The ^*Detail\* field selects
	!	whether or not to print order detail under each
	!	product.
	!	.p
	!	Selecting Detail will display the following additional
	!	information:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	Document Number
	!	.le
	!	Customer Name
	!	.le
	!	Document Date
	!	.le
	!	Transaction Date
	!	.le
	!	Transaction Quantity
	!	.els
	!	.lm -10
	!
	! Index:
	!	.x Detail
	!
	!--

320	!
	! Open Product Description file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

330	!
	! Open Inventory Transaction file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.OPN"
	USE
		FILENAME$ = "IC_TRANSACTION"
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
		TITLE$(1%) = " SALES ANALYSIS BY PRODUCT TYPE"

	CASE "C"
		K_NUM% = 2%
		TITLE$(1%) = " SALES ANALYSIS BY PRODUCT CATEGORY"

	CASE "P"
		K_NUM% = 0%
		TITLE$(1%) = " SALES ANALYSIS BY PRODUCT NUMBER"

	CASE "D"
		K_NUM% = 3%
		TITLE$(1%) = " SALES ANALYSIS BY PRODUCT DESCRIPTION"

	CASE "S"
		K_NUM% = 4%
		TITLE$(1%) = " SALES ANALYSIS BY SECONDARY PRODUCT CODE"

	END SELECT

	TITLE$(2%) = " FOR PERIOD " + YYYYPP$
	TITLE$(3%) = "Sales Analysis System"
	TITLE$(4%) = ""

	!
	! Heading
	!
	IF DETAIL$ <> "Y"
	THEN
		TITLE$(5%) = "ProductNumber  Description              "                 + &
			"      Type Categ         QtySold  CostofSale   SaleTotal" + &
			" Sale%  GrossMargn Gross% Margin%"

		TITLE$(6%) = "."
	ELSE
		TITLE$(5%) = SPACE$(15%) + "DocNumber  Customer" + SPACE$(28%) + &
			"   TranQty TranDate DocDate  DocLine"

		TITLE$(6%) = "ProductNumber  Description              "                 + &
			"      Type Categ         QtySold  CostofSale   SaleTotal" + &
			" Sale%  GrossMargn Gross% Margin%"

		TITLE$(7%) = "."
	END IF

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************
	!
	! If from ITEM blank then reset file
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
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

	LOOP% = LOOP% + 1%

 GetNextRec:
17020	!
	! Main loop
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get record from Product Description file
	!
	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record if should be printed
	!
	SELECT SORTBY$
	CASE "C"
		GOTO ExitTotal IF (PD_PRODUCT::CATEGORY > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(PD_PRODUCT::CATEGORY, -1%), &
			WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "T"
		GOTO ExitTotal IF (PD_PRODUCT::PROD_TYPE> TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(PD_PRODUCT::PROD_TYPE, -1%), &
			WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "P"
		GOTO ExitTotal IF (PD_PRODUCT::PRODUCT_NUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(PD_PRODUCT::PRODUCT_NUM, -1%), &
			WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "D"
		GOTO ExitTotal IF (PD_PRODUCT::DESCRIPTION > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(PD_PRODUCT::DESCRIPTION, -1%), &
			WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "S"
		GOTO ExitTotal IF (PD_PRODUCT::SECONDARY_CODE > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(PD_PRODUCT::SECONDARY_CODE, -1%), &
			WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	END SELECT

	TRANQTY   = 0.0
	COSTTOTAL = 0.0
	SALETOTAL = 0.0
	QTYTOTAL  = 0.0

17050	WHEN ERROR IN
		FIND #IC_TRANSACTION.CH%, &
			KEY #0% EQ PD_PRODUCT::PRODUCT_NUM, &
			REGARDLESS
	USE
		CONTINUE GetNextRec IF ERR = 155%
		FILENAME$ = "IC_TRANSACTION"
		CONTINUE HelpError
	END WHEN

 GetTrans:
17060	WHEN ERROR IN
		GET #IC_TRANSACTION.CH%, REGARDLESS
	USE
		CONTINUE ReadHistory IF ERR = 11%
		FILENAME$ = "IC_TRANSACTION"
		CONTINUE HelpError
	END WHEN

	GOTO ReadHistory IF IC_TRANSACTION::PRODUCT <> PD_PRODUCT::PRODUCT_NUM

	IF IC_TRANSACTION::TYPE_A = "SA"
	THEN
		SIG% = SGN(IC_TRANSACTION::QUANTITY_A)
		COSTTOTAL = COSTTOTAL - &
			FUNC_ROUND(SIG% * IC_TRANSACTION::COST, 2%)
		SALETOTAL = SALETOTAL - &
			FUNC_ROUND(SIG% * IC_TRANSACTION::PRICE, 2%)
		QTYTOTAL = QTYTOTAL + (-IC_TRANSACTION::QUANTITY_A)
		IF DETAIL$ = "Y" AND LOOP% > 1%
		THEN
			TRANQTY = -IC_TRANSACTION::QUANTITY_A
			GOSUB PrintDetail
		END IF
	END IF

	IF IC_TRANSACTION::TYPE_B = "SA"
	THEN
		SIG% = SGN(IC_TRANSACTION::QUANTITY_B)
		COSTTOTAL = COSTTOTAL + &
			FUNC_ROUND(SIG% * IC_TRANSACTION::COST, 2%)
		SALETOTAL = SALETOTAL + &
			FUNC_ROUND(SIG% * IC_TRANSACTION::PRICE, 2%)
		QTYTOTAL = QTYTOTAL + (-IC_TRANSACTION::QUANTITY_B)
		IF DETAIL$ = "Y" AND LOOP% > 1%
		THEN
			TRANQTY = -IC_TRANSACTION::QUANTITY_B
			GOSUB PrintDetail
		END IF
	END IF

	GOTO GetTrans

 ReadHistory:
	!
	! Lookup sales history here by using Read_History routine
	!
	V% = IC_READ_HISTORY(PD_PRODUCT::PRODUCT_NUM, "", YYYYPP$, &
		TOTHQ, TOTHS, TOTHC)

	TOTHQ = -TOTHQ
	TOTHS = -TOTHS
	TOTHC = -TOTHC

	!
	! Check if loop 1 and if so just add up for overall totals
	!
	IF LOOP% = 1%
	THEN
		PCOST = PCOST + COSTTOTAL
		PSALE = PSALE + SALETOTAL
		YCOST = YCOST + (COSTTOTAL + TOTHC)
		YSALE = YSALE + (SALETOTAL + TOTHS)
		GOTO GetNextRec
	END IF

	IF (SALETOTAL = 0.0) AND (COSTTOTAL = 0.0) AND &
		(QTYTOTAL = 0.0) AND (TOTHQ = 0.0) AND &
		(TOTHS = 0.0) AND (TOTHC = 0.0)
	THEN
		GOTO GetNextRec
	END IF

	SELECT SORTBY$

	CASE "C"
		GOSUB SubTotals &
			IF (PD_PRODUCT::CATEGORY <> TEMPCAT$) AND &
			(FIRSTLINE = -1%)
		TEMPCAT$ = PD_PRODUCT::CATEGORY

	CASE "T"
		GOSUB SubTotals &
			IF (PD_PRODUCT::PROD_TYPE <> TEMPTYPE$) AND &
			(FIRSTLINE = -1%)
		TEMPTYPE$ = PD_PRODUCT::PROD_TYPE
	END SELECT

	GROSS = SALETOTAL - COSTTOTAL

	IF SALETOTAL = 0.0
	THEN
		MARGINS = 0.0
	ELSE
		MARGINS = FUNC_ROUND(100 * (GROSS / SALETOTAL), 2%)
	END IF

	!
	! Print out one line
	!
	FIRSTP = 0.0
	SECOUNDP = 0.0
	FIRSTP = FUNC_ROUND(100 * (SALETOTAL / PSALE), 2%) IF PSALE <> 0.0
	SECOUNDP = FUNC_ROUND(100 * (GROSS / PGROSS), 2%) IF PGROSS <> 0.0

	TEXT$ = PD_PRODUCT::PRODUCT_NUM + " " + &
		LEFT$(PD_PRODUCT::DESCRIPTION, 30%) + " " + &
		PD_PRODUCT::PROD_TYPE + "   " + &
		PD_PRODUCT::CATEGORY + "  " + &
		"PTD:" + " " + &
		FORMAT$(QTYTOTAL, "#######.##") + " " + &
		FORMAT$(COSTTOTAL, "########.##") + " " + &
		FORMAT$(SALETOTAL, "########.##") + " " + &
		FORMAT$(FIRSTP, "###.#") + " " + &
		FORMAT$(GROSS, "########.##") + "  " + &
		FORMAT$(SECOUNDP, "###.#") + "  " + &
		FORMAT$(MARGINS, "####.#")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GROSSY = (SALETOTAL + TOTHS) - (COSTTOTAL + TOTHC)

	!
	! Check for margins
	!
	IF SALETOTAL + TOTHS = 0.0
	THEN
		MARGINS1 = 0.0
	ELSE
		MARGINS1 = FUNC_ROUND(100 * (GROSSY / (SALETOTAL + TOTHS)), 2%)
	END IF

	!
	! Print out one line
	!
	FIRSTP = 0.0
	SECOUNDP = 0.0
	FIRSTP = FUNC_ROUND(100 * ((SALETOTAL + TOTHS) / YSALE), 2%) &
		IF YSALE <> 0.0
	SECOUNDP = FUNC_ROUND(100 * (GROSSY / YGROSS), 2%) IF YGROSS <> 0.0

	TEXT$ = PD_PRODUCT::PRODUCT_NUM + SPACE$(43%) + &
		"YTD:" + " " + &
		FORMAT$(QTYTOTAL + TOTHQ, "#######.##") + " " + &
		FORMAT$(COSTTOTAL + TOTHC, "########.##") + " " + &
		FORMAT$(SALETOTAL + TOTHS, "########.##") + " " + &
		FORMAT$(FIRSTP, "###.#") + " " + &
		FORMAT$(GROSSY, "########.##") + "  " + &
		FORMAT$(SECOUNDP, "###.#") + "  " + &
		FORMAT$(MARGINS1, "####.#")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FIRSTLINE = -1%

	SUBTOTAL1 = SUBTOTAL1 + COSTTOTAL
	SUBTOTAL2 = SUBTOTAL2 + SALETOTAL
	SUBTOTAL3 = SUBTOTAL3 + QTYTOTAL

	YTDSUB1 = YTDSUB1 + COSTTOTAL + TOTHC
	YTDSUB2 = YTDSUB2 + SALETOTAL + TOTHS
	YTDSUB3 = YTDSUB3 + QTYTOTAL  + TOTHQ

	SUBCUST% = SUBCUST% + 1%
	CUSTTOTAL% = CUSTTOTAL% + 1%

	!
	! Try for next record
	!
	GOTO GetNextRec

 SubTotals:
	!
	! Print out the subtotals
	!
	MARGIN1 = SUBTOTAL2 - SUBTOTAL1

	IF SUBTOTAL2 = 0.0
	THEN
		MARGIN2 = 0.0
	ELSE
		MARGIN2 = FUNC_ROUND(100 * (MARGIN1 / SUBTOTAL2), 2%)
	END IF

	FIRSTP = 0.0
	SECOUNDP = 0.0
	FIRSTP = FUNC_ROUND(100 * (SUBTOTAL2 / PSALE), 2%) IF PSALE <> 0.0
	SECOUNDP = FUNC_ROUND(100 * (MARGIN1 / PGROSS), 2%) IF PGROSS <> 0.0

	IF SORTBY$ = "T"
	THEN
		TEXT$ = "            SubTotals for " + &
			FORMAT$(SUBCUST%, "###") + " products  Type: " + &
			TEMPTYPE$ + &
			"         PTD:" + &
			FORMAT$(SUBTOTAL3, "########.##") + " "  + &
			FORMAT$(SUBTOTAL1, "########.##") + " "  + &
			FORMAT$(SUBTOTAL2, "########.##") + " "  + &
			FORMAT$(FIRSTP, "###.#") + " "  + &
			FORMAT$(MARGIN1, "########.##") + "  " + &
			FORMAT$(SECOUNDP, "###.#") + "  " + &
			FORMAT$(MARGIN2, "####.#")
	ELSE
		TEXT$ = "            SubTotals for " + &
			FORMAT$(SUBCUST%, "###") + " products   Category: " + &
			TEMPCAT$ + &
			"  PTD:" + &
			FORMAT$(SUBTOTAL3, "########.##") + " " + &
			FORMAT$(SUBTOTAL1, "########.##") + " " + &
			FORMAT$(SUBTOTAL2, "########.##") + " " + &
			FORMAT$(FIRSTP, "###.#") + " "  + &
			FORMAT$(MARGIN1, "########.##") + "  " + &
			FORMAT$(SECOUNDP, "###.#") + "  "  + &
			FORMAT$(MARGIN2, "####.#")
	END IF

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	MARGIN1 = YTDSUB2 - YTDSUB1

	IF YTDSUB2 = 0.0
	THEN
		MARGIN2 = 0.0
	ELSE
		MARGIN2 = FUNC_ROUND(100 * (MARGIN1 / YTDSUB2), 2%)
	END IF

	FIRSTP = 0.0
	SECOUNDP = 0.0
	FIRSTP = FUNC_ROUND(100 * (YTDSUB2 / YSALE), 2%) IF YSALE <> 0.0
	SECOUNDP = FUNC_ROUND(100 * (MARGIN1 / YGROSS), 2%) IF YGROSS <> 0.0

	IF SORTBY$ = "T"
	THEN
		TEXT$ = "            SubTotals for " + &
			FORMAT$(SUBCUST%, "###") + " products  Type: " + &
			TEMPTYPE$ + &
			"         YTD:" + &
			FORMAT$(YTDSUB3, "########.##") + " " + &
			FORMAT$(YTDSUB1, "########.##") + " " + &
			FORMAT$(YTDSUB2, "########.##") + " " + &
			FORMAT$(FIRSTP, "###.#") + " " + &
			FORMAT$(MARGIN1, "########.##") + "  " + &
			FORMAT$(SECOUNDP, "###.#") + "  " + &
			FORMAT$(MARGIN2, "####.#")
	ELSE
		TEXT$ = "            SubTotals for " + &
			FORMAT$(SUBCUST%, "###") + " products   Category: " + &
			TEMPCAT$ + &
			"  YTD:" + &
			FORMAT$(YTDSUB3, "########.##") + " " + &
			FORMAT$(YTDSUB1, "########.##") + " " + &
			FORMAT$(YTDSUB2, "########.##") + " " + &
			FORMAT$(FIRSTP, "###.#") + " " + &
			FORMAT$(MARGIN1, "########.##") + "  " + &
			FORMAT$(SECOUNDP, "###.#") + "  " + &
			FORMAT$(MARGIN2, "####.#")
	END IF

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Reset the variables
	!
	SUBCUST% = 0%
	SUBTOTAL1 = 0.0
	SUBTOTAL2 = 0.0
	SUBTOTAL3 = 0.0

	YTDSUB1 = 0.0
	YTDSUB2 = 0.0
	YTDSUB3 = 0.0

	RETURN

 PrintDetail:
	SELECT SORTBY$

	CASE "C"
		GOSUB SubTotals &
			IF (PD_PRODUCT::CATEGORY <> TEMPCAT$) AND &
			(FIRSTLINE = -1%)
		TEMPCAT$ = PD_PRODUCT::CATEGORY

	CASE "T"
		GOSUB SubTotals &
			IF (PD_PRODUCT::PROD_TYPE <> TEMPTYPE$) AND &
			(FIRSTLINE = -1%)
		TEMPTYPE$ = PD_PRODUCT::PROD_TYPE
	END SELECT

	ORDNUM$ = LEFT$(IC_TRANSACTION::PRIMARY_REF, 10%)
	ORDLIN$ = MID$(IC_TRANSACTION::PRIMARY_REF, 11%, 4%)

	V% = OE_READ_REGHEADER(ORDNUM$, OE_REGHEADER_READ)
	V% = AR_EXAM_CUSTOM(OE_REGHEADER_READ::CUSNUM, AR_35CUSTOM_EXAM)

	TEXT$ = PD_PRODUCT::PRODUCT_NUM + " " + &
		ORDNUM$ + " " + &
		LEFT$(AR_35CUSTOM_EXAM::CUSNAM, 35%) + " " + &
		FORMAT$(TRANQTY, "#######.##") + " " + &
		PRNT_DATE(IC_TRANSACTION::TRANS_DATE, 6%) + " " + &
		PRNT_DATE(OE_REGHEADER_READ::ORDDATE, 6%) + " " + &
		ORDLIN$

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT
	TRANQTY = 0.0

	RETURN

 ExitTotal:
	!
	! Handle end of the report
	!
	IF LOOP% = 1%
	THEN
		PGROSS = PSALE - PCOST
		YGROSS = YSALE - YCOST
		GOTO 17000
	END IF

	IF FIRSTLINE = -1%
	THEN
		GOSUB SubTotals IF (SORTBY$ = "C") OR (SORTBY$ = "T")
		!
		! Print out one line
		!
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

		IF PSALE = 0.0
		THEN
			TOTMARGIN2 = 0.0
		ELSE
			TOTMARGIN2 = FUNC_ROUND(100 * (PGROSS / PSALE), 2%)
		END IF

		TEXT$ = "            Totals for " + &
			FORMAT$(CUSTTOTAL%, "###") + &
			" products                      PTD:            " + &
			FORMAT$(PCOST, "########.##")  + " "   + &
			FORMAT$(PSALE, "########.##")  + " "   + &
			FORMAT$(100.0, "###.#")        + " "   + &
			FORMAT$(PGROSS, "########.##") + "  "  + &
			FORMAT$(100.0, "###.#")        + "  "  + &
			FORMAT$(TOTMARGIN2, "####.#")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		!
		! Print out one line
		!
		IF YSALE = 0.0
		THEN
			YTDMARGIN2 = 0.0
		ELSE
			YTDMARGIN2 = FUNC_ROUND(100 * (YGROSS / YSALE), 2%)
		END IF

		TEXT$ = "            Totals for " + &
			FORMAT$(CUSTTOTAL%, "###") + &
			" products                      YTD:            " + &
			FORMAT$(YCOST, "########.##")  + " "   + &
			FORMAT$(YSALE, "########.##")  + " "   + &
			FORMAT$(100.0, "###.#")        + " "   + &
			FORMAT$(YGROSS, "########.##") + "  "  + &
			FORMAT$(100.0, "###.#")        + "  "  + &
			FORMAT$(YTDMARGIN2, "####.#")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
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

 HelpError:
	!***************************************************************
	! Help Message for an error
	!***************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	UTL_REPORTX::STAT = -1%
	!
	! Exit from the program after showing error message
	!
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
