1	%TITLE "Product Sales By Sales Volume"
	%SBTTL "SA_RPRT_SALPRODVOLUME"
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
	! ID:SA0019
	!
	! Abstract:HELP
	!	.p
	!	The ^*Product Sales by Sales Volume\* Report ranks
	!	the product sales by dollar sales volume for YTD
	!	sales with an option to list a selected number of products
	!	(ie: the top 10, the top 100, etc.) and/or with a dollar
	!	sales value limit (ie: only those products with 500.00
	!	or more in sales).
	!	The report contains the following information:
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
	!	(Period-to-date and Year-to_date)
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
	!	.x Report>Product Sales by Sales Volume
	!	.x Product Sales by Sales Volume>Report
	!
	! Compile:
	!
	!	$ BAS SA_SOURCE:SA_RPRT_SALPRODVOLUME/LINE
	!	$ LINK/EXE=SA_EXE: SA_RPRT_SALPRODVOLUME, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE SA_RPRT_SALPRODVOLUME.OBJ;*
	!
	! Author:
	!
	!	01/18/91 - Val James Allen
	!
	! Modification History:
	!
	!	07/18/91 - Craig Tanner
	!		Added Sortby and related feilds to allow multiple
	!		selection of products.
	!
	!	04/28/92 - Kevin Handy
	!		Clean up (check)
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
	!		Reformat source code.
	!
	!	06/03/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/26/87 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
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

	MAP (SA_DIMVALUE) &
		DECIMAL(11, 3)	SA_DIMVALUE_VALUE, &
		GFLOAT		SA_DIMVALUE_PSALE, &
		GFLOAT		SA_DIMVALUE_PCOST, &
		GFLOAT		SA_DIMVALUE_YSALE, &
		GFLOAT		SA_DIMVALUE_YCOST, &
		GFLOAT		SA_DIMVALUE_PQTY, &
		GFLOAT		SA_DIMVALUE_YQTY, &
		RFA		SA_DIMVALUE_PRODRFA

	!
	! Declare external functions
	!
	EXTERNAL LONG	FUNCTION IC_READ_HISTORY

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
	!	.p
	!	Although the report will print in order of sales volume regardless of the value
	!	of the ^*Sort by\* field, this field allows for the selection of customers to be
	!	printed by using their Number, Type, Category, or Alphabetical codes.
	!
	! Index:
	!	.x Sort by
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.p
	!	The ^*From Item\* field determines the item with which the
	!	report will begin printing.  The value must be in agreement with the value
	!	entered in field (01).
	!	.p
	!	When the setting is blank, the report will begin with the first customer
	!	in the file.
	!
	! Index:
	!	.x From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.p
	!	The ^*To Item\* field determines the customer with which the
	!	report will end.  The value must be in agreement with
	!	field (01).
	!	.p
	!	A blank setting will cause the report to end with the last customer in the
	!	file.
	!
	! Index:
	!	.x To Item
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.p
	!	The ^*Wildcard\* field selects
	!	designated programs to be printed by entering a "wildcard"
	!	through using the Wildcarding Technique.
	!
	! Index:
	!
	!--

	YYYYPP$ = EDIT$(UTL_REPORTX::OPTDEF(5%), 132%)

	!++
	! Abstract:FLD06
	!	^*(06) Period for Report\*
	!	.p
	!	The ^*Period for Report\* field enters the
	!	period to print on the report.
	!
	! Index:
	!	.x Period for Report
	!
	!--

	NUMPRINT% = VAL%(UTL_REPORTX::OPTDEF(6%))

	!++
	! Abstract:FLD07
	!	^*(07) Number to Print\*
	!	.p
	!	The ^*Number to Print\* field determines the number
	!	of products to print (highest volume first) on the report.
	!	.p
	!	When no number is entered, all products will be printed when there
	!	is any volume Year To Date.
	!
	! Index:
	!	.x Number to Print
	!
	!--

	DOLLIMIT = VAL(UTL_REPORTX::OPTDEF(7%))

	!++
	! Abstract:FLD08
	!	^*(08) Low Dollar Limit\*
	!	.p
	!	The ^*Low Dollar Limit\* field determines
	!	the low limit of Year To Date sales of products to print on the report.
	!	.p
	!	When no number is entered, all products will be printed when there
	!	is any volume Year To Date and the products fall within the number to print
	!	range.
	!
	! Index:
	!	.x Low Dollar Limit
	!
	!--

	CALL ASSG_CHANNEL(SA_TEMP.CH%, STAT%)

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

400	!
	! Create temporary file now
	!
	WHEN ERROR IN
		OPEN "SA_TEMP.TMP" FOR OUTPUT AS FILE SA_TEMP.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP SA_DIMVALUE, &
			PRIMARY KEY (SA_DIMVALUE_VALUE) DUPLICATES, &
			TEMPORARY, &
			BUFFER 32%, &
			ACCESS MODIFY, ALLOW NONE
	USE
		FILENAME$ = "TEMPORARY"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "PRODUCT SALES BY SALES VOLUME"
	TITLE$(2%) = "Report For "
	TITLE$(3%) = "Sales Analysis System"
	TITLE$(4%) = ""

	!
	! Heading
	!
	!	'     1234567890123456789012345678901234567890
	TITLE$(5%) = "ProductNumber  Description              " + &
		"            Type Categ        QtySold Co" + &
		"stofSale  SaleTotal Sale% GrossMargn Gross% Margin%"
	TITLE$(6%) = "."

	IF NUMPRINT% <> 0%
	THEN
		TITLE$(2%) = TITLE$(2%) + "Top " + &
			FORMAT$(NUMPRINT%, "####") + " Products"
	ELSE
		TITLE$(2%) = TITLE$(2%) + "All Products"
	END IF

	IF DOLLIMIT <> 0.0
	THEN
		TITLE$(2%) = TITLE$(2%) + " With Sales of " + &
			FORMAT$(DOLLIMIT, "###,###.##") + " or More"
	END IF

	SELECT SORTBY$
	CASE "N"
		K_NUM% = 0%
		TITLE$(1%) = TITLE$(1%) + " AND PRODUCT NUMBER"
	CASE "T"
		K_NUM% = 1%
		TITLE$(1%) = TITLE$(1%) + " AND PRODUCT TYPE"
	CASE "C"
		K_NUM% = 2%
		TITLE$(1%) = TITLE$(1%) + " AND PRODUCT CATEGORY"
	CASE "A"
		K_NUM% = 3%
		TITLE$(1%) = TITLE$(1%) + " AND PRODUCT DESCRIPTION"
	END SELECT

	%PAGE


17000	!***************************************************************
	! FIRST PASS - CREATE TEMPORARY FILE
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
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

	CALL ENTR_3MESSAGE(SCOPE, &
		"Creating temporary file.  Reading work files.", 1%)

 GetNextRecFirst:
17020	!
	! Main loop
	!
	! Get record from Product Description file
	!
	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, REGARDLESS
	USE
		CONTINUE 17200 IF ERR = 11%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	SELECT SORTBY$
	CASE "N"
		GOTO 17200 IF (PD_PRODUCT::PRODUCT_NUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""
		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(PD_PRODUCT::PRODUCT_NUM, -1%), &
			WLDCRD$) = 0% &
			AND WLDCRD$ <> ""
	CASE "T"
		GOTO 17200 IF (PD_PRODUCT::PROD_TYPE > TO_ITEM$) AND &
			TO_ITEM$ <> ""
		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(PD_PRODUCT::PROD_TYPE, -1%), &
			WLDCRD$) = 0% &
			AND WLDCRD$ <> ""
	CASE "C"
		GOTO 17200 IF (PD_PRODUCT::CATEGORY > TO_ITEM$) AND &
			TO_ITEM$ <> ""
		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(PD_PRODUCT::CATEGORY, -1%), &
			WLDCRD$) = 0% &
			AND WLDCRD$ <> ""
	CASE "A"
		GOTO 17200 IF (PD_PRODUCT::DESCRIPTION > TO_ITEM$) AND &
			TO_ITEM$ <> ""
		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(PD_PRODUCT::DESCRIPTION, -1%), &
			WLDCRD$) = 0% &
			AND WLDCRD$ <> ""
	END SELECT

	COSTTOTAL = 0.0
	SALETOTAL = 0.0
	QTYTOTAL  = 0.0

17050	WHEN ERROR IN
		FIND #IC_TRANSACTION.CH%, &
			KEY #0% EQ PD_PRODUCT::PRODUCT_NUM, &
			REGARDLESS
	USE
		CONTINUE 17020 IF ERR = 155%
		FILENAME$ = "IC_TRANSACTION"
		CONTINUE HelpError
	END WHEN

17060	WHEN ERROR IN
		GET #IC_TRANSACTION.CH%, REGARDLESS
	USE
		CONTINUE 17100 IF ERR = 11%
		FILENAME$ = "IC_TRANSACTION"
		CONTINUE HelpError
	END WHEN

	GOTO 17100 IF IC_TRANSACTION::PRODUCT <> PD_PRODUCT::PRODUCT_NUM

	IF IC_TRANSACTION::TYPE_A = "SA"
	THEN
		COSTTOTAL = COSTTOTAL + (-IC_TRANSACTION::COST)
		SALETOTAL = SALETOTAL + (-IC_TRANSACTION::PRICE)
		QTYTOTAL  = QTYTOTAL  + (-IC_TRANSACTION::QUANTITY_A)
	END IF

	IF IC_TRANSACTION::TYPE_B = "SA"
	THEN
		COSTTOTAL = COSTTOTAL + (-IC_TRANSACTION::COST)
		SALETOTAL = SALETOTAL + (-IC_TRANSACTION::PRICE)
		QTYTOTAL  = QTYTOTAL  + (-IC_TRANSACTION::QUANTITY_B)
	END IF

	GOTO 17060

17100	COSTTOTAL = FUNC_ROUND(COSTTOTAL, 2%)
	SALETOTAL = FUNC_ROUND(SALETOTAL, 2%)
	QTYTOTAL  = FUNC_ROUND(QTYTOTAL, 2%)
	!
	! Lookup sales history here by using Read_History routine
	!
	V% = IC_READ_HISTORY(PD_PRODUCT::PRODUCT_NUM, "", YYYYPP$, &
		TOTHQ, TOTHS, TOTHC)

	TOTHQ = -TOTHQ
	TOTHS = -TOTHS
	TOTHC = -TOTHC

	IF (SALETOTAL = 0.0) AND (COSTTOTAL = 0.0) AND &
		(QTYTOTAL = 0.0) AND (TOTHQ = 0.0) AND &
		(TOTHS = 0.0) AND (TOTHC = 0.0)
	THEN
		GOTO GetNextRecFirst
	END IF

	PCOST = PCOST + COSTTOTAL
	PSALE = PSALE + SALETOTAL
	YCOST = YCOST + (COSTTOTAL + TOTHC)
	YSALE = YSALE + (SALETOTAL + TOTHS)

	CUSTTOTAL% = CUSTTOTAL% + 1

	!
	! Screen for Dollar Volume sales limits now
	!
	GOTO GetNextRecFirst IF (PSALE + TOTHS) < DOLLIMIT AND DOLLIMIT <> 0.0

	!
	! Write out temporary file record
	!
	SA_DIMVALUE_VALUE = -(SALETOTAL + TOTHS)
	SA_DIMVALUE_PSALE = SALETOTAL
	SA_DIMVALUE_PCOST = COSTTOTAL
	SA_DIMVALUE_YSALE = TOTHS
	SA_DIMVALUE_YCOST = TOTHC
	SA_DIMVALUE_PQTY = QTYTOTAL
	SA_DIMVALUE_YQTY = TOTHQ
	SA_DIMVALUE_PRODRFA = GETRFA(PD_PRODUCT.CH%)


	PUT #SA_TEMP.CH%

	GOTO GetNextRecFirst

17200	!
	! Print the suckers out here
	!
	PGROSS = PSALE - PCOST
	YGROSS = YSALE - YCOST

	COUNTSTUFF% = 0%

	RESET #SA_TEMP.CH%

 GetNextRec:
17300	!
	! Read temp records and process
	!
	WHEN ERROR IN
		Get #SA_TEMP.CH%, REGARDLESS
	USE
		CONTINUE 17400 IF ERR = 11%
		FILENAME$ = "TEMPORARY"
		CONTINUE HelpError
	END WHEN

	COUNTSTUFF% = COUNTSTUFF% + 1%

	GOTO 17400 IF COUNTSTUFF% > NUMPRINT% AND NUMPRINT% <> 0%

17350	GET #PD_PRODUCT.CH%, RFA SA_DIMVALUE_PRODRFA, REGARDLESS

	SALETOTAL = SA_DIMVALUE_PSALE
	COSTTOTAL = SA_DIMVALUE_PCOST
	QTYTOTAL  = SA_DIMVALUE_PQTY
	TOTHS = SA_DIMVALUE_YSALE
	TOTHC = SA_DIMVALUE_YCOST
	TOTHQ   = SA_DIMVALUE_YQTY

		GROSS = SALETOTAL - COSTTOTAL

		IF SALETOTAL = 0.0
		THEN
			MARGINS = 0.0
		ELSE
			MARGINS = 100 * (GROSS / SALETOTAL)
		END IF
		!
		! Print out one line
		!
		FIRSTP = 0.0
		SECOUNDP = 0.0
		FIRSTP = 100*(SALETOTAL/PSALE) IF PSALE <> 0.0
		SECOUNDP = 100*(GROSS/PGROSS) IF PGROSS <> 0.0

		TEXT$ = PD_PRODUCT::PRODUCT_NUM + " " + &
			LEFT$(PD_PRODUCT::DESCRIPTION, 36%) + " " + &
			PD_PRODUCT::PROD_TYPE + "   " + &
			PD_PRODUCT::CATEGORY + "  " + &
			"PTD:" + &
			FORMAT$(QTYTOTAL, "#######.##") + " " + &
			FORMAT$(COSTTOTAL, "#######.##") + " " + &
			FORMAT$(SALETOTAL, "#######.##") + " " + &
			FORMAT$(FIRSTP, "###.#") + " " + &
			FORMAT$(GROSS, "#######.##") + "  " + &
			FORMAT$(SECOUNDP, "###.#") + "   " + &
			FORMAT$(MARGINS, "###.#")

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
			MARGINS1 = 100 * (GROSSY / (SALETOTAL + TOTHS))
		END IF

		!
		! Print out one line
		!
		FIRSTP = 0.0
		SECOUNDP = 0.0
		FIRSTP = 100 * ((SALETOTAL + TOTHS) / YSALE) IF YSALE <> 0.0
		SECOUNDP = 100 * (GROSSY/YGROSS) IF YGROSS <> 0.0

		TEXT$ = PD_PRODUCT::PRODUCT_NUM + &
			"                                                 " + &
			"YTD:" + &
			FORMAT$(QTYTOTAL + TOTHQ, "#######.##") + " " + &
			FORMAT$(COSTTOTAL + TOTHC, "#######.##") + " " + &
			FORMAT$(SALETOTAL + TOTHS, "#######.##") + " " + &
			FORMAT$(FIRSTP, "###.#") + " " + &
			FORMAT$(GROSSY, "#######.##") + "  " + &
			FORMAT$(SECOUNDP, "###.#") + "   " + &
			FORMAT$(MARGINS1, "###.#")



		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		FIRSTLINE = -1%

	!
	! Try for next record
	!
	GOTO GetNextRec


 ExitTotal:
17400	!
	! Handle end of the report
	!

	IF FIRSTLINE = -1%
	THEN

		!
		! Print out one line
		!
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

		IF PSALE = 0.0
		THEN
			TOTMARGIN2 = 0.0
		ELSE
			TOTMARGIN2 = 100 * (PGROSS / PSALE)
		END IF

		TEXT$ = "            Totals for " + &
			FORMAT$(CUSTTOTAL%, "###") + &
			" products                            PTD:           " + &
			FORMAT$(PCOST, "#######.##") + " " + &
			FORMAT$(PSALE, "#######.##") + " " + &
			FORMAT$(100.0, "###.#") + " " + &
			FORMAT$(PGROSS, "#######.##") + "  " + &
			FORMAT$(100.0, "###.#") + "   " + &
			FORMAT$(TOTMARGIN2, "###.#")


		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		!
		! Print out one line
		!
		IF YSALE = 0.0
		THEN
			YTDMARGIN2 = 0.0
		ELSE
			YTDMARGIN2 = 100 * (YGROSS / YSALE)
		END IF

		TEXT$ = "            Totals for " + &
			FORMAT$(CUSTTOTAL%, "###") + &
			" products                            YTD:           " + &
			FORMAT$(YCOST, "#######.##") + " " + &
			FORMAT$(YSALE, "#######.##") + " " + &
			FORMAT$(100.0, "###.#") + " " + &
			FORMAT$(YGROSS, "#######.##") + "  " + &
			FORMAT$(100.0, "###.#") + "   " + &
			FORMAT$(YTDMARGIN2, "###.#")


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
