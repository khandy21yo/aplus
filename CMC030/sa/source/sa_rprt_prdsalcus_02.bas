1	%TITLE "Sales by Product and Salesman"
	%SBTTL "SA_RPRT_PRDSALCUS_02"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 2001 BY
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
	! Software Solutions, Inc. assumes no responsibility for the use
	! or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	! ID:0012
	!
	! Abstract:HELP
	!	.p
	!	The ^*Sales by Product and Salesman Report\* option
	!	prints a sales report listing the following information:
	!	.b
	!	.lm +10
	!	.list 0,"*"
	!	.le
	!	Product Number
	!	.le
	!	Product Type
	!	.le
	!	Product Category
	!	.le
	!	Product Description
	!	.le
	!	Product Secondary Code
	!	.le
	!	Salesman Name
	!	.le
	!	Salesman Number
	!	.le
	!	Customer Number
	!	.le
	!	Customer Name
	!	.le
	!	Sales Period
	!	.le
	!	Sales Quantity
	!	.le
	!	Sales Amount
	!	.le
	!	Profit
	!	.le
	!	Variance
	!	.els
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS SA_SOURCE:SA_RPRT_PRDSALCUS_02/LINE
	!	$ LINK/EXECUTABLE=SA_EXE: SA_RPRT_PRDSALCUS_02, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE SA_RPRT_PRDSALCUS_02.OBJ;*
	!
	! Author:
	!
	!	03/29/2001 - Kevin Handy
	!
	! Modification history:
	!
	!--

	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION SA_EXAM_SALESMAN

	!
	! CDD inclusions
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[IC.OPEN]IC_35HISTORY.HB"
	MAP (IC_35HISTORY)	IC_35HISTORY_CDD	IC_35HISTORY

	%INCLUDE "SOURCE:[SA.OPEN]SA_SALESMAN.HB"
	DECLARE			SA_SALESMAN_CDD		SA_SALESMAN_EXAM

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	DECLARE			SB_SUBACCOUNT_CDD	SB_SUBACCOUNT_EXAM

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	DECLARE			AR_35CUSTOM_CDD		AR_35CUSTOM_EXAM

	!
	! Array for variances
	!
	DECLARE REAL VARIANCE(17%)

	!
	! Used in PrintLine, created at 17120
	!
	DECLARE LONG CONSTANT MAX_SALESMAN = 300%

	DECLARE REAL PQUANTITY(MAX_SALESMAN, 12%)
	DECLARE REAL PRICEAMT(MAX_SALESMAN, 12%)
	DECLARE REAL COSTAMT(MAX_SALESMAN, 12%)

	DIM SALESMAN$(MAX_SALESMAN)
	DIM QTYQTR(MAX_SALESMAN, 4%), PRCQTR(MAX_SALESMAN, 4%),	CSTQTR(MAX_SALESMAN, 4%)
	DIM QTYYTD(MAX_SALESMAN), PRCYTD(MAX_SALESMAN), CSTYTD(MAX_SALESMAN)

	!
	! Used in ProdTotal, created in SaleTotal
	!
	DECLARE REAL PRODPRICEAMT(MAX_SALESMAN, 12%)
	DECLARE REAL PRODCOSTAMT(MAX_SALESMAN, 12%)

	DIM PROD_SALESMAN$(MAX_SALESMAN)
	DIM PRODQTYQTR(MAX_SALESMAN, 4%), PRODPRCQTR(MAX_SALESMAN, 4%), PRODCSTQTR(MAX_SALESMAN, 4%)
	DIM PRODQTYYTD(MAX_SALESMAN), PRODPRCYTD(MAX_SALESMAN), PRODCSTYTD(MAX_SALESMAN)

	!
	! Used in ExitTotal, created in ProdTotal
	!
 !	DECLARE REAL TOTALPRICEAMT(MAX_SALESMAN, 12%)
 !	DECLARE REAL TOTALCOSTAMT(MAX_SALESMAN, 12%)
 !
 !	DIM TOTAL_SALESMAN$(MAX_SALESMAN)
 !	DIM TOTALQTYQTR(MAX_SALESMAN, 4%), TOTALPRCQTR(MAX_SALESMAN, 4%), TOTALCSTQTR(MAX_SALESMAN, 4%)
 !	DIM TOTALQTYYTD(MAX_SALESMAN), TOTALPRCYTD(MAX_SALESMAN), &
 !		TOTALCSTYTD(MAX_SALESMAN)

	!
	! Used in SubTotal
	!
	DECLARE REAL SUBTOTAL_PRICEAMT(12%)
	DECLARE REAL SUBTOTAL_COSTAMT(12%)

	DIM SUBTOTAL_QTYQTR(4%), SUBTOTAL_PRCQTR(4%), SUBTOTAL_CSTQTR(4%)

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

 Initialization:
	!
	! Initialize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)

	!++
	! Abstract:FLD01
	!	^*(01) Sort by (N,T,C,D,S)\*
	!	.p
	!	The ^*Sort by\* field determines the order
	!	in which the report will print.
	!	.p
	!	Valid settings are:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	P - Product Number
	!	.le
	!	T - Product Type
	!	.le
	!	C - Product Category
	!	.le
	!	D - Product Description
	!	.le
	!	S - Product Secondary_Code
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

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02)From Item\*
	!	.p
	!	The ^*From Item\* field enters the
	!	item with which to begin with.
	!	.p
	!	A blank field causes the report to begin with the first
	!	item in the file.
	!
	! Index:
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.p
	!	The ^*To Item\* field specifies the item
	!	with which to end the report.
	!	.p
	!	A blank field causes the report to end with the last
	!	item in the file.
	!
	! Index:
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.p
	!	The ^*Wildcard\* field selects
	!	designated items to be printed by entering a "wildcard"
	!	using the Wildcarding Technique.
	!
	! Index:
	!
	!--

	LOC_WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Location Wildcard\*
	!	.p
	!	The ^*Location Wildcard\* field selects
	!	designated locations to be printed by entering a "wildcard"
	!	using the Wildcarding Technique.  All locations will be included
	!	if this field is left blank
	!
	! Index:
	!
	!--

	PRINT_UNITS$ = EDIT$(UTL_REPORTX::OPTDEF(5%), -1%)

	!++
	! Abstract:FLD06
	!	^*(06) Print Units\*
	!	.p
	!	The ^*Print Unit\* field optionally
	!	shows the Unit Quantity.
	!
	! Index:
	!
	!--

	GROSS_PROFIT$ = EDIT$(UTL_REPORTX::OPTDEF(6%), -1%)

	!++
	! Abstract:FLD07
	!	^*(07) Print Gross Profit\*
	!	.p
	!	The ^*Print Gross Profit\* field optionally
	!	shows the gross profit.
	!
	! Index:
	!
	!--

	YYYY$ = EDIT$(UTL_REPORTX::OPTDEF(8%), -1%)

	!++
	! Abstract:FLD10
	!	^*(10) Number of Years\*
	!	.p
	!	The ^*Number of Years\* field
	!	prints that number of the fiscal years. If
	!	the selected inventory transaction history file doesn't
	!	exist, report will not search for the older years.
	!
	! Index:
	!
	!--

	%PAGE

300	!
	! Open Product file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

310	!
	! Open History file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_35HISTORY.OPN"
	USE
		FILENAME$ = "IC_35HISTORY"
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
	!
	! Title
	!
	SELECT SORTBY$
	CASE "P"
		K_NUM% = 0%
		TITLE$(1%) = "SALES BY PRODUCT NUMBER"
		SORT_NAME$ = "Number"

	CASE "T"
		K_NUM% = 1%
		TITLE$(1%) = "SALES BY PRODUCT TYPE"
		SORT_NAME$ = "Type"

	CASE "C"
		K_NUM% = 2%
		TITLE$(1%) = "SALES BY PRODUCT CATEGORY"
		SORT_NAME$ = "Category"

	CASE "D"
		K_NUM% = 3%
		TITLE$(1%) = "SALES BY PRODUCT DESCRIPTION"
		SORT_NAME$ = "Description"

	CASE "S"
		K_NUM% = 4%
		TITLE$(1%) = "SALES BY PRODUCT SECONDARY CODE"
		SORT_NAME$ = "Secondary"

	END SELECT

	TITLE$(2%) = "."
	TITLE$(3%) = "Sales Analysis System"
	TITLE$(4%) = ""

	!
	! Heading
	!
	TITLE$(5%) = "      Salesman#     Salesman                 " + &
		"          Cust#          CustName"

	TITLE$(6%) = "         Per1   Per2   Per3    Qtr1   Per4   Per5   " + &
		"Per6    Qtr2   Per7   Per8   Per9    Qtr3  " + &
		"Per10  Per11  Per12    Qtr4     YTD"

	TITLE$(7%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

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
		CONTINUE ExitProgram IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

	PRINT_FLAG% = 0%
	LAST_SORT$ = "~~~~~~~~~~"

 GetNextRec:
17020	!
	! Get next record
	!
	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

	!
	! Check status
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	SELECT SORTBY$

	CASE "P"
		GOTO ExitTotal IF (PD_PRODUCT::PRODUCT_NUM > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(PD_PRODUCT::PRODUCT_NUM, &
			-1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

		IF LAST_SORT$ <> PD_PRODUCT::PRODUCT_NUM
		THEN
			GOSUB SaleTotal
			GOTO ExitProgram IF UTL_REPORTX::STAT
		END IF

		LAST_SORT$ = PD_PRODUCT::PRODUCT_NUM

	CASE "T"
		GOTO ExitTotal IF (PD_PRODUCT::PROD_TYPE > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(PD_PRODUCT::PROD_TYPE, &
			-1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

		IF LAST_SORT$ <> PD_PRODUCT::PROD_TYPE
		THEN
			GOSUB SaleTotal
			GOTO ExitProgram IF UTL_REPORTX::STAT
		END IF

		LAST_SORT$ = PD_PRODUCT::PROD_TYPE

	CASE "C"
		GOTO ExitTotal IF (PD_PRODUCT::CATEGORY > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(PD_PRODUCT::CATEGORY, &
			-1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

		IF LAST_SORT$ <> PD_PRODUCT::CATEGORY
		THEN
			GOSUB SaleTotal
			GOTO ExitProgram IF UTL_REPORTX::STAT
		END IF

		LAST_SORT$ = PD_PRODUCT::CATEGORY

	CASE "D"
		GOTO ExitTotal IF (PD_PRODUCT::DESCRIPTION > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(PD_PRODUCT::DESCRIPTION, &
			-1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

		IF LAST_SORT$ <> PD_PRODUCT::DESCRIPTION
		THEN
			GOSUB SaleTotal
			GOTO ExitProgram IF UTL_REPORTX::STAT
		END IF

		LAST_SORT$ = PD_PRODUCT::DESCRIPTION

	CASE "S"
		GOTO ExitTotal &
			IF (PD_PRODUCT::SECONDARY_CODE > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING(EDIT$( &
			PD_PRODUCT::SECONDARY_CODE, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

		IF LAST_SORT$ <> PD_PRODUCT::SECONDARY_CODE
		THEN
			GOSUB SaleTotal
			GOTO ExitProgram IF UTL_REPORTX::STAT
		END IF

		LAST_SORT$ = PD_PRODUCT::SECONDARY_CODE

	END SELECT

	TEST.LINE$ = ""

17100	!
	! Get History records
	!
	WHEN ERROR IN
		FIND #IC_35HISTORY.CH%, &
			KEY #0% EQ PD_PRODUCT::PRODUCT_NUM, &
			REGARDLESS
	USE
		CONTINUE GetNextRec IF ERR = 155%
		FILENAME$ = "IC_35HISTORY"
		CONTINUE HelpError
	END WHEN

	!
	! Pull out next record from last file used.
	!
 GetHistRec:
17120	WHEN ERROR IN
		GET #IC_35HISTORY.CH%, REGARDLESS
	USE
		CONTINUE GetNextRec IF ERR = 11%
		FILENAME$ = "IC_35HISTORY"
		CONTINUE HelpError
	END WHEN

	GOTO GetNextRec IF PD_PRODUCT::PRODUCT_NUM <> IC_35HISTORY::PRODUCT

	GOTO GetHistRec IF IC_35HISTORY::TRANSTYPE <> "SA"

	IF LOC_WLDCRD$ <> ""
	THEN
		GOTO GetHistRec &
			IF COMP_STRING(EDIT$(IC_35HISTORY::LOCATION, -1%), &
			LOC_WLDCRD$) = 0%
	END IF

	PRINT_FLAG% = -1%

17130	!
	! Pick the matching salesman
	!
	Y% = 0%
	Y% = I% IF SALESMAN$(I%) = IC_35HISTORY::SUBACCT &
		FOR I% = 1% TO TOTAL_SALESMAN%
	IF Y% = 0%
	THEN
		!
		! New salesman that needs to be added to the list
		! Zero out any garbage that may be there
		!
		Y%, TOTAL_SALESMAN% = TOTAL_SALESMAN% + 1%
		SALESMAN$(Y%) = IC_35HISTORY::SUBACCT

		FOR I% = 1% TO 12%
			PQUANTITY(Y%, I%) = 0.0
			PRICEAMT(Y%, I%) = 0.0
			COSTAMT(Y%, I%) = 0.0
		NEXT I%

		FOR I% = 1% TO 4%
			QTYQTR(Y%, I%) = 0.0
			PRCQTR(Y%, I%) = 0.0
			CSTQTR(Y%, I%) = 0.0
		NEXT I%

		QTYYTD(Y%) = 0.0
		PRCYTD(Y%) = 0.0
		CSTYTD(Y%) = 0.0
	END IF

17140	FOR I% = 1% TO 12%
		PQUANTITY(Y%, I%) = PQUANTITY(Y%, I%) - &
			IC_35HISTORY::PQUANTITY(I%)
		PRICEAMT(Y%, I%) = PRICEAMT(Y%, I%) + IC_35HISTORY::PRICEAMT(I%)
		COSTAMT(Y%, I%) = COSTAMT(Y%, I%) + IC_35HISTORY::COSTAMT(I%)

		J% = INT((I% - 1%) / 3%) + 1%
		QTYQTR(Y%, J%) = QTYQTR(Y%, J%) - IC_35HISTORY::PQUANTITY(I%)
		PRCQTR(Y%, J%) = PRCQTR(Y%, J%) + IC_35HISTORY::PRICEAMT(I%)
		CSTQTR(Y%, J%) = CSTQTR(Y%, J%) + IC_35HISTORY::COSTAMT(I%)
		QTYYTD(Y%) = QTYYTD(Y%) - IC_35HISTORY::PQUANTITY(I%)
		PRCYTD(Y%) = PRCYTD(Y%) + IC_35HISTORY::PRICEAMT(I%)
		CSTYTD(Y%) = CSTYTD(Y%) + IC_35HISTORY::COSTAMT(I%)
	NEXT I%

	GOTO GetHistRec

	%PAGE

17900	!
 ExitTotal:
	GOSUB SaleTotal
	GOTO ExitProgram IF UTL_REPORTX::STAT
	GOSUB ProdTotal

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

18000	!*******************************************************************
	! Print salesman subtotals
	!*******************************************************************

 SaleTotal:

	GOTO SkipSaleTotal IF PRINT_FLAG% = 0%

	TEXT$ = SORT_NAME$ + "     " + LAST_SORT$
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 999%)

	FOR Y% = 1% TO TOTAL_SALESMAN%

		V% = SA_EXAM_SALESMAN(SALESMAN$(Y%), SA_SALESMAN_EXAM, &
			SB_SUBACCOUNT_EXAM)

		!
		! Print out one line
		!
		TEXT$ = "     " + SALESMAN$(Y%) + "     " + &
			LEFT$(SA_SALESMAN_EXAM::DESCR, 30%)

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 6%)

		TEXT$ = "Sales " + &
			FORMAT$(PRICEAMT(Y%, 1%), "#######") + &
			FORMAT$(PRICEAMT(Y%, 2%), "#######") + &
			FORMAT$(PRICEAMT(Y%, 3%), "#######") + &
			FORMAT$(PRCQTR(Y%, 1%), "########") + &
			FORMAT$(PRICEAMT(Y%, 4%), "#######") + &
			FORMAT$(PRICEAMT(Y%, 5%), "#######") + &
			FORMAT$(PRICEAMT(Y%, 6%), "#######") + &
			FORMAT$(PRCQTR(Y%, 2%), "########") + &
			FORMAT$(PRICEAMT(Y%, 7%), "#######") + &
			FORMAT$(PRICEAMT(Y%, 8%), "#######") + &
			FORMAT$(PRICEAMT(Y%, 9%), "#######") + &
			FORMAT$(PRCQTR(Y%, 3%), "########") + &
			FORMAT$(PRICEAMT(Y%, 10%), "#######") + &
			FORMAT$(PRICEAMT(Y%, 11%), "#######") + &
			FORMAT$(PRICEAMT(Y%, 12%), "#######") + &
			FORMAT$(PRCQTR(Y%, 4%), "########") + &
			FORMAT$(PRCYTD(Y%), "########")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		IF GROSS_PROFIT$ = "Y"
		THEN
			TEXT$ = "GProf " + &
				FORMAT$(PRICEAMT(Y%, 1%) - &
					COSTAMT(Y%, 1%), "#######") + &
				FORMAT$(PRICEAMT(Y%, 2%) - &
					COSTAMT(Y%, 2%), "#######") + &
				FORMAT$(PRICEAMT(Y%, 3%) - &
					COSTAMT(Y%, 3%), "#######") + &
				FORMAT$(PRCQTR(Y%, 1%) - &
					CSTQTR(Y%, 1%), "########") + &
				FORMAT$(PRICEAMT(Y%, 4%) - &
					COSTAMT(Y%, 4%), "#######") + &
				FORMAT$(PRICEAMT(Y%, 5%) - &
					COSTAMT(Y%, 5%), "#######") + &
				FORMAT$(PRICEAMT(Y%, 6%) - &
					COSTAMT(Y%, 6%), "#######") + &
				FORMAT$(PRCQTR(Y%, 2%) - &
					CSTQTR(Y%, 2%), "########") + &
				FORMAT$(PRICEAMT(Y%, 7%) - &
					COSTAMT(Y%, 7%), "#######") + &
				FORMAT$(PRICEAMT(Y%, 8%) - &
					COSTAMT(Y%, 8%), "#######") + &
				FORMAT$(PRICEAMT(Y%, 9%) - &
					COSTAMT(Y%, 9%), "#######") + &
				FORMAT$(PRCQTR(Y%, 3%) - &
					CSTQTR(Y%, 3%), "########") + &
				FORMAT$(PRICEAMT(Y%, 10%) - &
					COSTAMT(Y%, 10%), "#######") + &
				FORMAT$(PRICEAMT(Y%, 11%) - &
					COSTAMT(Y%, 11%), "#######") + &
				FORMAT$(PRICEAMT(Y%, 12%) - &
					COSTAMT(Y%, 12%), "#######") + &
				FORMAT$(PRCQTR(Y%, 4%) - &
					CSTQTR(Y%, 4%), "########") + &
				FORMAT$(PRCYTD(Y%) - &
					CSTYTD(Y%), "########")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 1%)

		END IF

	NEXT Y%

18020	FOR Y% = 1% TO TOTAL_SALESMAN%

		Y1% = 0%
		Y1% = I% IF SALESMAN$(Y%) = PROD_SALESMAN$(I%) &
			FOR I% = 1% TO PROD_SALESMAN%

		IF Y1% = 0%
		THEN
			!
			! New salesman that needs to be added to the list
			! Zero out any garbage that may be there
			!
			Y1%, PROD_SALESMAN% = PROD_SALESMAN% + 1%
			PROD_SALESMAN$(Y1%) = SALESMAN$(Y%)

			FOR I% = 1% TO 12%
				PRODPRICEAMT(Y1%, I%) = 0.0
				PRODCOSTAMT(Y1%, I%) = 0.0
			NEXT I%

			FOR I% = 1% TO 4%
				PRODQTYQTR(Y1%, I%) = 0.0
				PRODPRCQTR(Y1%, I%) = 0.0
				PRODCSTQTR(Y1%, I%) = 0.0
			NEXT I%

			PRODQTYYTD(Y1%) = 0.0
			PRODPRCYTD(Y1%) = 0.0
			PRODCSTYTD(Y1%) = 0.0
		END IF

		PRODPRCYTD(Y1%) = PRODPRCYTD(Y1%) + PRCYTD(Y%)
		SUBTOTAL_PRCYTD = SUBTOTAL_PRCYTD + PRCYTD(Y%)
		PRODCSTYTD(Y1%) = PRODCSTYTD(Y1%) + CSTYTD(Y%)
		SUBTOTAL_CSTYTD = SUBTOTAL_CSTYTD + CSTYTD(Y%)

		PRCYTD(Y%) = 0.0
		CSTYTD(Y%) = 0.0

		FOR I% = 1% TO 12%
			PRODPRICEAMT(Y1%, I%) = PRODPRICEAMT(Y%, I%) + &
				PRICEAMT(Y%, I%)
			SUBTOTAL_PRICEAMT(I%) = SUBTOTAL_PRICEAMT(I%) + &
				PRICEAMT(Y%, I%)
			PRODCOSTAMT(Y1%, I%) = PRODCOSTAMT(Y%, I%) + &
				COSTAMT(Y%, I%)
			SUBTOTAL_COSTAMT(I%) = SUBTOTAL_COSTAMT(I%) + &
				COSTAMT(Y%, I%)

			PRICEAMT(Y%, I%) = 0.0
			COSTAMT(Y%, I%) = 0.0
		NEXT I%

		FOR I% = 1% TO 4%
			PRODPRCQTR(Y1%, I%) = PRODPRCQTR(Y1%, I%) + &
				PRCQTR(Y%, I%)
			SUBTOTAL_PRCQTR(I%) = SUBTOTAL_PRCQTR(I%) + &
				PRCQTR(Y%, I%)
			PRODCSTQTR(Y1%, I%) = PRODCSTQTR(Y1%, I%) + &
				CSTQTR(Y%, I%)
			SUBTOTAL_CSTQTR(I%) = SUBTOTAL_CSTQTR(I%) + &
				CSTQTR(Y%, I%)

			PRCQTR(Y%, I%) = 0.0
			CSTQTR(Y%, I%) = 0.0

		NEXT I%

	NEXT Y%

	TOTAL_SALESMAN% = 0%

	TEXT$ = "CATEGORY TOTAL"
	GOSUB Subtotal_Total

 SkipSaleTotal:

	PRINT_FLAG% = 0%

	RETURN

	!*******************************************************************
	! Print product totals
	!*******************************************************************

18200	!
 ProdTotal:
	TEXT$ = "GRAND TOTALS"
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 999%)

	FOR Y% = 1% TO PROD_SALESMAN%

		V% = SA_EXAM_SALESMAN(PROD_SALESMAN$(Y%), SA_SALESMAN_EXAM, &
			SB_SUBACCOUNT_EXAM)

		!
		! Print out one line
		!
		TEXT$ = "     " + PROD_SALESMAN$(Y%) + "     " + &
			LEFT$(SA_SALESMAN_EXAM::DESCR, 30%)

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 6%)

		TEXT$ = "Sales " + &
			FORMAT$(PRODPRICEAMT(Y%, 1%), "#######") + &
			FORMAT$(PRODPRICEAMT(Y%, 2%), "#######") + &
			FORMAT$(PRODPRICEAMT(Y%, 3%), "#######") + &
			FORMAT$(PRODPRCQTR(Y%, 1%), "########") + &
			FORMAT$(PRODPRICEAMT(Y%, 4%), "#######") + &
			FORMAT$(PRODPRICEAMT(Y%, 5%), "#######") + &
			FORMAT$(PRODPRICEAMT(Y%, 6%), "#######") + &
			FORMAT$(PRODPRCQTR(Y%, 2%), "########") + &
			FORMAT$(PRODPRICEAMT(Y%, 7%), "#######") + &
			FORMAT$(PRODPRICEAMT(Y%, 8%), "#######") + &
			FORMAT$(PRODPRICEAMT(Y%, 9%), "#######") + &
			FORMAT$(PRODPRCQTR(Y%, 3%), "########") + &
			FORMAT$(PRODPRICEAMT(Y%, 10%), "#######") + &
			FORMAT$(PRODPRICEAMT(Y%, 11%), "#######") + &
			FORMAT$(PRODPRICEAMT(Y%, 12%), "#######") + &
			FORMAT$(PRODPRCQTR(Y%, 4%), "########") + &
			FORMAT$(PRODPRCYTD(Y%), "########")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		IF GROSS_PROFIT$ = "Y"
		THEN
			TEXT$ = "GProf " + &
				FORMAT$(PRODPRICEAMT(Y%, 1%) - &
					PRODCOSTAMT(Y%, 1%), "#######") + &
				FORMAT$(PRODPRICEAMT(Y%, 2%) - &
					PRODCOSTAMT(Y%, 2%), "#######") + &
				FORMAT$(PRODPRICEAMT(Y%, 3%) - &
					PRODCOSTAMT(Y%, 3%), "#######") + &
				FORMAT$(PRODPRCQTR(Y%, 1%) - &
					PRODCSTQTR(Y%, 1%), "########") + &
				FORMAT$(PRODPRICEAMT(Y%, 4%) - &
					PRODCOSTAMT(Y%, 4%), "#######") + &
				FORMAT$(PRODPRICEAMT(Y%, 5%) - &
					PRODCOSTAMT(Y%, 5%), "#######") + &
				FORMAT$(PRODPRICEAMT(Y%, 6%) - &
					PRODCOSTAMT(Y%, 6%), "#######") + &
				FORMAT$(PRODPRCQTR(Y%, 2%) - &
					PRODCSTQTR(Y%, 2%), "########") + &
				FORMAT$(PRODPRICEAMT(Y%, 7%) - &
					PRODCOSTAMT(Y%, 7%), "#######") + &
				FORMAT$(PRODPRICEAMT(Y%, 8%) - &
					PRODCOSTAMT(Y%, 8%), "#######") + &
				FORMAT$(PRODPRICEAMT(Y%, 9%) - &
					PRODCOSTAMT(Y%, 9%), "#######") + &
				FORMAT$(PRODPRCQTR(Y%, 3%) - &
					PRODCSTQTR(Y%, 3%), "########") + &
				FORMAT$(PRODPRICEAMT(Y%, 10%) - &
					PRODCOSTAMT(Y%, 10%), "#######") + &
				FORMAT$(PRODPRICEAMT(Y%, 11%) - &
					PRODCOSTAMT(Y%, 11%), "#######") + &
				FORMAT$(PRODPRICEAMT(Y%, 12%) - &
					PRODCOSTAMT(Y%, 12%), "#######") + &
				FORMAT$(PRODPRCQTR(Y%, 4%) - &
					PRODCSTQTR(Y%, 4%), "########") + &
				FORMAT$(PRODPRCYTD(Y%) - &
					PRODCSTYTD(Y%), "########")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 1%)

		END IF
	NEXT Y%

18220	FOR Y% = 1% TO TOTAL_SALESMAN%
		FOR I% = 1% TO 12%
			SUBTOTAL_PRICEAMT(I%) = SUBTOTAL_PRICEAMT(I%) + &
				PRODPRICEAMT(Y%, I%)
			SUBTOTAL_COSTAMT(I%) = SUBTOTAL_COSTAMT(I%) + &
				PRODCOSTAMT(Y%, I%)
		NEXT I%

		SUBTOTAL_PRCYTD = SUBTOTAL_PRCYTD + PRODPRCYTD(Y%)
		SUBTOTAL_CSTYTD = SUBTOTAL_CSTYTD + PRODCSTYTD(Y%)

		PRODPRCYTD(Y%) = 0.0
		PRODCSTYTD(Y%) = 0.0

		FOR I% = 1% TO 4%
			SUBTOTAL_PRCQTR(I%) = SUBTOTAL_PRCQTR(I%) + &
				PRODPRCQTR(Y%, I%)
			SUBTOTAL_CSTQTR(I%) = SUBTOTAL_CSTQTR(I%) + &
				PRODCSTQTR(Y%, I%)

			PRODPRICEAMT(Y%, I%) = 0.0
			PRODCOSTAMT(Y%, I%) = 0.0

			PRODPRICEAMT(Y%, I% + 4%) = 0.0
			PRODCOSTAMT(Y%, I% + 4%) = 0.0

			PRODPRICEAMT(Y%, I% + 8%) = 0.0
			PRODCOSTAMT(Y%, I% + 8%) = 0.0

			PRODPRCQTR(Y%, I%) = 0.0
			PRODCSTQTR(Y%, I%) = 0.0

		NEXT I%

	NEXT Y%

	TEXT$ = "GRAND TOTAL"
	GOSUB Subtotal_Total

	RETURN

	%PAGE

	!*******************************************************************
	! Print Subtotals
	!*******************************************************************

18400	!
 Subtotal_Total:
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 6%)

	TEXT$ = "Sales " + &
		FORMAT$(SUBTOTAL_PRICEAMT(1%), "#######") + &
		FORMAT$(SUBTOTAL_PRICEAMT(2%), "#######") + &
		FORMAT$(SUBTOTAL_PRICEAMT(3%), "#######") + &
		FORMAT$(SUBTOTAL_PRCQTR(1%), "########") + &
		FORMAT$(SUBTOTAL_PRICEAMT(4%), "#######") + &
		FORMAT$(SUBTOTAL_PRICEAMT(5%), "#######") + &
		FORMAT$(SUBTOTAL_PRICEAMT(6%), "#######") + &
		FORMAT$(SUBTOTAL_PRCQTR(2%), "########") + &
		FORMAT$(SUBTOTAL_PRICEAMT(7%), "#######") + &
		FORMAT$(SUBTOTAL_PRICEAMT(8%), "#######") + &
		FORMAT$(SUBTOTAL_PRICEAMT(9%), "#######") + &
		FORMAT$(SUBTOTAL_PRCQTR(3%), "########") + &
		FORMAT$(SUBTOTAL_PRICEAMT(10%), "#######") + &
		FORMAT$(SUBTOTAL_PRICEAMT(11%), "#######") + &
		FORMAT$(SUBTOTAL_PRICEAMT(12%), "#######") + &
		FORMAT$(SUBTOTAL_PRCQTR(4%), "########") + &
		FORMAT$(SUBTOTAL_PRCYTD, "########")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	IF GROSS_PROFIT$ = "Y"
	THEN
		TEXT$ = "GProf " + &
			FORMAT$(SUBTOTAL_PRICEAMT(1%) - &
				SUBTOTAL_COSTAMT(1%), "#######") + &
			FORMAT$(SUBTOTAL_PRICEAMT(2%) - &
				SUBTOTAL_COSTAMT(2%), "#######") + &
			FORMAT$(SUBTOTAL_PRICEAMT(3%) - &
				SUBTOTAL_COSTAMT(3%), "#######") + &
			FORMAT$(SUBTOTAL_PRCQTR(1%) - &
				SUBTOTAL_CSTQTR(1%), "########") + &
			FORMAT$(SUBTOTAL_PRICEAMT(4%) - &
				SUBTOTAL_COSTAMT(4%), "#######") + &
			FORMAT$(SUBTOTAL_PRICEAMT(5%) - &
				SUBTOTAL_COSTAMT(5%), "#######") + &
			FORMAT$(SUBTOTAL_PRICEAMT(6%) - &
				SUBTOTAL_COSTAMT(6%), "#######") + &
			FORMAT$(SUBTOTAL_PRCQTR(2%) - &
				SUBTOTAL_CSTQTR(2%), "########") + &
			FORMAT$(SUBTOTAL_PRICEAMT(7%) - &
				SUBTOTAL_COSTAMT(7%), "#######") + &
			FORMAT$(SUBTOTAL_PRICEAMT(8%) - &
				SUBTOTAL_COSTAMT(8%), "#######") + &
			FORMAT$(SUBTOTAL_PRICEAMT(9%) - &
				SUBTOTAL_COSTAMT(9%), "#######") + &
			FORMAT$(SUBTOTAL_PRCQTR(3%) - &
				SUBTOTAL_CSTQTR(3%), "########") + &
			FORMAT$(SUBTOTAL_PRICEAMT(10%) - &
				SUBTOTAL_COSTAMT(10%), "#######") + &
			FORMAT$(SUBTOTAL_PRICEAMT(11%) - &
				SUBTOTAL_COSTAMT(11%), "#######") + &
			FORMAT$(SUBTOTAL_PRICEAMT(12%) - &
				SUBTOTAL_COSTAMT(12%), "#######") + &
			FORMAT$(SUBTOTAL_PRCQTR(4%) - &
				SUBTOTAL_CSTQTR(4%), "########") + &
			FORMAT$(SUBTOTAL_PRCYTD - &
				SUBTOTAL_CSTYTD, "########")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 1%)

	END IF

 SUBTOTAL_Var:
18420	SUBTOTAL_PRCYTD = 0.0
	SUBTOTAL_CSTYTD = 0.0

	FOR I% = 1% TO 4%
		SUBTOTAL_PRICEAMT(I%) = 0.0
		SUBTOTAL_COSTAMT(I%) = 0.0

		SUBTOTAL_PRICEAMT(I% + 4%) = 0.0
		SUBTOTAL_COSTAMT(I% + 4%) = 0.0

		SUBTOTAL_PRICEAMT(I% + 8%) = 0.0
		SUBTOTAL_COSTAMT(I% + 8%) = 0.0

		SUBTOTAL_PRCQTR(I%) = 0.0
		SUBTOTAL_CSTQTR(I%) = 0.0

	NEXT I%

	RETURN

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	UTL_REPORTX::STAT = -1%
	GOTO ExitProgram

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Handle untrapped errors
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
