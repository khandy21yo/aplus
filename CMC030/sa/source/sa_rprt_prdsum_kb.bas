1	%TITLE "Sales Summary by Product"
	%SBTTL "SA_RPRT_PRDSUM_KB"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 2000 BY
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
	!	The ^*Sales Summary by Product Report\* option
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
	!	$ BAS SA_SOURCE:SA_RPRT_PRDSUM_KB/LINE
	!	$ LINK/EXECUTABLE=SA_EXE: SA_RPRT_PRDSUM_KB, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE SA_RPRT_PRDSUM_KB.OBJ;*
	!
	! Author:
	!
	!	10/03/2000 - Kevin Handy
	!		Based on SA_RPRT_PRDSUM for KBJ
	!
	! Modification history:
	!
	!	10/19/2000 - Kevin Handy
	!		Display Quantity of product and not price.
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
	! CDD inclusions
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[IC.OPEN]IC_35HISTORY.HB"
	MAP (IC_35HISTORY)	IC_35HISTORY_CDD	IC_35HISTORY

	DECLARE			IC_35HISTORY_CDD	IC_HISTORY(10%)

	!
	! Array for variances
	!
	DECLARE REAL VARIANCE(17%)

	! Used in ProdTotal, created in SaleTotal
	!
	DECLARE REAL PRODQTYAMT(10%, 12%)
	DECLARE REAL PRODPRICEAMT(10%, 12%)
	DECLARE REAL PRODCOSTAMT(10%, 12%)

	!
	! Used in ExitTotal, created in ProdTotal
	!
	DECLARE REAL TOTALQTYAMT(10%, 12%)
	DECLARE REAL TOTALPRICEAMT(10%, 12%)
	DECLARE REAL TOTALCOSTAMT(10%, 12%)

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
	!	^*(01) Sort by (P,T,C,D,S)\*
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
	!	^*(02) From Item\*
	!	.p
	!	The ^*From Item\* field selects where
	!	to begin.
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
	!	to end with.
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
	!	^*(04) Wildcard Type\*
	!	.p
	!	The ^*Wildcard\* field
	!	designates items to be printed by entering a "wildcard"
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
	!
	! Index:
	!
	!--

	GT_ONLY$ = EDIT$(UTL_REPORTX::OPTDEF(5%), -1%)

	!++
	! Abstract:FLD06
	!	^*(06) Grand Total Only\*
	!	.p
	!	The ^*Grand Total Only\* field determines if it should
	!	show only the last page containing
	!	sales grand total for selected products.
	!
	! Index:
	!
	!--

	GROSS_PROFIT$ = EDIT$(UTL_REPORTX::OPTDEF(6%), -1%)

	!++
	! Abstract:FLD07
	!	^*(07) Print Gross Profit\*
	!	.p
	!	The ^*Print Gross Profit\* field determines if it should
	!	show the gross profit.
	!
	! Index:
	!
	!--

	PRINT_VARIANCE$ = EDIT$(UTL_REPORTX::OPTDEF(7%), -1%)

	!++
	! Abstract:FLD08
	!	^*(08) Print Variance\*
	!	.p
	!	The ^*Print Variance\* field determines if it should show the
	!	yearly unit, sales, and gross profit variances.
	!
	! Index:
	!
	!--

	YYYY$ = EDIT$(UTL_REPORTX::OPTDEF(8%), -1%)

	!++
	! Abstract:FLD09
	!	^*(09) Fiscal Year\*
	!	.p
	!	The ^*Fiscal Year\* field
	!	selects the fiscal year to begin with.
	!
	! Index:
	!
	!--

	YEARS% = VAL%(EDIT$(UTL_REPORTX::OPTDEF(9%), -1%))

	!++
	! Abstract:FLD10
	!	^*(10) Number of Years\*
	!	.p
	!	The ^*Number of Years\* field
	!	determines the number of the fiscal years. If
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
	FOR Y% = 1% TO YEARS%

		WHEN ERROR IN
			%INCLUDE "SOURCE:[IC.OPEN]IC_35HISTORY.OPN"
		USE
			CONTINUE ReportTitle IF ERR = 5%
			FILENAME$ = "IC_35HISTORY"
			CONTINUE HelpError
		END WHEN

		VALID_YEARS% = Y%
		IC_35HISTORY.CH%(Y%) = IC_35HISTORY.CH%
		IC_35HISTORY.CH% = 0.0
		YYYY$(Y%) = YYYY$
		YYYY$ = FORMAT$(VAL%(YYYY$) - 1%, "####")
	NEXT Y%

	%PAGE

 ReportTitle:
	YEARS% = VALID_YEARS%
	!
	! Title
	!
	SELECT SORTBY$
	CASE "P"
		K_NUM% = 0%
		TITLE$(1%) = "SALES SUMMARY BY PRODUCT NUMBER"

	CASE "T"
		K_NUM% = 1%
		TITLE$(1%) = "SALES SUMMARY BY PRODUCT TYPE"

	CASE "C"
		K_NUM% = 2%
		TITLE$(1%) = "SALES SUMMARY BY PRODUCT CATEGORY"

	CASE "D"
		K_NUM% = 3%
		TITLE$(1%) = "SALES SUMMARY BY PRODUCT DESCRIPTION"

	CASE "S"
		K_NUM% = 4%
		TITLE$(1%) = "SALES SUMMARY BY PRODUCT SECONDARY CODE"

	END SELECT

	TITLE$(2%) = "Sales Analysis System"
	TITLE$(3%) = ""

	!
	! Heading
	!

	TITLE$(4%) = ":::::::::: Product#       Description               " + &
		"               PT PCat SecCode"
	TITLE$(5%) = "         Per1   Per2   Per3    Qtr1   Per4   Per5   " + &
		"Per6    Qtr2   Per7   Per8   Per9    Qtr3  "          + &
		"Per10  Per11  Per12    Qtr4     YTD"

	TITLE$(6%) = "."

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

	PAGE%, PRINT_FLAG% = 0%

 GetNextRec:
	!
	! Print totals after each salesman
	!
	IF PRINT_FLAG%
	THEN
		GOSUB ProdTotal
	END IF

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

	GOTO GetNextRec &
		IF COMP_STRING(EDIT$(PD_PRODUCT::PROD_TYPE, -1%), &
		WLDCRD$) = 0% AND WLDCRD$ <> ""

	SELECT SORTBY$
	CASE "P"
		GOTO ExitTotal IF (PD_PRODUCT::PRODUCT_NUM > TO_ITEM$) &
			AND TO_ITEM$ <> ""

	CASE "T"
		GOTO ExitTotal IF (PD_PRODUCT::PROD_TYPE > TO_ITEM$) &
			AND TO_ITEM$ <> ""

	CASE "C"
		GOTO ExitTotal IF (PD_PRODUCT::CATEGORY > TO_ITEM$) &
			AND TO_ITEM$ <> ""

	CASE "D"
		GOTO ExitTotal IF (PD_PRODUCT::DESCRIPTION > TO_ITEM$) &
			AND TO_ITEM$ <> ""

	CASE "S"
		GOTO ExitTotal IF (PD_PRODUCT::SECONDARY_CODE > TO_ITEM$) &
			AND TO_ITEM$ <> ""

	END SELECT

17100	!
	! Get History record
	!
	FOR Y% = 1% TO YEARS%

		EOF%(Y%) = 0%
		WHEN ERROR IN
			GET #IC_35HISTORY.CH%(Y%), &
				KEY #3% EQ PD_PRODUCT::PRODUCT_NUM, &
				REGARDLESS
		USE
			IF ERR = 155%
			THEN
				EOF%(Y%) = 1%
				CONTINUE 17115
			END IF

			FILENAME$ = "IC_35HISTORY"
			CONTINUE HelpError
		END WHEN

		IC_HISTORY(Y%) = IC_35HISTORY

17115	NEXT Y%
	SY% = 1%
	GOTO TestKey

 GetHistRec:
17120	SY% = Y%

	WHEN ERROR IN
		GET #IC_35HISTORY.CH%(Y%), REGARDLESS
	USE
		IF ERR = 11%
		THEN
			EOF%(Y%) = 1%
			CONTINUE TestKey
		END IF
		FILENAME$ = "IC_35HISTORY"
		CONTINUE HelpError
	END WHEN

	IC_HISTORY(Y%) = IC_35HISTORY

 TestKey:
	TEST_EOF% = 1%
	FOR Y% = 1% TO YEARS%
		EOF%(Y%) = 1% &
			IF IC_HISTORY(Y%)::PRODUCT <> PD_PRODUCT::PRODUCT_NUM
		TEST_EOF% = TEST_EOF% * EOF%(Y%)
	NEXT Y%

	GOTO GetNextRec IF TEST_EOF% = 1%

	FOR Y% = SY% TO YEARS%
		IF EOF%(Y%) <> 1%
		THEN
			GOTO GetHistRec IF IC_HISTORY(Y%)::TRANSTYPE <> "SA"

			GOTO GetHistRec &
				IF COMP_STRING(EDIT$(IC_HISTORY(Y%)::LOCATION, &
				-1%), LOC_WLDCRD$) = 0% AND LOC_WLDCRD$ <> ""
		END IF

	NEXT Y%

	FOR Y% = 1% TO YEARS%

		IF EOF%(Y%) = 0%
		THEN
			FOR I% = 1% TO 12%

				PRODPRICEAMT(Y%, I%) = PRODPRICEAMT(Y%, I%) + &
					IC_HISTORY(Y%)::PRICEAMT(I%)
				PRODQTYAMT(Y%, I%) = PRODQTYAMT(Y%, I%) - &
					IC_HISTORY(Y%)::PQUANTITY(I%)
				PRODCOSTAMT(Y%, I%) = PRODCOSTAMT(Y%, I%) + &
					IC_HISTORY(Y%)::COSTAMT(I%)
				J% = INT((I% - 1%) / 3%) + 1%
				PRODQTYQTR(Y%, J%) = PRODQTYQTR(Y%, J%) - &
					IC_HISTORY(Y%)::PQUANTITY(I%)
				PRODPRCQTR(Y%, J%) = PRODPRCQTR(Y%, J%) + &
					IC_HISTORY(Y%)::PRICEAMT(I%)
				PRODCSTQTR(Y%, J%) = PRODCSTQTR(Y%, J%) + &
					IC_HISTORY(Y%)::COSTAMT(I%)
				PRODQTYYTD(Y%) = PRODQTYYTD(Y%) - &
					IC_HISTORY(Y%)::PQUANTITY(I%)
				PRODPRCYTD(Y%) = PRODPRCYTD(Y%) + &
					IC_HISTORY(Y%)::PRICEAMT(I%)
				PRODCSTYTD(Y%) = PRODCSTYTD(Y%) + &
					IC_HISTORY(Y%)::COSTAMT(I%)

			NEXT I%
		END IF

	NEXT Y%

	PRINT_FLAG% = -1%

17130	FOR Y% = YEARS% TO 1% STEP -1%

		IF EOF%(Y%) = 0%
		THEN
			SY% = Y%

			WHEN ERROR IN
				GET #IC_35HISTORY.CH%(Y%), REGARDLESS
			USE
				IF ERR = 11%
				THEN
					EOF%(Y%) = 1%
					CONTINUE 17135
				END IF
				FILENAME$ = "IC_35HISTORY"
				CONTINUE HelpError
			END WHEN

			IC_HISTORY(Y%) = IC_35HISTORY
		END IF

17135	NEXT Y%

	GOTO TestKey

	%PAGE

 ExitTotal:
	!
	! Print out totals
	!
	FOR I% = 4% TO 6%
		TITLE$(I%) = TITLE$(I% + 1%)
	NEXT I%

	TEXT$ = "     Report Grand Total"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, PAGE%)

	FOR Y% = 1% TO YEARS%

		TEXT$ = SPACE$(25%) + ":   "
		TEXT$ = "Year " + YYYY$(Y%) + TEXT$ + TEXT$ + TEXT$ + TEXT$

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		TEXT$ = "Qty   " + &
			FORMAT$(TOTALQTYAMT(Y%, 1%), "#######") + &
			SPACE$(7%) + &
			FORMAT$(TOTALQTYAMT(Y%, 3%), "#######") + &
			SPACE$(8%) + &
			FORMAT$(TOTALQTYAMT(Y%, 4%), "#######") + &
			SPACE$(7%) + &
			FORMAT$(TOTALQTYAMT(Y%, 6%), "#######") + &
			SPACE$(8%) + &
			FORMAT$(TOTALQTYAMT(Y%, 7%), "#######") + &
			SPACE$(7%) + &
			FORMAT$(TOTALQTYAMT(Y%, 9%), "#######") + &
			SPACE$(8%) + &
			FORMAT$(TOTALQTYAMT(Y%, 10%), "#######") + &
			SPACE$(7%) + &
			FORMAT$(TOTALQTYAMT(Y%, 12%), "#######") + &
			SPACE$(8%) + &
			FORMAT$(TOTALQTYYTD(Y%),  "########")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		TEXT$ = "      " + &
			SPACE$(7%) + &
			FORMAT$(TOTALQTYAMT(Y%, 2%), "#######") + &
			SPACE$(7%) + &
			FORMAT$(TOTALQTYQTR(Y%, 1%), "########") + &
			SPACE$(7%) + &
			FORMAT$(TOTALQTYAMT(Y%, 5%), "#######") + &
			SPACE$(7%) + &
			FORMAT$(TOTALQTYQTR(Y%, 2%), "########") + &
			SPACE$(7%) + &
			FORMAT$(TOTALQTYAMT(Y%, 8%), "#######") + &
			SPACE$(7%) + &
			FORMAT$(TOTALQTYQTR(Y%, 3%), "########") + &
			SPACE$(7%) + &
			FORMAT$(TOTALQTYAMT(Y%, 11%), "#######") + &
			SPACE$(7%) + &
			FORMAT$(TOTALQTYQTR(Y%, 4%), "########")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		IF GROSS_PROFIT$ = "Y"
		THEN
			TEXT$ = "Sales " + &
				FORMAT$(TOTALPRICEAMT(Y%, 1%), "#######") + &
				SPACE$(7%) + &
				FORMAT$(TOTALPRICEAMT(Y%, 3%), "#######") + &
				SPACE$(8%) + &
				FORMAT$(TOTALPRICEAMT(Y%, 4%), "#######") + &
				SPACE$(7%) + &
				FORMAT$(TOTALPRICEAMT(Y%, 6%), "#######") + &
				SPACE$(8%) + &
				FORMAT$(TOTALPRICEAMT(Y%, 7%), "#######") + &
				SPACE$(7%) + &
				FORMAT$(TOTALPRICEAMT(Y%, 9%), "#######") + &
				SPACE$(8%) + &
				FORMAT$(TOTALPRICEAMT(Y%, 10%), "#######") + &
				SPACE$(7%) + &
				FORMAT$(TOTALPRICEAMT(Y%, 12%), "#######") + &
				SPACE$(8%) + &
				FORMAT$(TOTALPRCYTD(Y%), "########")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

			TEXT$ = "      " + &
				SPACE$(7%) + &
				FORMAT$(TOTALPRICEAMT(Y%, 2%), "#######") + &
				SPACE$(7%) + &
				FORMAT$(TOTALPRCQTR(Y%, 1%), "########") + &
				SPACE$(7%) + &
				FORMAT$(TOTALPRICEAMT(Y%, 5%), "#######") + &
				SPACE$(7%) + &
				FORMAT$(TOTALPRCQTR(Y%, 2%), "########") + &
				SPACE$(7%) + &
				FORMAT$(TOTALPRICEAMT(Y%, 8%), "#######") + &
				SPACE$(7%) + &
				FORMAT$(TOTALPRCQTR(Y%, 3%), "########") + &
				SPACE$(7%) + &
				FORMAT$(TOTALPRICEAMT(Y%, 11%), "#######") + &
				SPACE$(7%) + &
				FORMAT$(TOTALPRCQTR(Y%, 4%), "########")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

			TEXT$ = "GProf " + &
				FORMAT$(TOTALPRICEAMT(Y%, 1%) - TOTALCOSTAMT(Y%, 1%), "#######") + &
				SPACE$(7%) + &
				FORMAT$(TOTALPRICEAMT(Y%, 3%) - TOTALCOSTAMT(Y%, 3%), "#######") + &
				SPACE$(8%) + &
				FORMAT$(TOTALPRICEAMT(Y%, 4%) - TOTALCOSTAMT(Y%, 4%), "#######") + &
				SPACE$(7%) + &
				FORMAT$(TOTALPRICEAMT(Y%, 6%) - TOTALCOSTAMT(Y%, 6%), "#######") + &
				SPACE$(8%) + &
				FORMAT$(TOTALPRICEAMT(Y%, 7%) - TOTALCOSTAMT(Y%, 7%), "#######") + &
				SPACE$(7%) + &
				FORMAT$(TOTALPRICEAMT(Y%, 9%) - TOTALCOSTAMT(Y%, 9%), "#######") + &
				SPACE$(8%) + &
				FORMAT$(TOTALPRICEAMT(Y%, 10%) - TOTALCOSTAMT(Y%, 10%), "#######") + &
				SPACE$(7%) + &
				FORMAT$(TOTALPRICEAMT(Y%, 12%) - TOTALCOSTAMT(Y%, 12%), "#######") + &
				SPACE$(8%) + &
				FORMAT$(TOTALPRCYTD(Y%) - TOTALCSTYTD(Y%),  "########")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

			TEXT$ = "      " + &
				SPACE$(7%) + &
				FORMAT$(TOTALPRICEAMT(Y%, 2%) - TOTALCOSTAMT(Y%, 2%), "#######") + &
				SPACE$(7%) + &
				FORMAT$(TOTALPRCQTR(Y%, 1%) - TOTALCSTQTR(Y%, 1%), "########") + &
				SPACE$(7%) + &
				FORMAT$(TOTALPRICEAMT(Y%, 5%) - TOTALCOSTAMT(Y%, 5%), "#######") + &
				SPACE$(7%) + &
				FORMAT$(TOTALPRCQTR(Y%, 2%) - TOTALCSTQTR(Y%, 2%), "########") + &
				SPACE$(7%) + &
				FORMAT$(TOTALPRICEAMT(Y%, 8%) - TOTALCOSTAMT(Y%, 8%), "#######") + &
				SPACE$(7%) + &
				FORMAT$(TOTALPRCQTR(Y%, 3%) - TOTALCSTQTR(Y%, 3%), "########") + &
				SPACE$(7%) + &
				FORMAT$(TOTALPRICEAMT(Y%, 11%) - TOTALCOSTAMT(Y%, 11%), "#######") + &
				SPACE$(7%) + &
				FORMAT$(TOTALPRCQTR(Y%, 4%) - TOTALCSTQTR(Y%, 4%), "########")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		END IF

 TotalVar:
		IF PRINT_VARIANCE$ = "Y" AND Y% <> YEARS%
		THEN
			FOR I% = 1% TO 12%
				IF TOTALPRICEAMT(Y% + 1%, I%) = 0.0
				THEN
					VARIANCE(I%) = 100.00
					VARIANCE(I%) = 0.0 &
						IF TOTALPRICEAMT(Y%, I%) = 0.0
				ELSE
					VARIANCE(I%) = (TOTALPRICEAMT(Y%, I%) - &
						TOTALPRICEAMT(Y% + 1%, I%)) / &
						TOTALPRICEAMT(Y% + 1%, I%) * &
						100.0
				END IF
			NEXT I%

			FOR I% = 13% TO 16%
				IF TOTALPRCQTR(Y% + 1%, I% - 12%) = 0.0
				THEN
					VARIANCE(I%) = 100.0
					VARIANCE(I%) = 0.0 &
						IF TOTALPRCQTR(Y%, I% - 12%) = 0.0
				ELSE
					VARIANCE(I%) = (TOTALPRCQTR(Y%, I% - 12%) - &
						TOTALPRCQTR(Y% + 1%, I% - 12%)) / &
						TOTALPRCQTR(Y% + 1%, I% - 12%) * &
						100.0
				END IF
			NEXT I%

			IF TOTALPRCYTD(Y% + 1%) = 0.0
			THEN
				VARIANCE(17%) = 100.00
				VARIANCE(17%) = 0.0 IF TOTALPRCYTD(Y%) = 0.0
			ELSE
				VARIANCE(17%) = (TOTALPRCYTD(Y%) - &
					TOTALPRCYTD(Y% + 1%)) / &
					TOTALPRCYTD(Y% + 1%) * 100.0
			END IF

			TEXT$ = "SVar  " + &
				FORMAT$(VARIANCE(1%), "######%") + &
				FORMAT$(VARIANCE(2%), "######%") + &
				FORMAT$(VARIANCE(3%), "######%") + &
				FORMAT$(VARIANCE(13%), "#######%") + &
				FORMAT$(VARIANCE(4%), "######%") + &
				FORMAT$(VARIANCE(5%), "######%") + &
				FORMAT$(VARIANCE(6%), "######%") + &
				FORMAT$(VARIANCE(14%), "#######%") + &
				FORMAT$(VARIANCE(7%), "######%") + &
				FORMAT$(VARIANCE(8%), "######%") + &
				FORMAT$(VARIANCE(9%), "######%") + &
				FORMAT$(VARIANCE(15%), "#######%") + &
				FORMAT$(VARIANCE(10%), "######%") + &
				FORMAT$(VARIANCE(11%), "######%") + &
				FORMAT$(VARIANCE(12%), "######%") + &
				FORMAT$(VARIANCE(16%), "#######%") + &
				FORMAT$(VARIANCE(17%),  "#######%")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 1%)

			FOR I% = 1% TO 12%
				IF TOTALPRICEAMT(Y% + 1%, I%) - &
					TOTALCOSTAMT(Y% + 1%, I%) = 0.0
				THEN
					VARIANCE(I%) = 100.00
					VARIANCE(I%) = 0.0 &
						IF TOTALPRICEAMT(Y%, I%) - &
						TOTALCOSTAMT(Y%, I%) = 0.0
				ELSE
					VARIANCE(I%) = (TOTALPRICEAMT(Y%, I%) - &
						TOTALCOSTAMT(Y%, I%) - &
						(TOTALPRICEAMT(Y% + 1%, I%) - &
						TOTALCOSTAMT(Y% + 1%, I%))) / &
						(TOTALPRICEAMT(Y% + 1%, I%) - &
						TOTALCOSTAMT(Y% + 1%, I%)) * &
						100.0
				END IF
			NEXT I%

			FOR I% = 13% TO 16%
				IF TOTALPRCQTR(Y% + 1%, I% - 12%) - &
					TOTALCSTQTR(Y% + 1%, I% - 12%) = 0.0
				THEN
					VARIANCE(I%) = 100.0
					VARIANCE(I%) = 0.0 &
						IF TOTALPRCQTR(Y%, I% - 12%) - &
						TOTALCSTQTR(Y%, I% - 12%) = 0.0
				ELSE
					VARIANCE(I%) = (TOTALPRCQTR(Y%, I% - 12%) - &
						TOTALCSTQTR(Y%, I% - 12%) - &
						(TOTALPRCQTR(Y% + 1%, I% - 12%) - &
						TOTALCSTQTR(Y% + 1%, I% - 12%))) / &
						(TOTALPRCQTR(Y% + 1%, I% - 12%) - &
						TOTALCSTQTR(Y% + 1%, I% - 12%)) * &
						100.0
				END IF
			NEXT I%

			IF TOTALPRCYTD(Y% + 1%) - TOTALCSTYTD(Y% + 1%) = 0.0
			THEN
				VARIANCE(17%) = 100.00
				VARIANCE(17%) = 0.0 &
					IF TOTALPRCYTD(Y%) - TOTALCSTYTD(Y%) = 0.0
			ELSE
				VARIANCE(17%) = (TOTALPRCYTD(Y%) - &
					TOTALCSTYTD(Y%) - &
					(TOTALPRCYTD(Y% + 1%) - &
					TOTALCSTYTD(Y% + 1%))) / &
					(TOTALPRCYTD(Y% + 1%) - &
					TOTALCSTYTD(Y% + 1%)) * &
					100.0
			END IF

			TEXT$ = "GPVar " + &
				FORMAT$(VARIANCE(1%), "######%") + &
				FORMAT$(VARIANCE(2%), "######%") + &
				FORMAT$(VARIANCE(3%), "######%") + &
				FORMAT$(VARIANCE(13%), "#######%") + &
				FORMAT$(VARIANCE(4%), "######%") + &
				FORMAT$(VARIANCE(5%), "######%") + &
				FORMAT$(VARIANCE(6%), "######%") + &
				FORMAT$(VARIANCE(14%), "#######%") + &
				FORMAT$(VARIANCE(7%), "######%") + &
				FORMAT$(VARIANCE(8%), "######%") + &
				FORMAT$(VARIANCE(9%), "######%") + &
				FORMAT$(VARIANCE(15%), "#######%") + &
				FORMAT$(VARIANCE(10%), "######%") + &
				FORMAT$(VARIANCE(11%), "######%") + &
				FORMAT$(VARIANCE(12%), "######%") + &
				FORMAT$(VARIANCE(16%), "#######%") + &
				FORMAT$(VARIANCE(17%),  "#######%")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 1%) &
				IF GROSS_PROFIT$ = "Y"

		END IF

	NEXT Y%

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

 ProdTotal:

	GOTO AddGrandTotal IF GT_ONLY$ = "Y"

	PAGE% = 999%

	TEXT$ = ":::::::::: " + PD_PRODUCT::PRODUCT_NUM + " " + &
		PD_PRODUCT::DESCRIPTION + " " + &
		PD_PRODUCT::PROD_TYPE  + " " + &
		PD_PRODUCT::CATEGORY + " " + &
		PD_PRODUCT::SECONDARY_CODE

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3%)

	FOR Y% = 1% TO YEARS%

		GOTO ProdVar IF Y% <> 1% AND Y% = YEARS% AND PRINT_VARIANCE$ = "Y"

		TEXT$ = SPACE$(25%) + ":   "
		TEXT$ = "Year " + YYYY$(Y%) + TEXT$ + TEXT$ + TEXT$ + TEXT$

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		TEXT$ = "Qty   " + &
			FORMAT$(PRODQTYAMT(Y%, 1%), "#######") + &
			FORMAT$(PRODQTYAMT(Y%, 2%), "#######") + &
			FORMAT$(PRODQTYAMT(Y%, 3%), "#######") + &
			FORMAT$(PRODQTYQTR(Y%, 1%), "########") + &
			FORMAT$(PRODQTYAMT(Y%, 4%), "#######") + &
			FORMAT$(PRODQTYAMT(Y%, 5%), "#######") + &
			FORMAT$(PRODQTYAMT(Y%, 6%), "#######") + &
			FORMAT$(PRODQTYQTR(Y%, 2%), "########") + &
			FORMAT$(PRODQTYAMT(Y%, 7%), "#######") + &
			FORMAT$(PRODQTYAMT(Y%, 8%), "#######") + &
			FORMAT$(PRODQTYAMT(Y%, 9%), "#######") + &
			FORMAT$(PRODQTYQTR(Y%, 3%), "########") + &
			FORMAT$(PRODQTYAMT(Y%, 10%), "#######") + &
			FORMAT$(PRODQTYAMT(Y%, 11%), "#######") + &
			FORMAT$(PRODQTYAMT(Y%, 12%), "#######") + &
			FORMAT$(PRODQTYQTR(Y%, 4%), "########") + &
			FORMAT$(PRODQTYYTD(Y%),  "########")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		IF GROSS_PROFIT$ = "Y"
		THEN
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
				FORMAT$(PRODPRCYTD(Y%),  "########")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

			TEXT$ = "GProf " + &
				FORMAT$(PRODPRICEAMT(Y%, 1%) - PRODCOSTAMT(Y%, 1%), "#######") + &
				FORMAT$(PRODPRICEAMT(Y%, 2%) - PRODCOSTAMT(Y%, 2%), "#######") + &
				FORMAT$(PRODPRICEAMT(Y%, 3%) - PRODCOSTAMT(Y%, 3%), "#######") + &
				FORMAT$(PRODPRCQTR(Y%, 1%) - PRODCSTQTR(Y%, 1%), "########") + &
				FORMAT$(PRODPRICEAMT(Y%, 4%) - PRODCOSTAMT(Y%, 4%), "#######") + &
				FORMAT$(PRODPRICEAMT(Y%, 5%) - PRODCOSTAMT(Y%, 5%), "#######") + &
				FORMAT$(PRODPRICEAMT(Y%, 6%) - PRODCOSTAMT(Y%, 6%), "#######") + &
				FORMAT$(PRODPRCQTR(Y%, 2%) - PRODCSTQTR(Y%, 2%), "########") + &
				FORMAT$(PRODPRICEAMT(Y%, 7%) - PRODCOSTAMT(Y%, 7%), "#######") + &
				FORMAT$(PRODPRICEAMT(Y%, 8%) - PRODCOSTAMT(Y%, 8%), "#######") + &
				FORMAT$(PRODPRICEAMT(Y%, 9%) - PRODCOSTAMT(Y%, 9%), "#######") + &
				FORMAT$(PRODPRCQTR(Y%, 3%) - PRODCSTQTR(Y%, 3%), "########") + &
				FORMAT$(PRODPRICEAMT(Y%, 10%) - PRODCOSTAMT(Y%, 10%), "#######") + &
				FORMAT$(PRODPRICEAMT(Y%, 11%) - PRODCOSTAMT(Y%, 11%), "#######") + &
				FORMAT$(PRODPRICEAMT(Y%, 12%) - PRODCOSTAMT(Y%, 12%), "#######") + &
				FORMAT$(PRODPRCQTR(Y%, 4%) - PRODCSTQTR(Y%, 4%), "########") + &
				FORMAT$(PRODPRCYTD(Y%) - PRODCSTYTD(Y%), "########")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 1%)

		END IF

 ProdVar:
		IF PRINT_VARIANCE$ = "Y" AND Y% <> YEARS%
		THEN
			FOR I% = 1% TO 12%
				IF PRODPRICEAMT(Y% + 1%, I%) = 0.0
				THEN
					VARIANCE(I%) = 100.00
					VARIANCE(I%) = 0.0 &
						IF PRODPRICEAMT(Y%, I%) = 0.0
				ELSE
					VARIANCE(I%) = (PRODPRICEAMT(Y%, I%) - &
						PRODPRICEAMT(Y% + 1%, I%)) / &
						PRODPRICEAMT(Y% + 1%, I%) * &
						100.0
				END IF
			NEXT I%

			FOR I% = 13% TO 16%
				IF PRODPRCQTR(Y% + 1%, I% - 12%) = 0.0
				THEN
					VARIANCE(I%) = 100.0
					VARIANCE(I%) = 0.0 &
						IF PRODPRCQTR(Y%, I% - 12%) = 0.0
				ELSE
					VARIANCE(I%) = (PRODPRCQTR(Y%, I% - 12%) - &
						PRODPRCQTR(Y% + 1%, I% - 12%)) / &
						PRODPRCQTR(Y% + 1%, I% - 12%) * &
						100.0
				END IF
			NEXT I%

			IF PRODPRCYTD(Y% + 1%) = 0.0
			THEN
				VARIANCE(17%) = 100.00
				VARIANCE(17%) = 0.0 IF PRODPRCYTD(Y%) = 0.0
			ELSE
				VARIANCE(17%) = (PRODPRCYTD(Y%) - &
					PRODPRCYTD(Y% + 1%)) / &
					PRODPRCYTD(Y% + 1%) * 100.0
			END IF

			TEXT$ = "SVar  " + &
				FORMAT$(VARIANCE(1%), "######%") + &
				FORMAT$(VARIANCE(2%), "######%") + &
				FORMAT$(VARIANCE(3%), "######%") + &
				FORMAT$(VARIANCE(13%), "#######%") + &
				FORMAT$(VARIANCE(4%), "######%") + &
				FORMAT$(VARIANCE(5%), "######%") + &
				FORMAT$(VARIANCE(6%), "######%") + &
				FORMAT$(VARIANCE(14%), "#######%") + &
				FORMAT$(VARIANCE(7%), "######%") + &
				FORMAT$(VARIANCE(8%), "######%") + &
				FORMAT$(VARIANCE(9%), "######%") + &
				FORMAT$(VARIANCE(15%), "#######%") + &
				FORMAT$(VARIANCE(10%), "######%") + &
				FORMAT$(VARIANCE(11%), "######%") + &
				FORMAT$(VARIANCE(12%), "######%") + &
				FORMAT$(VARIANCE(16%), "#######%") + &
				FORMAT$(VARIANCE(17%),  "#######%")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 1%)

			FOR I% = 1% TO 12%
				IF PRODPRICEAMT(Y% + 1%, I%) - &
					PRODCOSTAMT(Y% + 1%, I%) = 0.0
				THEN
					VARIANCE(I%) = 100.00
					VARIANCE(I%) = 0.0 &
						IF PRODPRICEAMT(Y%, I%) - &
						PRODCOSTAMT(Y%, I%) = 0.0
				ELSE
					VARIANCE(I%) = (PRODPRICEAMT(Y%, I%) - &
						PRODCOSTAMT(Y%, I%) - &
						(PRODPRICEAMT(Y% + 1%, I%) - &
						PRODCOSTAMT(Y% + 1%, I%))) / &
						(PRODPRICEAMT(Y% + 1%, I%) - &
						PRODCOSTAMT(Y% + 1%, I%)) * 100.0
				END IF
			NEXT I%

			FOR I% = 13% TO 16%
				IF PRODPRCQTR(Y% + 1%, I% - 12%) - &
					PRODCSTQTR(Y% + 1%, I% - 12%) = 0.0
				THEN
					VARIANCE(I%) = 100.0
					VARIANCE(I%) = 0.0 &
						IF PRODPRCQTR(Y%, I% - 12%) - &
						PRODCSTQTR(Y%, I% - 12%) = 0.0
				ELSE
					VARIANCE(I%) = (PRODPRCQTR(Y%, I% - 12%) - PRODCSTQTR(Y%, I% - 12%) - &
						(PRODPRCQTR(Y% + 1%, I% - 12%) - PRODCSTQTR(Y%+1%, I% - 12%))) / &
						(PRODPRCQTR(Y% + 1%, I% - 12%) - PRODCSTQTR(Y%+1%,I% - 12%)) * 100.
				END IF
			NEXT I%

			IF PRODPRCYTD(Y% + 1%) - PRODCSTYTD(Y% + 1%) = 0.0
			THEN
				VARIANCE(17%) = 100.00
				VARIANCE(17%) = 0.0 &
					IF PRODPRCYTD(Y%) - PRODCSTYTD(Y%) = 0.0
			ELSE
				VARIANCE(17%) = (PRODPRCYTD(Y%) - &
					PRODCSTYTD(Y%) - &
					(PRODPRCYTD(Y% + 1%) - &
					PRODCSTYTD(Y% + 1%))) / &
					(PRODPRCYTD(Y% + 1%) - &
					PRODCSTYTD(Y% + 1%)) * 100.0
			END IF

			TEXT$ = "GPVar " + &
				FORMAT$(VARIANCE(1%), "######%") + &
				FORMAT$(VARIANCE(2%), "######%") + &
				FORMAT$(VARIANCE(3%), "######%") + &
				FORMAT$(VARIANCE(13%), "#######%") + &
				FORMAT$(VARIANCE(4%), "######%") + &
				FORMAT$(VARIANCE(5%), "######%") + &
				FORMAT$(VARIANCE(6%), "######%") + &
				FORMAT$(VARIANCE(14%), "#######%") + &
				FORMAT$(VARIANCE(7%), "######%") + &
				FORMAT$(VARIANCE(8%), "######%") + &
				FORMAT$(VARIANCE(9%), "######%") + &
				FORMAT$(VARIANCE(15%), "#######%") + &
				FORMAT$(VARIANCE(10%), "######%") + &
				FORMAT$(VARIANCE(11%), "######%") + &
				FORMAT$(VARIANCE(12%), "######%") + &
				FORMAT$(VARIANCE(16%), "#######%") + &
				FORMAT$(VARIANCE(17%),  "#######%")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 1%) &
				IF GROSS_PROFIT$ = "Y"

		END IF
	NEXT Y%

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%) &

 AddGrandTotal:
	FOR Y% = 1% TO YEARS%
		FOR I% = 1% TO 12%
			TOTALQTYAMT(Y%, I%) = TOTALQTYAMT(Y%, I%) + &
				PRODQTYAMT(Y%, I%)
			TOTALPRICEAMT(Y%, I%) = TOTALPRICEAMT(Y%, I%) + &
				PRODPRICEAMT(Y%, I%)
			TOTALCOSTAMT(Y%, I%) = TOTALCOSTAMT(Y%, I%) + &
				PRODCOSTAMT(Y%, I%)
		NEXT I%
	NEXT Y%

	FOR Y% = 1% TO YEARS%

		TOTALQTYYTD(Y%)  = TOTALQTYYTD(Y%)  + PRODQTYYTD(Y%)
		TOTALPRCYTD(Y%)  = TOTALPRCYTD(Y%)  + PRODPRCYTD(Y%)
		TOTALCSTYTD(Y%)  = TOTALCSTYTD(Y%)  + PRODCSTYTD(Y%)

		PRODQTYYTD(Y%)  = 0.0
		PRODPRCYTD(Y%)  = 0.0
		PRODCSTYTD(Y%)  = 0.0

		FOR I% = 1% TO 4%
			TOTALPRCQTR(Y%, I%) = TOTALPRCQTR(Y%, I%) + &
				PRODPRCQTR(Y%, I%)
			TOTALCSTQTR(Y%, I%) = TOTALCSTQTR(Y%, I%) + &
				PRODCSTQTR(Y%, I%)

			PRODQTYAMT(Y%, I%)       = 0.0
			PRODPRICEAMT(Y%, I%)       = 0.0
			PRODCOSTAMT(Y%, I%)        = 0.0

			PRODQTYAMT(Y%, I% + 4%)  = 0.0
			PRODPRICEAMT(Y%, I% + 4%)  = 0.0
			PRODCOSTAMT(Y%, I% + 4%)   = 0.0

			PRODQTYAMT(Y%, I% + 8%)  = 0.0
			PRODPRICEAMT(Y%, I% + 8%)  = 0.0
			PRODCOSTAMT(Y%, I% + 8%)   = 0.0

			PRODQTYQTR(Y%, I%)         = 0.0
			PRODPRCQTR(Y%, I%)         = 0.0
			PRODCSTQTR(Y%, I%)         = 0.0

		NEXT I%

	NEXT Y%

	PRINT_FLAG% = 0%

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

32767	!******************************************************************
	! End of report SA_RPRT_PRDSUM_KB
	!******************************************************************
	END
