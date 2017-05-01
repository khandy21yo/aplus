1	%TITLE "Product History Report"
	%SBTTL "SA_RPRT_PRODHIST"
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
	! ID:SA0021
	!
	! Abstract:HELP
	!	.p
	!	The ^*Product History Report\*
	!	prints a versitile report listing the following information:
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
	!	Transaction Period
	!	.le
	!	Transaction Quantity
	!	.le
	!	Dollar Volume
	!	.els
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS SA_SOURCE:SA_RPRT_PRODHIST/LINE
	!	$ LINK/EXECUTABLE=SA_EXE: SA_RPRT_PRODHIST, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE SA_RPRT_PRODHIST.OBJ;*
	!
	! Author:
	!
	!	05/01/92 - Dan Perkins
	!
	! Modification history:
	!
	!	06/12/92 - Kevin Handy
	!		Clean up (check)
	!
	!	12/06/94 - Kevin Handy
	!		Added "Total Only" option.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/20/97 - Kevin Handy
	!		Don't allocate channel for report
	!
	!	08/26/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excess %PAGE's
	!
	!	11/05/2000 - Kevin Handy
	!		Use A"x"B
	!		Use WHEN ERROR IN
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
	EXTERNAL LONG	FUNCTION COMP_STRING
	EXTERNAL LONG   FUNCTION SA_EXAM_SALESMAN
	EXTERNAL LONG	FUNCTION AR_EXAM_CUSTOM

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

	%INCLUDE "SOURCE:[SA.OPEN]SA_SALESMAN.HB"
	DECLARE			SA_SALESMAN_CDD		SA_SALESMAN_EXAM

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	DECLARE			SB_SUBACCOUNT_CDD	SB_SUBACCOUNT_EXAM

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	DECLARE			AR_35CUSTOM_CDD		AR_35CUSTOM_EXAM

	!
	! Used in PrintLine, created at 17120
	!
	DECLARE REAL QUANTITY(10%, 12%)
	DECLARE REAL PRICEAMT(10%, 12%)

	DECLARE REAL QTYQTR(10%, 4%)
	DECLARE REAL PRCQTR(10%, 4%)

	DECLARE REAL QTYYTD(10%)
	DECLARE REAL PRCYTD(10%)

	!
	! Used in SaleTotal, created in PrintLine
	!
	DECLARE REAL SALEQUANTITY(10%, 12%)
	DECLARE REAL SALEPRICEAMT(10%, 12%)

	DECLARE REAL SALEQTYQTR(10%, 4%)
	DECLARE REAL SALEPRCQTR(10%, 4%)

	DECLARE REAL SALEQTYYTD(10%)
	DECLARE REAL SALEPRCYTD(10%)

	!
	! Used in ProdTotal, created in SaleTotal
	!
	DECLARE REAL PRODQUANTITY(10%, 12%)
	DECLARE REAL PRODPRICEAMT(10%, 12%)

	DECLARE REAL PRODQTYQTR(10%, 4%)
	DECLARE REAL PRODPRCQTR(10%, 4%)

	DECLARE REAL PRODQTYYTD(10%)
	DECLARE REAL PRODPRCYTD(10%)

	!
	! Used in ExitTotal, created in ProdTotal
	!
	DECLARE REAL TOTALQUANTITY(10%, 12%)
	DECLARE REAL TOTALPRICEAMT(10%, 12%)

	DECLARE REAL TOTALQTYQTR(10%, 4%)
	DECLARE REAL TOTALPRCQTR(10%, 4%)

	DECLARE REAL TOTALQTYYTD(10%)
	DECLARE REAL TOTALPRCYTD(10%)

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
	!	with which to end with.
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

	TRANS_WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(5%), -1%)

	!++
	! Abstract:FLD06
	!	^*(06) Transaction Wildcard\*
	!	.p
	!	The ^*Transaction Wildcard\* field selects
	!	designated transactions to be printed by entering a "wildcard"
	!	using the Wildcarding Technique.  All transactions will be included
	!	if this field is left blank
	!
	! Index:
	!
	!--

	SUBTITLE$ = EDIT$(UTL_REPORTX::OPTDEF(6%), 132%)

	!++
	! Abstract:FLD07
	!	^*(07) Subtitle\*
	!	.p
	!	The ^*Subtitle\* field
	!	enters a subtitle to print on the report.
	!
	! Index:
	!
	!--

	TOTAL_ONLY$ = LEFT$(UTL_REPORTX::OPTDEF(7%), 1%)


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
	!	prints this number of the fiscal years. If
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
		TITLE$(1%) = "PRODUCT HISTORY BY PRODUCT NUMBER"

	CASE "T"
		K_NUM% = 1%
		TITLE$(1%) = "PRODUCT HISTORY BY PRODUCT TYPE"

	CASE "C"
		K_NUM% = 2%
		TITLE$(1%) = "PRODUCT HISTORY BY PRODUCT CATEGORY"

	CASE "D"
		K_NUM% = 3%
		TITLE$(1%) = "PRODUCT HISTORY BY PRODUCT DESCRIPTION"

	CASE "S"
		K_NUM% = 4%
		TITLE$(1%) = "PRODUCT HISTORY BY PRODUCT SECONDARY CODE"

	END SELECT

	IF SUBTITLE$ <> ""
	THEN
		TITLE$(3%) = SUBTITLE$
		TITLE$(4%) = "Sales Analysis System"
		TITLE$(5%) = ""

		!
		! Heading
		!
		TITLE$(6%) = "      Salesman#     Salesman                 " + &
			"          Cust#          CustName"

		TITLE$(7%) = "         Per1   Per2   Per3    Qtr1   Per4   Per5   " + &
			"Per6    Qtr2   Per7   Per8   Per9    Qtr3  "          + &
			"Per10  Per11  Per12    Qtr4     YTD"

		TITLE$(8%) = "."
	ELSE
		TITLE$(3%) = "Sales Analysis System"
		TITLE$(4%) = ""

		!
		! Heading
		!
		TITLE$(5%) = "      Salesman#     Salesman                 " + &
			"          Cust#          CustName"

		TITLE$(6%) = "         Per1   Per2   Per3    Qtr1   Per4   Per5   " + &
			"Per6    Qtr2   Per7   Per8   Per9    Qtr3  "          + &
			"Per10  Per11  Per12    Qtr4     YTD"

		TITLE$(7%) = "."
	END IF

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

 GetNextRec:
	!
	! Print totals after each salesman
	!
	IF PRINT_FLAG%
	THEN
		GOSUB PrintLine
		GOSUB SaleTotal
		GOSUB ProdTotal
	END IF

	NEW_PRODUCT% = -1%

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
			IF COMP_STRING(EDIT$(PD_PRODUCT::PRODUCT_NUM, -1%), &
			WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "T"
		GOTO ExitTotal IF (PD_PRODUCT::PROD_TYPE > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(PD_PRODUCT::PROD_TYPE, -1%), &
			WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "C"
		GOTO ExitTotal IF (PD_PRODUCT::CATEGORY > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(PD_PRODUCT::CATEGORY, -1%), &
			WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "D"
		GOTO ExitTotal IF (PD_PRODUCT::DESCRIPTION > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(PD_PRODUCT::DESCRIPTION, -1%), &
			WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "S"
		GOTO ExitTotal IF (PD_PRODUCT::SECONDARY_CODE > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(PD_PRODUCT::SECONDARY_CODE, -1%), &
			WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	END SELECT

	TEST_LINE$ = ""

17100	!
	! Get History record
	!
	FOR Y% = 1% TO YEARS%

		EOF%(Y%) = 0%
		NO_TEST%(Y%) = 0%
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
		IF EOF%(Y%) <> 1% AND NO_TEST%(Y%) <> 1%
		THEN
			GOTO GetHistRec &
				IF COMP_STRING(EDIT$(IC_HISTORY(Y%)::TRANSTYPE, -1%), &
				TRANS_WLDCRD$) = 0% &
				AND TRANS_WLDCRD$ <> ""

			GOTO GetHistRec &
				IF COMP_STRING(EDIT$(IC_HISTORY(Y%)::LOCATION, &
				-1%), LOC_WLDCRD$) = 0% &
				AND LOC_WLDCRD$ <> ""
		ELSE
			NO_TEST%(Y%) = 1%
		END IF

	NEXT Y%

	NO_TEST% = 1%
	SALE_TEST% = -1%
	FOR Y% = 1% TO YEARS%
		IF NO_TEST%(Y%) = 0%
		THEN
			IF TEST_LINE$ <> IC_HISTORY(Y%)::PRODUCT + &
				IC_HISTORY(Y%)::SUBACCT  + &
				IC_HISTORY(Y%)::CROSSREF AND &
				TEST_LINE$ <> ""
			THEN
				SALE_TEST% = SALE_TEST% AND &
					(IC_HISTORY(Y%)::SUBACCT <> &
					LAST_SUBACCT$)
				NO_TEST%(Y%) = 1%
			ELSE
				SALE_TEST% = 0%
			END IF
		ELSE
			SALE_TEST% = SALE_TEST% AND -1%
		END IF

		NO_TEST% = NO_TEST% * NO_TEST%(Y%)

	NEXT Y%

	IF NO_TEST%
	THEN
		GOSUB PrintLine

		IF SALE_TEST%
		THEN
			GOSUB SaleTotal
		END IF
	END IF

	TEST_LINE$ = PD_PRODUCT::PRODUCT_NUM + CHR$(255%)

	!
	! Find minimum
	!
	FOR Y% = 1% TO YEARS%

		IF EOF%(Y%) = 0% AND &
			TEST_LINE$ > IC_HISTORY(Y%)::PRODUCT + &
				IC_HISTORY(Y%)::SUBACCT + &
				IC_HISTORY(Y%)::CROSSREF
		THEN
			TEST_LINE$ = IC_HISTORY(Y%)::PRODUCT + &
				IC_HISTORY(Y%)::SUBACCT + &
				IC_HISTORY(Y%)::CROSSREF

			LAST_SUBACCT$ = IC_HISTORY(Y%)::SUBACCT
			LAST_CROSSREF$ = IC_HISTORY(Y%)::CROSSREF

		END IF
	NEXT Y%

	FOR Y% = 1% TO YEARS%

		IF TEST_LINE$ = IC_HISTORY(Y%)::PRODUCT + &
			IC_HISTORY(Y%)::SUBACCT + &
			IC_HISTORY(Y%)::CROSSREF
		THEN
			FOR I% = 1% TO 12%

				QUANTITY(Y%, I%) = QUANTITY(Y%, I%) + &
					IC_HISTORY(Y%)::PQUANTITY(I%)
				PRICEAMT(Y%, I%) = PRICEAMT(Y%, I%) + &
					IC_HISTORY(Y%)::PRICEAMT(I%)
				J% = INT((I% - 1%) / 3%) + 1%
				QTYQTR(Y%, J%) = QTYQTR(Y%, J%) + &
					IC_HISTORY(Y%)::PQUANTITY(I%)
				PRCQTR(Y%, J%) = PRCQTR(Y%, J%) + &
					IC_HISTORY(Y%)::PRICEAMT(I%)
				QTYYTD(Y%) = QTYYTD(Y%) + &
					IC_HISTORY(Y%)::PQUANTITY(I%)
				PRCYTD(Y%) = PRCYTD(Y%) + &
					IC_HISTORY(Y%)::PRICEAMT(I%)

			NEXT I%
		ELSE
			NO_TEST%(Y%) = 1%
		END IF

	NEXT Y%

	PRINT_FLAG% = -1%

	IF NEW_PRODUCT%
	THEN
		TITLE$(2%) = TRM$(PD_PRODUCT::PRODUCT_NUM) + " " + &
			TRM$(PD_PRODUCT::DESCRIPTION)

		TEXT$ = ":::::::::: " + PD_PRODUCT::PRODUCT_NUM + " " + &
			TRM$(PD_PRODUCT::DESCRIPTION) + " " + &
			PD_PRODUCT::PROD_TYPE + " " + &
			PD_PRODUCT::CATEGORY + " " + &
			TRM$(PD_PRODUCT::SECONDARY_CODE)

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, PAGE%)

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

		NEW_PRODUCT%, PAGE% = 0%
	END IF

17130	FOR Y% = YEARS% TO 1% STEP -1%

		IF NO_TEST%(Y%) = 0%
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
	TITLE$(2%) = "Sales Analysis System"
	TITLE$(3%) = ""
	TITLE$(4%) = "         Per1   Per2   Per3    Qtr1   Per4   Per5   " + &
		"Per6    Qtr2   Per7   Per8   Per9    Qtr3  "          + &
		"Per10  Per11  Per12    Qtr4     YTD"

	TITLE$(5%) = "."
	TITLE$(6%) = ""


	TEXT$ = "     Report Grand Total"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, PAGE%)

	FOR Y% = 1% TO YEARS%

		TEXT$ = SPACE$(25%) + ":   "
		TEXT$ = "Year " + YYYY$(Y%) + TEXT$ + TEXT$ + TEXT$ + TEXT$

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		TEXT$ = "Units " + &
			FORMAT$(TOTALQUANTITY(Y%, 1%), "#######") + &
			SPACE$(7%) + &
			FORMAT$(TOTALQUANTITY(Y%, 3%), "#######") + &
			SPACE$(8%) + &
			FORMAT$(TOTALQUANTITY(Y%, 4%), "#######") + &
			SPACE$(7%) + &
			FORMAT$(TOTALQUANTITY(Y%, 6%), "#######") + &
			SPACE$(8%) + &
			FORMAT$(TOTALQUANTITY(Y%, 7%), "#######") + &
			SPACE$(7%) + &
			FORMAT$(TOTALQUANTITY(Y%, 9%), "#######") + &
			SPACE$(8%) + &
			FORMAT$(TOTALQUANTITY(Y%, 10%), "#######") + &
			SPACE$(7%) + &
			FORMAT$(TOTALQUANTITY(Y%, 12%), "#######") + &
			SPACE$(8%) + &
			FORMAT$(TOTALQTYYTD(Y%), "########")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		TEXT$ = "      " + &
			SPACE$(7%) + &
			FORMAT$(TOTALQUANTITY(Y%, 2%), "#######") + &
			SPACE$(7%) + &
			FORMAT$(TOTALQTYQTR(Y%, 1%), "########") + &
			SPACE$(7%) + &
			FORMAT$(TOTALQUANTITY(Y%, 5%), "#######") + &
			SPACE$(7%) + &
			FORMAT$(TOTALQTYQTR(Y%, 2%), "########") + &
			SPACE$(7%) + &
			FORMAT$(TOTALQUANTITY(Y%, 8%), "#######") + &
			SPACE$(7%) + &
			FORMAT$(TOTALQTYQTR(Y%, 3%), "########") + &
			SPACE$(7%) + &
			FORMAT$(TOTALQUANTITY(Y%, 11%), "#######") + &
			SPACE$(7%) + &
			FORMAT$(TOTALQTYQTR(Y%, 4%), "########")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		TEXT$ = "DolVm " + &
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

 PrintLine:
	IF (TOTAL_ONLY$ <> "Y")
	THEN
		V% = SA_EXAM_SALESMAN(LAST_SUBACCT$, &
			SA_SALESMAN_EXAM, SB_SUBACCOUNT_EXAM)

		V% = AR_EXAM_CUSTOM(LAST_CROSSREF$, AR_35CUSTOM_EXAM)

		IF LAST_SUBACCT$ <> "" OR LAST_CROSSREF$ <> ""
		THEN
			!
			! Print out one line
			!
			TEXT$ = "     " + LAST_SUBACCT$ + "     " + &
				LEFT$(SA_SALESMAN_EXAM::DESCR, 30%) + "     " + &
				LAST_CROSSREF$ + "     " + &
				AR_35CUSTOM_EXAM::CUSNAM

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, PAGE%)
		END IF

		PAGE% = 4%

		FOR Y% = 1% TO YEARS%

			TEXT$ = SPACE$(25%) + ":   "
			TEXT$ = "Year " + YYYY$(Y%) + &
				TEXT$ + TEXT$ + TEXT$ + TEXT$

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3%)

			TEXT$ = "Units " + &
				FORMAT$(QUANTITY(Y%, 1%), "#######") + &
				FORMAT$(QUANTITY(Y%, 2%), "#######") + &
				FORMAT$(QUANTITY(Y%, 3%), "#######") + &
				FORMAT$(QTYQTR(Y%, 1%), "########") + &
				FORMAT$(QUANTITY(Y%, 4%), "#######") + &
				FORMAT$(QUANTITY(Y%, 5%), "#######") + &
				FORMAT$(QUANTITY(Y%, 6%), "#######") + &
				FORMAT$(QTYQTR(Y%, 2%), "########") + &
				FORMAT$(QUANTITY(Y%, 7%), "#######") + &
				FORMAT$(QUANTITY(Y%, 8%), "#######") + &
				FORMAT$(QUANTITY(Y%, 9%), "#######") + &
				FORMAT$(QTYQTR(Y%, 3%), "########") + &
				FORMAT$(QUANTITY(Y%, 10%), "#######") + &
				FORMAT$(QUANTITY(Y%, 11%), "#######") + &
				FORMAT$(QUANTITY(Y%, 12%), "#######") + &
				FORMAT$(QTYQTR(Y%, 4%), "########") + &
				FORMAT$(QTYYTD(Y%), "########")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

			TEXT$ = "DolVm " + &
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
				FORMAT$(PRCYTD(Y%),  "########")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		NEXT Y%

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

		GOTO ExitProgram IF UTL_REPORTX::STAT

	END IF

	FOR Y% = 1% TO YEARS%

		FOR I% = 1% TO 12%
			SALEQUANTITY(Y%,I%) = SALEQUANTITY(Y%,I%) + QUANTITY(Y%,I%)
			SALEPRICEAMT(Y%,I%) = SALEPRICEAMT(Y%,I%) + PRICEAMT(Y%,I%)
		NEXT I%

		FOR I% = 1% TO 4%
			SALEQTYQTR(Y%,I%) = SALEQTYQTR(Y%,I%) + QTYQTR(Y%,I%)
			SALEPRCQTR(Y%,I%) = SALEPRCQTR(Y%,I%) + PRCQTR(Y%,I%)
		NEXT I%

		SALEQTYYTD(Y%) = SALEQTYYTD(Y%) + QTYYTD(Y%)
		SALEPRCYTD(Y%) = SALEPRCYTD(Y%) + PRCYTD(Y%)
	NEXT Y%

	PRINT_FLAG% = 0%

	FOR Y% = 1% TO YEARS%

		NO_TEST%(Y%) = 0%

		QTYYTD(Y%) = 0.0
		PRCYTD(Y%) = 0.0

		FOR I% = 1% TO 4%
			QTYQTR(Y%, I%) = 0.0
			PRCQTR(Y%, I%) = 0.0

			QUANTITY(Y%, I%) = 0.0
			PRICEAMT(Y%, I%) = 0.0

			QUANTITY(Y%, I% + 4%) = 0.0
			PRICEAMT(Y%, I% + 4%) = 0.0

			QUANTITY(Y%, I% + 8%) = 0.0
			PRICEAMT(Y%, I% + 8%) = 0.0

		NEXT I%
	NEXT Y%

	RETURN

 SaleTotal:
	IF (TOTAL_ONLY$ <> "Y")
	THEN
		V% = SA_EXAM_SALESMAN(LAST_SUBACCT$, &
			SA_SALESMAN_EXAM, SB_SUBACCOUNT_EXAM)

		!
		! Print out one line
		!
		TEXT$ = "     " + LAST_SUBACCT$ + "     " + &
			LEFT$(SA_SALESMAN_EXAM::DESCR, 30%) + "     " + &
			STRING$(68%, A"."B)

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, PAGE%)

		FOR Y% = 1% TO YEARS%

			TEXT$ = SPACE$(25%) + ":   "
			TEXT$ = "Year " + YYYY$(Y%) + &
				TEXT$ + TEXT$ + TEXT$ + TEXT$

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3%)

			TEXT$ = "Units " + &
				FORMAT$(SALEQUANTITY(Y%, 1%), "#######") + &
				FORMAT$(SALEQUANTITY(Y%, 2%), "#######") + &
				FORMAT$(SALEQUANTITY(Y%, 3%), "#######") + &
				FORMAT$(SALEQTYQTR(Y%, 1%), "########") + &
				FORMAT$(SALEQUANTITY(Y%, 4%), "#######") + &
				FORMAT$(SALEQUANTITY(Y%, 5%), "#######") + &
				FORMAT$(SALEQUANTITY(Y%, 6%), "#######") + &
				FORMAT$(SALEQTYQTR(Y%, 2%), "########") + &
				FORMAT$(SALEQUANTITY(Y%, 7%), "#######") + &
				FORMAT$(SALEQUANTITY(Y%, 8%), "#######") + &
				FORMAT$(SALEQUANTITY(Y%, 9%), "#######") + &
				FORMAT$(SALEQTYQTR(Y%, 3%), "########") + &
				FORMAT$(SALEQUANTITY(Y%, 10%), "#######") + &
				FORMAT$(SALEQUANTITY(Y%, 11%), "#######") + &
				FORMAT$(SALEQUANTITY(Y%, 12%), "#######") + &
				FORMAT$(SALEQTYQTR(Y%, 4%), "########") + &
				FORMAT$(SALEQTYYTD(Y%), "########")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

			TEXT$ = "DolVm " + &
				FORMAT$(SALEPRICEAMT(Y%, 1%), "#######") + &
				FORMAT$(SALEPRICEAMT(Y%, 2%), "#######") + &
				FORMAT$(SALEPRICEAMT(Y%, 3%), "#######") + &
				FORMAT$(SALEPRCQTR(Y%, 1%), "########") + &
				FORMAT$(SALEPRICEAMT(Y%, 4%), "#######") + &
				FORMAT$(SALEPRICEAMT(Y%, 5%), "#######") + &
				FORMAT$(SALEPRICEAMT(Y%, 6%), "#######") + &
				FORMAT$(SALEPRCQTR(Y%, 2%), "########") + &
				FORMAT$(SALEPRICEAMT(Y%, 7%), "#######") + &
				FORMAT$(SALEPRICEAMT(Y%, 8%), "#######") + &
				FORMAT$(SALEPRICEAMT(Y%, 9%), "#######") + &
				FORMAT$(SALEPRCQTR(Y%, 3%), "########") + &
				FORMAT$(SALEPRICEAMT(Y%, 10%), "#######") + &
				FORMAT$(SALEPRICEAMT(Y%, 11%), "#######") + &
				FORMAT$(SALEPRICEAMT(Y%, 12%), "#######") + &
				FORMAT$(SALEPRCQTR(Y%, 4%), "########") + &
				FORMAT$(SALEPRCYTD(Y%),  "########")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		NEXT Y%
	END IF

	FOR Y% = 1% TO YEARS%

		FOR I% = 1% TO 12%
			PRODQUANTITY(Y%,I%) = PRODQUANTITY(Y%,I%) + &
				SALEQUANTITY(Y%,I%)
			PRODPRICEAMT(Y%,I%) = PRODPRICEAMT(Y%,I%) + &
				SALEPRICEAMT(Y%,I%)
		NEXT I%
	NEXT Y%

	FOR Y% = 1% TO YEARS%

		PRODQTYYTD(Y%) = PRODQTYYTD(Y%) + SALEQTYYTD(Y%)
		PRODPRCYTD(Y%) = PRODPRCYTD(Y%) + SALEPRCYTD(Y%)

		SALEQTYYTD(Y%) = 0.0
		SALEPRCYTD(Y%) = 0.0

		FOR I% = 1% TO 4%
			PRODQTYQTR(Y%, I%) = PRODQTYQTR(Y%, I%) + &
				SALEQTYQTR(Y%, I%)
			PRODPRCQTR(Y%, I%) = PRODPRCQTR(Y%, I%) + &
				SALEPRCQTR(Y%, I%)

			SALEQUANTITY(Y%, I%) = 0.0
			SALEPRICEAMT(Y%, I%) = 0.0

			SALEQUANTITY(Y%, I% + 4%) = 0.0
			SALEPRICEAMT(Y%, I% + 4%) = 0.0

			SALEQUANTITY(Y%, I% + 8%) = 0.0
			SALEPRICEAMT(Y%, I% + 8%) = 0.0

			SALEQTYQTR(Y%, I%) = 0.0
			SALEPRCQTR(Y%, I%) = 0.0

		NEXT I%

	NEXT Y%

	PAGE% = 999%

	RETURN

 ProdTotal:
	IF (TOTAL_ONLY$ <> "Y")
	THEN
		TEXT$ = ":::::::::: " + PD_PRODUCT::PRODUCT_NUM + " " + &
			TRM$(PD_PRODUCT::DESCRIPTION) + " " + &
			PD_PRODUCT::PROD_TYPE + " " + &
			PD_PRODUCT::CATEGORY + " " + &
			TRM$(PD_PRODUCT::SECONDARY_CODE)

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, PAGE%)
	END IF

	FOR Y% = 1% TO YEARS%

		TEXT$ = SPACE$(25%) + ":   "
		TEXT$ = "Year " + YYYY$(Y%) + TEXT$ + TEXT$ + TEXT$ + TEXT$

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		TEXT$ = "Units " + &
			FORMAT$(PRODQUANTITY(Y%, 1%), "#######") + &
			FORMAT$(PRODQUANTITY(Y%, 2%), "#######") + &
			FORMAT$(PRODQUANTITY(Y%, 3%), "#######") + &
			FORMAT$(PRODQTYQTR(Y%, 1%), "########") + &
			FORMAT$(PRODQUANTITY(Y%, 4%), "#######") + &
			FORMAT$(PRODQUANTITY(Y%, 5%), "#######") + &
			FORMAT$(PRODQUANTITY(Y%, 6%), "#######") + &
			FORMAT$(PRODQTYQTR(Y%, 2%), "########") + &
			FORMAT$(PRODQUANTITY(Y%, 7%), "#######") + &
			FORMAT$(PRODQUANTITY(Y%, 8%), "#######") + &
			FORMAT$(PRODQUANTITY(Y%, 9%), "#######") + &
			FORMAT$(PRODQTYQTR(Y%, 3%), "########") + &
			FORMAT$(PRODQUANTITY(Y%, 10%), "#######") + &
			FORMAT$(PRODQUANTITY(Y%, 11%), "#######") + &
			FORMAT$(PRODQUANTITY(Y%, 12%), "#######") + &
			FORMAT$(PRODQTYQTR(Y%, 4%), "########") + &
			FORMAT$(PRODQTYYTD(Y%),  "########")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		TEXT$ = "DolVm " + &
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

	NEXT Y%

	FOR Y% = 1% TO YEARS%
		FOR I% = 1% TO 12%
			TOTALQUANTITY(Y%, I%) = TOTALQUANTITY(Y%, I%) + &
				PRODQUANTITY(Y%, I%)
			TOTALPRICEAMT(Y%, I%) = TOTALPRICEAMT(Y%, I%) + &
				PRODPRICEAMT(Y%, I%)
		NEXT I%
	NEXT Y%

	FOR Y% = 1% TO YEARS%

		TOTALQTYYTD(Y%)  = TOTALQTYYTD(Y%)  + PRODQTYYTD(Y%)
		TOTALPRCYTD(Y%)  = TOTALPRCYTD(Y%)  + PRODPRCYTD(Y%)

		PRODQTYYTD(Y%) = 0.0
		PRODPRCYTD(Y%) = 0.0

		FOR I% = 1% TO 4%

			TOTALQTYQTR(Y%, I%) = TOTALQTYQTR(Y%, I%) + &
				PRODQTYQTR(Y%, I%)
			TOTALPRCQTR(Y%, I%) = TOTALPRCQTR(Y%, I%) + &
				PRODPRCQTR(Y%, I%)

			PRODQUANTITY(Y%, I%)       = 0.0
			PRODPRICEAMT(Y%, I%)       = 0.0

			PRODQUANTITY(Y%, I% + 4%)  = 0.0
			PRODPRICEAMT(Y%, I% + 4%)  = 0.0

			PRODQUANTITY(Y%, I% + 8%)  = 0.0
			PRODPRICEAMT(Y%, I% + 8%)  = 0.0

			PRODQTYQTR(Y%, I%)         = 0.0
			PRODPRCQTR(Y%, I%)         = 0.0

		NEXT I%

	NEXT Y%

	IF (TOTAL_ONLY$ = "Y")
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
		PAGE% = 3%
	ELSE
		PAGE% = 999%
	END IF

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
	! End of report SA_RPRT_PRODHIST
	!******************************************************************
	END
