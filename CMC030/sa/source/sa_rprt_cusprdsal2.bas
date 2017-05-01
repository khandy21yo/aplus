1	%TITLE "Sales by Customer and Product"
	%SBTTL "SA_RPRT_CUSPRDSAL2"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1998 BY
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
	! ID:SA0099
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Sales by Customer and Product Report\* option
	!	prints a sales report listing the following
	!	information:
	!	.table 3,25
	!	.te
	!	Customer Number
	!	.te
	!	Customer Name
	!	.te
	!	Customer Type
	!	.te
	!	Customer Category
	!	.te
	!	Customer Address
	!	.te
	!	Product Number
	!	.te
	!	Product Description
	!	.te
	!	Salesman Number
	!	.te
	!	Salesman Name
	!	.te
	!	Sales Period
	!	.te
	!	Sales Quantity
	!	.te
	!	Sales Amount
	!	.te
	!	Profit
	!	.te
	!	Variance
	!	.end table
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS SA_SOURCE:SA_RPRT_CUSPRDSAL2/LINE
	!	$ LINK/EXECUTABLE=SA_EXE: SA_RPRT_CUSPRDSAL2, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE SA_RPRT_CUSPRDSAL2.OBJ;*
	!
	! Author:
	!
	!	04/07/98 - Kevin Handy
	!		Based upon SA_RPRT_CUSPRDSAL
	!
	! Modification history:
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE's
	!
	!	11/27/2000 - Kevin Handy
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
	EXTERNAL LONG	FUNCTION PD_EXAM_PRODUCT
	EXTERNAL LONG	FUNCTION SA_EXAM_SALESMAN

	%PAGE

	!
	! CDD inclusions
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

	%INCLUDE "SOURCE:[IC.OPEN]IC_35HISTORY.HB"
	MAP (IC_35HISTORY)	IC_35HISTORY_CDD	IC_35HISTORY

	DECLARE			IC_35HISTORY_CDD	IC_HISTORY(10%)

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	%INCLUDE "SOURCE:[SA.OPEN]SA_SALESMAN.HB"
	DECLARE			SA_SALESMAN_CDD		SA_SALESMAN_EXAM

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	DECLARE			SB_SUBACCOUNT_CDD	SB_SUBACCOUNT_EXAM

	!
	! Array for variances
	!
	DECLARE REAL VARIANCE(17%)

	!
	! Used in PrintLine, created at 17120
	!
	DECLARE REAL PQUANTITY(10%, 12%)
	DECLARE REAL PRICEAMT(10%, 12%)
	DECLARE REAL COSTAMT(10%, 12%)

	!
	! Used in CustTotal, created in PrintLine
	!
	DECLARE REAL CUSTPRICEAMT(10%, 12%)
	DECLARE REAL CUSTCOSTAMT(10%, 12%)

	!
	! Used in ExitTotal, created in CustTotal
	!
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
	!	^*(01) Sort by (N,T,C,A)\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field determines the order
	!	in which the report will print.
	!	.b
	!	Valid settings are:
	!	.table 3,25
	!	.te
	!	^*N\* - Customer Number
	!	.te
	!	^*T\* - Customer Type
	!	.te
	!	^*C\* - Customer Category
	!	.te
	!	^*A\* - Customer Alpha Sort
	!	.end table
	!	A setting is required in this field.  No other settings are
	!	valid.
	!	.lm -5
	!
	! Index:
	!	.x Sort by
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field enters the
	!	item with which the report is to begin printing.
	!	.b
	!	A blank field will cause the report to begin with the first
	!	item in the file.
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
	!	The ^*To Item\* field specifies the item
	!	with which the report is to end printing.
	!	.b
	!	A blank field will cause the report to end with the last
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	WLDCRD$ = ""
	PRDCATWLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Product Category Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Product Category Wildcard\* field selects
	!	designated product categories to be printed by entering a
	!	"wildcard" using the Wildcarding Technique.
	!	.lm -5
	!
	! Index:
	!
	!--

	LOC_WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Location Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Location Wildcard\* field selects
	!	designated locations to be printed by entering a "wildcard"
	!	using the Wildcarding Technique.
	!	.lm -5
	!
	! Index:
	!
	!--

	PRINT_UNITS$ = EDIT$(UTL_REPORTX::OPTDEF(5%), -1%)

	!++
	! Abstract:FLD06
	!	^*(06) Print Units\*
	!	.b
	!	.lm +5
	!	The ^*Print Unit\* field etermines if it should
	!	show the unit quantity on the report.
	!	.lm -5
	!
	! Index:
	!
	!--

	GROSS_PROFIT$ = EDIT$(UTL_REPORTX::OPTDEF(6%), -1%)

	!++
	! Abstract:FLD07
	!	^*(07) Print Gross Profit\*
	!	.b
	!	.lm +5
	!	The ^*Print Gross Profit\* field determines if it should
	!	show the gross profit on the report.
	!	.lm -5
	!
	! Index:
	!
	!--

	PRINT_VARIANCE$ = EDIT$(UTL_REPORTX::OPTDEF(7%), -1%)

	!++
	! Abstract:FLD08
	!	^*(08) Print Variance\*
	!	.b
	!	.lm +5
	!	The ^*Print Variance\* field optionally shows the
	!	yearly unit, sales, and gross profit variances on the report.
	!	.lm -5
	!
	! Index:
	!
	!--

	YYYY$ = EDIT$(UTL_REPORTX::OPTDEF(8%), -1%)

	!++
	! Abstract:FLD09
	!	^*(09) Fiscal Year\*
	!	.b
	!	.lm +5
	!	The ^*Fiscal Year\* field
	!	selects the fiscal year with which the report will begin printing.
	!	.lm -5
	!
	! Index:
	!
	!--

	YEARS% = VAL%(EDIT$(UTL_REPORTX::OPTDEF(9%), -1%))

	!++
	! Abstract:FLD10
	!	^*(10) Number of Years\*
	!	.b
	!	.lm +5
	!	The ^*Number of Years\* field specifies the
	!	number of fiscal years which are to be printed. If
	!	the selected inventory transaction history file does not
	!	exist, the report will not search for prior years.
	!	.lm -5
	!
	! Index:
	!
	!--

	%PAGE

300	!
	! Open Customer file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.OPN"
	USE
		FILENAME$ = "AR_35CUSTOM"
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

		VALID.YEARS% = Y%
		IC_35HISTORY.CH%(Y%) = IC_35HISTORY.CH%
		IC_35HISTORY.CH% = 0.0
		YYYY$(Y%) = YYYY$
		YYYY$ = FORMAT$(VAL%(YYYY$) - 1%, "####")
	NEXT Y%

	%PAGE

 ReportTitle:
	YEARS% = VALID.YEARS%
	!
	! Title
	!
	SELECT SORTBY$

	CASE "N"
		K_NUM% = 0%
		TITLE$(1%) = "SALES BY CUSTOMER NUMBER"

	CASE "T"
		K_NUM% = 1%
		TITLE$(1%) = "SALES BY CUSTOMER TYPE"

	CASE "C"
		K_NUM% = 2%
		TITLE$(1%) = "SALES BY CUSTOMER CATEGORY"

	CASE "A"
		K_NUM% = 3%
		TITLE$(1%) = "SALES BY CUSTOMER ALPHA SORT"

	END SELECT

	TITLE$(3%) = "Sales Analysis System"
	TITLE$(4%) = ""

	!
	! Heading
	!
	TITLE$(5%) = "     Product#           Description              " + &
		"                    Salesman#      Name"

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
			RESET #AR_35CUSTOM.CH%, KEY #K_NUM%
		ELSE
			FIND #AR_35CUSTOM.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155% OR ERR = 9%
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

	PRINT_FLAG% = 0%

 GetNextRec:
	!
	! Print totals after each customer
	!
	IF PRINT_FLAG%
	THEN
		GOSUB PrintLine
		GOSUB CustTotal
	END IF

	NEW_CUSTOMER% = -1%

17020	!
	! Get next record
	!
	WHEN ERROR IN
		GET #AR_35CUSTOM.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

	!
	! Check status
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	SELECT SORTBY$

	CASE "N"
		GOTO ExitTotal IF (AR_35CUSTOM::CUSNUM > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_ARRAY(EDIT$( &
			AR_35CUSTOM::CUSNUM, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "T"
		GOTO ExitTotal IF (AR_35CUSTOM::TTYPE > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_ARRAY(EDIT$( &
			AR_35CUSTOM::TTYPE, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "C"
		GOTO ExitTotal IF (AR_35CUSTOM::CATEGORY > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_ARRAY(EDIT$( &
			AR_35CUSTOM::CATEGORY, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "A"
		GOTO ExitTotal IF (AR_35CUSTOM::ALPSRT > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_ARRAY(EDIT$( &
			AR_35CUSTOM::ALPSRT, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	END SELECT

	TEST_LINE$ = ""
	LAST_PRODUCT$ = ""

17100	!
	! Get History record
	!
	FOR Y% = 1% TO YEARS%

		EOF%(Y%) = 0%
		NO_TEST%(Y%) = 0%

		WHEN ERROR IN
			GET #IC_35HISTORY.CH%(Y%), &
				KEY #1% EQ AR_35CUSTOM::CUSNUM, &
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
			IF IC_HISTORY(Y%)::CROSSREF <> AR_35CUSTOM::CUSNUM
		TEST_EOF% = TEST_EOF% * EOF%(Y%)

	NEXT Y%

	GOTO GetNextRec IF TEST_EOF% = 1%

	FOR Y% = SY% TO YEARS%

		IF EOF%(Y%) <> 1% AND NO_TEST%(Y%) <> 1%
		THEN
			GOTO GetHistRec IF IC_HISTORY(Y%)::TRANSTYPE <> "SA"

			GOTO GetHistRec IF COMP_ARRAY(EDIT$( &
				IC_HISTORY(Y%)::LOCATION, -1%), &
				LOC_WLDCRD$) = 0% &
				AND LOC_WLDCRD$ <> ""

			GOTO GetHistRec IF PD_EXAM_PRODUCT( &
				IC_HISTORY(Y%)::PRODUCT, &
				PD_PRODUCT_EXAM) <> CMC$_NORMAL

			GOTO GetHistRec IF COMP_STRING(EDIT$( &
				PD_PRODUCT_EXAM::CATEGORY, -1%), PRDCATWLDCRD$) = 0% &
				AND PRDCATWLDCRD$ <> ""
		ELSE
			NO_TEST%(Y%) = 1%
		END IF

	NEXT Y%

	NO_TEST% = 1%

	FOR Y% = 1% TO YEARS%

		IF NO_TEST%(Y%) = 0% AND TEST_LINE$ <> ""
		THEN
			IF TEST_LINE$ <> IC_HISTORY(Y%)::CROSSREF  + &
				IC_HISTORY(Y%)::PRODUCT + &
				IC_HISTORY(Y%)::SUBACCT
			THEN
				NO_TEST%(Y%) = 1%
			END IF
		END IF

		NO_TEST% = NO_TEST% * NO_TEST%(Y%)

	NEXT Y%

	IF NO_TEST%
	THEN
		GOSUB PrintLine
	END IF

	TEST_LINE$ = AR_35CUSTOM::CUSNUM + CHR$(255%)

	!
	! Find minimum
	!
	FOR Y% = 1% TO YEARS%

		IF EOF%(Y%) = 0% AND &
			TEST_LINE$ > IC_HISTORY(Y%)::CROSSREF + &
				IC_HISTORY(Y%)::PRODUCT + &
				IC_HISTORY(Y%)::SUBACCT
		THEN
			TEST_LINE$ = IC_HISTORY(Y%)::CROSSREF + &
				IC_HISTORY(Y%)::PRODUCT + &
				IC_HISTORY(Y%)::SUBACCT

			LAST_PRODUCT$ = IC_HISTORY(Y%)::PRODUCT
			LAST.SALESMAN$ = IC_HISTORY(Y%)::SUBACCT

		END IF

	NEXT Y%

	FOR Y% = 1% TO YEARS%

		IF TEST_LINE$ = IC_HISTORY(Y%)::CROSSREF + &
			IC_HISTORY(Y%)::PRODUCT + &
			IC_HISTORY(Y%)::SUBACCT
		THEN
			FOR I% = 1% TO 12%

			PQUANTITY(Y%, I%) = PQUANTITY(Y%, I%) - &
				IC_HISTORY(Y%)::PQUANTITY(I%)
			PRICEAMT(Y%, I%)  = PRICEAMT(Y%, I%) + &
				IC_HISTORY(Y%)::PRICEAMT(I%)
			COSTAMT(Y%, I%)   = COSTAMT(Y%, I%) + &
				IC_HISTORY(Y%)::COSTAMT(I%)

			J% = INT((I% - 1%) / 3%) + 1%

			QTYQTR(Y%,J%) = QTYQTR(Y%,J%) - &
				IC_HISTORY(Y%)::PQUANTITY(I%)
			PRCQTR(Y%,J%) = PRCQTR(Y%,J%) + &
				IC_HISTORY(Y%)::PRICEAMT(I%)
			CSTQTR(Y%,J%) = CSTQTR(Y%,J%) + &
				IC_HISTORY(Y%)::COSTAMT(I%)
			QTYYTD(Y%) = QTYYTD(Y%)  - IC_HISTORY(Y%)::PQUANTITY(I%)
			PRCYTD(Y%) = PRCYTD(Y%)  + IC_HISTORY(Y%)::PRICEAMT(I%)
			CSTYTD(Y%) = CSTYTD(Y%)  + IC_HISTORY(Y%)::COSTAMT(I%)

			NEXT I%
		ELSE
			NO_TEST%(Y%) = 1%
		END IF

	NEXT Y%

	PRINT_FLAG% = -1%

	IF NEW_CUSTOMER%
	THEN
		TITLE$(2%) = TRM$(AR_35CUSTOM::CUSNUM) + " " + &
			TRM$(AR_35CUSTOM::CUSNAM)

		TEXT$ = ":::::::::: " + AR_35CUSTOM::CUSNUM + " " + &
			AR_35CUSTOM::CUSNAM + " " + &
			AR_35CUSTOM::TTYPE + " " + &
			AR_35CUSTOM::CATEGORY

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, PAGE%)

		TEXT$ = SPACE$(22%) + AR_35CUSTOM::ADD1 + " " + &
			AR_35CUSTOM::ADD2 + " " + &
			AR_35CUSTOM::CITY + " " + &
			AR_35CUSTOM::STATE + " " + &
			PRNT_PHONE(AR_35CUSTOM::PHONE, 0%)

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

		NEW_CUSTOMER%, PAGE% = 0%
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
		"Per6    Qtr2   Per7   Per8   Per9    Qtr3  " + &
		"Per10  Per11  Per12    Qtr4     YTD"

	TITLE$(5%) = "."
	TITLE$(6%) = ""


	TEXT$ = "     Report Grand Total"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, PAGE%)

	FOR Y% = 1% TO YEARS%

		TEXT$ = SPACE$(25%) + ":   "
		TEXT$ = "Year " + YYYY$(Y%) + TEXT$ + TEXT$ + TEXT$ + TEXT$

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%) &
			IF Y% = 1% OR Y% <> YEARS% OR PRINT_VARIANCE$ = "N"

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
			FORMAT$(TOTALPRCYTD(Y%),  "########")

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

		IF GROSS_PROFIT$ = "Y"
		THEN
			TEXT$ = "GProf " + &
				FORMAT$(TOTALPRICEAMT(Y%, 1%) - &
					TOTALCOSTAMT(Y%, 1%), "#######") + &
				SPACE$(7%) + &
				FORMAT$(TOTALPRICEAMT(Y%, 3%) - &
					TOTALCOSTAMT(Y%, 3%), "#######") + &
				SPACE$(8%) + &
				FORMAT$(TOTALPRICEAMT(Y%, 4%) - &
					TOTALCOSTAMT(Y%, 4%), "#######") + &
				SPACE$(7%) + &
				FORMAT$(TOTALPRICEAMT(Y%, 6%) - &
					TOTALCOSTAMT(Y%, 6%), "#######") + &
				SPACE$(8%) + &
				FORMAT$(TOTALPRICEAMT(Y%, 7%) - &
					TOTALCOSTAMT(Y%, 7%), "#######") + &
				SPACE$(7%) + &
				FORMAT$(TOTALPRICEAMT(Y%, 9%) - &
					TOTALCOSTAMT(Y%, 9%), "#######") + &
				SPACE$(8%) + &
				FORMAT$(TOTALPRICEAMT(Y%, 10%) - &
					TOTALCOSTAMT(Y%, 10%), "#######") + &
				SPACE$(7%) + &
				FORMAT$(TOTALPRICEAMT(Y%, 12%) - &
					TOTALCOSTAMT(Y%, 12%), "#######") + &
				SPACE$(8%) + &
				FORMAT$(TOTALPRCYTD(Y%) - &
					TOTALCSTYTD(Y%),  "########")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

			TEXT$ = "      " + &
				SPACE$(7%) + &
				FORMAT$(TOTALPRICEAMT(Y%, 2%) - &
					TOTALCOSTAMT(Y%, 2%), "#######")   + &
				SPACE$(7%) + &
				FORMAT$(TOTALPRCQTR(Y%, 1%) - &
					TOTALCSTQTR(Y%, 1%), "########")     + &
				SPACE$(7%) + &
				FORMAT$(TOTALPRICEAMT(Y%, 5%) - &
					TOTALCOSTAMT(Y%, 5%), "#######")   + &
				SPACE$(7%) + &
				FORMAT$(TOTALPRCQTR(Y%, 2%) - &
					TOTALCSTQTR(Y%, 2%), "########")     + &
				SPACE$(7%) + &
				FORMAT$(TOTALPRICEAMT(Y%, 8%) - &
					TOTALCOSTAMT(Y%, 8%), "#######")   + &
				SPACE$(7%) + &
				FORMAT$(TOTALPRCQTR(Y%, 3%) - &
					TOTALCSTQTR(Y%, 3%), "########")     + &
				SPACE$(7%) + &
				FORMAT$(TOTALPRICEAMT(Y%, 11%) - &
					TOTALCOSTAMT(Y%, 11%), "#######") + &
				SPACE$(7%) + &
				FORMAT$(TOTALPRCQTR(Y%, 4%) - &
					TOTALCSTQTR(Y%, 4%), "########")

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
						TOTALPRICEAMT(Y% + 1%, I%) * 100.0
				END IF

			NEXT I%

			FOR I% = 13% TO 16%

				IF TOTALPRCQTR(Y% + 1%, I% - 12%) = 0.0
				THEN
					VARIANCE(I%) = 100.0
					VARIANCE(I%) = 0.0 &
						IF TOTALPRCQTR(Y%, I% - 12%) = 0.0
				ELSE
					VARIANCE(I%) = &
						(TOTALPRCQTR(Y%, I% - 12%) - &
						TOTALPRCQTR(Y% + 1%, I% - 12%)) / &
						TOTALPRCQTR(Y% + 1%, I% - 12%) * 100.0
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
						TOTALCOSTAMT(Y% + 1%, I%)) * 100.0
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
					VARIANCE(I%) = &
						(TOTALPRCQTR(Y%, I% - 12%) - &
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
					IF TOTALPRCYTD(Y%) - &
					TOTALCSTYTD(Y%) = 0.0
			ELSE
				VARIANCE(17%) = (TOTALPRCYTD(Y%) - &
					TOTALCSTYTD(Y%) - &
					(TOTALPRCYTD(Y% + 1%) - &
					TOTALCSTYTD(Y% + 1%))) / &
					(TOTALPRCYTD(Y% + 1%) - &
					TOTALCSTYTD(Y% + 1%)) * 100.0
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

 PrintLine:
17500	V% = PD_EXAM_PRODUCT(LAST_PRODUCT$, PD_PRODUCT_EXAM)

	IF SA_EXAM_SALESMAN(LAST.SALESMAN$, SA_SALESMAN_EXAM, &
		SB_SUBACCOUNT_EXAM) <> CMC$_NORMAL
	THEN
		SA_SALESMAN_EXAM::DESCR = ""
	END IF

	!
	! Print out one line
	!
	TEXT$ = "     " + LAST_PRODUCT$ + "     " + &
		PD_PRODUCT_EXAM::DESCRIPTION + "     " + &
		LAST.SALESMAN$ + "     " + &
		SA_SALESMAN_EXAM::DESCR

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, PAGE%)

	PAGE% = 4%

	FOR Y% = 1% TO YEARS%

		GOTO ProdVar IF Y% <> 1% AND Y% = YEARS% AND &
			PRINT_VARIANCE$ = "Y"

		TEXT$ = SPACE$(25%) + ":   "
		TEXT$ = "Year " + YYYY$(Y%) + TEXT$ + TEXT$ + TEXT$ + TEXT$

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3%)

		IF PRINT_UNITS$ = "Y" AND (Y% = 1% OR Y% > 1% AND &
			PRINT_VARIANCE$ <> "Y")
		THEN
			TEXT$ = "Units " + &
				FORMAT$(PQUANTITY(Y%, 1%), "#######") + &
				FORMAT$(PQUANTITY(Y%, 2%), "#######") + &
				FORMAT$(PQUANTITY(Y%, 3%), "#######") + &
				FORMAT$(QTYQTR(Y%, 1%), "########") + &
				FORMAT$(PQUANTITY(Y%, 4%), "#######") + &
				FORMAT$(PQUANTITY(Y%, 5%), "#######") + &
				FORMAT$(PQUANTITY(Y%, 6%), "#######") + &
				FORMAT$(QTYQTR(Y%, 2%), "########") + &
				FORMAT$(PQUANTITY(Y%, 7%), "#######") + &
				FORMAT$(PQUANTITY(Y%, 8%), "#######") + &
				FORMAT$(PQUANTITY(Y%, 9%), "#######") + &
				FORMAT$(QTYQTR(Y%, 3%), "########") + &
				FORMAT$(PQUANTITY(Y%, 10%), "#######") + &
				FORMAT$(PQUANTITY(Y%, 11%), "#######") + &
				FORMAT$(PQUANTITY(Y%, 12%), "#######") + &
				FORMAT$(QTYQTR(Y%, 4%), "########") + &
				FORMAT$(QTYYTD(Y%), "########")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		END IF

		TEXT$ = "Sales " + &
			FORMAT$(PRICEAMT(Y%, 1%), "#######")  + &
			FORMAT$(PRICEAMT(Y%, 2%), "#######")  + &
			FORMAT$(PRICEAMT(Y%, 3%), "#######")  + &
			FORMAT$(PRCQTR(Y%, 1%), "########")   + &
			FORMAT$(PRICEAMT(Y%, 4%), "#######")  + &
			FORMAT$(PRICEAMT(Y%, 5%), "#######")  + &
			FORMAT$(PRICEAMT(Y%, 6%), "#######")  + &
			FORMAT$(PRCQTR(Y%, 2%), "########")   + &
			FORMAT$(PRICEAMT(Y%, 7%), "#######")  + &
			FORMAT$(PRICEAMT(Y%, 8%), "#######")  + &
			FORMAT$(PRICEAMT(Y%, 9%), "#######")  + &
			FORMAT$(PRCQTR(Y%, 3%), "########")   + &
			FORMAT$(PRICEAMT(Y%, 10%), "#######") + &
			FORMAT$(PRICEAMT(Y%, 11%), "#######") + &
			FORMAT$(PRICEAMT(Y%, 12%), "#######") + &
			FORMAT$(PRCQTR(Y%, 4%), "########")   + &
			FORMAT$(PRCYTD(Y%),  "########")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		IF GROSS_PROFIT$ = "Y" AND (Y% = 1% OR Y% > 1% AND &
			PRINT_VARIANCE$ <> "Y")
		THEN
			TEXT$ = "GProf " + &
				FORMAT$(PRICEAMT(Y%, 1%) - COSTAMT(Y%, 1%), &
					"#######")   + &
				FORMAT$(PRICEAMT(Y%, 2%) - COSTAMT(Y%, 2%), &
					"#######")   + &
				FORMAT$(PRICEAMT(Y%, 3%) - COSTAMT(Y%, 3%), &
					"#######")   + &
				FORMAT$(PRCQTR(Y%, 1%) - CSTQTR(Y%, 1%), &
					"########")     + &
				FORMAT$(PRICEAMT(Y%, 4%) - COSTAMT(Y%, 4%), &
					"#######")   + &
				FORMAT$(PRICEAMT(Y%, 5%) - COSTAMT(Y%, 5%), &
					"#######")   + &
				FORMAT$(PRICEAMT(Y%, 6%) - COSTAMT(Y%, 6%), &
					"#######")   + &
				FORMAT$(PRCQTR(Y%, 2%) - CSTQTR(Y%, 2%), &
					"########")     + &
				FORMAT$(PRICEAMT(Y%, 7%) - COSTAMT(Y%, 7%), &
					"#######")   + &
				FORMAT$(PRICEAMT(Y%, 8%) - COSTAMT(Y%, 8%), &
					"#######")   + &
				FORMAT$(PRICEAMT(Y%, 9%) - COSTAMT(Y%, 9%), &
					"#######")   + &
				FORMAT$(PRCQTR(Y%, 3%) - CSTQTR(Y%, 3%), &
					"########")     + &
				FORMAT$(PRICEAMT(Y%, 10%) - COSTAMT(Y%, 10%), &
					"#######") + &
				FORMAT$(PRICEAMT(Y%, 11%) - COSTAMT(Y%, 11%), &
					"#######") + &
				FORMAT$(PRICEAMT(Y%, 12%) - COSTAMT(Y%, 12%), &
					"#######") + &
				FORMAT$(PRCQTR(Y%, 4%) - CSTQTR(Y%, 4%), &
					"########")     + &
				FORMAT$(PRCYTD(Y%) - CSTYTD(Y%),  "########")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 1%)

		END IF

 ProdVar:
		IF PRINT_VARIANCE$ = "Y" AND Y% <> YEARS%
		THEN
			FOR I% = 1% TO 12%

				IF PQUANTITY(Y% + 1%, I%) = 0.0
				THEN
					VARIANCE(I%) = 100.00
					VARIANCE(I%) = 0.0 &
						IF PQUANTITY(Y%, I%) = 0.0
				ELSE
					VARIANCE(I%) = (PQUANTITY(Y%, I%) - &
						PQUANTITY(Y% + 1%, I%)) / &
						PQUANTITY(Y% + 1%, I%) * 100.0
				END IF

			NEXT I%

			FOR I% = 13% TO 16%

				IF QTYQTR(Y% + 1%, I% - 12%) = 0.0
				THEN
					VARIANCE(I%) = 100.0
					VARIANCE(I%) = 0.0 &
						IF QTYQTR(Y%, I% - 12%) = 0.0
				ELSE
					VARIANCE(I%) = (QTYQTR(Y%, I% - 12%) - &
						QTYQTR(Y% + 1%, I% - 12%)) / &
						QTYQTR(Y% + 1%, I% - 12%) * 100.0
				END IF

			NEXT I%

			IF QTYYTD(Y% + 1%) = 0.0
			THEN
				VARIANCE(17%) = 100.00
				VARIANCE(17%) = 0.0 IF QTYYTD(Y%) = 0.0
			ELSE
				VARIANCE(17%) = (QTYYTD(Y%) - QTYYTD(Y% + 1%)) / &
					QTYYTD(Y% + 1%) * 100.0
			END IF

			TEXT$ = "UVar  " + &
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
				IF PRINT_UNITS$ = "Y"

			FOR I% = 1% TO 12%

				IF PRICEAMT(Y% + 1%, I%) = 0.0
				THEN
					VARIANCE(I%) = 100.00
					VARIANCE(I%) = 0.0 &
						IF PRICEAMT(Y%, I%) = 0.0
				ELSE
					VARIANCE(I%) = (PRICEAMT(Y%, I%) - &
						PRICEAMT(Y% + 1%, I%)) / &
						PRICEAMT(Y% + 1%, I%) * 100.0
				END IF

			NEXT I%

			FOR I% = 13% TO 16%

				IF PRCQTR(Y% + 1%, I% - 12%) = 0.0
				THEN
					VARIANCE(I%) = 100.0
					VARIANCE(I%) = 0.0 &
						IF PRCQTR(Y%, I% - 12%) = 0.0
				ELSE
					VARIANCE(I%) = (PRCQTR(Y%, I% - 12%) - &
						PRCQTR(Y% + 1%, I% - 12%)) / &
						PRCQTR(Y% + 1%, I% - 12%) * 100.0
				END IF

			NEXT I%

			IF PRCYTD(Y% + 1%) = 0.0
			THEN
				VARIANCE(17%) = 100.00
				VARIANCE(17%) = 0.0 IF PRCYTD(Y%) = 0.0
			ELSE
				VARIANCE(17%) = (PRCYTD(Y%) - PRCYTD(Y% + 1%)) / &
					PRCYTD(Y% + 1%) * 100.0
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

				IF PRICEAMT(Y% + 1%, I%) - &
					COSTAMT(Y% + 1%, I%) = 0.0
				THEN
					VARIANCE(I%) = 100.00
					VARIANCE(I%) = 0.0 &
						IF PRICEAMT(Y%, I%) - &
						COSTAMT(Y%, I%) = 0.0
				ELSE
					VARIANCE(I%) = (PRICEAMT(Y%, I%) - &
						COSTAMT(Y%, I%) - &
						(PRICEAMT(Y% + 1%, I%) - &
						COSTAMT(Y% + 1%, I%))) / &
						(PRICEAMT(Y% + 1%, I%) - &
						COSTAMT(Y% + 1%, I%)) * 100.0
				END IF

			NEXT I%

			FOR I% = 13% TO 16%

				IF PRCQTR(Y% + 1%, I% - 12%) - &
					CSTQTR(Y% + 1%, I% - 12%) = 0.0
				THEN
					VARIANCE(I%) = 100.0
					VARIANCE(I%) = 0.0 &
						IF PRCQTR(Y%, I% - 12%) - &
						CSTQTR(Y%, I% - 12%) = 0.0
				ELSE
					VARIANCE(I%) = (PRCQTR(Y%, I% - 12%) - &
						CSTQTR(Y%, I% - 12%) - &
						(PRCQTR(Y% + 1%, I% - 12%) - &
						CSTQTR(Y% + 1%, I% - 12%))) / &
						(PRCQTR(Y% + 1%, I% - 12%) - &
						CSTQTR(Y% + 1%, I% - 12%)) * 100.0
				END IF

			NEXT I%

			IF PRCYTD(Y% + 1%) - CSTYTD(Y% + 1%) = 0.0
			THEN
				VARIANCE(17%) = 100.00
				VARIANCE(17%) = 0.0 &
					IF PRCYTD(Y%) - CSTYTD(Y%) = 0.0
			ELSE
				VARIANCE(17%) = (PRCYTD(Y%) - CSTYTD(Y%) - &
					(PRCYTD(Y% + 1%) - CSTYTD(Y% + 1%))) / &
					(PRCYTD(Y% + 1%) - CSTYTD(Y% + 1%)) * 100.0
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

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FOR Y% = 1% TO YEARS%

		FOR I% = 1% TO 12%

			CUSTPRICEAMT(Y%, I%) = CUSTPRICEAMT(Y%, I%) + &
				PRICEAMT(Y%, I%)
			CUSTCOSTAMT(Y%, I%) = CUSTCOSTAMT(Y%, I%) + COSTAMT(Y%, I%)

		NEXT I%

		FOR I% = 1% TO 4%

			CUSTPRCQTR(Y%, I%) = CUSTPRCQTR(Y%, I%) + PRCQTR(Y%, I%)
			CUSTCSTQTR(Y%, I%) = CUSTCSTQTR(Y%, I%) + CSTQTR(Y%, I%)

		NEXT I%

		CUSTPRCYTD(Y%) = CUSTPRCYTD(Y%) + PRCYTD(Y%)
		CUSTCSTYTD(Y%) = CUSTCSTYTD(Y%) + CSTYTD(Y%)

	NEXT Y%

	PRINT_FLAG% = 0%

17590	FOR Y% = 1% TO YEARS%

		NO_TEST%(Y%) = 0%

		QTYYTD(Y%) = 0.0
		PRCYTD(Y%) = 0.0
		CSTYTD(Y%) = 0.0

		FOR I% = 1% TO 4%

			QTYQTR(Y%, I%) = 0.0
			PRCQTR(Y%, I%) = 0.0
			CSTQTR(Y%, I%) = 0.0

			PQUANTITY(Y%, I%) = 0.0
			PRICEAMT(Y%, I%) = 0.0
			COSTAMT(Y%, I%) = 0.0

			PQUANTITY(Y%, I% + 4%) = 0.0
			PRICEAMT(Y%, I% + 4%)  = 0.0
			COSTAMT(Y%, I% + 4%)   = 0.0

			PQUANTITY(Y%, I% + 8%) = 0.0
			PRICEAMT(Y%, I% + 8%)  = 0.0
			COSTAMT(Y%, I% + 8%)   = 0.0

		NEXT I%

	NEXT Y%

	RETURN

 CustTotal:
	PAGE% = 999%
	TEXT$ = ":::::::::: " + AR_35CUSTOM::CUSNUM  + " " + &
		AR_35CUSTOM::CUSNAM + " " + &
		AR_35CUSTOM::TTYPE + " " + &
		AR_35CUSTOM::CATEGORY

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, PAGE%)

	TEXT$ = SPACE$(22%) + AR_35CUSTOM::ADD1 + " " + &
		AR_35CUSTOM::ADD2 + " " + &
		AR_35CUSTOM::CITY + " " + &
		AR_35CUSTOM::STATE + " " + &
		PRNT_PHONE(AR_35CUSTOM::PHONE, 0%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	FOR Y% = 1% TO YEARS%

		GOTO CustVar IF Y% <> 1% AND Y% = YEARS% AND &
			PRINT_VARIANCE$ = "Y"

		TEXT$ = SPACE$(25%) + ":   "
		TEXT$ = "Year " + YYYY$(Y%) + TEXT$ + TEXT$ + TEXT$ + TEXT$

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		TEXT$ = "Sales " + &
			FORMAT$(CUSTPRICEAMT(Y%, 1%), "#######") + &
			FORMAT$(CUSTPRICEAMT(Y%, 2%), "#######") + &
			FORMAT$(CUSTPRICEAMT(Y%, 3%), "#######") + &
			FORMAT$(CUSTPRCQTR(Y%, 1%), "########") + &
			FORMAT$(CUSTPRICEAMT(Y%, 4%), "#######") + &
			FORMAT$(CUSTPRICEAMT(Y%, 5%), "#######") + &
			FORMAT$(CUSTPRICEAMT(Y%, 6%), "#######") + &
			FORMAT$(CUSTPRCQTR(Y%, 2%), "########") + &
			FORMAT$(CUSTPRICEAMT(Y%, 7%), "#######") + &
			FORMAT$(CUSTPRICEAMT(Y%, 8%), "#######") + &
			FORMAT$(CUSTPRICEAMT(Y%, 9%), "#######") + &
			FORMAT$(CUSTPRCQTR(Y%, 3%), "########") + &
			FORMAT$(CUSTPRICEAMT(Y%, 10%), "#######") + &
			FORMAT$(CUSTPRICEAMT(Y%, 11%), "#######") + &
			FORMAT$(CUSTPRICEAMT(Y%, 12%), "#######") + &
			FORMAT$(CUSTPRCQTR(Y%, 4%), "########") + &
			FORMAT$(CUSTPRCYTD(Y%),  "########")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		IF GROSS_PROFIT$ = "Y"
		THEN
			TEXT$ = "GProf " + &
				FORMAT$(CUSTPRICEAMT(Y%, 1%) - &
					CUSTCOSTAMT(Y%, 1%), "#######") + &
				FORMAT$(CUSTPRICEAMT(Y%, 2%) - &
					CUSTCOSTAMT(Y%, 2%), "#######") + &
				FORMAT$(CUSTPRICEAMT(Y%, 3%) - &
					CUSTCOSTAMT(Y%, 3%), "#######") + &
				FORMAT$(CUSTPRCQTR(Y%, 1%) - &
					CUSTCSTQTR(Y%, 1%), "########") + &
				FORMAT$(CUSTPRICEAMT(Y%, 4%) - &
					CUSTCOSTAMT(Y%, 4%), "#######") + &
				FORMAT$(CUSTPRICEAMT(Y%, 5%) - &
					CUSTCOSTAMT(Y%, 5%), "#######") + &
				FORMAT$(CUSTPRICEAMT(Y%, 6%) - &
					CUSTCOSTAMT(Y%, 6%), "#######") + &
				FORMAT$(CUSTPRCQTR(Y%, 2%) - &
					CUSTCSTQTR(Y%, 2%), "########") + &
				FORMAT$(CUSTPRICEAMT(Y%, 7%) - &
					CUSTCOSTAMT(Y%, 7%), "#######") + &
				FORMAT$(CUSTPRICEAMT(Y%, 8%) - &
					CUSTCOSTAMT(Y%, 8%), "#######") + &
				FORMAT$(CUSTPRICEAMT(Y%, 9%) - &
					CUSTCOSTAMT(Y%, 9%), "#######") + &
				FORMAT$(CUSTPRCQTR(Y%, 3%) - &
					CUSTCSTQTR(Y%, 3%), "########") + &
				FORMAT$(CUSTPRICEAMT(Y%, 10%) - &
					CUSTCOSTAMT(Y%, 10%), "#######") + &
				FORMAT$(CUSTPRICEAMT(Y%, 11%) - &
					CUSTCOSTAMT(Y%, 11%), "#######") + &
				FORMAT$(CUSTPRICEAMT(Y%, 12%) - &
					CUSTCOSTAMT(Y%, 12%), "#######") + &
				FORMAT$(CUSTPRCQTR(Y%, 4%) - &
					CUSTCSTQTR(Y%, 4%), "########") + &
				FORMAT$(CUSTPRCYTD(Y%) - &
					CUSTCSTYTD(Y%),  "########")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 1%)

		END IF

 CustVar:
		IF PRINT_VARIANCE$ = "Y" AND Y% <> YEARS%
		THEN
			FOR I% = 1% TO 12%

				IF CUSTPRICEAMT(Y% + 1%, I%) = 0.0
				THEN
					VARIANCE(I%) = 100.00
					VARIANCE(I%) = 0.0 &
						IF CUSTPRICEAMT(Y%, I%) = 0.0
				ELSE
					VARIANCE(I%) = (CUSTPRICEAMT(Y%, I%) - &
						CUSTPRICEAMT(Y% + 1%, I%)) / &
						CUSTPRICEAMT(Y% + 1%, I%) * 100.0
				END IF

			NEXT I%

			FOR I% = 13% TO 16%

				IF CUSTPRCQTR(Y% + 1%, I% - 12%) = 0.0
				THEN
					VARIANCE(I%) = 100.0
					VARIANCE(I%) = 0.0 &
						IF CUSTPRCQTR(Y%, I% - 12%) = 0.0
				ELSE
					VARIANCE(I%) = &
						(CUSTPRCQTR(Y%, I% - 12%) - &
						CUSTPRCQTR(Y% + 1%, I% - 12%)) / &
						CUSTPRCQTR(Y% + 1%, I% - 12%) * 100.0
				END IF

			NEXT I%

			IF CUSTPRCYTD(Y% + 1%) = 0.0
			THEN
				VARIANCE(17%) = 100.00
				VARIANCE(17%) = 0.0 IF CUSTPRCYTD(Y%) = 0.0
			ELSE
				VARIANCE(17%) = (CUSTPRCYTD(Y%) - &
					CUSTPRCYTD(Y% + 1%)) / &
					CUSTPRCYTD(Y% + 1%) * 100.0
			END IF

			TEXT$ = "SVar  " + &
				FORMAT$(VARIANCE(1%), "######%")   + &
				FORMAT$(VARIANCE(2%), "######%")   + &
				FORMAT$(VARIANCE(3%), "######%")   + &
				FORMAT$(VARIANCE(13%), "#######%") + &
				FORMAT$(VARIANCE(4%), "######%")   + &
				FORMAT$(VARIANCE(5%), "######%")   + &
				FORMAT$(VARIANCE(6%), "######%")   + &
				FORMAT$(VARIANCE(14%), "#######%") + &
				FORMAT$(VARIANCE(7%), "######%")   + &
				FORMAT$(VARIANCE(8%), "######%")   + &
				FORMAT$(VARIANCE(9%), "######%")   + &
				FORMAT$(VARIANCE(15%), "#######%") + &
				FORMAT$(VARIANCE(10%), "######%")  + &
				FORMAT$(VARIANCE(11%), "######%")  + &
				FORMAT$(VARIANCE(12%), "######%")  + &
				FORMAT$(VARIANCE(16%), "#######%") + &
				FORMAT$(VARIANCE(17%),  "#######%")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 1%)

			FOR I% = 1% TO 12%

				IF CUSTPRICEAMT(Y% + 1%, I%) - &
					CUSTCOSTAMT(Y% + 1%, I%) = 0.0
				THEN
					VARIANCE(I%) = 100.00
					VARIANCE(I%) = 0.0 &
						IF CUSTPRICEAMT(Y%, I%) - &
						CUSTCOSTAMT(Y%, I%) = 0.0
				ELSE
					VARIANCE(I%) = (CUSTPRICEAMT(Y%, I%) - &
						CUSTCOSTAMT(Y%, I%) - &
						(CUSTPRICEAMT(Y% + 1%, I%) - &
						CUSTCOSTAMT(Y% + 1%, I%))) / &
						(CUSTPRICEAMT(Y% + 1%, I%) - &
						CUSTCOSTAMT(Y% + 1%, I%)) * 100.0
				END IF

			NEXT I%

			FOR I% = 13% TO 16%

				IF CUSTPRCQTR(Y% + 1%, I% - 12%) - &
					CUSTCSTQTR(Y% + 1%, I% - 12%) = 0.0
				THEN
					VARIANCE(I%) = 100.0
					VARIANCE(I%) = 0.0 &
						IF CUSTPRCQTR(Y%, I% - 12%) - &
						CUSTCSTQTR(Y%, I% - 12%) = 0.0
				ELSE
					VARIANCE(I%) = &
						(CUSTPRCQTR(Y%, I% - 12%) - &
						CUSTCSTQTR(Y%, I% - 12%) - &
						(CUSTPRCQTR(Y% + 1%, I% - 12%) - &
						CUSTCSTQTR(Y% + 1%, I% - 12%))) / &
						(CUSTPRCQTR(Y% + 1%, I% - 12%) - &
						CUSTCSTQTR(Y% + 1%, I% - 12%)) * 100.0
				END IF

			NEXT I%

			IF CUSTPRCYTD(Y% + 1%) - CUSTCSTYTD(Y% + 1%) = 0.0
			THEN
				VARIANCE(17%) = 100.00
				VARIANCE(17%) = 0.0 &
					IF CUSTPRCYTD(Y%) - CUSTCSTYTD(Y%) = 0.0
			ELSE
				VARIANCE(17%) = &
					(CUSTPRCYTD(Y%) - CUSTCSTYTD(Y%) - &
					(CUSTPRCYTD(Y% + 1%) - &
					CUSTCSTYTD(Y% + 1%))) / &
					(CUSTPRCYTD(Y% + 1%) - &
					CUSTCSTYTD(Y% + 1%)) * 100.0
			END IF

			TEXT$ = "GPVar " + &
				FORMAT$(VARIANCE(1%), "######%")   + &
				FORMAT$(VARIANCE(2%), "######%")   + &
				FORMAT$(VARIANCE(3%), "######%")   + &
				FORMAT$(VARIANCE(13%), "#######%") + &
				FORMAT$(VARIANCE(4%), "######%")   + &
				FORMAT$(VARIANCE(5%), "######%")   + &
				FORMAT$(VARIANCE(6%), "######%")   + &
				FORMAT$(VARIANCE(14%), "#######%") + &
				FORMAT$(VARIANCE(7%), "######%")   + &
				FORMAT$(VARIANCE(8%), "######%")   + &
				FORMAT$(VARIANCE(9%), "######%")   + &
				FORMAT$(VARIANCE(15%), "#######%") + &
				FORMAT$(VARIANCE(10%), "######%")  + &
				FORMAT$(VARIANCE(11%), "######%")  + &
				FORMAT$(VARIANCE(12%), "######%")  + &
				FORMAT$(VARIANCE(16%), "#######%") + &
				FORMAT$(VARIANCE(17%),  "#######%")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 1%) &
				IF GROSS_PROFIT$ = "Y"

		END IF

	NEXT Y%

	FOR Y% = 1% TO YEARS%

		FOR I% = 1% TO 12%

			TOTALPRICEAMT(Y%, I%) = TOTALPRICEAMT(Y%, I%) + &
				CUSTPRICEAMT(Y%, I%)
			TOTALCOSTAMT(Y%, I%) = TOTALCOSTAMT(Y%, I%) + &
				CUSTCOSTAMT(Y%, I%)

		NEXT I%

	NEXT Y%

	FOR Y% = 1% TO YEARS%

		TOTALPRCYTD(Y%)  = TOTALPRCYTD(Y%)  + CUSTPRCYTD(Y%)
		TOTALCSTYTD(Y%)  = TOTALCSTYTD(Y%)  + CUSTCSTYTD(Y%)

		CUSTPRCYTD(Y%)  = 0.0
		CUSTCSTYTD(Y%)  = 0.0

		FOR I% = 1% TO 4%

			TOTALPRCQTR(Y%, I%) = TOTALPRCQTR(Y%, I%) + &
				CUSTPRCQTR(Y%, I%)
			TOTALCSTQTR(Y%, I%) = TOTALCSTQTR(Y%, I%) + &
				CUSTCSTQTR(Y%, I%)

			CUSTPRICEAMT(Y%, I%) = 0.0
			CUSTCOSTAMT(Y%, I%) = 0.0

			CUSTPRICEAMT(Y%, I% + 4%)  = 0.0
			CUSTCOSTAMT(Y%, I% + 4%) = 0.0

			CUSTPRICEAMT(Y%, I% + 8%)  = 0.0
			CUSTCOSTAMT(Y%, I% + 8%) = 0.0

			CUSTPRCQTR(Y%, I%) = 0.0
			CUSTCSTQTR(Y%, I%) = 0.0

		NEXT I%

	NEXT Y%

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
	RESUME HelpError

32767	!******************************************************************
	! End of report SA_RPRT_CUSPRDSAL2
	!******************************************************************
	END
