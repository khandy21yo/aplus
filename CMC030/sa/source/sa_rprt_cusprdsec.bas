1	%TITLE "Sales by Customer and Product Secondary Code"
	%SBTTL "SA_RPRT_CUSPRDSEC"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1993 BY
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
	! ID:SA0009
	!
	! Abstract:HELP
	!	.p
	!	The ^*Sales by Customer and Product Report\* option
	!	prints a sales report listing the following
	!	information:
	!	.b
	!	.lm +10
	!	.list 0,"*"
	!	.le
	!	Customer Number
	!	.le
	!	Customer Name
	!	.le
	!	Customer Type
	!	.le
	!	Customer Category
	!	.le
	!	Customer Address
	!	.le
	!	Product Number
	!	.le
	!	Product Description
	!	.le
	!	Salesman Number
	!	.le
	!	Salesman Name
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
	!	$ BAS SA_SOURCE:SA_RPRT_CUSPRDSEC/LINE
	!	$ LINK/EXECUTABLE=SA_EXE: SA_RPRT_CUSPRDSEC, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE SA_RPRT_CUSPRDSEC.OBJ;*
	!
	! Author:
	!
	!	03/22/93 - Dan Perkins
	!
	! Modification history:
	!
	!	03/25/93 - Dan Perkins
	!		Modified dimension for PARRAY from 500 to 1500.
	!		Program crashed on ROBI system.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	06/07/96 - Kevin Handy
	!		Lose commented out code.
	!		Reformat source code.
	!		Lose code for commented out print commands.
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
	!		Lose excessive %PAGE's
	!
	!	11/29/2000 - Kevin Handy
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
	DECLARE			IC_35HISTORY_CDD	PARRAY(10%, 1500%)

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
	!
	!	^*(01) Sort by (N,T,C,A)\*
	!	.p
	!	The ^*Sort by\* field determines which order
	!	the report will be printed.
	!	.p
	!	Valid settings are:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	N - Customer Number
	!	.le
	!	T - Customer Type
	!	.le
	!	C - Customer Category
	!	.le
	!	A - Customer Alpha Sort
	!	.els
	!	.lm -10
	!	.p
	!	A setting is required in this field.  No other settings are
	!	valid.
	!
	! Index:
	!	.x Sort by
	! Datatype:TEXT
	! Size:1
	! Valid Input: N,T,C,A
	! Required
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02)From Item\*
	!	.p
	!	The ^*From Item\* field enters the
	!	item with which to begin the report.
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
	!	using the Wildcarding Technique.  All locations will be
	!	included if this field is left blank.
	!
	!
	! Index:
	!
	!--

	PRINT_UNITS$ = EDIT$(UTL_REPORTX::OPTDEF(5%), -1%)

	!++
	! Abstract:FLD06
	!	^*(06) Print Units\*
	!	.p
	!	The ^*Print Unit\* field
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
	!	The ^*Print Gross Profit\* field
	!	shows the gross profit.
	!
	! Index:
	!
	!--

	PRINT_VARIANCE$ = EDIT$(UTL_REPORTX::OPTDEF(7%), -1%)

	!++
	! Abstract:FLD08
	!	^*(08) Print Variance\*
	!	.p
	!	The ^*Print Variance\* field shows the
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
	!	prints the number of fiscal years. If
	!	the selected inventory transaction history file doesn't
	!	exist, report will not search for the older years.
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
		TITLE$(1%) = "UNIT SALES BY CUSTOMER NUMBER"

	CASE "T"
		K_NUM% = 1%
		TITLE$(1%) = "UNIT SALES BY CUSTOMER TYPE"

	CASE "C"
		K_NUM% = 2%
		TITLE$(1%) = "UNIT SALES BY CUSTOMER CATEGORY"

	CASE "A"
		K_NUM% = 3%
		TITLE$(1%) = "UNIT SALES BY CUSTOMER ALPHA SORT"

	END SELECT

	TITLE$(2%) = "Sales Analysis System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "     Product#           Description              " + &
		"                    SecondCode#"

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

	NEW.CUSTOMER% = -1%
	PLOOP% = 0%

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

		EOF%(Y%) = 1% IF IC_HISTORY(Y%)::CROSSREF <> AR_35CUSTOM::CUSNUM
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

			GOTO GetHistRec IF SA_EXAM_SALESMAN( &
				IC_HISTORY(Y%)::SUBACCT, SA_SALESMAN_EXAM, &
				SB_SUBACCOUNT_EXAM) <> CMC$_NORMAL
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
 !			LAST_SALESMAN$ = IC_HISTORY(Y%)::SUBACCT

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
			PRICEAMT(Y%, I%) = PRICEAMT(Y%, I%) + &
				IC_HISTORY(Y%)::PRICEAMT(I%)
			COSTAMT(Y%, I%) = COSTAMT(Y%, I%) + &
				IC_HISTORY(Y%)::COSTAMT(I%)

			J% = INT((I% - 1%) / 3%) + 1%

			QTYQTR(Y%, J%) = QTYQTR(Y%, J%) - &
				IC_HISTORY(Y%)::PQUANTITY(I%)
			PRCQTR(Y%, J%) = PRCQTR(Y%, J%) + &
				IC_HISTORY(Y%)::PRICEAMT(I%)
			CSTQTR(Y%, J%) = CSTQTR(Y%, J%) + &
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

	IF NEW.CUSTOMER%
	THEN
		NEW.CUSTOMER%, PAGE% = 0%
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


	FOR Y% = 1% TO YEARS%

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
					VARIANCE(I%) = (TOTALPRCQTR(Y%, I% - 12%) - &
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
					VARIANCE(I%) = (TOTALPRCQTR(Y%, I% - 12%) - &
						TOTALCSTQTR(Y%, I% - 12%) - &
						(TOTALPRCQTR(Y% + 1%, I%  -12%) - &
						TOTALCSTQTR(Y% + 1%, I% - 12%))) / &
						(TOTALPRCQTR(Y% + 1%, I% - 12%) - &
						TOTALCSTQTR(Y% + 1%, I% - 12%)) * 100.0
				END IF

			NEXT I%

			IF TOTALPRCYTD(Y% + 1%) - TOTALCSTYTD(Y% + 1%) = 0.0
			THEN
				VARIANCE(17%) = 100.00
				VARIANCE(17%) = 0.0 IF TOTALPRCYTD(Y%) - TOTALCSTYTD(Y%) = 0.0
			ELSE
				VARIANCE(17%) = (TOTALPRCYTD(Y%) - TOTALCSTYTD(Y%) - &
					(TOTALPRCYTD(Y% + 1%) - TOTALCSTYTD(Y% + 1%))) / &
					(TOTALPRCYTD(Y% + 1%) - TOTALCSTYTD(Y% + 1%)) * 100.0
			END IF

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
	V% = PD_EXAM_PRODUCT(LAST_PRODUCT$, PD_PRODUCT_EXAM)

	!
	! Print out one line
	!
	PLOOP% = PLOOP% + 1%

	FOR Y% = 1% TO YEARS%

		GOTO ProdVar IF Y% <> 1% AND Y% = YEARS% AND &
			PRINT_VARIANCE$ = "Y"

		IF PRINT_UNITS$ = "Y" AND (Y% = 1% OR Y% > 1% AND &
			PRINT_VARIANCE$ <> "Y")
		THEN
			!
			! Create one element of the array
			!
			PARRAY(Y%, PLOOP%)::PRODUCT	= LAST_PRODUCT$
			PARRAY(Y%, PLOOP%)::SUBACCT	= PD_PRODUCT_EXAM::SECONDARY_CODE

			FOR I% = 1% TO 12%

				PARRAY(Y%, PLOOP%)::PQUANTITY(I%) = &
					PQUANTITY(Y%, I%)
				PARRAY(Y%, PLOOP%)::PRICEAMT(I%) = &
					PRICEAMT(Y%, I%)
				PARRAY(Y%, PLOOP%)::COSTAMT(I%) = &
					COSTAMT(Y%, I%)

			NEXT I%

		END IF

 ProdVar:
		IF PRINT_VARIANCE$ = "Y" AND Y% <> YEARS%
		THEN
			FOR I% = 1% TO 12%

				IF PQUANTITY(Y% + 1%, I%) = 0.0
				THEN
					VARIANCE(I%) = 100.00
					VARIANCE(I%) = 0.0 IF PQUANTITY(Y%, I%) = 0.0
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
				VARIANCE(17%) = (QTYYTD(Y%) - &
					QTYYTD(Y% + 1%)) / QTYYTD(Y% + 1%) * 100.0
			END IF

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

			FOR I% = 1% TO 12%

				IF PRICEAMT(Y% + 1%, I%) - COSTAMT(Y% + 1%, I%) = 0.0
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
				VARIANCE(17%) = 0.0 IF PRCYTD(Y%) - CSTYTD(Y%) = 0.0
			ELSE
				VARIANCE(17%) = (PRCYTD(Y%) - CSTYTD(Y%) - &
					(PRCYTD(Y% + 1%) - CSTYTD(Y% + 1%))) / &
					(PRCYTD(Y% + 1%) - CSTYTD(Y% + 1%)) * 100.0
			END IF

		END IF

	NEXT Y%

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FOR Y% = 1% TO YEARS%

		FOR I% = 1% TO 12%

			CUSTPRICEAMT(Y%, I%) = CUSTPRICEAMT(Y%, I%) + &
				PRICEAMT(Y%, I%)
			CUSTCOSTAMT(Y%, I%) = CUSTCOSTAMT(Y%, I%) + &
				COSTAMT(Y%, I%)

		NEXT I%

		FOR I% = 1% TO 4%

			CUSTPRCQTR(Y%, I%) = CUSTPRCQTR(Y%, I%) + PRCQTR(Y%, I%)
			CUSTCSTQTR(Y%, I%) = CUSTCSTQTR(Y%, I%) + CSTQTR(Y%, I%)

		NEXT I%

		CUSTPRCYTD(Y%) = CUSTPRCYTD(Y%) + PRCYTD(Y%)
		CUSTCSTYTD(Y%) = CUSTCSTYTD(Y%) + CSTYTD(Y%)

	NEXT Y%

	PRINT_FLAG% = 0%

	FOR Y% = 1% TO YEARS%

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
			PRICEAMT(Y%, I% + 4%) = 0.0
			COSTAMT(Y%, I% + 4%) = 0.0

			PQUANTITY(Y%, I% + 8%) = 0.0
			PRICEAMT(Y%, I% + 8%) = 0.0
			COSTAMT(Y%, I% + 8%) = 0.0

		NEXT I%

	NEXT Y%

	RETURN

 CustTotal:
	PAGE% = 999% IF FIRST_PAGE%
	FIRST_PAGE% = -1%
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
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	!
	! Sort the array
	!
	FOR J% = 1% TO PLOOP% - 1%
		FOR P% = J% TO PLOOP%
			IF PARRAY(1%, P%)::SUBACCT < PARRAY(1%, J%)::SUBACCT
			THEN
				PARRAY(1%, 0%) = PARRAY(1%, J%)
				PARRAY(2%, 0%) = PARRAY(2%, J%)
				PARRAY(1%, J%) = PARRAY(1%, P%)
				PARRAY(2%, J%) = PARRAY(2%, P%)
				PARRAY(1%, P%) = PARRAY(1%, 0%)
				PARRAY(2%, P%) = PARRAY(2%, 0%)
			END IF
		NEXT P%
	NEXT J%

	!
	! Print the array
	!
	FOR P% = 1% TO PLOOP%

		FOR Y% = 1% TO YEARS%

			IF Y% = 1%
			THEN
				V% = PD_EXAM_PRODUCT(PARRAY(Y%, P%)::PRODUCT, &
					PD_PRODUCT_EXAM)

				TEXT$ = "     " + PARRAY(Y%, P%)::PRODUCT + "     " + &
					PD_PRODUCT_EXAM::DESCRIPTION + "     " + &
					PARRAY(Y%, P%)::SUBACCT

				CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
			END IF

			TEXT$ = SPACE$(25%) + ":   "
			TEXT$ = "Year " + YYYY$(Y%) + TEXT$ + TEXT$ + TEXT$ + TEXT$

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

			FOR J% = 1% TO YEARS%

				QTYYTD(J%) = 0.0
				PRCYTD(J%) = 0.0
				CSTYTD(J%) = 0.0

				FOR I% = 1% TO 4%

					QTYQTR(J%, I%) = 0.0
					PRCQTR(J%, I%) = 0.0
					CSTQTR(J%, I%) = 0.0

				NEXT I%

			NEXT J%

			FOR I% = 1% TO 12%

				J% = INT((I% - 1%) / 3%) + 1%

				QTYQTR(Y%, J%) = QTYQTR(Y%, J%) + &
					PARRAY(Y%, P%)::PQUANTITY(I%)
				PRCQTR(Y%, J%) = PRCQTR(Y%, J%) + &
					PARRAY(Y%, P%)::PRICEAMT(I%)
				CSTQTR(Y%, J%) = CSTQTR(Y%, J%) + &
					PARRAY(Y%, P%)::COSTAMT(I%)
				QTYYTD(Y%) = QTYYTD(Y%) + &
					PARRAY(Y%, P%)::PQUANTITY(I%)
				PRCYTD(Y%) = PRCYTD(Y%) + &
					PARRAY(Y%, P%)::PRICEAMT(I%)
				CSTYTD(Y%) = CSTYTD(Y%) + &
					PARRAY(Y%, P%)::COSTAMT(I%)

			NEXT I%

			TEXT$ = "Units "                              + &
				FORMAT$(PARRAY(Y%, P%)::PQUANTITY(1%), "#######")  + &
				FORMAT$(PARRAY(Y%, P%)::PQUANTITY(2%), "#######")  + &
				FORMAT$(PARRAY(Y%, P%)::PQUANTITY(3%), "#######")  + &
				FORMAT$(QTYQTR(Y%, 1%), "########")    + &
				FORMAT$(PARRAY(Y%, P%)::PQUANTITY(4%), "#######")  + &
				FORMAT$(PARRAY(Y%, P%)::PQUANTITY(5%), "#######")  + &
				FORMAT$(PARRAY(Y%, P%)::PQUANTITY(6%), "#######")  + &
				FORMAT$(QTYQTR(Y%, 2%), "########")    + &
				FORMAT$(PARRAY(Y%, P%)::PQUANTITY(7%), "#######")  + &
				FORMAT$(PARRAY(Y%, P%)::PQUANTITY(8%), "#######")  + &
				FORMAT$(PARRAY(Y%, P%)::PQUANTITY(9%), "#######")  + &
				FORMAT$(QTYQTR(Y%, 3%), "########")    + &
				FORMAT$(PARRAY(Y%, P%)::PQUANTITY(10%), "#######")  + &
				FORMAT$(PARRAY(Y%, P%)::PQUANTITY(11%), "#######")  + &
				FORMAT$(PARRAY(Y%, P%)::PQUANTITY(12%), "#######")  + &
				FORMAT$(QTYQTR(Y%, 4%), "########")    + &
				FORMAT$(QTYYTD(Y%), "########")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		NEXT Y%

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	NEXT P%

	FOR Y% = 1% TO YEARS%

		GOTO CustVar IF Y% <> 1% AND Y% = YEARS% AND &
			PRINT_VARIANCE$ = "Y"

 CustVar:
		IF PRINT_VARIANCE$ = "Y" AND Y% <> YEARS%
		THEN
			FOR I% = 1% TO 12%

				IF CUSTPRICEAMT(Y% + 1%, I%) = 0.0
				THEN
					VARIANCE(I%) = 100.00
					VARIANCE(I%) = 0.0 IF CUSTPRICEAMT(Y%, I%) = 0.0
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
					VARIANCE(I%) = (CUSTPRCQTR(Y%, I% - 12%) - &
						CUSTPRCQTR(Y% + 1%, I% - 12%)) / &
						CUSTPRCQTR(Y% + 1%, I% - 12%) * 100.0
				END IF

			NEXT I%

			IF CUSTPRCYTD(Y% + 1%) = 0.0
			THEN
				VARIANCE(17%) = 100.00
				VARIANCE(17%) = 0.0 IF CUSTPRCYTD(Y%) = 0.0
			ELSE
				VARIANCE(17%) = (CUSTPRCYTD(Y%) - CUSTPRCYTD(Y% + 1%)) / &
					CUSTPRCYTD(Y% + 1%) * 100.0
			END IF

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
					VARIANCE(I%) = (CUSTPRCQTR(Y%, I% - 12%) - &
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
				VARIANCE(17%) = 0.0 IF CUSTPRCYTD(Y%) - CUSTCSTYTD(Y%) = 0.0
			ELSE
				VARIANCE(17%) = (CUSTPRCYTD(Y%) - CUSTCSTYTD(Y%) - &
					(CUSTPRCYTD(Y% + 1%) - CUSTCSTYTD(Y% + 1%))) / &
					(CUSTPRCYTD(Y% + 1%) - CUSTCSTYTD(Y% + 1%)) * 100.0
			END IF

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

		TOTALPRCYTD(Y%) = TOTALPRCYTD(Y%)  + CUSTPRCYTD(Y%)
		TOTALCSTYTD(Y%) = TOTALCSTYTD(Y%)  + CUSTCSTYTD(Y%)

		CUSTPRCYTD(Y%) = 0.0
		CUSTCSTYTD(Y%) = 0.0

		FOR I% = 1% TO 4%

			TOTALPRCQTR(Y%, I%) = TOTALPRCQTR(Y%, I%) + &
				CUSTPRCQTR(Y%, I%)
			TOTALCSTQTR(Y%, I%) = TOTALCSTQTR(Y%, I%) + &
				CUSTCSTQTR(Y%, I%)

			CUSTPRICEAMT(Y%, I%) = 0.0
			CUSTCOSTAMT(Y%, I%) = 0.0

			CUSTPRICEAMT(Y%, I% + 4%) = 0.0
			CUSTCOSTAMT(Y%, I% + 4%) = 0.0

			CUSTPRICEAMT(Y%, I% + 8%) = 0.0
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
	FILENAME$ = ""
	RESUME HelpError

32767	!******************************************************************
	! End of report SA_RPRT_CUSPRDSEC
	!******************************************************************
	END
