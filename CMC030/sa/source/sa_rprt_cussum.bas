1	%TITLE "Sales Summary Report by Customer"
	%SBTTL "SA_RPRT_CUSSUM"
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
	! ID:SA0014
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Sales Summary by Customer Report\* option
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
	!	$ BAS SA_SOURCE:SA_RPRT_CUSSUM/LINE
	!	$ LINK/EXECUTABLE=SA_EXE: SA_RPRT_CUSSUM, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE SA_RPRT_CUSSUM.OBJ;*
	!
	! Author:
	!
	!	01/10/92 - Dan Perkins
	!
	! Modification history:
	!
	!	01/20/92 - Dan Perkins
	!		Offset Grand Total output so large numbers
	!		would be more readable.
	!
	!	02/04/92 - Kevin Handy
	!		Cleaned out junk (check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/13/96 - Kevin Handy
	!		Reformat source code.
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
	!	11/15/2000 - Kevin Handy
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

	!
	! Array for variances
	!
	DECLARE REAL VARIANCE(17%)

	!
	! Used in CustTotal
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
	!	The ^*To Item\* field enters the item with
	!	which the report is to end printing.
	!	.b
	!	A blank field will cause the report to end with the last
	!	item in the file.
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
	!	The ^*Wildcard\* field selects
	!	designated items to be printed by entering a "wildcard"
	!	using the Wildcarding Technique.
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

	TOTAL_ONLY$ = EDIT$(UTL_REPORTX::OPTDEF(5%), -1%)

	!++
	! Abstract:FLD06
	!	^*(06) Print Only Total\*
	!	.b
	!	.lm +5
	!	The ^*Print Only Total\* field
	!	shows only customer totals on the report.
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
	!	The ^*Print Gross Profit\* field
	!	shows the gross profit on the report.
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
	!	The ^*Print Variance\* field provides the option to show the
	!	yearly unit, sales, and gross profit variances on the report.
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
	!	The ^*Fiscal Year\* field provides the option to
	!	select the fiscal year with which the report is to begin.
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
	!	The ^*Number of Years\* field
	!	prints only selected fiscal years. If
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
	CASE "N"
		K_NUM% = 0%
		TITLE$(1%) = "SALES SUMMARY BY CUSTOMER NUMBER"

	CASE "T"
		K_NUM% = 1%
		TITLE$(1%) = "SALES SUMMARY BY CUSTOMER TYPE"

	CASE "C"
		K_NUM% = 2%
		TITLE$(1%) = "SALES SUMMARY BY CUSTOMER CATEGORY"

	CASE "A"
		K_NUM% = 3%
		TITLE$(1%) = "SALES SUMMARY BY CUSTOMER ALPHA SORT"

	END SELECT

	TITLE$(2%) = "Sales Analysis System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = SPACE$(12%) + "Customer#   Name" + SPACE$(48%) + &
		"CType  Category"

	TITLE$(5%) = "                          Address1        " + &
		"           Address2                   " + &
		"City             State  Phone"

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

	PAGE%, PRINT_FLAG% = 0%

 GetNextRec:
	!
	! Print totals after each customer
	!
	IF PRINT_FLAG%
	THEN
		GOSUB CustTotal
	END IF

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

		GOTO GetNextRec IF COMP_STRING(EDIT$(AR_35CUSTOM::CUSNUM, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "T"
		GOTO ExitTotal IF (AR_35CUSTOM::TTYPE > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$(AR_35CUSTOM::TTYPE, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "C"
		GOTO ExitTotal IF (AR_35CUSTOM::CATEGORY > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$(AR_35CUSTOM::CATEGORY, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "A"
		GOTO ExitTotal IF (AR_35CUSTOM::ALPSRT > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$(AR_35CUSTOM::ALPSRT, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	END SELECT

17100	!
	! Get History record
	!
	FOR Y% = 1% TO YEARS%

		EOF%(Y%) = 0%
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
		IF EOF%(Y%) <> 1%
		THEN
			GOTO GetHistRec IF IC_HISTORY(Y%)::TRANSTYPE <> "SA"

			GOTO GetHistRec IF COMP_STRING(EDIT$(IC_HISTORY(Y%)::LOCATION, -1%), LOC_WLDCRD$) = 0% &
				AND LOC_WLDCRD$ <> ""

			GOTO GetHistRec IF PD_EXAM_PRODUCT(IC_HISTORY(Y%)::PRODUCT, PD_PRODUCT_EXAM) <> CMC$_NORMAL
		END IF

	NEXT Y%

	FOR Y% = 1% TO YEARS%

		IF EOF%(Y%) = 0%
		THEN
			FOR I% = 1% TO 12%

				CUSTPRICEAMT(Y%, I%) = CUSTPRICEAMT(Y%, I%) + &
					IC_HISTORY(Y%)::PRICEAMT(I%)
				CUSTCOSTAMT(Y%, I%) = CUSTCOSTAMT(Y%, I%) + &
					IC_HISTORY(Y%)::COSTAMT(I%)
				J% = INT((I% - 1%) / 3%) + 1%
				CUSTPRCQTR(Y%, J%) = CUSTPRCQTR(Y%, J%) + &
					IC_HISTORY(Y%)::PRICEAMT(I%)
				CUSTCSTQTR(Y%, J%) = CUSTCSTQTR(Y%, J%) + &
					IC_HISTORY(Y%)::COSTAMT(I%)
				CUSTPRCYTD(Y%) = CUSTPRCYTD(Y%) + &
					IC_HISTORY(Y%)::PRICEAMT(I%)
				CUSTCSTYTD(Y%) = CUSTCSTYTD(Y%) + &
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
	FOR I% = 4% TO 7%
		TITLE$(I%) = TITLE$(I% + 2%)
	NEXT I%

	TEXT$ = "     Report Grand Total"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, PAGE%)

	FOR Y% = 1% TO YEARS%

		TEXT$ = SPACE$(25%) + ":   "
		TEXT$ = "Year " + YYYY$(Y%) + TEXT$ + TEXT$ + TEXT$ + TEXT$

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

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
				FORMAT$(TOTALPRCYTD(Y%) - TOTALCSTYTD(Y%),  &
				"########")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

			TEXT$ = "      " + &
				SPACE$(7%) + &
				FORMAT$(TOTALPRICEAMT(Y%, 2%) - &
				TOTALCOSTAMT(Y%, 2%), "#######") + &
				SPACE$(7%) + &
				FORMAT$(TOTALPRCQTR(Y%, 1%) - &
				TOTALCSTQTR(Y%, 1%), "########") + &
				SPACE$(7%) + &
				FORMAT$(TOTALPRICEAMT(Y%, 5%) - &
				TOTALCOSTAMT(Y%, 5%), "#######") + &
				SPACE$(7%) + &
				FORMAT$(TOTALPRCQTR(Y%, 2%) - &
				TOTALCSTQTR(Y%, 2%), "########") + &
				SPACE$(7%) + &
				FORMAT$(TOTALPRICEAMT(Y%, 8%) - &
				TOTALCOSTAMT(Y%, 8%), "#######") + &
				SPACE$(7%) + &
				FORMAT$(TOTALPRCQTR(Y%, 3%) - &
				TOTALCSTQTR(Y%, 3%), "########") + &
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
					VARIANCE(I%) = 0.0 IF TOTALPRICEAMT(Y%, I%) = 0.0
				ELSE
					VARIANCE(I%) = (TOTALPRICEAMT(Y%, I%) - &
						TOTALPRICEAMT(Y% + 1%, I%)) / &
						TOTALPRICEAMT(Y% + 1%, I%) * 100.
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
						TOTALPRCQTR(Y% + 1%, I% - 12%) * 100.
				END IF
			NEXT I%

			IF TOTALPRCYTD(Y% + 1%) = 0.0
			THEN
				VARIANCE(17%) = 100.00
				VARIANCE(17%) = 0.0 IF TOTALPRCYTD(Y%) = 0.0
			ELSE
				VARIANCE(17%) = (TOTALPRCYTD(Y%) - TOTALPRCYTD(Y%+1%)) / TOTALPRCYTD(Y%+1%) * 100.
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
					VARIANCE(I%) = (TOTALPRCQTR(Y%, I% - 12%) - &
						TOTALCSTQTR(Y%, I% - 12%) - &
						(TOTALPRCQTR(Y% + 1%, I% - 12%) - &
						TOTALCSTQTR(Y% + 1%, I% - 12%))) / &
						(TOTALPRCQTR(Y% + 1%, I% - 12%) - &
						TOTALCSTQTR(Y% + 1%, I% - 12%)) * 100.0
				END IF
			NEXT I%

			IF TOTALPRCYTD(Y% + 1%) - TOTALCSTYTD(Y% + 1%) = 0.0
			THEN
				VARIANCE(17%) = 100.00
				VARIANCE(17%) = 0.0 IF TOTALPRCYTD(Y%) - &
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

 CustTotal:
	IF TOTAL_ONLY$ = "Y"
	THEN
		GOTO SkipCust
	ELSE
		PAGE% = 999%
	END IF

	TEXT$ = "::::::::::  " + &
		AR_35CUSTOM::CUSNUM + "  " + &
		AR_35CUSTOM::CUSNAM + "  " + &
		AR_35CUSTOM::TTYPE + "     " + &
		AR_35CUSTOM::CATEGORY

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = SPACE$(26%) + &
		AR_35CUSTOM::ADD1 + "  " + &
		AR_35CUSTOM::ADD2 + "  " + &
		AR_35CUSTOM::CITY + "  " + &
		AR_35CUSTOM::STATE + "     " + &
		PRNT_PHONE(AR_35CUSTOM::PHONE, 0%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	FOR Y% = 1% TO YEARS%

		GOTO CustVar &
			IF Y% <> 1% AND Y% = YEARS% AND PRINT_VARIANCE$ = "Y"

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
				VARIANCE(17%) = (CUSTPRCYTD(Y%) - &
					CUSTPRCYTD(Y% + 1%)) / &
					CUSTPRCYTD(Y% + 1%) * 100.0
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
				VARIANCE(17%) = 0.0 IF CUSTPRCYTD(Y%) - &
					CUSTCSTYTD(Y%) = 0.0
			ELSE
				VARIANCE(17%) = (CUSTPRCYTD(Y%) - &
					CUSTCSTYTD(Y%) - &
					(CUSTPRCYTD(Y% + 1%) - &
					CUSTCSTYTD(Y% + 1%))) / &
					(CUSTPRCYTD(Y% + 1%) - &
					CUSTCSTYTD(Y% + 1%)) * 100.0
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

 SkipCust:
	FOR Y% = 1% TO YEARS%
		FOR I% = 1% TO 12%
			TOTALPRICEAMT(Y%, I%) = &
				TOTALPRICEAMT(Y%, I%) + &
				CUSTPRICEAMT(Y%, I%)
			TOTALCOSTAMT(Y%, I%) = &
				TOTALCOSTAMT(Y%, I%) + &
				CUSTCOSTAMT(Y%, I%)
		NEXT I%
	NEXT Y%

	FOR Y% = 1% TO YEARS%

		TOTALPRCYTD(Y%) = TOTALPRCYTD(Y%) + CUSTPRCYTD(Y%)
		TOTALCSTYTD(Y%) = TOTALCSTYTD(Y%) + CUSTCSTYTD(Y%)

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
	RESUME HelpError

32767	!******************************************************************
	! End of report SA_RPRT_CUSSUM
	!******************************************************************
	END
