1	%TITLE "Sales Summary Report by Salesman"
	%SBTTL "SA_RPRT_SALSUM"
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
	! ID:SA0013
	!
	! Abstract:HELP
	!	.p
	!	The ^*Sales Summary Report by Salesman\* option
	!	prints a sales report listing the following information:
	!	.b
	!	.lm +10
	!	.list 0,"*"
	!	.le
	!	Salesman Number
	!	.le
	!	Salesman Name
	!	.le
	!	Salesman Type
	!	.le
	!	Salesman Class
	!	.le
	!	Salesman Address
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
	!	$ BAS SA_SOURCE:SA_RPRT_SALSUM/LINE
	!	$ LINK/EXECUTABLE=SA_EXE: SA_RPRT_SALSUM, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE SA_RPRT_SALSUM.OBJ;*
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
	!	12/08/92 - Dan Perkins
	!		Print Customer Totals if TOTAL_ONLY$ = "N".
	!		Use COMP_ARRAY instead of COMP_STRING.
	!
	!	12/10/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	06/07/96 - Kevin Handy
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
	!		Lose excess %PAGE
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
	EXTERNAL LONG	FUNCTION AR_EXAM_CUSTOM
	EXTERNAL LONG	FUNCTION PD_EXAM_PRODUCT

	!
	! CDD inclusions
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP (SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT

	%INCLUDE "SOURCE:[SA.OPEN]SA_SALESMAN.HB"
	MAP (SB_SUBACCOUNT)	SA_SALESMAN_CDD		SA_SALESMAN

	%INCLUDE "SOURCE:[IC.OPEN]IC_35HISTORY.HB"
	MAP (IC_35HISTORY)	IC_35HISTORY_CDD	IC_35HISTORY

	DECLARE			IC_35HISTORY_CDD	IC_HISTORY(10%)

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	DECLARE			AR_35CUSTOM_CDD		AR_35CUSTOM_EXAM

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
	! Used in BrokerTotal
	!
	DECLARE REAL BROKERPRICEAMT(10%, 12%)
	DECLARE REAL BROKERCOSTAMT(10%, 12%)

	!
	! Used in ExitTotal, created in BrokerTotal
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
	!	^*(01) Sort by (S,C,T)\*
	!	.p
	!	The ^*Sort by\* field determines the order
	!	to print in.
	!	.p
	!	Valid settings are:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	S - Salesman
	!	.le
	!	C - Class
	!	.le
	!	T - Type
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
	!	The ^*From Item\* field enters the
	!	item to begin with.
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

	TOTAL_ONLY$ = EDIT$(UTL_REPORTX::OPTDEF(5%), -1%)

	!++
	! Abstract:FLD06
	!	^*(06) Print Totals Only\*
	!	.p
	!	The ^*Print Totals Only\* field optionally
	!	prints only salesman totals and report totals if "N" is
	!	entered in this field.  If "Y" is entered, the report will
	!	print customer totals as well as salesman and grand totals.
	!	.p
	!	An entry is required in this field.
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
	!	.p
	!	An entry is required in this field.
	!
	! Index:
	!
	!--

	PRINT_VARIANCE$ = EDIT$(UTL_REPORTX::OPTDEF(7%), -1%)

	!++
	! Abstract:FLD08
	!	^*(08) Print Variance\*
	!	.p
	!	The ^*Print Variance\* field optionally shows the
	!	yearly unit, sales, and gross profit variances.
	!	.p
	!	An entry is required in this field.
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
	!	.p
	!	An entry is required in this field.
	!
	! Index:
	!
	!--

	YEARS% = VAL%(EDIT$(UTL_REPORTX::OPTDEF(9%), -1%))
	YEARS% = 1% IF YEARS% = 0%

	!++
	! Abstract:FLD10
	!	^*(10) Number of Years\*
	!	.p
	!	The ^*Number of Years\* field
	!	prints that number of the fiscal years. If
	!	the selected inventory transaction history file doesn't
	!	exist, report will not search for the older years.
	!	.p
	!	An entry is required in this field.
	!
	! Index:
	!
	!--

	%PAGE

300	!
	! Open Subaccount file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.OPN"
	USE
		FILENAME$ = "SB_SUBACCOUNT"
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

	CASE "S"
		K_NUM% = 0%
		TITLE$(1%) = "SALES SUMMARY BY SALESMAN NUMBER"

	CASE "C"
		K_NUM% = 2%
		TITLE$(1%) = "SALES SUMMARY BY SALESMAN CLASS"

	CASE "T"
		K_NUM% = 1%
		TITLE$(1%) = "SALES SUMMARY BY SALESMAN TYPE"

	END SELECT

	IF LOC_WLDCRD$ = "" OR LOC_WLDCRD$ = "*"
	THEN
		TITLE$(2%) = "FOR ALL LOCATIONS"
	ELSE
		TITLE$(2%) = "FOR LOCATION " + LOC_WLDCRD$
	END IF

	TITLE$(3%) = "Sales Analysis System"
	TITLE$(4%) = ""

	!
	! Heading
	!
	TITLE$(5%) = "             Salesman#    Name" + SPACE$(39%) + &
		"SType   Class"

	TITLE$(6%) = "                            Address1        " + &
		"            Address2                    "     + &
		"City              State   Phone"

	IF TOTAL_ONLY$ = "Y"
	THEN
		TITLE$(7%) = "         Per1   Per2   Per3    Qtr1   Per4"  + &
			"   Per5   Per6    Qtr2   Per7   Per8   Per9" + &
			"    Qtr3  Per10  Per11  Per12    Qtr4     YTD"

		TITLE$(8%) = "."
	ELSE
		TITLE$(7%) = "     Cust #         Customer"

		TITLE$(8%) = "         Per1   Per2   Per3    Qtr1   Per4"  + &
			"   Per5   Per6    Qtr2   Per7   Per8   Per9" + &
			"    Qtr3  Per10  Per11  Per12    Qtr4     YTD"

		TITLE$(9%) = "."
	END IF

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			FIND #SB_SUBACCOUNT.CH%, KEY #K_NUM% GE "S", REGARDLESS
		ELSE
			FIND #SB_SUBACCOUNT.CH%, &
				KEY #K_NUM% GE "S" + FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155% OR ERR = 9%
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

	PAGE%, PRINT_FLAG% = 0%

 GetNextRec:
	!
	! Print totals after each salesman
	!
	IF PRINT_FLAG%
	THEN
		NO_TEST%(Y%) = 0% FOR Y% = 1% TO YEARS%
		GOSUB CustTotal
		GOSUB BrokerTotal
	END IF

	NEW_SALESMAN% = -1%

17020	!
	! Get next record
	!
	WHEN ERROR IN
		GET #SB_SUBACCOUNT.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

	!
	! Check status
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOTO ExitProgram IF SA_SALESMAN::SUBJECT <> "S"

	SELECT SORTBY$

	CASE "C"
		GOTO ExitTotal IF (SA_SALESMAN::CLASS > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_ARRAY(EDIT$( &
			SA_SALESMAN::CLASS, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "S"
		GOTO ExitTotal IF (SA_SALESMAN::SALESMAN > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_ARRAY(EDIT$( &
			SA_SALESMAN::SALESMAN, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "T"
		GOTO ExitTotal IF (SA_SALESMAN::TTYPE > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_ARRAY(EDIT$( &
			SA_SALESMAN::TTYPE, -1%), WLDCRD$) = 0% &
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
				KEY #2% EQ SA_SALESMAN::SALESMAN, &
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
			IF IC_HISTORY(Y%)::SUBACCT <> SA_SALESMAN::SALESMAN
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

			GOTO GetHistRec &
				IF PD_EXAM_PRODUCT(IC_HISTORY(Y%)::PRODUCT, &
				PD_PRODUCT_EXAM) <> CMC$_NORMAL
		ELSE
			NO_TEST%(Y%) = 1%
		END IF

	NEXT Y%

	NO_TEST% = 1%
	CUST_TEST% = -1%

	FOR Y% = 1% TO YEARS%

		!IF NO_TEST%(Y%) = 0%
		!THEN
			IF TEST_LINE$ <> IC_HISTORY(Y%)::SUBACCT + &
				IC_HISTORY(Y%)::CROSSREF + &
				IC_HISTORY(Y%)::PRODUCT AND &
					TEST_LINE$ <> ""
			THEN
				CUST_TEST% = 0% IF LEFT(TEST_LINE$, 20%) = &
					IC_HISTORY(Y%)::SUBACCT + &
					IC_HISTORY(Y%)::CROSSREF

				NO_TEST%(Y%) = 1%
			ELSE
				CUST_TEST% = 0%
			END IF
		!ELSE
		!	CUST_TEST% = CUST_TEST% AND -1%
		!END IF

		NO_TEST% = NO_TEST% * NO_TEST%(Y%)

	NEXT Y%

	IF NO_TEST%
	THEN
		NO_TEST%(Y%) = 0% FOR Y% = 1% TO YEARS%

		GOSUB CustTotal IF CUST_TEST%
	END IF

	TEST_LINE$ = SA_SALESMAN::SALESMAN + CHR$(255%)

	!
	! Find minimum
	!
	FOR Y% = 1% TO YEARS%

		IF EOF%(Y%) = 0% AND &
			TEST_LINE$ > IC_HISTORY(Y%)::SUBACCT + &
				IC_HISTORY(Y%)::CROSSREF + &
				IC_HISTORY(Y%)::PRODUCT
		THEN
			TEST_LINE$ = IC_HISTORY(Y%)::SUBACCT + &
				IC_HISTORY(Y%)::CROSSREF + &
				IC_HISTORY(Y%)::PRODUCT

			LAST_CROSSREF$ = IC_HISTORY(Y%)::CROSSREF

		END IF

	NEXT Y%

	FOR Y% = 1% TO YEARS%

		IF TEST_LINE$ = IC_HISTORY(Y%)::SUBACCT + &
			IC_HISTORY(Y%)::CROSSREF + &
			IC_HISTORY(Y%)::PRODUCT
		THEN
			FOR I% = 1% TO 12%

			CUSTPRICEAMT(Y%, I%)  = CUSTPRICEAMT(Y%, I%) + &
				IC_HISTORY(Y%)::PRICEAMT(I%)

			CUSTCOSTAMT(Y%, I%)   = CUSTCOSTAMT(Y%, I%) + &
				IC_HISTORY(Y%)::COSTAMT(I%)

			J% = INT((I% - 1%) / 3%) + 1%

			CUSTPRCQTR(Y%, J%) = CUSTPRCQTR(Y%, J%) + &
				IC_HISTORY(Y%)::PRICEAMT(I%)

			CUSTCSTQTR(Y%, J%) = CUSTCSTQTR(Y%, J%) + &
				IC_HISTORY(Y%)::COSTAMT(I%)

			CUSTPRCYTD(Y%)  = CUSTPRCYTD(Y%) + &
				IC_HISTORY(Y%)::PRICEAMT(I%)

			CUSTCSTYTD(Y%)  = CUSTCSTYTD(Y%) + &
				IC_HISTORY(Y%)::COSTAMT(I%)

			NEXT I%
		ELSE
			NO_TEST%(Y%) = 1%
		END IF

	NEXT Y%

	PRINT_FLAG% = -1%

	IF NEW_SALESMAN%
	THEN
		TEXT$ = "::::::::::" + "   " + &
		SA_SALESMAN::SALESMAN + "   " + &
		SA_SALESMAN::DESCR + "   " + &
		SA_SALESMAN::TTYPE + "      " + &
		SA_SALESMAN::CLASS

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		TEXT$ = SPACE$(28%) + &
			SA_SALESMAN::ADD1 + "   " + &
			SA_SALESMAN::ADD2 + "   " + &
			SA_SALESMAN::CITY + "   " + &
			SA_SALESMAN::STATE + "      " + &
			PRNT_PHONE(SA_SALESMAN::PHONE, 0%)

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		NEW_SALESMAN%, PAGE% = 0%
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
	!FOR I% = 4% TO 7%
	!	TITLE$(I%) = TITLE$(I% + 2%)
	!NEXT I%

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
		GOTO ExitProgram IF UTL_REPORTX::STAT

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
			GOTO ExitProgram IF UTL_REPORTX::STAT

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
						TOTALCSTQTR(Y% + 1%, I% - 12%)) * 100.
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

			GOTO ExitProgram IF UTL_REPORTX::STAT

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
	GOTO ExitCustTotal IF TOTAL_ONLY$ = "Y"

	AR_35CUSTOM_EXAM::CUSNAM = SPACE$(LEN(AR_35CUSTOM_EXAM::CUSNAM)) &
		IF AR_EXAM_CUSTOM(LAST_CROSSREF$, AR_35CUSTOM_EXAM) <> &
		CMC$_NORMAL

	!
	! Print out one line
	!
	TEXT$ = "     " + LAST_CROSSREF$ + "     " + &
		LEFT$(AR_35CUSTOM_EXAM::CUSNAM, 30%) + "     " + &
		STRING$(68%, A"."B)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	FOR Y% = 1% TO YEARS%

		GOTO CustVar IF Y% <> 1% AND Y% = YEARS% AND &
			PRINT_VARIANCE$ = "Y"

		TEXT$ = SPACE$(25%) + ":   "
		TEXT$ = "Year " + YYYY$(Y%) + TEXT$ + TEXT$ + TEXT$ + TEXT$

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3%)

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
		GOTO ExitProgram IF UTL_REPORTX::STAT

		IF GROSS_PROFIT$ = "Y" AND (Y% = 1% OR Y% > 1% AND &
			PRINT_VARIANCE$ <> "Y")
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
			GOTO ExitProgram IF UTL_REPORTX::STAT

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
				VARIANCE(17%) = 0.0 &
					IF CUSTPRCYTD(Y%) - CUSTCSTYTD(Y%) = 0.0
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

			GOTO ExitProgram IF UTL_REPORTX::STAT

		END IF

	NEXT Y%

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

 ExitCustTotal:
	FOR Y% = 1% TO YEARS%

		FOR I% = 1% TO 12%

			BROKERPRICEAMT(Y%, I%) = BROKERPRICEAMT(Y%, I%) + &
				CUSTPRICEAMT(Y%, I%)
			BROKERCOSTAMT(Y%, I%) = BROKERCOSTAMT(Y%, I%) + &
				CUSTCOSTAMT(Y%, I%)

		NEXT I%

	NEXT Y%

	FOR Y% = 1% TO YEARS%

		BROKERPRCYTD(Y%) = BROKERPRCYTD(Y%) + CUSTPRCYTD(Y%)
		BROKERCSTYTD(Y%) = BROKERCSTYTD(Y%) + CUSTCSTYTD(Y%)

		CUSTPRCYTD(Y%) = 0.0
		CUSTCSTYTD(Y%) = 0.0

		FOR I% = 1% TO 4%

			BROKERPRCQTR(Y%, I%) = BROKERPRCQTR(Y%, I%) + &
				CUSTPRCQTR(Y%, I%)
			BROKERCSTQTR(Y%, I%) = BROKERCSTQTR(Y%, I%) + &
				CUSTCSTQTR(Y%, I%)

			CUSTPRICEAMT(Y%, I%)       = 0.0
			CUSTCOSTAMT(Y%, I%)        = 0.0

			CUSTPRICEAMT(Y%, I% + 4%)  = 0.0
			CUSTCOSTAMT(Y%, I% + 4%)   = 0.0

			CUSTPRICEAMT(Y%, I% + 8%)  = 0.0
			CUSTCOSTAMT(Y%, I% + 8%)   = 0.0

			CUSTPRCQTR(Y%, I%)         = 0.0
			CUSTCSTQTR(Y%, I%)         = 0.0

		NEXT I%

	NEXT Y%

	RETURN

 BrokerTotal:
	!IF TOTAL_ONLY$ = "Y"
	!THEN
	!	GOTO SkipBroker
	!ELSE
	!	PAGE% = 999%
	!END IF

	TEXT$ = "::::::::::" + "   " + &
		SA_SALESMAN::SALESMAN + "   " + &
		SA_SALESMAN::DESCR + "   " + &
		SA_SALESMAN::TTYPE + "      " + &
		SA_SALESMAN::CLASS

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = SPACE$(28%) + &
		SA_SALESMAN::ADD1 + "   " + &
		SA_SALESMAN::ADD2 + "   " + &
		SA_SALESMAN::CITY + "   " + &
		SA_SALESMAN::STATE + "      " + &
		PRNT_PHONE(SA_SALESMAN::PHONE, 0%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	FOR Y% = 1% TO YEARS%

		GOTO BrokerVar &
			IF Y% <> 1% AND Y% = YEARS% AND PRINT_VARIANCE$ = "Y"

		TEXT$ = SPACE$(25%) + ":   "
		TEXT$ = "Year " + YYYY$(Y%) + TEXT$ + TEXT$ + TEXT$ + TEXT$

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		TEXT$ = "Sales " + &
			FORMAT$(BROKERPRICEAMT(Y%, 1%), "#######") + &
			FORMAT$(BROKERPRICEAMT(Y%, 2%), "#######") + &
			FORMAT$(BROKERPRICEAMT(Y%, 3%), "#######") + &
			FORMAT$(BROKERPRCQTR(Y%, 1%), "########") + &
			FORMAT$(BROKERPRICEAMT(Y%, 4%), "#######") + &
			FORMAT$(BROKERPRICEAMT(Y%, 5%), "#######") + &
			FORMAT$(BROKERPRICEAMT(Y%, 6%), "#######") + &
			FORMAT$(BROKERPRCQTR(Y%, 2%), "########") + &
			FORMAT$(BROKERPRICEAMT(Y%, 7%), "#######") + &
			FORMAT$(BROKERPRICEAMT(Y%, 8%), "#######") + &
			FORMAT$(BROKERPRICEAMT(Y%, 9%), "#######") + &
			FORMAT$(BROKERPRCQTR(Y%, 3%), "########") + &
			FORMAT$(BROKERPRICEAMT(Y%, 10%), "#######") + &
			FORMAT$(BROKERPRICEAMT(Y%, 11%), "#######") + &
			FORMAT$(BROKERPRICEAMT(Y%, 12%), "#######") + &
			FORMAT$(BROKERPRCQTR(Y%, 4%), "########") + &
			FORMAT$(BROKERPRCYTD(Y%),  "########")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		IF GROSS_PROFIT$ = "Y"
		THEN
			TEXT$ = "GProf " + &
				FORMAT$(BROKERPRICEAMT(Y%, 1%) - &
					BROKERCOSTAMT(Y%, 1%), "#######") + &
				FORMAT$(BROKERPRICEAMT(Y%, 2%) - &
					BROKERCOSTAMT(Y%, 2%), "#######") + &
				FORMAT$(BROKERPRICEAMT(Y%, 3%) - &
					BROKERCOSTAMT(Y%, 3%), "#######") + &
				FORMAT$(BROKERPRCQTR(Y%, 1%) - &
					BROKERCSTQTR(Y%, 1%), "########") + &
				FORMAT$(BROKERPRICEAMT(Y%, 4%) - &
					BROKERCOSTAMT(Y%, 4%), "#######") + &
				FORMAT$(BROKERPRICEAMT(Y%, 5%) - &
					BROKERCOSTAMT(Y%, 5%), "#######") + &
				FORMAT$(BROKERPRICEAMT(Y%, 6%) - &
					BROKERCOSTAMT(Y%, 6%), "#######") + &
				FORMAT$(BROKERPRCQTR(Y%, 2%) - &
					BROKERCSTQTR(Y%, 2%), "########") + &
				FORMAT$(BROKERPRICEAMT(Y%, 7%) - &
					BROKERCOSTAMT(Y%, 7%), "#######") + &
				FORMAT$(BROKERPRICEAMT(Y%, 8%) - &
					BROKERCOSTAMT(Y%, 8%), "#######") + &
				FORMAT$(BROKERPRICEAMT(Y%, 9%) - &
					BROKERCOSTAMT(Y%, 9%), "#######") + &
				FORMAT$(BROKERPRCQTR(Y%, 3%) - &
					BROKERCSTQTR(Y%, 3%), "########") + &
				FORMAT$(BROKERPRICEAMT(Y%, 10%) - &
					BROKERCOSTAMT(Y%, 10%), "#######") + &
				FORMAT$(BROKERPRICEAMT(Y%, 11%) - &
					BROKERCOSTAMT(Y%, 11%), "#######") + &
				FORMAT$(BROKERPRICEAMT(Y%, 12%) - &
					BROKERCOSTAMT(Y%, 12%), "#######") + &
				FORMAT$(BROKERPRCQTR(Y%, 4%) - &
					BROKERCSTQTR(Y%, 4%), "########") + &
				FORMAT$(BROKERPRCYTD(Y%) - &
					BROKERCSTYTD(Y%),  "########")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 1%)
			GOTO ExitProgram IF UTL_REPORTX::STAT

		END IF

 BrokerVar:
		IF PRINT_VARIANCE$ = "Y" AND Y% <> YEARS%
		THEN
			FOR I% = 1% TO 12%

				IF BROKERPRICEAMT(Y% + 1%, I%) = 0.0
				THEN
					VARIANCE(I%) = 100.00
					VARIANCE(I%) = 0.0 &
						IF BROKERPRICEAMT(Y%, I%) = 0.0
				ELSE
					VARIANCE(I%) = (BROKERPRICEAMT(Y%, I%) - &
						BROKERPRICEAMT(Y% + 1%, I%)) / &
						BROKERPRICEAMT(Y% + 1%, I%) * 100.0
				END IF

			NEXT I%

			FOR I% = 13% TO 16%

				IF BROKERPRCQTR(Y% + 1%, I% - 12%) = 0.0
				THEN
					VARIANCE(I%) = 100.0
					VARIANCE(I%) = 0.0 &
						IF BROKERPRCQTR(Y%, I% - 12%) = 0.0
				ELSE
					VARIANCE(I%) = (BROKERPRCQTR(Y%, I% - 12%) - &
						BROKERPRCQTR(Y% + 1%, I% - 12%)) / &
						BROKERPRCQTR(Y% + 1%, I% - 12%) * 100.
				END IF

			NEXT I%

			IF BROKERPRCYTD(Y% + 1%) = 0.0
			THEN
				VARIANCE(17%) = 100.00
				VARIANCE(17%) = 0.0 IF BROKERPRCYTD(Y%) = 0.0
			ELSE
				VARIANCE(17%) = (BROKERPRCYTD(Y%) - &
					BROKERPRCYTD(Y% + 1%)) / &
					BROKERPRCYTD(Y% + 1%) * 100.0
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

				IF BROKERPRICEAMT(Y% + 1%, I%) - &
					BROKERCOSTAMT(Y% + 1%, I%) = 0.0
				THEN
					VARIANCE(I%) = 100.00
					VARIANCE(I%) = 0.0 &
						IF BROKERPRICEAMT(Y%, I%) - &
						BROKERCOSTAMT(Y%, I%) = 0.0
				ELSE
					VARIANCE(I%) = (BROKERPRICEAMT(Y%, I%) - &
						BROKERCOSTAMT(Y%, I%) - &
						(BROKERPRICEAMT(Y% + 1%, I%) - &
						BROKERCOSTAMT(Y% + 1%, I%))) / &
						(BROKERPRICEAMT(Y% + 1%, I%) - &
						BROKERCOSTAMT(Y% + 1%, I%)) * 100.0
				END IF

			NEXT I%

			FOR I% = 13% TO 16%

				IF BROKERPRCQTR(Y% + 1%, I% - 12%) - &
					BROKERCSTQTR(Y% + 1%, I% - 12%) = 0.0
				THEN
					VARIANCE(I%) = 100.0
					VARIANCE(I%) = 0.0 &
						IF BROKERPRCQTR(Y%, I% - 12%) - &
						BROKERCSTQTR(Y%, I% - 12%) = 0.0
				ELSE
					VARIANCE(I%) = (BROKERPRCQTR(Y%, I% - 12%) - &
						BROKERCSTQTR(Y%, I% - 12%) - &
						(BROKERPRCQTR(Y% + 1%, I% - 12%) - &
						BROKERCSTQTR(Y% + 1%, I% - 12%))) / &
						(BROKERPRCQTR(Y% + 1%, I% - 12%) - &
						BROKERCSTQTR(Y% + 1%, I% - 12%)) * 100.
				END IF

			NEXT I%

			IF BROKERPRCYTD(Y% + 1%) - BROKERCSTYTD(Y% + 1%) = 0.0
			THEN
				VARIANCE(17%) = 100.00
				VARIANCE(17%) = 0.0 &
					IF BROKERPRCYTD(Y%) - &
					BROKERCSTYTD(Y%) = 0.0
			ELSE
				VARIANCE(17%) = (BROKERPRCYTD(Y%) - &
					BROKERCSTYTD(Y%) - &
					(BROKERPRCYTD(Y% + 1%) - &
					BROKERCSTYTD(Y% + 1%))) / &
					(BROKERPRCYTD(Y% + 1%) - &
					BROKERCSTYTD(Y% + 1%)) * 100.0
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

			GOTO ExitProgram IF UTL_REPORTX::STAT

		END IF

	NEXT Y%

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

 SkipBroker:
	FOR Y% = 1% TO YEARS%

		FOR I% = 1% TO 12%

			TOTALPRICEAMT(Y%, I%) = TOTALPRICEAMT(Y%, I%) + &
				BROKERPRICEAMT(Y%, I%)
			TOTALCOSTAMT(Y%, I%) = TOTALCOSTAMT(Y%, I%) + &
				BROKERCOSTAMT(Y%, I%)

		NEXT I%

	NEXT Y%

	FOR Y% = 1% TO YEARS%

		TOTALPRCYTD(Y%)  = TOTALPRCYTD(Y%)  + BROKERPRCYTD(Y%)
		TOTALCSTYTD(Y%)  = TOTALCSTYTD(Y%)  + BROKERCSTYTD(Y%)

		BROKERPRCYTD(Y%)  = 0.0
		BROKERCSTYTD(Y%)  = 0.0

		FOR I% = 1% TO 4%
			TOTALPRCQTR(Y%, I%) = TOTALPRCQTR(Y%, I%) + &
				BROKERPRCQTR(Y%, I%)
			TOTALCSTQTR(Y%, I%) = TOTALCSTQTR(Y%, I%) + &
				BROKERCSTQTR(Y%, I%)

			BROKERPRICEAMT(Y%, I%)       = 0.0
			BROKERCOSTAMT(Y%, I%)        = 0.0

			BROKERPRICEAMT(Y%, I% + 4%)  = 0.0
			BROKERCOSTAMT(Y%, I% + 4%)   = 0.0

			BROKERPRICEAMT(Y%, I% + 8%)  = 0.0
			BROKERCOSTAMT(Y%, I% + 8%)   = 0.0

			BROKERPRCQTR(Y%, I%)         = 0.0
			BROKERCSTQTR(Y%, I%)         = 0.0

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
	! End of report SA_RPRT_SALSUM
	!******************************************************************
	END
