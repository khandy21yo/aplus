1	%TITLE "Sales by Product and Salesman"
	%SBTTL "SA_RPRT_PRDSALCUS"
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
	!	$ BAS SA_SOURCE:SA_RPRT_PRDSALCUS/LINE
	!	$ LINK/EXECUTABLE=SA_EXE: SA_RPRT_PRDSALCUS, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE SA_RPRT_PRDSALCUS.OBJ;*
	!
	! Author:
	!
	!	01/08/92 - Dan Perkins
	!
	! Modification history:
	!
	!	01/20/92 - Dan Perkins
	!		Offset Grand Total Output to make large numbers
	!		more readable.
	!
	!	02/04/92 - Kevin Handy
	!		Cleaned out junk (check)
	!
	!	03/23/94 - Kevin Handy
	!		Formatted to 80 columns.
	!
	!	03/25/94 - Kevin Handy
	!		Added some comments while trying to figure out
	!		how it is supposed to work.
	!
	!	06/02/94 - Kevin Handy
	!		Clean up (Check)
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
	! Used in SaleTotal, created in PrintLine
	!
	DECLARE REAL SALEPRICEAMT(10%, 12%)
	DECLARE REAL SALECOSTAMT(10%, 12%)

	!
	! Used in ProdTotal, created in SaleTotal
	!
	DECLARE REAL PRODPRICEAMT(10%, 12%)
	DECLARE REAL PRODCOSTAMT(10%, 12%)

	!
	! Used in ExitTotal, created in ProdTotal
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
	!	^*(01) Sort by (N,T,C,D,S)\*
	!	.p
	!	The ^*Sort by\* field determines the order
	!	the report will print in.
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
	!	The ^*Print Variance\* field optionally shows the
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
	CASE "P"
		K_NUM% = 0%
		TITLE$(1%) = "SALES BY PRODUCT NUMBER"

	CASE "T"
		K_NUM% = 1%
		TITLE$(1%) = "SALES BY PRODUCT TYPE"

	CASE "C"
		K_NUM% = 2%
		TITLE$(1%) = "SALES BY PRODUCT CATEGORY"

	CASE "D"
		K_NUM% = 3%
		TITLE$(1%) = "SALES BY PRODUCT DESCRIPTION"

	CASE "S"
		K_NUM% = 4%
		TITLE$(1%) = "SALES BY PRODUCT SECONDARY CODE"

	END SELECT

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
			IF COMP_STRING(EDIT$(PD_PRODUCT::PRODUCT_NUM, &
			-1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "T"
		GOTO ExitTotal IF (PD_PRODUCT::PROD_TYPE > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(PD_PRODUCT::PROD_TYPE, &
			-1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "C"
		GOTO ExitTotal IF (PD_PRODUCT::CATEGORY > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(PD_PRODUCT::CATEGORY, &
			-1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "D"
		GOTO ExitTotal IF (PD_PRODUCT::DESCRIPTION > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(PD_PRODUCT::DESCRIPTION, &
			-1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "S"
		GOTO ExitTotal &
			IF (PD_PRODUCT::SECONDARY_CODE > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING(EDIT$( &
			PD_PRODUCT::SECONDARY_CODE, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	END SELECT

	TEST_LINE$ = ""

17100	!
	! Get History records
	!
	FOR Y% = 1% TO YEARS%

		EOF%(Y%) = 0%
		NO_TEST%(Y%) = 0%
		WHEN ERROR IN
			GET #IC_35HISTORY.CH%(Y%), &
				KEY #3% EQ PD_PRODUCT::PRODUCT_NUM, &
				REGARDLESS
			IC_HISTORY(Y%) = IC_35HISTORY
		USE
			IF ERR = 155%
			THEN
				EOF%(Y%) = 1%
				CONTINUE 17115
			END IF

			FILENAME$ = "IC_35HISTORY"
			CONTINUE HelpError
		END WHEN

17115	NEXT Y%
	SY% = 1%
	GOTO TestKey

	!
	! Pull out next record from last file used.
	!
 GetHistRec:
17120	SY% = Y%
	WHEN ERROR IN
		GET #IC_35HISTORY.CH%(Y%), REGARDLESS
		IC_HISTORY(Y%) = IC_35HISTORY
	USE
		IF ERR = 11%
		THEN
			EOF%(Y%) = 1%
			CONTINUE TestKey
		END IF
		FILENAME$ = "IC_35HISTORY"
		CONTINUE HelpError
	END WHEN

	!
	! Figure out which file to pull the next record from
	!
 TestKey:
	TEST_EOF% = 1%
	FOR Y% = 1% TO YEARS%
		EOF%(Y%) = 1% &
			IF IC_HISTORY(Y%)::PRODUCT <> PD_PRODUCT::PRODUCT_NUM
		TEST_EOF% = TEST_EOF% * EOF%(Y%)
	NEXT Y%

	!
	! Done if at enf of file in all files
	!
	GOTO GetNextRec IF TEST_EOF% = 1%

	!
	! Make sure we have a valid sales record for all years
	! (in those files haven't EOF'd in yet)
	!
	FOR Y% = SY% TO YEARS%
		IF EOF%(Y%) <> 1% AND NO_TEST%(Y%) <> 1%
		THEN
			GOTO GetHistRec IF IC_HISTORY(Y%)::TRANSTYPE <> "SA"

			GOTO GetHistRec &
				IF COMP_STRING(EDIT$(IC_HISTORY(Y%)::LOCATION, &
				-1%), LOC_WLDCRD$) = 0% &
				AND LOC_WLDCRD$ <> ""
		ELSE
			NO_TEST%(Y%) = 1%
		END IF

	NEXT Y%

	!
	! Calculate NO_TEST% and SALE_TEST%, whatever they may mean
	!
	NO_TEST% = 1%
	SALE_TEST% = -1%
	FOR Y% = 1% TO YEARS%
		IF NO_TEST%(Y%) = 0%
		THEN
			IF TEST_LINE$ <> IC_HISTORY(Y%)::PRODUCT + &
				IC_HISTORY(Y%)::SUBACCT + &
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
			!
			! A Silly NO-OP
			!
			SALE_TEST% = SALE_TEST% AND -1%
		END IF

		NO_TEST% = NO_TEST% * NO_TEST%(Y%)

	NEXT Y%

	!
	! If NO_TEST% tells up something, then print the line
	!
	IF NO_TEST%
	THEN
		GOSUB PrintLine

		!
		! Print sales totals if SALE_TEST% was magically set
		!
		IF SALE_TEST%
		THEN
			GOSUB SaleTotal
		END IF
	END IF

	TEST_LINE$ = PD_PRODUCT::PRODUCT_NUM + CHR$(255%)

	!
	! Find next item to process
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

	!
	! Process all years that match selected record
	!
	FOR Y% = 1% TO YEARS%

		IF TEST_LINE$ = IC_HISTORY(Y%)::PRODUCT + &
			IC_HISTORY(Y%)::SUBACCT + &
			IC_HISTORY(Y%)::CROSSREF
		THEN
			FOR I% = 1% TO 12%

				PQUANTITY(Y%, I%) = PQUANTITY(Y%, I%) - &
					IC_HISTORY(Y%)::PQUANTITY(I%)
				PRICEAMT(Y%, I%) = PRICEAMT(Y%, I%) + &
					IC_HISTORY(Y%)::PRICEAMT(I%)
				COSTAMT(Y%, I%) = COSTAMT(Y%, I%) + &
					IC_HISTORY(Y%)::COSTAMT(I%)
				J% = INT((I% - 1%) / 3%) + 1%
				QTYQTR(Y%,J%) = QTYQTR(Y%,J%) - &
					IC_HISTORY(Y%)::PQUANTITY(I%)
				PRCQTR(Y%,J%) = PRCQTR(Y%,J%) + &
					IC_HISTORY(Y%)::PRICEAMT(I%)
				CSTQTR(Y%,J%) = CSTQTR(Y%,J%) + &
					IC_HISTORY(Y%)::COSTAMT(I%)
				QTYYTD(Y%) = QTYYTD(Y%) - &
					IC_HISTORY(Y%)::PQUANTITY(I%)
				PRCYTD(Y%) = PRCYTD(Y%) + &
					IC_HISTORY(Y%)::PRICEAMT(I%)
				CSTYTD(Y%) = CSTYTD(Y%) + &
					IC_HISTORY(Y%)::COSTAMT(I%)

			NEXT I%
		ELSE
			NO_TEST%(Y%) = 1%
		END IF

	NEXT Y%

	PRINT_FLAG% = -1%

	!
	! Print product name if it has changed
	!
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

17130	!
	! Get next record in all files where we used a record
	!
	FOR Y% = YEARS% TO 1% STEP -1%

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
	! Print out grand totals
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
				FORMAT$(TOTALPRCYTD(Y%) - &
					TOTALCSTYTD(Y%), "########")

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
						TOTALPRCQTR(Y% + 1%, I% - 12%) * 100.
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
				FORMAT$(VARIANCE(17%), "#######%")

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
					VARIANCE(I%) = (TOTALPRCQTR(Y%, I% - 12%) &
						- TOTALCSTQTR(Y%, I% - 12%) - &
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
				FORMAT$(VARIANCE(17%), "#######%")

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

	!*******************************************************************
	! Print out one line of data
	!*******************************************************************

 PrintLine:
	V% = SA_EXAM_SALESMAN(LAST_SUBACCT$, SA_SALESMAN_EXAM, &
		SB_SUBACCOUNT_EXAM)

	V% = AR_EXAM_CUSTOM(LAST_CROSSREF$, AR_35CUSTOM_EXAM)

	IF LAST_SUBACCT$<>"" OR LAST_CROSSREF$<>""
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

		GOTO Var IF Y% <> 1% AND Y% = YEARS% AND PRINT_VARIANCE$ = "Y"

		TEXT$ = SPACE$(25%) + ":   "
		TEXT$ = "Year " + YYYY$(Y%) + TEXT$ + TEXT$ + TEXT$ + TEXT$

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3%)

		IF PRINT_UNITS$ = "Y" AND &
			(Y% = 1% OR Y% > 1% AND PRINT_VARIANCE$ <> "Y")
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

		IF GROSS_PROFIT$ = "Y" AND &
			(Y% = 1% OR Y% > 1% AND PRINT_VARIANCE$ <> "Y")
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
				FORMAT$(PRCYTD(Y%) - CSTYTD(Y%), "########")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 1%)

		END IF

 Var:
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
					VARIANCE(I%) = &
						(QTYQTR(Y%, I% - 12%) - &
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
				FORMAT$(VARIANCE(17%), "#######%")

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
						PRICEAMT(Y% + 1%, I%) * 100.
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
				VARIANCE(17%) = (PRCYTD(Y%) - &
					PRCYTD(Y% + 1%)) / &
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
				FORMAT$(VARIANCE(17%), "#######%")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 1%)

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
				FORMAT$(VARIANCE(17%), "#######%")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 1%) &
				IF GROSS_PROFIT$ = "Y"
		END IF

	NEXT Y%

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FOR Y% = 1% TO YEARS%

		FOR I% = 1% TO 12%
			SALEPRICEAMT(Y%, I%) = SALEPRICEAMT(Y%, I%) + &
				PRICEAMT(Y%, I%)
			SALECOSTAMT(Y%, I%) = SALECOSTAMT(Y%, I%) + &
				COSTAMT(Y%, I%)
		NEXT I%

		FOR I% = 1% TO 4%
			SALEPRCQTR(Y%, I%) = SALEPRCQTR(Y%, I%) + PRCQTR(Y%, I%)
			SALECSTQTR(Y%, I%) = SALECSTQTR(Y%, I%) + CSTQTR(Y%, I%)
		NEXT I%

		SALEPRCYTD(Y%) = SALEPRCYTD(Y%) + PRCYTD(Y%)
		SALECSTYTD(Y%) = SALECSTYTD(Y%) + CSTYTD(Y%)
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

	!*******************************************************************
	! Print salesman subtotals
	!*******************************************************************

 SaleTotal:
	V% = SA_EXAM_SALESMAN(LAST_SUBACCT$, SA_SALESMAN_EXAM, &
		SB_SUBACCOUNT_EXAM)

	!
	! Print out one line
	!
	TEXT$ = "     " + LAST_SUBACCT$ + "     " + &
		LEFT$(SA_SALESMAN_EXAM::DESCR, 30%) + "     " + &
		STRING$(68%, A"."B)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, PAGE%)

	FOR Y% = 1% TO YEARS%

		GOTO SaleVar &
			IF Y% <> 1% AND Y% = YEARS% AND PRINT_VARIANCE$ = "Y"

		TEXT$ = SPACE$(25%) + ":   "
		TEXT$ = "Year " + YYYY$(Y%) + TEXT$ + TEXT$ + TEXT$ + TEXT$

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3%)

		TEXT$ = "Sales " + &
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
			FORMAT$(SALEPRCYTD(Y%), "########")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		IF GROSS_PROFIT$ = "Y" AND (Y% = 1% OR Y% > 1% AND PRINT_VARIANCE$ <> "Y")
		THEN
			TEXT$ = "GProf " + &
				FORMAT$(SALEPRICEAMT(Y%, 1%) - &
					SALECOSTAMT(Y%, 1%), "#######") + &
				FORMAT$(SALEPRICEAMT(Y%, 2%) - &
					SALECOSTAMT(Y%, 2%), "#######") + &
				FORMAT$(SALEPRICEAMT(Y%, 3%) - &
					SALECOSTAMT(Y%, 3%), "#######") + &
				FORMAT$(SALEPRCQTR(Y%, 1%) - &
					SALECSTQTR(Y%, 1%), "########") + &
				FORMAT$(SALEPRICEAMT(Y%, 4%) - &
					SALECOSTAMT(Y%, 4%), "#######") + &
				FORMAT$(SALEPRICEAMT(Y%, 5%) - &
					SALECOSTAMT(Y%, 5%), "#######") + &
				FORMAT$(SALEPRICEAMT(Y%, 6%) - &
					SALECOSTAMT(Y%, 6%), "#######") + &
				FORMAT$(SALEPRCQTR(Y%, 2%) - &
					SALECSTQTR(Y%, 2%), "########") + &
				FORMAT$(SALEPRICEAMT(Y%, 7%) - &
					SALECOSTAMT(Y%, 7%), "#######") + &
				FORMAT$(SALEPRICEAMT(Y%, 8%) - &
					SALECOSTAMT(Y%, 8%), "#######") + &
				FORMAT$(SALEPRICEAMT(Y%, 9%) - &
					SALECOSTAMT(Y%, 9%), "#######") + &
				FORMAT$(SALEPRCQTR(Y%, 3%) - &
					SALECSTQTR(Y%, 3%), "########") + &
				FORMAT$(SALEPRICEAMT(Y%, 10%) - &
					SALECOSTAMT(Y%, 10%), "#######") + &
				FORMAT$(SALEPRICEAMT(Y%, 11%) - &
					SALECOSTAMT(Y%, 11%), "#######") + &
				FORMAT$(SALEPRICEAMT(Y%, 12%) - &
					SALECOSTAMT(Y%, 12%), "#######") + &
				FORMAT$(SALEPRCQTR(Y%, 4%) - &
					SALECSTQTR(Y%, 4%), "########") + &
				FORMAT$(SALEPRCYTD(Y%) - &
					SALECSTYTD(Y%), "########")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 1%)

		END IF

 SaleVar:
		IF PRINT_VARIANCE$ = "Y" AND Y% <> YEARS%
		THEN
			FOR I% = 1% TO 12%
				IF SALEPRICEAMT(Y% + 1%, I%) = 0.0
				THEN
					VARIANCE(I%) = 100.00
					VARIANCE(I%) = 0.0 &
						IF SALEPRICEAMT(Y%, I%) = 0.0
				ELSE
					VARIANCE(I%) = (SALEPRICEAMT(Y%, I%) - &
						SALEPRICEAMT(Y% + 1%, I%)) / &
						SALEPRICEAMT(Y% + 1%, I%) * 100.0
				END IF
			NEXT I%

			FOR I% = 13% TO 16%
				IF SALEPRCQTR(Y% + 1%, I% - 12%) = 0.0
				THEN
					VARIANCE(I%) = 100.0
					VARIANCE(I%) = 0.0 &
						IF SALEPRCQTR(Y%, I% - 12%) = 0.0
				ELSE
					VARIANCE(I%) = (SALEPRCQTR(Y%, I% - 12%) &
						- SALEPRCQTR(Y% + 1%, I% - 12%)) / &
						SALEPRCQTR(Y% + 1%, I% - 12%) * 100.0
				END IF
			NEXT I%

			IF SALEPRCYTD(Y% + 1%) = 0.0
			THEN
				VARIANCE(17%) = 100.00
				VARIANCE(17%) = 0.0 IF SALEPRCYTD(Y%) = 0.0
			ELSE
				VARIANCE(17%) = (SALEPRCYTD(Y%) - &
					SALEPRCYTD(Y% + 1%)) / &
					SALEPRCYTD(Y% + 1%) * 100.0
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
				FORMAT$(VARIANCE(17%), "#######%")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 1%)

			FOR I% = 1% TO 12%
				IF SALEPRICEAMT(Y% + 1%, I%) - &
					SALECOSTAMT(Y% + 1%, I%) = 0.0
				THEN
					VARIANCE(I%) = 100.00
					VARIANCE(I%) = 0.0 &
						IF SALEPRICEAMT(Y%, I%) - &
						SALECOSTAMT(Y%, I%) = 0.0
				ELSE
					VARIANCE(I%) = (SALEPRICEAMT(Y%, I%) - &
						SALECOSTAMT(Y%, I%) - &
						(SALEPRICEAMT(Y% + 1%, I%) - &
						SALECOSTAMT(Y% + 1%, I%))) / &
						(SALEPRICEAMT(Y% + 1%, I%) - &
						SALECOSTAMT(Y% + 1%, I%)) * 100.0
				END IF
			NEXT I%

			FOR I% = 13% TO 16%
				IF SALEPRCQTR(Y% + 1%, I% - 12%) - &
					SALECSTQTR(Y% + 1%, I% - 12%) = 0.0
				THEN
					VARIANCE(I%) = 100.0
					VARIANCE(I%) = 0.0 &
						IF SALEPRCQTR(Y%, I% - 12%) - &
						SALECSTQTR(Y%, I% - 12%) = 0.0
				ELSE
					VARIANCE(I%) = (SALEPRCQTR(Y%, I% - 12%) &
						- SALECSTQTR(Y%, I% - 12%) - &
						(SALEPRCQTR(Y% + 1%, I% - 12%) - &
						SALECSTQTR(Y% + 1%, I% - 12%))) / &
						(SALEPRCQTR(Y% + 1%, I% - 12%) - &
						SALECSTQTR(Y% + 1%, I% - 12%)) * 100.0
				END IF
			NEXT I%

			IF SALEPRCYTD(Y% + 1%) - SALECSTYTD(Y% + 1%) = 0.0
			THEN
				VARIANCE(17%) = 100.00
				VARIANCE(17%) = 0.0 &
					IF SALEPRCYTD(Y%) - SALECSTYTD(Y%) = 0.0
			ELSE
				VARIANCE(17%) = (SALEPRCYTD(Y%) - &
					SALECSTYTD(Y%) - &
					(SALEPRCYTD(Y% + 1%) - &
					SALECSTYTD(Y% + 1%))) / &
					(SALEPRCYTD(Y% + 1%) - &
					SALECSTYTD(Y% + 1%)) * 100.0
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
				FORMAT$(VARIANCE(17%), "#######%")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 1%) &
				IF GROSS_PROFIT$ = "Y"

		END IF

	NEXT Y%

	FOR Y% = 1% TO YEARS%

		FOR I% = 1% TO 12%
			PRODPRICEAMT(Y%, I%) = PRODPRICEAMT(Y%, I%) + &
				SALEPRICEAMT(Y%, I%)
			PRODCOSTAMT(Y%, I%) = PRODCOSTAMT(Y%, I%) + &
				SALECOSTAMT(Y%, I%)
		NEXT I%
	NEXT Y%

	FOR Y% = 1% TO YEARS%

		PRODPRCYTD(Y%) = PRODPRCYTD(Y%) + SALEPRCYTD(Y%)
		PRODCSTYTD(Y%) = PRODCSTYTD(Y%) + SALECSTYTD(Y%)

		SALEPRCYTD(Y%) = 0.0
		SALECSTYTD(Y%) = 0.0

		FOR I% = 1% TO 4%
			PRODPRCQTR(Y%, I%) = PRODPRCQTR(Y%, I%) + &
				SALEPRCQTR(Y%, I%)
			PRODCSTQTR(Y%, I%) = PRODCSTQTR(Y%, I%) + &
				SALECSTQTR(Y%, I%)

			SALEPRICEAMT(Y%, I%) = 0.0
			SALECOSTAMT(Y%, I%) = 0.0

			SALEPRICEAMT(Y%, I% + 4%) = 0.0
			SALECOSTAMT(Y%, I% + 4%) = 0.0

			SALEPRICEAMT(Y%, I% + 8%) = 0.0
			SALECOSTAMT(Y%, I% + 8%) = 0.0

			SALEPRCQTR(Y%, I%) = 0.0
			SALECSTQTR(Y%, I%) = 0.0

		NEXT I%

	NEXT Y%

	PAGE% = 999%

	RETURN

	!*******************************************************************
	! Print product totals
	!*******************************************************************

 ProdTotal:
	TEXT$ = ":::::::::: " + PD_PRODUCT::PRODUCT_NUM + " " + &
		TRM$(PD_PRODUCT::DESCRIPTION) + " " + &
		PD_PRODUCT::PROD_TYPE + " " + &
		PD_PRODUCT::CATEGORY + " " + &
		TRM$(PD_PRODUCT::SECONDARY_CODE)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, PAGE%)

	FOR Y% = 1% TO YEARS%

		GOTO ProdVar &
			IF Y% <> 1% AND Y% = YEARS% AND PRINT_VARIANCE$ = "Y"

		TEXT$ = SPACE$(25%) + ":   "
		TEXT$ = "Year " + YYYY$(Y%) + TEXT$ + TEXT$ + TEXT$ + TEXT$

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

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
						PRODPRICEAMT(Y% + 1%, I%) * 100.0
				END IF
			NEXT I%

			FOR I% = 13% TO 16%
				IF PRODPRCQTR(Y% + 1%, I% - 12%) = 0.0
				THEN
					VARIANCE(I%) = 100.0
					VARIANCE(I%) = 0.0 &
						IF PRODPRCQTR(Y%, I% - 12%) = 0.0
				ELSE
					VARIANCE(I%) = (PRODPRCQTR(Y%, I% - 12%) &
						- PRODPRCQTR(Y% + 1%, I% - 12%)) / &
						PRODPRCQTR(Y% + 1%, I% - 12%) * 100.
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
				FORMAT$(VARIANCE(17%), "#######%")

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
					VARIANCE(I%) = (PRODPRCQTR(Y%, I% - 12%) &
						- PRODCSTQTR(Y%, I% - 12%) - &
						(PRODPRCQTR(Y% + 1%, I% - 12%) - &
						PRODCSTQTR(Y% + 1%, I% - 12%))) / &
						(PRODPRCQTR(Y% + 1%, I% - 12%) - &
						PRODCSTQTR(Y% + 1%, I% - 12%)) * 100.
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
				FORMAT$(VARIANCE(17%), "#######%")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 1%) &
					IF GROSS_PROFIT$ = "Y"

		END IF
	NEXT Y%

	FOR Y% = 1% TO YEARS%
		FOR I% = 1% TO 12%
			TOTALPRICEAMT(Y%, I%) = TOTALPRICEAMT(Y%, I%) + &
				PRODPRICEAMT(Y%, I%)
			TOTALCOSTAMT(Y%, I%) = TOTALCOSTAMT(Y%, I%) + &
				PRODCOSTAMT(Y%, I%)
		NEXT I%
	NEXT Y%

	FOR Y% = 1% TO YEARS%

		TOTALPRCYTD(Y%) = TOTALPRCYTD(Y%) + PRODPRCYTD(Y%)
		TOTALCSTYTD(Y%) = TOTALCSTYTD(Y%) + PRODCSTYTD(Y%)

		PRODPRCYTD(Y%) = 0.0
		PRODCSTYTD(Y%) = 0.0

		FOR I% = 1% TO 4%
			TOTALPRCQTR(Y%, I%) = TOTALPRCQTR(Y%, I%) + &
				PRODPRCQTR(Y%, I%)
			TOTALCSTQTR(Y%, I%) = TOTALCSTQTR(Y%, I%) + &
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

	PAGE% = 999%

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
