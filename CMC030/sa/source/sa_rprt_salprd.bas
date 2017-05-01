1	%TITLE "Sales by Salesman and Product"
	%SBTTL "SA_RPRT_SALPRD"
	%IDENT "V3.6a Calico"

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
	! ID:SA0008
	!
	! Abstract:HELP
	!	.p
	!	The ^*Sales by Salesman and Product Report\* option
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
	!	Product Number
	!	.le
	!	Product Description
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
	!	$ BAS SA_SOURCE:SA_RPRT_SALPRD/LINE
	!	$ LINK/EXECUTABLE=SA_EXE: SA_RPRT_SALPRD, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE SA_RPRT_SALPRD.OBJ;*
	!
	! Author:
	!
	!	03/28/94 - Kevin Handy
	!		Taken from SA_RPRT_SALCUSPRD.
	!
	! Modification history:
	!
	!	03/30/94 - Kevin Handy
	!		Lose zero parts.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/13/96 - Kevin Handy
	!		Reformat source code
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
	!	11/22/2000 - Kevin Handy
	!		Comment out custtotal section (never called)
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

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP (SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT

	%INCLUDE "SOURCE:[SA.OPEN]SA_SALESMAN.HB"
	MAP (SB_SUBACCOUNT)	SA_SALESMAN_CDD		SA_SALESMAN

	%INCLUDE "SOURCE:[IC.OPEN]IC_35HISTORY.HB"
	MAP (IC_35HISTORY)	IC_35HISTORY_CDD	IC_35HISTORY

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	DECLARE			AR_35CUSTOM_CDD		AR_35CUSTOM_EXAM

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	!
	! Array for list of products
	!
	DIM PRODUCT_LIST$(500%)

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
	! Used in BrokerTotal, created in CustTotal
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

	PRINT_UNITS$ = EDIT$(UTL_REPORTX::OPTDEF(5%), -1%)

	!++
	! Abstract:FLD06
	!	^*(06) Print Units\*
	!	.p
	!	The ^*Print Units\* field optionally
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
	!	prints that number of the fiscal years. If
	!	the selected inventory transaction history file doesn't
	!	exist, report will not search for the older years.
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

	CASE "S"
		K_NUM% = 0%
		TITLE$(1%) = "SALES BY SALESMAN NUMBER"

	CASE "C"
		K_NUM% = 2%
		TITLE$(1%) = "SALES BY SALESMAN CLASS"

	CASE "T"
		K_NUM% = 1%
		TITLE$(1%) = "SALES BY SALESMAN TYPE"

	END SELECT

	IF LOC_WLDCRD$ = "" OR LOC_WLDCRD$ = "*"
	THEN
		TITLE$(2%) = "FOR ALL LOCATIONS"
	ELSE
		TITLE$(2%) = "FOR LOCATION " + EDIT$(LOC_WLDCRD$, -1%)
	END IF

	TITLE$(4%) = "Sales Analysis System"
	TITLE$(5%) = ""

	!
	! Heading
	!
	TITLE$(6%) = "     Cust #         Customer                   " + &
		"        Product#           Description"

	TITLE$(7%) = "         Per1   Per2   Per3    Qtr1   Per4   Per5   " + &
		"Per6    Qtr2   Per7   Per8   Per9    Qtr3  " + &
		"Per10  Per11  Per12    Qtr4     YTD"

	TITLE$(8%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			FIND #SB_SUBACCOUNT.CH%, &
				KEY #K_NUM% GE "S", &
				REGARDLESS
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

 GetNextRec:

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

17050	!*******************************************************************
	! Pass 1.
	! Figure out what products this salesman has sold
	!*******************************************************************

	PRODUCT_LIST% = 0%

	FOR Y% = 1% TO YEARS%

		WHEN ERROR IN
			FIND #IC_35HISTORY.CH%(Y%), &
				KEY #2% EQ SA_SALESMAN::SALESMAN, &
				REGARDLESS
		USE
			CONTINUE 17090
		END WHEN

17060		WHEN ERROR IN
			GET #IC_35HISTORY.CH%(Y%), &
				REGARDLESS
		USE
			CONTINUE 17090
		END WHEN

		!
		! Done with salesman
		!
		GOTO 17090 IF IC_35HISTORY::SUBACCT <> SA_SALESMAN::SALESMAN

		!
		! Wrong record type
		!
		GOTO 17060 IF IC_35HISTORY::TRANSTYPE <> "SA"

		!
		! Wrong location?
		!
		GOTO 17060 IF COMP_ARRAY(EDIT$( &
			IC_35HISTORY::LOCATION, -1%), LOC_WLDCRD$) = 0% &
			IF LOC_WLDCRD$ <> ""

		ZEROFLAG% = -1%

		FOR I% = 1% to 12%

			ZEROFLAG% = 0% IF IC_35HISTORY::PQUANTITY(I%) <> 0.0

			ZEROFLAG% = 0% IF IC_35HISTORY::PRICEAMT(I%) <> 0.0

			ZEROFLAG% = 0% IF IC_35HISTORY::COSTAMT(I%) <> 0.0

		NEXT I%

		GOTO 17060 IF ZEROFLAG%

17080		!
		! Line number here just in case of array overflow.
		!
		! See if we already know about this product
		!
		FOR I% = 1% TO PRODUCT_LIST%
			!
			! We already know about it
			!
			GOTO 17060 IF PRODUCT_LIST$(I%) = IC_35HISTORY::PRODUCT

			!
			! Insert product here?
			!
			IF PRODUCT_LIST$(I%) > IC_35HISTORY::PRODUCT
			THEN
				PRODUCT_LIST$(J% + 1%) = PRODUCT_LIST$(J%) &
					FOR J% = PRODUCT_LIST% TO I% STEP -1%
				PRODUCT_LIST$(I%) = IC_35HISTORY::PRODUCT
				PRODUCT_LIST% = PRODUCT_LIST% + 1%
				GOTO 17060
			END IF

		NEXT I%

		!
		! Append to end of list
		!
		PRODUCT_LIST% = PRODUCT_LIST% + 1%
		PRODUCT_LIST$(PRODUCT_LIST%) = IC_35HISTORY::PRODUCT

		GOTO 17060

17090	NEXT Y%

17095	!*******************************************************************
	! Pass 2.
	! Now, run through the file one for each product the
	! salesman has sold
	!*******************************************************************

	!
	! Don't bother printing if there isn't any products to print
	!
	GOTO GetNextRec IF PRODUCT_LIST% = 0%

	!
	! Print title for salesman
	!
	TITLE$(3%) = TRM$(SA_SALESMAN::SALESMAN) + " " + &
		TRM$(SA_SALESMAN::DESCR)

	TEXT$ = ":::::::::: " + SA_SALESMAN::SALESMAN + " " + &
		TRM$(SA_SALESMAN::DESCR) + " " + &
		SA_SALESMAN::TTYPE + " " + &
		SA_SALESMAN::CLASS + " " + &
		TRM$(SA_SALESMAN::ADD1) + " " + &
		TRM$(SA_SALESMAN::ADD2) + " " + &
		PRNT_PHONE(SA_SALESMAN::PHONE, 0%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 10%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	FOR DO_PRODUCT% = 1% TO PRODUCT_LIST%

		THIS_PRODUCT$ = PRODUCT_LIST$(DO_PRODUCT%)

		!
		! Print totals for one salesman/product
		!
		GOSUB 17100

	NEXT DO_PRODUCT%

	GOSUB BrokerTotal

	GOTO GetNextRec


17100	!*******************************************************************
	! Process one salesman/product
	!*******************************************************************

	FOR Y% = 1% TO YEARS%

		PRINT_FLAG% = 0%

		WHEN ERROR IN
			FIND #IC_35HISTORY.CH%(Y%), &
				KEY #2% EQ SA_SALESMAN::SALESMAN, &
				REGARDLESS
		USE
			IF ERR = 155%
			THEN
				CONTINUE 17190
			END IF

			FILENAME$ = "IC_35HISTORY"
			CONTINUE HelpError
		END WHEN

 GetHistRec:
17110		!
		! Get one record from this file
		!
		WHEN ERROR IN
			GET #IC_35HISTORY.CH%(Y%), REGARDLESS
		USE
			IF ERR = 11%
			THEN
				CONTINUE 17190
			END IF

			FILENAME$ = "IC_35HISTORY"
			CONTINUE HelpError
		END WHEN

		GOTO GetHistFile &
			IF IC_35HISTORY::SUBACCT <> SA_SALESMAN::SALESMAN

		GOTO GetHistRec &
			IF IC_35HISTORY::PRODUCT <> THIS_PRODUCT$

		GOTO GetHistRec IF IC_35HISTORY::TRANSTYPE <> "SA"

		GOTO GetHistRec IF COMP_ARRAY(EDIT$( &
			IC_35HISTORY::LOCATION, -1%), LOC_WLDCRD$) = 0% &
			AND LOC_WLDCRD$ <> ""

		FOR I% = 1% TO 12%

			PQUANTITY(Y%, I%) = PQUANTITY(Y%, I%) - &
				IC_35HISTORY::PQUANTITY(I%)

			PRICEAMT(Y%, I%)  = PRICEAMT(Y%, I%) + &
				IC_35HISTORY::PRICEAMT(I%)

			COSTAMT(Y%, I%)   = COSTAMT(Y%, I%) + &
				IC_35HISTORY::COSTAMT(I%)

			J% = INT((I% - 1%) / 3%) + 1%

			QTYQTR(Y%,J%) = QTYQTR(Y%,J%) - &
				IC_35HISTORY::PQUANTITY(I%)

			PRCQTR(Y%,J%) = PRCQTR(Y%,J%) + &
				IC_35HISTORY::PRICEAMT(I%)

			CSTQTR(Y%,J%) = CSTQTR(Y%,J%) + &
				IC_35HISTORY::COSTAMT(I%)

			QTYYTD(Y%) = QTYYTD(Y%) - &
				IC_35HISTORY::PQUANTITY(I%)

			PRCYTD(Y%)  = PRCYTD(Y%) + &
				IC_35HISTORY::PRICEAMT(I%)

			CSTYTD(Y%)  = CSTYTD(Y%) + &
				IC_35HISTORY::COSTAMT(I%)

		NEXT I%

		GOTO GetHistRec

 GetHistFile:
17190	NEXT Y%

	!
	! Print whatever we found on this product
	!
	GOSUB PrintLine

	RETURN

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

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 10%)

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
					VARIANCE(I%) = 0.0 IF TOTALPRICEAMT(Y%, I%) = 0.0
				ELSE
					VARIANCE(I%) = (TOTALPRICEAMT(Y%, I%) - &
						TOTALPRICEAMT(Y% + 1%, I%)) / TOTALPRICEAMT(Y% + 1%, I%) * 100.0
				END IF
			NEXT I%

			FOR I% = 13% TO 16%

				IF TOTALPRCQTR(Y% + 1%, I% - 12%) = 0.0
				THEN
					VARIANCE(I%) = 100.0
					VARIANCE(I%) = 0.0 IF TOTALPRCQTR(Y%, I% - 12%) = 0.0
				ELSE
					VARIANCE(I%) = (TOTALPRCQTR(Y%, I% - 12%) - &
						TOTALPRCQTR(Y% + 1%, I% - 12%)) / TOTALPRCQTR(Y% + 1%, I% - 12%) * 100.0
				END IF

			NEXT I%

			IF TOTALPRCYTD(Y% + 1%) = 0.0
			THEN
				VARIANCE(17%) = 100.00
				VARIANCE(17%) = 0.0 IF TOTALPRCYTD(Y%) = 0.0
			ELSE
				VARIANCE(17%) = (TOTALPRCYTD(Y%) - &
					TOTALPRCYTD(Y% + 1%)) / TOTALPRCYTD(Y% + 1%) * 100.0
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

				IF TOTALPRICEAMT(Y% + 1%, I%) - TOTALCOSTAMT(Y% + 1%, I%) = 0.0
				THEN
					VARIANCE(I%) = 100.00
					VARIANCE(I%) = 0.0 IF TOTALPRICEAMT(Y%, I%) - TOTALCOSTAMT(Y%, I%) = 0.0
				ELSE
					VARIANCE(I%) = (TOTALPRICEAMT(Y%, I%) - TOTALCOSTAMT(Y%, I%) - &
						(TOTALPRICEAMT(Y% + 1%, I%) - TOTALCOSTAMT(Y% + 1%, I%))) / &
						(TOTALPRICEAMT(Y% + 1%, I%) - TOTALCOSTAMT(Y% + 1%, I%)) * 100.0
				END IF

			NEXT I%

			FOR I% = 13% TO 16%

				IF TOTALPRCQTR(Y% + 1%, I% - 12%) - TOTALCSTQTR(Y% + 1%, I% - 12%) = 0.0
				THEN
					VARIANCE(I%) = 100.0
					VARIANCE(I%) = 0.0 IF TOTALPRCQTR(Y%, I% - 12%) - TOTALCSTQTR(Y%, I% - 12%) = 0.0
				ELSE
					VARIANCE(I%) = (TOTALPRCQTR(Y%, I% - 12%) - TOTALCSTQTR(Y%, I% - 12%) - &
						(TOTALPRCQTR(Y% + 1%, I% - 12%) - TOTALCSTQTR(Y% + 1%, I% - 12%))) / &
						(TOTALPRCQTR(Y% + 1%, I% - 12%) - TOTALCSTQTR(Y% + 1%, I% - 12%)) * 100.0
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

	!*******************************************************************
	! Print out one parts line
	!*******************************************************************
 PrintLine:

	!
	! Get info on product
	!
	V% = PD_EXAM_PRODUCT(THIS_PRODUCT$, PD_PRODUCT_EXAM)

	!
	! Print out product title
	!
	TEXT$ = THIS_PRODUCT$ + "  -  " + &
		PD_PRODUCT_EXAM::DESCRIPTION

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 4%)

	!
	! Print totals for all years
	!
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

		IF GROSS_PROFIT$ = "Y" AND (Y% = 1% OR Y% > 1% AND &
			PRINT_VARIANCE$ <> "Y")
		THEN
			TEXT$ = "GProf " + &
				FORMAT$(PRICEAMT(Y%, 1%) - COSTAMT(Y%, 1%), "#######") + &
				FORMAT$(PRICEAMT(Y%, 2%) - COSTAMT(Y%, 2%), "#######") + &
				FORMAT$(PRICEAMT(Y%, 3%) - COSTAMT(Y%, 3%), "#######") + &
				FORMAT$(PRCQTR(Y%, 1%) - CSTQTR(Y%, 1%), "########") + &
				FORMAT$(PRICEAMT(Y%, 4%) - COSTAMT(Y%, 4%), "#######") + &
				FORMAT$(PRICEAMT(Y%, 5%) - COSTAMT(Y%, 5%), "#######") + &
				FORMAT$(PRICEAMT(Y%, 6%) - COSTAMT(Y%, 6%), "#######") + &
				FORMAT$(PRCQTR(Y%, 2%) - CSTQTR(Y%, 2%), "########") + &
				FORMAT$(PRICEAMT(Y%, 7%) - COSTAMT(Y%, 7%), "#######") + &
				FORMAT$(PRICEAMT(Y%, 8%) - COSTAMT(Y%, 8%), "#######") + &
				FORMAT$(PRICEAMT(Y%, 9%) - COSTAMT(Y%, 9%), "#######") + &
				FORMAT$(PRCQTR(Y%, 3%) - CSTQTR(Y%, 3%), "########") + &
				FORMAT$(PRICEAMT(Y%, 10%) - COSTAMT(Y%, 10%), "#######") + &
				FORMAT$(PRICEAMT(Y%, 11%) - COSTAMT(Y%, 11%), "#######") + &
				FORMAT$(PRICEAMT(Y%, 12%) - COSTAMT(Y%, 12%), "#######") + &
				FORMAT$(PRCQTR(Y%, 4%) - CSTQTR(Y%, 4%), "########") + &
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
					VARIANCE(I%) = 0.0 IF PQUANTITY(Y%, I%) = 0.0
				ELSE
					VARIANCE(I%) = (PQUANTITY(Y%, I%) - PQUANTITY(Y% + 1%, I%)) / &
						PQUANTITY(Y% + 1%, I%) * 100.0
				END IF

			NEXT I%

			FOR I% = 13% TO 16%

				IF QTYQTR(Y% + 1%, I% - 12%) = 0.0
				THEN
					VARIANCE(I%) = 100.0
					VARIANCE(I%) = 0.0 IF QTYQTR(Y%, I% - 12%) = 0.0
				ELSE
					VARIANCE(I%) = (QTYQTR(Y%, I% - 12%) - QTYQTR(Y% + 1%, I% - 12%)) / &
						QTYQTR(Y% + 1%, I% - 12%) * 100.0
				END IF

			NEXT I%

			IF QTYYTD(Y% + 1%) = 0.0
			THEN
				VARIANCE(17%) = 100.00
				VARIANCE(17%) = 0.0 IF QTYYTD(Y%) = 0.0
			ELSE
				VARIANCE(17%) = (QTYYTD(Y%) - QTYYTD(Y% + 1%)) / QTYYTD(Y% + 1%) * 100.0
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
					VARIANCE(I%) = 0.0 IF PRICEAMT(Y%, I%) = 0.0
				ELSE
					VARIANCE(I%) = (PRICEAMT(Y%, I%) - PRICEAMT(Y% + 1%, I%)) / &
						PRICEAMT(Y% + 1%, I%) * 100.0
				END IF

			NEXT I%

			FOR I% = 13% TO 16%

				IF PRCQTR(Y% + 1%, I% - 12%) = 0.0
				THEN
					VARIANCE(I%) = 100.0
					VARIANCE(I%) = 0.0 IF PRCQTR(Y%, I% - 12%) = 0.0
				ELSE
					VARIANCE(I%) = (PRCQTR(Y%, I% - 12%) - PRCQTR(Y% + 1%, I% - 12%)) / &
						PRCQTR(Y% + 1%, I% - 12%) * 100.0
				END IF

			NEXT I%

			IF PRCYTD(Y% + 1%) = 0.0
			THEN
				VARIANCE(17%) = 100.00
				VARIANCE(17%) = 0.0 IF PRCYTD(Y%) = 0.0
			ELSE
				VARIANCE(17%) = (PRCYTD(Y%) - PRCYTD(Y% + 1%)) / PRCYTD(Y% + 1%) * 100.0
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

				IF PRICEAMT(Y% + 1%, I%) - COSTAMT(Y% + 1%, I%) = 0.0
				THEN
					VARIANCE(I%) = 100.00
					VARIANCE(I%) = 0.0 IF PRICEAMT(Y%, I%) - COSTAMT(Y%, I%) = 0.0
				ELSE
					VARIANCE(I%) = (PRICEAMT(Y%, I%) - COSTAMT(Y%, I%) - &
						(PRICEAMT(Y% + 1%, I%) - COSTAMT(Y% + 1%, I%))) / &
						(PRICEAMT(Y% + 1%, I%) - COSTAMT(Y% + 1%, I%)) * 100.0
				END IF

			NEXT I%

			FOR I% = 13% TO 16%

				IF PRCQTR(Y% + 1%, I% - 12%) - CSTQTR(Y% + 1%, I% - 12%) = 0.0
				THEN
					VARIANCE(I%) = 100.0
					VARIANCE(I%) = 0.0 IF PRCQTR(Y%, I% - 12%) - CSTQTR(Y%, I% - 12%) = 0.0
				ELSE
					VARIANCE(I%) = (PRCQTR(Y%, I% - 12%) - CSTQTR(Y%, I% - 12%) - &
						(PRCQTR(Y% + 1%, I% - 12%) - CSTQTR(Y% + 1%, I% - 12%))) / &
						(PRCQTR(Y% + 1%, I% - 12%) - CSTQTR(Y% + 1%, I% - 12%)) * 100.0
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

	!
	! Calculate totals
	!
	FOR Y% = 1% TO YEARS%

		FOR I% = 1% TO 12%

			CUSTPRICEAMT(Y%, I%) = CUSTPRICEAMT(Y%, I%) + PRICEAMT(Y%, I%)
			CUSTCOSTAMT(Y%, I%) = CUSTCOSTAMT(Y%, I%) + COSTAMT(Y%, I%)

			BROKERPRICEAMT(Y%, I%) = BROKERPRICEAMT(Y%, I%) + PRICEAMT(Y%, I%)
			BROKERCOSTAMT(Y%, I%) = BROKERCOSTAMT(Y%, I%) + COSTAMT(Y%, I%)

		NEXT I%

		FOR I% = 1% TO 4%

			CUSTPRCQTR(Y%, I%) = CUSTPRCQTR(Y%, I%) + PRCQTR(Y%, I%)
			CUSTCSTQTR(Y%, I%) = CUSTCSTQTR(Y%, I%) + CSTQTR(Y%, I%)

			BROKERPRCQTR(Y%, I%) = BROKERPRCQTR(Y%, I%) + PRCQTR(Y%, I%)
			BROKERCSTQTR(Y%, I%) = BROKERCSTQTR(Y%, I%) + CSTQTR(Y%, I%)


		NEXT I%

		CUSTPRCYTD(Y%) = CUSTPRCYTD(Y%) + PRCYTD(Y%)
		CUSTCSTYTD(Y%) = CUSTCSTYTD(Y%) + CSTYTD(Y%)

		BROKERPRCYTD(Y%) = BROKERPRCYTD(Y%) + PRCYTD(Y%)
		BROKERCSTYTD(Y%) = BROKERCSTYTD(Y%) + CSTYTD(Y%)


	NEXT Y%

	FOR Y% = 1% TO YEARS%

		QTYYTD(Y%) = 0.0
		PRCYTD(Y%) = 0.0
		CSTYTD(Y%) = 0.0

		FOR I% = 1% TO 4%

			QTYQTR(Y%, I%)         = 0.0
			PRCQTR(Y%, I%)         = 0.0
			CSTQTR(Y%, I%)         = 0.0

			PQUANTITY(Y%, I%)      = 0.0
			PRICEAMT(Y%, I%)       = 0.0
			COSTAMT(Y%, I%)        = 0.0

			PQUANTITY(Y%, I% + 4%) = 0.0
			PRICEAMT(Y%, I% + 4%)  = 0.0
			COSTAMT(Y%, I% + 4%)   = 0.0

			PQUANTITY(Y%, I% + 8%) = 0.0
			PRICEAMT(Y%, I% + 8%)  = 0.0
			COSTAMT(Y%, I% + 8%)   = 0.0

		NEXT I%

	NEXT Y%

	RETURN

	!
	! Print out one line
	!

 BrokerTotal:
	TEXT$ = ":Total:::: " + SA_SALESMAN::SALESMAN + " " + &
		TRM$(SA_SALESMAN::DESCR) + " " + &
		SA_SALESMAN::TTYPE + " " + &
		SA_SALESMAN::CLASS + " " + &
		TRM$(SA_SALESMAN::ADD1) + " " + &
		TRM$(SA_SALESMAN::ADD2) + " " + &
		PRNT_PHONE(SA_SALESMAN::PHONE, 0%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)

	FOR Y% = 1% TO YEARS%

		GOTO BrokerVar IF Y% <> 1% AND Y% = YEARS% AND &
			PRINT_VARIANCE$ = "Y"

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

		IF GROSS_PROFIT$ = "Y"
		THEN
			TEXT$ = "GProf " + &
				FORMAT$(BROKERPRICEAMT(Y%, 1%) - BROKERCOSTAMT(Y%, 1%), "#######") + &
				FORMAT$(BROKERPRICEAMT(Y%, 2%) - BROKERCOSTAMT(Y%, 2%), "#######") + &
				FORMAT$(BROKERPRICEAMT(Y%, 3%) - BROKERCOSTAMT(Y%, 3%), "#######") + &
				FORMAT$(BROKERPRCQTR(Y%, 1%) - BROKERCSTQTR(Y%, 1%), "########") + &
				FORMAT$(BROKERPRICEAMT(Y%, 4%) - BROKERCOSTAMT(Y%, 4%), "#######") + &
				FORMAT$(BROKERPRICEAMT(Y%, 5%) - BROKERCOSTAMT(Y%, 5%), "#######") + &
				FORMAT$(BROKERPRICEAMT(Y%, 6%) - BROKERCOSTAMT(Y%, 6%), "#######") + &
				FORMAT$(BROKERPRCQTR(Y%, 2%) - BROKERCSTQTR(Y%, 2%), "########") + &
				FORMAT$(BROKERPRICEAMT(Y%, 7%) - BROKERCOSTAMT(Y%, 7%), "#######") + &
				FORMAT$(BROKERPRICEAMT(Y%, 8%) - BROKERCOSTAMT(Y%, 8%), "#######") + &
				FORMAT$(BROKERPRICEAMT(Y%, 9%) - BROKERCOSTAMT(Y%, 9%), "#######") + &
				FORMAT$(BROKERPRCQTR(Y%, 3%) - BROKERCSTQTR(Y%, 3%), "########") + &
				FORMAT$(BROKERPRICEAMT(Y%, 10%) - BROKERCOSTAMT(Y%, 10%), "#######") + &
				FORMAT$(BROKERPRICEAMT(Y%, 11%) - BROKERCOSTAMT(Y%, 11%), "#######") + &
				FORMAT$(BROKERPRICEAMT(Y%, 12%) - BROKERCOSTAMT(Y%, 12%), "#######") + &
				FORMAT$(BROKERPRCQTR(Y%, 4%) - BROKERCSTQTR(Y%, 4%), "########") + &
				FORMAT$(BROKERPRCYTD(Y%) - BROKERCSTYTD(Y%),  "########")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 1%)

		END IF

 BrokerVar:
		IF PRINT_VARIANCE$ = "Y" AND Y% <> YEARS%
		THEN
			FOR I% = 1% TO 12%

				IF BROKERPRICEAMT(Y% + 1%, I%) = 0.0
				THEN
					VARIANCE(I%) = 100.00
					VARIANCE(I%) = 0.0 IF BROKERPRICEAMT(Y%, I%) = 0.0
				ELSE
					VARIANCE(I%) = (BROKERPRICEAMT(Y%, I%) - BROKERPRICEAMT(Y% + 1%, I%)) / &
						BROKERPRICEAMT(Y% + 1%, I%) * 100.0
				END IF

			NEXT I%

			FOR I% = 13% TO 16%

				IF BROKERPRCQTR(Y% + 1%, I% - 12%) = 0.0
				THEN
					VARIANCE(I%) = 100.0
					VARIANCE(I%) = 0.0 IF BROKERPRCQTR(Y%, I% - 12%) = 0.0
				ELSE
					VARIANCE(I%) = (BROKERPRCQTR(Y%, I% - 12%) - &
						BROKERPRCQTR(Y% + 1%, I% - 12%)) / &
						BROKERPRCQTR(Y% + 1%, I% - 12%) * 100.0
				END IF

			NEXT I%

			IF BROKERPRCYTD(Y% + 1%) = 0.0
			THEN
				VARIANCE(17%) = 100.00
				VARIANCE(17%) = 0.0 IF BROKERPRCYTD(Y%) = 0.0
			ELSE
				VARIANCE(17%) = (BROKERPRCYTD(Y%) - BROKERPRCYTD(Y% + 1%)) / &
					BROKERPRCYTD(Y% + 1%) * 100.0
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

				IF BROKERPRICEAMT(Y% + 1%, I%) - BROKERCOSTAMT(Y% + 1%, I%) = 0.0
				THEN
					VARIANCE(I%) = 100.00
					VARIANCE(I%) = 0.0 IF BROKERPRICEAMT(Y%, I%) - BROKERCOSTAMT(Y%, I%) = 0.0
				ELSE
					VARIANCE(I%) = (BROKERPRICEAMT(Y%, I%) - BROKERCOSTAMT(Y%, I%) - &
						(BROKERPRICEAMT(Y% + 1%, I%) - BROKERCOSTAMT(Y% + 1%, I%))) / &
						(BROKERPRICEAMT(Y% + 1%, I%) - BROKERCOSTAMT(Y% + 1%, I%)) * 100.0
				END IF

			NEXT I%

			FOR I% = 13% TO 16%

				IF BROKERPRCQTR(Y% + 1%, I% - 12%) - BROKERCSTQTR(Y% + 1%, I% - 12%) = 0.0
				THEN
					VARIANCE(I%) = 100.0
					VARIANCE(I%) = 0.0 IF BROKERPRCQTR(Y%, I% - 12%) - &
						BROKERCSTQTR(Y%, I% - 12%) = 0.0
				ELSE
					VARIANCE(I%) = (BROKERPRCQTR(Y%, I% - 12%) - &
						BROKERCSTQTR(Y%, I% - 12%) - &
						(BROKERPRCQTR(Y% + 1%, I% - 12%) - &
						BROKERCSTQTR(Y% + 1%, I% - 12%))) / &
						(BROKERPRCQTR(Y% + 1%, I% - 12%) - &
						BROKERCSTQTR(Y% + 1%, I% - 12%)) * 100.0
				END IF

			NEXT I%

			IF BROKERPRCYTD(Y% + 1%) - BROKERCSTYTD(Y% + 1%) = 0.0
			THEN
				VARIANCE(17%) = 100.00
				VARIANCE(17%) = 0.0 IF BROKERPRCYTD(Y%) - BROKERCSTYTD(Y%) = 0.0
			ELSE
				VARIANCE(17%) = (BROKERPRCYTD(Y%) - BROKERCSTYTD(Y%) - &
					(BROKERPRCYTD(Y% + 1%) - BROKERCSTYTD(Y% + 1%))) / &
					(BROKERPRCYTD(Y% + 1%) - BROKERCSTYTD(Y% + 1%)) * 100.0
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

	FOR Y% = 1% TO YEARS%

		FOR I% = 1% TO 12%

			TOTALPRICEAMT(Y%, I%) = TOTALPRICEAMT(Y%, I%) + BROKERPRICEAMT(Y%, I%)
			TOTALCOSTAMT(Y%, I%) = TOTALCOSTAMT(Y%, I%) + BROKERCOSTAMT(Y%, I%)

		NEXT I%

	NEXT Y%

	FOR Y% = 1% TO YEARS%

		TOTALPRCYTD(Y%)  = TOTALPRCYTD(Y%) + BROKERPRCYTD(Y%)
		TOTALCSTYTD(Y%)  = TOTALCSTYTD(Y%) + BROKERCSTYTD(Y%)

		BROKERPRCYTD(Y%)  = 0.0
		BROKERCSTYTD(Y%)  = 0.0

		FOR I% = 1% TO 4%

			TOTALPRCQTR(Y%, I%) = TOTALPRCQTR(Y%, I%) + BROKERPRCQTR(Y%, I%)
			TOTALCSTQTR(Y%, I%) = TOTALCSTQTR(Y%, I%) + BROKERCSTQTR(Y%, I%)

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
	! End of report SA_RPRT_SALPRD
	!******************************************************************
	END
