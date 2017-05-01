1	%TITLE "Sales Summary Report by Salesman for King B"
	%SBTTL "SA_RPRT_SALSUMKB"
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
	! ID:SA0025
	!
	! Abstract:HELP
	!	.p
	!	The ^*Price Type Sales Summary Report by Salesman\* option
	!	prints a sales report listing the following
	!	information:
	!	.b
	!	.lm +10
	!	.list 0,"*"
	!	.le
	!	Price Type
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
	!	$ BAS SA_SOURCE:SA_RPRT_SALSUMKB/LINE
	!	$ LINK/EXECUTABLE=SA_EXE: SA_RPRT_SALSUMKB, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE SA_RPRT_SALSUMKB.OBJ;*
	!
	! Author:
	!
	!	12/16/92 - Dan Perkins
	!
	! Modification history:
	!
	!	01/06/93 - Dan Perkins
	!
	!	01/06/93 - Frank F. Starman
	!		Fixed some tests in ARStuff subroutine.
	!
	!	01/11/93 - Dan Perkins
	!		Page after each salesman.
	!
	!	02/02/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	02/02/93 - Kevin Handy
	!		Changed error trap for 17310 to 17320 to match
	!		with existing line number.
	!
	!	03/02/93 - Dan Perkins
	!		Removed ARStuff for a sub-routine and placed it
	!		directly in CustTotal.
	!
	!	09/07/93 - Kevin Handy
	!		Removed code that had been commented out.
	!
	!	09/07/93 - Kevin Handy
	!		Changed SY% to SOMEYEAR% and Y% to YEARLOOP%
	!		so I didn't have to look up what they meant all the
	!		time.
	!
	!	01/05/95 - Kevin Handy
	!		Added looking at "RT" types. Was only looking at
	!		"SA" types before.
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
	!	11/05/2000 - Kevin Handy
	!		Use A"x"B
	!		Use WHEN ERROR IN
	!
	!	12/10/2002 - Kevin Handy
	!		Tried to increase the number of digits available
	!		to display numbers.
	!
	!	04/08/2003 - Kevin Handy
	!		Try to get more digits per column whre possible (kbj)
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
	EXTERNAL REAL	FUNCTION PC_READ_PRICE

	%PAGE

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
	DECLARE			IC_35HISTORY_CDD	IC_HISTORY(2%)

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	DECLARE			AR_35CUSTOM_CDD		AR_35CUSTOM_EXAM

	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.HB"
	MAP (AR_OPEN)		AR_OPEN_CDD		AR_OPEN

	%INCLUDE "SOURCE:[AR.OPEN]AR_CLOSED.HB"
	MAP (AR_CLOSED)		AR_CLOSED_CDD		AR_CLOSED

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	!
	! Array for variances
	!
	DECLARE REAL VARIANCE(17%)

	!
	! Used in CustTotal
	!
	DECLARE REAL CUSTPRICEAMT(2%, 12%)
	DECLARE REAL CUSTCOSTAMT(2%, 12%)

	!
	! Used in BrokerTotal
	!
	DECLARE REAL BROKERPRICEAMT(2%, 12%)
	DECLARE REAL BROKERCOSTAMT(2%, 12%)

	!
	! Used in ExitTotal, created in BrokerTotal
	!
	DECLARE REAL TOTALPRICEAMT(2%, 12%)
	DECLARE REAL TOTALCOSTAMT(2%, 12%)

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
	!	in which the report will PRINT_
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
	!	item to begin with.
	!	.p
	!	A blank field will cause the report to begin with the first
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
	!	to end the report with.
	!	.p
	!	A blank field causes the report to end with the last
	!	item in the file.
	!
	! Index:
	!
	!--

	PRICE.TYPE$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Price Type\*
	!	.p
	!	The ^*Price Type\* field contains the Price Type
	!	to generate the report for.
	!	The Price Type must be defined in the IC Price Type
	!	table or no prices will be printed.
	!	.p
	!	An entry is required in this field.
	!
	! Index:
	!
	!--

	LOC_WLDCRD$ = "*"

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

	TOTAL.ONLY$ = EDIT$(UTL_REPORTX::OPTDEF(5%), -1%)

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

	CUR_YEAR$, YYYY$ = EDIT$(UTL_REPORTX::OPTDEF(8%), -1%)
	CURR_YEAR% = VAL%(CUR_YEAR$)
	LAST_YEAR% = CURR_YEAR% - 1%
	LAST_YYYY$ = NUM1$(LAST_YEAR%)

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

	YEARS% = 2%

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
	FOR YEARLOOP% = 1% TO YEARS%

		WHEN ERROR IN
			%INCLUDE "SOURCE:[IC.OPEN]IC_35HISTORY.OPN"
		USE
			CONTINUE 320 IF ERR = 5%
			FILENAME$ = "IC_35HISTORY"
			CONTINUE HelpError
		END WHEN

		VALID_YEARS% = YEARLOOP%
		IC_35HISTORY.CH%(YEARLOOP%) = IC_35HISTORY.CH%
		IC_35HISTORY.CH% = 0.0
		YYYY$(YEARLOOP%) = YYYY$
		YYYY$ = FORMAT$(VAL%(YYYY$) - 1%, "####")

	NEXT YEARLOOP%

320	!
	! Open AR_OPEN file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.OPN"
	USE
		CONTINUE 330 IF ERR = 5%
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

330	!
	! Open AR_CLOSED file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CLOSED.OPN"
	USE
		CONTINUE ReportTitle IF ERR = 5%
		FILENAME$ = "AR_CLOSED"
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
	YEARS% = VALID_YEARS%

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
		TITLE2$, TITLE$(2%) = "  COST TYPE " + PRICE.TYPE$ + &
			" FOR ALL LOCATIONS"
	ELSE
		TITLE2$, TITLE$(2%) = "  COST TYPE " + PRICE.TYPE$ + &
			" FOR LOCATION " + LOC_WLDCRD$
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

	IF TOTAL.ONLY$ = "Y"
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

	PAGE%, PRINT_FLAG% = 0%

 GetNextRec:
	!
	! Print totals after each salesman
	!
	IF PRINT_FLAG%
	THEN
		NO_TEST%(YEARLOOP%) = 0% FOR YEARLOOP% = 1% TO YEARS%
		GOSUB CustTotal
		GOSUB BrokerTotal
	END IF

	NEW.SALESMAN% = -1%

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

	GOTO ExitProgram &
		IF SA_SALESMAN::SUBJECT <> "S"

	SELECT SORTBY$

	CASE "C"
		GOTO ExitTotal &
			IF (SA_SALESMAN::CLASS > TO_ITEM$) AND TO_ITEM$ <> ""

	CASE "S"
		GOTO ExitTotal &
			IF (SA_SALESMAN::SALESMAN > TO_ITEM$) AND TO_ITEM$ <> ""

	CASE "T"
		GOTO ExitTotal &
			IF (SA_SALESMAN::TTYPE > TO_ITEM$) AND TO_ITEM$ <> ""

	END SELECT

	TITLE$(2%) = "FOR SALESMAN " + TRM$(SB_SUBACCOUNT::SUBACCOUNT) + TITLE2$
	TEST.LINE$ = ""

17100	!
	! Get History record
	!
	FOR YEARLOOP% = 1% TO YEARS%

		EOF%(YEARLOOP%) = 0%
		NO_TEST%(YEARLOOP%) = 0%

		WHEN ERROR IN
			GET #IC_35HISTORY.CH%(YEARLOOP%), &
				KEY #2% EQ SA_SALESMAN::SALESMAN, &
				REGARDLESS
		USE
			IF ERR = 155%
			THEN
				EOF%(YEARLOOP%) = 1%
				CONTINUE 17115
			END IF

			FILENAME$ = "IC_35HISTORY"
			CONTINUE HelpError
		END WHEN

		IC_HISTORY(YEARLOOP%) = IC_35HISTORY

17115	NEXT YEARLOOP%

	SOMEYEAR% = 1%
	GOTO TestKey

 GetHistRec:
17120	SOMEYEAR% = YEARLOOP%

	WHEN ERROR IN
		GET #IC_35HISTORY.CH%(YEARLOOP%), REGARDLESS
	USE
		IF ERR = 11%
		THEN
			EOF%(YEARLOOP%) = 1%
			CONTINUE TestKey
		END IF

		FILENAME$ = "IC_35HISTORY"
		CONTINUE HelpError
	END WHEN

	IC_HISTORY(YEARLOOP%) = IC_35HISTORY

 TestKey:
	TEST_EOF% = 1%

	FOR YEARLOOP% = 1% TO YEARS%

		EOF%(YEARLOOP%) = 1% &
			IF IC_HISTORY(YEARLOOP%)::SUBACCT <> &
			SA_SALESMAN::SALESMAN
		TEST_EOF% = TEST_EOF% * EOF%(YEARLOOP%)

	NEXT YEARLOOP%

	GOTO GetNextRec &
		IF TEST_EOF% = 1%

	FOR YEARLOOP% = SOMEYEAR% TO YEARS%

		IF EOF%(YEARLOOP%) <> 1% AND NO_TEST%(YEARLOOP%) <> 1%
		THEN
			GOTO GetHistRec &
				IF IC_HISTORY(YEARLOOP%)::TRANSTYPE <> &
				"SA" AND &
				IC_HISTORY(YEARLOOP%)::TRANSTYPE <> "RT"

			GOTO GetHistRec &
				IF COMP_ARRAY(EDIT$( &
				IC_HISTORY(YEARLOOP%)::LOCATION, -1%), &
				LOC_WLDCRD$) = 0% &
				AND LOC_WLDCRD$ <> ""

			GOTO GetHistRec &
				IF PD_EXAM_PRODUCT(IC_HISTORY(YEARLOOP%)::PRODUCT, &
				PD_PRODUCT_EXAM) <> CMC$_NORMAL
		ELSE
			NO_TEST%(YEARLOOP%) = 1%
		END IF

	NEXT YEARLOOP%

	NO_TEST% = 1%
	CUST.TEST% = -1%

	FOR YEARLOOP% = 1% TO YEARS%

		IF TEST.LINE$ <> IC_HISTORY(YEARLOOP%)::SUBACCT + &
			IC_HISTORY(YEARLOOP%)::CROSSREF + &
			IC_HISTORY(YEARLOOP%)::PRODUCT AND &
				TEST.LINE$ <> ""
		THEN
			CUST.TEST% = 0% &
				IF LEFT(TEST.LINE$, 20%) = &
				IC_HISTORY(YEARLOOP%)::SUBACCT + &
				IC_HISTORY(YEARLOOP%)::CROSSREF

			NO_TEST%(YEARLOOP%) = 1%
		ELSE
			CUST.TEST% = 0%
		END IF

		NO_TEST% = NO_TEST% * NO_TEST%(YEARLOOP%)

	NEXT YEARLOOP%

	IF NO_TEST%
	THEN
		NO_TEST%(YEARLOOP%) = 0% FOR YEARLOOP% = 1% TO YEARS%

		GOSUB CustTotal &
			IF CUST.TEST%
	END IF

	TEST.LINE$ = SA_SALESMAN::SALESMAN + CHR$(255%)

	!
	! Find minimum
	!
	FOR YEARLOOP% = 1% TO YEARS%

		IF EOF%(YEARLOOP%) = 0% AND &
			TEST.LINE$ > IC_HISTORY(YEARLOOP%)::SUBACCT + &
				IC_HISTORY(YEARLOOP%)::CROSSREF + &
				IC_HISTORY(YEARLOOP%)::PRODUCT
		THEN
			TEST.LINE$ = IC_HISTORY(YEARLOOP%)::SUBACCT + &
				IC_HISTORY(YEARLOOP%)::CROSSREF + &
				IC_HISTORY(YEARLOOP%)::PRODUCT

			LAST_CROSSREF$ = IC_HISTORY(YEARLOOP%)::CROSSREF

		END IF

	NEXT YEARLOOP%

	FOR YEARLOOP% = 1% TO YEARS%

		IF TEST.LINE$ = IC_HISTORY(YEARLOOP%)::SUBACCT + &
			IC_HISTORY(YEARLOOP%)::CROSSREF + &
			IC_HISTORY(YEARLOOP%)::PRODUCT
		THEN
			COST = PC_READ_PRICE(IC_HISTORY(YEARLOOP%)::PRODUCT, &
				IC_HISTORY(YEARLOOP%)::LOCATION, &
				PRICE.TYPE$, "", "", &
				"", "")

			FOR I% = 1% TO 12%

				EXT.COST = FUNC_ROUND(COST * &
					-IC_HISTORY(YEARLOOP%)::PQUANTITY(I%), 2%)

				CUSTCOSTAMT(YEARLOOP%, I%) = &
					CUSTCOSTAMT(YEARLOOP%, I%) + &
					EXT.COST

				J% = INT((I% - 1%) / 3%) + 1%

				CUSTCSTQTR(YEARLOOP%, J%) = &
					CUSTCSTQTR(YEARLOOP%, J%) + &
					EXT.COST

				CUSTCSTYTD(YEARLOOP%) = &
					CUSTCSTYTD(YEARLOOP%) + &
					EXT.COST

			NEXT I%
		ELSE
			NO_TEST%(YEARLOOP%) = 1%
		END IF

	NEXT YEARLOOP%

	PRINT_FLAG% = -1%

	IF NEW.SALESMAN%
	THEN
		TEXT$ = "::::::::::"  + "   " + &
		SA_SALESMAN::SALESMAN + "   " + &
		SA_SALESMAN::DESCR + "   " + &
		SA_SALESMAN::TTYPE + "      " + &
		SA_SALESMAN::CLASS

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, PAGE%)

		TEXT$ = SPACE$(28%) + &
			SA_SALESMAN::ADD1 + "   " + &
			SA_SALESMAN::ADD2 + "   " + &
			SA_SALESMAN::CITY + "   " + &
			SA_SALESMAN::STATE + "      " + &
			PRNT_PHONE(SA_SALESMAN::PHONE, 0%)

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		NEW.SALESMAN%, PAGE% = 0%
	END IF

17130	FOR YEARLOOP% = YEARS% TO 1% STEP -1%

		IF NO_TEST%(YEARLOOP%) = 0%
		THEN
			SOMEYEAR% = YEARLOOP%

			WHEN ERROR IN
				GET #IC_35HISTORY.CH%(YEARLOOP%), REGARDLESS
			USE
				IF ERR = 11%
				THEN
					EOF%(YEARLOOP%) = 1%
					CONTINUE 17135
				END IF

				FILENAME$ = "IC_35HISTORY"
				CONTINUE HelpError
			END WHEN

			IC_HISTORY(YEARLOOP%) = IC_35HISTORY
		END IF

17135	NEXT YEARLOOP%

	GOTO TestKey

	%PAGE

 ExitTotal:
	!
	! Print out totals
	!
	TITLE$(2%) = TITLE2$

	TEXT$ = "     Report Grand Total"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, PAGE%)

	FOR YEARLOOP% = 1% TO YEARS%

		TEXT$ = SPACE$(25%) + ":   "
		TEXT$ = "Year " + YYYY$(YEARLOOP%) + &
			TEXT$ + TEXT$ + TEXT$ + TEXT$

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		TEXT$ = "Sales" + &
			FORMAT$(TOTALPRICEAMT(YEARLOOP%, 1%), "########") + &
			SPACE$(5%) + &
			FORMAT$(TOTALPRICEAMT(YEARLOOP%, 3%), "#########") + &
			SPACE$(6%) + &
			FORMAT$(TOTALPRICEAMT(YEARLOOP%, 4%), "#########") + &
			SPACE$(5%) + &
			FORMAT$(TOTALPRICEAMT(YEARLOOP%, 6%), "#########") + &
			SPACE$(6%) + &
			FORMAT$(TOTALPRICEAMT(YEARLOOP%, 7%), "#########") + &
			SPACE$(5%) + &
			FORMAT$(TOTALPRICEAMT(YEARLOOP%, 9%), "#########") + &
			SPACE$(6%) + &
			FORMAT$(TOTALPRICEAMT(YEARLOOP%, 10%), "#########") + &
			SPACE$(5%) + &
			FORMAT$(TOTALPRICEAMT(YEARLOOP%, 12%), "#########") + &
			SPACE$(6%) + &
			FORMAT$(TOTALPRCYTD(YEARLOOP%), "##########")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		TEXT$ = "      " + &
			SPACE$(5%) + &
			FORMAT$(TOTALPRICEAMT(YEARLOOP%, 2%), "#########") + &
			SPACE$(5%) + &
			FORMAT$(TOTALPRCQTR(YEARLOOP%, 1%), "##########") + &
			SPACE$(5%) + &
			FORMAT$(TOTALPRICEAMT(YEARLOOP%, 5%), "#########") + &
			SPACE$(5%) + &
			FORMAT$(TOTALPRCQTR(YEARLOOP%, 2%), "##########") + &
			SPACE$(5%) + &
			FORMAT$(TOTALPRICEAMT(YEARLOOP%, 8%), "#########") + &
			SPACE$(5%) + &
			FORMAT$(TOTALPRCQTR(YEARLOOP%, 3%), "##########") + &
			SPACE$(5%) + &
			FORMAT$(TOTALPRICEAMT(YEARLOOP%, 11%), "#########") + &
			SPACE$(5%) + &
			FORMAT$(TOTALPRCQTR(YEARLOOP%, 4%), "##########")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		IF GROSS_PROFIT$ = "Y"
		THEN
			TEXT$ = "GProf" + &
				FORMAT$(TOTALPRICEAMT(YEARLOOP%, 1%) - &
				TOTALCOSTAMT(YEARLOOP%, 1%), "########") + &
				SPACE$(5%) + &
				FORMAT$(TOTALPRICEAMT(YEARLOOP%, 3%) - &
				TOTALCOSTAMT(YEARLOOP%, 3%), "#########") + &
				SPACE$(6%) + &
				FORMAT$(TOTALPRICEAMT(YEARLOOP%, 4%) - &
				TOTALCOSTAMT(YEARLOOP%, 4%), "#########") + &
				SPACE$(5%) + &
				FORMAT$(TOTALPRICEAMT(YEARLOOP%, 6%) - &
				TOTALCOSTAMT(YEARLOOP%, 6%), "#########") + &
				SPACE$(6%) + &
				FORMAT$(TOTALPRICEAMT(YEARLOOP%, 7%) - &
				TOTALCOSTAMT(YEARLOOP%, 7%), "#########") + &
				SPACE$(5%) + &
				FORMAT$(TOTALPRICEAMT(YEARLOOP%, 9%) - &
				TOTALCOSTAMT(YEARLOOP%, 9%), "#########") + &
				SPACE$(6%) + &
				FORMAT$(TOTALPRICEAMT(YEARLOOP%, 10%) - &
				TOTALCOSTAMT(YEARLOOP%, 10%), "#########") + &
				SPACE$(5%) + &
				FORMAT$(TOTALPRICEAMT(YEARLOOP%, 12%) - &
				TOTALCOSTAMT(YEARLOOP%, 12%), "#########") + &
				SPACE$(6%) + &
				FORMAT$(TOTALPRCYTD(YEARLOOP%) - &
				TOTALCSTYTD(YEARLOOP%), "##########")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

			TEXT$ = "      " + &
				SPACE$(5%) + &
				FORMAT$(TOTALPRICEAMT(YEARLOOP%, 2%) - &
				TOTALCOSTAMT(YEARLOOP%, 2%), "#########") + &
				SPACE$(5%) + &
				FORMAT$(TOTALPRCQTR(YEARLOOP%, 1%) - &
				TOTALCSTQTR(YEARLOOP%, 1%), "##########") + &
				SPACE$(5%) + &
				FORMAT$(TOTALPRICEAMT(YEARLOOP%, 5%) - &
				TOTALCOSTAMT(YEARLOOP%, 5%), "#########") + &
				SPACE$(5%) + &
				FORMAT$(TOTALPRCQTR(YEARLOOP%, 2%) - &
				TOTALCSTQTR(YEARLOOP%, 2%), "##########") + &
				SPACE$(5%) + &
				FORMAT$(TOTALPRICEAMT(YEARLOOP%, 8%) - &
				TOTALCOSTAMT(YEARLOOP%, 8%), "#########") + &
				SPACE$(5%) + &
				FORMAT$(TOTALPRCQTR(YEARLOOP%, 3%) - &
				TOTALCSTQTR(YEARLOOP%, 3%), "##########") + &
				SPACE$(5%) + &
				FORMAT$(TOTALPRICEAMT(YEARLOOP%, 11%) - &
				TOTALCOSTAMT(YEARLOOP%, 11%), "#########") + &
				SPACE$(5%) + &
				FORMAT$(TOTALPRCQTR(YEARLOOP%, 4%) - &
				TOTALCSTQTR(YEARLOOP%, 4%), "##########")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
			GOTO ExitProgram IF UTL_REPORTX::STAT

		END IF

 TotalVar:
		IF PRINT_VARIANCE$ = "Y" AND YEARLOOP% <> YEARS%
		THEN
			FOR I% = 1% TO 12%

				IF TOTALPRICEAMT(YEARLOOP% + 1%, I%) = 0.0
				THEN
					VARIANCE(I%) = 100.00
					VARIANCE(I%) = 0.0 &
						IF TOTALPRICEAMT(YEARLOOP%, I%) = 0.0
				ELSE
					VARIANCE(I%) = (TOTALPRICEAMT(YEARLOOP%, I%) - &
						TOTALPRICEAMT(YEARLOOP% + 1%, I%)) / &
						TOTALPRICEAMT(YEARLOOP% + 1%, I%) * 100.0
				END IF

			NEXT I%

			FOR I% = 13% TO 16%

				IF TOTALPRCQTR(YEARLOOP% + 1%, I% - 12%) = 0.0
				THEN
					VARIANCE(I%) = 100.0
					VARIANCE(I%) = 0.0 &
						IF TOTALPRCQTR(YEARLOOP%, I% - 12%) = 0.0
				ELSE
					VARIANCE(I%) = (TOTALPRCQTR(YEARLOOP%, I% - 12%) - &
						TOTALPRCQTR(YEARLOOP% + 1%, I% - 12%)) / &
						TOTALPRCQTR(YEARLOOP% + 1%, I% - 12%) * 100.0
				END IF

			NEXT I%

			IF TOTALPRCYTD(YEARLOOP% + 1%) = 0.0
			THEN
				VARIANCE(17%) = 100.00
				VARIANCE(17%) = 0.0 &
					IF TOTALPRCYTD(YEARLOOP%) = 0.0
			ELSE
				VARIANCE(17%) = (TOTALPRCYTD(YEARLOOP%) - &
					TOTALPRCYTD(YEARLOOP% + 1%)) / &
					TOTALPRCYTD(YEARLOOP% + 1%) * 100.0
			END IF

			TEXT$ = "SVar " + &
				FORMAT$(VARIANCE(1%), "#######%") + &
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

				IF TOTALPRICEAMT(YEARLOOP% + 1%, I%) - &
					TOTALCOSTAMT(YEARLOOP% + 1%, I%) = 0.0
				THEN
					VARIANCE(I%) = 100.00
					VARIANCE(I%) = 0.0 &
						IF TOTALPRICEAMT(YEARLOOP%, I%) - &
						TOTALCOSTAMT(YEARLOOP%, I%) = 0.0
				ELSE
					VARIANCE(I%) = (TOTALPRICEAMT(YEARLOOP%, I%) - &
						TOTALCOSTAMT(YEARLOOP%, I%) - &
						(TOTALPRICEAMT(YEARLOOP% + 1%, I%) - &
						TOTALCOSTAMT(YEARLOOP% + 1%, I%))) / &
						(TOTALPRICEAMT(YEARLOOP% + 1%, I%) - &
						TOTALCOSTAMT(YEARLOOP% + 1%, I%)) * 100.0
				END IF

			NEXT I%

			FOR I% = 13% TO 16%

				IF TOTALPRCQTR(YEARLOOP% + 1%, I% - 12%) - &
					TOTALCSTQTR(YEARLOOP% + 1%, I% - 12%) = 0.0
				THEN
					VARIANCE(I%) = 100.0
					VARIANCE(I%) = 0.0 &
						IF TOTALPRCQTR(YEARLOOP%, I% - 12%) - &
						TOTALCSTQTR(YEARLOOP%, I% - 12%) = 0.0
				ELSE
					VARIANCE(I%) = (TOTALPRCQTR(YEARLOOP%, I% - 12%) - &
						TOTALCSTQTR(YEARLOOP%, I% - 12%) - &
						(TOTALPRCQTR(YEARLOOP% + 1%, I% - 12%) - &
						TOTALCSTQTR(YEARLOOP% + 1%, I% - 12%))) / &
						(TOTALPRCQTR(YEARLOOP% + 1%, I% - 12%) - &
						TOTALCSTQTR(YEARLOOP% + 1%, I% - 12%)) * 100.0
				END IF

			NEXT I%

			IF TOTALPRCYTD(YEARLOOP% + 1%) - &
				TOTALCSTYTD(YEARLOOP% + 1%) = 0.0
			THEN
				VARIANCE(17%) = 100.00
				VARIANCE(17%) = 0.0 &
					IF TOTALPRCYTD(YEARLOOP%) - &
					TOTALCSTYTD(YEARLOOP%) = 0.0
			ELSE
				VARIANCE(17%) = (TOTALPRCYTD(YEARLOOP%) - &
					TOTALCSTYTD(YEARLOOP%) - &
					(TOTALPRCYTD(YEARLOOP% + 1%) - &
					TOTALCSTYTD(YEARLOOP% + 1%))) / &
					(TOTALPRCYTD(YEARLOOP% + 1%) - &
					TOTALCSTYTD(YEARLOOP% + 1%)) * 100.0
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

	NEXT YEARLOOP%

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
	!
	! Here we will check out the AR stuff
	!
	! Find AR_OPEN record
	!
17200	WHEN ERROR IN
		FIND #AR_OPEN.CH%, &
			KEY #2% EQ SA_SALESMAN::SALESMAN + LAST_CROSSREF$, &
			REGARDLESS
	USE
		CONTINUE 17300 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

 GetOpenRec:
17220	WHEN ERROR IN
		GET #AR_OPEN.CH%, REGARDLESS
	USE
		CONTINUE 17300 IF ERR = 11%
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

	GOTO 17300 &
		IF AR_OPEN::SALNUM <> SA_SALESMAN::SALESMAN
	GOTO 17300 &
		IF AR_OPEN::CUSNUM <> LAST_CROSSREF$

	GOTO GetOpenRec &
		IF AR_OPEN::TRATYP <> "01" AND &
		AR_OPEN::TRATYP <> "02" AND &
		AR_OPEN::TRATYP <> "08"

	SELECT LEFT(AR_OPEN::UPDATED, 4%)

	CASE CUR_YEAR$
		YEARLOOP% = 1%

	CASE LAST_YYYY$
		YEARLOOP% = 2%

	CASE ELSE
		GOTO GetOpenRec

	END SELECT

	I% = VAL%(MID(AR_OPEN::UPDATED, 5%, 2%))

	CUSTPRICEAMT(YEARLOOP%,I%) = CUSTPRICEAMT(YEARLOOP%, I%) + &
		AR_OPEN::SALAMT

	J% = INT((I% - 1%) / 3%) + 1%

	CUSTPRCQTR(YEARLOOP%, J%) = CUSTPRCQTR(YEARLOOP%, J%) + AR_OPEN::SALAMT

	CUSTPRCYTD(YEARLOOP%) = CUSTPRCYTD(YEARLOOP%) + AR_OPEN::SALAMT

	GOTO GetOpenRec

17300	!
	! Find AR_CLOSED record
	!
	WHEN ERROR IN
		FIND #AR_CLOSED.CH%, &
			KEY #1% EQ SA_SALESMAN::SALESMAN + LAST_CROSSREF$, &
			REGARDLESS
	USE
		CONTINUE ExitARStuff IF ERR = 155% OR ERR = 9%
		FILENAME$ = "AR_CLOSED"
		CONTINUE HelpError
	END WHEN

 GetClosedRec:
17320	WHEN ERROR IN
		GET #AR_CLOSED.CH%, REGARDLESS
	USE
		CONTINUE ExitARStuff IF ERR = 11%
		FILENAME$ = "AR_CLOSED"
		CONTINUE HelpError
	END WHEN

	GOTO ExitARStuff &
		IF AR_CLOSED::SALNUM <> SA_SALESMAN::SALESMAN
	GOTO ExitARStuff &
		IF AR_CLOSED::CUSNUM <> LAST_CROSSREF$

	GOTO GetClosedRec &
		IF AR_CLOSED::TRATYP <> "01" AND &
		AR_CLOSED::TRATYP <> "02" AND &
		AR_CLOSED::TRATYP <> "08"

	SELECT LEFT(AR_CLOSED::UPDATED, 4%)

	CASE CUR_YEAR$
		YEARLOOP% = 1%

	CASE LAST_YYYY$
		YEARLOOP% = 2%

	CASE ELSE
		GOTO GetClosedRec

	END SELECT

	I% = VAL%(MID(AR_CLOSED::UPDATED, 5%, 2%))

	CUSTPRICEAMT(YEARLOOP%, I%) = CUSTPRICEAMT(YEARLOOP%, I%) + &
		AR_CLOSED::SALAMT

	J% = INT((I% - 1%) / 3%) + 1%

	CUSTPRCQTR(YEARLOOP%, J%) = CUSTPRCQTR(YEARLOOP%, J%) + AR_CLOSED::SALAMT
	CUSTPRCYTD(YEARLOOP%) = CUSTPRCYTD(YEARLOOP%) + AR_CLOSED::SALAMT

	GOTO GetClosedRec

 ExitARStuff:
	GOTO ExitCustTotal &
		IF TOTAL.ONLY$ = "Y"

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

	FOR YEARLOOP% = 1% TO YEARS%

		GOTO CustVar &
			IF YEARLOOP% <> 1% AND &
			YEARLOOP% = YEARS% AND &
			PRINT_VARIANCE$ = "Y"

		TEXT$ = SPACE$(25%) + ":   "
		TEXT$ = "Year " + YYYY$(YEARLOOP%) + &
			TEXT$ + TEXT$ + TEXT$ + TEXT$

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3%)

		TEXT$ = "Sales" + &
			FORMAT$(CUSTPRICEAMT(YEARLOOP%, 1%), "########") + &
			FORMAT$(CUSTPRICEAMT(YEARLOOP%, 2%), "#######") + &
			FORMAT$(CUSTPRICEAMT(YEARLOOP%, 3%), "#######") + &
			FORMAT$(CUSTPRCQTR(YEARLOOP%, 1%), "########") + &
			FORMAT$(CUSTPRICEAMT(YEARLOOP%, 4%), "#######") + &
			FORMAT$(CUSTPRICEAMT(YEARLOOP%, 5%), "#######") + &
			FORMAT$(CUSTPRICEAMT(YEARLOOP%, 6%), "#######") + &
			FORMAT$(CUSTPRCQTR(YEARLOOP%, 2%), "########") + &
			FORMAT$(CUSTPRICEAMT(YEARLOOP%, 7%), "#######") + &
			FORMAT$(CUSTPRICEAMT(YEARLOOP%, 8%), "#######") + &
			FORMAT$(CUSTPRICEAMT(YEARLOOP%, 9%), "#######") + &
			FORMAT$(CUSTPRCQTR(YEARLOOP%, 3%), "########") + &
			FORMAT$(CUSTPRICEAMT(YEARLOOP%, 10%), "#######") + &
			FORMAT$(CUSTPRICEAMT(YEARLOOP%, 11%), "#######") + &
			FORMAT$(CUSTPRICEAMT(YEARLOOP%, 12%), "#######") + &
			FORMAT$(CUSTPRCQTR(YEARLOOP%, 4%), "########") + &
			FORMAT$(CUSTPRCYTD(YEARLOOP%),  "########")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		IF GROSS_PROFIT$ = "Y" AND &
			(YEARLOOP% = 1% OR YEARLOOP% > 1% AND &
			PRINT_VARIANCE$ <> "Y")
		THEN
			TEXT$ = "GProf" + &
				FORMAT$(CUSTPRICEAMT(YEARLOOP%, 1%) - &
				CUSTCOSTAMT(YEARLOOP%, 1%), "########") + &
				FORMAT$(CUSTPRICEAMT(YEARLOOP%, 2%) - &
				CUSTCOSTAMT(YEARLOOP%, 2%), "#######") + &
				FORMAT$(CUSTPRICEAMT(YEARLOOP%, 3%) - &
				CUSTCOSTAMT(YEARLOOP%, 3%), "#######") + &
				FORMAT$(CUSTPRCQTR(YEARLOOP%, 1%) - &
				CUSTCSTQTR(YEARLOOP%, 1%), "########") + &
				FORMAT$(CUSTPRICEAMT(YEARLOOP%, 4%) - &
				CUSTCOSTAMT(YEARLOOP%, 4%), "#######") + &
				FORMAT$(CUSTPRICEAMT(YEARLOOP%, 5%) - &
				CUSTCOSTAMT(YEARLOOP%, 5%), "#######") + &
				FORMAT$(CUSTPRICEAMT(YEARLOOP%, 6%) - &
				CUSTCOSTAMT(YEARLOOP%, 6%), "#######") + &
				FORMAT$(CUSTPRCQTR(YEARLOOP%, 2%) - &
				CUSTCSTQTR(YEARLOOP%, 2%), "########") + &
				FORMAT$(CUSTPRICEAMT(YEARLOOP%, 7%) - &
				CUSTCOSTAMT(YEARLOOP%, 7%), "#######") + &
				FORMAT$(CUSTPRICEAMT(YEARLOOP%, 8%) - &
				CUSTCOSTAMT(YEARLOOP%, 8%), "#######") + &
				FORMAT$(CUSTPRICEAMT(YEARLOOP%, 9%) - &
				CUSTCOSTAMT(YEARLOOP%, 9%), "#######") + &
				FORMAT$(CUSTPRCQTR(YEARLOOP%, 3%) - &
				CUSTCSTQTR(YEARLOOP%, 3%), "########") + &
				FORMAT$(CUSTPRICEAMT(YEARLOOP%, 10%) - &
				CUSTCOSTAMT(YEARLOOP%, 10%), "#######") + &
				FORMAT$(CUSTPRICEAMT(YEARLOOP%, 11%) - &
				CUSTCOSTAMT(YEARLOOP%, 11%), "#######") + &
				FORMAT$(CUSTPRICEAMT(YEARLOOP%, 12%) - &
				CUSTCOSTAMT(YEARLOOP%, 12%), "#######") + &
				FORMAT$(CUSTPRCQTR(YEARLOOP%, 4%) - &
				CUSTCSTQTR(YEARLOOP%, 4%), "########") + &
				FORMAT$(CUSTPRCYTD(YEARLOOP%) - &
				CUSTCSTYTD(YEARLOOP%),  "########")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 1%)
			GOTO ExitProgram IF UTL_REPORTX::STAT

		END IF

 CustVar:
		IF PRINT_VARIANCE$ = "Y" AND YEARLOOP% <> YEARS%
		THEN
			FOR I% = 1% TO 12%

				IF CUSTPRICEAMT(YEARLOOP% + 1%, I%) = 0.0
				THEN
					VARIANCE(I%) = 100.00
					VARIANCE(I%) = 0.0 &
						IF CUSTPRICEAMT(YEARLOOP%,I%) = 0.0
				ELSE
					VARIANCE(I%) = (CUSTPRICEAMT(YEARLOOP%, I%) - &
						CUSTPRICEAMT(YEARLOOP% + 1%, I%)) / &
						CUSTPRICEAMT(YEARLOOP% + 1%, I%) * 100.0
				END IF

			NEXT I%

			FOR I% = 13% TO 16%

				IF CUSTPRCQTR(YEARLOOP% + 1%, I% - 12%) = 0.0
				THEN
					VARIANCE(I%) = 100.0
					VARIANCE(I%) = 0.0 &
						IF CUSTPRCQTR(YEARLOOP%, I% - 12%) = 0.0
				ELSE
					VARIANCE(I%) = (CUSTPRCQTR(YEARLOOP%, I% - 12%) - &
						CUSTPRCQTR(YEARLOOP% + 1%, I% - 12%)) / &
						CUSTPRCQTR(YEARLOOP% + 1%, I% - 12%) * 100.0
				END IF

			NEXT I%

			IF CUSTPRCYTD(YEARLOOP% + 1%) = 0.0
			THEN
				VARIANCE(17%) = 100.00
				VARIANCE(17%) = 0.0 &
					IF CUSTPRCYTD(YEARLOOP%) = 0.0
			ELSE
				VARIANCE(17%) = (CUSTPRCYTD(YEARLOOP%) - &
					CUSTPRCYTD(YEARLOOP% + 1%)) / &
					CUSTPRCYTD(YEARLOOP% + 1%) * 100.0
			END IF

			TEXT$ = "SVar " + &
				FORMAT$(VARIANCE(1%), "#######%") + &
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

				IF CUSTPRICEAMT(YEARLOOP% + 1%, I%) - &
					CUSTCOSTAMT(YEARLOOP% + 1%, I%) = 0.0
				THEN
					VARIANCE(I%) = 100.00
					VARIANCE(I%) = 0.0 &
						IF CUSTPRICEAMT(YEARLOOP%, I%) - &
						CUSTCOSTAMT(YEARLOOP%, I%) = 0.0
				ELSE
					VARIANCE(I%) = (CUSTPRICEAMT(YEARLOOP%, I%) - &
						CUSTCOSTAMT(YEARLOOP%, I%) - &
						(CUSTPRICEAMT(YEARLOOP% + 1%, I%) - &
						CUSTCOSTAMT(YEARLOOP% + 1%, I%))) / &
						(CUSTPRICEAMT(YEARLOOP% + 1%, I%) - &
						CUSTCOSTAMT(YEARLOOP% + 1%, I%)) * 100.0
				END IF

			NEXT I%

			FOR I% = 13% TO 16%

				IF CUSTPRCQTR(YEARLOOP% + 1%, I% - 12%) - &
					CUSTCSTQTR(YEARLOOP% + 1%, I% - 12%) = 0.0
				THEN
					VARIANCE(I%) = 100.0
					VARIANCE(I%) = 0.0 &
						IF CUSTPRCQTR(YEARLOOP%, I% - 12%) - &
						CUSTCSTQTR(YEARLOOP%, I% - 12%) = 0.0
				ELSE
					VARIANCE(I%) = (CUSTPRCQTR(YEARLOOP%, I% - 12%) - &
						CUSTCSTQTR(YEARLOOP%, I% - 12%) - &
						(CUSTPRCQTR(YEARLOOP% + 1%, I% - 12%) - &
						CUSTCSTQTR(YEARLOOP% + 1%, I% - 12%))) / &
						(CUSTPRCQTR(YEARLOOP% + 1%, I% - 12%) - &
						CUSTCSTQTR(YEARLOOP% + 1%, I% - 12%)) * 100.0
				END IF

			NEXT I%

			IF CUSTPRCYTD(YEARLOOP% + 1%) - &
				CUSTCSTYTD(YEARLOOP% + 1%) = 0.0
			THEN
				VARIANCE(17%) = 100.00
				VARIANCE(17%) = 0.0 &
					IF CUSTPRCYTD(YEARLOOP%) - &
					CUSTCSTYTD(YEARLOOP%) = 0.0
			ELSE
				VARIANCE(17%) = (CUSTPRCYTD(YEARLOOP%) - &
					CUSTCSTYTD(YEARLOOP%) - &
					(CUSTPRCYTD(YEARLOOP% + 1%) - &
					CUSTCSTYTD(YEARLOOP% + 1%))) / &
					(CUSTPRCYTD(YEARLOOP%+ 1%) - &
					CUSTCSTYTD(YEARLOOP% + 1%)) * 100.0
			END IF

			TEXT$ = "GPVar" + &
				FORMAT$(VARIANCE(1%), "#######%") + &
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

	NEXT YEARLOOP%

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

 ExitCustTotal:
	FOR YEARLOOP% = 1% TO YEARS%

		FOR I% = 1% TO 12%

			BROKERPRICEAMT(YEARLOOP%, I%) = &
				BROKERPRICEAMT(YEARLOOP%, I%) + &
				CUSTPRICEAMT(YEARLOOP%, I%)
			BROKERCOSTAMT(YEARLOOP%, I%) = &
				BROKERCOSTAMT(YEARLOOP%, I%) + &
				CUSTCOSTAMT(YEARLOOP%, I%)

		NEXT I%

	NEXT YEARLOOP%

	FOR YEARLOOP% = 1% TO YEARS%

		BROKERPRCYTD(YEARLOOP%) = BROKERPRCYTD(YEARLOOP%) + &
			CUSTPRCYTD(YEARLOOP%)
		BROKERCSTYTD(YEARLOOP%) = BROKERCSTYTD(YEARLOOP%) + &
			CUSTCSTYTD(YEARLOOP%)

		CUSTPRCYTD(YEARLOOP%) = 0.0
		CUSTCSTYTD(YEARLOOP%) = 0.0

		FOR I% = 1% TO 4%

			BROKERPRCQTR(YEARLOOP%, I%) = &
				BROKERPRCQTR(YEARLOOP%, I%) + &
				CUSTPRCQTR(YEARLOOP%, I%)
			BROKERCSTQTR(YEARLOOP%, I%) = &
				BROKERCSTQTR(YEARLOOP%, I%) + &
				CUSTCSTQTR(YEARLOOP%, I%)

			CUSTPRICEAMT(YEARLOOP%, I%)       = 0.0
			CUSTCOSTAMT(YEARLOOP%, I%)        = 0.0

			CUSTPRICEAMT(YEARLOOP%, I% + 4%)  = 0.0
			CUSTCOSTAMT(YEARLOOP%, I% + 4%)   = 0.0

			CUSTPRICEAMT(YEARLOOP%, I% + 8%)  = 0.0
			CUSTCOSTAMT(YEARLOOP%, I% + 8%)   = 0.0

			CUSTPRCQTR(YEARLOOP%, I%)         = 0.0
			CUSTCSTQTR(YEARLOOP%, I%)         = 0.0

		NEXT I%

	NEXT YEARLOOP%

	RETURN

 BrokerTotal:
	TEXT$ = "::::::::::" + "   "    + &
		SA_SALESMAN::SALESMAN + "   " + &
		SA_SALESMAN::DESCR + "   " + &
		SA_SALESMAN::TTYPE + "      " + &
		SA_SALESMAN::CLASS

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = SPACE$(28%) + &
		SA_SALESMAN::ADD1 + "   " + &
		SA_SALESMAN::ADD2 + "   " + &
		SA_SALESMAN::CITY + "   " + &
		SA_SALESMAN::STATE+ "      " + &
		PRNT_PHONE(SA_SALESMAN::PHONE, 0%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	FOR YEARLOOP% = 1% TO YEARS%

		GOTO BrokerVar &
			IF YEARLOOP% <> 1% AND &
			YEARLOOP% = YEARS% AND PRINT_VARIANCE$ = "Y"

		TEXT$ = SPACE$(25%) + ":   "
		TEXT$ = "Year " + YYYY$(YEARLOOP%) + &
			TEXT$ + TEXT$ + TEXT$ + TEXT$

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		TEXT$ = "Sales" + &
			FORMAT$(BROKERPRICEAMT(YEARLOOP%, 1%), "########") + &
			FORMAT$(BROKERPRICEAMT(YEARLOOP%, 2%), "#######") + &
			FORMAT$(BROKERPRICEAMT(YEARLOOP%, 3%), "#######") + &
			FORMAT$(BROKERPRCQTR(YEARLOOP%, 1%), "########") + &
			FORMAT$(BROKERPRICEAMT(YEARLOOP%, 4%), "#######") + &
			FORMAT$(BROKERPRICEAMT(YEARLOOP%, 5%), "#######") + &
			FORMAT$(BROKERPRICEAMT(YEARLOOP%, 6%), "#######") + &
			FORMAT$(BROKERPRCQTR(YEARLOOP%, 2%), "########") + &
			FORMAT$(BROKERPRICEAMT(YEARLOOP%, 7%), "#######") + &
			FORMAT$(BROKERPRICEAMT(YEARLOOP%, 8%), "#######") + &
			FORMAT$(BROKERPRICEAMT(YEARLOOP%, 9%), "#######") + &
			FORMAT$(BROKERPRCQTR(YEARLOOP%, 3%), "########") + &
			FORMAT$(BROKERPRICEAMT(YEARLOOP%, 10%), "#######") + &
			FORMAT$(BROKERPRICEAMT(YEARLOOP%, 11%), "#######") + &
			FORMAT$(BROKERPRICEAMT(YEARLOOP%, 12%), "#######") + &
			FORMAT$(BROKERPRCQTR(YEARLOOP%, 4%), "########") + &
			FORMAT$(BROKERPRCYTD(YEARLOOP%), "########")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		IF GROSS_PROFIT$ = "Y"
		THEN
			TEXT$ = "GProf" + &
				FORMAT$(BROKERPRICEAMT(YEARLOOP%, 1%) - &
				BROKERCOSTAMT(YEARLOOP%, 1%), "########") + &
				FORMAT$(BROKERPRICEAMT(YEARLOOP%, 2%) - &
				BROKERCOSTAMT(YEARLOOP%, 2%), "#######") + &
				FORMAT$(BROKERPRICEAMT(YEARLOOP%, 3%) - &
				BROKERCOSTAMT(YEARLOOP%, 3%), "#######") + &
				FORMAT$(BROKERPRCQTR(YEARLOOP%, 1%) - &
				BROKERCSTQTR(YEARLOOP%, 1%), "########") + &
				FORMAT$(BROKERPRICEAMT(YEARLOOP%, 4%) - &
				BROKERCOSTAMT(YEARLOOP%, 4%), "#######") + &
				FORMAT$(BROKERPRICEAMT(YEARLOOP%, 5%) - &
				BROKERCOSTAMT(YEARLOOP%, 5%), "#######") + &
				FORMAT$(BROKERPRICEAMT(YEARLOOP%, 6%) - &
				BROKERCOSTAMT(YEARLOOP%, 6%), "#######") + &
				FORMAT$(BROKERPRCQTR(YEARLOOP%, 2%) - &
				BROKERCSTQTR(YEARLOOP%, 2%), "########") + &
				FORMAT$(BROKERPRICEAMT(YEARLOOP%, 7%) - &
				BROKERCOSTAMT(YEARLOOP%, 7%), "#######") + &
				FORMAT$(BROKERPRICEAMT(YEARLOOP%, 8%) - &
				BROKERCOSTAMT(YEARLOOP%, 8%), "#######") + &
				FORMAT$(BROKERPRICEAMT(YEARLOOP%, 9%) - &
				BROKERCOSTAMT(YEARLOOP%, 9%), "#######") + &
				FORMAT$(BROKERPRCQTR(YEARLOOP%, 3%) - &
				BROKERCSTQTR(YEARLOOP%, 3%), "########") + &
				FORMAT$(BROKERPRICEAMT(YEARLOOP%, 10%) - &
				BROKERCOSTAMT(YEARLOOP%, 10%), "#######") + &
				FORMAT$(BROKERPRICEAMT(YEARLOOP%, 11%) - &
				BROKERCOSTAMT(YEARLOOP%, 11%), "#######") + &
				FORMAT$(BROKERPRICEAMT(YEARLOOP%, 12%) - &
				BROKERCOSTAMT(YEARLOOP%, 12%), "#######") + &
				FORMAT$(BROKERPRCQTR(YEARLOOP%, 4%) - &
				BROKERCSTQTR(YEARLOOP%, 4%), "########") + &
				FORMAT$(BROKERPRCYTD(YEARLOOP%) - &
				BROKERCSTYTD(YEARLOOP%), "########")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 1%)
			GOTO ExitProgram IF UTL_REPORTX::STAT

		END IF

 BrokerVar:
		IF PRINT_VARIANCE$ = "Y" AND YEARLOOP% <> YEARS%
		THEN
			FOR I% = 1% TO 12%

				IF BROKERPRICEAMT(YEARLOOP% + 1%, I%) = 0.0
				THEN
					VARIANCE(I%) = 100.00
					VARIANCE(I%) = 0.0 &
						IF BROKERPRICEAMT(YEARLOOP%, I%) = 0.0
				ELSE
					VARIANCE(I%) = (BROKERPRICEAMT(YEARLOOP%, I%) - &
						BROKERPRICEAMT(YEARLOOP% + 1%, I%)) / &
						BROKERPRICEAMT(YEARLOOP% + 1%, I%) * 100.0
				END IF

			NEXT I%

			FOR I% = 13% TO 16%

				IF BROKERPRCQTR(YEARLOOP% + 1%, I% - 12%) = 0.0
				THEN
					VARIANCE(I%) = 100.0
					VARIANCE(I%) = 0.0 &
						IF BROKERPRCQTR(YEARLOOP%, I% - 12%) = 0.0
				ELSE
					VARIANCE(I%) = (BROKERPRCQTR(YEARLOOP%, I% - 12%) - &
						BROKERPRCQTR(YEARLOOP% + 1%, I% - 12%)) / &
						BROKERPRCQTR(YEARLOOP% + 1%, I% - 12%) * 100.0
				END IF

			NEXT I%

			IF BROKERPRCYTD(YEARLOOP% + 1%) = 0.0
			THEN
				VARIANCE(17%) = 100.00
				VARIANCE(17%) = 0.0 &
					IF BROKERPRCYTD(YEARLOOP%) = 0.0
			ELSE
				VARIANCE(17%) = (BROKERPRCYTD(YEARLOOP%) - &
					BROKERPRCYTD(YEARLOOP% + 1%)) / &
					BROKERPRCYTD(YEARLOOP% + 1%) * 100.0
			END IF

			TEXT$ = "SVar " + &
				FORMAT$(VARIANCE(1%), "#######%") + &
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

				IF BROKERPRICEAMT(YEARLOOP% + 1%, I%) - &
					BROKERCOSTAMT(YEARLOOP% + 1%, I%) = 0.0
				THEN
					VARIANCE(I%) = 100.00
					VARIANCE(I%) = 0.0 &
						IF BROKERPRICEAMT(YEARLOOP%, I%) - &
						BROKERCOSTAMT(YEARLOOP%, I%) = 0.0
				ELSE
					VARIANCE(I%) = (BROKERPRICEAMT(YEARLOOP%, I%) - &
						BROKERCOSTAMT(YEARLOOP%, I%) - &
						(BROKERPRICEAMT(YEARLOOP% + 1%, I%) - &
						BROKERCOSTAMT(YEARLOOP% + 1%, I%))) / &
						(BROKERPRICEAMT(YEARLOOP% + 1%, I%) - &
						BROKERCOSTAMT(YEARLOOP% + 1%, I%)) * 100.0
				END IF

			NEXT I%

			FOR I% = 13% TO 16%

				IF BROKERPRCQTR(YEARLOOP% + 1%, I% - 12%) - &
					BROKERCSTQTR(YEARLOOP% + 1%, I% - 12%) = 0.0
				THEN
					VARIANCE(I%) = 100.0
					VARIANCE(I%) = 0.0 &
						IF BROKERPRCQTR(YEARLOOP%, I% - 12%) - &
						BROKERCSTQTR(YEARLOOP%, I% - 12%) = 0.0
				ELSE
					VARIANCE(I%) = (BROKERPRCQTR(YEARLOOP%, I% - 12%) - &
						BROKERCSTQTR(YEARLOOP%, I% - 12%) - &
						(BROKERPRCQTR(YEARLOOP% + 1%, I% - 12%) - &
						BROKERCSTQTR(YEARLOOP% + 1%, I% - 12%))) / &
						(BROKERPRCQTR(YEARLOOP% + 1%, I% - 12%) - &
						BROKERCSTQTR(YEARLOOP% + 1%, I% - 12%)) * 100.0
				END IF

			NEXT I%

			IF BROKERPRCYTD(YEARLOOP% + 1%) - &
				BROKERCSTYTD(YEARLOOP% + 1%) = 0.0
			THEN
				VARIANCE(17%) = 100.00
				VARIANCE(17%) = 0.0 &
					IF BROKERPRCYTD(YEARLOOP%) - &
					BROKERCSTYTD(YEARLOOP%) = 0.0
			ELSE
				VARIANCE(17%) = (BROKERPRCYTD(YEARLOOP%) - &
					BROKERCSTYTD(YEARLOOP%) - &
					(BROKERPRCYTD(YEARLOOP% + 1%) - &
					BROKERCSTYTD(YEARLOOP% + 1%))) / &
					(BROKERPRCYTD(YEARLOOP% + 1%) - &
					BROKERCSTYTD(YEARLOOP% + 1%)) * 100.0
			END IF

			TEXT$ = "GPVar" + &
				FORMAT$(VARIANCE(1%), "#######%") + &
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

	NEXT YEARLOOP%

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

 SkipBroker:
	FOR YEARLOOP% = 1% TO YEARS%

		FOR I% = 1% TO 12%

			TOTALPRICEAMT(YEARLOOP%, I%) = &
				TOTALPRICEAMT(YEARLOOP%, I%) + &
				BROKERPRICEAMT(YEARLOOP%, I%)
			TOTALCOSTAMT(YEARLOOP%, I%) = &
				TOTALCOSTAMT(YEARLOOP%, I%) + &
				BROKERCOSTAMT(YEARLOOP%, I%)

		NEXT I%

	NEXT YEARLOOP%

	FOR YEARLOOP% = 1% TO YEARS%

		TOTALPRCYTD(YEARLOOP%)  = TOTALPRCYTD(YEARLOOP%) + &
			BROKERPRCYTD(YEARLOOP%)
		TOTALCSTYTD(YEARLOOP%)  = TOTALCSTYTD(YEARLOOP%) + &
			BROKERCSTYTD(YEARLOOP%)

		BROKERPRCYTD(YEARLOOP%)  = 0.0
		BROKERCSTYTD(YEARLOOP%)  = 0.0

		FOR I% = 1% TO 4%
			TOTALPRCQTR(YEARLOOP%, I%) = &
				TOTALPRCQTR(YEARLOOP%, I%) + &
				BROKERPRCQTR(YEARLOOP%, I%)
			TOTALCSTQTR(YEARLOOP%, I%) = &
				TOTALCSTQTR(YEARLOOP%, I%) + &
				BROKERCSTQTR(YEARLOOP%, I%)

			BROKERPRICEAMT(YEARLOOP%, I%)       = 0.0
			BROKERCOSTAMT(YEARLOOP%, I%)        = 0.0

			BROKERPRICEAMT(YEARLOOP%, I% + 4%)  = 0.0
			BROKERCOSTAMT(YEARLOOP%, I% + 4%)   = 0.0

			BROKERPRICEAMT(YEARLOOP%, I% + 8%)  = 0.0
			BROKERCOSTAMT(YEARLOOP%, I% + 8%)   = 0.0

			BROKERPRCQTR(YEARLOOP%, I%)         = 0.0
			BROKERCSTQTR(YEARLOOP%, I%)         = 0.0

		NEXT I%

	NEXT YEARLOOP%

	PRINT_FLAG% = 0%
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

32767	!******************************************************************
	! End of report SA_RPRT_SALSUM
	!******************************************************************
	END
