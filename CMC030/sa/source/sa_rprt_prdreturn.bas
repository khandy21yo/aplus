1	%TITLE "Product Retruned Report"
	%SBTTL "SA_RPRT_PRDRETURN"
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
	! ID:SA0022
	!
	! Abstract:HELP
	!	.p
	!	The ^*Returned Product Report\* produces a report indicating
	!	why a particular product was returned.  The report contains
	!	the following fields:
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
	!	Units MTD
	!	.le
	!	Cost MTD
	!	.le
	!	Returned Dollars MTD
	!	.le
	!	% of Sales MTD
	!	.le
	!	Units YTD
	!	.le
	!	Cost YTD
	!	.le
	!	Returned Dollars YTD
	!	.le
	!	% of Sales YTD
	!	.els
	!
	!
	! Bugs:
	!
	!	Has PD_PRODUCT::LABEL = 'H' (constant)
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS SA_SOURCE:SA_RPRT_PRDRETURN/LINE
	!	$ LINK/EXECUTABLE=SA_EXE: SA_RPRT_PRDRETURN, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE SA_RPRT_PRDRETURN.OBJ;*
	!
	! Author:
	!
	!	04/23/93 - Dan Perkins
	!
	! Modification History:
	!
	!	06/11/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/05/93 - Frank F. Starman
	!		Print summary page for returns.
	!		Print product even if there is no returns.
	!
	!	03/22/94 - Kevin Handy
	!		Formatted to 80 columns.
	!
	!	03/22/94 - Kevin Handy
	!		Added comments.
	!
	!	06/02/94 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	06/21/95 - Kevin Handy
	!		Modified "FORMAT$" statements to make more available
	!		digits to reduce chances of scientific notation
	!		by pulling leading spaces into the format.
	!
	!	06/21/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!
	!	07/19/96 - Kevin Handy
	!		Reformat source code.
	!		Chech for undefined product after call to
	!		PD_EXAM_PRODUCT.
	!		Add more digits to final totals.
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
	EXTERNAL LONG    FUNCTION PD_EXAM_PRODUCT
	EXTERNAL REAL    FUNCTION PC_READ_COST
	EXTERNAL REAL    FUNCTION PC_READ_PRICE

	!
	! CDD inclusions
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT
	DECLARE	PD_PRODUCT_CDD	PD_PRODUCT_EXAM

	%INCLUDE "SOURCE:[BM.OPEN]BM_RELATION.HB"
	MAP (BM_RELATION)	BM_RELATION_CDD		BM_RELATION

	%INCLUDE "SOURCE:[IC.OPEN]IC_35HISTORY.HB"
	MAP (IC_35HISTORY)	IC_35HISTORY_CDD	IC_35HISTORY

	%INCLUDE "SOURCE:[OE.OPEN]OE_CREASON.HB"
	MAP (OE_CREASON)	OE_CREASON_CDD		OE_CREASON

	RECORD REASON
		STRING DESCR=40%, &
		REAL QTY1, &
		REAL PRDCOSTCP, &
		REAL PRDPRICECP, &
		REAL QTY2, &
		REAL PRDCOSTYTD, &
		REAL PRDPRICEYTD, &
		REAL SAPRDPRICECP, &
		REAL SAPRDPRICEYTD, &
		REAL SAPRDCOSTCP, &
		REAL SAPRDCOSTYTD
	END RECORD

	DECLARE REASON REASON(50%)

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
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field determines the order
	!	to print in.
	!	.b
	!	Valid settings are:
	!	.table 3,25
	!	.te
	!	^*P\* - Product Number
	!	.te
	!	^*T\* - Product Type
	!	.te
	!	^*C\* - Product Category
	!	.te
	!	^*D\* - Product Description
	!	.te
	!	^*S\* - Product Secondary Code
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
	!	.p
	!	The ^*From Item\* field enters the
	!	item with which to begin.
	!	.p
	!	A blank field will begin with the first
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
	!	with which to end.
	!	.p
	!	A blank field will end with the last
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

	YYYYPP$ = EDIT$(UTL_REPORTX::OPTDEF(5%), 132%)
	YYYY$ = LEFT(YYYYPP$, 4%)
	PP% = VAL%(RIGHT(YYYYPP$, 5%))

	!++
	! Abstract:FLD06
	!	.ts 55
	!	^*(06) Period	YYYYPP\*
	!	.b
	!	.lm +5
	!	The ^*Period\* field selects a specific
	!	monthly period which is to be printed.
	!	.lm -5
	!
	! Index:
	!
	!--

	PRICE_TYPE$ = EDIT$(UTL_REPORTX::OPTDEF(6%), 132%)

	!++
	! Abstract:FLD07
	!	.ts 55
	!	^*(07) Price Type\*
	!	.b
	!	.lm +5
	!	The ^*Price Type\* field selects a specific
	!	selling price which is to be printed.
	!	.lm -5
	!
	! Index:
	!	Price Type
	!--

	MAX_LEVEL% = VAL%(EDIT$(UTL_REPORTX::OPTDEF(5%), 132%))

	!++
	! Abstract:FLD08
	!	.ts 55
	!	^*(08) Maximum Level\*
	!	.b
	!	.lm +5
	!	The ^*Maximum Level\* field assigns the
	!	lowest BOM level to check for finish goods components.
	!	.lm -5
	!
	! Index:
	!
	!--

	%PAGE

	!
	! Open Product file
	!
300	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

	!
	! Open History file
	!
310	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_35HISTORY.OPN"
	USE
		FILENAME$ = "IC_35HISTORY"
		CONTINUE HelpError
	END WHEN

	!
	! Open Reason Code file
	!
320	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_CREASON.OPN"
	USE
		FILENAME$ = "OE_CREASON"
		CONTINUE HelpError
	END WHEN

	!
	! Open Relation file
	!
330	WHEN ERROR IN
		%INCLUDE "SOURCE:[BM.OPEN]BM_RELATION.OPN"
	USE
		CONTINUE 400 IF ERR = 5%
		FILENAME$ = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

	!*******************************************************************
	! Set up a big string with the reason codes
	!*******************************************************************

400	REASON$, COMMA$ = ""
	REASONS% = 0%

	WHEN ERROR IN
		RESET #OE_CREASON.CH%
	USE
		FILENAME$ = "OE_CREASON"
		CONTINUE HelpError
	END WHEN

 GetReason:
410	WHEN ERROR IN
		GET #OE_CREASON.CH%, REGARDLESS
	USE
		CONTINUE ExitReason IF ERR = 11%
		FILENAME$ = "OE_CREASON"
		CONTINUE HelpError
	END WHEN

	REASON$ = REASON$ + COMMA$ + TRM$(OE_CREASON::CREASON)

	COMMA$ = ","

	REASONS% = REASONS% + 1%

	GOTO GetReason

 ExitReason:
	GOTO ExitProgram &
		IF REASON$ = ""

	!
	! Add sales "SA" to the front of our string
	!
	REASON$ = "SA," + REASON$

	REASONS% = REASONS% + 1%

	!
	! Dimension arrays
	!
	! Second element 1% is PERIOD amounts
	! Second element 2% is YEARLY amounts
	!
	DIM REAL QTY(REASONS%, 2%)
	DIM REAL TOTQTY(REASONS%, 2%)
	DIM REAL GRTOTQTY(2%, 2%)

	DIM RFA RFA_LEVEL(500%)
	DIM REAL QTY_LEVEL(500%)
	DIM STRING TEST_PRODUCT(500%)

	!
	! Initialize variables
	!
	FOR I% = 1% TO REASONS%

		FOR J% = 1% TO 2%
			QTY(I%, J%) = 0.0
			TOTQTY(I%, J%) = 0.0
		NEXT J%

	NEXT I%

	%PAGE

 ReportTitle:
	!*******************************************************************
	! Title
	!*******************************************************************

	SELECT SORTBY$

	CASE "P"
		K_NUM% = 0%
		TITLE$(1%) = "RETURNED PRODUCT BY PRODUCT NUMBER"

	CASE "T"
		K_NUM% = 1%
		TITLE$(1%) = "RETURNED PRODUCT BY PRODUCT TYPE"

	CASE "C"
		K_NUM% = 2%
		TITLE$(1%) = "RETURNED PRODUCT BY PRODUCT CATEGORY"

	CASE "D"
		K_NUM% = 3%
		TITLE$(1%) = "RETURNED PRODUCT BY PRODUCT DESCRIPTION"

	CASE "S"
		K_NUM% = 4%
		TITLE$(1%) = "RETURNED PRODUCT BY PRODUCT SECONDARY CODE"

	END SELECT

	TITLE$(2%) = "FOR PERIOD " + YYYYPP$
	TITLE$(3%) = "Sales Analysis System"
	TITLE$(4%) = ""

	!
	! Heading
	!
	TITLE$(5%) = ":::::::::: Product#       Description               " + &
		"               PT PCat SecCode    UOM"

	TITLE$(6%) = "   Description                           " + &
		"    UnitsMTD   CostMTD   SalesMTD  %MoSales " + &
		" UnitsYTD    CostYTD    SalesYTD  %YrSales"

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
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

	PAGE% = 0%

 GetNextRec:
	!
	! Get next record
	!

17020	WHEN ERROR IN
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
		GOTO ExitTotal &
			IF (PD_PRODUCT::PRODUCT_NUM > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_ARRAY(EDIT$( &
			PD_PRODUCT::PRODUCT_NUM, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "T"
		GOTO ExitTotal &
			IF (PD_PRODUCT::PROD_TYPE > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_ARRAY(EDIT$( &
			PD_PRODUCT::PROD_TYPE, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "C"
		GOTO ExitTotal &
			IF (PD_PRODUCT::CATEGORY > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_ARRAY(EDIT$( &
			PD_PRODUCT::CATEGORY, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "D"
		GOTO ExitTotal &
			IF (PD_PRODUCT::DESCRIPTION > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_ARRAY(EDIT$( &
			PD_PRODUCT::DESCRIPTION, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "S"
		GOTO ExitTotal &
			IF (PD_PRODUCT::SECONDARY_CODE > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_ARRAY(EDIT$( &
			PD_PRODUCT::SECONDARY_CODE, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	END SELECT

	GOTO GetNextRec &
		IF TRM$(PD_PRODUCT::LABEL) = ""

17200	PRODUCT$ = PD_PRODUCT::PRODUCT_NUM
	PROD_TYPE$ = PD_PRODUCT::PROD_TYPE
	CONV_FACTOR = 1.0

	GOSUB 17500

	GOTO ProdTotal &
		IF PD_PRODUCT::LABEL = "H" OR MAX_LEVEL% <= 0%

	WHEN ERROR IN
		GET #BM_RELATION.CH%, &
			KEY #0% EQ PD_PRODUCT::PRODUCT_NUM, &
			REGARDLESS
	USE
		CONTINUE ProdTotal IF ERR = 155% OR ERR = 9%
		FILENAME$ = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

	QTY_LEVEL(0%), LEVEL% = 1%

 GoDownTree:
	GOTO GoUpTree &
		IF LEVEL% > MAX_LEVEL%

	TEST_PRODUCT(LEVEL%) = BM_RELATION::PRODUCT
	QTY_LEVEL(LEVEL%) = QTY_LEVEL(LEVEL% - 1%) * BM_RELATION::QUANTITY
	RFA_LEVEL(LEVEL%) = GETRFA(BM_RELATION.CH%)

	V% = PD_EXAM_PRODUCT(BM_RELATION::COMPONENT, PD_PRODUCT_EXAM)
	IF V% <> CMC$_NORMAL
	THEN
		PD_PRODUCT_EXAM::PRODUCT_NUM = BM_RELATION::COMPONENT
		PD_PRODUCT_EXAM::PROD_TYPE = ""
		PD_PRODUCT_EXAM::DESCRIPTION = ""
	END IF

	IF PD_PRODUCT_EXAM::PROD_TYPE = PROD_TYPE$
	THEN
		PRODUCT$ = BM_RELATION::COMPONENT
		CONV_FACTOR = QTY_LEVEL(LEVEL%)
		GOSUB 17500
	END IF

17320	WHEN ERROR IN
		GET #BM_RELATION.CH%, &
			KEY #0% EQ BM_RELATION::COMPONENT, &
			REGARDLESS
	USE
		CONTINUE 17330 IF ERR = 155%
		FILENAME$ = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

	LEVEL% = LEVEL% + 1%
	GOTO GoDownTree

 GoUpTree:
	LEVEL% = LEVEL% - 1%
	GOTO ProdTotal &
		IF LEVEL% = 0%

17330	WHEN ERROR IN
		GET #BM_RELATION.CH%, RFA RFA_LEVEL(LEVEL%), REGARDLESS
		GET #BM_RELATION.CH%, REGARDLESS
	USE
		CONTINUE GoUpTree IF ERR = 155% OR ERR = 11%
		FILENAME$ = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

	IF BM_RELATION::PRODUCT <> TEST_PRODUCT(LEVEL%)
	THEN
		GOTO GoUpTree
	ELSE
		GOTO GoDownTree
	END IF

 ProdTotal:
	PROD_CP = 0.0
	PROD_YTD = 0.0

	PRODCOST_CP = 0.0
	PRODPRICE_CP = 0.0

	PRODCOST_YTD = 0.0
	PRODPRICE_YTD = 0.0

	TEXT$ = ":::::::::: " + PD_PRODUCT::PRODUCT_NUM + " " + &
		PD_PRODUCT::DESCRIPTION + " " + &
		PD_PRODUCT::PROD_TYPE + " " + &
		PD_PRODUCT::CATEGORY + " " + &
		PD_PRODUCT::SECONDARY_CODE + " " + &
		PD_PRODUCT::UOM

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	!
	! Print out the Sales Amounts
	!
	UC = PC_READ_COST(PD_PRODUCT::PRODUCT_NUM, &
		IC_35HISTORY::LOCATION, "", "")
	UP = PC_READ_PRICE(PD_PRODUCT::PRODUCT_NUM, &
		IC_35HISTORY::LOCATION, PRICE_TYPE$, "", "", "", "")

	TEXT$ = "PRODUCT COST IN DOLLARS " + &
		FORMAT$(UC, "########.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "SELLING PRICE IN DOLLARS " + &
		FORMAT$(UP, "#######.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "TOTAL UNITS SOLD MTD " + &
		FORMAT$(QTY(1%, 1%), "###########.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "TOTAL UNITS SOLD YTD " + &
		FORMAT$(QTY(1%, 2%), "###########.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	SAPRDCOST_CP = FUNC_ROUND(QTY(1%, 1%) * UC, 2%)
	SAPRDPRICE_CP = FUNC_ROUND(QTY(1%, 1%) * UP, 2%)

	SAPRDCOST_YTD = FUNC_ROUND(QTY(1%, 2%) * UC, 2%)
	SAPRDPRICE_YTD = FUNC_ROUND(QTY(1%, 2%) * UP, 2%)

	TOTSAPRDPRICECP = TOTSAPRDPRICECP + SAPRDPRICE_CP
	TOTSAPRDPRICEYTD = TOTSAPRDPRICEYTD + SAPRDPRICE_YTD
	TOTSAPRDCOSTCP = TOTSAPRDCOSTCP + SAPRDCOST_CP
	TOTSAPRDCOSTYTD = TOTSAPRDCOSTYTD + SAPRDCOST_YTD

	!
	! Print the Returns
	!
	FOR I% = 2% TO REASONS%

		!
		! Make sure we have something to print, based on quantity
		!
		GOTO Init &
			IF QTY(I%, 2%) = 0.0

		PRDCOST_CP = FUNC_ROUND(QTY(I%, 1%) * UC, 2%)
		PRDPRICE_CP = FUNC_ROUND(QTY(I%, 1%) * UP, 2%)

		PRDCOST_YTD = FUNC_ROUND(QTY(I%, 2%) * UC, 2%)
		PRDPRICE_YTD = FUNC_ROUND(QTY(I%, 2%) * UP, 2%)

		PRODCOST_CP = PRODCOST_CP + FUNC_ROUND(QTY(I%, 1%) * UC, 2%)
		PRODPRICE_CP = PRODPRICE_CP + FUNC_ROUND(QTY(I%, 1%) * UP, 2%)

		PRODCOST_YTD = PRODCOST_YTD + FUNC_ROUND(QTY(I%, 2%) * UC, 2%)
		PRODPRICE_YTD = PRODPRICE_YTD + FUNC_ROUND(QTY(I%, 2%) * UP, 2%)

		IF SAPRDPRICE_CP = 0.0
		THEN
			PS = 0.0
		ELSE
			PS = FUNC_ROUND(PRDPRICE_CP / SAPRDPRICE_CP * 100.0, 2%)
		END IF

		IF SAPRDPRICE_YTD = 0.0
		THEN
			YPS = 0.0
		ELSE
			YPS = FUNC_ROUND(PRDPRICE_YTD / SAPRDPRICE_YTD * &
				100.0, 2%)
		END IF

		RCODE$ = MID(REASON$, (I% * 3%) - 2%, 2%)

		OE_CREASON::DESCR = ""

17400		WHEN ERROR IN
			GET #OE_CREASON.CH%, KEY #0% EQ RCODE$, REGARDLESS
		USE
			FILENAME$ = "OE_CREASON"
			CONTINUE HelpError
		END WHEN

		TEXT$ = "   " + OE_CREASON::DESCR + &
			FORMAT$(QTY(I%, 1%), "#######.##") + &
			FORMAT$(PRDCOST_CP, "#######.##") + &
			FORMAT$(PRDPRICE_CP, "########.##") + &
			FORMAT$(PS, "######.##%") + &
			FORMAT$(QTY(I%, 2%), "#######.##") + &
			FORMAT$(PRDCOST_YTD, "########.##") + &
			FORMAT$(PRDPRICE_YTD, "#########.##") + &
			FORMAT$(YPS, "######.##%")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		FOR LOOP% = 1% TO TOT_REASON%
			IF OE_CREASON::DESCR = REASON(LOOP%)::DESCR
			THEN
				REASON(LOOP%)::QTY1 = &
					REASON(LOOP%)::QTY1 + QTY(I%, 1%)
				REASON(LOOP%)::PRDCOSTCP = &
					REASON(LOOP%)::PRDCOSTCP + PRDCOST_CP
				REASON(LOOP%)::PRDPRICECP = &
					REASON(LOOP%)::PRDPRICECP + PRDPRICE_CP
				REASON(LOOP%)::QTY2 = &
					REASON(LOOP%)::QTY2 + QTY(I%, 2%)
				REASON(LOOP%)::PRDCOSTYTD = &
					REASON(LOOP%)::PRDCOSTYTD + PRDCOST_YTD
				REASON(LOOP%)::PRDPRICEYTD = &
					REASON(LOOP%)::PRDPRICEYTD + &
					PRDPRICE_YTD
				GOTO EndArray
			END IF
		NEXT LOOP%

		TOT_REASON%, LOOP% = TOT_REASON% + 1%
		REASON(LOOP%)::DESCR = OE_CREASON::DESCR
		REASON(LOOP%)::QTY1 = QTY(I%, 1%)
		REASON(LOOP%)::PRDCOSTCP = PRDCOST_CP
		REASON(LOOP%)::PRDPRICECP = PRDPRICE_CP
		REASON(LOOP%)::QTY2 = QTY(I%, 2%)
		REASON(LOOP%)::PRDCOSTYTD = PRDCOST_YTD
		REASON(LOOP%)::PRDPRICEYTD = PRDPRICE_YTD

 EndArray:
		PROD_CP = PROD_CP + QTY(I%, 1%)
		PROD_YTD = PROD_YTD + QTY(I%, 2%)
 Init:
		FOR J% = 1% TO 2%
			QTY(I%, J%) = 0.0
		NEXT J%

	NEXT I%

	QTY(1%, J%) = 0.0 FOR J% = 1% TO 2%

	!
	! Print Product totals if we have printed lines
	!
	IF SAPRDPRICE_CP = 0.0
	THEN
		PS = 0.0
	ELSE
		PS = FUNC_ROUND(PRODPRICE_CP / SAPRDPRICE_CP * 100.0, 2%)
	END IF

	IF SAPRDPRICE_YTD = 0.0
	THEN
		YPS = 0.0
	ELSE
		YPS = FUNC_ROUND(PRODPRICE_YTD / SAPRDPRICE_YTD * 100.0, 2%)
	END IF

	TEXT$ = "Total for " + PD_PRODUCT::PRODUCT_NUM + SPACE$(18%) + &
		FORMAT$(PROD_CP, "########.##") + &
		FORMAT$(PRODCOST_CP, "#######.##") + &
		FORMAT$(PRODPRICE_CP, "########.##") + &
		FORMAT$(PS, "######.##%") + &
		FORMAT$(PROD_YTD, "#######.##") + &
		FORMAT$(PRODCOST_YTD, "########.##") + &
		FORMAT$(PRODPRICE_YTD, "#########.##") + &
		FORMAT$(YPS, "######.##%")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

	PROD_CP, PROD_YTD = 0.0

	GOTO GetNextRec

	%PAGE

 ExitTotal:
	!
	! Print out the Sales Amounts
	!
	TEXT$ = ":::::::::: REPORT TOTALS"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 999%)

	TEXT$ = "Total Price Per:" + FORMAT$(TOTSAPRDPRICECP, "########.##")
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)
	TEXT$ = "Total Cost  Per:" + FORMAT$(TOTSAPRDCOSTCP, "########.##")
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)
	TEXT$ = "Total Price YTD:" + FORMAT$(TOTSAPRDPRICEYTD, "########.##")
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)
	TEXT$ = "Total Cost  YTD:" + FORMAT$(TOTSAPRDCOSTYTD, "########.##")
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)

	FOR LOOP% = 1% TO TOT_REASON%

		IF TOTSAPRDPRICECP = 0.0
		THEN
			PS = 0.0
		ELSE
			PS = FUNC_ROUND(REASON(LOOP%)::PRDPRICECP / &
				TOTSAPRDPRICECP * 100.0, 2%)
		END IF

		IF TOTSAPRDPRICEYTD = 0.0
		THEN
			YPS = 0.0
		ELSE
			YPS = FUNC_ROUND(REASON(LOOP%)::PRDPRICEYTD / &
				TOTSAPRDPRICEYTD * 100.0, 2%)
		END IF

		TEXT$ = "   " + REASON(LOOP%)::DESCR + &
			FORMAT$(REASON(LOOP%)::QTY1, "#######.##") + &
			FORMAT$(REASON(LOOP%)::PRDCOSTCP, "#######.##") + &
			FORMAT$(REASON(LOOP%)::PRDPRICECP, "########.##") + &
			FORMAT$(PS, "######.##%") + &
			FORMAT$(REASON(LOOP%)::QTY2, "#######.##") + &
			FORMAT$(REASON(LOOP%)::PRDCOSTYTD, "########.##") + &
			FORMAT$(REASON(LOOP%)::PRDPRICEYTD, "#########.##") + &
			FORMAT$(YPS, "######.##%")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		GRTOTQTY1 = GRTOTQTY1 + REASON(LOOP%)::QTY1
		GRTOTQTY2 = GRTOTQTY2 + REASON(LOOP%)::QTY2
		GRTOTCOST_CP = GRTOTCOST_CP + REASON(LOOP%)::PRDCOSTCP
		GRTOTCOST_YTD = GRTOTCOST_YTD + REASON(LOOP%)::PRDCOSTYTD
		GRTOTPRICE_CP = GRTOTPRICE_CP + REASON(LOOP%)::PRDPRICECP
		GRTOTPRICE_YTD = GRTOTPRICE_YTD + REASON(LOOP%)::PRDPRICEYTD

	NEXT LOOP%

	IF TOTSAPRDPRICECP = 0.0
	THEN
		PS = 0.0
	ELSE
		PS = FUNC_ROUND(GRTOTPRICE_CP / TOTSAPRDPRICECP * 100.0, 2%)
	END IF

	IF TOTSAPRDPRICEYTD = 0.0
	THEN
		YPS = 0.0
	ELSE
		YPS = FUNC_ROUND(GRTOTPRICE_YTD / TOTSAPRDPRICEYTD * 100.0, 2%)
	END IF

	TEXT$ = "Total of All Products" + SPACE$(21%) + &
		FORMAT$(GRTOTQTY1, "########.##") + &
		FORMAT$(GRTOTCOST_CP, "#######.##") + &
		FORMAT$(GRTOTPRICE_CP, "########.##") + &
		FORMAT$(PS, "######.##%") + &
		FORMAT$(GRTOTQTY2, "#######.##") + &
		FORMAT$(GRTOTCOST_YTD, "########.##") + &
		FORMAT$(GRTOTPRICE_YTD, "#########.##") + &
		FORMAT$(YPS, "######.##%")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

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

	!
	! Get History record
	!
17500	WHEN ERROR IN
		FIND #IC_35HISTORY.CH%, KEY #0% EQ PRODUCT$, REGARDLESS
	USE
		CONTINUE ExitHist IF ERR = 155%
		FILENAME$ = "IC_35HISTORY"
		CONTINUE HelpError
	END WHEN

 GetHistRec:
17520	WHEN ERROR IN
		GET #IC_35HISTORY.CH%, REGARDLESS
	USE
		CONTINUE ExitHist IF ERR = 11%
		FILENAME$ = "IC_35HISTORY"
		CONTINUE HelpError
	END WHEN

17530	GOTO ExitHist &
		IF IC_35HISTORY::PRODUCT <> PRODUCT$

	GOTO GetHistRec &
		IF COMP_ARRAY(EDIT$( &
		IC_35HISTORY::LOCATION, -1%), LOC_WLDCRD$) = 0% &
		AND LOC_WLDCRD$ <> ""

	L% = INSTR(1%, REASON$, IC_35HISTORY::TRANSTYPE)

	GOTO GetHistRec &
		IF L% = 0%

	L% = (L% + 2%) / 3%

	FOR I% = 1% TO PP%

		!
		! If We are on the current period
		!
		IF I% = PP%
		THEN
			!
			! If This is a sale record
			!
			IF L% = 1%
			THEN
				QTY(1%, 1%) = QTY(1%, 1%) - &
					FUNC_ROUND( &
					IC_35HISTORY::PQUANTITY(I%) / &
					CONV_FACTOR, 3%)

				TOTQTY(1%, 1%) = TOTQTY(1%, 1%) - &
					FUNC_ROUND( &
					IC_35HISTORY::PQUANTITY(I%) / &
					CONV_FACTOR, 3%)

			!
			! Not a sales record
			!
			ELSE
				QTY(L%, 1%) = QTY(L%, 1%) + &
					FUNC_ROUND( &
					IC_35HISTORY::PQUANTITY(I%) / &
					CONV_FACTOR, 3%)

				TOTQTY(L%, 1%) = TOTQTY(L%, 1%) + &
					FUNC_ROUND( &
					IC_35HISTORY::PQUANTITY(I%) / &
					CONV_FACTOR, 3%)

			END IF

		END IF

		!
		! If This is a sale record
		!
		IF L% = 1%
		THEN
			QTY(1%, 2%) = QTY(1%, 2%) - &
				FUNC_ROUND(IC_35HISTORY::PQUANTITY(I%) / &
				CONV_FACTOR, 3%)

			TOTQTY(1%, 2%) = TOTQTY(1%, 2%) - &
				FUNC_ROUND(IC_35HISTORY::PQUANTITY(I%) / &
				CONV_FACTOR, 3%)

		ELSE

			QTY(L%, 2%) = QTY(L%, 2%) + &
				FUNC_ROUND(IC_35HISTORY::PQUANTITY(I%) / &
				CONV_FACTOR, 3%)

			TOTQTY(L%, 2%) = TOTQTY(L%, 2%) + &
				FUNC_ROUND(IC_35HISTORY::PQUANTITY(I%) / &
				CONV_FACTOR, 3%)

		END IF

	NEXT I%

	GOTO GetHistRec

 ExitHist:
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
	! End of report SA_RPRT_PRDRETURN
	!******************************************************************
	END
