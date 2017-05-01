1	%TITLE "Product Cost Structure"
	%SBTTL "BM_RPRT_LOWCOST_02"
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
	! ID:BM005
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Product Cost Structure\* report gives information on
	!	how the product cost was derived.  This report includes the
	!	following fields:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	Product Number
	!	.le
	!	Product Description
	!	.le
	!	Product Type
	!	.le
	!	Product Category
	!	.le
	!	Product Secondary Code
	!	.le
	!	Unit of Measure
	!	.le
	!	Burden
	!	.le
	!	Labor Costs
	!	.le
	!	Material Costs
	!	.le
	!	Operation
	!	.le
	!	Labor Hours
	!	.le
	!	Material Type
	!	.le
	!	Material Description
	!	.els
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS BM_SOURCE:BM_RPRT_LOWCOST_02/LINE
	!	$ LINK/EXE=BM_EXE: BM_RPRT_LOWCOST_02, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE BM_RPRT_LOWCOST_02.OBJ;*
	!
	! Author:
	!
	!	08/16/2000 - Kevin Handy
	!		Taken from BM_RPRT_LOWCOST
	!
	! Modification History:
	!
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE		UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[BM.OPEN]BM_RELATION.HB"
	MAP (BM_RELATION)	BM_RELATION_CDD		BM_RELATION

	%INCLUDE "SOURCE:[BM.OPEN]BM_PRODOPER.HB"
	MAP (BM_PRODOPER)	BM_PRODOPER_CDD		BM_PRODOPER

	%INCLUDE "SOURCE:[BM.OPEN]BM_CONTROL.HB"
	MAP (BM_CONTROL)	BM_CONTROL_CDD		BM_CONTROL

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODTYPE.HB"
	MAP (PD_PRODTYPE)	PD_PRODTYPE_CDD		PD_PRODTYPE

	%INCLUDE "SOURCE:[PR.OPEN]PR_OPER.HB"
	MAP (PR_OPER)		PR_OPER_CDD		PR_OPER

	DECLARE INTEGER CONSTANT MAX_COMP = 1000%
	DECLARE INTEGER CONSTANT MAX_PROD = 500%
	DECLARE INTEGER CONSTANT MAX_LEVEL = 50%

	RECORD COMPONENT_RECORD
		STRING NUMBER   = 14%, &
		STRING PROD_TYPE = 2%, &
		REAL   QUANTITY
	END RECORD

	DIM COMPONENT_RECORD	COMPONENT(MAX_COMP)

	DIM RFA			RFA_LEVEL(MAX_PROD)
	DIM REAL		QTY_LEVEL(MAX_PROD)
	DIM STRING		TEST_PRODUCT(MAX_PROD)

	DIM REAL		PRODTYPEAMT(MAX_LEVEL)
	DIM REAL		HOURS(MAX_LEVEL)
	DIM REAL		RATE(MAX_LEVEL)
	DIM STRING		PRODTYPE(MAX_LEVEL)
	DIM STRING		OPERATION(MAX_LEVEL)

	!
	! External functions
	!
	EXTERNAL REAL    FUNCTION PC_READ_COST
	EXTERNAL LONG    FUNCTION PD_EXAM_PRODUCT
	EXTERNAL REAL    FUNCTION PC_READ_PRICE

	%PAGE

	!
 Init:	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field begins the report
	!	with a selected item by entering the selection in this field.
	!	.b
	!	A blank field will cause the report to begin with the first
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field enters an item
	!	with which the report will end by entering the selected
	!	item in this field.
	!	.b
	!	A blank setting will cause the report to end with the
	!	last item in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects designated
	!	items to be printed by entering a "Wildcard"
	!	selection in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

	WHEN ERROR IN
		ADD_BURDEN = VAL(UTL_REPORTX::OPTDEF(3%))
	USE
		FILENAME$ = ""
		Continue HelpError
	END WHEN

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects designated
	!	items to be printed by entering a "Wildcard"
	!	selection in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

	LOCATION$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Location _#\*
	!	.b
	!	.lm +5
	!	The ^*Location _#\* field is provided to enter a location
	!	code which is established in the Company Profile file
	!	located in the Utility system.
	!	.b
	!	This field will accommodate up to four (4) alphanumeric
	!	characters.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

	EFF_DATE$ = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(5%), 132%))
	EFF_DATE$ = DATE_TODAY IF EFF_DATE$ = ""

	!++
	! Abstract:FLD06
	!	^*(06) Effective Date\*
	!	.b
	!	.lm +5
	!	The ^*Effective Date\* field enters the date on which
	!	the standard cost is calculated.  The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!
	!--

	XTYPE$ = LEFT(UTL_REPORTX::OPTDEF(6%), 2%)

	!++
	! Abstract:FLD07
	!	^*(08) Price Type\*
	!	.b
	!	.lm +5
	!	Price type to use for the current price to comapre against.
	!	.lm -5
	!
	! Index:
	!
	!--

	SORT_BY$ = EDIT$(UTL_REPORTX::OPTDEF(9%), 132%)

	!++
	! Abstract:FLD10
	!	^*(10) Sort\*
	!	.b
	!	.lm +5
	!	The ^*Sort\* field selects an order
	!	by which the report will print.
	!	.b
	!	Valid settings are:
	!	.table 3,25
	!	.te
	!	^*C\* = Product Category
	!	.te
	!	^*D\* = Product Description
	!	.te
	!	^*P\* = Product Number
	!	.te
	!	^*S\* = Product Secondary Code
	!	.te
	!	^*T\* = Product Type
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[BM.OPEN]BM_RELATION.OPN"
	USE
		FILENAME$ = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[BM.OPEN]BM_PRODOPER.OPN"
	USE
		CONTINUE 320 IF ERR = 5%
		FILENAME$ = "BM_PRODOPER"
		CONTINUE HelpError
	END WHEN

320	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

340	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODTYPE.OPN"
	USE
		CONTINUE 350 IF ERR = 5%
		FILENAME$ = "PD_PRODTYPE"
		CONTINUE HelpError
	END WHEN

350	WHEN ERROR IN
		%INCLUDE "SOURCE:[BM.OPEN]BM_CONTROL.OPN"
		GET #BM_CONTROL.CH%, RECORD 1%, REGARDLESS
		CLOSE #BM_CONTROL.CH%
	USE
		CONTINUE 360 IF ERR = 5%
		FILENAME$ = "BM_CONTROL"
		CONTINUE HelpError
	END WHEN

360	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_OPER.OPN"
	USE
		CONTINUE ReportTitle IF ERR = 5%
		FILENAME$ = "PR_OPER"
		CONTINUE HelpError
	END WHEN

500	GOTO 600 IF SORT_BY$ <> "CD"

	CALL ENTR_3MESSAGE(SCOPE, "Creating sort file", 1%)

	!======================================================================
	! PD_PRODUCT file (open read only)
	!======================================================================

	CALL ASSG_CHANNEL(PD_PRODUCT_TEMP.CH%, STAT%)

	PD_PRODUCT_TEMP.NAME$ = "PD_PRODUCT.TEMP"

	WHEN ERROR IN
		OPEN PD_PRODUCT_TEMP.NAME$ FOR OUTPUT AS FILE PD_PRODUCT_TEMP.CH%, &
			ORGANIZATION INDEXED FIXED, &
			TEMPORARY, &
			MAP PD_PRODUCT, &
			PRIMARY KEY &
			( &
				PD_PRODUCT::CATEGORY, &
				PD_PRODUCT::DESCRIPTION &
			) DUPLICATES, &
			ACCESS MODIFY
	USE
		FILENAME$ = "SORT FILE"
		CONTINUE HelpError
	END WHEN

510	!
	! Build sorted file
	!
	WHEN ERROR IN
		IF FROM_ITEM$ <> ""
		THEN
			FIND #PD_PRODUCT.CH%, KEY #2% GE FROM_ITEM$, REGARDLESS
		ELSE
			RESET #PD_PRODUCT.CH%
		END IF
	USE
		FILENAME$ = "SORT FILE"
		CONTINUE HelpError
	END WHEN

520	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, REGARDLESS
	USE
		CONTINUE 590
	END WHEN

	GOTO 520 IF WILDLABEL$ <> "" AND &
		COMP_STRING(EDIT$(PD_PRODUCT::LABEL, -1%), WILDLABEL$) = 0%

	GOTO 520 IF PD_PRODUCT::SSTATUS <> "A"

	GOTO 520 IF (PD_PRODUCT::CATEGORY > TO_ITEM$) AND TO_ITEM$ <> ""

	PUT #PD_PRODUCT_TEMP.CH%

	GOTO 520

590	CLOSE #PD_PRODUCT.CH%

	PD_PRODUCT.CH% = PD_PRODUCT_TEMP.CH%

600	!

 ReportTitle:
	!
	! Title
	!
	SELECT SORT_BY$

	CASE "C"
		SORT_KEY% = 2%
		ADD_TITLE$ = "BY  CATEGORY"

	CASE "CD"
		SORT_KEY% = 0%
		ADD_TITLE$ = "BY  CATEGORY/DESCRIPTION"

	CASE "D"
		SORT_KEY% = 3%
		ADD_TITLE$ = "BY  DESCRIPTION"

	CASE "P"
		SORT_KEY% = 0%
		ADD_TITLE$ = "BY  PRODUCT  NUMBER"

	CASE "S"
		SORT_KEY% = 4%
		ADD_TITLE$ = "BY  SECONDARY  CODE"

	CASE "T"
		SORT_KEY% = 1%
		ADD_TITLE$ = "BY  PRODUCT  TYPE"

	END SELECT

	TITLE$(1%) = "PRODUCT  COST  STRUCTURE  " + ADD_TITLE$

	TITLE$(2%) = "Bill of Material System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "Product#       Description                   " + &
		" Hours   Burden" + &
		"      Labor  Materials Add.Burden    TotalCost      Price"

	TITLE$(5%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PD_PRODUCT.CH%, KEY #SORT_KEY%
		ELSE
			FIND #PD_PRODUCT.CH%, &
				KEY #SORT_KEY% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	SELECT SORT_BY$

	CASE "C", "CD"
		GOTO ExitProgram IF (PD_PRODUCT::CATEGORY > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(PD_PRODUCT::CATEGORY, -1%), &
			WLDCRD$) = 0%

		IF TEST_CATEGORY$ <> PD_PRODUCT::CATEGORY AND &
			TEST_CATEGORY$ <> "" AND PRINT_LINE%
		THEN
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
			PRINT_LINE% = 0%
		END IF

		TEST_CATEGORY$ = PD_PRODUCT::CATEGORY

	CASE "D"
		GOTO ExitProgram IF (PD_PRODUCT::DESCRIPTION > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(PD_PRODUCT::DESCRIPTION, -1%), &
			WLDCRD$) = 0%

	CASE "P"
		GOTO ExitProgram IF (PD_PRODUCT::PRODUCT_NUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(PD_PRODUCT::PRODUCT_NUM, -1%), &
			WLDCRD$) = 0%

	CASE "S"
		GOTO ExitProgram IF (PD_PRODUCT::SECONDARY_CODE > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(PD_PRODUCT::SECONDARY_CODE, -1%), &
			WLDCRD$) = 0%

	CASE "T"
		GOTO ExitProgram IF (PD_PRODUCT::PROD_TYPE> TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(PD_PRODUCT::PROD_TYPE, -1%), &
			WLDCRD$) = 0%

		IF TEST_PRODTYPE$ <> PD_PRODUCT::PROD_TYPE AND &
			TEST_PRODTYPE$ <>"" AND PRINT_LINE%
		THEN
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
			PRINT_LINE% = 0%
		END IF

		TEST_PRODTYPE$ = PD_PRODUCT::PROD_TYPE

	END SELECT

17200	WHEN ERROR IN
		GET #BM_RELATION.CH%, &
			KEY #0% EQ PD_PRODUCT::PRODUCT_NUM, &
			REGARDLESS
	USE
		CONTINUE GetNextRec IF ERR = 155%
		FILENAME$ = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

	!
	! Get the cost for this product
	!
	PROD_COST = PC_READ_COST(BM_RELATION::PRODUCT, LOCATION$, &
		EFF_DATE$, "")

	THIS_PRICE = PC_READ_PRICE(BM_RELATION::PRODUCT, LOCATION$, &
		XTYPE$, EFF_DATE$, "", USEDDATE$, USEDTIME$)

	TEXT$ = BM_RELATION::PRODUCT + " " + &
		LEFT(PD_PRODUCT::DESCRIPTION, 25%)

	!
	! Initialize some variables
	!
	FOR I% = 1% TO MAX_LEVEL

		PRODTYPE(I%)    = SPACE$(LEN(PD_PRODTYPE::CODE))
		PRODTYPEAMT(I%) = 0.0
		OPERATION(I%)   = SPACE$(LEN(BM_PRODOPER::OPERATION))
		HOURS(I%)       = 0.0
		RATE(I%)        = 0.0

	NEXT I%

	ITEM_LOOP% = 0%
	QTY_LEVEL(0%), LEVEL% = 1%

 GoDownTree:
	TEST_PRODUCT(LEVEL%) = BM_RELATION::PRODUCT
	QTY_LEVEL(LEVEL%) = QTY_LEVEL(LEVEL% - 1%) * BM_RELATION::QUANTITY
	RFA_LEVEL(LEVEL%) = GETRFA(BM_RELATION.CH%)

	!
	! Check for product loop.
	!
	FOR ERRORLOOP% = 0% TO LEVEL% - 1%

		IF BM_RELATION::PRODUCT = TEST_PRODUCT(ERRORLOOP%)
		THEN
			TEXT$ = "**** Product loop in product " + &
				BM_RELATION::PRODUCT + &
				"****"
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
			GOTO ExitProgram IF UTL_REPORTX::STAT

			GOTO 18000
		END IF
	NEXT ERRORLOOP%

17320	WHEN ERROR IN
		GET #BM_RELATION.CH%, &
			KEY #0% EQ BM_RELATION::COMPONENT, &
			REGARDLESS
	USE
		CONTINUE 18000 IF ERR = 155%
		FILENAME$ = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

	LEVEL% = LEVEL% + 1%
	GOTO GoDownTree

 GoUpTree:
	GOTO SortComponents IF LEVEL% - 1% = 0%

	LEVEL% = LEVEL% - 1%

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

 SortComponents:
	!
	! Sort array
	!
	FOR I% = 1% TO ITEM_LOOP%
		INDEX% = I%
		COMPONENT(0%) = COMPONENT(I%)

		FOR J% = I% TO ITEM_LOOP%

			IF COMPONENT(J%)::PROD_TYPE + COMPONENT(J%)::NUMBER < &
				COMPONENT(0%)::PROD_TYPE + COMPONENT(0%)::NUMBER
			THEN
				COMPONENT(0%) = COMPONENT(J%)
				INDEX% = J%
			END IF

		NEXT J%

		COMPONENT(INDEX%) = COMPONENT(I%)
		COMPONENT(I%) = COMPONENT(0%)

	NEXT I%

	COMPONENT(0%) = COMPONENT(1%)
	TOTAL_PRODUCT = 0.0

	!
	! Get Materials information
	!
	TYPE_LOOP%     = 0%
	LAST_PRODTYPE$ = ""
	TOTAL_MATERIAL = 0.0

	FOR I% = 1% TO ITEM_LOOP%

		V% = PD_EXAM_PRODUCT(COMPONENT(I%)::NUMBER, PD_PRODUCT_EXAM)

		PD_PRODUCT_EXAM::PRODUCT_FACTOR = 1.0 &
			IF PD_PRODUCT_EXAM::PRODUCT_FACTOR = 0.0

		COST = PC_READ_COST(COMPONENT(I%)::NUMBER, LOCATION$, &
			EFF_DATE$, "")

		COST = COST / PD_PRODUCT_EXAM::PRODUCT_FACTOR

		IF LAST_PRODTYPE$ <> COMPONENT(I%)::PROD_TYPE
		THEN
			TYPE_LOOP% = TYPE_LOOP% + 1%

			PRODTYPE(TYPE_LOOP%) = COMPONENT(I%)::PROD_TYPE

			LAST_PRODTYPE$ = COMPONENT(I%)::PROD_TYPE
		END IF

		PRODTYPEAMT(TYPE_LOOP%) = PRODTYPEAMT(TYPE_LOOP%) + &
			FUNC_ROUND(COMPONENT(I%)::QUANTITY * COST, 3%)

		TOTAL_MATERIAL = TOTAL_MATERIAL + &
			FUNC_ROUND(COMPONENT(I%)::QUANTITY * COST, 2%)

	NEXT I%

	!
	! Get Labor information
	!
	LABOR_LOOP%     = 0%
	LAST_OPERATION$ = SPACE$(LEN(BM_PRODOPER::OPERATION) + 1%)
	LAST_HOURS      = 0.0
	LAST_LABOR      = 0.0
	TOTAL_HOURS     = 0.0
	TOTAL_LABOR     = 0.0

17400	WHEN ERROR IN
		FIND #BM_PRODOPER.CH%, KEY #1% EQ TEST_PRODUCT(1%), REGARDLESS
	USE
		CONTINUE 17500 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "BM_PRODOPER"
		CONTINUE HelpError
	END WHEN

17420	WHEN ERROR IN
		GET #BM_PRODOPER.CH%, REGARDLESS
	USE
		CONTINUE 17500 IF ERR = 11%
		FILENAME$ = "BM_PRODOPER"
		CONTINUE HelpError
	END WHEN

	GOTO 17500 IF BM_PRODOPER::PRODUCT <> TEST_PRODUCT(1%)
	GOTO 17420 IF BM_PRODOPER::STAT <> "A"
	GOTO 17420 IF BM_PRODOPER::EFFDATE > EFF_DATE$

	IF BM_PRODOPER::OPERATION <> LAST_OPERATION$
	THEN
		LABOR_LOOP% = LABOR_LOOP% + 1%
		OPERATION(LABOR_LOOP%) = BM_PRODOPER::OPERATION

		OPERATION(LABOR_LOOP%) = SPACE$(LEN(BM_PRODOPER::OPERATION)) &
			IF EDIT$(OPERATION(LABOR_LOOP%), -1%) = ""

		LAST_OPERATION$ = BM_PRODOPER::OPERATION

		HOURS(LABOR_LOOP%) = HOURS(LABOR_LOOP%) + BM_PRODOPER::HOURS
		TOTAL_HOURS        = TOTAL_HOURS + BM_PRODOPER::HOURS
		LAST_LABOR         = 0.0
	ELSE
		HOURS(LABOR_LOOP%) = HOURS(LABOR_LOOP%) + &
			BM_PRODOPER::HOURS - LAST_HOURS
		TOTAL_HOURS = TOTAL_HOURS + BM_PRODOPER::HOURS - LAST_HOURS
	END IF

	LAST_HOURS = BM_PRODOPER::HOURS

	!
	! Get the rate
	!
	PR_OPER::HOUR_RATE = 0.0

17450	WHEN ERROR IN
		GET #PR_OPER.CH%, &
			KEY #0% EQ OPERATION(LABOR_LOOP%), &
			REGARDLESS
	USE
		CONTINUE 17460 IF ERR = 155% OR ERR = 9% OR ERR = 145%
		FILENAME$ = "PR_OPER"
		CONTINUE HelpError
	END WHEN

17460	RATE(LABOR_LOOP%) = PR_OPER::HOUR_RATE

	TOTAL_LABOR = TOTAL_LABOR + FUNC_ROUND(HOURS(LABOR_LOOP%) * &
		PR_OPER::HOUR_RATE, 2%) - LAST_LABOR

	LAST_LABOR = FUNC_ROUND(HOURS(LABOR_LOOP%) * PR_OPER::HOUR_RATE, 2%)

	GOTO 17420

17500	IF BM_CONTROL::BURDENPERC <> 0.0
	THEN
		TOTAL_BURDEN  = FUNC_ROUND(BM_CONTROL::BURDENPERC * &
			TOTAL_LABOR / 100.0, 2%)
	ELSE
		TOTAL_BURDEN  = FUNC_ROUND(BM_CONTROL::BURDENRATE * &
			TOTAL_HOURS, 2%)
	END IF

	TOTAL_PRODUCT = TOTAL_BURDEN + TOTAL_LABOR + TOTAL_MATERIAL

	IF FUNC_ROUND(TOTAL_PRODUCT, 2%) = FUNC_ROUND(PROD_COST, 2%)
	THEN
		MATCH$ = "   "
	ELSE
		MATCH$ = " ??"
	END IF

	MORE_BURDEN = FUNC_ROUND(ADD_BURDEN * TOTAL_HOURS, 2%)

	!
	! Print out Product
	!
	TEXT$ = TEXT$ + "   " + &
		FORMAT$(TOTAL_HOURS, "####.###") + " " + &
		FORMAT$(TOTAL_BURDEN, "#,###.##") + " " + &
		FORMAT$(TOTAL_LABOR, "###,###.##") + " " + &
		FORMAT$(TOTAL_MATERIAL, "###,###.##") + " " + &
		FORMAT$(MORE_BURDEN, "###,###.##") + " " + &
		FORMAT$(TOTAL_PRODUCT + MORE_BURDEN, "#,###,###.##") + &
		FORMAT$(THIS_PRICE, "#,###,###.##") + &
		MATCH$

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

17700	!
	! Try for next record
	!
	WHEN ERROR IN
		FIND #BM_RELATION.CH%, &
			KEY #0% GE TEST_PRODUCT(1%) + "9999", &
			REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

	GOTO GetNextRec

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

18000	!
	! Array of terminals
	!
	FOR I% = 1% TO ITEM_LOOP%

		IF BM_RELATION::COMPONENT = COMPONENT(I%)::NUMBER
		THEN
			COMPONENT(I%)::QUANTITY = COMPONENT(I%)::QUANTITY + &
				FUNC_ROUND(QTY_LEVEL(LEVEL%), 3%)

			GOTO Ret18010
		END IF

	NEXT I%

	V% = PD_EXAM_PRODUCT(BM_RELATION::COMPONENT, PD_PRODUCT_EXAM)

	GOTO Ret18010 IF COMP_ARRAY(EDIT$( &
		PD_PRODUCT_EXAM::PROD_TYPE, -1%), BM_CONTROL::PRODTYPE) = 0% &
		AND BM_CONTROL::PRODTYPE <> ""

	ITEM_LOOP% = ITEM_LOOP% + 1%
	COMPONENT(ITEM_LOOP%)::NUMBER = BM_RELATION::COMPONENT
	COMPONENT(ITEM_LOOP%)::PROD_TYPE = PD_PRODUCT_EXAM::PROD_TYPE
	COMPONENT(ITEM_LOOP%)::QUANTITY = QTY_LEVEL(LEVEL%)

 Ret18010:
	GOTO 17330

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	UTL_REPORTX::STAT = -1%
	GOTO ExitProgram

32767	END
