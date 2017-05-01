1	%TITLE "Bill of Material"
	%SBTTL "BM_RPRT_SUMREQ"
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
	! ID:BM007
	!
	! Abstract:HELP
	!	.p
	!	This program prints out Product lowest level structure
	!	by type in Bill of Materials System. This report includes the following fields:
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
	!	Component Number
	!	.le
	!	Component Description
	!	.le
	!	Component Type
	!	.le
	!	Category
	!	.le
	!	Product Unit of Measure
	!	.le
	!	Component Unit of Measure
	!	.le
	!	Quantity to Make
	!	.le
	!	Cost
	!	.le
	!	Extended Cost
	!	.els
	!
	! Index:
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS BM_SOURCE:BM_RPRT_SUMREQ/LINE
	!	$ LINK/EXE=BM_EXE: BM_RPRT_SUMREQ, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE BM_RPRT_SUMREQ.OBJ;*
	!
	! Author:
	!
	!	10/19/92 - Dan Perkins
	!
	! Modification History:
	!
	!	10/26/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	10/29/92 - Dan Perkins
	!		Added cost to report.
	!
	!	11/17/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	11/10/93 - Kevin Handy
	!		Modified so that large negitive numbers don't print
	!		out in '%' format as often.
	!
	!	11/11/93 - Frank F. Starman
	!		Print correct description for components.
	!
	!	12/03/93 - Kevin Handy
	!		Modified to disply quanity required in regular
	!		units of measure instead of BM units of measure.
	!		(Request from Jerry for Robinsons). Program
	!		was comparing BM units needed with IC available.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/03/96 - Kevin Handy
	!		Reformat source code.
	!
	!	11/27/96 - Kevin Handy
	!		Lose some goofy '+ ""' code.
	!
	!	12/20/96 - Kevin handy
	!		Added code to be able to stop at a given product type
	!
	!	12/26/96 - Kevin Handy
	!		Change a call of COMP_ARRAY using codes read from
	!		the comtrol file to COMP_STRING.
	!
	!	12/30/96 - Kevin Handy
	!		Re-enable test for product type in bill of material.
	!
	!	01/10/97 - Kevin Handy
	!		Stop at any level for product types not defined
	!		in the control file, and not just final level.
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE		UTL_REPORTX_CDD			UTL_REPORTX

	%INCLUDE "SOURCE:[BM.OPEN]BM_CONTROL.HB"
	MAP (BM_CONTROL)	BM_CONTROL_CDD		BM_CONTROL

	%INCLUDE "SOURCE:[BM.OPEN]BM_RELATION.HB"
	MAP (BM_RELATION)	BM_RELATION_CDD		BM_RELATION

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	RECORD COMPONENT_RECORD
		STRING NUMBER   = 14%, &
		STRING PROD_TYPE = 2%, &
		REAL   QUANTITY
	END RECORD

	DIM COMPONENT_RECORD	COMPONENT(1000%)
	DIM RFA			RFA_LEVEL(500%)
	DIM REAL		QTY_LEVEL(500%)
	DIM STRING		TEST_PRODUCT(500%)

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION PD_EXAM_PRODUCT
	EXTERNAL LONG    FUNCTION IC_READ_35BALANCE
	EXTERNAL REAL    FUNCTION PC_READ_COST

	%PAGE

	ON ERROR GOTO 19000

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
	!	The value entered must be in agreement with the value in field
	!	(10) Sort by.
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
	!	item in this field.  The value entered must be in agreement
	!	with the value in field (10) Sort by.
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
	!	items to be printed on the report by entering a "Wildcard"
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
	!	The ^*Location _#\* field enters a location
	!	code which is to be printed.
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

	END_TYPE$ = EDIT$(UTL_REPORTX::OPTDEF(7%), -1%)

	!++
	! Abstract:FLD08
	!	^*(08) End at Type\*
	!	.b
	!	.lm +5
	!	Used to control which level the report will go down to.
	!	A blank entry here will cause it to go to the lowest BOM level.
	!	Wildcards are supported in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

	QUANTITY% = VAL%(EDIT$(UTL_REPORTX::OPTDEF(8%), -1%))
	QUANTITY% = 1% IF QUANTITY% = 0%

	!++
	! Abstract:FLD09
	!	^*(09) Quantity to Make\*
	!	.b
	!	.lm +5
	!	The ^*Quantity to Make\* field enters the number
	!	of completed units which are desired.
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
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

320	WHEN ERROR IN
		%INCLUDE "SOURCE:[BM.OPEN]BM_CONTROL.OPN"

		GET #BM_CONTROL.CH%, RECORD 1%, REGARDLESS

		CLOSE #BM_CONTROL.CH%
	USE
		CONTINUE ReportTitle IF ERR = 5%
		FILENAME$ = "BM_CONTROL"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	SELECT SORT_BY$

	CASE "C"
		SORT_KEY% = 2%
		ADD_TITLE$ = "BY  CATEGORY"

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

	TITLE$(1%) = "PRODUCT  STRUCTURE  " + ADD_TITLE$ + &
		"  ON  THE  LOWEST  LEVEL"

	TITLE$(2%) = "Bill of Material System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "Product#       Description               Type" + &
		" Cat  SecCode    UOM    Ext_Qty    On_Hand   " + &
		"   Alloc    On_Ordr   Required       Cost"

	TITLE$(5%) = SPACE$(16%) + "Type Component#     " + &
		"Description"


	TITLE$(6%) = "."

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
	!GOTO GetNextRec IF PD_PRODUCT::SSTATUS <> "A"

	SELECT SORT_BY$

	CASE "C"
		GOTO ExitProgram IF (PD_PRODUCT::CATEGORY > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(PD_PRODUCT::CATEGORY, -1%), &
			WLDCRD$) = 0%

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
		GOTO ExitProgram IF (PD_PRODUCT::PROD_TYPE > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(PD_PRODUCT::PROD_TYPE, -1%), &
			WLDCRD$) = 0%

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

17300	!
	! Print out Product
	!
	PD_PRODUCT$ = BM_RELATION::PRODUCT

	V% = IC_READ_35BALANCE(PD_PRODUCT$, LOCATION$, BALANCE(,))

	ONHAND = BALANCE(1%, 1%) + BALANCE(1%, 2%) + BALANCE(1%, 3%)
	ALLOC   = BALANCE(2%, 1%) + BALANCE(2%, 2%) + BALANCE(2%, 3%)
	ONORDER = BALANCE(3%, 1%) + BALANCE(3%, 2%) + BALANCE(3%, 3%)

	REQUIRED = -(ONHAND + ALLOC + ONORDER - QUANTITY%)
	REQUIRED = QUANTITY% IF QUANTITY% < REQUIRED
	REQUIRED = 0.0 IF REQUIRED < 0.0

	TEXT$ = BM_RELATION::PRODUCT + " " + &
		LEFT(PD_PRODUCT::DESCRIPTION, 25%) + " " + &
		PD_PRODUCT::PROD_TYPE + "   " + &
		PD_PRODUCT::CATEGORY + " " + &
		PD_PRODUCT::SECONDARY_CODE + " " + &
		PD_PRODUCT::BOMUOM + &
		FORMAT$(QUANTITY%, "#,###,###.##") + &
		FORMAT$(ONHAND, "####,###.##") + &
		FORMAT$(-ALLOC, "####,###.##") + &
		FORMAT$(ONORDER, "####,###.##") + &
		FORMAT$(REQUIRED, "<%>###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	ITEM_LOOP% = 0%
	LEVEL% = 1%
	QTY_LEVEL(0%) = QUANTITY%

 GoDownTree:
	TEST_PRODUCT(LEVEL%) = BM_RELATION::PRODUCT
	QTY_LEVEL(LEVEL%) = QTY_LEVEL(LEVEL% - 1%) * BM_RELATION::QUANTITY
	RFA_LEVEL(LEVEL%) = GETRFA(BM_RELATION.CH%)

17320	!
	! Stop at a given category
	!
	WHEN ERROR IN
		GET #BM_RELATION.CH%, &
			KEY #0% EQ BM_RELATION::COMPONENT, &
			REGARDLESS
	USE
		CONTINUE 17325 IF ERR = 155%
		FILENAME$ = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

	V% = PD_EXAM_PRODUCT(BM_RELATION::COMPONENT, PD_PRODUCT_EXAM)

	!
	! Skip over some product types at any level
	!
 !	GOTO 17330 IF COMP_STRING(EDIT$( &
 !		PD_PRODUCT_EXAM::PROD_TYPE, -1%), BM_CONTROL::PRODTYPE) = 0% &
 !		AND BM_CONTROL::PRODTYPE <> ""

	!
	! Stop at specific product types
	!
	IF END_TYPE$ <> ""
	THEN
		IF COMP_STRING(END_TYPE$, &
			PD_PRODUCT_EXAM::PROD_TYPE) <> 0%
		THEN
			GOTO 17325
		END IF
	END IF

	LEVEL% = LEVEL% + 1%
	GOTO GoDownTree

17325	GOSUB 18000
	GOTO 17330

 GoUpTree:
	GOTO PrintComponents IF LEVEL% = 1%

	LEVEL% = LEVEL% - 1%

17330	WHEN ERROR IN
		GET #BM_RELATION.CH%, RFA RFA_LEVEL(LEVEL%), REGARDLESS
	USE
		CONTINUE GoUpTree IF ERR = 155%
		FILENAME$ = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

17335	WHEN ERROR IN
		GET #BM_RELATION.CH%, REGARDLESS
	USE
		CONTINUE GoUpTree IF ERR = 11%
		FILENAME$ = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

	IF BM_RELATION::PRODUCT <> TEST_PRODUCT(LEVEL%)
	THEN
		GOTO GoUpTree
	ELSE
		V% = PD_EXAM_PRODUCT(BM_RELATION::COMPONENT, PD_PRODUCT_EXAM)

		!
		! Skip over some product types at any level
		!
 !		GOTO 17335 IF COMP_STRING(EDIT$( &
 !			PD_PRODUCT_EXAM::PROD_TYPE, -1%), BM_CONTROL::PRODTYPE) = 0% &
 !			AND BM_CONTROL::PRODTYPE <> ""

		!
		! Stop at specific product types
		!
		IF END_TYPE$ <> ""
		THEN
			IF COMP_STRING(END_TYPE$, &
				PD_PRODUCT_EXAM::PROD_TYPE) <> 0%
			THEN
				GOSUB 18000
				GOTO 17335
			END IF
	END IF

		GOTO GoDownTree
	END IF

 PrintComponents:
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
	! Print array
	!
	FOR I% = 1% TO ITEM_LOOP%

		PD_PRODUCT$ = COMPONENT(I%)::NUMBER

		!
		! Read Product File
		!
		V% = PD_EXAM_PRODUCT(PD_PRODUCT$, PD_PRODUCT_EXAM)

		PD_PRODUCT_EXAM::PRODUCT_FACTOR = 1.0 &
			IF PD_PRODUCT_EXAM::PRODUCT_FACTOR = 0.0

		!
		! Calculate cost
		!
		COST = PC_READ_COST(PD_PRODUCT$, LOCATION$, "", "")
 !		COST = COST / PD_PRODUCT_EXAM::PRODUCT_FACTOR

		!
		! Convert quantity from BM to IC quantity
		!
		COMPONENT(I%)::QUANTITY = COMPONENT(I%)::QUANTITY / &
			PD_PRODUCT_EXAM::PRODUCT_FACTOR

		!
		! Compute $ totals for this part
		!
		TOTAL_COMPONENT = FUNC_ROUND(COMPONENT(I%)::QUANTITY * &
			COST, 3%)

		TOTAL_PRODUCT = TOTAL_PRODUCT + &
			FUNC_ROUND(COMPONENT(I%)::QUANTITY * COST, 3%)

		V% = IC_READ_35BALANCE(PD_PRODUCT$, LOCATION$, BALANCE(,))

		ONHAND = BALANCE(1%, 1%) + BALANCE(1%, 2%) + BALANCE(1%, 3%)
		ALLOC = BALANCE(2%, 1%) + BALANCE(2%, 2%) + BALANCE(2%, 3%)
		ONORDER = BALANCE(3%, 1%) + BALANCE(3%, 2%) + BALANCE(3%, 3%)

		REQUIRED = -(ONHAND + ALLOC + ONORDER - COMPONENT(I%)::QUANTITY)
		REQUIRED = COMPONENT(I%)::QUANTITY IF COMPONENT(I%)::QUANTITY<REQUIRED
		REQUIRED = 0.0 IF REQUIRED < 0.0

		!
		! Print out part
		!
		TEXT$ = TEST_PRODUCT(1%) + "  "  + &
			COMPONENT(I%)::PROD_TYPE + "   " + &
			COMPONENT(I%)::NUMBER + " " + &
			LEFT(PD_PRODUCT_EXAM::DESCRIPTION, 25%) + " " + &
			PD_PRODUCT_EXAM::UOM + &
			FORMAT$(COMPONENT(I%)::QUANTITY, &
				"#,###,###.##") + &
			FORMAT$(ONHAND, "####,###.##") + &
			FORMAT$(-ALLOC, "####,###.##") + &
			FORMAT$(ONORDER, "####,###.##") + &
			FORMAT$(REQUIRED, "<%>###,###.##") + &
			FORMAT$(TOTAL_COMPONENT, "###,###.###")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	NEXT I%

	IF ITEM_LOOP%
	THEN
		TEXT$ = TEST_PRODUCT(1%) + SPACE$(89%) + &
			"TOTAL" + SPACE$(10%) + &
			FORMAT$(TOTAL_PRODUCT, "#,###,###.###*")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3%)
	END IF

	GOTO ExitProgram IF UTL_REPORTX::STAT

17350	!
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

	!
	! If type is not listed in BM control file, leave it off
	! of the report
	!
	GOTO Ret18010 IF COMP_STRING(EDIT$( &
		PD_PRODUCT_EXAM::PROD_TYPE, -1%), BM_CONTROL::PRODTYPE) = 0% &
		AND BM_CONTROL::PRODTYPE <> ""

	ITEM_LOOP% = ITEM_LOOP% + 1%
	COMPONENT(ITEM_LOOP%)::NUMBER = BM_RELATION::COMPONENT
	COMPONENT(ITEM_LOOP%)::PROD_TYPE = PD_PRODUCT_EXAM::PROD_TYPE
	COMPONENT(ITEM_LOOP%)::QUANTITY = QTY_LEVEL(LEVEL%)

 Ret18010:
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
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
