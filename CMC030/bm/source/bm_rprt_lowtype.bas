1	%TITLE "Product Lowest Level by Type"
	%SBTTL "BM_RPRT_LOWTYPE"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1986, 1988 BY
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
	! ID:BM001
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This program prints out the Product lowest level structure
	!	by type in the Bill of Materials System. This report includes
	!	the following fields:
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
	!	Quantity
	!	.le
	!	Cost
	!	.le
	!	Extended Cost
	!	.els
	!	.lm -5
	!
	! Index:
	!	.x Product Lowest Level by Type Report
	!	.x Report>Product Lowest Level by Type
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS BM_SOURCE:BM_RPRT_LOWTYPE/LINE
	!	$ LINK/EXE=BM_EXE: BM_RPRT_LOWTYPE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE BM_RPRT_LOWTYPE.OBJ;*
	!
	! Author:
	!
	!	10/09/88 - Frank F. Starman
	!
	! Modification History:
	!
	!	03/12/92 - Kevin Handy
	!		Removed duplicate error trapping (check)
	!
	!	06/23/92 - Dan Perkins
	!		Added sort by SECONDARY_CODE.  Cleaned code.
	!
	!	06/24/92 - Frank F. Starman
	!		Use BOMUOM instead just UOM.
	!
	!	07/01/92 - Kevin Handy
	!		Clean up (check)
	!
	!	09/30/92 - Dan Perkins
	!		Get Component Type from Control file instead of
	!		input from user.
	!
	!	10/26/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	11/02/92 - Dan Perkins
	!		Added four decimal places to qty, cost, and ext cost
	!		fields.  Changed FUNC_ROUND to round to four decimal
	!		places.  Added option to page when printing new parent.
	!
	!	11/17/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	09/09/94 - Kevin Handy
	!		Modifications to display the correct description and
	!		unit of measure for the components.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/03/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/14/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	10/30/2000 - Kevin Handy
	!		Use A"x"B
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

	%INCLUDE "SOURCE:[BM.OPEN]BM_CONTROL.HB"
	MAP (BM_CONTROL)	BM_CONTROL_CDD		BM_CONTROL

	%INCLUDE "SOURCE:[BM.OPEN]BM_RELATION.HB"
	MAP (BM_RELATION)	BM_RELATION_CDD		BM_RELATION

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODTYPE.HB"
	MAP (PD_PRODTYPE)	PD_PRODTYPE_CDD		PD_PRODTYPE

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
	!	.x From Item>Product Lowest Level by Type Report
	!	.x Product Lowest Level by Type Report>From Item
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
	!	The value entered must be in agreement with the value in field
	!	(10) Sort by.
	!	.b
	!	A blank setting will cause the report to end with the
	!	last item in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Product Lowest Level by Type Report
	!	.x Product Lowest Level by Type Report>To Item
	!	.x Item>To
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
	!	.x Wildcard>Product Lowest Level by Type
	!	.x Product Lowest Level by Type>Wildcard
	!
	!--

	LOCATION$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Location _#\*
	!	.b
	!	.lm +5
	!	The ^*Location _#\* field enters a location
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
	!	.x Location Number>Product Lowest Level by Type Report
	!	.x Number>Location
	!	.x Product Lowest Level by Type Report>Location Number
	!
	!--

	!TYPEWLD$ = EDIT$(UTL_REPORTX::OPTDEF(6%), -1%)

	!++
	! Abstract:FLD07
	!	^*(07) Component Types\*
	!	.b
	!	.lm +5
	!	The ^*Component Type\* field enters the component
	!	type which will be printed on the report.  Wildcarding techniques may also
	!	be used in this field.
	!	.lm -5
	!
	! Index:
	!	.x Component Types
	!	.x Types>Component
	!
	!--

	PARENT_PAGE$ = EDIT$(UTL_REPORTX::OPTDEF(7%), -1%)

	!++
	! Abstract:FLD08
	!	^*(08) Page on New Parent\*
	!	.b
	!	.lm +5
	!	The ^*Page on New Parent\* field indicates whether
	!	a new page is to be printed for each parent.
	!	.b
	!	Valid codes are:
	!	.table 3,25
	!	.te
	!	^*Y\* = Yes
	!	.te
	!	^*N\* = No
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

	DISPLAY_COST$ = EDIT$(UTL_REPORTX::OPTDEF(8%), 132%)

	!++
	! Abstract:FLD09
	!	^*(09) Cost Display (Y,N)\*
	!	.b
	!	.lm +5
	!	The ^*Cost Display\* field enters a code
	!	to indicate whether the report is to contain the standard
	!	cost.
	!	.b
	!	Valid codes are:
	!	.table 3,25
	!	.te
	!	^*Y\* = Yes
	!	.te
	!	^*N\* = No
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Cost Display>Product Lowest Level by Type Report
	!	.x Product Lowest Level by Type Report>Cost Display
	!	.x Display>Cost
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
	!	.x Sort>Product Lowest Level by Type Report
	!	.x Product Lowest Level by Type Report>Sort
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
		CONTINUE 340 IF ERR = 5%
		FILENAME$ = "BM_CONTROL"
		CONTINUE HelpError
	END WHEN

340	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODTYPE.OPN"
	USE
		CONTINUE ReportTitle IF ERR = 5%
		FILENAME$ = "PD_PRODTYPE"
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
	TITLE$(4%) = "Product#       Description                   " + &
		"           Type Cat  SecCode    UOM"

	SELECT DISPLAY_COST$

	CASE "Y"
		TITLE$(5%) = SPACE$(26%) + "Type Component#     " + &
			"Description                         UOM     "  + &
			" Quantity         Cost         ExtCost"

	CASE "N"
		TITLE$(5%) = SPACE$(26%) + "Type Component#     " + &
			"Description                         UOM     "  + &
			" Quantity"

	END SELECT

	TITLE$(6%) = "."

	LIN% = 0%

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
			TEST_PRODTYPE$ <> "" AND PRINT_LINE%
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

17300	!
	! Print out Product
	!
	PD_PRODUCT$ = BM_RELATION::PRODUCT

	TEXT$ = BM_RELATION::PRODUCT + " " + &
		PD_PRODUCT::DESCRIPTION + " " + &
		PD_PRODUCT::PROD_TYPE + "   " + &
		PD_PRODUCT::CATEGORY + " " + &
		PD_PRODUCT::SECONDARY_CODE + " " + &
		PD_PRODUCT::BOMUOM

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, LIN%)

	LIN% = 999% IF PARENT_PAGE$ = "Y"

	ITEM_LOOP% = 0%
	QTY_LEVEL(0%), LEVEL% = 1%

 GoDownTree:
	TEST_PRODUCT(LEVEL%) = BM_RELATION::PRODUCT
	QTY_LEVEL(LEVEL%) = QTY_LEVEL(LEVEL% - 1%) * BM_RELATION::QUANTITY
	RFA_LEVEL(LEVEL%) = GETRFA(BM_RELATION.CH%)

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
	GOTO PrintComponents IF LEVEL% - 1% = 0%

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
	TOTAL_TYPE, TOTAL_PRODUCT = 0.0

	!
	! Print array
	!
	FOR I% = 1% TO ITEM_LOOP%

		PD_PRODUCT$ = COMPONENT(I%)::NUMBER

		SELECT DISPLAY_COST$

		CASE "Y"
			GOSUB 18700 IF COMPONENT(I%)::PROD_TYPE <> &
				COMPONENT(I% - 1%)::PROD_TYPE

			!
			! Read Product File
			!
			V% = PD_EXAM_PRODUCT(PD_PRODUCT$, PD_PRODUCT_EXAM)

			PD_PRODUCT_EXAM::PRODUCT_FACTOR = 1.0 &
				IF PD_PRODUCT_EXAM::PRODUCT_FACTOR = 0.0

			COST = PC_READ_COST(PD_PRODUCT$, LOCATION$, "", "")
			COST = COST / PD_PRODUCT_EXAM::PRODUCT_FACTOR

			TOTAL_COMPONENT = FUNC_ROUND(COMPONENT(I%)::QUANTITY * &
				COST, 4%)

			TOTAL_TYPE = TOTAL_TYPE + &
				FUNC_ROUND(COMPONENT(I%)::QUANTITY * COST, 4%)

			TOTAL_PRODUCT = TOTAL_PRODUCT + &
				FUNC_ROUND(COMPONENT(I%)::QUANTITY * COST, 4%)

			TEXT$ = TEST_PRODUCT(1%) + SPACE$(12%) + &
				COMPONENT(I%)::PROD_TYPE + "   " + &
				COMPONENT(I%)::NUMBER + " "   + &
				LEFT(PD_PRODUCT_EXAM::DESCRIPTION, 35%) + " " + &
				PD_PRODUCT_EXAM::BOMUOM + "   " + &
				FORMAT$(COMPONENT(I%)::QUANTITY, &
					"###,###.####") + " "   + &
				FORMAT$(COST, "###,###.####") + "  "  + &
				FORMAT$(TOTAL_COMPONENT, "#,###,###.####")

		CASE "N"
			V% = PD_EXAM_PRODUCT(PD_PRODUCT$, PD_PRODUCT_EXAM)

			TEXT$ = TEST_PRODUCT(1%) + SPACE$(12%) + &
				COMPONENT(I%)::PROD_TYPE + "   " + &
				COMPONENT(I%)::NUMBER + " "   + &
				LEFT(PD_PRODUCT_EXAM::DESCRIPTION, 35%) + " " + &
				PD_PRODUCT_EXAM::BOMUOM + "   " + &
				FORMAT$(COMPONENT(I%)::QUANTITY, &
					"###,###.####")

		END SELECT

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	NEXT I%

	IF DISPLAY_COST$ = "Y" AND ITEM_LOOP%
	THEN
		GOSUB 18700

		TEXT$ = TEST_PRODUCT(1%) + SPACE$(19%) + &
			"TOTAL" + SPACE$(76%) + &
			FORMAT$(TOTAL_PRODUCT, "#,###,###.####*")

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
				FUNC_ROUND(QTY_LEVEL(LEVEL%), 4%)

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

18700	!
	! Read Product Type File
	!
	PD_PRODTYPE::DESCRIPTION = &
		STRING$(LEN(PD_PRODTYPE::DESCRIPTION), A"?"B)

	WHEN ERROR IN
		GET #PD_PRODTYPE.CH%, &
			KEY #0% EQ PD_PRODUCT::PROD_TYPE, &
			REGARDLESS
	USE
		CONTINUE 18710 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PD_PRODTYPE"
		CONTINUE HelpError
	END WHEN

18710	TEXT$ = TEST_PRODUCT(1%) + SPACE$(12%) + &
		PD_PRODUCT::PROD_TYPE + "   " + &
		SPACE$(18%) + &
		PD_PRODTYPE::DESCRIPTION + SPACE$(45%) + &
		FORMAT$(TOTAL_TYPE, "#,###,###.####*")

	TOTAL_TYPE = 0.0

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 2%)

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
