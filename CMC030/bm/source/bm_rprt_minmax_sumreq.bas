1	%TITLE "Bill of Material"
	%SBTTL "BM_RPRT_MINMAX_SUMREQ"
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
	!	$ BAS BM_SOURCE:BM_RPRT_MINMAX_SUMREQ/LINE
	!	$ LINK/EXE=BM_EXE: BM_RPRT_MINMAX_SUMREQ, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE BM_RPRT_MINMAX_SUMREQ.OBJ;*
	!
	! Author:
	!
	!	06/05/2000 - Kevin Handy
	!
	! Modification History:
	!
	!	09/13/2000 - Kevin Handy
	!		Fix MINDOL in the grand totals
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

	%INCLUDE "SOURCE:[BM.OPEN]BM_MAXMIN.HB"
	MAP (BM_MAXMIN)		BM_MAXMIN_CDD		BM_MAXMIN

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

	RECORD TOTAL_RECORD
		STRING PRODUCT = 14%
		STRING PROD_TYPE = 2%
		STRING DESCRIPTION = 25%
		STRING UOM = 2%
		REAL MINQTY
		REAL MAXQTY
		REAL MINDOL
		REAL MAXDOL
	END RECORD

	MAP (TOTAL_MAP) TOTAL_RECORD TOTAL_MAP

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

305	WHEN ERROR IN
		%INCLUDE "SOURCE:[BM.OPEN]BM_MAXMIN.OPN"
	USE
		FILENAME$ = "BM_MAXMIN"
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

330	WHEN ERROR IN
		CALL ASSG_CHANNEL(TEMP.CH%, STAT%)
		OPEN "PR_TOTAL_FILE.DAT" FOR OUTPUT AS FILE TEMP.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP TOTAL_MAP, &
			PRIMARY KEY (TOTAL_MAP::PRODUCT), &
			ACCESS MODIFY, ALLOW NONE
	USE
		FILENAME$ = "PR_TEMP"
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
		" Cat  SecCode    UOM    Min_Qty     Max_Qty" + &
		"   Min_Cost    Max_Cost"

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
			RESET #BM_MAXMIN.CH%, KEY #SORT_KEY%
		ELSE
			FIND #BM_MAXMIN.CH%, &
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
		GET #BM_MAXMIN.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!

	SELECT SORT_BY$
	CASE "G"
		GOTO ExitTotal IF (BM_MAXMIN::MGROUP > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		IF WLDCRD$ <> ""
		THEN
			GOTO GetNextRec &
				IF COMP_ARRAY(EDIT$(BM_MAXMIN::MGROUP, -1%), &
				WLDCRD$) = 0%
		END IF

	CASE "P"
		GOTO ExitTotal IF (BM_MAXMIN::PRODUCT > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		IF WLDCRD$ <> ""
		THEN
			GOTO GetNextRec &
				IF COMP_ARRAY(EDIT$(PD_PRODUCT::PRODUCT_NUM, -1%), &
				WLDCRD$) = 0%
		END IF


	END SELECT

	!
	! Get the product number that goes with this record
	!
	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, KEY #0% EQ BM_MAXMIN::PRODUCT, REGARDLESS
	USE
		PD_PRODUCT::PRODUCT_NUM = BM_MAXMIN::PRODUCT
		PD_PRODUCT::DESCRIPTION = "????????????????????"
	END WHEN

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
		LEFT(PD_PRODUCT::DESCRIPTION, 25%) + " " + &
		PD_PRODUCT::PROD_TYPE + "   " + &
		PD_PRODUCT::CATEGORY + " " + &
		PD_PRODUCT::SECONDARY_CODE + " " + &
		PD_PRODUCT::BOMUOM + &
		FORMAT$(BM_MAXMIN::MINQTY, "#,###,###.##") + &
		FORMAT$(BM_MAXMIN::MAXQTY, "#,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	ITEM_LOOP% = 0%
	LEVEL% = 1%
	QTY_LEVEL(0%) = 1%

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
	TOTAL_PRODUCT_MIN = 0.0
	TOTAL_PRODUCT_MAX = 0.0

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

		TOTAL_PRODUCT_MIN = TOTAL_PRODUCT_MIN + &
			FUNC_ROUND(COMPONENT(I%)::QUANTITY * COST * &
			BM_MAXMIN::MINQTY, 3%)

		TOTAL_PRODUCT_MAX = TOTAL_PRODUCT_MAX + &
			FUNC_ROUND(COMPONENT(I%)::QUANTITY * COST * &
			BM_MAXMIN::MINQTY, 3%)

		!
		! Print out part
		!
		TEXT$ = TEST_PRODUCT(1%) + "  "  + &
			COMPONENT(I%)::PROD_TYPE + "   " + &
			COMPONENT(I%)::NUMBER + " " + &
			LEFT(PD_PRODUCT_EXAM::DESCRIPTION, 25%) + " " + &
			PD_PRODUCT_EXAM::UOM + &
			FORMAT$(COMPONENT(I%)::QUANTITY * BM_MAXMIN::MINQTY, &
				"#,###,###.##") + &
			FORMAT$(COMPONENT(I%)::QUANTITY * BM_MAXMIN::MAXQTY, &
				"#,###,###.##") + &
			FORMAT$(TOTAL_COMPONENT * BM_MAXMIN::MINQTY, &
				"###,###.###") + &
			FORMAT$(TOTAL_COMPONENT * BM_MAXMIN::MAXQTY, &
				"###,###.###")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		WHEN ERROR IN
			GET #TEMP.CH%, KEY #0% EQ COMPONENT(I%)::NUMBER
		USE
			CONTINUE 17340
		END WHEN

		TOTAL_MAP::MINQTY = FUNC_ROUND(TOTAL_MAP::MINQTY + &
			COMPONENT(I%)::QUANTITY * BM_MAXMIN::MINQTY, 2%)
		TOTAL_MAP::MAXQTY = FUNC_ROUND(TOTAL_MAP::MAXQTY + &
			COMPONENT(I%)::QUANTITY * BM_MAXMIN::MAXQTY, 2%)
		TOTAL_MAP::MINDOL = FUNC_ROUND(TOTAL_MAP::MINDOL + &
			TOTAL_COMPONENT * BM_MAXMIN::MINQTY, 2%)
		TOTAL_MAP::MAXDOL = FUNC_ROUND(TOTAL_MAP::MAXDOL + &
			TOTAL_COMPONENT * BM_MAXMIN::MAXQTY, 2%)

		UPDATE #TEMP.CH%

		GOTO 17345

17340		TOTAL_MAP::PRODUCT = COMPONENT(I%)::NUMBER
		TOTAL_MAP::PROD_TYPE = COMPONENT(I%)::PROD_TYPE
		TOTAL_MAP::DESCRIPTION = PD_PRODUCT_EXAM::DESCRIPTION
		TOTAL_MAP::UOM = PD_PRODUCT_EXAM::UOM
		TOTAL_MAP::MINQTY = COMPONENT(I%)::QUANTITY * BM_MAXMIN::MINQTY
		TOTAL_MAP::MAXQTY = COMPONENT(I%)::QUANTITY * BM_MAXMIN::MAXQTY
		TOTAL_MAP::MINDOL = TOTAL_COMPONENT * BM_MAXMIN::MINQTY
		TOTAL_MAP::MAXDOL = TOTAL_COMPONENT * BM_MAXMIN::MAXQTY

		PUT #TEMP.CH%

17345	NEXT I%

	IF ITEM_LOOP%
	THEN
		TEXT$ = TEST_PRODUCT(1%) + SPACE$(55%) + &
			"TOTAL" + SPACE$(12%) + &
			FORMAT$(TOTAL_PRODUCT_MIN, "#,###,###.###*") + &
			FORMAT$(TOTAL_PRODUCT_MAX, "##,###.###*")

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
		CONTINUE ExitTotal IF ERR = 155%
		FILENAME$ = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

	GOTO GetNextRec

 ExitTotal:

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 6%)
	TEXT$ = "Summarized Totals"
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	RESET #TEMP.CH%

17410	WHEN ERROR IN
		GET #TEMP.CH%
	USE
		CONTINUE 17490
	END WHEN

	TEXT$ = SPACE$(14%) + "  "  + &
		TOTAL_MAP::PROD_TYPE + "   " + &
		TOTAL_MAP::PRODUCT + " " + &
		LEFT(TOTAL_MAP::DESCRIPTION, 25%) + " " + &
		TOTAL_MAP::UOM + &
		FORMAT$(TOTAL_MAP::MINQTY, "#,###,###.##") + &
		FORMAT$(TOTAL_MAP::MAXQTY, "#,###,###.##") + &
		FORMAT$(TOTAL_MAP::MINDOL, "###,###.###") + &
		FORMAT$(TOTAL_MAP::MAXDOL, "###,###.###")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO 17410

17490	!

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
	!+-+-+
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
