1	%TITLE "Product Indented Labor"
	%SBTTL "BM_RPRT_INDLABOR"
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
	! ID:BM006
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This program prints out product indented Labor in the Bill of
	!	Materials System. Included in this report are the following fields:
	!	.b
	!	.lm 15
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
	!	Product Unit of Measure
	!	.le
	!	Item Number
	!	.le
	!	Component Unit of Measure
	!	.le
	!	Quantity
	!	.le
	!	Operation
	!	.le
	!	Labor Hours
	!	.le
	!	Extended Labor Hours
	!	.le
	!	Extended Labor Cost
	!	.els
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS BM_SOURCE:BM_RPRT_INDLABOR/LINE
	!	$ LINK/EXE=BM_EXE: BM_RPRT_INDLABOR, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE BM_RPRT_INDLABOR.OBJ;*
	!
	! Author:
	!
	!	08/07/92 - Dan Perkins
	!
	! Modification History:
	!
	!	08/14/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	09/16/92 - Dan Perkins
	!		Changed STAR$ = "*" to MATCH$ = "?" in order
	!		not to confuse with subtotals.
	!
	!	10/04/93 - Kevin Handy
	!		Changed "=>" to >=".
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	05/15/95 - Kevin Handy
	!		Fix -- at end of modification section.
	!		Reformat. Lose excess externals.
	!
	!	09/03/96 - Kevin Handy
	!		Clean up (check)
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/19/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!		Add a REGARDLESS clause
	!		Change variable MAX.LEVEL% to a constant MAX.LEVEL
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	%INCLUDE "SOURCE:[BM.OPEN]BM_RELATION.HB"
	MAP (BM_RELATION)	BM_RELATION_CDD		BM_RELATION

	%INCLUDE "SOURCE:[BM.OPEN]BM_PRODOPER.HB"
	DECLARE			BM_PRODOPER_CDD		BM_PRODOPER_READ

	%INCLUDE "SOURCE:[PR.OPEN]PR_OPER.HB"
	DECLARE			PR_OPER_CDD		PR_OPER_READ

	!
	! Set constants, and dimension arrays
	!
	DECLARE INTEGER CONSTANT MAX.LEVEL = 1%

	DIM	RFA	RFA_LEVEL(MAX.LEVEL)
	DIM	REAL	QTY_LEVEL(MAX.LEVEL)
	DIM	STRING	TEST_PRODUCT(MAX.LEVEL)

	DECLARE  LONG	EXIT_STATUS

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION BM_READ_PRODOPER
	EXTERNAL LONG    FUNCTION PD_EXAM_PRODUCT
	EXTERNAL LONG    FUNCTION PR_READ_OPERATION

	%PAGE

	ON ERROR GOTO 19000

	!
 Init:	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	SORT_BY$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)

	!++
	! Abstract:FLD01
	!	^*(01) Sort\*
	!	.B
	!	.LM +5
	!	The ^*Sort\* field selects the order
	!	the report will print in.
	!	.B
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
	!	.LM -5
	!
	! Index:
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.B
	!	.LM +5
	!	The ^*From Item\* field begins the report
	!	with a selected item by entering the selection in this field.  The value
	!	entered must be in agreement with the value in field (01) Sort by.
	!	.B
	!	A blank field will cause the report to begin with the first
	!	item in the file.
	!	.LM -5
	!
	! Index:
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.B
	!	.LM +5
	!	The ^*To Item\* field enters an item
	!	with which the report will end by entering the selected
	!	item in this field.  The value entered must be in agreement with
	!	the value in field (01) Sort by.
	!	.B
	!	A blank setting will cause the report to end with the
	!	last item in the file.
	!	.LM -5
	!
	! Index:
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.B
	!	.LM +5
	!	The ^*Wildcard\* field selects designated
	!	items to be printed on the report by entering a "Wildcard"
	!	selection in this field.
	!	.LM -5
	!
	! Index:
	!
	!--

	EFF_DATE$ = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(5%), 132%))
	EFF_DATE$ = DATE_TODAY IF EFF_DATE$ = ""

	!++
	! Abstract:FLD06
	!	^*(06) Effective Date\*
	!	.B
	!	.LM +5
	!	The ^*Effective Date\* field enters a date on which
	!	the labor cost is calculated.
	!	.B
	!	This date field will default to today's date.  The date may be overridden
	!	by entering the correct date and pressing ^*Return\*.
	!	.lm -5
	!
	! Index:
	!
	!--

	PARENT_ONLY$ = EDIT$(UTL_REPORTX::OPTDEF(6%), -1%)

	!++
	! Abstract:FLD07
	!	^*(07) Parent Labor Only (Y/N)\*
	!	.B
	!	.LM +5
	!	The ^*Parent Labor Only\* field selects
	!	if only the parent product and its labor operations will be
	!	printed.
	!	.B
	!	Typing a ^*Y\* in this filed means only the parent will be
	!	listed.  Selecting an ^*N\* means a full report will be printed.
	!	.LM -5
	!
	! Index:
	!
	!--

	ZERO_ONLY$ = EDIT$(UTL_REPORTX::OPTDEF(7%), -1%)

	!++
	! Abstract:FLD08
	!	^*(08) Zero Labor Only (Y/N)\*
	!	.B
	!	.LM +5
	!	The ^*Zero Labor Only\* field selects
	!	if only operations with zero labor will be printed.
	!	.B
	!	Typing a ^*Y\* in this filed means only zero labor will be
	!	listed.  Selecting ^*N\* will indicate a full report is to be printed.
	!	.LM -5
	!
	! Index:
	!
	!--

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[BM.OPEN]BM_RELATION.OPN"
	USE
		FILENAME$ = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	SELECT SORT_BY$
	CASE "C"
		SORT_KEY% = 2%
		ADD_TITLE$ = "BY CATEGORY"

	CASE "D"
		SORT_KEY% = 3%
		ADD_TITLE$ = "BY DESCRIPTION"

	CASE "P"
		SORT_KEY% = 0%
		ADD_TITLE$ = "BY PRODUCT NUMBER"

	CASE "S"
		SORT_KEY% = 4%
		ADD_TITLE$ = "BY PRODUCT SECONDARY CODE"

	CASE "T"
		SORT_KEY% = 1%
		ADD_TITLE$ = "BY PRODUCT TYPE"
	END SELECT

	TITLE$(1%) = "PRODUCT INDENTED LABOR TREE " + ADD_TITLE$
	TITLE$(2%) = "Bill of Material System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "Product#       Description                   " + &
		"           Type Cat  SecCode    UOM"

	TITLE$(5%) = SPACE$(19%) + "Item Type Component#     Description" + &
		"               UOM    Quantity Operation"           + &
		"    LBHours ExtLBHours     LBCost"

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
		FILENAME$  = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO GetNextRec IF PD_PRODUCT::SSTATUS <> "A"

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
		GOTO ExitProgram IF (PD_PRODUCT::PROD_TYPE> TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(PD_PRODUCT::PROD_TYPE, -1%), &
			WLDCRD$) = 0%

	END SELECT

	GT_HOURS     = 0.0
	GT_EXTHOURS  = 0.0
	GT_LABORCOST = 0.0

17200	WHEN ERROR IN
		GET #BM_RELATION.CH%, &
			KEY #0% EQ PD_PRODUCT::PRODUCT_NUM, &
			REGARDLESS
	USE
		CONTINUE GetNextRec IF ERR = 155%
		FILENAME$  = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

	OPERATION$ = "        "
	PASSFLAG%  = 0%
	TOT_PRODHOURS     = 0.0
	TOT_PRODLABORCOST = 0.0

	!
	! Check out product operations
	!
 ProductOperation:
	EXIT_STATUS = BM_READ_PRODOPER(BM_RELATION::PRODUCT, &
		OPERATION$, "GT", EFF_DATE$, BM_PRODOPER_READ)

	IF EXIT_STATUS <> CMC$_NORMAL
	THEN
		GOTO EndProductOperation IF PASSFLAG%
		OPERATION$ = "        "
		BM_PRODOPER_READ::HOURS = 0.0
	ELSE
		OPERATION$ = BM_PRODOPER_READ::OPERATION
	END IF

	PR_OPER_READ::HOUR_RATE = 0.0 IF PR_READ_OPERATION( &
		OPERATION$, EFF_DATE$, PR_OPER_READ) <> CMC$_NORMAL

	EXT_PRODLABORCOST = FUNC_ROUND(BM_PRODOPER_READ::HOURS * &
		PR_OPER_READ::HOUR_RATE, 2%)

	IF PASSFLAG% = 0%
	THEN
		TEXT$ = BM_RELATION::PRODUCT + " " + &
			PD_PRODUCT::DESCRIPTION + " " + &
			PD_PRODUCT::PROD_TYPE + "   " + &
			PD_PRODUCT::CATEGORY + " " + &
			PD_PRODUCT::SECONDARY_CODE + " " + &
			PD_PRODUCT::BOMUOM + "       " + &
			OPERATION$ + "  " + &
			FORMAT$(BM_PRODOPER_READ::HOURS, "<%>##,###.##") + " " + &
			FORMAT$(BM_PRODOPER_READ::HOURS, "<%>##,###.##") + "    " + &
			FORMAT$(EXT_PRODLABORCOST, "###,###.##")
	ELSE
		TEXT$ = BM_RELATION::PRODUCT + SPACE$(72%) + &
			OPERATION$ + "  " + &
			FORMAT$(BM_PRODOPER_READ::HOURS, "<%>##,###.##") + " " + &
			FORMAT$(BM_PRODOPER_READ::HOURS, "<%>##,###.##") + "    " + &
			FORMAT$(EXT_PRODLABORCOST, "###,###.##")
	END IF

	TOT_PRODHOURS = TOT_PRODHOURS + BM_PRODOPER_READ::HOURS
	TOT_PRODLABORCOST = TOT_PRODLABORCOST + EXT_PRODLABORCOST

	!
	! Print out header info
	!
	IF ZERO_ONLY$ = "N" OR EXT_PRODLABORCOST = 0.
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	ELSE
		GOTO GetNextRec
	END IF

	IF EXIT_STATUS = CMC$_NORMAL
	THEN
		PASSFLAG%  = -1%
		GOTO ProductOperation
	END IF

 EndProductOperation:
	TEXT$ = BM_RELATION::PRODUCT + SPACE$(44%) + &
		"Product Totals: " + SPACE$(22%) + &
		FORMAT$(TOT_PRODHOURS, "###,###.##") + " " + &
		FORMAT$(TOT_PRODHOURS, "###,###.##") + "    " + &
		FORMAT$(TOT_PRODLABORCOST, "###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)

	IF PARENT_ONLY$ = "Y"
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
		GOTO GetNextRec
	END IF

	QTY_LEVEL(0%), LEVEL% = 1%

 GoDownTree:
	GOTO GoUpTree IF LEVEL% > MAX.LEVEL

	TEST_PRODUCT(LEVEL%) = BM_RELATION::PRODUCT
	QTY_LEVEL(LEVEL%) = QTY_LEVEL(LEVEL% - 1%) * BM_RELATION::QUANTITY
	RFA_LEVEL(LEVEL%) = GETRFA(BM_RELATION.CH%)

	!
	! We can now get line info and print the line
	!
	V% = PD_EXAM_PRODUCT(BM_RELATION::COMPONENT, PD_PRODUCT_EXAM)

	OPERATION$ = "        "
	PASSFLAG%  = 0%

 ReadOperation:
	EXIT_STATUS = BM_READ_PRODOPER(BM_RELATION::COMPONENT, &
		OPERATION$, "GT", EFF_DATE$, BM_PRODOPER_READ)

	IF EXIT_STATUS <> CMC$_NORMAL
	THEN
		GOTO 17320 IF PASSFLAG%
		OPERATION$ = "        "
		BM_PRODOPER_READ::HOURS = 0.0
	ELSE
		OPERATION$ = BM_PRODOPER_READ::OPERATION
	END IF

	EXT.HOURS = FUNC_ROUND(QTY_LEVEL(LEVEL%) * BM_PRODOPER_READ::HOURS, 2%)

	PR_OPER_READ::HOUR_RATE = 0.0 IF PR_READ_OPERATION( &
		OPERATION$, EFF_DATE$, PR_OPER_READ) <> CMC$_NORMAL

	EXT_LABORCOST = FUNC_ROUND(EXT.HOURS * PR_OPER_READ::HOUR_RATE, 2%)

	IF PASSFLAG% = 0%
	THEN
		TEXT$ = TEST_PRODUCT(1%) + "     " + &
			BM_RELATION::ITEMNUM + " " + &
			PD_PRODUCT_EXAM::PROD_TYPE + "   " + &
			BM_RELATION::COMPONENT + " " + &
			LEFT(PD_PRODUCT_EXAM::DESCRIPTION, 25%) + " " + &
			PD_PRODUCT_EXAM::BOMUOM + "  " + &
			FORMAT$(QTY_LEVEL(LEVEL%), "###,###.###") + " " + &
			OPERATION$ + "  " + &
			FORMAT$(BM_PRODOPER_READ::HOURS, "<%>##,###.##") + " " + &
			FORMAT$(EXT.HOURS, "<%>##,###.##") + " " + &
			FORMAT$(EXT_LABORCOST, "###,###.##")
	ELSE
		TEXT$ = TEST_PRODUCT(1%) + "     " + &
			BM_RELATION::ITEMNUM + SPACE$(63%) + &
			OPERATION$ + "  " + &
			FORMAT$(BM_PRODOPER_READ::HOURS, "<%>##,###.##") + " " + &
			FORMAT$(EXT.HOURS, "<%>##,###.##") + " " + &
			FORMAT$(EXT_LABORCOST, "###,###.##")
	END IF

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GT_HOURS     = GT_HOURS + BM_PRODOPER_READ::HOURS
	GT_EXTHOURS  = GT_EXTHOURS + EXT.HOURS
	GT_LABORCOST = GT_LABORCOST + EXT_LABORCOST

	IF EXIT_STATUS = CMC$_NORMAL
	THEN
		PASSFLAG%  = -1%
		GOTO ReadOperation
	END IF

17320	WHEN ERROR IN
		GET #BM_RELATION.CH%, &
			KEY #0% EQ BM_RELATION::COMPONENT, &
			REGARDLESS
	USE
		CONTINUE 17330 IF ERR = 155%
		FILENAME$  = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

	LEVEL% = LEVEL% + 1%
	GOTO GoDownTree

 GoUpTree:
	LEVEL% = LEVEL% - 1%
	GOTO 17350 IF LEVEL% = 0%

17330	WHEN ERROR IN
		GET #BM_RELATION.CH%, RFA RFA_LEVEL(LEVEL%), REGARDLESS

		GET #BM_RELATION.CH%, REGARDLESS
	USE
		CONTINUE GoUpTree IF ERR = 155% OR ERR = 11%
		FILENAME$  = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

	IF BM_RELATION::PRODUCT <> TEST_PRODUCT(LEVEL%)
	THEN
		GOTO GoUpTree
	ELSE
		GOTO GoDownTree
	END IF

17350	GOTO ExitProgram IF UTL_REPORTX::STAT

	IF GT_LABORCOST >= TOT_PRODLABORCOST
	THEN
		MATCH$ = " ??"
	ELSE
		MATCH$ = "   "
	END IF

	TEXT$ = TEST_PRODUCT(1%) + SPACE$(44%) + &
		"Component Totals: " + SPACE$(20%) + &
		FORMAT$(GT_HOURS, "###,###.##") + " " + &
		FORMAT$(GT_EXTHOURS, "###,###.##") + " " + &
		FORMAT$(GT_LABORCOST, "###,###.##") + &
		MATCH$

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

	!
	! Try for next record
	!
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
