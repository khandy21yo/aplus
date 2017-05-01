1	%TITLE "Product Indented Structure"
	%SBTTL "BM_RPRT_INDENTED"
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
	! ID:BM002
	!
	! Abstract:HELP
	!	.B
	!	.LM +5
	!	This program prints out the product indented structure
	!	in the Bill of Materials System. Included in this report
	!	are the following fields:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	Product Number
	!	.le
	!	Product Description
	!	.le
	!	Product Level
	!	.le
	!	Item
	!	.le
	!	Product Type
	!	.le
	!	Product Category
	!	.le
	!	Product Unit of Measure
	!	.le
	!	Category Description
	!	.le
	!	Category Unit of Measure
	!	.le
	!	Quantity
	!	.le
	!	Operation
	!	.le
	!	Scrap Percentage
	!	.els
	!	.LM -5
	!
	! Index:
	!	.x Product Indented Structure Report
	!	.x Report>Product Indented Structure
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS BM_SOURCE:BM_RPRT_INDENTED/LINE
	!	$ LINK/EXE=BM_EXE: BM_RPRT_INDENTED, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE BM_RPRT_INDENTED.OBJ;*
	!
	! Author:
	!
	!	10/09/88 - Frank F. Starman
	!
	! Modification History:
	!
	!	08/27/91 - JEFF BEARD
	!		MODIFIED TO SHOW INDENTION OF THE TREE
	!
	!	03/12/92 - Kevin Handy
	!		Removed duplicate error trapping (check)
	!
	!	06/24/92 - Frank F. Starman
	!		Use BOMUOM instead just UOM.
	!
	!	11/03/92 - Dan Perkins
	!		Added product Secondary Code to sort order.
	!		Added option to page on new parent product.
	!		Cleaned program code.
	!
	!	11/17/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	11/06/93 - Frank F. Starman
	!		Display cost.
	!
	!	01/10/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	02/01/95 - Kevin Handy
	!		Fixed bug where case had twp "P" cases, instead
	!		of a "P" and a "S" case.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	02/07/96 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/26/97 - Kevin Handy
	!		Lose unecessary definitions
	!		Align title with printout
	!		Add 'label' column to report. (LL)
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/05/2000 - Kevin Handy
	!		Use WHEN ERROR IN
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

	%INCLUDE "SOURCE:[BM.OPEN]BM_RELATION.HB"
	MAP (BM_RELATION)	BM_RELATION_CDD		BM_RELATION

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	DIM RFA RFA_LEVEL(500%)
	DIM REAL QTY_LEVEL(500%)
	DIM STRING TEST_PRODUCT(500%)

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION PD_EXAM_PRODUCT
	EXTERNAL REAL	FUNCTION PC_READ_COST

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
	!	.x From Item>Product Indented Structure Report
	!	.x Product Indented Structure Report>From Item
	!	.x Item>From
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
	!	item in this field.  The value entered must be in agreement with
	!	the value in field (10) Sort by.
	!	.b
	!	A blank setting will cause the report to end with the
	!	last item in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Product Indented Structure Report
	!	.x Product Indented Structure Report>To Item
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
	!	.x Wildcard>Product Indented Structure Report
	!	.x Product Indented Structure Report>Wildcard
	!
	!--

	WLDLEVEL$ = EDIT$(UTL_REPORTX::OPTDEF(6%), -1%)

	!++
	! Abstract:FLD07
	!	^*(07) Level(s)\*
	!	.b
	!	.lm +5
	!	The ^*Level(s)\* option prints the
	!	report showing only desired levels.
	!	.b
	!	A blank setting will cause the report to print
	!	all levels.
	!	.b
	!	Wildcarding may also be used in this field to select
	!	designated levels.
	!	.lm -5
	!
	! Index:
	!	.x Levels>Product Indented Structure Report
	!	.x Product Indented Structure Report>Levels
	!
	!--

	PARENT_PAGE$ = EDIT$(UTL_REPORTX::OPTDEF(7%), -1%)

	!++
	! Abstract:FLD08
	!	^*(08) Page on New Parent\*
	!	.B
	!	.LM +5
	!	The ^*Page on New Parent\* field entry indicates whether
	!	or not a new page will be printed for each parent.
	!	.B
	!	Valid codes are:
	!	.TABLE 3,25
	!	.te
	!	^*Y\* = Yes
	!	.te
	!	^*N\* = No
	!	.end table
	!	An entry is required in this field.
	!	.LM -5
	!
	! Index:
	!
	!--

	IF PARENT_PAGE$ = "Y"
	THEN
		LIN% = 999%
	ELSE
		LIN% = 0%
	END IF

	COST_FLAG$ = EDIT$(UTL_REPORTX::OPTDEF(8%), -1%)

	!++
	! Abstract:FLD09
	!	^*(09) Display Cost\*
	!	.B
	!	.LM +5
	!	The ^*Display Cost\* field determines whether
	!	or not the report should print cost for a component.
	!	.B
	!	Valid codes are:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	^*Y\* = Yes
	!	.le
	!	^*N\* = No
	!	.els
	!	.lm -5
	!	An entry is required in this field.
	!	.LM -5
	!
	! Index:
	!
	!--

	SORT_BY$ = EDIT$(UTL_REPORTX::OPTDEF(9%), 132%)

	!++
	! Abstract:FLD10
	!	^*(10) Sort\*
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
	!	.x Sort>Product Indented Structure Report
	!	.x Product Indented Structure Report>Sort
	!
	!--

	SELECT SORT_BY$

	CASE "C"
		SORT_KEY% = 2%
		ADD_TITLE$ = "BY  PRODUCT  CATEGORY"

	CASE "D"
		SORT_KEY% = 3%
		ADD_TITLE$ = "BY  PRODUCT  DESCRIPTION"

	CASE "P"
		SORT_KEY% = 0%
		ADD_TITLE$ = "BY  PRODUCT  NUMBER"

	CASE "S"
		SORT_KEY% = 4%
		ADD_TITLE$ = "BY  PRODUCT  SECONDARY  CODE"

	CASE "T"
		SORT_KEY% = 1%
		ADD_TITLE$ = "BY  PRODUCT  TYPE"
	END SELECT

	MAX_LEVEL% = 500%
	GOTO OpenFile IF WLDLEVEL$ = ""

	LEVEL$ = WLDLEVEL$
	MAX_LEVEL% = 0%
	LEVEL$ = LEVEL$ + ","

200	WHILE LEVEL$ <> ""
		COMMA% = INSTR(1%, LEVEL$, ",")
		TEST_LEVEL$ = LEVEL$
		LEVEL$ = RIGHT(LEVEL$, COMMA% + 1%)
		WHEN ERROR IN
			TEST_LEVEL% = VAL%(LEFT(TEST_LEVEL$, COMMA% - 1%))
		USE
			TEST_LEVEL% = 0%
		END WHEN
		MAX_LEVEL% = TEST_LEVEL% IF MAX_LEVEL% < TEST_LEVEL%
	NEXT

 OpenFile:

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

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "PRODUCT  INDENTED  TREE  " + ADD_TITLE$
	TITLE$(2%) = "Bill of Material System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "Product#       Description                   " + &
		"           Type Cat  SecCode    UOM"

	TITLE$(5%) = SPACE$(15%) + "Level  Item Type Component#    " + &
		"Description                            Label UOM  " + &
		"Quantity Operation Scrap    ExtCost"

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

		IF TEST_CATEGORY$ <> PD_PRODUCT::CATEGORY AND &
			TEST_CATEGORY$ <>"" AND PRINT_LINE%
		THEN
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 2%)
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
		GOTO ExitProgram &
			IF (PD_PRODUCT::SECONDARY_CODE > TO_ITEM$) AND &
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
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 2%)
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
		FILENAME$  = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

	TEXT_TEXT$ = BM_RELATION::PRODUCT  + " " + &
		PD_PRODUCT::DESCRIPTION + " " + &
		PD_PRODUCT::PROD_TYPE + "   " + &
		PD_PRODUCT::CATEGORY + " " + &
		PD_PRODUCT::SECONDARY_CODE + " " + &
		PD_PRODUCT::BOMUOM

	TEXT_TEXT% = 0%
	QTY_LEVEL(0%), LEVEL% = 1%

 GoDownTree:
	GOTO GoUpTree IF LEVEL% > MAX_LEVEL%

	TEST_PRODUCT(LEVEL%) = BM_RELATION::PRODUCT
	QTY_LEVEL(LEVEL%) = QTY_LEVEL(LEVEL% - 1%) * BM_RELATION::QUANTITY
	RFA_LEVEL(LEVEL%) = GETRFA(BM_RELATION.CH%)

	IF COMP_ARRAY(NUM1$(LEVEL%), WLDLEVEL$) <> 0% OR &
		EDIT$(WLDLEVEL$, -1%) = ""
	THEN
		V% = PD_EXAM_PRODUCT(BM_RELATION::COMPONENT, PD_PRODUCT_EXAM)

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT_TEXT$, LIN%) &
			IF TEXT_TEXT% = 0%

		COMPONENT$ = SPACE$(LEVEL% - 1%) + BM_RELATION::COMPONENT

		DESCRIPTION$ = SEG$(PD_PRODUCT_EXAM::DESCRIPTION, 1%, &
			LEN(PD_PRODUCT_EXAM::DESCRIPTION) - (LEVEL% - 1%))

		TEXT$ = TEST_PRODUCT(1%) + SPACE$(2%) + &
			FORMAT$(LEVEL%,	"####") + "  "  + &
			BM_RELATION::ITEMNUM + " " + &
			PD_PRODUCT_EXAM::PROD_TYPE + "  " + &
			COMPONENT$ + " " + &
			DESCRIPTION$ + " " + &
			PD_PRODUCT_EXAM::LABEL + " " + &
			PD_PRODUCT_EXAM::BOMUOM + " " + &
			FORMAT$(QTY_LEVEL(LEVEL%), "#,###.###") + " " + &
			BM_RELATION::OPERATION + " " + &
			FORMAT$(0.01 * BM_RELATION::SCRAP, "<%>##.##")

		IF COST_FLAG$ = "Y"
		THEN
			PROD_COST = PC_READ_COST(BM_RELATION::COMPONENT, &
				"????", "", "") * QTY_LEVEL(LEVEL%)
			TEXT$ = TEXT$ + FORMAT$(PROD_COST, " ##,###.###")
		END IF

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		TEXT_TEXT% = -1%
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

	!
	! Try for next record
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%) IF TEXT_TEXT%

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
