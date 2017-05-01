1	%TITLE "Unused Components Report"
	%SBTTL "BM_RPRT_NOTUSED"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 2004 BY
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
	! ID:BM003
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This program prints out where the components are used. Included in this report
	!	are the following fields:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	Component Number
	!	.le
	!	Component Description
	!	.le
	!	Level
	!	.le
	!	Item
	!	.le
	!	Component Type
	!	.le
	!	Product Number
	!	.le
	!	Product Type
	!	.le
	!	Product Category
	!	.le
	!	Product Description
	!	.le
	!	Component Unit of Measure
	!	.le
	!	Product Unit of Measure
	!	.le
	!	Quantity
	!	.le
	!	Operation
	!	.le
	!	Scrap Percentage
	!	.els
	!	.lm -5
	!
	! Index:
	!	.x Where Used Components Report
	!	.x Report>Where Used Components
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS BM_SOURCE:BM_RPRT_NOTUSED/LINE
	!	$ LINK/EXE=BM_EXE: BM_RPRT_NOTUSED, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE BM_RPRT_NOTUSED.OBJ;*
	!
	! Author:
	!
	!	03/16/2004 - Kevin Handy
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
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[BM.OPEN]BM_RELATION.HB"
	MAP (BM_RELATION)	BM_RELATION_CDD		BM_RELATION

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	DIM RFA RFA_LEVEL(500%)
	DIM REAL QTY_LEVEL(500%)
	DIM STRING TEST_COMPONENT(500%)

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
	!	with the selected item. A blank setting causes the report to begin with
	!	the first item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field ends the report with
	!	the selected item. A blank setting causes the report to end with the last
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects designated items to
	!	be printed by entering a "Wildcard" value in this field.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard
	!
	!--

	MAIN_TYPE$ = TRM$(UTL_REPORTX::OPTDEF(3%))

	!++
	! Abstract:FLD04
	!	^*(04) Main Type\*
	!	.b
	!	.lm +5
	!	Selects the main type which, when found as a parent,
	!	determines that it is not a "not-used" part.
	!	.b
	!	This is NOT a wildcard.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard
	!
	!--

	LIN% = 0%

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
	!	^*T\* = Product Type
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Sort>Where Used Components Report
	!	.x Where Used Components Report>Sort
	!
	!--

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

	CASE "T"
		SORT_KEY% = 1%
		ADD_TITLE$ = "BY  PRODUCT  TYPE"
	END SELECT

	MAX_LEVEL% = 500%

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

320	PD_PRODUCT2.CH% = PD_PRODUCT.CH%
	PD_PRODUCT.CH% = 0%

	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		CONTINUE ReportTitle IF ERR = 5%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "WHERE-USED  COMPONENTS  " + ADD_TITLE$
	TITLE$(2%) = "Bill of Material System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "Component#     Description                   " + &
		"                Type Cat  UOM"

	TITLE$(5%) = SPACE$(25%) + "Level  Item Type Product#       " + &
		"Description                              UOM     " + &
		"Quantity Operation   Scrap"

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
		CALL ENTR_3MESSAGE(SCOPE, &
			"Unable to find beginning record!", 0%)
		CONTINUE ExitProgram
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
	GOTO GetNextRec IF PD_PRODUCT::SSTATUS <> "A"

	SELECT SORT_BY$

	CASE "C"
		GOTO ExitProgram IF (PD_PRODUCT::CATEGORY > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		IF TEST_CATEGORY$ <> PD_PRODUCT::CATEGORY AND &
			TEST_CATEGORY$ <> "" AND PRINT_LINE%
		THEN
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 2%)
			GOTO ExitProgram IF UTL_REPORTX::STAT
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

	CASE "T"
		GOTO ExitProgram IF (PD_PRODUCT::PROD_TYPE> TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(PD_PRODUCT::PROD_TYPE, -1%), &
			WLDCRD$) = 0%

		IF TEST_PRODTYPE$ <> PD_PRODUCT::PROD_TYPE AND &
			TEST_PRODTYPE$ <> "" AND PRINT_LINE%
		THEN
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 2%)
			GOTO ExitProgram IF UTL_REPORTX::STAT
			PRINT_LINE% = 0%
		END IF

		TEST_PRODTYPE$ = PD_PRODUCT::PROD_TYPE

	END SELECT

	GOTO GetNextRec IF PD_PRODUCT::PROD_TYPE = MAIN_TYPE$

	TEXT_TEXT$ = PD_PRODUCT::PRODUCT_NUM + " " + &
		PD_PRODUCT::DESCRIPTION + "      " + &
		PD_PRODUCT::PROD_TYPE + "   " + &
		PD_PRODUCT::CATEGORY + " " + &
		PD_PRODUCT::BOMUOM

17200	WHEN ERROR IN
		GET #BM_RELATION.CH%, &
			KEY #1% EQ PD_PRODUCT::PRODUCT_NUM, &
			REGARDLESS
	USE
		CONTINUE 17350 IF ERR = 155%
		FILENAME$ = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

17300	IC_INGREDIENT$ = BM_RELATION::COMPONENT

	TEXT_TEXT$ = BM_RELATION::COMPONENT + " " + &
		PD_PRODUCT::DESCRIPTION + "      " + &
		PD_PRODUCT::PROD_TYPE + "   " + &
		PD_PRODUCT::CATEGORY + " " + &
		PD_PRODUCT::BOMUOM

	TEXT_TEXT% = 0%
	QTY_LEVEL(0%), LEVEL% = 1%

 GoDownTree:
	GOTO GoUpTree IF LEVEL% > MAX_LEVEL%

	TEST_COMPONENT(LEVEL%) = BM_RELATION::COMPONENT
	QTY_LEVEL(LEVEL%) = QTY_LEVEL(LEVEL% - 1%) * BM_RELATION::QUANTITY
	RFA_LEVEL(LEVEL%) = GETRFA(BM_RELATION.CH%)

	!
	! Skip out if we have found a parent
	!
	IC_INGREDIENT$ = BM_RELATION::PRODUCT
	GOSUB 18500
	GOTO GetNextRec IF PD_PRODUCT::PROD_TYPE = MAIN_TYPE$

17320	WHEN ERROR IN
		GET #BM_RELATION.CH%, &
			KEY #1% EQ BM_RELATION::PRODUCT, &
			REGARDLESS
	USE
		IF ERR = 155% OR ERR = 11%
		THEN
			CONTINUE 17330
		END IF
		FILENAME$ = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

	LEVEL% = LEVEL% + 1%
	GOTO GoDownTree

 GoUpTree:
	!
	LEVEL% = LEVEL% - 1%
	GOTO 17350 IF LEVEL% = 0%

17330	WHEN ERROR IN
		GET #BM_RELATION.CH%, &
			KEY #1% EQ TEST_COMPONENT(LEVEL%), &
			REGARDLESS
	USE
		IF ERR = 155%
		THEN
			CONTINUE GoUpTree
		END IF
		FILENAME$ = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

	RFA_LEVEL(0%) = GETRFA(BM_RELATION.CH%)

	UNTIL RFA_LEVEL(0%) = RFA_LEVEL(LEVEL%)

		WHEN ERROR IN
			GET #BM_RELATION.CH%, REGARDLESS
			RFA_LEVEL(0%) = GETRFA(BM_RELATION.CH%)
		USE
			IF ERR = 11%
			THEN
				CONTINUE GoUpTree
			END IF
			FILENAME$ = "BM_RELATION"
			CONTINUE HelpError
		END WHEN
	NEXT

	WHEN ERROR IN
		GET #BM_RELATION.CH%, REGARDLESS
	USE
		IF ERR = 11%
		THEN
			CONTINUE GoUpTree
		END IF
		FILENAME$ = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

	IF BM_RELATION::COMPONENT <> TEST_COMPONENT(LEVEL%)
	THEN
		GOTO GoUpTree
	ELSE
		GOTO GoDownTree
	END IF

17350	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! If we reach here, we have not found the main type yet.
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT_TEXT$, 0%)

	!
	! Try for next record
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

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

18500	!
	! Read Product File
	!
	WHEN ERROR IN
		GET #PD_PRODUCT2.CH%, KEY #0% EQ IC_INGREDIENT$, REGARDLESS
	USE
		PD_PRODUCT::DESCRIPTION = &
			STRING$(LEN(PD_PRODUCT::DESCRIPTION), A"?"B)

		PD_PRODUCT::PROD_TYPE = &
			STRING$(LEN(PD_PRODUCT::PROD_TYPE), A"?"B)

		PD_PRODUCT::BOMUOM = &
			STRING$(LEN(PD_PRODUCT::BOMUOM), A"?"B)

		CONTINUE 18510 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PD_PRODUCT2"
		CONTINUE HelpError
	END WHEN

18510	RETURN

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
