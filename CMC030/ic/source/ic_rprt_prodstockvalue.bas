1	%TITLE "Product Stock Value Report"
	%SBTTL "IC_RPRT_PRODSTOCKVALUE"
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
	! ID:IC027
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Product Stock Value Report\*
	!	prints the value of the
	!	inventory depending on the costing method being used. The ^*Product Stock Value
	!	Report\* contains the following fields:
	!	.table 3,25
	!	.te
	!	Product Number	Description
	!	.te
	!	Type	Category
	!	.te
	!	On Hand	Cost Method
	!	.te
	!	Standard Cost	Amount
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Product Stock Value>Report
	!	.x Report>Product Stock Value
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_RPRT_PRODSTOCKVALUE/LINE
	!	$ LINK/EXE=IC_EXE: IC_RPRT_PRODSTOCKVALUE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE IC_RPRT_PRODSTOCKVALUE.OBJ;*
	!
	! Author:
	!
	!	11/26/91 - Dan Perkins
	!
	! Modification History:
	!
	!	01/16/92 - Dan Perkins
	!		Changed quantities to display integer values.
	!
	!	02/04/92 - Kevin Handy
	!		Cleaned out junk (check)
	!
	!	04/01/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	12/04/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!
	!	04/01/96 - Kevin Handy
	!		Reformat goofy source code.
	!
	!	09/05/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	07/30/97 - Kevin Handy
	!		Change XAGE parameter of READ_PERIOD to integer
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	05/05/99 - Kevin Handy
	!		Use FUNC_ROUND instead of ABS to check for zero
	!		stock value.
	!
	!	05/05/99 - Kevin Handy
	!		Handle subtotals better (no more subtotals after
	!		each part number, which didn't really make sense)
	!
	!	11/13/2000 - Kevin Handy
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
	DECLARE		UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.HB"
	MAP	(IC_CONTROL)	IC_CONTROL_CDD		IC_CONTROL

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP	(PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP	(UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	!
	! External functions
	!
	EXTERNAL REAL    FUNCTION PC_READ_COST
	EXTERNAL LONG    FUNCTION IC_READ_35BALANCE

	%PAGE

	ON ERROR GOTO 19000


 Init:	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	SORT_BY$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)

	!++
	! Abstract:FLD01
	!	^*(01) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field prints the
	!	report in a selected order.
	!	.b
	!	Valid values are:
	!	.table 3,25
	!	.te
	!	^*C\* - Product Category
	!	.te
	!	^*D\* - Product Description
	!	.te
	!	^*P\* - Product Number
	!	.te
	!	^*T\* - Product Type
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Sort by>Product Stock Value Report
	!	.x Product Stock Value Report>Sort by
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field causes the
	!	report to begin printing with a selected item number.
	!	The value entered must be in agreement with
	!	field (01) Sort by.
	!	.b
	!	A blank field will cause the report to begin with the first
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Product Stock Value Report
	!	.x Product Stock Value Report>From Item
	!	.x Item>From
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field causes
	!	the report to end printing with a selected item number.
	!	The value entered must be in agreement with
	!	field (01) Sort by.
	!	.b
	!	A blank setting will cause the report to end with the
	!	last item in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Product Stock Value Report
	!	.x Product Stock Value Report>To Item
	!	.x Item>To
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects designated items
	!	to be printed by entering a "wildcard" value.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Product Stock Value Report
	!	.x Product Stock Value Report>Wildcard
	!
	!--

	ZERO_BAL$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Print Zero Balances\*
	!	.b
	!	.lm +5
	!	The ^*Print Zero Balances\* field enters
	!	the option of whether or not to print zero balances.
	!	.b
	!	Valid values are:
	!	.table 3,25
	!	.te
	!	^*Y\* - Yes
	!	.te
	!	^*N\* - N
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Print Zero Balances>Product Stock Value Report
	!	.x Product Stock Value Report>Print Zero Balances
	!
	!--

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.OPN"

		GET #IC_CONTROL.CH%, RECORD 1%, REGARDLESS
		CLOSE IC_CONTROL.CH%
	USE
		FILENAME$ = "IC_CONTROL"
		CONTINUE HelpError
	END WHEN

	V% = READ_PERIOD("READ", IC_CONTROL::ERA, IC_CONTROL::PERIOD, &
		PERIOD_DESCR$, STAT$, START_DATE$, END_DATE$, 0%)

	YYYYPP$ = IC_CONTROL::PERIOD

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

320	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.OPN"
	USE
		FILENAME$ = "UTL_LOCATION"
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

	CASE "T"
		SORT_KEY% = 1%
		ADD_TITLE$ = "BY  PRODUCT  TYPE"

	END SELECT

	TITLE$(1%) = "PRODUCT  INVENTORY  STOCK  VALUE  " + ADD_TITLE$
	TITLE$(2%) = "FOR PERIOD " + YYYYPP$
	TITLE$(3%) = "Inventory Control System"
	TITLE$(4%) = ""

	!
	! Heading
	!
	TITLE$(5%) = "Product#       Description              " + &
		"       Loc  UOM Tp Cat  CostMethod        BegBal     " + &
		"    EndBal          Cost"

	TITLE$(6%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	TEST_SUBTOTAL$ = ""
	GRAND_TOTAL = 0.0
	PRINT_LINE% = 0%

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
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO GetNextRec IF PD_PRODUCT::SSTATUS <> "A"

	SELECT SORT_BY$

	CASE "C"
		GOTO ExitTotal IF (PD_PRODUCT::CATEGORY > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			PD_PRODUCT::CATEGORY, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

		IF PD_PRODUCT::CATEGORY <> TEST_SUBTOTAL$
		THEN
			GOSUB SubTotal

			GRAND_TOTAL = GRAND_TOTAL + TOTAL_COST
			TOTAL_COST = 0.0
			TEST_SUBTOTAL$ = PD_PRODUCT::CATEGORY
		END IF

	CASE "D"
		GOTO ExitTotal IF (PD_PRODUCT::DESCRIPTION > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			PD_PRODUCT::DESCRIPTION, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

		IF PD_PRODUCT::DESCRIPTION <> TEST_SUBTOTAL$
		THEN
			GOSUB SubTotal

			GRAND_TOTAL = GRAND_TOTAL + TOTAL_COST
			TOTAL_COST = 0.0
			TEST_SUBTOTAL$ = PD_PRODUCT::DESCRIPTION
		END IF

	CASE "P"
		GOTO ExitTotal IF (PD_PRODUCT::PRODUCT_NUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			PD_PRODUCT::PRODUCT_NUM, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

		IF PD_PRODUCT::PRODUCT_NUM <> TEST_SUBTOTAL$
		THEN
			GOSUB SubTotal

			GRAND_TOTAL = GRAND_TOTAL + TOTAL_COST
			TOTAL_COST = 0.0
			TEST_SUBTOTAL$ = PD_PRODUCT::PRODUCT_NUM
		END IF

	CASE "T"
		GOTO ExitProgram IF (PD_PRODUCT::PROD_TYPE > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			PD_PRODUCT::PROD_TYPE, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

		IF PD_PRODUCT::PROD_TYPE <> TEST_SUBTOTAL$
		THEN
			GOSUB SubTotal

			GRAND_TOTAL = GRAND_TOTAL + TOTAL_COST
			TOTAL_COST = 0.0
			TEST_SUBTOTAL$ = PD_PRODUCT::PROD_TYPE
		END IF

	END SELECT

	RESET #UTL_LOCATION.CH%

 GetLoc:
17300	WHEN ERROR IN
		GET #UTL_LOCATION.CH%, REGARDLESS
	USE
		CONTINUE GetNextRec IF ERR = 11%
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

	!
	! See if we can find a product for this location
	!
	GOTO GetLoc IF 1% AND IC_READ_35BALANCE(PD_PRODUCT::PRODUCT_NUM, &
		UTL_LOCATION::LOCATION, BALANCE(,)) = 0%

	BEGBAL = BALANCE(1%, 1%)
	ENDBAL = BEGBAL + BALANCE(1%, 2%)

	GOTO GetLoc &
		IF FUNC_ROUND(BEGBAL + ENDBAL, 2%) = 0.0 AND ZERO_BAL$ <> "Y"

	IF PD_PRODUCT::METHOD = "STD"
	THEN
		COST = PC_READ_COST(PD_PRODUCT::PRODUCT_NUM, &
			UTL_LOCATION::LOCATION, END_DATE$, "")
	ELSE
		COST = 0.0
	END IF

	EXTCOST = FUNC_ROUND(COST * ENDBAL, 2%)

	!
	! Print out one line
	!
	TEXT$ = PD_PRODUCT::PRODUCT_NUM + " " + &
		LEFT(PD_PRODUCT::DESCRIPTION, 31%) + " " + &
		UTL_LOCATION::LOCATION + " " + &
		PD_PRODUCT::UOM + "  " + &
		PD_PRODUCT::PROD_TYPE + " " + &
		PD_PRODUCT::CATEGORY + " " + &
		PD_PRODUCT::METHOD + "      " + &
		FORMAT$(BEGBAL, "##,###,###,###") + " " + &
		FORMAT$(ENDBAL, "##,###,###,###") + " " + &
		FORMAT$(EXTCOST, "##,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	PRINT_LINE% = -1%
	TOTAL_COST = TOTAL_COST + EXTCOST

	!
	! Try for next record
	!
	GOTO GetLoc

 SubTotal:
	IF PRINT_LINE%
	THEN
		!
		! Print total cost
		!
		TEXT$ = SPACE$(LEN(PD_PRODUCT::PRODUCT_NUM)) + " " + &
			"T O T A L" + SPACE$(80%) + &
			FORMAT$(TOTAL_COST, "##,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
		PRINT_LINE% = 0%
	END IF

	GRAND_TOTAL = GRAND_TOTAL + TOTAL_COST
	TOTAL_COST = 0.0

	RETURN

 ExitTotal:
	!
	! Finish up subtotals
	!
	GOSUB SubTotal

	!
	! Print total cost
	!
	IF PRINT_LINE%
	THEN
		TEXT$ = SPACE$(LEN(PD_PRODUCT::PRODUCT_NUM)) + " " + &
			"T O T A L" + SPACE$(80%) + &
			FORMAT$(TOTAL_COST, "##,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

		GRAND_TOTAL = GRAND_TOTAL + TOTAL_COST
		PRINT_LINE% = 0%
	END IF

	TEXT$ = SPACE$(LEN(PD_PRODUCT::PRODUCT_NUM)) + " " + &
		"G R A N D  T O T A L" + SPACE$(69%) + &
		FORMAT$(GRAND_TOTAL, "##,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

	TOTAL_COST = 0.0
	GRAND_TOTAL = 0.0

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
