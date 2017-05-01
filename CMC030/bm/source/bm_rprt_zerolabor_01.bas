1	%TITLE "Product Cost Structure"
	%SBTTL "BM_RPRT_ZEROLABOR_01"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1996 BY
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
	!	.lm +5
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
	!	If the cost of a product does not match the cost of its components
	!	(plus labor and burden), then the product will be flagged on the right
	!	hand side with two question marks (??).
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS BM_SOURCE:BM_RPRT_ZEROLABOR_01/LINE
	!	$ LINK/EXE=BM_EXE: BM_RPRT_ZEROLABOR_01, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE BM_RPRT_ZEROLABOR_01.OBJ;*
	!
	! Author:
	!
	!	09/04/96 - Kevin Handy
	!		Modified from BM_RPRT_ZEROLABOR.
	!
	! Modification History:
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
	DECLARE		UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[BM.OPEN]BM_PRODOPER.HB"
	MAP (BM_PRODOPER)	BM_PRODOPER_CDD		BM_PRODOPER

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

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
	!	with a selected item.
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
	!	with which the report will end.
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
	!	selection.
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

	! FIXME: EFF_DATE$ doesn't seem to get used for anything

	EFF_DATE$ = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(5%), 132%))
	EFF_DATE$ = DATE_TODAY IF EFF_DATE$ = ""

	!++
	! Abstract:FLD08
	!	^*(08) Effective Date\*
	!	.b
	!	.lm +5
	!	The ^*Effective Date\* field enters date on which
	!	the standard cost is calculated.  The format for entry is MMDDYYYY or MMDDYY.
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
	!	The ^*Sort\* field selects the order
	!	in which the report will print.
	!	.b
	!	Valid settings are:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	^*C\* = Product Category
	!	.le
	!	^*D\* = Product Description
	!	.le
	!	^*P\* = Product Number
	!	.le
	!	^*S\* = Product Secondary Code
	!	.le
	!	^*T\* = Product Type
	!	.els
	!	.lm -5
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

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

360	!

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

	TITLE$(1%) = "ZERO LABOR ITEMS"

	TITLE$(2%) = "Bill of Material System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "Product#       Description                   " + &
		"           Type Cat  SecCode   UOM   " + &
		"Operat   TotHrs   ThsHrs"

	TITLE$(5%) = "."
	TITLE$(6%) = ""

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PD_PRODUCT.CH%, KEY # SORT_KEY%
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
			TEST_PRODTYPE$ <>"" AND PRINT_LINE%
		THEN
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
			PRINT_LINE% = 0%
		END IF

		TEST_PRODTYPE$ = PD_PRODUCT::PROD_TYPE

	END SELECT

17200	WHEN ERROR IN
		GET #BM_PRODOPER.CH%, &
			KEY #0% EQ PD_PRODUCT::PRODUCT_NUM, &
			REGARDLESS
	USE
		CONTINUE GetNextRec IF ERR = 155%
		FILENAME$ = "BM_PRODOPER"
		CONTINUE HelpError
	END WHEN

17220	GOTO 17020 IF BM_PRODOPER::PRODUCT <> PD_PRODUCT::PRODUCT_NUM

	!
	! Get the cost for this product
	!
	IF BM_PRODOPER::THISHOURS = 0.0
	THEN
		TEXT$ = BM_PRODOPER::PRODUCT + " " + &
			PD_PRODUCT::DESCRIPTION + " " + &
			PD_PRODUCT::PROD_TYPE + "   " + &
			PD_PRODUCT::CATEGORY + " " + &
			PD_PRODUCT::SECONDARY_CODE + " " + &
			PD_PRODUCT::BOMUOM + " " + &
			BM_PRODOPER::OPERATION + " " + &
			FORMAT$(BM_PRODOPER::HOURS, "####.### ") + &
			FORMAT$(BM_PRODOPER::THISHOURS, "####.### ")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		PRINT_LINE% = -1%
	END IF

17700	WHEN ERROR IN
		GET #BM_PRODOPER.CH%, REGARDLESS
	USE
		CONTINUE GetNextRec IF ERR = 11%
		FILENAME$ = "BM_PRODOPER"
		CONTINUE HelpError
	END WHEN

	GOTO 17220

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
