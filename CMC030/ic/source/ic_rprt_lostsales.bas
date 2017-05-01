1	%TITLE "Product Ordering Report"
	%SBTTL "IC_RPRT_LOSTSALES"
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
	! ID:IC024
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	Lost Sales Report.
	!	.lm -5
	!
	! Index:
	!	.x Lost Sales>Report
	!	.x Report>Lost Sales
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_RPRT_LOSTSALES/LINE
	!	$ LINK/EXE=IC_EXE: IC_RPRT_LOSTSALES, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE IC_RPRT_LOSTSALES.OBJ;*
	!
	! Author:
	!
	!	01/21/94 - Kevin Handy
	!		Taken from IC_RPRT_PRODORDER and extensively
	!		modified.
	!
	! Modification History:
	!
	!	01/25/94 - Kevin Handy
	!		Modified to split "ExitProgram" into "ExitTotal" and
	!		"ExitProgram".
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/05/96 - Kevin Handy
	!		Reformat source code.
	!
	!	11/18/97 - Kevin Handy
	!		Reformat source code
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/29/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.HB"
	MAP (IC_CONTROL)	IC_CONTROL_CDD		IC_CONTROL

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[IC.OPEN]IC_35HISTORY.HB"
	MAP (IC_35HISTORY)	IC_35HISTORY_CDD	IC_35HISTORY
	DECLARE			IC_35HISTORY_CDD	IC_HISTORY(2%)

	!
	! External functions
	!
	EXTERNAL REAL    FUNCTION PC_READ_COST

	!
	! Dimensions
	!
	DIM PRODU_TOTAL(36%)
	DIM GROUP_TOTAL(36%)
	DIM GRAND_TOTAL(36%)
	DIM PRODU_DOLLAR(36%)
	DIM GROUP_DOLLAR(36%)
	DIM GRAND_DOLLAR(36%)

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
	!	Valid entries are:
	!	.table 3,25
	!	.te
	!	^*C\* - Product Category
	!	.te
	!	^*D\* - Product Description
	!	.te
	!	^*P\* - Product Number
	!	.te
	!	^*S\* - Product Secondary Code
	!	.te
	!	^*T\* - Product Type
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Sort by>Product Ordering Report
	!	.x Product Ordering Report>Sort by
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
	!	The value entered must be in agreement with the value in field
	!	(01) Sort by.
	!	.b
	!	A blank field will cause the report to begin with the first
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Product Ordering Report
	!	.x Product Ordering RepoIrt>From Item
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
	!	The value entered must be in agreement with the value in field (01)
	!	Sort by.
	!	.b
	!	A blank setting will cause the report to end with the
	!	last item in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Product Ordering Report
	!	.x Product Ordering Report>To Item
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
	!	.x Wildcard>Product Ordering Report
	!	.x Product Ordering Report>Wildcard
	!
	!--

	LOCATION$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Location\*
	!	.b
	!	.lm +5
	!	The ^*Location\* field selects a designated
	!	product location for which the report will be printed.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Location>Product Ordering Report
	!	.x Product Ordering Report>Location
	!
	!--

	STATUS_WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(5%), -1%)

	!++
	! Abstract:FLD06
	!	^*(06) Product Status Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Product Status Wildcard\* field selects
	!	the status of the product which will be printed
	!	by entering a "wildcard" value in this field.
	!	.lm -5
	!
	! Index:
	!	.x Product Status>Product Ordering Report
	!	.x Product Ordering Report>Product Status
	!
	!--
	FROM_PERIOD$ = EDIT$(UTL_REPORTX::OPTDEF(6%), -1%)

	!++
	! Abstract:FLD07
	!	^*(07) From Period\*
	!	.b
	!	.lm +5
	!	.lm -5
	!
	! Index:
	!
	!--

	TO_PERIOD$ = EDIT$(UTL_REPORTX::OPTDEF(7%), -1%)

	!++
	! Abstract:FLD08
	!	^*(08) To Period\*
	!	.b
	!	.lm +5
	!	.lm -5
	!
	! Index:
	!
	!--

300	!
	! Calculate the period ranges that we will be working in
	!
	FROM_YEAR% = VAL%(LEFT(FROM_PERIOD$, 4%))
	FROM_PERIOD% = VAL%(MID(FROM_PERIOD$, 5%, 2%))
	TO_YEAR% = VAL%(LEFT(TO_PERIOD$, 4%))
	TO_PERIOD% = VAL%(MID(TO_PERIOD$, 5%, 2%))

	TOTAL_YEARS% = TO_YEAR% - FROM_YEAR% + 1%

	FOR I% = 1% TO TOTAL_YEARS%
		STARTPERIOD%(I%) = 1%
		ENDPERIOD%(I%) = 12%
	NEXT I%
	STARTPERIOD%(1%) = FROM_PERIOD%
	ENDPERIOD%(TOTAL_YEARS%) = TO_PERIOD%

	TOTAL_PERIODS% = 0%
	TOTAL_PERIODS% = TOTAL_PERIODS% + &
		(ENDPERIOD%(I%) - STARTPERIOD%(I%) + 1%) &
		FOR I% = 1% TO TOTAL_YEARS%

310	!
	! Open product description file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

320	!
	! Open History files for however many years we need
	!
	FOR Y% = 1% TO TOTAL_YEARS%

		YYYY$ = FORMAT$(FROM_YEAR% + Y% - 1%, "<0>###")
		WHEN ERROR IN
			%INCLUDE "SOURCE:[IC.OPEN]IC_35HISTORY.OPN"
		USE
			FILENAME$ = "IC_35HISTORY"
			CONTINUE HelpError
		END WHEN

		IC_35HISTORY.CH%(Y%) = IC_35HISTORY.CH%
		IC_35HISTORY.CH% = 0.0

	NEXT Y%

 ReportTitle:
330	!
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
		ADD_TITLE$ = "BY  PRODUCT  SECONDARY  CODE"

	CASE "T"
		SORT_KEY% = 1%
		ADD_TITLE$ = "BY  PRODUCT  TYPE"

	END SELECT

	TITLE$(1%) = "LOST  SALES  " + ADD_TITLE$
	TITLE$(2%) = "FOR  LOCATION  " + LOCATION$
	TITLE$(3%) = "Inventory Control System"
	TITLE$(4%) = ""

	!
	! Heading
	!
	TEXT$ = ""
	FOR I% = 1% TO TOTAL_YEARS%

		FOR J% = STARTPERIOD%(I%) TO ENDPERIOD%(I%)

			TEXT$ = TEXT$ + &
				FORMAT$(FROM_YEAR% + I% - 1%, " <0>###") + &
				"_" + &
				FORMAT$(J%, "<0>#")

		NEXT J%

	NEXT I%

	TITLE$(5%) = "Product#       Description          "
	TITLE$(6%) = "      " + TEXT$ + "  Total"
	TITLE$(7%) = "."
	TITLE$(8%) = ""

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
		CONTINUE ExitTotal IF ERR = 155%
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
	GOTO GetNextRec IF COMP_STRING(EDIT$( &
		PD_PRODUCT::SSTATUS, -1%), STATUS_WLDCRD$) = 0% &
		AND STATUS_WLDCRD$ <> ""

	SELECT SORT_BY$

	CASE "C"
		GOTO ExitTotal IF (PD_PRODUCT::CATEGORY > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			PD_PRODUCT::CATEGORY, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "D"
		GOTO ExitTotal IF (PD_PRODUCT::DESCRIPTION > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			PD_PRODUCT::DESCRIPTION, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "P"
		GOTO ExitTotal IF (PD_PRODUCT::PRODUCT_NUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			PD_PRODUCT::PRODUCT_NUM, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "S"
		GOTO ExitTotal IF (PD_PRODUCT::SECONDARY_CODE > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			PD_PRODUCT::SECONDARY_CODE, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "T"
		GOTO ExitTotal IF (PD_PRODUCT::PROD_TYPE > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			PD_PRODUCT::PROD_TYPE, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	END SELECT

	!
	! See if we can get a balance for the product
	!
17040	PERIOD% = 1%

17050	!
	! For each year we are doing
	!
	FOR I% = 1% TO TOTAL_YEARS%

		!
		! Search for 1st matching line for this product
		!
		WHEN ERROR IN
			GET #IC_35HISTORY.CH%(I%), &
				KEY #0% EQ PD_PRODUCT::PRODUCT_NUM + &
				LOCATION$ + &
				"LS", &
				REGARDLESS
		USE
			CONTINUE 17070
		END WHEN

17060		!
		! While we have the correct product
		!
		IF (IC_35HISTORY::PRODUCT = PD_PRODUCT::PRODUCT_NUM) AND &
			(IC_35HISTORY::LOCATION = LOCATION$) AND &
			IC_35HISTORY::TRANSTYPE = "LS"
		THEN
			!
			! Total lost sales by period
			!
			FOR J% = STARTPERIOD%(I%) TO ENDPERIOD%(I%)

				THIS_PERIOD% = PERIOD% + J% - STARTPERIOD%(I%)

				PRODU_TOTAL(THIS_PERIOD%) = &
					PRODU_TOTAL(THIS_PERIOD%) + &
					IC_35HISTORY::PQUANTITY(J%)
				PRODU_DOLLAR(THIS_PERIOD%) = &
					PRODU_DOLLAR(THIS_PERIOD%) + &
					IC_35HISTORY::PRICEAMT(J%)

			NEXT J%

			WHEN ERROR IN
				GET #IC_35HISTORY.CH%(I%), REGARDLESS
			USE
				CONTINUE 17070
			END WHEN

			GOTO 17060
		END IF

17070		PERIOD% = PERIOD% + (13% - STARTPERIOD%(I%))

	NEXT I%

	!
	! Get the product cost
	!
	COST = PC_READ_COST(PD_PRODUCT::PRODUCT_NUM, LOCATION$, END_DATE$, "")


 PrintLine:
17900	!
	! Print out one line (If non-zero)
	!
	FLAG% = 0%
	FLAG% = -1% &
		IF PRODU_TOTAL(I%) <> 0.0 &
		FOR I% = 1% TO TOTAL_PERIODS%
	FLAG% = -1% &
		IF PRODU_DOLLAR(I%) <> 0.0 &
		FOR I% = 1% TO TOTAL_PERIODS%

	IF FLAG%
	THEN
		TEXT$ = PD_PRODUCT::PRODUCT_NUM + " "  + &
			PD_PRODUCT::DESCRIPTION + " "  + &
			PD_PRODUCT::PROD_TYPE + " "  + &
			PD_PRODUCT::CATEGORY + " "  + &
			PD_PRODUCT::SECONDARY_CODE

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		TEXT$ = " Units"
		TOTAL = 0.0
		FOR I% = 1% TO TOTAL_PERIODS%
			TEXT$ = TEXT$ + &
				FORMAT$(FUNC_ROUND(PRODU_TOTAL(I%), 0%), &
				"########")
			TOTAL = TOTAL + PRODU_TOTAL(I%)
		NEXT I%
		TEXT$ = TEXT$ + FORMAT$(FUNC_ROUND(TOTAL, 0%), "########")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 2%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		TEXT$ = "Dollar"
		TOTAL = 0.0
		FOR I% = 1% TO TOTAL_PERIODS%
			TEXT$ = TEXT$ + &
				FORMAT$(FUNC_ROUND(PRODU_DOLLAR(I%), 0%), &
				"########")
			TOTAL = TOTAL + PRODU_DOLLAR(I%)
		NEXT I%
		TEXT$ = TEXT$ + FORMAT$(FUNC_ROUND(TOTAL, 0%), "########")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -2%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -3%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		!
		! Create totals
		!
		FOR I% = 1% TO TOTAL_PERIODS%
			GROUP_TOTAL(I%) = GROUP_TOTAL(I%) + PRODU_TOTAL(I%)
			GROUP_DOLLAR(I%) = GROUP_DOLLAR(I%) + PRODU_DOLLAR(I%)

			GRAND_TOTAL(I%) = GRAND_TOTAL(I%) + PRODU_TOTAL(I%)
			GRAND_DOLLAR(I%) = GRAND_DOLLAR(I%) + PRODU_DOLLAR(I%)

			PRODU_TOTAL(I%) = 0.0
			PRODU_DOLLAR(I%) = 0.0
		NEXT I%
	END IF

	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:
	!
	! Print out grand total
	!
	TEXT$ = "**** Grand Total ****"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TEXT$ = " Units"
	TOTAL = 0.0
	FOR I% = 1% TO TOTAL_PERIODS%
		TEXT$ = TEXT$ + &
			FORMAT$(FUNC_ROUND(GRAND_TOTAL(I%), 0%), "########")
		TOTAL = TOTAL + GRAND_TOTAL(I%)
	NEXT I%
	TEXT$ = TEXT$ + FORMAT$(FUNC_ROUND(TOTAL, 0%), "########")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 2%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TEXT$ = "Dollar"
	TOTAL = 0.0
	FOR I% = 1% TO TOTAL_PERIODS%
		TEXT$ = TEXT$ + &
			FORMAT$(FUNC_ROUND(GRAND_DOLLAR(I%), 0%), "########")
		TOTAL = TOTAL + GRAND_DOLLAR(I%)
	NEXT I%
	TEXT$ = TEXT$ + FORMAT$(FUNC_ROUND(TOTAL, 0%), "########")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -2%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

 ExitProgram:
	!
	! Quit
	!
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
