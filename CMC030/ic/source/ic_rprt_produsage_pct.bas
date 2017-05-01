1	%TITLE "Product Usage Report by Percentage"
	%SBTTL "IC_RPRT_PRODUSAGE_PCT"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1997 BY
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
	! ID:IC034
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Product Usage Report\* is intended to show usage
	!	for inventory products.
	!	fields:
	!	.table 3,25
	!	.te
	!	Product Number	Description
	!	.te
	!	Type	Category
	!	.te
	!	Secondary Code	Location
	!	.te
	!	Sales History	On Order
	!	.te
	!	On Hand	Allocated
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Product Usage>Report
	!	.x Report>Product Usage
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_RPRT_PRODUSAGE_PCT/LINE
	!	$ LINK/EXE=IC_EXE: IC_RPRT_PRODUSAGE_PCT, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE IC_RPRT_PRODUSAGE_PCT.OBJ;*
	!
	! Author:
	!
	!	11/24/97 - Kevin Handy
	!		Based upon IC_RPRT_PRODUSAGE
	!
	! Modification History:
	!
	!	12/02/97 - Kevin Handy
	!		Added excess cost field
	!
	!	02/04/98 - Kevin Handy
	!		Break 300 into two lines to simplify error trapping.
	!
	!	02/05/98 - Kevin Handy
	!		Modified to use "quarters" which are three month
	!		groups starting with last month.
	!
	!	02/08/98 - Kevin Handy
	!		Modified to sort the resulting file by extended
	!		cost.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/24/2000 - Kevin Handy
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
	DECLARE			IC_35HISTORY_CDD	IC_HISTORY(3%)

	RECORD TEMP_SORT_CDD
		DECIMAL(12, 2) SORTBY
		STRING LINE1 = 132%
		STRING LINE2 = 132%
	END RECORD

	MAP (TEMP_SORT) TEMP_SORT_CDD TEMP_SORT

	!
	! Dimension arrays
	!
	DECLARE REAL QTYQTR(12%)
	DIM YYYY$(3%)
	DIM IC_35HISTORY.CH%(3%)
	DIM USE_YYYY$(24%)
	DIM USE_PERIOD%(24%)

	!
	! External functions
	!
	EXTERNAL REAL    FUNCTION PC_READ_COST
	EXTERNAL LONG    FUNCTION IC_READ_35BALANCE

	DEF FNPCT(X, Y)

		IF (FUNC_ROUND(Y, 2%) <> 0.0)
		THEN
			XPCT = (X / Y) * 100.0
			IF XPCT% > 9999.99
			THEN
				FNPCT = 9999.99
			ELSE
				IF XPCT% < -9999.99
				THEN
					FNPCT = -9999.99
				ELSE
					FNPCT = XPCT
				END IF
			END IF
		ELSE
			FNPCT = 0.0
		END IF
	FNEND

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
	!	.x Sort by>Product Usage Report
	!	.x Product Usage Report>Sort by
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
	!	.x From Item>Product Usage Report
	!	.x Product Usage RepoIrt>From Item
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
	!	.x To Item>Product Usage Report
	!	.x Product Usage Report>To Item
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
	!	.x Wildcard>Product Usage Report
	!	.x Product Usage Report>Wildcard
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
	!	.x Location>Product Usage Report
	!	.x Product Usage Report>Location
	!
	!--

	USAGE_LEVEL = VAL(EDIT$(UTL_REPORTX::OPTDEF(5%), -1%))

	!++
	! Abstract:FLD06
	!	^*(06) Usage Level\*
	!	.b
	!	.lm +5
	!	The ^*Usage Level\* field prints products
	!	with usage higher in the last 15 periods.
	!	.lm -5
	!
	! Index:
	!	.x Usage Level>Product Usage Report
	!	.x Product Usage Report>Usage Level
	!
	!--

	WILDLABEL$ = TRM$(UTL_REPORTX::OPTDEF(6%))

	!++
	! Abstract:FLD07
	!	^*(07) Wildcard Label\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard Label\* field causes only products with a label
	!	that matches the wildcard to be printed.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard Label>Product Usage Report
	!	.x Product Usage Report>Wildcard Label
	!
	!--

	USAGE_PERIOD% = VAL%(EDIT$(UTL_REPORTX::OPTDEF(7%), -1%))
	USAGE_PERIOD% = 1% IF USAGE_PERIOD% < 1%
	USAGE_PERIOD% = 8% IF USAGE_PERIOD% > 8%

	!++
	! Abstract:FLD08
	!	^*(08) Usage Periods\*
	!	.b
	!	.lm +5
	!	.lm -5
	!
	! Index:
	!	.x Periods>Product Usage Report
	!	.x Product Usage Report>Periods
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

305	START_DATE$ = DATE_TODAY

	V% = READ_PERIOD("DATE", IC_CONTROL::ERA, PERIOD$, &
		PERIOD_DESCR$, STAT$, START_DATE$, END_DATE$, 0%)

	USE_YEAR% = 0%
	FOR LOOP% = 1% TO USAGE_PERIOD% * 3%

		!
		! Calculate previous period
		!
		YYYY% = VAL%(LEFT(PERIOD$, 4%))
		MM% = VAL%(RIGHT(PERIOD$, 5%)) - 1%

		IF MM% <= 0%
		THEN
			MM% = 12%
			YYYY% = YYYY% - 1%
		END IF

		YYYY$ = FORMAT$(YYYY%, "<0>###")
		PERIOD$ = YYYY$ + FORMAT$(MM%, "<0>#")

		IF (USE_YEAR% = 0%) OR (YYYY$(USE_YEAR%) <> YYYY$)
		THEN
			USE_YEAR% = USE_YEAR% + 1%
			YYYY$(USE_YEAR%) = YYYY$
		END IF

		USE_YYYY$(LOOP%) = YYYY$
		USE_PERIOD%(LOOP%) = MM%

	NEXT LOOP%

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

320	!
	! Open History file
	!
	FOR LOOP% = 1% TO USE_YEAR%
		YYYY$ = YYYY$(LOOP%)

		WHEN ERROR IN
			%INCLUDE "SOURCE:[IC.OPEN]IC_35HISTORY.OPN"
		USE
			CONTINUE ReportTitle IF ERR = 5%
			FILENAME$ = "IC_35HISTORY"
			CONTINUE HelpError
		END WHEN

		IC_35HISTORY.CH%(LOOP%) = IC_35HISTORY.CH%
		IC_35HISTORY.CH% = 0.0
	NEXT LOOP%

330	!
	! Create a temporary file
	!
	CALL ASSG_CHANNEL(TEMP_SORT.CH%, TEMP.STATUS%)
	OPEN "TEMP.SORT" FOR OUTPUT AS FILE TEMP_SORT.CH%, &
		TEMPORARY, &
		INDEXED FIXED, &
		MAP TEMP_SORT, &
		PRIMARY KEY TEMP_SORT::SORTBY DUPLICATES, &
		ACCESS MODIFY, &
		ALLOW NONE

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
		ADD_TITLE$ = "BY  PRODUCT  SECONDARY  CODE"

	CASE "T"
		SORT_KEY% = 1%
		ADD_TITLE$ = "BY  PRODUCT  TYPE"

	END SELECT

	TITLE$(1%) = "USAGE  PERCENTAGE  " + ADD_TITLE$
	TITLE$(2%) = "FOR  LOCATION  " + LOCATION$ + &
		" FOR " + NUM1$(USAGE_PERIOD%) + " PERIODS"
	TITLE$(3%) = "Inventory Control System"
	TITLE$(4%) = ""

	!
	! Heading
	!
	TITLE$(5%) = "Product#       Description          " + &
		"Tp Cat  SecCode        Qtr8  "  + &
		"   Qtr7     Qtr6     Qtr5  OnHand  Unit Cost     Ext Cost"
	TITLE$(6%) = "                                    " + &
		"                       Qtr4  "  + &
		"   Qtr3     Qtr2     QCur    Used  Percentage Excess Cost"

	TITLE$(7%) = "."

	TOTAL_COST = 0.0

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	GOSUB InitArray

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
	! Check label
	!
	GOTO GetNextRec &
		IF COMP_STRING(TRM$(PD_PRODUCT::LABEL), WILDLABEL$) = 0% &
		AND WILDLABEL$ <> ""

	!
	! Check current record
	!
	GOTO GetNextRec IF PD_PRODUCT::SSTATUS = "O"

	SELECT SORT_BY$

	CASE "C"
		GOTO  ExitTotal IF (PD_PRODUCT::CATEGORY > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			PD_PRODUCT::CATEGORY, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "D"
		GOTO  ExitTotal IF (PD_PRODUCT::DESCRIPTION > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			PD_PRODUCT::DESCRIPTION, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "P"
		GOTO  ExitTotal IF (PD_PRODUCT::PRODUCT_NUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			PD_PRODUCT::PRODUCT_NUM, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "S"
		GOTO  ExitTotal IF (PD_PRODUCT::SECONDARY_CODE > TO_ITEM$) AND &
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
	V% = IC_READ_35BALANCE(PD_PRODUCT::PRODUCT_NUM, &
		LOCATION$, BALANCE(,))

	ON_HAND = BALANCE(1%, 1%) + BALANCE(1%, 2%) + BALANCE(1%, 3%)

	!
	! Get the product cost
	!
	COST = PC_READ_COST(PD_PRODUCT::PRODUCT_NUM, LOCATION$, END_DATE$, "")

17100	!
	! Get History record
	!
	FOR Y% = 1% TO USE_YEAR%

		WHEN ERROR IN
			FIND #IC_35HISTORY.CH%(Y%), &
				KEY #0% EQ PD_PRODUCT::PRODUCT_NUM + &
				LOCATION$, &
				REGARDLESS
		USE
			CONTINUE 17130 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "IC_35HISTORY"
			CONTINUE HelpError
		END WHEN

 GetHistRec:
17120		WHEN ERROR IN
			GET #IC_35HISTORY.CH%(Y%), REGARDLESS
		USE
			CONTINUE 17130 IF ERR = 11%
			FILENAME$ = "IC_35HISTORY"
			CONTINUE HelpError
		END WHEN

		IC_HISTORY(Y%) = IC_35HISTORY

		GOTO 17130 IF IC_HISTORY(Y%)::PRODUCT <> &
			PD_PRODUCT::PRODUCT_NUM

		GOTO 17130 IF IC_HISTORY(Y%)::LOCATION <> LOCATION$

		GOTO GetHistRec IF COMP_STRING(EDIT$( &
			IC_HISTORY(Y%)::TRANSTYPE, -1%), &
			"SA,RT,IS,SE,SP,WR") = 0%

		FOR I% = 1% TO USAGE_PERIOD% * 3%

			IF USE_YYYY$(I%) = YYYY$(Y%)
			THEN
				J% = (I% - 1%) / 3% + 1%
				QTYQTR(J%) = QTYQTR(J%) - &
					IC_HISTORY(Y%)::PQUANTITY(USE_PERIOD%(I%))
			END IF
		NEXT I%

		GOTO GetHistRec

17130	NEXT Y%

	USAGE = 0.0
	USAGE = USAGE + QTYQTR(I%) &
		FOR I% = 1% TO 8%
	USAGE = FUNC_ROUND(USAGE, 0%)
	ON_HAND = FUNC_ROUND(ON_HAND, 0%)

	USAGE_PCT = FNPCT(USAGE, ON_HAND)

	IF ((USAGE_PCT = 0.0) AND (ON_HAND <> 0.0)) OR &
		((USAGE_PCT <> 0.0) AND (USAGE_PCT <= USAGE_LEVEL))
	THEN
		!
		! Print out one line
		!
		TEXT1$ = PD_PRODUCT::PRODUCT_NUM + " " + &
			LEFT(PD_PRODUCT::DESCRIPTION, 20%) + " " + &
			PD_PRODUCT::PROD_TYPE + " " + &
			PD_PRODUCT::CATEGORY + " " + &
			PD_PRODUCT::SECONDARY_CODE + " " + &
			FORMAT$(QTYQTR(8%), "###,### ") + " "  + &
			FORMAT$(QTYQTR(7%), "###,### ") + " "  + &
			FORMAT$(QTYQTR(6%), "###,### ") + " "  + &
			FORMAT$(QTYQTR(5%), "###,### ") + " "  + &
			FORMAT$(ON_HAND, "##,###") + &
			FORMAT$(COST, "########.##") + &
			FORMAT$(COST * ON_HAND, "########.##")

		TEXT2$ = "              " + " " + &
			"                    " + " " + &
			"  " + " " + &
			"    " + " " + &
			"          " + " " + &
			FORMAT$(QTYQTR(4%), "###,### ") + " "  + &
			FORMAT$(QTYQTR(3%), "###,### ") + " "  + &
			FORMAT$(QTYQTR(2%), "###,### ") + " "  + &
			FORMAT$(QTYQTR(1%), "###,### ") + " "  + &
			FORMAT$(USAGE, "##,###") + &
			FORMAT$(USAGE_PCT, "#####.##") + "%  " + &
			FORMAT$(COST * (ON_HAND - USAGE), "########.##")

 !		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT1$, 0%)
 !		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT2$, -1%)
 !		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -2%)
 !		GOTO ExitProgram IF UTL_REPORTX::STAT

		TEMP_SORT::SORTBY = -(COST * (ON_HAND - USAGE))
		TEMP_SORT::LINE1 = TEXT1$
		TEMP_SORT::LINE2 = TEXT2$
		PUT #TEMP_SORT.CH%

		TOTAL_COST = TOTAL_COST + COST * ON_HAND
		TOTAL_EXCESS = TOTAL_EXCESS + COST * (ON_HAND - USAGE)

	END IF

	GOSUB InitArray

	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:
17900	WHEN ERROR IN
		RESET #TEMP_SORT.CH%
	USE
		CONTINUE 17950
	END WHEN

17910	WHEN ERROR IN
		GET #TEMP_SORT.CH%
	USE
		CONTINUE 17950
	END WHEN

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEMP_SORT::LINE1, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEMP_SORT::LINE2, -1%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -2%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOTO 17910

17950	TEXT$ = "              " + " " + &
		" Grand Total On Hand" + " " + &
		"  " + " " + &
		"    " + " " + &
		"          " + " " + &
		"        " + " "  + &
		"        " + " "  + &
		"        " + " "  + &
		"        " + " "  + &
		"      " + &
		"           " + &
		FORMAT$(TOTAL_COST, "########.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -2%)

	TEXT$ = "              " + " " + &
		" Grand Total Excess " + " " + &
		"  " + " " + &
		"    " + " " + &
		"          " + " " + &
		"        " + " "  + &
		"        " + " "  + &
		"        " + " "  + &
		"        " + " "  + &
		"      " + &
		"           " + &
		FORMAT$(TOTAL_EXCESS, "########.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -2%)

 ExitProgram:
	CLOSE TEMP_SORT.CH%

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

 InitArray:
	FOR J% = 1% TO 12%
		QTYQTR(J%) = 0.0
	NEXT J%

	RETURN

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
