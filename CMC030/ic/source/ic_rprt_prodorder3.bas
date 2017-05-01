1	%TITLE "Product Ordering Report"
	%SBTTL "IC_RPRT_PRODORDER3"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 2002 BY
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
	! Software Solutions, Inc. assumes no responsibility for the
	! use or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	! ID:IC024
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Product Ordering Report\* assists in the
	!	purchasing of products, by indicating past sales history, and
	!	by reporting on quantities on order, on hand and allocated.
	!	It contains the following
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
	!	.x Product Ordering>Report
	!	.x Report>Product Ordering
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_RPRT_PRODORDER3/LINE
	!	$ LINK/EXE=IC_EXE: IC_RPRT_PRODORDER3, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE IC_RPRT_PRODORDER3.OBJ;*
	!
	! Author:
	!
	!	10/02/2002 - Kevin Handy
	!		Taken from IC_RPRT_PRODORDER2 and modified
	!
	! Modification History:
	!
	!	10/08/2002 - Kevin Handy
	!		Fix several bugs in calculation.
	!		Add ability to select quarter to start SO calc with.
	!		Fix LOCATION in title so it isn't blank.
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

	%INCLUDE "SOURCE:[PO.OPEN]PO_PARTCROSS.HB"
	MAP (PO_PARTCROSS)	PO_PARTCROSS_CDD	PO_PARTCROSS

	%INCLUDE "SOURCE:[IC.OPEN]IC_35HISTORY.HB"
	MAP (IC_35HISTORY)	IC_35HISTORY_CDD	IC_35HISTORY
	DECLARE			IC_35HISTORY_CDD	IC_HISTORY(2%)

	!
	! Dimension arrays
	!
	DECLARE REAL QTYQTR(8%)

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
	!	The ^*From Item\* value field causes the
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

	LOCATION_LIST$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Location\*
	!	.b
	!	.lm +5
	!	The ^*Location\* field selects designated
	!	product locations for which the report will be printed.
	!	Seperate locations to be included with comma's.
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
	!	the status of the product which will be printed,
	!	by entering a "wildcard" value.
	!	.lm -5
	!
	! Index:
	!	.x Product Status>Product Ordering Report
	!	.x Product Ordering Report>Product Status
	!
	!--

	START_PERIOD$ = LEFT$(UTL_REPORTX::OPTDEF(6%), 1%)
	START_PERIOD$ = "5" IF INSTR(1%, " 5432", START_PERIOD$) < 2%
	START_PERIOD% = VAL%(START_PERIOD$) - 1%

	!++
	! Abstract:FLD07
	!	^*(07) Starting Period\*
	!	.b
	!	.lm +5
	!	Which period should the calculation for the SO
	!	column use.
	!	.list
	!	.le
	!	*5 = Qtr5 + Qtr4
	!	.le
	!	*4 = Qtr4 + Qtr3
	!	.le
	!	*3 = Qtr3 + Qtr2
	!	.le
	!	*2 = Qtr2 + QCur
	!	.end list
	!	.lm -5
	!
	! Index:
	!	.x Period>Product Ordering Report
	!	.x Product Ordering Report>Period
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

	START_DATE$ = DATE_TODAY

	V% = READ_PERIOD("DATE", IC_CONTROL::ERA, PERIOD$, &
		PERIOD_DESCR$, STAT$, START_DATE$, END_DATE$, 0%)

	START% = 4% + VAL%(STAT$)

	STARTPERIOD%(1%) = 1%
	ENDPERIOD%(1%)   = 3% * VAL%(STAT$)
	STARTPERIOD%(2%) = 3% * VAL%(STAT$) - 2%
	ENDPERIOD%(2%)   = 12%

	YYYY$ = LEFT(PERIOD$, 4%)

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

320	!
	! Open History file
	!
	FOR Y% = 1% TO 2%

		WHEN ERROR IN
			%INCLUDE "SOURCE:[IC.OPEN]IC_35HISTORY.OPN"
		USE
			CONTINUE 330 IF ERR = 5%
			FILENAME$ = "IC_35HISTORY"
			CONTINUE HelpError
		END WHEN

		IC_35HISTORY.CH%(Y%) = IC_35HISTORY.CH%
		IC_35HISTORY.CH% = 0.0
		YYYY$(Y%) = YYYY$
		YYYY$ = FORMAT$(VAL%(YYYY$) - 1%, "####")
	NEXT Y%

330	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_PARTCROSS.OPN"
	USE
		CONTINUE ReportTitle IF ERR = 5%
		FILENAME$ = "PO_PARTCROSS"
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
		ADD_TITLE$ = "BY  PRODUCT  SECONDARY  CODE"

	CASE "T"
		SORT_KEY% = 1%
		ADD_TITLE$ = "BY  PRODUCT  TYPE"

	END SELECT

	TITLE$(1%) = "PRODUCT  ORDERING  " + ADD_TITLE$
	TITLE$(2%) = "FOR  LOCATION  " + LOCATION_LIST$ + &
		"  STARTING PERIOD " + START_PERIOD$
	TITLE$(3%) = "Inventory Control System"
	TITLE$(4%) = ""

	!
	! Heading
	!
	TITLE$(5%) = "Product#           Description        " + &
		"     Qtr5     Qtr4  "  + &
		"   Qtr3     Qtr2     QCur OnOrder  OnHand   Alloc" + &
		"      SO   1.25%   1.30%"

	TITLE$(6%) = "."

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

	GOSUB OneProduct

	!
	! Try for next record
	!
	GOTO GetNextRec

 OneProduct:
	!
	! Do all locations in list for one product
	!
	LOCATION_LOOP$ = LOCATION_LIST$ + ","
	PRINTED_HEADER% = 0%

 OneProductLoop:
	!
	! Pop off one location (and force it to 4 characters)
	!
	I% = INSTR(1%, LOCATION_LOOP$, ",")
	RETURN IF I% = 0%
	LOCATION$ = LEFT(LEFT(LOCATION_LOOP$, I% - 1%) + "    ", 4%)
	LOCATION_LOOP$ = RIGHT(LOCATION_LOOP$, I% + 1%)

	!
	! See if we can get a balance for the product
	!
	V% = IC_READ_35BALANCE(PD_PRODUCT::PRODUCT_NUM, &
		LOCATION$, BALANCE(,))

	ON_HAND = BALANCE(1%, 1%) + BALANCE(1%, 2%) + BALANCE(1%, 3%)
	ON_ORDER = BALANCE(3%, 1%) + BALANCE(3%, 2%) + BALANCE(3%, 3%)
	ALLOC = BALANCE(2%, 1%) + BALANCE(2%, 2%) + BALANCE(2%, 3%)

	!
	! Get the product cost
	!
	COST = PC_READ_COST(PD_PRODUCT::PRODUCT_NUM, LOCATION$, END_DATE$, "")

17100	!
	! Get History record
	!
	FOR Y% = 1% TO 2%

		WHEN ERROR IN
			FIND #IC_35HISTORY.CH%(Y%), &
				KEY #0% EQ PD_PRODUCT::PRODUCT_NUM + LOCATION$, &
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
			"SA,LS,RT,IS,SE,SP,WR") = 0%

		FOR I% = STARTPERIOD%(Y%) TO ENDPERIOD%(Y%)

			J% = 4% * (2% - Y%) + INT((I% - 1%) / 3%) + 1%

			SELECT IC_HISTORY(Y%)::TRANSTYPE

			CASE "LS"
				LOST_SALES = LOST_SALES + &
					IC_HISTORY(Y%)::PQUANTITY(I%)
			CASE ELSE
				QTYQTR(J%) = QTYQTR(J%) - &
					IC_HISTORY(Y%)::PQUANTITY(I%)
			END SELECT

		NEXT I%

		GOTO GetHistRec

17130	NEXT Y%

	!
	! Get some info from the Partcross file
	!
	PO_PARTCROSS::VENDOR = ""
	PO_PARTCROSS::MINQTY = 0.0

17200	WHEN ERROR IN
		GET #PO_PARTCROSS.CH%, &
			KEY #0% EQ PD_PRODUCT::PRODUCT_NUM, &
			REGARDLESS
	USE
		CONTINUE PrintLine IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PO_PARTCROSS"
		CONTINUE HelpError
	END WHEN

 PrintLine:
	!
	! Print out one line
	!
	GOSUB ProductHeader

	SO_VAL = (QTYQTR(START% - START_PERIOD%) + &
		QTYQTR(START% - START_PERIOD% + 1%)) / 2.0 - &
		ON_ORDER - ON_HAND - ALLOC

	TEXT$ = LEFT(HEADER$, 14%) + &
		" " + LOCATION$ + " "  + &
		LEFT(PD_PRODUCT::DESCRIPTION, 18%) + "  " + &
		FORMAT$(QTYQTR(START% - 4%), "###,### ") + " " + &
		FORMAT$(QTYQTR(START% - 3%), "###,### ") + " " + &
		FORMAT$(QTYQTR(START% - 2%), "###,### ") + " " + &
		FORMAT$(QTYQTR(START% - 1%), "###,### ") + " " + &
		FORMAT$(QTYQTR(START%), "###,### ") + " " + &
		FORMAT$(ON_ORDER, "##,### ") + " " + &
		FORMAT$(ON_HAND, "##,### ") + " " + &
		FORMAT$(-ALLOC, "##,### ") + " " + &
		FORMAT$(SO_VAL, "##,### ") + " " + &
		FORMAT$(SO_VAL * 1.25, "##,### ") + " " + &
		FORMAT$(SO_VAL * 1.3, "##,### ")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT
	HEADER$ = SPACE$(LEN(PD_PRODUCT::PRODUCT_NUM))

	GOSUB InitArray

	GOTO OneProductLoop

 ProductHeader:
	IF (PRINTED_HEADER% = 0%)
	THEN
		PRINTED_HEADER% = -1%
		HEADER$ = PD_PRODUCT::PRODUCT_NUM
	END IF

	RETURN

	!
	! Print final totals
	!
 ExitTotal:

	!
	! Leave Program
	!
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

 InitArray:
	FOR J% = 1% TO 8%
		QTYQTR(J%) = 0.0
	NEXT J%

	LOST_SALES = 0.0

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
