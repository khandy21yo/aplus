1	%TITLE "Manufacture Ordering Report"
	%SBTTL "IC_RPRT_MANUFORDER_LL"
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
	! available to any other persON_  No title to and ownership of
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
	! ID:IC024
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Manufacture Ordering Report\* is intended to assist in the
	!	purchasing of products, by indicating cost, past sales history,
	!	and by reporting on quantities on order, and on hand.
	!	The ^*Manufacture Ordering Report\* contains the following
	!	fields:
	!	.table 3,25
	!	.te
	!	Location	Product Number
	!	.te
	!	Description	Type
	!	.te
	!	Category	Product Type
	!	.te
	!	Product Category	Product Secondary Code
	!	.te
	!	Cost	Sales History
	!	.te
	!	On Order	On Hand
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Manufacture Ordering>Report
	!	.x Report>Manufacture Ordering
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_RPRT_MANUFORDER_LL/LINE
	!	$ LINK/EXE=IC_EXE: IC_RPRT_MANUFORDER_LL, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE IC_RPRT_MANUFORDER_LL.OBJ;*
	!
	! Author:
	!
	!	06/12/2002 - Kevin Handy
	!		Copied from IC_RPRT_MANUFORDER
	!
	! Modification History:
	!
	!	06/12/2003 - Kevin Handy
	!		Fix up titles
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

	!
	! Dimension arrays
	!
	DECLARE INTEGER CONSTANT MAX_YEARS = 5%

	DECLARE REAL QTYQTR(8%)
	DIM IC_35HISTORY_FILE$(100%)
	DIM IC_35HISTORY.CH%(MAX_YEARS)

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
	!	.x Sort by>Manufacture Ordering Report
	!	.x Manufacture Ordering Report>Sort by
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field causes the
	!	report to begin with a selected item number.  The value entered
	!	must be in agreement with the value in field (01) Sort by.
	!	.b
	!	A blank field will cause the report to begin with the first
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Manufacture Ordering Report
	!	.x Manufacture Ordering RepoIrt>From Item
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
	!	.x To Item>Manufacture Ordering Report
	!	.x Manufacture Ordering Report>To Item
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
	!	to be printed by entering a "wildcard" value in this field.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Manufacture Ordering Report
	!	.x Manufacture Ordering Report>Wildcard
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
	!	.x Location>Manufacture Ordering Report
	!	.x Manufacture Ordering Report>Location
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

	!
	! Look up available history files
	!
	CALL READ_DEVICE("IC_35HISTORY", IC_35HISTORY.DEV$, STAT%)

	CALL FIND_FILE(IC_35HISTORY.DEV$ + "IC_35HISTORY_%%%%.HIS", &
		IC_35HISTORY_FILE$(), 16%, "", "")

	IC_35HISTORY_FILE% = VAL%(IC_35HISTORY_FILE$(0%))

	!
	! Strip dowbn to just the year
	!
	FOR LOOP% = 1% TO IC_35HISTORY_FILE%
		IC_35HISTORY_FILE$(LOOP%) = &
			MID(IC_35HISTORY_FILE$(LOOP%), 14%, 4%)
	NEXT LOOP%

	FOR LOOP% = IC_35HISTORY_FILE% + 1% TO 5%
		IC_35HISTORY_FILE$(LOOP%) = "    "
	NEXT LOOP%

	!
	! Reverse the list
	!
	FOR LOOP% = 1% TO IC_35HISTORY_FILE% / 2%
		TEMP$ = IC_35HISTORY_FILE$(LOOP%)
			IC_35HISTORY_FILE$(LOOP%) = &
			IC_35HISTORY_FILE$(IC_35HISTORY_FILE% - &
			LOOP% + 1%)
		IC_35HISTORY_FILE$(LOOP%) = &
			IC_35HISTORY_FILE$(IC_35HISTORY_FILE% - &
			LOOP% + 1%)
		IC_35HISTORY_FILE$(IC_35HISTORY_FILE% - &
			LOOP% + 1%) = TEMP$
	NEXT LOOP%

	!
	! Allow maximum of 5 years
	!
	IC_35HISTORY_FILE% = MAX_YEARS IF IC_35HISTORY_FILE% > MAX_YEARS

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

320	!
	! Open History file
	!
	FOR Y% = 1% TO IC_35HISTORY_FILE%

		YYYY$ = IC_35HISTORY_FILE$(Y%)
		IC_35HISTORY.CH% = 0%

		WHEN ERROR IN
			%INCLUDE "SOURCE:[IC.OPEN]IC_35HISTORY.OPN"
		USE
			CONTINUE 330 IF ERR = 5%
			FILENAME$ = "IC_35HISTORY"
			CONTINUE HelpError
		END WHEN

		IC_35HISTORY.CH%(Y%) = IC_35HISTORY.CH%
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

	TITLE$(1%) = "ANNUAL  MANUFACTURE  ORDERING  " + ADD_TITLE$
	TITLE$(2%) = "FOR  LOCATION  " + LOCATION$
	TITLE$(3%) = "Inventory Control System"
	TITLE$(4%) = ""

	!
	! Heading
	!
	TITLE$(5%) = "Product#       Description       " + &
		"Tp Cat  SecCode          Cost"
	TITLE$(5%) = TITLE$(5%) + "     " + IC_35HISTORY_FILE$(Y%) &
		FOR Y% = 1% TO 5%
	TITLE$(5%) = TITLE$(5%) + " OnOrder  OnHand   Alloc"

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
		GOTO ExitProgram IF (PD_PRODUCT::PROD_TYPE > TO_ITEM$) AND &
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
	ON_ORDER = BALANCE(3%, 1%) + BALANCE(3%, 2%) + BALANCE(3%, 3%)
	ALLOC = BALANCE(2%, 1%) + BALANCE(2%, 2%) + BALANCE(2%, 3%)

	!
	! Get the product cost
	!
	COST = PC_READ_COST(PD_PRODUCT::PRODUCT_NUM, LOCATION$, END_DATE$, "")

17100	!
	! Get History record
	!
	FOR Y% = 1% TO IC_35HISTORY_FILE%

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

		GOTO 17130 IF IC_35HISTORY::PRODUCT <> &
			PD_PRODUCT::PRODUCT_NUM

		GOTO 17130 IF IC_35HISTORY::LOCATION <> LOCATION$

		GOTO GetHistRec IF COMP_STRING(EDIT$( &
			IC_35HISTORY::TRANSTYPE, -1%), &
			"SA,RT,IS,SE,SP,WR") = 0%

		FOR I% = 1% TO 12%

			QTYQTR(Y%) = QTYQTR(Y%) - IC_35HISTORY::PQUANTITY(I%)

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
	TEXT$ = PD_PRODUCT::PRODUCT_NUM + " " + &
		LEFT(PD_PRODUCT::DESCRIPTION, 17%) + " " + &
		PD_PRODUCT::PROD_TYPE + " " + &
		PD_PRODUCT::CATEGORY + " " + &
		PD_PRODUCT::SECONDARY_CODE + "  " + &
		FORMAT$(COST, "##,###.##  ") + &
		FORMAT$(QTYQTR(1%), "###,###  ") + &
		FORMAT$(QTYQTR(2%), "###,###  ") + &
		FORMAT$(QTYQTR(3%), "###,###  ") + &
		FORMAT$(QTYQTR(4%), "###,###  ") + &
		FORMAT$(QTYQTR(5%), "###,###  ") + &
		FORMAT$(ON_ORDER, "##,###  ") + &
		FORMAT$(ON_HAND, "##,###  ") + &
		FORMAT$(-ALLOC, "##,### ")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOSUB InitArray

	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:

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

	COST = 0.0

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
