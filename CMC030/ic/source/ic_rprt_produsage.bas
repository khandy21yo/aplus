1	%TITLE "Product Usage Report"
	%SBTTL "IC_RPRT_PRODUSAGE"
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
	!	$ BAS IC_SOURCE:IC_RPRT_PRODUSAGE/LINE
	!	$ LINK/EXE=IC_EXE: IC_RPRT_PRODUSAGE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE IC_RPRT_PRODUSAGE.OBJ;*
	!
	! Author:
	!
	!	08/08/93 - Frank F. Starman
	!
	! Modification History:
	!
	!	08/12/93 - Frank F. Starman
	!		Changed flag zero only to a number.
	!
	!	10/21/93 - Frank F. Starman
	!		Do not print items if there is zero usage and zero
	!		on the hand.
	!
	!	01/12/94 - Kevin Handy
	!		Clean up (Check)
	!
	!	01/14/94 - Kevin Handy
	!		Modified error trap for 300 to say it is the
	!		period definition file if the error is "Illegal
	!		number"
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	05/16/95 - Kevin Handy
	!		Added wildcard label.
	!		Lose excess externals.
	!		Reformat source.
	!
	!	09/05/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	07/30/97 - Kevin Handy
	!		Change XAGE parameter of READ_PERIOD to integer
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	11/24/97 - Kevin Handy
	!		Added cost info to report
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/26/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	06/20/2001 - Kevin Handy
	!		Use INSTR instead of COMP_STRING to check transaction
	!		types against a fixed list.
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
	!	The ^*From Item\* value causes the
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
	!	.x Only Zero>Product Usage Report
	!	.x Product Usage Report>Only Zero
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

	WHEN ERROR IN
		START% = 4% + VAL%(STAT$)

		STARTPERIOD%(1%) = 1%
		ENDPERIOD%(1%) = 3% * VAL%(STAT$)
		STARTPERIOD%(2%) = 3% * VAL%(STAT$) - 2%
		ENDPERIOD%(2%) = 12%

		YYYY$ = LEFT(PERIOD$, 4%)
	USE
		FILENAME$ = "UTL_PERIOD"
		CONTINUE HelpError
	END WHEN

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
			CONTINUE ReportTitle IF ERR = 5%
			FILENAME$ = "IC_35HISTORY"
			CONTINUE HelpError
		END WHEN

		IC_35HISTORY.CH%(Y%) = IC_35HISTORY.CH%
		IC_35HISTORY.CH% = 0.0
		YYYY$(Y%) = YYYY$
		YYYY$ = FORMAT$(VAL%(YYYY$) - 1%, "####")
	NEXT Y%

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

	TITLE$(1%) = "PRODUCT  USAGE " + ADD_TITLE$
	TITLE$(2%) = "FOR  LOCATION  " + LOCATION$
	TITLE$(3%) = "Inventory Control System"
	TITLE$(4%) = ""

	!
	! Heading
	!
	TITLE$(5%) = "Product#       Description          " + &
		"Tp Cat  SecCode        Qtr5     Qtr4  " + &
		"   Qtr3     Qtr2     QCur  OnHand  Unit Cost     Ext Cost"

	TITLE$(6%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	GOSUB InitArray

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PD_PRODUCT.CH%, &
				KEY #SORT_KEY%
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
	! Check label
	!
	IF WILDLABEL$ <> ""
	THEN
		GOTO GetNextRec &
			IF COMP_STRING(TRM$(PD_PRODUCT::LABEL), WILDLABEL$) = 0%
	END IF

	!
	! Check current record
	!
	GOTO GetNextRec IF PD_PRODUCT::SSTATUS = "O"

	SELECT SORT_BY$

	CASE "C"
		GOTO ExitProgram IF (PD_PRODUCT::CATEGORY > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			PD_PRODUCT::CATEGORY, -1%), WLDCRD$) = 0% AND &
			WLDCRD$ <> ""

	CASE "D"
		GOTO ExitProgram IF (PD_PRODUCT::DESCRIPTION > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			PD_PRODUCT::DESCRIPTION, -1%), WLDCRD$) = 0% AND &
			WLDCRD$ <> ""

	CASE "P"
		GOTO ExitProgram IF (PD_PRODUCT::PRODUCT_NUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			PD_PRODUCT::PRODUCT_NUM, -1%), WLDCRD$) = 0% AND &
			WLDCRD$ <> ""

	CASE "S"
		GOTO ExitProgram &
			IF (PD_PRODUCT::SECONDARY_CODE > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			PD_PRODUCT::SECONDARY_CODE, -1%), WLDCRD$) = 0% AND &
			WLDCRD$ <> ""

	CASE "T"
		GOTO ExitProgram IF (PD_PRODUCT::PROD_TYPE > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			PD_PRODUCT::PROD_TYPE, -1%), WLDCRD$) = 0% AND &
			WLDCRD$ <> ""

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
	FOR Y% = 1% TO 2%

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

		GOTO GetHistRec IF INSTR(1%, "SA,RT,IS,SE,SP,WR", &
			IC_HISTORY(Y%)::TRANSTYPE) = 0%

		FOR I% = STARTPERIOD%(Y%) TO ENDPERIOD%(Y%)

			J% = 4% * (2% - Y%) + INT((I% - 1%) / 3%) + 1%

			QTYQTR(J%) = QTYQTR(J%) - IC_HISTORY(Y%)::PQUANTITY(I%)

		NEXT I%

		GOTO GetHistRec

17130	NEXT Y%

	USAGE = ABS(QTYQTR(START%) + QTYQTR(START% - 1%) + &
		QTYQTR(START% - 2%) + &
		QTYQTR(START% - 3%) + QTYQTR(START% - 4%))
	IF USAGE <= USAGE_LEVEL AND (ON_HAND <> 0.0 OR USAGE > 0.0)
	THEN
		!
		! Print out one line
		!
		TEXT$ = PD_PRODUCT::PRODUCT_NUM + " " + &
			LEFT(PD_PRODUCT::DESCRIPTION, 20%) + " " + &
			PD_PRODUCT::PROD_TYPE + " " + &
			PD_PRODUCT::CATEGORY + " " + &
			PD_PRODUCT::SECONDARY_CODE + "  " + &
			FORMAT$(QTYQTR(START% - 4%), "###,### ") + " " + &
			FORMAT$(QTYQTR(START% - 3%), "###,### ") + " " + &
			FORMAT$(QTYQTR(START% - 2%), "###,### ") + " " + &
			FORMAT$(QTYQTR(START% - 1%), "###,### ") + " " + &
			FORMAT$(QTYQTR(START%), "###,### ") + " " + &
			FORMAT$(ON_HAND, "##,###") + &
			FORMAT$(COST, "########.##") + &
			FORMAT$(COST * ON_HAND, "##########.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	GOSUB InitArray

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

 InitArray:
	FOR J% = 1% TO 8%
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
