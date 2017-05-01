1	%TITLE "Stock Value Report"
	%SBTTL "IC_RPRT_STOCKVALUE_PERIOD"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1999 BY
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
	! ID:IC026
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Stock Value Report\*
	!	prints the
	!	value of the inventory depending
	!	on the costing method being used.
	!	The ^*Stock Value Report\*
	!	contains the following fields:
	!	.table 3,25
	!	.te
	!	Product Number	Description
	!	.te
	!	Type	Category
	!	.te
	!	Posted On Hand	Cost Method
	!	.te
	!	Standard Cost	Amount
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Stock Value>Report
	!	.x Report>Stock Value
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_RPRT_STOCKVALUE_PERIOD/LINE
	!	$ LINK/EXE=IC_EXE: IC_RPRT_STOCKVALUE_PERIOD, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE IC_RPRT_STOCKVALUE_PERIOD.OBJ;*
	!
	! Author:
	!
	!	05/06/99 - Kevin Handy
	!		Based on IC_RPRT_STOCKVALUE report
	!
	! Modification History:
	!
	!	05/08/99 - Kevin Handy
	!		Watch location when looking at transactions
	!
	!	06/03/99 - Kevin Handy
	!		Trap undefined transaction type errors
	!
	!	04/28/2000 - Kevin Handy
	!		Added PERIOD to the title
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

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP	(PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP	(UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.HB"
	MAP	(IC_TRANSACTION) IC_TRANSACTION_CDD	IC_TRANSACTION

	%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.HB"
	MAP	(IC_CONTROL)	IC_CONTROL_CDD		IC_CONTROL

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_TRANSTYPE.HB"
	MAP (UTL_TRANSTYPE)	UTL_TRANSTYPE_CDD	UTL_TRANSTYPE

	COM (CH_UTL_TRANSTYPE_READ) UTL_TRANSTYPE.CH%

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

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\*
	!	field causes the
	!	report to begin printing
	!	with a selected item number.
	!	The value entered must be in agreement
	!	with the value in field (10)
	!	Sort by.
	!	.b
	!	A blank field will cause the report
	!	to begin with the first
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Stock Value Report
	!	.x Stock Value Report>From Item
	!	.x Item>From
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field causes
	!	the report to end printing
	!	with a selected item number.
	!	The value entered must be in agreement
	!	with the value in field (10)
	!	Sort by.
	!	.b
	!	A blank setting will cause the report to end with the
	!	last item in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Stock Value Report
	!	.x Stock Value Report>To Item
	!	.x Item>To
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field
	!	selects designated items
	!	to be printed by entering
	!	a "wildcard" value.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Stock Value Report
	!	.x Stock Value Report>Wildcard
	!
	!--

	LOCATION$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Locations\*
	!	.b
	!	.lm +5
	!	The ^*Locations\* field
	!	enters the location
	!	codes (which have been established
	!	in the Utility system) that are
	!	to be printed.
	!	.lm -5
	!
	! Index:
	!	.x Locations>Stock Value Report
	!	.x Stock Value Report>Locations
	!
	!--

	SORT_BY$ = EDIT$(UTL_REPORTX::OPTDEF(9%), 132%)

	!++
	! Abstract:FLD10
	!	^*(10) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field
	!	prints the
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
	!	^*T\* - Product Type
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Sort by>Stock Value Report
	!	.x Stock Value Report>Sort by
	!
	!--

300	!
	WHEN ERROR IN
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
		%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.OPN"
	USE
		FILENAME$ = "IC_TRANSACTION"
		CONTINUE HelpError
	END WHEN

320	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_TRANSTYPE.OPN"
	USE
		CONTINUE 330 IF ERR = 5%
		FILENAME$ = "UTL_TRANSTYPE"
		CONTINUE HelpError
	END WHEN

330	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

340	WHEN ERROR IN
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

	TITLE$(1%) = "INVENTORY  STOCK  VALUE  " + ADD_TITLE$
	TITLE$(3%) = "Inventory Control System Period " + YYYYPP$
	TITLE$(4%) = ""

	!
	! Heading
	!
	TITLE$(5%) = "                                        " + &
		"                   Cost       STD                       " + &
		"                Extended"
	TITLE$(6%) = "Product#       Description              " + &
		"       UOM Tp Cat  Method    Cost            BegBal     " + &
		"    EndBal        Cost"
	TITLE$(7%) = "."
	TITLE$(8%) = ""

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		RESET #UTL_LOCATION.CH%
	USE
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

 NextLocation:
	WHEN ERROR IN
		GET #UTL_LOCATION.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

	GOTO NextLocation IF LOCATION$ <> "" AND &
		COMP_STRING(EDIT$(UTL_LOCATION::LOCATION, -1%), LOCATION$) = 0%

	IF PRINT_LINE%
	THEN
		GOSUB 18100
		LIN% = 999%
	END IF

	PRINT_LINE% = 0%

	TITLE$(2%) = "AT LOCATION " + UTL_LOCATION::LOCATION + " " + &
		UTL_LOCATION::LOCNAME

	SUBTOTAL_COST = 0.0

17010	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PD_PRODUCT.CH%, KEY #SORT_KEY%
		ELSE
			FIND #PD_PRODUCT.CH%, &
				KEY #SORT_KEY% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE NextLocation IF ERR = 155%
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
		CONTINUE NextLocation IF ERR = 11%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO GetNextRec IF PD_PRODUCT::SSTATUS <> "A"

	SELECT SORT_BY$

	CASE "C"
		GOTO NextLocation IF (PD_PRODUCT::CATEGORY > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		IF TEST_CATEGORY$ <> PD_PRODUCT::CATEGORY AND &
			TEST_CATEGORY$ <>"" AND PRINT_LINE%
		THEN
			GOSUB 18100
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
			PRINT_LINE% = 0%
		END IF

		TEST_CATEGORY$ = PD_PRODUCT::CATEGORY

	CASE "D"
		GOTO NextLocation IF (PD_PRODUCT::DESCRIPTION > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(PD_PRODUCT::DESCRIPTION, -1%), &
			WLDCRD$) = 0%

	CASE "P"
		GOTO NextLocation IF (PD_PRODUCT::PRODUCT_NUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(PD_PRODUCT::PRODUCT_NUM, -1%), &
			WLDCRD$) = 0%

	CASE "T"
		GOTO NextLocation IF (PD_PRODUCT::PROD_TYPE> TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(PD_PRODUCT::PROD_TYPE, -1%), &
			WLDCRD$) = 0%

		IF TEST_PRODTYPE$ <> PD_PRODUCT::PROD_TYPE AND &
			TEST_PRODTYPE$ <>"" AND PRINT_LINE%
		THEN
			GOSUB 18100
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
			PRINT_LINE% = 0%
		END IF

		TEST_PRODTYPE$ = PD_PRODUCT::PROD_TYPE

	END SELECT

17300
	!
	! Print out one line
	!
	GOTO GetNextRec IF IC_READ_35BALANCE(PD_PRODUCT::PRODUCT_NUM, &
		UTL_LOCATION::LOCATION, BALANCE(,)) <> CMC$_NORMAL

	BEGBAL = BALANCE(1%, 1%)

	ENDBAL = BEGBAL
	GOSUB GetTransactions

	GOTO 17350 IF FUNC_ROUND(BEGBAL, 4%) = 0.0 AND &
		FUNC_ROUND(ENDBAL, 4%) = 0.0

	IF PD_PRODUCT::METHOD = "STD"
	THEN
		COST = PC_READ_COST(PD_PRODUCT::PRODUCT_NUM, &
			UTL_LOCATION::LOCATION, DATE_TODAY, "")
	ELSE
		COST = 0.0
	END IF

	EXTCOST = FUNC_ROUND(COST * ENDBAL, 2%)

	SPAC$ = PD_PRODUCT::UOM + "  " + &
		PD_PRODUCT::PROD_TYPE + " " + &
		PD_PRODUCT::CATEGORY + " " + &
		PD_PRODUCT::METHOD + "  " + &
		FORMAT$(COST, "<%>#,###.##   ") + &
		FORMAT$(BEGBAL, "##,###,###,###") + " " + &
		FORMAT$(ENDBAL, "##,###,###,###") + " "

	TEXT_LIN$ = PD_PRODUCT::PRODUCT_NUM + " " + &
		LEFT(PD_PRODUCT::DESCRIPTION, 31%) + " " + &
		SPAC$ + &
		FORMAT$(EXTCOST, "##,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT_LIN$, LIN%)
	LIN% = 0%

	TOTAL_COST = TOTAL_COST + EXTCOST
	SUBTOTAL_COST = SUBTOTAL_COST + EXTCOST

	PRINT_LINE% = -1%

17350	!
	! Try for next record
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT
	GOTO GetNextRec

 ExitTotal:
	!
	! Handle end of report
	!
	GOSUB 18100 IF PRINT_LINE%

	GOSUB 18000

 ExitProgram:
	CALL OUTP_FINISH(UTL_REPORTX)

	!
	! Exit to next program or menu
	!
	IF TRM$(UTL_REPORTX::NEXTRUN) = ""
	THEN
		CALL SUBR_3EXITPROGRAM(SCOPE, "", "")
	ELSE
		CALL SUBR_3EXITPROGRAM(SCOPE, &
			"RUN " + UTL_REPORTX::NEXTRUN, "")
	END IF

	%PAGE

18000	GOTO Ret18000 IF UTL_REPORTX::PAGENO = 0%

	!
	! Print total cost
	!
	TEXT$ = SPACE$(LEN(PD_PRODUCT::PRODUCT_NUM)) + " " + &
		"T O T A L" + SPACE$(23% + LEN(SPAC$)) + &
		FORMAT$(TOTAL_COST, "##,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

 Ret18000:
	RETURN

18100	!
	! Print subtotal cost
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	TEXT$ = SPACE$(LEN(PD_PRODUCT::PRODUCT_NUM)) + " " + &
		"TOTAL      " + &
		SPACE$(21% + LEN(SPAC$)) + &
		FORMAT$(SUBTOTAL_COST, "##,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	RETURN

 GetTransactions:
18300	!*******************************************************************
	! Scan all transactions for this period
	!*******************************************************************

	WHEN ERROR IN
		FIND #IC_TRANSACTION.CH%, &
			KEY #0% EQ PD_PRODUCT::PRODUCT_NUM + UTL_LOCATION::LOCATION, &
			REGARDLESS
	USE
		CONTINUE 18390
	END WHEN

18310	WHEN ERROR IN
		GET #IC_TRANSACTION.CH%, REGARDLESS
	USE
		CONTINUE 18390
	END WHEN

	GOTO 18390 IF IC_TRANSACTION::PRODUCT <> PD_PRODUCT::PRODUCT_NUM
	GOTO 18390 IF IC_TRANSACTION::LOCATION <> UTL_LOCATION::LOCATION

18320	!
	! Transaction A
	!
	GOTO 18330 IF IC_TRANSACTION::QUANTITY_A = 0.0
	IF (UTL_TRANSTYPE::CODE <> IC_TRANSACTION::TYPE_A)
	THEN
		WHEN ERROR IN
			GET #UTL_TRANSTYPE.CH%, &
				KEY #0% EQ IC_TRANSACTION::TYPE_A, &
				REGARDLESS
		USE
			TTYPE$ = IC_TRANSACTION::TYPE_A
			CONTINUE 18400
		END WHEN
	END IF

18325	IND% = VAL%(UTL_TRANSTYPE::CLASS)

	IF IND% = 1%
	THEN
		ENDBAL = ENDBAL + IC_TRANSACTION::QUANTITY_A
	END IF

18330	!
	! Transaction B
	!
	GOTO 18350 IF IC_TRANSACTION::QUANTITY_B = 0.0
	IF (UTL_TRANSTYPE::CODE <> IC_TRANSACTION::TYPE_B)
	THEN
		WHEN ERROR IN
			GET #UTL_TRANSTYPE.CH%, &
				KEY #0% EQ IC_TRANSACTION::TYPE_B, &
				REGARDLESS
		USE
			TTYPE$ = IC_TRANSACTION::TYPE_B
			CONTINUE 18400
		END WHEN
	END IF

18335	IND% = VAL%(UTL_TRANSTYPE::CLASS)

	IF IND% = 1%
	THEN
		ENDBAL = ENDBAL + IC_TRANSACTION::QUANTITY_B
	END IF

18350	GOTO 18310

18390	RETURN

18400	!
	! Undefined transaction type
	!
	CALL ENTR_3MESSAGE(SCOPE, "****Undefined tran type Part:" + &
		IC_TRANSACTION::PRODUCT + " Type: " + TTYPE$ + &
		" Date: " + IC_TRANSACTION::TRANS_DATE + &
		" Ref: " + IC_TRANSACTION::PRIMARY_REF, 0%)
	GOTO 18350

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
