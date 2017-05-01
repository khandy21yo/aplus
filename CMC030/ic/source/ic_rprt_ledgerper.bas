1	%TITLE "Inventory Ledger Period Report"
	%SBTTL "IC_RPRT_LEDGERPER"
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
	! ID:IC012
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Period Ledger\* report prints a report containing the transactions
	!	during the period. All active products will be listed in this report.
	!	This report contains the following fields:
	!	.table 3,25
	!	.te
	!	Product Number	Description
	!	.te
	!	Transaction Date	Primary Reference
	!	.te
	!	Secondary Reference	Cross Reference
	!	.te
	!	SubAccount	Location
	!	.te
	!	On Hand	Sales Order
	!	.te
	!	Allocated	On Order
	!	.te
	!	Back Order
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Period Ledger
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_RPRT_LEDGERPER/LINE
	!	$ LINK/EXE=IC_EXE: IC_RPRT_LEDGERPER, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE IC_RPRT_LEDGERPER.OBJ;*
	!
	! Author:
	!
	!	09/22/87 - Frank F. Starman
	!
	! Modification History:
	!
	!	04/01/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	07/30/96 - Kevin Handy
	!		Reformat source code.
	!
	!	09/05/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/28/97 - Kevin Handy
	!		Use integer for #key
	!
	!	07/30/97 - Kevin Handy
	!		Change XAGE parameter of READ_PERIOD to integer
	!
	!	04/21/98 - Kevin Handy
	!		Reformat source code
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/24/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	10/31/2000 - Kevin Handy
	!		Use A"x"B
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

	%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.HB"
	MAP	(IC_TRANSACTION) IC_TRANSACTION_CDD	IC_TRANSACTION

	%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.HB"
	MAP	(IC_CONTROL)	IC_CONTROL_CDD		IC_CONTROL

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_TRANSTYPE.HB"
	MAP	(UTL_TRANSTYPE) UTL_TRANSTYPE_CDD	UTL_TRANSTYPE

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP	(PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	COM (CH_UTL_TRANSTYPE) UTL_TRANSTYPE.CH%

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION IC_READ_35BALANCE

	DECLARE STRING TRANS_TYPE(5%)

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
	!	The ^*From Item\* setting enters an
	!	item number with which the report will begin printing.
	!	.b
	!	A blank setting will cause the report to begin with the
	!	first item number in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Period Ledger
	!	.x Period Ledger>From Item
	!	.x Item>From
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field ends the report
	!	with a selected item number by entering the selection in this
	!	field.
	!	.b
	!	A blank setting will cause the report to end with the last
	!	item number in the file.
	!	.lm -5
	!
	! Index:
	!	.x Item>To
	!	.x To Item>Period Ledger Report
	!	.x Period Ledger Report>To Item
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects designated
	!	item numbers to be printed by entering a "wildcard"
	!	in this field.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Period Ledger Report
	!	.x Period Ledger Report>Wildcard
	!
	!--

	SORT_BY$ = EDIT$(UTL_REPORTX::OPTDEF(9%), 132%)

	!++
	! Abstract:FLD10
	!	^*(10) Sort\*
	!	.b
	!	.lm +5
	!	The ^*Sort (L,P)\* field indicates
	!	the order in which the report is to print.
	!	.b
	!	Valid settings are:
	!	.table 3,25
	!	.te
	!	^*L\* - Location
	!	.te
	!	^*P\* - Product Type
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Sort>Period Ledger Report
	!	.x Period Ledger Report>Sort
	!
	!--


	SELECT SORT_BY$
	CASE "L"
		SORT_KEY% = 4%
	CASE "P"
		SORT_KEY% = 0%
	END SELECT

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
		CONTINUE ReportTitle IF ERR = 5%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "INVENTORY  PERIOD  LEDGER  " + YYYYPP$ + "  " + &
		PERIOD_DESCR$
	TITLE$(2%) = "Inventory control system"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "Product#       Description              " + &
		"                Loc         OnHand     " + &
		"   Alloc        OnOrder"
	TITLE$(5%) = "           TransDate  PrimRef        " + &
		"   CrossRef   SubAcct"
	TITLE$(6%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #IC_TRANSACTION.CH%, KEY #SORT_KEY%
		ELSE
			FIND #IC_TRANSACTION.CH%, &
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
		GET #IC_TRANSACTION.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "IC_TRANSACTION"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	SELECT SORT_BY$

	CASE "L"
		GOTO ExitTotal IF (IC_TRANSACTION::LOCATION > TO_ITEM$) AND &
			TO_ITEM$ <> ""
		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(IC_TRANSACTION::LOCATION, -1%), &
			WLDCRD$) = 0%

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), " ", 2%) &
			IF TEST_LOCATION$ <> IC_TRANSACTION::LOCATION AND &
			TEST_LOCATION$ <>""

		TEST_LOCATION$ = IC_TRANSACTION::LOCATION

	CASE "P"
		GOTO ExitTotal IF (IC_TRANSACTION::PRODUCT > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(IC_TRANSACTION::PRODUCT, -1%), &
			WLDCRD$) = 0%

	END SELECT

17300	!
	! Print out one line
	!
	GOTO 17320 IF TEST_PRODUCT_LOCATION$ = IC_TRANSACTION::PRODUCT + &
		IC_TRANSACTION::LOCATION

	IF TEST_PRODUCT_LOCATION$ <> ""
	THEN
		TEXT$ = SPACE$(62%) + &
			FORMAT$(TOTAL(1%), "########.###") + "  " + &
			FORMAT$(TOTAL(2%), "########.###") + "  " + &
			FORMAT$(TOTAL(3%), "########.###")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 1%)

	END IF

	PD_PRODUCT::DESCRIPTION = &
		STRING$(LEN(PD_PRODUCT::DESCRIPTION), A"?"B)

	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, &
			KEY #0% EQ IC_TRANSACTION::PRODUCT, &
			REGARDLESS
	USE
		CONTINUE 17305 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

17305	GOTO GetNextRec IF IC_READ_35BALANCE(IC_TRANSACTION::PRODUCT, &
		IC_TRANSACTION::LOCATION, BALANCE(,)) <> CMC$_NORMAL

	ONHAND = BALANCE(1%, 1%)
	ALLOC   = BALANCE(2%, 1%)
	ONORDER = BALANCE(3%, 1%)

17310	TEXT$ = IC_TRANSACTION::PRODUCT + " " + &
		PD_PRODUCT::DESCRIPTION + " " + &
		IC_TRANSACTION::LOCATION + "  " + &
		FORMAT$(ONHAND, "########.###") + "  " + &
		FORMAT$(ALLOC, "########.###") + "  " + &
		FORMAT$(ONORDER, "########.###")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)

	TOTAL(1%) = ONHAND
	TOTAL(2%) = ALLOC
	TOTAL(3%) = ONORDER

17320	UTL_TRANSTYPE::CLASS = STRING$(LEN(UTL_TRANSTYPE::CLASS), A"0"B)
	GOTO 17330 IF IC_TRANSACTION::QUANTITY_A = 0.0
	WHEN ERROR IN
		GET #UTL_TRANSTYPE.CH%, &
			KEY #0% EQ IC_TRANSACTION::TYPE_A, &
			REGARDLESS
	USE
		CONTINUE 17325
	END WHEN

17325	IND% = VAL%(UTL_TRANSTYPE::CLASS)
	TRANS_TYPE(IND%)= IC_TRANSACTION::TYPE_A
	QUANTITY(IND%) = QUANTITY(IND%) + IC_TRANSACTION::QUANTITY_A
	TOTAL(IND%) = TOTAL(IND%) + FUNC_ROUND(IC_TRANSACTION::QUANTITY_A, 3%)

	GOSUB 18000

17330	UTL_TRANSTYPE::CLASS = STRING$(LEN(UTL_TRANSTYPE::CLASS), A"0"B)
	GOTO 17350 IF IC_TRANSACTION::QUANTITY_B = 0.0
	WHEN ERROR IN
		GET #UTL_TRANSTYPE.CH%, &
			KEY #0% EQ IC_TRANSACTION::TYPE_B, &
			REGARDLESS
	USE
		CONTINUE 17335
	END WHEN

17335	IND% = VAL%(UTL_TRANSTYPE::CLASS)
	TRANS_TYPE(IND%) = IC_TRANSACTION::TYPE_B
	QUANTITY(IND%) = QUANTITY(IND%) + IC_TRANSACTION::QUANTITY_B
	TOTAL(IND%) = TOTAL(IND%) + FUNC_ROUND(IC_TRANSACTION::QUANTITY_B, 3%)

	GOSUB 18000

17350	!
	! Try for next record
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT
	TEST_PRODUCT_LOCATION$ = IC_TRANSACTION::PRODUCT + &
		IC_TRANSACTION::LOCATION
	GOTO GetNextRec

 ExitTotal:
17400	!
	! Handle end of report
	!
	IF TEST_PRODUCT_LOCATION$ <> ""
	THEN
		TEXT$ = SPACE$(62%) + &
			FORMAT$(TOTAL(1%), "########.###") + "  " + &
			FORMAT$(TOTAL(2%), "########.###") + "  " + &
			FORMAT$(TOTAL(3%), "########.###")
	END IF

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

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

18000	TEXT$ = SPACE$(11%) + &
		PRNT_DATE(IC_TRANSACTION::TRANS_DATE, 8%) +  " " + &
		IC_TRANSACTION::PRIMARY_REF + "  " + &
		IC_TRANSACTION::CROSS_REF + " " + &
		IC_TRANSACTION::SUBACCOUNT + " " + &
		FORMAT$(QUANTITY(1%), "<%>#######.###") + &
		TRANS_TYPE(1%) + &
		FORMAT$(QUANTITY(2%), "<%>#######.###") + &
		TRANS_TYPE(2%) + &
		FORMAT$(QUANTITY(3%), "<%>#######.###") + &
		TRANS_TYPE(3%) + &
		FORMAT$(QUANTITY(4%), "<%>#######.###") + &
		TRANS_TYPE(4%) + &
		FORMAT$(QUANTITY(5%), "<%>#######.###") + &
		TRANS_TYPE(5%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)

	!
	! Clean quantity and transaction types
	!

	FOR I% = 1% TO 5%
		QUANTITY(I%) = 0.0
		TRANS_TYPE(I%) = "  "
	NEXT I%

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
