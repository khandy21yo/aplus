1	%TITLE "Inventory Transaction File List"
	%SBTTL "IC_RPRT_TRANSACTION"
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
	! ID:IC010
	!
	! Abstract:HELP
	!	.B
	!	.LM +5
	!	The ^*Inventory Transaction File List\* prints
	!	the Period Ledger File for the specified period.
	!	The following fields are included:
	!	.TABLE 3,25
	!	.te
	!	Product Number	Description
	!	.te
	!	Primary Reference	Secondary Reference
	!	.te
	!	Cross Reference	Location
	!	.te
	!	SubAccount	Lot
	!	.te
	!	Transaction Date	Account
	!	.te
	!	Station Man	Post Date
	!	.te
	!	Cost	Post Time
	!	.te
	!	Price	Batch
	!	.te
	!	Transaction Type	Quantity
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Inventory Transaction File List
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_RPRT_TRANSACTION/LINE
	!	$ LINK/EXE=IC_EXE: IC_RPRT_TRANSACTION, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE IC_RPRT_TRANSACTION.OBJ;*
	!
	! Author:
	!
	!	09/10/88 - J. Shad Rydalch
	!
	! Modification History:
	!
	!	12/26/91 - Dan Perkins
	!		Changed Sort By option to match defined keys.
	!		Use PD_EXAM_PRODUCT function to get product description.
	!
	!	01/16/92 - Dan Perkins
	!		Changed quantities to display integer values.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/05/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/21/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.HB"
	MAP (IC_TRANSACTION)	IC_TRANSACTION_CDD	IC_TRANSACTION

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION PD_EXAM_PRODUCT

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
	!	The ^*From Item\* field
	!	begins printing with a selected item number.
	!	The value entered must be in agreement with field (10)
	!	Sort by.
	!	.b
	!	A blank field will cause the report to begin with the first
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Inventory Transaction File List
	!	.x Inventory Transaction File List>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\*
	!	ends printing with a selected item number.
	!	The value entered must be in agreement with field
	!	(10) Sort by.
	!	.b
	!	A blank setting will cause the report to end with the
	!	last item in the file.
	!
	! Index:
	!	.x To Item>Inventory Transaction File List
	!	.x Inventory Transaction File List>To Item
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects designated items
	!	to be printed by entering a "wildcard" value.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Inventory Transaction File List
	!	.x Inventory Transaction File List>Wildcard
	!
	!--

	YYYYPP$ = EDIT$(UTL_REPORTX::OPTDEF(5%), 132%)

	!++
	! Abstract:FLD06
	!	^*(06) Period\*
	!	.b
	!	.lm +5
	!	The ^*Period\* field enters the period number for which
	!	the report will be printed.
	!	.lm -5
	!
	! Index:
	!	.x Period
	!
	!--

	SORT_BY$ = EDIT$(UTL_REPORTX::OPTDEF(9%), 132%)

	!++
	! Abstract:FLD10
	!	^*(10) Sort\*
	!	.b
	!	.lm +5
	!	The Sort field causes the report
	!	to print in a selected order.
	!	.b
	!	Valid settings are:
	!	.table 3,25
	!	.te
	!	^*B\* - Posting Batch
	!	.te
	!	^*C\* - Cross Reference
	!	.te
	!	^*L\* - Location
	!	.te
	!	^*P\* - Product Number
	!	.te
	!	^*S\* - Subaccount
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Sort>Inventory Transaction File List
	!	.x Inventory Transaction File List>Sort
	!
	!--

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.OPN"
	USE
		FILENAME$ = "IC_TRANSACTION"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	SELECT SORT_BY$

	CASE "B"
		SORT_KEY% = 1%
		ADD_TITLE$ = "BY  BATCH  NUMBER"

	CASE "C"
		SORT_KEY% = 3%
		ADD_TITLE$ = "BY  CROSS  REFERENCE"

	CASE "L"
		SORT_KEY% = 4%
		ADD_TITLE$ = "BY  LOCATION"

	CASE "P"
		SORT_KEY% = 0%
		ADD_TITLE$ = "BY  PRODUCT NUMBER"

	CASE "S"
		SORT_KEY% = 2%
		ADD_TITLE$ = "BY  SUBACCOUNT"
	END SELECT

	TITLE$(1%) = "INVENTORY  TRANSACTION  FILE  LIST  " + YYYYPP$ + &
		"  " + ADD_TITLE$

	TITLE$(2%) = "Inventory Control System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "Product#       Description              " + &
		"          Loc  TranDate Account                  Cost" + &
		"     Price  TA   QuanityA TB   QuanityB"

	TITLE$(5%) = "               PrimRef           CrossRef   " + &
		"SubAccount Lot        StationMan PostDate   PostTime Batch "

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
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "IC_TRANSACTION"
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
		GET #IC_TRANSACTION.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "IC_TRANSACTION"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	SELECT SORT_BY$

	CASE "B"
		GOTO ExitProgram IF (IC_TRANSACTION::BATCH > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(IC_TRANSACTION::BATCH, -1%), &
			WLDCRD$) = 0%

	CASE "C"
		GOTO ExitProgram IF (IC_TRANSACTION::CROSS_REF > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(IC_TRANSACTION::CROSS_REF, -1%), &
			WLDCRD$) = 0%

	CASE "L"
		GOTO ExitProgram IF (IC_TRANSACTION::LOCATION > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(IC_TRANSACTION::LOCATION, -1%), &
			WLDCRD$) = 0%

	CASE "P"
		GOTO ExitProgram IF (IC_TRANSACTION::PRODUCT > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(IC_TRANSACTION::PRODUCT, -1%), &
			WLDCRD$) = 0%

	CASE "S"
		GOTO ExitProgram IF (IC_TRANSACTION::SUBACCOUNT > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(IC_TRANSACTION::SUBACCOUNT, -1%), &
			WLDCRD$) = 0%

	END SELECT

	!
	! Get product description
	!
	V% = PD_EXAM_PRODUCT(IC_TRANSACTION::PRODUCT, PD_PRODUCT_EXAM)

	!
	! Print out one line
	!
	TEXT$ = IC_TRANSACTION::PRODUCT + " " + &
		LEFT$(PD_PRODUCT_EXAM::DESCRIPTION, 34%) + " " + &
		IC_TRANSACTION::LOCATION + " " + &
		PRNT_DATE(IC_TRANSACTION::TRANS_DATE, 6%) + " " + &
		IC_TRANSACTION::TRANSACCT + " " + &
		FORMAT$(IC_TRANSACTION::COST, "#######.##") + " " + &
		FORMAT$(IC_TRANSACTION::PRICE, "#######.##") + " " + &
		IC_TRANSACTION::TYPE_A + " " + &
		FORMAT$(IC_TRANSACTION::QUANTITY_A, "##########") + " " + &
		IC_TRANSACTION::TYPE_B + " " + &
		FORMAT$(IC_TRANSACTION::QUANTITY_B, "##########")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "               " + &
		IC_TRANSACTION::PRIMARY_REF + "  " + &
		IC_TRANSACTION::CROSS_REF + " " + &
		IC_TRANSACTION::SUBACCOUNT + " " + &
		IC_TRANSACTION::LOT + " " + &
		IC_TRANSACTION::STATIONMAN + " " + &
		PRNT_DATE(IC_TRANSACTION::POSTDATE, 8%) + " " + &
		PRNT_TIME(IC_TRANSACTION::POSTTIME, 0%) + " " + &
		IC_TRANSACTION::BATCH

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

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
