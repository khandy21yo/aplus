1	%TITLE "Inventory Summary Ledger Report"
	%SBTTL "IC_RPRT_LEDGERSUM"
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
	! ID:IC013
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Summary Ledger\* report prints a report containing all products and
	!	all transactions for the period, including period inactive products. A
	!	beginning and ending balance for each product will also be included.
	!	The fields contained in this report include:
	!	.table 3,25
	!	.te
	!	Product Number	Description
	!	.te
	!	Transaction Date	Primary Reference
	!	.te
	!	Secondary Reference	Cross Reference
	!	.te
	!	SubAccount	Type
	!	.te
	!	Category	On Hand
	!	.te
	!	Sales Order	Allocated
	!	.te
	!	On Order	Back Order
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Summary Ledger>Report
	!	.x Report>Summary Ledger
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_RPRT_LEDGERSUM/LINE
	!	$ LINK/EXE=IC_EXE: IC_RPRT_LEDGERSUM, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE IC_RPRT_LEDGERSUM.OBJ;*
	!
	! Author:
	!
	!	06/10/88 - Frank F. Starman
	!
	! Modification History:
	!
	!	10/29/91 -  Frank F. Starman
	!		Change GOTO from transaction B.
	!
	!	05/14/92 - Dan Perkins
	!		Added sort by Secondary Code.  Subtotals for
	!		report on Category and Type.
	!
	!	06/15/92 - Kevin Handy
	!		Clean up (check)
	!
	!	08/10/92 -  Frank F. Starman
	!		Added adjustment column.
	!
	!	11/23/92 - Dan Perkins
	!		Changed location filed to location wildcard.
	!		Added code to print by location.
	!
	!	12/01/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	01/02/92 -  Frank F. Starman
	!		Test if there are any transaction in a location before
	!		reading PD_PRODUCT file.
	!
	!	01/08/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/01/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	07/07/93 -  Frank F. Starman
	!		Fixed bug to print subtotals if needed.
	!
	!	04/13/95 - Kevin Handy
	!		V3.6
	!		Update to V3.6 source code format.
	!		Modified to put a blank line after each product,
	!		and not just those with a TOTAL_
	!		Modified to optionally print three blank lines between
	!		each product.
	!
	!	01/29/96 - Kevin Handy
	!		Reformat source code.
	!		Change STRING$(...,ASCII(" ")) to SPACE$(...) in
	!		several places.
	!
	!	04/01/96 - Kevin Handy
	!		Reformat source code.
	!
	!	11/02/96 - Kevin Handy
	!		Loose goofy formatting in 'text$=' lines.
	!
	!	07/30/97 - Kevin Handy
	!		Change XAGE parameter of READ_PERIOD to integer
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/24/2000 - Kevin Handy
	!		Look in IC_35BALANCE to test for the existance of
	!		any data for a location instead of IC_TRANSACTION
	!		which may not have data for one period.
	!
	!	03/24/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	10/31/2000 - Kevin Handy
	!		Use A"x"B
	!
	!	03/30/2005 - Kevin Handy
	!		Added a bunch of LOG_FLAG settings to make sure that
	!		it will page between locations, instead of believing
	!		that nothing was printed for the location.
	!
	!	03/30/2005 - Kevin Handy
	!		Added "Fix Cost" option.
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

	%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.HB"
	MAP (IC_CONTROL)	IC_CONTROL_CDD		IC_CONTROL

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_TRANSTYPE.HB"
	DECLARE			UTL_TRANSTYPE_CDD	UTL_TRANSTYPE_EXAM

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[PD.OPEN]PD_ACCOUNT.HB"
	DECLARE			PD_ACCOUNT_CDD		PD_ACCOUNT_READ

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	DECLARE			GL_CHART_CDD		GL_CHART_EXAM

	%INCLUDE "SOURCE:[GL.OPEN]POST_TO_GL.HB"
	MAP (IC_TOTAL)		POST_TO_GL_CDD		IC_TOTAL

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[IC.OPEN]IC_35BALANCE.HB"
	MAP (IC_35BALANCE)	IC_35BALANCE_CDD	IC_35BALANCE

	COM (CH_IC_35BALANCE_READ) IC_35BALANCE.CH%

	!
	! External functions
	!
	EXTERNAL REAL    FUNCTION PC_READ_COST
	EXTERNAL LONG    FUNCTION IC_READ_35BALANCE
	EXTERNAL LONG    FUNCTION PD_READ_ACCOUNT
	EXTERNAL LONG    FUNCTION GL_EXAM_CHART
	EXTERNAL LONG    FUNCTION UTL_EXAM_TRANSTYPE

	DECLARE STRING TRANS_TYPE(3%)

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
	!	The ^*From Item\* field begins the
	!	report printing with a selected item number.
	!	.b
	!	A blank setting will cause the report to begin with the
	!	first item number in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Summary Ledger Report
	!	.x Summary Ledger Report>From Item
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
	!	printing with a selected item number.
	!	.b
	!	A blank setting in this field will cause the report to end
	!	with the last item number in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Summary Ledger Report
	!	.x Summary Ledger Report>To Item
	!	.x Item>To
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field designates
	!	item numbers to be printed
	!	by entering that value
	!	in the "wildcard" field.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Summary Ledger Report
	!	.x Summary Ledger Report>Wildcard
	!
	!--

	LOCATION$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Location Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Location Wildcard\* field designates
	!	the location codes
	!	(which have been established in the Utility system)
	!	that are to be printed.
	!	.lm -5
	!
	! Index:
	!	.x Location Number>Summary Ledger Report
	!	.x Summary Ledger Report>Location Number
	!	.x Number>Location
	!
	!--

	ZERO$ = EDIT$(UTL_REPORTX::OPTDEF(5%), 132%)

	!++
	! Abstract:FLD06
	!	^*(06) Print Zero\*
	!	.b
	!	.lm +5
	!	The ^*Print Zero\* value indicates
	!	whether or not items showing a
	!	zero value are to be printed.
	!	.b
	!	Valid settings are:
	!	.table 3,25
	!	.te
	!	^*Y\* - Yes
	!	.te
	!	^*N\* - No
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Print Zero>Summary Ledger Report
	!	.x Summary Ledger Report>Print Zero
	!
	!--

	SUM_ONLY$ = EDIT$(UTL_REPORTX::OPTDEF(6%), 132%)

	!++
	! Abstract:FLD07
	!	^*(07) Print Only Summary Page\*
	!	.b
	!	.lm +5
	!	The ^*Summary Page Only\* field gives
	!	the option of printing
	!	only the Summary Page instead of
	!	the entire report.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Summary Page Only>Summary Ledger Report
	!	.x Summary Ledger Report>Summary Page Only
	!
	!--

	FIX_COST$ = EDIT$(UTL_REPORTX::OPTDEF(7%), 132%)

	!++
	! Abstract:FLD08
	!	^*(08) Fix zero costs\*
	!	.b
	!	.lm +5
	!	Determines if the report should fix items in the transaction
	!	journal that have a zero cost by re-calculating the cost.
	!	.lm -5
	!
	! Index:
	!	.x Summary Page Only>Summary Ledger Report
	!	.x Summary Ledger Report>Summary Page Only
	!
	!--

	WIDE_SPACE$ = LEFT(UTL_REPORTX::OPTDEF(8%), 1%)

	!++
	! Abstract:FLD09
	!	^*(09) Large Spacing\*
	!	.b
	!	This field specifies if it should triple
	!	space between products instead
	!	of single spacing.
	!
	! Index:
	!	.x Wide Space>Summary Ledger Report
	!	.x Summary Ledger Report>Wide Space
	!
	!--

	SORT_BY$ = EDIT$(UTL_REPORTX::OPTDEF(9%), 132%)

	!++
	! Abstract:FLD10
	!	^*(10) Sort\*
	!	.b
	!	.lm +5
	!	The ^*Sort\* field indicates the
	!	order in which the report is to print.
	!	.b
	!	Valid settings are:
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
	!	.x Sort>Summary Ledger Report
	!	.x Summary Ledger Report>Sort
	!
	!--

	SELECT SORT_BY$

	CASE "C"
		SORT_KEY% = 2%
		ADD_TITLE$ = "  BY  CATEGORY"

	CASE "D"
		SORT_KEY% = 3%
		ADD_TITLE$ = "  BY  DESCRIPTION"

	CASE "P"
		SORT_KEY% = 0%
		ADD_TITLE$ = "  BY  PRODUCT  NUMBER"

	CASE "S"
		SORT_KEY% = 4%
		ADD_TITLE$ = "  BY  PRODUCT  SECONDARY  CODE"

	CASE "T"
		SORT_KEY% = 1%
		ADD_TITLE$ = "  BY  PRODUCT  TYPE"

	END SELECT

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
		%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.OPN"
	USE
		FILENAME$ = "IC_TRANSACTION"
		CONTINUE HelpError
	END WHEN

320	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.OPN"
	USE
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

330	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

335	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_35BALANCE.OPN"
	USE
		FILENAME$ = "IC_35BALANCE"
		CONTINUE HelpError
	END WHEN

340	!
	! Open TEMP file
	!
	CALL ASSG_CHANNEL(IC_TOTAL_IDX%, STAT%)

	OPEN "IC_TOTAL_IDX" FOR OUTPUT AS FILE IC_TOTAL_IDX%, &
		ORGANIZATION INDEXED FIXED, &
		MAP IC_TOTAL, &
		BUFFER 32%, &
		TEMPORARY, &
		PRIMARY KEY IC_TOTAL::ACCT, &
		ALLOW NONE, &
		ACCESS MODIFY

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = EDIT$("INVENTORY  SUMMARY  LEDGER  " + YYYYPP$ + "  " + &
		PERIOD_DESCR$ + ADD_TITLE$, 16%)

	TITLE$(3%) = "Inventory Control System"

	TITLE$(4%) = ""

	!
	! Heading
	!
	IF SUM_ONLY$ = "Y"
	THEN
		TITLE$(5%) = "."
	ELSE
		TITLE$(5%) = "Product#       Description                   " + &
			"      Tp Cat  SecCode            OnHand     "  + &
			"      Alloc        OnOrder            Cost"

		TITLE$(6%) = "                TransDate  PrimRef        " + &
			"  CrossRef   SubAcct"

		TITLE$(7%) = "."
	END IF

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	WHEN ERROR IN
		RESET #UTL_LOCATION.CH%
	USE
		CONTINUE ExitTotal IF ERR = 11%
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
		COMP_ARRAY(EDIT$(UTL_LOCATION::LOCATION, -1%), LOCATION$) = 0%

17005	WHEN ERROR IN
		FIND #IC_35BALANCE.CH%, &
			KEY #1% EQ UTL_LOCATION::LOCATION, &
			REGARDLESS
	USE
		CONTINUE NextLocation IF ERR = 155%
		FILENAME$ = "IC_TRANSACTION"
		CONTINUE HelpError
	END WHEN

	TITLE$(2%) = "FOR LOCATION " + UTL_LOCATION::LOCATION

	LIN% = 999% IF LOC_FLAG%
	SOMETHING_PRINTED% = 0%

	!
	! Initialize some variables
	!
	FOR I% = 1% TO 3%
		QUANTITY(I%) = 0.0
		TRANS_TYPE(I%) = "  "
	NEXT I%

	SUB_BBAL = 0.0
	SUB_RUNNING = 0.0

	PRINT_LINE% = 0%
	BLANK_LINE% = 0%
	LOC_FLAG% = 0%

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
		CONTINUE NextLocation IF ERR = 11%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	!GOTO GetNextRec IF PD_PRODUCT::SSTATUS <> "A"

	SELECT SORT_BY$

	CASE "C"
		IF (PD_PRODUCT::CATEGORY > TO_ITEM$) AND &
			TO_ITEM$ <> ""
		THEN
			GOSUB Subtotals
			GOTO NextLocation
		END IF

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(PD_PRODUCT::CATEGORY, -1%), &
			WLDCRD$) = 0%

		IF TEST_CATEGORY$ <> PD_PRODUCT::CATEGORY &
			AND PRINT_LINE%
		THEN
			GOSUB Subtotals
		END IF

		TEST_CATEGORY$ = PD_PRODUCT::CATEGORY

	CASE "D"
		GOTO NextLocation IF (PD_PRODUCT::DESCRIPTION > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(PD_PRODUCT::DESCRIPTION, -1%), &
			WLDCRD$) = 0%

	CASE "P"
		GOTO NextLocation IF (PD_PRODUCT::PRODUCT_NUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(PD_PRODUCT::PRODUCT_NUM, -1%), &
			WLDCRD$) = 0%

	CASE "S"
		GOTO NextLocation &
			IF (PD_PRODUCT::SECONDARY_CODE > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(PD_PRODUCT::SECONDARY_CODE, -1%), &
			WLDCRD$) = 0%

	CASE "T"
		IF (PD_PRODUCT::PROD_TYPE > TO_ITEM$) AND &
			TO_ITEM$ <> ""
		THEN
			GOSUB Subtotals
			GOTO NextLocation
		END IF

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(PD_PRODUCT::PROD_TYPE, -1%), &
			WLDCRD$) = 0%

		IF TEST_PRODTYPE$ <> PD_PRODUCT::PROD_TYPE &
			AND PRINT_LINE%
		THEN
			GOSUB Subtotals
		END IF

		TEST_PRODTYPE$ = PD_PRODUCT::PROD_TYPE

	END SELECT

	!
	! Print out one line
	!
	GOTO GetNextRec IF IC_READ_35BALANCE(PD_PRODUCT::PRODUCT_NUM, &
		UTL_LOCATION::LOCATION, BALANCE(,)) <> CMC$_NORMAL

	SOMETHING_PRINTED% = 0%

	ONHAND = BALANCE(1%, 1%)
	ALLOC = BALANCE(2%, 1%)
	ONORDER = BALANCE(3%, 1%)

	ZERO_TOTAL = ABS(ONHAND) + ABS(ALLOC) + ABS(ONORDER)

	COST = PC_READ_COST(PD_PRODUCT::PRODUCT_NUM, UTL_LOCATION::LOCATION, &
		START_DATE$, "")

	COST = FUNC_ROUND(ONHAND * COST, 2%)

	END_COST = PC_READ_COST(PD_PRODUCT::PRODUCT_NUM, &
		UTL_LOCATION::LOCATION, &
		END_DATE$, "")

	ADJ_COST = FUNC_ROUND(ONHAND * END_COST, 2%)

	SUB_BBAL = SUB_BBAL + COST

	TEXT$ = PD_PRODUCT::PRODUCT_NUM + " " + &
		LEFT(PD_PRODUCT::DESCRIPTION, 35%) + " " + &
		PD_PRODUCT::PROD_TYPE + " " + &
		PD_PRODUCT::CATEGORY + " " + &
		PD_PRODUCT::SECONDARY_CODE + "    " + &
		FORMAT$(ONHAND, "#######.###") + "     " + &
		FORMAT$(ALLOC, "#######.###") + "    " + &
		FORMAT$(ONORDER, "#######.###") + "      " + &
		FORMAT$(COST, "#######.##")

	PRINT_LINE% = -1%

	IF ZERO$ = "Y" OR ZERO_TOTAL <> 0.0
	THEN
		IF SUM_ONLY$ <> "Y"
		THEN
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, LIN%)
			LOC_FLAG% = -1%
			LIN% = 0%
			SOMETHING_PRINTED% = -1%
		END IF
	END IF

	GOSUB 18040

	TOTAL(1%) = ONHAND
	TOTAL(2%) = ALLOC
	TOTAL(3%) = ONORDER
	TOTAL_COST = COST

17200	WHEN ERROR IN
		FIND #IC_TRANSACTION.CH%, &
			KEY #0% EQ PD_PRODUCT::PRODUCT_NUM + &
			UTL_LOCATION::LOCATION, &
			REGARDLESS
	USE
		CONTINUE 17250 IF ERR = 155%
		FILENAME$ = "IC_TRANSACTION"
		CONTINUE HelpError
	END WHEN

 NextTrans:
17210	WHEN ERROR IN
		GET #IC_TRANSACTION.CH%, REGARDLESS
	USE
		CONTINUE 17250 IF ERR = 11%
		FILENAME$ = "IC_TRANSACTION"
		CONTINUE HelpError
	END WHEN

	GOTO 17250 IF IC_TRANSACTION::PRODUCT <> PD_PRODUCT::PRODUCT_NUM OR &
		IC_TRANSACTION::LOCATION <> UTL_LOCATION::LOCATION

	GOTO 17230 IF IC_TRANSACTION::QUANTITY_A = 0.0

17220	GOTO 17230 IF UTL_EXAM_TRANSTYPE(IC_TRANSACTION::TYPE_A, &
		UTL_TRANSTYPE_EXAM) <> CMC$_NORMAL

17225	IND% = VAL%(UTL_TRANSTYPE_EXAM::CLASS)
	TRANS_TYPE(IND%) = IC_TRANSACTION::TYPE_A
	QUANTITY(IND%) = IC_TRANSACTION::QUANTITY_A
	TOTAL(IND%) = TOTAL(IND%) + FUNC_ROUND(IC_TRANSACTION::QUANTITY_A, 3%)

	GOTO 17240 IF IC_TRANSACTION::QUANTITY_B = 0.0

17230	GOTO 17240 IF UTL_EXAM_TRANSTYPE(IC_TRANSACTION::TYPE_B, &
		UTL_TRANSTYPE_EXAM) <> CMC$_NORMAL

17235	IND% = VAL%(UTL_TRANSTYPE_EXAM::CLASS)
	TRANS_TYPE(IND%) = IC_TRANSACTION::TYPE_B
	QUANTITY(IND%) = QUANTITY(IND%) + IC_TRANSACTION::QUANTITY_B
	TOTAL(IND%) = TOTAL(IND%) + FUNC_ROUND(IC_TRANSACTION::QUANTITY_B, 3%)

17240	GOSUB 18000

	GOTO NextTrans

17250	IF PRINT_TRANSACTION%
	THEN
		TEXT$ = PD_PRODUCT::PRODUCT_NUM + &
			" Ending Balance" + &
			SPACE$(44%) + &
			FORMAT$(TOTAL(1%), "#######.###") + "     " + &
			FORMAT$(TOTAL(2%), "#######.###") + "    " + &
			FORMAT$(TOTAL(3%), "#######.###") + "     " + &
			FORMAT$(TOTAL_COST, "########.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -2%)

		PRINT_TRANSACTION% = 0%

		BLANK_LINE% = -1%
		LOC_FLAG% = -1%
	END IF

	IF SOMETHING_PRINTED%
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

		IF WIDE_SPACE$ = "Y"
		THEN
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -3%)
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -3%)
		END IF

	END IF

	!
	! Try for next record
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT
	GOTO GetNextRec

 ExitTotal:
	!
	! Handle end of report
	!
	IF PRINT_TRANSACTION%
	THEN
		TEXT$ = PD_PRODUCT::PRODUCT_NUM + &
			" Ending Balance" + &
			SPACE$(44%) + &
			FORMAT$(TOTAL(1%), "#######.###") + "     " + &
			FORMAT$(TOTAL(2%), "#######.###") + "    " + &
			FORMAT$(TOTAL(3%), "#######.###") + "     " + &
			FORMAT$(TOTAL_COST, "########.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)
		LOC_FLAG% = -1%
	END IF

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -3%)

	IF WIDE_SPACE$ = "Y"
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -3%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -3%)
	END IF

	!
	! Get ready to print Debit/Credit (New page, reset titles, etc.)
	!
	TITLE$(5%) = "Account            Description"       + &
		"                            BegBal  " + &
		"   +    Debit    -    Credit  =  EndBalance     EndInvent"

	TITLE$(6%) = "."

	TITLE$(7%) = ""

	LIN% = 999%

	LOCATION$ = "ALL" IF LOCATION$ = "" OR LOCATION$ = "*"
	TITLE$(2%) = "FOR LOCATION " + LOCATION$

	TOTAL_DB, TOTAL_CR, TOTAL_BB, TOTAL_ADJ = 0.0

17300	WHEN ERROR IN
		RESET #IC_TOTAL_IDX%
	USE
		CONTINUE TempBalance IF ERR = 11%
		FILENAME$ = "IC_TOTAL"
		CONTINUE HelpError
	END WHEN

 GetTemp:
	!
	! Print out the Debit/Credit information
	!
	WHEN ERROR IN
		GET #IC_TOTAL_IDX%
	USE
		CONTINUE TempBalance IF ERR = 11%
		FILENAME$ = "IC_TOTAL"
		CONTINUE HelpError
	END WHEN

	V% = GL_EXAM_CHART(IC_TOTAL::ACCT, GL_CHART_EXAM)

	TEXT$ = IC_TOTAL::ACCT + " " + &
		LEFT(GL_CHART_EXAM::DESCR, 30%) + " " + &
		FORMAT$(IC_TOTAL::BEGBAL, "###,###,###.## ") + &
		FORMAT$(IC_TOTAL::DEBIT, "<%>##,###,###.## ") + &
		FORMAT$(-IC_TOTAL::CREDIT, "<%>##,###,###.## ") + &
		FORMAT$(IC_TOTAL::BEGBAL + IC_TOTAL::DEBIT + &
			IC_TOTAL::CREDIT, "###,###,###.## ") + &
		FORMAT$(IC_TOTAL::UNITS, "###,###,###.## ")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, LIN%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	SOMETHING_PRINTED% = -1%
	LIN% = 0%
	LOC_FLAG% = -1%

	!
	! Add credit/debit amounts
	!
	TOTAL_DB = FUNC_ROUND(TOTAL_DB + IC_TOTAL::DEBIT, 2%)
	TOTAL_CR = FUNC_ROUND(TOTAL_CR + IC_TOTAL::CREDIT, 2%)
	TOTAL_BB = FUNC_ROUND(TOTAL_BB + IC_TOTAL::BEGBAL, 2%)
	TOTAL_ADJ = FUNC_ROUND(TOTAL_ADJ + IC_TOTAL::UNITS, 2%)

	LIN% = 2%

	GOTO GetTemp

 TempBalance:
	!
	! Print out a blank, then Debit/Credit totals
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", LIN%)
	GOTO ExitProgram IF UTL_REPORTX::STAT
	LIN% = 0%

	TEXT$ = "Totals" + SPACE$(44%) + &
		FORMAT$(TOTAL_BB, "###,###,###.## ") + &
		FORMAT$(TOTAL_DB, "###,###,###.## ") + &
		FORMAT$(-TOTAL_CR, "###,###,###.## ") + &
		FORMAT$(TOTAL_BB + TOTAL_DB + TOTAL_CR, "###,###,###.## ") + &
		FORMAT$(TOTAL_ADJ, "###,###,###.## ")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	LOC_FLAG% = -1%

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

18000	IF ZERO_TOTAL = 0.0 AND ZERO$ <> "Y"
	THEN
		IF SUM_ONLY$ <> "Y"
		THEN
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, LIN%)
			SOMETHING_PRINTED% = -1%
		END IF
		ZERO_TOTAL  = -1.0
		PRINT_LINE% = -1%
		LOC_FLAG% = -1%
		LIN% =  0%
	END IF

	IF (IC_TRANSACTION::COST = 0.0 AND FIX_COST$ = "Y")
	THEN
		COST = PC_READ_COST(IC_TRANSACTION::PRODUCT, &
			IC_TRANSACTION::LOCATION, &
			IC_TRANSACTION::TRANS_DATE, "") * &
			QUANTITY(1%)
	ELSE
		COST = ABS(IC_TRANSACTION::COST)
	END IF

	COST = SGN(QUANTITY(1%)) * COST
	ADJ_COST = FUNC_ROUND(QUANTITY(1%) * END_COST, 2%)

	IF SUM_ONLY$ <> "Y"
	THEN
		TEXT$ = PD_PRODUCT::PRODUCT_NUM + "  " + &
			PRNT_DATE(IC_TRANSACTION::TRANS_DATE, 8%) + " "  + &
			IC_TRANSACTION::PRIMARY_REF + " "  + &
			IC_TRANSACTION::CROSS_REF + " "  + &
			IC_TRANSACTION::SUBACCOUNT + &
			SPACE$(7%) + &
			FORMAT$(QUANTITY(1%), "<%>#######.###") + " " + &
			TRANS_TYPE(1%) + " " + &
			FORMAT$(QUANTITY(2%), "<%>#######.###") + " " + &
			TRANS_TYPE(2%) + " " + &
			FORMAT$(QUANTITY(3%), "<%>#######.###") + " " + &
			TRANS_TYPE(3%) + " " + &
			FORMAT$(COST, "<%>#######.## ")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)

		PRINT_TRANSACTION% = -1%
		SOMETHING_PRINTED% = -1%
		LOC_FLAG% = -1%
	END IF

	SUB_RUNNING = SUB_RUNNING + COST

	TOTAL_COST = TOTAL_COST + COST

	GOSUB 18050

	!
	! Clean quantity and transaction types
	!
	FOR I% = 1% TO 3%
		QUANTITY(I%) = 0.0
		TRANS_TYPE(I%) = "  "
	NEXT I%

	RETURN

18040	!
	! Deal with the beginnig balance
	!
	! Find the record in the file that has this account
	!
	V% = PD_READ_ACCOUNT(UTL_LOCATION::LOCATION, PD_PRODUCT::PROD_TYPE, &
		PD_ACCOUNT_READ)

	DEBIT, CREDIT = 0.0
	BEGBAL = COST

	GOTO 18060

18050	!
	! Put/update record in the IC temporary total file
	!
	! Credit/debit amounts first
	!
	BEGBAL, DEBIT, CREDIT = 0.0

	IF (COST > 0.0)
	THEN
		DEBIT = COST
	ELSE
		CREDIT = COST
	END IF

18060	WHEN ERROR IN
		GET #IC_TOTAL_IDX%, KEY #0% EQ PD_ACCOUNT_READ::INVACCT
	USE
		CONTINUE 18070 IF ERR = 155%
		FILENAME$ = "IC_TOTAL"
		CONTINUE HelpError
	END WHEN

	IC_TOTAL::DEBIT		= IC_TOTAL::DEBIT + DEBIT
	IC_TOTAL::CREDIT	= IC_TOTAL::CREDIT + CREDIT
	IC_TOTAL::BEGBAL	= IC_TOTAL::BEGBAL + BEGBAL
	IC_TOTAL::UNITS		= IC_TOTAL::UNITS + ADJ_COST

	UPDATE #IC_TOTAL_IDX%

	GOTO EndTemp

18070	!
	! Record not found; create it
	!
	IC_TOTAL::ACCT		= PD_ACCOUNT_READ::INVACCT
	IC_TOTAL::DESCR		= ""
	IC_TOTAL::ACCTYPE	= ""
	IC_TOTAL::BEGBAL	= BEGBAL
	IC_TOTAL::UNITS		= ADJ_COST
	IC_TOTAL::HOURS		= 0.0
	IC_TOTAL::DEBIT		= DEBIT
	IC_TOTAL::CREDIT	= CREDIT

	PUT #IC_TOTAL_IDX%

 EndTemp:
	RETURN

 Subtotals:
	IF PRINT_LINE%
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%) &
			IF BLANK_LINE% = 0%

		SELECT SORT_BY$

		CASE "C"
			TEXT$ = "          Subtotal for Category " + &
				TEST_CATEGORY$ + " " + &
				STRING$(10%, A"."B)

		CASE "T"
			TEXT$ = "          Subtotal for Type " + &
				TEST_PRODTYPE$ + SPACE$(7%) + &
				STRING$(10%, A"."B)

		END SELECT

		SUB_TOTAL = SUB_BBAL + SUB_RUNNING

		TEXT$ = TEXT$ + SPACE$(10%) + &
			FORMAT$(SUB_BBAL, "BegBal $########.##") + "     " + &
			FORMAT$(SUB_RUNNING, "########.##") + "     " + &
			FORMAT$(SUB_TOTAL, "EndBal $########.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, LIN%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

		LOC_FLAG% = -1%
		LIN% = 0%

		IF WIDE_SPACE$ = "Y"
		THEN
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -3%)
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -3%)
		END IF
	END IF

	SUB_BBAL = 0.0
	SUB_RUNNING = 0.0

	PRINT_LINE% = 0%
	BLANK_LINE% = 0%

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
