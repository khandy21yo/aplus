1	%TITLE "Stock Value Report"
	%SBTTL "IC_RPRT_STOCKVALUECOMP"
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
	!	On Hand	Cost Method
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
	!	$ BAS IC_SOURCE:IC_RPRT_STOCKVALUECOMP/LINE
	!	$ LINK/EXE=IC_EXE: IC_RPRT_STOCKVALUECOMP, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE IC_RPRT_STOCKVALUECOMP.OBJ;*
	!
	! Author:
	!
	!	01/12/96 - Kevin Handy
	!		Based on IC_RPRT_STOCKVALUE
	!
	! Modification History:
	!
	!	09/05/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	03/23/1998 - Kevin Handy
	!		Fix subtotals so they are zeroed after each
	!		subTOTAL_
	!
	!	03/23/98 - Kevin Handy
	!		Many changes trying to get good beginning balance
	!
	!	03/26/98 - Kevin Handy
	!		A Blank Period will use the running balance
	!
	!	04/14/98 - Kevin Handy
	!		Make to print out everything with a qty, ignore
	!		STD in product definition.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/18/1998 - Kevin Handy
	!		Add totals by GL account, and options to only
	!		print totals.
	!
	!	08/25/99 - Kevin Handy
	!		Improved documentation, reformatted source code,
	!		and fixed bug in the-report settings so that a
	!		blank period was legal.
	!
	!	11/12/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.HB"
	MAP (IC_CONTROL) IC_CONTROL_CDD IC_CONTROL

	%INCLUDE "SOURCE:[GL.OPEN]POST_TO_GL.HB"
	MAP (IC_TOTAL)		POST_TO_GL_CDD		IC_TOTAL

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	DECLARE			GL_CHART_CDD		GL_CHART_EXAM

	%INCLUDE "SOURCE:[PD.OPEN]PD_ACCOUNT.HB"
	DECLARE			PD_ACCOUNT_CDD		PD_ACCOUNT_READ

	!
	! External functions
	!
	EXTERNAL REAL	FUNCTION PC_READ_COST
	EXTERNAL LONG	FUNCTION IC_READ_35BALANCE_PERIOD
	EXTERNAL LONG	FUNCTION IC_READ_35BALANCE
	EXTERNAL LONG	FUNCTION GL_EXAM_CHART
	EXTERNAL LONG	FUNCTION PD_READ_ACCOUNT

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
	!	The ^*To Item\* causes
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

	DATE_ONE$ = DATE_STOREDATE(UTL_REPORTX::OPTDEF(5%))

	!++
	! Abstract:FLD06
	!	^*(06) First Date\*
	!	.b
	!	.lm +5
	!	Specifies the first date to use to pull up the costs.
	!	.lm -5
	!
	! Index:
	!	.x Date>Stock Value Report
	!	.x Stock Value Report>Date
	!
	!--

	DATE_TWO$ = DATE_STOREDATE(UTL_REPORTX::OPTDEF(6%))

	!++
	! Abstract:FLD07
	!	^*(07) Second Date\*
	!	.b
	!	.lm +5
	!	Specifies the second date to use for the costs.
	!	.lm -5
	!
	! Index:
	!	.x Date>Stock Value Report
	!	.x Stock Value Report>Date
	!
	!--

	PERIOD$ = EDIT$(UTL_REPORTX::OPTDEF(7%), -1%)

	!++
	! Abstract:FLD08
	!	^*(08) Period\*
	!	.b
	!	.lm +5
	!	Allows specifying the period for which the beginning balance
	!	is wanted.
	!	.b
	!	If this field is blank, then the current posted balance will be used.
	!	.lm -5
	!
	! Index:
	!	.x Period>Stock Value Report
	!	.x Stock Value Report>Period
	!
	!--

	TOTALS_ONLY$ = LEFT$(UTL_REPORTX::OPTDEF(8%), 1%)

	!++
	! Abstract:FLD09
	!	^*(08) Totals Only\*
	!	.b
	!	.lm +5
	!	A "Yes" in this field specifies that you only want grand totals.
	!	No detail by part number will be shown.
	!	.lm -5
	!
	! Index:
	!	.x Totals Only>Stock Value Report
	!	.x Stock Value Report>Totals Only
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

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

320	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.OPN"
	USE
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

330	!
	! Read Period
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.OPN"

		GET #IC_CONTROL.CH%, RECORD 1%, REGARDLESS
		CLOSE IC_CONTROL.CH%
	USE
		FILENAME$ = "UTL_CONTROL"
		CONTINUE HelpError
	END WHEN

	IF PERIOD$ > IC_CONTROL::PERIOD AND PERIOD$ <> ""
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "Period is not closed!", 0%)
		GOTO ExitProgram
	END IF

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
	TITLE$(3%) = "Inventory Control System"
	TITLE$(4%) = ""

	!
	! Heading
	!
	TITLE$(5%) = "                                        " + &
		"                     Period  -------Cost-------   " + &
		"  -----Extension-------"
	TITLE$(6%) = "Product#       Description              " + &
		"   Tp Cat  Method   Balance  " + &
		PRNT_DATE(DATE_ONE$, 6%) + "  " + &
		PRNT_DATE(DATE_TWO$, 6%) + "     " + &
		PRNT_DATE(DATE_ONE$, 6%) + "     " + &
		PRNT_DATE(DATE_TWO$, 6%) + &
		"     Varience"
	TITLE$(7%) = "."
	TITLE$(8%) = ""

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
		COMP_STRING(EDIT$(UTL_LOCATION::LOCATION, -1%), LOCATION$) = 0%

	IF PRINT_LINE%
	THEN
		GOSUB 18100
		LIN% = 999% IF TOTALS_ONLY$ <> "Y"
	END IF

	PRINT_LINE% = 0%

	TITLE$(2%) = "AT LOCATION " + UTL_LOCATION::LOCATION + " " + &
		UTL_LOCATION::LOCNAME

	SUBTOTAL_COST1 = 0.0
	SUBTOTAL_COST2 = 0.0

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
 !	GOTO GetNextRec IF PD_PRODUCT::SSTATUS <> "A"

17030	SELECT SORT_BY$

	CASE "C"
		GOTO NextLocation &
			IF (PD_PRODUCT::CATEGORY > TO_ITEM$) AND &
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
		GOTO NextLocation &
			IF (PD_PRODUCT::DESCRIPTION > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(PD_PRODUCT::DESCRIPTION, -1%), &
			WLDCRD$) = 0%

	CASE "P"
		GOTO NextLocation &
			IF (PD_PRODUCT::PRODUCT_NUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(PD_PRODUCT::PRODUCT_NUM, -1%), &
			WLDCRD$) = 0%

	CASE "T"
		GOTO NextLocation &
			IF (PD_PRODUCT::PROD_TYPE > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(PD_PRODUCT::PROD_TYPE, -1%), &
			WLDCRD$) = 0%

		IF TEST_PRODTYPE$ <> PD_PRODUCT::PROD_TYPE AND &
			TEST_PRODTYPE$ <> "" AND PRINT_LINE%
		THEN
			GOSUB 18100
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
			PRINT_LINE% = 0%
		END IF

		TEST_PRODTYPE$ = PD_PRODUCT::PROD_TYPE

	END SELECT

17300	!
	! Print out one line
	!
	IF PERIOD$ = ""
	THEN
		GOTO GetNextRec &
			IF IC_READ_35BALANCE(PD_PRODUCT::PRODUCT_NUM, &
			UTL_LOCATION::LOCATION, BALANCE(,)) AND 1% = 0%

		BEGBAL = BALANCE(1%, 1%)
		ENDBAL = FUNC_ROUND(BEGBAL + BALANCE(1%, 2%), 2%)
	ELSE
		GOTO GetNextRec &
			IF IC_READ_35BALANCE_PERIOD(PD_PRODUCT::PRODUCT_NUM, &
			UTL_LOCATION::LOCATION, BALANCE(,), PERIOD$) AND 1% = 0%

		BEGBAL = BALANCE(1%, 1%)
		ENDBAL = FUNC_ROUND(BEGBAL, 2%)
	END IF

	GOTO 17350 IF ENDBAL = 0.0

 !	IF PD_PRODUCT::METHOD = "STD"
 !	THEN
		COST1 = PC_READ_COST(PD_PRODUCT::PRODUCT_NUM, &
			UTL_LOCATION::LOCATION, DATE_ONE$, "")
		COST2 = PC_READ_COST(PD_PRODUCT::PRODUCT_NUM, &
			UTL_LOCATION::LOCATION, DATE_TWO$, "")
 !	ELSE
 !		COST1 = 0.0
 !		COST2 = 0.0
 !	END IF

	EXTCOST1 = FUNC_ROUND(COST1 * ENDBAL, 2%)
	EXTCOST2 = FUNC_ROUND(COST2 * ENDBAL, 2%)

	IF TOTALS_ONLY$ <> "Y"
	THEN
		TEXT_LIN$ = PD_PRODUCT::PRODUCT_NUM + " " + &
			LEFT(PD_PRODUCT::DESCRIPTION, 27%) + " " + &
			PD_PRODUCT::PROD_TYPE + " " + &
			PD_PRODUCT::CATEGORY + " " + &
			PD_PRODUCT::METHOD + &
			FORMAT$(ENDBAL, "####,###,###") + &
			FORMAT$(COST1, "<%>##,###.##") + &
			FORMAT$(COST2, "<%>##,###.##") + &
			FORMAT$(EXTCOST1, "##,###,###.##") + &
			FORMAT$(EXTCOST2, "##,###,###.##") + &
			FORMAT$(EXTCOST1 - EXTCOST2, "##,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT_LIN$, LIN%)
		LIN% = 0%
	END IF

	TOTAL_COST1 = TOTAL_COST1 + EXTCOST1
	SUBTOTAL_COST1 = SUBTOTAL_COST1 + EXTCOST1
	TOTAL_COST2 = TOTAL_COST2 + EXTCOST2
	SUBTOTAL_COST2 = SUBTOTAL_COST2 + EXTCOST2

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

	!
	! Get ready to print Debit/Credit (New page, reset titles, etc.)
	!
	TITLE$(5%) = "Account            Description" + &
		"                            BegBal  " + &
		"   +    Debit    -    Credit  =  EndBalance     EndInvent"

	TITLE$(6%) = "."

	TITLE$(7%) = ""

	LIN% = 999%

	LOCATION$ = "ALL" IF LOCATION$ = "" OR LOCATION$ = "*"
	TITLE$(2%) = "FOR LOCATION " + LOCATION$

	TOTAL_DB, TOTAL_CR, TOTAL_BB, TOTAL_ADJ = 0.0

17500	WHEN ERROR IN
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

	TEXT$ = "Totals" + SPACE$(44%) + &
		FORMAT$(TOTAL_BB, "###,###,###.## ") + &
		FORMAT$(TOTAL_DB, "###,###,###.## ") + &
		FORMAT$(-TOTAL_CR, "###,###,###.## ") + &
		FORMAT$(TOTAL_BB + TOTAL_DB + TOTAL_CR, "###,###,###.## ") + &
		FORMAT$(TOTAL_ADJ, "###,###,###.## ")

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
		CALL SUBR_3EXITPROGRAM(SCOPE, &
			"RUN " + UTL_REPORTX::NEXTRUN, "")
	END IF

	%PAGE

18000	GOTO Ret18000 IF UTL_REPORTX::PAGENO = 0%

	!
	! Print total cost
	!
	TEXT$ = SPACE$(LEN(PD_PRODUCT::PRODUCT_NUM)) + " " + &
		"T O T A L" + SPACE$(63%) + &
		FORMAT$(TOTAL_COST1, "##,###,###.##") + &
		FORMAT$(TOTAL_COST2, "##,###,###.##") + &
		FORMAT$(TOTAL_COST1 - TOTAL_COST2, "##,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOSUB 18040

 Ret18000:
	RETURN

	%PAGE

18040	!
	! Deal with the beginnig balance
	!
	! Find the record in the file that has this account
	!
	V% = PD_READ_ACCOUNT(UTL_LOCATION::LOCATION, PD_PRODUCT::PROD_TYPE, &
		PD_ACCOUNT_READ)

18050	!
	! Put/update record in the IC temporary total file
	!
	! Credit/debit amounts first
	!
	BEGBAL, DEBIT, CREDIT = 0.0

	IF (TOTAL_COST1 > 0.0)
	THEN
		DEBIT = TOTAL_COST1
	ELSE
		CREDIT = TOTAL_COST1
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
	IC_TOTAL::UNITS		= IC_TOTAL::UNITS + ADJ.COST

	UPDATE #IC_TOTAL_IDX%

	GOTO EndTemp

18070	!
	! Record not found; create it
	!
	IC_TOTAL::ACCT		= PD_ACCOUNT_READ::INVACCT
	IC_TOTAL::DESCR		= ""
	IC_TOTAL::ACCTYPE	= ""
	IC_TOTAL::BEGBAL	= BEGBAL
	IC_TOTAL::UNITS		= ADJ.COST
	IC_TOTAL::HOURS		= 0.0
	IC_TOTAL::DEBIT		= DEBIT
	IC_TOTAL::CREDIT	= CREDIT

	PUT #IC_TOTAL_IDX%

 EndTemp:
	RETURN

18100	!
	! Print subtotal cost
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	IF SORT_BY$ = "T"
	THEN
		TEST$ = TEST_PRODTYPE$
	ELSE
		TEST$ = "  "
	END IF

	TEXT$ = SPACE$(LEN(PD_PRODUCT::PRODUCT_NUM)) + " " + &
		"TOTAL    " + TEST$ + SPACE$(61%) + &
		FORMAT$(SUBTOTAL_COST1, "##,###,###.##") + &
		FORMAT$(SUBTOTAL_COST2, "##,###,###.##") + &
		FORMAT$(SUBTOTAL_COST1 - SUBTOTAL_COST2, "##,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	SUBTOTAL_COST1 = 0.0
	SUBTOTAL_COST2 = 0.0

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
