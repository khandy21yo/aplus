1	%TITLE "Accounts Receivable Salesman Report"
	%SBTTL "AR_RPRT_SALNUM_KBJ"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1988 BY
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
	! ID:AR033
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Salesman Report\* program prints a
	!	report of the salesman who
	!	completed the sale.
	!	The information is accessed in the open General Ledger
	!	file and compiled to
	!	show the salesman receiving credit for each sale.
	!	.lm -5
	!
	! Index:
	!	.x Salesman Report
	!
	! Option:
	!
	! Author:
	!
	!	03/08/2000 - Kevin Handy
	!		Modified version of AR_RPRT_SALNUM for KingB
	!
	! Compile:
	!
	!	$  BAS AR_SOURCE:AR_RPRT_SALNUM_KBJ.BAS/LINE
	!	$  LINK/EXE=AR_EXE:*.EXE AR_RPRT_SALNUM_KBJ, FUNC_LIB:CMCLINK/OPTION
	!	$  DELETE AR_RPRT_SALNUM_KBJ.OBJ;*
	!
	! Modification history:
	!
	!	10/30/2000 - Kevin Handy
	!		Use A"x"B
	!--

	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.HB"
	MAP (AR_OPEN)		AR_OPEN_CDD		AR_OPEN
	DECLARE			AR_OPEN_CDD		AR_OPEN_TEMP

	%INCLUDE "SOURCE:[AR.OPEN]AR_CLOSED.HB"
	MAP (AR_CLOSED)		AR_CLOSED_CDD		AR_CLOSED

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP (SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT

	%INCLUDE "SOURCE:[SA.OPEN]SA_SALESMAN.HB"
	MAP (SB_SUBACCOUNT)	SA_SALESMAN_CDD		SA_SALESMAN

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	DECLARE			AR_35CUSTOM_CDD		AR_35CUSTOM_EXAM

	RECORD SUBTOTAL_CDD
		STRING ACCOUNT = 18%
		GFLOAT AMOUNT
	END RECORD

	DECLARE SUBTOTAL_CDD GRAND_TOTAL(10%)
	DECLARE SUBTOTAL_CDD SALESMAN_TOTAL(10%)
	DECLARE SUBTOTAL_CDD CUSTOMER_TOTAL(10%)

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION AR_EXAM_CUSTOM
	EXTERNAL SUB ADD_TOTAL(INTEGER, SUBTOTAL_CDD DIM(), GFLOAT, STRING)

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

	!
	! Set some initial variable values
	!
	GRAND_TOTAL%    = 0%
	SALESMAN_TOTAL% = 0%
	CUSTOMER_TOTAL% = 0%

 Init:
	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)

	!++
	! Abstract:FLD01
	!	^*(01) Sort by	(S,C,T)\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field determines the order
	!	in which the report will print.
	!	.b
	!	Valid settings are:
	!	.te
	!	.table 3,25
	!	^*S\* - Salesman
	!	.te
	!	^*C\* - Class
	!	.te
	!	^*T\* - Type
	!	.end table
	!	A setting is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.b
	!	.lm +5
	!	^*From Item\* causes the printing
	!	to begin with the selected Item.  The value
	!	entered must be in agreement
	!	with the value in Field (01) Sort by.
	!	.b
	!	A blank setting will cause the report
	!	to begin with the first item in
	!	the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.b
	!	.lm +5
	!	^*To Item\* causes the printing
	!	to end with the selected Item.
	!	The value must be in
	!	agreement with the value
	!	entered in field (01) Sort by.
	!	.b
	!	A blank setting will cause the report to
	!	end with the last item in the
	!	file.
	!	.lm -5
	!
	! Index:
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.b
	!	.lm +5
	!	^*Wildcard\* is used to designated items, selected
	!	by the sort-by field, to be printed
	!	by entering a "wildcard"
	!	using the Wildcarding Technique.
	!	.lm -5
	!
	! Index:
	!
	!--

	FROM_DATE$ = DATE_STOREDATE(TRM$(UTL_REPORTX::OPTDEF(5%)))

	!++
	! Abstract:FLD06
	!	^*(06) From Date\*
	!	.b
	!	.lm +5
	!	The ^*From Date\* field specifies
	!	the starting date for which the data
	!	on the salesmen will be printed.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!	.x From Date>Salesman Report
	!	.x Salesman Report>From Date
	!
	!--

	TO_DATE$ = DATE_STOREDATE(TRM$(UTL_REPORTX::OPTDEF(6%)))

	!++
	! Abstract:FLD07
	!	^*(07) To Date\*
	!	.b
	!	.lm +5
	!	The ^*To Date\* field specifies
	!	the ending date for which the data
	!	on the salesmen will be printed.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!	.x To Date>Salesman Report
	!	.x Salesman Report>To Date
	!
	!--

	DO_GROUP$ = LEFT(EDIT$(UTL_REPORTX::OPTDEF(7%), -1%), 1%)

	!++
	! Abstract:FLD08
	!	^*(08) Sales/Cash	(S,C)\*
	!	.b
	!	.lm +5
	!	The ^*Sales/Cash\* field specifies if the report is generated
	!	in sales made (*S), or cash collected (*C).
	!	The entry in this field must relate
	!	with the type entered in field
	!	(3). A sales entry corresponds
	!	with types '01', '02', '03', and '04'
	!	and a cash entry corresponds
	!	with types '02', '09', and '10'.
	!	.lm -5
	!
	! Index:
	!	.x Sales/Cash>Salesman Report
	!	.x Salesman Report>Sales/Cash
	!
	!--

	SELECT DO_GROUP$

	CASE "S"
		DO_LIST$   = "01,02,03,08"
		ADD_TITLE$ = "SHOWING SALES"
		FACTOR     = 1.0

	CASE "C"
		DO_LIST$   = "02,09"
		ADD_TITLE$ = "SHOWING CASH RECEIVED"
		FACTOR     = -1.0

	END SELECT

	CUS_TOTAL$ = LEFT(UTL_REPORTX::OPTDEF(8%), 1%)

	!++
	! Abstract:FLD09
	!	^*(09) Customer Totals\*
	!	.b
	!	.lm +5
	!	This field specifies if subtotals should be printed by
	!	customer within the salesman.
	!	.lm -5
	!
	! Index:
	!	.x Customer Total>Salesman Report
	!	.x Salesman Report>Customer Total
	!
	!--


300	WHEN ERROR IN
		%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.OPN"
	USE
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.OPN"
	USE
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

320	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CLOSED.OPN"
	USE
		CONTINUE ReportTitle IF ERR = 5%
		FILENAME$ = "AR_CLOSED"
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
	!
	! Set up titles
	!
	TITLE$(1%) = "BROKER/SALESMAN SALES REPORT " + ADD_TITLE$

	SELECT SORTBY$

	CASE "S"
		K_NUM%     = 0%
		TITLE$(2%) = "SORTED BY BROKER/SALESMAN"

	CASE "C"
		K_NUM%     = 2%
		TITLE$(2%) = "SORTED BY BROKER/SALESMAN CLASS"

	CASE "T"
		K_NUM%     = 1%
		TITLE$(2%) = "SORTED BY BROKER/SALESMAN TYPE"

	END SELECT

	TITLE$(3%) = "AR System"
	TITLE$(4%) = "From " + PRNT_DATE(FROM_DATE$, 8%) + &
		" To " + PRNT_DATE(TO_DATE$, 8%)
	TITLE$(4%) = "Before " + PRNT_DATE(TO_DATE$, 8%) IF FROM_DATE$ = ""
	TITLE$(4%) = "After " + PRNT_DATE(FROM_DATE$, 8%) IF TO_DATE$ = ""
	TITLE$(4%) = "For All Dates" IF FROM_DATE$ + TO_DATE$ = ""
	TITLE$(5%) = ""

	!
	! Column headings
	!
	TITLE$(6%) = "Broker/Salesman#      Name" + SPACE$(38%) + &
		"Type  Class"

	TITLE$(7%) = "     Invoice   InvDate   Cust#       " + &
		"Name                  TraDate" + &
		"   Account             Description                        Amount"

	TITLE$(8%) = "."

	!
	! Adjust Dates
	!
	FROM_DATE$ = "01010001" IF FROM_DATE$ = ""
	TO_DATE$   = "31129999" IF TO_DATE$   = ""

	!
	! Initialize variables
	!
	LIN% = 0%

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			FIND #SB_SUBACCOUNT.CH%, KEY #K_NUM% GE "S", REGARDLESS
		ELSE
			FIND #SB_SUBACCOUNT.CH%, &
				KEY #K_NUM% GE "S" + FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE ExitTotal IF ERR = 155%
		FILENAME$ = "SA_SALESMAN"
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
		GET #SB_SUBACCOUNT.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "SA_SALESMAN"
		CONTINUE HelpError
	END WHEN

	GOTO GetNextRec IF SA_SALESMAN::SUBJECT <> "S"

	SELECT SORTBY$

	CASE "C"
		GOTO ExitTotal IF (SA_SALESMAN::CLASS > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_ARRAY(EDIT$(SA_SALESMAN::CLASS, -1%), &
			WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "S"
		GOTO ExitTotal IF (SA_SALESMAN::SALESMAN > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_ARRAY(EDIT$(SA_SALESMAN::SALESMAN, -1%), &
			WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "T"
		GOTO ExitTotal IF (SA_SALESMAN::TTYPE > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_ARRAY(EDIT$(SA_SALESMAN::TTYPE, -1%), &
			WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	END SELECT

	SALESLINE_FLAG% = -1%
	TEST_INVOICE$ = ""
	INVOICE_DATE$ = ""
	OPEN_FLAG% = -1%
	CLOSED_FLAG% = -1%
	LAST_CUSTOMER$ = "~~~~~~~~~~~~~~~~"
	LAST_CUSTOMER% = 0%

17200	!
	! See if salesman exists in AR_OPEN file
	!
	WHEN ERROR IN
		GET #AR_OPEN.CH%, KEY #2% EQ SA_SALESMAN::SALESMAN, REGARDLESS
	USE
		CONTINUE 17210 IF ERR = 155%
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

	OPEN_FLAG% = 0%

17210	!
	! See if salesman exists in AR_CLOSED file
	!
	WHEN ERROR IN
		GET #AR_CLOSED.CH%, &
			KEY #1% EQ SA_SALESMAN::SALESMAN, &
			REGARDLESS
	USE
		CONTINUE EndFirstTest IF ERR = 155%
		FILENAME$ = "AR_CLOSED"
		CONTINUE HelpError
	END WHEN

	CLOSED_FLAG% = 0%

 EndFirstTest:
	GOTO GetNextRec IF OPEN_FLAG% * CLOSED_FLAG%

17220	!
	! Are we already done
	!
	GOTO SalesmanTotal IF OPEN_FLAG% * CLOSED_FLAG%

	!
	! Decide which file to look in. (Open or Closed)
	!
	IF (CLOSED_FLAG% <> 0%) OR &
		((OPEN_FLAG% = 0%) AND &
		(AR_OPEN::CUSNUM + AR_OPEN::INVNUM < &
		AR_CLOSED::CUSNUM + AR_CLOSED::INVNUM))
	THEN
		!
		! output from the open file
		!
		AR_OPEN_TEMP = AR_OPEN
		GOSUB TestRecord

		!
		! Get next record out of open file
		!
		OPEN_FLAG% = -1%
		WHEN ERROR IN
			GET #AR_OPEN.CH%, REGARDLESS
		USE
			CONTINUE 17220 IF ERR = 11%
			FILENAME$ = "AR_OPEN"
			CONTINUE HelpError
		END WHEN

		!
		! Are we at the end of what we want in the open file
		!
		IF AR_OPEN::SALNUM = SA_SALESMAN::SALESMAN
		THEN
			OPEN_FLAG% = 0%
		END IF

	ELSE
		!
		! Output from the Cloaed file
		!
		AR_OPEN_TEMP = AR_CLOSED
		GOSUB TestRecord

		!
		! Get next record out of open file
		!
		CLOSED_FLAG% = -1%
		WHEN ERROR IN
			GET #AR_CLOSED.CH%, REGARDLESS
		USE
			CONTINUE 17220 IF ERR = 11%
			FILENAME$ = "AR_CLOSED"
			CONTINUE HelpError
		END WHEN

		!
		! Are we at the end of what we want in the open file
		!
		IF AR_CLOSED::SALNUM = SA_SALESMAN::SALESMAN
		THEN
			CLOSED_FLAG% = 0%
		END IF
	END IF

	GOTO 17220

 TestRecord:
	IF AR_OPEN_TEMP::INVNUM <> TEST_INVOICE$
	THEN
		INVOICE_DATE$ = ""
		TEMP_DESCR$   = ""
	END IF

	TEST_INVOICE$ = AR_OPEN_TEMP::INVNUM
	INVOICE_DATE$ = AR_OPEN_TEMP::TRADAT IF AR_OPEN_TEMP::TRATYP = "01"
	TEMP_DESCR$   = AR_OPEN_TEMP::DESCR IF AR_OPEN_TEMP::TRATYP = "08"

	RETURN IF (INSTR(1%, DO_LIST$, AR_OPEN_TEMP::TRATYP) = 0%)
	RETURN IF AR_OPEN_TEMP::TRADAT < FROM_DATE$
	RETURN IF AR_OPEN_TEMP::TRADAT > TO_DATE$

	INVOICE_DATE$ = AR_OPEN_TEMP::TRADAT IF INVOICE_DATE$ = ""
	TEMP_DESCR$   = AR_OPEN_TEMP::DESCR IF TEMP_DESCR$ = ""

	!
	! If we made it this far, we can print the salesman information
	!
	IF SALESLINE_FLAG%
	THEN
		TEXT$ = SA_SALESMAN::SALESMAN + SPACE$(12%) + &
			SA_SALESMAN::DESCR + "  " + &
			SA_SALESMAN::TTYPE + "    " + &
			SA_SALESMAN::CLASS

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, LIN%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

		SALESLINE_FLAG% = 0%
		LIN% = 0%
	END IF

	IF AR_OPEN_TEMP::TRATYP = "02" AND DO_GROUP$ = "C"
	THEN
		AR_OPEN_TEMP::SALAMT = -AR_OPEN_TEMP::SALAMT
	END IF

	!
	! Subtotal by customer?
	!
	IF LAST_CUSTOMER$ <> AR_OPEN_TEMP::CUSNUM
	THEN
		GOSUB CustomerTotal
	END IF

	!
	! Look up customer name
	!
	IF (AR_OPEN_TEMP::CUSNUM <> AR_35CUSTOM_EXAM::CUSNUM)
	THEN
		IF AR_EXAM_CUSTOM(AR_OPEN_TEMP::CUSNUM, AR_35CUSTOM_EXAM) <> &
			CMC$_NORMAL
		THEN
			AR_35CUSTOM_EXAM::CUSNAM = &
				STRING$(LEN(AR_35CUSTOM_EXAM::CUSNAM), A"?"B)
		END IF
	END IF

	!
	! Print the cash receipt line
	!
	TEXT$ = "     " + AR_OPEN_TEMP::INVNUM + "  " + &
		PRNT_DATE(INVOICE_DATE$, 6%) + "  " + &
		AR_OPEN_TEMP::CUSNUM + " " + &
		LEFT(AR_35CUSTOM_EXAM::CUSNAM, 22%) + " " + &
		PRNT_DATE(AR_OPEN_TEMP::TRADAT, 6%) + "  " + &
		AR_OPEN_TEMP::ARACCT + "  " + &
		TEMP_DESCR$ + "  " + &
		FORMAT$(FACTOR * AR_OPEN_TEMP::SALAMT, "###,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	CALL ADD_TOTAL(GRAND_TOTAL%, GRAND_TOTAL(), &
		AR_OPEN_TEMP::SALAMT, AR_OPEN_TEMP::ARACCT)
	CALL ADD_TOTAL(SALESMAN_TOTAL%, SALESMAN_TOTAL(), &
		AR_OPEN_TEMP::SALAMT, AR_OPEN_TEMP::ARACCT)
	CALL ADD_TOTAL(CUSTOMER_TOTAL%, CUSTOMER_TOTAL(), &
		AR_OPEN_TEMP::SALAMT, AR_OPEN_TEMP::ARACCT)

	LAST_CUSTOMER% = -1%

	RETURN

 SalesmanTotal:
	!
	! Make sure we have subtotaled by customer
	!
	GOSUB CustomerTotal

	!
	! Print a total for one salesman
	!
	IF SALESLINE_FLAG% = 0%
	THEN
		IF (CUS_TOTAL$ <> "Y")
		THEN
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
		END IF

		FINAL = 0.0

		FOR LOOP% = 1% TO SALESMAN_TOTAL%

			TEXT$ = SPACE$(73%) + &
				SALESMAN_TOTAL(LOOP%)::ACCOUNT + &
				" Broker/Salesman Total:  " + &
				FORMAT$(FACTOR * SALESMAN_TOTAL(LOOP%)::AMOUNT, &
				"###,###,###.##")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
			GOTO ExitProgram IF UTL_REPORTX::STAT

			FINAL = FINAL + SALESMAN_TOTAL(LOOP%)::AMOUNT
		NEXT LOOP%

		IF SALESMAN_TOTAL% > 1%
		THEN
			TEXT$ = SPACE$(73%) + &
				SPACE$(18%) + &
				" Broker/Salesman Total:  " + &
				FORMAT$(FACTOR * FINAL, "###,###,###.##")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
			GOTO ExitProgram IF UTL_REPORTX::STAT
		END IF
		LIN% = 999%
	END IF

	SALESMAN_TOTAL% = 0%
	SALESLINE_FLAG% = -1%

	GOTO GetNextRec

 CustomerTotal:

	!
	! Print a total for one customer
	!
	IF (CUS_TOTAL$ = "Y") AND (LAST_CUSTOMER% <> 0%)
	THEN
		FINAL = 0.0

		FOR LOOP% = 1% TO CUSTOMER_TOTAL%
			TEXT$ = SPACE$(73%) + &
				CUSTOMER_TOTAL(LOOP%)::ACCOUNT + &
				" Customer Total:         " + &
				FORMAT$(FACTOR * CUSTOMER_TOTAL(LOOP%)::AMOUNT, &
				"###,###,###.##")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
			GOTO ExitProgram IF UTL_REPORTX::STAT

			FINAL = FINAL + CUSTOMER_TOTAL(LOOP%)::AMOUNT
		NEXT LOOP%

		IF CUSTOMER_TOTAL% > 1%
		THEN
			TEXT$ = SPACE$(73%) + &
				SPACE$(18%) + &
				" Customer Total:         " + &
				FORMAT$(FACTOR * FINAL, "###,###,###.##")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
			GOTO ExitProgram IF UTL_REPORTX::STAT
		END IF

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

	END IF

	LAST_CUSTOMER% = 0%
	LAST_CUSTOMER$ = AR_OPEN_TEMP::CUSNUM
	CUSTOMER_TOTAL% = 0%

	RETURN

 ExitTotal:
	!
	! Handle end of report
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 999%)

	FINAL = 0.0

	FOR LOOP% = 1% TO GRAND_TOTAL%
		TEXT$ = SPACE$(83%) + &
			GRAND_TOTAL(LOOP%)::ACCOUNT + &
			" Grand Total:  " + &
			FORMAT$(FACTOR * GRAND_TOTAL(LOOP%)::AMOUNT, "###,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		FINAL = FINAL + GRAND_TOTAL(LOOP%)::AMOUNT
	NEXT LOOP%

	IF GRAND_TOTAL% > 1%
	THEN
		TEXT$ = SPACE$(83%) + &
			SPACE$(18%) + &
			" Grand Total:  " + &
			FORMAT$(FACTOR * FINAL, "###,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

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

	END

30000	!*******************************************************************
	! Subroutine to summarize totals by account number
	!*******************************************************************

	SUB ADD_TOTAL(INTEGER TOTAL_COUNT, SUBTOTAL_CDD TOTAL(), &
		GFLOAT SALAMT, STRING ACCOUNT)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	RECORD SUBTOTAL_CDD
		STRING ACCOUNT = 18%
		GFLOAT AMOUNT
	END RECORD

	!
	! See if it already exists
	!
	FOR LOOP% = 1% TO TOTAL_COUNT

		IF TOTAL(LOOP%)::ACCOUNT = ACCOUNT
		THEN
			TOTAL(LOOP%)::AMOUNT = TOTAL(LOOP%)::AMOUNT + SALAMT
			GOTO EndTotal
		END IF

		GOTO TotalInsert IF ACCOUNT < TOTAL(LOOP%)::ACCOUNT
	NEXT LOOP%

	LOOP% = TOTAL_COUNT + 1%

	!
	! Create a new one
	!
 TotalInsert:
	TOTAL_COUNT = TOTAL_COUNT + 1%

	FOR LOOP1% = TOTAL_COUNT TO LOOP% STEP -1%
		TOTAL(LOOP1%) = TOTAL(LOOP1% - 1%)
	NEXT LOOP1%

	TOTAL(LOOP%)::ACCOUNT = ACCOUNT
	TOTAL(LOOP%)::AMOUNT = SALAMT

 EndTotal:
	END SUB
