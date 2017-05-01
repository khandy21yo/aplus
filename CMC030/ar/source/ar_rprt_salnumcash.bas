1	%TITLE "Accounts Receivable Salesman Report"
	%SBTTL "AR_RPRT_SALNUMCASH"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1996 BY
	!
	! Software Solutions Inc.
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
	! Software Solutions Inc.
	!
	! Software Solutions Inc. assumes no responsibility for the use
	! or reliability of its software on equipment which is not
	! supported by Software Solutions Inc.
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
	!
	! Author:
	!
	!	04/03/96 - Kevin Handy
	!
	! Compile:
	!
	!	$  BAS AR_SOURCE:AR_RPRT_SALNUMCASH.BAS/LINE
	!	$  LINK/EXE=AR_EXE:*.EXE AR_RPRT_SALNUMCASH, FUNC_LIB:CMCLINK/OPTION
	!	$  DELETE AR_RPRT_SALNUMCASH.OBJ;*
	!
	! Modification history:
	!
	!	04/09/96 - Kevin Handy
	!		Debugging, clean up.
	!
	!	06/21/96 - Kevin Handy
	!		Fix bug caused when invoice amount was zero, no
	!		charges were handled.
	!		Don't print service charge invoices out.
	!
	!	06/26/96 - Kevin Handy
	!		Reset SVC_FLAG on every invoce # change.
	!
	!	08/13/96 - Kevin Handy
	!		Reformat source code.
	!		Add space after quote in open.
	!
	!	09/27/96 - Kevin Handy
	!		Grab salesman from customer file is not
	!		defined in ar register.
	!
	!	11/04/96 - Kevin Handy
	!		Zip out salesman number after stashing
	!		the invoice.
	!
	!	11/04/96 - Kevin Handy
	!		Trap error at 17407 instead of 174000 for
	!		undefined salesman number.
	!
	!	11/04/96 - Kevin Handy
	!		Salesman number must come from AR_OPEN_TEMP
	!		instead of AR_OPEN.
	!
	!	11/04/96 - Kevin Handy
	!		Moved around test in Stash routine so that
	!		it wont get errors about key size in
	!		AR_EXAM_CUSTOM
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/26/2000 - Kevin Handy
	!		Use WHEN ERROR IN
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

	RECORD AR_TEMP_CDD
		STRING SALNUM = 10%		! Salesman number
		STRING SORTER = 10%		! Additional sort info
		STRING SORTES = 10%		! Additional sort info

		STRING INVNUM = 8%		! Invoice number
		STRING TRADAT = 8%		! Transaction Date
		STRING CUSNUM = 10%		! Customer number
		STRING CUSNAM = 30%		! Customer name
		DOUBLE INVAMT			! Invoice amount
		DOUBLE OTHAMT			! Other amount
		DOUBLE PAYAMT			! Current Payments
		DOUBLE PREAMT			! Previous payments
	END RECORD

	MAP (AR_TEMP) AR_TEMP_CDD AR_TEMP

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION AR_EXAM_CUSTOM

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

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
	!	to begin
	!	with the selected Item.  The value
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

	SORTITEM$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(01) Sort Items	(I,C)\*
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

	CUS_TOTAL$ = "N" IF SORTITEM$ = "I"

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

330	CALL ASSG_CHANNEL(AR_TEMP.CH%, STAT%)

	OPEN "AR_TEMP.TMP" FOR OUTPUT AS FILE AR_TEMP.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP AR_TEMP, &
		PRIMARY KEY (AR_TEMP::SALNUM, AR_TEMP::SORTER, &
			AR_TEMP::SORTES) DUPLICATES, &
		TEMPORARY, &
		ACCESS MODIFY, &
		ALLOW NONE

	%PAGE

 ReportTitle:
	!
	! Set up titles
	!
	TITLE$(1%) = "BROKER/SALESMAN SALES REPORT"

	SELECT SORTBY$

	CASE "S"
		K_NUM% = 0%
		TITLE$(2%) = "SORTED BY BROKER/SALESMAN"

	CASE "C"
		K_NUM% = 2%
		TITLE$(2%) = "SORTED BY BROKER/SALESMAN CLASS"

	CASE "T"
		K_NUM% = 1%
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
		"Name                               " + &
		"  InvComAmt     Payments   %Paid"

	TITLE$(8%) = "."

	!
	! Adjust Dates
	!
	FROM_DATE$ = "01010001" IF FROM_DATE$ = ""
	TO_DATE$   = "31129999" IF TO_DATE$   = ""

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	SALESLINE_FLAG% = -1%
	TEST_INVOICE$ = ""
	TEST_CUSNUM$ = ""
	TEST_SALESMAN$ = ""
	INVOICE.DATE$ = ""
	OPEN_FLAG% = -1%
	CLOSED_FLAG% = -1%
	LAST_CUSTOMER$ = "~~~~~~~~~~~~~~~~"
	LAST_CUSTOMER% = 0%

	TOTAL_INVOICE = 0.0
	TOTAL_OTHER = 0.0
	PREVIOUS_PAYMENT = 0.0
	CURRENT_PAYMENT = 0.0
	SVC_FLAG% = 0%

17200	!
	! See AR_OPEN file
	!
	WHEN ERROR IN
		RESET #AR_OPEN.CH%
	USE
		CONTINUE 17210 IF ERR = 155%
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

17210	!
	! See AR_CLOSED file
	!
	WHEN ERROR IN
		RESET #AR_CLOSED.CH%
	USE
		CONTINUE 17220 IF ERR = 155%
		FILENAME$ = "AR_CLOSED"
		CONTINUE HelpError
	END WHEN

17220	!
	! output from the open file
	!
	WHEN ERROR IN
		GET #AR_OPEN.CH%, REGARDLESS
	USE
		CONTINUE 17230 IF ERR = 11%
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

	AR_OPEN_TEMP = AR_OPEN
	GOSUB TestRecord

	GOTO 17220

17230	!
	! Output from the Closed file
	!
	WHEN ERROR IN
		GET #AR_CLOSED.CH%, REGARDLESS
	USE
		CONTINUE 17500 IF ERR = 11%
		FILENAME$ = "AR_CLOSED"
		CONTINUE HelpError
	END WHEN

	AR_OPEN_TEMP = AR_CLOSED
	GOSUB TestRecord

	GOTO 17230

 TestRecord:
	IF AR_OPEN_TEMP::INVNUM <> TEST_INVOICE$
	THEN
		GOSUB StashInvoice
		SVC_FLAG% = 0%
		INVOICE.DATE$ = ""
		TEST_SALESMAN$ = ""
	END IF

	TEST_INVOICE$ = AR_OPEN_TEMP::INVNUM
	TEST_CUSNUM$ = AR_OPEN_TEMP::CUSNUM

	!
	! Invoice amount
	!
	IF AR_OPEN_TEMP::TRATYP = "01" OR &
		AR_OPEN_TEMP::TRATYP = "02"
	THEN
		TOTAL_INVOICE = TOTAL_INVOICE + AR_OPEN_TEMP::SALAMT
		TOTAL_OTHER = TOTAL_OTHER + AR_OPEN_TEMP::OTHCHG
		INVOICE.DATE$ = AR_OPEN_TEMP::TRADAT
		TEST_SALESMAN$ = AR_OPEN_TEMP::SALNUM
	END IF

	!
	! Receipt
	!
	IF AR_OPEN_TEMP::TRATYP = "02"
	THEN
		IF AR_OPEN_TEMP::TRADAT >= FROM_DATE$ AND &
			AR_OPEN_TEMP::TRADAT <= TO_DATE$
		THEN
			CURRENT_PAYMENT = FUNC_ROUND(CURRENT_PAYMENT + &
				AR_OPEN_TEMP::SALAMT, 2%)
		ELSE
			PREVIOUS_PAYMENT = FUNC_ROUND(PREVIOUS_PAYMENT + &
				AR_OPEN_TEMP::SALAMT, 2%)
		END IF
	END IF

	IF AR_OPEN_TEMP::TRATYP = "04"
	THEN
		SVC_FLAG% = -1%
	END IF

	IF AR_OPEN_TEMP::TRATYP = "09" OR &
		AR_OPEN_TEMP::TRATYP = "10"
	THEN
		IF AR_OPEN_TEMP::TRADAT >= FROM_DATE$ AND &
			AR_OPEN_TEMP::TRADAT <= TO_DATE$
		THEN
			CURRENT_PAYMENT = FUNC_ROUND(CURRENT_PAYMENT - &
				AR_OPEN_TEMP::SALAMT, 2%)
		ELSE
			PREVIOUS_PAYMENT = FUNC_ROUND(PREVIOUS_PAYMENT - &
				AR_OPEN_TEMP::SALAMT, 2%)
		END IF
	END IF

	INVOICE.DATE$ = AR_OPEN_TEMP::TRADAT &
		IF INVOICE.DATE$ = ""

	TEST_SALESMAN$ = AR_OPEN_TEMP::SALNUM &
		IF TEST_SALESMAN$ = ""

	RETURN

	%PAGE

17400	!*******************************************************************
	! Save this invoice if it has any usefull information on it
	!*******************************************************************
 StashInvoice:

	GOTO 17450 IF CURRENT_PAYMENT = 0.0
	GOTO 17450 IF SVC_FLAG%

	!
	! Dig up a customer name
	!
	IF (TEST_CUSNUM$ <> AR_35CUSTOM_EXAM::CUSNUM)
	THEN
		IF AR_EXAM_CUSTOM(TEST_CUSNUM$, AR_35CUSTOM_EXAM) <> &
			CMC$_NORMAL
		THEN
			AR_35CUSTOM_EXAM::CUSNAM = &
				STRING$(LEN(AR_35CUSTOM_EXAM::CUSNAM), A"?"B)
		END IF
	END IF

	!
	! Use customers salesman ID if haven't gotten one yet.
	!
	IF TEST_SALESMAN$ = ""
	THEN
		TEST_SALESMAN$ = AR_35CUSTOM_EXAM::SALESMAN
	END IF

17405	GOTO 17450 IF TEST_SALESMAN$ = ""

	IF TEST_CUSNUM$ <> PRINTED_CUSNUM$
	THEN
		PRINTED_CUSNUM$ = TEST_CUSNUM$
		CALL ENTR_3MESSAGE(SCOPE, "Scanning: " + TEST_CUSNUM$, 1%)
	END IF

17407	!
	! Chech out the salesman number
	!
	IF TEST_SALESMAN$ <> SA_SALESMAN::SALESMAN
	THEN
		WHEN ERROR IN
			GET #SB_SUBACCOUNT.CH%, &
				KEY #K_NUM% GE "S" + TEST_SALESMAN$, &
				REGARDLESS
		USE
			SA_SALESMAN::SALESMAN = TEST_SALESMAN$
			SA_SALESMAN::CLASS = ""

			CONTINUE 17410 IF ERR = 155%
			CONTINUE HelpError
		END WHEN
	END IF

17410	SELECT SORTBY$

	CASE "C"
		GOTO 17450 IF (SA_SALESMAN::CLASS > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO 17450 IF (SA_SALESMAN::CLASS < FROM_ITEM$)

		GOTO 17450 &
			IF COMP_ARRAY(EDIT$(SA_SALESMAN::CLASS, -1%), &
			WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "S"
		GOTO 17450 IF (SA_SALESMAN::SALESMAN > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO 17450 IF (SA_SALESMAN::SALESMAN < FROM_ITEM$)

		GOTO 17450 &
			IF COMP_ARRAY(EDIT$(SA_SALESMAN::SALESMAN, -1%), &
			WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "T"
		GOTO 17450 IF (SA_SALESMAN::TTYPE > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO 17450 IF (SA_SALESMAN::TTYPE < FROM_ITEM$)

		GOTO 17450 &
			IF COMP_ARRAY(EDIT$(SA_SALESMAN::TTYPE, -1%), &
			WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	END SELECT

	!
	! Generate a record to keep
	!
	AR_TEMP::SALNUM = TEST_SALESMAN$
	IF SORTITEM$ = "I"
	THEN
		AR_TEMP::SORTER = TEST_INVOICE$
		AR_TEMP::SORTES = TEST_CUSNUM$
	ELSE
		AR_TEMP::SORTER = TEST_CUSNUM$
		AR_TEMP::SORTES = TEST_INVOICE$
	END IF
	AR_TEMP::INVNUM = TEST_INVOICE$
	AR_TEMP::TRADAT = INVOICE.DATE$
	AR_TEMP::CUSNUM = TEST_CUSNUM$
	AR_TEMP::CUSNAM = AR_35CUSTOM_EXAM::CUSNAM
	AR_TEMP::INVAMT = TOTAL_INVOICE
	AR_TEMP::OTHAMT = TOTAL_OTHER
	AR_TEMP::PAYAMT = CURRENT_PAYMENT
	AR_TEMP::PREAMT = PREVIOUS_PAYMENT

	PUT #AR_TEMP.CH%

17450	TOTAL_INVOICE = 0.0
	TOTAL_OTHER = 0.0
	PREVIOUS_PAYMENT = 0.0
	CURRENT_PAYMENT = 0.0
	SVC_FLAG% = 0%

	RETURN

17500	!*******************************************************************
	! Possibly print out this invoice's information
	!*******************************************************************
 PrintInvoice:

	GOSUB StashInvoice

	RESET #AR_TEMP.CH%

17510	WHEN ERROR IN
		GET #AR_TEMP.CH%
	USE
		CONTINUE ExitTotal
	END WHEN

17520	IF AR_TEMP::SALNUM <> SA_SALESMAN::SALESMAN
	THEN
		GOSUB SalesmanTotal

		SA_SALESMAN::SALESMAN = AR_TEMP::SALNUM
		SA_SALESMAN::DESCR = ""
		SA_SALESMAN::TTYPE = ""
		SA_SALESMAN::CLASS = ""

		SALESLINE_FLAG% = -1%

		WHEN ERROR IN
			GET #SB_SUBACCOUNT.CH%, &
				KEY #0% EQ "S" + AR_TEMP::SALNUM, &
				REGARDLESS
		USE
			CONTINUE 17530
		END WHEN
	END IF

17530	!
	! Force current payment to exclude freight, ...
	!
	IF ((AR_TEMP::PREAMT + AR_TEMP::PAYAMT) > &
		(AR_TEMP::INVAMT - AR_TEMP::OTHAMT)) AND &
		((AR_TEMP::INVAMT - AR_TEMP::OTHAMT) <> 0.0)
	THEN
		CURRENT_PAYMENT = &
			(AR_TEMP::INVAMT - AR_TEMP::OTHAMT) - &
			AR_TEMP::PREAMT

		CURRENT_PAYMENT = 0.0 IF CURRENT_PAYMENT < 0.0
	ELSE
		CURRENT_PAYMENT = AR_TEMP::PAYAMT
	END IF

	!
	! Nothing to report
	!
	GOTO ExitPrintInvoice IF CURRENT_PAYMENT = 0.0

	!
	! Calculate percentage
	!
	IF (AR_TEMP::INVAMT - AR_TEMP::OTHAMT) = 0.0
	THEN
		PERCENTAGE = 0.0
	ELSE
		PERCENTAGE = FUNC_ROUND((CURRENT_PAYMENT / &
			(AR_TEMP::INVAMT - AR_TEMP::OTHAMT)) * 100.0, 2%)

		IF (PERCENTAGE > 99.99) AND (PERCENTAGE < 100.01)
		THEN
			PERCENTAGE = 0.0
		END IF
	END IF

	!
	! If we made it this far, we can print the salesman information
	!
	IF SALESLINE_FLAG%
	THEN
		TEXT$ = SA_SALESMAN::SALESMAN + SPACE$(12%) + &
			SA_SALESMAN::DESCR + "  " + &
			SA_SALESMAN::TTYPE + "    " + &
			SA_SALESMAN::CLASS

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -2%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

		SALESLINE_FLAG% = 0%
	END IF

	!
	! Subtotal by customer?
	!
	IF LAST_CUSTOMER$ <> AR_TEMP::CUSNUM
	THEN
		GOSUB CustomerTotal
	END IF

	!
	! Print the cash receipt line
	!
	TEXT$ = "     " + AR_TEMP::INVNUM + "  " + &
		PRNT_DATE(AR_TEMP::TRADAT, 6%) + "  " + &
		AR_TEMP::CUSNUM + "  " + &
		AR_TEMP::CUSNAM + "  " + &
		FORMAT$(AR_TEMP::INVAMT - AR_TEMP::OTHAMT, "###,###,###.##") + &
		FORMAT$(CURRENT_PAYMENT, "###,###,###.##") + &
		FORMAT$(PERCENTAGE, "<%>###.##%")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	SALESMAN_TOTAL_INVOICE = &
		SALESMAN_TOTAL_INVOICE + AR_TEMP::INVAMT
	SALESMAN_TOTAL_OTHER = &
		SALESMAN_TOTAL_OTHER + AR_TEMP::OTHAMT
	SALESMAN_CURRENT_PAYMENT = &
		SALESMAN_CURRENT_PAYMENT + CURRENT_PAYMENT

	CUSTOMER_TOTAL_INVOICE = &
		CUSTOMER_TOTAL_INVOICE + AR_TEMP::INVAMT
	CUSTOMER_TOTAL_OTHER = &
		CUSTOMER_TOTAL_OTHER + AR_TEMP::OTHAMT
	CUSTOMER_CURRENT_PAYMENT = &
		CUSTOMER_CURRENT_PAYMENT + CURRENT_PAYMENT

	GRAND_TOTAL_INVOICE = &
		GRAND_TOTAL_INVOICE + AR_TEMP::INVAMT
	GRAND_TOTAL_OTHER = &
		GRAND_TOTAL_OTHER + AR_TEMP::OTHAMT
	GRAND_CURRENT_PAYMENT = &
		GRAND_CURRENT_PAYMENT + CURRENT_PAYMENT

	LAST_CUSTOMER% = -1%

 ExitPrintInvoice:

	GOTO 17510

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

		!
		! Calculate percentage
		!
		IF (SALESMAN_TOTAL_INVOICE - SALESMAN_TOTAL_OTHER) = 0.0
		THEN
			PERCENTAGE = 0.0
		ELSE
			PERCENTAGE = FUNC_ROUND((SALESMAN_CURRENT_PAYMENT / &
				(SALESMAN_TOTAL_INVOICE - &
				SALESMAN_TOTAL_OTHER)) * 100.0, 2%)
		END IF

		TEXT$ = "                 " + &
			"          " + &
			"          " + &
			"                Salesman Total" + "  " + &
			FORMAT$(SALESMAN_TOTAL_INVOICE - &
				SALESMAN_TOTAL_OTHER, "###,###,###.##") + &
			FORMAT$(SALESMAN_CURRENT_PAYMENT, "###,###,###.##") + &
			FORMAT$(PERCENTAGE, "<%>###.##%")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	SALESMAN_TOTAL_INVOICE = 0.0
	SALESMAN_TOTAL_OTHER = 0.0
	SALESMAN_CURRENT_PAYMENT = 0.0

	RETURN

 CustomerTotal:

	!
	! Print a total for one customer
	!
	IF (CUS_TOTAL$ = "Y") AND (LAST_CUSTOMER% <> 0%)
	THEN
		IF (CUSTOMER_TOTAL_INVOICE - CUSTOMER_TOTAL_OTHER) = 0.0
		THEN
			PERCENTAGE = 0.0
		ELSE
			PERCENTAGE = FUNC_ROUND((CUSTOMER_CURRENT_PAYMENT / &
				(CUSTOMER_TOTAL_INVOICE - &
				CUSTOMER_TOTAL_OTHER)) * 100.0, 2%)
		END IF

		TEXT$ = "                 " + &
			"          " + &
			"          " + &
			"                Customer Total" + "  " + &
			FORMAT$(CUSTOMER_TOTAL_INVOICE - &
				CUSTOMER_TOTAL_OTHER, "###,###,###.##") + &
			FORMAT$(CUSTOMER_CURRENT_PAYMENT, "###,###,###.##") + &
			FORMAT$(PERCENTAGE, "<%>###.##%")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

	END IF

	LAST_CUSTOMER% = 0%
	LAST_CUSTOMER$ = AR_TEMP::CUSNUM

	CUSTOMER_TOTAL_INVOICE = 0.0
	CUSTOMER_TOTAL_OTHER = 0.0
	CUSTOMER_CURRENT_PAYMENT = 0.0

	RETURN

 ExitTotal:
	!
	! Handle end of report
	!
	GOSUB CustomerTotal
	GOSUB SalesmanTotal

	IF (GRAND_TOTAL_INVOICE - GRAND_TOTAL_OTHER) = 0.0
	THEN
		PERCENTAGE = 0.0
	ELSE
		PERCENTAGE = FUNC_ROUND((GRAND_CURRENT_PAYMENT / &
			(GRAND_TOTAL_INVOICE - &
			GRAND_TOTAL_OTHER)) * 100.0, 2%)
	END IF

	TEXT$ = "                 " + &
		"          " + &
		"          " + &
		"                   Grand Total" + "  " + &
		FORMAT$(GRAND_TOTAL_INVOICE - &
			GRAND_TOTAL_OTHER, "###,###,###.##") + &
		FORMAT$(GRAND_CURRENT_PAYMENT, "###,###,###.##") + &
		FORMAT$(PERCENTAGE, "<%>###.##%")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 999%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

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
