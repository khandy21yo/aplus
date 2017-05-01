1	%TITLE "Accounts Payable Age Detail Report"
	%SBTTL "AP_RPRT_AGE_DETAIL"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1987, 1988 BY
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
	! ID:APAGED
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Payables Detail Aging Report\* option
	!	prints a report displaying each
	!	item owed each vendor and aging those items in categories ranging
	!	from "Current" to "121 days _& Over" in thirty day increments.
	!	.b
	!	At the user's option, the report may be aged from the date
	!	of the vendors' invoices or from the date the payment is due on
	!	the invoices.
	!	The ^*Aging Report\* contains the following fields:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	Vendor Number
	!	.le
	!	Vendor Name
	!	.le
	!	Invoice Date
	!	.le
	!	Invoice Number
	!	.le
	!	Current
	!	.le
	!	31 to 60 Days
	!	.le
	!	61 to 90 Days
	!	.le
	!	91 to 120 Days
	!	.le
	!	121 Days and Over
	!	.le
	!	Balance
	!	.els
	!
	! Index:
	!	.x Accounts Payable>Reports>Detail Aging
	!	.x Accounts Payable>Reports>Detail Aging
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_RPRT_AGE_DETAIL/LINE
	!	$ LINK/EXECUTABLE=AP_EXE:*.EXE AP_RPRT_AGE_DETAIL, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_RPRT_AGE_DETAIL.OBJ;*
	!
	! Author:
	!
	!	07/31/87 - B. Craig Larsen
	!
	! Modification history:
	!
	!	08/25/88 - Kevin Handy
	!		Modified to show correct invoice number/date on
	!		the amount.
	!
	!	05/21/91 - Kevin Handy
	!		Unwound error trapping.
	!
	!	05/21/91 - Kevin Handy
	!		Changed aging from strange for-next loop
	!		to a simpler division.
	!
	!	05/21/91 - Kevin Handy
	!		Added cutoff date.
	!
	!	05/21/91 - Kevin Handy
	!		Modifications to handling of base date.
	!		Use functions to process date instead of mid's.
	!
	!	09/06/91 - Kevin Handy
	!		Modified so that title will change date field
	!		title depending on what is printed.
	!
	!	09/17/91 - Kevin Handy
	!		Modified so that Zero balances can be suppressed.
	!		This is probibly 99.99% of the time, but I made
	!		it optional since it wasn't that way before.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	03/18/96 - Kevin Handy
	!		Reformat source code.
	!
	!	03/18/96 - Kevin Handy
	!		Add wildcard ap account.
	!		Fix OPTDEF numbering (cutoff period was 5, s/b 6)
	!
	!	05/13/97 - Kevin Handy
	!		Reformat source code.
	!
	!	08/20/97 - Kevin Handy
	!		Don't need to assign channel for report.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose an excessive number of %PAGE's
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!******************************************************************
	! External modules needed
	!******************************************************************

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!******************************************************************
	! Set up data storage areas (MAPs, DIMENSIONs, DECLAREs, etc.)
	!******************************************************************

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD	UTL_REPORTX

	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.HB"
	MAP	(AP_OPEN)	AP_OPEN_CDD	AP_OPEN

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP	(AP_VENDOR)	AP_VENDOR_CDD	AP_VENDOR

	%PAGE

	!******************************************************************
	! Take care of anything else before starting the report
	!******************************************************************

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

 Initialization:
	!******************************************************************
	! Get ready to begin
	!******************************************************************

	!
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
	!	The ^*From Item\* field determines the vendor number
	!	(or vendor name, if the report is to be printed in alphabetical
	!	order) with which the report will begin. If the setting is blank,
	!	the report will begin with the first vendor number in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Detail Aging Report
	!	.x Detail Aging Report>From Item
	!	.x From>Item
	!	.x Item>From
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field determines the vendor number
	!	(or vendor name if the report is to be printed in alphabetical
	!	order) with which the report will end. If this setting is blank,
	!	the report will end with the last vendor in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Detail Aging Report
	!	.x Detail Aging Report>To Item
	!	.x To>Item
	!	.x Item>To
	!
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Sort By (NU,S)\*
	!	.b
	!	.lm +5
	!	The ^*Sort By (NU,S)\* field determines if the
	!	report is to be printed in vendor number order or in alphabetical
	!	order.
	!	.lm 15
	!	.LS "*"
	!	.LE
	!	^*NU\*#=#Vendor Number order
	!	.LE
	!	^*S\*##=#Alphabetical order
	!	.LE
	!	^*Blank\* = A blank setting will cause the report to print in numerical
	!	order.
	!	.ELS
	!	.lm -5
	!	There are no other valid values for this field.
	!
	! Index:
	!	.x Sort By>Detail Aging Report
	!	.x Detail Aging Report>Sort By
	!
	!--

	DATE_TO_USE$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Date to use (I,D)\*
	!	.b
	!	.lm +5
	!	The ^*Date to use (I,D)\* field determines if the
	!	aging is dependent upon the dates of vendors' invoices or the dates
	!	that payment of the invoices is due.
	!	.lm 15
	!	.LS "*"
	!	.LE
	!	^*I\*#=#Invoice dates
	!	.LE
	!	^*D\*
	!	#=#Due Dates
	!	.ELS
	!	.lm -5
	!	There are no other valid values for this field.
	!
	! Index:
	!	.x Date to use>Detail Aging Report
	!	.x Detail Aging Report>Date to use
	!
	!--

	REP_DATE$ = DATE_STOREDATE(UTL_REPORTX::OPTDEF(4%))

	!++
	! Abstract:FLD05
	!	^*(05) Age Date (MMDDYYYY)\*
	!	.b
	!	.lm +5
	!	The ^*Age Date\* field determines the base date
	!	from which ages of the Accounts Payable items will be calculated.
	!	The date is to be entered in MMDDYYYY format.
	!	.lm -5
	!
	! Index:
	!	.x Age Date>Detail Aging Report
	!	.x Detail Aging Report>Age Date
	!
	!--

	CUTOFF_DATE$ = TRM$(LEFT(UTL_REPORTX::OPTDEF(5%), 6%))
	BASE_DAY% = DATE_DAYCODE(REP_DATE$)

	!++
	! Abstract:FLD06
	!	^*(06) Cutoff Period\*
	!	.b
	!	.lm +5
	!	The ^*Cutoff Period\* field generates
	!	a "cutoff register" aging report.  Entries posted after the
	!	cutoff period will not appear on the report.
	!	.b
	!	The format for entry is YYYYPP.
	!	.lm -5
	!
	! Index:
	!	.x Cutoff Period>Aging Report
	!	.x Aging Report>Cutoff Period
	!
	!--

	ZERO_BALANCE$ = LEFT(UTL_REPORTX::OPTDEF(6%), 1%)

	!++
	! Abstract:FLD07
	!	^*(07) Show Zero Balance\*
	!	.b
	!	.lm +5
	!	The ^*Show Zero Balance\* field indicates if the report
	!	is to contain invoices that balance to zero (Y), or to
	!	only show invoices with a non-zero amount (N).
	!	.lm -5
	!
	! Index:
	!	.x Show Zero Balance>Aging Report
	!	.x Zero Balance>Aging Report
	!	.x Balance>Aging Report
	!	.x Aging Report>Show Zero Balance
	!
	!--

	WILD_ACCT$ = TRM$(UTL_REPORTX::OPTDEF(7%))

	!++
	! Abstract:FLD08
	!	^*(08) Show Zero Balance\*
	!	.b
	!	.lm +5
	!	The ^*Show Zero Balance\* field indicates if the report
	!	is to contain invoices that balance to zero (Y), or to
	!	only show invoices with a non-zero amount (N).
	!	.lm -5
	!
	! Index:
	!	.x Show Zero Balance>Aging Report
	!	.x Zero Balance>Aging Report
	!	.x Balance>Aging Report
	!	.x Aging Report>Show Zero Balance
	!
	!--

	SELECT SORTBY$

	CASE "NU"
		K_NUM% = 0%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(AP_VENDOR::VENNUM))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(AP_VENDOR::VENNUM))

	CASE "NA"
		K_NUM% = 1%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(AP_VENDOR::VENNAM))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(AP_VENDOR::VENNAM))

	CASE ELSE
		K_NUM% = 2%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(AP_VENDOR::ALPSRT))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(AP_VENDOR::ALPSRT))

	END SELECT

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.OPN"
	USE
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

	!
	! Open the AP Vendor file
	!
310	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.OPN"
	USE
		FILENAME$ = "AP_VENDOR"
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
	!
	! Title
	!
	IF DATE_TO_USE$ = "D"
	THEN
		TITLE$(1%) = "Accounts Payable Aged Detail Using the Due Date"
	ELSE
		TITLE$(1%) = &
			"Accounts Payable Aged Detail Using the Invoice Date"
	END IF

	TITLE$(2%) = "Aged as of " + PRNT_DATE(REP_DATE$, 8%)

	I% = 3%

	IF CUTOFF_DATE$ <> ""
	THEN
		TITLE$(I%) = "Cutoff Period: " + CUTOFF_DATE$
		I% = I% + 1%
	END IF

	TITLE$(I%) = ""

	!
	! Heading
	!
	IF DATE_TO_USE$ = "D"
	THEN
		TITLE$(I% + 1%) = "VendorNum  VendorName          DueDate      " + &
			"InvoiceNum      Current     31 to 60     61 to 90    " + &
			"91 to 120   121 & Over      Balance"
	ELSE
		TITLE$(I% + 1%) = "VendorNum  VendorName          InvDate      " + &
			"InvoiceNum      Current     31 to 60     61 to 90    " + &
			"91 to 120   121 & Over      Balance"
	END IF

	TITLE$(I% + 2%) = ""

	!
	! Layouts for printed lines
	!
	!LYT_LINE$ = "$VendorNum:010,$VendorName:030,$InvNum:045," + &
	!	"DDate:054,VBalance1:067,VBalance2:080,VBalance3:093," + &
	!	"VBalance4:106,VBalance5:119,VBalance6:132"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #AP_VENDOR.CH%, KEY #K_NUM%
		ELSE
			FIND #AP_VENDOR.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		FILENAME$ = "AP_VENDOR"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17100	!******************************************************************
	! Main report loop starts here
	!******************************************************************

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #AP_VENDOR.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "AP_VENDOR"
		CONTINUE HelpError
	END WHEN

	!
	! Check status
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Check current record
	!
	SELECT SORTBY$

	CASE "NU"
		GOTO ExitTotal IF (AP_VENDOR::VENNUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""

	CASE "NA"
		GOTO ExitTotal IF (AP_VENDOR::VENNAM > TO_ITEM$) AND &
			TO_ITEM$ <> ""

	CASE ELSE
		GOTO ExitTotal IF (AP_VENDOR::ALPSRT > TO_ITEM$) AND &
			TO_ITEM$ <> ""

	END SELECT

	!
	! Get ready to start a sub-loop
	!
17200	WHEN ERROR IN
		FIND #AP_OPEN.CH%, KEY #0% EQ AP_VENDOR::VENNUM, REGARDLESS
	USE
		CONTINUE GetNextRec IF ERR = 155%
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

	TEXT$ = AP_VENDOR::VENNUM + " " + LEFT(AP_VENDOR::VENNAM, 19%) + " "
	TRANKEY$ = "ZZZZZZZZZZZZZZZZZ"
	INVKEY$ = ""

	AGE_BAL(LOOP%), VENDOR_TOTAL(LOOP%) = 0.0 &
		FOR LOOP% = 1% TO 5%

	LINE_HAS_BEEN_PRINTED% = 0%
	PRINT_LINE% = 0%

	!
	! Sub-loop starts here
	!
17210	WHEN ERROR IN
		GET #AP_OPEN.CH%, REGARDLESS
	USE
		CONTINUE 17220 IF ERR = 11%
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

	GOTO 17220 IF AP_VENDOR::VENNUM <> AP_OPEN::VENNUM

	!
	! Skip record if past cutoff date
	!
	IF (CUTOFF_DATE$ <> "") AND (CUTOFF_DATE$ < LEFT(AP_OPEN::UPDATED, 6%))
	THEN
		GOTO 17210
	END IF

	!
	! Check on the account number
	!
	IF (WILD_ACCT$ <> "")
	THEN
		GOTO 17210 &
			IF COMP_STRING(TRM$(AP_OPEN::AP_ACCT), WILD_ACCT$) = 0%
	END IF

	IF AP_OPEN::TRANKEY <> TRANKEY$
	THEN
		!
		! Print info before screwing around with the titles
		!
		IF PRINT_LINE%
		THEN
			GOSUB PrintLine
			GOTO ExitProgram IF UTL_REPORTX::STAT

			PRINT_LINE% = 0%
		END IF

		IF DATE_TO_USE$ = "D"
		THEN
			AGE_DAY% = BASE_DAY% - DATE_DAYCODE(AP_OPEN::DUEDAT)
			AGE_DAY$ = PRNT_DATE(AP_OPEN::DUEDAT, 6%)
		ELSE
			AGE_DAY% = BASE_DAY% - DATE_DAYCODE(AP_OPEN::INVDAT)
			AGE_DAY$ = PRNT_DATE(AP_OPEN::INVDAT, 6%)
		END IF

		!
		! Calculate Aging Period
		!
		AGE_INTRVL% = (AGE_DAY% + 1%) / 30% + 1%
		AGE_INTRVL% = 1% IF AGE_INTRVL% < 1%
		AGE_INTRVL% = 5% IF AGE_INTRVL% > 5%

	END IF

	TRANKEY$ = AP_OPEN::TRANKEY
	INVKEY$ = AP_OPEN::INVNUM

	AGE_BAL(AGE_INTRVL%) = FUNC_ROUND(AGE_BAL(AGE_INTRVL%) + &
		(AP_OPEN::INVAMT - AP_OPEN::DISAMT) - AP_OPEN::CKAMT, 2%)

	PRINT_LINE% = -1%

	GOTO 17210

	!
	! Exit from Sub-loop; print out Vendor totals
	!
17220	GOSUB PrintLine IF PRINT_LINE%

	GOSUB VendorTotal
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Try for next record
	!
	GOTO GetNextRec

	%PAGE

	!******************************************************************
	! Handle totals and other items before EXITing
	!******************************************************************

 ExitTotal:
	!
	! Print out totals
	!
	TEXT$ = "          Grand Total" + SPACE$(33%)

	FOR LOOP% = 1% TO 6%

		IF TOTAL(LOOP%) <> 0.0
		THEN
			TEXT$ = TEXT$ + FORMAT$(TOTAL(LOOP%), " #,###,###.##")
		ELSE
			TEXT$ = TEXT$ + SPACE$(13%)
		END IF

	NEXT LOOP%

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

 ExitProgram:
	!
	! Finish up the report
	!
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

 PrintLine:
	!******************************************************************
	! Subroutine to print one line of the report
	!******************************************************************

	AGE_BAL(6%) = 0.0

	AGE_BAL(6%) = FUNC_ROUND(AGE_BAL(6%) + AGE_BAL(LOOP%), 2%) &
		FOR LOOP% = 1% TO 5%

	IF (AGE_BAL(6%) <> 0.0) OR (ZERO_BALANCE$ = "Y")
	THEN
		TEXT$ = TEXT$ + AGE_DAY$ + " " + LEFT(INVKEY$, 13%) + " "

		FOR LOOP% = 1% TO 5%

			IF AGE_BAL(LOOP%) <> 0.0
			THEN
				TEXT$ = TEXT$ + &
					FORMAT$(AGE_BAL(LOOP%), " #,###,###.##")
			ELSE
				TEXT$ = TEXT$ + SPACE$(13%)
			END IF

		NEXT LOOP%

		TEXT$ = TEXT$ + FORMAT$(AGE_BAL(6%), " #,###,###.##")

		VENDOR_TOTAL(LOOP%) = FUNC_ROUND(VENDOR_TOTAL(LOOP%) + &
			AGE_BAL(LOOP%), 2%) &
			FOR LOOP% = 1% TO 6%

		LINE_HAS_BEEN_PRINTED% = -1%

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		TEXT$ = SPACE$(31%)

	END IF

	AGE_BAL(LOOP%) = 0.0 FOR LOOP% = 1% TO 6%

 ComeBack1:
	RETURN

	%PAGE

 VendorTotal:
	!******************************************************************
	! Subroutine to print AP Vendor totals
	!******************************************************************
	VENDOR_TOTAL_TEST% = 0%

	VENDOR_TOTAL_TEST% = -1% IF FUNC_ROUND(VENDOR_TOTAL(LOOP%), 2%) <> 0.0 &
		FOR LOOP% = 1% TO 5%

	GOTO ComeBack2 &
		IF (VENDOR_TOTAL_TEST% = 0%) AND &
		(LINE_HAS_BEEN_PRINTED% = 0%)

	VENDOR_TOTAL(6%) = 0.0

	VENDOR_TOTAL(6%) = &
		FUNC_ROUND(VENDOR_TOTAL(6%) + VENDOR_TOTAL(LOOP%), 2%) &
		FOR LOOP% = 1% TO 5%

	TEXT$ = "     Vendor Total" + SPACE$(37%)

	FOR LOOP% = 1% TO 5%

		IF VENDOR_TOTAL(LOOP%) <> 0.0
		THEN
			TEXT$ = TEXT$ + &
				FORMAT$(VENDOR_TOTAL(LOOP%), " #,###,###.##")
		ELSE
			TEXT$ = TEXT$ + SPACE$(13%)
		END IF

	NEXT LOOP%

	TEXT$ = TEXT$ + FORMAT$(VENDOR_TOTAL(6%), " #,###,###.##")

	TOTAL(LOOP%) = FUNC_ROUND(TOTAL(LOOP%) + VENDOR_TOTAL(LOOP%), 2%) &
		FOR LOOP% = 1% TO 6%

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO ComeBack2 IF UTL_REPORTX::STAT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

 ComeBack2:
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
	! Handle untrapped errors
	!
	FILENAME$ = ""
	RESUME HelpError

32767	!******************************************************************
	! End of report AP_RPRT_AGE_DETAIL
	!******************************************************************
	END
