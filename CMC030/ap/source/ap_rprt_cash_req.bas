1	%TITLE "Accounts Payable Cash Requirements Report"
	%SBTTL "AP_RPRT_CASH_REQ"
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
	! ID:APCSHR
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Cash Requirements Report\* option
	!	prints a report which details
	!	each item owed each vendor and projects the cash requirements
	!	in order to meet the obligations to vendors in a timely manner.
	!	.b
	!	The cash requirements can be projected based on discount
	!	dates, due dates, or invoice dates.
	!	.b
	!	The cash requirement intervals are "Past Due", "Current to
	!	7 Days", "8 to 14 Days", "12 to 21 Days", "22 Days and over".
	!	.b
	!	This report consists of the following fields:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	Vendor Number
	!	.le
	!	Vendor Name
	!	.le
	!	Invoice Number
	!	.le
	!	Due Date
	!	.le
	!	Past Due
	!	.le
	!	Current to 7 Days
	!	.le
	!	8 to 14 Days
	!	.le
	!	12 to 21 Days
	!	.le
	!	22 Days and Over
	!	.le
	!	Balance
	!	.els
	!
	! Index:
	!	.x Accounts payable>Reports>Cash requirements
	!	.x Reports>Cash requirements
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_RPRT_CASH_REQ/LINE
	!	$ LINK/EXECUTABLE=AP_EXE:*.EXE AP_RPRT_CASH_REQ, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_RPRT_CASH_REQ.OBJ;*
	!
	! Author:
	!
	!	07/31/87 - B. Craig Larsen
	!
	! Modification history:
	!
	!	09/21/88 - Kevin Handy
	!		Modified so line title will change depending
	!		which date is selected.
	!
	!	03/12/92 - Kevin Handy
	!		Unrolled error trapping
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	03/19/96 - Kevin Handy
	!		Reformat source code.
	!
	!	03/19/96 - Kevin Handy
	!		Added wildcard ap account.
	!
	!	03/22/96 - Kevin Handy
	!		Added line numbers between 18000 to 18999, so I can
	!		track down an error easier.
	!
	!	08/27/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/20/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/23/2000 - Kevin Handy
	!		Use WHEN ERROR IN
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

	!
	! Set up data storage areas (MAPs, DIMENSIONs, DECLAREs, etc.)
	!
	DIM DAYS%(6%)

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE UTL_REPORTX_CDD	UTL_REPORTX

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

	!
	! Set up days array
	!
	DAYS%(1%) = 0%
	DAYS%(I%) = 7% FOR I% = 2% TO 5%

 Initialization:
	!******************************************************************
	! Get ready to begin
	!******************************************************************

	!
	! Initialize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field determine the vendor number
	!	with which the report will begin. If the setting is blank,
	!	the report will begin with the first vendor in the file.
	!
	! Index:
	!	.x From Item>Cash Requirements Report
	!	.x Cash Requirements Report>From Item
	!	.x From>Item
	!	.x ITem>From
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field determine the vendor number
	!	with which the report is to end. If this setting is blank,
	!	the report will end with the last vendor in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Cash Requirements Report
	!	.x Cash Requirements Report>To Item
	!
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Sort Order (NU,S)\*
	!	.b
	!	.lm +5
	!	The ^*Sort Order (NU,S)\* field determines if the
	!	report is to be printed in vendor number order or in alphabetical
	!	order.
	!	.b
	!	.lm 15
	!	.LS 0,"*"
	!	.LE
	!	^*NU\*#=#Vendor Number order
	!	.LE
	!	^*S\*##=#Alphabetical order
	!	.ELS
	!	.lm -5
	!	A blank setting will cause the report to print in numerical
	!	order.
	!	.b
	!	There are no other valid values for this field.
	!
	! Index:
	!	.x Sort Order>Cash Requirements Report
	!	.x Cash Requirements Report>Sort Order
	!
	!--

	DATE_TO_USE$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Date to use (DI,I,D)\*
	!	.b
	!	.lm +5
	!	The ^*Date to use (DI,I,D)\* field determines
	!	whether the Cash Requirements report will be based on discount
	!	date, invoice date, or due date.
	!	.b
	!	.lm 15
	!	.LS 0,"*"
	!	.LE
	!	^*DI\*#=#Discount Date
	!	.LE
	!	^*I\*##=#Invoice Date
	!	.LE
	!	^*D\*##=#Due Date
	!	.ELS
	!
	! Index:
	!	.x Date to use>Cash Requirements Report
	!	.x Cash Requirements Report>Date to use
	!
	!--

	REP_DATE$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Age Date (MMDDYYYY)\*
	!	.b
	!	.lm +5
	!	The ^*Age Date\* field determines the date from
	!	which cash requirements will be projected. Ordinarily, the
	!	current date would be entered in MMDDYYYY format.
	!	.lm -5
	!
	! Index:
	!	.x Age Date>Cash Requirements Report
	!	.x Cash Requirements Report>Age Date
	!	.x Age>Date
	!	.x Date>Age
	!
	!--

	WILD_ACCT$ = TRM$(UTL_REPORTX::OPTDEF(5%))

	!++
	! Abstract:FLD06
	!	^*(06) Wildcard AP Account\*
	!	.b
	!	.lm +5
	!	This field specifies which AP account is to be printed.
	!	.b
	!	Leaving this fiel blank will cause all AP accounts
	!	to be printed.
	!	.lm -5
	!
	! Index:
	!	.x AP Account>Cash Requirements Report
	!	.x Cash Requirements Report>AP Account
	!
	!--

	SELECT SORTBY$

	CASE "NU"
		K_NUM% = 0%

	CASE "NA"
		K_NUM% = 1%

	CASE ELSE
		K_NUM% = 2%

	END SELECT

	BASE.DAY% = DATE_DAYCODE(DATE_STOREDATE(REP_DATE$))

	%PAGE

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.OPN"
	USE
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

	!
	! Open the Accounts Payable Vendor file
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
	SELECT DATE_TO_USE$

	CASE "D"
		TEMP$ = "Due Date"

	CASE "DI"
		TEMP$ = "Discount Date"

	CASE ELSE
		TEMP$ = "Invoice Date"
	END SELECT

	TITLE$(1%) = "AP Cash Requirements Report by " + TEMP$
	TITLE$(2%) = "Aged as of " + REP_DATE$
	TITLE$(3%) = ""

	!
	! Heading
	!
	SELECT DATE_TO_USE$
	CASE "D"
		TEMP$ = "DueDate"

	CASE "DI"
		TEMP$ = "DscDate"

	CASE ELSE
		TEMP$ = "InvDate"
	END SELECT

	TITLE$(4%) = "VendorNum  VendorName          InvoiceNum     " + &
		TEMP$ + &
		"       PastDue     Cur to 7      " + &
		"8 to 14     15 to 21    22 & Over      Balance"
	TITLE$(5%) = ""

	!
	! Layouts for lines printed
	!
	LYT_LINE$ = "$VendorNum:010,$VendorName:030,$InvoiceNum:045," + &
		"VBalance1:059,VBalance2:072,VBalance3:085,VBalance4:098," + &
		"VBalance5:111,VBalance6:124"

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
		CONTINUE ExitTotal IF (ERR = 11%)
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
		CONTINUE GetNextRec IF (ERR = 155%)
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

	TEMP_TEXT$ = AP_VENDOR::VENNUM + " " + &
		LEFT(AP_VENDOR::VENNAM, 19%) + " "

	!
	! Assign some initial values
	!
	AGE_BAL(LOOP%),	VENDOR_TOTAL(LOOP%) = 0.0 &
		FOR LOOP% = 1% TO 5%
	LINE_HAS_BEEN_PRINTED%,	PRINT_LINE% = 0%
	TRANKEY$ = "ZZZZZZZZZZZ"

	!
	! Sub-loop begins here
	!
17210	WHEN ERROR IN
		GET #AP_OPEN.CH%, REGARDLESS
	USE
		CONTINUE 17300 IF (ERR = 11%)
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

	GOTO 17300 IF AP_VENDOR::VENNUM <> AP_OPEN::VENNUM

	IF WILD_ACCT$ <> ""
	THEN
		GOTO 17210 IF COMP_STRING(AP_OPEN::AP_ACCT, WILD_ACCT$) = 0%
	END IF

	IF AP_OPEN::TRANKEY <> TRANKEY$
	THEN
		IF PRINT_LINE%
		THEN

			GOSUB PrintLine
			GOTO ExitProgram IF UTL_REPORTX::STAT

			PRINT_LINE% = 0%
			TEXT$ = ""

		END IF

		TEXT$ = TEMP_TEXT$ + &
			LEFT(AP_OPEN::INVNUM, 14%) + " "

		SELECT DATE_TO_USE$

		CASE "D"
			AGE_DAY% = DATE_DAYCODE(AP_OPEN::DUEDAT) - BASE.DAY%
			TEXT$ = TEXT$ + PRNT_DATE(AP_OPEN::DUEDAT, 6%)

		CASE "DI"
			AGE_DAY% = DATE_DAYCODE(AP_OPEN::DISCDAT) - BASE.DAY%
			TEXT$ = TEXT$ + PRNT_DATE(AP_OPEN::DISCDAT, 6%)

		CASE ELSE
			AGE_DAY% = DATE_DAYCODE(AP_OPEN::INVDAT) - BASE.DAY%
			TEXT$ = TEXT$ + PRNT_DATE(AP_OPEN::INVDAT, 6%)

		END SELECT

		BEG_DAY%, AGE_INTRVL% = 0%

		FOR LOOP% = 1% TO 5%
			BEG_DAY% = BEG_DAY% + DAYS%(LOOP%)
			AGE_INTRVL% = LOOP% &
				IF (AGE_INTRVL% = 0%) AND (AGE_DAY% <= BEG_DAY%)
		NEXT LOOP%

		AGE_INTRVL% = 5% IF AGE_INTRVL% = 0%

	END IF

	TRANKEY$ = AP_OPEN::TRANKEY

	AGE_BAL(AGE_INTRVL%) = FUNC_ROUND(AGE_BAL(AGE_INTRVL%) + &
		(AP_OPEN::INVAMT - AP_OPEN::DISAMT) - AP_OPEN::CKAMT, 2%)

	PRINT_LINE% = -1%

	GOTO 17210

17300	GOSUB PrintLine IF PRINT_LINE%
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOSUB VendorTotal
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Try for next record
	!
	GOTO GetNextRec

	%PAGE

18000	!******************************************************************
	! Handle totals and other items before EXITing
	!******************************************************************

 ExitTotal:
	!
	! Handle end of report
	!
	TEXT$ = "     Grand Total" + SPACE$(38%)

	FOR LOOP% = 1% TO 6%

		TEXT$ = TEXT$ + FORMAT$(TOTAL(LOOP%), "<%>#,###,###.##")

	NEXT LOOP%

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

 ExitProgram:
18100	!
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
18200	!******************************************************************
	! Subroutine to print out one line of the report
	!******************************************************************
	VENDOR_TOTAL_TEST% = 0%

	VENDOR_TOTAL_TEST% = -1% &
		IF (FUNC_ROUND(AGE_BAL(LOOP%), 2%) <> 0.0) &
		FOR LOOP% = 1% TO 5%

	GOTO ComeBack1 IF VENDOR_TOTAL_TEST% = 0%

	AGE_BAL(6%) = 0.0
	AGE_BAL(6%)  = FUNC_ROUND(AGE_BAL(6%) + AGE_BAL(LOOP%), 2%) &
		FOR LOOP% = 1% TO 5%

	FOR LOOP% = 1% TO 5%
		TEXT$ = TEXT$ + FORMAT$(AGE_BAL(LOOP%), "<%>#,###,###.##")
	NEXT LOOP%

	TEXT$ = TEXT$ + FORMAT$(AGE_BAL(6%), " #,###,###.##")

	VENDOR_TOTAL(LOOP%) = &
		FUNC_ROUND(VENDOR_TOTAL(LOOP%) + AGE_BAL(LOOP%), 2%) &
		FOR LOOP% = 1% TO 6%

	AGE_BAL(LOOP%) = 0.0 &
		FOR LOOP% = 1% TO 6%

	TEMP_TEXT$ = SPACE$(31%)

	LINE_HAS_BEEN_PRINTED% = -1%

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)

 ComeBack1:
	RETURN

	%PAGE

 VendorTotal:
18300	!******************************************************************
	! Subroutine to print out the Vendor total
	!******************************************************************
	VENDOR_TOTAL_TEST% = 0%

	VENDOR_TOTAL_TEST% = -1% &
		IF FUNC_ROUND(VENDOR_TOTAL(LOOP%), 2%) <> 0.0 &
		FOR LOOP% = 1% TO 5%

	GOTO ComeBack2 &
		IF (VENDOR_TOTAL_TEST% = 0%) AND (LINE_HAS_BEEN_PRINTED% = 0%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ComeBack2 IF UTL_REPORTX::STAT

	VENDOR_TOTAL(6%) = 0.0
	VENDOR_TOTAL(6%) = &
		FUNC_ROUND(VENDOR_TOTAL(6%) + VENDOR_TOTAL(LOOP%), 2%) &
		FOR LOOP% = 1% TO 5%

	TEXT$ = "     Totals for Vendor Number " + AP_VENDOR::VENNUM + &
		SPACE$(14%)

	FOR LOOP% = 1% TO 5%
		TEXT$ = TEXT$ + FORMAT$(VENDOR_TOTAL(LOOP%), "<%>#,###,###.##")
	NEXT LOOP%

	TEXT$ = TEXT$ + FORMAT$(VENDOR_TOTAL(6%), " #,###,###.##")

	TOTAL(LOOP%) = FUNC_ROUND(TOTAL(LOOP%) + VENDOR_TOTAL(LOOP%), 2%) &
		FOR LOOP% = 1% TO 6%

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ComeBack2 IF UTL_REPORTX::STAT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
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
	! End of report AP_RPRT_CASH_REQ
	!******************************************************************
	END
