1	%TITLE "Accounts Payable Vendor Analysis"
	%SBTTL "AP_RPRT_VENDOR_ANAL"
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
	! ID:APANAL
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Vendor Analysis Report\* option
	!	prints a report which analyzes
	!	the data in both the Accounts Payable Open file and the Accounts
	!	Payable Closed file in respect to each vendor.
	!	.b
	!	The report accumulates each vendor's charges and total
	!	payments made to each vendor for each accounting period, both
	!	currently and historically. The amounts for all periods are
	!	totaled and the period totals are calculated as a percentage
	!	of the vendor totals. The number of transactions per period
	!	and the average amount per transaction are also displayed.
	!	.b
	!	The following fields are included in this report:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	Vendor Number
	!	.le
	!	Vendor Name
	!	.le
	!	Period
	!	.le
	!	Net Billed
	!	.le
	!	Net Billed Percentage of Total
	!	.le
	!	Number of Transactions
	!	.le
	!	Average Amount
	!	.le
	!	Payments
	!	.le
	!	Payment Percentage of Total
	!	.els
	!
	! Index:
	!	.x Accounts Payable>Reports>Vendor Analysis
	!	.x Report>Vendor Analysis Report
	!
	! Option:
	!
	! Author:
	!
	!	07/31/87 - B. Craig Larsen
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_RPRT_VENDOR_ANAL/LINE
	!	$ LINK/EXECUTABLE=AP_EXE:*.EXE AP_RPRT_VENDOR_ANAL, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_RPRT_VENDOR_ANAL.OBJ;*
	!
	! Modification history:
	!
	!	06/21/90 - Aaron Redd
	!		Added line layout information so that the report could
	!		also be sent to either a spreadsheet or a DIF file.
	!
	!	06/06/91 - Kevin Handy
	!		Unwound error trapping.
	!
	!	04/01/92 - Kevin Handy
	!		Clean up (check)
	!
	!	03/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/29/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/13/97 - Kevin Handy
	!		Reformat source code.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/25/2000 - Kevin Handy
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
	DECLARE		UTL_REPORTX_CDD	UTL_REPORTX

	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.HB"
	MAP (AP_OPEN)		AP_OPEN_CDD		AP_OPEN

	%INCLUDE "SOURCE:[AP.OPEN]AP_CLOSE.HB"
	MAP (AP_CLOSE)		AP_CLOSE_CDD		AP_CLOSE

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP (AP_VENDOR)		AP_VENDOR_CDD		AP_VENDOR

	!
	! Declare variables and constants
	!
	DECLARE	STRING	LYT_LINE

	!
	! Dimension arrays and/or tables
	!
	DIM ANA_PERIOD(100%, 6%), PERIOD$(100%)

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

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Item\*
	!	.B
	!	.LM +5
	!	The ^*From Item\* field determines the item with
	!	which the report will begin. If the setting is blank,
	!	the report will begin with the first item in the file.
	!	.b
	!	The value entered must be in agreement with the value in
	!	field (03) Sort by.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Vendor Analysis Report
	!	.x Vendor Analysis Report>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field determines the item
	!	with which the report will end. If this setting is blank,
	!	the report will end with the last item in the file.
	!	.b
	!	The value entered must be in agreement with the value in field
	!	(03) Sort by.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Vendor Analysis Report
	!	.x Vendor Analysis Report>To Item
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
	!	.lm 15
	!	.b
	!	.LS "*"
	!	.LE
	!	^*NU\*#=#Vendor Number order
	!	.LE
	!	^*S\*##=#Alphabetical order
	!	.LE
	!	A blank setting will cause the report to print in numerical
	!	order.
	!	.ELS
	!	.lm -5
	!	There are no other valid values for this field.
	!	.lm -5
	!
	! Index:
	!	.x Sort Order>Vendor Analysis Report
	!	.x Vendor Analysis Report>Sort Order
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

300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.OPN"
	USE
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

310	!
	! Open Vendor file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.OPN"
	USE
		FILENAME$ = "AP_VENDOR"
		CONTINUE HelpError
	END WHEN

320	!
	! Open ap close file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_CLOSE.OPN"
	USE
		FILENAME$ = "AP_CLOSE"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "Accounts Payable Vendor Analysis"
	TITLE$(2%) = ""

	!
	! Heading
	!
	TITLE$(3%) = "VendorNum  VendorName                            " + &
		"Period     Net Billed  % of Tot  # of Trn    Average Amt       Payments  % of Tot"

	TITLE$(4%) = ""

	!
	! Line layout information
	!
	LYT_LINE = "$VendorNum:010,$VendorName:034,$Period:056," + &
		"VNetBilled:070,VPercentOfTotal:079,VNumOfTrn:091," + &
		"VAverageAmt:105,VPayments:120,VPercentOfTotal:129"

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
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

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

	TEMP_TEXT$ = AP_VENDOR::VENNUM + " " + &
		LEFT(AP_VENDOR::VENNAM, 23%) + " "

	ANA_PERIOD(X%,Y%) = 0.0 FOR Y% = 1% TO 6% FOR X% = 1% TO 100%

	ANA_VENDOR(LOOP%) = 0.0 FOR LOOP% = 1% TO 6%

	PERIOD$(LOOP%) = "" FOR LOOP% = 1% TO 100%

	PERIOD_LOOP%, TRAN_COUNTER% = 0%

	!
	! Assign something impossible
	!
	TRANKEY$ = "ZZZZZZZZZZZZZZZZ"

17030	WHEN ERROR IN
		FIND #AP_CLOSE.CH%, KEY #0% EQ AP_VENDOR::VENNUM, REGARDLESS
	USE
		CONTINUE 17100 IF ERR = 155%
		FILENAME$ = "AP_CLOSE"
		CONTINUE HelpError
	END WHEN

17040	WHEN ERROR IN
		GET #AP_CLOSE.CH%, REGARDLESS
	USE
		CONTINUE 17100 IF ERR = 11%
		FILENAME$ = "AP_CLOSE"
		CONTINUE HelpError
	END WHEN

	GOTO 17100 IF AP_VENDOR::VENNUM <> AP_CLOSE::VENNUM

	IF TRANKEY$ <> AP_CLOSE::TRANKEY
	THEN
		TRAN_COUNTER% = 1%
	END IF

	TRANKEY$ = AP_CLOSE::TRANKEY

	!
	! Search list for existing period
	!
	GOTO 17050 IF PERIOD$(LOOP%) = AP_CLOSE::UPDATED &
		FOR LOOP% = 1% TO PERIOD_LOOP%

	!
	! Item not found, create it
	!
	PERIOD_LOOP%, LOOP% = PERIOD_LOOP% + 1%

	WHILE (LOOP% > 1%) AND ( PERIOD$(LOOP% - 1%) > AP_CLOSE::UPDATED)
		PERIOD$(LOOP%) = PERIOD$(LOOP% - 1%)
		ANA_PERIOD(LOOP%, 1%) = ANA_PERIOD(LOOP% - 1%, 1%)
		ANA_PERIOD(LOOP%, 3%) = ANA_PERIOD(LOOP% - 1%, 3%)
		ANA_PERIOD(LOOP%, 5%) = ANA_PERIOD(LOOP% - 1%, 5%)

		LOOP% = LOOP% - 1%
	NEXT

	PERIOD$(LOOP%) = AP_CLOSE::UPDATED
	ANA_PERIOD(LOOP%, 1%) = 0.0
	ANA_PERIOD(LOOP%, 3%) = 0.0
	ANA_PERIOD(LOOP%, 5%) = 0.0

17050	ANA_PERIOD(LOOP%, 1%) = ANA_PERIOD(LOOP%, 1%) + &
		AP_CLOSE::INVAMT - AP_CLOSE::DISAMT
	ANA_PERIOD(LOOP%, 3%) = ANA_PERIOD(LOOP%, 3%) + TRAN_COUNTER%
	ANA_PERIOD(LOOP%, 5%) = ANA_PERIOD(LOOP%, 5%) + AP_CLOSE::CKAMT

	ANA_VENDOR(1%) = ANA_VENDOR(1%) + AP_CLOSE::INVAMT - AP_CLOSE::DISAMT
	ANA_VENDOR(3%) = ANA_VENDOR(3%) + TRAN_COUNTER%
	ANA_VENDOR(5%) = ANA_VENDOR(5%) + AP_CLOSE::CKAMT

	GOTO 17040

17100	WHEN ERROR IN
		FIND #AP_OPEN.CH%, KEY #0% EQ AP_VENDOR::VENNUM, REGARDLESS
	USE
		CONTINUE 17200 IF ERR = 155%
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

17140	WHEN ERROR IN
		GET #AP_OPEN.CH%, REGARDLESS
	USE
		CONTINUE 17200 IF ERR = 11%
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

	GOTO 17200 IF AP_VENDOR::VENNUM <> AP_OPEN::VENNUM

	IF TRANKEY$ <> AP_OPEN::TRANKEY
	THEN
		TRAN_COUNTER% = 1%
	END IF

	TRANKEY$ = AP_OPEN::TRANKEY

	!
	! Search list for existing period
	!
	GOTO 17150 IF PERIOD$(LOOP%) = AP_OPEN::UPDATED &
		FOR LOOP% = 1% TO PERIOD_LOOP%

	!
	! Item not found, create it
	!
	PERIOD_LOOP%, LOOP% = PERIOD_LOOP% + 1%

	WHILE (LOOP% > 1%) AND ( PERIOD$(LOOP% - 1%) > AP_OPEN::UPDATED)
		PERIOD$(LOOP%) = PERIOD$(LOOP% - 1%)
		ANA_PERIOD(LOOP%, 1%) = ANA_PERIOD(LOOP% - 1%, 1%)
		ANA_PERIOD(LOOP%, 3%) = ANA_PERIOD(LOOP% - 1%, 3%)
		ANA_PERIOD(LOOP%, 5%) = ANA_PERIOD(LOOP% - 1%, 5%)

		LOOP% = LOOP% - 1%
	NEXT

	PERIOD$(LOOP%) = AP_OPEN::UPDATED
	ANA_PERIOD(LOOP%, 1%) = 0.0
	ANA_PERIOD(LOOP%, 3%) = 0.0
	ANA_PERIOD(LOOP%, 5%) = 0.0

17150	ANA_PERIOD(LOOP%, 1%) = ANA_PERIOD(LOOP%, 1%) + &
		AP_OPEN::INVAMT - AP_OPEN::DISAMT
	ANA_PERIOD(LOOP%, 3%) = ANA_PERIOD(LOOP%, 3%) + TRAN_COUNTER%
	ANA_PERIOD(LOOP%, 5%) = ANA_PERIOD(LOOP%, 5%) + AP_OPEN::CKAMT

	ANA_VENDOR(1%) = ANA_VENDOR(1%) + AP_OPEN::INVAMT - AP_OPEN::DISAMT
	ANA_VENDOR(3%) = ANA_VENDOR(3%) + TRAN_COUNTER%
	ANA_VENDOR(5%) = ANA_VENDOR(5%) + AP_OPEN::CKAMT

	GOTO 17140

17200	GOSUB VendorTotal

	GOTO ExitProgram IF UTL_REPORTX::STAT

17350	!
	! Try for next record
	!
	GOTO 17020

 ExitTotal:
	!
	! Handle end of report
	!
	ANA_TOTAL(4%) = FUNC_ROUND(ANA_TOTAL(1%) / ANA_TOTAL(3%), 2%) &
		IF ANA_TOTAL(3%) <> 0.0

	TEMP_TEXT$ = "          Grand Total"

	TEXT$ = TEMP_TEXT$ + SPACE$(56% - LEN(TEMP_TEXT$)) + &
		FORMAT$(ANA_TOTAL(1%), " ##,###,###.##") + &
		FORMAT$(100.0, "  ####.## ") + &
		FORMAT$(ANA_TOTAL(3%), "    #######") + &
		FORMAT$(ANA_TOTAL(4%), "  #,###,###.##") + &
		FORMAT$(ANA_TOTAL(5%), "  ##,###,###.##") + &
		FORMAT$(100.0, "  ####.## ")

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

 VendorTotal:
	!
	! Print Vendor total
	!
	RETURN IF PERIOD_LOOP% = 0%

	FOR LOOP% = 1% TO PERIOD_LOOP%
		TEXT$ = TEMP_TEXT$ + SPACE$(49% - LEN(TEMP_TEXT$)) + &
			LEFT(PERIOD$(LOOP%), 4%) + "-" + &
			MID(PERIOD$(LOOP%), 5%, 2%)

		ANA_PERIOD(LOOP%, 2%) = FUNC_ROUND(ANA_PERIOD(LOOP%, 1%) / &
			ANA_VENDOR(1%) * 100.0, 2%) &
			IF ANA_VENDOR(1%) <> 0.0

		ANA_PERIOD(LOOP%, 4%) = 0.0
		ANA_PERIOD(LOOP%, 4%) = FUNC_ROUND(ANA_PERIOD(LOOP%, 1%) / &
			ANA_PERIOD(LOOP%, 3%), 2%) &
			IF ANA_PERIOD(LOOP%, 3%) <> 0.0

		ANA_PERIOD(LOOP%, 6%) = &
			FUNC_ROUND(ANA_PERIOD(LOOP%, 5%) / &
			ANA_VENDOR(5%) * 100.0, 2%) &
			IF ANA_VENDOR(5%) <> 0.0

		IF ANA_PERIOD(LOOP%, 1%) <> 0.0
		THEN
			TEXT$ = TEXT$ + &
				FORMAT$(ANA_PERIOD(LOOP%, 1%), " ##,###,###.##")
		ELSE
			TEXT$ = TEXT$ + SPACE$(LEN(" ##,###,###.##"))
		END IF

		IF ANA_PERIOD(LOOP%, 2%) <> 0.0
		THEN
			TEXT$ = TEXT$ + &
				FORMAT$(ANA_PERIOD(LOOP%, 2%), "  ####.## ")
		ELSE
			TEXT$ = TEXT$ + SPACE$(LEN("  ####.## "))
		END IF

		IF ANA_PERIOD(LOOP%, 3%) <> 0.0
		THEN
			TEXT$ = TEXT$ + &
				FORMAT$(ANA_PERIOD(LOOP%, 3%), "    #######")
		ELSE
			TEXT$ = TEXT$ + SPACE$(LEN("    #######"))
		END IF

		IF ANA_PERIOD(LOOP%, 4%) <> 0.0
		THEN
			TEXT$ = TEXT$ + &
				FORMAT$(ANA_PERIOD(LOOP%, 4%), "  #,###,###.##")
		ELSE
			TEXT$ = TEXT$ + SPACE$(LEN("  #,###,###.##"))
		END IF

		IF ANA_PERIOD(LOOP%, 5%) <> 0.0
		THEN
			TEXT$ = TEXT$ + &
				FORMAT$(ANA_PERIOD(LOOP%, 5%), "  ##,###,###.##")
		ELSE
			TEXT$ = TEXT$ + SPACE$(LEN("  #,###,###.##"))
		END IF

		IF ANA_PERIOD(LOOP%, 6%) <> 0.0
		THEN
			TEXT$ = TEXT$ + &
				FORMAT$(ANA_PERIOD(LOOP%, 6%), "  ####.## ")
		ELSE
			TEXT$ = TEXT$ + SPACE$(LEN("  ####.## "))
		END IF

		CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)

		RETURN IF UTL_REPORTX::STAT

		TEMP_TEXT$ = ""

	NEXT LOOP%

	ANA_VENDOR(4%) = FUNC_ROUND(ANA_VENDOR(1%) / ANA_VENDOR(3%), 2%) &
		IF ANA_VENDOR(3%) <> 0.0

	TEXT$ = SPACE$(49%) + "Total  " + &
		FORMAT$(ANA_VENDOR(1%), " ##,###,###.##") + &
		FORMAT$(100.0, "  ####.## ") + &
		FORMAT$(ANA_VENDOR(3%), "    #######") + &
		FORMAT$(ANA_VENDOR(4%), "  #,###,###.##") + &
		FORMAT$(ANA_VENDOR(5%), "  ##,###,###.##") + &
		FORMAT$(100.0, "  ####.## ")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	ANA_TOTAL(LOOP%) = ANA_TOTAL(LOOP%) + ANA_VENDOR(LOOP%) &
		FOR LOOP% = 1% TO 6%

	RETURN IF UTL_REPORTX::STAT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

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
