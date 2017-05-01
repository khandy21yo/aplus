1	%TITLE "Accounts Payable Register Distribution Report"
	%SBTTL "AP_RPRT_REG_DIST"
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
	! ID:APREGD
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Accounts Payable with Expense Distribution\* report
	!	prints a special Accounts Payable Register. This includes
	!	information as to which General Ledger accounts were charged
	!	and the dollar amount of the charges distributed when a vendor's
	!	invoice was entered in the Purchases Journal.  The following fields
	!	are included:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	Transaction Number
	!	.le
	!	Invoice Number
	!	.le
	!	Invoice Date
	!	.le
	!	Check Description
	!	.le
	!	Due Amount
	!	.le
	!	Paid Amount
	!	.le
	!	Balance
	!	.le
	!	Discount Date
	!	.le
	!	Due Date
	!	.le
	!	Discount Account
	!	.le
	!	Discount Amount
	!	.els
	!
	! Index:
	!	.x Accounts Payable>Reports>AP with Expense Distribution
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_RPRT_REG_DIST/LINE
	!	$ LINK/EXECUTABLE=AP_EXE: AP_RPRT_REG_DIST, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_RPRT_REG_DIST.OBJ;*
	!
	! Author:
	!
	!	07/31/87 - B. Craig Larsen
	!
	! Modification history:
	!
	!	06/21/90 - Aaron Redd
	!		Added line layout information so that the report could
	!		also be sent to either a spreadsheet or a DIF file.
	!
	!	06/05/91 - Kevin Handy
	!		Unwound error trapping.
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
	!	08/20/97 - Kevin Handy
	!		Don't need to assign channel for report
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/25/2000 - Kevin Handy
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

	!******************************************************************
	! Set up data storage areas (MAPs, DIMENSIONs, DECLAREs, etc.)
	!******************************************************************

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.HB"
	MAP	(AP_OPEN)	AP_OPEN_CDD		AP_OPEN

	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN_DIST.HB"
	MAP	(AP_OPEN_DIST)	AP_OPEN_DIST_CDD	AP_OPEN_DIST

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP	(AP_VENDOR)	AP_VENDOR_CDD		AP_VENDOR

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP	(GL_CHART)	GL_CHART_CDD		GL_CHART

	!
	! Declare variables and constants
	!
	DECLARE	STRING	LYT_LINE

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
	!	The ^*From Item\* field determines the vendor
	!	number with which the report will begin printing.
	!	.b
	!	If the setting is blank, the report will begin with the first
	!	vendor number in the file.
	!	.lm -5
	!
	! Index:
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
	!	The ^*To Item\* field determines the vendor
	!	number (or vendor name, if the report is to be printed in
	!	alphabetical order) with which the report will end. If this
	!	setting is blank, the report will end with the last vendor in
	!	the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Register Distribution
	!	.x Register Distribution>To Item
	!	.x To>Item
	!	.x Item>To
	!
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Sort Order (NU,S,NA)\*
	!	.b
	!	.lm +5
	!	The ^*Sort Order (NU,S,NA)\* field determines the order in
	!	which the report is to be printed.
	!	.b
	!	Valid entries are:
	!	.lm 15
	!	.LIST "*"
	!	.LIST ELEMENT
	!	^*NU\*#=#Vendor Number order
	!	.LIST ELEMENT
	!	^*S\*##=#Alphabetical order
	!	.LIST ELEMENT
	!	^*NA\*#=#Name field order
	!	.LIST ELEMENT
	!	A blank setting will cause the report to print in vendor number
	!	order.
	!	.els
	!	.lm -5
	!	There are no other valid values for this field.
	!
	! Index:
	!	.x Sort Order>Register Distribution
	!	.x Register Distribution>Sort Order
	!
	!--

	BATCH$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Batch\*
	!	.b
	!	.lm +5
	!	The ^*Batch\* field enters the number of the batch
	!	which will be printed on the report.
	!	.b
	!	Only one batch at a time may be printed.
	!	.lm -5
	!
	! Index:
	!	.x Batch
	!
	!--

	TEXT_BATCH$ = ""
	TEXT_BATCH$ = " Batch " + BATCH$ IF BATCH$ <> ""

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
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(AP_VENDOR::ALPSRT))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(AP_VENDOR::ALPSRT))
		K_NUM% = 2%

	END SELECT

	%PAGE

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.OPN"
	USE
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

	!
	! Open the Accounts Payable Open Distribution file
	!
310	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN_DIST.OPN"
	USE
		FILENAME$ = "AP_OPEN_DIST"
		CONTINUE HelpError
	END WHEN

	!
	! Open the Accounts Payable Vendor file
	!
320	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.OPN"
	USE
		FILENAME$ = "AP_VENDOR"
		CONTINUE HelpError
	END WHEN

	!
	! Open the General Ledger Chart of Accounts file
	!
330	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.OPN"
	USE
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "Accounts Payable Register With Distribution" + TEXT_BATCH$
	TITLE$(2%) = ""

	!
	! Heading
	!
	TITLE$(3%) = "Trans# InvoiceNumber   InvDate   CheckDescr   " + &
		"        DueAmt    PaidAmt    Balance  DiscDate DueDate" + &
		"   DistAcct              DistAmt"

	TITLE$(4%) = ""

	!
	! Line layout information
	!
	LYT_LINE = "$TranKey:006,$InvoiceNum:022,DInvoiceDate:031," + &
		"$CheckDescr:049,VAmtDue:060,VAmtPaid:071,VBalanceDue:082," + &
		"DDiscountDate:092,DDueDate:101,$DistAcct:121,VDistAmt:132"

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
17020	!******************************************************************
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

17030	WHEN ERROR IN
		FIND #AP_OPEN.CH%, KEY #0% EQ AP_VENDOR::VENNUM, REGARDLESS
	USE
		CONTINUE 17350 IF ERR = 155%
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

	TEXT$ = AP_VENDOR::VENNUM + " " + &
		AP_VENDOR::VENNAM

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Assign something impossible
	!
	TRANKEY$ = "ZZZZZZZZZZZZZ"

	NET_DUE, PAID, &
		VENDOR_NET_DUE, VENDOR_PAID = 0.0

	PRINT_LINE%, PRINT_VENDOR_TOTAL% = 0%

	!
	! Sub-loop begins here
	!
17040	WHEN ERROR IN
		GET #AP_OPEN.CH%, REGARDLESS
	USE
		CONTINUE 17100 IF ERR = 11%
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

	GOTO 17040 &
		IF BATCH$ <> AP_OPEN::BATCH AND BATCH$ <> ""

	GOTO 17100 IF AP_VENDOR::VENNUM <> AP_OPEN::VENNUM

	IF AP_OPEN::TRANKEY <> TRANKEY$
	THEN
		IF PRINT_LINE%
		THEN
			GOSUB PrintLine
			GOTO ExitProgram IF UTL_REPORTX::STAT
			PRINT_LINE% = 0%
			TEXT$ = ""
		END IF

		TEXT$ = AP_OPEN::TRANKEY + " " + &
			AP_OPEN::INVNUM + " " + &
			PRNT_DATE(AP_OPEN::INVDAT, 0%) + "  " + &
			LEFT(AP_OPEN::CKDESC, 16%) + " "

		TEXT1$ = PRNT_DATE(AP_OPEN::DISCDAT, 6%) + " " + &
			PRNT_DATE(AP_OPEN::DUEDAT, 6%) + " "
	END IF

	TRANKEY$ = AP_OPEN::TRANKEY

	NET_DUE = NET_DUE + AP_OPEN::INVAMT - AP_OPEN::DISAMT
	PAID = PAID + AP_OPEN::CKAMT

	VENDOR_NET_DUE = VENDOR_NET_DUE + AP_OPEN::INVAMT - AP_OPEN::DISAMT
	VENDOR_PAID = VENDOR_PAID + AP_OPEN::CKAMT

	TOTAL_NET_DUE = TOTAL_NET_DUE + AP_OPEN::INVAMT - AP_OPEN::DISAMT
	TOTAL_PAID = TOTAL_PAID + AP_OPEN::CKAMT

	PRINT_LINE% = -1%

	GOTO 17040

17100	IF PRINT_LINE%
	THEN
		GOSUB PrintLine
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	IF PRINT_VENDOR_TOTAL%
	THEN
		GOSUB VendorTotal
	END IF

	GOTO ExitProgram IF UTL_REPORTX::STAT

17350	!
	! Try for next record
	!
	GOTO 17020

	%PAGE

	!******************************************************************
	! Handle totals and other items before EXITing
	!******************************************************************

 ExitTotal:
	!
	! Print out totals
	!
	TEXT$ = "          Grand Total" + SPACE$(28%) + &
		FORMAT$(TOTAL_NET_DUE, " #######.##") + &
		FORMAT$(TOTAL_PAID, " #######.##") + &
		FORMAT$(TOTAL_NET_DUE - TOTAL_PAID, " #######.##")

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
	!
	! Print Register distribution
	!
	PRINT_VENDOR_TOTAL% = -1%

	TEXT$ = LEFT(TEXT$ + SPACE$(50%), 50%) + &
		FORMAT$(NET_DUE, "#######.## ") + &
		FORMAT$(PAID, "#######.## ") + &
		FORMAT$(NET_DUE - PAID, "#######.##  ") + &
		TEXT1$

18000	WHEN ERROR IN
		FIND #AP_OPEN_DIST.CH%, KEY #0% EQ TRANKEY$, REGARDLESS
	USE
		CONTINUE 18090 IF ERR = 155%
		FILENAME$ = "AP_OPEN_DIST"
		CONTINUE HelpError
	END WHEN

18010	WHEN ERROR IN
		GET #AP_OPEN_DIST.CH%, REGARDLESS
	USE
		CONTINUE 18090 IF ERR = 11%
		FILENAME$ = "AP_OPEN_DIST"
		CONTINUE HelpError
	END WHEN

	GOTO 18010 &
		IF BATCH$ <> AP_OPEN_DIST::BTHNUM AND BATCH$ <> ""

	GOTO 18090 IF TRANKEY$ <> AP_OPEN_DIST::TRANKEY

18030	TEXT$ = LEFT(TEXT$ + SPACE$(103% - LEN(TEXT$)), 103%) + &
		AP_OPEN_DIST::ACCT + " " + &
		FORMAT$(AP_OPEN_DIST::AMOUNT - &
		AP_OPEN_DIST::DISCAMT, "#######.##")

	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = ""

	RETURN IF UTL_REPORTX::STAT

	GOTO 18010

18090	NET_DUE, PAID = 0.0

	IF TEXT$ <> ""
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	RETURN

	%Page

 VendorTotal:
	!
	! Print Vendor total
	!
	TEMP$ = "     Vendor Total"
	TEXT$ = LEFT(TEMP$ + SPACE$(50% - LEN(TEMP$)), 50%) + &
		FORMAT$(VENDOR_NET_DUE, "#######.## ") + &
		FORMAT$(VENDOR_PAID, "#######.## ") + &
		FORMAT$(VENDOR_NET_DUE - VENDOR_PAID, "#######.##  ")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	RETURN IF UTL_REPORTX::STAT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	RETURN

	%Page

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
