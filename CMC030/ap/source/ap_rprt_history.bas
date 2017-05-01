1	%TITLE "Accounts Payable History File"
	%SBTTL "AP_RPRT_HISTORY"
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
	! ID:APHIST
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*History Report\*
	!	prints a data dump of information contained
	!	in the Accounts Payable Closed file. The format
	!	is similar to the Accounts Payable Subsidiary Ledger or the
	!	Accounts Payable Register. This report consists of the following fields:
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
	!	Purchase Order Number
	!	.le
	!	Description
	!	.le
	!	Invoice Amount
	!	.le
	!	Discount Amount
	!	.le
	!	Net Amount
	!	.le
	!	Due Date
	!	.le
	!	Discount Date
	!	.le
	!	Check Number
	!	.els
	!
	! Index:
	!	.x Accounts Payable>Reports>History Report
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_RPRT_HISTORY/LINE
	!	$ LINK/EXECUTABLE=AP_EXE:*.EXE AP_RPRT_HISTORY, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_RPRT_HISTORY.OBJ;*
	!
	! Author:
	!
	!	07/31/87 - B. Craig Larsen
	!
	! Modification history:
	!
	!	07/05/90 - Kevin Handy
	!		Modified to use formatted PO number.
	!
	!	06/05/91 - Kevin Handy
	!		Unwound error trapping.
	!
	!	03/05/92 - Dan Perkins
	!		Modified to use CONV_STRING instead of PRNT_PO.
	!
	!	06/14/93 - Kevin Handy
	!		Added REGARDLESS to AP_1099_TABLE.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/27/96 - Kevin Handy
	!		Reformat source code.
	!
	!	01/20/97 - Kevin Handy
	!		Added from/to date fields.
	!		Lose lot's of commented out code.
	!		Format closer to 80 columns.
	!		Lose several unecessary variables.
	!
	!	02/18/97 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/21/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/20/97 - Kevin Handy
	!		Don't need to assign channel for report.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/24/2000 - Kevin Handy
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
	! Set up data storage areas  (MAPs, DIMENSIONs, DECLAREs, etc.)
	!******************************************************************

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[AP.OPEN]AP_1099_TABLE.HB"
	MAP	(AP_1099_TABLE)	AP_1099_TABLE_CDD	AP_1099_TABLE

	%INCLUDE "SOURCE:[AP.OPEN]AP_CLOSE.HB"
	MAP	(AP_CLOSE)	AP_CLOSE_CDD		AP_CLOSE

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP	(AP_VENDOR)	AP_VENDOR_CDD		AP_VENDOR

	%PAGE

	!******************************************************************
	! Take care of anything else before starting report
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
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 204%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 210%)

	!++
	! Abstract:FLD01
	!	^*(01) From Item\*
	!	.b
	!	.lm +5
	!	This field determines the order in which
	!	the report is to begin printing.
	!	.b
	!	If the field is left blank,
	!	the report begins with the first
	!	vendor number in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Histroy Report
	!	.x Histroy Report>From Item
	!	.X From>Item
	!	.x Item>From
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.b
	!	.lm +5
	!	This field will determines the ending item which the
	!	report will print.
	!	.b
	!	If this setting is blank,
	!	the report ends with the last item in
	!	the file.
	!
	! Index:
	!	.x To Item>Histroy Report
	!	.x History Report>To Item
	!	.x To>Item
	!	.x Item>To
	!
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Sort Order (NU,S)\*
	!	.b
	!	.lm +5
	!	This field determines if the
	!	report is to be printed in vendor number
	!	or in alphabetical order.
	!	.lm 15
	!	.LIST "*"
	!	.LE
	!	^*NU\*#=#Vendor Number order
	!	.LE
	!	^*S\*##=#Alphabetical order
	!	.els
	!	.lm -5
	!	A blank setting will print in
	!	vendor number order.
	!	.b
	!	There are no other valid values for this field.
	!	.lm -5
	!
	! Index:
	!	.x Sort Order>Histroy Report
	!	.x Histroy Report>Sort Order
	!
	!--


	FROMDATE$ = DATE_STOREDATE(UTL_REPORTX::OPTDEF(5%))

	!++
	! Abstract:FLD06
	!	^*(06) From Date\*
	!	.b
	!	This field determines what the minimum
	!	invoice date should be included in the
	!	report.
	!
	! Index:
	!	.x From Date>Histroy Report
	!	.x Histroy Report>From Date
	!
	!--

	TODATE$ = DATE_STOREDATE(UTL_REPORTX::OPTDEF(6%))

	!++
	! Abstract:FLD07
	!	^*(07) From Date\*
	!	.b
	!	This field determines the maximum invoice
	!	date to be printed.
	!
	! Index:
	!	.x From Date>Histroy Report
	!	.x Histroy Report>From Date
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

	%PAGE


300	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_CLOSE.OPN"
	USE
		FILENAME$ = "AP_CLOSE"
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

	!
	! Open the Accounts Payable 1099 Table file
	!
320	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_1099_TABLE.OPN"
	USE
		FILENAME$ = "AP_1099_TABLE"
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "Accounts Payable History"
	TITLE$(2%) = ""

	!
	! Heading
	!
	TITLE$(3%) = "Trans#  InvNumber        InvDate   PONumber" + &
		"    Description             InvAmount    DiscAmt   " + &
		"       Net DueDate  DiscDate CkNum    CkDate       " + &
		"CheckAmt       BalDue  CD CodeDescription        1099Amount"
	TITLE$(4%) = ""

	!
	! Layouts for lines printed
	!
	LYT_LINE$ = "$TransNum:006,$InvNum:023,DInvDat:033," + &
		"$PONumber:045,$CheckDescr:067,VInvAmt:080," + &
		"VDiscountAmt:091,VNetAmt:104,DDueDate:113," + &
		"DDiscountDate:122,$CheckNum:129,DCheckDate:140," + &
		"VCheckAmt:153,VBalanceDue:166,$1099Code:170," + &
		"$CodeDescription:191,V1099Amount:204"

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

	TITLE_FLAG% = 0%
	INVOICE_FLAG% = 0%

17030	WHEN ERROR IN
		FIND #AP_CLOSE.CH%, KEY #0% EQ AP_VENDOR::VENNUM, REGARDLESS
	USE
		CONTINUE 17350 IF ERR = 155%
		FILENAME$ = "AP_CLOSE"
		CONTINUE HelpError
	END WHEN

	!
	! Assign something impossible
	!
	TRANKEY$ = "ZZZZZZZZZZZZZZZ"
	CHK_TEST%, FIRST_PASS% = 0%

	INV_AMT, DIS_AMT, NET_AMT, CHK_AMT, BAL_DUE, AMT_1099 = 0.0

	TEXT_1099$ = ""

17040	!
	! Grab next AP record
	!
	WHEN ERROR IN
		GET #AP_CLOSE.CH%, REGARDLESS
	USE
		CONTINUE 17100 IF ERR = 11%
		FILENAME$ = "AP_CLOSE"
		CONTINUE HelpError
	END WHEN

	!
	! Validate record
	!
	GOTO 17100 IF AP_VENDOR::VENNUM <> AP_CLOSE::VENNUM

	IF (AP_CLOSE::INVDAT < FROMDATE$) OR &
		((TODATE$ <> "") AND (AP_CLOSE::INVDAT > TODATE$))
	THEN
		GOTO 17040
	END IF

	INVOICE_FLAG% = -1%

	IF FIRST_PASS%
	THEN
		IF AP_CLOSE::TRANKEY = TRANKEY$
		THEN
			IF CHK_TEST% = 0% AND AP_CLOSE::INVAMT = 0.0 AND &
				AP_CLOSE::DISAMT = 0.0 AND &
				AP_CLOSE::CKAMT <> 0.0 &
				OR CHK_TEST% = 0% AND &
				AP_CLOSE::INVAMT = 0.0 AND &
				AP_CLOSE::DISAMT = 0.0 AND &
				AP_CLOSE::CKNUM <> ""
			THEN
				GOTO 17090
			END IF
		ELSE
			TEXT$ = TEXT$ + SPACE$(154% - LEN(TEXT$))
			TEXT$ = TEXT$ + FORMAT$(BAL_DUE, "#########.##")
			BAL_DUE = 0.0
		END IF

		IF TITLE_FLAG% = 0%
		THEN
			XTEXT$ = AP_VENDOR::VENNUM + " " + &
				AP_VENDOR::VENNAM

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), XTEXT$, 0%)

			GOTO ExitProgram IF UTL_REPORTX::STAT

			TITLE_FLAG% = -1%
		END IF

		TEXT$ = TEXT$ + SPACE$(166% - LEN(TEXT$)) + TEXT_1099$

		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)

		TEXT_1099$ = ""

		GOTO ExitProgram IF UTL_REPORTX::STAT

		CHK_TEST% = 0%
	END IF

	TEXT$ = ""

	IF AP_CLOSE::TRANKEY = TRANKEY$ AND FIRST_PASS%
	THEN
		TEXT$ = SPACE$(68%)
	ELSE
		TEXT$ = AP_CLOSE::TRANKEY + "  " + &
			AP_CLOSE::INVNUM + "  " + &
			PRNT_DATE(AP_CLOSE::INVDAT, 6%) + "   " + &
			CONV_STRING(AP_CLOSE::PONUM, CMC$_LEFT) + "  " + &
			AP_CLOSE::CKDESC
	END IF

	IF AP_CLOSE::INVAMT <> 0.0
	THEN
		TEXT$ = TEXT$ + FORMAT$(AP_CLOSE::INVAMT, "#########.## ")
	ELSE
		TEXT$ = TEXT$ + SPACE$(LEN("#########.## "))
	END IF

	IF AP_CLOSE::DISAMT <> 0.0
	THEN
		TEXT$ = TEXT$ + FORMAT$(AP_CLOSE::DISAMT, "#######.## ")
	ELSE
		TEXT$ = TEXT$ + SPACE$(LEN("#######.## "))
	END IF

	NET = AP_CLOSE::INVAMT - AP_CLOSE::DISAMT
	IF NET <> 0.0
	THEN
		TEXT$ = TEXT$ + FORMAT$(NET, "#########.## ")
	ELSE
		TEXT$ = TEXT$ + SPACE$(LEN("#########.## "))
	END IF

	IF AP_CLOSE::TRANKEY = TRANKEY$
	THEN
		TEXT$ = TEXT$ + SPACE$(18%)
	ELSE
		TEXT$ = TEXT$ + &
			PRNT_DATE(AP_CLOSE::DUEDAT, 6%) + " " + &
			PRNT_DATE(AP_CLOSE::DISCDAT, 6%) + " "
	END IF

17090	IF AP_CLOSE::CKAMT <> 0.0 OR AP_CLOSE::CKNUM <> ""
	THEN
		TEXT$ = TEXT$ + &
			AP_CLOSE::CKNUM + "   " + &
			PRNT_DATE(AP_CLOSE::CKDAT, 6%) + " " + &
			FORMAT$(AP_CLOSE::CKAMT, "#########.## ")
		CHK_TEST% = -1%

		GOSUB Get1099Data
	END IF

	FIRST_PASS% = -1%
	TRANKEY$ = AP_CLOSE::TRANKEY
	INV_AMT = INV_AMT + AP_CLOSE::INVAMT
	DIS_AMT = DIS_AMT + AP_CLOSE::DISAMT
	NET_AMT = NET_AMT + AP_CLOSE::INVAMT - AP_CLOSE::DISAMT
	CHK_AMT = CHK_AMT + AP_CLOSE::CKAMT

	BAL_DUE = BAL_DUE + (AP_CLOSE::INVAMT - AP_CLOSE::DISAMT) - &
		AP_CLOSE::CKAMT

	GOTO 17040

17100	GOSUB VendorTotal

	GOTO ExitProgram IF UTL_REPORTX::STAT

17350	!
	! Try for next record
	!
	GOTO 17020

 ExitTotal:
	!
	! Handle end of report
	!
	TEMP$ = "          Grand Total"

	TEXT$ = TEMP$ + SPACE$(68% - LEN(TEMP$)) + &
		FORMAT$(TOT_INV_AMT, "#########.## ") + &
		FORMAT$(TOT_DIS_AMT, "#######.## ") + &
		FORMAT$(TOT_NET_AMT, "#########.## ") + &
		SPACE$(36%) + &
		FORMAT$(TOT_CHK_AMT, "#########.## ") + &
		FORMAT$(TOT_NET_AMT - TOT_CHK_AMT, "#########.##") + &
		SPACE$(26%) + &
		FORMAT$(TOT_1099_AMT, "#########.##")

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

	!*******************************************************************
	! Print Vendor total
	!*******************************************************************
 VendorTotal:
	IF INVOICE_FLAG% <> 0%
	THEN
		IF TITLE_FLAG% = 0%
		THEN
			XTEXT$ = AP_VENDOR::VENNUM + " " + &
				AP_VENDOR::VENNAM

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), XTEXT$, 0%)

			GOTO ExitProgram IF UTL_REPORTX::STAT
		END IF

		TEXT$ = TEXT$ + SPACE$(154% - LEN(TEXT$))

		TEXT$ = TEXT$ + FORMAT$(BAL_DUE, "#########.##") + &
			TEXT_1099$

		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		TOT_INV_AMT = TOT_INV_AMT + INV_AMT
		TOT_DIS_AMT = TOT_DIS_AMT + DIS_AMT
		TOT_NET_AMT = TOT_NET_AMT + NET_AMT
		TOT_CHK_AMT = TOT_CHK_AMT + CHK_AMT
		TOT_1099_AMT = TOT_1099_AMT + AMT_1099

		TEMP$ = "     Vendor Total"

		TEXT$ = TEMP$ + SPACE$(68% - LEN(TEMP$)) + &
			FORMAT$(INV_AMT, "#########.## ") + &
			FORMAT$(DIS_AMT, "#######.## ") + &
			FORMAT$(NET_AMT, "#########.## ") + &
			SPACE$(36%) + &
			FORMAT$(CHK_AMT, "#########.## ") + &
			FORMAT$(NET_AMT-CHK_AMT, "#########.##") + &
			SPACE$(26%) + &
			FORMAT$(AMT_1099, "#########.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

	END IF

	RETURN

	%Page

 Get1099Data:
	!******************************************************************
	! Get 1099 data
	!*****************************************************************
18000	AP_1099_TABLE::DESCR = STRING$(LEN(AP_1099_TABLE::DESCR), 63%)

	WHEN ERROR IN
		FIND #AP_1099_TABLE.CH%, &
			KEY #0% EQ AP_CLOSE::CODE_1099, &
			REGARDLESS
		GET #AP_1099_TABLE.CH%, REGARDLESS
	USE
		CONTINUE 18010 IF ERR = 155%
		FILENAME$ = "AP_1099_TABLE"
		CONTINUE HelpError
	END WHEN

18010	TEXT_1099$ = "  " + AP_CLOSE::CODE_1099 + " " + &
		LEFT(AP_1099_TABLE::DESCR + SPACE$(20%), 20%) + " " + &
		FORMAT$(AP_CLOSE::AMT_1099, "#########.##")
	AMT_1099 = AMT_1099 + AP_CLOSE::AMT_1099

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
