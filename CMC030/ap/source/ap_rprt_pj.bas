1	%TITLE "Print Purchases Journal"
	%SBTTL "AP_RPRT_PJ"
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
	! ID:APPJRP
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Print Purchases Journal\* option prints
	!	the information entered in a Purchases Journal batch. This print out contains
	!	the following fields:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	Transaction Number
	!	.le
	!	Purchase Order Number
	!	.le
	!	Vendor Number
	!	.le
	!	Purchase Order Line
	!	.le
	!	Vendor Name
	!	.le
	!	Account
	!	.le
	!	Account Description
	!	.le
	!	Invoice Number
	!	.le
	!	Invoice Date
	!	.le
	!	Sub Account
	!	.le
	!	Due Date
	!	.le
	!	Operation
	!	.le
	!	Discount Date
	!	.le
	!	Units
	!	.le
	!	Check Number
	!	.le
	!	Check Date
	!	.le
	!	Gross Amount
	!	.le
	!	Net Amount
	!	.els
	!	.lm -5
	!	.NOTE RETENTION
	!	The final printing of the Purchases Journal report
	!	should be permanently filed along with the related
	!	Purchases Journal posting transmittals.
	!	.END NOTE
	!	.NOTE
	!	The Purchases Journal report ^&must\& be printed prior
	!	to posting a Purchases Journal batch since the batch
	!	is deleted upon the completion of the posting routine.
	!	.END NOTE
	!	The hash totals printed at the bottom can
	!	be used to validate the entries. The Vendor number hash total
	!	is the total of all of the vendor numbers added together
	!	(after stripping out non-numeric characters). The invoice
	!	hash total is the total of all of the invoice numbers.
	!	To use these, first manually calculate the hash totals from
	!	your entry forms, and then after entering, compare your calculations
	!	with the printed versions.
	!	Any differences could be caused by:
	!	.b
	!	.list 0
	!	.le
	!	Error in calculating hash total.
	!	.le
	!	Missing invoice.
	!	.le
	!	Typing error in invoice or vendor number.
	!	.els
	!
	! Index:
	!	.x Purchases Journal>Print
	!	.x Print>Purchases Journal
	!	.x Hash total>Purchase Journal
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_RPRT_PJ/LINE
	!	$ LINK/EXE=AP_EXE: AP_RPRT_PJ, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_RPRT_PJ.OBJ;*
	!
	! Author:
	!
	!	08/12/87 - B. Craig Larsen
	!
	! Modification history:
	!
	!	09/26/88 - Kevin Handy
	!		Slight modification to title (added "AP System")
	!
	!	10/18/88 - Kevin Handy
	!		Added description to printout.
	!
	!	10/18/88 - Kevin Handy
	!		Added chart description from GL file.
	!
	!	12/07/88 - B. Craig Larsen
	!		Added AP_PJH::VENNUM and AP_PJH::INVNUM
	!		hash totals.
	!
	!	08/10/89 - Aaron Redd
	!		Added Debit/Credit page(s) to end of report.
	!
	!	07/04/90 - Kevin Handy
	!		Modified to use formatted PO number.
	!
	!	07/19/90 - Kevin Handy
	!		Fixed formatting of PO number so it uses PRNT_PO
	!		instead of PRNT_DATE.
	!
	!	08/13/90 - Kevin Handy
	!		Fixed bug where a header without line items would
	!		use totals from the previous item.
	!
	!	06/06/91 - Kevin Handy
	!		Unwound error trapping.
	!
	!	03/05/92 - Dan Perkins
	!		Modified to use CONV_STRING instead of PRNT_PO.
	!
	!	09/01/92 - Kevin Handy
	!		Added code to handle variances.
	!
	!	10/23/92 - Dan Perkins
	!		Added arguement to GL_OUTP_ACCTSUM because of
	!		change in the function.
	!
	!	03/18/93 - Kevin Handy
	!		Added field in GL_OUTP_ACCTSUM for units.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/29/96 - Kevin Handy
	!		Reformat source code.
	!
	!	10/09/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/23/97 - Kevin Handy
	!		Use 'val%' instead of 'val'
	!
	!	04/13/98 - Kevin Handy
	!		Fix VAL% in hash total calculation to limit
	!		it to 9 digits (otherwise an overflow can
	!		occur)
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/12/98 - Kevin Handy
	!		Add percentage to the variance column
	!
	!	08/15/2000 - Kevin Handy
	!		Pass transaction date to AP_FUNC_CALCVARIANCE
	!		(robson)
	!	Use WHEN ERROR IN.
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
	! External functions
	!
	EXTERNAL LONG	FUNCTION GL_EXAM_CHART
	EXTERNAL LONG   FUNCTION GL_OUTP_ACCTSUM
	EXTERNAL LONG	FUNCTION AP_FUNC_CALCVARIANCE

	%PAGE

	!******************************************************************
	! Set up data storage areas (MAPs, DIMENSIONs, DECLAREs, etc.)
	!******************************************************************

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[AP.OPEN]AP_PJH.HB"
	MAP (AP_PJH)		AP_PJH_CDD		AP_PJH

	%INCLUDE "SOURCE:[AP.OPEN]AP_PJL.HB"
	MAP (AP_PJL)		AP_PJL_CDD		AP_PJL

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP (AP_VENDOR)		AP_VENDOR_CDD		AP_VENDOR

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	DECLARE			GL_CHART_CDD		GL_CHART_EXAM

	%PAGE

	!******************************************************************
	! Take care of anything else before starting the report
	!******************************************************************

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

	%PAGE

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
	!	^*(01) From Transaction _#\*
	!	.b
	!	.lm +5
	!	The ^*From Transaction _#\* field determines the transaction
	!	number with which the report will begin printing. If this setting is
	!	blank, the report will begin with the first transaction number in the
	!	file.
	!	.lm -5
	!
	! Index:
	!	.x From Transaction _#>Purchase Journal Report
	!	.x Transaction>From
	!	.x From>Transaction
	!	.x Purchase Journal Report>From Transaction _#
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Transaction _#\*
	!	.b
	!	.lm +5
	!	The ^*To Transaction _#\* field determines the last
	!	transaction number which will be printed. If this
	!	setting is blank, the report will end with the last transaction
	!	number in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Transaction _#>Purchase Journal Report
	!	.x Purchase Journal Report>To Transaction _#
	!	.x To>Transaction _#
	!	.x Transaction _#>To
	!
	!--

	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) Batch Number\*
	!	.b
	!	.lm +5
	!	The ^*Batch Number\* field determines which batch
	!	is to be printed. The ^*Batch Number\* field must contain a
	!	valid value, a batch number which contains data.
	!	.b
	!	Only one batch at a time may be printed.
	!	.lm -5
	!
	! Index:
	!	.x Batch Number>Purchase Journal Report
	!	.x Batch Number>Purchase Journal Report
	!	.x Batch Number>Purchase Journal Report
	!	.x Batch Number>Purchase Journal Report
	!
	!--

	%PAGE

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_PJH.OPN"
	USE
		FILENAME$ = "AP_PJH"
		CONTINUE HelpError
	END WHEN

	!
	! Open the Accounts Payable Purchase Journal Line Item file
	!
310	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_PJL.OPN"
	USE
		FILENAME$ = "AP_PJL"
		CONTINUE HelpError
	END WHEN

	!
	! Open the AP Vendor file
	!
320	WHEN ERROR IN
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
	TITLE$(1%) = "Purchase Journal"
	TITLE$(2%) = "Batch No. " + BATCH_NO$
	TITLE$(3%) = "AP System"
	TITLE$(4%) = ""

	!
	! Headers
	!
	TITLE$(5%) = "Trans# VendorNum  VendorName                     " + &
		"InvoiceNum      InvDate  DueDate  DiscDate CkNum  CheckDate"

	TITLE$(6%) = "PO Number  PO Line Account            " + &
		"Account Description       " + &
		"SubAcct    Operation    Units      GrossAmt       NetAmount"

	TITLE$(7%) = "."

	!
	! Initialize some totals
	!
	CASH, AP_GROSS, AP_DISC, AP_NET = 0.0
	GROSS_OB_TOTAL, NET_OB_TOTAL = 0.0
	GRAND_HASH_VENNUM, GRAND_HASH_INVNUM = 0.0

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #AP_PJH.CH%
		ELSE
			FIND #AP_PJH.CH%, &
				KEY #0% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		FILENAME$ = "AP_PJH"
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
		GET #AP_PJH.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF (ERR = 11%)
		FILENAME$ = "AP_PJH"
		CONTINUE HelpError
	END WHEN

	!
	! Check status
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Check current record
	!
	GOTO ExitTotal &
		IF (AP_PJH::TRANKEY > TO_ITEM$) AND (TO_ITEM$ <> "")

	AP_VENDOR::VENNAM = STRING$(20%, 63%)

	!
	! Get the AP Vendor Description
	!
17200	WHEN ERROR IN
		GET #AP_VENDOR.CH%, &
			KEY #0% EQ AP_PJH::VENNUM, &
			REGARDLESS
	USE
		CONTINUE 17300 IF (ERR = 155%)
		FILENAME$ = "AP_VENDOR"
		CONTINUE HelpError
	END WHEN

	!
	! Build the line to be printed
	!
17300	TEXT$ = AP_PJH::TRANKEY + " " + &
		AP_PJH::VENNUM + " " + &
		LEFT(AP_VENDOR::VENNAM, 30%) + " " + &
		AP_PJH::INVNUM + " " + &
		PRNT_DATE(AP_PJH::INVDAT, 0%) + " " + &
		PRNT_DATE(AP_PJH::DUEDAT, 0%) + " " + &
		PRNT_DATE(AP_PJH::DISCDAT, 0%)

	TEXT$ = TEXT$ + " " + &
		AP_PJH::CKNUM + " " + &
		PRNT_DATE(AP_PJH::CKDAT, 0%) &
		IF (AP_PJH::CKNUM <> "")

	!
	! Print out the line
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get the hash on the AP_PJH::VENNUM and AP_PJH::INVNUM and save.
	!
	GRAND_HASH_VENNUM = GRAND_HASH_VENNUM + &
		VAL%(LEFT(XLATE(AP_PJH::VENNUM, &
		STRING$(48%, 0%) + "0123456789"), 9%))
	GRAND_HASH_INVNUM = GRAND_HASH_INVNUM + &
		VAL%(LEFT(XLATE(AP_PJH::INVNUM, &
		STRING$(48%, 0%) + "0123456789"), 9%))

	!
	! Print out description, if there is one
	!
	IF TRM$(AP_PJH::DESCR) <> ""
	THEN
		TEXT$ = SPACE$(18%) + AP_PJH::DESCR

		!
		! Print out the line
		!
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	!
	! Get ready to start a sub-loop
	!
17400	!
	! Set the subtotals to zero
	!
	NET_SUB_TOTAL, GROSS_SUB_TOTAL = 0.0

	!
	! Find first record for PJ
	!
	WHEN ERROR IN
		FIND #AP_PJL.CH%, &
			KEY #0% EQ AP_PJH::TRANKEY, &
			REGARDLESS
	USE
		CONTINUE 17500 IF (ERR = 155%)
		FILENAME$ = "AP_PJL"
		CONTINUE HelpError
	END WHEN

17410	WHEN ERROR IN
		GET #AP_PJL.CH%, REGARDLESS
	USE
		CONTINUE 17500 IF (ERR = 11%)
		FILENAME$ = "AP_PJL"
		CONTINUE HelpError
	END WHEN

	GOTO 17500 &
		IF (AP_PJL::TRANKEY <> AP_PJH::TRANKEY)

	ST% = AP_FUNC_CALCVARIANCE(AP_PJL::PONUM, AP_PJL::PO_LINE, &
		AP_PJL::ACCT, AP_PJL::UNITS, AP_PJL::AMOUNT, &
		PO_ACCOUNT$, PO_VARIANCE, PO_AMOUNT, AP_PJH::TRANKEY_DATE)

	NET = AP_PJL::AMOUNT - AP_PJL::DISCAMT

	ST% = GL_EXAM_CHART(AP_PJL::ACCT, GL_CHART_EXAM)

	!
	! Build the Line Item line to print
	!
	TEXT$ = CONV_STRING(AP_PJL::PONUM, CMC$_LEFT) + "  "  + &
		AP_PJL::PO_LINE + "   " + &
		AP_PJL::ACCT + " " + &
		LEFT(GL_CHART_EXAM::DESCR, 25%) + " " + &
		AP_PJL::SUBACC + " " + &
		AP_PJL::OPERATION + " " + &
		FORMAT$(AP_PJL::UNITS, "  ####.##") + &
		FORMAT$(PO_AMOUNT, "  #,###,###.##") + &
		FORMAT$(NET, "  ###,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	IF PO_VARIANCE <> 0.0
	THEN
		ST% = GL_EXAM_CHART(PO_ACCOUNT$, GL_CHART_EXAM)

		IF AP_PJL::AMOUNT = PO_VARIANCE
		THEN
			PERCENTAGE = 0.0
		ELSE
			PERCENTAGE = 100.0 * PO_VARIANCE / &
				(AP_PJL::AMOUNT - PO_VARIANCE)
		END IF

		PERCENTAGE = 9999.99 IF PERCENTAGE > 9999.99
		PERCENTAGE = -999.99 IF PERCENTAGE < -999.99

		TEXT$ = CONV_STRING(AP_PJL::PONUM, CMC$_LEFT) + "  "  + &
			AP_PJL::PO_LINE + "   " + &
			PO_ACCOUNT$ + " " + &
			LEFT(GL_CHART_EXAM::DESCR, 25%) + " " + &
			AP_PJL::SUBACC + " " + &
			AP_PJL::OPERATION + " " + &
			"         " + &
			FORMAT$(PO_VARIANCE, "  #,###,###.##") + &
			FORMAT$(PERCENTAGE, " ###,###,###.##%")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		GOTO ExitProgram IF GL_OUTP_ACCTSUM (OPT_ADDREC, PO_ACCOUNT$, &
			0.0, PO_VARIANCE, 0.0, TITLE$(), &
			UTL_REPORTX) <> CMC$_NORMAL
	END IF

	!
	! Add to subtotals
	!
	GROSS_SUB_TOTAL = GROSS_SUB_TOTAL + AP_PJL::AMOUNT
	NET_SUB_TOTAL = NET_SUB_TOTAL + NET

	DIST_GROSS = DIST_GROSS + AP_PJL::AMOUNT
	DIST_NET = DIST_NET + NET

	!
	! Put the Debit/Credit information into the temporary file
	!
	!	ACCT_NUM$ = AP_PJL::ACCT
	!	ACCT_AMT = NET
	!	GOSUB PutInGL
	!
	GOTO ExitProgram IF GL_OUTP_ACCTSUM (OPT_ADDREC, AP_PJL::ACCT, &
		0.0, NET - PO_VARIANCE, AP_PJL::UNITS, TITLE$(), &
		UTL_REPORTX) <> CMC$_NORMAL

	!
	! End of sub-loop
	!
	GOTO 17410

	!
	! Print out some amounts
	!
17500	TEST_AMT = FUNC_ROUND((AP_PJH::INVAMT - AP_PJH::DISCAMT) - &
		AP_PJH::CKAMT, 2%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	IF (AP_PJH::CKAMT <> 0.0)
	THEN
		TEXT$ = SPACE$(20%) + "Cash" + SPACE$(16%) + &
			AP_PJH::CASH_ACCT + SPACE$(8%) + &
			FORMAT$(-AP_PJH::CKAMT, " ###,###,###.##") + &
			FORMAT$(-AP_PJH::CKAMT, "  ###,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		!
		! Put the Debit/Credit information into the temporary file
		!
	!		ACCT_NUM$ = AP_PJH::CASH_ACCT
	!		ACCT_AMT = -AP_PJH::CKAMT
	!		GOSUB PutInGL

		GOTO ExitProgram &
			IF GL_OUTP_ACCTSUM (OPT_ADDREC, AP_PJH::CASH_ACCT, &
			0.0, -AP_PJH::CKAMT, 0.0, TITLE$(), &
			UTL_REPORTX) <>CMC$_NORMAL

	END IF

	IF (FUNC_ROUND(TEST_AMT, 2%) <> 0.0)
	THEN
		TEXT$ = SPACE$(20%) + "Accounts Payable    " + &
			AP_PJH::AP_ACCT + SPACE$(34%) + &
			FORMAT$(-TEST_AMT, " ###,###,###.##") + &
			FORMAT$(-TEST_AMT, "  ###,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		!
		! Put the Debit/Credit information into the temporary file
		!
	!		ACCT_NUM$ = AP_PJH::AP_ACCT
	!		ACCT_AMT = -TEST_AMT
	!		GOSUB PutInGL

		GOTO ExitProgram &
			IF GL_OUTP_ACCTSUM (OPT_ADDREC, AP_PJH::AP_ACCT, &
			0.0, -TEST_AMT, 0.0, TITLE$(), &
			UTL_REPORTX) <> CMC$_NORMAL

	END IF

	IF (FUNC_ROUND(AP_PJH::DISCAMT, 2%) <> 0.0)
	THEN
		TEXT$ = SPACE$(20%) + "Discount" + SPACE$(64%) + &
			FORMAT$(-AP_PJH::DISCAMT, " ###,###,###.##") + &
			"            0.00"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	CASH = CASH + AP_PJH::CKAMT
	AP_GROSS = AP_GROSS + TEST_AMT
	AP_DISC = AP_DISC + AP_PJH::DISCAMT
	AP_NET = AP_NET + TEST_AMT

	GROSS_OB_TOTAL = FUNC_ROUND((GROSS_SUB_TOTAL - AP_PJH::INVAMT), 2%)
	NET_OB_TOTAL = FUNC_ROUND((NET_SUB_TOTAL - (AP_PJH::INVAMT - &
		AP_PJH::DISCAMT)), 2%)

	IF (GROSS_OB_TOTAL <> 0.0)	OR &
		(NET_OB_TOTAL <> 0.0)
	THEN
		TEXT$ = SPACE$(20%) + "OUT OF BALANCE" + SPACE$(58%) + &
			FORMAT$(GROSS_OB_TOTAL, " ###,###,###.##") + &
			FORMAT$(NET_OB_TOTAL, "  ###,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	!
	! Print two blank lines
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
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
	TEXT$ = SPACE$(20%) + "Distribution" + SPACE$(60%) + &
		FORMAT$(DIST_GROSS, " ###,###,###.##") + &
		FORMAT$(DIST_NET, "  ###,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TEXT$ = SPACE$(20%) + "Cash" + SPACE$(68%) + &
		FORMAT$(-CASH, " ###,###,###.##") + &
		FORMAT$(-CASH, "  ###,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TEXT$ = SPACE$(20%) + "Accounts Payable" + SPACE$(56%) + &
		FORMAT$(-AP_GROSS, " ###,###,###.##") + &
		FORMAT$(-AP_NET, "  ###,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	IF FUNC_ROUND(DIST_NET, 2%)
	THEN
		TEXT$ = SPACE$(20%) + "Discount" + SPACE$(65%) + &
			FORMAT$(-AP_DISC, "###,###,###.##") + "            0.00"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	NET_OB_TOTAL = FUNC_ROUND((DIST_NET - CASH - AP_NET), 2%)
	GROSS_OB_TOTAL = &
		FUNC_ROUND((DIST_GROSS - CASH - AP_GROSS - AP_DISC), 2%)

	IF (FUNC_ROUND(GROSS_OB_TOTAL, 2%) <> 0.0) OR &
		(FUNC_ROUND(NET_OB_TOTAL, 2%) <> 0.0)
	THEN
		TEXT$ = SPACE$(20%) + "TOTAL OUT OF BALANCE" + SPACE$(52%) + &
			FORMAT$(GROSS_OB_TOTAL, " ###,###,###.##") + &
			FORMAT$(NET_OB_TOTAL, "  ###,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	!
	! Print Hash Totals
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -4%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TEXT$ = "Hash Totals:  Vendor Number                " + &
		FORMAT$(GRAND_HASH_VENNUM, "###,###,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT


	TEXT$ = SPACE$(14%) + "Invoice Number               " + &
		FORMAT$(GRAND_HASH_INVNUM, "###,###,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	%PAGE

	!
	! Get ready to print Debit/Credit (New page, reset titles, etc.)
	!
	V% = GL_OUTP_ACCTSUM (OPT_SUMMARY, "", 0.0, 0.0, 0.0, &
		TITLE$(), UTL_REPORTX)

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
	! End of report AP_RPRT_PJ
	!******************************************************************
	END
