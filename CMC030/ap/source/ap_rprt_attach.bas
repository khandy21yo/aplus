1	%TITLE "Print Invoice Attachment"
	%SBTTL "AP_RPRT_ATTACH"
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
	! ID:APPJAT
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Print Invoice Attachment\* option prints
	!	a dump of each vendor's charge entered in the Purchases Journal.
	!	It is intended that each transaction dump be attached to the
	!	vendor's invoice and/or other documents authorizing entry into the
	!	Purchases Journal. It is suggested that a duplicate copy of the
	!	Invoice Attachment forms be filed in Transaction Number order.
	!	This attachment includes the following information:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	Transaction Number
	!	.le
	!	Transaction Date
	!	.le
	!	Vendor Number
	!	.le
	!	Vendor Name
	!	.le
	!	Invoice Number
	!	.le
	!	Invoice Date
	!	.le
	!	Invoice Amount
	!	.le
	!	Description
	!	.le
	!	Accounts Payable Account
	!	.le
	!	Due Date
	!	.le
	!	Discount Date
	!	.le
	!	Discount Amount
	!	.le
	!	1099 Code
	!	.le
	!	1099 Amount
	!	.le
	!	Check Number
	!	.le
	!	Check Date
	!	.le
	!	Check Amount
	!	.le
	!	Cash Account
	!	.le
	!	Purchase Order Number
	!	.le
	!	Purchase Order Line
	!	.le
	!	Account
	!	.le
	!	Sub Account
	!	.le
	!	Operation
	!	.le
	!	Units
	!	.le
	!	Use Tax
	!	.le
	!	Amount
	!	.le
	!	Discount Amount
	!	.els
	!	.lm -5
	!	.note
	!	The Invoice Attachments must be printed prior to posting
	!	a Purchases Journal batch, since a batch file is deleted
	!	upon the completion of a posting routine.
	!	.end note
	!
	! Index:
	!	.x Print>Invoice Attachment
	!	.x Purchases Journal>Print Invoice Attachment
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_RPRT_ATTACH/LINE
	!	$ LINK/EXEC=AP_EXE:*.EXE AP_RPRT_ATTACH, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_RPRT_ATTACH.OBJ;*
	!
	! Author:
	!
	!	08/12/87 - B. Craig Larsen
	!
	! Modification history:
	!
	!	07/02/90 - Kevin Handy
	!		Modified to use formatted PO number.
	!
	!	03/05/92 - Dan Perkins
	!		Modified to use CONV_STRING instead of PRNT_PO.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	03/20/96 - Kevin Handy
	!		Reformat source code.
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

	%INCLUDE "SOURCE:[AP.OPEN]AP_PJH.HB"
	MAP	(AP_PJH)	AP_PJH_CDD	AP_PJH

	%INCLUDE "SOURCE:[AP.OPEN]AP_PJL.HB"
	MAP	(AP_PJL)	AP_PJL_CDD	AP_PJL

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
	!	The ^*From Transaction _#\* field determins the transaction
	!	number with which the report will begin. A blank setting will
	!	cause the report to begin with the first transaction number
	!	in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Transaction #>Purchases Journal Attachment
	!	.x Purchases Journal Attachment>From Transaction #
	!	.x From>Transaction
	!	.x Transaction>From
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Transaction _#\*
	!	.b
	!	.lm +5
	!	The ^*To Transaction _#\* field enters a
	!	transaction number with which the report will end. A
	!	blank setting will cause the report to end with the last
	!	transaction number in the file.
	!	.lm -5
	!
	! Index:
	!	.x Purchases Journal Attachment>To Transaction #
	!	.x To Transaction #>Purchases Journal Attachment
	!	.x To>Transaction #
	!	.x Transaction #>To
	!
	!--

	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) Batch Number\*
	!	.b
	!	.lm +5
	!	The ^*Batch Number\* field determines which batch
	!	file will be printed. The ^*Batch Number\* field must contain a
	!	valid value, a batch number which contains data.
	!	.lm -5
	!
	! Index:
	!	.x Batch Number>Purchases Journal Attachment
	!	.x Purchases Journal Attachment>Batch Number
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
	! Open the Accounts Payable Purchases Journal Line Item file
	!
310	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_PJL.OPN"
	USE
		FILENAME$ = "AP_PJL"
		CONTINUE HelpError
	END WHEN

	!
	! Open the Accounts Payable Vendor master file
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
	TITLE$(1%) = "Invoice Attachment (Purchase Transaction Dump)"
	TITLE$(2%) = ""

	!
	! Headers
	!
	SUBTITLE1$ = "TransNum  TransDate  VendorNum   " + &
		"VendorName                          InvoiceNum       " + &
		"InvDate    InvoiceAmt  Description         "
	SUBTITLE2$ = "AP Account          DueDate  DiscDate   " + &
		"DiscountAmt  1099Code   1099Amount  CheckNum  CheckDate  " + &
		"CheckAmount  Cash Account"
	SUBTITLE3$ = "PONumber    POLine  Account             " + &
		"SubAccount  Operation   Units UT         Amount    DiscountAmt"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #AP_PJH.CH%
		ELSE
			FIND #AP_PJH.CH%, KEY #0% GE FROM_ITEM$, REGARDLESS
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
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "AP_PJH"
		CONTINUE HelpError
	END WHEN

	!
	! Check status
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Force a new page
	!
	IF UTL_REPORTX::PAGENO > UTL_REPORTX::STARTP
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 1000%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	!
	! Check current record
	!
	GOTO ExitTotal &
		IF (AP_PJH::TRANKEY > TO_ITEM$) AND (TO_ITEM$ <> "")

	AP_VENDOR::VENNAM = STRING$(40%, 63%)

	!
	! Get the Vendor Number description
	!
17200	WHEN ERROR IN
		GET #AP_VENDOR.CH%, KEY #0% EQ AP_PJH::VENNUM, REGARDLESS
	USE
		CONTINUE 17300 IF ERR = 155%
		FILENAME$ = "AP_VENDOR"
		CONTINUE HelpError
	END WHEN

	!
	! Print the title to the first line
	!
17300	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), SUBTITLE1$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Build the first line
	!
	TEXT$ = AP_PJH::TRANKEY + "    " + &
		PRNT_DATE(AP_PJH::TRANKEY_DATE, 0%) + "   " + &
		AP_PJH::VENNUM + "  " + &
		LEFT(AP_VENDOR::VENNAM, 35%) + " " + &
		AP_PJH::INVNUM + "  " + &
		PRNT_DATE(AP_PJH::INVDAT, 0%) + " " + &
		FORMAT$(AP_PJH::INVAMT, "#,###,###.##  ") + &
		AP_PJH::DESCR

	!
	! Print out the first line
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Print out the title to the second line
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), SUBTITLE2$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Build the second line
	!
	TEXT$ = AP_PJH::AP_ACCT + "  " + &
		PRNT_DATE(AP_PJH::DUEDAT, 0%) + " " + &
		PRNT_DATE(AP_PJH::DISCDAT, 0%) + &
		FORMAT$(AP_PJH::DISCAMT, "  #,###,###.##") + "  " + &
		AP_PJH::CODE_1099 + "     " + &
		FORMAT$(AP_PJH::AMT_1099, "  #,###,###.##") + "  " + &
		AP_PJH::CKNUM + "    " + &
		PRNT_DATE(AP_PJH::CKDAT, 0%) + &
		FORMAT$(AP_PJH::CKAMT, "  #,###,###.##") + "  " + &
		AP_PJH::CASH_ACCT

	!
	! Print out the second line
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get ready to print out the Line Items
	!
	TEXT$ = SPACE$(35%) + "* * * * *  L I N E  I T E M S  * * * * *"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Print out the Line Item title
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), SUBTITLE3$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Set the totals to zero
	!
	AMOUNT_TOTAL, DISC_TOTAL = 0.0

	!
	! Find the first record
	!
17400	WHEN ERROR IN
		FIND #AP_PJL.CH%, KEY #0% EQ AP_PJH::TRANKEY, REGARDLESS
	USE
		CONTINUE 17700 IF ERR = 155%
		FILENAME$ = "AP_PJL"

		CONTINUE HelpError
	END WHEN

	!
	! Get the (next) record
	!
17600	WHEN ERROR IN
		GET #AP_PJL.CH%, REGARDLESS
	USE
		CONTINUE 17700 IF ERR = 11%
		FILENAME$ = "AP_PJL"
		CONTINUE HelpError
	END WHEN

	GOTO 17700 IF AP_PJL::TRANKEY <> AP_PJH::TRANKEY

	!
	! Build the line to print
	!
	TEXT$ = CONV_STRING(AP_PJL::PONUM, CMC$_LEFT) + "  " + &
		AP_PJL::PO_LINE + "    " + &
		AP_PJL::ACCT + "  " + &
		AP_PJL::SUBACC + "  " + &
		AP_PJL::OPERATION + " " + &
		FORMAT$(AP_PJL::UNITS, " ####.##") + " " + &
		AP_PJL::USE_TAX_FLAG + " " + &
		FORMAT$(AP_PJL::AMOUNT, " ###,###,###.##") + &
		FORMAT$(AP_PJL::DISCAMT, " ###,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Add to the totals
	!
	AMOUNT_TOTAL = AMOUNT_TOTAL + AP_PJL::AMOUNT
	DISC_TOTAL = DISC_TOTAL + AP_PJL::DISCAMT

	!
	! End of Sub-loop
	!
	GOTO 17600

17700	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Build the line containing the Line Item totals
	!
	TEXT$ = SPACE$(62%) + "TOTALS    " + &
		FORMAT$(AMOUNT_TOTAL, " ###,###,###.##") + &
		FORMAT$(DISC_TOTAL, " ###,###,###.##")

	!
	! Print out the Line Item totals
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

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
	! End of report AP_RPRT_ATTACH
	!******************************************************************
	END
