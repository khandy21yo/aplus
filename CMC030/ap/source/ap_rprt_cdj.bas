1	%TITLE "Print Cash Disbursements Journal"
	%SBTTL "AP_RPRT_CDJ"
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
	! ID:APCDRP
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Print Cash Disbursements Journal\* option
	!	prints the subject report subsequent to writing the checks. This journal
	!	includes the following fields:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	Check Number
	!	.le
	!	Check Date
	!	.le
	!	Vendor Number
	!	.le
	!	Vendor Name
	!	.le
	!	Transaction Number
	!	.le
	!	Invoice Number
	!	.le
	!	Invoice Date
	!	.le
	!	Gross Amount
	!	.le
	!	Discount Lost
	!	.le
	!	Payment Amount
	!	.els
	!	.lm -5
	!	.note RETENTION
	!	The Cash Disbursements Journal report should be
	!	permanently filed along with the related Cash
	!	Disbursements Posting Transmittal report.
	!	.end note
	!	.note
	!	The Cash Disbursements Journal ^&must\& be printed prior
	!	to executing the Cash Disbursements Post option, since
	!	the journal file is deleted at the completion of the
	!	posting routine.
	!	.end note
	!
	! Index:
	!	.x Cash Disbursements>Print CD Journal
	!	.x Reports>Cash Disbursements Journal
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_RPRT_CDJ/LINE
	!	$ LINK/EXECUTABLE=AP_EXE:*.EXE AP_RPRT_CDJ, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_RPRT_CDJ.OBJ;*
	!
	! Author:
	!
	!	09/29/87 - B. Craig Larsen
	!
	! Modification history:
	!
	!	08/20/88 - Kevin Handy
	!		Fixed minor formatting problems.
	!
	!	09/09/88 - Kevin Handy
	!		Fixed so will handle check totals correctly.
	!
	!	11/02/88 - Kevin Handy
	!		Fixed so it prints invoice date always in the
	!		invdate column instead of sometimes the check
	!		date.
	!
	!	08/10/89 - Aaron Redd
	!		Added Debit/Credit page(s) to end of report.
	!
	!	06/04/91 - Craig Tanner
	!		Modified to use function GL_OUTP_ACCTSUM to do
	!		Debit/Credit page.
	!
	!	12/18/91 - Kevin Handy
	!		Removed POST_TO_GL structure, which was not used
	!		in this program.
	!
	!	10/23/92 - Dan Perkins
	!		Added agruement to GL_OUTP_ACCTSUM because of
	!		change to this function.
	!
	!	10/26/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	03/18/93 - Kevin Handy
	!		Added parameter to GL_OUTP_ACCTSUM for units.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	02/20/96 - Kevin Handy
	!		Reformat source code.
	!
	!	06/04/96 - Kevin Handy
	!		Added batch number to AP_CDJ.
	!
	!	08/20/97 - Kevin Handy
	!		Don't need to assign channel for report
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE's
	!
	!	08/05/99 - Kevin Handy
	!		Add some more '#' where possible
	!
	!	10/23/2000 - Kevin Handy
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
	! External functions
	!
	EXTERNAL LONG   FUNCTION GL_OUTP_ACCTSUM

	!******************************************************************
	! Set up data storage areas (MAPs, DIMENSIONs, DECLAREs, etc.)
	!******************************************************************

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[AP.OPEN]AP_CDJ.HB"
	MAP (AP_CDJ)		AP_CDJ_CDD		AP_CDJ

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP (AP_VENDOR)		AP_VENDOR_CDD		AP_VENDOR

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
	! Initialize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	CDJ_BATCH$ = TRM$(UTL_REPORTX::OPTDEF(0%))
	!++
	! Abstract:FLD01
	!	^*(01) Batch Number\*
	!	.b
	!	This field specifies which batch to print.
	!
	! Index:
	!	.x Batch Number>Cash Disbursements Journal
	!	.x Cash Disbursements Journal>Batch Number
	!
	!--

	T_COUNT% = 0%
	A_CKNUM$, A_VENNUM$ = ""
	CK_TOTAL, DIS_LOST_TOTAL, NET_TOTAL, NET_AMOUNT = 0.0

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.OPN"
	USE
		FILENAME$ = "AP_VENDOR"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_CDJ.OPN"
	USE
		FILENAME$ = "AP_CDJ"
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
	TITLE$(1%) = "Cash Disbursements Journal Report"
	TITLE$(2%) = "Accounts Payable System"
	TITLE$(3%) = ""

	TITLE$(4%) = "CkNum   CkDate    VendorNum  VendorName    " + &
		"           TransNum  InvNum          InvDate       " + &
		" GrossAmt     DscntLost        PayAmt"
	TITLE$(5%) = "."

	LYT_LINE$ = "$CheckNum:006,DCheckDate:016,$VendorNum:028," + &
		"$VendorName:053,$TransNum:060,$InvoiceNum:079," + &
		"DInvoiceDate:088,VCheckAmt:103,VAmtDiscountLost:117," + &
		"VNetAmount:131"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		RESET #AP_CDJ.CH%, KEY #1%
	USE
		FILENAME$ = "AP_CDJ"
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
		GET #AP_CDJ.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "AP_CDJ"
		CONTINUE HelpError
	END WHEN

	!
	! Check status
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Check current record
	!
	GOTO GetNextRec &
		IF (TRM$(AP_CDJ::CKNUM) = "")

	!
	! Get the AP Vendor description
	!
	AP_VENDOR::VENNAM = STRING$(20%, 63%)

17200	WHEN ERROR IN
		GET #AP_VENDOR.CH%, KEY #0% EQ AP_CDJ::VENNUM, REGARDLESS
	USE
		CONTINUE 17300 IF ERR = 155%
		FILENAME$ = "AP_VENDOR"
		CONTINUE HelpError
	END WHEN

	!
	! Put the Debit/Credit information into the temporary file
	!
	GOTO ExitProgram &
		IF GL_OUTP_ACCTSUM (OPT_ADDREC, AP_CDJ::DISCLOST_ACCT, &
		0.0, AP_CDJ::DISC_LOST_AMT, 0.0, TITLE$(), UTL_REPORTX) <> &
		CMC$_NORMAL

	IF (AP_CDJ::CKAMT <> 0.0)
	THEN
		!
		! Put the Debit/Credit information into the temporary file
		!
		GOTO ExitProgram &
			IF GL_OUTP_ACCTSUM (OPT_ADDREC, AP_CDJ::AP_ACCT, &
			0.0, AP_CDJ::CKAMT, 0.0, TITLE$(), UTL_REPORTX) <> &
			CMC$_NORMAL
	END IF

	IF ((AP_CDJ::CKAMT + AP_CDJ::DISC_LOST_AMT) <> 0.0)
	THEN
		!
		! Put the Debit/Credit information into the temporary file
		!
		ACCT_AMT = -(AP_CDJ::CKAMT + AP_CDJ::DISC_LOST_AMT)
		GOTO ExitProgram &
			IF GL_OUTP_ACCTSUM (OPT_ADDREC, AP_CDJ::CASH_ACCT, &
			0.0, ACCT_AMT, 0.0, TITLE$(), UTL_REPORTX) <> &
			CMC$_NORMAL

	END IF

	!
	! Print check total
	!
17300	NET_AMOUNT = FUNC_ROUND(AP_CDJ::CKAMT + AP_CDJ::DISC_LOST_AMT, 2%)

	!
	! Print out one line
	!
	IF (A_CKNUM$ = AP_CDJ::CKNUM) AND (A_VENNUM$ = AP_CDJ::VENNUM)
	THEN

		TEXT$ = SPACE$(54%) + &
			AP_CDJ::TRANKEY + "    " + &
			AP_CDJ::INVNUM + " " + &
			PRNT_DATE(AP_CDJ::INVDAT, 6%) + &
			FORMAT$(AP_CDJ::CKAMT, " ###,###,###.##") + &
			FORMAT$(AP_CDJ::DISC_LOST_AMT, "###,###,###.##") + &
			FORMAT$(NET_AMOUNT, "###,###,###.##")
	ELSE
		GOSUB CheckTotal

		TEXT$ = AP_CDJ::CKNUM + "  " + &
			PRNT_DATE(AP_CDJ::CKDAT, 6%) + "  " + &
			AP_CDJ::VENNUM + " " + &
			LEFT(AP_VENDOR::VENNAM, 24%) + " " + &
			AP_CDJ::TRANKEY + "    " + &
			AP_CDJ::INVNUM + " " + &
			PRNT_DATE(AP_CDJ::INVDAT, 6%) + &
			FORMAT$(AP_CDJ::CKAMT, " ###,###,###.##") + &
			FORMAT$(AP_CDJ::DISC_LOST_AMT, "###,###,###.##") + &
			FORMAT$(NET_AMOUNT, "###,###,###.##")

		A_CKNUM$ = AP_CDJ::CKNUM
		A_VENNUM$ = AP_CDJ::VENNUM

	END IF

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	T_COUNT% = T_COUNT% + 1%
	CK_TOTAL = CK_TOTAL + FUNC_ROUND(AP_CDJ::CKAMT, 2%)
	DIS_LOST_TOTAL = DIS_LOST_TOTAL + FUNC_ROUND(AP_CDJ::DISC_LOST_AMT, 2%)
	NET_TOTAL = NET_TOTAL + NET_AMOUNT

	GRAND_CK_TOT = GRAND_CK_TOT + FUNC_ROUND(AP_CDJ::CKAMT, 2%)
	GRAND_DIS_LOST_TOT = GRAND_DIS_LOST_TOT + &
		FUNC_ROUND(AP_CDJ::DISC_LOST_AMT, 2%)
	GRAND_NET_TOT = GRAND_NET_TOT + NET_AMOUNT

	NET_AMOUNT = 0.0

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
	! Print check total
	!
	GOSUB CheckTotal

	!
	! Print out Journal total
	!
	TEXT$ = SPACE$(75%) + "Journal Total " + &
		FORMAT$(GRAND_CK_TOT, "###,###,###.##") + &
		FORMAT$(GRAND_DIS_LOST_TOT, "###,###,###.##") + &
		FORMAT$(GRAND_NET_TOT, "###,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	V% = GL_OUTP_ACCTSUM(OPT_SUMMARY, "", 0.0, 0.0, 0.0, &
		TITLE$(), UTL_REPORTX)

	%PAGE

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

 CheckTotal:
	!******************************************************************
	! Subroutine to print out Check total if need be
	!******************************************************************
	SELECT T_COUNT%

	CASE > 1%
		TEXT$ = SPACE$(76%) + "Check Total  " + &
			FORMAT$(CK_TOTAL, "###,###,###.##") + &
			FORMAT$(DIS_LOST_TOTAL, "###,###,###.##") + &
			FORMAT$(NET_TOTAL, "###,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

	CASE 1%
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

	END SELECT

	T_COUNT%, CK_TOTAL, DIS_LOST_TOTAL, NET_TOTAL = 0.0

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

	%PAGE

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Handle untrapped errors
	!
	FILENAME$ = ""
	RESUME HelpError

32767	!******************************************************************
	! End of report AP_RPRT_CDJ
	!******************************************************************
	END
