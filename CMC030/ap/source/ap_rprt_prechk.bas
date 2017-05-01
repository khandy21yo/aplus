1	%TITLE "Accounts Payable Precheck Report"
	%SBTTL "AP_RPRT_PRECHK"
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
	! ID:APCDPC
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Print Precheck Report\* option prints,
	!	prior to printing checks, all items which have been
	!	selected for payment from the Accounts Payable Subsidiary Ledger
	!	file. Included in this report are the following fields:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	Vendor Number
	!	.le
	!	Transaction Number
	!	.le
	!	Invoice Number
	!	.le
	!	Invoice Date
	!	.le
	!	Purchase Order Number
	!	.le
	!	Check Description
	!	.le
	!	Gross Amount
	!	.le
	!	Discount Amount
	!	.le
	!	Discount Lost Amount
	!	.le
	!	Net Amount
	!	.le
	!	Check Number
	!	.le
	!	Check Date
	!	.els
	!
	! Index:
	!	.x Precheck Report
	!	.x Report>Precheck
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_RPRT_PRECHK/LINE
	!	$ LINK/EXECUTABLE=AP_EXE:*.EXE AP_RPRT_PRECHK, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_RPRT_PRECHK.OBJ;*
	!
	! Author:
	!
	!	09/29/87 - B. Craig Larsen
	!
	! Modification history:
	!
	!	07/05/90 - Kevin Handy
	!		Modified to use formatted PO number.
	!
	!	03/04/92 - Dan Perkins
	!		Use function CONV_STRING instead of PRNT_PO.
	!
	!	03/12/92 - Kevin Handy
	!		Clean up (check).
	!
	!	03/12/92 - Kevin Handy
	!		Unrolled error trapping (check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	06/04/96 - Kevin Handy
	!		Reformat source code.
	!		Add batch number to AP_CDJ.
	!
	!	05/13/97 - Kevin Handy
	!		Reformat source code
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

	!
	! External functions
	!
	EXTERNAL STRING	FUNCTION CONV_STRING

	!******************************************************************
	! Set up data storage areas (MAPs, DIMENSIONs, DECLAREs, etc.)
	!******************************************************************

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD	UTL_REPORTX

	%INCLUDE "SOURCE:[AP.OPEN]AP_CDJ.HB"
	MAP	(AP_CDJ)	AP_CDJ_CDD	AP_CDJ

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
	! Initialize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)

	!++
	! Abstract:FLD01
	!	^*(01) Sort Order (NU,NA,S)\*
	!	.b
	!	.lm +5
	!	The ^*Sort Order (NU, NA, S)\* field determines
	!	how the report is to be printed. Valid choices are:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	^*NU\* = Vendor Number Order
	!	.le
	!	^*NA\* = Vendor Name
	!	.le
	!	^*S\* #= Alphabetical Sort
	!	.els
	!	.lm -5
	!	.b
	!	A blank setting will cause the report to print in numerical order.
	!	.b
	!	There are no other valid values for this field.
	!	.lm -5
	!
	! Index:
	!	.x Preprint Report>Sort Order
	!	.x Sort Order>Preprint Report
	!
	!--

	CDJ_BATCH$ = EDIT$(UTL_REPORTX::OPTDEF(1%), -1%)

	!++
	! Abstract:FLD02
	!	^*(02) Batch Number\*
	!	.b
	!	This field specifies which journal to print.
	!
	! Index:
	!	.x Preprint Report>Batch Number
	!	.x Batch Number>Preprint Report
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

	!
	! Initialize variables
	!
	TRAN_COUNT% = 0%
	VEN_AMT, VEN_DIS, VEN_LOST, VEN_NET, &
		TOT_AMT, TOT_LOST, TOT_DIS, TOT_NET = 0.0
	VEN_TEST$ = ""

300	!
	! Open vendor file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.OPN"
	USE
		FILENAME$ = "AP_VENDOR"
		CONTINUE HelpError
	END WHEN

	!
	! Open the AP Cash Disbursments Journal file
	!
310	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_CDJ.OPN"
	USE
		FILENAME$ = "AP_CDJ"
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "Pre Check Report"
	TITLE$(2%) = ""

	!
	! Heading
	!
	TITLE$(3%) = "VendorNum  Trans# InvoiceNum      InvDate  " + &
		"PONumber    CheckDescr             GrossAmt DiscAmt " + &
		"LostAmt     NetAmt CkNum  CkDate"
	TITLE$(4%) = ""

	!
	! Layouts for printed lines
	!
	LYT_LINE$ = "$VendorNum:010,$TransNum:017,$InvoiceNum:033," + &
		"DInvoiceDate:042,$PONumber:054,$CheckDescr:075," + &
		"VGrossAmt:086,VDiscountAmt:094,VAmtDiscountLost:102," + &
		"VNetAmount:113,$CheckNum:120,DCheckDate:129"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		RESET #AP_VENDOR.CH%, KEY #K_NUM%
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
	! Get ready to start a Sub-loop
	!
17200	WHEN ERROR IN
		FIND #AP_CDJ.CH%, KEY #0% EQ AP_VENDOR::VENNUM, REGARDLESS
	USE
		CONTINUE GetNextRec IF (ERR = 155%)
		FILENAME$ = "AP_CDJ"
		CONTINUE HelpError
	END WHEN

	!
	! Set some initial values for variables
	!
	VEN_AMT, VEN_DIS, VEN_NET = 0.0
	TRAN_COUNT% = 0%

	!
	! Print out a line identifying the Vendor
	!
	TEXT$ = AP_VENDOR::VENNUM + " " + &
		AP_VENDOR::VENNAM + " " + &
		TRM$(AP_VENDOR::ADD1) + " " + &
		TRM$(AP_VENDOR::ADD2) + ", " + &
		TRM$(AP_VENDOR::CITY) + ", " + &
		TRM$(AP_VENDOR::STATE) + "  " + &
		TRM$(AP_VENDOR::ZIP)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get the (next) record
	!
17210	WHEN ERROR IN
		GET #AP_CDJ.CH%, REGARDLESS
	USE
		CONTINUE 17300 IF (ERR = 11%)
		FILENAME$ = "AP_CDJ"
		CONTINUE HelpError
	END WHEN

	GOTO 17300 IF AP_VENDOR::VENNUM <> AP_CDJ::VENNUM

	!
	! Play with some variables
	!
	TRAN_COUNT% = TRAN_COUNT% + 1%

	AMT = AP_CDJ::CKAMT + AP_CDJ::DISAMT
	DIS = AP_CDJ::DISAMT
	LOST = AP_CDJ::DISC_LOST_AMT
	NET = FUNC_ROUND(AMT - DIS + LOST, 2%)

	VEN_AMT = FUNC_ROUND(VEN_AMT + AMT, 2%)
	VEN_DIS = FUNC_ROUND(VEN_DIS + DIS, 2%)
	VEN_LOST = FUNC_ROUND(VEN_LOST + LOST, 2%)
	VEN_NET = FUNC_ROUND(VEN_NET + NET, 2%)

	TOT_AMT = FUNC_ROUND(TOT_AMT + AMT, 2%)
	TOT_DIS = FUNC_ROUND(TOT_DIS + DIS, 2%)
	TOT_LOST = FUNC_ROUND(TOT_LOST + LOST, 2%)
	TOT_NET = TOT_NET + NET

	!
	! Print out one line, from the CD Journal
	!
	TEXT$ = AP_CDJ::VENNUM + " " + &
		AP_CDJ::TRANKEY + " " + &
		AP_CDJ::INVNUM + " " + &
		PRNT_DATE(AP_CDJ::INVDAT, 0%) + " " + &
		CONV_STRING(AP_CDJ::PONUM, CMC$_LEFT) + "  " + &
		AP_CDJ::CKDESC + &
		FORMAT$(AMT, " #######.##") + &
		FORMAT$(DIS, " ####.##") + &
		FORMAT$(LOST, " ####.##") + &
		FORMAT$(NET, " #######.##") + " " + &
		AP_CDJ::CKNUM + " " + &
		PRNT_DATE(AP_CDJ::CKDAT, 0%)

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! End of the Sub-loop
	!
	GOTO 17210

	!
	! Print total for a vendor
	!
17300	GOSUB CheckTotal IF (TRAN_COUNT% > 1%) OR (VEN_NET < 0.0)
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
	TEXT$ = SPACE$(59%) + &
		"Journal Total  " + &
		FORMAT$(TOT_AMT, " #######.##") + &
		FORMAT$(TOT_DIS, " ####.##") + &
		FORMAT$(TOT_LOST, " ####.##") + &
		FORMAT$(TOT_NET, " #######.##")

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

 CheckTotal:
	!******************************************************************
	! Subroutine to print out Check total if need be
	!******************************************************************
	IF VEN_NET < 0.0
	THEN
		TEXT$ = "**CHECK HAS A NEGATIVE BALANCE." + &
			"  REMOVING FROM PAY RUN**"
		CALL OUTP_LINE("",  UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ComeBack IF UTL_REPORTX::STAT

		TOT_AMT = TOT_AMT - FUNC_ROUND(VEN_AMT, 2%)
		TOT_DIS = TOT_DIS - FUNC_ROUND(VEN_DIS, 2%)
		TOT_LOST = TOT_LOST- FUNC_ROUND(VEN_LOST, 2%)
		TOT_NET = TOT_NET - FUNC_ROUND(VEN_NET, 2%)
		VEN_AMT, VEN_DIS, VEN_LOST, VEN_NET = 0.0
	END IF

	TEXT$ = SPACE$(61%) + &
		"Check Total  " + &
		FORMAT$(VEN_AMT, " #######.##") + &
		FORMAT$(VEN_DIS, " ####.##") + &
		FORMAT$(VEN_LOST, " ####.##") + &
		FORMAT$(VEN_NET, " #######.##")

	CALL OUTP_LINE("",  UTL_REPORTX, TITLE$(), TEXT$, 0%)

 ComeBack:
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
	! End of report AP_RPRT_PRECHK
	!******************************************************************
	END
