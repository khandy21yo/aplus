1	%TITLE "Accounts Payable Report"
	%SBTTL "AP_RPRT_INVEXCEPT"
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
	! ID:APINV
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This report prints out an Accounts Payable
	!	Register or an Accounts Payable Cutoff Ledger.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_RPRT_INVEXCEPT /LINE
	!	$ LINK/EXECUTABLE=AP_EXE:*.EXE AP_RPRT_INVEXCEPT, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_RPRT_INVEXCEPT.OBJ;*
	!
	! Author:
	!
	!	06/19/89 - Lance Williams
	!
	! Modification history:
	!
	!	06/21/90 - Aaron Redd
	!		Added line layout information so that the report could
	!		also be sent to either a spreadsheet or a DIF file.
	!
	!	06/05/91 - Kevin Handy
	!		Removed junk code in error trapping.
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
	!	08/20/97 - Kevin Handy
	!		Don't need to assign channel to report
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

	!
	! Declare Temporary Record
	!
	DECLARE	AP_OPEN_CDD	AP_OPEN_PRIOR
	DECLARE	STRING		LYT_LINE

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

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.OPN"
	USE
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.OPN"
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "AP_VENDOR"
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
	!
	! Set up titles
	!
	TITLE$(1%) = "Accounts Payable Invoice Except Report"
	TITLE$(2%) = "Accounts Payable System"
	TITLE$(3%) = ""

	!
	! Column headings
	!
	TITLE$(4%) = "VendorNam  VendorName                               " + &
		"InvoiceNum      Transkey  TransferDate     InvDate        " + &
		"InvAmt "
	TITLE$(5%) = ""

	!
	! Line layouts
	!
	LYT_LINE = "$VendorNum:010,$VendorName:041,$InvoiceNum:057," + &
		"$TranKey:064,DTranKeyDate:076,DInvDate:093,VInvAmt:106"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	RESET #AP_OPEN.CH%, KEY #1%

 GetNextRec:
17010	!******************************************************************
	! Main report loop starts here
	!******************************************************************

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #AP_OPEN.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "AP_VENDOR"
		CONTINUE HelpError
	END WHEN

	!
	! Check status
	!
	IF AP_OPEN::INVAMT = 0.0
	THEN
		GOTO GetNextRec
	END IF

17020	GET #AP_VENDOR.CH%, &
		KEY #0% EQ AP_OPEN::VENNUM, &
		REGARDLESS

	IF AP_OPEN_PRIOR::VENNUM = ""
	THEN
		AP_OPEN_PRIOR::VENNUM = AP_OPEN::VENNUM
	END IF

	IF AP_OPEN_PRIOR::VENNUM = AP_OPEN::VENNUM AND &
		AP_OPEN_PRIOR::INVNUM = AP_OPEN::INVNUM
	THEN
		GOTO 17030
	ELSE
		GOTO 17040
	END IF


17030	IF VENDORNUM$ = AP_OPEN::INVNUM
	THEN
		GOTO 17035
	END IF

	GOTO ExitProgram IF UTL_REPORTX::STAT

	TEXT$ = AP_OPEN_PRIOR::VENNUM + " " + &
		AP_VENDOR::VENNAM + " " + &
		AP_OPEN_PRIOR::INVNUM + " " + &
		AP_OPEN_PRIOR::TRANKEY + "    " + &
		PRNT_DATE(AP_OPEN_PRIOR::TRANKEY_DATE, 0%) + "         " + &
		PRNT_DATE(AP_OPEN_PRIOR::INVDAT, 0%) + " " + &
		FORMAT$(AP_OPEN_PRIOR::INVAMT, "<%>########.## ")

	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)

17035	TEXT$ = AP_OPEN::VENNUM + " " + &
		AP_VENDOR::VENNAM + " " + &
		AP_OPEN::INVNUM + " "  + &
		AP_OPEN::TRANKEY + "    " + &
		PRNT_DATE(AP_OPEN::TRANKEY_DATE, 0%) + "         " + &
		PRNT_DATE(AP_OPEN::INVDAT, 0%) + " " + &
		FORMAT$(AP_OPEN::INVAMT, "<%>########.## ")

	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)

	IF AP_OPEN_PRIOR::INVNUM <> AP_OPEN::INVNUM
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	END IF

17040	VENDORNUM$ = AP_OPEN_PRIOR::INVNUM
	AP_OPEN_PRIOR::VENNUM = AP_OPEN::VENNUM
	AP_OPEN_PRIOR::INVNUM = AP_OPEN::INVNUM
	AP_OPEN_PRIOR::TRANKEY =  AP_OPEN::TRANKEY
	AP_OPEN_PRIOR::TRANKEY_DATE = AP_OPEN::TRANKEY_DATE
	AP_OPEN_PRIOR::INVDAT = AP_OPEN::INVDAT
	AP_OPEN_PRIOR::INVAMT = AP_OPEN::INVAMT
	!
	! Try for next record
	!
	GOTO GetNextRec

	%PAGE

	!******************************************************************
	! Handle totals and other items before EXITing
	!******************************************************************

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
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
