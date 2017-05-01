1	%TITLE "Accounts Payable Transaction Report"
	%SBTTL "AP_RPRT_APTRAN"
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
	! ID:APTRAN
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Print by Transaction Key\* option
	!	printes a report which
	!	lists all accounts payable transactions for a specified accounting
	!	period in transaction key order. The fields contained in this report consist
	!	of the following:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	Transaction Number
	!	.le
	!	Vendor Number
	!	.le
	!	Vendor Name
	!	.le
	!	Invoice Number
	!	.le
	!	Invoice Date
	!	.le
	!	Purchase Order Number
	!	.le
	!	Description
	!	.le
	!	Account Number
	!	.le
	!	Net Amount
	!	.els
	!
	! Index:
	!	.x Accounts Payable>Reports>Transaction Key Audit
	!	.x Reports>Accounts Payable Transaction Key Audit
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_RPRT_APTRAN/LINE
	!	$ LINK/EXECUTABLE=AP_EXE:*.EXE AP_RPRT_APTRAN, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_RPRT_APTRAN.OBJ;*
	!
	! Author:
	!
	!	07/31/87 - B. Craig Larsen
	!
	! Modification history:
	!
	!	10/21/87 - Robert Peterson
	!		Added interrupt menu during creation of work file
	!
	!	11/15/87 - Robert Peterson
	!		Changed process to automatically calculate the
	!		ledger date.
	!
	!	07/05/90 - Kevin Handy
	!		Modified to handle formatted PO number.
	!
	!	06/05/91 - Kevin Handy
	!		Unwound error trapping.
	!
	!	03/05/92 - Dan Perkins
	!		Use CONV_STRING instead of PRNT_PO.
	!
	!	03/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	12/14/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!		Change RIGHT(NUM1$()) to FORMAT$().
	!
	!	08/28/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	10/06/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/22/97 - Kevin Handy
	!		Use 'val%' instead of 'val'
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/16/99 - Kevin Handy
	!		Fix calls to unsolicited input
	!
	!	11/06/99 - Kevin Handy
	!		Lose maps for AP_CLOSE which were never used.
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
	EXTERNAL LONG	OUTP_XUNSOL ! (It's really an AST routine)

	%PAGE

	!******************************************************************
	! Set up data storage areas (MAPs, DIMENSIONs, DECLAREs, etc.)
	!******************************************************************

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD	UTL_REPORTX

	%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL.HB"
	MAP	(AP_CONTROL)	AP_CONTROL_CDD	AP_CONTROL

	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.HB"
	MAP	(AP_OPEN)	AP_OPEN_CDD	AP_OPEN

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP	(AP_VENDOR)	AP_VENDOR_CDD	AP_VENDOR

	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	MAP	(GL_PERIOD)	GL_PERIOD_CDD	GL_PERIOD

	MAP (DP_OUTP_XUNSOL) &
		LONG	RRR_FLAG

	MAP (AP_TEMP) &
		STRING	AP_TEMP.TYPE =  1%, &
		STRING	AP_TEMP.TRAN_CK =  6%, &
		STRING	AP_TEMP.VENNUM = 10%, &
		STRING	AP_TEMP.INVNUM = 15%, &
		STRING	AP_TEMP.INVDAT =  8%, &
		STRING	AP_TEMP.TRAN_PO = 10%, &
		STRING	AP_TEMP.DESC = 20%, &
		STRING	AP_TEMP.ACCT = 18%, &
		REAL	AP_TEMP.AMOUNT

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
	! Initialize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	%PAGE

	CALL ASSG_CHANNEL(AP_TEMP.CH%, STAT%)

	CALL READ_DEVICE("UTL_WORK", UTL_WORK.DEV$, STAT%)

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.OPN"
	USE
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.OPN"
	USE
		FILENAME$ = "AP_VENDOR"
		CONTINUE HelpError
	END WHEN

320	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.OPN"
		GET #GL_PERIOD.CH%, RECORD 1%, REGARDLESS
		CLOSE GL_PERIOD.CH%
	USE
		FILENAME$ = "GL_PERIOD"
		CONTINUE HelpError
	END WHEN

330	WHEN ERROR IN
		OPEN UTL_WORK.DEV$ + "AP_TEMP.TMP" FOR OUTPUT AS FILE AP_TEMP.CH%, &
			ORGANIZATION INDEXED FIXED, &
			TEMPORARY, &
			BUFFER 32%, &
			MAP AP_TEMP, &
			PRIMARY KEY (AP_TEMP.TYPE, AP_TEMP.TRAN_CK) DUPLICATES, &
			ACCESS MODIFY, ALLOW NONE
	USE
		FILENAME$ = "AP_TEMP"
		CONTINUE HelpError
	END WHEN

340	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL.OPN"
		GET #AP_CONTROL.CH%, RECORD 1%, REGARDLESS
		CLOSE AP_CONTROL.CH%
	USE
		FILENAME$ = "AP_CONTROL"
		CONTINUE HelpError
	END WHEN

	IF AP_CONTROL::AP_ACCT = ""
	THEN
		CALL HELP_3MESSAGE(SCOPE, &
			"AP account not defined", "ERR", "AP_ACCT", &
			"ERROR_NOAPACCT")
		UTL_REPORTX::STAT = -1%
		GOTO ExitProgram
	END IF

	SELECT AP_CONTROL::CLOSEFLAG

	CASE "1"
		CALL HELP_3MESSAGE(SCOPE, "AP close in process", "ERR", &
			"AP_CLOSE", "ERROR_CLOSE")
		UTL_REPORTX::STAT = -1%
		GOTO ExitProgram

	CASE "2"
		CALL HELP_3MESSAGE(SCOPE, &
			"AP Reset in process", "ERR", "AP_RESET", &
			"ERROR_RESET")
		UTL_REPORTX::STAT = -1%
		GOTO ExitProgram

	CASE "3"
		CALL HELP_3MESSAGE(SCOPE, &
			"AP Purge in process", "ERR", "AP_PURGE", &
			"ERROR_PURGE")
		UTL_REPORTX::STAT = -1%
		GOTO ExitProgram

	END SELECT

	CUR_PERIOD% = AP_CONTROL::LASTPERCLOSE + 1%
	YEAR$ = AP_CONTROL::YEAR

	IF CUR_PERIOD% > GL_PERIOD::FPFY
	THEN
		CUR_PERIOD% = 1%
		YEAR$ = FORMAT$(VAL%(YEAR$) + 1%, "<0>#")
	END IF

	TEST_YYYY_PP$, YYYY_PP$ = YEAR$ + FORMAT$(CUR_PERIOD%, "<0>#")

	YYYY_PP$ = LEFT(YYYY_PP$, 4%) + "_" + RIGHT(YYYY_PP$, 5%)

	%PAGE

1000	!******************************************************************

	WHEN ERROR IN
		RESET #AP_OPEN.CH%, KEY #0%
	USE
		CONTINUE 2020
	END WHEN

	CALL ENTR_3MESSAGE(SCOPE, &
		"Creating work file.  Reading Open Item file", 1%)

	SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
		LOC(OUTP_XUNSOL) BY VALUE, LOC(SCOPE::SMG_KBID) BY VALUE)

	RRR_FLAG = 0%

2000	WHEN ERROR IN
		GET #AP_OPEN.CH%, REGARDLESS
	USE
		CONTINUE 2020 IF ERR = 11%
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

	SELECT RRR_FLAG

	CASE SMG$K_TRM_F11, SMG$K_TRM_CTRLW
		SMG_STATUS% = SMG$REPAINT_SCREEN(SCOPE::SMG_PBID)
		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)

	CASE SMG$K_TRM_HELP
		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)
		CALL HELP_3MESSAGE(SCOPE, "", SCOPE::PRG_IDENT, &
			SCOPE::PRG_PROGRAM, SCOPE::PRG_ITEM)
		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)

	CASE SMG$K_TRM_F6, SMG$K_TRM_F20
		SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

		CALL MENU_3INTERRUPT(SCOPE)

		SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
			LOC(OUTP_XUNSOL) BY VALUE, &
			LOC(SCOPE::SMG_KBID) BY VALUE)

	END SELECT

	RRR_FLAG = 0%

2010	IF LEFT(AP_OPEN::UPDATED, 6%) = TEST_YYYY_PP$
	THEN
		AP_TEMP.TYPE = "A"
		AP_TEMP.TRAN_CK = AP_OPEN::TRANKEY
		AP_TEMP.VENNUM = AP_OPEN::VENNUM
		AP_TEMP.INVNUM = AP_OPEN::INVNUM
		AP_TEMP.INVDAT = AP_OPEN::INVDAT
		AP_TEMP.TRAN_PO = AP_OPEN::PONUM
		AP_TEMP.DESC = AP_OPEN::CKDESC
		AP_TEMP.ACCT = AP_OPEN::AP_ACCT
		AP_TEMP.AMOUNT = (AP_OPEN::INVAMT - AP_OPEN::DISAMT) - &
			AP_OPEN::CKAMT

		WHEN ERROR IN
			PUT #AP_TEMP.CH% IF AP_OPEN::INVAMT <> 0.0
		USE
			FILENAME$ = "AP_TEMP"
			CONTINUE HelpError
		END WHEN

		AP_TEMP.TYPE = "B"
		AP_TEMP.TRAN_CK = AP_OPEN::CKNUM
		AP_TEMP.VENNUM = AP_OPEN::VENNUM
		AP_TEMP.INVNUM = AP_OPEN::INVNUM
		AP_TEMP.INVDAT = AP_OPEN::INVDAT
		AP_TEMP.TRAN_PO = AP_OPEN::TRANKEY
		AP_TEMP.DESC = AP_OPEN::CKDESC
		AP_TEMP.ACCT = AP_OPEN::CASH_ACCT
		AP_TEMP.AMOUNT = AP_OPEN::CKAMT

		WHEN ERROR IN
			PUT #AP_TEMP.CH% IF AP_OPEN::CKAMT <> 0.0
		USE
			FILENAME$ = "AP_TEMP"
			CONTINUE HelpError
		END WHEN
	END IF

	GOTO 2000

2020	SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

	%PAGE

 ReportTitle:
	TITLE$(1%) = "AP Transaction Report"
	TITLE$(2%) = "Accounting Period Ended " + &
		RIGHT(YYYY_PP$, 6%) + " " + LEFT(YYYY_PP$, 4%)
	TITLE$(3%) = ""

	TITLE$(4%) = "Trans# VendorNum  VendorName               " + &
		"InvNum          InvDate   PO #        Description   " + &
		"       AccountNum            NetAmount"
	TITLE$(5%) = ""

	LYT_LINE$ = "$CheckNum:006,$VendorNum:017,$VendorName:042," + &
		"$InvoiceNum:058,DInvDate:067,$TranKey:076,$Descr:101," + &
		"$AccountNum:116,VAmount:129"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	WHEN ERROR IN
		RESET #AP_TEMP.CH%, KEY #0%
	USE
		FILENAME$ = "AP_TEMP"
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
		GET #AP_TEMP.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "AP_TEMP"
		CONTINUE HelpError
	END WHEN

	!
	! Check status
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! If this is the first record with a type B, print out
	! the type A totals and continue.
	!
	IF AP_TEMP.TYPE = "B" AND TEST% = 0%
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		TEXT$ = SPACE$(110%) + "Total:" + &
			FORMAT$(TOTAL, "  ###,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		TITLE$(4%) = &
			"Chk #  Vendor #   Name                     Inv" + &
			"oice #         Date    Tran #     Description " + &
			"         Acct #               Net Amount"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 1000%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		TOTAL = 0.0
		TEST% = -1%
	END IF

	!
	! Get the AP Vendor description
	!
17200	WHEN ERROR IN
		GET #AP_VENDOR.CH%, KEY #0% EQ AP_TEMP.VENNUM, REGARDLESS
	USE
		AP_VENDOR::VENNAM = STRING$(LEN(AP_VENDOR::VENNAM), 63%)

		CONTINUE 17300 IF ERR = 155%
		FILENAME$ = "AP_VENDOR"
		CONTINUE HelpError
	END WHEN

17300	TEXT$ = AP_TEMP.TRAN_CK	+ " " + &
		AP_TEMP.VENNUM	+ " " + &
		LEFT(AP_VENDOR::VENNAM, 24%) + " " + &
		AP_TEMP.INVNUM	+ " " + &
		PRNT_DATE(AP_TEMP.INVDAT, 6%) + "  " + &
		CONV_STRING(AP_TEMP.TRAN_PO, CMC$_LEFT)	+ "  " + &
		AP_TEMP.DESC	+ " " + &
		AP_TEMP.ACCT	+ " " + &
		FORMAT$(AP_TEMP.AMOUNT, "#,###,###.##")

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TOTAL = TOTAL + AP_TEMP.AMOUNT

	!
	! Try for another record
	!
	GOTO GetNextRec

	%PAGE

	!******************************************************************
	! Handle totals and other items before EXITing
	!******************************************************************

 ExitTotal:
	!
	! Print out totals for type B
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TEXT$ = SPACE$(110%) + "Total:" + &
		FORMAT$(TOTAL, "  ###,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

 ExitProgram:
	!
	! Finish up the report
	!
	CALL OUTP_FINISH(UTL_REPORTX)

	!
	! Close channel
	!
	CLOSE AP_TEMP.CH%

17500	!
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
	! End of report AP_RPRT_APTRAN
	!******************************************************************
	END
