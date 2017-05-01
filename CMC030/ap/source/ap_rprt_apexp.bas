1	%TITLE "Accounts Payable Expense Accrual by Period"
	%SBTTL "AP_RPRT_APEXP"
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
	! ID:APEXP
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Print Expenses Distribution\* option
	!	prints a report which lists, in General Ledger account number order,
	!	all items in the Accounts Payable Open file. A subtotal is printed
	!	for each account number listed, as well as a grand total for the
	!	report. This report contains the following:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	Account Number
	!	.le
	!	Description
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
	!	Unit
	!	.le
	!	Amount
	!	.els
	!
	! Index:
	!	.x Accounts Payable>Reports>Expenses Distribution
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_RPRT_APEXP/LINE
	!	$ LINK/EXECUTABLE=AP_EXE:*.EXE AP_RPRT_APEXP, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_RPRT_APEXP.OBJ;*
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
	!	06/05/91 - Kevin Handy
	!		Unwound error trapping.
	!
	!	06/14/93 - Kevin Handy
	!		Added several REDARDLESS.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	05/12/95 - Kevin Handy
	!		Open AP_CONTROL as ".OPN" instead of ".MOD".
	!
	!	12/14/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!		Change RIGHT(NUM1$()) to FORMAT$().
	!
	!	07/02/96 - Kevin Handy
	!		Reformat source code,
	!
	!	10/06/96 - Kevin Handy
	!		Clean up.
	!
	!	05/20/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/22/97 - Kevin Handy
	!		Use 'val%' instead of 'val'
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/16/99 - Kevin Handy
	!		Fix calles for unsolicited input
	!
	!	01/20/2000 - Kevin Handy
	!		Use "WHEN ERROR IN"
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

	!******************************************************************
	! Set up data storage areas (MAPs, DIMENSIONs, DECLAREs, etc.)
	!******************************************************************

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL.HB"
	MAP	(AP_CONTROL)	AP_CONTROL_CDD		AP_CONTROL

	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.HB"
	MAP	(AP_OPEN)	AP_OPEN_CDD		AP_OPEN

	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN_DIST.HB"
	MAP	(AP_OPEN_DIST)	AP_OPEN_DIST_CDD	AP_OPEN_DIST

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP	(AP_VENDOR)	AP_VENDOR_CDD		AP_VENDOR

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP	(GL_CHART)	GL_CHART_CDD		GL_CHART

	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	MAP	(GL_PERIOD)	GL_PERIOD_CDD		GL_PERIOD

	MAP (DP_OUTP_XUNSOL) &
		LONG	RRR_FLAG

	MAP (AP_TEMP) &
		STRING	AP_TEMP.ACCT = 18%, &
		STRING	AP_TEMP.VENNUM = 10%, &
		STRING	AP_TEMP.TRANKEY = 6%, &
		STRING	AP_TEMP.INVNUM = 15%, &
		STRING	AP_TEMP.INVDAT = 8%, &
		STRING	AP_TEMP.PONUM = 10%, &
		STRING	AP_TEMP.PO_LINE = 4%, &
		STRING	AP_TEMP.SUBACC = 10%, &
		STRING	AP_TEMP.OPERATION = 6%, &
		REAL	AP_TEMP.UNITS, &
		REAL	AP_TEMP.AMOUNT, &
		REAL	AP_TEMP.DISCAMT

	%PAGE

	!******************************************************************
	! Take care of anything else before beginning report
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

	TEST_ACCT$ = ""
	FIRST_RECORD%, RECORD_COUNT% = 0%

	%PAGE

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.OPN"
	USE
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN_DIST.OPN"
	USE
		FILENAME$ = "AP_OPEN_DIST"
		CONTINUE HelpError
	END WHEN

320	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.OPN"
	USE
		FILENAME$ = "AP_VENDOR"
		CONTINUE HelpError
	END WHEN

330	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.OPN"
	USE
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

340	CALL ASSG_CHANNEL(AP_TEMP.CH%, STAT%)
	CALL READ_DEVICE("UTL_WORK", UTL_WORK.DEV$, STAT%)

	WHEN ERROR IN
		OPEN UTL_WORK.DEV$ + "AP_TEMP.TMP" FOR OUTPUT AS FILE AP_TEMP.CH%, &
			ORGANIZATION INDEXED FIXED, &
			TEMPORARY, &
			BUFFER 32%, &
			MAP AP_TEMP, &
			PRIMARY KEY (AP_TEMP.ACCT, AP_TEMP.VENNUM) DUPLICATES, &
			ACCESS MODIFY, &
			ALLOW NONE
	USE
		FILENAME$ = "AP_TEMP"
		CONTINUE HelpError
	END WHEN

350	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.OPN"
		GET #GL_PERIOD.CH%, RECORD 1%, REGARDLESS
		CLOSE GL_PERIOD.CH%
	USE
		FILENAME$ = "GL_PERIOD"
		CONTINUE HelpError
	END WHEN

360	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL.OPN"
		GET #AP_CONTROL.CH%, RECORD 1%, REGARDLESS
	USE
		FILENAME$ = "AP_CONTROL"
		CONTINUE HelpError
	END WHEN

	SELECT AP_CONTROL::CLOSEFLAG

	CASE "1"
		CALL HELP_3MESSAGE(SCOPE, &
			"AP Close in process", "ERR", "AP_CLOSE", &
			"ERROR_CLOSE")
		GOTO ExitProgram

	CASE "2"
		CALL HELP_3MESSAGE(SCOPE, &
			"AP Reset in process", "ERR", "AP_RESET", &
			"ERROR_RESET")
		GOTO ExitProgram

	CASE "3"
		CALL HELP_3MESSAGE(SCOPE, &
			"AP Purge in process", "ERR", "AP_PURGE", &
			"ERROR_PURGE")
		GOTO ExitProgram

	END SELECT

	CUR_PERIOD% = AP_CONTROL::LASTPERCLOSE + 1%
	YEAR$ = AP_CONTROL::YEAR

	IF CUR_PERIOD% > GL_PERIOD::FPFY
	THEN
		CUR_PERIOD% = 1%
		YEAR$ = FORMAT$(VAL%(YEAR$) + 1%, "<0>###")
	END IF

	TEST_YYYY_PP$, YYYY_PP$ = YEAR$ + FORMAT$(CUR_PERIOD%, "<0>#")

	YYYY_PP$ = LEFT(YYYY_PP$, 4%) + "_" + RIGHT(YYYY_PP$, 5%)

	%PAGE

1000	WHEN ERROR IN
		RESET #AP_OPEN.CH%, KEY #0%
	USE
		CONTINUE 2100
	END WHEN

	CALL ENTR_3MESSAGE(SCOPE, &
		"Creating work file.  Reading Open Item file", 1%)

	SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
		LOC(OUTP_XUNSOL) BY VALUE, LOC(SCOPE::SMG_KBID) BY VALUE)

	RRR_FLAG = 0%

2000	WHEN ERROR IN
		GET #AP_OPEN.CH%, REGARDLESS
	USE
		CONTINUE 2100 IF ERR = 11%
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

	SELECT RRR_FLAG

	CASE SMG$K_TRM_F11, SMG$K_TRM_CTRLW
		SMG_STATUS% = SMG$REPAINT_SCREEN(SCOPE::SMG_PBID)
		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)

	CASE SMG$K_TRM_HELP
		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)
		CALL HELP_34MESSAGE(SCOPE, "", SCOPE::PRG_IDENT, &
			SCOPE::PRG_PROGRAM, "", SCOPE::PRG_ITEM)
		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)

	CASE SMG$K_TRM_F6, SMG$K_TRM_F20
		SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

		CALL MENU_3INTERRUPT(SCOPE)

		SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
			LOC(OUTP_XUNSOL) BY VALUE, &
			LOC(SCOPE::SMG_KBID) BY VALUE)

	END SELECT

	RRR_FLAG = 0%

	GOTO 2030 IF LEFT(AP_OPEN::UPDATED, 6%) <> TEST_YYYY_PP$

2010	WHEN ERROR IN
		FIND #AP_OPEN_DIST.CH%, KEY #0% EQ AP_OPEN::TRANKEY, REGARDLESS
	USE
		CONTINUE 2030 IF ERR = 155%
		FILENAME$ = "AP_OPEN_DIST"
		CONTINUE HelpError
	END WHEN

2020	WHEN ERROR IN
		GET #AP_OPEN_DIST.CH%, REGARDLESS
	USE
		CONTINUE 2030 IF ERR = 11%
		FILENAME$ = "AP_OPEN_DIST"
		CONTINUE HelpError
	END WHEN

	GOTO 2030 IF AP_OPEN::TRANKEY <> AP_OPEN_DIST::TRANKEY

	AP_TEMP.ACCT = AP_OPEN_DIST::ACCT
	AP_TEMP.VENNUM = AP_OPEN::VENNUM
	AP_TEMP.TRANKEY = AP_OPEN::TRANKEY
	AP_TEMP.INVNUM = AP_OPEN::INVNUM
	AP_TEMP.INVDAT = AP_OPEN::INVDAT
	AP_TEMP.PONUM = AP_OPEN_DIST::PONUM
	AP_TEMP.PO_LINE = AP_OPEN_DIST::PO_LINE
	AP_TEMP.SUBACC = AP_OPEN_DIST::SUBACC
	AP_TEMP.OPERATION = AP_OPEN_DIST::OPERATION
	AP_TEMP.UNITS = AP_OPEN_DIST::UNITS
	AP_TEMP.AMOUNT = AP_OPEN_DIST::AMOUNT
	AP_TEMP.DISCAMT = AP_OPEN_DIST::DISCAMT

	PUT #AP_TEMP.CH%

	GOTO 2020

2030	GOTO 2000

2100	SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

	%PAGE

 ReportTitle:
	TITLE$(1%) = "Expense Distribution"
	TITLE$(2%) = "Accounting Period Ended " + RIGHT(YYYY_PP$, 6%) + &
		" " + LEFT(YYYY_PP$, 4%)
	TITLE$(3%) = ""

	TITLE$(4%) = "AccountNum         Description             " + &
		"VendorNum  VendorName              Trans# InvNum   " + &
		"     InvDate         Unit      Amount"
	TITLE$(5%) = ""

	LYT_LINE$ = "$Account:018,$Descr:042,$VendorNum:053," + &
		"$VendorName:077,$TranKey:084,$InvoiceNum:098," + &
		"DInvDate:107,VUnits:119,VAmount:131"

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

	GOTO 17400 IF AP_TEMP.ACCT = TEST_ACCT$ AND FIRST_RECORD%

	!
	! Get the GL Account Description
	!
	GL_CHART::DESCR = STRING$(LEN(GL_CHART::DESCR), 63%)

17200	WHEN ERROR IN
		GET #GL_CHART.CH%, KEY #0% EQ AP_TEMP.ACCT, REGARDLESS
	USE
		CONTINUE 17300 IF ERR = 155%
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

17300	IF FIRST_RECORD%
	THEN
		GOSUB AcctTotal IF RECORD_COUNT% > 1%

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	ELSE
		FIRST_RECORD% = -1%
	END IF

	RECORD_COUNT% = 0%

	ACCT_UNITS, ACCT_AMOUNT = 0.0

	!
	! Get the AP Vendor description
	!
	AP_VENDOR::VENNAM = STRING$(LEN(AP_VENDOR::VENNAM), 63%)

17400	WHEN ERROR IN
		GET #AP_VENDOR.CH%, KEY #0% EQ AP_TEMP.VENNUM, REGARDLESS
	USE
		CONTINUE 17500 IF ERR = 155%
		FILENAME$ = "AP_VENDOR"
		CONTINUE HelpError
	END WHEN

17500	TEXT$ = AP_TEMP.ACCT + " " + &
		LEFT(GL_CHART::DESCR, 23%) + " " + &
		AP_TEMP.VENNUM + " " + &
		LEFT(AP_VENDOR::VENNAM, 23%) + " " + &
		AP_TEMP.TRANKEY + " " + &
		LEFT(AP_TEMP.INVNUM, 13%) + " " + &
		PRNT_DATE(AP_TEMP.INVDAT, 6%) + " " + &
		FORMAT$(AP_TEMP.UNITS, "########.## ") + &
		FORMAT$(AP_TEMP.AMOUNT - AP_TEMP.DISCAMT, "########.##")

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	ACCT_UNITS = ACCT_UNITS + AP_TEMP.UNITS
	TOTAL_UNITS = TOTAL_UNITS + AP_TEMP.UNITS

	ACCT_AMOUNT = ACCT_AMOUNT + (AP_TEMP.AMOUNT - AP_TEMP.DISCAMT)
	TOTAL_AMOUNT = TOTAL_AMOUNT + (AP_TEMP.AMOUNT - AP_TEMP.DISCAMT)

	TEST_ACCT$ = AP_TEMP.ACCT

	RECORD_COUNT% = RECORD_COUNT% + 1%

	!
	! Try for next record
	!
	GOTO GetNextRec

	%PAGE

	!******************************************************************
	! Handle end of report
	!******************************************************************

 ExitTotal:
	!
	! Print out totals
	!
	GOSUB AcctTotal IF RECORD_COUNT% > 1%

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TEXT$ = SPACE$(54%) + "Grand Total" + SPACE$(42%) + &
		FORMAT$(TOTAL_UNITS, " ########.##") + &
		FORMAT$(TOTAL_AMOUNT, " ########.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

 ExitProgram:
	!
	! Finish up report
	!
	CALL OUTP_FINISH(UTL_REPORTX)

	!
	! Close channel
	!
	CLOSE AP_TEMP.CH%

17700	!
	! Exit to next program or menu
	!
	IF TRM$(UTL_REPORTX::NEXTRUN) = ""
	THEN
		CALL SUBR_3EXITPROGRAM(SCOPE, "", "")
	ELSE
		CALL SUBR_3EXITPROGRAM(SCOPE, "RUN " + UTL_REPORTX::NEXTRUN, "")
	END IF

	%PAGE

 AcctTotal:
	!******************************************************************
	! Subroutine to print the account total
	!******************************************************************
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	TEXT$ = SPACE$(54%) + "Account Total" + SPACE$(40%) + &
		FORMAT$(ACCT_UNITS, " ########.##") + &
		FORMAT$(ACCT_AMOUNT, " ########.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

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
	! End of report AP_RPRT_APEXP
	!******************************************************************
	END
