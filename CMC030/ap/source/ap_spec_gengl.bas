1	%TITLE "Accounts Payable Ledger Report"
	%SBTTL "AP_SPEC_GENGL"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 2004 BY
	!
	! Software Solutions, Inc.
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
	! Software Solutions, Inc.
	!
	! Software Solutions, Inc. assumes no responsibility for the use or
	! reliability of its software on equipment which is not supported
	! by Software Solutions, Inc.
	!
	!++
	! ID:APREG
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Print Ledger Report\* option
	!	prints the Accounts Payable
	!	Ledger in either vendor number or alphabetical order.
	!	.b
	!	The format of this report is identical to the Accounts Payable
	!	Subsidiary Ledger report. There is, however, no cut-off period
	!	consideration. All data in the Accounts Payable Open file is
	!	printed. It is, in effect, a dump of the file. Included in this report are
	!	the following fields:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	Transaction Number
	!	.le
	!	Invoice Number
	!	.le
	!	Invoice Date
	!	.le
	!	Invoice Amount
	!	.le
	!	Discount Date
	!	.le
	!	Due Date
	!	.le
	!	Check Number
	!	.le
	!	Check Date
	!	.le
	!	Check Amount
	!	.le
	!	Balance Due
	!	.els
	!
	! Index:
	!	.x Accounts Payable>Print Ledger
	!	.x Print>Accounts Payable Ledger
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_SPEC_GENGL/LINE
	!	$ LINK/EXECUTABLE=AP_EXE:*.EXE AP_SPEC_GENGL, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_SPEC_GENGL.OBJ;*
	!
	! Author:
	!
	!	02/11/2004 - Kevin Handy
	!
	! Modification history:
	!
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

	%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL.HB"
	MAP	(AP_CONTROL)	AP_CONTROL_CDD	AP_CONTROL

	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.HB"
	MAP	(AP_OPEN)	AP_OPEN_CDD	AP_OPEN

	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN_DIST.HB"
	MAP	(AP_OPEN_DIST)	AP_OPEN_DIST_CDD	AP_OPEN_DIST

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP	(AP_VENDOR)	AP_VENDOR_CDD	AP_VENDOR

	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	MAP	(GL_PERIOD)	GL_PERIOD_CDD	GL_PERIOD

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP	(GL_YYYY_PP)	GL_YYYY_PP_CDD	GL_YYYY_PP

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

	FROM_BATCH$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field determines the order
	!	in which the report will begin printing.  If the setting is left blank,
	!	the report will begin with the first vendor number in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Print Payables Ledger
	!	.x Print Payables Ledger>From Item
	!
	!--

	OTHER_GL$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field determines the vendor number
	!	with which the report will end printing.  If this setting is left blank,
	!	the report will end with the last vendor in the file.
	!	.lm -5
	!
	! Index:
	!	.x To>Item
	!	.x Item>To
	!	.x To Item>Print Payables Ledger
	!	.x Print Payables Ledger>To Item
	!
	!--

	GL_PERIOD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)
	YYYY_PP$ = LEFT(GL_PERIOD$, 4%) + "_" + &
		MID(GL_PERIOD$, 5%, 2%)

	!++
	! Abstract:FLD03
	!	^*(03) GL Period\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field determines the vendor number
	!	with which the report will end printing.  If this setting is left blank,
	!	the report will end with the last vendor in the file.
	!	.lm -5
	!
	! Index:
	!	.x To>Item
	!	.x Item>To
	!	.x To Item>Print Payables Ledger
	!	.x Print Payables Ledger>To Item
	!
	!--

	%PAGE

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.OPN"
	USE
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

305	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN_DIST.OPN"
	USE
		FILENAME$ = "AP_OPEN_DIST"
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
	! Open the Accounts Payable Vendor file
	!
320	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.MOD"
	USE
		FILENAME$ = "GL_YYYY_PP"
		CONTINUE HelpError
	END WHEN

	!
	! Open the Accounts Payable Controlling file
	!
330	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL.OPN"
		GET #AP_CONTROL.CH%, RECORD 1%, REGARDLESS
	USE
		FILENAME$ = "AP_CONTROL"
		CONTINUE HelpError
	END WHEN

	SELECT AP_CONTROL::CLOSEFLAG

	CASE "1"
		CALL HELP_3MESSAGE(SCOPE, "AP Close in process", &
			"ERR", "AP_CLOSE", "ERROR_CLOSE")
		GOTO ExitProgram

	CASE "2"
		CALL HELP_3MESSAGE(SCOPE, "AP Reset in process", &
			"ERR", "AP_RESET", "ERROR_RESET")
		GOTO ExitProgram

	CASE "3"
		CALL HELP_3MESSAGE(SCOPE, "AP Purge in process", &
			"ERR", "AP_PURGE", "ERROR_PURGE")
		GOTO ExitProgram

	END SELECT

	CUR_PERIOD% = AP_CONTROL::LASTPERCLOSE + 1%
	YEAR$ = AP_CONTROL::YEAR

	IF CUR_PERIOD% > GL_PERIOD::FPFY
	THEN
		CUR_PERIOD% = 1%
		YEAR$ = FORMAT$(VAL%(YEAR$) + 1%, "<0>###")
	END IF

	CUT_OFF_DATE$ = YEAR$ + FORMAT$(CUR_PERIOD%, "<0>#")

	%PAGE

 ReportTitle:
	!
	! Titles
	!
	TITLE$(1%) = "Accounts Payable Regen GL of " + FROM_BATCH$
	TITLE$(2%) = "GL Period " + GL_PERIOD$
	TITLE$(3%) = ""

	!
	! Headers
	!
	TITLE$(4%) = "Transaction   Account          Amount"
	TITLE$(5%) = ""

	!
	! Layouts for lines printed
	!
	LYT_LINE$ = ""

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	WHEN ERROR IN
		FIND #AP_OPEN.CH%, &
			KEY #2% EQ FROM_BATCH$, &
			REGARDLESS
	USE
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

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
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "AP_VENDOR"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO ExitTotal IF AP_OPEN::BATCH <> FROM_BATCH$

	WHEN ERROR IN
		GET #AP_VENDOR.CH%, &
			KEY #0% EQ AP_OPEN::VENNUM, &
			REGARDLESS
	USE
		AP_VENDOR::VENNAM = ""
		CONTINUE
	END WHEN

	!
	! Generate a GL record to pass through to the post function
	!
	NET = FUNC_ROUND(AP_OPEN::INVAMT - AP_OPEN::CKAMT, 2%)

	GL_YYYY_PP::ACCT	= AP_OPEN::AP_ACCT
	GL_YYYY_PP::SOURCE	= "PJ"
	GL_YYYY_PP::REFNO	= AP_OPEN::INVNUM
	GL_YYYY_PP::TRANDAT	= AP_OPEN::INVDAT
	GL_YYYY_PP::DESCR	= AP_VENDOR::VENNAM
	GL_YYYY_PP::AMOUNT	= -NET
	GL_YYYY_PP::XREFNO	= AP_OPEN::VENNUM
	GL_YYYY_PP::POSTIM	= TIME_NOW
	GL_YYYY_PP::POSDAT	= DATE_TODAY
	GL_YYYY_PP::CKNO	= AP_OPEN::CKNUM
	GL_YYYY_PP::TRANKEY	= AP_OPEN::TRANKEY
	GL_YYYY_PP::SUBACC	= EDIT$(AP_OPEN::USE_JOB_NUM, 4%)
	GL_YYYY_PP::OPERATION	= ""
	GL_YYYY_PP::UNITS	= 0.0
	GL_YYYY_PP::HOURS	= 0.0
	GL_YYYY_PP::UPDSTA	= ""
	GL_YYYY_PP::BTHNUM	= FROM_BATCH$

	PUT #GL_YYYY_PP.CH%

	!
	! Generate a GL record to pass through to the post function
	!
	IF (AP_OPEN::CKAMT <> 0.0)
	THEN
		GL_YYYY_PP::ACCT	= AP_CONTROL::CASH_ACCT
		GL_YYYY_PP::SOURCE	= "PJ"
		GL_YYYY_PP::REFNO	= AP_OPEN::INVNUM
		GL_YYYY_PP::TRANDAT	= AP_OPEN::INVDAT
		GL_YYYY_PP::DESCR	= AP_VENDOR::VENNAM
		GL_YYYY_PP::AMOUNT	= -AP_OPEN::CKAMT
		GL_YYYY_PP::XREFNO	= AP_OPEN::VENNUM
		GL_YYYY_PP::POSTIM	= TIME_NOW
		GL_YYYY_PP::POSDAT	= DATE_TODAY
		GL_YYYY_PP::CKNO	= AP_OPEN::CKNUM
		GL_YYYY_PP::TRANKEY	= AP_OPEN::TRANKEY
		GL_YYYY_PP::SUBACC	= EDIT$(AP_OPEN::USE_JOB_NUM, 4%)
		GL_YYYY_PP::OPERATION	= ""
		GL_YYYY_PP::UNITS	= 0.0
		GL_YYYY_PP::HOURS	= 0.0
		GL_YYYY_PP::UPDSTA	= ""
		GL_YYYY_PP::BTHNUM	= FROM_BATCH$

		PUT #GL_YYYY_PP.CH%

		NET = FUNC_ROUND(NET + AP_OPEN::CKAMT, 2%)
	END IF

17100	WHEN ERROR IN
		GET #AP_OPEN_DIST.CH%, KEY #1% EQ FROM_BATCH$
	USE
		CONTINUE 17200
	END WHEN

17110	GOTO 17200 IF AP_OPEN_DIST::BTHNUM <> FROM_BATCH$

	!
	! Generate a GL record to pass through to the post function
	!
	IF (AP_OPEN_DIST::TRANKEY = AP_OPEN::TRANKEY)
	THEN
		GL_YYYY_PP::ACCT	= AP_OPEN_DIST::ACCT
		GL_YYYY_PP::SOURCE	= "PJ"
		GL_YYYY_PP::REFNO	= AP_OPEN::INVNUM
		GL_YYYY_PP::TRANDAT	= AP_OPEN::INVDAT
		GL_YYYY_PP::DESCR	= AP_VENDOR::VENNAM
		GL_YYYY_PP::AMOUNT	= AP_OPEN_DIST::AMOUNT
		GL_YYYY_PP::XREFNO	= AP_OPEN::VENNUM
		GL_YYYY_PP::POSTIM	= TIME_NOW
		GL_YYYY_PP::POSDAT	= DATE_TODAY
		GL_YYYY_PP::CKNO	= AP_OPEN::CKNUM
		GL_YYYY_PP::TRANKEY	= AP_OPEN::TRANKEY
		GL_YYYY_PP::SUBACC	= EDIT$(AP_OPEN::USE_JOB_NUM, 4%)
		GL_YYYY_PP::OPERATION	= ""
		GL_YYYY_PP::UNITS	= 0.0
		GL_YYYY_PP::HOURS	= 0.0
		GL_YYYY_PP::UPDSTA	= ""
		GL_YYYY_PP::BTHNUM	= FROM_BATCH$

		PUT #GL_YYYY_PP.CH%

		NET = FUNC_ROUND(NET - AP_OPEN_DIST::AMOUNT, 2%)
	END IF

17190	GET #AP_OPEN_DIST.CH%

	GOTO 17110

17200	!
	! Generate a GL record to pass through to the post function
	!
	IF (NET <> 0.0)
	THEN
		GL_YYYY_PP::ACCT	= OTHER_GL$
		GL_YYYY_PP::SOURCE	= "PJ"
		GL_YYYY_PP::REFNO	= AP_OPEN::INVNUM
		GL_YYYY_PP::TRANDAT	= AP_OPEN::INVDAT
		GL_YYYY_PP::DESCR	= AP_VENDOR::VENNAM
		GL_YYYY_PP::AMOUNT	= NET
		GL_YYYY_PP::XREFNO	= AP_OPEN::VENNUM
		GL_YYYY_PP::POSTIM	= TIME_NOW
		GL_YYYY_PP::POSDAT	= DATE_TODAY
		GL_YYYY_PP::CKNO	= AP_OPEN::CKNUM
		GL_YYYY_PP::TRANKEY	= AP_OPEN::TRANKEY
		GL_YYYY_PP::SUBACC	= EDIT$(AP_OPEN::USE_JOB_NUM, 4%)
		GL_YYYY_PP::OPERATION	= ""
		GL_YYYY_PP::UNITS	= 0.0
		GL_YYYY_PP::HOURS	= 0.0
		GL_YYYY_PP::UPDSTA	= ""
		GL_YYYY_PP::BTHNUM	= FROM_BATCH$

		PUT #GL_YYYY_PP.CH%
	END IF

	!
	! Try for next record
	!
	GOTO GetNextRec

	%PAGE

 ExitTotal:

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
