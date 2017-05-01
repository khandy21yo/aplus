1	%TITLE "Accounts Payable Ledger Report"
	%SBTTL "AP_RPRT_REG"
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
	!	$ BAS AP_SOURCE:AP_RPRT_REG/LINE
	!	$ LINK/EXECUTABLE=AP_EXE:*.EXE AP_RPRT_REG, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_RPRT_REG.OBJ;*
	!
	! Author:
	!
	!	07/31/87 - B. Craig Larsen
	!
	! Modification history:
	!
	!	11/15/87 - Robert Peterson
	!		Changed the process to automatically calculate
	!		ledger date.
	!
	!	08/20/88 - Kevin Handy
	!		Fixed numerous bugs (no vendor names, random
	!		totals, etc), cleaned out dozens of flags
	!		(multiple flags doing the same things), and
	!		various clean-ups.  (I can't have made it
	!		any worse than it was)
	!
	!	09/09/88 - Kevin Handy
	!		Modified to display both sides (invamt - chkamt)
	!		pushed up so that it looks better.
	!
	!	06/05/91 - Kevin Handy
	!		Unwound error trapping.
	!
	!	06/07/91 - Frank F. Starman
	!		Correct the sort options. Add BATCH field.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	05/12/95 - Kevin Handy
	!		Open AP_CONTROL as .OPN instead of .MOD.
	!
	!	03/19/96 - Kevin Handy
	!		Reformat source code.
	!
	!	03/19/96 - Kevin Handy
	!		Add ability wo select AP account.
	!
	!	08/27/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/21/97 - Kevin Handy
	!		Use intger for #key
	!
	!	08/20/97 - Kevin Handy
	!		Don't need to assign channel for report
	!
	!	08/22/97 - Kevin Handy
	!		Use 'val%' instead of 'val'
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/18/99 - Kevin Handy
	!		Added grand totals by AP Account
	!
	!	04/28/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL.HB"
	MAP	(AP_CONTROL)	AP_CONTROL_CDD	AP_CONTROL

	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.HB"
	MAP	(AP_OPEN)	AP_OPEN_CDD	AP_OPEN

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP	(AP_VENDOR)	AP_VENDOR_CDD	AP_VENDOR

	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	MAP	(GL_PERIOD)	GL_PERIOD_CDD	GL_PERIOD

	!
	! Dimension arrays
	!
	DIM STRING TEXT_LEFT(100%), TEXT_RIGHT(100%)

	!
	! Array to hold totals
	!
	RECORD TOTAL_RECORD
		STRING ACCT = 18
		DOUBLE INV_AMT
		DOUBLE DIS_AMT
		DOUBLE NET_AMT
		DOUBLE CHK_AMT
	END RECORD

	DECLARE INTEGER CONSTANT MAX_TOTAL = 1000

	DIM TOTAL_RECORD TOTAL(MAX_TOTAL)

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

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

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

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Sort Order (NU,NA,AL)\*
	!	.b
	!	.lm +5
	!	The ^*Sort Order\* field determines the
	!	order in which the report is to print.
	!	.b
	!	Valid entries are:
	!	.b
	!	.lm 15
	!	.list "*"
	!	.le
	!	^*NU\*#=#Vendor Number order
	!	.le
	!	^*NA\*#=#Vendor Name order
	!	.le
	!	^*AL\*#=#Alphabetical order
	!	.els
	!	.lm -5
	!	There are no other valid values for this field.
	!	.lm -5
	!
	! Index:
	!	.x Sort Order>Accounts Payable Ledger
	!	.x Accounts Payable Ledger>Sort Order
	!
	!--

	PRINT_DUE_ONLY$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Open Invoice Only\*
	!	.b
	!	.lm +5
	!	The ^*Open Invoice Only\* field determines if
	!	paid and/or unpaid invoices should be printed on the report.
	!	If only unpaid invoices are to be printed, change the
	!	field to a ^*Y\*. If all records in the file are to be printed,
	!	the field should be blank or ^*N\*.
	!	.lm -5
	!
	! Index:
	!	.x Open Invoice Only>Accounts Payable Register
	!	.x Accounts Payable Register>Open Invoice Only
	!
	!--

	CUT_OFF_REG$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!
	! Datatype:TEXT
	! Size:1
	! Valid Input: YN
	!--

	BATCH$ = EDIT$(UTL_REPORTX::OPTDEF(5%), -1%)

	!++
	! Abstract:FLD06
	!	^*(06) Batch\*
	!	.b
	!	.lm +5
	!	The ^*Batch\* field enters the number of the batch which
	!	will be printed.
	!	.b
	!	Only one batch at a time may be printed.
	!	.lm -5
	!
	! Index:
	!	.x Batch
	!
	!--

	WILD_ACCT$ = TRM$(UTL_REPORTX::OPTDEF(6%))

	!++
	! Abstract:FLD07
	!	^*(07) Wildcard AP Account\*
	!	.b
	!	.lm +5
	!	Specifies which AP accounts should be printed.
	!	.b
	!	Leaving this field blank will cause all AP accounts
	!	to be printed.
	!	.lm -5
	!
	! Index:
	!	.x AP Account>Register
	!	.x Register>AP Account
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
	! Open the Accounts Payable Vendor file
	!
310	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.OPN"
	USE
		FILENAME$ = "AP_VENDOR"
		CONTINUE HelpError
	END WHEN

	!
	! We don't need to open any more files if this is a Cutoff Ledger
	!
	GOTO ReportTitle &
		IF CUT_OFF_REG$ <> "Y"

	!
	! Open the General Ledger Controlling file (GL_PERIOD.CTR)
	!
320	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.OPN"

		GET #GL_PERIOD.CH%, RECORD 1%, REGARDLESS
		CLOSE GL_PERIOD.CH%
	USE
		FILENAME$ = "GL_PERIOD"
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
	TITLE$(1%) = "Accounts Payable Ledger" + TEXT_BATCH$

	ADD_ONE% = 0%
	IF CUT_OFF_REG$ = "Y"
	THEN
		TITLE$(2%) = "For Accounting Period Ended " + CUT_OFF_DATE$
		ADD_ONE% = 1%
	END IF

	TITLE$(2% + ADD_ONE%) = ""

	!
	! Headers
	!
	TITLE$(3% + ADD_ONE%) = "Trans#  InvoiceNum       InvDate        " + &
		"InvAmt    DiscAmt       NetAmt DiscDate DueDate  " + &
		"CkNum      CkDate        CkAmt   BalanceDue"
	TITLE$(4% + ADD_ONE%) = "."

	!
	! Layouts for lines printed
	!
	LYT_LINE$ = "$TRANS:8,$INVOICE:25,DINVDATE:33,VINVAMT:47," + &
		"VDISAMT:58,VNETAMT:71,DDISDAT:80,DDUEDAT:89," + &
		"$CKNUM:98,DCKDAT:107,VCKAMT:121,VBALANCE:132"

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
17010	!******************************************************************
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
		GOTO ExitTotal &
			IF (AP_VENDOR::VENNUM > TO_ITEM$) AND &
			(TO_ITEM$ <> "")

	CASE "NA"
		GOTO ExitTotal &
			IF (AP_VENDOR::VENNAM > TO_ITEM$) AND &
			(TO_ITEM$ <> "")

	CASE ELSE
		GOTO ExitTotal &
			IF (AP_VENDOR::ALPSRT > TO_ITEM$) AND &
			(TO_ITEM$ <> "")

	END SELECT

	!
	! Get ready to start a sub-loop
	!
17030	WHEN ERROR IN
		FIND #AP_OPEN.CH%, KEY #0% EQ AP_VENDOR::VENNUM, REGARDLESS
	USE
		CONTINUE GetNextRec IF ERR = 155%
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

	LC_LEFT% = 0%
	LC_RIGHT% = 0%
	VENDOR_NAME_TEST% = -1%

	INV_AMT, DIS_AMT, NET_AMT, CHK_AMT, BAL_DUE, &
		VEN_INV_AMT, VEN_DIS_AMT, VEN_NET_AMT, VEN_CHK_AMT = 0.0

	TRANKEY$ = AP_OPEN::TRANKEY + ""

	!
	! Get the (next) record
	!
17040	WHEN ERROR IN
		GET #AP_OPEN.CH%, REGARDLESS
	USE
		CONTINUE 17900 IF ERR = 11%
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

	GOTO 17900 &
		IF (AP_VENDOR::VENNUM <> AP_OPEN::VENNUM)

	GOTO 17040 &
		IF BATCH$ <> AP_OPEN::BATCH AND BATCH$ <> ""

	GOTO 17040 &
		IF (CUT_OFF_DATE$ < LEFT(AP_OPEN::UPDATED, 6%)) AND &
			(CUT_OFF_REG$ = "Y")

	IF WILD_ACCT$ <> ""
	THEN
		GOTO 17040 IF COMP_STRING(AP_OPEN::AP_ACCT, WILD_ACCT$) = 0%
	END IF

	IF AP_OPEN::TRANKEY <> TRANKEY$
	THEN
		GOSUB VendorLine
		GOTO ExitProgram IF UTL_REPORTX::STAT

		TRANKEY$ = AP_OPEN::TRANKEY + ""
	END IF

	AP_ACCT$ = AP_OPEN::AP_ACCT

17080	!
	! Skip if nothing being invoiced
	!
	GOTO 17090 &
		IF (AP_OPEN::INVAMT = 0.0) AND (AP_OPEN::DISAMT = 0.0) AND &
			(LC_LEFT% >= 1%)

	LC_LEFT% = LC_LEFT% + 1%

	IF (LC_LEFT% = 1%)
	THEN
		TEXT_LEFT(LC_LEFT%) = AP_OPEN::TRANKEY + "  " + &
			AP_OPEN::INVNUM + "  " + &
			PRNT_DATE(AP_OPEN::INVDAT, 0%) + " "
	ELSE
		TEXT_LEFT(LC_LEFT%) = SPACE$(34%)
	END IF

	NET = AP_OPEN::INVAMT - AP_OPEN::DISAMT

	TEXT_LEFT(LC_LEFT%) = TEXT_LEFT(LC_LEFT%) + &
		FORMAT$(AP_OPEN::INVAMT, "<%>########.## ") + &
		FORMAT$(AP_OPEN::DISAMT, "<%>######.## ") + &
		FORMAT$(NET, "<%>########.## ")

	IF (LC_LEFT% = 1%)
	THEN
		TEXT_LEFT(LC_LEFT%) = TEXT_LEFT(LC_LEFT%) + &
			PRNT_DATE(AP_OPEN::DISCDAT, 0%) + " " + &
			PRNT_DATE(AP_OPEN::DUEDAT, 0%) + " "
	END IF

17090	IF (AP_OPEN::CKAMT <> 0.0) OR (AP_OPEN::CKNUM <> "")
	THEN
		LC_RIGHT% = LC_RIGHT% + 1%

		TEXT_RIGHT(LC_RIGHT%) = &
			AP_OPEN::CKNUM + "   " + &
			PRNT_DATE(AP_OPEN::CKDAT, 0%) + &
			FORMAT$(AP_OPEN::CKAMT, " #########.## ")
	END IF

	INV_AMT = INV_AMT + AP_OPEN::INVAMT
	DIS_AMT = DIS_AMT + AP_OPEN::DISAMT
	NET_AMT = NET_AMT + AP_OPEN::INVAMT - AP_OPEN::DISAMT
	CHK_AMT = CHK_AMT + AP_OPEN::CKAMT
	BAL_DUE = BAL_DUE + (AP_OPEN::INVAMT - AP_OPEN::DISAMT) - AP_OPEN::CKAMT

	!
	! End of sub-loop
	!
	GOTO 17040

17900	GOSUB VendorLine
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOSUB VendorTotal IF (VENDOR_NAME_TEST% = 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Try for next record
	!
	GOTO GetNextRec

	%PAGE

	!******************************************************************
	! Hendle totals and other items before EXITing
	!******************************************************************

 ExitTotal:
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
		"*** Account Totals **", 5%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FOR LOOP% = 1% TO TOTAL_ACCT%

		TEXT$ = "           " + TOTAL(LOOP%)::ACCT + "    " + &
			FORMAT$(TOTAL(LOOP%)::INV_AMT, " #########.##") + &
			FORMAT$(TOTAL(LOOP%)::DIS_AMT, " #######.##") + &
			FORMAT$(TOTAL(LOOP%)::NET_AMT, " #########.##") + &
			SPACE$(36%) + &
			FORMAT$(TOTAL(LOOP%)::CHK_AMT, " #########.##") + &
			FORMAT$(TOTAL(LOOP%)::NET_AMT - &
				TOTAL(LOOP%)::CHK_AMT, " #########.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

	NEXT LOOP%

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	!
	! Handle end of report
	!
	TEXT$ = "          Grand Total" + SPACE$(12%) + &
		FORMAT$(TOT_INV_AMT, " #########.##") + &
		FORMAT$(TOT_DIS_AMT, " #######.##") + &
		FORMAT$(TOT_NET_AMT, " #########.##") + &
		SPACE$(36%) + &
		FORMAT$(TOT_CHK_AMT, " #########.##") + &
		FORMAT$(TOT_NET_AMT - TOT_CHK_AMT, " #########.##")

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

 VendorLine:
	!******************************************************************
	! Subroutine to print vendor register detail lines
	!******************************************************************

	RETURN IF (LC_LEFT% = 0%) AND (LC_RIGHT% = 0%)

	IF LC_LEFT% > LC_RIGHT%
	THEN
		MAX_LC% = LC_LEFT%
	ELSE
		MAX_LC% = LC_RIGHT%
	END IF

	IF (PRINT_DUE_ONLY$ = "Y") AND &
		(FUNC_ROUND(BAL_DUE, 2%) <> 0.0) OR &
		(PRINT_DUE_ONLY$ <> "Y")
	THEN
		IF (VENDOR_NAME_TEST%)
		THEN
			TEXT$ = AP_VENDOR::VENNUM + " " + AP_VENDOR::VENNAM
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 5%)

			VENDOR_NAME_TEST% = 0%
		END IF

		FOR LOOP% = 1% TO MAX_LC%
			TEXT$ = ""
			TEXT$ = TEXT_LEFT(LOOP%) IF LOOP% <= LC_LEFT%
			TEXT$ = TEXT$ + SPACE$(89% - LEN(TEXT$)) + &
				TEXT_RIGHT(LOOP%) &
				IF LOOP% <= LC_RIGHT%
			TEXT$ = TEXT$ + SPACE$(120% - LEN(TEXT$)) + &
				FORMAT$(BAL_DUE, "#########.##") &
				IF LOOP% = MAX_LC%

			CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), &
				TEXT$, 0%)
		NEXT LOOP%

		VEN_INV_AMT = VEN_INV_AMT + INV_AMT
		VEN_DIS_AMT = VEN_DIS_AMT + DIS_AMT
		VEN_NET_AMT = VEN_NET_AMT + NET_AMT
		VEN_CHK_AMT = VEN_CHK_AMT + CHK_AMT

		!
		! Summarize for totals
		!
		FOR I% = 1% TO TOTAL_ACCT%

			GOTO L18110 IF TOTAL(I%)::ACCT = AP_ACCT$

			IF TOTAL(I%)::ACCT > AP_ACCT$
			THEN
				TOTAL(J% + 1%) = TOTAL(J%) &
					FOR J% = TOTAL_ACCT% TO I% STEP -1%
				TOTAL_ACCT% = TOTAL_ACCT% + 1%
				TOTAL(I%)::ACCT = AP_ACCT$
				TOTAL(I%)::INV_AMT = 0.0
				TOTAL(I%)::DIS_AMT = 0.0
				TOTAL(I%)::NET_AMT = 0.0
				TOTAL(I%)::CHK_AMT = 0.0

				GOTO L18110
			END IF

		NEXT I%

		TOTAL_ACCT%, I% = TOTAL_ACCT% + 1%
		TOTAL(I%)::ACCT = AP_ACCT$
		TOTAL(I%)::INV_AMT = 0.0
		TOTAL(I%)::DIS_AMT = 0.0
		TOTAL(I%)::NET_AMT = 0.0
		TOTAL(I%)::CHK_AMT = 0.0

 L18110:	TOTAL(I%)::INV_AMT = TOTAL(I%)::INV_AMT + INV_AMT
		TOTAL(I%)::DIS_AMT = TOTAL(I%)::DIS_AMT + DIS_AMT
		TOTAL(I%)::NET_AMT = TOTAL(I%)::NET_AMT + NET_AMT
		TOTAL(I%)::CHK_AMT = TOTAL(I%)::CHK_AMT + CHK_AMT

	END IF

 ComeBack1:
	INV_AMT, DIS_AMT, NET_AMT, CHK_AMT, BAL_DUE = 0.0
	LC_LEFT% = 0%
	LC_RIGHT% = 0%

	RETURN

	%PAGE

 VendorTotal:
	!******************************************************************
	! Subroutine to print Vendor total
	!******************************************************************
	TOT_INV_AMT = TOT_INV_AMT + VEN_INV_AMT
	TOT_DIS_AMT = TOT_DIS_AMT + VEN_DIS_AMT
	TOT_NET_AMT = TOT_NET_AMT + VEN_NET_AMT
	TOT_CHK_AMT = TOT_CHK_AMT + VEN_CHK_AMT

	TEXT$ = "     Vendor Total" + SPACE$(16%) + &
		FORMAT$(VEN_INV_AMT, " #########.##") + &
		FORMAT$(VEN_DIS_AMT, " #######.##") + &
		FORMAT$(VEN_NET_AMT, " #########.##") + &
		SPACE$(36%) + &
		FORMAT$(VEN_CHK_AMT, " #########.##") + &
		FORMAT$(VEN_NET_AMT - VEN_CHK_AMT, " #########.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

 ComeBack2:
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
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
