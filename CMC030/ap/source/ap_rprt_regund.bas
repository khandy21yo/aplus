1	%TITLE "Accounts Payable Register Report"
	%SBTTL "AP_RPRT_REGUND"
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
	! ID:APREGU
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
	!	$ BAS AP_SOURCE:AP_RPRT_REGUND/LINE
	!	$ LINK/EXECUTABLE=AP_EXE:*.EXE AP_RPRT_REGUND, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_RPRT_REGUND.OBJ;*
	!
	! Author:
	!
	!	07/28/89 - Kevin Handy
	!
	! Modification history:
	!
	!	06/05/91 - Kevin Handy
	!		Unwound error trapping.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/29/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/13/97 - Kevin Handy
	!		Reformat source code.
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

	!******************************************************************
	! Set up data storage areas (MAPs, DIMENSIONs, DECLAREs, etc.)
	!******************************************************************

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL.HB"
	MAP (AP_CONTROL)	AP_CONTROL_CDD		AP_CONTROL

	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.HB"
	MAP (AP_OPEN)		AP_OPEN_CDD		AP_OPEN

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP (AP_VENDOR)		AP_VENDOR_CDD		AP_VENDOR

	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	MAP (GL_PERIOD)		GL_PERIOD_CDD		GL_PERIOD

	!
	! Dimension arrays
	!
	DIM STRING TEXT_LEFT(100%), TEXT_RIGHT(100%)

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
	!
	! Datatype:TEXT
	! Size:15
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!
	! Datatype:TEXT
	! Size:15
	!--

	PRINT_DUE_ONLY$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!
	! Datatype:TEXT
	! Size:1
	! Valid Input: Y,N,y,n
	!--

	FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(AP_VENDOR::VENNUM))
	TO_ITEM$ = LEFT(TO_ITEM$, LEN(AP_VENDOR::VENNUM))

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

 ReportTitle:
	!
	! Titles
	!
	TITLE$(1%) = "Accounts Payable Register Including Undefined Vendors"
	TITLE$(2%) = ""

	!
	! Headers
	!
	TITLE$(3%) = "Trans#  InvoiceNum       InvDate        " + &
		"InvAmt    DiscAmt       NetAmt DiscDate DueDate  " + &
		"CkNum      CkDate        CkAmt   BalanceDue"

	TITLE$(4%) = ""

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

	IF FROM_ITEM$ = ""
	THEN
		THIS_VENDOR$ = STRING$(10%, 0%)
	ELSE
		THIS_VENDOR$ = FROM_ITEM$
	END IF

 GetNextRec:
17010	!******************************************************************
	! Main report loop starts here
	!******************************************************************

	!
	! Check status
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #AP_OPEN.CH%, KEY #0% GT THIS_VENDOR$, REGARDLESS
	USE
		CONTINUE ExitTotal
	END WHEN

	THIS_VENDOR$ = AP_OPEN::VENNUM

17015	WHEN ERROR IN
		GET #AP_VENDOR.CH%, KEY #0% EQ AP_OPEN::VENNUM, REGARDLESS
	USE
		AP_VENDOR::VENNUM = THIS_VENDOR$
		AP_VENDOR::VENNAM = "** Undefined Vendor **"

		CONTINUE 17020
	END WHEN

17020	!
	! Check current record
	!
	GOTO ExitTotal &
		IF (AP_VENDOR::VENNUM > TO_ITEM$) AND &
			(TO_ITEM$ <> "")

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

	IF AP_OPEN::TRANKEY <> TRANKEY$
	THEN
		GOSUB VendorLine
		GOTO ExitProgram IF UTL_REPORTX::STAT

		TRANKEY$ = AP_OPEN::TRANKEY + ""
	END IF

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

			CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, &
				TITLE$(), TEXT$, 0%)
		NEXT LOOP%

		VEN_INV_AMT = VEN_INV_AMT + INV_AMT
		VEN_DIS_AMT = VEN_DIS_AMT + DIS_AMT
		VEN_NET_AMT = VEN_NET_AMT + NET_AMT
		VEN_CHK_AMT = VEN_CHK_AMT + CHK_AMT

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
