1	%TITLE "Accounts Payable Duplicated Invoice Report"
	%SBTTL "AP_RPRT_DUPINV"
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
	! ID:APDPIN
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Duplicate Invoice Report\* prints a listing of all duplicated invoice
	!	numbers to prevent issuing the same numbers to the same accounts.
	!	.lm -5
	!
	! Index:
	!	.X Print>Duplicate invoices
	!	.x Duplicate invoices>Print
	!	.x Report>Duplicate Invoices
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_RPRT_DUPINV/LINE
	!	$ LINK/EXECUTABLE=AP_EXE:*.EXE AP_RPRT_DUPINV, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_RPRT_DUPINV.OBJ;*
	!
	! Author:
	!
	!	10/27/88 - Kevin Handy
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
	!	08/27/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/21/97 - Kevin Handy
	!		Use integer for #key
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
	DECLARE			UTL_REPORTX_CDD	UTL_REPORTX

	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.HB"
	MAP	(AP_OPEN)	AP_OPEN_CDD	AP_OPEN

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP	(AP_VENDOR)	AP_VENDOR_CDD	AP_VENDOR

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
	!	^*(01) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field determins the vendor number
	!	(or vendor name, if the report is to be printed in alphabetical
	!	order) with which the report will begin printing. If the setting
	!	is blank, the report will begin with the first vendor in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Duplicate Invoices Report
	!	.x Duplicate Invoices Report>From Item
	!
	!--
	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field determins the vendor number
	!	(or vendor name if the report is to be printed in alphabetical
	!	order) with which the report will end. If this setting is blank,
	!	the report will end with the last vendor in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Duplicate Invoices Report
	!	.x Duplicate Invoices Report>To Item
	!
	!--
	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Sort By (NU,NA,S)\*
	!	.b
	!	.lm +5
	!	The ^*Sort By (NU,NA,S)\* field determins if the
	!	report is to be printed in vendor number order, name order, or
	!	alphabetical order.
	!	.lm 15
	!	.LIST "*"
	!	.LIST ELEMENT
	!	^*NU\*#=#Vendor Number order
	!	.LIST ELEMENT
	!	^*NA\*
	!	#=#Vendor Name order
	!	.LIST ELEMENT
	!	^*S\*
	!	##=#Alphabetical order
	!	.END LIST
	!	.lm -5
	!	There are no other valid values for this field.
	!	.note
	!	The Name Order selection sorts on the name field.
	!	Hence, personal names would sort by first name first.
	!	A name field with a title would sort by the title
	!	first, i.e. "Dr. James Petersen" would be listed
	!	with the "D's".
	!	.end note
	!
	! Index:
	!	.x Sort By>Duplicate Invoices Report
	!	.x Duplicate Invoices Report>Sort By
	!
	!--

	SELECT SORTBY$

	CASE "NA"
		K_NUM% = 1%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(AP_VENDOR::VENNAM))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(AP_VENDOR::VENNAM))

	CASE "AL"
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(AP_VENDOR::ALPSRT))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(AP_VENDOR::ALPSRT))
		K_NUM% = 2%

	CASE ELSE
		K_NUM% = 0%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(AP_VENDOR::VENNUM))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(AP_VENDOR::VENNUM))

	END SELECT

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
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "AP_VENDOR"
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
	!
	! Titles
	!
	TITLE$(1%) = "Accounts Payable Register"
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
		CONTINUE ExitTotal IF ERR = 11%
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
		FIND #AP_OPEN.CH%, KEY #1% EQ AP_VENDOR::VENNUM, REGARDLESS
	USE
		CONTINUE GetNextRec IF ERR = 155%
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

	LC_LEFT% = 0%
	LC_RIGHT% = 0%
	VENDOR_NAME_TEST% = -1%

	BAL_DUE = 0.0

	TRANKEY$ = AP_OPEN::INVNUM + ""

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

	IF AP_OPEN::INVNUM <> TRANKEY$
	THEN
		GOSUB VendorLine
		GOTO ExitProgram IF UTL_REPORTX::STAT

		TRANKEY$ = AP_OPEN::INVNUM + ""
	END IF

17080	!
	! Skip if nothing being invoiced
	!
	GOTO 17090 &
		IF (AP_OPEN::INVAMT = 0.0) AND (AP_OPEN::DISAMT = 0.0) AND &
			(LC_LEFT% >= 1%)

	LC_LEFT% = LC_LEFT% + 1%

	TEXT_LEFT(LC_LEFT%) = AP_OPEN::TRANKEY + "  " + &
		AP_OPEN::INVNUM + "  " + &
		PRNT_DATE(AP_OPEN::INVDAT, 0%) + " "

	NET = AP_OPEN::INVAMT - AP_OPEN::DISAMT

	TEXT_LEFT(LC_LEFT%) = TEXT_LEFT(LC_LEFT%) + &
		FORMAT$(AP_OPEN::INVAMT, "<%>########.## ") + &
		FORMAT$(AP_OPEN::DISAMT, "<%>######.## ") + &
		FORMAT$(NET, "<%>########.## ")

	TEXT_LEFT(LC_LEFT%) = TEXT_LEFT(LC_LEFT%) + &
		PRNT_DATE(AP_OPEN::DISCDAT, 0%) + " " + &
		PRNT_DATE(AP_OPEN::DUEDAT, 0%) + " "

17090	IF (AP_OPEN::CKAMT <> 0.0) OR (AP_OPEN::CKNUM <> "")
	THEN
		LC_RIGHT% = LC_RIGHT% + 1%

		TEXT_RIGHT(LC_RIGHT%) = &
			AP_OPEN::CKNUM + "   " + &
			PRNT_DATE(AP_OPEN::CKDAT, 0%) + &
			FORMAT$(AP_OPEN::CKAMT, " #########.## ")
	END IF

	BAL_DUE = BAL_DUE + (AP_OPEN::INVAMT - AP_OPEN::DISAMT) - AP_OPEN::CKAMT

	!
	! End of sub-loop
	!
	GOTO 17040

17900	GOSUB VendorLine
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

	GOTO ComeBack1 IF (LC_LEFT% <= 1%)

	IF LC_LEFT% > LC_RIGHT%
	THEN
		MAX_LC% = LC_LEFT%
	ELSE
		MAX_LC% = LC_RIGHT%
	END IF

	IF (VENDOR_NAME_TEST%)
	THEN
		TEXT$ = AP_VENDOR::VENNUM + " " + AP_VENDOR::VENNAM
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 5%)

		VENDOR_NAME_TEST% = 0%
	END IF

	FOR LOOP% = 1% TO MAX_LC%
		TEXT$ = ""
		TEXT$ = TEXT_LEFT(LOOP%) IF LOOP% <= LC_LEFT%
		TEXT$ = TEXT$ + SPACE$(89% - LEN(TEXT$)) + TEXT_RIGHT(LOOP%) &
			IF LOOP% <= LC_RIGHT%
		TEXT$ = TEXT$ + SPACE$(120% - LEN(TEXT$)) + &
			FORMAT$(BAL_DUE, "#########.##") &
			IF LOOP% = MAX_LC%

		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	NEXT LOOP%

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), "", -1%)

 ComeBack1:
	LC_LEFT% = 0%
	LC_RIGHT% = 0%
	BAL_DUE = 0.0

	RETURN

	%PAGE

 VendorTotal:
	!******************************************************************
	! Subroutine to print Vendor total
	!******************************************************************

 ! ComeBack2:
 !	RETURN

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
