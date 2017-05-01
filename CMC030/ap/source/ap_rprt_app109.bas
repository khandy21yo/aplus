1	%TITLE "Accounts Payable 1099 for a Period"
	%SBTTL "AP_RPRT_APP109"
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
	! ID:APP109
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Print by Vendor and 1099\* option
	!	prints a report which will
	!	list all accounts payable transactions for a specified accounting
	!	period with specific reference to information pertaining to Form
	!	1099 reporting. This report contains the following fields:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	Vendor Number
	!	.le
	!	Flag
	!	.le
	!	Vendor Name
	!	.le
	!	Code
	!	.le
	!	Code Description
	!	.le
	!	Transaction Number
	!	.le
	!	Invoice Number
	!	.le
	!	Invoice Date
	!	.le
	!	Check Number
	!	.le
	!	Check Date
	!	.le
	!	Check Amount
	!	.le
	!	1099 Amount
	!	.els
	!
	! Index:
	!	.x Accounts Payable>Reports>Vendor and 1099 Audit
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_RPRT_APP109/LINE
	!	$ LINK/EXECUTABLE=AP_EXE:*.EXE AP_RPRT_APP109, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_RPRT_APP109.OBJ;*
	!
	! Author:
	!
	!	10/21/87 - Robert Peterson
	!
	! Modification history:
	!
	!	11/15/87 - Robert Peterson
	!		Changed process to automatically calculate ledger date.
	!
	!	08/23/88 - Kevin Handy
	!		Fixed printing of vendor name, so doesn't sometimes
	!		miss it.
	!
	!	06/14/93 - Kevin Handy
	!		Added REGARDLESS to AP_OPEN.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	05/12/95 - Kevin Handy
	!		Modified to open AP_CONTROL as .OPN instead of .MOD.
	!
	!	12/14/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!		Change RIGHT(NUM1$()) to FORMAT$().
	!
	!	08/27/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/13/97 - Kevin Handy
	!		Reformat source code
	!
	!	08/20/97 - Kevin Handy
	!		Don't need to assign channel for report.
	!
	!	08/22/97 - Kevin Handy
	!		Use 'val%' instead of 'val'
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Remove an excessive number of %PAGE's
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

	!******************************************************************
	! Set up data storage areas (MAPs, DIMENSIONs, DECLAREs, etc.)
	!******************************************************************

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[AP.OPEN]AP_1099_TABLE.HB"
	MAP	(AP_1099_TABLE)	AP_1099_TABLE_CDD	AP_1099_TABLE

	%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL.HB"
	MAP	(AP_CONTROL)	AP_CONTROL_CDD		AP_CONTROL

	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.HB"
	MAP	(AP_OPEN)	AP_OPEN_CDD		AP_OPEN

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP	(AP_VENDOR)	AP_VENDOR_CDD		AP_VENDOR

	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	MAP	(GL_PERIOD)	GL_PERIOD_CDD		GL_PERIOD

	%PAGE

	!******************************************************************
	! Take care of anything else before starting report
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

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field determines the vendor number
	!	(or vendor name, if the report is to be printed in alphabetical
	!	order) with which the report will begin printing. If the setting
	!	is blank, the report will being with the first vendor in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>1099 Payments
	!	.x 1099 Payments>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field determines the vendor number
	!	(or vendor name if the report is to be printed in alphabetical
	!	order) with which the report will end. If this setting is blank,
	!	the report will end with the last vendor in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>1099 Payments
	!	.x 1099 Payments>To Item
	!
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Sort By (NU,NA,S)\*
	!	.b
	!	.lm +5
	!	The ^*Sort By (NU,NA,S)\* field determines if the
	!	report is to be printed in vendor number order, name order, or
	!	alphabetical order.
	!	.lm 15
	!	.LIST "*"
	!	.LIST ELEMENT
	!	^*NU\*#=#Vendor Number order
	!	.LIST ELEMENT
	!	^*NA\*#=#Vendor Name order
	!	.LIST ELEMENT
	!	^*S\*##=#Alphabetical order
	!	.END LIST
	!	.lm -5
	!	.b
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
	!	.x Sort By>1099 Payments
	!	.x 1099 Payments>Sort By
	!
	!--

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
		K_NUM% = 2%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(AP_VENDOR::ALPSRT))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(AP_VENDOR::ALPSRT))

	END SELECT

	%PAGE

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.OPN"
	USE
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

	!
	! Open the AP Vendor file
	!
310	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.OPN"
	USE
		FILENAME$ = "AP_VENDOR"
		CONTINUE HelpError
	END WHEN

	!
	! Open the Accounts Payable 1099 Table file
	!
320	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_1099_TABLE.OPN"
	USE
		FILENAME$ = "AP_1099_TABLE"
		CONTINUE HelpError
	END WHEN

	!
	! Open the GL Controlling file (GL_PERIOD), and get its record
	!
330	WHEN ERROR IN
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
340	WHEN ERROR IN
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

	TEST_YYYY_PP$ = YEAR$ + FORMAT$(CUR_PERIOD%, "<0>#")
	YYYY_PP$ = YEAR$ + "_" + FORMAT$(CUR_PERIOD%, "<0>#")

	%PAGE

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "1099 Payments by Period"
	TITLE$(2%) = "Accounting Period Ended " + &
		RIGHT(YYYY_PP$, 6%) + " " + LEFT(YYYY_PP$, 4%)
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "VendorNum  Fl VendorName              CD " + &
		"CodeDescription   Trans# InvoiceNumber   InvDate  " + &
		"Check# CheckDate    CheckAmt     1099 Amt"
	TITLE$(5%) = ""

	!
	! Layouts for lines printed
	!
	LYT_LINE$ = "$VendorNum:010,$1099Flag:012,$VendorName:037," + &
		"$1099Code:040,$CodeDescr:058,$TranKey:065,$InvNum:081," + &
		"DInvDate:090,$CheckNum:097,DCheckDate:106," + &
		"VCheckAmt:119,V1099WorkAmt:132"

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
17100	!******************************************************************
	! Main report loop starts here
	!******************************************************************

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #AP_VENDOR.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal
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
		GOTO ExitTotal IF (AP_VENDOR::VENNUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""

	CASE "NA"
		GOTO ExitTotal IF (AP_VENDOR::VENNAM > TO_ITEM$) AND &
			TO_ITEM$ <> ""

	CASE ELSE
		GOTO ExitTotal IF (AP_VENDOR::ALPSRT > TO_ITEM$) AND &
			TO_ITEM$ <> ""

	END SELECT

	!
	! Get ready to start a sub-loop
	!
17200	WHEN ERROR IN
		FIND #AP_OPEN.CH%, KEY #0% EQ AP_VENDOR::VENNUM, REGARDLESS
	USE
		CONTINUE GetNextRec
	END WHEN

	TRANKEY$ = "ZZZZZZZZZ"
	PRINT_TEST% = 0%
	VENDOR_CKAMT, VENDOR_AMT1099 = 0.0
	TEMP_TEXT$ = AP_VENDOR::VENNUM + " " + AP_VENDOR::FLG1099 + "  " + &
		LEFT(AP_VENDOR::VENNAM, 23%) + " "

	!
	! Get the AP Open Item file record
	!
17300	WHEN ERROR IN
		GET #AP_OPEN.CH%, REGARDLESS
	USE
		CONTINUE 17600
	END WHEN

	!
	! Exit the sub-loop if the Vendor number has changed
	!
	GOTO 17600 IF AP_VENDOR::VENNUM <> AP_OPEN::VENNUM

	IF AP_OPEN::TRANKEY <> TRANKEY$
	THEN
		INVAMT = AP_OPEN::INVAMT
		AMT_1099 = AP_OPEN::AMT_1099
		CODE_1099$ = AP_OPEN::CODE_1099
	END IF

	TRANKEY$ = AP_OPEN::TRANKEY

	!
	! Get the next record if the date is not the updated date
	!
	GOTO 17300 IF TEST_YYYY_PP$ <> LEFT(AP_OPEN::UPDATED, 6%)

17400	IF CODE_1099$ <> ""
	THEN
		WHEN ERROR IN
			GET #AP_1099_TABLE.CH%, &
				KEY #0% EQ CODE_1099$, &
				REGARDLESS
		USE
			AP_1099_TABLE::DESCR = &
				STRING$(LEN(AP_1099_TABLE::DESCR), 63%)

			CONTINUE 17500
		END WHEN

		TEMP1_TEXT$ = CODE_1099$ + " " + &
			LEFT(AP_1099_TABLE::DESCR, 17%) + " "
	ELSE
		TEMP1_TEXT$ = SPACE$(21%)
	END IF

17500	IF AP_OPEN::CKAMT <> 0.0
	THEN
		FACTOR = 1.0
		FACTOR = AP_OPEN::CKAMT / INVAMT IF INVAMT <> 0.0
		WORK_AMT1099 = FUNC_ROUND(AMT_1099 * FACTOR, 2%)

		TEXT$ = TEMP_TEXT$ + SPACE$(38% - LEN(TEMP_TEXT$)) + &
			TEMP1_TEXT$ + &
			AP_OPEN::TRANKEY + " " + &
			AP_OPEN::INVNUM + " " + &
			PRNT_DATE(AP_OPEN::INVDAT, 6%) + " " + &
			AP_OPEN::CKNUM + " " + &
			PRNT_DATE(AP_OPEN::CKDAT, 6%) + " " + &
			FORMAT$(AP_OPEN::CKAMT, "#,###,###.## ") + &
			FORMAT$(WORK_AMT1099, "#,###,###.##")

		VENDOR_CKAMT = VENDOR_CKAMT + AP_OPEN::CKAMT
		VENDOR_AMT1099 = VENDOR_AMT1099 + WORK_AMT1099

		TOTAL_CKAMT = TOTAL_CKAMT + AP_OPEN::CKAMT
		TOTAL_AMT1099 = TOTAL_AMT1099 + WORK_AMT1099

		PRINT_TEST% = -1%

		TEMP_TEXT$ = ""

		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

	END IF

	!
	! Get the next record
	!
	GOTO 17300

17600	!
	! Print vendor total
	!
	GOSUB VendorTotal IF PRINT_TEST%

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
	! Handle end of report
	!
	TEXT$ = SPACE$(14%) + "Grand Total" + SPACE$(82%) + &
		FORMAT$(TOTAL_CKAMT, "#,###,###.## ") + &
		FORMAT$(TOTAL_AMT1099, "#,###,###.##")

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

 VendorTotal:
	!******************************************************************
	! Subroutine to print out the Vendor total
	!******************************************************************
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ComeBack IF UTL_REPORTX::STAT

	TEXT$ = SPACE$(14%) + "Vendor Total" + SPACE$(81%) + &
		FORMAT$(VENDOR_CKAMT, "#,###,###.## ") + &
		FORMAT$(VENDOR_AMT1099, "#,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ComeBack IF UTL_REPORTX::STAT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

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

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Handle untrapped errors
	!
	FILENAME$ = ""
	RESUME HelpError

32767	!******************************************************************
	! End of report AP_RPRT_APP109
	!******************************************************************
	END
