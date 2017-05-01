1	%TITLE "Sales Journal Report"
	%SBTTL "AR_RPRT_SJ"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1988 BY
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
	! ID:AR001
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Print Sales Journal\* routine will provide a report
	!	containing the following information:
	!	.table 3,25
	!	.te
	!	Invoice _#	Customer
	!	.te
	!	Type	Date
	!	.te
	!	Receipt _#	Check _#
	!	.te
	!	Deposit _#	Account _#
	!	.te
	!	Sub-Acct _#	Description
	!	.te
	!	Amount	Gross Margin
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Print>Sales Journal
	!	.x Sales Journal>Report
	!	.x Report>Sales Journal
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_RPRT_SJ.BAS/LINE
	!	$ LINK/EXE=AR_EXE: AR_RPRT_SJ, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_RPRT_SJ.OBJ;*
	!
	! Author:
	!
	!	02/25/88 - Aaron Redd
	!
	! Modification history:
	!
	!	03/21/88 - Kevin Handy
	!		Reformatted output.
	!
	!	08/10/89 - Aaron Redd
	!		Added Debit/Credit page(s) to end of report.
	!
	!	06/06/91 - Kevin Handy
	!		Unwound error trapping.
	!
	!	06/10/91 - Craig Tanner
	!		Modified to use GL_OUTP_ACCTSUM to do Debit/Credit page.
	!
	!	10/23/92 - Dan Perkins
	!		Added arguement to GL_OUTP_ACCTSUM because of a change
	!		in that function.
	!
	!	03/18/93 - Kevin Handy
	!		Added arguement to GL_OUTP_ACCTSUM for quanity.
	!
	!	03/19/93 - Kevin Handy
	!		Modified to display units on report.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	05/24/95 - Kevin Handy
	!		Format closer to 80 columns.
	!		Fix totals so they handle "H" types as an
	!		invoice amount.
	!
	!	08/30/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/20/97 - Kevin Handy
	!		Don't need to assign channel for report
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	07/05/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! CDD inclusions and related memory allocations
	!
	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.HB"
	MAP (AR_CONTROL)	AR_CONTROL_CDD		AR_CONTROL

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

	%INCLUDE "SOURCE:[AR.OPEN]AR_SJH.HB"
	MAP (AR_SJH)		AR_SJH_CDD		AR_SJH

	%INCLUDE "SOURCE:[AR.OPEN]AR_SJL.HB"
	MAP (AR_SJL)		AR_SJL_CDD		AR_SJL

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION GL_OUTP_ACCTSUM

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

 Init:	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) Batch to Print\*
	!	.b
	!	.lm +5
	!	The ^*Batch to Print\* field refers to a particular batch number
	!	which is to be printed.
	!	.b
	!	Only one batch at a time may be printed.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Batch to Print>Print Sales Journal
	!	.x Print Sales Journal>Batch to Print
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Invoice\*
	!	.b
	!	.lm +5
	!	The ^*From Invoice\* field causes printing
	!	to begin with a specified invoice number.
	!	.b
	!	A blank will cause the report to start with the first invoice
	!	number in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Invoice>Print Sales Journal
	!	.x Print Sales Journal>From Invoice
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(02) To Invoice\*
	!	.b
	!	.lm +5
	!	The ^*To Invoice\* field causes printing
	!	to end with a specified invoice number.
	!	.b
	!	A blank will cause the report to end with the last invoice
	!	number in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Invoice>Print Sales Journal
	!	.x Print Sales Journal>To Invoice
	!
	!--


300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_SJH.OPN"
	USE
		FILENAME$ = "AR_SJH"
		CONTINUE HelpError
	END WHEN

310	!
	! Open AR_SJL file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_SJL.OPN"
	USE
		FILENAME$ = "AR_SJL"
		CONTINUE HelpError
	END WHEN

320	!
	! Open control file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.OPN"
		GET #AR_CONTROL.CH%, RECORD 1%, REGARDLESS
	USE
		IF ERR = 5%
		THEN
			AR_CONTROL::CTITLE = "Customer"
			CONTINUE 330
		END IF

		FILENAME$ = "AR_CONTROL"
		CONTINUE HelpError
	END WHEN

330	!
	! Open Customer file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.OPN"
	USE
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "Sales Journal File List"
	TITLE$(2%) = "List of Sales - Batch #" + BATCH_NO$
	TITLE$(3%) = ""


	TITLE$(4%) = "Invoice   " + LEFT(AR_CONTROL::CTITLE, 10%) + &
		" Type Date      Receipt   Chk #   Dpst #  Account    " + &
		"         Sub-Acct    Description                     Amount"
	TITLE$(5%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #AR_SJH.CH%
		ELSE
			FIND #AR_SJH.CH%, KEY #0% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
		CONTINUE ExitTotal
	END WHEN

	!
	! Declare some variables
	!
	!GRAND_TOTAL = 0.0
	GROSS_TOTAL = 0.0
	INVOICE_TOTAL = 0.0
	DIST_TOTAL = 0.0

	SALE_DIST = 0.0
	CASH_DIST = 0.0
	DEBIT_DIST = 0.0
	SERV_DIST = 0.0
	CREDIT_DIST = 0.0

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #AR_SJH.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal
	END WHEN

	!
	! Check current record
	!
	GOTO ExitTotal IF (AR_SJH::INVNUM > TO_ITEM$) AND TO_ITEM$ <> ""

17100	!
	! Look up customer name
	!
	AR_35CUSTOM::CUSNAM = STRING$(LEN(AR_35CUSTOM::CUSNAM), 63%)
	WHEN ERROR IN
		GET #AR_35CUSTOM.CH%, KEY #0% GE AR_SJH::CUSNUM, REGARDLESS
	USE
		CONTINUE 17120 IF ERR = 155%
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

17120	!
	! Print the Sales Journal line
	!
	TEXT$ = AR_SJH::INVNUM + "  " + &
		AR_SJH::CUSNUM + "  " + &
		AR_35CUSTOM::CUSNAM

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 4%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TEXT$ = SPACE$(22%) + &
		AR_SJH::TRATYP + " " + &
		PRNT_DATE(AR_SJH::TRADAT, 8%) + " " + &
		AR_SJH::RECNUM + "  " + &
		AR_SJH::CHECK + "  " + &
		AR_SJH::DEPOSIT + "  " + &
		AR_SJH::ARACCT + "  " + &
		AR_SJH::SUBACCT + "  " + &
		AR_SJH::DESCR + "  " + &
		FORMAT$(AR_SJH::AMOUNT, "#######.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Total header
	!
	!GRAND_TOTAL = GRAND_TOTAL + AR_SJH::AMOUNT
	GROSS_MARGIN = 0.0
	THIS_TOTAL = AR_SJH::AMOUNT
	INVOICE_TOTAL = FUNC_ROUND(INVOICE_TOTAL + AR_SJH::AMOUNT, 2%)

	SELECT AR_SJH::TRATYP

	CASE "01"
		SALE_DIST = FUNC_ROUND(SALE_DIST + AR_SJH::AMOUNT, 2%)

	CASE "02"
		CASH_DIST = FUNC_ROUND(CASH_DIST + AR_SJH::AMOUNT, 2%)

	CASE "03"
		DEBIT_DIST = FUNC_ROUND(DEBIT_DIST + AR_SJH::AMOUNT, 2%)

	CASE "04"
		SERV_DIST = FUNC_ROUND(SERV_DIST + AR_SJH::AMOUNT, 2%)

	CASE "08"
		CREDIT_DIST = FUNC_ROUND(CREDIT_DIST + AR_SJH::AMOUNT, 2%)

	END SELECT

	!
	! Print out line items
	!
	GOSUB Print_Lines
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Put the Debit/Credit information into the temporary file
	!
	GOTO ExitProgram IF GL_OUTP_ACCTSUM (OPT_ADDREC, AR_SJH::ARACCT, &
		0.0, AR_SJH::AMOUNT, 0.0, TITLE$(), UTL_REPORTX) <> CMC$_NORMAL

17200	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:
	!
	! Handle end of report
	!
	DIST_TOTAL = FUNC_ROUND(DIST_TOTAL, 2%)
	TEXT$ = "                                                      " + &
		"                                        Distribution " + &
		"Grand Total:  " + FORMAT$(DIST_TOTAL, "########.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	INVOICE_TOTAL = FUNC_ROUND(INVOICE_TOTAL, 2%)
	TEXT$ = "                                                      " + &
		"                                             Invoice " + &
		"Grand Total:  " + FORMAT$(INVOICE_TOTAL, "########.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	SALE_DIST = FUNC_ROUND(SALE_DIST, 2%)
	IF SALE_DIST <> 0.0
	THEN
		TEXT$ = "                                                      " + &
			"                          " + &
			"                      Sales on Account" + &
			":  " + FORMAT$(SALE_DIST, "########.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	CASH_DIST = FUNC_ROUND(CASH_DIST, 2%)
	IF CASH_DIST <> 0.0
	THEN
		TEXT$ = "                                                      " + &
			"                                                      " + &
			"Cash Sales:  " + FORMAT$(CASH_DIST, "########.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	DEBIT_DIST = FUNC_ROUND(DEBIT_DIST, 2%)
	IF DEBIT_DIST <> 0.0
	THEN
		TEXT$ = "                                                      " + &
			"                          " + &
			"                            Debit Memo" + &
			":  " + FORMAT$(DEBIT_DIST, "########.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	SERV_DIST = FUNC_ROUND(SERV_DIST, 2%)
	IF SERV_DIST <> 0.0
	THEN
		TEXT$ = "                                                      " + &
			"                          " + &
			"                        Service Charge" + &
			":  " + FORMAT$(SERV_DIST, "########.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	CREDIT_DIST = FUNC_ROUND(CREDIT_DIST, 2%)
	IF CREDIT_DIST <> 0.0
	THEN
		TEXT$ = "                                                      " + &
			"                          " + &
			"                           Credit Memo" + &
			":  " + FORMAT$(CREDIT_DIST, "########.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GROSS_TOTAL = FUNC_ROUND(GROSS_TOTAL, 2%)
	IF GROSS_TOTAL <> 0.0
	THEN
		TEXT$ = "                                                      " + &
			"                          " + &
			"                          Gross Margin" + &
			":  " + FORMAT$(GROSS_TOTAL, "########.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	!
	! Print out the Debit/Credit information
	!
	V% = GL_OUTP_ACCTSUM(OPT_SUMMARY, "", 0.0, 0.0, 0.0, &
		TITLE$(), UTL_REPORTX)

	%PAGE

 ExitProgram:
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

 Print_Lines:
17740	!****************************************************************
	!*  Subroutine for printing out Line Items			*
	!****************************************************************
	!

	WHEN ERROR IN
		FIND #AR_SJL.CH%, KEY #0% GE AR_SJH::INVNUM, REGARDLESS
	USE
		CONTINUE LastLine
	END WHEN

 Get_Next_Line:
17750	!
	! Loop starts here
	!
	GOTO LastLine IF UTL_REPORTX::STAT

	!
	! Get next line item
	!
	WHEN ERROR IN
		GET #AR_SJL.CH%, REGARDLESS
	USE
		CONTINUE LastLine
	END WHEN

	!
	! Check current line item
	!
	GOTO LastLine IF AR_SJL::INVNUM <> AR_SJH::INVNUM

	!
	! Print the line item line
	!
	TEXT$ = "                      " + &
		AR_SJL::LTYPE + " " + &
		AR_SJL::TAXTYP + "                       " + &
		FORMAT$(AR_SJL::QTY, "<%>#######.###  ") + &
		AR_SJL::ACCT + "  " + &
		AR_SJL::SUBACCT + "  " + &
		AR_SJL::DESCR + " " + &
		FORMAT$(AR_SJL::AMOUNT, "########.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO LastLine IF UTL_REPORTX::STAT

	!
	! Put the Debit/Credit information into the temporary file
	!
	GOTO ExitProgram IF GL_OUTP_ACCTSUM (OPT_ADDREC, AR_SJL::ACCT, &
		0.0, AR_SJL::AMOUNT, AR_SJL::QTY, &
		TITLE$(), UTL_REPORTX) <> CMC$_NORMAL

	!
	! Total in the line
	!
	THIS_TOTAL = FUNC_ROUND(THIS_TOTAL + AR_SJL::AMOUNT, 2%)

	SELECT AR_SJL::LTYPE

	CASE "S", "C"
		GROSS_MARGIN = FUNC_ROUND(GROSS_MARGIN + AR_SJL::AMOUNT, 2%)
		GROSS_TOTAL = FUNC_ROUND(GROSS_TOTAL + AR_SJL::AMOUNT, 2%)
		DIST_TOTAL = FUNC_ROUND(DIST_TOTAL + AR_SJL::AMOUNT, 2%)

	CASE "H"
		SALE_DIST = FUNC_ROUND(SALE_DIST + AR_SJL::AMOUNT, 2%)
		INVOICE_TOTAL = FUNC_ROUND(INVOICE_TOTAL + AR_SJL::AMOUNT, 2%)

	CASE ELSE
		DIST_TOTAL = FUNC_ROUND(DIST_TOTAL + AR_SJL::AMOUNT, 2%)

	END SELECT

	!
	! Try for next Line
	!
	GOTO Get_Next_Line

 LastLine:
	GROSS_MARGIN = FUNC_ROUND(GROSS_MARGIN, 2%)
	IF GROSS_MARGIN <> 0.0
	THEN
		TEXT$ = "                                                      " + &
			"                                                    " + &
			"Gross Margin:  " + FORMAT$(GROSS_MARGIN, "########.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -2%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -2%)
	END IF

	THIS_TOTAL = FUNC_ROUND(THIS_TOTAL, 2%)
	IF THIS_TOTAL <> 0.0
	THEN
		TEXT$ = "                                                      " + &
			"                           " + &
			"         *** Invoice does not balance" + &
			":  " + FORMAT$(THIS_TOTAL, "########.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -2%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -2%)
	END IF

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

	RETURN

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
