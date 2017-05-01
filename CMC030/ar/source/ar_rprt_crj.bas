1	%TITLE "Cash Receipts Journal Report"
	%SBTTL "AR_RPRT_CRJ"
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
	! ID:AR002
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Print Cash Receipts Journal\* option will provide a
	!	report containing the following information:
	!	.table 3,25
	!	.te
	!	Receipt _#	Customer Code
	!	.te
	!	Check _#	Deposit _#
	!	.te
	!	Description	Date
	!	.te
	!	Invoice _#	Line
	!	.te
	!	Sub-Acct _#	Account _#
	!	.te
	!	Amount
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Cash Receipts Journal
	!	.x Cash Receipts Journal>Report
	!	.x Print>Cash Receipts Journal
	!	.x Cash Receipts Journal>Print
	!
	! Option:
	!
	! Compile:
	!
	!	$  BAS AR_SOURCE:AR_RPRT_CRJ.BAS/LINE
	!	$  LINK/EXE=AR_EXE:*.EXE AR_RPRT_CRJ, FUNC_LIB:CMCLINK/OPTION
	!	$  DELETE AR_RPRT_CRJ.OBJ;*
	!
	! Author:
	!
	!	02/25/88 - Aaron Redd
	!
	! Modification history:
	!
	!	03/18/88 - Kevin Handy
	!		Reformatted.
	!
	!	08/05/88 - Kevin Handy
	!		Added Salesman to cash receipts journal.
	!
	!	08/10/89 - Aaron Redd
	!		Added Debit/Credit page(s) to end of report.
	!
	!	05/17/91 - Frank F. Starman
	!		Print description instead cutomer name if type is
	!		not 09.
	!
	!	06/06/91 - Kevin Handy
	!		Unwound error trapping.
	!
	!	06/10/91 - Craig Tanner
	!		Modified to use function GL_OUTP_ACCTSUM to do last page
	!		summary.
	!
	!	10/23/92 - Dan Perkins
	!		Added arguement to GL_OUTP_ACCTSUM because of a
	!		change in this function.
	!
	!	10/26/92 - Kevin Handy
	!		Clean up (check)
	!
	!	03/18/93 - Kevin Handy
	!		Added field to GL_OUTP_ACCTSUM for units.
	!
	!	06/01/93 - Dan Perkins
	!		Print Transaction Type when line is printed.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	06/19/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/25/97 - Kevin Handy
	!		Lose unecessary function definitions
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/13/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!		Add more error traps
	!
	!	06/03/2003 - Kevin Handy
	!		Increase digits available for numbers (kbj)
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
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.HB"
	MAP (AR_CONTROL)	AR_CONTROL_CDD		AR_CONTROL

	%INCLUDE "SOURCE:[AR.OPEN]AR_CRJH.HB"
	MAP (AR_CRJH)		AR_CRJH_CDD		AR_CRJH

	%INCLUDE "SOURCE:[AR.OPEN]AR_CRJL.HB"
	MAP (AR_CRJL)		AR_CRJL_CDD		AR_CRJL

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION GL_OUTP_ACCTSUM

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

	!
	! Declare some variables
	!
	TOTAL = 0.0
	THIS_TOTAL = 0.0
	CASH_TOTAL = 0.0

 Init:	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) Batch Number\*
	!	.b
	!	.lm +5
	!	The ^*Batch Number\* field refers to a particular batch number
	!	which is to be printed.
	!	.b
	!	Only one batch at a time may be printed.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Batch Number>Print Cash Receipts Journal
	!	.x Print Cash Receipts Journal>Batch Number
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Receipt\*
	!	.b
	!	.lm +5
	!	The ^*From Receipt\* field causes the
	!	printing to begin with a specified
	!	receipt number.
	!	.b
	!	A blank field will cause the report to start with the first
	!	receipt number in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Receipt>Print Cash Receipts Journal
	!	.x Print Cash Receipts Journal>From Receipt
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Receipt\*
	!	.b
	!	.lm +5
	!	The ^*To Receipt\* field causes the
	!	printing to end with a specified receipt number.
	!	.b
	!	A blank field will cause the report to end with the last
	!	receipt number in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Receipt>Print Cash Receipts Journal
	!	.x Print Cash Receipts Journal>To Receipt
	!
	!--


300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CRJH.OPN"
	USE
		FILENAME$ = "AR_CRJH"
		CONTINUE HelpError
	END WHEN

310	!
	! Open AR_CRJL file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CRJL.OPN"
	USE
		FILENAME$ = "AR_CRJL"
		CONTINUE HelpError
	END WHEN

320	!
	! Open control file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.OPN"
		GET #AR_CONTROL.CH%, RECORD 1%, REGARDLESS
		CLOSE AR_CONTROL.CH%
	USE
		FILENAME$ = "AR_CONTROL"
		CONTINUE HelpError
	END WHEN

330	!
	! Open customer file
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
	TITLE$(1%) = "Cash Receipt File List"
	TITLE$(2%) = "List of Cash Receipts - Batch #" + BATCH_NO$
	TITLE$(3%) = ""

	!
	! Headings
	!
	TITLE$(4%) = "Rec #     " + LEFT(AR_CONTROL::CTITLE, 10%)   + &
		"  Chk #   Deposit Description                Date   " + &
		"                             Account                Amount"

	TITLE$(5%) = "                                            "  + &
		"                       Line  TTyp  Invoice   Salesman" + &
		"    Account                Amount"

	TITLE$(6%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #AR_CRJH.CH%
		ELSE
			FIND #AR_CRJH.CH%, KEY #0% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
		CONTINUE ExitTotal
	END WHEN

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #AR_CRJH.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal
	END WHEN

	!
	! Check current record
	!
	GOTO ExitTotal IF (AR_CRJH::RECNUM > TO_ITEM$) AND TO_ITEM$ <> ""

17030	!
	! Look up customer name
	!
	AR_35CUSTOM::CUSNAM = STRING$(LEN(AR_35CUSTOM::CUSNAM), 63%)
	WHEN ERROR IN
		GET #AR_35CUSTOM.CH%, KEY #0% EQ AR_CRJH::CUSNUM, REGARDLESS
	USE
		CONTINUE 17040 IF ERR = 155%
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

17040	!
	! Print the cash receipt header
	!
	DESCR$ = AR_CRJH::CUSNUM + "  " + AR_35CUSTOM::CUSNAM
	DESCR$ = AR_CRJH::DESCR IF AR_CRJH::TRATYP <> "09"

	TEXT$ = AR_CRJH::RECNUM + "  " + &
		DESCR$

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 5%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TEXT$ = SPACE$(22%) + &
		AR_CRJH::CHECK + "  " + &
		AR_CRJH::DEPOSIT + "  " + &
		AR_CRJH::DESCR + "  " + &
		PRNT_DATE(AR_CRJH::TRADAT, 8%) + SPACE$(26%) + &
		AR_CRJH::ACCT + &
		FORMAT$(AR_CRJH::AMNT, "########.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 5%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TOTAL = TOTAL + AR_CRJH::AMNT
	THIS_TOTAL = AR_CRJH::AMNT
	CASH_TOTAL = CASH_TOTAL + AR_CRJH::AMNT

	!
	! Print out line items
	!
	GOSUB Print_Lines
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Put the Debit/Credit information into the temporary file
	!
	GOTO ExitProgram IF GL_OUTP_ACCTSUM (OPT_ADDREC, AR_CRJH::ACCT, &
		0.0, AR_CRJH::AMNT, 0.0, TITLE$(), UTL_REPORTX) <> CMC$_NORMAL

17200	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:
	!
	! Handle end of report
	!
	TOTAL = FUNC_ROUND(TOTAL, 2%)

	TEXT$ = SPACE$(105%) + &
		"Journal Total:" + FORMAT$(TOTAL, "########.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	CASH_TOTAL = FUNC_ROUND(CASH_TOTAL, 2%)

	TEXT$ = SPACE$(100%) + &
		"Total Cash Receipt:" + FORMAT$(CASH_TOTAL, "########.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	IF TOTAL <> 0.0
	THEN
		TEXT$ = "       **** ERROR: Journal does not balance ****"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	V% = GL_OUTP_ACCTSUM (OPT_SUMMARY, "", 0.0, 0.0, 0.0, &
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
		FIND #AR_CRJL.CH%, KEY #0% GE AR_CRJH::RECNUM, REGARDLESS
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
		GET #AR_CRJL.CH%, REGARDLESS
	USE
		CONTINUE LastLine
	END WHEN

	!
	! Check current line item
	!
	GOTO LastLine IF AR_CRJL::RECNUM <> AR_CRJH::RECNUM

	!
	! Print the line item line
	!
	TEXT$ = SPACE$(67%) + &
		AR_CRJL::LLINE + "   " + &
		AR_CRJL::TRATYP + "     " + &
		AR_CRJL::INVNUM + "  " + &
		AR_CRJL::SALNUM + "  " + &
		AR_CRJL::ACCT + &
		FORMAT$(AR_CRJL::AMOUNT, "########.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO LastLine IF UTL_REPORTX::STAT

	TOTAL = TOTAL + AR_CRJL::AMOUNT
	THIS_TOTAL = THIS_TOTAL + AR_CRJL::AMOUNT

	!
	! Put the Debit/Credit information into the temporary file
	!
	GOTO ExitProgram IF GL_OUTP_ACCTSUM (OPT_ADDREC, AR_CRJL::ACCT, &
		0.0, AR_CRJL::AMOUNT, 0.0, TITLE$(), UTL_REPORTX) <> CMC$_NORMAL

	!
	! Try for next Line
	!
	GOTO Get_Next_Line

 LastLine:
	THIS_TOTAL = FUNC_ROUND(THIS_TOTAL, 2%)

	IF THIS_TOTAL <> 0.0
	THEN
		TEXT$ = SPACE$(86%) + &
			"*** Receipt does not balance ***:" + &
			FORMAT$(THIS_TOTAL, "########.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -3%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -2%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	GOTO LastLine IF UTL_REPORTX::STAT

	THIS_TOTAL = 0.0

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
