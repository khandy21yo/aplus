1	%TITLE "Check Accounts Receivable Open File"
	%SBTTL "AR_RPRT_REGCHECK"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1993 BY
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
	! ID:AR031
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	Scans the open invoices file for bad codes,
	!	such as invalid invoice numbers, customer numbers, salesmen,
	!	sub-accounts, etc.
	!	.lm -5
	!
	! Index:
	!	.x Accounts Receivable>Check
	!	.x Check>Accounts Receivable
	!
	! Option:
	!
	!
	! Author:
	!
	!	09/30/93 - Kevin Handy
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_RPRT_REGCHECK/LINE
	!	$ LINK/EXECUTABLE=AR_EXE:*.EXE AR_RPRT_REGCHECK, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_RPRT_REGCHECK.OBJ;*
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	06/19/96 - Kevin Handy
	!		Add transaction type "11", adjustments
	!		Reformat source code.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/06/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.HB"
	MAP	(AR_OPEN)	AR_OPEN_CDD	AR_OPEN

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP	(AR_35CUSTOM)	AR_35CUSTOM_CDD	AR_35CUSTOM

	%PAGE

	ON ERROR GOTO 19000


 Init:	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field causes the
	!	printing to begin with the selected Item _#.
	!	The value must be in agreement with field
	!	(03) Sort by.
	!	.b
	!	A blank setting will cause the report to begin with the first
	!	Item _# in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Print Receivable Ledger
	!	.x Print Receivable Ledger>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field causes the printing
	!	to end with the selected Item _#. The value must
	!	be in agreement with the value in field (03) Sort by.
	!	.b
	!	A blank setting will cause the report to end with the last
	!	Item _# in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Print Receivable Ledger
	!	.x Print Receivable Ledger>To Item
	!
	!--

	K_NUM% = 0%

300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.OPN"
	USE
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

310	!
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
	TITLE$(1%) = "Accounts Receviable Register Check"
	TITLE$(2%) = ""

	TITLE$(3%) = "Customer  Invoice"
	TITLE$(4%) = ""

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #AR_OPEN.CH%, KEY #K_NUM%
		ELSE
			FIND #AR_OPEN.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	WHEN ERROR IN
		GET #AR_OPEN.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO ExitTotal IF (AR_OPEN::CUSNUM > TO_ITEM$) AND &
		TO_ITEM$ <> ""

17030	!
	! Look for easy errors
	!
	IF EDIT$(AR_OPEN::CUSNUM, -1%) <> TRM$(AR_OPEN::CUSNUM)
	THEN
		GOSUB DisplayInvoice

		MESSAGE$ = "Bad Characters in Customer Number"
		GOSUB DisplayError
	END IF

	IF EDIT$(AR_OPEN::SALNUM, -1%) <> TRM$(AR_OPEN::SALNUM)
	THEN
		GOSUB DisplayInvoice

		MESSAGE$ = "Bad Characters in Salesman Number"
		GOSUB DisplayError
	END IF

	IF INSTR(1%, "01,02,03,04,08,09,10,11", AR_OPEN::TRATYP) = 0%
	THEN
		GOSUB DisplayInvoice

		MESSAGE$ = "Invalid Transaction Type"
		GOSUB DisplayError
	END IF

	IF AR_OPEN::DUEDATE < "        "
	THEN
		GOSUB DisplayInvoice

		MESSAGE$ = "Bad Characters in Due Date"
		GOSUB DisplayError
	END IF

	IF AR_OPEN::DISCOUNTDATE < "        "
	THEN
		GOSUB DisplayInvoice

		MESSAGE$ = "Bad Characters in Discount Date"
		GOSUB DisplayError
	END IF

17090	GOTO 17020

	%PAGE


 ExitTotal:
	!*******************************************************************
	! Handle end of report
	!*******************************************************************

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

 DisplayInvoice:
	!*******************************************************************
	! Print out invoice title if we havent already
	!*******************************************************************

	IF THIS_INVOICE$ <> AR_OPEN::CUSNUM + AR_OPEN::INVNUM
	THEN
		THIS_INVOICE$ = AR_OPEN::CUSNUM + AR_OPEN::INVNUM

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

		TEXT$ = AR_OPEN::CUSNUM + " " + AR_OPEN::INVNUM

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	RETURN

 DisplayError:
	!*******************************************************************
	! Print out error message
	!*******************************************************************

	TEXT$ = "        " + MESSAGE$

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
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
