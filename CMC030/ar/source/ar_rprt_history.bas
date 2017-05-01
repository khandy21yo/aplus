1	%TITLE "Accounts Receivable History Report"
	%SBTTL "AR_RPRT_HISTORY"
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
	! ID:AR032
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Accounts Receivable History\* report will include all transactions which
	!	have been transferred from the A/R Open file.
	!	.b
	!	The report includes the following information:
	!	.table 3,25
	!	.te
	!	Number	Name
	!	.te
	!	Invoice _#	Description
	!	.te
	!	Account	Date
	!	.te
	!	Sale Amount	Discount Amount
	!	.te
	!	Other	Gross Amount
	!	.te
	!	Receipt Check	Date
	!	.te
	!	Amount	Balance
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Print>AR History
	!	.x AR History>Print
	!
	! Option:
	!
	!
	! Author:
	!
	!	03/16/88 - Kevin Handy
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_RPRT_HISTORY/LINE
	!	$ LINK/EXECUTABLE=AR_EXE:*.EXE AR_RPRT_HISTORY, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_RPRT_HISTORY.OBJ;*
	!
	! Modification history:
	!
	!	11/15/87 - Robert Peterson
	!		Changed process to automatically calculate
	!		ledger date.
	!
	!	08/10/90 - Kevin Handy
	!		Modified to put credit memo's on the left hand
	!		side of the page instead of the right hand side.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	06/19/96 - Kevin Handy
	!		Added transaction type "11", adjustment.
	!		Reformat source code.
	!
	!	05/13/97 - Kevin Handy
	!		Reformat source code.
	!
	!	07/30/97 - Kevin Handy
	!		Changed dimension for left/right from 200 to 400
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/26/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	04/19/2001 - Kevin Handy
	!		Increase MAX+CLOSE from 400 to 800
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

	DECLARE INTEGER CONSTANT MAX_CLOSE = 800%

	%INCLUDE "SOURCE:[AR.OPEN]AR_CLOSED.HB"
	MAP	(AR_CLOSED)	AR_CLOSED_CDD	AR_CLOSED
	DIM	AR_CLOSED_CDD	LEFT_CLOSED(MAX_CLOSE), RIGHT_CLOSED(MAX_CLOSE)

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP	(AR_35CUSTOM)	AR_35CUSTOM_CDD	AR_35CUSTOM

	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.HB"
	MAP	(AR_CONTROL)	AR_CONTROL_CDD	AR_CONTROL

	!
	! Array to hold totals
	!
	RECORD TOTAL_RECORD
		STRING ACCT = 18
		DOUBLE SALAMT
		DOUBLE DISAMT
		DOUBLE OTHCHG
		DOUBLE CREDIT
		DOUBLE INVTOTAL
	END RECORD

	DECLARE INTEGER CONSTANT MAX_TOTAL = 500

	DIM TOTAL_RECORD TOTAL(MAX_TOTAL)

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
	!	The ^*From Item\* field causes the printing
	!	to begin with the selected Item _#. The
	!	value in this field must be in agreement with the value in field (03)
	!	Sort by.
	!	.b
	!	A blank field will cause the report to begin with the first
	!	Item _# in the file.
	!	.lm -5
	!
	! Index:
	!	.x Print Accounts Receivable History>From Item
	!	.x From Item>Print Accounts Receivable History
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field causes the printing
	!	to end with the selected Item _#.
	!	This
	!	field must be in agreement with field (03) Sort by.
	!	.b
	!	A blank field will cause the report to end with the last
	!	Item _# in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Print Accounts Receivable History
	!	.x Print Accounts Receivable History>To Item
	!
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	.x Sort by>Print Accounts Receivable History
	!	^*(03) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field determines the order
	!	in which the report will print.
	!	.b
	!	Valid settings are:
	!	.table 3,25
	!	.te
	!	^*N\* - Number
	!	.te
	!	^*T\* - Type
	!	.te
	!	^*C\* - Category
	!	.te
	!	^*A\* - Alphabetical
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Print Accounts Receivable History>Sort by
	!
	!--

	SELECT SORTBY$
	CASE "N"
		K_NUM% = 0%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(AR_35CUSTOM::CUSNUM))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(AR_35CUSTOM::CUSNUM))

	CASE "T"
		K_NUM% = 1%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(AR_35CUSTOM::TTYPE))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(AR_35CUSTOM::TTYPE))

	CASE "T"
		K_NUM% = 2%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(AR_35CUSTOM::CATEGORY))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(AR_35CUSTOM::CATEGORY))

	CASE "A"
		K_NUM% = 3%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(AR_35CUSTOM::ALPSRT))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(AR_35CUSTOM::ALPSRT))

	END SELECT

	GL_WILDCARD$ = EDIT$(UTL_REPORTX::OPTDEF(5%), 132%)

	!++
	! Abstract:FLD06
	!	^*(06) Wildcard Account\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field
	!	selects designated accounts to be
	!	printed by entering a "wildcard" value.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard Account>Print Accounts Receivable History
	!	.x Print Accounts Receivable History>Wildcard Account
	!
	!--

	GL_WILDCARD$ = "*" IF GL_WILDCARD$ = ""


300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CLOSED.OPN"
	USE
		FILENAME$ = "AR_CLOSED"
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

330	!
	! Open control file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.OPN"
		GET #AR_CONTROL.CH%, RECORD 1%, REGARDLESS
	USE
		FILENAME$ = "AR_CONTROL"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "Accounts Receivable History"
	TITLE$(2%) = ""
	TITLE% = 3%

	!
	! Display Heading
	!
	TITLE$(TITLE%) = LEFT(AR_CONTROL::CTITLE, 10%) + &
		" Name"
	TITLE$(TITLE% + 1%) = "Invoice  Descr     Account             " + &
		" Date      Sale Amt  DisAmt    Other " + &
		"   Grs Amt Receipt  Chck #   Date" + &
		"      Amount    Balance"

	TITLE$(TITLE% + 2%) = ""

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #AR_35CUSTOM.CH%, KEY #K_NUM%
		ELSE
			FIND #AR_35CUSTOM.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
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
		GET #AR_35CUSTOM.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	SELECT SORTBY$
	CASE "N"
		GOTO ExitTotal IF (AR_35CUSTOM::CUSNUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	CASE "T"
		GOTO ExitTotal IF (AR_35CUSTOM::TTYPE > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	CASE "C"
		GOTO ExitTotal IF (AR_35CUSTOM::CATEGORY > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	CASE "A"
		GOTO ExitTotal IF (AR_35CUSTOM::ALPSRT > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	END SELECT

17030	!
	! Search for first invoice record for this customer
	!
	LEFT_CLOSED% = 0%		! Total # items in left column
	RIGHT_CLOSED% = 0%		! Total # items in right column
	THIS_INVOICE$ = "1234567890123"	! Impossible invoice number.
	CUSTOMER_PRINTED% = 0%		! Has customer name been printed?

	CUS_SALAMT = 0.0		! Zero customer totals
	CUS_DISAMT = 0.0
	CUS_OTHCHG = 0.0
	CUS_CREDIT = 0.0

	WHEN ERROR IN
		FIND #AR_CLOSED.CH%, KEY #0% EQ AR_35CUSTOM::CUSNUM, REGARDLESS
	USE
		CONTINUE 17090
	END WHEN

17040	!
	! Pull in next record
	!
	WHEN ERROR IN
		GET #AR_CLOSED.CH%, REGARDLESS
	USE
		CONTINUE 17090 IF ERR = 11%
		FILENAME$ = "AR_CLOSED"
		CONTINUE HelpError
	END WHEN

	GOTO 17090 IF AR_35CUSTOM::CUSNUM <> AR_CLOSED::CUSNUM

	IF (AR_CLOSED::INVNUM <> THIS_INVOICE$)
	THEN
		GOSUB DumpInvoice
	END IF

	SELECT AR_CLOSED::TRATYP

	CASE "02"
		!
		! Cash sale.  Goes in both columns.  Balances to zero.
		!
		LEFT_CLOSED% = LEFT_CLOSED% + 1%
		LEFT_CLOSED(LEFT_CLOSED%) = AR_CLOSED
		RIGHT_CLOSED% = RIGHT_CLOSED% + 1%
		RIGHT_CLOSED(RIGHT_CLOSED%) = AR_CLOSED
		RIGHT_CLOSED(RIGHT_CLOSED%)::SALAMT = &
			-RIGHT_CLOSED(RIGHT_CLOSED%)::SALAMT

	CASE "09", "10", "11"
		!
		! Payments.  Goes in right column.
		!
		RIGHT_CLOSED% = RIGHT_CLOSED% + 1%
		RIGHT_CLOSED(RIGHT_CLOSED%) = AR_CLOSED

	CASE ELSE
		!
		! All else goes in left column
		!
		LEFT_CLOSED% = LEFT_CLOSED% + 1%
		LEFT_CLOSED(LEFT_CLOSED%) = AR_CLOSED
	END SELECT

	GOTO 17040

17090	!
	! Finish up this customer
	!
	GOSUB DumpInvoice	! Dump any undumped info

	IF CUSTOMER_PRINTED%
	THEN
		TEXT$ = SPACE$(17%) + &
			FORMAT$(TRM$(AR_CONTROL::CTITLE) + " Total", &
				"'LLLLLLLLLLLLLLLLLLLLLLLLLLLLL") + &
			FORMAT$(CUS_SALAMT - &
				CUS_DISAMT - &
				CUS_OTHCHG, "#######.## ") + &
			FORMAT$(CUS_DISAMT, "#####.## ") + &
			FORMAT$(CUS_OTHCHG, "#####.## ") + &
			FORMAT$(CUS_SALAMT, "#######.## ") + &
			SPACE$(24%) + &
			FORMAT$(-CUS_CREDIT, "#######.##") + &
			FORMAT$(CUS_SALAMT + &
				CUS_CREDIT, "########.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 6%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 6%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		TOT_SALAMT = TOT_SALAMT + CUS_SALAMT
		TOT_DISAMT = TOT_DISAMT + CUS_DISAMT
		TOT_OTHCHG = TOT_OTHCHG + CUS_OTHCHG
		TOT_CREDIT = TOT_CREDIT + CUS_CREDIT
	END IF

	GOTO 17020

	%PAGE

 ExitTotal:
	!*******************************************************************
	! Handle end of report
	!*******************************************************************
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 10%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
		"*** Account Totals **", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FOR LOOP% = 1% TO TOTAL_ACCT%

		TEXT$ = SPACE$(17%) + TOTAL(LOOP%)::ACCT + SPACE$(11%) + &
			FORMAT$(TOTAL(LOOP%)::SALAMT - &
				TOTAL(LOOP%)::DISAMT - &
				TOTAL(LOOP%)::OTHCHG, "########.##") + &
			FORMAT$(TOTAL(LOOP%)::DISAMT, "######.##") + &
			FORMAT$(TOTAL(LOOP%)::OTHCHG, "######.##") + &
			FORMAT$(TOTAL(LOOP%)::SALAMT, "########.##") + &
			SPACE$(24%) + &
			FORMAT$(-TOTAL(LOOP%)::CREDIT, "########.##") + &
			FORMAT$(TOTAL(LOOP%)::SALAMT + &
				TOTAL(LOOP%)::CREDIT, "########.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

	NEXT LOOP%

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	TEXT$ = SPACE$(17%) + "Grand Total" + SPACE$(19%) + &
		FORMAT$(TOT_SALAMT - &
			TOT_DISAMT - &
			TOT_OTHCHG, "#######.## ") + &
		FORMAT$(TOT_DISAMT, "#####.## ") + &
		FORMAT$(TOT_OTHCHG, "#####.## ") + &
		FORMAT$(TOT_SALAMT, "#######.## ") + &
		SPACE$(24%) + &
		FORMAT$(-TOT_CREDIT, "#######.##") + &
		FORMAT$(TOT_SALAMT + &
			TOT_CREDIT, "########.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 6%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 6%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

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

 DumpInvoice:
18000	!*******************************************************************
	! Dump out all information collected for this one invoice.
	!*******************************************************************

	!
	! Skip if there is nothing here
	!
	GOTO 18090 IF (LEFT_CLOSED% = 0%) AND (RIGHT_CLOSED% = 0%)

	!
	! Force item into left size for titles if none there
	!
	LEFT_CLOSED(1%) = RIGHT_CLOSED(1%) IF LEFT_CLOSED% = 0%

	!
	! Skip if account number doesn't match
	!
	IF (GL_WILDCARD$ <> "")
	THEN
		GOTO 18090 &
			IF COMP_STRING(LEFT_CLOSED(1%)::ARACCT, &
			GL_WILDCARD$) = 0%
	END IF

	!
	! Calculate total for this invoice
	!
	!
	! Calculate total for this invoice and customer
	!
	SALAMT, DISAMT, OTHCHG, CREDIT, INVTOTAL = 0.0

	FOR LOOP% = 1% TO LEFT_CLOSED%
		INVTOTAL = INVTOTAL + LEFT_CLOSED(LOOP%)::SALAMT
		SALAMT = SALAMT + LEFT_CLOSED(LOOP%)::SALAMT
		DISAMT = DISAMT + LEFT_CLOSED(LOOP%)::DISAMT
		OTHCHG = OTHCHG + LEFT_CLOSED(LOOP%)::OTHCHG
	NEXT LOOP%

	FOR LOOP% = 1% TO RIGHT_CLOSED%
		INVTOTAL = INVTOTAL + RIGHT_CLOSED(LOOP%)::SALAMT
		CREDIT = CREDIT + RIGHT_CLOSED(LOOP%)::SALAMT
	NEXT LOOP%

	CUS_SALAMT = CUS_SALAMT + SALAMT
	CUS_DISAMT = CUS_DISAMT + DISAMT
	CUS_OTHCHG = CUS_OTHCHG + OTHCHG
	CUS_CREDIT = CUS_CREDIT + CREDIT
	INVTOTAL = FUNC_ROUND(INVTOTAL, 2%)

	!
	! Summarize for totals
	!
	FOR I% = 1% TO TOTAL_ACCT%

		GOTO 18010 IF TOTAL(I%)::ACCT = LEFT_CLOSED(1%)::ARACCT

		IF TOTAL(I%)::ACCT > LEFT_CLOSED(1%)::ARACCT
		THEN
			TOTAL(J% + 1%) = TOTAL(J%) &
				FOR J% = TOTAL_ACCT% TO I% STEP -1%
			TOTAL_ACCT% = TOTAL_ACCT% + 1%
			TOTAL(I%)::ACCT = LEFT_CLOSED(1%)::ARACCT
			TOTAL(I%)::SALAMT = 0.0
			TOTAL(I%)::DISAMT = 0.0
			TOTAL(I%)::OTHCHG = 0.0
			TOTAL(I%)::CREDIT = 0.0
			TOTAL(I%)::INVTOTAL = 0.0

			GOTO 18010
		END IF

	NEXT I%

	TOTAL_ACCT%, I% = TOTAL_ACCT% + 1%
	TOTAL(I%)::ACCT = LEFT_CLOSED(1%)::ARACCT
	TOTAL(I%)::SALAMT = 0.0
	TOTAL(I%)::DISAMT = 0.0
	TOTAL(I%)::OTHCHG = 0.0
	TOTAL(I%)::CREDIT = 0.0
	TOTAL(I%)::INVTOTAL = 0.0

18010	TOTAL(I%)::SALAMT = TOTAL(I%)::SALAMT + SALAMT
	TOTAL(I%)::DISAMT = TOTAL(I%)::DISAMT + DISAMT
	TOTAL(I%)::OTHCHG = TOTAL(I%)::OTHCHG + OTHCHG
	TOTAL(I%)::CREDIT = TOTAL(I%)::CREDIT + CREDIT
	TOTAL(I%)::INVTOTAL = TOTAL(I%)::SALAMT + INVTOTAL

	!
	! If we haven't printed the customer name yet, then do so.
	!
	IF CUSTOMER_PRINTED% = 0%
	THEN
		TEXT$ = AR_35CUSTOM::CUSNUM + "  " + AR_35CUSTOM::CUSNAM

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 6%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		CUSTOMER_PRINTED% = -1%
	END IF

	!
	! Loop through all of the collected records
	!
	IF LEFT_CLOSED% > RIGHT_CLOSED%
	THEN
		ENDLOOP% = LEFT_CLOSED%
	ELSE
		ENDLOOP% = RIGHT_CLOSED%
	END IF

	FOR LOOP% = 1% TO ENDLOOP%
		!
		! Show invoice number only on first line of type
		!
		IF LOOP% = 1%
		THEN
			TEXT$ = LEFT_CLOSED(LOOP%)::INVNUM + " " + &
				LEFT(LEFT_CLOSED(LOOP%)::DESCR, 9%) + " " + &
				LEFT_CLOSED(LOOP%)::ARACCT + " "
		ELSE
			TEXT$ = "         " + &
				"          " + &
				"                   "
		END IF

		!
		! Handle left column
		!
		IF LOOP% <= LEFT_CLOSED%
		THEN
			TEXT$ = TEXT$ + &
				PRNT_DATE(LEFT_CLOSED(LOOP%)::TRADAT, 6%) + &
				" " + &
				FORMAT$(LEFT_CLOSED(LOOP%)::SALAMT - &
				LEFT_CLOSED(LOOP%)::DISAMT - &
				LEFT_CLOSED(LOOP%)::OTHCHG, "#######.## ") + &
				FORMAT$(LEFT_CLOSED(LOOP%)::DISAMT, &
				"#####.## ") + &
				FORMAT$(LEFT_CLOSED(LOOP%)::OTHCHG, &
				"#####.## ") + &
				FORMAT$(LEFT_CLOSED(LOOP%)::SALAMT, &
				"#######.## ")
		ELSE
			TEXT$ = TEXT$ + SPACE$(49%)
		END IF

		!
		! Handle right column
		!
		IF LOOP% <= RIGHT_CLOSED%
		THEN
			TEXT$ = TEXT$ + &
				RIGHT_CLOSED(LOOP%)::RECNUM + " " + &
				RIGHT_CLOSED(LOOP%)::CHKNUM + " " + &
				PRNT_DATE(RIGHT_CLOSED(LOOP%)::TRADAT, 6%) + &
				FORMAT$(-RIGHT_CLOSED(LOOP%)::SALAMT, &
				"#######.## ")

		ELSE
			TEXT$ = TEXT$ + SPACE$(35%)
		END IF

		!
		! Handle final total
		!
		IF LOOP% = ENDLOOP%
		THEN
			TEXT$ = TEXT$ + FORMAT$(INVTOTAL, "#######.##")
		END IF

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 6%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

	NEXT LOOP%

18090	!
	! Prepare for next invoice number
	!
	LEFT_CLOSED% = 0%
	RIGHT_CLOSED% = 0%
	THIS_INVOICE$ = AR_CLOSED::INVNUM + ""

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
