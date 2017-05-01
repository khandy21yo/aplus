1	%TITLE "Accounts Receivable Register"
	%SBTTL "AR_RPRT_AUDT_LEDGER"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1992 BY
	!
	! Computer Management Center, Inc.
	! Idaho Falls, Idaho  83402.
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
	!	The ^*Accounts Receivable Register\* option provides a report which
	!	details all open transactions which have occurred relative to
	!	each customer. The entire file or selected ranges may be printed.
	!	Wildcarding techniques may be used in order to select specific accounts
	!	to be printed.
	!	.lm -5
	!
	! Index:
	!	.x Accounts Receivable>Ledger
	!	.x Reports>Accounts Receivable Ledger
	!
	! Option:
	!
	!
	! Author:
	!
	!	05/20/92 - Kevin Handy
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_RPRT_AUDT_LEDGER/LINE
	!	$ LINK/EXECUTABLE=AR_EXE:*.EXE AR_RPRT_AUDT_LEDGER, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_RPRT_AUDT_LEDGER.OBJ;*
	!
	! Modification history:
	!
	!	06/16/92 - Kevin Handy
	!		Clean up (check)
	!
	!	06/14/93 - Kevin Handy
	!		Added REGARDLESS to AR_CUSBAL.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	05/12/95 - Kevin Handy
	!		Opened AR_CONTROL as .OPN instead of .MOD.
	!
	!	06/19/96 - Kevin Handy
	!		Added transaction type "11", adjustments.
	!		Reformat source code.
	!
	!	05/13/97 - Kevin Handy
	!		Reformat source code.
	!
	!	09/10/97 - Kevin Handy
	!		Changed "x"+"y" to "xy"
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/17/98 - Kevin Handy
	!		Drop the balance forward information, which is
	!		not meaningful for this report.
	!
	!	08/10/99 - Kevin Handy
	!		Added option to only print grand totals (robson)
	!
	!	05/10/2000 - Kevin Handy
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
	DIM	AR_OPEN_CDD	LEFT_OPEN(500%), RIGHT_OPEN(500%)

	%INCLUDE "SOURCE:[AR.OPEN]AR_CLOSED.HB"
	MAP	(AR_CLOSED)	AR_CLOSED_CDD	AR_CLOSED

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP	(AR_35CUSTOM)	AR_35CUSTOM_CDD	AR_35CUSTOM

	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.HB"
	MAP	(AR_CONTROL)	AR_CONTROL_CDD	AR_CONTROL

	%INCLUDE "SOURCE:[AR.OPEN]AR_CUSBAL.HB"
	MAP	(AR_CUSBAL)	AR_CUSBAL_CDD	AR_CUSBAL

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

	DECLARE INTEGER CONSTANT MAX_TOTAL = 1000

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
	!	be in agreement with field (03) Sort by.
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

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field determines the order
	!	in which the report will print.
	!	.b
	!	Valid entries are:
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
	!	.x Sort by>Print Receivable Ledger
	!	.x Print Receivable Ledger>Sort by
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

	CASE "C"
		K_NUM% = 1%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(AR_35CUSTOM::CATEGORY))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(AR_35CUSTOM::CATEGORY))

	CASE "A"
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(AR_35CUSTOM::ALPSRT))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(AR_35CUSTOM::ALPSRT))
		K_NUM% = 2%

	END SELECT

	PRINT_ZERO_INVOICE$ = LEFT(EDIT$(UTL_REPORTX::OPTDEF(3%), -1%), 1%)
	!++
	! Abstract:FLD04
	!	^*(04) Print Zero Invoices\*
	!	.b
	!	.lm +5
	!	The ^*Print Zero Invoices\* field suppresses the printing
	!	of invoices with a zero balance.
	!	.table 3,25
	!	.te
	!	^*Y\* - Yes
	!	.te
	!	^*N\* - No
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Print Zero Invoices>Print Receivable Ledger
	!	.x Print Receivable Ledger>Print Zero Invoices
	!
	!--

	PERIOD_WILD$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)
	!++
	! Abstract:FLD05
	!
	!
	! Index:
	!
	!
	!--

	GL_WILDCARD$ = EDIT$(UTL_REPORTX::OPTDEF(5%), 132%)
	GL_WILDCARD$ = "*" IF GL_WILDCARD$ = ""

	!++
	! Abstract:FLD06
	!	^*(06) Wildcard Account\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects designated
	!	accounts receivable to be printed by entering a "wildcard" value
	!	in this field.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard Account>Print Receivable Ledger
	!	.x Print Receivable Ledger>Wildcard Account
	!
	!--

	TOTAL_ONLY$ = LEFT$(UTL_REPORTX::OPTDEF(6%), 1%)
	!++
	! Abstract:FLD07
	!	^*(07) Totals only (Y,N)\*
	!	.b
	!	.lm +5
	!	Should only the final grand total be printed.
	!	.lm -5
	!
	! Index:
	!	.x Totals only>Print Receivable Ledger
	!	.x Print Receivable Ledger>Totals only
	!
	!--

300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.OPN"
	USE
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

302	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CLOSED.OPN"
	USE
		FILENAME$ = "AR_CLOSED"
		CONTINUE HelpError
	END WHEN

305	!
	! Open customer balance file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CUSBAL.OPN"
	USE
		CONTINUE 310 IF ERR = 5%
		FILENAME$ = "AR_CUSBAL"
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
	! Open AR Control file
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
	TITLE$(1%) = "Accounts Receivable Audit Period"

	TITLE$(2%) = ""

	!
	! Display Heading
	!
	TITLE$(3%) = LEFT(AR_CONTROL::CTITLE, 10%) + "  Name"
	TITLE$(4%) = "Invoice  Descr     Account            " + &
		"Date       Sale Amt   DisAmt    Other " + &
		"   Grs Amt Receipt  Chck # Date" + &
		"        Amount    Balance"

	TITLE$(5%) = "."

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
		IF TO_ITEM$ <> ""
		THEN
			GOTO ExitTotal IF (AR_35CUSTOM::CUSNUM > TO_ITEM$)
		END IF
	CASE "T"
		IF TO_ITEM$ <> ""
		THEN
			GOTO ExitTotal IF (AR_35CUSTOM::TTYPE > TO_ITEM$)
		END IF
	CASE "C"
		IF TO_ITEM$ <> ""
		THEN
			GOTO ExitTotal IF (AR_35CUSTOM::CATEGORY > TO_ITEM$)
		END IF
	CASE "A"
		IF TO_ITEM$ <> ""
		THEN
			GOTO ExitTotal IF (AR_35CUSTOM::ALPSRT > TO_ITEM$)
		END IF
	END SELECT

17030	!
	! Search for first invoice record for this customer
	!
	LEFT_OPEN% = 0%			! Total # items in left column
	RIGHT_OPEN% = 0%		! Total # items in right column
	THIS_INVOICE$ = "1234567890123"	! Impossible invoice number.
	CUSTOMER_PRINTED% = 0%		! Has customer name been printed?

	CUS_SALAMT = 0.0		! Zero customer totals
	CUS_DISAMT = 0.0
	CUS_OTHCHG = 0.0
	CUS_CREDIT = 0.0

	WHEN ERROR IN
		FIND #AR_OPEN.CH%, KEY #0% EQ AR_35CUSTOM::CUSNUM, REGARDLESS
	USE
		CONTINUE 17050
	END WHEN

17040	!
	! Pull in next record
	!
	WHEN ERROR IN
		GET #AR_OPEN.CH%, REGARDLESS
	USE
		CONTINUE 17050 IF ERR = 11%
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

	GOTO 17050 IF AR_35CUSTOM::CUSNUM <> AR_OPEN::CUSNUM

	GOTO 17040 IF COMP_STRING(AR_OPEN::UPDATED, PERIOD_WILD$) = 0%

	IF (AR_OPEN::INVNUM <> THIS_INVOICE$)
	THEN
		GOSUB DumpInvoice
	END IF

	SELECT AR_OPEN::TRATYP

	CASE "02"
		!
		! Cash sale.  Goes in both columns.  Balances to zero.
		!
		LEFT_OPEN% = LEFT_OPEN% + 1%
		LEFT_OPEN(LEFT_OPEN%) = AR_OPEN
		RIGHT_OPEN% = RIGHT_OPEN% + 1%
		RIGHT_OPEN(RIGHT_OPEN%) = AR_OPEN
		RIGHT_OPEN(RIGHT_OPEN%)::SALAMT = &
			-RIGHT_OPEN(RIGHT_OPEN%)::SALAMT

	CASE "09", "10", "11"
		!
		! Payments.  Goes in right column.
		!
		RIGHT_OPEN% = RIGHT_OPEN% + 1%
		RIGHT_OPEN(RIGHT_OPEN%) = AR_OPEN

	CASE ELSE
		!
		! All else goes in left column
		!
		LEFT_OPEN% = LEFT_OPEN% + 1%
		LEFT_OPEN(LEFT_OPEN%) = AR_OPEN
	END SELECT

	GOTO 17040

17050	WHEN ERROR IN
		FIND #AR_CLOSED.CH%, KEY #0% EQ AR_35CUSTOM::CUSNUM, REGARDLESS
	USE
		CONTINUE 17090
	END WHEN

17060	!
	! Pull in next record
	!
	WHEN ERROR IN
		GET #AR_CLOSED.CH%, REGARDLESS
	USE
		CONTINUE 17090
	END WHEN

	GOTO 17090 IF AR_35CUSTOM::CUSNUM <> AR_CLOSED::CUSNUM

	GOTO 17060 IF COMP_STRING(AR_CLOSED::UPDATED, PERIOD_WILD$) = 0%

	IF (AR_CLOSED::INVNUM <> THIS_INVOICE$)
	THEN
		GOSUB DumpInvoice
	END IF

	SELECT AR_CLOSED::TRATYP

	CASE "02"
		!
		! Cash sale.  Goes in both columns.  Balances to zero.
		!
		LEFT_OPEN% = LEFT_OPEN% + 1%
		LEFT_OPEN(LEFT_OPEN%) = AR_CLOSED
		RIGHT_OPEN% = RIGHT_OPEN% + 1%
		RIGHT_OPEN(RIGHT_OPEN%) = AR_CLOSED
		RIGHT_OPEN(RIGHT_OPEN%)::SALAMT = &
			-RIGHT_OPEN(RIGHT_OPEN%)::SALAMT

	CASE "09", "10", "11"
		!
		! Payments.  Goes in right column.
		!
		RIGHT_OPEN% = RIGHT_OPEN% + 1%
		RIGHT_OPEN(RIGHT_OPEN%) = AR_CLOSED

	CASE ELSE
		!
		! All else goes in left column
		!
		LEFT_OPEN% = LEFT_OPEN% + 1%
		LEFT_OPEN(LEFT_OPEN%) = AR_CLOSED
	END SELECT

	GOTO 17060

17090	!
	! Finish up this customer
	!
	GOSUB DumpInvoice	! Dump any undumped info

	IF CUSTOMER_PRINTED%
	THEN
		IF TOTAL_ONLY$ <> "Y"
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
				FORMAT$(-CUS_CREDIT, "#######.## ") + &
				FORMAT$(CUS_SALAMT + &
					CUS_CREDIT, "#######.## ")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -2%)
			GOTO ExitProgram IF UTL_REPORTX::STAT
		END IF

		TOT.SALAMT = TOT.SALAMT + CUS_SALAMT
		TOT.DISAMT = TOT.DISAMT + CUS_DISAMT
		TOT.OTHCHG = TOT.OTHCHG + CUS_OTHCHG
		TOT.CREDIT = TOT.CREDIT + CUS_CREDIT
	END IF

	GOTO 17020

	%PAGE

 ExitTotal:
	!*******************************************************************
	! Handle end of report
	!*******************************************************************
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
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
			SPACE$(23%) + &
			FORMAT$(-TOTAL(LOOP%)::CREDIT, "########.##") + &
			FORMAT$(TOTAL(LOOP%)::SALAMT + &
				TOTAL(LOOP%)::CREDIT, "########.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

	NEXT LOOP%

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	TEXT$ = SPACE$(17%) + "Grand Total" + SPACE$(18%) + &
		FORMAT$(TOT.SALAMT - &
			TOT.DISAMT - &
			TOT.OTHCHG, "########.##") + &
		FORMAT$(TOT.DISAMT, "######.##") + &
		FORMAT$(TOT.OTHCHG, "######.##") + &
		FORMAT$(TOT.SALAMT, "########.##") + &
		SPACE$(23%) + &
		FORMAT$(-TOT.CREDIT, "########.##") + &
		FORMAT$(TOT.SALAMT + &
			TOT.CREDIT, "########.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

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
18100	!*******************************************************************
	! Dump out all information collected for this one invoice.
	!*******************************************************************

	!
	! Skip if there is nothing here
	!
	GOTO 18190 IF (LEFT_OPEN% = 0%) AND (RIGHT_OPEN% = 0%)

	!
	! Force item into left size for titles if none there
	!
	LEFT_OPEN(1%) = RIGHT_OPEN(1%) IF LEFT_OPEN% = 0%

	!
	! Skip if account number doesn't match
	!
	IF (GL_WILDCARD$ <> "")
	THEN
		GOTO 18190 &
			IF COMP_STRING(LEFT_OPEN(1%)::ARACCT, GL_WILDCARD$) = 0%
	END IF

	!
	! Calculate total for this invoice and customer
	!
	SALAMT, DISAMT, OTHCHG, CREDIT, INVTOTAL = 0.0

	INVTOTAL = FUNC_ROUND(INVTOTAL + LEFT_OPEN(LOOP%)::SALAMT, 2%) &
		FOR LOOP% = 1% TO LEFT_OPEN%
	INVTOTAL = FUNC_ROUND(INVTOTAL + RIGHT_OPEN(LOOP%)::SALAMT, 2%) &
		FOR LOOP% = 1% TO RIGHT_OPEN%

	!
	! Skip invoice if not printing zero balance invoices
	!
	IF (PRINT_ZERO_INVOICE$ = "N") AND (INVTOTAL = 0.0)
	THEN
		GOTO 18190
	END IF

	FOR LOOP% = 1% TO LEFT_OPEN%
		SALAMT = SALAMT + LEFT_OPEN(LOOP%)::SALAMT
		DISAMT = DISAMT + LEFT_OPEN(LOOP%)::DISAMT
		OTHCHG = OTHCHG + LEFT_OPEN(LOOP%)::OTHCHG
	NEXT LOOP%

	FOR LOOP% = 1% TO RIGHT_OPEN%
		CREDIT = CREDIT + RIGHT_OPEN(LOOP%)::SALAMT
	NEXT LOOP%

	CUS_SALAMT = CUS_SALAMT + SALAMT
	CUS_DISAMT = CUS_DISAMT + DISAMT
	CUS_OTHCHG = CUS_OTHCHG + OTHCHG
	CUS_CREDIT = CUS_CREDIT + CREDIT

	!
	! Summarize for totals
	!
	FOR I% = 1% TO TOTAL_ACCT%

		GOTO 18110 IF TOTAL(I%)::ACCT = LEFT_OPEN(1%)::ARACCT

		IF TOTAL(I%)::ACCT > LEFT_OPEN(1%)::ARACCT
		THEN
			TOTAL(J% + 1%) = TOTAL(J%) &
				FOR J% = TOTAL_ACCT% TO I% STEP -1%
			TOTAL_ACCT% = TOTAL_ACCT% + 1%
			TOTAL(I%)::ACCT = LEFT_OPEN(1%)::ARACCT
			TOTAL(I%)::SALAMT = 0.0
			TOTAL(I%)::DISAMT = 0.0
			TOTAL(I%)::OTHCHG = 0.0
			TOTAL(I%)::CREDIT = 0.0
			TOTAL(I%)::INVTOTAL = 0.0

			GOTO 18110
		END IF

	NEXT I%

	TOTAL_ACCT%, I% = TOTAL_ACCT% + 1%
	TOTAL(I%)::ACCT = LEFT_OPEN(1%)::ARACCT
	TOTAL(I%)::SALAMT = 0.0
	TOTAL(I%)::DISAMT = 0.0
	TOTAL(I%)::OTHCHG = 0.0
	TOTAL(I%)::CREDIT = 0.0
	TOTAL(I%)::INVTOTAL = 0.0

18110	TOTAL(I%)::SALAMT = TOTAL(I%)::SALAMT + SALAMT
	TOTAL(I%)::DISAMT = TOTAL(I%)::DISAMT + DISAMT
	TOTAL(I%)::OTHCHG = TOTAL(I%)::OTHCHG + OTHCHG
	TOTAL(I%)::CREDIT = TOTAL(I%)::CREDIT + CREDIT
	TOTAL(I%)::INVTOTAL = TOTAL(I%)::SALAMT + INVTOTAL

	!
	! If we haven't printed the customer name yet, then do so.
	!
	IF CUSTOMER_PRINTED% = 0%
	THEN
		IF TOTAL_ONLY$ <> "Y"
		THEN
			TEXT$ = AR_35CUSTOM::CUSNUM + "  " + AR_35CUSTOM::CUSNAM

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 6%)
			GOTO ExitProgram IF UTL_REPORTX::STAT
		END IF

		CUSTOMER_PRINTED% = -1%
	END IF

	!
	! Loop through all of the collected records
	!
	IF TOTAL_ONLY$ <> "Y"
	THEN
		IF LEFT_OPEN% > RIGHT_OPEN%
		THEN
			ENDLOOP% = LEFT_OPEN%
		ELSE
			ENDLOOP% = RIGHT_OPEN%
		END IF

		FOR LOOP% = 1% TO ENDLOOP%
			!
			! Show invoice number only on first line of type
			!
			IF LOOP% = 1%
			THEN
				TEXT$ = LEFT_OPEN(LOOP%)::INVNUM + " " + &
					LEFT(LEFT_OPEN(LOOP%)::DESCR, 9%) + " " + &
					LEFT_OPEN(LOOP%)::ARACCT + " "
			ELSE
				TEXT$ = "         " + &
					"          " + &
					"                   "
			END IF

			!
			! Handle left column
			!
			IF LOOP% <= LEFT_OPEN%
			THEN
				TEXT$ = TEXT$ + &
					PRNT_DATE(LEFT_OPEN(LOOP%)::TRADAT, 6%) + &
					" " + &
					FORMAT$(LEFT_OPEN(LOOP%)::SALAMT - &
					LEFT_OPEN(LOOP%)::DISAMT - &
					LEFT_OPEN(LOOP%)::OTHCHG, "#######.## ") + &
					FORMAT$(LEFT_OPEN(LOOP%)::DISAMT, &
					"#####.## ") + &
					FORMAT$(LEFT_OPEN(LOOP%)::OTHCHG, &
					"#####.## ") + &
					FORMAT$(LEFT_OPEN(LOOP%)::SALAMT, "#######.## ")
			ELSE
				TEXT$ = TEXT$ + SPACE$(49%)
			END IF

			!
			! Handle right column
			!
			IF LOOP% <= RIGHT_OPEN%
			THEN
				TEXT$ = TEXT$ + &
					RIGHT_OPEN(LOOP%)::RECNUM + " " + &
					RIGHT_OPEN(LOOP%)::CHKNUM + " " + &
					PRNT_DATE(RIGHT_OPEN(LOOP%)::TRADAT, 6%) + &
					FORMAT$(-RIGHT_OPEN(LOOP%)::SALAMT, &
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

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
			GOTO ExitProgram IF UTL_REPORTX::STAT

		NEXT LOOP%
	END IF

18190	!
	! Prepare for next invoice number
	!
	LEFT_OPEN% = 0%
	RIGHT_OPEN% = 0%
	THIS_INVOICE$ = AR_OPEN::INVNUM + ""

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
