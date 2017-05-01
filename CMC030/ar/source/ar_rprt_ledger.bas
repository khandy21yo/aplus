1	%TITLE "Accounts Receivable Register"
	%SBTTL "AR_RPRT_LEDGER"
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
	! ID:AR031
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Accounts Receivable Register\* option provides a report which
	!	details all open transactions which have occurred relative to
	!	each customer.  The entire file or selected ranges may be printed.
	!	Wildcarding techniques may be used in order to select specific accounts
	!	to be printed.
	!	.b
	!	The report contains the following information:
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
	!	Receipt _#	Check _#
	!	.te
	!	Date	Amount
	!	.te
	!	Balance
	!	.end table
	!	.lm -5d
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
	!	03/16/88 - Kevin Handy
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_RPRT_LEDGER/LINE
	!	$ LINK/EXECUTABLE=AR_EXE:*.EXE AR_RPRT_LEDGER, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_RPRT_LEDGER.OBJ;*
	!
	! Modification history:
	!
	!	11/15/87 - Robert Peterson
	!		Changed process to automatically calculate
	!		ledger date.
	!
	!	11/30/88 - Kevin Handy
	!		Modified calculation of cutoff month to consider
	!		those who do not use January as the first month.
	!
	!	08/10/90 - Kevin Handy
	!		Put type "08" (Credit Memo) in the right hand
	!		column instead of the left hand one.
	!
	!	10/02/90 - Kevin Handy
	!		Modified to handle paging a little better, so the
	!		customer name will not come on the very bottom of
	!		a page and his first line of detail on the next.
	!
	!	03/18/91 - Frank F. Starman
	!		Print title based on CUT_OFF_REG$ flag
	!
	!	06/06/91 - Kevin Handy
	!		Unwound error trapping.
	!
	!	03/12/92 - Kevin Handy
	!		Removed duplicate error trap (check)
	!
	!	03/16/92 - Kevin Handy
	!		Used FUNC_ROUND to try to make "Zero inv"
	!		option work.
	!
	!	04/02/92 - Kevin Handy
	!		Fixed bug in TO_ITEM$ where it checked for
	!		the sortkey being "A" in one place, and "AL"
	!		in another.
	!
	!	02/01/93 - Kevin Handy
	!		Fixed Sort-By so that it used the proper key
	!		for each sortby.  Probibly wasn't changed when
	!		Frack rearranged the keys in the customer file.
	!
	!	06/14/93 - Kevin Handy
	!		Added REGARDLESS to AR_CUSBAL.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	05/12/95 - Kevin Handy
	!		Open AR_CONTROL as .OPN instead of .MOD.
	!
	!	12/14/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!		Change RIGHT(NUM1$()) to FORMAT$().
	!
	!	05/13/97 - Kevin Handy
	!		Reformat source code
	!
	!	08/24/97 - Kevin Handy
	!		Use 'val%' instead of 'val'
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/26/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP	(AR_35CUSTOM)	AR_35CUSTOM_CDD	AR_35CUSTOM

	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.HB"
	MAP	(AR_CONTROL)	AR_CONTROL_CDD	AR_CONTROL

	%INCLUDE "SOURCE:[AR.OPEN]AR_CUSBAL.HB"
	MAP	(AR_CUSBAL)	AR_CUSBAL_CDD	AR_CUSBAL

	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	MAP	(GL_PERIOD)	GL_PERIOD_CDD	GL_PERIOD

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
	!	A ^*From Item\* field causes the
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
	!	A ^*To Item\* field causes the printing
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
	!	A setting is required in this field.
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
		K_NUM% = 2%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(AR_35CUSTOM::CATEGORY))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(AR_35CUSTOM::CATEGORY))

	CASE "A"
		K_NUM% = 3%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(AR_35CUSTOM::ALPSRT))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(AR_35CUSTOM::ALPSRT))

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
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Print Zero Invoices>Print Receivable Ledger
	!	.x Print Receivable Ledger>Print Zero Invoices
	!
	!--

	CUT_OFF_REG$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)
	!++
	! Abstract:FLD05
	!
	!
	! Index:
	!
	!
	!--

	GL_WILDCARD$ = EDIT$(UTL_REPORTX::OPTDEF(5%), 132%)
	!++
	! Abstract:FLD06
	!	^*(06) Wildcard Account\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects designated
	!	accounts receivable to be printed by entering a
	!	"wildcard" value in this field.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard Account>Print Receivable Ledger
	!	.x Print Receivable Ledger>Wildcard Account
	!
	!--

	GL_WILDCARD$ = "*" IF GL_WILDCARD$ = ""


300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.OPN"
	USE
		FILENAME$ = "AR_OPEN"
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

	GOTO ReportTitle IF CUT_OFF_REG$ <> "Y"

340	!
	! Figure out what in the world needs done (a whole lot)
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.OPN"
		GET #GL_PERIOD.CH%, RECORD 1%, REGARDLESS
		CLOSE GL_PERIOD.CH%
	USE
		FILENAME$ = "GL_PERIOD"
		CONTINUE HelpError
	END WHEN

	IF AR_CONTROL::CLOSEFLAG = "1"
	THEN
		CALL HELP_3MESSAGE(SCOPE, "AR Close in process", &
			"ERR", "AR_CLOSE", &
			"ERROR_CLOSE")
		GOTO ExitProgram
	END IF

	IF AR_CONTROL::CLOSEFLAG = "2"
	THEN
		CALL HELP_3MESSAGE(SCOPE, "AR Reset in process", &
			"ERR", "AR_RESET", &
			"ERROR_RESET")
		GOTO ExitProgram
	END IF

	IF AR_CONTROL::CLOSEFLAG = "3"
	THEN
		CALL HELP_3MESSAGE(SCOPE, "AR Purge in process", &
			"ERR", "AR_PURGE", &
			"ERROR_PURGE")
		GOTO ExitProgram
	END IF

	CUR_PERIOD% = AR_CONTROL::LASTPERCLOSE + 1%
	YEAR$ = AR_CONTROL::YEAR

	IF CUR_PERIOD% > GL_PERIOD::FPFY
	THEN
		CUR_PERIOD% = 1%
		YEAR$ = FORMAT$(VAL%(YEAR$) + 1%, "<0>###")
	END IF

	CUT_OFF_DATE$ = YEAR$ + FORMAT$(CUR_PERIOD%, "<0>#")
	UPDATE_GL$ = LEFT(CUT_OFF_DATE$, 6%)

 ReportTitle:
	!
	! Title
	!
	IF CUT_OFF_REG$ <> "Y"
	THEN
		TITLE$(1%) = "Accounts Receivable Register"
		ADD_ONE% = 0%
	ELSE
		TITLE$(1%) = "Accounts Receivable Ledger"
		TITLE$(2%) = "For Accounting Period Ended " + CUT_OFF_DATE$
		ADD_ONE% = 1%
	END IF
	TITLE$(2% + ADD_ONE%) = ""

	!
	! Display Heading
	!
	TITLE$(3% + ADD_ONE%) = LEFT(AR_CONTROL::CTITLE, 10%) + "  Name"
	TITLE$(4% + ADD_ONE%) = "Invoice  Descr     Account            " + &
		"Date       Sale Amt   DisAmt    Other " + &
		"   Grs Amt Receipt  Chck # Date" + &
		"        Amount    Balance"

	TITLE$(5% + ADD_ONE%) = "."

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
	LEFT_OPEN% = 0%			! Total # items in left column
	RIGHT_OPEN% = 0%		! Total # items in right column
	THIS_INVOICE$ = "1234567890123"	! Impossible invoice number.
	CUSTOMER_PRINTED% = 0%		! Has customer name been printed?

	CUS_SALAMT = 0.0		! Zero customer totals
	CUS.DISAMT = 0.0
	CUS.OTHCHG = 0.0
	CUS.CREDIT = 0.0

	!
	! Handle Balance Foreward Customers
	!
	GOSUB BalanceForeward IF AR_35CUSTOM::METHOD = "B"

	WHEN ERROR IN
		FIND #AR_OPEN.CH%, KEY #0% EQ AR_35CUSTOM::CUSNUM, REGARDLESS
	USE
		CONTINUE 17090
	END WHEN

17040	!
	! Pull in next record
	!
	WHEN ERROR IN
		GET #AR_OPEN.CH%, REGARDLESS
	USE
		CONTINUE 17090 IF ERR = 11%
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

	GOTO 17090 IF AR_35CUSTOM::CUSNUM <> AR_OPEN::CUSNUM

	GOTO 17040 IF UPDATE_GL$ < LEFT(AR_OPEN::UPDATED, 6%) AND &
		CUT_OFF_REG$ = "Y"

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
				CUS.DISAMT - &
				CUS.OTHCHG, "#######.## ") + &
			FORMAT$(CUS.DISAMT, "#####.## ") + &
			FORMAT$(CUS.OTHCHG, "#####.## ") + &
			FORMAT$(CUS_SALAMT, "#######.## ") + &
			SPACE$(24%) + &
			FORMAT$(-CUS.CREDIT, "#######.## ") + &
			FORMAT$(CUS_SALAMT + &
				CUS.CREDIT, "#######.## ")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -2%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		TOT_SALAMT = TOT_SALAMT + CUS_SALAMT
		TOT.DISAMT = TOT.DISAMT + CUS.DISAMT
		TOT.OTHCHG = TOT.OTHCHG + CUS.OTHCHG
		TOT.CREDIT = TOT.CREDIT + CUS.CREDIT
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
		FORMAT$(TOT_SALAMT - &
			TOT.DISAMT - &
			TOT.OTHCHG, "########.##") + &
		FORMAT$(TOT.DISAMT, "######.##") + &
		FORMAT$(TOT.OTHCHG, "######.##") + &
		FORMAT$(TOT_SALAMT, "########.##") + &
		SPACE$(23%) + &
		FORMAT$(-TOT.CREDIT, "########.##") + &
		FORMAT$(TOT_SALAMT + &
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
	SALAMT,DISAMT,OTHCHG,CREDIT,INVTOTAL = 0.0

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
	CUS.DISAMT = CUS.DISAMT + DISAMT
	CUS.OTHCHG = CUS.OTHCHG + OTHCHG
	CUS.CREDIT = CUS.CREDIT + CREDIT

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
		TEXT$ = AR_35CUSTOM::CUSNUM + "  " + AR_35CUSTOM::CUSNAM

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 6%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		CUSTOMER_PRINTED% = -1%
	END IF

	!
	! Loop through all of the collected records
	!
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
				FORMAT$(-RIGHT_OPEN(LOOP%)::SALAMT, "#######.## ")
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

18190	!
	! Prepare for next invoice number
	!
	LEFT_OPEN% = 0%
	RIGHT_OPEN% = 0%
	THIS_INVOICE$ = AR_OPEN::INVNUM + ""

	RETURN

	%PAGE

 BalanceForeward:
18200	!*******************************************************************
	! Pull in beginning balances for balance foreward customers
	!*******************************************************************

	WHEN ERROR IN
		FIND #AR_CUSBAL.CH%, KEY #0% GE AR_35CUSTOM::CUSNUM, REGARDLESS
	USE
		CONTINUE 18290 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "AR_CUSBAL"
		CONTINUE HelpError
	END WHEN

18210	!
	! Get one record from cusbal
	!
	WHEN ERROR IN
		GET #AR_CUSBAL.CH%, REGARDLESS
	USE
		CONTINUE 18290 IF ERR = 11%
		FILENAME$ = "AR_CUSBAL"
		CONTINUE HelpError
	END WHEN

	GOTO 18290 IF AR_CUSBAL::CUSNUM <> AR_35CUSTOM::CUSNUM

	!
	! Skip if account number doesn't match
	!
	IF (GL_WILDCARD$ <> "")
	THEN
		GOTO 18210 IF COMP_STRING(AR_CUSBAL::ACCT, GL_WILDCARD$) = 0%
	END IF

	!
	! Calculate total balance
	!
	INVTOTAL = FUNC_ROUND(AR_CUSBAL::CHARGE + AR_CUSBAL::FUTURE, 2%)
	INVTOTAL = FUNC_ROUND(INVTOTAL + AR_CUSBAL::AGING(LOOP%), 2%) &
		FOR LOOP% = 0% TO 4%

	GOTO 18210 IF INVTOTAL = 0.0

	!
	! Summarize for totals
	!
	FOR I% = 1% TO TOTAL_ACCT%

		GOTO 18220 IF TOTAL(I%)::ACCT = AR_CUSBAL::ACCT

		IF TOTAL(I%)::ACCT > AR_CUSBAL::ACCT
		THEN
			TOTAL(J% + 1%) = TOTAL(J%) &
				FOR J% = TOTAL_ACCT% TO I% STEP -1%
			TOTAL_ACCT% = TOTAL_ACCT% + 1%
			TOTAL(I%)::ACCT = AR_CUSBAL::ACCT
			TOTAL(I%)::SALAMT = 0.0
			TOTAL(I%)::DISAMT = 0.0
			TOTAL(I%)::OTHCHG = 0.0
			TOTAL(I%)::CREDIT = 0.0
			TOTAL(I%)::INVTOTAL = 0.0

			GOTO 18220
		END IF

	NEXT I%

	TOTAL_ACCT%, I% = TOTAL_ACCT% + 1%
	TOTAL(I%)::ACCT = AR_CUSBAL::ACCT
	TOTAL(I%)::SALAMT = 0.0
	TOTAL(I%)::DISAMT = 0.0
	TOTAL(I%)::OTHCHG = 0.0
	TOTAL(I%)::CREDIT = 0.0
	TOTAL(I%)::INVTOTAL = 0.0

18220	TOTAL(I%)::INVTOTAL = TOTAL(I%)::SALAMT + INVTOTAL

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
	! Print out balance
	!
	TEXT$ = "Beginning Balance  " + &
		AR_CUSBAL::ACCT + "          " + &
		FORMAT$(INVTOTAL, "#######.## ") + &
		SPACE$(64%) + &
		FORMAT$(INVTOTAL, "#######.## ")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	CUS_SALAMT = CUS_SALAMT + INVTOTAL

	GOTO 18210

18290	RETURN

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
