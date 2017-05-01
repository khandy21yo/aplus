1	%TITLE "Accounts Receivable Aging Report"
	%SBTTL "AR_RPRT_AGE02"
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
	! ID:AR030
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Print Accounts Receivable Aging Report\*
	!	option prints an Accounts Receivable aging report.
	!	Using the report settings, a
	!	base is selected from which aging is calculated. The report may
	!	also be set to print accounts in order of
	!	location, customer number,
	!	or customer name.
	!	.b
	!	^*Note:\* If items are not aging as
	!	you think they should, the most
	!	common problems are the cutoff date in the register
	!	and the open item/balance forward in
	!	the customer file (in balance forward,
	!	anything in the register is current, whatever it is dated).
	!	.lm -5
	!
	! Index:
	!	.x Aging Report>Print
	!	.x Report>Aging
	!
	! Option:
	!
	!
	! Input:
	!
	!
	! Output:
	!
	!
	! Example:
	!
	!
	! Author:
	!
	!	03/03/88 - Kevin Handy
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_RPRT_AGE02/LINE
	!	$ LINK/EXECUTABLE=AR_EXE:*.EXE AR_RPRT_AGE02, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_RPRT_AGE02.OBJ;*
	!
	! Modification history:
	!
	!	04/22/88 - Kevin Handy
	!		Added totals to bottom of report
	!
	!	06/26/89 - Kevin Handy
	!		Modified for a cutoff period instead of a
	!		cutoff date.
	!
	!	10/03/91 - Kevin Handy
	!		Modified so that AR_CUSBAL open error is handled
	!		better.
	!
	!	10/28/91 - Kevin Handy
	!		Modified so that FUTURE amounts are added into
	!		the balance instead of causing the total to be
	!		wrong.
	!
	!	10/28/91 - Kevin Handy
	!		Modified so that the grand totals could be printed
	!		without printing any detail.
	!
	!	03/22/92 - Kevin Handy
	!		Clean up (check)
	!
	!	05/29/92 - Dan Perkins
	!		Add future to AR balance.
	!
	!	07/01/92 - Dan Perkins
	!		Display last payment date and payment amount.
	!
	!	08/19/92 - Dan Perkins
	!		Modified to print only past due accounts if desired.
	!
	!	03/10/93 - Kevin Handy
	!		Increased dimension of ARRAY_CUSBAL from 50 to 200
	!		due to FJ crashing with subscript out of range.
	!
	!	06/14/93 - Kevin Handy
	!		Added REGARDLESS and CLOSE to AR_CONTROL.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	05/25/95 - Kevin Handy
	!		Remove extra externals.
	!		Reformat closer to 80 columns.
	!
	!	11/02/95 - Kevin Handy
	!		Modifications to handling of last payment,
	!		so balance file doesn't confuse open item
	!		customers.
	!
	!	12/04/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!
	!	12/04/95 - Kevin Handy
	!		Modified to use new field LAST_PAID to get last
	!		payment, instead of using CREDIT field.
	!
	!	12/04/95 - Kevin Handy
	!		Modified to use AR_CUSBAL::LAST_PA* stuff instead
	!		of only looking through the closed file.
	!
	!	06/19/96 - Kevin Handy
	!		Add new transaction type "11", adjustment.
	!		Reformat source code.
	!
	!	10/15/97 - Kevin Handy
	!		Display cutoff date on top of report.
	!
	!	01/07/98 - Kevin Handy
	!		Oops, cutoff is a period, not a date, so don't
	!		format it using the PRNT_DATE function.
	!
	!	08/10/98 - Kevin Handy
	!		Use '%INCLUDE' instead of '%INCLUDE %FROM %CDD'
	!
	!	08/10/98 - Kevin Handy
	!		Made option to display credit limit.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	05/23/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	02/15/2001 - Kevin Handy
	!		Add a LAST_PAYMENT_FLAG% which is used to disable
	!		adding open information into the last payment
	!		information if it was read from the balance forward
	!		file.
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

	%INCLUDE "SOURCE:[AR.OPEN]AR_CUSBAL.HB"
	MAP (AR_CUSBAL)		AR_CUSBAL_CDD		AR_CUSBAL
	DIM			AR_CUSBAL_CDD		ARRAY_CUSBAL(200%)
	DIM			AR_CUSBAL_CDD		TOTAL_CUSBAL(200%)
	DECLARE			AR_CUSBAL_CDD		GRAND_CUSBAL

	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.HB"
	MAP (AR_CONTROL)	AR_CONTROL_CDD		AR_CONTROL

	%INCLUDE "SOURCE:[AR.OPEN]AR_CLOSED.HB"
	MAP (AR_CLOSED)		AR_CLOSED_CDD		AR_CLOSED

	!
	! External functions
	!
	EXTERNAL LONG		FUNCTION AR_FUNC_AGE

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
	!	The ^*From Item\* field
	!	causes the printing
	!	to begin with the selected Item _#.
	!	The value must be in agreement with
	!	field (03)
	!	Sort by.
	!	.b
	!	A blank field will cause the report to start with the first
	!	Item _# in the file.
	!	.lm -5
	!
	! Index:
	!	.X From>Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.b
	!	.lm +5
	!	A ^*To Item _#\* entered in this
	!	field will cause the printing
	!	to end with
	!	the selected Item _#. The value must
	!	be in agreement with field (03) Sort by.
	!	.b
	!	A blank field will cause the report to end with the last
	!	Item _# in the file.
	!	.lm -5
	!
	! Index:
	!	.x To>Item
	!
	!--

	GL_WILDCARD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)
	GL_WILDCARD$ = "*" IF GL_WILDCARD$ = ""

	!++
	! Abstract:FLD03
	!	^*(03) Account Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Account Wildcard\* field
	!	selects designated items to be printed by entering
	!	a "wildcard" value in this field.
	!	.lm -5
	!
	! Index:
	!	.x Account>Wildcard
	!	.x Wildcard>Account
	!
	!--

	ZERO_BALANCE$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)

	!++
	! Abstract:FLD04
	!	.x Print>Zero
	!	^*(04) Print zero balance\*
	!	.b
	!	.lm +5
	!	The ^*Print zero balance\* field suppresses
	!	the printing of accounts with a zero balance.
	!	.b
	!	Valid codes are:
	!	.table 3,25
	!	.te
	!	^*Y\* - Yes
	!	.te
	!	^*N\* - No
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Zero>Balance
	!	.x Balance>Zero
	!
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	.x Sort by
	!	^*(05) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field determines the order
	!	in which the report will be printed.
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
	!	A setting is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

	AGE_DATE$ = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(5%), -1%))

	!++
	! Abstract:FLD06
	!	.x Age>Date
	!	^*(06) Age as of date\*
	!	.b
	!	.lm +5
	!	The ^*Age as of date\* field enters
	!	the base date from which account balances will be aged.
	!	.B
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!	.x Date>Age
	!
	!--

	CUTOFF$ = EDIT$(UTL_REPORTX::OPTDEF(6%), -1%)

	!++
	! Abstract:FLD07
	!	.x Cutoff>Period
	!	^*(07) Cutoff Period\*
	!	.b
	!	.lm +5
	!	The ^*Cutoff Period\* will be entered
	!	with the date corresponding
	!	to the statement date. No transactions beyond the date entered
	!	will appear on the report.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!	.x Period>Cutoff
	!
	!--

	PASTDUE_ONLY$ = LEFT$(UTL_REPORTX::OPTDEF(8%), 1%)

	!++
	! Abstract:FLD09
	!	.ts 55
	!	^*(09) Past Due	(P,C,F)\*
	!	.b
	!	.lm +5
	!	The ^*Past Due\* field determines what is displayed
	!	about the customers balances
	!	.b
	!	.list 0,"*"
	!	.le
	!	*P Past Due, only past due invoices are displayed.
	!	.le
	!	*F Future column is displayed
	!	.le
	!	*C Customers with Credit Limit problems are displayed.
	!	(Uses Credit Limit defined in the customer file)
	!	.lm -5
	!	.end list
	!
	! Index:
	!
	!--

	TOTALSONLY% = (LEFT(UTL_REPORTX::OPTDEF(9%), 1%) = "Y")

	!++
	! Abstract:FLD10
	!	.x Totals>Aging
	!	^*(10) Totals Only\*
	!
	! Index:
	!	.x Aging>Totals>Only
	!
	!--

	SELECT SORTBY$

	CASE "N"
		K_NUM% = 0%

	CASE "T"
		K_NUM% = 1%

	CASE "C"
		K_NUM% = 2%

	CASE "A"
		K_NUM% = 3%

	END SELECT

300	!
	! Open Customer File
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.OPN"
	USE
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

310	!
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

320	!
	! Open customer balance file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CUSBAL.OPN"
	USE
		CONTINUE 330 IF ERR = 5%
		FILENAME$ = "AR_CUSBAL"
		CONTINUE HelpError
	END WHEN

330	!
	! Open closed itemfile
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CLOSED.OPN"
	USE
		CONTINUE ReportTitle IF ERR = 5%
		FILENAME$ = "AR_CLOSED"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "Aging Report for " + PRNT_DATE(AGE_DATE$, 8%)
	TITLE$(2%) = "AR System"
	TITLE% = 3%

	IF CUTOFF$ <> ""
	THEN
		TITLE$(TITLE%) = "Cutoff Period " + CUTOFF$
		TITLE% = TITLE% + 1%
	END IF

	TITLE$(TITLE%) = ""
	TITLE% = TITLE% + 1%

	!
	! Heading
	!
	INTRVL$(I%) = &
		LEFT(SPACE$(12% - LEN(EDIT$(AR_CONTROL::AGENAM(I%), 140%))) + &
		EDIT$(AR_CONTROL::AGENAM(I%), 140%), 12%) &
			FOR I% = 0% TO 4%

	TITLE$(TITLE%) = LEFT(AR_CONTROL::CTITLE, 10%) + &
		"   Name                           " + &
		"                     Phone" + &
		"             LastPaymentDate       LastAmountPaid"
	TITLE% = TITLE% + 1%

	SELECT PASTDUE_ONLY$
	CASE "Y", "P"
		TITLE$(TITLE%) = "    Account           " + &
			SPACE$(LEN(INTRVL$(0%))) + " " + INTRVL$(1%) + " " + &
			INTRVL$(2%) + " " + INTRVL$(3%) + " " + &
			INTRVL$(4%) + "   ServCharge      Balance"
		TITLE% = TITLE% + 1%

	CASE "C"
		TITLE$(TITLE%) = "    Account           " + &
			SPACE$(LEN(INTRVL$(0%))) + " " + INTRVL$(1%) + " " + &
			INTRVL$(2%) + " " + INTRVL$(3%) + " " + &
			"   ServCharge      Balance    CreditLmt CreditRemain"
		TITLE% = TITLE% + 1%

	CASE ELSE
		TITLE$(TITLE%) = "    Account           " + &
			INTRVL$(0%) + " " + INTRVL$(1%) + " " + &
			INTRVL$(2%) + " " + INTRVL$(3%) + " " + &
			INTRVL$(4%) + "   ServCharge      Balance       Future"
		TITLE% = TITLE% + 1%
	END SELECT

	TITLE$(TITLE%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #AR_35CUSTOM.CH%, &
				KEY #K_NUM%
		ELSE
			FIND #AR_35CUSTOM.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

	LAST_CUSTOM$ = "01234567890123456789"
	TOTAL_CUSBAL% = 0%

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

	LAST_PAYMEMT$ = ""
	LAST_PAYAMT   = 0.0
	LAST_PAYMENT_FLAG% = 0%

	!
	! Collect aging information
	!
	IF AR_FUNC_AGE(AR_35CUSTOM::CUSNUM, AR_35CUSTOM::METHOD, &
		AGE_DATE$, CUTOFF$, NUM_ACCT%, ARRAY_CUSBAL()) <> 0%
	THEN
		!
		! Bitch about aging problems
		!
		TEXT$ = AR_35CUSTOM::CUSNUM + "  " + &
			AR_35CUSTOM::CUSNAM + "  " + &
			PRNT_PHONE(AR_35CUSTOM::PHONE, 0%)

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		TEXT$ = "           *** Unable to age ***"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		GOTO GetNextRec
	END IF

	ARRAY_CUSBAL(I%)::CREDIT = AR_35CUSTOM::CREDITLIM &
		FOR I% = 1% TO NUM_ACCT%

	!
	! Skip if no aging information
	!
	GOTO GetNextRec IF NUM_ACCT% = 0%

	IF AR_35CUSTOM::METHOD = "B"
	THEN
		!
		! Check out last payment junk
		!
		LAST_PAYMENT$ = ARRAY_CUSBAL(1%)::LAST_PAYMENT
		LAST_PAYAMT   = ARRAY_CUSBAL(1%)::LAST_PAID
		LAST_PAYMENT_FLAG% = -1%

		FOR LOOP% = 2% TO NUM_ACCT%

			IF LAST_PAYMENT$ = ARRAY_CUSBAL(LOOP%)::LAST_PAYMENT
			THEN
				LAST_PAYAMT = LAST_PAYAMT + &
					ARRAY_CUSBAL(LOOP%)::LAST_PAID
			END IF

			IF LAST_PAYMENT$ < ARRAY_CUSBAL(LOOP%)::LAST_PAYMENT
			THEN
				LAST_PAYMENT$ = &
					ARRAY_CUSBAL(LOOP%)::LAST_PAYMENT
				LAST_PAYAMT = ARRAY_CUSBAL(LOOP%)::LAST_PAID
			END IF

		NEXT LOOP%
	ELSE
		!
		! Check out last payment junk
		!
		LAST_PAYMENT$ = ARRAY_CUSBAL(1%)::LAST_PAYMENT
		LAST_PAYAMT   = ARRAY_CUSBAL(1%)::LAST_PAID
		LAST_PAYMENT_FLAG% = -1%
	END IF

17100	WHEN ERROR IN
		FIND #AR_CLOSED.CH%, KEY #0% EQ AR_35CUSTOM::CUSNUM, REGARDLESS
	USE
		CONTINUE PrintLine IF ERR = 9% OR ERR = 155%
		FILENAME$ = "AR_CLOSED"
		CONTINUE HelpError
	END WHEN

17120	WHEN ERROR IN
		GET #AR_CLOSED.CH%, REGARDLESS
	USE
		CONTINUE PrintLine IF ERR = 11%
		FILENAME$ = "AR_CLOSED"
		CONTINUE HelpError
	END WHEN

	GOTO PrintLine IF AR_CLOSED::CUSNUM <> AR_35CUSTOM::CUSNUM

	SELECT AR_CLOSED::TRATYP

	CASE "09", "10", "11"
		IF LAST_PAYMENT$ = AR_CLOSED::TRADAT AND &
			LAST_PAYMENT_FLAG% = 0%
		THEN
			LAST_PAYAMT = LAST_PAYAMT - AR_CLOSED::SALAMT
		END IF

		IF LAST_PAYMENT$ < AR_CLOSED::TRADAT
		THEN
			LAST_PAYMENT$ = AR_CLOSED::TRADAT
			LAST_PAYAMT   = -AR_CLOSED::SALAMT
			LAST_PAYMENT_FLAG% = 0%
		END IF
	END SELECT

	GOTO 17120

 PrintLine:
	!
	! Set print flag to zero
	!
	PRINT_FLAG% = 0%

	!
	! Print out aging information
	!
	FOR LOOP% = 1% TO NUM_ACCT%

		GOTO SkipPrint IF COMP_STRING(EDIT$(ARRAY_CUSBAL(LOOP%)::ACCT, &
			-1%), GL_WILDCARD$) = 0% AND GL_WILDCARD$ <> ""

		SELECT PASTDUE_ONLY$
		CASE "Y", "P"
			BALANCE = &
				FUNC_ROUND(ARRAY_CUSBAL(LOOP%)::AGING(1%) + &
				ARRAY_CUSBAL(LOOP%)::AGING(2%) + &
				ARRAY_CUSBAL(LOOP%)::AGING(3%) + &
				ARRAY_CUSBAL(LOOP%)::AGING(4%) + &
				ARRAY_CUSBAL(LOOP%)::CHARGE, 2%)
		CASE "C"
			BALANCE = &
				FUNC_ROUND(ARRAY_CUSBAL(LOOP%)::AGING(0%) + &
				ARRAY_CUSBAL(LOOP%)::AGING(1%) + &
				ARRAY_CUSBAL(LOOP%)::AGING(2%) + &
				ARRAY_CUSBAL(LOOP%)::AGING(3%) + &
				ARRAY_CUSBAL(LOOP%)::AGING(4%) + &
				ARRAY_CUSBAL(LOOP%)::CHARGE + &
				ARRAY_CUSBAL(LOOP%)::FUTURE, 2%)
			GOTO SkipPrint &
				IF (ARRAY_CUSBAL(LOOP%)::CREDIT - BALANCE) >= 0.0

		CASE ELSE
			BALANCE = &
				FUNC_ROUND(ARRAY_CUSBAL(LOOP%)::AGING(0%) + &
				ARRAY_CUSBAL(LOOP%)::AGING(1%) + &
				ARRAY_CUSBAL(LOOP%)::AGING(2%) + &
				ARRAY_CUSBAL(LOOP%)::AGING(3%) + &
				ARRAY_CUSBAL(LOOP%)::AGING(4%) + &
				ARRAY_CUSBAL(LOOP%)::CHARGE + &
				ARRAY_CUSBAL(LOOP%)::FUTURE, 2%)
		END SELECT

		!
		! Code to skip over those that are not wanted
		!
		IF BALANCE = 0.0 AND ZERO_BALANCE$ <> "Y"
		THEN
			GOTO SkipPrint
		END IF

		PRINT_FLAG% = -1%

		IF LAST_CUSTOM$ <> AR_35CUSTOM::CUSNUM
		THEN
			!
			! Print out customer name
			!
			TEXT$ = AR_35CUSTOM::CUSNUM + "   " + &
				AR_35CUSTOM::CUSNAM + "  " + &
				PRNT_PHONE(AR_35CUSTOM::PHONE, 0%) + "     " + &
				PRNT_DATE(LAST_PAYMENT$, 8%) + SPACE$(15%) + &
				FORMAT$(LAST_PAYAMT, "<%>#######.##")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%) &
				UNLESS TOTALSONLY%
			GOTO ExitProgram IF UTL_REPORTX::STAT

			LAST_CUSTOM$ = AR_35CUSTOM::CUSNUM + ""
		END IF

		SELECT PASTDUE_ONLY$
		CASE "Y", "P"
			TEXT$ = "    " + &
				ARRAY_CUSBAL(LOOP%)::ACCT + " " + &
				SPACE$(13%)              + &
				FORMAT$(ARRAY_CUSBAL(LOOP%)::AGING(1%), &
					"########.##  ") + &
				FORMAT$(ARRAY_CUSBAL(LOOP%)::AGING(2%), &
					"########.##  ") + &
				FORMAT$(ARRAY_CUSBAL(LOOP%)::AGING(3%), &
					"########.##  ") + &
				FORMAT$(ARRAY_CUSBAL(LOOP%)::AGING(4%), &
					"########.##  ") + &
				FORMAT$(ARRAY_CUSBAL(LOOP%)::CHARGE, &
					"########.##  ") + &
				FORMAT$(BALANCE, &
					"########.##  ")
		CASE "C"
			TEXT$ = "    " + &
				ARRAY_CUSBAL(LOOP%)::ACCT + " " + &
				FORMAT$(ARRAY_CUSBAL(LOOP%)::AGING(0%), &
					"########.##  ") + &
				FORMAT$(ARRAY_CUSBAL(LOOP%)::AGING(1%), &
					"########.##  ") + &
				FORMAT$(ARRAY_CUSBAL(LOOP%)::AGING(2%), &
					"########.##  ") + &
				FORMAT$(ARRAY_CUSBAL(LOOP%)::AGING(3%) + &
					ARRAY_CUSBAL(LOOP%)::AGING(4%), &
					"########.##  ") + &
				FORMAT$(ARRAY_CUSBAL(LOOP%)::CHARGE, &
					"########.##  ") + &
				FORMAT$(BALANCE, &
					"########.##  ") + &
				FORMAT$(ARRAY_CUSBAL(LOOP%)::CREDIT, &
					"########.##  ") + &
				FORMAT$(ARRAY_CUSBAL(LOOP%)::CREDIT - BALANCE, &
					"########.##")

		CASE ELSE
			TEXT$ = "    " + &
				ARRAY_CUSBAL(LOOP%)::ACCT + " " + &
				FORMAT$(ARRAY_CUSBAL(LOOP%)::AGING(0%), &
					"########.##  ") + &
				FORMAT$(ARRAY_CUSBAL(LOOP%)::AGING(1%), &
					"########.##  ") + &
				FORMAT$(ARRAY_CUSBAL(LOOP%)::AGING(2%), &
					"########.##  ") + &
				FORMAT$(ARRAY_CUSBAL(LOOP%)::AGING(3%), &
					"########.##  ") + &
				FORMAT$(ARRAY_CUSBAL(LOOP%)::AGING(4%), &
					"########.##  ") + &
				FORMAT$(ARRAY_CUSBAL(LOOP%)::CHARGE, &
					"########.##  ") + &
				FORMAT$(BALANCE, &
					"########.##  ") + &
				FORMAT$(ARRAY_CUSBAL(LOOP%)::FUTURE, &
					"########.##")
		END SELECT

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%) &
			UNLESS TOTALSONLY%

		GOTO ExitProgram IF UTL_REPORTX::STAT

		!
		! Summarize for totals
		!
		FOR I% = 1% TO TOTAL_CUSBAL%

			GOTO TotalCusbal &
				IF TOTAL_CUSBAL(I%)::ACCT = &
				ARRAY_CUSBAL(LOOP%)::ACCT

			IF TOTAL_CUSBAL(I%)::ACCT > ARRAY_CUSBAL(LOOP%)::ACCT
			THEN
				TOTAL_CUSBAL(J% + 1%) = TOTAL_CUSBAL(J%) &
					FOR J% = TOTAL_CUSBAL% TO I% STEP -1%

				TOTAL_CUSBAL% = TOTAL_CUSBAL% + 1%
				TOTAL_CUSBAL(I%)::ACCT = &
					ARRAY_CUSBAL(LOOP%)::ACCT

				TOTAL_CUSBAL(I%)::AGING(J%) = 0.0 &
					FOR J% = 0% TO 4%

				TOTAL_CUSBAL(I%)::CHARGE = 0.0
				TOTAL_CUSBAL(I%)::LAST_PAID = 0.0
				TOTAL_CUSBAL(I%)::FUTURE = 0.0
				TOTAL_CUSBAL(I%)::CREDIT = 0.0

				GOTO TotalCusbal
			END IF

		NEXT I%

		TOTAL_CUSBAL%, I% = TOTAL_CUSBAL% + 1%
		TOTAL_CUSBAL(I%)::ACCT = ARRAY_CUSBAL(LOOP%)::ACCT

		TOTAL_CUSBAL(I%)::AGING(J%) = 0.0 &
			FOR J% = 0% TO 4%

		TOTAL_CUSBAL(I%)::CHARGE = 0.0
		TOTAL_CUSBAL(I%)::LAST_PAID = 0.0
		TOTAL_CUSBAL(I%)::FUTURE = 0.0
		TOTAL_CUSBAL(I%)::CREDIT = 0.0

 TotalCusbal:
		TOTAL_CUSBAL(I%)::AGING(J%) = TOTAL_CUSBAL(I%)::AGING(J%) + &
			ARRAY_CUSBAL(LOOP%)::AGING(J%) &
			FOR J% = 0% TO 4%

		TOTAL_CUSBAL(I%)::CHARGE = TOTAL_CUSBAL(I%)::CHARGE + &
			ARRAY_CUSBAL(LOOP%)::CHARGE

		TOTAL_CUSBAL(I%)::LAST_PAID = TOTAL_CUSBAL(I%)::LAST_PAID + &
			ARRAY_CUSBAL(LOOP%)::LAST_PAID

		TOTAL_CUSBAL(I%)::FUTURE = TOTAL_CUSBAL(I%)::FUTURE + &
			ARRAY_CUSBAL(LOOP%)::FUTURE

		TOTAL_CUSBAL(I%)::CREDIT = TOTAL_CUSBAL(I%)::CREDIT + &
			ARRAY_CUSBAL(LOOP%)::CREDIT

 SkipPrint:
	NEXT LOOP%

	IF PRINT_FLAG%
	THEN
		!
		! Add a blank line to seperate the records
		!
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%) &
			UNLESS TOTALSONLY%

		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:
	!
	! Handle end of report
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 10%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
		"*** Grand Totals **", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GRAND_CUSBAL::AGING(J%) = 0.0 &
		FOR J% = 0% TO 4%

	GRAND_CUSBAL::CHARGE = 0.0
	GRAND_CUSBAL::LAST_PAID = 0.0
	GRAND_CUSBAL::FUTURE = 0.0
	GRAND_CUSBAL::CREDIT = 0.0

	FOR LOOP% = 1% TO TOTAL_CUSBAL%

		SELECT PASTDUE_ONLY$
		CASE "Y", "P"
			BALANCE = &
				TOTAL_CUSBAL(LOOP%)::AGING(1%) + &
				TOTAL_CUSBAL(LOOP%)::AGING(2%) + &
				TOTAL_CUSBAL(LOOP%)::AGING(3%) + &
				TOTAL_CUSBAL(LOOP%)::AGING(4%) + &
				TOTAL_CUSBAL(LOOP%)::CHARGE

			TEXT$ = "    " + &
				TOTAL_CUSBAL(LOOP%)::ACCT + " " + &
				SPACE$(13%)              + &
				FORMAT$(TOTAL_CUSBAL(LOOP%)::AGING(1%), &
					"########.##  ") + &
				FORMAT$(TOTAL_CUSBAL(LOOP%)::AGING(2%), &
					"########.##  ") + &
				FORMAT$(TOTAL_CUSBAL(LOOP%)::AGING(3%), &
					"########.##  ") + &
				FORMAT$(TOTAL_CUSBAL(LOOP%)::AGING(4%), &
					"########.##  ") + &
				FORMAT$(TOTAL_CUSBAL(LOOP%)::CHARGE, &
					"########.##  ") + &
				FORMAT$(BALANCE, &
					"########.##  ")
		CASE "C"
			BALANCE = &
				TOTAL_CUSBAL(LOOP%)::AGING(0%) + &
				TOTAL_CUSBAL(LOOP%)::AGING(1%) + &
				TOTAL_CUSBAL(LOOP%)::AGING(2%) + &
				TOTAL_CUSBAL(LOOP%)::AGING(3%) + &
				TOTAL_CUSBAL(LOOP%)::AGING(4%) + &
				TOTAL_CUSBAL(LOOP%)::CHARGE    + &
				TOTAL_CUSBAL(LOOP%)::FUTURE

			TEXT$ = "    " + &
				TOTAL_CUSBAL(LOOP%)::ACCT + " " + &
				FORMAT$(TOTAL_CUSBAL(LOOP%)::AGING(0%), &
					"########.##  ") + &
				FORMAT$(TOTAL_CUSBAL(LOOP%)::AGING(1%), &
					"########.##  ") + &
				FORMAT$(TOTAL_CUSBAL(LOOP%)::AGING(2%), &
					"########.##  ") + &
				FORMAT$(TOTAL_CUSBAL(LOOP%)::AGING(3%) + &
					TOTAL_CUSBAL(LOOP%)::AGING(4%), &
					"########.##  ") + &
				FORMAT$(TOTAL_CUSBAL(LOOP%)::CHARGE, &
					"########.##  ") + &
				FORMAT$(BALANCE, &
					"########.##  ") + &
				FORMAT$(TOTAL_CUSBAL(LOOP%)::CREDIT, &
					"########.##  ") + &
				FORMAT$(TOTAL_CUSBAL(LOOP%)::CREDIT - BALANCE, &
					"########.##")

		CASE ELSE
			BALANCE = &
				TOTAL_CUSBAL(LOOP%)::AGING(0%) + &
				TOTAL_CUSBAL(LOOP%)::AGING(1%) + &
				TOTAL_CUSBAL(LOOP%)::AGING(2%) + &
				TOTAL_CUSBAL(LOOP%)::AGING(3%) + &
				TOTAL_CUSBAL(LOOP%)::AGING(4%) + &
				TOTAL_CUSBAL(LOOP%)::CHARGE    + &
				TOTAL_CUSBAL(LOOP%)::FUTURE

			TEXT$ = "    " + &
				TOTAL_CUSBAL(LOOP%)::ACCT + " " + &
				FORMAT$(TOTAL_CUSBAL(LOOP%)::AGING(0%), &
					"########.##  ") + &
				FORMAT$(TOTAL_CUSBAL(LOOP%)::AGING(1%), &
					"########.##  ") + &
				FORMAT$(TOTAL_CUSBAL(LOOP%)::AGING(2%), &
					"########.##  ") + &
				FORMAT$(TOTAL_CUSBAL(LOOP%)::AGING(3%), &
					"########.##  ") + &
				FORMAT$(TOTAL_CUSBAL(LOOP%)::AGING(4%), &
					"########.##  ") + &
				FORMAT$(TOTAL_CUSBAL(LOOP%)::CHARGE, &
					"########.##  ") + &
				FORMAT$(BALANCE, &
					"########.##  ") + &
				FORMAT$(TOTAL_CUSBAL(LOOP%)::FUTURE, &
					"########.##")
		END SELECT

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		GRAND_CUSBAL::AGING(J%) = GRAND_CUSBAL::AGING(J%) + &
			TOTAL_CUSBAL(LOOP%)::AGING(J%) &
			FOR J% = 0% TO 4%

		GRAND_CUSBAL::CHARGE = GRAND_CUSBAL::CHARGE + &
			TOTAL_CUSBAL(LOOP%)::CHARGE

		GRAND_CUSBAL::LAST_PAID = GRAND_CUSBAL::LAST_PAID + &
			TOTAL_CUSBAL(LOOP%)::LAST_PAID

		GRAND_CUSBAL::FUTURE = GRAND_CUSBAL::FUTURE + &
			TOTAL_CUSBAL(LOOP%)::FUTURE

		GRAND_CUSBAL::CREDIT = GRAND_CUSBAL::CREDIT + &
			TOTAL_CUSBAL(LOOP%)::CREDIT

	NEXT LOOP%

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	SELECT PASTDUE_ONLY$
	CASE "Y", "P"
		BALANCE = &
			GRAND_CUSBAL::AGING(1%) + &
			GRAND_CUSBAL::AGING(2%) + &
			GRAND_CUSBAL::AGING(3%) + &
			GRAND_CUSBAL::AGING(4%) + &
			GRAND_CUSBAL::CHARGE

		TEXT$ = "    " + &
			"Grand Total        " + &
			SPACE$(13%)              + &
			FORMAT$(GRAND_CUSBAL::AGING(1%), &
				"########.##  ") + &
			FORMAT$(GRAND_CUSBAL::AGING(2%), &
				"########.##  ") + &
			FORMAT$(GRAND_CUSBAL::AGING(3%), &
				"########.##  ") + &
			FORMAT$(GRAND_CUSBAL::AGING(4%), &
				"########.##  ") + &
			FORMAT$(GRAND_CUSBAL::CHARGE, &
				"########.##  ") + &
			FORMAT$(BALANCE, &
				"########.##  ")
	CASE "C"
		BALANCE = &
			GRAND_CUSBAL::AGING(0%) + &
			GRAND_CUSBAL::AGING(1%) + &
			GRAND_CUSBAL::AGING(2%) + &
			GRAND_CUSBAL::AGING(3%) + &
			GRAND_CUSBAL::AGING(4%) + &
			GRAND_CUSBAL::CHARGE    + &
			GRAND_CUSBAL::FUTURE

		TEXT$ = "    " + &
			"Grand Total        " + &
			FORMAT$(GRAND_CUSBAL::AGING(0%), &
				"########.##  ") + &
			FORMAT$(GRAND_CUSBAL::AGING(1%), &
				"########.##  ") + &
			FORMAT$(GRAND_CUSBAL::AGING(2%), &
				"########.##  ") + &
			FORMAT$(GRAND_CUSBAL::AGING(3%) + &
				GRAND_CUSBAL::AGING(4%), &
				"########.##  ") + &
			FORMAT$(GRAND_CUSBAL::CHARGE, &
				"########.##  ") + &
			FORMAT$(BALANCE, &
				"########.##  ") + &
			FORMAT$(GRAND_CUSBAL::CREDIT, &
				"########.##  ") + &
			FORMAT$(GRAND_CUSBAL::CREDIT - BALANCE, &
				"########.##")
	CASE ELSE
		BALANCE = &
			GRAND_CUSBAL::AGING(0%) + &
			GRAND_CUSBAL::AGING(1%) + &
			GRAND_CUSBAL::AGING(2%) + &
			GRAND_CUSBAL::AGING(3%) + &
			GRAND_CUSBAL::AGING(4%) + &
			GRAND_CUSBAL::CHARGE + &
			GRAND_CUSBAL::FUTURE

		TEXT$ = "    " + &
			"Grand Total        " + &
			FORMAT$(GRAND_CUSBAL::AGING(0%), &
				"########.##  ") + &
			FORMAT$(GRAND_CUSBAL::AGING(1%), &
				"########.##  ") + &
			FORMAT$(GRAND_CUSBAL::AGING(2%), &
				"########.##  ") + &
			FORMAT$(GRAND_CUSBAL::AGING(3%), &
				"########.##  ") + &
			FORMAT$(GRAND_CUSBAL::AGING(4%), &
				"########.##  ") + &
			FORMAT$(GRAND_CUSBAL::CHARGE, &
				"########.##  ") + &
			FORMAT$(BALANCE, &
				"########.##  ") + &
			FORMAT$(GRAND_CUSBAL::FUTURE, &
				"########.##")
	END SELECT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
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
