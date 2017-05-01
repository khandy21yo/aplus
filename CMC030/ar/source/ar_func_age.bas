1	%TITLE "Accounts Receivable Aging Subroutine"
	%SBTTL "AR_FUNC_AGE"
	%IDENT "V3.6a Calico"

	FUNCTION INTEGER AR_FUNC_AGE(CUSNUM$, &
		CUSTOM.METHOD$, &
		BASE_DAY$, &
		CUTOFF$, &
		NUM_ACCT%, &
		AR_CUSBAL_CDD ARRAY_CUSBAL())

	!
	! COPYRIGHT (C) 1988 BY
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
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This function is used to calculate the aging
	!	information for one customer.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Input:
	!
	!	CUSNUM$ - Customer number to age.
	!
	!	CUSTOM.METHOD$ - Method B-balance foreward, O-open item.
	!
	!	BASE_DAY$ - Date to age to.
	!
	!	CUTOFF$ - Cutoff period. (Pretends that anything updated
	!		after this date does not yet exist).  This is
	!		used for a cutoff register.  Based on the general
	!		ledger period updated to.
	!
	! Output:
	!
	!	NUM_ACCT% - Number of account groupings for this
	!		customer.
	!
	!	ARRAY_CUSBAL - Array of account aging information.
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_FUNC_AGE/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP AR_FUNC_AGE
	!	$ DELETE AR_FUNC_AGE.OBJ;*
	!
	! Author:
	!
	!	03/03/88 - Kevin Handy
	!
	! Modification history:
	!
	!	05/14/88 - Lance Williams
	!		Clean up the header.
	!
	!	09/11/91 - Dan Perkins
	!		Modified error trapping.
	!
	!	10/29/91 - Kevin Handy
	!		Modified to trap CUSBAL NOT OPEN errors.
	!
	!	11/09/91 - Frank F. Starman
	!		Open the files in this function, that there is no
	!		need to hassle with opening in the main program.
	!
	!	02/08/92 - Kevin Handy
	!		Due to Franks last change, the close program did
	!		not work (obviously well tested change), and the
	!		close program was unable to work.
	!
	!	03/17/92 - Frank F. Starman
	!		Add abbility to have invoice with more than one due day.
	!
	!	03/22/92 - Kevin Handy
	!		Clean up (check)
	!
	!	07/02/92 - Dan Perkins
	!		Move credits to current for every line instead
	!		of in summary.
	!
	!	08/14/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/26/92 - Dan Perkins
	!		Use NUM_ACCT% argument to see if we need to calculate
	!		service charges by passing -1%.
	!
	!	01/25/92 - Frank F. Starman
	!		Apply credits against the future amount if any.
	!
	!	02/02/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	03/10/93 - Kevin Handy
	!		Modified so that invoice type "02" is not aged,
	!		because it doesn't have an AR account and it
	!		really confuses the aging report.
	!
	!	04/19/93 - Kevin Handy
	!		Modified to use DUEDATE for aging if it is
	!		available, else use TRADAT.
	!
	!	06/24/93 - Kevin Handy
	!		Modified so that anything with a future due
	!		date within current period will be current,
	!		but invoices with future dates are future.
	!
	!	07/02/93 - Kevin Handy
	!		Modified aging method for DueDate items.
	!		Now I am just shifting the aging for DueDate
	!		invoices by one period, instead of spreading
	!		the current column 30 days in both directions.
	!
	!	07/06/93 - Kevin Handy
	!		Re-enabled the Balance Foreward that Frank
	!		commented out sime time ago without putting
	!		any comment about it.
	!
	!	07/07/93 - Kevin Handy
	!		Re-arrange code to make it more obvious what
	!		happens to the payments when there is more than
	!		one due-date. (Gets 1st invoice date)
	!
	!	01/26/94 - Kevin Handy
	!		Modifications to handle last service charge
	!		field (and not leave it full of garbage).
	!
	!	03/08/94 - Kevin Handy
	!		Modified so it doesn't screw around with the
	!		credit limits.
	!
	!	03/08/94 - Kevin Handy
	!		Formatted to 80 columns.
	!
	!	03/08/94 - Kevin Handy
	!		Modified to always use AR_OPEN::SALAMT instead
	!		of working through a seperate variable (easier to
	!		follow this way).
	!
	!	04/12/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico Standards.
	!		Lose excess parameter on ASSG_FREECHANNEL.
	!
	!	05/25/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!
	!	10/16/95 - Kevin Handy
	!		Modifications to handle YTD Service and YTD Sales.
	!
	!	12/04/95 - Kevin Handy
	!		Changed field PTDSALES to LAST_PAID, and stored
	!		the last payment therin, instead of in the CREDIT
	!		field.
	!
	!	06/19/96 - Kevin Handy
	!		Added transaction type "11".
	!
	!	09/18/96 - Kevin Handy
	!		Threw in a lot of "REGARDLESS", to see if we
	!		can speed up the aging report for LL.
	!
	!	10/15/97 - Kevin Handy
	!		Don't apply current payments to future amounts.
	!
	!	09/10/98 - Kevin Handy
	!		Lose a bunch of commented out code
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	08/07/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[AR.OPEN]AR_CUSBAL.HB"
	MAP	(AR_CUSBAL)	AR_CUSBAL_CDD	AR_CUSBAL

	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.HB"
	MAP	(AR_CONTROL)	AR_CONTROL_CDD	AR_CONTROL

	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.HB"
	MAP	(AR_OPEN)	AR_OPEN_CDD	AR_OPEN

	COM (CH_AR_CUSBAL) AR_CUSBAL.CH%
	COM (CH_AR_OPEN) AR_OPEN.CH%
	COM (CH_AR_CONTROL) AR_CONTROL.CH%

	%PAGE

	!
	! Force the method to be open item if is is not balance foreward,
	! since it is easier to change an open item customer to a
	! balance foreward than the other way around.
	!
	METHOD$ = CUSTOM.METHOD$
	METHOD$ = "O" IF METHOD$ <> "B"

	!
	! Assume zilch for initil aging
	!
	NUM_ACCT% = 0%

300	IF AR_CUSBAL.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_CUSBAL.OPN"
		USE
			CONTINUE 310 IF ERR = 5%
			FILENAME$ = "AR_CUSBAL"
			CONTINUE HelpError
		END WHEN
	END IF

310	IF AR_OPEN.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.OPN"
		USE
			CONTINUE 320 IF ERR = 5%
			FILENAME$ = "AR_OPEN"
			CONTINUE HelpError
		END WHEN
	END IF

320	IF AR_CONTROL.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.OPN"
			GET #AR_CONTROL.CH%, RECORD 1%, REGARDLESS
			CLOSE AR_CONTROL.CH%
		USE
			CONTINUE 1000 IF ERR = 5% OR ERR = 155%
			FILENAME$ = "AR_CONTROL"
			CONTINUE HelpError
		END WHEN

		CALL ASSG_FREECHANNEL(AR_CONTROL.CH%)
	END IF

1000	!******************************************************************
	! Handle CUSBAL file.  I am assuming that it will come in in sorted
	! order, since RMS should be handling that.
	!******************************************************************

	WHEN ERROR IN
		FIND #AR_CUSBAL.CH%, &
			KEY #0% GE CUSNUM$, &
			REGARDLESS
	USE
		CONTINUE 2000 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "AR_CUSBAL"
		CONTINUE HelpError
	END WHEN

	FFLAG% = 0%

1010	!
	! Pull in record
	!
	WHEN ERROR IN
		GET #AR_CUSBAL.CH%, REGARDLESS
	USE
		CONTINUE 2000 IF ERR = 11%
		FILENAME$ = "AR_CUSBAL"
		CONTINUE HelpError
	END WHEN

	!
	! Exit if not right customer
	!
	GOTO 2000 IF AR_CUSBAL::CUSNUM <> CUSNUM$

	!
	! Allocate an account
	!
	NUM_ACCT% = NUM_ACCT% + 1%
	ARRAY_CUSBAL(NUM_ACCT%) = AR_CUSBAL

	IF METHOD$ = "B"
	THEN
		!
		! If this is balance foreward, then rotate the aging
		! information one period foreward, so that current
		! information may be placed in age(0).
		!
		ARRAY_CUSBAL(NUM_ACCT%)::AGING(4%) = &
			ARRAY_CUSBAL(NUM_ACCT%)::AGING(4%) + &
			ARRAY_CUSBAL(NUM_ACCT%)::AGING(3%)

		ARRAY_CUSBAL(NUM_ACCT%)::AGING(I%) = &
			ARRAY_CUSBAL(NUM_ACCT%)::AGING(I% - 1%) &
			FOR I% = 3% TO 1% STEP -1%

		ARRAY_CUSBAL(NUM_ACCT%)::AGING(0%) = 0.0
		ARRAY_CUSBAL(NUM_ACCT%)::FUTURE = 0.0
	ELSE
		!
		! If this is an open item customer, the remove all
		! of the useless information that is in the balance
		! record so it can be re-computed,
		!
		ARRAY_CUSBAL(NUM_ACCT%)::AGING(I%) = 0.0 &
			FOR I% = 0% TO 4%

		ARRAY_CUSBAL(NUM_ACCT%)::FUTURE = 0.0
		ARRAY_CUSBAL(NUM_ACCT%)::CHARGE = 0.0
	END IF

	GOTO 1010

	%PAGE

2000	!******************************************************************
	! Handle AROPEN records
	!******************************************************************

	WHEN ERROR IN
		FIND #AR_OPEN.CH%, &
			KEY #0% GE CUSNUM$, &
			REGARDLESS
	USE
		CONTINUE 3000 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

	TEST_INVNUM$ = "12345678901234567890"	! Too long to be a valid one
	FFLAG% = -1%

2010	!
	! Get AR_OPEN record
	!
	WHEN ERROR IN
		GET #AR_OPEN.CH%, REGARDLESS
	USE
		CONTINUE 3000 IF ERR = 11%
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

	!
	! Skip out if done with this customer number
	!
	GOTO 3000 IF AR_OPEN::CUSNUM <> CUSNUM$

	!
	! Ignore item if update date is too late
	!
	GOTO 2010 IF (LEFT(AR_OPEN::UPDATED, 6%) > LEFT(CUTOFF$, 6%)) &
		AND CUTOFF$ > "00000000"

	GOTO 2010 IF AR_OPEN::TRATYP = "02"

	!
	! Handle balance foreward item (B)
	!
2100	IF METHOD$ = "B"
	THEN
		!
		! Only switch accounts when invoice number changes
		!
		IF AR_OPEN::INVNUM <> TEST_INVNUM$
		THEN
			!
			! Allocate an account
			!
			TEST_INVNUM$ = AR_OPEN::INVNUM + ""
			TEST_ACCT$ = AR_OPEN::ARACCT
			GOSUB 8000
		END IF

		!
		! Age service charge into service charge column
		!
		IF AR_OPEN::TRATYP = "04"
		THEN
			ARRAY_CUSBAL(CURACC%)::CHARGE = &
				ARRAY_CUSBAL(CURACC%)::CHARGE + &
				AR_OPEN::SALAMT

			IF ARRAY_CUSBAL(CURACC%)::LAST_CHARGE > AR_OPEN::TRADAT
			THEN
				ARRAY_CUSBAL(CURACC%)::LAST_CHARGE = &
					AR_OPEN::TRADAT
			END IF
			GOTO 2010
		END IF

		!
		! Age Payments to oldest (starting at service charge),
		! invoices to current.
		!
		IF INSTR(1%, "08,09,10,11", AR_OPEN::TRATYP)
		THEN
			ARRAY_CUSBAL(CURACC%)::CHARGE = &
				ARRAY_CUSBAL(CURACC%)::CHARGE + &
				AR_OPEN::SALAMT

			IF ARRAY_CUSBAL(CURACC%)::LAST_PAYMENT < AR_OPEN::TRADAT
			THEN
				ARRAY_CUSBAL(CURACC%)::LAST_PAYMENT = &
					AR_OPEN::TRADAT
				ARRAY_CUSBAL(CURACC%)::LAST_PAID = 0.0
			END IF

			IF ARRAY_CUSBAL(CURACC%)::LAST_PAYMENT = AR_OPEN::TRADAT
			THEN
				ARRAY_CUSBAL(CURACC%)::LAST_PAID = &
					ARRAY_CUSBAL(CURACC%)::LAST_PAID + &
					-AR_OPEN::SALAMT
			END IF
		ELSE
			ARRAY_CUSBAL(CURACC%)::AGING(0%) = &
				ARRAY_CUSBAL(CURACC%)::AGING(0%) + &
				AR_OPEN::SALAMT
		END IF

		GOTO 2010
	END IF

	!
	! Handle open item (O)
	!
	IF AR_OPEN::INVNUM <> TEST_INVNUM$ OR AR_OPEN::TRATYP = "01"
	THEN

		!
		! Calculate the age of this invoice
		!
		!
		! Using a due date, we want to age them one
		! period ahead of where they will be than
		! if we aged them from the invoice date.
		!
		! Example: On a statement dated 07/01/93,
		! a invoice date of 07/10/93 would be future
		! by one month, and an invoice of 06/30/93
		! would be current, but a due date of 07/10/93
		! would be current, and a due date of 06/10/93
		! would be past due.
		!
		! Hope this method now makes everyone happy.
		!
		IF AR_OPEN::DUEDATE > "00000000"
		THEN
			USEDAY$ = AR_OPEN::DUEDATE
			AGING.DAY% = DATE_DAYCODE(BASE_DAY$) - &
				DATE_DAYCODE(USEDAY$) + &
				AR_CONTROL::AGEPER(0%)
		ELSE
			USEDAY$ = AR_OPEN::TRADAT
			AGING.DAY% = DATE_DAYCODE(BASE_DAY$) - &
				DATE_DAYCODE(USEDAY$)
		END IF


		!
		! Test for future date
		!
		IF AGING.DAY% < 0.0
		THEN
			AGEFLAG% = 1%
		ELSE
			!
			! If it is service charge, than handle it properly
			!
			IF AR_OPEN::TRATYP = "04"
			THEN
				AGEFLAG% = 2%
			ELSE
				!
				! Calculate aging period
				!
				AGEDAY% = AR_CONTROL::AGEPER(0%)
				D% = 0%

				WHILE (AGING.DAY% > AGEDAY%) AND (D% < 3%)
					D% = D% + 1%
					AGEDAY% = AGEDAY% + &
						AR_CONTROL::AGEPER(D%)
				NEXT

				D% = 4% IF AGING.DAY% > AGEDAY%
				AGEFLAG% = 3%
			END IF
		END IF

		!
		! Allocate an account if this is the first record
		! of this invoice.
		!
		IF AR_OPEN::INVNUM <> TEST_INVNUM$
		THEN
			TEST_ACCT$ = AR_OPEN::ARACCT
			TEST_INVNUM$ = AR_OPEN::INVNUM + ""

			GOSUB 8000

			!
			! Remember the first date found
			!
			FIRST.D% = D%
			FIRST.AGE% = AGEFLAG%
		END IF
	ELSE
		!
		! Anything that is not an invoice gets dated as of the
		! first date found, not the last date found.
		!
		AGEFLAG% = FIRST.AGE%
		D% = FIRST.D%
	END IF

	!
	! Handle adding one invoice item
	!
	SELECT AGEFLAG%

	!
	! Future
	!
	CASE 1%
		ARRAY_CUSBAL(CURACC%)::FUTURE = &
			ARRAY_CUSBAL(CURACC%)::FUTURE + AR_OPEN::SALAMT

	!
	! Service Charge
	!
	CASE 2%
		ARRAY_CUSBAL(CURACC%)::CHARGE = &
			ARRAY_CUSBAL(CURACC%)::CHARGE + AR_OPEN::SALAMT

	!
	! Period
	!
	CASE 3%
		ARRAY_CUSBAL(CURACC%)::AGING(D%) = &
			ARRAY_CUSBAL(CURACC%)::AGING(D%) + AR_OPEN::SALAMT

	END SELECT

	SELECT AR_OPEN::TRATYP

	CASE "01", "02"

		!
		! Total up YTD Sales
		!
		IF LEFT(AR_OPEN::TRADAT, 4%) = LEFT(BASE_DAY$, 4%)
		THEN
			ARRAY_CUSBAL(CURACC%)::YTDSALES = &
				ARRAY_CUSBAL(CURACC%)::YTDSALES + &
				AR_OPEN::SALAMT
		END IF

	CASE "04"

		!
		! Total up YTD Service
		!
		IF LEFT(AR_OPEN::TRADAT, 4%) = LEFT(BASE_DAY$, 4%)
		THEN
			ARRAY_CUSBAL(CURACC%)::YTDSERVICE = &
				ARRAY_CUSBAL(CURACC%)::YTDSERVICE + &
				AR_OPEN::SALAMT
		END IF

	CASE "09", "10", "11"
		IF ARRAY_CUSBAL(CURACC%)::LAST_PAYMENT = AR_OPEN::TRADAT
		THEN
			ARRAY_CUSBAL(CURACC%)::LAST_PAID = &
				ARRAY_CUSBAL(CURACC%)::LAST_PAID - &
				AR_OPEN::SALAMT
		END IF

		IF ARRAY_CUSBAL(CURACC%)::LAST_PAYMENT < AR_OPEN::TRADAT
		THEN
			ARRAY_CUSBAL(CURACC%)::LAST_PAYMENT = &
				AR_OPEN::TRADAT
			ARRAY_CUSBAL(CURACC%)::LAST_PAID = -AR_OPEN::SALAMT
		END IF

	END SELECT

2300	GOTO 2010


3000	!
	! Loop thru all accounts and clean up age balance
	!
	FOR CURACC% = 1% TO NUM_ACCT%

		!
		! Find all credits
		!
		CREDIT = 0.0

		IF (ARRAY_CUSBAL(CURACC%)::CHARGE < 0.0) AND (METHOD$ = "B")
		THEN
			CREDIT = ARRAY_CUSBAL(CURACC%)::CHARGE
			ARRAY_CUSBAL(CURACC%)::CHARGE = 0.0
		END IF

		FOR LOOP% = 0% TO 4%
			IF ARRAY_CUSBAL(CURACC%)::AGING(LOOP%) < 0.0
			THEN
				CREDIT = CREDIT + &
					ARRAY_CUSBAL(CURACC%)::AGING(LOOP%)
				ARRAY_CUSBAL(CURACC%)::AGING(LOOP%) = 0.0
			END IF
		NEXT LOOP%

		!
		! Apply credits to service charge first
		!
		IF (ARRAY_CUSBAL(CURACC%)::CHARGE <> 0.0) AND (METHOD$ = "B")
		THEN
			ARRAY_CUSBAL(CURACC%)::CHARGE = &
				ARRAY_CUSBAL(CURACC%)::CHARGE + CREDIT

			IF ARRAY_CUSBAL(CURACC%)::CHARGE > 0.0
			THEN
				CREDIT = 0.0
			ELSE
				CREDIT = ARRAY_CUSBAL(CURACC%)::CHARGE
				ARRAY_CUSBAL(CURACC%)::CHARGE = 0.0
			END IF
		END IF

		!
		! Take any payment amount from the last period
		! and apply it to all previous balances.
		!
		FOR LOOP% = 4% TO 1% STEP -1%
			IF ARRAY_CUSBAL(CURACC%)::AGING(LOOP%) <> 0.0
			THEN
				ARRAY_CUSBAL(CURACC%)::AGING(LOOP%) = &
					ARRAY_CUSBAL(CURACC%)::AGING(LOOP%) + &
					CREDIT

				IF ARRAY_CUSBAL(CURACC%)::AGING(LOOP%) > 0.0
				THEN
					CREDIT = 0.0
				ELSE
					CREDIT = ARRAY_CUSBAL(CURACC%)::AGING(LOOP%)
					ARRAY_CUSBAL(CURACC%)::AGING(LOOP%) = 0.0
				END IF
			END IF
		NEXT LOOP%

		!
		! Now add credit to current
		!
		ARRAY_CUSBAL(CURACC%)::AGING(0%) = &
			ARRAY_CUSBAL(CURACC%)::AGING(0%) + CREDIT

	NEXT CURACC%

	!
	! Return a successful flag
	!
	AR_FUNC_AGE = 0%

4000	EXIT FUNCTION

	%PAGE

8000	!******************************************************************
	! Search for a spot for the account TEMP.ACC$, and create it
	! if it is not found in the list already.
	!******************************************************************

	!
	! Already exists in list?
	!
	RETURN IF ARRAY_CUSBAL(CURACC%)::ACCT = TEST_ACCT$ &
		FOR CURACC% = 1% TO NUM_ACCT%

	!
	! Add to list
	!
	CURACC%, NUM_ACCT% = NUM_ACCT% + 1%
	ARRAY_CUSBAL(CURACC%)::CUSNUM = CUSNUM$
	ARRAY_CUSBAL(CURACC%)::ACCT = TEST_ACCT$
	ARRAY_CUSBAL(CURACC%)::CREDIT = 0.0
	ARRAY_CUSBAL(CURACC%)::FUTURE = 0.0
	ARRAY_CUSBAL(CURACC%)::AGING(X%) = 0.0 FOR X% = 0% TO 4%
	ARRAY_CUSBAL(CURACC%)::YTDSERVICE = 0.0
	ARRAY_CUSBAL(CURACC%)::LAST_PAID = 0.0
	ARRAY_CUSBAL(CURACC%)::YTDSALES = 0.0
	ARRAY_CUSBAL(CURACC%)::CHARGE = 0.0
	ARRAY_CUSBAL(CURACC%)::LAST_PAYMENT = ""
	ARRAY_CUSBAL(CURACC%)::LAST_UPDATE = ""
	ARRAY_CUSBAL(CURACC%)::LAST_CHARGE = ""

	RETURN

 HelpError:
	!
	! Untrapped error
	!
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_3MESSAGE(SCOPE, FILENAME$ + " " + &
		NUM1$(ERL) + " " + ERT$(ERR), &
		"ERR", ERN$, "ERROR" + NUM1$(ERR))
	AR_FUNC_AGE = -1%
	GOTO 4000

	END FUNCTION
