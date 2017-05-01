1	%TITLE "Calculate Due/Discount date given date, day, days"
	%SBTTL "DATE_DUEDATE"
	%IDENT "V3.6a Calico"

	FUNCTION STRING DATE_DUEDATE(INVOICE_DATE$, DUEDAYS%, DUEDATE$)

	!
	! COPYRIGHT (C) 1993 BY
	! Computer Management Center, Inc.
	! Idaho Falls, Idaho. 83402
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
	!	This function is used to calculate the due date (and
	!	discount date) based upon the invoice date, the due days
	!	information, and the due date information.
	!	.lm -5
	!
	! Index:
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:DATE_DUEDATE/NOLINE
	!	$ LIB FUNC_LIB:CMCFUN/REP DATE_DUEDATE
	!	$ DELETE DATE_DUEDATE.OBJ;*
	!
	! AUTHOR:
	!
	!	02/05/93 - Kevin Handy
	!		Ripped logic out of AP_MAIN_PJ_H because I needed
	!		to do this in several other places, and didn't
	!		want all the repeated code everywhere.
	!
	! MODIFICATION HISTORY:
	!
	!	03/24/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	07/19/93 - Kevin Handy
	!		Modified to handle blank invoice dates without
	!		generating an error.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--
	%PAGE


	OPTION SIZE = (INTEGER LONG, REAL DOUBLE)


	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION DATE_DAYCODE
	EXTERNAL STRING FUNCTION DATE_INVDCODE

	%PAGE

	ON ERROR GO BACK

	DATE_DUE$ = INVOICE_DATE$

	GOTO ExitFunction IF TRM$(INVOICE_DATE$) = ""

	IF DUEDAYS% <> 0%
	THEN
		!
		! Paid so many days after the invoice date.
		!
		DATE_DUE$ = DATE_INVDCODE(DATE_DAYCODE( &
			INVOICE_DATE$) + DUEDAYS%)
	ELSE
		!
		! Assuming that there won't be an error in this VAL.
		!
		GOTO CheckDate IF VAL%(DUEDATE$) = 0%

		!
		! Paid on a specific day of the month
		!
		NEW_MON% = &
			VAL%(MID$(INVOICE_DATE$, 5%, 2%)) + 1%
		NEW_YEAR% = VAL%(MID$(INVOICE_DATE$, 1%, 4%))
		IF NEW_MON% > 12%
		THEN
			NEW_MON% = 1%
			NEW_YEAR% = NEW_YEAR% + 1%
		END IF

		DATE_DUE$ = &
			FORMAT$(NEW_YEAR%, "<0>###") + &
			FORMAT$(NEW_MON%, "<0>#") + &
			FORMAT$(VAL%(DUEDATE$), "<0>#")

	END IF

 CheckDate:
	!
	! Make sure the date exists this month
	!
	IF (DATE_DUE$ <> DATE_INVDCODE(DATE_DAYCODE( &
		DATE_DUE$)))
	THEN
		DATE_DUE$ = LEFT(DATE_DUE$, 6%) + &
			FORMAT$(VAL%(MID(DATE_DUE$, 7%, 2%)) - 1%, "<0>#")
		GOTO CheckDate
	END IF

	DATE_DUEDATE = DATE_DUE$

 ExitFunction:
	END FUNCTION
