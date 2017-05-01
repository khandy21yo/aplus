1	%TITLE "Requistion Summary"
	%SBTTL "WP_READ_REQREGISTER"
	%IDENT "V3.6a Calico"

	FUNCTION LONG WP_READ_REQREGISTER( STRING JOBNUMBER, STRING JOBLINE, &
		STRING REQNUMLIN, STRING NEXT_GET, &
		WP_REQREGISTER_CDD WP_REQREGISTER_READ, REAL QTY())

	!
	! COPYRIGHT (C) 1991 BY
	!
	! Computer Management Center, Inc.
	! Idaho Falls, Idaho
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
	! Computer Management Center assumes no responsibility for the use
	! or reliability of its software on equipment which is not supported
	! by Computer Management Center.
	!
	!++
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This function returns the quantities of a WIP order that are
	!	being ordered, completed or canceled.
	!	.lm -5
	!
	! Index:
	!
	! Input:
	!
	!	JOBNUMBER
	!		is the Job Number in the Register to look for.
	!
	!	JOBLINE
	!		is the Job Line Number is the Register.
	!
	!	REQNUMLIN
	!		is the Requisition Number plus the Requisition Line
	!		that is to be searched for. It may be sent blank in
	!		whole or in part if "GT" is used in NEXT_GET.
	!
	!	NEXT_GET
	!		"EQ" results in the exact specified record being searched
	!		for, "GT" will get the next one.
	!
	! Output:
	!
	!	QTY() is the quantity array
	!		.list 0,""
	!		.le
	!		0% - Balance
	!		.le
	!		1% - On order
	!		.le
	!		2% - Shipped (posted)
	!		.le
	!		3% - Canceled (posted)
	!		.le
	!		4% - Shipped (in journal)
	!		.le
	!		5% - Canceled (in journal)
	!		.le
	!		6% - Balance (incl in journal)
	!		.le
	!		7% - Back Order (posted)
	!		.le
	!		8% - Back Order (incl in journal)
	!		.le
	!		9% - Total extended cost
	!		.le
	!		10% - undefined
	!		.els
	!
	! Example:
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_READ_REQREGISTER/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP WP_READ_REQREGISTER
	!	$ DELETE WP_READ_REQREGISTER.OBJ;*
	!
	! Author:
	!
	!	07/24/91 - Jeff Beard
	!
	! Modification history:
	!
	!	09/25/91 - Dan Perkins
	!		Return back more info through QTY array (balance...).
	!		Modified error trapping.
	!
	!	07/28/92 - Dan Perkins
	!		Return back total ISSUE cost on QTY(9%).
	!
	!	07/29/92 - Dan Perkins
	!		Added GE to NEXT_GET.
	!
	!	09/01/92 - Kevin Handy
	!		Clean up (check)
	!
	!	10/10/92 - Frank F. Starman
	!		Set QTY(0%) to zero if original required
	!		quantity is less or equal to zero.
	!
	!	10/26/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	11/03/92 - Dan Perkins
	!		Commented out CASE 12 and CASE 13 for the time
	!
	!	12/01/92 - Frank F. Starman
	!		Added FUNC_ROUND function
	!
	!	12/14/92 - Frank F. Starman
	!		Added more conditions to force zero balance.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	08/11/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!
	!	03/21/96 - Kevin Handy
	!		Reformat source code.
	!
	!	06/16/98 - Kevin Handy
	!		Change QTY(9) to be total extended cost instead of a
	!		total of the costs.
	!
	!	07/31/98 - Kevin Handy
	!		Type 02 is an extended cost, 01 is unit cost.
	!		Fix calculations to handle that.
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/16/98 - Kevin Handy
	!		Play games with QTY(9) in an attempt to get a
	!		usable sign out of it.
	!
	!	06/26/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Include CDD'S
	!
	%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.HB"
	MAP (WP_REQREGISTER)	WP_REQREGISTER_CDD	WP_REQREGISTER

	!
	! Common Statements
	!
	COM (CH_WP_REQREGISTER_READ) WP_REQREGISTER.CH%

	DECLARE LONG   EXIT_STATUS
	DECLARE STRING REQ_DATE
	DECLARE STRING EQ_JOBLINE

	ON ERROR GOTO 19000

	%PAGE

	!
	! Set initial value
	!
	IND% = 0%
	QTY(I%) = 0.0 FOR I% = 1% TO 10%

	EXIT_STATUS = CMC$_UNDEFINED
	LAST_RECORD$ = ""
	REQ_DATE = DATE_TODAY

	!
	! Open WP_REQREGISTER file
	!
1000	IF WP_REQREGISTER.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.OPN"
		USE
			CONTINUE ExitFunction IF ERR = 5%
			FILENAME$ = "WP_REQREGISTER"
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Find WP_REQREGISTER file
	!
17300	WHEN ERROR IN
		SELECT EDIT$(NEXT_GET, -1%)

		CASE "EQ"
			FIND #WP_REQREGISTER.CH%, KEY #0% EQ JOBNUMBER + &
				JOBLINE + REQNUMLIN, REGARDLESS

			EQ_JOBLINE = JOBLINE

		CASE "GE"
			FIND #WP_REQREGISTER.CH%, KEY #0% GE JOBNUMBER + &
				JOBLINE + REQNUMLIN, REGARDLESS

			EQ_JOBLINE = ""

		CASE "GT"
			FIND #WP_REQREGISTER.CH%, KEY #0% GT JOBNUMBER + &
				JOBLINE + REQNUMLIN, REGARDLESS

			EQ_JOBLINE = ""

		END SELECT
	USE
		CONTINUE ExitFunction IF ERR = 155% OR ERR = 11% OR ERR = 9%
		FILENAME$ = "WP_REQREGISTER"
		CONTINUE HelpError
	END WHEN

	!
	! Get WP_REQREGISTER file
	!
17320	WHEN ERROR IN
		GET #WP_REQREGISTER.CH%, REGARDLESS
	USE
		CONTINUE ExitFunction IF ERR = 11% OR ERR = 155%
		FILENAME$ = "WP_REQREGISTER"
		CONTINUE HelpError
	END WHEN

	IF LAST_RECORD$ <> ""
	THEN
		GOTO ExitFunction IF LAST_RECORD$ <> WP_REQREGISTER::JOB + &
			WP_REQREGISTER::LLINE + WP_REQREGISTER::REQNUM + &
			WP_REQREGISTER::REQLIN
	END IF

	GOTO ExitFunction &
		IF WP_REQREGISTER::JOB <> JOBNUMBER

	GOTO ExitFunction &
		IF WP_REQREGISTER::LLINE <> EQ_JOBLINE AND EQ_JOBLINE <> ""

	EQ_JOBLINE = WP_REQREGISTER::LLINE
	EXIT_STATUS = CMC$_NORMAL

17330	WP_REQREGISTER_READ = WP_REQREGISTER IF IND% = 0%

	LAST_RECORD$ = WP_REQREGISTER::JOB + WP_REQREGISTER::LLINE + &
		WP_REQREGISTER::REQNUM + WP_REQREGISTER::REQLIN

	SELECT WP_REQREGISTER::RECTYP

	CASE "01" ! Order
		IND% = 1%
		REQ_DATE = WP_REQREGISTER::TRANDATE

	CASE "02" ! Issue
		IND% = 2%
		IF (WP_REQREGISTER::QTY >= 0%)
		THEN
			QTY(9%) = QTY(9%) + ABS(WP_REQREGISTER::AMT)
		ELSE
			QTY(9%) = QTY(9%) - ABS(WP_REQREGISTER::AMT)
		END IF

	CASE "03" ! Cancel
		IND% = 3%

	CASE ELSE
		IND% = 10%

	END SELECT

	QTY(IND%) = QTY(IND%) + WP_REQREGISTER::QTY

	!
	! Go for next line
	!
	GOTO 17320

 ExitFunction:

	!
	! Balance - remaining qty to ship
	!
	QTY(0%) = FUNC_ROUND(QTY(1%) - QTY(2%) - QTY(3%), 5%)
	QTY(0%) = 0.0 &
		IF QTY(0%) < 0.0 AND QTY(1%) > 0.0 OR &
		QTY(1%) = 0.0 OR &
		QTY(0%) > 0.0 AND QTY(1%) < 0.0

	!
	! Balance - remaining qty to ship incl journals qty
	!
	!QTY(6%) = FUNC_ROUND(QTY(1%) - QTY(2%) - QTY(3%) - QTY(4%) - QTY(5%), 5%)
	QTY(6%) = QTY(0%)

	!
	! Back Orders
	!
	IF DATE_TODAY > REQ_DATE AND QTY(1%) <> 0.0
	THEN
		QTY(7%) = QTY(0%)
		QTY(8%) = QTY(6%)
	END IF

	WP_READ_REQREGISTER = EXIT_STATUS
	EXIT FUNCTION

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	EXIT_STATUS = CMC$_UNTERROR
	GOTO ExitFunction

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Untrapped error
	!
	RESUME HelpError

	END FUNCTION
