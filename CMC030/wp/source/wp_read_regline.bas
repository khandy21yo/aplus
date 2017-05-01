1	%TITLE "WIP Register Line Function"
	%SBTTL "WP_READ_REGLINE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG WP_READ_REGLINE(STRING ORDNUM, STRING LINES, &
		STRING NEXT_GET, &
		WP_REGLINE_CDD WP_REGLINE_READ, REAL QTY())

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
	!	This function returns the quantities of an WIP order that are
	!	being ordered, completed or canceled.
	!	.lm -5
	!
	! Index:
	!
	! Input:
	!
	!	ORDNUM
	!		is an order number
	!
	!	LINES
	!		is a line number
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
	!		2% - Completed (posted)
	!		.le
	!		3% - Canceled (posted)
	!		.le
	!		4% - Completed (in BUYOFF)
	!		.le
	!		5% - Canceled (in BUYOFF)
	!		.le
	!		6% - Balance (incl in journal)
	!		.le
	!		7% - Balance 0% as of today
	!		.le
	!		8% - Balance 6% as of today
	!		.le
	!		9% - Extended Cost
	!		.le
	!		10% - undefined
	!		.els
	!
	! Example:
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_READ_REGLINE/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP WP_READ_REGLINE
	!	$ DELETE WP_READ_REGLINE.OBJ;*
	!
	! Author:
	!
	!	06/11/91 - Val James "Dazed an' Confused" Allen
	!
	! Modification history:
	!
	!	07/16/91 - Craig Tanner
	!		Fixed so that function will not crash when it finds
	!		line types "12" and "13".
	!
	!	08/27/91 - Dan Perkins
	!		Changed ORDNUM variable to internal variable
	!		ORIG_ORDNUM.
	!
	!	09/25/91 - Dan Perkins
	!		Return back more info through  QTY array (balancd...)
	!
	!	07/29/92 - Dan Perkins
	!		Return back Cost on QTY(9%).
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	06/16/98 - Kevin Handy
	!		Make QTY(9) return extended cost instead of cost.
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/31/99 - Kevin Handy
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
	%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.HB"
	MAP	(WP_REGLINE)	WP_REGLINE_CDD	WP_REGLINE

	!
	! Common Statements
	!
	COM (CH_WP_REGLINE_READ) WP_REGLINE.CH%

	DECLARE LONG   EXIT_STATUS
	DECLARE STRING ORIG_ORDNUM
	DECLARE STRING LAST_LIN
	DECLARE STRING REQ_DATE

	%PAGE

	!
	! Set initial value
	!
	IND% = 0%
	QTY(I%) = 0.0 FOR I% = 1% TO 10%
	ORIG_ORDNUM = ORDNUM

	EXIT_STATUS = CMC$_UNDEFINED

	!
	! Open WP_REGLINE file
	!
1000	IF WP_REGLINE.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.OPN"
		USE
			CONTINUE ExitFunction IF ERR = 5%
			FILENAME$ = "WP_REGLINE"
			CONTINUE HelpError
		END WHEN

	END IF

	!
	! Find WP_REGLINE file
	!
2000	WHEN ERROR IN
		RESET #WP_REGLINE.CH%
	USE
		CONTINUE ExitFunction IF ERR = 9%
		FILENAME$ = "WP_REGLINE"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
	!
	! Get WP_REGLINE file
	!
17300	WHEN ERROR IN
		IF EDIT$(NEXT_GET, -1%) = "EQ"
		THEN
			FIND #WP_REGLINE.CH%, &
				KEY #0% EQ ORDNUM + LINES, REGARDLESS
		ELSE
			!
			! GT
			!
			FIND #WP_REGLINE.CH%, &
				KEY #0% GT ORDNUM + LINES, REGARDLESS
		END IF
	USE
		CONTINUE ExitFunction IF ERR = 155%
		FILENAME$ = "WP_REGLINE"
		CONTINUE HelpError
	END WHEN

17320	WHEN ERROR IN
		GET #WP_REGLINE.CH%, REGARDLESS
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE ExitFunction IF ERR = 11%
		FILENAME$ = "WP_REGLINE"
		CONTINUE HelpError
	END WHEN

	GOTO ExitFunction IF (WP_REGLINE::JOB <> ORIG_ORDNUM)

	GOTO ExitFunction IF (WP_REGLINE::LLINE <> LAST_LIN) AND (IND% <> 0%)

	EXIT_STATUS = CMC$_NORMAL

17330	WP_REGLINE_READ = WP_REGLINE IF IND% = 0%
	LAST_LIN = WP_REGLINE::LLINE

	SELECT WP_REGLINE::REC_TYPE

	CASE "01" ! Order
		IND% = 1%
		REQ_DATE = WP_REGLINE::COMP_DATE

	CASE "02" ! Completed
		IND% = 2%
		QTY(9%) = QTY(9%) + WP_REGLINE::COST * WP_REGLINE::QTY

	CASE "03" ! Cancel
		IND% = 3%

	CASE "12" ! Completed in BUYOFF
		IND% = 4%

	CASE "13" ! Canceled in BUYOFF
		IND% = 5%

	CASE ELSE
		IND% = 10%

	END SELECT

	QTY(IND%) = QTY(IND%) + WP_REGLINE::QTY

	!
	! Go for next line
	!
	GOTO 17320

 ExitFunction:
	!
	! Balance - remaining qty
	!
	QTY(0%) = QTY(1%) - QTY(2%) - QTY(3%)
	QTY(0%) = 0.0 IF QTY(0%) < 0.0

	!
	! Balance - remaining qty incl journals qty
	!
	QTY(6%) = QTY(1%) - QTY(2%) - QTY(3%) - QTY(4%) - QTY(5%)
	QTY(6%) = 0.0 IF QTY(6%) < 0.0

	!
	! Back orders
	!
	IF DATE_TODAY > REQ_DATE AND QTY(1%) <> 0.0
	THEN
		QTY(7%) = QTY(0%)
		QTY(8%) = QTY(6%)
	END IF

	WP_READ_REGLINE = EXIT_STATUS
	EXIT FUNCTION

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	EXIT_STATUS = CMC$_UNTERROR
	GOTO ExitFunction

	END FUNCTION
