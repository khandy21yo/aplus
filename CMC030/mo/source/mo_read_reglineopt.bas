1	%TITLE "Register Line Function"
	%SBTTL "MO_READ_REGLINEOPT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG MO_READ_REGLINEOPT(STRING ORDNUM, STRING LINES, &
		STRING OPTLINE, STRING NEXT_GET, &
		MO_REGLINEOPT_CDD MO_REGLINEOPT_READ, REAL QTY())

	!
	! COPYRIGHT (C) 1992 BY
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
	!	This function returns the quantities of an order that are
	!	being shipped, ordered, or canceled.
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
	!	OPTLINE
	!		is an option line number
	!
	!	NEXT_GET
	!		is passed as "GT" or "EQ" and it means weather the
	!		program is to get the record asked for or the next one.
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
	!		10% - undefined
	!		.els
	!
	! Example:
	!
	! Compile:
	!
	!	$ BAS MO_SOURCE:MO_READ_REGLINEOPT/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP MO_READ_REGLINEOPT
	!	$ DELETE MO_READ_REGLINEOPT.OBJ;*
	!
	! Author:
	!
	!	10/06/92 - Dan Perkins
	!		Copied from MO_READ_REGLINE.
	!
	! Modification history:
	!
	!	10/26/92 - Kevin Handy
	!		Cleaned up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/01/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	07/31/2000 - Kevin Handy
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
	%INCLUDE "SOURCE:[MO.OPEN]MO_REGLINEOPT.HB"
	MAP (MO_REGLINEOPT)	MO_REGLINEOPT_CDD	MO_REGLINEOPT

	!
	! Common Statements
	!
	COM (READ_MO_REGLINEOPT) MO_REGLINEOPT.CH%

	DECLARE LONG   EXIT_STATUS
	DECLARE STRING ORIG_ORDNUM
	DECLARE STRING LAST_LIN
	DECLARE STRING LAST_OPTLIN

	%PAGE

	!
	! Set initial value
	!
	IND% = 0%
	QTY(I%) = 0.0 FOR I% = 1% TO 10%
	ORIG_ORDNUM = ORDNUM
	EXIT_STATUS = CMC$_UNDEFINED

	!
	! Open MO_REGLINEOPT file
	!
1000	IF MO_REGLINEOPT.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[MO.OPEN]MO_REGLINEOPT.OPN"
		USE
			CONTINUE ExitFunction IF ERR = 5%
			FILENAME$ = "MO_REGLINEOPT"
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Get MO_REGLINEOPT file
	!
17300	WHEN ERROR IN
		IF EDIT$(NEXT_GET, -1%) = "EQ"
		THEN
			FIND #MO_REGLINEOPT.CH%, KEY #0% EQ ORDNUM + LINES + &
				OPTLINE, REGARDLESS
		ELSE
			FIND #MO_REGLINEOPT.CH%, KEY #0% GT ORDNUM + LINES + &
				OPTLINE, REGARDLESS
		END IF
	USE
		CONTINUE ExitFunction IF ERR = 155%
		FILENAME$ = "MO_REGLINEOPT"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17320	WHEN ERROR IN
		GET #MO_REGLINEOPT.CH%, REGARDLESS
	USE
		CONTINUE ExitFunction IF ERR = 11%
		FILENAME$ = "MO_REGLINEOPT"
		CONTINUE HelpError
	END WHEN

	GOTO ExitFunction IF MO_REGLINEOPT::ORDNUM <> ORIG_ORDNUM

	GOTO ExitFunction IF MO_REGLINEOPT::LIN <> LINES

	GOTO ExitFunction IF (MO_REGLINEOPT::OPTLIN <> LAST_OPTLIN) &
		AND (IND% <> 0%)

	EXIT_STATUS = CMC$_NORMAL

17330	MO_REGLINEOPT_READ = MO_REGLINEOPT IF IND% = 0%
	LAST_LIN = MO_REGLINEOPT::LIN
	LAST_OPTLIN = MO_REGLINEOPT::OPTLIN

	SELECT MO_REGLINEOPT::TRANTYPE

	CASE "01" ! Order
		IND% = 1%

	CASE "02" ! Ship
		IND% = 2%

	CASE "03" ! Cancel
		IND% = 3%

	CASE "12"
		IND% = 4%

	CASE "13"
		IND% = 5%

	CASE ELSE
		IND% = 10%

	END SELECT

	QTY(IND%) = QTY(IND%) + MO_REGLINEOPT::QTY

	!
	! Go for next line
	!
	GOTO GetNextRec

 ExitFunction:
	!
	! Balance - remaining qty to ship
	!
	QTY(0%) = QTY(1%) - QTY(2%) - QTY(3%)
	QTY(0%) = 0.0 IF FUNC_ROUND(QTY(0%), 3%) <= 0.0

	!
	! Balance - remaining qty to ship incl journals qty
	!
	QTY(6%) = QTY(1%) - QTY(2%) - QTY(3%) - QTY(4%) - QTY(5%)
	QTY(6%) = 0.0 IF FUNC_ROUND(QTY(6%), 3%) <= 0.0

	MO_READ_REGLINEOPT = EXIT_STATUS

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
