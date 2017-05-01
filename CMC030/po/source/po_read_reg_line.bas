1	%TITLE "Register Line Function"
	%SBTTL "PO_READ_REG_LINE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PO_READ_REG_LINE(STRING PONUM, STRING LINES, &
		STRING NEXT_GET, &
		PO_REG_LINE_CDD PO_REG_LINE_READ, &
		PO_REG_SUB_LINE_CDD PO_REG_SUB_LINE_READ, REAL QTY(), &
		STRING CUTOFF)

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
	!	.p
	!	This function returns the quantities of an order that are
	!	being shipped, ordered, or canceled.
	!
	! Index:
	!
	! Input:
	!
	!	PONUM
	!		is a Purchase Order number
	!
	!	LINES
	!		is a line number
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
	!		2% - Received (posted)
	!		.le
	!		3% - Canceled (posted)
	!		.le
	!		4% - Received (in journal)
	!		.le
	!		5% - Canceled (in journal)
	!		.le
	!		6% - Balance (incl in journal)
	!		.le
	!		8% - Price (on First sub-line)
	!		.le
	!		9% - Invoiced (posted)
	!		.le
	!		10% - Undefined
	!		.els
	!
	! Example:
	!
	!
	! Compile:
	!
	!	$ BAS PO_SOURCE:PO_READ_REG_LINE/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP PO_READ_REG_LINE
	!	$ DELETE PO_READ_REG_LINE.OBJ;*
	!
	! Author:
	!
	!	02/06/92 - Dan Perkins
	!
	! Modification history:
	!
	!
	!	02/16/92 - Frank F. Starman
	!		Return also record from the sub line file.
	!
	!	02/24/92 - Kevin Handy
	!		Cleaned up (check)
	!
	!	11/03/93 - Kevin Handy
	!		Handle the case where there are no register files
	!		yet (error 9 at 17300).
	!
	!	11/03/94 - Kevin Handy
	!		Added CUTOFF parameter.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/21/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	08/04/99 - Kevin Handy
	!		Modified to try to pass latest price through
	!		QTY(8), which was documented as being in QTY(10)
	!		even though it wasn't.
	!
	!	10/29/99 - Kevin Handy
	!		Use 'WHEN ERROR IN'
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
	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.HB"
	MAP (PO_REG_LINE)	PO_REG_LINE_CDD		PO_REG_LINE

	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_SUB_LINE.HB"
	MAP (PO_REG_SUB_LINE)	PO_REG_SUB_LINE_CDD	PO_REG_SUB_LINE

	!
	! Common Statements
	!
	COM (READ_PO_REG_LINE)	PO_REG_LINE.CH%, &
		PO_REG_SUB_LINE.CH%

	DECLARE LONG   EXIT_STATUS
	DECLARE STRING ORIG_PONUM
	DECLARE STRING LAST_LIN

	ON ERROR GOTO 19000

	%PAGE

	!
	! Set initial value
	!
	IND% = 0%
	QTY(I%) = 0.0 FOR I% = 1% TO 10%
	ORIG_PONUM = PONUM
	EXIT_STATUS = CMC$_UNDEFINED

	!
	! Open PO_REG_LINE file
	!
1000	IF PO_REG_LINE.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.OPN"
		USE
			!
			! Exit if can't open Register Line file
			!
			CONTINUE ExitFunction IF ERR = 5%
			FILENAME$ = "PO_REG_LINE"
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Open PO_REG_SUB_LINE file
	!
1010	IF PO_REG_SUB_LINE.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PO.OPEN]PO_REG_SUB_LINE.OPN"
		USE
			!
			! Exit if can't open Register Sub Line file
			!
			CONTINUE ExitFunction IF ERR = 5%
			FILENAME$ = "PO_REG_SUB_LINE"
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Get PO_REG_LINE file
	!
17300	WHEN ERROR IN
		IF EDIT$(NEXT_GET, -1%) = "EQ"
		THEN
			FIND #PO_REG_LINE.CH%, &
				KEY #0% EQ PONUM + LINES, &
				REGARDLESS
		ELSE
			! GT
			FIND #PO_REG_LINE.CH%, &
				KEY #0% GT PONUM + LINES, &
				REGARDLESS
		END IF

		GET #PO_REG_LINE.CH%, REGARDLESS
	USE
		!
		! Can't find the Register Line file record
		!
		CONTINUE ExitFunction IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PO_REG_LINE"
		CONTINUE HelpError
	END WHEN

	GOTO ExitFunction IF PO_REG_LINE::PO <> ORIG_PONUM
	PO_REG_LINE_READ = PO_REG_LINE
	EXIT_STATUS = CMC$_NORMAL

17400	WHEN ERROR IN
		FIND #PO_REG_SUB_LINE.CH%, &
			KEY #0% EQ PO_REG_LINE::PO + PO_REG_LINE::PO_LINE, &
			REGARDLESS
	USE
		!
		! Can't find the Register Sub Line file record
		!
		EXIT_STATUS = CMC$_UNTERROR
		CONTINUE ExitFunction
	END WHEN

 GetSubLine:
17410	WHEN ERROR IN
		GET #PO_REG_SUB_LINE.CH%, REGARDLESS
	USE
		!
		! Can't find the Register Sub Line file record
		!
		CONTINUE ExitFunction IF ERR = 155% OR ERR = 11% OR ERR = 9%
		FILENAME$ = "PO_REG_SUB_LINE"
		CONTINUE HelpError
	END WHEN

	IF PO_REG_SUB_LINE::PO = PO_REG_LINE::PO AND &
		PO_REG_SUB_LINE::PO_LINE = PO_REG_LINE::PO_LINE
	THEN
		IF CUTOFF <> ""
		THEN
			GOTO 17410 &
				IF PO_REG_SUB_LINE::ACTION_DATE > CUTOFF
		END IF

		PO_REG_SUB_LINE_READ = PO_REG_SUB_LINE IF IND% = 0%

		SELECT PO_REG_SUB_LINE::PO_ACTION

		CASE "01"	! On Order
			IND% = 1%

		CASE "02"	! Received (posted)
			IND% = 2%

		CASE "03"	! Canceled (posted)
			IND% = 3%

		CASE "09"	! Invoiced (posted)
			IND% = 9%

		CASE "12"	! Received (in journal)
			IND% = 4%

		CASE "13"	! Canceled (in journal)
			IND% = 5%

		CASE ELSE	! Undefined
			IND% = 10%

		END SELECT

		QTY(IND%) = QTY(IND%) + PO_REG_SUB_LINE::QTY

		IF (IND% = 1% OR IND% = 2% OR QTY(8%) = 0.0)
		THEN
			QTY(8%) = PO_REG_SUB_LINE::PRICE
		END IF

		!
		! Go for next line
		!
		GOTO GetSubLine

	END IF

 ExitFunction:

	!
	! Balance of order remaining
	!
	QTY(0%) = QTY(1%) - QTY(2%) - QTY(3%)
	QTY(0%) = 0.0 IF QTY(0%) < 0.0

	QTY(6%) = QTY(1%) - QTY(2%) - QTY(3%) - QTY(4%) - QTY(5%)
	QTY(6%) = 0.0 IF QTY(6%) < 0.0

	PO_READ_REG_LINE = EXIT_STATUS

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
	FILENAME$ = ""
	RESUME HelpError

	END FUNCTION
