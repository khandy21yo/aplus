1	%TITLE "WIP Write to Register Line Function"
	%SBTTL "WP_WRIT_REGLINE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG WP_WRIT_REGLINE( STRING KINGDOMKEY, STRING TTYPE, &
		REAL QTY)

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
	!	This function writes a record into the WIP register file
	!	for buyoffs (online) quantities being completed or cancelled.
	!	.lm -5
	!
	! Index:
	!
	! Input:
	!
	!	KINGDOMKEY is the key to the record for posting into the WIP register
	!	file.
	!
	!	TTYPE is the type of transaction
	!		12 = completed
	!		13 = cancelled
	!
	!	QTY is the actual quantity to post into the new record
	!
	! Output:
	!
	!
	! Example:
	!
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_WRIT_REGLINE/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP WP_WRIT_REGLINE
	!	$ DELETE WP_WRIT_REGLINE.OBJ;*
	!
	! Author:
	!
	!	06/11/91 - Val James "Gismo" Allen
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/31/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/25/2000 - Kevin Handy
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
	MAP (WP_REGLINE)	WP_REGLINE_CDD		WP_REGLINE

	!
	! Common Statements
	!
	COM (WRIT_WP_REGLINE) WP_REGLINE.CH%

	DECLARE LONG EXIT_STATUS

	ON ERROR GOTO 19000

	%PAGE

	!
	! Set initial value
	!
	EXIT_STATUS = CMC$_NORMAL

	!
	! Open WP_REGLINE file
	!
1000	IF WP_REGLINE.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.CRE"
		USE
			EXIT_STATUS = CMC$_UNTERROR
			CONTINUE ExitFunction IF ERR = 5%
			FILENAME$ = "WP_REGLINE"
			CONTINUE HelpError
		END WHEN
	END IF

1100	WHEN ERROR IN
		FIND #WP_REGLINE.CH%, KEY #0% GE KINGDOMKEY + TTYPE, REGARDLESS
	USE
		CONTINUE 1200 IF ERR = 155%
		FILENAME$ = "WP_REGLINE"
		CONTINUE HelpError
	END WHEN

1110	WHEN ERROR IN
		GET #WP_REGLINE.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE 1200 IF ERR = 11% OR ERR = 9%
		FILENAME$ = "WP_REGLINE"
		CONTINUE HelpError
	END WHEN

	GOTO 1200 IF WP_REGLINE::JOB + &
		WP_REGLINE::LLINE + &
		WP_REGLINE::REC_TYPE &
		<> KINGDOMKEY + TTYPE

	WP_REGLINE::QTY = WP_REGLINE::QTY + QTY

	UPDATE #WP_REGLINE.CH%

	GOTO ExitFunction

1200	WHEN ERROR IN
		GET #WP_REGLINE.CH%, KEY #0% EQ KINGDOMKEY
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE ExitFunction IF ERR = 155%
		FILENAME$ = "WP_REGLINE"
		CONTINUE HelpError
	END WHEN

	WP_REGLINE::BATCH    = "BUYOFF"
	WP_REGLINE::QTY      = QTY
	WP_REGLINE::REC_TYPE = TTYPE

2000	WHEN ERROR IN
		PUT #WP_REGLINE.CH%
	USE
		EXIT_STATUS = CMC$_UNTERROR
		CONTINUE ExitFunction IF ERR = 9%
		FILENAME$ = "WP_REGLINE"
		CONTINUE HelpError
	END WHEN

 ExitFunction:
	WP_WRIT_REGLINE = EXIT_STATUS
	EXIT FUNCTION

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	EXIT_STATUS = CMC$_UNTERROR
	GOTO ExitFunction

	%PAGE

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************


	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

	END FUNCTION
