1	%TITLE "Update Period File"
	%SBTTL "WRIT_PERIOD"
	%IDENT "V3.6a Calico"

	FUNCTION LONG WRIT_PERIOD
	!
	!	COPYRIGHT (C) 1986 BY
	!	Computer Management Center, Idaho Falls, Idaho.
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
	!	This function updates the period file.
	!	.lm -5
	!
	! Index:
	!
	! Parameters:
	!
	!	This function will return the updated version of the period
	!	file.
	!
	! Example:
	!
	!	PERIOD = WRIT_PERIOD
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:WRIT_PERIOD/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP WRIT_PERIOD
	!	$ DELETE WRIT_PERIOD.OBJ;*
	!
	! Author:
	!
	!	11/24/87 - Frank F. Starman
	!
	! Modification history:
	!
	!	03/14/91 - Frank F. Starman
	!		Enable error message.
	!
	!	03/26/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/17/96 - Kevin Handy
	!		Reformat source code
	!
	!	05/23/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/05/99 - Kevin Handy
	!		Use new 'WHEN ERROR'
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Map statements
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_PERIOD.HB"
	MAP (UTL_PERIOD)	UTL_PERIOD_CDD	UTL_PERIOD

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_ERA.HB"
	MAP (UTL_ERA)	UTL_ERA_CDD	UTL_ERA

	COM (CH_UTL_PERIOD) &
		UTL_PERIOD.CH%

	COM (CH_UTL_ERA) &
		UTL_ERA.CH%

	%PAGE

300	IF UTL_PERIOD.CH% = 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_PERIOD.CRE"
		USE
			FILENAME$ = "UTL_PERIOD"
			CONTINUE HelpError
		END WHEN
	END IF


	%PAGE

1000	!******************************************************************
	! Create chronology file
	!******************************************************************
	CALL ENTR_3MESSAGE(SCOPE, "Testing Dates", 1% + 16%)

	RESET #UTL_ERA.CH%

1020	!
	! Main loop starts here
	!
	!
	! Get next record
	!
	WHEN ERROR IN
		GET #UTL_ERA.CH%,REGARDLESS
		BEG_DATE$ = UTL_ERA::BEG_DATE
		PERIOD% = 0% IF UTL_ERA::ERA <> ERA$

		FIND #UTL_PERIOD.CH%, KEY #0% EQ UTL_ERA::ERA, REGARDLESS
	USE
		CONTINUE 1020 IF ERR = 155%
		CONTINUE 1500 IF ERR = 11%
		EXIT HANDLER
	END WHEN

1030	!
	! Get next record
	!
	WHEN ERROR IN
		GET #UTL_PERIOD.CH%
	USE
		CONTINUE 1020 IF ERR = 11%
		EXIT HANDLER
	END WHEN

	GOTO 1020 IF UTL_PERIOD::ERA <> UTL_ERA::ERA

	!
	! Test conflict date
	IF UTL_PERIOD::END_DATE < BEG_DATE$ AND PERIOD%
	THEN
		CALL HELP_34MESSAGE(SCOPE, "Period " + UTL_PERIOD::YEAR + &
			UTL_PERIOD::CYCLE + " has conflict end date.", "E", &
			"WRIT_PERIOD", "", "CONFDATE")
	!++
	! Error:CONFDATE
	!--
		WRIT_PERIOD = 2%
		GOTO ExitFunction
	END IF

	PERIOD% = PERIOD% + 1%

	UTL_PERIOD::BEG_DATE	= BEG_DATE$
	UTL_PERIOD::AGE		= FORMAT$(PERIOD%, "<0>###")

	UPDATE #UTL_PERIOD.CH%

	!
	! Find next day (beginning date for the next period)
	!
	BEG_DATE$ = DATE_INVDCODE(DATE_DAYCODE(UTL_PERIOD::END_DATE) + 1%)
	ERA$ = UTL_PERIOD::ERA

	GOTO 1030

	%PAGE

1500	!
	! Exit the program
	!
 ExitFunction:
	CALL ENTR_3MESSAGE(SCOPE, "                 ", 1%)
	EXIT FUNCTION

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	WRIT_PERIOD = 1%
	GOTO ExitFunction

32767	END FUNCTION
