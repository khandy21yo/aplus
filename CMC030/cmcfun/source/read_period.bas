1	%TITLE "Read Period File"
	%SBTTL "READ_PERIOD"
	%IDENT "V3.6a Calico"

	FUNCTION LONG READ_PERIOD(XOPTION$, XERA$, XPERIOD$, XPER_DESC$, &
		XSTATUS$, XSTART_DATE$, XFINISH_DATE$, XAGE%)

	!
	! COPYRIGHT (C) 1987 BY
	! Computer Management Center
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
	! Computer Management Center
	!
	! Computer Management Center assumes no responsibility for the use
	! or reliability of its software on equipment which is not supported
	! by Computer Management Center.
	!
	!++
	!
	! Abstract:HELP
	!	.lm +5
	!	.b
	!	This function reads the Period file.
	!	.lm -5
	!
	! Index:
	!
	! Parameters:
	!
	!	XOPTION$
	!	.table
	!		READ  Read period status
	!		FIND  Search for a period
	!		DATE  Search for period by XSTART_DATE$
	!	.endtable
	!
	!	XERA$
	!
	! Return Values:
	!
	!	XPERIOD$
	!
	!	XPER_DESC$
	!		String that is the period description.
	!
	!	XSTATUS$
	!		String that returns the quarter.
	!	.table
	!		'1' 1st quarter
	!		'2' 2nd quarter
	!		'3' 3rd quarter
	!		'4' 4th quarter
	!	.endtable
	!
	!	XSTART_DATE$
	!		String that is the beginning date.
	!
	!	XFINISH_DATE$
	!		String that is the last date.
	!
	!	XAGE%
	!		When used in FIND, the number of periods to skip
	!		after the searched period.
	!		Else returns the age from the record.
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:READ_PERIOD/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP READ_PERIOD
	!	$ DELETE READ_PERIOD.OBJ;*
	!
	! Author:
	!
	!	12/18/87 - Frank Starman
	!
	! Modification history:
	!
	!	09/17/88 - Frank Starman
	!		Change from sub to function
	!
	!	11/20/90 - Val Allen
	!		Modified to add "DATE" case for return of a
	!		period based on a date entered.
	!
	!	08/14/92 - Kevin Handy
	!		Clean up (check)
	!
	!	10/01/92 - Kevin Handy
	!		Added "END IF" to "IF" statement that terminated at
	!		line number.
	!
	!	03/26/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/10/93 - Kevin Handy
	!		Add list of parameters to description message.
	!		Comment "Real" values as "Real" instead of "Integer"
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	11/07/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!
	!	10/17/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	07/30/97 - Kevin Handy
	!		Change XAGE from a REAL to an INTEGER.
	!		Use VAL% instead of VAL in several places.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/09/99 - Kevin Handy
	!		Fix bug where SCOPE structure wasn't defined
	!
	!	05/03/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	10/30/2000 - Kevin Handy
	!		Use A"x"B
	!--
	%Page

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"
	MAP (SCOPE) SCOPE_STRUCT SCOPE

	COM (CH_UTL_PERIOD_READ) UTL_PERIOD.CH%

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_PERIOD.HB"
	MAP (UTL_PERIOD)	UTL_PERIOD_CDD		UTL_PERIOD

	ON ERROR GOTO 19000

	!
	! Initialize
	!
	READ_PERIOD = 0%

	XSTATUS$ = "?"
	TEMP_XPERIOD$ = XPERIOD$
	XPER_DESC$ = STRING$(LEN(UTL_PERIOD::DESCRIPTION), A"?"B)

	IF XOPTION$ <> "DATE"
	THEN
		XSTART_DATE$ = "00000000"
		XFINISH_DATE$ = "00000000"
	END IF

1000	IF UTL_PERIOD.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_PERIOD.OPN"
		USE
			IF ERR = 5% OR ERR = 9%
			THEN
				READ_PERIOD = 1%
				CONTINUE ExitFunction
			END IF
			FILENAME$ = "UTL_PERIOD"
			CONTINUE HelpError
		END WHEN

	END IF

	SELECT XOPTION$

	CASE "READ"
		!
		! Read info about the period
		!
		WHEN ERROR IN
			IF EDIT$(XERA$, -1%) <> ""
			THEN
				GET #UTL_PERIOD.CH%, &
					KEY #0% EQ XERA$ + XPERIOD$, &
					REGARDLESS
			ELSE
				GET #UTL_PERIOD.CH%, &
					KEY #1% EQ XPERIOD$, &
					REGARDLESS
			END IF
		USE
			IF ERR = 155%
			THEN
				READ_PERIOD = 1%
				CONTINUE ExitFunction
			END IF
			FILENAME$ = "UTL_PERIOD"
			CONTINUE HelpError
		END WHEN

		XPER_DESC$ = UTL_PERIOD::DESCRIPTION
		XSTART_DATE$ = UTL_PERIOD::BEG_DATE
		XFINISH_DATE$ = UTL_PERIOD::END_DATE
		XSTATUS$ = UTL_PERIOD::PERIOD_STATUS
		XAGE% = VAL%(UTL_PERIOD::AGE)

	CASE "FIND"
		!
		! Find period and return info about it
		!
		TEMP_XPERIOD$ = STRING$(LEN(UTL_PERIOD::YEAR) + &
			LEN(UTL_PERIOD::CYCLE), A"?"B)

		IF EDIT$(XERA$, -1%) <> ""
		THEN
			IF TRM$(XPERIOD$) <> ""
			THEN
				WHEN ERROR IN
					GET #UTL_PERIOD.CH%, &
						KEY #0% EQ XERA$ + XPERIOD$, &
						REGARDLESS
				USE
					IF ERR = 5% OR ERR = 9% OR ERR = 155%
					THEN
						READ_PERIOD = 1%
						CONTINUE ExitFunction
					END IF
					FILENAME$ = "UTL_PERIOD"
					CONTINUE HelpError
				END WHEN

				PER_XAGE% = VAL%(UTL_PERIOD::AGE)
			ELSE
				PER_XAGE% = 0%
			END IF

			SEARCH_AGE$ = FORMAT$(PER_XAGE% + XAGE%, "<0>###")

			WHEN ERROR IN
				GET #UTL_PERIOD.CH%, &
					KEY #2% EQ XERA$ + SEARCH_AGE$, &
					REGARDLESS
			USE
				IF ERR = 155%
				THEN
					READ_PERIOD = 1%
					CONTINUE ExitFunction
				END IF
				FILENAME$ = "UTL_PERIOD"
				CONTINUE HelpError
			END WHEN

			TEMP_XPERIOD$ = &
				UTL_PERIOD::YEAR + UTL_PERIOD::CYCLE

			XPER_DESC$ = UTL_PERIOD::DESCRIPTION
			XSTATUS$ = UTL_PERIOD::PERIOD_STATUS
			XSTART_DATE$ = UTL_PERIOD::BEG_DATE
			XFINISH_DATE$ = UTL_PERIOD::END_DATE
		END IF

	CASE "DATE"
		!
		! Find a period for the era that contains the entered
		! date XSTART_DATE$
		!
1500		IF EDIT$(XERA$, -1%) <> ""
		THEN
			WHEN ERROR IN
				FIND #UTL_PERIOD.CH%, &
					KEY #0% EQ XERA$, &
					REGARDLESS
			USE
				FILENAME$ = "UTL_PERIOD"
				CONTINUE HelpError
			END WHEN

		ELSE
			READ_PERIOD = 1%
			GOTO ExitFunction

		END IF

1600		WHEN ERROR IN
			GET #UTL_PERIOD.CH%, REGARDLESS
		USE
			IF ERR = 11%
			THEN
				READ_PERIOD = 1%
				CONTINUE ExitFunction
			END IF
			FILENAME$ = "UTL_PERIOD"
			CONTINUE HelpError
		END WHEN

		IF UTL_PERIOD::ERA <> XERA$
		THEN
			READ_PERIOD = 1%
			GOTO ExitFunction
		END IF

		GOTO 1600 IF UTL_PERIOD::END_DATE < XSTART_DATE$

		IF UTL_PERIOD::BEG_DATE > XSTART_DATE$
		THEN
			READ_PERIOD = 1%
			GOTO ExitFunction
		ELSE
			TEMP_XPERIOD$ = &
				UTL_PERIOD::YEAR + UTL_PERIOD::CYCLE

			XPER_DESC$ = UTL_PERIOD::DESCRIPTION
			XSTART_DATE$ = UTL_PERIOD::BEG_DATE
			XFINISH_DATE$ = UTL_PERIOD::END_DATE
			XSTATUS$ = UTL_PERIOD::PERIOD_STATUS
			XAGE% = VAL%(UTL_PERIOD::AGE)
		END IF
	END SELECT

 ExitFunction:
	XPERIOD$ = TEMP_XPERIOD$

	EXIT FUNCTION

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	CALL ASSG_FREECHANNEL(UTL_PERIOD.CH%)
	READ_PERIOD = 1%
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
