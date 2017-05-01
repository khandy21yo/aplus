1	%TITLE "Read Operation File"
	%SBTTL "PR_READ_SUBJOPER"
	%IDENT "V3.6a Calico"

	SUB PR_READ_SUBJOPER(STRING OPERATION, STRING EFF_DATE, &
		REAL PIECE_RATE, REAL HOUR_RATE)

	!
	!	COPYRIGHT (C) 1987 BY
	!	Computer Management Center
	!	Idaho Falls, Idaho
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
	!	.p
	!	This function returns the operation file
	!
	! Index:
	!
	! Option:
	!
	! Input:
	!
	!	OPERATION = Operation number
	!	EFF_DAT$ = Effective date
	!	PIECE_RATE = Piece rate
	!	HOUR_RATE = Hourly rate
	!
	! Output:
	!
	!	PIECE_RATE = Piece rate
	!	HOUR_RATE = Hourly rate
	!
	! Example:
	!
	!	CALL PR_READ_SUBJOPER("WELD","19870101", &
	!		PIECE_RATE, &
	!		HOUR_RATE)
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_READ_SUBJOPER/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP PR_READ_SUBJOPER
	!	$ DELETE PR_READ_SUBJOPER.OBJ;*
	!
	! Author:
	!
	!	12/13/87 - Frank F. Starman
	!
	! Modification history:
	!
	!	04/14/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/25/96 - Kevin Handy
	!		Reformat source code
	!
	!	05/29/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	08/02/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%PAGE

	COM (CH_PR_OPER) PR_OPER.CH%

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[PR.OPEN]PR_OPER.HB"
	MAP (PR_OPER)	PR_OPER_CDD		PR_OPER

	DECLARE	STRING	TEST_DATE

	%PAGE

	TEST_DATE = EFF_DATE
	TEST_DATE = DATE_TODAY IF TEST_DATE = ""

	HOUR_RATE = 0.0
	PIECE_RATE = 0.0

1000	IF PR_OPER.CH% <= 0%
	THEN
		CALL ASSG_FREECHANNEL(PR_OPER.CH%)
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_OPER.OPN"
		USE
			CONTINUE ExitSub IF ERR = 5%
			FILENAME$ = "PR_OPER"
			CONTINUE HelpError
		END WHEN
	END IF

2000	WHEN ERROR IN
		FIND #PR_OPER.CH%, KEY #0% EQ OPERATION, REGARDLESS
	USE
		CONTINUE ExitSub IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_OPER"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
2010	WHEN ERROR IN
		GET #PR_OPER.CH%, REGARDLESS
	USE
		CONTINUE ExitSub IF ERR = 11%
		FILENAME$ = "PR_OPER"
		CONTINUE HelpError
	END WHEN

	GOTO ExitSub IF PR_OPER::OPER <> OPERATION

	GOTO ExitSub IF PR_OPER::EFFDATE > TEST_DATE

	PIECE_RATE = PR_OPER::PIECE_RATE
	HOUR_RATE = PR_OPER::HOUR_RATE

	GOTO GetNextRec

 ExitSub:
	EXIT SUB

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitSub

	END SUB
