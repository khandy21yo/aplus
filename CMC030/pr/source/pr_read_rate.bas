1	%TITLE "Read Employee Rate from the Employee Rate File"
	%SBTTL "PR_READ_RATE"
	%IDENT "V3.6a Calico"

	SUB PR_READ_RATE(EMPNUM$, &
		OPER$, &
		EFFDAT$, &
		RATE_TYPE$, &
		RATE_CDE$, &
		HOUR_RATE, &
		PIECE_RATE, &
		FACTOR%, &
		STDEFF, &
		EVAL_DATE$, &
		EFF_DATE$)

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
	!	This function returns the employee rate from the employee
	!	rate file based on the operation and the effective date.
	!
	! Index:
	!	.x Employee>Rate
	!	.x Rate>Employee
	!	.x Pay Rate>Employee
	!	.x Employee>Pay Rate
	!
	! Option:
	!
	!
	! Input:
	!
	!	EMPNUM$ = Employee number
	!	OPER$ = Operation number
	!	EFFDAT$ = Effective date  [format (YYYYMMDD)]
	!
	! Output:
	!
	!	RATE_TYPE$ = Rate type
	!	RATE_CDE$ = Rate code
	!	HOUR_RATE = Hourly rate
	!	PIECE_RATE = Piece rate
	!	FACTOR% = Factor
	!	STDEFF = Standard efficiency percentage
	!	EVAL_DATE$ = Evaluation Date
	!
	! Example:
	!
	!	CALL PR_READ_RATE("XZY", &
	!		"WELD", &
	!		"19870101", &
	!		RATE_CDE$, &
	!		HOUR_RATE, &
	!		PIECE_RATE, &
	!		FACTOR%, &
	!		STDEFF, &
	!		ENAL_DATE$)
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_READ_RATE/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP PR_READ_RATE
	!	$ DELETE PR_READ_RATE.OBJ;*
	!
	! Author:
	!
	!	12/4/87 - Robert Peterson
	!
	! Modification history:
	!
	!	01/12/88 - Robert Peterson
	!		Add search in operation file is rate is
	!		not found in the employee rate file.
	!
	!	03/18/91 - Kevin Handy
	!		Added return of evaluation date.
	!
	!	04/08/91 - Kevin Handy
	!		Modified to look up in operation file if
	!		the rate file is missing.  Was only doing
	!		so if the rate was not there.
	!
	!	07/13/91 - Kevin Handy
	!		Changed the common area for the rate channel
	!		from PR_READ_RATE to CH_PR_EMP_RATE to match
	!		the maintenance programs, and changed the open
	!		to MOD for the same reason.
	!
	!	08/12/92 - Kevin Handy
	!		Modified to lose the "ON ERROR GO BACK" call
	!		when didn't recognize the error.  Made a call
	!		to HelpError instead.
	!		Programs that received the error didn't handle
	!		it correctly.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/25/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/22/2000 - Kevin Handy
	!		Add EFF_DATE$ to returned values
	!
	!	07/10/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	COM (CH_PR_EMP_RATE) &
		PR_EMP_RATE.CH%

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_RATE.HB"
	MAP (PR_EMP_RATE)	PR_EMP_RATE_CDD		PR_EMP_RATE

	%PAGE

	EFFDAT$ = DATE_TODAY IF EFFDAT$ = ""
	RATE_TYPE$ = ""
	RATE_CDE$ = ""
	HOUR_RATE = 0.0
	PIECE_RATE = 0.0
	FACTOR% = 0%
	STDEFF = 0.0
	EVAL_DATE$ = ""
	EFF_DATE$ = ""

1000	IF PR_EMP_RATE.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_RATE.MOD"
		USE
			CONTINUE 3000 IF ERR = 5%
			FILENAME$ = "PR_EMP_RATE"
			CONTINUE HelpError
		END WHEN
	END IF

2000	WHEN ERROR IN
		FIND #PR_EMP_RATE.CH%, &
			KEY #0% EQ EMPNUM$ + OPER$, &
			REGARDLESS
	USE
		CONTINUE 3000 IF ERR = 155%
		FILENAME$ = "PR_EMP_RATE"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
2010	WHEN ERROR IN
		GET #PR_EMP_RATE.CH%, REGARDLESS
	USE
		CONTINUE ExitSub IF ERR = 11%
		FILENAME$ = "PR_EMP_RATE"
		CONTINUE HelpError
	END WHEN

	GOTO ExitSub &
		IF PR_EMP_RATE::EMPNUM <> EMPNUM$ OR &
		PR_EMP_RATE::OPER <> OPER$

	GOTO ExitSub &
		IF PR_EMP_RATE::EFFDAT > EFFDAT$

	RATE_TYPE$ = PR_EMP_RATE::RATE_TYPE
	RATE_CDE$ = PR_EMP_RATE::RATE_CDE
	HOUR_RATE = PR_EMP_RATE::HOUR_RATE
	PIECE_RATE = PR_EMP_RATE::PIECE_RATE
	FACTOR% = PR_EMP_RATE::FACTOR
	STDEFF = PR_EMP_RATE::STDEFF
	EVAL_DATE$ = PR_EMP_RATE::EVAL_DATE
	EFF_DATE$ = PR_EMP_RATE::EFFDAT

	GOTO GetNextRec

3000	!*****************************************************************
	! If rate not found in the employee master file then look it up
	! in the operation rate file
	!*****************************************************************

	CALL PR_READ_SUBJOPER(OPER$, EFFDAT$, PIECE_RATE, HOUR_RATE)
	EVAL_DATE$ = ""
	EFF_DATE$ = ""

 ExitSub:

	EXIT SUB

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	EXIT SUB

	END SUB
