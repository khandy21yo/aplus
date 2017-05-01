1	%TITLE "Read Employee Rate from the Employee Rate File"
	%SBTTL "PR_READ_RATE_CODE"
	%IDENT "V3.6a Calico"

	SUB PR_READ_RATE_CODE(EMPNUM$, &
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
	!	COPYRIGHT (C) 2001 BY
	!	Software Solutions, Inc.
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
	! Software Solutions, Inc.
	!
	! Software Solutions, Inc. assumes no responsibility for the use
	! or reliability of its software on equipment which is not supported
	! by Software Solutions, Inc.
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
	!	RATE_CDE$ = Rate code
	!
	! Output:
	!
	!	RATE_TYPE$ = Rate type
	!	HOUR_RATE = Hourly rate
	!	PIECE_RATE = Piece rate
	!	FACTOR% = Factor
	!	STDEFF = Standard efficiency percentage
	!	EVAL_DATE$ = Evaluation Date
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_READ_RATE_CODE/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP PR_READ_RATE_CODE
	!	$ DELETE PR_READ_RATE_CODE.OBJ;*
	!
	! Author:
	!
	!	07/16/2001 - Kevin Handy
	!		Based on PR_READ_RATE, modified to care about the
	!		rate code
	!
	! Modification history:
	!
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
		FIND #PR_EMP_RATE.CH%, KEY #0% EQ EMPNUM$ + OPER$, REGARDLESS
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

	GOTO ExitSub IF PR_EMP_RATE::EMPNUM <> EMPNUM$ OR &
		PR_EMP_RATE::OPER <> OPER$

	GOTO ExitSub IF PR_EMP_RATE::EFFDAT > EFFDAT$

	IF RATE_CDE$ = PR_EMP_RATE::RATE_CDE
	THEN
		RATE_TYPE$ = PR_EMP_RATE::RATE_TYPE
		HOUR_RATE = PR_EMP_RATE::HOUR_RATE
		PIECE_RATE = PR_EMP_RATE::PIECE_RATE
		FACTOR% = PR_EMP_RATE::FACTOR
		STDEFF = PR_EMP_RATE::STDEFF
		EVAL_DATE$ = PR_EMP_RATE::EVAL_DATE
		EFF_DATE$ = PR_EMP_RATE::EFFDAT
	END IF

	GOTO GetNextRec

3000	!*****************************************************************
	! If rate not found in the employee master file then look it up
	! in the operation rate file
	!*****************************************************************

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
