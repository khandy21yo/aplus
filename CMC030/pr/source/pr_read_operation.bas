1	%TITLE "Read PR Operation"
	%SBTTL "PR_READ_OPERATION"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PR_READ_OPERATION(STRING OPERATION, &
		STRING EFFDATE, PR_OPER_CDD PR_OPER_READ)

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
	!	This function returns a record from the PR Operation
	!	file.
	!
	! Index:
	!
	! Option:
	!
	! Input:
	!
	!	OPERATION	is the operation number
	!	EFFDATE		is the effective date
	!
	! Output:
	!
	!	PR_OPER_READ	is the record
	!
	! Example:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_READ_OPERATION/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP PR_READ_OPERATION
	!	$ DELETE PR_READ_OPERATION.OBJ;*
	!
	! Author:
	!
	!	08/05/92 - Dan Perkins
	!
	! Modification history:
	!
	!	08/26/92 - Kevin Handy
	!		Clean up (check)
	!
	!	10/05/92 - Dan Perkins
	!		Trap error 9 at line 2000.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	08/02/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	04/19/2003 - Kevin Handy
	!		Let's try to speed this sucker up.
	!--

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%PAGE

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[PR.OPEN]PR_OPER.HB"
	MAP (PR_OPER)		PR_OPER_CDD		PR_OPER

	COM (CH_PR_OPERATION_READ) PR_OPER.CH%

	MAP (PR_READ_OPERATION_LAST_TIME) &
		STRING LAST_OPERATION = 8%, &
		STRING LAST_EFFDATE = 8%, &
		PR_OPER_CDD LAST_PR_OPER, &
		INTEGER LAST_STATUS

	!
	! External functions
	!
	EXTERNAL STRING  FUNCTION DATE_TODAY

	!
	! Set initial value
	!
	EXIT_STATUS = CMC$_UNDEFINED

	EFFDATE = DATE_TODAY IF EFFDATE = ""

	!
	! See if we just did this one
	!
	IF LAST_OPERATION = OPERATION AND LAST_EFFDATE = EFFDATE
	THEN
		PR_OPER_READ = LAST_PR_OPER
		EXIT_STATUS  = LAST_STATUS
		PR_READ_OPERATION = EXIT_STATUS
		EXIT FUNCTION
	END IF

	!
	! Open the file
	!
1000	IF PR_OPER.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_OPER.OPN"
		USE
			CONTINUE ExitFunction IF ERR = 5%
			FILENAME$ = "PR_OPER"
			CONTINUE HelpError
		END WHEN
	END IF

2000	WHEN ERROR IN
		FIND #PR_OPER.CH%, KEY #0% EQ OPERATION, REGARDLESS
	USE
		CONTINUE ExitFunction IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_OPER"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
2010	WHEN ERROR IN
		GET #PR_OPER.CH%, REGARDLESS
	USE
		CONTINUE ExitFunction IF ERR = 11%
		FILENAME$ = "PR_OPER"
		CONTINUE HelpError
	END WHEN

	IF PR_OPER::EFFDATE <= EFFDATE AND PR_OPER::OPER = OPERATION
	THEN
		PR_OPER_READ = PR_OPER
		EXIT_STATUS  = CMC$_NORMAL
		GOTO GetNextRec
	END IF

 ExitFunction:
	LAST_OPERATION = OPERATION
	LAST_EFFDATE = EFFDATE
	LAST_PR_OPER = PR_OPER_READ
	LAST_STATUS = EXIT_STATUS

	PR_READ_OPERATION = EXIT_STATUS

	EXIT FUNCTION

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitFunction

	END FUNCTION
