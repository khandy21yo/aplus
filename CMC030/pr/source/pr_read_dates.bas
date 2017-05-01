1	%TITLE "Read Employee Chronicle information from Chronicle file"
	%SBTTL "PR_READ_DATES"
	%IDENT "V3.6a Calico"

	SUB PR_READ_DATES(EMPNUM$, &
		DATECD$, &
		EFFDAT$, &
		FLAGS%, &
		PR_EMP_DATES_CDD CURRENT_INFORMATION)

	!
	!	COPYRIGHT (C) 1991 by
	!	Computer Management Center, Inc.
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
	!	This function will return the last most active date
	!	record for a given employee, date, and date-code.
	!
	! Index:
	!	.x Employee>Date
	!	.x Date>Employee
	!	.x Chronicle>Employee
	!	.x Employee>Chronicle
	!
	! Option:
	!
	!
	! Input:
	!
	!	EMPNUM$ = Employee number
	!	OPER$ = Operation number
	!	EFFDAT$ = Effective date  [format (YYYYMMDD)]
	!	FLAGS%  = Option flags
	!		1 = Open file Read/Write if must open.
	!
	! Output:
	!
	!
	! Example:
	!
	!	CALL PR_READ_DATES("XZY", &
	!		"WELD", &
	!		"19870101", &
	!		1%, &
	!		DATE_RECORD)
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_READ_DATES/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP PR_READ_DATES
	!	$ DELETE PR_READ_DATES.OBJ;*
	!
	! Author:
	!
	!	04/08/91 - Kevin Handy
	!
	! Modification history:
	!
	!	04/13/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!		Lose extra channel on ASSG_FREECHANNEL.
	!
	!	10/25/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	COM (CH_PR_EMP_DATES) &
		PR_EMP_DATES.CH%, &
		PR_EMP_DATES.READONLY%

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_DATES.HB"
	MAP (PR_EMP_DATES)	PR_EMP_DATES_CDD	PR_EMP_DATES

	%PAGE

	EFFDAT$ = DATE_TODAY IF EFFDAT$ = ""
	DATECD$ = "AC" IF DATECD$ = ""

	CURRENT_INFORMATION::EMPLOYEE	= EMPNUM$
	CURRENT_INFORMATION::DATECD	= DATECD$
	CURRENT_INFORMATION::DATEBEGIN	= ""
	CURRENT_INFORMATION::DATEEND	= ""
	CURRENT_INFORMATION::DESCR	= ""

1000	IF PR_EMP_DATES.CH% <= 0%
	THEN
		WHEN ERROR IN
			IF FLAGS% AND 1%
			THEN
				%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_DATES.MOD"
				PR_EMP_DATES.READONLY% = 0%
			ELSE
				%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_DATES.OPN"
				PR_EMP_DATES.READONLY% = -1%
			END IF
		USE
			IF ERR = 5%
			THEN
				CALL ASSG_FREECHANNEL(PR_EMP_DATES.CH%)
				PR_EMP_DATES.CH% = 0%
				CONTINUE 3000
			END IF
			FILENAME$ = "PR_EMP_DATES"
			CONTINUE HelpError
		END WHEN
	END IF

2000	WHEN ERROR IN
		FIND #PR_EMP_DATES.CH%, &
			KEY #0% EQ EMPNUM$ + DATECD$, &
			REGARDLESS
	USE
		CONTINUE 3000
	END WHEN

 GetNextRec:
2010	WHEN ERROR IN
		GET #PR_EMP_DATES.CH%, REGARDLESS
	USE
		CONTINUE ExitSub
	END WHEN

	GOTO ExitSub IF (PR_EMP_DATES::EMPLOYEE <> EMPNUM$) OR &
		(PR_EMP_DATES::DATECD <> DATECD$)

	GOTO ExitSub IF PR_EMP_DATES::DATEBEGIN > EFFDAT$

	CURRENT_INFORMATION = PR_EMP_DATES

	GOTO GetNextRec

 ExitSub:
3000	!*****************************************************************

	EXIT SUB

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitSub

	END SUB
