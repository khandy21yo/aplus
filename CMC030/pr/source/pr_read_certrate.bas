1	%TITLE "Read Certificate of Minimum Wage File"
	%SBTTL "PR_READ_CERTRATE"
	%IDENT "V3.6a Calico"

	FUNCTION REAL PR_READ_CERTRATE(STRING EFF_DATE)

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
	!	This function returns the minimum wage rate from the
	!	certificate of minimum wagw file
	!
	! Index:
	!
	! Option:
	!
	! Input:
	!
	!	EFF_DATE = Effective date
	!
	! Output:
	!
	!	PR_READ_CERTRATE = Rate
	!
	! Example:
	!
	!	Rate = PR_READ_CERTRATE("19870101")
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_READ_CERTRATE/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP PR_READ_CERTRATE
	!	$ DELETE PR_READ_CERTRATE.OBJ;*
	!
	! Author:
	!
	!	12/13/87 - Frank F. Starman
	!
	! Modification history:
	!
	!	10/12/92 - Kevin Handy
	!		Trap error on 2000 if file doesn't exist.
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
	!	08/02/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%PAGE

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[PR.OPEN]PR_CERT_MIN_WAGE.HB"
	MAP (PR_CERT_MIN_WAGE)	PR_CERT_MIN_WAGE_CDD	PR_CERT_MIN_WAGE

	COM (CH_PR_CERT_MIN_WAGE) PR_CERT_MIN_WAGE.CH%

	DECLARE	STRING	TEST_DATE

	%PAGE

	TEST_DATE = EFF_DATE
	TEST_DATE = DATE_TODAY IF TEST_DATE = ""

	PR_READ_CERTRATE = 0.0

	!
	! Skip if we decided file doesn't exist
	!
	GOTO ExitFunc IF PR_CERT_MIN_WAGE.CH% < 0%

1000	IF PR_CERT_MIN_WAGE.CH% <= 0%
	THEN
		WHEN ERROR IN
			CALL ASSG_FREECHANNEL(PR_CERT_MIN_WAGE.CH%)
			%INCLUDE "SOURCE:[PR.OPEN]PR_CERT_MIN_WAGE.OPN"
		USE
			IF ERR = 5%
			THEN
				PR_CERT_MIN_WAGE.CH% = -PR_CERT_MIN_WAGE.CH%
				CONTINUE ExitFunc
			END IF
			FILENAME$ = "PR_CERT_MIN_WAGE"
			CONTINUE HelpError
		END WHEN
	END IF

2000	WHEN ERROR IN
		RESET #PR_CERT_MIN_WAGE.CH%
	USE
		CONTINUE ExitFunc
	END WHEN

 GetNextRec:
2010	WHEN ERROR IN
		GET #PR_CERT_MIN_WAGE.CH%, REGARDLESS
	USE
		CONTINUE ExitFunc IF ERR = 11%
		FILENAME$ = "PR_CERT_MIN_WAGE"
		CONTINUE HelpError
	END WHEN

	GOTO ExitFunc IF PR_CERT_MIN_WAGE::EFF_DATE > TEST_DATE

	PR_READ_CERTRATE = PR_CERT_MIN_WAGE::RATE

	GOTO GetNextRec

 ExitFunc:

	EXIT FUNCTION

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitFunc

	END FUNCTION
