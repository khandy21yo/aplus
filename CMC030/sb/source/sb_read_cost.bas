1	%TITLE "Read Actual Cost for Subaccount"
	%SBTTL "SB_READ_COST"
	%IDENT "V3.6a Calico"

	FUNCTION REAL SB_READ_COST(XSUBACCOUNT$, XSYSTEM$)

	!
	! COPYRIGHT (C) 1987, 1988 BY
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
	!	This function returns cost
	!	from the subaccount Balance file.
	!
	! Index:
	!
	! Option:
	!
	! Input:
	!
	!	XSUBACCOUNT$ is a subaccount number
	!	XSYSTEM$   is a system ID.
	!
	! Output:
	!
	!	cost of a subaccount
	!
	! Example:
	!
	! Compile:
	!
	!	$ BAS SB_SOURCE:SB_READ_COST/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP SB_READ_COST
	!	$ DELETE SB_READ_COST.OBJ;*
	!
	! Author:
	!
	!	07/22/93 - Frank F. Starman
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/29/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	08/04/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%PAGE

	COM (CH_SB_BALANCE_READ) SB_BALANCE.CH%
	COM (CH_SB_READ_COST) SB_CONTROL.CH%, &
		PERIOD$ = 6%

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[SB.OPEN]SB_BALANCE.HB"
	MAP	(SB_BALANCE)	SB_BALANCE_CDD	SB_BALANCE

	%INCLUDE "SOURCE:[SB.OPEN]SB_CONTROL.HB"
	MAP	(SB_CONTROL)	SB_CONTROL_CDD	SB_CONTROL

	DECLARE REAL COST

	!
	! Set initial value
	!
	COST = 0.0
	YSUBACCOUNT$ = LEFT(XSUBACCOUNT$, 10%)

1000	IF SB_BALANCE.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[SB.OPEN]SB_BALANCE.OPN"
		USE
			CONTINUE Cost IF ERR = 5%
			FILENAME$ = "SB_BALANCE"
			CONTINUE HelpError
		END WHEN
	END IF

1100	IF SB_CONTROL.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[SB.OPEN]SB_CONTROL.OPN"
			GET #SB_CONTROL.CH%, KEY #0% EQ XSYSTEM$, REGARDLESS
			PERIOD$ = TRM$(SB_CONTROL::PERIOD)
			CLOSE #SB_CONTROL.CH%
			!CALL ASSG_FREECHANNEL(SB_CONTROL.CH%)
		USE
			PERIOD$ = "      "

			CONTINUE Cost IF ERR = 5% OR ERR = 155%
			FILENAME$ = "SB_CONTROL"
			CONTINUE HelpError
		END WHEN
	END IF

2000	WHEN ERROR IN
		FIND #SB_BALANCE.CH%, &
			KEY #1% GE PERIOD$ + XSYSTEM$ + YSUBACCOUNT$, &
			REGARDLESS
	USE
		CONTINUE Cost IF ERR = 11% OR ERR = 9% OR ERR = 155%
		FILENAME$ = "SB_BALANCE"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
	WHEN ERROR IN
		GET #SB_BALANCE.CH%, REGARDLESS
	USE
		CONTINUE Cost IF ERR = 11% OR ERR = 9% OR ERR = 155%
		FILENAME$ = "SB_BALANCE"
		CONTINUE HelpError
	END WHEN

	GOTO Cost IF SB_BALANCE::SYSTEM <> XSYSTEM$ OR &
		SB_BALANCE::SUBACCOUNT <> YSUBACCOUNT$

	COST = COST + SB_BALANCE::BEG_AMOUNT + SB_BALANCE::AMOUNT

	GOTO GetNextRec

 Cost:
	SB_READ_COST = COST

	EXIT FUNCTION

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO Cost

	END FUNCTION
