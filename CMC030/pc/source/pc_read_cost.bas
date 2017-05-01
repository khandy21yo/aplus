1	%TITLE "Read Product Standard Cost"
	%SBTTL "PC_READ_COST"
	%IDENT "V3.6a Calico"

	FUNCTION REAL PC_READ_COST(XPRODUCT$, XLOCATION$, XDATE$, EFFDATE$)

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
	!	.b
	!	.lm +5
	!	This function returns Product standard cost
	!	from the Cost file and effective date
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	!
	! Input:
	!
	!	XPRODUCT$ is a product number
	!	XLOCATION$   is a location number
	!	XDATE$   is date
	!
	! Output:
	!
	!	cost of a product
	!	EFFDATE$   is eff date
	!
	! Example:
	!
	! Compile:
	!
	!	$ BAS PC_SOURCE:PC_READ_COST/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP PC_READ_COST
	!	$ DELETE PC_READ_COST.OBJ;*
	!
	! Author:
	!
	!	06/22/88 - Frank F. Starman
	!
	! Modification history:
	!
	!	07/22/93 - Frnak F. Starman
	!		Added SB_READ_COST
	!
	!	11/30/94 - Kevin Handy
	!		Modified so that all RESUME's have a line
	!		number, so they return to the correct
	!		place.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/20/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	08/02/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	COM (CH_PC_COST_READ) PC_COST.CH%
	COM (CH_UTL_PROFILE_READ) UTL_PROFILE.CH%

	%PAGE

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[PC.OPEN]PC_COST.HB"
	MAP	(PC_COST)	PC_COST_CDD	PC_COST
	MAP	(PC_COST_INIT)	PC_COST_CDD	PC_COST_INIT

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_PROFILE.HB"
	MAP	(UTL_PROFILE)	UTL_PROFILE_CDD	UTL_PROFILE

	DECLARE REAL COST

	!
	! External functions
	!
	EXTERNAL REAL    FUNCTION SB_READ_COST

	!
	! Set initial value
	!
	PC_COST_INIT = PC_COST

	COST = 0.0
	INIT_LOCATION$ = XLOCATION$
	XDATE$ = DATE_TODAY IF XDATE$ = ""
	EFFDATE$ = "00000000"

1000	IF PC_COST.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PC.OPEN]PC_COST.OPN"
		USE
			CONTINUE Cost IF ERR = 5%
			FILENAME$ = "PC_COST"
			CONTINUE HelpError
		END WHEN
	END IF

1100	IF UTL_PROFILE.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_PROFILE.OPN"
			GET #UTL_PROFILE.CH%, RECORD 1%, REGARDLESS
			CLOSE #UTL_PROFILE.CH%
		USE
			CONTINUE 2000 IF ERR = 5%
			FILENAME$ = "UTL_PROFILE"
			CONTINUE HelpError
		END WHEN
	END IF

2000	WHEN ERROR IN
		FIND #PC_COST.CH%, &
			KEY #0% EQ XPRODUCT$ + INIT_LOCATION$, &
			REGARDLESS
	USE
		CONTINUE Cost IF ERR = 9%

		IF ERR = 155%
		THEN
			IF INIT_LOCATION$ = UTL_PROFILE::DEFLOCATION
			THEN
				CONTINUE Equip
			ELSE
				INIT_LOCATION$ = UTL_PROFILE::DEFLOCATION
				CONTINUE 2000
			END IF
		END IF

		FILENAME$ = "PC_COST"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
	WHEN ERROR IN
		GET #PC_COST.CH%, REGARDLESS
	USE
		CONTINUE Cost IF ERR = 11% OR ERR = 9%
		FILENAME$ = "PC_COST"
		CONTINUE HelpError
	END WHEN

	GOTO Cost &
		IF PC_COST::PRODUCT <> XPRODUCT$ OR &
		PC_COST::LOCATION <> INIT_LOCATION$

	GOTO Cost &
		IF PC_COST::EFFDATE > XDATE$

	COST = PC_COST::COST
	EFFDATE$ = PC_COST::EFFDATE

	GOTO GetNextRec

 Equip:
	COST = SB_READ_COST(XPRODUCT$, "EL")

 Cost:
	PC_READ_COST = COST
	PC_COST = PC_COST_INIT

	EXIT FUNCTION

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO Cost

	END FUNCTION
