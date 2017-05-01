1	%TITLE "Recalculate Balance Forward Subroutine"
	%SBTTL "AR_FUNC_REBAL"
	%IDENT "V3.6a Calico"

	FUNCTION INTEGER AR_FUNC_REBAL(CUSNUM$)

	!
	! COPYRIGHT (C) 1994 BY
	! Computer Management Center, Inc.
	! Idaho Falls, Idaho.
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
	!	This function recalculates the balance forward values
	!	for a specified customer from information in the
	!	closed ledger.
	!	.b
	!	This function assumes that customer is not missing any
	!	data in the closed ledger, and that the customer is
	!	indeed flagged as "B".
	!	.B
	!	Also assumes that there will only be one account number
	!	used.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	!
	! Input:
	!
	!	CUSNUM$ - Customer number to age.
	!
	! Output:
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_FUNC_REBAL/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP AR_FUNC_REBAL
	!	$ DELETE AR_FUNC_REBAL.OBJ;*
	!
	! Author:
	!
	!	02/24/94 - Kevin Handy
	!
	! Modification history:
	!
	!	03/03/94 - Kevin Handy
	!		Fixed bugs with service charge and payments.
	!
	!	03/14/94 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/12/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!		Lose extra parameter on ASSG_FREECHANNEL.
	!
	!	06/19/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	08/07/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[AR.OPEN]AR_CUSBAL.HB"
	MAP	(AR_CUSBAL)	AR_CUSBAL_CDD	AR_CUSBAL

	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.HB"
	MAP	(AR_CONTROL)	AR_CONTROL_CDD	AR_CONTROL

	%INCLUDE "SOURCE:[AR.OPEN]AR_CLOSED.HB"
	MAP	(AR_CLOSED)	AR_CLOSED_CDD	AR_CLOSED

	COM (CH_AR_CUSBAL) AR_CUSBAL.CH%
	COM (CH_AR_CLOSED) AR_CLOSED.CH%
	COM (CH_AR_CONTROL) AR_CONTROL.CH%

	%PAGE

300	IF AR_CUSBAL.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_CUSBAL.MOD"
		USE
			CONTINUE 310 IF ERR = 5%
			FILENAME$ = "AR_CUSBAL"
			CONTINUE HelpError
		END WHEN
	END IF

310	IF AR_CLOSED.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_CLOSED.OPN"
		USE
			CONTINUE 320 IF ERR = 5%
			FILENAME$ = "AR_CLOSED"
			CONTINUE HelpError
		END WHEN
	END IF

320	IF AR_CONTROL.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.OPN"
			GET #AR_CONTROL.CH%, RECORD 1%, REGARDLESS
			CLOSE AR_CONTROL.CH%
		USE
			CONTINUE 1000 IF ERR = 5% OR ERR = 155%
			FILENAME$ = "AR_CONTROL"
			CONTINUE HelpError
		END WHEN

		CALL ASSG_FREECHANNEL(AR_CONTROL.CH%)
	END IF

1000	!******************************************************************
	! Handle CUSBAL file.  I am assuming that it will come in in sorted
	! order, since RMS should be handling that.
	!******************************************************************

	WHEN ERROR IN
		GET #AR_CUSBAL.CH%, KEY #0% EQ CUSNUM$
	USE
		IF ERR = 154%
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE 9000
	END WHEN

	!
	! Start out with nothing
	!
	AR_CUSBAL::AGING(I%) = 0.0 FOR I% = 0% TO 4%
	AR_CUSBAL::CHARGE = 0.0
	ARPERIOD% = VAL%(AR_CONTROL::YEAR) * 12% + AR_CONTROL::LASTPERCLOSE
	PAYMENT = 0.0


2000	!*******************************************************************
	! Scan through the AR CLosed file
	!*******************************************************************

	WHEN ERROR IN
		FIND #AR_CLOSED.CH%, KEY #0% EQ CUSNUM$, REGARDLESS
	USE
		CONTINUE 3000
	END WHEN

2010	WHEN ERROR IN
		GET #AR_CLOSED.CH%, REGARDLESS
	USE
		CONTINUE 3000 IF ERR = 11%
		FILENAME$ = "AR_CLOSED"
		CONTINUE HelpError
	END WHEN

	GOTO 3000 IF AR_CLOSED::CUSNUM <> CUSNUM$

	IF AR_CLOSED::TRATYP = "04"
	THEN
		AR_CUSBAL::CHARGE = AR_CUSBAL::CHARGE + AR_CLOSED::SALAMT
	ELSE
		IF AR_CLOSED::SALAMT < 0.0
		THEN
			PAYMENT = FUNC_ROUND(PAYMENT + AR_CLOSED::SALAMT, 2%)
		ELSE
			THISPERIOD% = VAL%(LEFT(AR_CLOSED::CLOSEDATE, 4%)) * 12% + &
				VAL%(MID(AR_CLOSED::CLOSEDATE, 5%, 2%))

			USEPERIOD% = ARPERIOD% - THISPERIOD%
			USEPERIOD% = 0% IF USEPERIOD% < 0%
			USEPERIOD% = 4% IF USEPERIOD% > 4%

			AR_CUSBAL::AGING(USEPERIOD%) = &
				FUNC_ROUND(AR_CUSBAL::AGING(USEPERIOD%) + &
				AR_CLOSED::SALAMT, 2%)
		END IF
	END IF

	GOTO 2010

3000	!
	! Apply payments
	!
	AR_CUSBAL::CHARGE = AR_CUSBAL::CHARGE + PAYMENT
	PAYMENT = 0.0

	IF AR_CUSBAL::CHARGE < 0
	THEN
		PAYMENT = AR_CUSBAL::CHARGE
		AR_CUSBAL::CHARGE = 0.0
	END IF

	FOR I% = 4% TO 1% STEP -1%

		AR_CUSBAL::AGING(I%) = &
			FUNC_ROUND(AR_CUSBAL::AGING(I%) + &
			PAYMENT, 2%)

		PAYMENT = 0.0

		IF AR_CUSBAL::AGING(I%) < 0%
		THEN
			PAYMENT = AR_CUSBAL::AGING(I%)
			AR_CUSBAL::AGING(I%) = 0%
		END IF
	NEXT I%

	AR_CUSBAL::AGING(0%) = AR_CUSBAL::AGING(0%) + PAYMENT

	UPDATE #AR_CUSBAL.CH%

	AR_FUNC_REBAL = 0%

9000	EXIT FUNCTION

 HelpError:
	!
	! Untrapped error
	!
	!******************************************************************
	! Help Message for an error
	!******************************************************************
 !	CALL HELP_3MESSAGE(SCOPE, FILENAME$ + " " + NUM1$(ERL) + " " + ERT$(ERR), &
 !		"ERR", ERN$, "ERROR" + NUM1$(ERR))

	PRINT ERL; ERR; ERN$; " "; ERT$(ERR)

	AR_FUNC_REBAL = -1%
	GOTO 9000

	END FUNCTION
