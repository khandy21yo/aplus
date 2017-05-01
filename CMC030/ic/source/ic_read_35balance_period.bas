1	%TITLE "Read Inventory Balance File"
	%SBTTL "IC_READ_35BALANCE_PERIOD"
	%IDENT "V3.6a Calico"

	FUNCTION LONG IC_READ_35BALANCE_PERIOD(XPRODUCT$, &
		XLOCATION$, &
		REAL XBALANCE(,), &
		STRING FOR_PERIOD)

	!
	! COPYRIGHT (C) 1998 BY
	!
	! Software Solutions, Inc.
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
	! Software Solutions, Inc.
	!
	! Software Solutions, Inc. assumes no responsibility for the use
	! or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	!
	! Abstract:HELP
	!	.p
	!	This function returns Product location balances for
	!	a specific period.
	!
	! Index:
	!
	! Option:
	!
	! Input:
	!
	!	XPRODUCT$ is a product number
	!	XLOCATION$ is a location number
	!	XBALANCE(3%,3%)  balance array for each class and record type
	!		XBALANCE(X%,1%) = Begin Balance
	!		XBALANCE(X%,2%) = Posted Balance
	!
	! Output:
	!
	!	Balance array
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_READ_35BALANCE_PERIOD/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP IC_READ_35BALANCE_PERIOD
	!	$ DELETE IC_READ_35BALANCE_PERIOD.OBJ;*
	!
	! Author:
	!
	!	03/23/98 - Kevin Handy
	!		Based upon IC_READ_35HISTORY
	!
	! Modification history:
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	07/15/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	COM (CH_IC_35BALANCE_READ) IC_35BALANCE.CH%
	COM (CH_UTL_TRANSTYPE_READ) UTL_TRANSTYPE.CH%

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[IC.OPEN]IC_35BALANCE.HB"
	MAP (IC_35BALANCE)	IC_35BALANCE_CDD	IC_35BALANCE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_TRANSTYPE.HB"
	MAP (UTL_TRANSTYPE)	UTL_TRANSTYPE_CDD	UTL_TRANSTYPE

	%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.HB"
	MAP (IC_CONTROL) IC_CONTROL_CDD IC_CONTROL

	%INCLUDE "SOURCE:[IC.OPEN]IC_35HISTORY.HB"
	MAP (IC_35HISTORY) IC_35HISTORY_CDD IC_35HISTORY

	MAP (IC_READ_35BALPERPER) &
		HISTORY%(10%)		! IC_35HISTORY Channels

	DECLARE LONG EXIT_STATUS

	%PAGE

	ON ERROR GOTO 19000

	!
	! Set initial value
	!
	XBALANCE(I%, J%) = 0.0 &
		FOR I% = 0% TO 3% &
		FOR J% = 0% TO 2%

	!
	! Assume cannot find any balance
	!
	EXIT_STATUS = CMC$_WARNING

	!
	! Open up any history files that we need
	!
	START_YEAR% = VAL(LEFT(IC_CONTROL::PERIOD, 4%))
	END_YEAR% = VAL(LEFT(FOR_PERIOD, 4%))

	FOR LOOP% = START_YEAR% TO END_YEAR% STEP -1%

400		IF HISTORY%(START_YEAR% - LOOP% + 1%) = 0%
		THEN
			YYYY$ = FORMAT$(LOOP%, "<0>###")
			IC_35HISTORY.CH% = 0%

			WHEN ERROR IN
				%INCLUDE "SOURCE:[IC.OPEN]IC_35HISTORY.OPN"
			USE
				CONTINUE 410
			END WHEN

			HISTORY%(START_YEAR% - LOOP% + 1%) = &
				IC_35HISTORY.CH%
		END IF

410	NEXT LOOP%

1000	IF IC_35BALANCE.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[IC.OPEN]IC_35BALANCE.OPN"
		USE
			CONTINUE ExitFunction IF ERR = 5%
			FILENAME$ = "IC_35BALANCE"
			CONTINUE HelpError
		END WHEN
	END IF

1010	IF UTL_TRANSTYPE.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_TRANSTYPE.OPN"
		USE
			CONTINUE ExitFunction IF ERR = 5%
			FILENAME$ = "UTL_TRANSTYPE"
			CONTINUE HelpError
		END WHEN
	END IF

2000	WHEN ERROR IN
		FIND #IC_35BALANCE.CH%, &
			KEY #0% EQ XPRODUCT$ + XLOCATION$, &
			REGARDLESS
	USE
		CONTINUE 3000 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "IC_35BALANCE"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
	WHEN ERROR IN
		GET #IC_35BALANCE.CH%, REGARDLESS
	USE
		CONTINUE 3000 IF ERR = 11% OR ERR = 9%
		FILENAME$ = "IC_35BALANCE"
		CONTINUE HelpError
	END WHEN

	GOTO 3000 &
		IF IC_35BALANCE::PRODUCT <> XPRODUCT$ OR &
		XLOCATION$ <> IC_35BALANCE::LOCATION

	X% = 0%

2010	IF UTL_TRANSTYPE::CODE <> IC_35BALANCE::TRANSTYPE
	THEN
		WHEN ERROR IN
			GET #UTL_TRANSTYPE.CH%, &
				KEY #0% EQ IC_35BALANCE::TRANSTYPE, &
				REGARDLESS
		USE
			CONTINUE CreateArray IF ERR = 155%
			FILENAME$ = "UTL_TRANSTYPE"
			CONTINUE HelpError
		END WHEN
	END IF

	X% = VAL%(UTL_TRANSTYPE::CLASS)

 CreateArray:
	XBALANCE(X%, 1%) = XBALANCE(X%, 1%) + IC_35BALANCE::BBALANCE
	XBALANCE(X%, 2%) = XBALANCE(X%, 2%) + IC_35BALANCE::PBALANCE

	EXIT_STATUS = CMC$_NORMAL

	GOTO GetNextRec

3000	!
	! Now, loop through the history file and subtract out
	! all of the garbage therin
	!
	WORK_PERIOD$ = TRM$(IC_CONTROL::PERIOD)
	START_YEAR% = 1%
	WHILE WORK_PERIOD$ >= FOR_PERIOD

		WORK_YEAR% = VAL%(LEFT(WORK_PERIOD$, 4%))
		WORK_PERIOD% = VAL%(RIGHT(WORK_PERIOD$, 5%))

		GOTO 3900 IF WORK_PERIOD$ = TRM$(IC_CONTROL::PERIOD)

3100		WHEN ERROR IN
			FIND #HISTORY%(START_YEAR%), &
				KEY #0% EQ XPRODUCT$ + XLOCATION$, &
				REGARDLESS
		USE
			CONTINUE 3900
		END WHEN

3110		WHEN ERROR IN
			GET #HISTORY%(START_YEAR%), REGARDLESS
		USE
			CONTINUE 3900
		END WHEN

		GOTO 3900 &
			IF IC_35HISTORY::PRODUCT <> XPRODUCT$ OR &
			IC_35HISTORY::LOCATION <> XLOCATION$

3120		IF UTL_TRANSTYPE::CODE <> IC_35HISTORY::TRANSTYPE
		THEN
			GET #UTL_TRANSTYPE.CH%, &
				KEY #0% EQ IC_35HISTORY::TRANSTYPE, &
				REGARDLESS
		END IF

		X% = VAL%(UTL_TRANSTYPE::CLASS)

3130		XBALANCE(X%, 1%) = XBALANCE(X%, 1%) - &
			IC_35HISTORY::PQUANTITY(WORK_PERIOD%)
		XBALANCE(X%, 2%) = XBALANCE(X%, 2%) + &
			IC_35HISTORY::PQUANTITY(WORK_PERIOD%)

		GOTO 3110

3900		WORK_PERIOD% = WORK_PERIOD% - 1%
		IF WORK_PERIOD% = 0%
		THEN
			WORK_PERIOD% = 12%
			WORK_YEAR% = WORK_YEAR% - 1%
			START_YEAR% = START_YEAR% + 1%
		END IF

		WORK_PERIOD$ = FORMAT$(WORK_YEAR%, "<0>###") + &
			FORMAT$(WORK_PERIOD%, "<0>#")
	NEXT

 ExitFunction:
 ! PRINT "END: "; XBALANCE(1%, 1%)
	IC_READ_35BALANCE_PERIOD = EXIT_STATUS
	EXIT FUNCTION

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	CALL ASSG_FREECHANNEL(IC_35BALANCE.CH%)
	CALL ASSG_FREECHANNEL(UTL_TRANSTYPE.CH%)
	IC_35BALANCE.CH% = 0%
	UTL_TRANSTYPE.CH% = 0%

	EXIT_STATUS = CMC$_UNTERROR
	GOTO ExitFunction

	%PAGE

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	FILENAME$ = ""
	RESUME HelpError

	END FUNCTION
