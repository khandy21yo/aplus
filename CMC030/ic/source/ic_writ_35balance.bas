1	%TITLE "Post Journal Transactions to Inventory"
	%SBTTL "IC_WRIT_35BALANCE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG IC_WRIT_35BALANCE(XPRODUCT$, XLOCATION$, &
		XTRANSTYPE$, REAL XQTY)

	!
	! COPYRIGHT (C) 1990 BY
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
	! CMC assumes no responsibility for the use or reliability of
	! its software on equipment which is not supported by CMC.
	!
	!++
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This function posts journal entry quantities to inventory
	!	balance file - the IC_TRAN_POST function nets out these
	!	running balance totals.
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
	!	XLOCATION$ is a location number
	!	XTRANSTYPE$ is the transtype code
	!	XQTY is the amount to post - always goes to RBALANCE
	!
	! Output:
	!
	!
	! Example:
	!
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_WRIT_35BALANCE/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP IC_WRIT_35BALANCE
	!	$ DELETE IC_WRIT_35BALANCE.OBJ;*
	!
	! Author:
	!
	!	12/21/90 - Val James Allen
	!
	! Modification history:
	!
	!	01/02/91 - Frank F. Starman
	!		Modify for a new IC_35BALANCE layout.
	!
	!	04/01/93 - Frank F. Starman
	!		Do nothing if transaction type is blank.
	!
	!	04/05/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	07/18/93 - Frank F. Starman
	!		Make a difference for Lost Sale if it is comming
	!		from a back order.
	!
	!	02/09/95 - Kevin Handy
	!		Changed "find regardless, ..., get" to "get key"
	!		at line 2000.
	!		Broke 2000 into two lines (2000, 2050) so errors
	!		are easier to check out.
	!
	!	02/09/95 - Kevin Handy
	!		Modified to force correct lengths on items used
	!		for find at 2000.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	01/22/96 - Kevin Handy
	!		Reformat closer to 80 columns.
	!
	!	01/22/96 - Kevin Handy
	!		Lose goofiness that is not needed for "LS"
	!		codes (Lost Sales) (Marco)
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/21/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	COM (CH_IC_35BALANCE) IC_35BALANCE.CH%
	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[IC.OPEN]IC_35BALANCE.HB"
	MAP (IC_35BALANCE)	IC_35BALANCE_CDD	IC_35BALANCE

	DECLARE LONG EXIT_STATUS

	%PAGE

	ON ERROR GOTO 19000

	IF XQTY = 0.0 OR XTRANSTYPE$ = ""
	THEN
		EXIT_STATUS = CMC$_NORMAL
		GOTO ExitFunction
	END IF

	YTRANSTYPE$ = XTRANSTYPE$

	IF YTRANSTYPE$ = "LL"
	THEN
		YTRANSTYPE$ = "LS"
	END IF

	EXIT_STATUS = CMC$_WARNING

1000	IF IC_35BALANCE.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[IC.OPEN]IC_35BALANCE.CRE"
		USE
			FILENAME$ = "IC_35BALANCE"
			CONTINUE HelpError
		END WHEN
	END IF

2000	IC_35BALANCE::PRODUCT	= XPRODUCT$
	IC_35BALANCE::LOCATION	= XLOCATION$
	IC_35BALANCE::TRANSTYPE	= YTRANSTYPE$

	WHEN ERROR IN
		GET #IC_35BALANCE.CH%, &
			KEY #0% EQ IC_35BALANCE::PRODUCT	+ &
			IC_35BALANCE::LOCATION + &
			IC_35BALANCE::TRANSTYPE
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE 2200 IF ERR = 155%
		FILENAME$ = "IC_35BALANCE"
		CONTINUE HelpError
	END WHEN

	IC_35BALANCE::RBALANCE = IC_35BALANCE::RBALANCE + XQTY

2050	UPDATE #IC_35BALANCE.CH%

	EXIT_STATUS = CMC$_NORMAL

	GOTO ExitFunction

2200	!
	! Can't find a record so make a new one
	!
	IC_35BALANCE::PRODUCT	= XPRODUCT$
	IC_35BALANCE::LOCATION	= XLOCATION$
	IC_35BALANCE::TRANSTYPE	= YTRANSTYPE$
	IC_35BALANCE::BBALANCE	= 0.0
	IC_35BALANCE::PBALANCE	= 0.0
	IC_35BALANCE::RBALANCE	= XQTY

	PUT #IC_35BALANCE.CH%

	EXIT_STATUS = CMC$_NORMAL

 ExitFunction:

 !	IF XTRANSTYPE$ = "LS" AND YTRANSTYPE$ = "LS"
 !	THEN
 !		YTRANSTYPE$ = "SO"
 !		XQTY = -XQTY
 !		GOTO 2000
 !	END IF

	IC_WRIT_35BALANCE = EXIT_STATUS
	EXIT FUNCTION

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	EXIT_STATUS = CMC$_UNTERROR
	GOTO ExitFunction

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	FILENAME$ = ""
	RESUME HelpError

	END FUNCTION
