1	%TITLE "Product Sales Function"
	%SBTTL "IC_READ_HISTORY"
	%IDENT "V3.6a Calico"

	FUNCTION LONG IC_READ_HISTORY( STRING PRODUCT, STRING FPERIOD, &
		STRING TPERIOD, REAL TOTHQ, REAL TOTHS, REAL TOTHC)


	!
	! COPYRIGHT (C) 1991 BY
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
	!	This function returns the total product quantity
	!	sold, the sales dollars and cost dollars based on
	!	the product, from-period and to-period.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Input:
	!
	!	PRODUCT is the part number
	!	FPERIOD is the from-period
	!	TPERIOD is the to-period
	!
	! Output:
	!
	!	TOTHQ is the total quantity sold for the product
	!	TOTHS is the total sales dollars for the product
	!	TOTHC is the total cost dollars for the product
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_READ_HISTORY/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP IC_READ_HISTORY
	!	$ DELETE IC_READ_HISTORY.OBJ;*
	!
	! Author:
	!
	!	01/21/91 - Val James Allen
	!
	! Modification history:
	!
	!	02/28/92 - Kevin Handy
	!		Changed "CMC$NORMAL" to "CMC$_NORMAL".
	!
	!	04/01/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/18/96 - Kevin Handy
	!		Clean up source code.
	!
	!	05/27/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	07/18/2000 - Kevin Handy
	!		Add a REGARDLESS to read history
	!		Use WHEN ERROR IN
	!--

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%PAGE

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	DECLARE LONG EXIT_STATUS

	!
	! Include CDD'S
	!
	%INCLUDE "SOURCE:[IC.OPEN]IC_HISTORY.HB"
	MAP (IC_HISTORY) IC_HISTORY_CDD IC_HISTORY

	COM (READ_IC_HISTORY) IC_HISTORY.CH%

	!
	! Assume undefined
	!
	EXIT_STATUS = CMC$_UNDEFINED
	YYYY$ = LEFT$(TPERIOD, 4%)
	TOTHQ = 0.0
	TOTHS = 0.0
	TOTHC = 0.0

	!
	! Open IC_HISTORY file
	!
1000	IF IC_HISTORY.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[IC.OPEN]IC_HISTORY.OPN"
		USE
			CONTINUE ExitFunction IF ERR = 5%
			FILENAME$ = "IC_HISTORY"
			CONTINUE HelpError
		END WHEN
	END IF

1500	WHEN ERROR IN
		FIND #IC_HISTORY.CH%, &
			KEY #2% EQ "SA" + PRODUCT + FPERIOD, &
			REGARDLESS
	USE
		CONTINUE 2000 IF ERR = 155%
		FILENAME$ = "IC_HISTORY"
		CONTINUE HelpError
	END WHEN

	!
	! Get IC_HISTORY file
	!
2000	WHEN ERROR IN
		GET #IC_HISTORY.CH%, REGARDLESS
	USE
		CONTINUE 2100 IF ERR = 11%
		FILENAME$ = "IC_HISTORY"
		CONTINUE HelpError
	END WHEN

	GOTO 2100 IF IC_HISTORY::PERIOD > TPERIOD

	GOTO 2100 IF IC_HISTORY::PRODUCT <> PRODUCT

	GOTO 2100 IF IC_HISTORY::TRANSTYPE <> "SA"

	TOTHQ = TOTHQ + IC_HISTORY::PQUANTITY
	TOTHS = TOTHS + IC_HISTORY::SALEAMT
	TOTHC = TOTHC + IC_HISTORY::COSTAMT

	GOTO 2000

2100	EXIT_STATUS = CMC$_NORMAL

 ExitFunction:
	IC_READ_HISTORY = EXIT_STATUS

	EXIT FUNCTION

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", "SA_READ_SALCUST", FILENAME$, NUM1$(ERR))

	EXIT_STATUS = CMC$_UNTERROR
	GOTO ExitFunction

	END FUNCTION
