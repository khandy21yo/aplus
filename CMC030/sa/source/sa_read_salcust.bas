1	%TITLE "Customer Sales Function"
	%SBTTL "SA_READ_SALCUST"
	%IDENT "V3.6a Calico"

	FUNCTION LONG SA_READ_SALCUST(STRING CUSNUM, STRING TYPES, &
		STRING PERIOD, REAL BBPER, REAL BBYEAR)


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
	!	This function returns the record by using the customer
	!	type, the order type, and the location number.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	!
	! Input:
	!
	!	CUSTYPE is a customer type
	!	ORDTYPE is an order type
	!	LOCATION is the location number
	!
	! Output:
	!
	!	SA_SALCUST is the SALCUST returned
	!
	! Example:
	!
	! Compile:
	!
	!	$ BAS SA_SOURCE:SA_READ_SALCUST/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP SA_READ_SALCUST
	!	$ DELETE SA_READ_SALCUST.OBJ;*
	!
	! Author:
	!
	!	07/10/90 - Lance Williams
	!
	! Modification history:
	!
	!	01/17/91 - Val James Allen
	!		Modified so that it works
	!
	!	02/28/92 - Kevin Handy
	!		Changed "CMC$NORMAL" to "CMC$_NORMAL".
	!
	!	04/23/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/14/96 - Kevin Handy
	!		Reformat source code.
	!
	!	06/03/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	08/04/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	COM(SA_READ_SALCUST.COM) SA_SALCUST.CH%

	DECLARE LONG EXIT_STATUS

	!
	! Include CDD'S
	!
	%INCLUDE "SOURCE:[SA.OPEN]SA_SALCUST.HB"
	MAP	(SA_SALCUST)	SA_SALCUST_CDD	SA_SALCUST

	EXTERNAL LONG FUNCTION SB_READ_ACCOUNT

	%PAGE

	!
	! Assume undefined
	!
	EXIT_STATUS = CMC$_UNDEFINED
	BBPER, BBYEAR = 0.0
	YYYY$ = LEFT$(PERIOD, 4%)

	!
	! Open SA_SALCUST file
	!
1000	WHEN ERROR IN
		%INCLUDE "SOURCE:[SA.OPEN]SA_SALCUST.OPN"
	USE
		CONTINUE ExitFunction IF ERR = 5%
		FILENAME$ = "SA_SALCUST"
		CONTINUE HelpError
	END WHEN

1500	WHEN ERROR IN
		FIND #SA_SALCUST.CH%, KEY #0% EQ CUSNUM + PERIOD, REGARDLESS
	USE
		CONTINUE 2010 IF ERR = 155%
		FILENAME$ = "SA_SALCUST"
		CONTINUE HelpError
	END WHEN

	!
	! Get SA_SALCUST file
	!
2000	WHEN ERROR IN
		GET #SA_SALCUST.CH%
	USE
		CONTINUE 2010 IF ERR = 11%
		FILENAME$ = "SA_SALCUST"
		CONTINUE HelpError
	END WHEN

	GOTO 2010 IF SA_SALCUST::CUSNUM <> CUSNUM

	GOTO 2010 IF SA_SALCUST::PERIOD <> PERIOD

	V% = SB_READ_ACCOUNT("SA", SA_SALCUST::ACCOUNT, FLAG$)

	GOTO 2000 IF FLAG$ <> TYPES

	BBPER = SA_SALCUST::AMOUNT

2010	WHEN ERROR IN
		FIND #SA_SALCUST.CH%, &
			KEY #0% EQ CUSNUM + LEFT$(PERIOD, 4%), &
			REGARDLESS
	USE
		CONTINUE ExitFunction IF ERR = 9% OR ERR = 155%
		FILENAME$ = "SA_SALCUST"
		CONTINUE HelpError
	END WHEN

2100	WHEN ERROR IN
		GET #SA_SALCUST.CH%
	USE
		CONTINUE 2200 IF ERR = 11%
		FILENAME$ = "SA_SALCUST"
		CONTINUE HelpError
	END WHEN

	GOTO 2200 IF SA_SALCUST::CUSNUM <> CUSNUM

	GOTO 2200 IF LEFT$(SA_SALCUST::PERIOD, 4%) <> LEFT$(PERIOD, 4%)

	V% = SB_READ_ACCOUNT("SA", SA_SALCUST::ACCOUNT, FLAG$)

	GOTO 2100 IF FLAG$ <> TYPES

	BBYEAR =  SA_SALCUST::AMOUNT

2200	EXIT_STATUS = CMC$_NORMAL

 ExitFunction:
	SA_READ_SALCUST = EXIT_STATUS

	CLOSE #SA_SALCUST.CH%

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
