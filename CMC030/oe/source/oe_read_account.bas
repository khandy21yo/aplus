1	%TITLE "Account Function"
	%SBTTL "OE_READ_ACCOUNT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG OE_READ_ACCOUNT(STRING CUSTYPE, STRING ORDTYPE, &
		STRING LOCATION, OE_ACCOUNT_CDD OE_ACCOUNT_READ)


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
	! Input:
	!
	!	CUSTYPE is a customer type
	!	ORDTYPE is an order type
	!	LOCATION is the location number
	!
	! Output:
	!
	!	OE_ACCOUNT is the account returned
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_READ_ACCOUNT/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP OE_READ_ACCOUNT
	!	$ DELETE OE_READ_ACCOUNT.OBJ;*
	!
	! Author:
	!
	!	07/10/90 - Lance Williams
	!
	! Modification history:
	!
	!	06/24/91 - Frank F. Starman
	!		Check also for a location.
	!
	!	02/28/92 - Kevin Handy
	!		Changed "CMC$NORMAL" to "CMC$_NORMAL".
	!
	!	04/30/92 - Dan Perkins
	!		Return description of accounts if undefined.
	!
	!	02/19/93 - Dan Perkins
	!		Added code to return exact record, if found,
	!		instead of wildcard record which may come first.
	!
	!	06/20/93 - Frank F. Starman
	!		Return desc of account if account is not present.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	11/20/95 - Kevin Handy
	!		Clean up source code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	07/31/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--

	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	COM(OE_READ_ACCOUNT.COM) OE_ACCOUNT.CH%

	DECLARE LONG EXIT_STATUS

	!
	! Include CDD'S
	!
	%INCLUDE "SOURCE:[OE.OPEN]OE_ACCOUNT.HB"
	MAP (OE_ACCOUNT)	OE_ACCOUNT_CDD		OE_ACCOUNT

	!
	! Assume undefined
	!
	EXIT_STATUS = CMC$_UNDEFINED

	OE_ACCOUNT_READ::ACCOUNT	= "AR Acct"
	OE_ACCOUNT_READ::DISACCT	= "Order Disc Acct"
	OE_ACCOUNT_READ::FRACCT		= "Freight Acct"
	OE_ACCOUNT_READ::HANDLING	= "Handling Acct"
	OE_ACCOUNT_READ::SALES		= "Order Sales Acct"
	OE_ACCOUNT_READ::COSACCT	= "Order COS Acct"

	!
	! Open OE_ACCOUNT file
	!
1000	IF OE_ACCOUNT.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[OE.OPEN]OE_ACCOUNT.OPN"
		USE
			CONTINUE ExitFunction IF ERR = 5%
			FILENAME$ = "OE_ACCOUNT"
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! See if we can find a record that exactly matches the
	! function arguements
	!
1500	WHEN ERROR IN
		GET #OE_ACCOUNT.CH%, &
			KEY #0% EQ CUSTYPE + ORDTYPE + LOCATION, &
			REGARDLESS
	USE
		CONTINUE 2000 IF ERR = 155%
		FILENAME$ = "OE_ACCOUNT"
		CONTINUE HelpError
	END WHEN

	OE_ACCOUNT_READ = OE_ACCOUNT
	EXIT_STATUS = CMC$_NORMAL
	GOTO ExitFunction

	!
	! Find OE_ACCOUNT file
	!
2000	WHEN ERROR IN
		RESET #OE_ACCOUNT.CH%
	USE
		FILENAME$ = "OE_ACCOUNT"
		CONTINUE HelpError
	END WHEN

 GetNextRec:

	!
	! Get OE_ACCOUNT file
	!
2010	WHEN ERROR IN
		GET #OE_ACCOUNT.CH%, REGARDLESS
	USE
		CONTINUE ExitFunction IF ERR = 155% OR ERR = 11%
		FILENAME$ = "OE_ACCOUNT"
		CONTINUE HelpError
	END WHEN

	IF COMP_STRING(TRM$(CUSTYPE), OE_ACCOUNT::CUSTTYPE) AND &
		COMP_STRING(TRM$(ORDTYPE), OE_ACCOUNT::ORDTYPE) &
		AND COMP_STRING(TRM$(LOCATION), OE_ACCOUNT::LOCATION)
	THEN
		OE_ACCOUNT_READ = OE_ACCOUNT
		EXIT_STATUS = CMC$_NORMAL
		GOTO ExitFunction
	END IF

	!
	! Get the next record
	!
	GOTO GetNextRec

 ExitFunction:
	OE_READ_ACCOUNT = EXIT_STATUS

	!
	! Fill out any undefined accounts
	!
	OE_ACCOUNT_READ::ACCOUNT = "AR Acct" &
		IF OE_ACCOUNT_READ::ACCOUNT = ""
	OE_ACCOUNT_READ::DISACCT = "Order Disc Acct" &
		IF OE_ACCOUNT_READ::DISACCT = ""
	OE_ACCOUNT_READ::FRACCT = "Freight Acct" &
		IF OE_ACCOUNT_READ::FRACCT = ""
	OE_ACCOUNT_READ::HANDLING = "Handling Acct" &
		IF OE_ACCOUNT_READ::HANDLING = ""
	OE_ACCOUNT_READ::SALES = "Order Sales Acct" &
		IF OE_ACCOUNT_READ::SALES = ""
	OE_ACCOUNT_READ::COSACCT = "Order COS Acct" &
		IF OE_ACCOUNT_READ::COSACCT = ""

	EXIT FUNCTION

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", "OE_READ_ACCOUNT", FILENAME$, NUM1$(ERR))

	EXIT_STATUS = CMC$_UNTERROR
	GOTO ExitFunction

	END FUNCTION
