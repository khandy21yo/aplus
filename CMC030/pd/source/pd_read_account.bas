1	%TITLE "Read Product Type Account File"
	%SBTTL "PD_READ_ACCOUNT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PD_READ_ACCOUNT(STRING LOCATION, &
		STRING PRODTYPE, &
		PD_ACCOUNT_CDD PD_ACCOUNT_READ)

	!
	! COPYRIGHT (C) 1989 BY
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
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This function returns a General Ledger Account
	!	number from the Product Type Account file.
	!	.lm -5
	!
	! Index:
	!
	! Input:
	!
	!	LOCATION	is a location number
	!	PRODTYPE	is a product type
	!
	! Output:
	!
	!	PD_ACCOUNT_READ	is a data structure containing, among other
	!		things, the GL Account number
	!
	!	Returned Value is CMC$_NORMAL if the record was found
	!		without mishap, CMC$_UNDEFINED if the record could
	!		not be found, or CMC$_UNTERROR if there was some
	!		other error.
	!
	! Compile:
	!
	!	$ BAS PD_SOURCE:PD_READ_ACCOUNT/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP PD_READ_ACCOUNT
	!	$ DELETE PD_READ_ACCOUNT.OBJ;*
	!
	! Author:
	!
	!	07/18/88 - Frank Starman
	!
	! Modification history:
	!
	!	03/25/91 - Frank F. Starman
	!		Allow wildcard characters for product type and location.
	!
	!	04/30/92 - Frank F. Starman
	!		Return description of accounts if undefined.
	!
	!	06/12/92 - Kevin Handy
	!		Clean up (check)
	!
	!	08/28/92 - Dan Perkins
	!		Modified to accomodate addition of PRICEVARACCT and
	!		UNUSED fileds.
	!
	!	06/15/94 - Kevin Handy
	!		Took over UNUSED field for MISCH2ACCT.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	11/20/95 - Kevin Handy
	!		Clean up source code.
	!
	!	10/20/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	01/10/2000 - Kevin Handy
	!		Modified to pull in the PD_ACCOUNT tables only once,
	!		and then use them from memory ever after. Hopefully
	!		this will speed up the function quite a bit.
	!
	!	01/10/2000 - Kevin Handy
	!		Lose error trap to 19000.
	!--

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include special CMC codes
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! CDD inclusions
	!
	DECLARE INTEGER CONSTANT MAX_ACCOUNT = 150%

	%INCLUDE "SOURCE:[PD.OPEN]PD_ACCOUNT.HB"
	MAP (PD_ACCOUNT)	PD_ACCOUNT_CDD		PD_ACCOUNT
	MAP (XXX_PD_ACCOUNT) &
		LONG XXX_PD_COUNT, &
		PD_ACCOUNT_CDD XXX_PD_ACCOUNT(MAX_ACCOUNT)

	!
	! Common memory areas
	!
	COM (PD_READ_ACCOUNT.COM) PD_ACCOUNT.CH%

	%PAGE

	!
	! Set up error trapping
	!
 !	ON ERROR GOTO 19000

	!
	! Assume undefined
	!
	EXIT_STATUS = CMC$_UNDEFINED

	PD_ACCOUNT_READ::INVACCT = ""
	PD_ACCOUNT_READ::WIPACCT = ""
	PD_ACCOUNT_READ::COSACCT = ""
	PD_ACCOUNT_READ::DISCACCT = ""
	PD_ACCOUNT_READ::MISCHACCT = ""
	PD_ACCOUNT_READ::PRICEVARACCT = ""
	PD_ACCOUNT_READ::MISCH2ACCT = ""

	%PAGE

	!
	! Open the PD Account file if not already open
	!
1000	IF PD_ACCOUNT.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PD.OPEN]PD_ACCOUNT.OPN"
		USE
			CONTINUE ExitFunction IF ERR = 5%
			FILENAME$ = "PD_ACCOUNT"
			CONTINUE HelpError
		END WHEN
	END IF

	IF XXX_PD_COUNT = 0%
	THEN
		WHEN ERROR IN
			RESET #PD_ACCOUNT.CH%
		USE
			CONTINUE 2000
		END WHEN

		WHILE (1%)

			WHEN ERROR IN
				GET #PD_ACCOUNT.CH%, REGARDLESS
			USE
				CONTINUE 2000
			END WHEN

			WHEN ERROR IN
				XXX_PD_ACCOUNT(XXX_PD_COUNT) = PD_ACCOUNT
				XXX_PD_COUNT = XXX_PD_COUNT + 1%
			USE
				FILENAME$ = "PD_ACCOUNT"
				CONTINUE HelpError
			END WHEN
		NEXT
	END IF

2000 !	WHEN ERROR IN
 !		RESET #PD_ACCOUNT.CH%
 !	USE
 !		CONTINUE ExitFunction IF ERR = 9%
 !		FILENAME$ = "PD_ACCOUNT"
 !		CONTINUE HelpError
 !	END WHEN
 !
 ! GetNextRec:
	!
	! Get the PD_ACCOUNT record and get it ready to return to the program
	!
 !2010	WHEN ERROR IN
 !		GET #PD_ACCOUNT.CH%, REGARDLESS
 !	USE
 !		CONTINUE ExitFunction IF ERR = 11% OR ERR = 155%
 !		FILENAME$ = "PD_ACCOUNT"
 !		CONTINUE HelpError
 !	END WHEN
 !
 !	IF (COMP_STRING(TRM$(PRODTYPE), PD_ACCOUNT::PRODTYPE) AND &
 !		COMP_STRING(TRM$(LOCATION), PD_ACCOUNT::LOCATION))
 !	THEN
 !		PD_ACCOUNT_READ = PD_ACCOUNT
 !		EXIT_STATUS = CMC$_NORMAL
 !		GOTO ExitFunction
 !	END IF

	TRM_PRODTYPE$ = TRM$(PRODTYPE)
	TRM_LOCATION$ = TRM$(LOCATION)

	FOR I% = 0% TO XXX_PD_COUNT - 1%

		IF (COMP_STRING(TRM_PRODTYPE$, XXX_PD_ACCOUNT(I%)::PRODTYPE) AND &
			COMP_STRING(TRM_LOCATION$, XXX_PD_ACCOUNT(I%)::LOCATION))
		THEN
			PD_ACCOUNT_READ = XXX_PD_ACCOUNT(I%)
			EXIT_STATUS = CMC$_NORMAL
			GOTO ExitFunction
		END IF

	NEXT I%

	!
	! Get the next record
	!
 !	GOTO GetNextRec

 ExitFunction:
	PD_READ_ACCOUNT = EXIT_STATUS

	PD_ACCOUNT_READ::INVACCT   = "Inventory Acct" &
		IF PD_ACCOUNT_READ::INVACCT = ""

	PD_ACCOUNT_READ::WIPACCT   = "WIP Acct" &
		IF PD_ACCOUNT_READ::WIPACCT = ""

	PD_ACCOUNT_READ::COSACCT   = "Prod COS  Acct" &
		IF PD_ACCOUNT_READ::COSACCT = ""

	PD_ACCOUNT_READ::DISCACCT  = "Prod Disc Acct" &
		IF PD_ACCOUNT_READ::DISCACCT = ""

	PD_ACCOUNT_READ::MISCHACCT = "Prod Misc Acct" &
		IF PD_ACCOUNT_READ::MISCHACCT = ""

	PD_ACCOUNT_READ::PRICEVARACCT = "Prod Prc Var Acct" &
		IF PD_ACCOUNT_READ::PRICEVARACCT = ""

	PD_ACCOUNT_READ::MISCH2ACCT = "Prod Misc 2 Acct" &
		IF PD_ACCOUNT_READ::MISCH2ACCT = ""

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

	%PAGE

 !19000	******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Trap untrapped errors
	!
 !	FILENAME$ = "PD_ACCOUNT"
 !	RESUME HelpError

32767	!******************************************************************
	! End of function PD_READ_ACCOUNT
	!******************************************************************
	END FUNCTION
