1	%TITLE "Read Asset Account File"
	%SBTTL "AD_READ_ACCOUNT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AD_READ_ACCOUNT(STRING LOCA_NUM, &
		STRING ASST_TYP, &
		AD_ACCOUNT_CDD AD_ACCOUNT_READ)

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
	!
	! Abstract:HELP
	!	.lm +5
	!	.b
	!	This function, given a location number and an asset
	!	type, returns the associated AD Account record structure
	!	from the Asset Account file.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	!
	! Input:
	!
	!	LOCA_NUM	is the given location number
	!	ASST_TYP	is the asset type
	!
	! Output:
	!
	!	AD_ACCOUNT	is the record structure which contains the
	!		the three GL account numbers associated with the
	!		inputs, in addition to the location and asset type
	!		themselves.
	!	Returned Value	is CMC$_UNDEFINED if the record could not
	!		be found, CMC$_NORMAL if everything worked, or
	!		CMC$_UNTERROR if there was an error.
	!
	! Example:
	!
	!	STAT% = AD_READ_ACCOUNT( LOCATION$, TYPE$, AD_ACCOUNT)
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_READ_ACCOUNT/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP AD_READ_ACCOUNT
	!	$ DELETE AD_READ_ACCOUNT.OBJ;*
	!
	! Author:
	!
	!	06/16/89 - Aaron Redd
	!
	! Modification history:
	!
	!	04/30/92 - Dan Perkins
	!		Return description of accounts if undefined.
	!
	!	04/10/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!
	!	05/12/97 - Kevin Handy
	!		Reformat source code
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	07/14/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--

	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include the CMC special codes
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! CDD inclusions
	!
	%INCLUDE "SOURCE:[AD.OPEN]AD_ACCOUNT.HB"
	MAP	(AD_ACCOUNT)	AD_ACCOUNT_CDD	AD_ACCOUNT

	!
	! Common memory areas
	!
	COM (CH_AD_ACCOUNT) AD_ACCOUNT.CH%

	%PAGE

	!
	! Set initial value of AD_READ_ACCOUNT (assume failure)
	!
	EXIT_STATUS = CMC$_UNDEFINED

	AD_ACCOUNT_READ::ASS_ACCT = ""
	AD_ACCOUNT_READ::DEP_ACCT = ""
	AD_ACCOUNT_READ::EXP_ACCT = ""

	!
	! Open AD_ACCOUNT file if not already open (with a valid channel)
	!
1000	IF AD_ACCOUNT.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AD.OPEN]AD_ACCOUNT.OPN"
		USE
			CONTINUE ExitFunction IF ERR = 5%
			FILENAME$ = "AD_ACCOUNT"
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Get the record structure associated with the inputs
	!
2000	WHEN ERROR IN
		GET #AD_ACCOUNT.CH%, &
			KEY #0% EQ (LOCA_NUM + ASST_TYP), &
			REGARDLESS
	USE
		CONTINUE ExitFunction IF (ERR = 155%) OR (ERR = 9%)
		FILENAME$ = "AD_ACCOUNT"
		CONTINUE HelpError
	END WHEN

	!
	! If record is found, set it to be returned to the main program
	!
	AD_ACCOUNT_READ = AD_ACCOUNT

	!
	! Set value for success
	!
	AD_READ_ACCOUNT = CMC$_NORMAL

 ExitFunction:
	AD_READ_ACCOUNT = EXIT_STATUS

	AD_ACCOUNT_READ::ASS_ACCT = "Asset Account" &
		IF AD_ACCOUNT_READ::ASS_ACCT = ""
	AD_ACCOUNT_READ::DEP_ACCT = "Depreciation Acct" &
		IF AD_ACCOUNT_READ::DEP_ACCT = ""
	AD_ACCOUNT_READ::EXP_ACCT = "Expense Account" &
		IF AD_ACCOUNT_READ::EXP_ACCT = ""

	EXIT FUNCTION

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	GOTO ExitFunction

	END FUNCTION
