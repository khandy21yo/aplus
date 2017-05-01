1	%TITLE "Account Function"
	%SBTTL "SB_READ_ACCOUNT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG SB_READ_ACCOUNT(STRING SYSTEM, STRING ACCOUNT, &
		STRING FLAG)

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
	!	This function returns the flag by using the system
	!	and the account number.
	!
	! Index:
	!
	! Option:
	!
	! Input:
	!
	!	SYSTEM is the system
	!	ACCOUNT is the account number
	!
	! Output:
	!
	!	FLAG is the flag returned
	!
	! Example:
	!
	! Compile:
	!
	!	$ BAS SB_SOURCE:SB_READ_ACCOUNT/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP SB_READ_ACCOUNT
	!	$ DELETE SB_READ_ACCOUNT.OBJ;*
	!
	! Author:
	!
	!	07/10/90 - Lance Williams
	!
	! Modification history:
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
	!	12/12/95 - Kevin Handy
	!		Reformat source cloer to 80 columns.
	!
	!	06/04/97 - Kevin Handy
	!		Use integer for #key
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

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	COM(SB_READ_ACCOUNT.COM) SB_ACCOUNT.CH%

	DECLARE LONG EXIT_STATUS

	!
	! Include CDD'S
	!
	%INCLUDE "SOURCE:[SB.OPEN]SB_ACCOUNT.HB"
	MAP	(SB_ACCOUNT)	SB_ACCOUNT_CDD	SB_ACCOUNT

	!
	! Assume undefined
	!
	EXIT_STATUS = CMC$_UNDEFINED
	FLAG = ""

	!
	! Open SB_ACCOUNT file
	!
1000	IF SB_ACCOUNT.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[SB.OPEN]SB_ACCOUNT.OPN"
		USE
			CONTINUE ExitFunction IF ERR = 5%
			FILENAME$ = "SB_ACCOUNT"
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Find SB_ACCOUNT file
	!
2000	WHEN ERROR IN
		FIND #SB_ACCOUNT.CH%, KEY #0% EQ SYSTEM, REGARDLESS
	USE
		CONTINUE ExitFunction IF ERR = 9% OR ERR = 155%
		FILENAME$ = "SB_ACCOUNT"
		CONTINUE HelpError
	END WHEN

 GetNextRec:

	!
	! Get SB_ACCOUNT file
	!
	WHEN ERROR IN
		GET #SB_ACCOUNT.CH%, REGARDLESS
	USE
		CONTINUE ExitFunction IF ERR = 11% OR ERR = 9%
		FILENAME$ = "SB_ACCOUNT"
		CONTINUE HelpError
	END WHEN

	GOTO ExitFunction IF SB_ACCOUNT::SYSTEM <> SYSTEM

	IF COMP_STRING(ACCOUNT, SB_ACCOUNT::ACCOUNT)
	THEN
		FLAG = SB_ACCOUNT::ACCTGROUP
		EXIT_STATUS = CMC$_NORMAL
		GOTO ExitFunction
	END IF

	!
	! Get the next record
	!
	GOTO GetNextRec

 ExitFunction:
	SB_READ_ACCOUNT = EXIT_STATUS

	EXIT FUNCTION

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", "SB_READ_ACCOUNT", FILENAME$, NUM1$(ERR))

	EXIT_STATUS = CMC$_UNTERROR
	GOTO ExitFunction

	END FUNCTION
