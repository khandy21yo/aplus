1	%TITLE "Read Reason Account Table"
	%SBTTL "OE_READ_REASONACCT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG OE_READ_REASONACCT(STRING REASON, &
		STRING LOCATION, &
		OE_REASONACCT_CDD OE_REASONACCT_READ)

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
	!	This function returns the record by using the reason
	!	code, and the location number.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	!
	! Input:
	!
	!	REASON is a reason code
	!	LOCATION is the location number
	!
	! Output:
	!
	!	OE_REASONACCT is the account returned
	!
	! Example:
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_READ_REASONACCT/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP OE_READ_REASONACCT
	!	$ DELETE OE_READ_REASONACCT.OBJ;*
	!
	! Author:
	!
	!	04/15/92 - Frank F. Starman
	!
	! Modification history:
	!
	!	04/30/92 - Dan Perkins
	!		Return description of accounts if undefined.
	!
	!	06/12/92 - Kevin Handy
	!		Clean up (check)
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
	!	08/01/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%PAGE

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	COM(OE_READ_REASONACCT) OE_REASONACCT.CH%

	DECLARE LONG EXIT_STATUS

	!
	! Include CDD'S
	!
	%INCLUDE "SOURCE:[OE.OPEN]OE_REASONACCT.HB"
	MAP (OE_REASONACCT)	OE_REASONACCT_CDD	OE_REASONACCT

	!
	! Assume undefined
	!
	EXIT_STATUS = CMC$_UNDEFINED

	OE_REASONACCT_READ::ACCOUNT = ""

	!
	! Open OE_REASONACCT file
	!
1000	IF OE_REASONACCT.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[OE.OPEN]OE_REASONACCT.OPN"
		USE
			CONTINUE ExitFunction IF ERR = 5%
			FILENAME$ = "OE_REASONACCT"
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Find OE_REASONACCT file
	!
2000	WHEN ERROR IN
		RESET #OE_REASONACCT.CH%
	USE
		CONTINUE ExitFunction IF ERR = 5%
		FILENAME$ = "OE_REASONACCT"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
	!
	! Get OE_REASONACCT file
	!
2010	WHEN ERROR IN
		GET #OE_REASONACCT.CH%, REGARDLESS
	USE
		CONTINUE ExitFunction IF ERR = 11% OR ERR = 9%
		FILENAME$ = "OE_REASONACCT"
		CONTINUE HelpError
	END WHEN

	IF COMP_STRING(TRM$(REASON), OE_REASONACCT::CREASON) AND &
		COMP_STRING(TRM$(LOCATION), OE_REASONACCT::LOCATION)
	THEN
		OE_REASONACCT_READ = OE_REASONACCT
		EXIT_STATUS = CMC$_NORMAL
		GOTO ExitFunction
	END IF

	!
	! Get the next record
	!
	GOTO GetNextRec

 ExitFunction:
	OE_REASONACCT_READ::ACCOUNT = "Reason Acct" &
		IF OE_REASONACCT_READ::ACCOUNT = ""

	OE_READ_REASONACCT = EXIT_STATUS

	EXIT FUNCTION

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", "OE_READ_REASONACCT", FILENAME$, NUM1$(ERR))

	EXIT_STATUS = CMC$_UNTERROR
	GOTO ExitFunction

	END FUNCTION
