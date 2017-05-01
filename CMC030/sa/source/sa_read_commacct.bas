1	%TITLE "Read Commission Accounting"
	%SBTTL "SA_READ_COMMACCT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG SA_READ_COMMACCT(LOCATION$, SALESTYPE$, &
		SA_COMMACCT_CDD SA_COMMACCT_READ)


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
	!	This function returns the record from the commission account
	!	by using the location number and the salesman type.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	!
	! Input:
	!
	!	LOCATION$ is a location number
	!	SALESTYPE$ is an salesman type
	!
	! Output:
	!
	!	SA_COMMACCT is the record
	!
	! Example:
	!
	! Compile:
	!
	!	$ BAS SA_SOURCE:SA_READ_COMMACCT/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP SA_READ_COMMACCT
	!	$ DELETE SA_READ_COMMACCT.OBJ;*
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
	!	04/30/92 - Dan Perkins
	!		Return description of accounts if undefined.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/29/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	08/04/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Common Statements
	!

	%PAGE

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	COM(SA_READ_COMMACCT.COM) SA_COMMACCT.CH%

	DECLARE LONG EXIT_STATUS

	!
	! Include CDD'S
	!
	%INCLUDE "SOURCE:[SA.OPEN]SA_COMMACCT.HB"
	MAP	(SA_COMMACCT)	SA_COMMACCT_CDD	SA_COMMACCT

	!
	! Set initial value
	!
	EXTERNAL INTEGER FUNCTION COMP_STRING

	!
	! Assume undefined
	!
	EXIT_STATUS = CMC$_UNDEFINED

	SA_COMMACCT_READ::EXPACCT = ""
	SA_COMMACCT_READ::PAYACCT = ""

	!
	! Open SA_COMMACCT file
	!
1000	IF SA_COMMACCT.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[SA.OPEN]SA_COMMACCT.OPN"
		USE
			CONTINUE ExitFunction IF ERR = 5%
			FILENAME$ = "SA_COMMACCT"
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Find SA_COMMACCT file
	!
2000	WHEN ERROR IN
		RESET #SA_COMMACCT.CH%
	USE
		CONTINUE ExitFunction IF ERR = 9%
		FILENAME$ = "SA_COMMACCT"
		CONTINUE HelpError
	END WHEN

 GetNextRec:

	!
	! Get SA_COMMACCT file
	!
2010	WHEN ERROR IN
		GET #SA_COMMACCT.CH%, REGARDLESS
	USE
		CONTINUE ExitFunction IF ERR = 11% OR ERR = 9%
		FILENAME$ = "SA_COMMACCT"
		CONTINUE HelpError
	END WHEN

	IF (COMP_STRING(TRM$(LOCATION$), SA_COMMACCT::LOCATION) = -1%) AND &
		(COMP_STRING(TRM$(SALESTYPE$), SA_COMMACCT::SALTYPE) = -1%)
	THEN
		SA_COMMACCT_READ = SA_COMMACCT
		EXIT_STATUS = CMC$_NORMAL
		GOTO ExitFunction
	END IF

	!
	! Get the next record
	!
	GOTO GetNextRec

 ExitFunction:
	SA_READ_COMMACCT = EXIT_STATUS

	SA_COMMACCT_READ::EXPACCT = "Sales Com Exp Acct" &
		IF SA_COMMACCT_READ::EXPACCT = ""
	SA_COMMACCT_READ::PAYACCT = "Sales Com Pay Acct" &
		IF SA_COMMACCT_READ::PAYACCT = ""

	EXIT FUNCTION

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", "SA_READ_COMMACCT", FILENAME$, NUM1$(ERR))

	EXIT_STATUS = CMC$_UNTERROR
	GOTO ExitFunction

	END FUNCTION
