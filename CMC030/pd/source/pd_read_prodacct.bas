1	%TITLE "Read Product Account"
	%SBTTL "PD_READ_PRODACCT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PD_READ_PRODACCT(LOCATION$, PRODTYPE$, &
		PD_PRODACCT_CDD PD_PRODACCT_READ)

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
	!	This function returns the product record by using the location
	!	number and the product type.
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
	!	PRODTYPE$ is an product type
	!
	! Output:
	!
	!	PD_PRODACCT is the record
	!
	! Example:
	!
	! Compile:
	!
	!	$ BAS PD_SOURCE:PD_READ_PRODACCT/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP PD_READ_PRODACCT
	!	$ DELETE PD_READ_PRODACCT.OBJ;*
	!
	! Author:
	!
	!	07/10/90 - Lance Williams
	!
	! Modification history:
	!
	!	02/28/92 - Kevin Handy
	!		Changed "CMC$NORMAL" to "CMC$_NORMAL"
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
	!	08/02/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Common Statements
	!
	COM(PD_READ_PRODACCT.COM) PD_PRODACCT.CH%

	DECLARE LONG EXIT_STATUS

	%PAGE

	!
	! Include CDD'S
	!
	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODACCT.HB"
	MAP	(PD_PRODACCT)	PD_PRODACCT_CDD	PD_PRODACCT

	EXTERNAL INTEGER FUNCTION COMP_STRING

	!
	! Assume undefined
	!
	EXIT_STATUS = CMC$_UNDEFINED

	PD_PRODACCT_READ::INVACC   = ""
	PD_PRODACCT_READ::COSACCT  = ""
	PD_PRODACCT_READ::DISCACCT = ""

	!
	! Open PD_PRODACCT file
	!
1000	IF PD_PRODACCT.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PD.OPEN]PD_PRODACCT.OPN"
		USE
			CONTINUE ExitFunction IF ERR = 5%
			FILENAME$ = "PD_PRODACCT"
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Find PD_PRODACCT file
	!
	WHEN ERROR IN
		RESET #PD_PRODACCT.CH%
	USE
		CONTINUE ExitFunction IF ERR = 9%
		FILENAME$ = "PD_PRODACCT"
		CONTINUE HelpError
	END WHEN

 GetNextRec:

	!
	! Get PD_PRODACCT file
	!
2010	WHEN ERROR IN
		GET #PD_PRODACCT.CH%, REGARDLESS
	USE
		CONTINUE ExitFunction IF ERR = 11% OR ERR = 9% OR ERR = 155%
		FILENAME$ = "PD_PRODACCT"
		CONTINUE HelpError
	END WHEN

	IF (COMP_STRING(TRM$(LOCATION$), PD_PRODACCT::LOCATION) = -1%) AND &
		(COMP_STRING(TRM$(PRODTYPE$), PD_PRODACCT::PRODTYPE) = -1%)
	THEN
			PD_PRODACCT_READ = PD_PRODACCT
			EXIT_STATUS = CMC$_NORMAL
			GOTO ExitFunction
	END IF

	!
	! Get the next record
	!
	GOTO GetNextRec

 ExitFunction:
	PD_READ_PRODACCT = EXIT_STATUS

	PD_PRODACCT_READ::INVACC   = "Inventory Acct" &
		IF PD_PRODACCT_READ::INVACC = ""
	PD_PRODACCT_READ::COSACCT  = "Prod COS  Acct" &
		IF PD_PRODACCT_READ::COSACCT = ""
	PD_PRODACCT_READ::DISCACCT = "Prod Disc Acct" &
		IF PD_PRODACCT_READ::DISCACCT = ""

	EXIT FUNCTION

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", "PD_READ_PRODACCT", FILENAME$, NUM1$(ERR))

	EXIT_STATUS = CMC$_UNTERROR
	GOTO ExitFunction

	END FUNCTION
