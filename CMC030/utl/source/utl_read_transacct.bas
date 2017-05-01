1	%TITLE "Read Transaction Account"
	%SBTTL "UTL_READ_TRANSACCT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG UTL_READ_TRANSACCT(LOCATION$, TRANSTYPE$, &
		PRODTYPE$, UTL_TRANSACCT_CDD UTL_TRANSACCT_READ)

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
	!	This function returns the transaction record by using the
	!	location number, transaction type, and product type.
	!
	! Index:
	!
	! Option:
	!
	! Input:
	!
	!	LOCATION$ is a location number
	!	TRANSTYPE$ is an transaction type
	!	PRODTYPE$ is an product type
	!
	! Output:
	!
	!	SA_COMMACCT is the record
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UTL_READ_TRANSACCT/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP UTL_READ_TRANSACCT
	!	$ DELETE UTL_READ_TRANSACCT.OBJ;*
	!
	! Author:
	!
	!	07/10/90 - Lance Williams
	!
	! Modification history:
	!
	!	04/03/91 - Frank F. Starman
	!		Transaction type cannot have wildcard characters.
	!		Use FIND instead RESET.
	!
	!	02/28/92 - Kevin Handy
	!		Changed "CMC$NORMAL" to "CMC$_NORMAL".
	!
	!	04/30/92 - Dan Perkins
	!		Return account description if undefined.
	!
	!	05/13/92 - Frank F. Starman
	!		Return more specific account description if undefined.
	!
	!	06/12/92 - Kevin Handy
	!		Clean up (check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	11/20/95 - Kevin Handy
	!		Clean up source code.
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	07/13/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Common Statements
	!
	COM (UTL_READ_TRANSACCT.COM) UTL_TRANSACCT.CH%

	DECLARE LONG EXIT_STATUS

	%PAGE

	!
	! Include CDD'S
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_TRANSACCT.HB"
	MAP (UTL_TRANSACCT) UTL_TRANSACCT_CDD UTL_TRANSACCT

	!
	! Set initial value
	!
	SELECT TRANSTYPE$

	CASE "CC"
		UTL_TRANSACCT_READ::ACCOUNT = "Cycle Count Acct"

	CASE "IS"
		UTL_TRANSACCT_READ::ACCOUNT = "Inv Issue Acct"

	CASE "MA"
		UTL_TRANSACCT_READ::ACCOUNT = "Manuf Prod Acct"

	CASE "RE"
		UTL_TRANSACCT_READ::ACCOUNT = "Inv Receiver Acct"

	CASE "RT"
		UTL_TRANSACCT_READ::ACCOUNT = "Return Inv Acct"

	CASE "SA"
		UTL_TRANSACCT_READ::ACCOUNT = "Reg Sale Acct"

	CASE "SE"
		UTL_TRANSACCT_READ::ACCOUNT = "Empl Sale Acct"

	CASE "SP"
		UTL_TRANSACCT_READ::ACCOUNT = "Prom Sale Acct"

	CASE "TR"
		UTL_TRANSACCT_READ::ACCOUNT = "Transfer Acct"

	CASE "WA"
		UTL_TRANSACCT_READ::ACCOUNT = "Waste Acct"

	CASE "WR"
		UTL_TRANSACCT_READ::ACCOUNT = "Warranty Acct"

	CASE ELSE
		UTL_TRANSACCT_READ::ACCOUNT = "Transaction Acct"
	END SELECT

	!
	! Open UTL_TRANSACCT file
	!
1000	IF UTL_TRANSACCT.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_TRANSACCT.OPN"
		USE
			CONTINUE ExitFunction IF ERR = 5%
			FILENAME$ = "UTL_TRANSACCT"
			CONTINUE HelpError
		END WHEN

	END IF

	!
	! Find UTL_TRANSACCT file
	!
	WHEN ERROR IN
		FIND #UTL_TRANSACCT.CH%, KEY #0% EQ TRANSTYPE$, REGARDLESS
	USE
		CONTINUE ExitFunction IF ERR = 9% OR ERR = 155%
		FILENAME$ = "UTL_TRANSACCT"
		CONTINUE HelpError
	END WHEN

 GetNextRec:

	!
	! Get UTL_TRANSACCT file
	!
2010	WHEN ERROR IN
		GET #UTL_TRANSACCT.CH%, REGARDLESS
	USE
		CONTINUE ExitFunction IF ERR = 11%
		FILENAME$ = "UTL_TRANSACCT"
		CONTINUE HelpError
	END WHEN

	IF COMP_STRING(TRM$(LOCATION$), UTL_TRANSACCT::LOCATION) AND &
		COMP_STRING(TRM$(PRODTYPE$), UTL_TRANSACCT::PRODTYPE)
	THEN
		UTL_TRANSACCT_READ = UTL_TRANSACCT
		EXIT_STATUS = CMC$_NORMAL
	ELSE
		!
		! Get the next record
		!
		GOTO GetNextRec
	END IF


 ExitFunction:
	UTL_READ_TRANSACCT = EXIT_STATUS

	EXIT FUNCTION

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", "UTL_READ_TRANSACCT", FILENAME$, NUM1$(ERR))

	EXIT_STATUS = CMC$_UNTERROR
	GOTO ExitFunction

	END FUNCTION
