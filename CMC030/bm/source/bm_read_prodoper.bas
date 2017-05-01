1	%TITLE "Read Labor Hours"
	%SBTTL "BM_READ_PRODOPER"
	%IDENT "V3.6a Calico"

	FUNCTION LONG BM_READ_PRODOPER( STRING PRODUCT, STRING OPERATION, &
		STRING NEXT_GET, &
		STRING EFFDATE, &
		BM_PRODOPER_CDD BM_PRODOPER_READ)

	!
	! COPYRIGHT (C) 1992 BY
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
	!	This function returns the labor hours for a given product and
	!	operation.
	!	.lm -5
	!
	! Index:
	!
	! Input:
	!
	!	PRODUCT		is a product number
	!
	!	OPERATION	is the operation required to make the product
	!
	!	NEXT_GET	is passed as "GT" or "EQ" and it means weather
	!			the program is to get the record asked for or
	!			the next one.
	!
	!	EFFDATE		is the date on which this particular hourly
	!			labor became effective for this product operation.
	!
	! Output:
	!
	!	BM_PRODOPER_READ	is the record whic is returned if the
	!				function returns a successful record.
	!
	! Example:
	!
	!
	! Compile:
	!
	!	$ BAS BM_SOURCE:BM_READ_PRODOPER/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP BM_READ_PRODOPER
	!	$ DELETE BM_READ_PRODOPER.OBJ;*
	!
	! Author:
	!
	!	08/07/92 - Dan Perkins
	!
	! Modification history:
	!
	!	08/14/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	05/15/95 - Kevin Handy
	!		Format to 80 columns.
	!
	!	02/08/96 - Kevin Handy
	!		Lose date check. since you can only have one
	!		labor rate in the file. This should probibly
	!		be fixed to handle the dates.
	!
	!	10/17/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	07/14/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Include CDD'S
	!
	%INCLUDE "SOURCE:[BM.OPEN]BM_PRODOPER.HB"
	MAP (BM_PRODOPER)	BM_PRODOPER_CDD		BM_PRODOPER

	!
	! Common Statements
	!
	COM (CH_BM_PRODOPER_READ) BM_PRODOPER.CH%

	DECLARE LONG   EXIT_STATUS
	DECLARE STRING ORIG_PRODUCT
	DECLARE STRING LAST_OPERATION

	%PAGE

	!
	! Set initial value
	!
	ORIG_PRODUCT = PRODUCT
	LAST_OPERATION = ""
	EXIT_STATUS = CMC$_UNDEFINED

	!
	! Open BM_PRODOPER file
	!
1000	IF BM_PRODOPER.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[BM.OPEN]BM_PRODOPER.OPN"
		USE
			CONTINUE ExitFunction IF ERR = 5%
			FILENAME$ = "BM_PRODOPER"
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Get BM_PRODOPER file
	!
17300	WHEN ERROR IN
		IF EDIT$(NEXT_GET, -1%) = "EQ"
		THEN
			FIND #BM_PRODOPER.CH%, &
				KEY #1% EQ PRODUCT + OPERATION, &
				REGARDLESS
		ELSE
			FIND #BM_PRODOPER.CH%, &
				KEY #1% GT PRODUCT + OPERATION, &
				REGARDLESS
		END IF
	USE
		CONTINUE ExitFunction IF ERR = 155%
		FILENAME$ = "BM_PRODOPER"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17320	WHEN ERROR IN
		GET #BM_PRODOPER.CH%, REGARDLESS
	USE
		CONTINUE ExitFunction IF ERR = 11%
		FILENAME$ = "BM_PRODOPER"
		CONTINUE HelpError
	END WHEN

	GOTO ExitFunction IF BM_PRODOPER::PRODUCT <> ORIG_PRODUCT

	GOTO ExitFunction IF BM_PRODOPER::OPERATION <> LAST_OPERATION AND &
		LAST_OPERATION <> ""

17330	LAST_OPERATION = BM_PRODOPER::OPERATION

 !	IF BM_PRODOPER::EFFDATE <= EFFDATE
 !	THEN
		BM_PRODOPER_READ = BM_PRODOPER
		EXIT_STATUS  = CMC$_NORMAL
		GOTO GetNextRec
 !	END IF

	!
	! Go for next line
	!
 !	GOTO GetNextRec

 ExitFunction:
	BM_READ_PRODOPER = EXIT_STATUS

	EXIT FUNCTION

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	EXIT_STATUS = CMC$_UNTERROR
	GOTO ExitFunction

	END FUNCTION
