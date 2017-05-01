1	%TITLE "Add Missing Transaction Codes"
	%SBTTL "UTL_WRIT_TRANSTYPE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG UTL_WRIT_TRANSTYPE ( STRING TRANSTYPE())

	!
	! COPYRIGHT (C) 1990 BY
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
	! CMC assumes no responsibility for the use or reliability of
	! its software on equipment which is not supported by CMC.
	!
	!++
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This function puts missing transaction codes in the
	!	Transaction file.
	!	.lm -5
	!
	! Index:
	!
	! Output:
	!	transaction type table
	!
	! Example:
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UTL_WRIT_TRANSTYPE/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP UTL_WRIT_TRANSTYPE
	!	$ DELETE UTL_WRIT_TRANSTYPE.OBJ;*
	!
	! Author:
	!
	!	09/13/91 - Frank F. Starman
	!
	! Modification history:
	!
	!	02/21/92 - Frank F. Starman
	!		Add TO and LS codes.
	!
	!	04/13/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!		Lose extra parameter on ASSG_FREECHANNEL.
	!
	!	08/13/96 - Kevin Handy
	!		Change 'CASE = x' to 'CASE x' twice.
	!		Recormat source code.
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/19/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_TRANSTYPE.HB"
	MAP (UTL_TRANSTYPE) UTL_TRANSTYPE_CDD	UTL_TRANSTYPE

	DECLARE LONG EXIT_STATUS

	%PAGE

	TRANSTYPE(0%) = "19"
	TRANSTYPE(1%) = "+CC Cycle Counting Adjustment"
	TRANSTYPE(2%) = "-IS Issue from the Inventory"
	TRANSTYPE(3%) = "+MA Manufacturing Production"
	TRANSTYPE(4%) = "+LS Lost Sale"
	TRANSTYPE(5%) = "+MO Manufacturing Order"
	TRANSTYPE(6%) = "+PO On Purchase Order"
	TRANSTYPE(7%) = "+RE Receiver"
	TRANSTYPE(8%) = "-RQ Requisition"
	TRANSTYPE(9%) = "+RT Return to the Inv"
	TRANSTYPE(10%) = "-SA Regular Sale"
	TRANSTYPE(11%) = "-SE Employee's Sale"
	TRANSTYPE(12%) = "-SO On Sales Order"
	TRANSTYPE(13%) = "-SP Promotional Sales"
	TRANSTYPE(14%) = "-TO On Ticket Order"
	TRANSTYPE(15%) = "+TR Transfer between Location"
	TRANSTYPE(16%) = "-WA Waste"
	TRANSTYPE(17%) = "+WO On Work Order"
	TRANSTYPE(18%) = "-WR Warranty"

	%PAGE
	ON ERROR GOTO 19000

	EXIT_STATUS = CMC$_NORMAL

1000	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_TRANSTYPE.CRE"
	USE
		FILENAME$ = "UTL_TRANSTYPE"
		CONTINUE HelpError
	END WHEN

	FOR I% = 1% TO VAL%(TRANSTYPE(0%)) - 1%

		UTL_TRANSTYPE::CODE = MID(TRANSTYPE(I%), 2%, 2%)
		UTL_TRANSTYPE::DESCRIPTION = RIGHT(TRANSTYPE(I%), 5%)
		UTL_TRANSTYPE::TRANSSIGN = LEFT(TRANSTYPE(I%), 1%)

		SELECT UTL_TRANSTYPE::CODE
		CASE "SO", "MO", "RQ", "TO", "LS"
			UTL_TRANSTYPE::CLASS = "02"
		CASE "PO", "WO"
			UTL_TRANSTYPE::CLASS = "03"
		CASE ELSE
			UTL_TRANSTYPE::CLASS = "01"
		END SELECT

		WHEN ERROR IN
			PUT #UTL_TRANSTYPE.CH%
		USE
			CONTINUE 1010 IF ERR = 134%
			FILENAME$ = "UTL_TRANSTYPE"
			CONTINUE HelpError
		END WHEN

1010	NEXT I%

 ExitFunction:
	CLOSE UTL_TRANSTYPE.CH%
	CALL ASSG_FREECHANNEL(UTL_TRANSTYPE.CH%)

	UTL_WRIT_TRANSTYPE = EXIT_STATUS
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

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	FILENAME$ = ""
	RESUME HelpError

	END FUNCTION
