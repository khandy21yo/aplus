1	%TITLE "Read Record from Register Order File"
	%SBTTL "OE_READ_REGHEADER"
	%IDENT "V3.6a Calico"

	FUNCTION LONG OE_READ_REGHEADER( STRING ORDER, &
		OE_REGHEADER_CDD OE_REGHEADER_READ)

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
	!	This function returns the record from Sales Order Register
	!	file by Order number.
	!	.lm -5
	!
	! Index:
	!
	! Input:
	!
	!	ORDER is a sales order number
	!
	! Output:
	!
	!	OE_REGHEADER is the record returned
	!
	! Example:
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_READ_REGHEADER/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP OE_READ_REGHEADER
	!	$ DELETE OE_READ_REGHEADER.OBJ;*
	!
	! Author:
	!
	!	09/13/90 - Frank F. Starman
	!
	! Modification history:
	!
	!	11/06/90 - Kevin Handy
	!		Fixed to return CMC$_NORMAL instead of CMC$NORMAL
	!		which is undefined, and the post program
	!		thought was an error.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/19/96 - Kevin Handy
	!		Reformat source code.
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

	COM (OE_READ_REGHEADER_CH) &
		OE_REGHEADER.CH%

	DECLARE LONG EXIT_STATUS

	!
	! Include CDD'S
	!
	%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.HB"
	MAP	(OE_REGHEADER)	OE_REGHEADER_CDD	OE_REGHEADER

	!
	! Assume undefined
	!
	EXIT_STATUS = CMC$_UNDEFINED

	!
	! Open OE_REGHEADER file
	!
1000	IF OE_REGHEADER.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.OPN"
		USE
			CONTINUE ExitFunction IF ERR = 5%
			FILENAME$ = "OE_REGHEADER"
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Find OE_REGHEADER file
	!
2000	WHEN ERROR IN
		GET #OE_REGHEADER.CH%, KEY #0% EQ ORDER, REGARDLESS
	USE
		CONTINUE ExitFunction IF ERR = 155% OR ERR = 9%
		FILENAME$ = "OE_REGHEADER"
		CONTINUE HelpError
	END WHEN

	OE_REGHEADER_READ = OE_REGHEADER
	EXIT_STATUS = CMC$_NORMAL

 ExitFunction:
	OE_READ_REGHEADER = EXIT_STATUS

	EXIT FUNCTION

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", "OE_READ_REGHEADER", FILENAME$, NUM1$(ERR))

	EXIT_STATUS = CMC$_UNTERROR
	GOTO ExitFunction

	END FUNCTION
