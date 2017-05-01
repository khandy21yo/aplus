1	%TITLE "Sales Percentage Calculation"
	%SBTTL "OE_READ_SALESTAX"
	%IDENT "V3.6a Calico"

	FUNCTION LONG OE_READ_SALESTAX(TAXCODE$, TAXFLAG$, &
		OE_SALESTAX_CDD OE_SALESTAX_READ)

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
	!	This function returns from the percentage of tax that is
	!	charged.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	!
	! Input:
	!
	! Output:
	!
	! Example:
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_READ_SALESTAX/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP OE_READ_SALESTAX
	!	$ DELETE OE_READ_SALESTAX.OBJ;*
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
	!	04/30/92 - Frank F. Starman
	!		Return description of account if undefined.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/20/96 - Kevin Handy
	!		Reformat source code
	!
	!	05/28/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/13/98 - Kevin Handy
	!		Add new field TAXFLAG$ so that there is less
	!		external code to worry about.
	!
	!	08/02/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--

	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	COM (OE_READ_SALESTAX.COM) OE_SALESTAX.CH%

	DECLARE LONG EXIT_STATUS

	!
	! Include CDD'S
	!
	%INCLUDE "SOURCE:[OE.OPEN]OE_SALESTAX.HB"
	MAP	(OE_SALESTAX)	OE_SALESTAX_CDD	OE_SALESTAX

	!
	! Assume undefined
	!
	EXIT_STATUS = CMC$_UNDEFINED

	OE_SALESTAX_READ::STATEACC  = ""
	OE_SALESTAX_READ::STATETAX  = 0.0
	OE_SALESTAX_READ::CITYACC   = ""
	OE_SALESTAX_READ::CITYTAX   = 0.0
	OE_SALESTAX_READ::COUNTYACC = ""
	OE_SALESTAX_READ::COUNTYTAX = 0.0

	!
	! Handle all the non-taxable cases
	!
	IF TAXFLAG$ <> "1"
	THEN
		EXIT_STATUS = CMC$_NORMAL
		EXIT FUNCTION
	END IF

	!
	! Open OE_SALESTAX file
	!
1000	IF OE_SALESTAX.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[OE.OPEN]OE_SALESTAX.OPN"
		USE
			CONTINUE ExitFunction IF ERR = 5%
			FILENAME$ = "OE_SALESTAX"
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Find OE_SALESTAX file
	!
2000	WHEN ERROR IN
		FIND #OE_SALESTAX.CH%, KEY #0% EQ TAXCODE$, REGARDLESS
	USE
		CONTINUE ExitFunction IF ERR = 9% OR ERR = 155%
		FILENAME$ = "OE_SALESTAX"
		CONTINUE HelpError
	END WHEN

	!
	! Get OE_SALESTAX file
	!
	WHEN ERROR IN
		GET #OE_SALESTAX.CH%, REGARDLESS
	USE
		CONTINUE ExitFunction IF ERR = 11% OR ERR = 9%
		FILENAME$ = "OE_SALESTAX"
		CONTINUE HelpError
	END WHEN

	!
	! Calculate the percentage
	!
	OE_SALESTAX_READ = OE_SALESTAX
	EXIT_STATUS = CMC$_NORMAL

 ExitFunction:
	OE_READ_SALESTAX = EXIT_STATUS

	OE_SALESTAX_READ::STATEACC  = "Sales State Tax" &
		IF OE_SALESTAX_READ::STATEACC = ""
	OE_SALESTAX_READ::CITYACC   = "Sales City Tax" &
		IF OE_SALESTAX_READ::CITYACC = ""
	OE_SALESTAX_READ::COUNTYACC = "Sales County Tax" &
		IF OE_SALESTAX_READ::COUNTYACC = ""

	EXIT FUNCTION

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", "OE_READ_SALESTAX", FILENAME$, NUM1$(ERR))

	EXIT_STATUS = CMC$_UNTERROR
	GOTO ExitFunction

	END FUNCTION
