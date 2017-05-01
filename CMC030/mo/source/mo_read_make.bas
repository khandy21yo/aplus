1	%TITLE "Read Record from Make Master File"
	%SBTTL "MO_READ_MAKE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG MO_READ_MAKE( STRING MAKE, STRING YEAR, STRING MTYPE, &
		STRING MSIZE, MO_MAKE_CDD MO_MAKE_READ)


	!
	! COPYRIGHT (C) 1991 BY
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
	!	This function returns the record from Make Master file
	!	by make, year, type, and size.
	!	.lm -5
	!
	! Index:
	!
	! Input:
	!
	!	MAKE is the entered make
	!	YEAR is the entered year
	!	TYPE is the entered type
	!	SIZE is the entered size
	!
	! Output:
	!
	!	MO_MAKE is the record returned
	!
	! Example:
	!
	!
	! Compile:
	!
	!	$ BAS MO_SOURCE:MO_READ_MAKE/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP MO_READ_MAKE
	!	$ DELETE MO_READ_MAKE.OBJ;*
	!
	! Author:
	!
	!	03/19/91 - Val James Allen
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/02/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	07/31/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%PAGE

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE


	COM (MO_READ_MAKE_CH) &
		MO_MAKE.CH%

	DECLARE LONG EXIT_STATUS

	!
	! Include CDD'S
	!
	%INCLUDE "SOURCE:[MO.OPEN]MO_MAKE.HB"
	MAP (MO_MAKE)		MO_MAKE_CDD		MO_MAKE

	!
	! Assume undefined
	!
	EXIT_STATUS = CMC$_UNDEFINED

	!
	! Open MO_MAKE file
	!
1000	IF MO_MAKE.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[MO.OPEN]MO_MAKE.OPN"
		USE
			CONTINUE ExitFunction IF ERR = 5%
			FILENAME$ = "MO_MAKE"
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Find MO_MAKE file
	!
1050	WHEN ERROR IN
		FIND #MO_MAKE.CH%, KEY #0% GE MAKE, REGARDLESS
	USE
		CONTINUE ExitFunction IF ERR = 155% OR ERR = 9%
		FILENAME$ = "MO_MAKE"
		CONTINUE HelpError
	END WHEN

2000	WHEN ERROR IN
		GET #MO_MAKE.CH%, REGARDLESS
	USE
		CONTINUE ExitFunction IF ERR = 11%
		FILENAME$ = "MO_MAKE"
		CONTINUE HelpError
	END WHEN

	GOTO ExitFunction IF MO_MAKE::MAKE <> MAKE
	GOTO ExitFunction IF MO_MAKE::YEAR > YEAR
	GOTO 2000 IF MO_MAKE::MTYPE <> MTYPE
	GOTO 2000 IF MO_MAKE::MSIZE <> MSIZE

	MO_MAKE_READ = MO_MAKE

	EXIT_STATUS = CMC$_NORMAL

	GOTO 2000

 ExitFunction:
	MO_READ_MAKE = EXIT_STATUS

	EXIT FUNCTION

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", "MO_READ_MAKE", FILENAME$, NUM1$(ERR))

	EXIT_STATUS = CMC$_UNTERROR
	GOTO ExitFunction

	END FUNCTION
