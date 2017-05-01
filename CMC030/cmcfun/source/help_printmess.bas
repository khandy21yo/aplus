1	%TITLE "Print and Display Help Message"
	%SBTTL "HELP_PRINTMESS"
	%IDENT "V3.6a Calico"

	SUB HELP_PRINTMESS(SCOPE_STRUCT SCOPE, &
		STRING MESSAGES, &
		STRING HELP_SEVERITY, &
		STRING HELP_PROGNAME, &
		STRING HELP_FILENAME, &
		STRING HELP_ITEM, &
		UTL_REPORTX_CDD UTL_REPORTX, &
		STRING HDR(), &
		INTEGER LINEOFF)
	!
	! COPYRIGHT (C) 1987 BY
	!
	! Computer Management Center
	! Idaho Falls, Idaho.
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
	!	.p
	!	This function displays messages to the screen and also
	!	send then to the printer.
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:HELP_PRINTMESS/NOLINE
	!	$ LIB FUNC_LIB:CMCFUN/REP HELP_PRINTMESS
	!	$ DELETE HELP_PRINTMESS.OBJ;*
	!
	! Author:
	!
	!	09/11/90 - Frank F. Starman
	!
	! Modification history:
	!
	!	03/26/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	03/25/96 - Kevin Handy
	!		Reformat source code.
	!
	!	03/25/96 - Kevin Handy
	!		Convert LINEOFF from 'WORD' to 'INTEGER',
	!		since that is what really gets passed in.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--

	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	!
	! Maps
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"

	CALL HELP_34MESSAGE(SCOPE, MESSAGES, HELP_SEVERITY, HELP_PROGNAME, &
		HELP_FILENAME, HELP_ITEM)

	IF HELP_SEVERITY = "E" OR HELP_SEVERITY = "F"
	THEN
		TEXT$ = "%" + HELP_PROGNAME + "-" + &
			HELP_SEVERITY + "-" + HELP_ITEM + " " + &
			EDIT$(HELP_FILENAME, 2% + 4% + 32%) + " " + MESSAGES
	ELSE
		TEXT$ = " " + MESSAGES
	END IF

	CALL OUTP_LINE("", UTL_REPORTX, HDR(), TEXT$, LINEOFF)

	END SUB
