1	%TITLE "Return current default directory"
	%SBTTL "READ_DEFAULT"
	%IDENT "V3.6a Calico"

	FUNCTION STRING READ_DEFAULT

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
	!	This function returns a string containing the logical
	!	equivalence name of the given logical name or the given
	!	logical name if there is no equivalence.
	!
	! Index:
	!
	! Parameters:
	!
	!	LOGNAM1$
	!		The passed logical name the user wants to translate.
	!
	!	Returned value
	!		This function returns a string containing the logical
	!		equivalence name of the given logical name.
	!
	! Example:
	!
	!	NAME$ = READ_DEFAULT("CMC$CLIPBOARD")
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:READ_DEFAULT/NOLINE
	!	$ LIB FUNC_LIB:CMC_3VECTOR/REP READ_DEFAULT
	!	$ DELETE READ_DEFAULT.OBJ;*
	!
	! AUTHOR:
	!
	!	05/20/91 - Kevin Handy
	!
	! MODIFICATION HISTORY:
	!
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! External functions
	!
	EXTERNAL LONG FUNCTION SYS$SETDDIR
	EXTERNAL STRING FUNCTION READ_SYSLOG

	%PAGE

	B$ = SPACE$(64%)
	A% = SYS$SETDDIR(, B% BY REF, B$)

	READ_DEFAULT = READ_SYSLOG("SYS$DISK") + LEFT(B$, B%)

32767	END FUNCTION
