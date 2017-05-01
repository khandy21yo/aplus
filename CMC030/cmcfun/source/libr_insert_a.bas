1	%TITLE "LIBR_INSERT_A - Insert a Text File into a Library"
	%SBTTL ""
	%IDENT "V3.6a Calico"

	FUNCTION LONG LIBR_INSERT_A(KEY_NAME$, RFA TEXT.RFA)

	!
	!	COPYRIGHT (C) 1987 BY
	!	Computer Management Center, Idaho Falls, Idaho.
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
	! Abstract:
	!	^*LIBR_INSERT_A - Insert a Text File into a Library\*
	!	.b
	!	This function will insert text into a library,
	!	and append a key to that text.
	!
	! Index:
	!
	! Parameter:
	!
	!
	! Example:
	!
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:LIBR_INSERT_A/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP LIBR_INSERT_A
	!	$ DELETE LIBR_INSERT_A.OBJ;*
	!
	! Author:
	!
	!	01/07/87 - Kevin Handy
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE's
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! This sub-function is used to find all of the key names
	! associated with the given text record.
	!
	MAP (LBR_JUNKJUNK) K.NAME%, K.NAME$(64%) = 39%

	K.NAME% = K.NAME% + 1%
	K.NAME$(K.NAME%) = KEY_NAME$

	LIBR_INSERT_A = 1%

	END FUNCTION
