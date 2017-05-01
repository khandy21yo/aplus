1	%TITLE "Generates Controlling Number"
	%SBTTL "TK_SPEC_CODEGEN"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1987 BY
	!
	! Computer Management Center
	! Idaho Falls, Idaho.
	!
	! This software is furnished under a license and may be used and
	! copied only in accordance with terms of such license and with
	! the inclusion of the above copyright notice.  This software or
	! any other copies therof may not be provided or otherwise made
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
	! Abstract:HELP
	!	.p
	!	This program generates controlling codes for
	!	expiration date
	!
	! Index:
	!
	! Option:
	!
	! Author:
	!
	!	08/24/89 - Frank F. Starman
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_SPEC_CODEGEN
	!	$ LINK/EXEC=TK_EXE:*.EXE TK_SPEC_CODEGEN, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE TK_SPEC_CODEGEN.OBJ;*
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	04/27/95 - Kevin Handy
	!		Move modification history section to the bottom
	!		of the comment section.
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/06/2000 - Kevin Handy
	!		Use A"x"B
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	!
	! External functions
	!
	EXTERNAL LONG FUNCTION FUNC_BANNERCODE

	%PAGE

	INPUT "lICENCE"; LIC$
	INPUT "eXPdATE"; EDATE$

	PRINT STRING$(29%, A"#"B)
	PRINT "Controlling Number: ";FUNC_BANNERCODE(EDATE$ + LIC$)
	PRINT STRING$(29%, A"#"B)

	END
