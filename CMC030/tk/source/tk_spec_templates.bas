1	%TITLE "Maintains Document Templates"
	%SBTTL "TK_SPEC_TEMPLATES"
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
	!	This program maintains the document templates.
	!
	! Index:
	!
	! Option:
	!
	! Author:
	!
	!	06/19/87 - B. Craig Larsen
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_SPEC_TEMPLATES/LINE
	!	$ LINK/EXECUTABLE=TK_EXE: TK_SPEC_TEMPLATES, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE TK_SPEC_TEMPLATES.OBJ;*
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/30/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--
	%PAGE

	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

1000	CALL READ_INITIALIZE

	CALL TK_MAIN_TEMPLATES

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

32767	END
