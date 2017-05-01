1	%TITLE "Convert from RSTS/E to VMS"
	%SBTTL "GL_CONV_CONVERT_01"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1987, 1988 BY
	!
	! Computer Management Center, Inc.
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
	! Abstract:HELP
	!	.B
	!	.LM +5
	!	This program is used in the conversion from RSTS/E
	!	to VMS.
	!	.LM -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_CONV_CONVERT_01/LINE
	!	$ LINK/EXECUTABLE=GL_EXE: GL_CONV_CONVERT_01, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_CONV_CONVERT_01.OBJ;*
	!
	! Author:
	!
	!	01/12/86 - Kevin Handy and B. Craig Larsen
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/14/98 - Kevin Handy
	!		Change '%INCLUDE %FROM %CDD' to '%INCLUDE'
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/13/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "SOURCE:[GL.OPEN]GL_BUD_YYYY.HB"
	MAP (GL_BUD_YYYY) GL_BUD_YYYY_CDD GL_BUD_YYYY

	DIM BDGT$(16%)

100	WHEN ERROR IN
		OPEN "GLSYS1.ASC" FOR INPUT AS FILE 1%
	USE
		PRINT "Unable to find ascii file"
		EXIT HANDLER
	END WHEN

110	!======================================================================
	! GL_BUD_YYYY file (create, open read/write)
	!======================================================================

	GL_BUD_YYYY.CH% = 2%
	GL_BUD_YYYY.DEV$ = ""
	GL_BUD_YYYY.NAME$ = GL_BUD_YYYY.DEV$ + "GL_BUD_1989.MAS"

	WHEN ERROR IN
		OPEN GL_BUD_YYYY.NAME$ AS FILE GL_BUD_YYYY.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP GL_BUD_YYYY, &
			PRIMARY KEY &
			GL_BUD_YYYY::ACCT, &
			ACCESS MODIFY, ALLOW MODIFY
	USE
		PRINT "Unable to create budget file"
		EXIT HANDLER
	END WHEN

200	!
	! Main loop
	!
	WHEN ERROR IN
		LINPUT #1%, BDGT$(I%) FOR I% = 1% TO 15%
	USE
		CONTINUE 300 IF ERR = 11%
		EXIT HANDLER
	END WHEN

250	!
	! Copy over data
	!
	GL_BUD_YYYY::ACCT = BDGT$(1%)
	GL_BUD_YYYY::DOLLAR(I%) = VAL(BDGT$(I% + 2%)) FOR I% = 1% TO 12%
	GL_BUD_YYYY::DOLLAR(13%) = 0.0
	GL_BUD_YYYY::UNIT(I%) = 0.0 FOR I% = 1% TO 13%
	GL_BUD_YYYY::HOUR(I%) = 0.0 FOR I% = 1% TO 13%

	PUT #GL_BUD_YYYY.CH%

	GOTO 200

300	!
	! Finished
	!
	CLOSE #1%
	CLOSE #GL_BUD_YYYY.CH%

32767	END
