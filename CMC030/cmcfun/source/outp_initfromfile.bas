1	%TITLE "Initilize REPORT Output Information"
	%SBTTL "OUTP_INITFROMFILE"
	%IDENT "V3.6a Calico"

	SUB OUTP_INITFROMFILE(UTL_REPORTX_CDD UTL_REPORTX, WORD XWIDTH)

	!
	!	COPYRIGHT (C) 1986 BY
	!	Software Solutions, Inc.
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
	! Software Solutions, Inc.
	!
	! Software Solutions, Inc. assumes no responsibility for the use
	! or reliability of its software on equipment which is not supported
	! by Software Solutions, Inc.
	!
	!++
	!
	! Abstract:HELP
	!	.p
	!	This function initilizes the REPORT output functions.
	!
	! Parameters:
	!
	!	UTL_REPORTX
	!		The file used to initilize the report functions.
	!
	!	XWIDTH
	!		The returned variable used for the report output.
	!
	!	Returned value
	!		Initilizes the report output functions and other
	!		information the file has.
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:OUTP_INITFROMFILE/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP OUTP_INITFROMFILE
	!	$ DELETE OUTP_INITFROMFILE.OBJ;*
	!
	! Author:
	!
	!	10/19/95 - Kevin Handy
	!		Original function rename to OUTP_36INITFROMFILE,
	!		with different parameters. This function written
	!		to reference the changed function.
	!
	! Modification history:
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"

	%PAGE

	ON ERROR GO BACK

	!
	! Open keyboard if not open
	!
	IF (SCOPE::SMG_OPTION = 0%)
	THEN
		CALL READ_INITIALIZE
	END IF

	CALL OUTP_36INITFROMFILE(SCOPE, UTL_REPORTX, XWIDTH)

	END SUB
