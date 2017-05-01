1	%TITLE "Get Protection Code from DEVICE File"
	%SBTTL "READ_PROTECTION"
	%IDENT "V3.6a Calico"

	SUB READ_PROTECTION(PROGNAME$, PROTECT$, STATUS%)
	!
	! COPYRIGHT (C) 1986 BY
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
	!	.b
	!	.lm +5
	!	This function reads the DEVICE file to get the protection
	!	code for the file. Will default device if it
	!	does not exist in the file.
	!	.lm -5
	!
	! Index:
	!
	! Parameters:
	!
	!	PROGNAME$
	!		The passed name of the file the user wants to get the
	!		protection code of.
	!
	!	STATUS%
	!		Returned : 0 = Successful, -1 = Failed
	!
	!	Returned value
	!		It will return the protection code of the file.
	!
	! Example:
	!
	!	UTL_BATCH_CONTROL.PRO$ = READ_PROTECTION('UTL_BATCH_CONTROL')
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:READ_PROTECTION/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP READ_PROTECTION
	!	$ DELETE READ_PROTECTION.OBJ;*
	!
	! AUTHOR:
	!
	!	11/20/86 - Kevin Handy
	!
	! MODIFICATION HISTORY:
	!
	!	08/12/88 - Kevin Handy
	!		Modified to open device file if is is not already
	!		open.  Added REGARDLESS to get.
	!
	!	10/27/92 - Kevin Handy
	!		Added readonly flag to channel.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	11/20/95 - Kevin Handy
	!		Clean up source code.
	!
	!	10/17/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/04/99 - Kevin Handy
	!		Lose variable 'DEVICE$'
	!
	!	06/30/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_DEVICE.HB"
	MAP (UTL_DEVICE) UTL_DEVICE_CDD UTL_DEVICE

	COM (CH_UTL_DEVICE) &
		UTL_DEVICE.CH%, &
		UTL_DEVICE.READONLY%

100	!
	! Open device file if it isn't already
	!
	IF UTL_DEVICE.CH% = 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_DEVICE.OPN"
		USE
			CALL ASSG_FREECHANNEL(UTL_DEVICE.CH%)
			STATUS% = -1%
			CONTINUE 2000
		END WHEN
	END IF

	!
	! Try to read device name from DEVICE
	!
	A$ = SPACE$(39%)
	LSET A$ = PROGNAME$

1000	WHEN ERROR IN
		GET #UTL_DEVICE.CH%, KEY #0% EQ A$, REGARDLESS
	USE
		PROTECT$ = ""
		STATUS% = -1%
		CONTINUE 2000
	END WHEN

	PROTECT$ = TRM$(UTL_DEVICE::PROCOD)
	STATUS% = 0%

2000	END SUB
