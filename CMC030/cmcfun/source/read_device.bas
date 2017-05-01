1	%TITLE "Get Device Name from DEVICE File"
	%SBTTL "READ_DEVICE"
	%IDENT "V3.6a Calico"

	SUB READ_DEVICE(PROGNAME$, DEVICENAME$, STATUS%)

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
	!	This function reads the DEVICE file to get the device
	!	which the file exists on. Will default device if it
	!	does not exist in the file.
	!	.lm -5
	!
	! Index:
	!
	! Parameters:
	!
	!	PROGNAME$
	!		The passed name of the file the user wants to get the
	!		device name of.
	!
	!	STATUS%
	!		Returned : 0 = Successful, -1 = Failed
	!
	!	Returned value
	!		It will return the device name of the file.
	!
	! Example:
	!
	!	CALL READ_DEVICE('UTL_BATCH_CONTROL',DEVICE$,STATUS)
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:READ_DEVICE/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP READ_DEVICE
	!	$ DELETE READ_DEVICE.OBJ;*
	!
	! AUTHOR:
	!
	!	07/15/88 - Lance Williams
	!
	! MODIFICATION HISTORY:
	!
	!	08/12/88 - Kevin Handy
	!		Modified to open device file if is is not already
	!		open.
	!
	!	10/27/92 - Kevin Handy
	!		Added the readonly flag to the ch common area
	!		to match up with all others.
	!
	!	11/22/93 - Frank F. Starman
	!		Do nothing if device is already assigned.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/17/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/23/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/04/99 - Kevin Handy
	!		Change field 'DEVICE' to 'DEVICENAME'
	!
	!	06/13/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Maps
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_DEVICE.HB"
	MAP (UTL_DEVICE) UTL_DEVICE_CDD UTL_DEVICE

	COM (CH_UTL_DEVICE) &
		UTL_DEVICE.CH%, &
		UTL_DEVICE.READONLY%

	%PAGE

	STATUS% = 0%

	GOTO 2000 IF EDIT$(DEVICENAME$, -1%) <> ""

100	!
	! Open device file if it isn't already
	!
	IF UTL_DEVICE.CH% = 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_DEVICE.OPN"
		USE
			CALL ASSG_FREECHANNEL(UTL_DEVICE.CH%)
			DEVICENAME$ = ""
			STATUS% = -1%
			CONTINUE 2000
		END WHEN
	END IF

	!
	! Try to read device name from DEVICE
	!
	A$ = SPACE$(39%)
	LSET A$ = PROGNAME$
	STATUS% = 0%

1000	WHEN ERROR IN
		GET #UTL_DEVICE.CH%, KEY #0% EQ A$, REGARDLESS
	USE
		DEVICENAME$ = ""
		STATUS% = -1%
		CONTINUE 2000
	END WHEN

	DEVICENAME$ = TRM$(UTL_DEVICE::DEVICENAME)

2000	END SUB
