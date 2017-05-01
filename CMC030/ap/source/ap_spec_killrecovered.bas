1	%TITLE "Accounts Payable Vendor Maintenance"
	%SBTTL "AP_SPEC_KILLRECOVERED"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 2006 BY
	!
	! Software Solutions, Inc.
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
	! Software Solutions, Inc.
	!
	! Software Solutions, Inc. assumes no responsibility for the use
	! or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	! Abstract:HELP
	!	.P
	!
	! Index:
	!
	! Option:
	!
	! COMPILE:
	!
	!	$ BAS AP_SOURCE:AP_SPEC_KILLRECOVERED/LINE
	!	$ LINK/EXECUTABLE=AP_EXE:*.EXE AP_SPEC_KILLRECOVERED, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_SPEC_KILLRECOVERED.OBJ;*
	!
	! Author:
	!
	!	01/25/2006 - Kevin Handy
	!
	! Modification history:
	!
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	!
	! MAP's
	!
	%INCLUDE "SOURCE:[AP.OPEN]AP_CLOSE.HB"
	MAP	(AP_CLOSE)	AP_CLOSE_CDD	AP_CLOSE

	%PAGE

	CALL READ_INITIALIZE

	!
	! Open files
	!
300	%INCLUDE "SOURCE:[AP.OPEN]AP_CLOSE.MOD"

390	!
	! Handle one GL period
	!
	PRINT "Press return to continue";
	LINPUT YYYY_PP$


1100	!
	! SKip to the next batch
	!
	WHEN ERROR IN
		GET #AP_CLOSE.CH%
	USE
		CONTINUE ExitProgram
	END WHEN


	IF AP_CLOSE::SELECTED = "Z"
	THEN
		DELETE #AP_CLOSE.CH%
		PRINT "X";
		PRINT IF CCPOS(0%) >= 50%
	END IF

	GOTO 1100

	!*******************************************************************
	! Exit the program
	!*******************************************************************

 ExitProgram:

	PRINT "Done."

	GOTO 32767

32767	END
