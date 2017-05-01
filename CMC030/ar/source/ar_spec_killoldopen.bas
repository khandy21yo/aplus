1	%TITLE "Accounts Payable Closing Program"
	%SBTTL "AR_SPEC_KILLOLDOPEN"
	%IDENT "V3.6a Calico"

	!
	!	COPYRIGHT (C) 2009 BY
	!	Software Solutions, Inc.
	!	Idaho Falls, Idaho.
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
	!	.b
	!	.lm +5
	!	This special program is used to kill old data in the AR
	!	register.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_SPEC_KILLOLDOPEN/LINE
	!	$ LINK/EXECUTABLE=AR_EXE: AR_SPEC_KILLOLDOPEN, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_SPEC_KILLOLDOPEN.OBJ;*
	!
	! Author:
	!
	!	02/01/2000 - Kevin Handy
	!		Based on AP_CLOSE_KILLOLDOPEN
	!
	! Modification history:
	!
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Map's
	!
	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.HB"
	MAP (AR_OPEN) AR_OPEN_CDD AR_OPEN


	!*******************************************************************
	! Open files
	!*******************************************************************
	!
	! Open up the file to merge into
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.MOD"
	USE
		PRINT "Unable to open current AR_OPEN file"
		CONTINUE Done
	END WHEN


	!*******************************************************************
	! Loop over the original file
	!*******************************************************************

	RESET #AR_OPEN.CH%

	WHILE 1%
		WHEN ERROR IN
			GET #AR_OPEN.CH%
		USE
			CONTINUE StartAddit
		END WHEN

		IF AR_OPEN::UPDATED < "2009"
		THEN
			DELETE #AR_OPEN.CH%
			PRINT "d";
		ELSE
			PRINT ".";
		END IF

		PRINT IF CCPOS(0%) >= 50%
	NEXT

	!*******************************************************************
	! Add additional records
	!*******************************************************************

 StartAddit:
 Done:

	END
