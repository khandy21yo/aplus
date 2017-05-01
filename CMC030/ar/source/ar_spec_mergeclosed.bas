1	%TITLE "Accounts Payable Closing Program"
	%SBTTL "AR_SPEC_MERGECLOSED"
	%IDENT "V3.6a Calico"

	!
	!	COPYRIGHT (C) 2000 BY
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
	!	This special program is used to merge two closed
	!	files together that may have duplications.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_SPEC_MERGECLOSED/LINE
	!	$ LINK/EXECUTABLE=AR_EXE: AR_SPEC_MERGECLOSED, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_SPEC_MERGECLOSED.OBJ;*
	!
	! Author:
	!
	!	02/01/2000 - Kevin Handy
	!		Based on AP_CLOSE_MERGECLOSED
	!
	! Modification history:
	!
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Map's
	!
	%INCLUDE "SOURCE:[AR.OPEN]AR_CLOSED.HB"
	MAP (AR_CLOSED) AR_CLOSED_CDD AR_CLOSED


	!*******************************************************************
	! Open files
	!*******************************************************************
	!
	! Open up the file to merge into
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CLOSED.MOD"
	USE
		PRINT "Unable to open current AR_CLOSED file"
		CONTINUE Done
	END WHEN

 ItsEmpty:
	!
	! Open up the original file
	!
	WHEN ERROR IN
	!======================================================================
	! AR_CLOSED file (open read only)
	!======================================================================

	CALL ASSG_CHANNEL(AR_CLOSED_ORIG.CH%, STAT%)
	CALL READ_DEVICE('AR_CLOSED', AR_CLOSED.DEV$, STAT%)

	AR_CLOSED_ORIG.NAME$ = AR_CLOSED.DEV$ + "AR_CLOSED_NEWSTUFF.LED"

	OPEN AR_CLOSED_ORIG.NAME$ FOR INPUT AS FILE AR_CLOSED_ORIG.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP AR_CLOSED, &
		PRIMARY KEY &
		( &
			AR_CLOSED::CUSNUM, &
			AR_CLOSED::INVNUM, &
			AR_CLOSED::TRATYP &
		)	DUPLICATES, &
		ALTERNATE KEY &
		( &
			AR_CLOSED::SALNUM, &
			AR_CLOSED::CUSNUM, &
			AR_CLOSED::INVNUM, &
			AR_CLOSED::TRATYP &
		)	DUPLICATES CHANGES, &
		ACCESS READ, ALLOW MODIFY
	USE
		PRINT "Cannot open original file! " + ERT$(ERR)
		CONTINUE Done
	END WHEN


	!*******************************************************************
	! Copy over the original file
	!*******************************************************************

	RESET #AR_CLOSED_ORIG.CH%

	WHILE 1%
		WHEN ERROR IN
			GET #AR_CLOSED_ORIG.CH%
			PUT #AR_CLOSED.CH%

			PRINT "A";
			PRINT IF CCPOS(0%) >= 50%
		USE
			CONTINUE StartAddit
		END WHEN
	NEXT

	!*******************************************************************
	! Add additional records
	!*******************************************************************

 StartAddit:
 Done:

	END
