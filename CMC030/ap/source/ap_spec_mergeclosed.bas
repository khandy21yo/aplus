1	%TITLE "Accounts Payable Closing Program"
	%SBTTL "AP_SPEC_MERGECLOSED"
	%IDENT "V3.6a Calico"

	!
	!	COPYRIGHT (C) 1999 BY
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
	!	$ BAS AP_SOURCE:AP_SPEC_MERGECLOSED/LINE
	!	$ LINK/EXECUTABLE=AP_EXE: AP_SPEC_MERGECLOSED, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_SPEC_MERGECLOSED.OBJ;*
	!
	! Author:
	!
	!	04/22/99 - Kevin Handy
	!
	! Modification history:
	!
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Map's
	!
	%INCLUDE "SOURCE:[AP.OPEN]AP_CLOSE.HB"
	MAP (AP_CLOSE) AP_CLOSE_CDD AP_CLOSE


	!*******************************************************************
	! Open files
	!*******************************************************************
	!
	! Open up the file to merge into
	!
	WHEN ERROR IN
		CALL ASSG_CHANNEL(AP_CLOSE.CH%, STAT%)
		AP_CLOSE.DEV$ = ""

		AP_CLOSE.NAME$ = AP_CLOSE.DEV$+"AP_CLOSE.LED"

		OPEN AP_CLOSE.NAME$ AS FILE AP_CLOSE.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP AP_CLOSE, &
			PRIMARY KEY &
			( &
				AP_CLOSE::VENNUM, &
				AP_CLOSE::TRANKEY &
			)	DUPLICATES, &
			ALTERNATE KEY &
			( &
				AP_CLOSE::VENNUM, &
				AP_CLOSE::INVNUM &
			)	DUPLICATES CHANGES, &
			ALTERNATE KEY &
				AP_CLOSE::BATCH &
				DUPLICATES CHANGES, &
			ACCESS MODIFY, ALLOW NONE
	USE
		PRINT "Unable to open current AP_CLOSE file"
		CONTINUE Done
	END WHEN

 ItsEmpty:
	!
	! Open up the original file
	!
	WHEN ERROR IN
		CALL ASSG_CHANNEL(AP_CLOSE_ORIG.CH%, STAT%)

		AP_CLOSE_ORIG.NAME$ = AP_CLOSE.DEV$+"AP_CLOSE_NEWSTUFF.LED"

		OPEN AP_CLOSE_ORIG.NAME$ FOR INPUT AS FILE AP_CLOSE_ORIG.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP AP_CLOSE, &
			PRIMARY KEY &
			( &
				AP_CLOSE::VENNUM, &
				AP_CLOSE::TRANKEY &
			)	DUPLICATES, &
			ALTERNATE KEY &
			( &
				AP_CLOSE::VENNUM, &
				AP_CLOSE::INVNUM &
			)	DUPLICATES CHANGES, &
			ALTERNATE KEY &
				AP_CLOSE::BATCH &
				DUPLICATES CHANGES, &
			ACCESS READ, ALLOW NONE
	USE
		PRINT "Cannot open original file! " + ERT$(ERR)
		CONTINUE Done
	END WHEN


	!*******************************************************************
	! Copy over the original file
	!*******************************************************************

	RESET #AP_CLOSE_ORIG.CH%

	WHILE 1%
		WHEN ERROR IN
			GET #AP_CLOSE_ORIG.CH%
			PUT #AP_CLOSE.CH%

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
