1	%TITLE "Erase a Posted Batch"
	%SBTTL "AP_SPEC_JETAUTO"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 2003 BY
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
	!	.b
	!	.lm +5
	!	This program is used to move a batch in the AP OPEN
	!	file when posted to the wrong period.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_SPEC_JETAUTO/LINE
	!	$ LINK/EXE=AP_EXE: AP_SPEC_JETAUTO, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_SPEC_JETAUTO.OBJ;*
	!
	! Author:
	!
	!	09/10/2003 - Kevin Handy
	!
	! Modification history:
	!
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!******************************************************************
	! External modules needed
	!******************************************************************

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!******************************************************************
	! Set up data storage areas (MAPs, DIMENSIONs, DECLAREs)
	!******************************************************************

	!
	! CDD inclusions
	!
	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.HB"
	MAP	(AP_OPEN)	AP_OPEN_CDD	AP_OPEN

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP	(AP_VENDOR)	AP_VENDOR_CDD	AP_VENDOR

	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN_DIST.HB"
	MAP	(AP_OPEN_DIST)	AP_OPEN_DIST_CDD	AP_OPEN_DIST

	%PAGE

 Initialization:
	!******************************************************************
	! Get ready to begin
	!******************************************************************

	!
	! Initialize for output
	!

	PRINT "Wildcard Vendor number ";
	LINPUT WILDCARD$

	!++
	! Abstract:FLD01
	!	^*(01) Wildcard Vendor Number\*
	!	.b
	!	.lm +5
	!	.lm -5
	!
	! Index:
	!
	!--

	PRINT "Accounting Period Shift";
	INPUT SHIFT%

	!++
	! Abstract:FLD03
	!	^*(03) Accounting Period Shift\*
	!	.b
	!	.lm +5
	!	The number of periods to shift the retained history.
	!	.lm -5
	!
	! Index:
	!	.x Accounting Period
	!	.x Period>Accounting
	!
	!--

	%PAGE

300	!******************************************************************
	! Open all files
	!******************************************************************

	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.MOD"
	USE
		CONTINUE 320
	END WHEN

	WHEN ERROR IN
		RESET #AP_OPEN.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF
		CONTINUE 320
	END WHEN

310	WHEN ERROR IN
		GET #AP_OPEN.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF
		CONTINUE 320
	END WHEN

	IF COMP_STRING(AP_OPEN::VENNUM, WILDCARD$)
	THEN
		IF (AP_OPEN::UPDATED <> "        ")
		THEN
			AP_OPEN::UPDATED = &
				DATE_INVMCODE(DATE_MONCODE(AP_OPEN::UPDATED) + &
				SHIFT%)
		END IF

		IF (AP_OPEN::CLOSEDATE <> "      ")
		THEN
			AP_OPEN::CLOSEDATE = &
				DATE_INVMCODE( &
				DATE_MONCODE(AP_OPEN::CLOSEDATE) + SHIFT%)
		END IF

		UPDATE #AP_OPEN.CH%
		PRINT "+";
	ELSE
		DELETE #AP_OPEN.CH%
		PRINT "-";
	END IF

 !	PRINT ".";
	PRINT IF CCPOS(0%) >= 50%

	GOTO 310

320	CLOSE #AP_OPEN.CH%


400	!******************************************************************
	! Open all files
	!******************************************************************

	PRINT
	PRINT

	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.MOD"
	USE
		CONTINUE 420
	END WHEN

	WHEN ERROR IN
		RESET #AP_VENDOR.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF
		CONTINUE 420
	END WHEN

410	WHEN ERROR IN
		GET #AP_VENDOR.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF
		CONTINUE 420
	END WHEN

	IF COMP_STRING(AP_VENDOR::VENNUM, WILDCARD$)
	THEN
		PRINT "+";
	ELSE
		DELETE #AP_VENDOR.CH%
		PRINT "-";
	END IF

 !	PRINT ".";
	PRINT IF CCPOS(0%) >= 50%

	GOTO 410

420	CLOSE #AP_VENDOR.CH%


 ExitProgram:
	GOTO 32767

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	PRINT "Error: "; ERT$(ERR); " at "; ERL

32767	!******************************************************************
	! End of report GL_SPEC_MOVEBATCH
	!******************************************************************
	END
