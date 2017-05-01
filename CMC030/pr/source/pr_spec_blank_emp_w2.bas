1	%TITLE "Maintain W2 file"
	%SBTTL "PR_SPEC_BLANK_EMP_W2"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 2005 BY
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
	!	This option blanks the data in the special W2
	!	routines (PR_EMP_MASTER_W2)
	!
	! Index:
	!
	! Option:
	!
	!	PR_MAIN_EMP_W2$HELP
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_SPEC_BLANK_EMP_W2/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_SPEC_BLANK_EMP_W2, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_SPEC_BLANK_EMP_W2.OBJ;*
	!
	! Author:
	!
	!	01/17/2005 - Kevin Handy
	!
	! Modification history:
	!
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:PR_WINDOW.INC"

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_W2.HB"
	MAP (PR_EMP_W2) PR_EMP_W2_CDD PR_EMP_W2

1000	!
	! Open File
	!
	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_W2.MOD"

2000	RESET #PR_EMP_W2.CH%

4000	WHEN ERROR IN
		GET #PR_EMP_W2.CH%
	USE
		CONTINUE Finished
	END WHEN

	FOR I% = 0% TO 10%
		PR_EMP_W2::EARNINGS(I%) = 0.0
 !		PR_EMP_W2::EARNINGS_CODE(I%) = ""
		PR_EMP_W2::TAXES(I%) = 0.0
		PR_EMP_W2::TAXABLE(I%) = 0.0
 !		PR_EMP_W2::TAXES_CODE(I%) = ""
 !		PR_EMP_W2::TAXES_STATE(I%) = ""
 !		PR_EMP_W2::TAXES_ID(I%) = ""
		PR_EMP_W2::DEDUCTIONS(I%) = 0.0
 !		PR_EMP_W2::DEDUCTIONS_CODE(I%) = ""
	NEXT I%

	WHEN ERROR IN
		UPDATE #PR_EMP_W2.CH%
	USE
		PRINT
		PRINT "Can't update "; PR_EMP_W2::EMPNUM
		PRINT "Error: "; ERR; ERT$(ERR)
		CONTINUE FInished
	END WHEN

	PRINT ".";
	PRINT PR_EMP_W2::EMPNUM IF CCPOS(0%) >= 50%

	GOTO 4000

 Finished:
9000	!
	CLOSE #PR_EMP_W2.CH%

32767	END
