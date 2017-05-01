1	%TITLE "Payroll Conversion"
	%SBTTL "PR_SPEC_FIXMILES"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1999 BY
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
	!	.p
	!	This program fixes Miles Produce employee file after a
	!	conversion was run.
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_SPEC_FIXMILES/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_SPEC_FIXMILES, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_SPEC_FIXMILES.OBJ;*
	!
	! Author:
	!
	!	09/24/99 - Kevin Handy
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


	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP	(PR_EMP_MASTER)		PR_EMP_MASTER_CDD	PR_EMP_MASTER


100	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.MOD"

1000	RESET #PR_EMP_MASTER.CH%

2000	WHEN ERROR IN
		GET #PR_EMP_MASTER.CH%
	USE
		CLOSE PR_EMP_MASTER.CH%
		CONTINUE 32767
	END WHEN

2100	!
	PRINT PR_EMP_MASTER::EMPNUM

	PR_EMP_MASTER::RATE_TYPE = "H"
	PR_EMP_MASTER::RATE_CDE = "TX"
	PR_EMP_MASTER::ACTIVE_FLAG = "N"

2200	UPDATE #PR_EMP_MASTER.CH%

	GOTO 2000

32767	END
