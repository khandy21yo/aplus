1	%TITLE "Move Unposted Items"
	%SBTTL "PR_SPEC_MOVEUNPOSTED"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1988 BY
	!
	! Computer Management Center, Inc.
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
	! Abstract:HELP
	!	.p
	!	This program is a special routine to fix a problem
	!	at Coastal Group where they entered the payroll into
	!	the wrong file.
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_SPEC_MOVEUNPOSTED/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_SPEC_MOVEUNPOSTED, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_SPEC_MOVEUNPOSTED.OBJ;*
	!
	! Author:
	!
	!	05/30/89 - Kevin Handy
	!
	! Modification history:
	!
	!	04/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.HB"
	MAP (PR_TRN_PAY)	PR_TRN_PAY_CDD	PR_TRN_PAY

	ON ERROR GOTO 19000

100	!======================================================================
	! PR_TRN_PAY file (open read only)
	!======================================================================

	PR_TRN_PAY.CH% = 10%
	PR_TRN_PAY.DEV$ = ""

	PR_TRN_PAY.NAME$ = PR_TRN_PAY.DEV$+"PR_TRN_PAY_19890520.JRL"

	OPEN PR_TRN_PAY.NAME$ FOR INPUT AS FILE PR_TRN_PAY.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PR_TRN_PAY, &
		PRIMARY KEY &
		( &
			PR_TRN_PAY::EMPNUM, &
			PR_TRN_PAY::PR_END_DATE &
		)	DUPLICATES, &
		ALTERNATE KEY &
		( &
			PR_TRN_PAY::SUBACC, &
			PR_TRN_PAY::OPER, &
			PR_TRN_PAY::ACCT &
		)	DUPLICATES CHANGES, &
		ALTERNATE KEY &
		( &
			PR_TRN_PAY::LOCATION, &
			PR_TRN_PAY::DEPT, &
			PR_TRN_PAY::WORK_CENTER, &
			PR_TRN_PAY::EMPNUM, &
			PR_TRN_PAY::PR_END_DATE &
		)	DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

200	!======================================================================
	! PR_TRN_PAY file (open read only)
	!======================================================================

	PR_TRN_PAY.CH_NEW% = 11%
	PR_TRN_PAY.DEV$ = ""

	PR_TRN_PAY.NAME$ = PR_TRN_PAY.DEV$+"PR_TRN_PAY_19890527.JRL"

	OPEN PR_TRN_PAY.NAME$ FOR INPUT AS FILE PR_TRN_PAY.CH_NEW%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PR_TRN_PAY, &
		PRIMARY KEY &
		( &
			PR_TRN_PAY::EMPNUM, &
			PR_TRN_PAY::PR_END_DATE &
		)	DUPLICATES, &
		ALTERNATE KEY &
		( &
			PR_TRN_PAY::SUBACC, &
			PR_TRN_PAY::OPER, &
			PR_TRN_PAY::ACCT &
		)	DUPLICATES CHANGES, &
		ALTERNATE KEY &
		( &
			PR_TRN_PAY::LOCATION, &
			PR_TRN_PAY::DEPT, &
			PR_TRN_PAY::WORK_CENTER, &
			PR_TRN_PAY::EMPNUM, &
			PR_TRN_PAY::PR_END_DATE &
		)	DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

300	RESET #PR_TRN_PAY.CH%

400	GET #PR_TRN_PAY.CH%

	IF (PR_TRN_PAY::UPDATE_FLAG AND 4%) = 0%
	THEN
		PR_TRN_PAY::PR_END_DATE = "19890527"
		PUT #PR_TRN_PAY.CH_NEW%
		DELETE #PR_TRN_PAY.CH%
		PRINT "N";
	ELSE
		PRINT "O";
	END IF

	GOTO 400

500	CLOSE #PR_TRN_PAY.CH%
	CLOSE #PR_TRN_PAY.CH_NEW%

	GOTO 32767

19000	!
	! Trap errors
	!
	RESUME 500 IF ERR=11% AND ERL=400%

	ON ERROR GOTO 0

32767	END
