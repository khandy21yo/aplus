1	%TITLE "Convert AP Zip Code from RSTS/E"
	%SBTTL "AP_SPEC_CONVERTZIP"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1987, 1988 BY
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
	!
	! Index:
	!
	! Option:
	!
	!	AP_SPEC_CONVERTZIP$HELP
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_SPEC_CONVERTZIP/LINE
	!	$ LINK/EXEC:AP_EXE AP_SPEC_CONVERTZIP,FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_SPEC_CONVERTZIP.OBJ;*
	!
	! Author:
	!
	!	06/19/89 - Kevin Handy
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	05/21/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--
	%PAGE

	OPTION SIZE=(INTEGER LONG, REAL GFLOAT)

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP (AP_VENDOR)		AP_VENDOR_CDD	AP_VENDOR

	ON ERROR GOTO 19000

100	APSYS.CH% = 1%
	OPEN "APSYS1.ASC" FOR INPUT AS FILE APSYS.CH%

200	!======================================================================
	! AP_VENDOR file (open read/write)
	!======================================================================

	AP_VENDOR.CH% = 2%
	AP_VENDOR.DEV$ = ""

	AP_VENDOR.NAME$ = AP_VENDOR.DEV$ + "AP_VENDOR.MAS"

	OPEN AP_VENDOR.NAME$ FOR INPUT AS FILE AP_VENDOR.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP AP_VENDOR, &
		PRIMARY KEY &
			AP_VENDOR::VENNUM, &
		ALTERNATE KEY &
			AP_VENDOR::VENNAM &
			DUPLICATES CHANGES, &
		ALTERNATE KEY &
			AP_VENDOR::ALPSRT &
			DUPLICATES CHANGES, &
		ALTERNATE KEY &
			AP_VENDOR::STATE &
			DUPLICATES CHANGES, &
		ALTERNATE KEY &
			AP_VENDOR::ZIP &
			DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY


1000	LINPUT #APSYS.CH%, INLINE$
	VENNUM$ = LEFT(INLINE$, 6%)
	VENZIP$ = RIGHT(INLINE$, 7%)

1050	GET #AP_VENDOR.CH%, KEY #0% EQ VENNUM$

	AP_VENDOR::ZIP = VENZIP$

	UPDATE #AP_VENDOR.CH%

	PRINT ".";

	GOTO 1000

2000	CLOSE AP_VENDOR.CH%
	CLOSE APSYS.CH%
	GOTO 32767

19000	SELECT ERL
	CASE 1000%
		RESUME 2000

	CASE 1050%
		RESUME 1000
	END SELECT

	ON ERROR GOTO 0

32767	END
