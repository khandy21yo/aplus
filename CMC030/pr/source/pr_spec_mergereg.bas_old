1	%TITLE "Merge 1st Quarter into Main Payroll"
	%SBTTL "PR_SPEC_MERGEREG"
	%IDENT "V3.6a Calico"

	!
	!		COPYRIGHT (C) 1989 BY
	!		Computer Management Center, Inc.
	!		Idaho Falls, Idaho.
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
	!	This program will merge in the first quarter payroll
	!	information (restored onto SYS$MANAGER, not the real
	!	payroll account) into the real payroll.
	!	NOTE: Replaces the first quarter information, does not
	!	add onto it.
	!	NOTE: This program has most information hardcoded into
	!	it for this thing for coastal, and all opens are
	!	pulled into this program to make it much faster to
	!	kermit.  (Size is everything)
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_SPEC_MERGEREG
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_SPEC_MERGEREG, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_SPEC_MERGEREG.OBJ;*
	!
	! Author:
	!
	!	07/14/89 - Kevin Handy
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	06/02/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--
	%PAGE

	!
	! Define options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Maps
	!
	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.HB"
	MAP (PR_REG_ERNDED_FROM) PR_REG_ERNDED_CDD PR_REG_ERNDED_FROM
	MAP (PR_REG_ERNDED) PR_REG_ERNDED_CDD PR_REG_ERNDED

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.HB"
	MAP (PR_REG_TAXES_FROM) PR_REG_TAXES_CDD PR_REG_TAXES_FROM
	MAP (PR_REG_TAXES) PR_REG_TAXES_CDD PR_REG_TAXES

	%PAGE

	ON ERROR GOTO 19000

	YYYY$ = "1989"		! 1989
	QTR% = 0%		! 1st Quarter

	!
	! Open all files (Assume files are on users account, or
	! SYS$MANAGER)
	!

300	!======================================================================
	! PR_REG_ERNDED file (open read/write)
	!======================================================================

	PR_REG_ERNDED.CH% = 10%
	PR_REG_ERNDED.DEV$ = ""

	PR_REG_ERNDED.NAME$ = PR_REG_ERNDED.DEV$+"PR_REG_ERNDED_"+YYYY$+".LED"

	OPEN PR_REG_ERNDED.NAME$ FOR INPUT AS FILE PR_REG_ERNDED.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PR_REG_ERNDED, &
		PRIMARY KEY &
		( &
			PR_REG_ERNDED::EMPNUM, &
			PR_REG_ERNDED::ETYPE, &
			PR_REG_ERNDED::CODE &
		), &
		ACCESS MODIFY, ALLOW MODIFY

310	!======================================================================
	! PR_REG_ERNDED file (open read only)
	! Modified to open on SYS$MANAGER, and to use other map than
	! the above PR_REG_ERNDED so that we can have both records
	! in memory at the same time.
	!======================================================================

	PR_REG_ERNDED_FROM.CH% = 11%
	PR_REG_ERNDED_FROM.DEV$ = "SYS$MANAGER:"

	PR_REG_ERNDED_FROM.NAME$ = PR_REG_ERNDED_FROM.DEV$ + &
		"PR_REG_ERNDED_" + YYYY$ + ".LED"

	OPEN PR_REG_ERNDED_FROM.NAME$ FOR INPUT &
		AS FILE PR_REG_ERNDED_FROM.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PR_REG_ERNDED_FROM, &
		PRIMARY KEY &
		( &
			PR_REG_ERNDED_FROM::EMPNUM, &
			PR_REG_ERNDED_FROM::ETYPE, &
			PR_REG_ERNDED_FROM::CODE &
		), &
		ACCESS READ, ALLOW MODIFY

320	!======================================================================
	! PR_REG_TAXES file (open read/write)
	!======================================================================

	PR_REG_TAXES.CH% = 12%
	PR_REG_TAXES.DEV$ = ""

	PR_REG_TAXES.NAME$ = PR_REG_TAXES.DEV$+"PR_REG_TAXES_"+YYYY$+".LED"

	OPEN PR_REG_TAXES.NAME$ FOR INPUT AS FILE PR_REG_TAXES.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PR_REG_TAXES, &
		PRIMARY KEY &
		( &
			PR_REG_TAXES::EMPNUM, &
			PR_REG_TAXES::TTYPE, &
			PR_REG_TAXES::CODE &
		), &
		ACCESS MODIFY, ALLOW MODIFY

330	!======================================================================
	! PR_REG_TAXES file (open read only)
	! Modified to open on SYS$MANAGER, and to use other map than
	! the above PR_REG_TAXES so that we can have both records
	! in memory at the same time.
	!======================================================================

	PR_REG_TAXES_FROM.CH% = 13%
	PR_REG_TAXES_FROM.DEV$ = "SYS$MANAGER:"

	PR_REG_TAXES_FROM.NAME$ = PR_REG_TAXES_FROM.DEV$ + &
		"PR_REG_TAXES_" + YYYY$ + ".LED"

	OPEN PR_REG_TAXES_FROM.NAME$ FOR INPUT AS FILE PR_REG_TAXES_FROM.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PR_REG_TAXES_FROM, &
		PRIMARY KEY &
		( &
			PR_REG_TAXES_FROM::EMPNUM, &
			PR_REG_TAXES_FROM::TTYPE, &
			PR_REG_TAXES_FROM::CODE &
		), &
		ACCESS READ, ALLOW MODIFY


1000	!*******************************************************************
	! First pass: Go through the ERNDED file
	!*******************************************************************

	RESET #PR_REG_ERNDED_FROM.CH%

1010	!
	! Get a record to add in
	!
	GET #PR_REG_ERNDED_FROM.CH%

	l% = l% + 1%
	print l%;chr$(13%);

1020	!
	! If record already exists, modify 1st quarter
	!
	GET #PR_REG_ERNDED.CH%, KEY #0% EQ &
		PR_REG_ERNDED_FROM::EMPNUM + &
		PR_REG_ERNDED_FROM::ETYPE + &
		PR_REG_ERNDED_FROM::CODE

1025	PR_REG_ERNDED::QTR_DOLL(QTR%) = PR_REG_ERNDED_FROM::QTR_DOLL(QTR%)
	PR_REG_ERNDED::REG_HRS(QTR%) = PR_REG_ERNDED_FROM::REG_HRS(QTR%)
	PR_REG_ERNDED::PRE_HRS(QTR%) = PR_REG_ERNDED_FROM::PRE_HRS(QTR%)
	PR_REG_ERNDED::UNITS(QTR%) = PR_REG_ERNDED_FROM::UNITS(QTR%)

	UPDATE #PR_REG_ERNDED.CH%

	GOTO 1010

1040	!
	! If record doesn't exist, create it
	!
	PR_REG_ERNDED::EMPNUM = PR_REG_ERNDED_FROM::EMPNUM
	PR_REG_ERNDED::ETYPE = PR_REG_ERNDED_FROM::ETYPE
	PR_REG_ERNDED::CODE = PR_REG_ERNDED_FROM::CODE
	PR_REG_ERNDED::UPDATE_COUNTER = PR_REG_ERNDED_FROM::UPDATE_COUNTER

	FOR I% = 0% TO 3%
		PR_REG_ERNDED::QTR_DOLL(I%) = 0.0
		PR_REG_ERNDED::REG_HRS(I%) = 0.0
		PR_REG_ERNDED::PRE_HRS(I%) = 0.0
		PR_REG_ERNDED::UNITS(I%) = 0.0
	NEXT I%

	PR_REG_ERNDED::QTR_DOLL(QTR%) = PR_REG_ERNDED_FROM::QTR_DOLL(QTR%)
	PR_REG_ERNDED::REG_HRS(QTR%) = PR_REG_ERNDED_FROM::REG_HRS(QTR%)
	PR_REG_ERNDED::PRE_HRS(QTR%) = PR_REG_ERNDED_FROM::PRE_HRS(QTR%)
	PR_REG_ERNDED::UNITS(QTR%) = PR_REG_ERNDED_FROM::UNITS(QTR%)

	PUT #PR_REG_ERNDED.CH%

	GOTO 1010

2000	!*******************************************************************
	! First pass: Go through the ERNDED file
	!*******************************************************************

	RESET #PR_REG_TAXES_FROM.CH%

2010	!
	! Get a record to add in
	!
	GET #PR_REG_TAXES_FROM.CH%

	j% = j% + 1%
	print j%;chr$(13%);

2020	!
	! If record already exists, modify 1st quarter
	!
	GET #PR_REG_TAXES.CH%, KEY #0% EQ &
		PR_REG_TAXES_FROM::EMPNUM + &
		PR_REG_TAXES_FROM::TTYPE + &
		PR_REG_TAXES_FROM::CODE

	PR_REG_TAXES::TAXABLE(QTR%) = PR_REG_TAXES_FROM::TAXABLE(QTR%)
	PR_REG_TAXES::REPORTABLE(QTR%) = PR_REG_TAXES_FROM::REPORTABLE(QTR%)
	PR_REG_TAXES::TAX(QTR%) = PR_REG_TAXES_FROM::TAX(QTR%)
	PR_REG_TAXES::WKWRK(QTR%) = PR_REG_TAXES_FROM::WKWRK(QTR%)

	UPDATE #PR_REG_TAXES.CH%

	GOTO 2010

2040	!
	! If record doesn't exist, create it
	!
	PR_REG_TAXES::EMPNUM = PR_REG_TAXES_FROM::EMPNUM
	PR_REG_TAXES::TTYPE = PR_REG_TAXES_FROM::TTYPE
	PR_REG_TAXES::CODE = PR_REG_TAXES_FROM::CODE
	PR_REG_TAXES::UPDATE_COUNTER = PR_REG_TAXES_FROM::UPDATE_COUNTER

	FOR I% = 0% TO 3%
		PR_REG_TAXES::TAXABLE(I%) = 0.0
		PR_REG_TAXES::REPORTABLE(I%) = 0.0
		PR_REG_TAXES::TAX(I%) = 0.0
		PR_REG_TAXES::WKWRK(I%) = 0.0
	NEXT I%

	PR_REG_TAXES::TAXABLE(QTR%) = PR_REG_TAXES_FROM::TAXABLE(QTR%)
	PR_REG_TAXES::REPORTABLE(QTR%) = PR_REG_TAXES_FROM::REPORTABLE(QTR%)
	PR_REG_TAXES::TAX(QTR%) = PR_REG_TAXES_FROM::TAX(QTR%)
	PR_REG_TAXES::WKWRK(QTR%) = PR_REG_TAXES_FROM::WKWRK(QTR%)

	PUT #PR_REG_TAXES.CH%

	GOTO 2010

8000	!*******************************************************************
	! End of the program
	!*******************************************************************

	CLOSE #PR_REG_ERNDED.CH%
	CLOSE #PR_REG_ERNDED_FROM.CH%
	CLOSE #PR_REG_TAXES.CH%
	CLOSE #PR_REG_TAXES_FROM.CH%

	GOTO 32767

19000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	SELECT ERL

	CASE 1010%
		RESUME 2000 IF ERR = 11%

	CASE 1020%
		RESUME 1040 IF ERR = 155%

	CASE 2010%
		RESUME 8000 IF ERR = 11%

	CASE 2020%
		RESUME 2040 IF ERR = 155%

	END SELECT

	ON ERROR GOTO 0

32767	END
