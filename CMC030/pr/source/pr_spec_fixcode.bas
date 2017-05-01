1	!
	! This program is used to fix up the codes in
	! Coastals Payroll files when they were entered
	! without the right codes.
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.HB"
	MAP (PR_TRN_DED) PR_TRN_DED_CDD PR_TRN_DED

	LINPUT "Folder (YYYYMMDD) "; BATCH_NO$

100	!
	!======================================================================
	! PR_TRN_DED file (open read only)
	!======================================================================

	PR_TRN_DED.CH% = 10%
	PR_TRN_DED.DEV$ = ""

	PR_TRN_DED.NAME$ = PR_TRN_DED.DEV$ + "PR_TRN_DED_" + BATCH_NO$ + ".JRL"

	OPEN PR_TRN_DED.NAME$ FOR INPUT AS FILE PR_TRN_DED.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PR_TRN_DED, &
		PRIMARY KEY &
		( &
			PR_TRN_DED::EMPNUM, &
			PR_TRN_DED::PR_END_DATE, &
			PR_TRN_DED::DTYPE, &
			PR_TRN_DED::CODE &
		)	DUPLICATES, &
		ACCESS MODIFY, ALLOW MODIFY

1000	!
	! Start file
	!
	RESET #PR_TRN_DED.CH%

1100	GET #PR_TRN_DED.CH%

	IF (PR_TRN_DED::CODE = "SU") AND (PR_TRN_DED::TAX_CODE = "")
	THEN
		PR_TRN_DED::TAX_CODE = "NJ"

		UPDATE #PR_TRN_DED.CH%
		PRINT PR_TRN_DED::EMPNUM
	END IF

	GOTO 1100

32767	END
