	!======================================================================
	! PR_TRN_DED file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PR_TRN_DED.CH%, STAT%)
	CALL READ_DEVICE('PR_TRN_DED',PR_TRN_DED.DEV$, STAT%)

	PR_TRN_DED.NAME$ = PR_TRN_DED.DEV$+"PR_TRN_DED_"+BATCH_NO$+".JRL"

	OPEN PR_TRN_DED.NAME$ FOR INPUT AS FILE PR_TRN_DED.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PR_TRN_DED, &
		PRIMARY KEY &
		( &
			PR_TRN_DED::EMPNUM, &
			PR_TRN_DED::PR_END_DATE, &
			PR_TRN_DED::DTYPE, &
			PR_TRN_DED::CODE &
		)	DUPLICATES , &
		ACCESS MODIFY, ALLOW MODIFY

