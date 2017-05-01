	!======================================================================
	! PR_HIS_DED file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PR_HIS_DED.CH%, STAT%)
	CALL READ_DEVICE('PR_HIS_DED',PR_HIS_DED.DEV$, STAT%)

	PR_HIS_DED.NAME$ = PR_HIS_DED.DEV$+"PR_HIS_DED_"+BATCH_NO$+".ARC"

	OPEN PR_HIS_DED.NAME$ FOR INPUT AS FILE PR_HIS_DED.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PR_HIS_DED, &
		PRIMARY KEY &
		( &
			PR_HIS_DED::EMPNUM, &
			PR_HIS_DED::PR_END_DATE, &
			PR_HIS_DED::DTYPE, &
			PR_HIS_DED::CODE &
		)	DUPLICATES , &
		ACCESS MODIFY, ALLOW MODIFY

