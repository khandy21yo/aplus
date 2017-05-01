	!======================================================================
	! PR_HIS_CHECK file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PR_HIS_CHECK.CH%, STAT%)
	CALL READ_DEVICE('PR_HIS_CHECK',PR_HIS_CHECK.DEV$, STAT%)

	PR_HIS_CHECK.NAME$ = PR_HIS_CHECK.DEV$+"PR_HIS_CHECK_"+BATCH_NO$+".ARC"

	OPEN PR_HIS_CHECK.NAME$ FOR INPUT AS FILE PR_HIS_CHECK.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PR_HIS_CHECK, &
		PRIMARY KEY &
		( &
			PR_HIS_CHECK::EMPNUM, &
			PR_HIS_CHECK::PR_END_DATE &
		)	, &
		ALTERNATE KEY &
		( &
			PR_HIS_CHECK::CHECK, &
			PR_HIS_CHECK::EMPNUM &
		)	DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

