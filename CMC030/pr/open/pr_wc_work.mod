	!======================================================================
	! PR_WC_WORK file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PR_WC_WORK.CH%, STAT%)
	CALL READ_DEVICE('PR_WC_WORK',PR_WC_WORK.DEV$, STAT%)

	PR_WC_WORK.NAME$ = PR_WC_WORK.DEV$+"PR_WC_WORK.MAS"

	OPEN PR_WC_WORK.NAME$ FOR INPUT AS FILE PR_WC_WORK.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PR_WC_WORK, &
		PRIMARY KEY &
		( &
			PR_WC_WORK::STATE, &
			PR_WC_WORK::CODE, &
			PR_WC_WORK::EMPNUM &
		)	DUPLICATES , &
		ALTERNATE KEY &
		( &
			PR_WC_WORK::CODE, &
			PR_WC_WORK::EMPNUM &
		)	DUPLICATES CHANGES, &
		ALTERNATE KEY &
			PR_WC_WORK::EMPNUM &
			DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

