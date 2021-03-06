	!======================================================================
	! PR_WC_INSURANCE file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PR_WC_INSURANCE.CH%, STAT%)
	CALL READ_DEVICE('PR_WC_INSURANCE',PR_WC_INSURANCE.DEV$, STAT%)

	PR_WC_INSURANCE.NAME$ = PR_WC_INSURANCE.DEV$+"PR_WC_INSURANCE.TBL"

	OPEN PR_WC_INSURANCE.NAME$ FOR INPUT AS FILE PR_WC_INSURANCE.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PR_WC_INSURANCE, &
		PRIMARY KEY &
		( &
			PR_WC_INSURANCE::CODE, &
			PR_WC_INSURANCE::STATE, &
			PR_WC_INSURANCE::INS_TYPE, &
			PR_WC_INSURANCE::EFFDAT &
		)	, &
		ACCESS MODIFY, ALLOW MODIFY

