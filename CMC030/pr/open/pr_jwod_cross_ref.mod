	!======================================================================
	! PR_JWOD_CROSS_REF file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PR_JWOD_CROSS_REF.CH%, STAT%)
	CALL READ_DEVICE('PR_JWOD_CROSS_REF',PR_JWOD_CROSS_REF.DEV$, STAT%)

	PR_JWOD_CROSS_REF.NAME$ = PR_JWOD_CROSS_REF.DEV$+"PR_JWOD_CROSS_REF.TBL"

	OPEN PR_JWOD_CROSS_REF.NAME$ FOR INPUT AS FILE PR_JWOD_CROSS_REF.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PR_JWOD_CROSS_REF, &
		PRIMARY KEY &
			PR_JWOD_CROSS_REF::SUBACCT, &
		ACCESS MODIFY, ALLOW MODIFY

