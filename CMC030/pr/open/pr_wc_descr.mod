	!======================================================================
	! PR_WC_DESCR file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PR_WC_DESCR.CH%, STAT%)
	CALL READ_DEVICE('PR_WC_DESCR',PR_WC_DESCR.DEV$, STAT%)

	PR_WC_DESCR.NAME$ = PR_WC_DESCR.DEV$+"PR_WC_DESCR.MAS"

	OPEN PR_WC_DESCR.NAME$ FOR INPUT AS FILE PR_WC_DESCR.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PR_WC_DESCR, &
		PRIMARY KEY &
			PR_WC_DESCR::CODE, &
		ACCESS MODIFY, ALLOW MODIFY

